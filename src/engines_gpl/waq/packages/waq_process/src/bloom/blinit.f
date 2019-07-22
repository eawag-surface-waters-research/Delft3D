!!  Copyright (C)  Stichting Deltares, 2012-2019.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine blinit (lprino,ldumpo)

      use bloom_data_dim
      use bloom_data_matrix  
      use bloom_data_phyt    
      use bloom_data_sumou   

      implicit none

      integer lprino              ! Saves original value of LPRINT
      integer ldumpo              ! Saves original value of IDUMP

!     Local variables
      integer i, j                ! indexes

!     Convert BLOOM II specific units to DLWQWQ specific units
      call cvrblm

! Set A-matrix. Copy nutrient rows from AA (stochiometry matrix).
! Copy the extinction rows.
! Note: in steady state version of BLOOM the A matrix is updated each
! call of subroutine SETABC. This is not necessary now; the section
! in SETABC is skipped in the the dynamic version of the model.
      do j = 1,nuspec
         do i = 1,nunuco
            a(i,j) = aa(i,j)
         enddo
      enddo
      do j = 1,nuspec
         do i = nufili,nuabco
            a(i,j) = ekx(j)
         enddo
      enddo 

!  Call subroutine HDRBLM to write the headers for a number of output files.
      if (lprint .gt. 1) call hdrblm

!  Save originals of print flags for later use in BLOUTC
      lprino = lprint
      ldumpo = idump

      return
      end
