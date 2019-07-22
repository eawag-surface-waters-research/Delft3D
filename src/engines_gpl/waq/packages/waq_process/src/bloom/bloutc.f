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

!    Date:       7 Januari 1994
!    Program:    BLOUTC.FOR
!    Version:    0.1
!    Programmer: Jos van Gils
!
!    Set output controls for BLOOM
!
!    Called by: BLOOMC
!    Calls    : -

      subroutine bloutc (histor,lprino,ldumpo)
      
      use bloom_data_dim 
      use bloom_data_phyt    
      use bloom_data_sumou   
      
      implicit none

      logical histor    !  Flag to activate output
      integer lprino    !  Saves original value of LPRINT
      integer ldumpo    !  Saves original value of IDUMP

!     LPRINT and IDUMP are output control flags of Bloom
!     They have been read from the input and their value has been
!     saved in LPRINO and LDUMPU
!     Here we set them to their original values, ONLY if HISTOR is
!     true, that is for history elements at history times.
!     This is to avoid excessively sized output files of Bloom

      lprint = 0
      idump  = 0
      if (histor) then
          lprint = lprino
          idump  = ldumpo
      endif

      return
      end

