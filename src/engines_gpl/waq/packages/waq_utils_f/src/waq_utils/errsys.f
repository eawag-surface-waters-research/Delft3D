!!  Copyright (C)  Stichting Deltares, 2012-2023.
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
      module m_errsys
      use m_srstop
      use m_monsys


      implicit none

      contains


      SUBROUTINE ERRSYS(STRING,IERR)
!
      integer :: IERR
      integer :: IMLUN
      CHARACTER*(*) STRING
!
      CALL GETMLU(IMLUN)
      IF ( IMLUN .NE. 0 ) THEN
         WRITE(IMLUN,'(A)') STRING
      ELSE
         WRITE(*,*) STRING
      ENDIF
!
      CALL SRSTOP(2)
!
      RETURN
      END
      end module m_errsys
