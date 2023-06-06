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
      module m_dherrs

      use m_monsys
      use m_srstop

      implicit none

      contains


      SUBROUTINE DHERRS(STRING,IERR)
!
      CHARACTER*(*) STRING

      INTEGER       LUNREP
      INTEGER       IERR

!     message to screen

      WRITE(*,*) STRING

!     message to monitor or report file

      CALL GETMLU(LUNREP)
      IF ( LUNREP .GT. 0 ) THEN
         WRITE(LUNREP,*) STRING
      ENDIF
!
      CALL SRSTOP(1)
!
      RETURN
      END
      SUBROUTINE DHERR2 ( NAME  , VALUE , ISEG  , MODULE )
      CHARACTER*(*) NAME
      REAL          VALUE
      INTEGER       ISEG
      CHARACTER*(*) MODULE

      INTEGER       LUNREP

!     message to screen

      WRITE (*,*) ' Coefficient value out of range'
      WRITE (*,*) ' Coefficient name:',NAME
      WRITE (*,*) ' Coefficient value',VALUE
      WRITE (*,*) ' Coefficient value',MODULE
      IF ( ISEG .GT. 0 ) WRITE(*,*) ' in segment number:',ISEG

!     message to monitor or report file

      CALL GETMLU(LUNREP)
      IF ( LUNREP .GT. 0 ) THEN
         WRITE (LUNREP,*) ' Coefficient value out of range'
         WRITE (LUNREP,*) ' Coefficient name:',NAME
         WRITE (LUNREP,*) ' Coefficient value',VALUE
         IF ( ISEG .GT. 0 ) WRITE(LUNREP,*) ' in segment number:',ISEG
         WRITE (LUNREP,*) ' In subroutine ',MODULE
      ENDIF

      CALL SRSTOP(1)

      RETURN
      END
      end module m_dherrs
