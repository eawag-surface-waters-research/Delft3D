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

      subroutine ddepth ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
      use m_errsys
      use m_dhkmrk

!>\file
!>       Dynamic calculation of the depth as volume / surf

!
!     Description of the module :
!
!        General water quality module for DELWAQ:
!        DEPTH CALCULATION FROM HORIZONTAL SURFACE AREA OF A SEGMENT
!
! Name    T   L I/O   Description                                    Units
! ----    --- -  -    -------------------                             ----
! DEPTH   R*4 1 O depth of the water column                            [m]
! SURF    R*4 1 I surface area of segment                             [m2]
! VOLUME  R*4 1 I volume of segment                                   [m3]

!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
      character(55) message

      message = 'SURF in DDEPTH zero at segment:'
      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
!
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
      CALL DHKMRK(3,IKNMRK(ISEG),IKMRK3)
      IF (IKMRK3.EQ.1 .OR. IKMRK3.EQ.3) THEN
!
      VOLUME = PMSA(IP1 )
      SURF   = PMSA(IP2 )

      IF (SURF    .LT. 1E-30) THEN
         write ( message(32:55) , '(i9,1x,e14.6)' ) iseg, surf
         CALL ERRSYS ( message, 1 )
      ENDIF

!***********************************************************************
!**** Calculate DEPTH - minimum: "TINY" to avoid divisions by zero if
!     the volume happens to be zero
!***********************************************************************
!
      DEPTH = MAX( TINY(1.0), VOLUME / SURF )
!
      PMSA (IP3 ) = DEPTH
!
      ENDIF
!
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + INCREM (  1 )
      IP2   = IP2   + INCREM (  2 )
      IP3   = IP3   + INCREM (  3 )
!
 9000 CONTINUE
!
      RETURN
!
      END
