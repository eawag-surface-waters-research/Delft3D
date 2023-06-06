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

      subroutine rdbalg ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
      use m_errsys

!>\file
!>       Light efficiency function DYNAMO algae

!
!     Description of the module :
!
! Name    T   L I/O   Description                                   Unit
! ----    --- -  -    -------------------                            ---
! DEPTH   R*4 1 I depth of the water column                            [
! EFF     R*4 1 L average light efficiency green-algea                 [
! ACTRAD  R*4 1 I radiation                                         [W/m
! SATRAD  R*4 1 I radiation growth saturation green-algea           [W/m

!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      LOGICAL  LGTOPT
!
      IN1  = INCREM( 1)
      IN2  = INCREM( 2)
      IN3  = INCREM( 3)
      IN4  = INCREM( 4)
      IN5  = INCREM( 5)
      IN6  = INCREM( 6)
!
      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
!
      IF ( IN2 .EQ. 0 .AND. IN3 .EQ. 0 .AND. IN5 .EQ. 0 ) THEN
         ACTRAD = PMSA(IP2 )
         SATRAD = PMSA(IP3 )
         TFGRO  = PMSA(IP5 )
!
!        Correct SATRAD for temperature using Temp function for growth
!
!        SATRAD = TFGRO * SATRAD
         SATRAD = SATRAD
!     actuele straling / straling voor groei verzadiging
         FRAD   = ACTRAD / SATRAD
         LGTOPT = .FALSE.
      ELSE
         LGTOPT = .TRUE.
      ENDIF
!
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG

      IF (BTEST(IKNMRK(ISEG),0)) THEN
!
      IF ( LGTOPT ) THEN
         ACTRAD = PMSA(IP2 )
         SATRAD = PMSA(IP3 )
         TFGRO  = PMSA(IP5 )
!
!        Correct SATRAD for temperature using Temp function for growth
!
!        SATRAD = TFGRO * SATRAD
         SATRAD = SATRAD
!     actuele straling / straling voor groei verzadiging
         FRAD   = ACTRAD / SATRAD
      ENDIF
!
      PMSA(IP6) = MAX(MIN(FRAD,1.0),0.0)
!
      IF (SATRAD .LT. 1E-20 )  CALL ERRSYS ('SATRAD in RADALG zero', 1 )

 8900 CONTINUE
!
      ENDIF
!
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + IN1
      IP2 = IP2 + IN2
      IP3 = IP3 + IN3
      IP5 = IP5 + IN5
      IP4   = IP4   + IN4
      IP6   = IP6   + IN6
!
 9000 CONTINUE
!
      RETURN
!
      END
