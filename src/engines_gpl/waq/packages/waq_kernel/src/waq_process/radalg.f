!!  Copyright (C)  Stichting Deltares, 2012-2014.
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

      subroutine radalg ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Light efficiency function DYNAMO algae

!
!     Description of the module :
!
!        General water quality module for DELWAQ:
!        DYNAMIC ALGEA MODEL: GREEN, BLUE AND DIATOMS
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
         SATRAD = TFGRO * SATRAD
!     actuele straling / straling voor groei verzadiging
         FRAD   = ACTRAD / SATRAD
         IF (ACTRAD .GT. 1E-5) THEN
            LNFRAD = LOG ( FRAD )
         ELSE
            LNFRAD = 0.0
         ENDIF
         LGTOPT = .FALSE.
      ELSE
         LGTOPT = .TRUE.
      ENDIF
!
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
!
      IF ( LGTOPT ) THEN
         ACTRAD = PMSA(IP2 )
         SATRAD = PMSA(IP3 )
         TFGRO  = PMSA(IP5 )
!
!        Correct SATRAD for temperature using Temp function for growth
!
         SATRAD = TFGRO * SATRAD
!     actuele straling / straling voor groei verzadiging
         FRAD   = ACTRAD / SATRAD
         IF (ACTRAD .GT. 1E-5) THEN
            LNFRAD = LOG ( FRAD )
         ELSE
            LNFRAD = 0.0
         ENDIF
      ENDIF
!
      DEPTH     = PMSA(IP1 )
      EXTINC    = PMSA(IP4 )
!
      IF (SATRAD .LT. 1E-20 )  CALL ERRSYS ('SATRAD in RADALG zero', 1 )
      IF (EXTINC .LT. 1E-20 )  CALL ERRSYS ('EXTINC in RADALG zero', 1 )

!     Light limitation functions (no inhibition assumed)
!     CALL EFFIC( EFF  , RAD  , RAD1  , EXT , DEPTH )
!     SUBROUTINE EFFIC( EFF , ACTRAD , SATRAD , EXTINC , DIEPTE )
! ----------------------------------------------------------------------
!     BEREKENING VAN DE GEMIDDELDE PRODUKTIE-EFFICIENTIE:
!     INTEGRATIE VAN DE LICHTFUNKTIE VOOR DE PRODUKTIESNELHEID OVER DE
!     DEPTH. DE FUNKTIE:
!     F(X) = X ,ALS X < 1
!     F(X) = 1 ,ALS X > 1
!     HIERIN IS X DE LICHTINTENSITEIT GEDEELD DOOR DE LICHTINTENSITEIT
!     WAARBIJ VERZADIGING OPTREEDT.
!     ER MOET REKENING GEHOUDEN WORDEN MET DE LIGGING VAN DE LICHTNIVEAU
!     AAN DE BEIDE GRENZEN: X(0), X(H) GROTER OF KLEINER DAN 1.
! ----------------------------------------------------------------------
!
!     Local
!

! ----Lichtniveau bij de bodem
      EXTDPT = EXTINC * DEPTH
      EXDIEP = EXP( - EXTDPT )
      RADBOD = FRAD * EXDIEP
!
!---- Treedt er aan het oppervlak verzadiging op of niet
      IF ( FRAD .GT. 1.0 ) GOTO 100
!
      PMSA(IP6 )  = ( FRAD - RADBOD ) / EXTDPT
      GOTO 8900
!
!---- er treedt verzadiging op, maar hoe zit het aan de bodem
  100 CONTINUE
      IF ( RADBOD .GT. 1.0 ) GOTO 110
!
      PMSA(IP6 )  = ( 1.0 + LNFRAD - RADBOD ) / EXTDPT
      GOTO 8900
!
!---- over de hele waterkolom treedt verzadiging op
  110 CONTINUE
      PMSA(IP6 )  =   1.0

 8900 CONTINUE
!
      ENDIF
!
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + IN1
         IP2 = IP2 + IN2
         IP3 = IP3 + IN3
      IP4   = IP4   + IN4
         IP5 = IP5 + IN5
      IP6   = IP6   + IN6
!
 9000 CONTINUE
!
      RETURN
!
      END
