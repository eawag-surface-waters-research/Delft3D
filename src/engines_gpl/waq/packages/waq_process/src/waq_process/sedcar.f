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

      subroutine sedcar ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Sedimentation routine used for OOC, algae, BOD pools, bacteria etc.

!
!     Description of the module :
!
!        General water quality module for DELWAQ:
!        SEDIMENTATION FORMULATIONS
!        MODULE VALID FOR DETC, OOC, DIAT, AAP, BLOOM ALGAE
!
! Name    T   L I/O   Description                                    Units
! ----    --- -  -    -------------------                            -----
! CONC    R*4 1 I  concentration sedimenting material water        [gX/m3]
! DEPTH   R*4 1 I  DELWAQ depth                                        [m]
! FL (1)  R*4 1 O  sedimentation flux (water->mixinglayer)       [gX/m3/d]
! MINDEP  R*4 1 I  minimal depth for sedimentation                     [m]
! PSED    R*4 1 L  sedimentaion probability (0 - 1)                    [-]
! POTSED  R*4 1 L  potential sedimentation flux                  [gX/m2/d]
! TAU     R*4 1 I  calculated sheerstress                        [kg/m/s2]
! TAUVEL  R*4 1 I  total velocity calcualted from tau                [m/s]
! TCRSED  R*4 1 I  critical sheerstress sedimentation            [kg/m/s2]
! VCRSED  R*4 1 I  critical velocity sedimentation                   [m/s]
! VSED    R*4 1 O  first order sedimentaion rate (calculated)        [m/d]
! ZERSED  R*4 1 I  zeroth order sedimentation flux               [gX/m2/d]

!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------

      use m_monsys
      use m_getcom
      use m_dhkmrk
      USE BottomSet     !  Module with definition of the waterbottom segments

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      REAL     MINDEP, MINDE2, DEPTH , DEPTH2

      LOGICAL, SAVE :: FIRST = .TRUE.
      LOGICAL       :: SW_PSEDMIN
      INTEGER       :: IDUMMY
      REAL   , SAVE :: PSEDMIN
      CHARACTER     :: CDUMMY
      INTEGER       :: IERR2
      INTEGER       :: LUNREP

      IF ( FIRST ) THEN
         CALL GETCOM('-psedmin', 2 , SW_PSEDMIN, IDUMMY, PSEDMIN, CDUMMY, IERR2)
         IF ( SW_PSEDMIN ) THEN
            CALL GETMLU(LUNREP)
            IF ( IERR2 .EQ. 0 ) THEN
               WRITE(LUNREP,*) ' option -psedmin found, value: ',PSEDMIN
            ELSE
               WRITE(LUNREP,*) ' ERROR: option -psedmin found but value not correct: ',PSEDMIN
            ENDIF
         ELSE
            PSEDMIN = 0.0
         ENDIF
         FIRST = .FALSE.
      ENDIF

      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
      IP8  = IPOINT( 8)
      IP9  = IPOINT( 9)
      IP10 = IPOINT(10)
      IP11 = IPOINT(11)
      IP12 = IPOINT(12)

      IN1  = INCREM(1 )
      IN2  = INCREM(2 )
      IN3  = INCREM(3 )
      IN4  = INCREM(4 )
      IN5  = INCREM(5 )
      IN6  = INCREM(6 )
      IN7  = INCREM(7 )
      IN8  = INCREM(8 )
      IN9  = INCREM(9 )
      IN10 = INCREM(10)
      IN11 = INCREM(11)
      IN12 = INCREM(12)

      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG

!     zero output

      PMSA (IP10) = 0.0
      PMSA (IP11) = 0.0

!     sedimentation towards the bottom

      CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
      IF (IKMRK1.EQ.1) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN
!
      CONC    = MAX (0.0, PMSA(IP1) )
      ZERSED  = PMSA(IP2 )
      VSED    = MAX (0.0, PMSA(IP3) )  ! Avoid inadvertent source if VSED negative (Delft3D-35562)
      TAU     = PMSA(IP4 )
      TCRSED  = PMSA(IP5 )
      DEPTH   = PMSA(IP6 )
      DELT    = PMSA(IP7 )
      MINDEP  = PMSA(IP8 )

!***********************************************************************
!**** Processes connected to the SEDIMENTATION
!***********************************************************************

!     Calculate sedimenation probability

      IF (TAU .EQ. -1.0) THEN
          PSED = 1.0
      ELSEIF (TCRSED .LT. 1E-20 )  THEN
          PSED = 0.0
      ELSE
!         vergelijking met critische schuifspanning
          PSED = MAX ( 0.0, (1.0 - TAU/TCRSED) )
      ENDIF
      PSED = MAX( PSEDMIN, PSED )

!     Calculate potential sedimentation fluxes
!     No sedimentation when depth below min depth

      IF ( DEPTH .LT. MINDEP) THEN
         MAXSED       = 0.0
         FL( 1+IFLUX) = 0.0
      ELSE
         POTSED = ZERSED + ( VSED * CONC ) * PSED

!        limit sedimentation to available mass (M/L2/DAY)
         MAXSED = MIN (POTSED, CONC / DELT * DEPTH)

!        convert sedimentation to flux in M/L3/DAY
         FL( 1+IFLUX) =  MAXSED / DEPTH
      ENDIF

!     Output of calculated sedimentation rate
      PMSA (IP10) = PSED
      PMSA (IP11) = MAXSED
!
      ENDIF
      ENDIF
!
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + IN1
      IP2   = IP2   + IN2
      IP3   = IP3   + IN3
      IP4   = IP4   + IN4
      IP5   = IP5   + IN5
      IP6   = IP6   + IN6
      IP7   = IP7   + IN7
      IP8   = IP8   + IN8
      IP10  = IP10  + IN10
      IP11  = IP11  + IN11
!
 9000 CONTINUE
!
      IP1  = IPOINT(1 )
      IP6  = IPOINT(6 )
      IP8  = IPOINT(8 )
      IP11 = IPOINT(11)

!.....Exchangeloop over de horizontale richting
      DO 8000 IQ=1,NOQ1+NOQ2

         PMSA(IP12) = 0.0

         IP12 = IP12 + IN12

 8000 CONTINUE

      IP9 = IP9 + ( NOQ1+NOQ2 ) * IN9

!.....Exchangeloop over de verticale richting
      DO 7000 IQ = NOQ1+NOQ2+1 , NOQ1+NOQ2+NOQ3+NOQ4

         IVAN  = IEXPNT(1,IQ)
         INAAR = IEXPNT(2,IQ)

         IF ( IVAN .GT. 0 .AND. INAAR .GT. 0 ) THEN

!           Zoek eerste kenmerk van- en naar-segmenten

            CALL DHKMRK(1,IKNMRK(IVAN ),IKMRKV)
            CALL DHKMRK(1,IKNMRK(INAAR),IKMRKN)
            IF (IKMRKV.EQ.1.AND.IKMRKN.EQ.3) THEN

!               Bodem-water uitwisseling: NUL FLUX OM OOK OUDE PDF's
!                                         TE KUNNEN GEBRUIKEN
!               Snelheid behoeft niet gezet (gebeurt in TRASED)

!               MAXSED = PMSA (IP11+(IVAN-1)*IN11)
!               CONC   = MAX (1E-20, PMSA(IP1+(IVAN-1)*IN1) )
!               PMSA(IP12) = MAXSED/86400./CONC
                FL ( 1 + (IVAN-1)*NOFLUX ) = 0.0

            ELSEIF (IKMRKV.EQ.1.AND.IKMRKN.EQ.1) THEN
!               Water-water uitwisseling
!rs             merk op: sedimentatie tussen waterlagen: geen taucr correctie,
!rs             alleen conversie van 1/d naar 1/s. Ten overvloede:
!rs             scu (s) en aux-timer (d) liggen dus vast!

                DEPTH  = PMSA(IP6+(IVAN -1)*IN6)
                DEPTH2 = PMSA(IP6+(INAAR-1)*IN6)
                MINDEP = PMSA(IP8+(IVAN -1)*IN8)
                MINDE2 = PMSA(IP8+(INAAR-1)*IN8)
                IF ( DEPTH .GT. MINDEP .AND. DEPTH2 .GT. MINDE2 ) THEN
                    PMSA(IP12) = PMSA(IP9)/86400.
                ELSE
                    PMSA(IP12) = 0.0
                ENDIF
            ELSE
                PMSA(IP12) = 0.0
            ENDIF

         ENDIF

         IP9 = IP9 + IN9
         IP12= IP12+ IN12

 7000 CONTINUE

!     Handle velocity to the delwaq-g bottom

      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
      IP8  = IPOINT( 8)
      IP9  = IPOINT( 9)
      IP10 = IPOINT(10)
      IP11 = IPOINT(11)
      IP12 = IPOINT(12)

      DO IK = 1 , Coll%cursize

         IWA1 = Coll%set(IK)%fstwatsed
         IWA2 = Coll%set(IK)%lstwatsed

         DO IQ = IWA1,IWA2
            IWATER  = IEXPNT(1,IQ)

            CONC    = MAX (0.0, PMSA(IP1+(IWATER-1)*IN1) )
            ZERSED  = PMSA(IP2+(IWATER-1)*IN2)
            VSED    = MAX (0.0, PMSA(IP3+(IWATER-1)*IN3) )
            TAU     = PMSA(IP4+(IWATER-1)*IN4)
            TCRSED  = PMSA(IP5+(IWATER-1)*IN5)
            DEPTH   = PMSA(IP6+(IWATER-1)*IN6)
            DELT    = PMSA(IP7+(IWATER-1)*IN7)
            MINDEP  = PMSA(IP8+(IWATER-1)*IN8)

!           Calculate sedimenation probability

            IF (TAU .EQ. -1.0) THEN
                PSED = 1.0
            ELSEIF (TCRSED .LT. 1E-20 )  THEN
                PSED = 0.0
            ELSE
!               vergelijking met critische schuifspanning
                PSED = MAX ( 0.0, (1.0 - TAU/TCRSED) )
            ENDIF

!           Bereken de potentiele sedimentatie fluxen
!           Geen sedimentatie onder een minimale diepte

            IF ( DEPTH .LT. MINDEP) THEN
               MAXSED       = 0.0
            ELSE
               POTSED = ZERSED + ( VSED * CONC ) * PSED

!              sedimenteer maximaal de aanwezige hoeveelheid (M/L2/DAY)
               MAXSED = MIN (POTSED, CONC / DELT * DEPTH)

            ENDIF

            IF ( CONC .GT. 1.E-10 ) THEN
               PMSA(IP12+(IQ-1)*IN12) = MAXSED/86400./CONC
            ENDIF

         ENDDO

      ENDDO
!
      RETURN
      END
