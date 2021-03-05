!!  Copyright(C) Stichting Deltares, 2012.
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

      SUBROUTINE MPBNLM ( PMSA   , FL     , IPOINT , INCREM , NOSEG  ,
     +                    NOFLUX , IEXPNT , IKNMRK , NOQ1   , NOQ2   ,
     +                    NOQ3   , NOQ4   )
C***********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     +----------------------------------------+
C***********************************************************************
C
C     Function : MPB nutrient limitation function
C
C***********************************************************************

      IMPLICIT NONE

C     arguments

      REAL               :: PMSA(*)            ! in/out input-output array space to be adressed with IPOINT/INCREM
      REAL               :: FL(*)              ! in/out flux array
      INTEGER            :: IPOINT(*)          ! in     start index input-output parameters in the PMSA array (segment or exchange number 1)
      INTEGER            :: INCREM(*)          ! in     increment for each segment-exchange for the input-output parameters in the PMSA array
      INTEGER            :: NOSEG              ! in     number of segments
      INTEGER            :: NOFLUX             ! in     total number of fluxes (increment in FL array)
      INTEGER            :: IEXPNT(4,*)        ! in     exchange pointer table
      INTEGER            :: IKNMRK(*)          ! in     segment features array
      INTEGER            :: NOQ1               ! in     number of exchanges in first direction
      INTEGER            :: NOQ2               ! in     number of exchanges in second direction
      INTEGER            :: NOQ3               ! in     number of exchanges in third direction
      INTEGER            :: NOQ4               ! in     number of exchanges in fourth direction

C     from PMSA array

      REAL               :: CAM                !  1 in  Ammonium (NH4)                             (gN/m3)
      REAL               :: CNI                !  2 in  Nitrate (NO3)                              (gN/m3)
      REAL               :: CPHO               !  3 in  Ortho-Phosphate (PO4)                      (gP/m3)
      REAL               :: CSI                !  4 in  dissolved Silica (Si)                     (gSi/m3)
      REAL               :: KAM                !  5 in  MPB1 half saturation constant NH4          (gN/m3)
      REAL               :: KNI                !  6 in  MPB1 half saturation constant NO3          (gN/m3)
      REAL               :: KPHO               !  7 in  MPB1 half saturation constant PO4          (gP/m3)
      REAL               :: KSI                !  8 in  MPB1 half saturation constant Si          (gSi/m3)
      LOGICAL            :: S1_BOTTOM          !  9 in  switch for MPB model (0=segment,1=S1)          (-)
      REAL               :: CAMS1              ! 10 in  Ammonium concentration in the bottom       (gN/m3)
      REAL               :: CNIS1              ! 11 in  Nitrate concentration in layer S1          (gN/m3)
      REAL               :: CPHOS1             ! 12 in  Phosphate concentration in the bottom      (gP/m3)
      REAL               :: CSIS1              ! 13 in  Silicium concentration in layer S1        (gSi/m3)
      REAL               :: FAM                ! 14 out MPB ammonium limitation                        (-)
      REAL               :: FNI                ! 15 out MPB nitrate limitation                         (-)
      REAL               :: FPHO               ! 16 out MPB phosphate limitation                       (-)
      REAL               :: FSI                ! 17 out MPB silicate limitation                        (-)
      REAL               :: FNUT               ! 18 out MPB nutrient limitation                        (-)
      REAL               :: FAMS1              ! 19 out MPB ammonium limitation S1                     (-)
      REAL               :: FNIS1              ! 20 out MPB nitrate limitation S1                      (-)
      REAL               :: FPHOS1             ! 21 out MPB phosphate limitation S1                    (-)
      REAL               :: FSIS1              ! 22 out MPB silicate limitation S1                     (-)
      REAL               :: FNUTS1             ! 23 out MPB nutrient limitation S1                     (-)

C     local decalrations

      INTEGER            :: ISEG               ! loop counter segment loop
      INTEGER            :: IKMRK1             ! first feature inactive(0)-active(1)-bottom(2) segment
      INTEGER            :: IKMRK2             ! second feature 2D(0)-surface(1)-middle(2)-bottom(3) segment
      INTEGER, parameter :: NO_POINTER = 23    ! number of input output variables in PMSA array
      INTEGER            :: IP(NO_POINTER)     ! index pointer in PMSA array updated for each segment
      REAL               :: FNS1               ! N nutrient limitation
      REAL               :: FN                 ! N nutrient limitation S1
      REAL               :: CNN                ! Weigthed nitrogen concentration (a la DYNAMO)     (gN/m3)
      REAL               :: CNNS1              ! Weigthed nitrogen concentration, bottom           (gN/m3)
      REAL               :: AMOPRF = 1.0       ! Preference factor ammomium over nitrate (DYNAMO)      (-)

C     initialise pointers for PMSA and FL array

      IP = IPOINT(1:NO_POINTER)

C     loop over the segments

      DO 1000 ISEG = 1 , NOSEG

         CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
         CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)

         CAM       = MAX(PMSA(IP(1)),0.0)
         CNI       = MAX(PMSA(IP(2)),0.0)
         CPHO      = MAX(PMSA(IP(3)),0.0)
         CSI       = MAX(PMSA(IP(4)),0.0)
         KAM       = PMSA(IP(5))
         KNI       = PMSA(IP(6))
         KPHO      = PMSA(IP(7))
         KSI       = PMSA(IP(8))
         S1_BOTTOM = NINT(PMSA(IP(9))) .EQ. 1
         CAMS1     = MAX(PMSA(IP(10)),0.0)
         CNIS1     = MAX(PMSA(IP(11)),0.0)
         CPHOS1    = MAX(PMSA(IP(12)),0.0)
         CSIS1     = MAX(PMSA(IP(13)),0.0)

         CNN       = CAM   + CNI   / AMOPRF
         CNNS1     = CAMS1 + CNIS1 / AMOPRF

C        water en delwaq-g bodem

         IF ( (IKMRK1.EQ.1) .OR. (IKMRK1.EQ.2) ) THEN

            !FAM  = CAM/(KAM+CAM)
            !FNI  = CNI/(KNI+CNI)
            FN   = CNN/(KNI+CNN)
            FPHO = CPHO/(KPHO+CPHO)
            IF ( KSI .LT. 1E-20 ) THEN
               FSI = 1.0
            ELSE
               FSI  = CSI/(KSI+CSI)
            ENDIF
            !FN   = FAM + ( 1.0 - FAM ) * FNI
            FNUT = MAX( 0.0, MIN(FN,FPHO,FSI) )

         ELSE

            FAM  = 0.0
            FNI  = 0.0
            FPHO = 0.0
            FSI  = 0.0
            FNUT = 0.0

         ENDIF

C        s1 bodem

         IF ( S1_BOTTOM .AND. (IKMRK2.EQ.0 .OR. IKMRK2.EQ.3) ) THEN

            !FAMS1  = CAMS1/(KAM+CAMS1)
            !FNIS1  = CNIS1/(KNI+CNIS1)
            FNS1   = CNNS1/(KNI+CNNS1)
            FPHOS1 = CPHOS1/(KPHO+CPHOS1)
            IF ( KSI .LT. 1E-20 ) THEN
               FSIS1 = 1.0
            ELSE
               FSIS1  = CSIS1/(KSI+CSIS1)
            ENDIF
            !FNS1   = FAMS1 + ( 1.0 - FAMS1 ) * FNIS1
            FNUTS1 = MAX( 0.0, MIN(FNS1,FPHOS1,FSIS1) )

         ELSE

            FAMS1  = 0.0
            FNIS1  = 0.0
            FPHOS1 = 0.0
            FSIS1  = 0.0
            FNUTS1 = 0.0

         ENDIF

         PMSA(IP(14)) = FN !FAM
         PMSA(IP(15)) = FN !FNI
         PMSA(IP(16)) = FPHO
         PMSA(IP(17)) = FSI
         PMSA(IP(18)) = FNUT
         PMSA(IP(19)) = FNS1 !FAMS1
         PMSA(IP(20)) = FNS1 !FNIS1
         PMSA(IP(21)) = FPHOS1
         PMSA(IP(22)) = FSIS1
         PMSA(IP(23)) = FNUTS1

C        update pointering in PMSA

         IP    = IP    + INCREM(1:NO_POINTER)

 1000 CONTINUE


      RETURN
      END
