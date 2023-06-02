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

      subroutine burial ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
      use m_srstop
      use m_monsys
      use m_dhkmrk

!>\file
!>       Burial total bottom mass (dry matter)

!
!     Description of the module :
!
!        General water quality module for DELWAQ:
!        BURIAL FLUX OF DRY MATTER ONLY
!
! Name    T   L I/O   Description                                    Units
! ----    --- -  -    -------------------                            -----
! ACTHS1  R*4 1 I  actual thickness of S1                              [m]
! ACTHS2  R*4 1 I  actual thickness of S2                              [m]
! DMS1    R*4 1 I  dry matter in S1                                    [g]
! DMS2    R*4 1 I  dry matter in S1                                    [g]
! DELT    R*4 1 I  DELWAQ timestep                                   [scu]
! EXCBS1  R*4 1 L  excess burial flux layer S1                  [gDM/m2/d]
! EXCBS2  R*4 1 L  excess burial flux layer S2                  [gDM/m2/d]
! FIXS1   R*4 1 I  fixed thickness of layer S1 (option fixed)          [m]
! FIXS2   R*4 1 I  fixed thickness of layer S2 (option fixed)          [m]
! FL (1)  R*4 1 O  burial flux S1->S2                           [gDM/m3/d]
! FL (2)  R*4 1 O  burial flxu S2->from system                  [gDM/m3/d]
! IAUSYS  R*4 1 I  ratio between auxiliary and system clock unit       [-]
! MAXS1   R*4 1 I  maximum thickness of layer S1 (option variable)     [m]
! MAXS2   R*4 1 I  maximum thickness of layer S2 (option variable)     [m]
! MAXBS1  R*4 1 L  max. burial flux layer S1                    [gDM/m2/d]
! MAXBS2  R*4 1 L  max. burial flux layer S2                    [gDM/m2/d]
! SOMSED  R*4 1 I  total sedimentation flux                     [gDM/m2/d]
! SW      R*4 1 I  swithc for burial option                            [-]
! UDFBS1  R*4 1 L  user defined burial from layer S1            [gDM/m2/d]
! UDFBS2  R*4 1 L  user defined burial from layer S2            [gDM/m2/d]
! VBUR    R*4 1 I  first order burial rate constant                  [1/d]
! SURF    R*4 1 I  surfce area                                        [m2]
! ZERBUR  R*4 1 I  zeroth order burial flux                     [gDM/m2/d]

!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      INTEGER  LUNREP
!
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
      IP13 = IPOINT(13)
      IP14 = IPOINT(14)
      IP15 = IPOINT(15)
      IP16 = IPOINT(16)
      IP17 = IPOINT(17)
      IP18 = IPOINT(18)
      IP19 = IPOINT(19)
      IP20 = IPOINT(20)
      IP21 = IPOINT(21)
      IP22 = IPOINT(22)
      IP23 = IPOINT(23)
      IP24 = IPOINT(24)
      IP25 = IPOINT(25)
      IP26 = IPOINT(26)
!
      IN1  = INCREM( 1)
      IN2  = INCREM( 2)
      IN3  = INCREM( 3)
      IN4  = INCREM( 4)
      IN5  = INCREM( 5)
      IN6  = INCREM( 6)
      IN7  = INCREM( 7)
      IN8  = INCREM( 8)
      IN9  = INCREM( 9)
      IN10 = INCREM(10)
      IN11 = INCREM(11)
      IN12 = INCREM(12)
      IN13 = INCREM(13)
      IN14 = INCREM(14)
      IN15 = INCREM(15)
      IN16 = INCREM(16)
      IN17 = INCREM(17)
      IN18 = INCREM(18)
      IN19 = INCREM(19)
      IN20 = INCREM(20)
      IN21 = INCREM(21)
      IN22 = INCREM(22)
      IN23 = INCREM(23)
      IN24 = INCREM(24)
      IN25 = INCREM(25)
      IN26 = INCREM(26)
!
      DO 9000 ISEG = 1 , NOSEG

      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN
!
      SOMSED  = PMSA(IP1 )
      RESS1   = PMSA(IP2 )
      RESS2   = PMSA(IP3 )
      ZBURS1  = PMSA(IP4 )
      ZBURS2  = PMSA(IP5 )
      VBURS1  = PMSA(IP6 )
      VBURS2  = PMSA(IP7 )
      ACTHS1  = PMSA(IP8 )
      ACTHS2  = PMSA(IP9 )
      MAXS1   = PMSA(IP10 )
      MAXS2   = PMSA(IP11 )
      ISW     = NINT(PMSA(IP12 ))
      FIXS1   = PMSA(IP13 )
      FIXS2   = PMSA(IP14 )
      RHOS1   = PMSA(IP15 )
      RHOS2   = PMSA(IP16 )
      PORS1   = PMSA(IP17 )
      PORS2   = PMSA(IP18 )
      DELT    = PMSA(IP19 )
      SURF    = PMSA(IP20 )

!*******************************************************************************
!**** Processes connected to the BURIAL of dry matter
!***********************************************************************

      BURS1   =  0.0
      UDFBS1  =  0.0
      EXCBS1  =  0.0
      BURS2   =  0.0
      UDFBS2  =  0.0
      EXCBS2  =  0.0

! --- First option (fixed layer thickness)
      IF ( ISW .EQ. 0 ) THEN

         IF (ACTHS1 .LT. FIXS1 ) THEN
            BURS1 = 0.0
         ELSE
            EXCBS1 = (ACTHS1-FIXS1)*RHOS1*(1.0-PORS1) / DELT
            BURS1 = SOMSED + EXCBS1

         ENDIF

         IF (ACTHS2 .LT. FIXS2 ) THEN
            BURS2 = 0.0
         ELSE
            EXCBS2 = (ACTHS2-FIXS2)*RHOS2*(1.0-PORS2) / DELT
            BURS2 = BURS1 + EXCBS2
         ENDIF

! --- Second  option (variable layer with variable but maximum thickness)
      ELSEIF ( ISW .EQ. 1 ) THEN

      DMS1 = ACTHS1 * SURF * RHOS1 * (1.-PORS1)
      DMS2 = ACTHS2 * SURF * RHOS2 * (1.-PORS2)

!     Determine maximum burial flux for layer S1
!     (available mass + sedimentation - resuspension)
      MAXBS1 = MAX (0.0, DMS1 / DELT / SURF +  SOMSED - RESS1 )

!     Determine user-defined burial fluxes
      UDFBS1 = ZBURS1 + VBURS1 * MAX (DMS1, 0.0) / SURF
      UDFBS2 = ZBURS2 + VBURS2 * MAX (DMS2, 0.0) / SURF

!     Determine excess burial fluxes (if layer is > max u-d thickness)
      EXCBS1 = MAX(0.0,(ACTHS1-MAXS1)) * RHOS1 * (1-PORS1) / DELT
      EXCBS2 = MAX(0.0,(ACTHS2-MAXS2)) * RHOS2 * (1-PORS2) / DELT

!     Determine actual burial flux layer S1
      BURS1 = MIN ( UDFBS1 + EXCBS1, MAXBS1 )

!     Determine maximum burial flux for layer S2
!     (available mass + sedimentation - resuspension)
      MAXBS2 = MAX (0.0, DMS2 / DELT / SURF +  BURS1  - RESS2 )

!     Determine actual burial flux layer S1
      BURS2 = MIN ( UDFBS2 + EXCBS2, MAXBS2 )

!     Unknown option SwSediment
      ELSE
        CALL GETMLU(LUNREP)
        WRITE(LUNREP,*) 'BURIAL: SwSediment should equal 0 or 1! Not',
     &   ISW
        WRITE(*,*) 'BURIAL: SwSediment should equal 0 or 1! Not', ISW
        CALL SRSTOP(1)

      ENDIF

      PMSA ( IP21 ) =  BURS1
      PMSA ( IP22 ) =  UDFBS1
      PMSA ( IP23 ) =  EXCBS1
      PMSA ( IP24 ) =  BURS2
      PMSA ( IP25 ) =  UDFBS2
      PMSA ( IP26 ) =  EXCBS2
!
      ENDIF
      ENDIF
!
      IP1   = IP1   + IN1
      IP2   = IP2   + IN2
      IP3   = IP3   + IN3
      IP4   = IP4   + IN4
      IP5   = IP5   + IN5
      IP6   = IP6   + IN6
      IP7   = IP7   + IN7
      IP8   = IP8   + IN8
      IP9   = IP9   + IN9
      IP10  = IP10  + IN10
      IP11  = IP11  + IN11
      IP12  = IP12  + IN12
      IP13  = IP13  + IN13
      IP14  = IP14  + IN14
      IP15  = IP15  + IN15
      IP16  = IP16  + IN16
      IP17  = IP17  + IN17
      IP18  = IP18  + IN18
      IP19  = IP19  + IN19
      IP20  = IP20  + IN20
      IP21  = IP21  + IN21
      IP22  = IP22  + IN22
      IP23  = IP23  + IN23
      IP24  = IP24  + IN24
      IP25  = IP25  + IN25
      IP26  = IP26  + IN26
!
 9000 CONTINUE
!
      RETURN
!
      END
