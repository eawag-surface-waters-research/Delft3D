!!  Copyright (C)  Stichting Deltares, 2012-2013.
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

      subroutine decbod ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Oxydation of BOD-fractions with Monod kinetics for the TEWOR models

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WaterKwaliteit en Ecologie          |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : OSIRIS Q2943
C     Author  : Annette Beems-Kuin
C     Date    : 030807             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     030807  Annette Beems   Decay of BOD for TEWOR models
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        Oxydation of three fractions of BOD (background, sewage overflow
C        slow and fast settling) via MONOD-kinetics
C
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C BOD5_1  R*4 1 I carbonaceous BOD (first pool) at 5 days          [gO/m3]
C BOD5_2  R*4 1 I carbonaceous BOD (second pool) at 5 days         [gO/m3]
C BOD5_3  R*4 1 I carbonaceous BOD (third pool) at 5 days          [gO/m3]
C BOD5    R*4 1 O total BOD at 5 days                              [gO/m3]
C BODU_1  R*4 1 L carbonaceous BOD (first pool) ultimate           [gO/m3]
C BODU_2  R*4 1 L carbonaceous BOD (second pool) ultimate          [gO/m3]
C BODU_3  R*4 1 L carbonaceous BOD (third pool) ultimate           [gO/m3]
C BODU    R*4 1 L total BOD ultimate                               [gO/m3]
C BOD5_1  R*4 1 I oxydation flux of BOD (first pool) at 5 days   [gO/m3,d]
C BOD5_2  R*4 1 I oxydation flux of BOD (second pool) at 5 days  [gO/m3,d]
C BOD5_3  R*4 1 I oxydation flux of BOD (third pool) at 5 days   [gO/m3,d]
C BODU_1  R*4 1 L oxydation flux of BOD (first pool) ultimate    [gO/m3,d]
C BODU_2  R*4 1 L oxydation flux of BOD (second pool) ultimate   [gO/m3,d]
C BODU_3  R*4 1 L oxydation flux of BOD (third pool) ultimate    [gO/m3,d]
C OXYDEM  R*4 1 I total oxygen demand oxydation of BOD           [gO/m3,d]
C RCBOD1  R*4 1 I oxydation reaction rate BOD (first pool)           [1/d]
C RCBOD2  R*4 1 I oxydation reaction rate BOD (second pool)          [1/d]
C RCBOD3  R*4 1 I oxydation reaction rate BOD (third pool)           [1/d]
C KMOX    R*4 1 I half sat const for oxygen limit. of BOD oxydation[gO/m3]
C OXFUNC  R*4 1 O limitation function of OXY on BOD oxydation          [-]
C OXY     R*4 1 I concentration of dissolved oxygen                 [g/m3]
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------
C
      IMPLICIT NONE
C
      REAL     PMSA  (*) , FL  (*)
      INTEGER  NOSEG , NOFLUX, NOQ1, NOQ2, NOQ3, NOQ4
      INTEGER  IPOINT(*)       , INCREM(*),
     +         IEXPNT(4,*)     , IKNMRK(*)
C
      INTEGER  IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10,
     +         IP11, IP12, IP13, IP14
      INTEGER  IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9, IN10,
     +         IN11, IN12, IN13, IN14
      INTEGER  IFLUX, ISEG, IKMRK1
      REAL     OXFUNC, OXY, BOD5_1, BOD5_2, BOD5_3, BODU_1, BODU_2,
     +         BODU_3, RCBOD1, RCBOD2, RCBOD3, BOD5, BODU, KMOX,
     +         dBOD51, dBOD52, dBOD53, dBODU1, dBODU2, dBODU3, OXYDEM
C
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
C
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
C
      IFLUX = 0
      DO ISEG = 1 , NOSEG
C
!!       CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!       IF ( IKMRK1 .GT. 0) THEN
         IF (BTEST(IKNMRK(ISEG),0)) THEN

            BOD5_1 = MAX ( 0.0, PMSA(IP1 ))
            BOD5_2 = MAX ( 0.0, PMSA(IP2 ))
            BOD5_3 = MAX ( 0.0, PMSA(IP3 ))
            RCBOD1 = PMSA(IP4 )
            RCBOD2 = PMSA(IP5 )
            RCBOD3 = PMSA(IP6 )
            KMOX   = PMSA(IP7 )
            OXY    = PMSA(IP8 )

C           Check if RC's are non zero

            IF (RCBOD1 .LT. 1E-10) THEN
               WRITE (*,*) 'RCBOD: Invalid value (zero)!'
               CALL SRSTOP(1)
            ENDIF
            IF (RCBOD2 .LT. 1E-10) THEN
               WRITE (*,*) 'RCBOD_2: Invalid value (zero)!'
               CALL SRSTOP(1)
            ENDIF
            IF (RCBOD3 .LT. 1E-10) THEN
               WRITE (*,*) 'RCBOD_3: Invalid value (zero)!'
               CALL SRSTOP(1)
            ENDIF
C
C           Calculation of ultimate BOD concentrations
C
            BODU_1 = BOD5_1 / (1 - EXP(-5 * RCBOD1))
            BODU_2 = BOD5_2 / (1 - EXP(-5 * RCBOD2))
            BODU_3 = BOD5_3 / (1 - EXP(-5 * RCBOD3))
C
C           Calculation of oxygen limitation
C
            OXFUNC = OXY / ( KMOX + OXY )
C
C           Calculation of fluxes
C
            dBOD51 = RCBOD1 * OXFUNC * BOD5_1
            dBOD52 = RCBOD2 * OXFUNC * BOD5_2
            dBOD53 = RCBOD3 * OXFUNC * BOD5_3
            dBODU1 = RCBOD1 * OXFUNC * BODU_1
            dBODU2 = RCBOD2 * OXFUNC * BODU_2
            dBODU3 = RCBOD3 * OXFUNC * BODU_3
            OXYDEM = dBODU1 + dBODU2 + dBODU3
C
            FL( 1 + IFLUX ) = dBOD51
            FL( 2 + IFLUX ) = dBOD52
            FL( 3 + IFLUX ) = dBOD53
            FL( 4 + IFLUX ) = OXYDEM
C
C           Output of module
C
            PMSA(IP9 ) = BODU_1
            PMSA(IP10) = BODU_2
            PMSA(IP11) = BODU_3
            PMSA(IP12) = OXFUNC
C
            BOD5 = BOD5_1 + BOD5_2 + BOD5_3
            BODU = BODU_1 + BODU_2 + BODU_3
C
            PMSA(IP13) = BOD5
            PMSA(IP14) = BODU
C
         ENDIF
C
         IFLUX = IFLUX + NOFLUX
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
C
      ENDDO
C
      RETURN
C
      END
