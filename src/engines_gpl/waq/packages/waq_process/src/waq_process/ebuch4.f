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

      subroutine ebuch4 ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
      use m_monsys

!>\file
!>       Ebullition of methane (new, generic!)

!
!     Description of the module :
!
!        General water quality module for DELWAQ:
!        Methane ebullition on the basis of the assumption that
!        all methane in excess of saturation excapes the water system
!        instantaneously. The building up of a gas bubbles stock in
!        in the sediment is ignored. Process is valid for overlying
!        water as well as sediment.
!
!        ----- description of parameters -----
! Name    T   L I/O   Description                                   Units
! ----    --- -  -    -------------------                            ----
! CCH4    R*4 1 I concentration of dissolved methane               [gC/m3]
! CCH4S   R*4 1 O saturation concentration of dissolved methane    [gC/m3]
! DCH4    R*4 1 L diff. between actual and saturation methane conc.[gC/m3]
! DELT    R*4 1 I computational time-step                              [d]
! DEPTH   R*4 1 I total depth of the overlying water column            [m]
! FSCALE  R*4 1 I scaling factor methane ebullition                    [-]
! POROS   R*4 1 I porosity                                             [-]
! TEMP    R*4 1 I ambient temperature                                 [oC]
! TEMP20  R*4 1 L stand. temperature (20) minus ambient temperature   [oC]
!
!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------
!
      IMPLICIT NONE
!
      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
!
      INTEGER  IP1, IP2, IP3, IP4, IP5, IP6, IP7
      INTEGER  IN1, IN2, IN3, IN4, IN5, IN6, IN7
      INTEGER  IFLUX  , ISEG   , ILUMON
!
      REAL     CCH4   , CCH4S  , DCH4  , FSCALE
      REAL     POROS  , TEMP   , TEMP20, DEPTH  , DELT
!
      LOGICAL  FIRST
      SAVE     FIRST
      DATA     FIRST /.TRUE./
!
      CALL GETMLU(ILUMON)
!
      IN1  = INCREM( 1)
      IN2  = INCREM( 2)
      IN3  = INCREM( 3)
      IN4  = INCREM( 4)
      IN5  = INCREM( 5)
      IN6  = INCREM( 6)
      IN7  = INCREM( 7)
!
      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
!
!     -----Warnings-----
!
      IF (FIRST) THEN
      IF (PMSA(IP4) .LE. 0.0) THEN
              WRITE (ILUMON, *) 'WARNING : Poros',
     +                          ' should be greater than zero'
          ENDIF
          FIRST = .FALSE.
      ENDIF
!
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG

      IF (BTEST(IKNMRK(ISEG),0)) THEN
!
            CCH4   = MAX ( 0.0, PMSA(IP1 ) )
            FSCALE = PMSA(IP2 )
            TEMP   = PMSA(IP3 )
            POROS  = PMSA(IP4 )
            DEPTH  = PMSA(IP5 )
            DELT   = PMSA(IP6 )
!
!           Calculate the saturation concentration
!
            TEMP20 = 20 - TEMP
            CCH4S  = 18.76 * (1 + DEPTH/10.0) * (1.024**TEMP20)
            DCH4   = CCH4 / POROS - CCH4S
!
!           Calculate the ebullition flux
!
            IF ( DCH4 .LT. 0.0 ) THEN
                  FL( 1+IFLUX ) = 0.0
            ELSE
                  FL( 1+IFLUX ) = ( FSCALE * DCH4 ) / DELT
            ENDIF
!
!           The saturation concentration is output
!
            PMSA(IP7 ) = CCH4S
!
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
!
 9000 CONTINUE
!
      RETURN
!
      END
