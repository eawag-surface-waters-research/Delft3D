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
      module m_swoxy

      implicit none

      contains


      subroutine swoxy  ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Partitioning switch in WC, S1 and S2 based on actual and critical oxygen concentration

!
!     Description of the module :
!
! Name    T   L I/O   Description                                    Units
! ----    --- -  -    -------------------                            -----
! OXY     R*4 1 I     oxygen concentration                           [gO/m3]
! CROXY   R*4 1 I     critical oxygen concentration                  [gO/m3]
! SWITCH  R*4 1 O     switch for partitioning                        [-]
!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------

      IMPLICIT NONE

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      INTEGER  ISEG,
     +         IP1, IP2, IP3, IP4, IP5, IP6,
     +         IN1, IN2, IN3, IN4, IN5, IN6

      INTEGER  ISWWK, ISWS1, ISWS2
      REAL     OXY, CROXY, POROS

      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)
      IP6 = IPOINT(6)

      IN1 = INCREM(1)
      IN2 = INCREM(2)
      IN3 = INCREM(3)
      IN4 = INCREM(4)
      IN5 = INCREM(5)
      IN6 = INCREM(6)
!
      DO 9000 ISEG = 1 , NOSEG

      IF (BTEST(IKNMRK(ISEG),0)) THEN
!
      OXY   = PMSA(IP1)
      CROXY = PMSA(IP2)
      POROS = PMSA(IP3)

!*******************************************************************************
!**** if OXY > CROXY ISWOXY = 1  in Water Column and S1 (poriewater)
!****           else ISWOXY = 0  in Water Column and S1 (poriewater)
!****                ISWOXY = 0  always in S2
!***********************************************************************

      IF (OXY/POROS.LE.CROXY) THEN
        ISWWK = 0
        ISWS1 = 0
        ISWS2 = 0
      ELSE
        ISWWK = 1
        ISWS1 = 1
        ISWS2 = 0
      ENDIF

      PMSA(IP4) = ISWWK
      PMSA(IP5) = ISWS1
      PMSA(IP6) = ISWS2

      ENDIF
!
      IP1 = IP1 + IN1
      IP2 = IP2 + IN2
      IP3 = IP3 + IN3
      IP4 = IP4 + IN4
      IP5 = IP5 + IN5
      IP6 = IP6 + IN6
!
 9000 CONTINUE
!
      RETURN
!
      END

      end module m_swoxy
