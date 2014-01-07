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

      subroutine atmdep ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Atmosferic deposition and diffuse input of IMx, N, P, Org_us and Metals

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : STANDAARDISATIE PROCES FORMULES T721.72
C     Author  :
C     Date    : 921210             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     ......  ..............  ..............................
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        Atmosferic deposition
C
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                            -----
C ZFL     REAL        Zero'th oreder flux         M/m3/s
C DEPTH   R*4 1 I     depth                                          [m]
C SW1                 load option 0=all, 1=top, 2=bottom segments    (-)
C SW2                 maximise withdrawel to mass 0=no, 1=yes        (-)
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
      CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
      IF (IKMRK1.EQ.1) THEN
!     IF (BTEST(IKNMRK(ISEG),0)) THEN

      ZFL   = PMSA( IP1 )
      DEPTH = PMSA( IP2 )
      CONC  = PMSA( IP3 )
      ISW1  = NINT(PMSA( IP4 ))
      ISW2  = NINT(PMSA( IP5 ))
      DELT  = PMSA( IP6 )

      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ( ( ISW1   .EQ. 0                   ) .OR.    ! option load in all segments
     +     ( IKMRK2 .EQ. 0                   ) .OR.    ! segment with surface and bottom always a load
     +     ( IKMRK2 .EQ. 1 .AND. ISW1 .EQ. 1 ) .OR.    ! top segment and option top segment
     +     ( IKMRK2 .EQ. 3 .AND. ISW1 .EQ. 2 ) ) THEN  ! bottom segment and option bottom segment
C

C*******************************************************************************
C**** FLUX equals input divided by depth , M/m2/d * 1/m = M/m3/d
C***********************************************************************

      ZFL = ZFL / DEPTH

      IF ( ISW2 .EQ. 1 ) THEN
         ! maximise a withdrawel to the amount availeble in one timestep
         IF ( ZFL .LT. 0.0 ) THEN
            ZFL = MAX(ZFL,-CONC/DELT)
         ENDIF
      ENDIF

      FL( 1 + IFLUX ) =   ZFL

      ENDIF
      ENDIF
C
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + INCREM (  1 )
      IP2   = IP2   + INCREM (  2 )
      IP3   = IP3   + INCREM (  3 )
      IP4   = IP4   + INCREM (  4 )
      IP5   = IP5   + INCREM (  5 )
      IP6   = IP6   + INCREM (  6 )
C
 9000 CONTINUE
C
      RETURN
C
      END
