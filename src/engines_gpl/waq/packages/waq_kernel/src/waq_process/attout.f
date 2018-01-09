!!  Copyright (C)  Stichting Deltares, 2012-2018.
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

      subroutine attout ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Returns the selected attribute

!
!     Description of the module :
!
! Name    T   L I/O   Description                                    Units
! ----    --- -  -    -------------------                            -----
! IDX     I*4 1 I  selected attribute number (note the type!)         [-]
! ATTRIB  I*4 1 O  value of the attribute                             [-]
!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      INTEGER  IDX
      INTEGER  ATTRIB

      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
!
      DO 9000 ISEG = 1 , NOSEG

      IDX    = PMSA(IP1 )
      CALL DHKMRK(IDX,IKNMRK(ISEG),ATTRIB)

      ! Store the value

      PMSA(IP2 ) = ATTRIB

      ! Next segment

      IP1   = IP1   + INCREM (  1 )
      IP2   = IP2   + INCREM (  2 )
!
 9000 CONTINUE
!
      RETURN
!
      END
