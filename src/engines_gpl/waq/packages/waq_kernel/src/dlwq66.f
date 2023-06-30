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
      module m_dlwq66

      implicit none

      contains


      SUBROUTINE DLWQ66 ( AMASS  , VOLUME , CONC   , NOTOT  , NOSEG  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED             : june 1988 by L.Postma
!
!     FUNCTION            : makes masses from conc and volumes
!
!     LOGICAL UNITNUMBERS : none
!
!     SUBROUTINES CALLED  : none
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     AMASS   REAL   NOTOT*NOSEG  OUTPUT  closure error correction
!     VOLUME  REAL      NOSEG     INPUT   volume
!     CONC    REAL   NOTOT*NOSEG  INPUT   concentrations
!     NOTOT   INTEGER     1       INPUT   number of systems
!     NOSEG   INTEGER     1       INPUT   number of segments
!
      use timers

      real       AMASS(NOTOT,*) , VOLUME(*) , CONC(NOTOT,*)
      real       v1

      integer    notot, noseg
      integer    isys, iseg

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq66", ithandl )
!
!         loop over the number of segments and systems
!
      DO 10 ISEG = 1 , NOSEG
      V1 = VOLUME(ISEG)
      DO 10 ISYS = 1 , NOTOT
      AMASS(ISYS,ISEG) = CONC(ISYS,ISEG)*V1
   10 CONTINUE
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END

      end module m_dlwq66
