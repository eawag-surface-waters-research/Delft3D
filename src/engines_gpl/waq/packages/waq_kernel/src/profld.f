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
      module m_profld

      implicit none

      contains


      SUBROUTINE PROFLD (NOFLUX, NFLUX1, NFLUXP, IGRID , NOSEG2,
     +                   NOSEG , NDT   , ISDMP , GRDSEG, FLUX  ,
     +                   VOLUME, FLXDMP)
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:            : Oct 1998 by Jan van Beek
!
!     FUNCTION            : make FLXDMP array from FLUX array
!
!     SUBROUTINES CALLED  : -
!
!     FILES               : -
!
!     COMMON BLOCKS       : -
!
!     PARAMETERS          : 12
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     NOFLUX  INTEGER       1     INPUT   Nr. of fluxes (total)
!     NFLUX1  INTEGER       1     INPUT   first flux to be dumped
!     NFLUXP  INTEGER       1     INPUT   number of fluxes to be dumped
!     IGRID   INTEGER       1     INPUT   Grid number for FLUX array
!     NOSEG2  INTEGER       1     INPUT   number of segments in IGRID
!     NOSEG   INTEGER       1     INPUT   number of segments
!     NDT     INTEGER       1     INPUT   timestep multiplier in fractional step
!     ISDMP   INTEGER       *     INPUT   Segment to dumped segment pointer
!     GRDSEG  INTEGER       *     INPUT   Segment to sub-segment pointer for grids
!     FLUX    REAL          *     INPUT   fluxes at all segments
!     VOLUME  REAL          *     INPUT   Segment volumes
!     FLXDMP  REAL    NOFLUX*?    OUTPUT  fluxes at dump segments
!
!     Declaration of arguments
!
      use timers

      INTEGER NOFLUX, NFLUX1, NFLUXP, IGRID , NOSEG2,
     +        NOSEG , NDT
      INTEGER ISDMP(NOSEG)       , GRDSEG(NOSEG,*)
      REAL    FLUX(NOFLUX,NOSEG2), VOLUME(NOSEG)       ,
     +        FLXDMP(NOFLUX,*)

!     local
      integer iseg, iseg2, ips, iflux
      real    vol
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "profld", ithandl )
!
!     We construeren nu de FLUXDUMPEN
!
      DO ISEG = 1 , NOSEG
         IF ( ISDMP(ISEG) .GT. 0 ) THEN
            VOL   = VOLUME(ISEG)
            IPS   = ISDMP(ISEG)
            IF ( IGRID .NE. 1 ) THEN
               ISEG2 = GRDSEG(ISEG,IGRID)
            ELSE
               ISEG2 = ISEG
            ENDIF
            DO IFLUX = NFLUX1 , NFLUX1 + NFLUXP - 1
               FLXDMP(IFLUX,IPS) = FLUX(IFLUX,ISEG2)*VOL*NDT
            ENDDO
         ENDIF
      ENDDO
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END

      end module m_profld
