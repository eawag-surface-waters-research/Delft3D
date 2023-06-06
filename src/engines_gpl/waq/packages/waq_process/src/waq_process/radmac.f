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

      SUBROUTINE RADMAC     ( PMSA   , FL     , IPOINT , INCREM, NOSEG ,
     +                        NOFLUX , IEXPNT , IKNMRK , NOQ1  , NOQ2  ,
     +                        NOQ3   , NOQ4   )
      use m_monsys
      use m_dhkmrk

!
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      REAL(4) PMSA(*)     !I/O Process Manager System Array, window of routine to process library
      REAL(4) FL(*)       ! O  Array of fluxes made by this process in mass/volume/time
      INTEGER IPOINT( 9 ) ! I  Array of pointers in PMSA to get and store the data
      INTEGER INCREM( 9 ) ! I  Increments in IPOINT for segment loop, 0=constant, 1=spatially varying
      INTEGER NOSEG       ! I  Number of computational elements in the whole model schematisation
      INTEGER NOFLUX      ! I  Number of fluxes, increment in the FL array
      INTEGER IEXPNT      ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      INTEGER IKNMRK(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      INTEGER NOQ1        ! I  Nr of exchanges in 1st direction, only horizontal dir if irregular mesh
      INTEGER NOQ2        ! I  Nr of exchanges in 2nd direction, NOQ1+NOQ2 gives hor. dir. reg. grid
      INTEGER NOQ3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      INTEGER NOQ4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      INTEGER IPNT( 9 )   !    Local work array for the pointering
      INTEGER ISEG        !    Local loop counter for computational element loop
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
      REAL(4) ACTRAD, SATRAD, FRAD, RADTOP, HACT, TOTDEP, LOCDEP, DEPTH,
     J        EXT, ZM, Z1, DZ
      INTEGER IKMRK1, IKMRK2, ITOPSEG

      INTEGER       :: LUNREP
      INTEGER, SAVE :: NR_MSG = 0

!*******************************************************************************
!
      IPNT = IPOINT
!
      DO 9000 ISEG = 1 , NOSEG

      CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
      IF (IKMRK1.EQ.1) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN

!         Access conditions from the cell where the top of the plant is
          ITOPSEG = NINT(PMSA(IPNT(1)))
          IF (ITOPSEG .LE. 0 ) THEN
             CALL GETMLU( LUNREP )
             WRITE(LUNREP,*) 'RADMAC: top segment missing - needed for light intensity at tip of plant'
             WRITE(LUNREP,*) '   ISEG    =',ISEG
             WRITE(LUNREP,*) '   ITOPSEG =',ITOPSEG
             STOP 'Problem in RADMAC'
          ENDIF

!         Calculate light intensity at the tip of the plant
          RADTOP = PMSA(IPOINT(2)+(ITOPSEG-1)*INCREM(2))
          HACT   = PMSA(IPNT(3))
          TOTDEP = PMSA(IPNT(4))
          LOCDEP = PMSA(IPOINT(5)+(ITOPSEG-1)*INCREM(5))
          DEPTH  = PMSA(IPOINT(6)+(ITOPSEG-1)*INCREM(6))
          EXT    = PMSA(IPOINT(7)+(ITOPSEG-1)*INCREM(7))
          ZM     = TOTDEP - HACT
          Z1     = LOCDEP - DEPTH
          DZ     = ZM-Z1

          IF (DZ.LT.0.0 .OR. DZ.GT.DEPTH) THEN
             NR_MSG = NR_MSG + 1
             IF ( NR_MSG <= 25 ) THEN
                CALL GETMLU( LUNREP )
                WRITE(LUNREP,*) 'RADMAC: depth out of range'
                WRITE(LUNREP,*) '   ISEG  =',ISEG
                WRITE(LUNREP,*) '   ITOPS =',ITOPSEG
                WRITE(LUNREP,*) '   HACT  =',HACT
                WRITE(LUNREP,*) '   TOTDEP=',TOTDEP
                WRITE(LUNREP,*) '   LOCDEP=',LOCDEP
                WRITE(LUNREP,*) '   DEPTH =',DEPTH
                WRITE(LUNREP,*) '   ZM=',ZM
                WRITE(LUNREP,*) '   Z1=',Z1
                WRITE(LUNREP,*) '   DZ=',DZ
                IF ( NR_MSG == 25 ) THEN
                   WRITE(LUNREP,*) 'RADMAC: 25 messages written'
                   WRITE(LUNREP,*) 'RADMAC: further messages will be suppressed'
                ENDIF
             ENDIF
          ENDIF
          ACTRAD = RADTOP*EXP(-EXT*DZ)

!         Calculate and store light efficiency
          SATRAD = PMSA(IPNT(8))

          FRAD = MIN(1.0,ACTRAD/SATRAD)
          PMSA(IPNT(9))  = FRAD
!
      ENDIF
      ENDIF
!
      IPNT = IPNT + INCREM
!
 9000 CONTINUE
!
      RETURN
!
      END
