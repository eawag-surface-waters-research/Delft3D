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

      SUBROUTINE DLWQTK ( LUN    , ITIME  , IKTIM  , IKNMRK , NOSEG  ,
     +                    IS     , LUNTXT , ISFLAG , IFFLAG , IFIOPK )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:            : december 1994 by Jan van Beek
!
!     FUNCTION            : Updates kenmerk array
!
!     LOGICAL UNITNUMBERS : LUN(IS) - input unit intermediate file
!                           LUN(19) - job-log output file
!
!     SUBROUTINES CALLED  : SRSTOP, stops execution
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     LUN     INTEGER       *     INPUT   unit number intermediate file
!     ITIME   INTEGER       1     INPUT   Model timer
!     IKTIM   INTEGER       *     IN/OUT  Timers in file
!     IKNMRK  INTEGER   NOSEG,*   IN/OUT  Kenmerk array
!     NOSEG   INTEGER       1     INPUT   number of segments
!     IS      INTEGER       1     INPUT   Index number intermediate file
!     LUNTXT  CHAR*(*)      *     INPUT   text with the unit number
!     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
!     IFFLAG  INTEGER       1     INPUT   = 1 then first invocation
!     IFIOPK  INTEGER       1     IN/OUT  file option kenmerk array
!
!     DECLARATIONS        :
!
      use m_srstop
      use m_dhopnf
      use m_dhkmrk
      use m_dhimov
      use timers
      INTEGER       ITIME , NOSEG , IS    , ISFLAG, IFFLAG,
     +              IFIOPK, IKMRK1
      INTEGER       LUN(*)   , IKNMRK(NOSEG,*),
     +              IKTIM(*)

      CHARACTER*(*) LUNTXT(*)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqtk", ithandl )

!
!     Allocate the work array
!
!
!
!     If time variable then get variable kenmerk array
!
      IF ( IFIOPK .GT. 0 ) THEN
         LUNOUT = LUN(19)
!
!        if first time open intermediate file and
!        move original kenmerk array (column 1) to constant kenmerk array
!        (column 2)
!
         IF ( IFFLAG .EQ. 1 ) THEN
            CALL DHOPNF ( LUN(IS) , LUNTXT(IS) , IS    , 2     , IERR )
            CALL DHIMOV ( IKNMRK(1,1), IKNMRK(1,2), NOSEG )
         ENDIF
!
!        evaluate file option; read time-dependent kenmerk array into column 3
!
         IF ( IFIOPK .EQ. 1 ) THEN
!
!           one record per time step
!
            CALL DLWQKV(LUN(IS)   , LUNOUT, ITIME , IKNMRK(1,3), NOSEG ,
     +                  LUNTXT(IS), ISFLAG, IFFLAG)
            IF ( IFFLAG .EQ. -1 ) THEN
               IFIOPK =  0
               IFFLAG =  1
               CLOSE ( LUN(IS) )
            ENDIF
!
         ELSEIF ( IFIOPK .EQ. 2 ) THEN
!
!           Block function
!
            CALL DLWQKB ( LUN(IS)    , LUNOUT     ,
     +                    ITIME      , IKTIM(1)   ,
     +                    IKTIM(2)   , IKTIM(3)   ,
     +                    IKNMRK(1,3), IKNMRK(1,4),
     +                    NOSEG      , LUNTXT(IS) ,
     +                    ISFLAG     , IFFLAG     )
!
         ELSE
!
!           Wrong option
!
            WRITE(LUNOUT,2000)
            CALL SRSTOP(1)
!
         ENDIF
!
!        (column 2)
!
         DO 100 ISEG = 1 , NOSEG
            CALL DHKMRK(4,IKNMRK(ISEG,2),IKMRK4)
  100    CONTINUE
!
!        Change the time-variable kenmerk-array (column 3) such that it
!
         CALL CHKNMR ( LUN(19) , NOSEG  , IKNMRK(1,3) )

!
!        OR the constant and the time variable array's
!
         DO 200 ISEG = 1 , NOSEG
            IKNMRK(ISEG,1) = IKNMRK(ISEG,2) + IKNMRK(ISEG,3)
  200    CONTINUE
!
      ENDIF

      if ( timon ) call timstop ( ithandl )
      RETURN
!
 2000 FORMAT ('ERROR: wrong file option for kenmerk array')
      END
