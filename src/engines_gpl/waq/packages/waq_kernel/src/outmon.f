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

      SUBROUTINE OUTMON ( IOUT  , IDUMP , CONC  , AMASS2, ITIME ,
     +                    DNAME , SNAME , MNAME , NODUMP, NOTOT ,
     +                    IP    , ISFLAG, ASMASS, IBFLAG, NOTOT2,
     +                    SYNAM2, CONC2 , ITSTRT, ITSTOP, NDMPAR,
     +                    DANAM )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: april 4, 1988 by L.Postma
!
!     FUNCTION            : Writes monitoring results to IOUT in
!                                          blocks of 10 systems.
!
!     LOGICAL UNITNUMBERS : IOUT = number of monitoring output file
!
!     SUBROUTINES CALLED  : OUTMO1, print routine
!                           OUTMO2, print routine
!                           REPTIM, writes time in specific formats
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     IOUT    INTEGER     1       INPUT   unit number output file
!     IDUMP   INTEGER  NODUMP     INPUT   segment numbers for dump
!     AMASS   REAL     NOTOT*?    INPUT   mass in the whole system
!     CONC    REAL     NOTOT*?    INPUT   concentration values
!     AMASS2  REAL     NOTOT*5    INPUT   mass balance whole system
!     ITIME   INTEGER     1       INPUT   present time in clock units
!     DNAME   CHAR*20   NODUMP    INPUT   names of monitoring stations
!     SNAME   CHAR*20   NOTOT     INPUT   names of substances
!     MNAME   CHAR*40     4       INPUT   model identification
!     NODUMP  INTEGER     1       INPUT   amount of dump segments
!     NOTOT   INTEGER     1       INPUT   total number of systems
!     IP      INTEGER     4       IN/OUT  paging structure
!     ISFLAG  INTEGER     1       INPUT   if 1 then dd-hh:mm'ss"
!     ASMASS  REAL NOTOT*NDMPAR*? INPUT   Mass balance per segment
!     IBFLAG  INTEGER     1       INPUT   Flag = 1 then balances
!     NOTOT2  INTEGER             INPUT   Number of extra output vars
!     SYNAM2  CHAR*20             INPUT   Names of extra output vars
!     CONC2   REAL    NOTOT1*?    INPUT   Value of extra vars
!     ITSTRT  INTEGER     1       INPUT   start time
!     ITSTOP  INTEGER     1       INPUT   stop time
!
!
      use m_reptim
      use timers

      INTEGER      IOUT  , ITIME , NODUMP, NOTOT , ISFLAG,
     +             IBFLAG, NOTOT2, ITSTRT, ITSTOP, NDMPAR
      INTEGER      IDUMP(*)      , IP(4)
      REAL         CONC(NOTOT,*)         , AMASS2(NOTOT,5),
     +             ASMASS(NOTOT,NDMPAR,*) , CONC2(*)
      CHARACTER*20 DNAME(*) , SNAME(*) , SYNAM2(*) , DANAM(*)
      CHARACTER*40 MNAME(*)
!
!     Local declaration
!
      CHARACTER*40 VNAME
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "outmon", ithandl )
!
!         initialise the paging, accumulation arrays and acumul flag
!
      IF ( IP(3) .EQ. 0 ) THEN
         IP(3) = MAX(1,IP(1)/(7+(NODUMP+7)*((NOTOT+IP(2)-1)/IP(2))))
         IP(4) = 0
      ENDIF
!
!         start printing
!
      IF ( MOD(IP(4),IP(3)) .EQ. 0 ) THEN
         WRITE (IOUT,'('' '')')
         WRITE (IOUT,2100 ) ( MNAME(K),K=1,4)
      ENDIF
      IP(4) = IP(4) + 1
!
      PERCIT = 100.*(ITIME-ITSTRT)/(ITSTOP-ITSTRT)
      WRITE ( IOUT, 2080 ) PERCIT
      CALL REPTIM ( 6     , ITIME , ISFLAG, PERCIT)
      WRITE ( IOUT, 2000 )
      CALL REPTIM ( IOUT  , ITIME , ISFLAG, -999.0)
      WRITE ( IOUT, *    )
!
      DO 50 ID = 1 , NOTOT , IP(2)
         NEND = MIN ( NOTOT , ID+IP(2)-1 )
         WRITE (IOUT,2030) (AMASS2(K,1)    ,K=ID,NEND)
         WRITE (IOUT,2040) (AMASS2(K,2)    ,K=ID,NEND)
         WRITE (IOUT,2050) (AMASS2(K,3)    ,K=ID,NEND)
         WRITE (IOUT,2060) (AMASS2(K,4)    ,K=ID,NEND)
         WRITE (IOUT,2070) (AMASS2(K,5)    ,K=ID,NEND)
         WRITE (IOUT,2020) (SNAME(K)( 1:10),K=ID,NEND)
         WRITE (IOUT,2020) (SNAME(K)(11:20),K=ID,NEND)
!
         VNAME = 'CONCENTRATION'
         CALL OUTMO1 ( IOUT  , IDUMP , CONC  , VNAME , DNAME ,
     *                 NODUMP, ID    , NEND  , NOTOT )
         IF ( IBFLAG .EQ. 1 ) THEN
            VNAME = 'MASS'
            CALL OUTMO2 ( IOUT  , ASMASS(1,1,1), VNAME , DANAM , NDMPAR,
     +                    ID    , NEND         , NOTOT )
            VNAME = 'PROCESSES'
            CALL OUTMO2 ( IOUT  , ASMASS(1,1,2), VNAME , DANAM , NDMPAR,
     +                    ID    , NEND         , NOTOT )
            VNAME = 'LOADS ( IN )'
            CALL OUTMO2 ( IOUT  , ASMASS(1,1,3), VNAME , DANAM , NDMPAR,
     +                    ID    , NEND         , NOTOT )
            VNAME = 'LOADS ( OUT )'
            CALL OUTMO2 ( IOUT  , ASMASS(1,1,4), VNAME , DANAM , NDMPAR,
     +                    ID    , NEND         , NOTOT )
            VNAME = 'TRANSPORT ( IN )'
            CALL OUTMO2 ( IOUT  , ASMASS(1,1,5), VNAME , DANAM , NDMPAR,
     +                    ID    , NEND         , NOTOT )
            VNAME = 'TRANSPORT ( OUT )'
            CALL OUTMO2 ( IOUT  , ASMASS(1,1,6), VNAME , DANAM , NDMPAR,
     +                    ID    , NEND         , NOTOT )
         ENDIF
!
         WRITE (IOUT,'('' '')')
   50 CONTINUE
!
!     extra vars
!
      DO 60 ID = 1 , NOTOT2, IP(2)
         NEND = MIN ( NOTOT2, ID+IP(2)-1 )
         WRITE (IOUT,2020) (SYNAM2(K)( 1:10),K=ID,NEND)
         WRITE (IOUT,2020) (SYNAM2(K)(11:20),K=ID,NEND)
!
         VNAME = 'VALUE'
         CALL OUTMO2 ( IOUT  , CONC2 , VNAME , DNAME , NODUMP,
     +                 ID    , NEND  , NOTOT2)
!
         WRITE (IOUT,'('' '')')
   60 CONTINUE
!
      if ( timon ) call timstop ( ithandl )
      RETURN
!
 2000 FORMAT (//' DUMP OF INTERMEDIATE RESULTS IN SELECTED SEGMENTS')
 2020 FORMAT (22X,10(A10,' '))
 2030 FORMAT (  ' TOTAL MASS IN SYSTEM',10(1P,E11.4))
 2040 FORMAT (  ' CHANGES BY PROCESSES',10(1P,E11.4))
 2050 FORMAT (  ' CHANGES BY LOADS    ',10(1P,E11.4))
 2060 FORMAT (  ' BOUNDARY INFLOWS    ',10(1P,E11.4))
 2070 FORMAT (  ' BOUNDARY OUTFLOWS   ',10(1P,E11.4))
 2080 FORMAT (' ',F6.2,'% Completed')
 2100 FORMAT (       45X, A40                       )
!
      END
