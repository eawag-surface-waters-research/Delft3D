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

      SUBROUTINE DLWQT3 ( ITIME  , IPERIO , APHASE , AVALUE , NRHARM ,
     *                    NOSUB  , NOSPAC , IPOINT , NPOINT , RESULT ,
     *                    LUNTXT , LUNIN  , LUNOUT , ISFLAG , IFFLAG ,
     *                    UPDATE )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: april- 8-1988 by L.Postma
!
!     FUNCTION            : Makes harmonic function values.
!
!     LOGICAL UNITNUMBERS : LUNIN file for initialisation of harmonics
!                           LUNOUT - monitor file
!
!     SUBROUTINES CALLED  : SRSTOP, stops execution
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     ITIME   INTEGER       1     INPUT   Model timer
!     IPERIO  INTEGER   NRHARM    IN/OUT  harmonic periods
!     APHASE  REAL      NRHARM    IN/OUT  harmonic phases
!     AVALUE  REAL   NOSUB*NRHARM IN/OUT  amplitudes for NOSUB values
!     NRHARM  INTEGER       1     INPUT   number of harmonic records
!     NOSUB   INTEGER       1     INPUT   number of values in an item
!     NOSPAC  INTEGER       1     OUTPUT  space occupied by harmonics
!     IPOINT  INTEGER       ?     INPUT   pointer to output array
!     NPOINT  INTEGER       1     OUTPUT  last pointer in IPOINT
!     RESULT  REAL     NOSUB*?    OUTPUT  function values at ITIME
!     LUNTXT  CHAR*(*)      1     INPUT   text with unitnumber
!     LUNIN   INTEGER       1     INPUT   unit number intermediate file
!     LUNOUT  INTEGER       1     INPUT   unit number monitor file
!     ISFLAG  INTEGER       1     INPUT   = 1, 'DDHHMMSS' format
!     IFFLAG  INTEGER       1     INPUT   = 1, first invocation
!     UPDATE  LOGICAL       1     OUTPUT  set to T if function is updated
!                                         else set to F
!
!     Internal file structure:
!     IPERIO    APHASE     AVALUE --->
!     npoints1  n1-harmos  nosub averages             .
!     period1   phase1     nosub amplitudes           .
!        .         .                                  .
!     period-n1 phase-n1   nosub amplitudes           .
!     npoints2  n2-harmos  nosub averages             .
!     period1   phase1     nosub amplitudes           .
!        .         .                                  .
!     period-n2 phase-n2   nosub amplitudes   ---- NRHARMth line
!     IPOINT:
!     -npoints1--->---npoints2----->.....NPOINT     pointers
!
!     DECLARATIONS        :
!
      use m_srstop
      use timers

      PARAMETER     ( TWOPI = 6.28319 )
      DIMENSION     IPERIO(*) , APHASE(*) , AVALUE(*) , IPOINT(*) ,
     *              RESULT(NOSUB,*)
      CHARACTER*(*) LUNTXT
      LOGICAL       UPDATE
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqt3", ithandl )
!
!         are there harmonics? is this the initialisation phase?
!
      NOSPAC = 0
      NPOINT = 0
      UPDATE = .FALSE.
      IF ( NRHARM .EQ. 0 ) goto 9999  !   RETURN
      UPDATE = .TRUE.
      IREC   = 1
      ITEL   = 1
      IF ( IFFLAG .EQ. 0 ) GOTO 40
!
!         at first time, initialise arrays
!         loop over the blocks of harmonics ( must be less than NRHARM )
!
      DO 20 IB = 1 , NRHARM+1
      IF ( IREC .GT. NRHARM ) GOTO 30
!
!         loop over the number of harmonics
!
      READ ( LUNIN , END=80 , ERR=80 )   NOTOT      , APHASE(IREC) ,
     *                                 ( AVALUE(K+NOSPAC) , K=1,NOTOT )
      NOSPAC       = NOSPAC + NOTOT
      IPERIO(IREC) = NOTOT
      IHSTOP       = APHASE(IREC) + 0.5
      IREC         = IREC + 1
      DO 10 IH = 1 , IHSTOP
      READ ( LUNIN , END=80 , ERR=80 ) IPERIO(IREC) , APHASE(IREC) ,
     *                                 ( AVALUE(K+NOSPAC) , K=1,NOTOT )
      NOSPAC = NOSPAC + NOTOT
      IREC = IREC + 1
   10 CONTINUE
!
!         return only by IREC > NRHARM
!
   20 CONTINUE
   30 NOSPAC = 0
      NPOINT = 0
      IREC   = 1
      ITEL   = 1
!
!         loop over the blocks of harmonics ( must be less than NRHARM )
!
   40 DO 70 IB = 1 , NRHARM+1
      IF ( IREC .GT. NRHARM ) goto 9999  !   RETURN
!
!         loop over the number of harmonics
!
      NOTOT  = IPERIO(IREC)
      IHSTOP = APHASE(IREC) + 0.5
      ISTART = NPOINT + 1
      NPOINT = NPOINT + NOTOT/NOSUB
      DO 60 IH = 1 , IHSTOP + 1
!
!         harmonic function
!
      IF ( IH .EQ. 1 ) THEN
       FUNC = 1.0
      ELSE
       FUNC = SIN( ( FLOAT(ITIME)/IPERIO(IREC) - APHASE(IREC) )*TWOPI )
      ENDIF
!
!         loop over the pointers and the values
!
      DO 50 I1 = ISTART , NPOINT
      IV = IPOINT(I1)
      DO 50 I2 = 1,NOSUB
      RESULT(I2,IV) = RESULT(I2,IV) + FUNC*AVALUE(ITEL)
      ITEL = ITEL+1
   50 CONTINUE
!
!         increase the record counter
!
      IREC   = IREC + 1
      NOSPAC = NOSPAC + NOTOT
   60 CONTINUE
!
!         return only by IREC > NRHARM
!
   70 CONTINUE
!
!         errors during read
!
   80 IF ( ISFLAG .EQ. 1 ) THEN
           WRITE(LUNOUT,2020) LUNIN, LUNTXT,
     *                        ITIME/86400, MOD(ITIME,86400/3600),
     *                        MOD(ITIME ,3600)/60, MOD(ITIME ,60)
      ELSEIF ( ISFLAG .EQ. 2 ) THEN
           WRITE(LUNOUT,2030) LUNIN, LUNTXT,
     *                            ITIME /31536000           ,
     *                        MOD(ITIME ,31536000)/86400    ,
     *                        MOD(ITIME ,86400)/3600        ,
     *                        MOD(ITIME ,3600)/60           ,
     *                        MOD(ITIME ,60)
      ELSE
           WRITE(LUNOUT,2010) LUNIN, LUNTXT, ITIME
      ENDIF
      CALL SRSTOP(1)
 9999 if ( timon ) call timstop ( ithandl )
      RETURN
!
 2010 FORMAT ( ' ERROR   ON UNIT:',I10,', READING: ',A,/
     *         ' SIMULATION TIME:',I10,' !')
 2020 FORMAT ( ' ERROR   ON UNIT:',I10,', READING: ',A,/
     *         ' SIMULATION TIME:',I5,'D ',I2,'H ',I2,'M ',I2,'S !')
 2030 FORMAT ( ' ERROR   ON UNIT:',I10,', READING: ',A,/
     *    ' SIMULATION TIME:',I2,'Y ',I3,'D ',I2,'H ',I2,'M ',I2,'S .')
!
      END
