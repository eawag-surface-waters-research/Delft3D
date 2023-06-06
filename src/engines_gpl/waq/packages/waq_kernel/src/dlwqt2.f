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

      SUBROUTINE DLWQT2 ( LUNIN  , LUNOUT , ITIME  , RESULT , NTOTAL ,
     *                    LUNTXT , ISFLAG , IFFLAG , ONLINE  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:    march 1988 by L.Postma
!
!     FUNCTION            : Makes values at ITIME for user supplied
!                                         binary intermediate files
!
!     LOGICAL UNITNUMBERS : LUNIN  - input unit intermediate file
!                           LUNOUT - monitor file
!
!     SUBROUTINES CALLED  : SRSTOP, stops execution
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     LUNIN   INTEGER       1     INPUT   unit number intermediate file
!     LUNOUT  INTEGER       1     INPUT   unit number monitor file
!     ITIME   INTEGER       1     INPUT   Model timer
!     RESULT  REAL     NTOTAL     OUTPUT  result array at time ITIME
!     NTOTAL  INTEGER       1     INPUT   number of items to be filled
!     LUNTXT  CHAR*(*)      1     INPUT   text concerning unit numbers
!     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
!     IFFLAG  INTEGER       1     INPUT   = 1 then first invocation
!
!     DECLARATIONS        :
!
      use m_srstop
      use timers

      DIMENSION     RESULT(NTOTAL)
      CHARACTER*10  MSGTXT(3)
      CHARACTER*(*) LUNTXT
      LOGICAL       ONLINE
      DATA          MSGTXT /' REWIND   ' , ' CONSTANT ' , ' ERROR    '/
      logical        stream_access                     ! help variable to detect the type of file access
      character(20)  access                            ! help variable to detect the type of file access

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqt2", ithandl )

      IF ( ONLINE ) THEN
         if ( lunin .eq. 20 ) write (*,*) ' Read VOLUME record'
         if ( lunin .eq. 24 ) write (*,*) ' Read FLOW   record'
      ENDIF
!
!         is this the first time?
!         BYPASS FOR ONLINE MODE, TO AVOID APPARENT CONSTANT FUNCTION
!
      MESSGE = 0
      IF ( IFFLAG .EQ. 1 .AND. .NOT. ONLINE ) GOTO 20
!
!         normal time varying read
!
      READ  ( LUNIN , END=10 , ERR=40 ) ITIME1 , RESULT
      goto 9999  !   RETURN
!
!         normal rewind.
!
   10 MESSGE = 1
      IF ( ONLINE ) STOP 'REWIND NOT POSSIBLE IN ON-LINE MODE'
      inquire( lunin, access = access )
      stream_access = access == 'STREAM'
      if (stream_access) then
         read( lunin, iostat = ierr, pos = 1 )
      else
         rewind lunin                            ! Start at the beginning again
      endif
      READ  ( LUNIN , END=40 , ERR=40 ) ITIME1 , RESULT
      GOTO 50
!
!         This is the first time, check only for nr of records.
!
   20 CONTINUE
      READ  ( LUNIN , END=40 , ERR=40 ) ITIME1 , RESULT
      READ  ( LUNIN , END=30 , ERR=40 ) ITIME1 , RESULT
      inquire( lunin, access = access )
      stream_access = access == 'STREAM'
      if (stream_access) then
         read( lunin, iostat = ierr, pos = 1 )
      else
         rewind lunin                            ! Start at the beginning again
      endif
      READ  ( LUNIN , END=30 , ERR=40 ) ITIME1 , RESULT
      goto 9999  !   RETURN
!
!         file has only one record, array is constant
!
   30 MESSGE =  2
      inquire( lunin, access = access )
      stream_access = access == 'STREAM'
      if (stream_access) then
         read( lunin, iostat = ierr, pos = 1 )
      else
         rewind lunin                            ! Start at the beginning again
      endif
      READ  ( LUNIN , END=40 , ERR=40 ) ITIME1 , RESULT
      IFFLAG = -1
      GOTO 50
!
!         error, during read
!
   40 MESSGE = 3
   50 IF ( ISFLAG .EQ. 1 ) THEN
           WRITE(LUNOUT,2010) MSGTXT(MESSGE), LUNIN, LUNTXT ,
     *                        ITIME /86400, MOD(ITIME ,86400)/3600,
     *                        MOD(ITIME ,3600)/60, MOD(ITIME ,60) ,
     *                        ITIME1/86400, MOD(ITIME1,86400)/3600,
     *                        MOD(ITIME1,3600)/60, MOD(ITIME1,60)
      ELSEIF ( ISFLAG .EQ. 2 ) THEN
           WRITE(LUNOUT,2020) MSGTXT(MESSGE), LUNIN, LUNTXT ,
     *                            ITIME /31536000           ,
     *                        MOD(ITIME ,31536000)/86400    ,
     *                        MOD(ITIME ,86400)/3600        ,
     *                        MOD(ITIME ,3600)/60           ,
     *                        MOD(ITIME ,60)                ,
     *                            ITIME1/31536000           ,
     *                        MOD(ITIME1,31536000)/86400    ,
     *                        MOD(ITIME1,86400)/3600        ,
     *                        MOD(ITIME1,3600)/60           ,
     *                        MOD(ITIME1,60)
      ELSE
           WRITE(LUNOUT,2000) MSGTXT(MESSGE), LUNIN, LUNTXT ,
     *                        ITIME, ITIME1
      ENDIF
      IF ( MESSGE .LT. 3 ) goto 9999  !   RETURN
      CALL SRSTOP ( 1 )
 9999 if ( timon ) call timstop ( ithandl )
      RETURN
!
 2000 FORMAT (   A10  ,  'ON UNIT:',I10,', READING: ',A,/
     *         ' SIMULATION TIME :',I10,' !  TIME IN FILE: ',I10,' !')
 2010 FORMAT (   A10  ,  'ON UNIT:',I10,', READING: ',A,/
     *         ' SIMULATION TIME :',I5,'D ',I2,'H ',I2,'M ',I2,'S ! ',/
     *         ' TIME IN FILE    :',I5,'D ',I2,'H ',I2,'M ',I2,'S ! ')
 2020 FORMAT (   A10  ,  'ON UNIT:',I10,', READING: ',A,/
     *   ' SIMULATION TIME :',I2,'Y ',I3,'D ',I2,'H ',I2,'M ',I2,'S .',/
     *   ' TIME IN FILE    :',I2,'Y ',I3,'D ',I2,'H ',I2,'M ',I2,'S .')
!
      END
