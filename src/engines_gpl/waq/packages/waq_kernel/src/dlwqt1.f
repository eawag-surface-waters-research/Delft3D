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

      SUBROUTINE DLWQT1 ( LUN    , ITIME  , ITIMEL , IHARM  , HARMAT ,
     *                    FARRAY , IPOINT , RESULT , NOSUB  , NRHARM ,
     *                    NTOT   , NRFTOT , IPA    , IPH    , IPF    ,
     *                    IPI    , LUNTXT , IS     , ISFLAG , IFFLAG ,
     *                    UPDATE , NEWSET , IOFF   , IWORK  , LSTREC ,
     *                    LREWIN , RECLST , ftype  , dlwqd  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:            : March   1988 by L.Postma
!
!     UPDATED:            : January 2001 by J.v.Gils:
!                           add Synchronisation mode
!
!                           July    2002 by Leo Postma
!                           Call to DLWQT4 changed and file opening in T4.
!
!     FUNCTION            : Makes values at ITIME for time dependent
!                                                            aspects
!
!     LOGICAL UNITNUMBERS : LUN(IS) - input unit intermediate file
!                           LUN( 4) - function pointers file
!                           LUN(19) - job-log output file
!
!     SUBROUTINES CALLED  : DLWQT2, makes values for user supplied files
!                           DLWQT3, makes values for harmonic function
!                           DLWQT4, makes values for block / linear
!                                   interpolated functions
!                           DHOPNF, opens files
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     LUN     INTEGER       *     INPUT   unit numbers
!     ITIME   INTEGER       1     INPUT   Model timer
!     ITIMEL  INTEGER       1     INPUT   Model timer previous time step
!     IHARM   INTEGER   NRHARM    IN/OUT  integer harmonics space
!                           *     INPUT   integer array space new version
!     HARMAT  REAL    (NRHARM,*)  INPUT   matrix with harmonic info
!                           *     INPUT   real array space new version
!     FARRAY  REAL    (NRFTOT,2)  INPUT   double file buffer
!     IPOINT  INTEGER   NTOT+3    INPUT   pointer to result array + ...
!                                 INPUT   type definition of items
!     RESULT  REAL          *     OUTPUT  result array at time ITIME
!     NOSUB   INTEGER       1     INPUT   amount of values per item
!     NRHARM  INTEGER       1     INPUT   amount of harmonic records
!     NTOT    INTEGER       1     INPUT   number of items to be filled
!     NRFTOT  INTEGER       1     INPUT   record lengt file
!     IPA     INTEGER       1     IN/OUT  pointer in FARRAY
!                                 INPUT   array space IHARM (new version)
!     IPH     INTEGER       1     IN/OUT  pointer in HARMAT
!                                 INPUT   array space HARMAT (new version)
!     IPF     INTEGER       1     IN/OUT  pointer in IHARM
!     IPI     INTEGER       1     IN/OUT  pointer in IPOINT
!     LUNTXT  CHAR*(*)      ?     INPUT   txt with the unit numbers
!     IS      INTEGER       1     INPUT   offset in LUN and LUNTXT
!     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
!     IFFLAG  INTEGER       1     INPUT   = 1 then first invocation
!     UPDATE  LOGICAL       1     OUTPUT  set to T if function is updated
!                                         else set to F
!     NEWSET  LOGICAL       1     INPUT   T if new function processing
!     IOFF    INTEGER       1     LOCAL   offset in the concentration array
!     IWORK   INTEGER       *     LOCAL   workspace
!     LSTREC  LOGICAL       1     INPUT   Switch last record on rewind wanted
!     LREWIN  LOGICAL       1     OUTPUT  Then rewind took place
!     RECLST  REAL          *     OUTPUT  Last record before rewind
!
!     DECLARATIONS        :
!
      use m_srstop
      use m_dhopnf
      use timers
      use delwaq2_data

      integer, intent(in   )           :: ftype  (*) !< type of files to be opened
      type(delwaq_data), intent(inout) :: dlwqd      !< derived type for persistent storage

      DIMENSION     IHARM (*) , HARMAT(*) , FARRAY(*) , IPOINT(*) ,
     *              RESULT(*) , LUN   (*) , IWORK (*) , RECLST(*)
      CHARACTER*(*) LUNTXT(*)
      CHARACTER*12  CHLP
      LOGICAL       UPDATE    , NEWSET    , LSTREC    , LREWIN
!
!     Local
!
      LOGICAL       UPDATH    , UPDATB
      LOGICAL       ONLINE

!     Common to define external communications in SOBEK
!     OLCFWQ             Flag indicating ONLINE running of CF and WQ
!     SRWACT             Flag indicating active data exchange with SRW
!     RTCACT             Flag indicating output for RTC

      LOGICAL            OLCFWQ, SRWACT, RTCACT
      COMMON /COMMUN/    OLCFWQ, SRWACT, RTCACT
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqt1", ithandl )
!
!         Prescribe ONLINE mode for selected files
!
      ONLINE = .FALSE.
      IF ( OLCFWQ .OR. SRWACT )
     JONLINE = ( IS.EQ.11 .OR. IS.EQ.7 .OR. IS.EQ.10 )
!
!         If NRHARM =  0 and NRFTOT= 0, one record per time step,
!                                       no harmonics and interpolation.
      UPDATE = .FALSE.
      NTOTAL = NOSUB*NTOT
      IERR   = 0
      IF ( NRHARM+NRFTOT .GT.  0 ) GOTO 10
      IF ( NTOTAL .GT. 0 ) THEN
         IF ( IFFLAG .EQ. 1 ) THEN
            IF ( .NOT. NEWSET ) THEN
               CALL DHOPNF ( LUN(IS) , LUNTXT(IS) , IS , 2+ftype(is), IERR )
               IF ( IERR .NE. 0 ) THEN
                  WRITE(LUN(19),*) 'ERROR in DLWQT1, opening file'
                  WRITE(LUN(19),*) 'number  :',IS
                  WRITE(LUN(19),*) 'file    :',LUNTXT(IS)
                  WRITE(LUN(19),*) 'unit    :',LUN(IS)
                  CALL SRSTOP(1)
               ENDIF
               READ ( LUN(IS) , IOSTAT = IOERR ) CHLP
               IF ( IOERR.EQ.0 .AND. CHLP(1:6) .EQ. ' 4.900' ) THEN
                  NEWSET = .TRUE.
                  goto 9999        !  RETURN
               ELSE
                  CLOSE ( LUN(IS) )
                  CALL DHOPNF ( LUN(IS) , LUNTXT(IS) , IS , 2+ftype(is), IERR )
               ENDIF
            ELSE
               IPSI = IPA
               IPSA = IPH
               CALL DLWQIB ( LUN(IS) , LUN(19) , HARMAT , IHARM , IS   ,
     *                                           IPA    , IPH   , IERR )
               IPI = IPA - IPSI
               IPA = IPSI
               IPH = IPSA
               CLOSE ( LUN(IS) )
               IF ( IERR .NE. 0 ) THEN
                  WRITE(LUN(19),*) 'ERROR in DLWQT1'
                  WRITE(LUN(19),*) 'after call to DLWQIB'
                  CALL SRSTOP(1)
               ENDIF
            ENDIF
         ENDIF
!
!        If new time setting processing, only take this:
!
         IF ( NEWSET ) THEN
            IPB = IPH
            CALL DLWQTB ( LUN(19), IOFF   , HARMAT , IHARM  , IPA    ,
     *                    IPH    , IPI    , ITIME  , IPOINT , RESULT ,
     *                                               IWORK  , IERR   )
            IF ( IERR .NE. 0 ) THEN
               WRITE(LUN(19),*) 'ERROR in DLWQT1'
               WRITE(LUN(19),*) 'after call to DLWQTB'
               CALL SRSTOP(1)
            ENDIF
            goto 9999        !  RETURN
         ENDIF

         CALL DLWQT2 ( LUN(IS)    , LUN(19) , ITIME  , RESULT , NTOTAL,
     *                 LUNTXT(IS) , ISFLAG  , IFFLAG , ONLINE )
         IF ( IFFLAG .EQ. -1 ) THEN
            NRHARM = -1
            IFFLAG =  1
            CLOSE ( LUN(IS) )
         ENDIF
         UPDATE = .TRUE.
      ELSE
         NRHARM = -1
      ENDIF
      goto 9999    !   return
!
!         first set result zero and evaluate the harmonic components
!
   10 IF ( IFFLAG .EQ. 1 ) THEN
         READ ( LUN(4) ) ( IPOINT(K),K=1,NTOT+3 )
      ENDIF
      DO 20 I = 1 , NTOTAL
         RESULT(I) = 0.0
   20 CONTINUE
!
      I2 =  NRHARM+1
      CALL DLWQT3 ( ITIME  , IHARM  , HARMAT   , HARMAT(I2) , NRHARM ,
     *              NOSUB  , NOSPAC , IPOINT   , NPOINT     , RESULT ,
     *              LUNTXT(3), LUN(3), LUN(19) , ISFLAG     , IFFLAG ,
     *              UPDATH )
      IF ( UPDATH ) UPDATE = .TRUE.
!
      NPOINT = NPOINT + 1
!
!         then evaluate the block- and linear functions
!
      I2 =  NTOT +1
      J2 =  NRFTOT +1
!         5 arguments of integer and real array space removed
!         opening of binary file moved inside DLWQT4         July 2002
      CALL DLWQT4 ( LUN    , LUNTXT , ftype         , LUN(19) , IS     ,
     *              ITIME  , RESULT , IPOINT(NPOINT), NOSUB   , NRFTOT ,
     *              ISFLAG , IFFLAG , UPDATB        , NTOTAL  , LSTREC ,
     *              LREWIN , RECLST , dlwqd         )
      IF ( UPDATB ) UPDATE = .TRUE.
!
!         update the pointers
!
      IPH = IPH + NOSPAC + NRHARM
      IPF = IPF + NRHARM
      IPA = IPA + NRFTOT*2
      IPI = IPI + NTOT + 3
 9999 if ( timon ) call timstop ( ithandl )
      RETURN
      END
