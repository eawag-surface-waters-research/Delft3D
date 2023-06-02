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

      SUBROUTINE GETTME ( FNAME  , ITYPE  , TIMDEF , MAXDEF , IPRDEP ,
     *                    LOCDEP , MAXLST , TIMLST , ITMTYP , NRLST  ,
     *                                               IERROR , OPTION )
!
!
!     Deltares        MARINE & COASTAL MANAGEMENT
!
!     CREATED            : May '96  by L. Postma
!
!     MODIFIED           :
!
!     FUNCTION           : ODS GETTME routine for DELWAQ HIS-files
!
!     SUBROUTINES CALLED :
!
!     LOGICAL UNITS      :
!
!     PARAMETERS    :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     FNAME   CHAR*256   3        IN/LOC  Complete file name
!     ITYPE   INTEGER    1        INPUT   File type
!     TIMDEF  REAL*8   2,MAXDEF   INPUT   Wanted start and stop time
!     MAXDEF  INTEGER    1        INPUT   Wanted time dimension
!     IPRDEP  INTEGER    1        INPUT   Par code for dimensions
!     LOCDEP  INTEGER    1        INPUT   Par code for dimensions
!     MAXLST  INTEGER    1        INPUT   Dimension output arrays
!     TIMLST  REAL*8   MAXLST     OUTPUT  List with times found
!     ITMTYP  INTEGER  MAXLST     OUTPUT  List with time types
!     NRLST   INTEGER    1        OUTPUT  Nr of times found
!     IERROR  INTEGER    1        OUTPUT  Error code
!     OPTION  CHAR*256   1        IN/OUT  For future use
!
!
      use m_julian
      use m_dhucas
      use m_dhopnf
      use m_dhfext

      CHARACTER*256    FNAME (3) , OPTION
      INTEGER          ITMTYP(*)
      DOUBLE PRECISION TIMLST(*) , TIMDEF(2,*) , ATIME , OTIME, SECOND
      LOGICAL          SETALL
!
      REAL, ALLOCATABLE :: RDATA(:)
      character*256         :: ext     ! file extension
      integer               :: extpos  ! position of extension
      integer               :: extlen  ! length of file extension
      logical               :: mapfil  ! true if map file extension
      integer               :: lun
!
!         Open the DELWAQ .HIS file
!
      CALL DHOPNF ( lun , FNAME(1) , 24 , 2 , IERROR )
      IF ( IERROR .NE. 0 ) RETURN

      ! map or his

      call dhfext(fname(1), ext, extpos, extlen)
      call dhucas(ext, ext, extlen)
      if ( ext .eq. 'MAP' ) then
         mapfil = .true.
      else
         mapfil = .false.
      endif
!
!         Read primary system characteristics
!
      READ ( lun , ERR=100 )   FNAME(3)(1:160)
      IF ( FNAME(3)(121:123) .NE. 'T0: ' .AND.
     *     FNAME(3)(121:123) .NE. 't0: ' .AND.
     *     FNAME(3)(121:123) .NE. 'T0= ' .AND.
     *     FNAME(3)(121:123) .NE. 't0= '       ) THEN
         GOTO 150
      ENDIF
      READ ( FNAME(3)(125:128) , '(I4)' ) IYEAR
      READ ( FNAME(3)(130:131) , '(I2)' ) IMONTH
      READ ( FNAME(3)(133:134) , '(I2)' ) IDAY
      READ ( FNAME(3)(136:137) , '(I2)' ) IHOUR
      READ ( FNAME(3)(139:140) , '(I2)' ) IMINUT
      READ ( FNAME(3)(142:143) , '(I2)' ) ISECND
      READ ( FNAME(3)(151:158) , '(I8)' ) ISFACT
      READ ( lun , ERR=110 )   NOTOT, NODUMP
      READ ( lun , ERR=120 ) ( FNAME(3)(181:200) , K = 1,NOTOT )
      if ( .not. mapfil ) then
         READ ( lun , ERR=130 ) ( IDUMMY, FNAME(3)(221:240) , K = 1,NODUMP )
      endif
      IDATE  = IYEAR*10000+IMONTH*100+IDAY
      ITIME  = IHOUR*10000+IMINUT*100+ISECND
      OTIME  = JULIAN ( IDATE , ITIME )
      SECOND = 1/864.00D+02
!
!         Read the values at all times
!
      NTT   = NODUMP*NOTOT
      ALLOCATE(RDATA(NTT))
      NRLST = 0
      SETALL = .FALSE.
      IF ( TIMDEF(1,1) .LT. 0.5 ) SETALL = .TRUE.

   10 READ ( lun , ERR=140 , END=200 ) IDUMMY, ( RDATA(K), K=1,NTT )
      DO 20 I = 1 , MAXDEF
         ATIME = OTIME + IDUMMY*ISFACT*SECOND
         IF ( (ATIME.GT.TIMDEF(1,I) .AND. ATIME.LT.TIMDEF(2,I)) .OR.
     *                                             SETALL    ) THEN
            NRLST = NRLST + 1
            IF ( NRLST .GT. MAXLST ) GOTO 160
            TIMLST(NRLST) = ATIME
            ITMTYP(NRLST) = 2
            GOTO 10
         ENDIF
   20 CONTINUE
      GOTO 10
!
!         Error messages
!
  100 IERROR = 10
      GOTO 200
  110 IERROR = 11
      GOTO 200
  120 IERROR = 12
      GOTO 200
  130 IERROR = 13
      GOTO 200
  140 IERROR = 14
      GOTO 200
  150 IERROR = 15
      GOTO 200
  160 IERROR = 16
!
  200 CLOSE ( lun )
      IF (ALLOCATED(RDATA)) DEALLOCATE(RDATA)
      RETURN
!
      END
