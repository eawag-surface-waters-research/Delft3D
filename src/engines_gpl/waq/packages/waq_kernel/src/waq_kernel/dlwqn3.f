!!  Copyright (C)  Stichting Deltares, 2012-2014.
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

      subroutine dlwqn3 ( a     , j     , c     , lun   , lchar  ,
     &                    action, dlwqd , gridps)

!       Deltares Software Centre

!>\file
!>                         Second order Lax Wendrof in space and time (3)
!>
!>                         Performs time dependent integration according to the
!>                         second order Lax-Wendroff method. The method is explict
!>                         and thus has a time step stability constraint. The method
!>                         is not monotoneous. Method 5 is based on this method but
!>                         remains monotoneous.

C     CREATED            : june 1988 by L. Postma
C
C     LOGICAL UNITS      : LUN(19) , output, monitoring file
C                          LUN(20) , output, formatted dump file
C                          LUN(21) , output, unformatted hist. file
C                          LUN(22) , output, unformatted dump file
C                          LUN(23) , output, unformatted dump file
C
C     SUBROUTINES CALLED : DLWQTR, user transport routine
C                          DLWQWQ, user waterquality routine
C                          PROCES, DELWAQ proces system
C                          DLWQO2, DELWAQ output system
C                          DLWQPP, user postprocessing routine
C                          DLWQT0, sets time functions
C                          DLWQ13, system postpro-dump routine
C                          DLWQ14, scales waterquality
C                          DLWQ15, wasteload routine
C                          DLWQ17, boundary routine
C                          DLWQ18, integration step
C                          DLWQ30, transport routine
C                          PROINT, integration of fluxes
C                          DHOPNF, opens files
C                          ZERCUM, zero's the cummulative array's
C
C     PARAMETERS    :
C
C     NAME    KIND     LENGTH   FUNC.  DESCRIPTION
C     ---------------------------------------------------------
C     A       REAL       *      LOCAL  real      workspace array
C     J       INTEGER    *      LOCAL  integer   workspace array
C     C       CHARACTER  *      LOCAL  character workspace array
C     LUN     INTEGER    *      INPUT  array with unit numbers
C     LCHAR   CHARACTER  *      INPUT  filenames
C
C     Declaration of arguments
C
      use grids
      use timers
      use m_timers_waq
      use m_couplib
      use delwaq2_data
      use waqmem          ! module with the more recently added arrays
      use m_openda_exchange_items, only : get_openda_buffer

      implicit none

      include 'actions.inc'
C
C     Declaration of arguments
C
      REAL, DIMENSION(*)             :: A
      INTEGER, DIMENSION(*)          :: J
      INTEGER, DIMENSION(*)          :: LUN
      CHARACTER*(*), DIMENSION(*)    :: C
      CHARACTER*(*), DIMENSION(*)    :: LCHAR
      INTEGER                        :: ACTION
      TYPE(DELWAQ_DATA), TARGET      :: DLWQD
      type(GridPointerColl)          :: GridPs               ! collection of all grid definitions

C
C     COMMON  /  SYSN   /   System characteristics
C
      INCLUDE 'sysn.inc'
C
C     COMMON  /  SYSI  /    Timer characteristics
C
      INCLUDE 'sysi.inc'
C
C     COMMON  /  SYSA   /   Pointers in real array workspace
C
      INCLUDE 'sysa.inc'
C
C     COMMON  /  SYSJ   /   Pointers in integer array workspace
C
      INCLUDE 'sysj.inc'
C
C     COMMON  /  SYSC   /   Pointers in character array workspace
C
      INCLUDE 'sysc.inc'
C
C     Local declarations
C
      LOGICAL          :: IMFLAG , IDFLAG , IHFLAG , UPDATR
      LOGICAL          :: LDUMMY , LSTREC , LREWIN
      LOGICAL          :: FORESTER
      INTEGER          :: IFFLAG , IAFLAG , IBFLAG , NOWARN
      INTEGER          :: NOSSS  , NOQTT  , NOPRED , ITIMEL
      INTEGER          :: NDDIM  , NVDIM  , LLENG
      INTEGER          :: ITIME
      INTEGER          :: NSTEP

      INTEGER          :: IBND
      INTEGER          :: ISYS
      INTEGER          :: INWTYP
      INTEGER          :: IDTOLD
      INTEGER          :: IDTTOT
      INTEGER          :: NOQT
      INTEGER          :: I
      INTEGER         sindex

      integer          :: ithandl

      !
      ! Dummy variables - used in DLWQD
      !
      integer          :: ioptzb
      real(kind=kind(1.0d0)) :: tol

      INCLUDE 'state_data.inc'

      if ( action == action_finalisation ) then
          include 'dlwqdata_restore.inc'
          goto 20
      endif

      IF ( ACTION == ACTION_INITIALISATION  .OR.
     &     ACTION == ACTION_FULLCOMPUTATION        ) THEN

C
C          some initialisation
C
          ithandl = 0
          ITIME   = ITSTRT
          NSTEP   = (ITSTOP-ITSTRT)/IDT
          IFFLAG  = 0
          IAFLAG  = 0
          IBFLAG  = 0
          IF ( MOD(INTOPT,16) .GE. 8 ) IBFLAG = 1
          LDUMMY  = .FALSE.
          IF ( NDSPN .EQ. 0 ) THEN
             NDDIM = NODISP
          ELSE
             NDDIM = NDSPN
          ENDIF
          IF ( NVELN .EQ. 0 ) THEN
             NVDIM = NOVELO
          ELSE
             NVDIM = NVELN
          ENDIF
          LSTREC = ICFLAG .EQ. 1
          NOQT   = NOQ1+NOQ2
          LLENG  = ILENG+NOQT*2
          FORESTER = BTEST(INTOPT,6)
          NOWARN   = 0
          nosss  = noseg + nseg2
          NOQTT  = NOQ + NOQ4
          inwtyp = intyp + nobnd

          CALL INITIALISE_PROGRESS( DLWQD%PROGRESS, NSTEP, LCHAR(44) )
C
C          initialize second volume array with the first one
C
          CALL MOVE   ( A(IVOL ), A(IVOL2) , NOSEG   )

      ENDIF

C
C     Save/restore the local persistent variables,
C     if the computation is split up in steps
C
C     Note: the handle to the timer (ithandl) needs to be
C     properly initialised and restored
C
      IF ( ACTION == ACTION_INITIALISATION ) THEN
          if ( timon ) call timstrt ( "dlwqn3", ithandl )
          INCLUDE 'dlwqdata_save.inc'
          if ( timon ) call timstop ( ithandl )
          RETURN
      ENDIF

      IF ( ACTION == ACTION_SINGLESTEP ) THEN
          INCLUDE 'dlwqdata_restore.inc'
          call apply_operations( dlwqd )
      ENDIF

      if ( timon ) call timstrt ( "dlwqn3", ithandl )

!======================= simulation loop ============================

   10 continue

!        Determine the volumes and areas that ran dry at start of time step

         call hsurf  ( noseg    , nopa     , c(ipnam) , a(iparm) , nosfun   ,
     &                 c(isfna) , a(isfun) , surface  , lun(19)  )
         call dryfld ( noseg    , nosss    , nolay    , a(ivol)  , noq1+noq2,
     &                 a(iarea) , nocons   , c(icnam) , a(icons) , surface  ,
     &                 j(iknmr) , iknmkv   )

!        user transport processes

         call dlwqtr ( notot    , nosys    , nosss    , noq      , noq1     ,
     &                 noq2     , noq3     , nopa     , nosfun   , nodisp   ,
     &                 novelo   , j(ixpnt) , a(ivol)  , a(iarea) , a(iflow) ,
     &                 a(ileng) , a(iconc) , a(idisp) , a(icons) , a(iparm) ,
     &                 a(ifunc) , a(isfun) , a(idiff) , a(ivelo) , itime    ,
     &                 idt      , c(isnam) , nocons   , nofun    , c(icnam) ,
     &                 c(ipnam) , c(ifnam) , c(isfna) , ldummy   , ilflag   ,
     &                 npartp   )

!jvb  Temporary ? set the variables grid-setting for the DELWAQ variables

         call setset ( lun(19)  , nocons   , nopa     , nofun    , nosfun   ,
     &                 nosys    , notot    , nodisp   , novelo   , nodef    ,
     &                 noloc    , ndspx    , nvelx    , nlocx    , nflux    ,
     &                 nopred   , novar    , nogrid   , j(ivset) )

!        return conc and take-over from previous step or initial condition,
!        and do particle tracking of this step (will be back-coupled next call)

         call delpar01( itime   , noseg    , nolay    , noq      , nosys    ,
     &                  notot   , a(ivol)  , surface  , a(iflow) , c(isnam) ,
     &                  nosfun  , c(isfna) , a(isfun) , a(imass) , a(iconc) ,
     &                  iaflag  , intopt   , ndmps    , j(isdmp) , a(idmps) ,
     &                  a(imas2))

!        call PROCES subsystem

         call proces ( notot    , nosss    , a(iconc) , a(ivol)  , itime    ,
     &                 idt      , a(iderv) , ndmpar   , nproc    , nflux    ,
     &                 j(iipms) , j(insva) , j(iimod) , j(iiflu) , j(iipss) ,
     &                 a(iflux) , a(iflxd) , a(istoc) , ibflag   , ipbloo   ,
     &                 ipchar   , ioffbl   , ioffch   , a(imass) , nosys    ,
     &                 itfact   , a(imas2) , iaflag   , intopt   , a(iflxi) ,
     &                 j(ixpnt) , iknmkv   , noq1     , noq2     , noq3     ,
     &                 noq4     , ndspn    , j(idpnw) , a(idnew) , nodisp   ,
     &                 j(idpnt) , a(idiff) , ndspx    , a(idspx) , a(idsto) ,
     &                 nveln    , j(ivpnw) , a(ivnew) , novelo   , j(ivpnt) ,
     &                 a(ivelo) , nvelx    , a(ivelx) , a(ivsto) , a(idmps) ,
     &                 j(isdmp) , j(ipdmp) , ntdmpq   , a(idefa) , j(ipndt) ,
     &                 j(ipgrd) , j(ipvar) , j(iptyp) , j(ivarr) , j(ividx) ,
     &                 j(ivtda) , j(ivdag) , j(ivtag) , j(ivagg) , j(iapoi) ,
     &                 j(iaknd) , j(iadm1) , j(iadm2) , j(ivset) , j(ignos) ,
     &                 j(igseg) , novar    , a        , nogrid   , ndmps    ,
     &                 c(iprna) , intsrt   , j(iowns) , j(iownq) , mypart   ,
     &                 j(iprvpt), j(iprdon), nrref    , j(ipror) , nodef    ,
     &                 surface  , lun(19)  )

!        communicate boundaries

         call dlwq_boundio ( lun(19)  , notot    , nosys    , nosss    , nobnd    ,
     &                       c(isnam) , c(ibnid) , j(ibpnt) , a(iconc) , a(ibset) ,
     &                       lchar(19))

!        set new boundaries

         if ( itime .ge. 0   ) then
             ! first: adjust boundaries by OpenDA
             if ( dlwqd%inopenda ) then
                 do ibnd = 1,nobnd
                     do isys = 1,nosys
                         call get_openda_buffer(isys,ibnd, 1,1,
     &                                          A(ibset+(ibnd-1)*nosys + isys-1))
                     enddo
                 enddo
             endif
             call dlwq17 ( a(ibset), a(ibsav), j(ibpnt), nobnd   , nosys   ,
     &                     notot   , idt     , a(iconc), a(iflow), a(iboun))
         endif

!     Call OUTPUT system

      CALL DLWQO2 ( NOTOT   , nosss   , NOPA    , NOSFUN  , ITIME   ,
     +              C(IMNAM), C(ISNAM), C(IDNAM), J(IDUMP), NODUMP  ,
     +              A(ICONC), A(ICONS), A(IPARM), A(IFUNC), A(ISFUN),
     +              A(IVOL) , NOCONS  , NOFUN   , IDT     , NOUTP   ,
     +              LCHAR   , LUN     , J(IIOUT), J(IIOPO), A(IRIOB),
     +              C(IONAM), NX      , NY      , J(IGRID), C(IEDIT),
     +              NOSYS   , A(IBOUN), J(ILP)  , A(IMASS), A(IMAS2),
     +              A(ISMAS), NFLUX   , A(IFLXI), ISFLAG  , IAFLAG  ,
     +              IBFLAG  , IMSTRT  , IMSTOP  , IMSTEP  , IDSTRT  ,
     +              IDSTOP  , IDSTEP  , IHSTRT  , IHSTOP  , IHSTEP  ,
     +              IMFLAG  , IDFLAG  , IHFLAG  , NOLOC   , A(IPLOC),
     +              NODEF   , A(IDEFA), ITSTRT  , ITSTOP  , NDMPAR  ,
     +              C(IDANA), NDMPQ   , NDMPS   , J(IQDMP), J(ISDMP),
     +              J(IPDMP), A(IDMPQ), A(IDMPS), A(IFLXD), NTDMPQ  ,
     +              C(ICBUF), NORAAI  , NTRAAQ  , J(IORAA), J(NQRAA),
     +              J(IQRAA), A(ITRRA), C(IRNAM), A(ISTOC), NOGRID  ,
     +              NOVAR   , J(IVARR), J(IVIDX), J(IVTDA), J(IVDAG),
     +              J(IAKND), J(IAPOI), J(IADM1), J(IADM2), J(IVSET),
     +              J(IGNOS), J(IGSEG), A       , NOBND   , NOBTYP  ,
     +              C(IBTYP), J(INTYP), C(ICNAM), noqtt   , J(IXPNT),
     +              INTOPT  , C(IPNAM), C(IFNAM), C(ISFNA), J(IDMPB),
     +              NOWST   , NOWTYP  , C(IWTYP), J(IWAST), J(INWTYP),
     +              A(IWDMP), iknmkv  , J(IOWNS), MYPART  )

!          zero cummulative array's

         if ( imflag .or. ( ihflag .and. noraai .gt. 0 ) ) then
            call zercum ( notot   , nosys   , nflux   , ndmpar  , ndmpq   ,
     &                    ndmps   , a(ismas), a(iflxi), a(imas2), a(iflxd),
     &                    a(idmpq), a(idmps), noraai  , imflag  , ihflag  ,
     &                    a(itrra), ibflag  , nowst   , a(iwdmp))
         endif
         if (mypart.eq.1) call write_progress( dlwqd%progress )

!          simulation done ?

         if ( itime .lt. 0      ) goto 9999
         if ( itime .ge. itstop ) goto 20

!          add processes

         call dlwq14 ( a(iderv), notot   , nosss   , itfact  , a(imas2),
     &                 idt     , iaflag  , a(idmps), intopt  , j(isdmp),
     &                 j(iowns), mypart )

!     get new volumes
         itimel = itime
         itime  = itime + idt
         select case ( ivflag )
            case ( 1 )                 !     computation of volumes for computed volumes only
               call move   ( a(ivol) , a(ivol2), noseg   )
               call dlwqb3 ( a(iarea), a(iflow), a(ivnew), j(ixpnt), notot   ,
     &                       noq     , nvdim   , j(ivpnw), a(ivol2), intopt  ,
     &                       a(imas2), idt     , iaflag  , nosys   , a(idmpq),
     &                       ndmpq   , j(iqdmp))
               updatr = .true.
            case ( 2 )                 !     the fraudulent computation option
               call dlwq41 ( lun     , itime   , itimel  , a(iharm), a(ifarr),
     &                       j(inrha), j(inrh2), j(inrft), noseg   , a(ivoll),
     &                       j(ibulk), lchar   , ftype   , isflag  , ivflag  ,
     &                       updatr  , j(inisp), a(inrsp), j(intyp), j(iwork),
     &                       lstrec  , lrewin  , a(ivol2), mypart  , dlwqd   )
               if ( lrewin ) call move ( a(ivol2), a(ivoll) , noseg   )
               call dlwqf8 ( noseg   , noq     , j(ixpnt), idt     , iknmkv  ,
     &                       a(ivol ), a(iflow), a(ivoll), a(ivol2))
               updatr = .true.
               lrewin = .true.
               lstrec = .true.
            case default               !     read new volumes from files
               call dlwq41 ( lun     , itime   , itimel  , a(iharm), a(ifarr),
     &                       j(inrha), j(inrh2), j(inrft), noseg   , a(ivol2),
     &                       j(ibulk), lchar   , ftype   , isflag  , ivflag  ,
     &                       updatr  , j(inisp), a(inrsp), j(intyp), j(iwork),
     &                       lstrec  , lrewin  , a(ivoll), mypart  , dlwqd   )
         end select

!        update the info on dry volumes with the new volumes

         call dryfle ( noseg    , nosss    , a(ivol2) , nolay    , nocons   ,
     &                 c(icnam) , a(icons) , surface  , j(iknmr) , iknmkv   )

!          add the waste loads

         call dlwq15 ( nosys    , notot    , noseg    , noq      , nowst    ,
     &                 nowtyp   , ndmps    , intopt   , idt      , itime    ,
     &                 iaflag   , c(isnam) , a(iconc) , a(ivol)  , a(ivol2) ,
     &                 a(iflow ), j(ixpnt) , c(iwsid) , c(iwnam) , c(iwtyp) ,
     &                 j(inwtyp), j(iwast) , iwstkind , a(iwste) , a(iderv) ,
     &                 iknmkv   , nopa     , c(ipnam) , a(iparm) , nosfun   ,
     &                 c(isfna ), a(isfun) , j(isdmp) , a(idmps) , a(imas2) ,
     &                 a(iwdmp) , 1        , notot    , j(iowns ), mypart   )

!          do the transport itself

         call dlwq30 ( nosys    , notot    , nosss    , noq1     , noq2     ,
     &                 noq3     , noqtt    , nddim    , nvdim    , a(idisp) ,
     &                 a(idnew) , a(ivnew) , a(iarea) , a(iflow) , a(ileng) ,
     &                 j(ixpnt) , iknmkv   , j(idpnw) , j(ivpnw) , a(iconc) ,
     &                 a(iboun) , intopt   , ilflag   , idt      , a(iderv) ,
     &                 iaflag   , a(imas2) , ndmpq    , j(iqdmp) , a(idmpq) )

!          new time values, except the volumes

         idtold = idt
         call dlwqt0 ( lun      , itime    , itimel   , a(iharm) , a(ifarr) ,
     &                 j(inrha) , j(inrh2) , j(inrft) , idt      , a(ivol)  ,
     &                 a(idiff) , a(iarea) , a(iflow) , a(ivelo) , a(ileng) ,
     &                 a(iwste) , a(ibset) , a(icons) , a(iparm) , a(ifunc) ,
     &                 a(isfun) , j(ibulk) , lchar    , c(ilunt) , ftype    ,
     &                 intsrt   , isflag   , ifflag   , ivflag   , ilflag   ,
     &                 updatr   , j(iktim) , j(iknmr) , j(inisp) , a(inrsp) ,
     &                 j(intyp) , j(iwork) , .false.  , lrewin   , a(ivoll) ,
     &                 .false.  , gridps   , dlwqd    )

!          set a time step

         call dlwq18 ( nosys    , notot    , nototp   , nosss    , a(ivol2) ,
     &                 surface  , a(imass) , a(iconc) , a(iderv) , idtold   ,
     &                 ivflag   , lun(19)  , j(iowns) , mypart   )
C
C       Forester filter on the vertical
C
      IF ( FORESTER ) THEN
         CALL DLWQD2 ( LUN(19) , NOSYS   , NOTOT   , NOSEG   , NOQ3    ,
     *                 KMAX    , A(ICONC), A(LLENG), NOWARN  , J(IOWNS),
     *                 MYPART )
      ENDIF

!     calculate closure error
         if ( lrewin .and. lstrec ) then
            call dlwqce ( a(imass), a(ivoll), a(ivol2), nosys , notot ,
     &                    noseg   , lun(19) )
            call move   ( a(ivoll), a(ivol) , noseg   )
         else
!     replace old by new volumes
            call move   ( a(ivol2), a(ivol) , noseg   )
         endif

!          integrate the fluxes at dump segments fill ASMASS with mass

         if ( ibflag .gt. 0 ) then
            call proint ( nflux   , ndmpar  , idtold  , itfact  , a(iflxd),
     &                    a(iflxi), j(isdmp), j(ipdmp), ntdmpq  )
         endif

!          end of loop

         if ( ACTION == ACTION_FULLCOMPUTATION ) goto 10

   20 continue

      if ( ACTION == ACTION_FINALISATION    .or.
     &     ACTION == ACTION_FULLCOMPUTATION      ) then
          if (mypart.eq.1) then

!            close files, except monitor file

             call CloseHydroFiles( dlwqd%collcoll )
             call close_files( lun )

!            write restart file

             CALL DLWQ13 ( LUN      , LCHAR , A(ICONC) , ITIME , C(IMNAM) ,
     &                     C(ISNAM) , NOTOT , nosss    )
          end if ! mypart.eq.1
      endif

 9999 if ( timon ) call timstop ( ithandl )

      dlwqd%itime = itime

      RETURN
C
      END
