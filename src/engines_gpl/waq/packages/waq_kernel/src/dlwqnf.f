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

      subroutine dlwqnf ( a     , j     , c     , lun   , lchar  ,
     &                    action, dlwqd , gridps)

!       Deltares Software Centre

!>\file
!>                         Upwind discretisation, implicit in time, GMRES solver (15)
!>
!>                         Performs time dependent integration. Upwind advection, implicit
!>                         in time. Uses the GMRES iterative solver with Krilov
!>                         orthonormal subspaces vectors.\n

!     Created            : April     1992 by Jos van Gils

!     Modified           : September 1996 Leo Postma Fast solver data structure
!                             up to       Kian Tan   Fast solver GMRES method
!                          November  1996 Robert Vos Fast solver enhancements
!                          July      2009 Leo Postma Double precission version
!                          August    2010 Leo Postma OMP parallel version

!     Logical units      : lun(19) , output, monitoring file
!                          lun(20) , output, formatted dump file
!                          lun(21) , output, unformatted hist. file
!                          lun(22) , output, unformatted dump file
!                          lun(23) , output, unformatted dump file

!     Subroutines called in their order of appearance in the code:
!                          dlwqf5: sets the numerical parameters for GMRES and reports to monitoring file
!                          move  : copies one array to another (usefull for ranges in the big array)
!                          dlwqf1: initializes matrix pointer administration fast solver
!     VVV                  dryfld: changes the property array for drying and flooding of start volume
!      V                   dlwqtr: user transport routine (a value in delwaq2 dir. is used for reasons)
!      |                   setset: not so clear what this does, probably process related
!      |                   hsurf : set the surface array from the proces parameters
!      |                   proces: DELWAQ water quality process system
!      |                   dlwq_boundio: interface to on line boundary provision in bigger systems
!      |                   dlwqo2: DELWAQ output system, provides all output to files
!    time  ===> jump out   zercum: zero's the cummulative array's of balances and monitoring areas
!    loop        point     dlwqb8: restores conc array (in case other routines did distroy ?!?)
!      |                   dlwq14: scales waterquality derivative according to right time step size
!      |                   dlwqb3: computes new volumes in case 'computed volumes' was switched 'on'
!      |                   dlwq41: updates new volumes from file/core in case 'comp vols' was 'off'
!      |                   dryfle: does the same as dryfld now taking into account the last volumes
!      |                   dlwq15: updates derivative with all specified wasteload forcing
!      |                   dlwqm7: makes mixleng once for all substances based on area/(leng1+leng2)
!      |            V      dlwqm0: makes flowtot and disptot for the substance in the substance loop
!      |      paralellized dlwqf3: fills the sparse transport matrix, scales with diagonal value
!      |       substances  dlwqf4: sets the (scaled) right-hand-side of the equations
!      |          loop     sgmres: solves (iteratively) the system of equations
!      |            V      dlwqf7: copies solution in the conc. array updates mass balance arrays
!      |                   dlwqb4: update mass arrays, set explicit step for all passive substances
!      |                   dlwqce: computes closure error correction at rewind of volume file
!      |                   proint: integration of fluxes for mass balances per monitoring area
!      |                   rtcshl: call to the real-time-control interface if switched 'on'
!     VVV                  srwshl: call to the standaard-raamwerk-water interface if switched 'on'
!      V                   dlwqt0: updates all time dependent items (in this case exclusive of volumes)
!                          dlwq13: system dump routine of restart files at the end
!   Not mentioned are the routines: to start and stop the performance timers
!                                   the routines used for stepwise execution within a
!                                   stepwise executing user interface

      use m_move
      use m_fileutils
      use grids
      use timers
      use waqmem                         ! Global memory with allocatable GMRES arrays
      use delwaq2_data
      use m_openda_exchange_items, only : get_openda_buffer
      use report_progress
      use m_actions
      use m_sysn          ! System characteristics
      use m_sysi          ! Timer characteristics
      use m_sysa          ! Pointers in real array workspace
      use m_sysj          ! Pointers in integer array workspace
      use m_sysc          ! Pointers in character array workspace
      use m_dlwqdata_save_restore

      implicit none


!     Arguments:

!     Kind                           Name         Description

      real                        :: a     (*)  !< real linear workspace array
      integer                     :: j     (*)  !< integer linear workspace array
      integer                     :: lun   (*)  !< file unit numbers
      character(*)                :: c     (*)  !< character linear workspace array
      character(*)                :: lchar (*)  !< file names
      integer                     :: action     !< handle to stepwise call
      type(delwaq_data), target   :: dlwqd      !< data structure stepwize call
      type(gridpointercoll)       :: gridps     !< collection off all grid definitions

!$    include "omp_lib.h"



!     Common to define external communications in SOBEK
!     olcfwq             Flag indicating ONLINE running of CF and WQ
!     srwact             Flag indicating active data exchange with SRW
!     rtcact             Flag indicating output for RTC

      logical            olcfwq, srwact, rtcact
      common /commun/    olcfwq, srwact, rtcact

!     Local declarations

      real            rdummy(1)
      logical         imflag , idflag , ihflag
      logical         update , lrewin
      logical         timon_old
      integer         laatst
      INTEGER         sindex

      integer, save :: ithand1 = 0 ! Leave local

      integer         isys
      integer         nstep

      integer         iseg
      integer         ibnd

      integer         noth
      integer         ith

!       Variables specific to this method: leave them SAVEd

      integer, save          :: ioptpc
      integer, save          :: iter
      integer, save          :: iscale




!     SPECIAL REMARKS    : MASS-ARRAY IS USED FOR RHS VECTOR!!
!
!     This option is a mix of option 1 (discretization of transport
!     in space) and option 6 (matrix inversion to perform implicit
!     integration in time.
!     The processes part is integrated EXPLICITLY, in order to allow
!     for any complexity of the processes.
!     Strictly speaking, a loop over substances should be added
!     To anticipate this, the method uses an
!     extra VOLUME-array (IVOL2), and uses the AMASS-array (IMASS)
!     for the rhs-matrix, instead of the DERIV-array as in method 6.
!     (JvG, May 8 1992)
!
!     This option implements:
!     1) Euler backward time integration
!     2) upwind differences for advection
!     3) central differences for diffusion
!     The resulting systems of equations are solved by an iterative
!     solution method (GMRES).
!     With such an iterative method, systems with multiple rhs cannot be solved
!     (simultaneously). So we loop over the substances and solve each system
!     individually. So RHS can be reduced to an REAL array of size NOSEG+NOBND.
!
!     possible improvements:
!
!     - Use FGMRES instead of GMRES for solving system of equations.
!       This makes it possible to keep search directions which have already
!       been computed in previous FGMRES calls and hence find the solution
!       of new systems at lower costs!
!
!     - Tune the preconditioner to speed up the iteration process.
!       Only Gaus-Seidel, and SSOR preconditioning has been implemented yet.
!
!     - Integrate processes in an implicit way as well. Enables users to
!       potentially take larger time steps (better stability properties)
!       or even compute steady states in "one time step" (the latter subject
!       to constraint that proces formulation is time independent).
!       Implicit time integration of processes requires the Inexact Newton
!       solution method described in:
!
!       "DELWAQ FASTSOLVER II"
!       Newton-Krylov methods for solving linear and non-linear equations
!       report T1596, January 1996, Deltares
!                                                              (KHT, 13/11/96)

      if ( action == ACTION_FINALISATION ) then
          call dlwqdata_restore(dlwqd)
          if ( timon ) call timstrt ( "dlwqnf", ithandl )
          goto 50
      endif

      if ( action == ACTION_INITIALISATION  .or.
     &     action == ACTION_FULLCOMPUTATION        ) then

!
!        some initialisation
!        IOPTPC = preconditioner switch [0 = none, 1 = GS (L), 2 = GS (U),
!        3 = SSOR], ITER = maximum number of iterations [ > 0],
!        TOL = relative tolerance [10^-3, 10^-10], ISCALE = row scaling
!        of system of equations [0 = no, 1 =yes], KLAT = number of
!        layers in preconditioner [1,KMAX]
!
          call dlwqf5 ( lun(19) , nocons  , c(icnam), a(icons), ioptpc  ,
     &                  iter    , tol     , iscale  , litrep  , noseg   ,
     &                  noq3    , noq     , nobnd   , novec   , nomat   ,
     &                  nolay   , intsrt  , intopt  )

          ithandl = 0
          itime   = itstrt
          nstep   = (itstop-itstrt)/idt
          ifflag  = 0
          iaflag  = 0
          ibflag  = 0
          if ( mod(intopt,16) .ge. 8 ) ibflag = 1
          if ( ndspn .eq. 0 ) then
             nddim = nodisp
          else
             nddim = ndspn
          endif
          if ( nveln .eq. 0 ) then
             nvdim = novelo
          else
             nvdim = nveln
          endif
          lstrec   = icflag .eq. 1
          nosss    = noseg + nseg2
          noqtt    = noq   + noq4
          inwtyp   = intyp + nobnd
          noqt     = noq1  + noq2

          call initialise_progress( dlwqd%progress, nstep, lchar(44) )

!          initialize second volume array with the first one

          call move   ( a(ivol ), a(ivol2) , nosss   )
      endif

!
!     Save/restore the local persistent variables,
!     if the computation is split up in steps
!
!     Note: the handle to the timer (ithandl) needs to be
!     properly initialised and restored
!
      if ( action == ACTION_INITIALISATION ) then
          if ( timon ) call timstrt ( "dlwqnf", ithandl )
          call dlwqdata_save(dlwqd)
          if ( timon ) call timstop ( ithandl )
          return
      endif

      if ( action == ACTION_SINGLESTEP ) then
          call dlwqdata_restore(dlwqd)
          call apply_operations( dlwqd )
      endif

      if ( timon ) call timstrt ( "dlwqnf", ithandl )

      iexseg = 1     !  There is nothing to mask.

!======================= simulation loop ============================

   10 continue

!        Determine the volumes and areas that ran dry at start of time step

         call hsurf  ( noseg    , nopa     , c(ipnam) , a(iparm) , nosfun   ,
     &                 c(isfna) , a(isfun) , surface  , lun(19)  )
         call dryfld ( noseg    , nosss    , nolay    , a(ivol)  , noq1+noq2,
     &                 a(iarea) , nocons   , c(icnam) , a(icons) , surface  ,
     &                 j(iknmr) , iknmkv   )

!        mt3d coupling

         call dlwq_mt3d   ( lun  (19), itime   , idt     , itstop  , notot   ,
     &                      nosys    , nosss   , nobnd   , c(isnam), c(ibnid),
     &                      j(ibpnt) , a(iconc), a(ibset), noqtt   , j(ixpnt),
     &                      a(iflow) , ndmpq   , j(iqdmp), a(idmpq))

!        user transport processes
         update = updatr
         call dlwqtr ( notot    , nosys    , noseg    , noq      , noq1     ,
     &                 noq2     , noq3     , nopa     , nosfun   , nodisp   ,
     &                 novelo   , j(ixpnt) , a(ivol)  , a(iarea) , a(iflow) ,
     &                 a(ileng) , a(iconc) , a(idisp) , a(icons) , a(iparm) ,
     &                 a(ifunc) , a(isfun) , a(idiff) , a(ivelo) , itime    ,
     &                 idt      , c(isnam) , nocons   , nofun    , c(icnam) ,
     &                 c(ipnam) , c(ifnam) , c(isfna) , update   , ilflag   )
         if ( update ) updatr = .true.

!jvb     Temporary ? set the variables grid-setting for the DELWAQ variables

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
     &                 c(iprna) , intsrt   ,
     &                 j(iprvpt), j(iprdon), nrref    , j(ipror) , nodef    ,
     &                 surface  , lun(19)  )



!          communicate with flow

         call waq2flow ( nrvart   , c(ionam) , j(iiopo) , nocons   , nopa     ,
     &                   nofun    , nosfun   , notot    , a(iconc) , a(isfun) ,
     &                   a(ifunc) , a(iparm) , a(icons) , idt      , itime    ,
     &                   a(ivol)  , noseg    , nosys    , nodump   , j(idump) ,
     &                   nx       , ny       , j(igrid) , a(iboun) , noloc    ,
     &                   a(iploc) , nodef    , a(idefa) , lun(19)  )


!     communicate boundaries
         call dlwq_boundio( lun  (19), notot   , nosys   , noseg   , nobnd   ,
     &                      c(isnam) , c(ibnid), j(ibpnt), a(iconc), a(ibset),
     &                      lchar(19))

!     set new boundaries
         if ( itime .ge. 0   ) then
              ! first: adjust boundaries by OpenDA
              if ( dlwqd%inopenda ) then
                  do ibnd = 1,nobnd
                      do isys = 1,nosys
                          call get_openda_buffer(isys,ibnd, 1,1,
     &                                    A(ibset+(ibnd-1)*nosys + isys-1))
                      enddo
                  enddo
              endif
             call dlwq17 ( a(ibset), a(ibsav), j(ibpnt), nobnd   , nosys   ,
     &                     notot   , idt     , a(iconc), a(iflow), a(iboun))
         endif

!     call output system
         call dlwqo2 ( notot   , noseg   , nopa    , nosfun  , itime   ,
     &                 c(imnam), c(isnam), c(idnam), j(idump), nodump  ,
     &                 a(iconc), a(icons), a(iparm), a(ifunc), a(isfun),
     &                 a(ivol) , nocons  , nofun   , idt     , noutp   ,
     &                 lchar   , lun     , j(iiout), j(iiopo), a(iriob),
     &                 c(iosnm), c(iouni), c(iodsc), c(issnm), c(isuni), c(isdsc), 
     &                 c(ionam), nx      , ny      , j(igrid), c(iedit),
     &                 nosys   , a(iboun), j(ilp)  , a(imass), a(imas2),
     &                 a(ismas), nflux   , a(iflxi), isflag  , iaflag  ,
     &                 ibflag  , imstrt  , imstop  , imstep  , idstrt  ,
     &                 idstop  , idstep  , ihstrt  , ihstop  , ihstep  ,
     &                 imflag  , idflag  , ihflag  , noloc   , a(iploc),
     &                 nodef   , a(idefa), itstrt  , itstop  , ndmpar  ,
     &                 c(idana), ndmpq   , ndmps   , j(iqdmp), j(isdmp),
     &                 j(ipdmp), a(idmpq), a(idmps), a(iflxd), ntdmpq  ,
     &                 c(icbuf), noraai  , ntraaq  , j(ioraa), j(nqraa),
     &                 j(iqraa), a(itrra), c(irnam), a(istoc), nogrid  ,
     &                 novar   , j(ivarr), j(ividx), j(ivtda), j(ivdag),
     &                 j(iaknd), j(iapoi), j(iadm1), j(iadm2), j(ivset),
     &                 j(ignos), j(igseg), a       , nobnd   , nobtyp  ,
     &                 c(ibtyp), j(intyp), c(icnam), noq     , j(ixpnt),
     &                 intopt  , c(ipnam), c(ifnam), c(isfna), j(idmpb),
     &                 nowst   , nowtyp  , c(iwtyp), j(iwast), j(inwtyp),
     &                 a(iwdmp), iknmkv  , isegcol )

!        zero cumulative arrays

         if ( imflag .or. ( ihflag .and. noraai .gt. 0 ) ) then
            call zercum ( notot   , nosys   , nflux   , ndmpar  , ndmpq   ,
     &                    ndmps   , a(ismas), a(iflxi), a(imas2), a(iflxd),
     &                    a(idmpq), a(idmps), noraai  , imflag  , ihflag  ,
     &                    a(itrra), ibflag  , nowst   , a(iwdmp))
         endif
         call write_progress( dlwqd%progress )

!        simulation done ?

         if ( itime .lt. 0      ) goto 9999
         if ( itime .ge. itstop ) goto 50

!        restore conc-array from mass array

         call dlwqb8 ( nosys    , notot    , nototp   , noseg    , a(ivol ) ,
     &                 surface  , a(imass) , a(iconc) )

!        add processes

         call dlwq14 ( a(iderv), notot   , noseg   , itfact  , a(imas2),
     &                 idt     , iaflag  , a(idmps), intopt  , j(isdmp))

!        get new volumes

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
     &                       lstrec  , lrewin  , a(ivol2), dlwqd   )
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
     &                       lstrec  , lrewin  , a(ivoll), dlwqd   )
         end select

!        Update the info on dry volumes with the new volumes       ( dryfle )
!        Compute new from-topointer on the basis of non-zeroflows  ( zflows )
!        Initialize pointer matices for fast solvers               ( dlwqf1 )

         call dryfle ( noseg    , nosss    , a(ivol2) , nolay    , nocons   ,
     &                 c(icnam) , a(icons) , surface  , j(iknmr) , iknmkv   )
         call zflows ( noq      , noqt     , nolay    , nocons   , c(icnam) ,
     &                 a(iflow) , j(ixpnt) )
         call dlwqf1 ( noseg    , nobnd    , noq      , noq1     , noq2     ,
     &                 nomat    , j(ixpnt) , j(iwrk)  , j(imat)  , rowpnt   ,
     &                 fmat     , tmat     )
         iexseg = 1
         do iseg = 1, noseg
            if ( iknmkv(iseg,1) .eq. 0 ) iexseg(iseg,1) = 0
         enddo

!        add the waste loads

         call dlwq15 ( nosys    , notot    , noseg    , noq      , nowst    ,
     &                 nowtyp   , ndmps    , intopt   , idt      , itime    ,
     &                 iaflag   , c(isnam) , a(iconc) , a(ivol)  , a(ivol2) ,
     &                 a(iflow ), j(ixpnt) , c(iwsid) , c(iwnam) , c(iwtyp) ,
     &                 j(inwtyp), j(iwast) , iwstkind , a(iwste) , a(iderv) ,
     &                 iknmkv   , nopa     , c(ipnam) , a(iparm) , nosfun   ,
     &                 c(isfna ), a(isfun) , j(isdmp) , a(idmps) , a(imas2) ,
     &                 a(iwdmp) , 1        , notot     )

!          Here we implement a loop that inverts the same matrix
!          for series of subsequent substances having the same
!          additional VELO and DISPER array. (JvG, April 24, 1993).

!          In solving equations with multiple rhs, the *FULL* fast (or should
!          I say slow?) solver algorithm needs to be applied to each rhs vector
!          So DELMAT may outperform FS when we deal with a large number of
!          substances with the same additional velocity and dispersion field
!          In future we need a smart switch between DELMAT and FS at this place
!          For now always do FS!
!                                                               (KHT, 11/11/96)

         call dlwqm7 ( noq     , noq1    , noq2    , a(iarea), a(iflow),
     &                 a(ileng), ilflag  , intopt  , j(ixpnt), mixlen  ,
     &                 iknmkv  )

         if ( timon ) call timstrt ( "ADE solver", ithand1 )
         timon_old = timon
         noth = OMP_GET_MAX_THREADS()
         if ( noth .gt. 1 ) timon = .false.

!$OMP PARALLEL
!$OMP DO PRIVATE(ith) SCHEDULE(DYNAMIC)
! start of loop over substances

      do 40 isys = 1, nosys

         ith = OMP_GET_THREAD_NUM()+1

!     make flow and dispersion arrays
         call dlwqm0 ( isys          , nosys         , noq           , noq1          , noq2          ,
     &                 a(iarea)      , a(iflow)      , flowtot(1,ith), nvdim         , j(ivpnw)      ,
     &                 a(ivnew)      , a(idisp)      , disptot(1,ith), nddim         , j(idpnw)      ,
     &                 a(idnew)      , mixlen        )

!     do the transport itself, fill matrix, scale diagonal
         call dlwqf3 ( idt           , noseg         , a(ivol2)      , nobnd         , noq           ,
     &                 j(ixpnt)      , flowtot(1,ith), disptot(1,ith), gm_diag(1,ith), iscale        ,
     &                 gm_diac(1,ith), nomat         , gm_amat(1,ith), rowpnt        , fmat          ,
     &                 tmat          )

!     compute RHS (substance after substance)
         call dlwqf4 ( noseg         , nobnd         , nosys         , notot         , isys          ,
     &                 idt           , a(iconc)      , a(iderv)      , a(ivol)       , a(iboun)      ,
     &                 gm_rhs(1,ith) , gm_diac(1,ith), gm_sol(1,ith) )

!     solve linear system of equations
         call sgmres ( noseg+nobnd   , gm_rhs (1,ith), gm_sol (1,ith), novec         , gm_work(1,ith),
     &                 noseg+nobnd   , gm_hess(1,ith), novec+1       , iter          , tol           ,
     &                 nomat         , gm_amat(1,ith), j(imat)       , gm_diag(1,ith), rowpnt        ,
     &                 nolay         , ioptpc        , nobnd         , gm_trid(1,ith), iexseg (1,ith),
     &                 lun(19)       , litrep        )

!     mass balance of transport and copy of solution in the concentration array
         call dlwqf7 ( isys          , nosys         , notot         , noseg         , a(iconc)      ,
     &                 gm_sol (1,ith), nobnd         , a(iboun)      , noq           , j(ixpnt)      ,
     &                 flowtot(1,ith), disptot(1,ith), a(imas2)      , ndmpq         , j(iqdmp)      ,
     &                 a(idmpq)      , iknmkv        , idt           )

!        end loop over the substances

   40 continue

!$OMP ENDDO
!$OMP ENDPARALLEL
      if ( noth .gt. 1 ) timon = timon_old
      if ( timon ) call timstop ( ithand1 )

!        update mass array, explicit step for passive substances

         call dlwqb4 ( nosys   , notot   , nototp  , noseg   , a(ivol2),
     &                 surface , a(imass), a(iconc), a(iderv), idt     )

!     calculate closure error
         if ( lrewin .and. lstrec ) then
            call dlwqce ( a(imass), a(ivoll), a(ivol2), nosys , notot ,
     &                    noseg   , lun(19) )
            call move   ( a(ivoll), a(ivol) , noseg   )
         else
!     replace old by new volumes
            call move   ( a(ivol2), a(ivol) , noseg   )
         endif

!     integrate the fluxes at dump segments fill asmass with mass
         if ( ibflag .gt. 0 ) then
            call proint ( nflux   , ndmpar  , idt     , itfact  , a(iflxd),
     &                    a(iflxi), j(isdmp), j(ipdmp), ntdmpq  )
         endif

         if ( rtcact ) call rtcshl (itime, a, j, c) ! Interface to RTC (i)
         if ( srwact ) call srwshl (itime, a, j, c) ! Interface to SRW (i)

         if ( olcfwq ) then
            call putpcf('wqtocf','datawqtocf')
            if ( itime+idt .lt. itstop ) then
               call getpcf('cftowq','datacftowq')
               laatst = 0
            else
               laatst = -1
            endif
         endif

!     new time values, volumes excluded
         if ( olcfwq .or. srwact ) then
            call putpev ( 'WQtoWQI', 'DataWQtoWQI', laatst )
            call getper ( 'WQItoWQ', 'DataWQItoWQ' )
         endif

!     update all other time functions
         call dlwqt0 ( lun     , itime   , itimel  , a(iharm), a(ifarr),
     &                 j(inrha), j(inrh2), j(inrft), idt     , a(ivol) ,
     &                 a(idiff), a(iarea), a(iflow), a(ivelo), a(ileng),
     &                 a(iwste), a(ibset), a(icons), a(iparm), a(ifunc),
     &                 a(isfun), j(ibulk), lchar   , c(ilunt), ftype   ,
     &                 intsrt  , isflag  , ifflag  , ivflag  , ilflag  ,
     &                 update  , j(iktim), j(iknmr), j(inisp), a(inrsp),
     &                 j(intyp), j(iwork), .false. , ldummy  , rdummy  ,
     &                 .false. , gridps  , dlwqd   )
         if ( update ) updatr = .true.

!     end of time loop
         if ( action == ACTION_FULLCOMPUTATION ) then
            goto 10
         endif

   50 continue

      if ( action == ACTION_FINALISATION    .or.
     &     action == ACTION_FULLCOMPUTATION      ) then

!     close files, except monitor file
         call CloseHydroFiles( dlwqd%collcoll )
         call close_files( lun )

!     write restart file
         call dlwq13 ( lun      , lchar , a(iconc) , itime , c(imnam) ,
     *                 c(isnam) , notot , noseg    )
      endif

 9999 if ( timon ) call timstop ( ithandl )

      dlwqd%iaflag = iaflag
      dlwqd%itime = itime

      return

      end subroutine dlwqnf
