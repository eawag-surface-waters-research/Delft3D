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

      subroutine dlwqp1 ( lun          , lchar        ,
     +                    statprocesdef, allitems     ,
     +                    ioutps       , outputs      ,
     +                    nomult       , imultp       ,
     +                    constants    , noinfo       ,
     +                    refday       ,
     +                    nowarn       , ierr         )

!       Deltares Software Centre

!>\file
!>                          Defines process steering for all water quality processing
!>
!>                          This routine processes all information of
!>                             - processes that have been switched on
!>                             - constants and functions that have been supplied
!>                             - output variables that have been asked to become available
!>                             .
!>                          to a consistent set of sequential processes for the simulation part

      use m_zoek
      use m_startup_screen
      use m_srstop
      use m_rdwrk4
      use m_monsys
      use m_getcom
      use m_dhopnf
      use timers       !   performance timers
      use dlwq_data
      use processet
      use output
      use partable
      use string_module
      use m_sysn          ! System characteristics
      use m_sysi          ! Timer characteristics

      implicit none

      ! declaration of arguments

      integer             , intent(inout) :: lun(*)          !< unit numbers
      character(len=*)    , intent(inout) :: lchar(*)        !< filenames
      type(procespropcoll), intent(in   ) :: statprocesdef   !< the statistical proces definition
      type(itempropcoll)  , intent(in   ) :: allitems        !< all items of the proces system
      integer             , intent(inout) :: ioutps(7,*)     !< (old) output structure
      type(outputcoll)    , intent(inout) :: outputs         !< output structure
      integer  ( 4)       , intent(in   ) :: nomult          !< number of multiple substances
      integer  ( 4)       , intent(in   ) :: imultp(2,nomult)!< multiple substance administration
      type(t_dlwq_item)   , intent(inout) :: constants       !< delwaq constants list
      integer             , intent(inout) :: noinfo          !< count of informative message
      integer  ( 4)       , intent(in)    :: refday          !< reference day, varying from 1 till 365
      integer             , intent(inout) :: nowarn          !< count of warnings
      integer             , intent(inout) :: ierr            !< error count

      ! local declarations

      real, parameter           :: versip = 5.07   ! version process system
      real                      :: verspe = 1.0    ! version bloom.spe file
      integer, parameter        :: novarm = 15000  ! max number of variables overall
      integer, parameter        :: nbprm  = 1750   ! max number of processes
      integer, parameter        :: nopred = 6      ! number of pre-defined variables

      integer                   :: noqtt           ! total number of exhanges
      integer                   :: nosss           ! total number of segments
      integer                   :: no_in           ! number of input items
      integer                   :: no_out          ! number of output items
      integer                   :: no_ins          ! number of output items
      integer                   :: no_ine          ! number of output items
      integer                   :: no_ous          ! number of output items
      integer                   :: no_oue          ! number of output items
      integer                   :: no_flu          ! number of output items
      integer                   :: no_sto          ! number of output items
      integer                   :: no_dis          ! number of output items
      integer                   :: no_vel          ! number of output items
      integer                   :: noconm          ! number of constants plus some extra max
      integer                   :: nocon2          ! number of constants plus some extra
      integer                   :: nmis            ! number of missing items
      integer                   :: maxdef          ! length defaul array

      integer                   :: lurep           ! unit number report file
      integer                   :: lunblm          ! unit number bloom file
      integer                   :: lunfrm          ! unit number bloom frm file
      integer                   :: lund09          ! unit number bloom d09 file
      integer                   :: mlevel          ! monitoring level

      integer                   :: isys            ! index variable
      integer                   :: igrp            ! index variable
      integer                   :: iatyp           ! index variable
      integer                   :: ialg            ! index variable
      integer                   :: icof            ! index variable
      integer                   :: ico             ! index variable
      integer                   :: iconf           ! index variable
      integer                   :: istat           ! index variable
      integer                   :: iioitem         ! index variable
      integer                   :: ioutp           ! index variable
      integer                   :: i               ! index variable
      integer                   :: iitem           ! index variable
      integer                   :: iindx           ! index variable
      integer                   :: ix_act          ! index variable
      integer                   :: ix_dbl          ! index variable
      integer                   :: ioff            ! offset for index item
      integer                   :: ioffx           ! offset for index item on exchange
      integer                   :: idef            ! offset to defualt items
      integer                   :: iflx            ! offset to flux items
      integer                   :: iret            ! return value
      integer                   :: ierr2           ! error count
      integer                   :: ierr_alloc      ! error
      integer                   :: ierr_dalloc     ! error

      integer                   :: idummy          ! dummy variable
      real                      :: rdummy          ! dummy variable
      character                 :: cdummy          ! dummy variable

      integer     ,allocatable  :: idpnt(:)        ! dispersion pointers
      integer     ,allocatable  :: ivpnt(:)        ! velocity pointers
      integer     ,allocatable  :: grdref(:)       ! reference grid
      integer     ,allocatable  :: sysgrd(:)       ! substance grid
      integer     ,allocatable  :: sysndt(:)       ! substance timestep multiplier

      character*40              :: modid (4)       ! model id
      character*20,allocatable  :: syname(:)       ! substance names
      character*20,allocatable  :: coname(:)       ! constant names
      character*20,allocatable  :: paname(:)       ! parameter names
      character*20,allocatable  :: funame(:)       ! function names
      character*20,allocatable  :: sfname(:)       ! segm.func. names
      character*20,allocatable  :: diname(:)       ! dispersion names
      character*20,allocatable  :: vename(:)       ! velocity names
      character*20,allocatable  :: dename(:)       ! default array names
      character*20,allocatable  :: locnam(:)       ! local array names
      character*20 ,allocatable :: ainame(:)       ! all item names names in the proc_def
      character*20              :: subname         ! substance name
      character*100,allocatable :: substdname(:)   ! substance standard name
      character*40 ,allocatable :: subunit(:)      ! substance unit
      character*60 ,allocatable :: subdescr(:)     ! substance description
      character*20              :: outname         ! output name


      ! proces definition structure

      type(procespropcoll)      :: procesdef       ! the complete process definition
      integer                   :: nbpr            ! number of processes
      integer                   :: no_act          ! number of activated processes
      integer                   :: serial          ! serial number process definition
      integer                   :: target_serial   ! target serial number process definition
      real                      :: versio          ! version process defintion
      character*20 , allocatable :: actlst(:)

      ! proces "output" structure

      integer      , pointer :: idpnw(:)
      integer      , pointer :: ivpnw(:)
      real         , pointer :: defaul(:)
      real         , pointer :: dsto(:)
      real         , pointer :: vsto(:)

      ! settings

      character*80   swinam
      character*80   blmnam
      character*80   line
      character*256  pdffil
      character*10   config
      logical        lfound, laswi , swi_nopro
      integer        blm_act                       ! index of ACTIVE_BLOOM_P

      ! charon coupling

      type(procesprop)      :: procha              ! charon (extra) process definition
      character*256  chemid
      logical        l_chem

      ! information

      character*20   rundat
      logical        ex

      ! bloom-species database

      character*256 blmfil
      logical        l_eco
      integer       maxtyp, maxcof
      parameter   ( maxtyp = 500 , maxcof = 50 )
      integer       notyp , nocof , nogrp
      character*10  alggrp(maxtyp), algtyp(maxtyp)
      character*5   abrgrp(maxtyp), abrtyp(maxtyp)
      character*80  algdsc(maxtyp)
      character*10  cofnam(maxcof)
      real          algcof(maxcof,maxtyp)
      integer       algact(maxtyp)
      integer       noutgrp, nouttyp
      character*10  outgrp(maxtyp), outtyp(maxtyp)
      integer       noprot , nopralg
      character*10  namprot(maxtyp), nampact(maxtyp),
     +              nampralg(maxtyp)

      ! actual algae

      integer       noalg
      character*10  name10
      character*10  grpnam(maxtyp)
      character*5   grpabr(maxtyp)
      character*10  typnam(maxtyp)
      character*5   typabr(maxtyp)

      ! output things

      character(len=20)     :: parnam                    ! output parameter name
      integer               :: parindx                   ! index in output parameter name array
      integer  ,pointer     :: proref(:,:)
      integer               :: nothread                  ! nr of threads

      ! old_items and replacent things

      type(old_item_coll)                :: old_items        ! the old_items table

      ! performance timer

      integer(4)                :: ithndl = 0
      if (timon) call timstrt( "dlwqp1", ithndl )

      ! how many threads ?
      nothread = nothrd

      ! allocate

      allocate(actlst(nbprm))

      ! start

      lchar(34) = 'proc_def'
      noloc  = 0
      nodef  = 0
      ndspx  = 0
      nvelx  = 0
      nlocx  = 0
      ndspn  = 0
      nveln  = 0
      noqtt  = noq   + noq4
      nosss  = noseg + nseg2
      procesdef%cursize=0
      procesdef%maxsize=0
      old_items%cursize = 0
      old_items%maxsize = 0

      ! open report file

      call dhopnf ( lun(35) , lchar(35), 35    , 1     , ierr2 )
      lurep = lun(35)
      line = ' '
      call setmlu ( lurep )
      call startup_screen( lurep )
      call monsys(line,11)
      call monsys(line,1)

      ! command line settingen , commands

      ! case sensitivity

      call getcom ( '-cs' , 0    , lfound, idummy, rdummy,
     +              cdummy, ierr2)
      if ( lfound ) then
         call setzmo ( 1 )
      endif

      ! monitoring level

      call getcom ( '-m'  , 1    , lfound, mlevel, rdummy,
     +              cdummy, ierr2)
      if ( lfound ) then
         if ( ierr2.eq. 0 ) then
            call setmmo ( mlevel )
         else
            call setmmo ( 10     )
         endif
      endif

      ! active processes only switch

      call getcom ( '-a'  , 1    , lfound, idummy, rdummy,
     +              cdummy, ierr2)
      if ( lfound ) then
         write(line,'(a)' ) ' found -a command line switch'
         call monsys(line,1)
         write(line,'(a)' ) ' only activated processes are switched on'
         call monsys(line,1)
         laswi = .true.
      else
         laswi = .false.
      endif

      ! no processes

      call getcom ( '-np' , 0    , lfound, idummy, rdummy,
     +              cdummy, ierr2)
      if ( lfound ) then
         swi_nopro = .true.
         write(line,'(a)' ) ' found -np command line switch'
         call monsys(line,1)
         write(line,'(a)' ) ' no processes from the process definition file are switched on'
         call monsys(line,1)
         versio = versip
      else
         swi_nopro = .false.
      endif

      ! process definition file

      if ( .not. swi_nopro ) then
         call getcom ( '-p'  , 3    , lfound, idummy, rdummy,
     +                 pdffil, ierr2)
         if ( lfound ) then
            if ( ierr2.ne. 0 ) then
               pdffil = ' '
            endif
         else
            pdffil = ' '
         endif
         if ( pdffil .ne. ' ' ) then
            lchar(34) = pdffil
            write(line,'(a)' ) ' found -p command line switch'
            call monsys(line,1)
         else
            pdffil = lchar(34)
         endif
         ierr2 = ierr
         call rd_tabs( pdffil, lurep , versio, serial, noinfo,
     +                 nowarn, ierr )
         if (ierr.gt.ierr2) then
            write(lurep,*) ' '
            write(lurep,*) ' ERROR: Could not read the process definition file.'
            write(lurep,*) '        Check if the filename after -p is correct, and exists.'
            write(lurep,*) '        Use -np if you want to run without processes.'
            write(lurep,*) ' '
            write(*,*) ' error opening nefis file(s):', trim(pdffil)
            write(*,*) ' '
            write(*,*) ' ERROR: Could not read the process definition file.'
            write(*,*) '        Check if the filename after -p is correct, and exists.'
            write(*,*) '        Use -np if you want to run without processes.'
            write(*,*) ' '
            call srstop(1)
         else
            write (lurep, *  )
            write (lurep,2001) trim(lchar(34))
            write (lurep,2002) versio
            write (lurep,2003) serial
            write (lurep, *  )

            ! fill the old_items conversion table

            call fill_old_items(old_items)
         endif
      endif

      ! old serial definitions

      if ( .not. swi_nopro ) then
         call getcom ( '-target_serial'  , 1    , lfound, target_serial, rdummy, cdummy, ierr2)
         if ( lfound ) then
            write(line,'(a)' ) ' found -target_serial command line switch'
            call monsys(line,1)
            if ( ierr2.ne. 0 ) then
               old_items%target_serial = target_serial
               write(line,'(a)')' no serial number given, using current'
               call monsys(line,1)
               old_items%target_serial = serial
            else
               write(line,'(a,i13)') ' using target serial number: ', target_serial
               call monsys(line,1)
               old_items%target_serial = target_serial
            endif
         else
            old_items%target_serial = serial
         endif
      endif

      ! configuration

      call getcom ( '-conf'  , 3    , lfound, idummy, rdummy,
     +              config, ierr2)
      if ( lfound ) then
         write(line,'(a)' ) ' found -conf command line switch'
         call monsys(line,1)
         if ( ierr2.ne. 0 ) then
            write(line,'(a)')' no configuration id given, using default'
            call monsys(line,1)
            config = ' '
         else
            write(line,'(a25,a10)') ' using configuration id: ', config
            call monsys(line,1)
         endif
      else
         config = ' '
      endif

      ! eco coupling

      call getcom ( '-eco'  , 3    , lfound, idummy, rdummy,
     +              blmfil, ierr2)
      if ( lfound ) then
         l_eco = .true.
         line = ' '
         call monsys(line,1)
         write(line,'(a)' ) ' found -eco command line switch'
         call monsys(line,1)
         if ( ierr2.ne. 0 ) then
            blmfil = 'bloom.spe'
            write(line,'(a30,a50)') ' using default eco input file:', blmfil
            call monsys(line,1)
         else
            write(line,'(a22,a58)') ' using eco input file:', blmfil
            call monsys(line,1)
         endif
      else
         blmnam = 'ACTIVE_BLOOM_P'
         blm_act = dlwq_find(constants,blmnam)
         if ( blm_act .gt. 0 .and. .not.swi_nopro) then
            l_eco = .true.
            line = ' '
            call monsys(line,1)
            write(line,'(a)' ) ' found constant ACTIVE_BLOOM_P without -eco command line switch'
            call monsys(line,1)
            blmfil = 'bloom.spe'
            write(line,'(a39,a41)') ' will try using default eco input file:', blmfil
            call monsys(line,1)
         else
            l_eco = .false.
            noprot  = 0
            nopralg = 0
         endif
      endif

      ! read the bloom-species database.
      if ( l_eco ) then
          open ( newunit=lunblm, file=blmfil, status = 'old', iostat = ierr2 )
          if ( ierr2 /= 0 ) then
             ierr = ierr + 1
             write(line,'(3a)') ' eco input file - ', trim(blmfil), ' not found! Exiting'
             call monsys(line,1)
             return
          endif

          read ( lunblm    , '(a)' ) line
          verspe = 1.0
          ioff =  index(line, 'BLOOMSPE_VERSION_')
          if(ioff.eq.0) then
             rewind( lunblm )
          else
             read (line(ioff+17:), *, err = 100) verspe
  100        continue
          endif

         call reaalg ( lurep  , lunblm , verspe , maxtyp , maxcof ,
     +                 notyp  , nocof  , noutgrp, nouttyp, alggrp ,
     +                 abrgrp , algtyp , abrtyp , algdsc , cofnam ,
     +                 algcof , outgrp , outtyp , noprot , namprot,
     +                 nampact, nopralg, nampralg)
      endif
      ! chem coupling

      call getcom ( '-chem'  , 3    , lfound, idummy, rdummy,
     +              chemid, ierr2)
      if ( lfound ) then
         l_chem = .true.
         write(line,'(a)' ) ' found -chem command line switch'
         call monsys(line,1)
         write(line,'(a)' ) ' chem coupling activated'
         call monsys(line,1)
         if ( ierr2.ne. 0 ) then
            chemid = 'charon'
            write(line,'(a30,a50)') ' using default chem input id: ', chemid
            call monsys(line,1)
         else
            write(line,'(a22,a58)') ' using chem input id: ', chemid
            call monsys(line,1)
         endif
      else
         l_chem = .false.
      endif

      ! check local dimensions

      allocate(idpnt(notot))
      allocate(ivpnt(notot))
      allocate(grdref(nogrid))
      allocate(sysgrd(notot))
      allocate(sysndt(notot))
      allocate(syname(notot))
      noconm = nocons + 1000
      allocate(coname(noconm))
      allocate(paname(nopa))
      allocate(funame(nofun))
      allocate(sfname(nosfun))
      allocate(diname(nodisp))
      allocate(vename(novelo))

      ! read ( rest ) of relevant delwaq files

      call dhopnf ( lun(2) , lchar(2), 2     , 2     , ierr2 )
      call rdwrk4 ( lun(2) , lurep  , modid  , syname , notot  ,
     +              nodump , nosys  , nobnd  , nowst  , nocons ,
     +              nopa   , noseg  , nseg2  , coname , paname ,
     +              funame , nofun  , sfname , nosfun , nodisp ,
     +              novelo , diname , vename , idpnt  , ivpnt  ,
     +              ndmpar , ntdmpq , ntdmps , noqtt  , noraai ,
     +              ntraaq , nobtyp , nowtyp , nogrid , grdref ,
     +              sysgrd , sysndt  )
      write ( lurep   , 2020 ) (modid(i),i=1,2)
      write ( lurep   , 2030 ) (modid(i),i=3,4)
      close ( lun(2) )

      ! change names according to old_items table

      nocon2 = nocons
      call set_old_items( lurep , old_items, notot , nopa  , nofun    ,
     +                    nosfun, nodisp   , novelo, syname, paname   ,
     +                    funame, sfname   , diname, vename, constants)

      ! replace proto with actual processes

      if ( l_eco ) then

         ! set algal type list, order is the (prescribed) order in the bloom database

         noalg = 0
         do ialg = 1 , notyp
            name10 = algtyp(ialg)
            call zoek( name10, notot, syname, 10 , isys )
            if ( isys .gt. 0 ) then
               noalg        = noalg + 1
               algact(ialg) = 1
               typnam(noalg)= algtyp(ialg)
               typabr(noalg)= abrtyp(ialg)
            else
               algact(ialg) = 0
            endif
         enddo

         ! when no algae were found, turn of eco mode
         if (noalg == 0) then
            write(line,'(a)') ' no BLOOM algae were found, switching off eco mode.'
            call monsys(line,1)
            l_eco = .false.
         else
         ! set algal group list
            nogrp = 0
            do iatyp = 1 , notyp
               if ( algact(iatyp) .eq. 1 ) then
                  call zoek( alggrp(iatyp), nogrp , grpnam, 10 , igrp )
                  if ( igrp .le. 0 ) then
                     nogrp = nogrp + 1
                     grpnam(nogrp)= alggrp(iatyp)
                     grpabr(nogrp)= abrgrp(iatyp)
                  endif
               endif
            enddo

            ! replace proto with actual processes in constant list
            call actrep( noalg   , noprot   , namprot, nampact, nopralg,
     +                   nampralg, constants)
         endif
      endif

      ! active only switch set trough a constant

      swinam = 'only_active'
      ix_act = dlwq_find(constants,swinam)
      if ( ix_act .gt. 0 ) then
         write(line,'(a)' ) ' found only_active constant'
         call monsys(line,1)
         write(line,'(a)' ) ' only activated processes are switched on'
         call monsys(line,1)
         laswi = .true.
      endif

      ! if active only make list of active processes

      no_act = 0
      if ( laswi ) then
         call set_active(constants, nbprm, no_act, actlst)
      endif

      ! if not active only and no configuration set default

      if ( .not. laswi ) then
         if ( config .eq. ' ' ) then
            if ( l_chem ) then
               config = 'chem'
            elseif ( l_eco ) then
               config = 'eco'
            else
               config = 'waq'
            endif
            write(line,'(a,a10)') ' using default configuration: ',
     +                            config
            call monsys(line,1)
         endif
      endif

      ! from nefis tables to proces definition structure

      if ( .not. swi_nopro ) then

         ! copy the configuration info for the eco proto processes to the actual processes

         if ( l_eco ) then
            call cnfrep( noalg   , noprot, namprot, nampact, nopralg,
     +                   nampralg)
         endif

         ! add the processes in the structure

         call prprop ( lurep, laswi, config, no_act, actlst, allitems, procesdef,
     +                 noinfo, nowarn, old_items, ierr2 )
         if ( ierr2 .ne. 0 ) ierr = ierr + 1
         nbpr   = procesdef%cursize

      else
         nbpr   = 0
      endif

      ! charon coupling

      if ( l_chem ) then

      endif

      ! add the statistical processes in the structure

      if ( statprocesdef%cursize .gt. 0 ) then
         do istat = 1 , statprocesdef%cursize
            statprocesdef%procesprops(istat)%sfrac_type = 0
            iret = procespropcolladd( procesdef , statprocesdef%procesprops(istat) )
            actlst(no_act+istat) = statprocesdef%procesprops(istat)%name
         enddo
         nbpr   = nbpr + statprocesdef%cursize
         no_act = no_act + statprocesdef%cursize
      endif

      ! set processes and fluxes for the substance fractions, this adds and alters processes in procesdef!

      call set_fraction( lurep    , notot   , syname, nomult, imultp,
     +                   procesdef, allitems, no_act, actlst, nbpr  )

      ! sort processes according to input - output relation

      call prsort ( lurep , procesdef, notot , nopa     , nosfun,
     +              syname, nocons   , nofun , constants, paname,
     +              funame, sfname   , nowarn)

      ! handle output from statistical processes

      call set_stat_output( statprocesdef, noutp, ioutps, nrvart, outputs)

      ! set output boot dimensions, attention !!!!! is new ncbufm written to work file?

       call outbo2 ( noutp , ioutps, nosss , nodump, nx    ,
     +               ny    , nrvart, nbufmx, ndmpar, notot ,
     +               ncbufm, noraai)


      ! replace names of bloom algea with actual names

      if ( l_eco .and. nbpr .gt. 0 ) then

         ! now replace process parameters

         call algrep ( procesdef, notyp , nocof , algtyp , algact,
     +                 abrtyp   , cofnam, algcof, maxcof , alggrp,
     +                 nogrp    , grpnam, grpabr, nouttyp, outtyp,
     +                 noutgrp  , outgrp)

         ! write the bloom efficiency file

         open ( newunit=lunfrm, file='bloominp.frm' )
         call blmeff (lurep , lunblm, verspe, lunfrm, grpnam, nogrp , typnam, noalg)
         close(lunblm)
         close(lunfrm)
      endif

      ! calculate new totals

      call proc_totals( lurep , procesdef, no_ins  , no_ine, no_ous,
     +                  no_oue, no_flu   , no_sto  , no_dis, no_vel)

      ! set offset local array

      ioff   = nopred + nocons + nopa + nofun + nosfun + notot

      ! check which processes can be turned on

      call makbar ( procesdef, notot , syname, nocons, constants,
     +              nopa     , paname, nofun , funame, nosfun,
     +              sfname   , nodisp, diname, novelo, vename,
     +              noqtt    , laswi , no_act, actlst, noinfo,
     +              nowarn   , ierr  )
      deallocate(       actlst,stat=ierr_dalloc)

      ! determine wich primary processes must be turned on

      ioffx = 4+nodisp+novelo+nofun+nocons
      allocate(idpnw(notot))
      allocate(ivpnw(notot))
      allocate(dsto(nosys*no_dis))
      allocate(vsto(nosys*no_vel))
      idpnw  = 0
      ivpnw  = 0
      dsto   = 0.0
      vsto   = 0.0
      call primpro ( procesdef, notot , syname, ndspx , nvelx ,
     &               ioffx    , nosys , dsto  , vsto  , ndspn ,
     &               idpnw    , nveln , ivpnw , noqtt , noinfo,
     &               nowarn   , ierr  )

      ! determine wich processes must be turned on for output purposes

      call setopp ( procesdef, outputs, ioff  )

      ! set pointers to input variables and output variables, if nessacary turn processes on.

      nmis  = 0
      noloc = 1
      nlocx = 0
      nodef = nopred
      maxdef = nodef + no_ins + no_ine
      allocate(defaul(maxdef))
      allocate(dename(maxdef))
      defaul    = 0.0
      defaul(5) = float(itstrt)
      defaul(6) = float(itstop)
      allocate(locnam(novarm))

      ! put theta in local array if wanted for output, the value will be filled by the integration routine
      ! noloc is already 1?, use this space!

      parnam = 'theta'
      call zoek(parnam,outputs%cursize,outputs%names,20,parindx)
      if ( parindx .gt. 0 .and. (intsrt .eq. 21 .or. intsrt .eq. 22) ) then
         locnam(1) = parnam
         outputs%pointers(parindx) = nopred + nocons + nopa + nofun + nosfun + notot + 1
         write ( line , '(3a)' ) ' output [',parnam,'] will be generated by numerical scheme'
         call monsys( line , 4 )
      endif

      call getinv ( procesdef, notot , syname, nocons, constants,
     +              nopa     , paname, nofun , funame, nosfun,
     +              sfname   , nodisp, diname, novelo, vename,
     +              nmis     , defaul, noloc , nodef , dename, outputs,
     +              ndspx    , nvelx , nlocx , locnam, refday)

      ! report on the use of the delwaq input

      call repuse ( procesdef, nocons, coname, nopa  , paname,
     +              nofun    , funame, nosfun, sfname, noinfo)

      ! a table will be made on selected processes
      ! to ensure resolved inputs with parallel processing

      call partab ( procesdef, notot , syname, nocons, constants,
     &              nopa     , paname, nofun , funame, nosfun   ,
     &              sfname   , proref, nrref , nowarn, nothread ,
     &              nopred   , noloc , nodef )

      ! set output pointers to process arrays parloc and defaul

      idef = ioff + noloc
      iflx = idef + nodef
      call setopo ( procesdef, outputs, ioff  , idef  , iflx  ,
     +              nowarn   )

      ! if not all input present , stop with exit code

      if ( nmis .gt. 0 ) then
         call dhopnf ( lun(24) , lchar(24), 24    , 1     , ierr2 )
         close ( lun(24) )
         write(lurep,*) ' not all input available.'
         write(lurep,*) ' number off missing variables :',nmis
         write(lurep,*) ' simulation impossible.'
         call srstop(1)
      endif

      ! set new pointer for dispersion and velocity

      call setdvp ( nodisp, idpnt , ndspn , idpnw , nosys ,
     +              ndspx , dsto  )
      call setdvp ( novelo, ivpnt , nveln , ivpnw , nosys ,
     +              nvelx , vsto  )

      ! set grid for processes

      call setprg ( procesdef, nogrid, notot, grdref, sysgrd,
     +              sysndt   )
      deallocate(grdref,sysgrd,sysndt)

      ! write proces work file
      call wr_proceswrk( lurep , procesdef, nodef , defaul, idpnw ,
     +                   ivpnw , dsto     , vsto  , locnam, nopred,
     +                   nocons, nopa     , nofun , nosfun, notot ,
     +                   noloc , nodisp   , novelo, ndspx , nvelx ,
     +                   nlocx , nosys    , nogrid, dename, coname, paname,
     +                   funame, sfname   , syname, intopt, lun   ,
     +                   lchar , noutp    , ioutps, outputs,ndmpar,
     +                   nbufmx, versio   , ndspn , nveln , nrref ,
     +                   proref, nproc    , nflux , novar , nipmsa)
      deallocate(defaul,dsto,vsto)
      deallocate(idpnw,ivpnw)
      deallocate(locnam)

      ! nrvart is in the boot sysn common

      nrvart = outputs%cursize

      ! Prepare descrtion and unit information for output from the proces library to be written in the NetCDF-file

      ! Extract names list from allitems
      allocate(ainame(allitems%cursize))
      do iitem = 1, allitems%cursize
         ainame(iitem) = allitems%itemproppnts(iitem)%pnt%name
      enddo

      ! Get location of FixAlg in algcof
      name10 = 'FixAlg'
      call zoek( name10, maxcof, cofnam, 10 , icof )

      ! Get information about the substances
      allocate (substdname(notot))
      allocate (subunit(notot))
      allocate (subdescr(notot))
      do isys = 1, notot
         subname = syname(isys)
         call str_lower(subname)
         call zoek(subname,allitems%cursize,ainame,20,iindx)
         if ( iindx .gt. 0) then
            substdname(isys) = allitems%itemproppnts(iindx)%pnt%stdn
            subunit(isys) = allitems%itemproppnts(iindx)%pnt%stdu
            subdescr(isys) = trim(allitems%itemproppnts(iindx)%pnt%text)//' '//
     &                            allitems%itemproppnts(iindx)%pnt%unit
         else
            ! Is it an algae?
            call zoek( subname(1:10), maxtyp, algtyp, 10 , ialg )
            if ( ialg .gt. 0) then
               if (algcof(icof, ialg) .ge. 0) then
                  substdname(isys) = ' '
                  subunit(isys) = 'g m-3'
                  subdescr(isys) = algdsc(ialg)//' (gC/m3)'
               else
                  substdname(isys) = ' '
                  subunit(isys) = 'g m-2'
                  subdescr(isys) = algdsc(ialg)//' (gC/m2)'
               endif
            else
               substdname(isys) = ' '
               subunit(isys) = ' '
               subdescr(isys) = syname(isys)
            endif
         endif
      enddo

      ! Lookup output names in names list
      do ioutp = 1, outputs%cursize
         outname = outputs%names(ioutp)
         call str_lower(outname)
         call zoek(outname,allitems%cursize,ainame,20,iindx)
         if ( iindx .gt. 0) then
            outputs%stdnames(ioutp) = allitems%itemproppnts(iindx)%pnt%stdn
            outputs%units(ioutp) = allitems%itemproppnts(iindx)%pnt%stdu
            outputs%descrs(ioutp) = trim(allitems%itemproppnts(iindx)%pnt%text)//' '//allitems%itemproppnts(iindx)%pnt%unit
         else if (outname.eq.'theta') then
            outputs%stdnames(ioutp) = ' '
            outputs%units(ioutp) = ' '
            outputs%descrs(ioutp) = 'Local-theta, generated by numerical scheme (-)'
         else
            ! Is it an algae?
            call zoek( outname(1:10), maxtyp, algtyp, 10 , ialg )
            if ( ialg .gt. 0) then
               if (algcof(icof, ialg) .ge. 0) then
                  outputs%stdnames(ioutp) = ' '
                  outputs%units(ioutp) = 'g m-3'
                  outputs%descrs(ioutp) = trim(algdsc(ialg))//' (gC/m3)'
               else
                  outputs%stdnames(ioutp) = ' '
                  outputs%units(ioutp) = 'g m-2'
                  outputs%descrs(ioutp) = trim(algdsc(ialg))//' (gC/m2)'
               endif
            else
               outputs%stdnames(ioutp) = ' '
               outputs%units(ioutp) = ' '
               outputs%descrs(ioutp) = outputs%names(ioutp)
            endif
         endif
      enddo
      ! write updated output work file ( output.wrk )

      call dhopnf ( lun(25), lchar(25), 25    , 1     , ierr2 )
      call wrwrko ( lun(25), noutp , nbufmx , ioutps, outputs,
     &              notot,  substdname, subunit, subdescr )
      close ( lun(25) )

      ! write altoys input files, only for old balance file
      ! ( altoys.inp batoys.inp altoys.ini altoys.fil)

      if ( btest(intopt,3) .and. .not. btest(intopt,4) ) then
         call wrtoys ( lchar  , lun   , notot , syname, noutp ,
     +                 ioutps , outputs)
      endif

      if (timon) call timstop( ithndl )
      return
 2001 format( ' Using process definition file : ',a    )
 2002 format( ' Version number                : ',f10.2)
 2003 format( ' Serial                        : ',i10  )
 2020 format (//' Model :            ',a40,/20x,a40 )
 2030 format (//' Run   :            ',a40,/20x,a40//)
      end
