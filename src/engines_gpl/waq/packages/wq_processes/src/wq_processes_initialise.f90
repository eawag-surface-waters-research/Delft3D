!!  Copyright (C)  Stichting Deltares, 2012-2019.
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
      
      subroutine wq_processes_initialise ( lunlsp       , pdffil       , blmfil    , &
                                           statprocesdef,                outputs   , &
                                           nomult       , imultp       , constants , &
                                           noinfo       , nowarn       , ierr)

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

      use processes_input
      use processes_pointers
      
      use dlwq_data
      use processet
      use output
      use string_module
      use m_alloc

      implicit none


      ! declaration of arguments

      integer             , intent(in   ) :: lunlsp          !< unit number spe
      character(len=*)    , intent(inout) :: pdffil          !< filename proc_def
      character(len=*)    , intent(inout) :: blmfil          !< filename spe

      type(procespropcoll), intent(in   ) :: statprocesdef   !< the statistical proces definition
      type(outputcoll)    , intent(inout) :: outputs         !< output structure
      integer  ( 4)       , intent(in   ) :: nomult          !< number of multiple substances
      integer  ( 4)       , intent(in   ) :: imultp(2,nomult)!< multiple substance administration
      type(t_dlwq_item)   , intent(inout) :: constants       !< delwaq constants list
      integer             , intent(inout) :: noinfo          !< count of informative message
      integer             , intent(inout) :: nowarn          !< count of warnings
      integer             , intent(inout) :: ierr            !< error count

      ! local declarations
      type(itempropcoll)        :: allitems        !< all items of the proces system

      real, parameter           :: versip = 5.06   ! version process system
      real                      :: verspe = 1.0    ! version bloom.spe file
      integer, parameter        :: novarm = 15000  ! max number of variables overall
      integer, parameter        :: nbprm  = 1750   ! max number of processes
      integer, parameter        :: nopred = 6      ! number of pre-defined variables

      integer                   :: noqtt           ! total number of exhanges
      integer                   :: no_ins          ! number of output items
      integer                   :: no_ine          ! number of output items
      integer                   :: no_ous          ! number of output items
      integer                   :: no_oue          ! number of output items
      integer                   :: no_sto          ! number of output items
      integer                   :: no_dis          ! number of output items
      integer                   :: no_vel          ! number of output items
      integer                   :: nocon2          ! number of constants plus some extra
      integer                   :: nmis            ! number of missing items
      integer                   :: maxdef          ! length defaul array

      integer                   :: lunblm          ! unit number bloom file
      integer                   :: lunfrm          ! unit number bloom frm file
      integer                   :: lund09          ! unit number bloom d09 file

      integer                   :: isys            ! index variable
      integer                   :: igrp            ! index variable
      integer                   :: iatyp           ! index variable
      integer                   :: ialg            ! index variable
      integer                   :: icof            ! index variable
      integer                   :: istat           ! index variable
      integer                   :: ioutp           ! index variable
      integer                   :: iitem           ! index variable
      integer                   :: iproc           ! index variable
      integer                   :: iindx           ! index variable
      integer                   :: ix_act          ! index variable
      integer                   :: ioff            ! offset for index item
      integer                   :: ioffx           ! offset for index item on exchange
      integer                   :: idef            ! offset to defualt items
      integer                   :: iflx            ! offset to flux items
      integer                   :: nflx            ! offset to flux items
      integer                   :: ifluxsys        ! index of flux items
      integer                   :: istochi         ! offset to stochi
      integer                   :: mxpmsa          ! maximum size of ipmsa (=max nr of input variables)
      integer                   :: iret            ! return value
      integer                   :: ierr2           ! error count

      integer                   :: idummy          ! dummy variable
      real                      :: rdummy          ! dummy variable
      character                 :: cdummy          ! dummy variable

      character*20 ,allocatable :: ainame(:)       ! all item names names in the proc_def
      character*20              :: subname         ! substance name
      character*100,allocatable :: substdname(:)   ! substance standard name
      character*40 ,allocatable :: subunit(:)      ! substance unit
      character*60 ,allocatable :: subdescr(:)     ! substance description
      character*20              :: outname         ! output name

      ! proces definition structure

      type(procespropcoll)      :: procesdef       ! the complete process definition
      type(procesprop), pointer :: proc            ! process description
      real                      :: scale           ! stochi factor
      character(len=20)         :: flxnam          ! output buffer
      integer                   :: nbpr            ! number of processes
      integer                   :: no_act          ! number of activated processes
      integer                   :: serial          ! serial number process definition
      integer                   :: target_serial   ! target serial number process definition
      real                      :: versio          ! version process defintion
      character*20 , allocatable :: actlst(:)

      ! settings

      character*80   swinam
      character*80   blmnam
      character*80   line
      character*80   idstr
      character*20   rundat
      character*10   config
      logical        lfound, laswi , swi_nopro, l3dmod, nolic
      integer        blm_act                       ! index of ACTIVE_BLOOM_P

      ! information

      logical        ex

      ! bloom-species database

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
      character*10  namprot(maxtyp), nampact(maxtyp), nampralg(maxtyp)

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

      ! old_items and replacent things

      type(old_item_coll)                :: old_items        ! the old_items table

      ! Dummy file structure (temp?)
      integer               :: lun(50)                 !< unit numbers
      character(len=255)    :: lchar(50)               !< filenames

      ierr2 = 0
      lun =  0
      lchar = ' '
      lun(24) = 1972
      lchar(24) = 'test-proces.wrk'
      
      ! allocate

      call realloc(actlst, nbprm, keepExisting=.false.,Fill=' ')

      ! start

      noloc  = 0
      nodef  = 0
      ndspx  = 0
      nvelx  = 0
      nlocx  = 0
      ndspn  = 0
      nveln  = 0
      noqtt  = 1
!      nosss  = noseg + nseg2
      allitems%cursize = 0
      allitems%maxsize = 0
      procesdef%cursize=0
      procesdef%maxsize=0
      old_items%cursize = 0
      old_items%maxsize = 0

      ! when we assume one column per call, is it really needed?
!      if ( noseg .gt. 1 ) then
         l3dmod = .true.
!      else
!         l3dmod = .false.
!      endif

      ! open report file

      ! Header for lsp
      call getidentification(idstr)
      write( lunlsp, '(XA/)') idstr
      call dattim(rundat)
      write (lunlsp,'(A,A/)') ' Execution start: ',rundat
      
      ! Active/inactive substance list
      write ( lunlsp , 2080 ) nosys , notot-nosys , notot
      write ( lunlsp , 2100 )
      do isys = 1,nosys
          write(lunlsp , '(I7,A,A)' ) isys, '  active      ', syname_sub(isys)
      end do
      do isys = nosys+1,notot
          write(lunlsp , '(I7,A,A)' ) isys, '  inactive    ', syname_sub(isys)
      end do
      write( lunlsp, '(/)')
      ! command line settingen , commands

      ! case sensitivity
      call setzmo ( 0 )
      ! call setzmo ( 1 )

      ! monitoring level
      !call setmmo ( mlevel )
      call setmmo ( 10     )

      ! active processes only switch
      ! only activated processes are switched on
      laswi = .true.
      ! laswi = .false.

      ! read process definition file

      call rd_tabs( pdffil, lunlsp , versio, serial, noinfo, nowarn, ierr )
      if (ierr.gt.ierr2) then
         write(lunlsp,*) ' '
         write(lunlsp,*) ' ERROR: Could not read the process definition file.'
         write(lunlsp,*) '        Check if the filename after -p is correct, and exists.'
         write(lunlsp,*) '        Use -np if you want to run without processes.'
         write(lunlsp,*) ' '
         write(*,*) ' error opening nefis file(s):', trim(pdffil)
         write(*,*) ' '
         write(*,*) ' ERROR: Could not read the process definition file.'
         write(*,*) '        Check if the filename after -p is correct, and exists.'
         write(*,*) '        Use -np if you want to run without processes.'
         write(*,*) ' '
         call srstop(1)
      else
         write (lunlsp, *  )
         write (lunlsp,2001) trim(pdffil)
         write (lunlsp,2002) versio
         write (lunlsp,2003) serial
         write (lunlsp, *  )

         ! fill the old_items conversion table

         call fill_old_items(old_items)
      endif

      ! old serial definitions
      swi_nopro = .false.
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

      call getcom ( '-conf'  , 3    , lfound, idummy, rdummy, config, ierr2)
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

      l_eco = blmfil .ne. ' '

      if (.not.l_eco) then
         blmnam = 'ACTIVE_BLOOM_P'
         blm_act = dlwq_find(constants,blmnam)
         if ( blm_act .gt. 0 .and. .not.swi_nopro) then
            blmfil = 'bloom.spe'
            inquire(file=blmfil,exist=l_eco)
            if (l_eco) then
               line = ' '
               call monsys(line,1)
               write(line,'(a)' ) ' found constant ACTIVE_BLOOM_P without -eco command line switch'
               call monsys(line,1)
               write(line,'(a)') ' and found default file bloom.spe. Will using default BLOOM file.'
               call monsys(line,1)
            else
               l_eco = .false.
               noprot  = 0
               nopralg = 0
            endif
         else
            l_eco = .false.
            noprot  = 0
            nopralg = 0
         endif
      endif
         ! read the bloom-species database.

      if ( l_eco ) then
         write (lunlsp,2004) trim(blmfil)
         open ( newunit = lunblm    , file=blmfil )
         read ( lunblm    , '(a)' ) line
         verspe = 1.0
         ioff =  index(line, 'BLOOMSPE_VERSION_')
         if(ioff.eq.0) then
            rewind( lunblm )
         else
            read (line(ioff+17:), *, err = 100) verspe
100         continue
         endif

         call reaalg ( lunlsp  , lunblm , verspe , maxtyp , maxcof , &
                       notyp   , nocof  , noutgrp, nouttyp, alggrp , &
                       abrgrp  , algtyp , abrtyp , algdsc , cofnam , &
                       algcof  , outgrp , outtyp , noprot , namprot, &
                       nampact , nopralg, nampralg)
      endif

      ! check local dimensions

      call realloc(idpnt, notot, keepExisting=.false.,Fill=0)
      call realloc(ivpnt, notot, keepExisting=.false.,Fill=0)

      ! change names according to old_items table

      nocon2 = nocons
      call set_old_items( lunlsp , old_items, notot , nopa  , nofun    , &
                          nosfun , nodisp   , novelo, syname, paname   , &
                          funame , sfname   , diname, vename, constants)

      ! replace proto with actual processes

      if ( l_eco ) then

         ! set algal type list, order is the (prescribed) order in the bloom database

         noalg = 0
         do ialg = 1 , notyp
            name10 = algtyp(ialg)
            call zoekns( name10, notot, syname, 10 , isys )
            if ( isys .gt. 0 ) then
               noalg        = noalg + 1
               algact(ialg) = 1
               typnam(noalg)= algtyp(ialg)
               typabr(noalg)= abrtyp(ialg)
            else
               algact(ialg) = 0
            endif
         enddo

         ! set algal group list

         nogrp = 0
         do iatyp = 1 , notyp
            if ( algact(iatyp) .eq. 1 ) then
               call zoekns( alggrp(iatyp), nogrp , grpnam, 10 , igrp )
               if ( igrp .le. 0 ) then
                  nogrp = nogrp + 1
                  grpnam(nogrp)= alggrp(iatyp)
                  grpabr(nogrp)= abrgrp(iatyp)
               endif
            endif
         enddo

         ! replace proto with actual processes in constant list

         call actrep( noalg   , noprot   , namprot, nampact, nopralg, nampralg, constants)

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
            if ( l_eco ) then
               config = 'eco'
            else
               config = 'waq'
            endif
            write(line,'(a,a10)') ' using default configuration: ', config
            call monsys(line,1)
         endif
      endif

      ! from nefis tables to proces definition structure

      if ( .not. swi_nopro ) then

         ! copy the configuration info for the eco proto processes to the actual processes

         if ( l_eco ) then
            call cnfrep( noalg   , noprot, namprot, nampact, nopralg, nampralg)
         endif

         ! add the processes in the strucure

         call prprop ( lunlsp    , laswi   , l3dmod   , config, no_act, &
                       actlst    , allitems, procesdef, noinfo, nowarn, &
                       old_items , ierr2 )
         if ( ierr2 .ne. 0 ) ierr = ierr + 1
         nbpr   = procesdef%cursize

      else
         nbpr   = 0
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

      call set_fraction( lunlsp    , notot   , syname, nomult, imultp, procesdef, allitems, no_act, actlst, nbpr  )

      ! sort processes according to input - output relation

      call prsort ( lunlsp , procesdef, notot , nopa     , nosfun, &
                    syname, nocons   , nofun , constants, paname,  &
                    funame, sfname   , nowarn)

      ! handle output from statistical processes

!      call set_stat_output( statprocesdef, noutp, ioutps, nrvart, outputs)

      ! set output boot dimensions, attention !!!!! is new ncbufm written to work file?

!       call outbo2 ( noutp , ioutps, nosss , nodump, nx    ,
!     +               ny    , nrvart, nbufmx, ndmpar, notot ,
!     +               ncbufm, noraai)


      ! replace names of bloom algea with actual names

      if ( l_eco .and. nbpr .gt. 0 ) then

         ! now replace process parameters

         call algrep ( procesdef, notyp , nocof , algtyp , algact, &
                       abrtyp   , cofnam, algcof, maxcof , alggrp, &
                       nogrp    , grpnam, grpabr, nouttyp, outtyp, &
                       noutgrp  , outgrp)

         ! write the bloom efficiency file

         lunfrm = 89
         open ( lunfrm    , file='bloominp.frm' )
         call blmeff (lunlsp , lunblm, verspe, lunfrm, grpnam, nogrp , typnam, noalg)
         close(lunblm)
         close(lunfrm)

         inquire (file = 'bloominp.d09', exist = ex)
         if(.not.ex) then
            lund09 = 89
            open ( lund09    , file='bloominp.d09' )
            call blmd09 (lunlsp , lund09)
            close(lund09)
         endif
      endif

      ! calculate new totals

      call proc_totals( lunlsp , procesdef, no_ins  , no_ine, no_ous, &
                        no_oue, no_flu   , no_sto  , no_dis, no_vel)

      ! set offset local array

      ioff   = nopred + nocons + nopa + nofun + nosfun + notot

      ! check which processes can be turned on

      call makbar ( procesdef, notot , syname, nocons, constants, &
                    nopa     , paname, nofun , funame, nosfun,    &
                    sfname   , nodisp, diname, novelo, vename,    &
                    noqtt    , laswi , no_act, actlst, noinfo,    &
                    nowarn   , ierr  )
      deallocate(actlst)

      ! determine wich primary processes must be turned on

      ioffx = 4+nodisp+novelo+nofun+nocons
      call realloc(idpnw, notot, keepExisting=.false.,Fill=0)
      call realloc(ivpnw, notot, keepExisting=.false.,Fill=0)
      call realloc(dsto , nosys*no_dis, keepExisting=.false.,Fill=0.0e0)
      call realloc(vsto , nosys*no_vel, keepExisting=.false.,Fill=0.0e0)
      idpnw  = 0
      ivpnw  = 0
      dsto   = 0.0
      vsto   = 0.0
      call primpro ( procesdef, notot , syname, ndspx , nvelx , &
                     ioffx    , nosys , dsto  , vsto  , ndspn , &
                     idpnw    , nveln , ivpnw , noqtt , noinfo, &
                     nowarn   , ierr  )

      ! determine wich processes must be turned on for output purposes

      call setopp ( procesdef, outputs, ioff  )

      ! set pointers to input variables and output variables, if nessacary turn processes on.

      nmis  = 0
      noloc = 1
      nlocx = 0
      nodef = nopred
      maxdef = nodef + no_ins + no_ine
      call realloc(defaul, maxdef, keepExisting=.false.,Fill=0.0e0)
      call realloc(dename, maxdef, keepExisting=.false.,Fill=' ')

      defaul    = 0.0
      defaul(5) = float(itstrt)
      defaul(6) = float(itstop)
      call realloc(locnam, novarm, keepExisting=.false.,Fill=' ')

      ! put theta in local array if wanted for output, the value will be filled by the integration routine
      ! noloc is already 1?, use this space!

      parnam = 'theta'
      call zoekns(parnam,outputs%cursize,outputs%names,20,parindx)
      if ( parindx .gt. 0 .and. (intsrt .eq. 21 .or. intsrt .eq. 22) ) then
         locnam(1) = parnam
         outputs%pointers(parindx) = nopred + nocons + nopa + nofun + nosfun + notot + 1
         write ( line , '(3a)' ) ' output [',parnam,'] will be generated by numerical scheme'
         call monsys( line , 4 )
      endif

      call getinv ( procesdef, notot , syname, nocons, constants, &
                    nopa     , paname, nofun , funame, nosfun,    &
                    sfname   , nodisp, diname, novelo, vename,    &
                    nmis     , defaul, noloc , nodef , dename, outputs,   &
                    ndspx    , nvelx , nlocx , locnam   )

      ! report on the use of the delwaq input

      call repuse ( procesdef, nocons, coname, nopa  , paname, nofun    , funame, nosfun, sfname, noinfo)

      ! set output pointers to process arrays parloc and defaul

      idef = ioff + noloc
      iflx = idef + nodef
      call setopo ( procesdef, outputs, ioff  , idef  , iflx  , nowarn   )

      ! if not all input present , stop with exit code

      if ( nmis .gt. 0 ) then
!         call dhopnf ( lun(24) , lchar(24), 24    , 1     , ierr2 )   why?
!         close ( lun(24) )                                            why?
         write(lunlsp,*) ' not all input available.'
         write(lunlsp,*) ' number off missing variables :',nmis
         write(lunlsp,*) ' simulation impossible.'
         call srstop(1)
      endif

      ! set new pointer for dispersion and velocity

      call setdvp ( nodisp, idpnt , ndspn , idpnw , nosys , ndspx , dsto  )
      call setdvp ( novelo, ivpnt , nveln , ivpnw , nosys , nvelx , vsto  )

      ! set grid for processes
      procesdef%procesprops%grid = 1

      ! write proces work file
      nproc = 0
      nflux = 0

      nbpr  = 0
      do iproc = 1, procesdef%cursize
         if ( procesdef%procesprops(iproc)%active ) then
            nbpr = nbpr + 1
         endif
      enddo

      ! calculate new totals

      call proc_totals( lunlsp , procesdef, no_ins  , no_ine, no_ous, &
                        no_oue, no_flu   , no_sto  , no_dis, no_vel)

      ! calculate and fill output structure

      nipmsa = 0
      ioffx = nopred+nocons+nopa+nofun+nosfun+notot+noloc+nodef
      mxpmsa = no_ine+no_ins+no_ous+no_oue+no_flu
      call realloc(prvnio, nbpr  , keepExisting=.false.,Fill=0)
      call realloc(iflux , nbpr  , keepExisting=.false.,Fill=0)
      call realloc(ipmsa , mxpmsa, keepExisting=.false.,Fill=0)
      call realloc(ipssa , mxpmsa, keepExisting=.false.,Fill=0)
      call realloc(prvvar, mxpmsa, keepExisting=.false.,Fill=0)
      call realloc(prvtyp, mxpmsa, keepExisting=.false.,Fill=0)
      call realloc(progrd, nbpr  , keepExisting=.false.,Fill=0)
      call realloc(prondt, nbpr  , keepExisting=.false.,Fill=0)
      call realloc(pronam, nbpr  , keepExisting=.false.,Fill=' ')
      call intoou ( procesdef, nproc , nflux , prvnio, pronam, &
                    iflux    , ipmsa , ipssa , nipmsa, ioffx , &
                    nocons   , nopa  , nofun , nosfun, notot , &
                    nodisp   , novelo, nodef , noloc , ndspx , &
                    nvelx    , nlocx , nopred, prvvar, prvtyp, &
                    novar    , progrd, prondt)

      deallocate(ipmsa,ipssa)

      ! set variables attribute's for aggregation dis-aggregation

      call realloc(varnam, novar, keepExisting=.false.,Fill=' ')
      varnam = ' '
      call realloc(vararr, novar, keepExisting=.false.,Fill=0)
      call realloc(varidx, novar, keepExisting=.false.,Fill=0)
      call realloc(vartda, novar, keepExisting=.false.,Fill=0)
      call realloc(vardag, novar, keepExisting=.false.,Fill=0)
      call realloc(vartag, novar, keepExisting=.false.,Fill=0)
      call realloc(varagg, novar, keepExisting=.false.,Fill=0)
      call setvat ( lunlsp, nocons, nopa  , nofun , nosfun, &
                    nosys , notot , nodisp, novelo, nodef , &
                    noloc , ndspx , nvelx , nlocx , nflux , &
                    nopred, novar , vararr, varidx, vartda, &
                    vardag, vartag, varagg, nogrid, coname, &
                    paname, funame, sfname, dename, syname, &
                    locnam, varnam)

      ! determine stochi

      call realloc(stochi,(/notot,nflux/), keepExisting=.false.,Fill=0.0e0)
      call realloc(fluxname, nflux, keepExisting=.false.,Fill=' ')
      call realloc(fluxprocname, nflux, keepExisting=.false.,Fill=' ')
      do iflx = 1 , nflux
         do isys = 1 , notot
            stochi(isys,iflx) = 0.0
         enddo
      enddo

      nflx = 0
      totfluxsys = 0
      do iproc = 1 , nproc
         proc => procesdef%procesprops(iproc)
         if ( proc%active ) then
            do istochi = 1, proc%no_fluxstochi
               flxnam = proc%fluxstochi(istochi)%ioitem
               isys   = proc%fluxstochi(istochi)%subindx
               scale  = proc%fluxstochi(istochi)%scale
               if ( isys.gt.0 .and. abs(scale).gt.1e-10) then
                  call zoekio ( flxnam, proc%no_fluxoutput, proc%fluxoutput, 20, iflx)
                  stochi(isys,nflx + iflx) = scale
                  fluxname(nflx + iflx) = flxnam(1:10)
                  fluxprocname(nflx + iflx) = proc%name(1:10)
                  totfluxsys = totfluxsys + 1
               endif
            enddo
            nflx = nflx + proc%no_fluxoutput
         endif
      enddo

      call realloc(nfluxsys, notot, keepExisting=.false.,Fill=0)
      call realloc(fluxsys, totfluxsys, keepExisting=.false.,Fill=0)
      
      ifluxsys = 0
      do isys = 1 , notot
         do iflx = 1 , nflux
            if(stochi(isys,iflx).ne.0.0) then
               ifluxsys = ifluxsys + 1
               nfluxsys(isys) = nfluxsys(isys) + 1
               fluxsys(ifluxsys) = iflx
            endif
         enddo
      enddo

      ! nrvart is in the boot sysn common

      nrvart = outputs%cursize

      ! Prepare descrtion and unit information for output from the proces library to be written in the NetCDF-file

      ! Extract names list from allitems
      call realloc(ainame, allitems%cursize, keepExisting=.false.,Fill=' ')
      do iitem = 1, allitems%cursize
         ainame(iitem) = allitems%itemproppnts(iitem)%pnt%name
      enddo

      ! Get location of FixAlg in algcof
      name10 = 'FixAlg'
      call zoekns( name10, maxcof, cofnam, 10 , icof )

      ! Get information about the substances
      call realloc (substdname, notot, keepExisting=.false.,Fill=' ')
      call realloc (subunit   , notot, keepExisting=.false.,Fill=' ')
      call realloc (subdescr  , notot, keepExisting=.false.,Fill=' ')
      do isys = 1, notot
         subname = syname(isys)
         call str_lower(subname)
         call zoekns(subname,allitems%cursize,ainame,20,iindx)
         if ( iindx .gt. 0) then
            substdname(isys) = allitems%itemproppnts(iindx)%pnt%stdn
            subunit(isys) = allitems%itemproppnts(iindx)%pnt%stdu
            subdescr(isys) = trim(allitems%itemproppnts(iindx)%pnt%text)//' '// &
                                  allitems%itemproppnts(iindx)%pnt%unit
         else
            ! Is it an algae?
            call zoekns( subname(1:10), maxtyp, algtyp, 10 , ialg )
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
         call zoekns(outname,allitems%cursize,ainame,20,iindx)
         if ( iindx .gt. 0) then
            outputs%stdnames(ioutp) = allitems%itemproppnts(iindx)%pnt%stdn
            outputs%units(ioutp) = allitems%itemproppnts(iindx)%pnt%unit
            outputs%descrs(ioutp) = allitems%itemproppnts(iindx)%pnt%text//' '//allitems%itemproppnts(iindx)%pnt%unit
         else if (outname.eq.'theta') then
            outputs%stdnames(ioutp) = ' '
            outputs%units(ioutp) = ' '
            outputs%descrs(ioutp) = 'Local-theta, generated by numerical scheme (-)'
         else
            ! Is it an algae?
            call zoekns( outname(1:10), maxtyp, algtyp, 10 , ialg )
            if ( ialg .gt. 0) then
               if (algcof(icof, ialg) .ge. 0) then
                  outputs%stdnames(ioutp) = ' '
                  outputs%units(ioutp) = 'g m-3'
                  outputs%descrs(ioutp) = algdsc(ialg)//' (gC/m3)'
               else
                  outputs%stdnames(ioutp) = ' '
                  outputs%units(ioutp) = 'g m-2'
                  outputs%descrs(ioutp) = algdsc(ialg)//' (gC/m2)'
               endif
            else
               outputs%stdnames(ioutp) = ' '
               outputs%units(ioutp) = ' '
               outputs%descrs(ioutp) = outputs%names(ioutp)
            endif
         endif
      enddo

      ! Determine pointer from prvnio, and promnr from pronam
      call realloc(prvpnt, nproc, keepExisting=.false.,Fill=0)
      call realloc(promnr, nproc, keepExisting=.false.,Fill=0)
      prvpnt(1) = 1
      do iproc = 2,nproc
          prvpnt(iproc) = prvpnt(iproc-1)+prvnio(iproc-1)
      end do
      do iproc = 1,nproc
          call pronrs(pronam(iproc),promnr(iproc))
      end do

      return
 2001 format( ' Using process definition file : ',a    )
 2002 format( ' Version number                : ',f10.2)
 2003 format( ' Serial                        : ',i10  )
 2004 format( ' Using BLOOM definition file   : ',a    /)
 2020 format (//' Model :            ',a40,/20x,a40 )
 2030 format (//' Run   :            ',a40,/20x,a40//)
 2080 format ( /' Number of active (transported) constituents       :',I3,/ &
                ' Number of inactive (not transported) constituents :',I3,/ &
                ' Total number of constituents                      :',I3  )
 2100 format ( /' Number  (in)active  name')
 2090 format ( 'I4,8X,A,4X,A' )
      end
