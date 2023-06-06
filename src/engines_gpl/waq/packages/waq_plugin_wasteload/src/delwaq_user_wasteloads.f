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

      module delwaq_user_wasteloads

      use m_zoek
      use m_srstop
      use m_dhopnf
      use m_dhnolay
      use delwaq_loads
      use waq_plugin_wasteload_version_module

      contains
      subroutine delwaq_user_wasteload ( nowst , wasteloads, notot , nosys , noseg ,
     +                                   itime , conc      , syname)
      !DEC$ ATTRIBUTES DLLEXPORT::delwaq_user_wasteload

      ! routine to insert user functionality to the wasteloads
      ! contains currently the inlet-outlet coupling functionality

      implicit none

      ! arguments declarations

      integer                             :: nowst                  ! number of wasteloads
      type(wasteload), pointer            :: wasteloads(:)          ! array of all wasteloads (structure)
      integer                             :: notot                  ! total number of substances
      integer                             :: nosys                  ! number of active substances
      integer                             :: noseg                  ! number of segments
      integer                             :: itime                  ! system time
      real                                :: conc(notot,noseg)      ! concentration array
      character(len=*)                    :: syname(notot)          ! substance names
      character(len=120)                  :: idstr                  ! waq_plugin_wasteload version number

      ! local declarations

      integer, save       :: ifirst = 1
      integer, save       :: lunrep
      integer             :: ierr
      integer             :: iwst
      integer             :: isys

      ! the inlet outlet coupling

      if (ifirst == 1) then
         ifirst = 0

         call dhopnf (lunrep, 'delwaq_user_wasteloads.mon', 19, 1, ierr)
         if (ierr .ne. 0) then
            write(*,'(A)') 'Could not open delwaq_user_wasteloads.mon for writing.'
            call srstop(1)
         endif

         ! waq_plugin_wasteload version number
         call getfullversionstring_waq_plugin_wasteload(idstr)
         write ( lunrep , * ) idstr
      endif

      call delwaq_user_inlet_outlet ( nowst , wasteloads, notot , nosys , noseg ,
     +                                itime , conc      , syname, lunrep)

      ! walking discharges

      call delwaq_user_walking_discharges ( nowst , wasteloads, notot , nosys , noseg ,
     +                                      itime , conc      , syname, lunrep)

      ! report on wasteloads

      call delwaq_user_bubble_bath  ( nowst , wasteloads, notot , nosys , noseg ,
     &                                itime , conc      , syname, lunrep)

      return
      end subroutine delwaq_user_wasteload

      subroutine delwaq_user_bubble_bath  ( nowst  , wls    , notot  , nosys  , noseg  ,
     &                                      itime  , conc   , syname , lunrep)

!       routine to set the bubble screen option for Nieuwe Meer
!                made by Leo Postma at 6 october 2006

!       global declarations

      implicit none

!       arguments declarations

      integer                  :: nowst             ! number of wasteloads
      type(wasteload), pointer :: wls(:)            ! array of all wasteloads (structure)
      integer                  :: notot             ! total number of substances
      integer                  :: nosys             ! number of active substances
      integer                  :: noseg             ! number of segments
      integer                  :: itime             ! system time
      real                     :: conc(notot,noseg) ! concentration array
      character(len=IDLEN)     :: syname(notot)     ! substance names

!       local declarations

      logical                  :: first = .true.    ! initialisation indicator
      integer                  :: lunrep            ! logical unit of report file
      integer                  :: lunscr            ! logical unit of screen file
      logical                  :: l_exi             ! file exists or not
      integer                  :: noscrn            ! number of bubble screens
      integer                  :: iscrn             ! loop counter screens
      integer                  :: iwst              ! loop counter loads
      integer                  :: isub              ! loop counter substances
      real                     :: wflow             ! flow of a load
      character(len=IDLEN), pointer :: scrnam(:)    ! array with screen names
      integer             , pointer :: scrloc(:)    ! array with screen locations
      real                , pointer :: scrwtd(:,:)  ! withdrawal mass per substance
      real                , pointer :: scrwdf(:)    ! withdrawal flow

      ! Save all local variables

      SAVE

      if ( first ) then
         first = .false.
         noscrn = 0
         inquire (file='screen.dat',exist=l_exi)
         if ( l_exi ) then
            open  ( newunit = lunscr , file='screen.dat' )        !  read file with
            read  ( lunscr , * ) noscrn                 !  screen-names
            write ( lunrep , * ) 'Number of screens:', noscrn
            if ( noscrn .gt. 0 ) then               !  may be more names
               allocate ( scrnam(noscrn) )          !  than existing in the
               do iscrn = 1, noscrn                 !  problem
                  read  ( lunscr , * ) scrnam(iscrn)
                  write ( lunrep , * ) 'Screen:',iscrn,' is called: ',scrnam(iscrn)
               enddo
               close ( lunscr )
               allocate ( scrloc( nowst  ) )        !  pointer from waste to screen
               allocate ( scrwtd( noscrn, notot ) )
               allocate ( scrwdf( noscrn ) )
               scrloc = 0
               do iwst = 1, nowst
                  do iscrn = 1, noscrn
                     if ( find_string( scrnam(iscrn), wls(iwst)%id%id ) ) then
                        scrloc(iwst) = iscrn
                        write ( lunrep , * ) 'Load:',iwst,' is part of screen:',iscrn
                        exit
                     endif
                  enddo
               enddo
            endif
         else
            write ( lunrep , * ) 'No file <screen.dat> detected'
         endif
      endif

      if ( noscrn .eq. 0 ) return

!        First  step: sum the withdrawn masses and flow per screen

      scrwtd = 0.0                                  !  zero the withdrawal
      scrwdf = 0.0                                  !  accumulation arrays
      do iwst = 1, nowst
         iscrn = scrloc(iwst)
         if ( iscrn .ne. 0 ) then                   !  screens only
            wflow = wls(iwst)%flow
            if ( wflow .lt. 0.0 ) then              !  withdrawals only
               scrwdf( iscrn ) = scrwdf( iscrn ) + wflow
               do isub = 1, nosys
                  scrwtd ( iscrn, isub ) = scrwtd ( iscrn, isub ) +
     &                                        wflow * conc( isub, wls(iwst)%loc%segnr )
                  wls(iwst)%loads(isub ) = 0.0      !  for safety
               enddo
               do isub = nosys+1, notot
                  scrwtd ( iscrn, isub ) = 0.0
                  wls(iwst)%loads(isub ) = 0.0      !  for safety
               enddo
            endif
         endif
      enddo

!        Second step: mix the withdrawal to get concentrations

      do iscrn = 1, noscrn                          !  make the mixed
         wflow = scrwdf( iscrn )                    !  concentrations
         if ( wflow .ne. 0.0 ) then                 !  per active screen

            do isub = 1, notot
               scrwtd ( iscrn, isub ) = scrwtd ( iscrn, isub ) / wflow
            enddo
         endif
      enddo

!        Third  step: set the mixed concentrations for the releases

      do iwst = 1, nowst
         iscrn = scrloc(iwst)
         if ( iscrn .ne. 0 ) then                   !  screens only
            wflow = wls(iwst)%flow
            if ( wflow .gt. 0.0 ) then              !  releases only
               do isub = 1, notot
                  wls(iwst)%loads(isub ) = scrwtd ( iscrn, isub )
               enddo
            endif
         endif
      enddo

!        NB: in this code it is assumed that also inactive substances are
!            withdrawn and released like active substances ( so with a flow
!            concentration that form together the mass )

      return

      end subroutine delwaq_user_bubble_bath

      subroutine delwaq_user_inlet_outlet ( nowst , wasteloads, notot , nosys , noseg ,
     +                                      itime , conc      , syname, lunrep)

      ! routine to set the default inlet-outlet coupling

      ! global declarations

      implicit none

      ! arguments declarations

      integer                             :: nowst                  ! number of wasteloads
      type(wasteload), pointer            :: wasteloads(:)          ! array of all wasteloads (structure)
      integer                             :: notot                  ! total number of substances
      integer                             :: nosys                  ! number of active substances
      integer                             :: noseg                  ! number of segments
      integer                             :: itime                  ! system time
      real                                :: conc(notot,noseg)      ! concentration array
      character(len=*)                    :: syname(notot)          ! substance names

      ! local declarations

      integer, save       :: ifirst = 1                       ! initialisation indicator
      integer             :: lunrep                           ! report file
      integer             :: luninout                         ! inlet/outlet file
      logical             :: l_exi                            ! file exists or not
      integer             :: ncomb                            ! number of possible inlet outlet combinations
      integer             :: ninout                           ! actual number of inlet outlet combinations
      character(len=20)   :: c_in                             ! read buffer name inlet
      character(len=20)   :: c_out                            ! read buffer name outlet
      character(len=20), dimension(:), allocatable  :: namin  ! names inlet in the possible combinations
      character(len=20), dimension(:), allocatable  :: namout ! names outlet in the possible combinations
      integer, dimension(:), allocatable  ::           iwin   ! wasteload number inlet of the actual combinations
      integer, dimension(:), allocatable  ::           iwout  ! wasteload number outlet of the actual combinations
      real                :: flow                             ! inlet flow rate
      integer             :: ipin                             ! wasteload number inlet
      integer             :: ipout                            ! wasteload number outlet
      integer             :: iseg                             ! inlet segment number
      integer             :: isego                            ! outlet segment number
      integer             :: iwst                             ! loop counter wasteloads
      integer             :: isys                             ! loop counter substances
      integer             :: icomb                            ! loop counter combinations
      integer             :: iinout                           ! loop counter of inlet outlet combinations
      integer             :: ierr                             ! local I/O error
      integer             :: i                                ! loop counter

      ! Save all local variables

      SAVE

      ! test if there are inlet outlet combinations

      if ( ifirst .eq. 1 ) then
         ifirst = 0
         write(lunrep,*)
         write(lunrep,2000)

         ! if availeble read list of possible inlet outlet combinations

         inquire (file='inloutl.dat',exist=l_exi)
         if ( l_exi ) then
            write(lunrep,2004)
            open ( newunit = luninout , file='inloutl.dat' )
            ncomb = 0
            do
               read ( luninout , '(2a20)' , iostat = ierr ) c_in,c_out
               if ( ierr /= 0 ) then
                   exit
               endif

               ncomb = ncomb + 1
            enddo

            allocate( namin(ncomb), namout(ncomb), iwin(ncomb), iwout(ncomb) )

            rewind( luninout )

            do i = 1,ncomb
               read ( luninout , '(2a20)' ) c_in,c_out
               namin(i) = c_in
               namout(i) = c_out
            enddo
            close ( luninout )
         else

            ! construct the default list of combination INLETxx/OUTLETxx

            ncomb = max(999,size(wasteloads))

            allocate( namin(ncomb), namout(ncomb), iwin(ncomb), iwout(ncomb) )

            write(lunrep,2006)
            do i = 1 , ncomb
               write(namin(i),'(a,i0)')  'INLET',  i
               write(namout(i),'(a,i0)') 'OUTLET', i
            enddo
         endif

         ! check the actual list of wasteloads with the list of possible combinations

         ninout = 0
         do icomb = 1 , ncomb
            ipin  = find_wasteload( namin(icomb) , wasteloads)
            ipout = find_wasteload( namout(icomb), wasteloads)
            if ( ipin .gt. 0 .and. ipout .gt. 0 ) then

               ! a combination found, print and set administration

               write(lunrep,*)
               write(lunrep,2001) ipin,wasteloads(ipin)%id%id
               write(lunrep,2002) ipout,wasteloads(ipout)%id%id
               write(lunrep,*)
               ninout = ninout + 1
               iwin(ninout)  = ipin
               iwout(ninout) = ipout

            endif
         enddo
         if ( ninout .eq. 0 ) write(lunrep,2013)
         write(lunrep,2003)

      endif

      ! set outlet equal to inlet flow * concentration at inlet segment for all found combinations

      do iinout = 1 , ninout
         ipin  = iwin(iinout)
         ipout = iwout(iinout)
         iseg  = wasteloads(ipin)%loc%segnr
         isego = wasteloads(ipout)%loc%segnr
         flow  = wasteloads(ipin)%flow
         if ( flow <= 0.0 ) then
            wasteloads(ipout)%flow = 0.0
            do isys = 1, nosys
               wasteloads(ipout)%loads(isys) = -flow*conc(isys,iseg)
            enddo
            do isys = nosys + 1 , notot
               wasteloads(ipout)%loads(isys) = 0.0
            enddo
         else
            ! Reversed flow - still using the flow rate at the inlet (!)
            wasteloads(ipin)%flow = 0.0
            do isys = 1, nosys
               wasteloads(ipin)%loads(isys) = flow*conc(isys,isego)
            enddo
            do isys = nosys + 1 , notot
               wasteloads(ipin)%loads(isys) = 0.0
            enddo
         endif
      enddo
!
      return
 2000 format (' extra functionality INLET/OUTLET')
 2001 format ('    waste number:',i5,' name:',a20,' (INLET) coupled to')
 2002 format ('    waste number:',i5,' name:',a20,' (OUTLET)')
 2003 format (' end extra functionality INLET/OUTLET')
 2004 format ('    possible INLET/OUTLET combinations will be read from ',
     +        'file <inloutl.dat>')
 2005 format ('    error : number of combinations exceed maximum:',i4)
 2006 format ('    no file <inloutl.dat> using default combinations names')
 2013 format ('    no INLET/OUTLET combination found')

      end subroutine delwaq_user_inlet_outlet

      function find_wasteload( waste_id, wasteloads) result ( iwst)

         ! function to find a wasteload on id in an array of wasteloads

         character(len=*)                    :: waste_id               ! wasteload id to be found
         type(wasteload), pointer            :: wasteloads(:)          ! array of all wasteloads (structure)
         integer                             :: iwst                   ! on return if found wasteload number, otherwise zero

         ! local declarations

         character(len=20)                   :: name                   ! fixed length copy of waste_id
         integer                             :: nowst                  ! length of wasteloads array
         integer                             :: i                      ! loop counter
         integer                             :: ifound                 ! loop counter

         ! loop over the wasteloads and compare id with delwaq routine zoekns

         nowst = size(wasteloads)
         name  = waste_id
         iwst = 0
         do i = 1 , nowst
            call zoekns(name,1,wasteloads(i)%id%id,20,ifound)
            if ( ifound .eq. 1 ) then
               iwst = i
               return
            endif
         end do

      end function find_wasteload

      function find_substance( substance_id, syname) result ( isys)

         ! function to find a substance on id

         character(len=*)                    :: substance_id           ! substance id to be found
         character(len=20)                   :: syname(:)              ! substance names
         integer                             :: isys                   ! on return if found substance number, otherwise zero

         ! local declarations

         character(len=20)                   :: name                   ! fixed length copy of waste_id
         integer                             :: notot                  ! length of syname array

         ! call the delwaq routine zoekns

         notot = size(syname)
         name  = substance_id
         call zoekns(name,notot,syname,20,isys)

      end function find_substance

      function find_string( string, test ) result ( found )

!            function to find a string in a test-string

      character(*) string
      character(*) test
      logical      found

!            local declarations

      integer      lens, lent, i

      found = .true.
      lens  = len_trim(string) - 1
      lent  = len_trim(test  )
      do i = 1, lent-lens
         if ( string .eq. test(i:i+lens) ) return
      enddo
      found = .false.

      end function find_string

      subroutine delwaq_user_walking_discharges ( nowst , wasteloads, notot , nosys , noseg ,
     +                                            itime , conc      , syname, lunrep)

      ! routine to handle walking discharges

      ! global declarations

      implicit none

      ! arguments declarations

      integer                             :: nowst                  ! number of wasteloads
      type(wasteload), pointer            :: wasteloads(:)          ! array of all wasteloads (structure)
      integer                             :: notot                  ! total number of substances
      integer                             :: nosys                  ! number of active substances
      integer                             :: noseg                  ! number of segments
      integer                             :: itime                  ! system time
      real                                :: conc(notot,noseg)      ! concentration array
      character(len=*)                    :: syname(notot)          ! substance names

      ! local variables

      logical, save                       :: first = .true.
      integer, save                       :: nowalk                 ! number of walking discharges
      integer, save                       :: next_time_in_file      ! next time to read the locations
      integer, save                       :: time_offset            ! time offset because of rewinding
      integer, save                       :: timestep               ! timestep, anticipate next time
      integer, save                       :: period                 ! period covered in the file
      integer, save                       :: nosegl                 ! number of segments per layer
      integer, save                       :: nolay                  ! number of layers
      integer, dimension(:,:), allocatable, save :: lgrid           ! matrix with segment numbers

      integer                             :: newsegment
      integer                             :: i
      integer                             :: id
      integer                             :: ierr
      integer                             :: lunrep, lunlga
      integer, save                       :: lunwlk
      integer                             :: ix, iy, iz, jz, offset
      integer                             :: mmax, nmax, noq1, noq2, noq3
      logical                             :: l_exi

      ! test if there are any walking discharges

      if ( first ) then
         first = .false.
         nowalk = 0

         inquire (file='walking.dat',exist=l_exi)
         if ( l_exi ) then
            write(lunrep,*)
            write(lunrep,2000)
            write(lunrep,2001)

            open( newunit=lunwlk , file='walking.dat' )
            read( lunwlk, * ) nowalk
            if ( nowalk > 0 ) then
               open( newunit=lunlga , file='walking.lga', access = 'stream', status = 'old' )
               read( lunlga ) mmax, nmax, nosegl, nolay, noq1, noq2, noq3

               ! check if the grids match

               if ( mod(noseg, nosegl) /= 0 ) then
                   write(lunrep,2002) noseg, nosegl
                   nowalk = 0
                   close( lunlga )
                   close( lunwlk )
                   return
               endif

               call dhnolay( nolay )

               allocate( lgrid(mmax,nmax) )
               read( lunlga ) lgrid
               close( lunlga )
            endif
         else
            write(lunrep,2005)
            return
         endif

         offset = 0
         do i = 1,nowalk
            read( lunwlk, *, iostat = ierr ) id, ix, iy, iz
            if ( id > 0 .and. id+offset <= nowst ) then
               if ( iz > 0 ) then
                  newsegment = lgrid(ix,iy) + nosegl * (iz-1)
                  wasteloads(id+offset)%loc%segnr = newsegment
               else
                  do jz = 1,nolay
                     newsegment = lgrid(ix,iy) + nosegl * (jz-1)
                     wasteloads(id+offset)%loc%segnr = newsegment
                     offset = offset + 1
                  enddo
               endif
            endif
            write(lunrep,*) id, ix, iy, iz
         enddo
         call reposition_file( lunwlk )

         call determine_times( lunwlk, nowalk, next_time_in_file, period, timestep )
         time_offset  = 0

         write( lunrep,2006)
      endif

      ! do not bother with this if there are no walking discharges
      if ( nowalk == 0 ) then
         return
      endif

      write( lunrep, * ) 'Time in file: ', next_time_in_file, itime, nowalk

      ! position the file pointer and read the information
      do
         if ( next_time_in_file <= itime ) then
            offset = 0
            do i = 1,nowalk
               read( lunwlk, *, iostat = ierr ) id, ix, iy, iz
               if ( id > 0 .and. id+offset <= nowst ) then
                  if ( iz > 0 ) then
                     newsegment = lgrid(ix,iy) + nosegl * (iz-1)
                     wasteloads(id+offset)%loc%segnr = newsegment
                  else
                     do jz = 1,nolay
                        newsegment = lgrid(ix,iy) + nosegl * (jz-1)
                        wasteloads(id+offset)%loc%segnr = newsegment
                        offset = offset + 1
                     enddo
                  endif
               endif
               write(lunrep,*) id, ix, iy, iz
            enddo

            read( lunwlk, *, iostat = ierr ) next_time_in_file
            if ( ierr /= 0 ) then
               call reposition_file( lunwlk )
               time_offset = time_offset + period
            endif

            next_time_in_file = next_time_in_file + time_offset

            if ( next_time_in_file + timestep > itime ) then
               exit
            endif
         else
            exit
         endif
      enddo

 2000 format (' extra functionality WALKING DISCHARGES')
 2001 format ('    walking discharges file found - walking.dat')
 2002 format ('    grid mismatch in LGA file (walking.lga): ',/,
     &        '    number of segments',i10,
     &        ' not a multiple of number of segments per layer:',i10)
 2003 format ('    unexpected end of file at simulation time: ', i10,/,
     &        '    time in walking.dat file:                  ', i10)
 2005 format (' No file <walking.dat> detected' )
 2006 format (' end extra functionality WALKING DISCHARGES')

      contains

      subroutine determine_times( lunwlk, nowalk, start_time, period, timestep )
      !
      ! Scan the file to determine the start time and the period
      ! Then reposition the pointer
      !
      integer :: lunwlk, nowalk, start_time, period, timestep

      integer :: i, next_time, dummy
      integer :: ierr

      read( lunwlk, * ) start_time

      ! Skip the second block
      do i = 1,nowalk
         read( lunwlk, * ) dummy, dummy, dummy, dummy
      enddo

      read( lunwlk, * ) next_time
      timestep = next_time - start_time

      ! Read until the end of the file
      do
         do i = 1,nowalk
            read( lunwlk, *, iostat = ierr ) dummy, dummy, dummy, dummy
            if ( ierr /= 0 ) then
               write( lunrep, 2004 ) next_time
               call srstop(1)
            endif
         enddo

         read( lunwlk, *, iostat = ierr ) next_time
         if ( ierr /= 0 ) then
            exit
         endif
      enddo

      period = next_time + timestep - start_time

      ! Reposition the file
      call reposition_file( lunwlk )

 2004 format ('   Unexpected end of file with walking discharges at tim
     &e = ',i10)
      end subroutine determine_times

      subroutine reposition_file( lunwlk )

      integer, intent(in) :: lunwlk

      integer :: i, dummy, nolines

      rewind( lunwlk )
      read( lunwlk, * ) nolines
      do i = 1,nolines
         read( lunwlk, * ) dummy, dummy, dummy
      enddo

      read( lunwlk, * ) dummy ! The first time - we already know that!

      end subroutine reposition_file

      end subroutine delwaq_user_walking_discharges

      end module delwaq_user_wasteloads
