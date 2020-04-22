!!  Copyright (C)  Stichting Deltares, 2012-2020.
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

      module Timers
      implicit none
!
!     Deltares | Deltares Software Centre
!
!     Created        : February 2008 by Leo Postma
!     Modified       : November 2010 by Leo Postma: a decent call tree in the reporting
!                                                   complete documentation in this source code
!
!     Function       : Handles everything to conveniently time a FORTRAN program
!
!         contains                 meaning
!         timini   ( )               : Should be called once first to initialize. Logical 'timon'
!                                      must be switched to .true. afterwards by the caller !
!         timinc   ( )               : Used internally if the arrays run out of space
!         timstrt(subrou,ihandl)     : Starts timing for this subroutine or program part.
!                                      'subrou' is a max. 40 character ID-string.
!                                      'ihandl' must be saved by the caller
!         timstrt(ihandl)            : See previous definition, however the subrou parameter is not
!                                      required.
!         timstop(ihandl)            : stops timing for this handle and accumulates the result
!         timdump(filename)          : writes the results to the report file 'filename'
!         tim_get_new_handle(subrou) : Define a new timer handle with name 'subrou'
!         tim_get_cpu(handle)        : Get the cpu time for 'handle'  
!         tim_get_cpu_inc(handle)    : Get the incremental cpu time for 'handle' (= latest interval 
!                                      between start and stop)
!         tim_get_wallclock(handle)  : Get the wall clock time for 'handle'  
!         tim_get_label(handle)      : Get the incremental wall clock time for 'handle' (= latest 
!                                      interval between start and stop)  
!         !      
!     Example:
!
!     *** in the highest level calling program: ***
!
!     use timers                     ! to make the logical 'timon' available  (before implicit none)
!         .......
!     integer(4) ithndl              ! handle to time this subroutine
!     data       ithndl / 0 /
!     call timini ( )
!     timon = .true.
!     call timstrt( "your main program name", ithndl )
!         .......
!     if ( timon ) then
!        call timstop ( ithndl )
!        call timdump ( ifnam(1:index(ifnam,".",.true.)-1)//'-timers.out' )
!     endif                          ! the file name is constructed here from the model file name
!
!     *** in each subroutine: ***
!
!     use timers                     ! to make the logical 'timon' available  (before implicit none)
!         .......
!     integer(4) ithndl              ! handle to time this subroutine
!     integer(4) ithnd2              ! handle to time a critical section of this subroutine
!     data       ithndl / 0 /        ! initialize it to zero for the first call
!     data       ithnd2 / 0 /        ! initialize it to zero for the first call
!     if ( timon ) call timstrt( "your subroutine name", ithndl )
!         .......
!        if ( timon ) call timstrt( "the name of your critical section", ithnd2 )
!         .......
!           timon = .false.
!                 OMP parallel section with subroutine calls that may contain timer calls
!           timon = .true.
!         .......
!        if ( timon ) call timstop ( ithnd2 )    ! here your critical section stops
!         .......
!     if ( timon ) call timstop ( ithndl )
!     return
!     end
!
!     NOTE1: You must take care that you do not leave your subroutine before you close its timer
!
!     NOTE2: There is no maximum to the number of subroutines or program parts that can be timed.
!            The maximum number of times that a timer for the same subroutine / program part
!            can appear in a different call tree (called context) is 99.
!
!     NOTE3: Since the call-tree is introduced, the computation time of a subroutine or program
!            part may be spilt up into parts corresponding to their different location in the call
!            tree. This may result in multiple lines for the same subroutine or program part in the
!            report file. Each line gives the computation time of the subroutine at that location
!            in the call tree. The total time in the subroutine or program part is then not printed.
!
!     NOTE4: You may start a timer with your own explanatory indentification string at any point
!            in your program, you only must take care that always a corresponding stop of the
!            timer occurs. So you are not limited to the timing of whole subroutines only.
!            The ease of use facilitates to easily split up a routine into several parts that are
!            each timed with an own timer to identify bottle necks within a routine.
!
!     NOTE5: Since the use of subroutines is only advantageous if a subroutine is called multiple
!            times, it is more efficient to time your subroutines from inside like in the example
!            above, than from outside in the calling program. An additional advantage of this
!            policy is that the location of your timing commands is limited to the top and the
!            bottom of your subroutine and not somewhere else throughout the code.
!
!     NOTE6: These timers are NOT treadsafe (yet), so always set 'timon' to .false. before you enter an
!            OMP parallel section. You switch timon to .true. after you left the parallel section.
!            (The instructions to switch 'timon' off and on cannot be nested)
!            Furthermore it is assumed that your handles are saved by the program parts, so you
!            must insert the 'save' instruction for the handle if that is not done automatically.
      interface timstrt
         module procedure timstrt_original
         module procedure timstrt_single_parameter
      end interface timstrt

      logical                             :: timon          !< is the timer switched on or off
      integer  ( 4), private              :: nohmax         !< current maximum size of the timer arrays
      integer  ( 4), private              :: nohandl        !< current highest timer handle
      integer  ( 4), private              :: handinc        !< increment of # of timer handles
      integer  ( 4), private              :: noshndl        !< current highest subroutine handle
      integer  ( 4), private              :: prevhnd        !< previous timer handle
      integer  ( 4), private              :: dlevel         !< current level in the call tree
      integer  ( 4), private              :: maxlvl         !< maximum level of call trees
      integer  ( 4), private, pointer     :: ntimcal(:) => null()    !< call frequency
      integer  ( 4), private, pointer     :: level  (:)     !< call level
      real     ( 8), private, pointer     :: cpstart(:)     !< to save cpu startimes
      real     ( 8), private, pointer     :: cptime (:)     !< to accumulate cpu times
      real     ( 8), private, pointer     :: cpinc (:)      !< incremental cpu times
      real     ( 8), private, pointer     :: wcstart(:)     !< to save wall clock startimes
      real     ( 8), private, pointer     :: wctime (:)     !< to accumulate wall clock times
      real     ( 8), private, pointer     :: wcinc (:)      !< incremental wall clock times
      
      character(40), private, pointer     :: tmsubnm(:)     !< name of the subroutine
      integer  ( 4), private, pointer     :: ncontxt(:)     !< number of contexts per subroutine
      integer  ( 4), private, pointer     :: context(:,:,:) !< call context
      integer  ( 4), private, pointer     :: ntim1(:)       !< call frequency
      integer  ( 4), private, pointer     :: levl1(:)       !< call level
      real     ( 8), private, pointer     :: cpst1(:)       !< to save cp startimes
      real     ( 8), private, pointer     :: cptt1(:)       !< to save cp times
      real     ( 8), private, pointer     :: cpin1 (:)      !< incremental cpu times
      real     ( 8), private, pointer     :: wcst1(:)       !< to save wc startimes
      real     ( 8), private, pointer     :: wctt1(:)       !< to save wc times
      real     ( 8), private, pointer     :: wcin1 (:)      !< incremental wall clock times
      character(40), private, pointer     :: tmsu1(:)       !< name of the subroutine
      integer  ( 4), private, pointer     :: ncon1(:)       !< nr of contexts per subroutine
      integer  ( 4), private, pointer     :: cont1(:,:,:)   !< context
      integer  ( 8), private              :: count          !< system clock count
      integer  ( 8), private              :: rate           !< ticks per second
      logical      , private, pointer     :: isactive(:)    !< flag indicating if a timer is started
      logical      , private, pointer     :: isactive1(:)    !< flag indicating if a timer is started

   contains

!***************
!                                      
   !> Should be called once first to initialize. Logical 'timon'
   !! must be switched to .true. afterwards by the caller !
   subroutine timini  ( )

      nohmax  =  10
      handinc =  10
      nohandl =   0
      noshndl =   0
      prevhnd =   0
      timon   = .false.
      dlevel  =   0
      maxlvl  =   0
      if (.not. associated(ntimcal)) then

         allocate ( ntimcal(     nohmax) )
         allocate ( level  (     nohmax) )
         allocate ( cpstart(     nohmax) )
         allocate ( cptime (     nohmax) )
         allocate ( cpinc  (     nohmax) )
         allocate ( wcstart(     nohmax) )
         allocate ( wctime (     nohmax) )
         allocate ( wcinc  (     nohmax) )
         allocate ( tmsubnm(     nohmax) )
         allocate ( ncontxt(     nohmax) )
         allocate ( context(99,3,nohmax) )
         allocate ( isactive(    nohmax) )
      endif

      ntimcal = 0
      level   = 0
      ncontxt = 0
      cpstart = 0d0
      cptime  = 0d0
      cpinc   = 0d0
      wcstart = 0d0
      wctime  = 0d0
      wcinc   = 0d0
      tmsubnm = ''
      ncontxt = 0
      context = 0
      isactive= .false.

      return
      end subroutine timini

!***************

      !> Used internally if the arrays run out of space
      subroutine timinc  ( )

      allocate ( ntim1(     nohmax+handinc) )
      allocate ( levl1(     nohmax+handinc) )
      allocate ( cpst1(     nohmax+handinc) )
      allocate ( cptt1(     nohmax+handinc) )
      allocate ( cpin1(     nohmax+handinc) )
      allocate ( wcst1(     nohmax+handinc) )
      allocate ( wctt1(     nohmax+handinc) )
      allocate ( wcin1(     nohmax+handinc) )
      allocate ( tmsu1(     nohmax+handinc) )
      allocate ( ncon1(     nohmax+handinc) )
      allocate ( cont1(99,3,nohmax+handinc) )
      allocate ( isactive1( nohmax+handinc) )
      ncon1 = 0

      ntim1(    1:nohmax) = ntimcal
      levl1(    1:nohmax) = level
      cpst1(    1:nohmax) = cpstart
      cptt1(    1:nohmax) = cptime
      cpin1(    1:nohmax) = cpinc
      wcst1(    1:nohmax) = wcstart
      wctt1(    1:nohmax) = wctime
      wcin1(    1:nohmax) = wcinc
      tmsu1(    1:nohmax) = tmsubnm
      ncon1(    1:nohmax) = ncontxt
      cont1(:,:,1:nohmax) = context
      isactive1(1:nohmax) = isactive

      deallocate ( ntimcal )
      deallocate ( level   )
      deallocate ( cpstart )
      deallocate ( cptime  )
      deallocate ( cpinc   )
      deallocate ( wcstart )
      deallocate ( wctime  )
      deallocate ( wcinc   )
      deallocate ( tmsubnm )
      deallocate ( ncontxt )
      deallocate ( context )
      deallocate ( isactive)

      ntimcal => ntim1
      level   => levl1
      cpstart => cpst1
      cptime  => cptt1
      cpinc   => cpin1
      wcstart => wcst1
      wctime  => wctt1
      wcinc   => wcin1
      tmsubnm => tmsu1
      ncontxt => ncon1
      context => cont1
      isactive=> isactive1

      nohmax  = nohmax + handinc

      return
      end subroutine timinc

!***************

      !> Starts timing for this subroutine or program part.
      !! 'subrou' is a max. 40 character ID-string.
      !! 'ihandl' must be saved by the caller
      subroutine timstrt_original ( subrou, ihandl )
      
         character(len=*), intent(in   ) :: subrou    !<  name of (part of) subroutine to monitor
         integer(4)      , intent(inout) :: ihandl    !<  handle of the timer
         integer(4)                      handle    !  handle of the timer
         integer(4)                      i         !  loop counter
         integer(4)                      ival(8)
         real   (4)                      time
   
         handle = 0
         if ( ihandl .eq. 0 ) then                              !  first time that the timer is called for
            noshndl = noshndl + 1                               !  this handle
            if ( noshndl .eq. nohmax ) call timinc ( )          !  allocate new batch of memory
            ihandl  = noshndl
         else                                                   !  find its occurence in the call trees
            do i = 1, ncontxt(ihandl)
               if ( context(i,1,ihandl) .eq. prevhnd ) then
                  handle = context(i,2,ihandl)
                  exit
               endif
            enddo
         endif
         if ( handle .eq. 0 ) then                              !  this is new call tree entry
            i               = ncontxt(ihandl) + 1               !  increase context counter for this ihandl
            ncontxt(ihandl) = i
            context(i,1,ihandl) = prevhnd                       !  save unique timer handle of the caller,
            nohandl = nohandl + 1                               !  to allow to find the calling context
            if ( nohandl .eq. nohmax ) call timinc ( )
            handle  = nohandl                                   !  make a new timer handle
            context(i,2,ihandl) = handle                        !  save this handle for this context
            context(1,3,handle) = ihandl
            tmsubnm(handle) = subrou                            !  save the ID of this timer
            ntimcal(handle) = 0                                 !  zero the accumulators
            cptime (handle) = 0.0d00
            wctime (handle) = 0.0d00
         endif
         dlevel  = dlevel + 1                                   !  level is 1 deeper than previous level
         level  (handle) = dlevel                               !  levels are only used to indent the
         maxlvl = max (maxlvl,dlevel)                           !  reported output
         prevhnd = handle                                       !  now this timer may become the caller
   
         call timstrt_single_parameter(handle)
   
         return
      !end subroutine timstrt
      end subroutine timstrt_original

      !> Define a new timer handle with name 'subrou'
      integer function tim_get_new_handle( subrou ) result(handle)
      
         character*(*), intent(in   ) :: subrou    !<  name of (part of) subroutine to monitor
         integer(4)                      ihandl    !<  timer handle 

   
         handle = 0
         noshndl = noshndl + 1                               !  this handle
         if ( noshndl .eq. nohmax ) call timinc ( )          !  allocate new batch of memory
         handle  = noshndl                                   !  make a new timer handle
         tmsubnm(handle) = subrou                            !  save the ID of this timer
         ntimcal(handle) = 0                                 !  zero the accumulators
         cptime (handle) = 0.0d00
         wctime (handle) = 0.0d00
         isactive(handle) = .false.
         return
      end function tim_get_new_handle

      !> start the timer for handle IHANDL
      subroutine timstrt_single_parameter ( handle )
   
         integer(4)   , intent(in) :: handle    !<  handle of the section

         integer(4)                      i         !  loop counter
         integer(4)                      ival(8)
         real   (4)                      time
   
   
         call cpu_time      (          time )                   !  this is straight forward timing
         cpstart(handle) = time
         call system_clock  ( count, rate )
         call date_and_time ( values = ival )
         wcstart(handle) = real( count, 8 ) / real( rate, 8 )
         ntimcal(handle) = ntimcal(handle) + 1
         isactive(handle) = .true.
   
         return
      end subroutine timstrt_single_parameter
      
!***************

      !> stops timing for this handle and accumulates the result
      subroutine timstop ( ihandl )

      integer(4)   , intent(in   ) :: ihandl    !<  handle of the subroutine
      integer(4)                      handle    !  handle of the timer
      integer(4)                      i         !  loop counter
      real   (8)                      stopt
      real   (4)                      time

      if (isactive(ihandl)) then
         isactive(ihandl) = .false.
         handle = ihandl
      else
         dlevel = dlevel-1                                      !  we return, decrease the level
         handle = -1
         do i = 1, ncontxt(ihandl)                              !  find the context of the timer handle
            if ( context(i,2,ihandl) .eq. prevhnd ) then        !  (prevhnd) that we close now
               handle  = prevhnd                                !  this is the handle that we close
               prevhnd = context(i,1,ihandl)                    !  we retrieve the calling handle from
               exit                                             !  context
            endif
         enddo
      endif

      if ( handle == -1 ) then
         write( *, * ) 'Programming error: unbalanced calls to timstart/timstop'
         write( *, * ) 'Found in the context of handle ', ihandl, ncontxt(ihandl)
         return
      endif

      call cpu_time ( time )
      cpinc(handle)  = time  - cpstart(handle)
      cptime(handle) = cptime(handle) + time  - cpstart(handle)

      call system_clock  ( count, rate )
      stopt = real( count, 8 ) / real( rate, 8 )
      wcinc(handle)  = stopt  - wcstart(handle)
      wctime(handle) = wctime(handle) + stopt - wcstart(handle)

      return
      end subroutine timstop

!***************

      !> writes the results to the report file 'filename'
      subroutine timdump ( afile )

      integer(4)    i                         !   loop accross timer handles
	  integer       lun
      character*(*) afile

      open(newunit=lun, file=afile, recl=100+maxlvl*5 )
      write(lun, '(a,98(a ))' ) ' nr.     times     cpu time      cpu    wall clock      wc', &
                                  '  level',('     ',i=3,maxlvl),' routine name'
      write(lun, '(a,98(i5))' ) '        called    in seconds      %     in seconds       %', &
                                  ( i, i=2,maxlvl)
      do i = 1, nohandl
         if ( level(i) .eq. -1 ) cycle
         call timline ( i, lun )
      enddo

      close ( lun )

      return
      end subroutine timdump

!***************

      recursive subroutine timline ( ihandl, lun)

      integer(4)    ihandl
      integer(4)    lun
      integer(4)    i, j, k
      real   (8)    cpfact, wcfact

      character(60) forchr
      data          forchr / '(i5,i11,x,D13.6,x,f7.2,x,D13.6,'  /

      cpfact = 100.0d00/cptime(1)
      wcfact = 100.0d00/wctime(1)
      write ( forchr(32:), '(i4,''x,f6.2,'',i4,''x,a40)'')' ) &
                                         (level(ihandl)-1)*5+2,(maxlvl-level(ihandl))*5+1
      write(lun, forchr ) ihandl,ntimcal(ihandl),cptime(ihandl),cptime(ihandl)*cpfact, &
                                          wctime(ihandl),wctime(ihandl)*wcfact,tmsubnm(ihandl)
      
      level(ihandl) = -1

      do i = ihandl+1, nohandl
         if ( level(i) .eq. -1 ) cycle
         j = context(1,3,i)
         do k = 1, ncontxt(j)
            if ( context(k,1,j) .eq. ihandl .and. context(k,2,j) .eq. i ) call timline ( i, lun )
         enddo
      enddo

      return
      end subroutine timline

!***************

      subroutine timfinalize  ( )

      implicit none
      integer :: ierr

      if ( associated ( ntimcal ) ) deallocate ( ntimcal )
      if ( associated ( level   ) ) deallocate ( level   )
      if ( associated ( cpstart ) ) deallocate ( cpstart )
      if ( associated ( cptime  ) ) deallocate ( cptime  )
      if ( associated ( wcstart ) ) deallocate ( wcstart )
      if ( associated ( wctime  ) ) deallocate ( wctime  )
      if ( associated ( tmsubnm ) ) deallocate ( tmsubnm )
      if ( associated ( ncontxt ) ) deallocate ( ncontxt )
      if ( associated ( context ) ) deallocate ( context )

      timon = .false.

      return
      end subroutine timfinalize

!***************
   
      !> Get the cpu time for 'handle'  
      real(8) function tim_get_cpu(handle)

      integer, intent(in) :: handle          !<  handle of the timer

      tim_get_cpu = cptime(handle)
      end function tim_get_cpu

      !>  Get the incremental cpu time for 'handle' (= latest interval 
      !!  between start and stop)
      real(8) function tim_get_cpu_inc(handle)

      integer, intent(in) :: handle          !<  handle of the timer

      tim_get_cpu_inc = cpstart(handle)
      end function tim_get_cpu_inc

      !> Get the wall clock time for 'handle'  
      real(8) function tim_get_wallclock(handle)

      integer, intent(in) :: handle          !<  handle of the timer

      tim_get_wallclock = wctime(handle)
      end function tim_get_wallclock

      !> Get the incremental wall clock time for 'handle' (= latest 
      !! interval between start and stop)  
      character(len=40) function tim_get_label(handle)
      integer, intent(in) :: handle          !<  handle of the timer
      
      tim_get_label = tmsubnm(handle)
      end function tim_get_label

      end module Timers
