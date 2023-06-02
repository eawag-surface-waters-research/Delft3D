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

      subroutine dlwq02 ( lun     , lchar   , filtype , nrftot  , nlines  ,
     &                    npoins  , dtflg1  , dtflg2  , nodump  , iopt    ,
     &                    noint   , iwidth  , dtflg3  , ndmpar  , ntdmps  ,
     &                    noraai  , ntraaq  , nosys   , notot   , nototp  ,
     &                    vrsion  , ioutpt  , nsegdmp , isegdmp , nexcraai,
     &                    iexcraai, ioptraai, ierr    , iwar    )

!       Deltares Software Centre

!>\file
!>             Reads integration method; monitoring areas/transects and timers
!>
!>             This routine reads:
!>                - the options for time strings (none, ddhhmmss or yydddhh)
!>                - integration method and type
!>                - start, stop and step of integration
!>                - nr and names and cell nr's of monitoring areas
!>                - nr and names and exchange surface nr's of monitoring transects
!>                - start, step and stop time of the monitoring file
!>                - start, step and stop time of the history file
!>                - start, step and stop time of the map file

!       Created           :   April    1988 by M.E. Sileon / L. Postma

!       Modified          : ?????           by Jan van Beek  : monitoring areas and stretches
!                             April    1996 by Leo Postma    : Version number support
!                             April    1997 by Rinze Bruinsma: Tokenized input data file reading added
!                                                              Reshaped by L. Postma
!                             April    2011 by Leo Postma    : Fortran 90 look and feel

!       Subroutine called : srstop  stops processing with return code
!                           dlwq0i  checks string agains allowed tokens
!                           dlwq0t  converts an absolute time string to seconds
!                           cnvtim  converts a 'DATE' integer to seconds
!                           conver  converts an array of 'DATE' integers to seconds
!                           dhopnf  opens files
!                           zoek    seaches a string in a set of strings
!                           readmp  reads dump area's, new input style
!                           rearaa  reads transects, new input style
!                           timer   reads a time
!                           check   end of data block

!       Logical units     : LUN(27) = unitnumber stripped DELWAQ input file
!                           LUN(29) = unitnumber formatted output file
!                           LUN(2) = unit intermediate file (system)
!                           LUN(4) = unit intermediate file (pointers)
!                           LUN(5) = unit intermediate file (timesteps)

      use m_zoek
      use m_srstop
      use m_dhopnf
      use rd_token     !   for the reading of tokens
      use subs02
      use partmem      !   for PARTicle tracking
      use fileinfo     !   a filename array in PART
      use alloc_mod
      use dlwq0t_data
      use timers       !   performance timers
      use m_sysi          ! Timer characteristics
      use m_cnvtim


      implicit none

!     parameters

!     kind           function         name                Descriptipon

      integer  ( 4), intent(inout) :: lun    (*)        !< array with unit numbers
      character( *), intent(inout) :: lchar  (*)        !< array with file names of the files
      integer  ( 4), intent(inout) :: filtype(*)        !< type of binary file
      integer  ( 4), intent(inout) :: nrftot (*)        !< number of function items
      integer  ( 4), intent(inout) :: nlines            !< cumulative record  space
      integer  ( 4), intent(inout) :: npoins            !< cumulative pointer space
      logical      , intent(  out) :: dtflg1            !< 'date'-format 1st timescale
      logical      , intent(  out) :: dtflg2            !< 'date'-format 2nd timescale
      integer  ( 4), intent(  out) :: nodump            !< number of monitoring points output
      integer  ( 4), intent(in   ) :: iopt (*)          !< array with valid integration options
      integer  ( 4), intent(in   ) :: noint             !< dimension of iopt
      integer  ( 4), intent(in   ) :: iwidth            !< width of the output file
      logical      , intent(  out) :: dtflg3            !< 'date'-format (F;ddmmhhss,T;yydddhh)
      integer  ( 4), intent(  out) :: ndmpar            !< number of dump areas
      integer  ( 4), intent(  out) :: ntdmps            !< total number segments in dump area
      integer  ( 4), intent(  out) :: noraai            !< number of raaien
      integer  ( 4), intent(  out) :: ntraaq            !< total number of exch. in raaien
      integer  ( 4), intent(in   ) :: nosys             !< number of transported substances
      integer  ( 4), intent(inout) :: notot             !< total number of substances
      integer  ( 4), intent(  out) :: nototp            !< notot inclusive of partcle substances
      real     ( 4), intent(in   ) :: vrsion            !< version number of this input
      integer  ( 4), intent(in   ) :: ioutpt            !< flag for more or less output
      integer  ( 4), pointer       :: nsegdmp (:)       !< number of volumes in this monitoring area
      integer  ( 4), pointer       :: isegdmp (:)       !< computational volume numbers
      integer  ( 4), pointer       :: nexcraai(:)       !< number of exchanges in this monitoring transect
      integer  ( 4), pointer       :: iexcraai(:)       !< exchange area numbers of the transect
      integer  ( 4), pointer       :: ioptraai(:)       !< option for the transects
      integer  ( 4), intent(inout) :: ierr              !< cumulative error   count
      integer  ( 4), intent(inout) :: iwar              !< cumulative warning count

!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     ITSTRT  INTEGER    1         INPUT   Simulation start time ( scu )
!     ITSTOP  INTEGER    1         INPUT   Simulation stop time ( scu )
!     ISFACT  INTEGER    1         INPUT   system clock in seconds
!     OTIME   REAL*8     1         INPUT   Julian offset of the real time

      integer         ierr2, iwar2             !  local error and warning variables
      real            aint                     !  to read the integration option
      integer         i, k, i2, ibrk           !  loop counters
      integer         nobrk                    !  number of brakpoints
      integer         ifound                   !  help variable for string search
      integer         idummy                   !  help variables to read tokens
      integer         itype                    !  help variables to read tokens
      character(255)  cdummy                   !  help variables to read tokens
      character( 8 )  date1, date2             !  help variables to read date strings
      integer         ierr_alloc               !  help variable to identify allocation errors
      integer         ioerr                    !  help variable for errors on open files
      integer         ibflag                   !  balances ?
      character( 20), pointer     :: duname(:)
      integer       , pointer     :: dmpbal(:)
      character( 20), pointer     :: raname(:)
      integer  (  4), allocatable :: iar   (:) !  integer workspace
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwq02", ithndl )

!       Initialisation of timers

      dtflg1 = .false.
      dtflg2 = .false.
      dtflg3 = .false.
      isflag = 0
      iposr  = 0
      ierr2  = 0
      iwar2  = 0
      alone  = .true.      ! not coupled with Delpar
      lchar(45) = ' '      ! so no Delpar input file

!     There should be at least one substance in the input.

      if ( notot .lt. 1 ) then
         write ( lunut , 2000 )
         call srstop(1)
      endif

!        Read timers

      if ( gettoken( itfact, ierr2 ) .gt. 0 ) goto 30
      if ( gettoken( date1 , ierr2 ) .gt. 0 ) goto 30
      if ( gettoken( date2 , ierr2 ) .gt. 0 ) goto 30
      write ( lunut , 2010 ) itfact

!        Day string for date1                                              for date2:

      if ( date1 .eq. 'DDHHMMSS' .or. date1 .eq. 'ddhhmmss' ) then
         dtflg1 = .true.
         isflag = 1
         write ( lunut , 2020 )
         if ( date2 .eq. 'DDHHMMSS' .or. date2 .eq. 'ddhhmmss' ) then    ! allowed
            dtflg2 = .true.
            write ( lunut , 2030 )
            if ( itfact .ne. 1 ) write ( lunut , 2040 )
         elseif ( date2 .eq. 'YYDDDHH'.or. date2 .eq.'yydddhh' ) then    ! not allowed
            write ( lunut , 2050 )
            ierr = ierr + 1
         elseif ( date2 .ne. ' ' ) then                                  ! or blank
            write ( lunut , 2060 ) date2
            ierr = ierr + 1
         endif

!        Year string for date1

      elseif ( date1 .eq. 'YYDDDHH' .or. date1 .eq. 'yydddhh' ) then
         dtflg1 = .true.
         dtflg3 = .true.
         isflag = 1
         write ( lunut , 2070 )
         if ( date2 .eq. 'YYDDDHH' .or. date2 .eq. 'yydddhh' ) then      ! allowed
            dtflg2 = .true.
            write ( lunut , 2080 )
            if ( itfact .ne. 1 ) write ( lunut , 2040 )
         elseif ( date2 .eq.'DDHHMMSS' .or. date2 .eq.'ddhhmmss' ) then  ! not allowed
            write ( lunut , 2090 )
            ierr = ierr + 1
         elseif ( date2 .ne. ' ' ) then                                  ! or blank
            write ( lunut , 2060 ) date2
            ierr = ierr + 1
         endif

!        or blank for date1

      elseif ( date1 .ne. ' ' ) then
         write ( lunut , 2100 ) date1
         ierr = ierr + 1
      endif

!        Read integration type

      if ( gettoken( aint  , ierr2 ) .gt. 0 ) goto 30
      intsrt = int( aint )
      intopt = int( aint*100.0 - intsrt*100 + 0.5 )
      if ( mod(intopt,10) .eq. 0 ) then
         intopt=intopt/10
         ibflag = 0
      else
         if ( mod(int(aint*1000.+0.5),100) .eq. 10 ) then
            intopt=intopt/10+8
         elseif ( mod(int(aint*1000.+0.5),100) .le. 20 ) then
            intopt=intopt/10+8+16
         else
            intopt=intopt/10+8+16+32
         endif
         ibflag = 1
      endif
      do i = 1 , noint
         if ( int( 10.*aint ) .eq. iopt(i) ) goto 10
      enddo
      write ( lunut , 2110 )
      ierr = ierr + 1
   10 write ( lunut , 2120 ) aint

!     No in-active substances for fast solver steady state

      if ( notot .ne. nosys .and. ( intsrt .eq. 17 .or. intsrt .eq. 18 ) ) then
         ierr = ierr + 1
         write ( lunut, 2130 )
      endif

!        Read optional keywords and start time of integration

      nototp = 0
      if ( gettoken( cdummy, idummy, itype, ierr2 ) .gt. 0 ) goto 30
      do while ( itype .eq. 1 )                                      ! read a collection of tokens
         call dlwq0i ( cdummy, intopt, lunut  , ierr2 )
         if ( btest(intopt,17) .and. alone ) then
            alone = .false.

!                Delpar in Delwaq

            if ( gettoken( cdummy, ierr2 ) .gt. 0 ) goto 30          ! get the input file name for particles
            lchar(45) = cdummy
            call rdfnam ( lunitp   , cdummy   , fnamep   , nfilesp  , 2       ,
     &                    1        , .false.  )
            call report_date_time  ( lunitp(2))
            call rdlgri ( nfilesp  , lunitp   , fnamep   )
            call rdccol ( nmaxp    , mmaxp    , lunitp(5), fnamep(5),
     &                    lgrid2   , xb       , yb       , lunitp(2))
            call part01 ( lgrid    , lgrid2   , xb       , yb       , dx      ,
     &                    dy       , area     , angle    , nmaxp    , mmaxp   )
            nolayp = layt
            call rdpart ( lunitp(1), lunitp(2), fnamep(1))
            write ( lunut, 2470 ) nosubs
            do i = 1, nosubs
               write ( lunut, '(i4,2x,a)' ) notot+i, substi(i)
               write ( lun(2) ) substi(i)
            enddo
            nototp = nosubs
            notot  = notot  + nototp
            call exit_alloc ( i2 )
         endif

         if ( ierr2 .eq. 0 ) then
            if ( gettoken( cdummy, idummy, itype, ierr2 ) .gt. 0 ) goto 30
         else                                                        ! it was no keyword but a time string
            call dlwq0t ( cdummy, itstrt, .false., .false., ierr2 )  ! returns start time in seconds since time offset,
            if ( itstrt .eq. -999 ) then                             ! max is about 68 year since time offset
               write ( lunut , 2140 ) trim(cdummy)
               goto 30
            endif
            if ( ierr2 .gt. 0 ) then
               write ( lunut , 2150 ) trim(cdummy)
               goto 30
            endif
            itype = 0                                                !  no further keywords expected
         endif
      enddo
      if ( itype .eq. 2 ) then
         itstrt = idummy                                             !  it was an integer for start time
         call cnvtim ( itstrt, 1      , dtflg1 , dtflg3 )    !  convert it to seconds
      endif
      if ( .not. alone ) then
         if ( itstrt .ne. itstrtp ) then
            write ( lunut, 2430 ) itstrt, itstrtp
            ierr = ierr + 1
         endif
      endif

!        Read stop time of integration

      if ( gettoken( cdummy, idummy, itype, ierr2 ) .gt. 0 ) goto 30
      if ( itype .eq. 1 ) then                                       !  a time string
         call dlwq0t ( cdummy, itstop, .false., .false., ierr2 )
         if ( itstop .eq. -999 ) then
            write ( lunut , 2140 ) trim(cdummy)
            goto 30
         endif
         if ( ierr2 .gt. 0 ) then
            write ( lunut , 2150 ) trim(cdummy)
            goto 30
         endif
      else                                                           !  an integer for stop time
         itstop = idummy
         call cnvtim ( itstop , 1      , dtflg1 , dtflg3 )
      endif
      if ( .not. alone ) then
         if ( itstop .ne. itstopp ) then
            write ( lunut, 2440 ) itstop, itstopp
            ierr = ierr + 1
         endif
      endif
      if ( itstrt .lt. 0 ) then
         write ( lunut , 2155 ) itstrt
         ierr = ierr+1
      endif
      if ( itstrt .gt. itstop ) then
         write ( lunut , 2160 ) itstop, itstrt
         ierr = ierr+1
      endif
      if ( dtflg1 ) then
         write ( lunut, 2170)
     &           itstrt /31536000       , mod(itstrt ,31536000)/86400,
     &           mod(itstrt ,86400)/3600, mod(itstrt ,3600)/60       ,
     &           mod(itstrt ,60)        ,
     &           itstop /31536000       , mod(itstop ,31536000)/86400,
     &           mod(itstop ,86400)/3600, mod(itstop ,3600)/60       ,
     &           mod(itstop ,60)
      else
           write ( lunut , 2180 ) itstrt, itstop
      endif
      if ( ( intsrt .gt.  5 .and. intsrt .lt. 10 ).or.            ! stationary solvers
     &       intsrt .eq. 17 .or.  intsrt .eq. 18       ) then
         idt = itstop-itstrt
         if ( intsrt .eq. 8 .or.  intsrt .eq.  9 ) then
            if ( gettoken( itstop, ierr2 ) .gt. 0 ) goto 30
            if ( gettoken( imstop, ierr2 ) .gt. 0 ) goto 30
            write ( lunut , 2190 ) itstop, imstop
         endif
         goto 20
      endif

!        Now the time step size

      if ( gettoken( itype, ierr2 ) .gt. 0 ) goto 30
      write ( lunut , 2200 ) itype
      select case ( itype )
         case ( 0 )                                             !    constant time step
            if ( gettoken( idt   , ierr2 ) .gt. 0 ) goto 30
            if ( dtflg1 ) then
               call cnvtim ( idt   , 1      , dtflg1 , dtflg3 )
               write ( lunut , 2210 )  idt /31536000       , mod(idt ,31536000)/86400,
     &                                 mod(idt ,86400)/3600, mod(idt ,3600)/60       ,
     &                                 mod(idt ,60)
            else
               write ( lunut , 2220 ) idt
            endif
            if ( idt .le. 0 ) then
               write ( lunut , 2230 ) idt
               ierr = ierr+1
            endif
            if ( .not. alone ) then
               if ( idt .ne. idelt ) then
                  write ( lunut, 2450 ) idt, idelt
                  ierr = ierr + 1
               endif
            endif

         case ( 1 )                                             !    time varying time step
            if ( .not. alone ) then
               write ( lunut, 2460 )
               ierr = ierr + 1
            endif
            if ( gettoken( nobrk , ierr2 ) .gt. 0 ) goto 30
            write ( lunut, 2240 ) nobrk
            allocate ( iar(nobrk*2), stat=ierr_alloc )
            if ( ierr_alloc .ne. 0 ) then
               write ( lunut , 2250 ) ierr_alloc
               ierr = ierr + 1
               goto 30
            endif
            do k = 1, nobrk*2
               if ( gettoken( iar(k), ierr2 ) .gt. 0 ) goto 30
            enddo
            nrftot (1) = 1
            nlines     = nlines + 2
            npoins     = npoins + 1 + 3
            write ( lun(4) ) -1, ( 0 , k=1,3 )
            if ( dtflg1 ) then
               call conver ( iar , nobrk*2 , 1 , dtflg1 , dtflg3 )
               if ( ioutpt .ge. 4 ) then
                  write ( lunut , 2260 )
                  write ( lunut , 2270 )
     &                 ( iar(k)/31536000       , mod(iar(k),31536000)/86400,
     &                   mod(iar(k),86400)/3600, mod(iar(k),3600)/60       ,
     &                   mod(iar(k),60)        ,      k = 1,nobrk*2        )
               else
                  write ( lunut , 2280 )
               endif
            else
               if ( ioutpt .ge. 4 ) then
                  write ( lunut , 2260 )
                  write ( lunut , 2290 ) ( iar(k), k = 1,nobrk*2 )
               else
                  write ( lunut , 2280 )
               endif
            endif
            if ( iar(1) .gt. itstrt ) then
               write ( lunut , 2300 ) iar(1) , itstrt
               ierr = ierr+1
            endif
            call dhopnf  ( lun(5 ) , lchar(5 ) , 5      , 1     , ioerr )
            do ibrk = 1,nobrk*2,2
               write ( lun(5) ) iar(ibrk), float (iar(ibrk+1))
               if ( iar(ibrk+1) .le. 0 ) then
                  write ( lunut , 2310 ) iar(ibrk+1)
                  ierr = ierr+1
               endif
               if ( ibrk .eq. 1 ) cycle
               if ( iar(ibrk) .le. iar(ibrk-2) ) then
                  write ( lunut , 2320 ) iar(ibrk) , iar(ibrk-2)
                  ierr = ierr+1
               endif
            enddo
            close ( lun(5) )
         case default                                           !    option not implemented
            write ( lunut , 2330 )
            ierr = ierr + 1
      end select

!     Copy timers data to dlwqt0_data
      dlwq0t_itstrt = itstrt
      dlwq0t_itstop = itstop

!     Read monitoring area's

   20 nodump = 0
      nullify(duname)
      if ( vrsion .le. 4.29 ) then

!             allocate memory

         if ( gettoken( ndmpar, ierr2 ) .gt. 0 ) goto 30
         write ( lunut , 2340 ) ndmpar
         allocate ( duname(ndmpar), stat=ierr2 )
         if ( ierr2 .ne. 0 ) then
            write ( lunut , 2350 ) ierr2
            goto 30
         endif
         allocate ( nsegdmp(ndmpar),dmpbal(ndmpar),stat=ierr2 )
         if ( ierr2 .ne. 0 ) then
            write ( lunut , 2360 ) ierr2
            goto 30
         endif
         ntdmps = ndmpar
         allocate ( isegdmp(ntdmps),stat=ierr2 )
         if ( ierr2 .ne. 0 ) then
            write ( lunut , 2360 ) ierr2
            goto 30
         endif

!             read the monitoring information

         do k = 1 , ndmpar
            dmpbal(k)  = 1
            nsegdmp(k) = 1
            if ( gettoken( isegdmp(k), ierr2 ) .gt. 0 ) goto 30
            if ( gettoken( duname (k), ierr2 ) .gt. 0 ) goto 30
         enddo

!             check if name is unique

         do i = 1 , ndmpar
            if ( duname(i) .eq. ' ' ) write ( duname(i), 2370 ) isegdmp(i)
            do i2 = 1 , i-1
               call ZOEK( duname(i), 1, duname(i2:), 20, ifound )
               if ( ifound .gt. 0 ) then
                  write( lunut, 2380 ) duname(i)
                  ierr = ierr + 1
               endif
            enddo
         enddo

!             write output

         if ( ioutpt .ge. 3 ) then
            write ( lunut , 2390 )
            write ( lunut , 2400 ) ( isegdmp(k), duname(k), k=1, ndmpar)
         else
            write ( lunut , 2410 )   ndmpar
         endif
      else

!             new input processssing

         ierr2 = 0
         call readmp ( lun    , lchar  , filtype, duname , nsegdmp,
     &                 isegdmp, dmpbal , ndmpar , ntdmps , ioutpt ,
     &                 ierr2  , iwar   )
         if ( ierr2 .ne. 0 ) goto 30
      endif

      if ( ndmpar .gt. 0 ) then
         write ( lun(2) ) ( duname(k), k=1, ndmpar )
         write ( lun(2) ) ( dmpbal(k), k=1, ndmpar )
      endif
      if ( associated(duname) ) deallocate(duname)
      if ( associated(dmpbal) ) deallocate(dmpbal)

!     Read transects

      if ( vrsion .le. 4.29 ) then
         noraai = 0
         ntraaq = 0
      else
         ierr2 = 0
         nullify(raname)
         call rearaa ( lun     , lchar   , filtype , raname  , nexcraai,
     &                 iexcraai, ioptraai, noraai  , ntraaq  , ioutpt  ,
     &                 ierr2   , iwar    )
         if ( ierr2 .ne. 0 ) goto 30
         if ( noraai .gt. 0 .and. ibflag .eq. 0 ) then
            write ( lunut,2420 )
            iwar2 = iwar2 + 1
            intopt = ibset(intopt,3)
            intopt = ibset(intopt,4)
         endif
         if ( noraai .gt. 0 .and. ierr .eq. 0 ) then
            write ( lun(2) ) ( raname(k), k=1, noraai )
         endif
         if ( associated(raname) ) deallocate(raname)
      endif

!       Read timings

      if ( ( intsrt .lt.  6 .or. intsrt .gt. 9  ) .and.
     &     ( intsrt .lt. 17 .or. intsrt .gt. 18 )        ) then
         call timer  ( dtflg1 , imstrt , imstop , imstep , 1      ,
     &                 dtflg3 , ierr2  )
         if ( ierr2 .gt. 0 ) goto 30

         call timer  ( dtflg1 , idstrt , idstop , idstep , 2      ,
     &                 dtflg3 , ierr2  )
         if ( ierr2 .gt. 0 ) goto 30

         call timer  ( dtflg1 , ihstrt , ihstop , ihstep , 3      ,
     &                 dtflg3 , ierr2  )
         if ( ierr2 .gt. 0 ) goto 30
      endif
      ierr2 = 0

!        Check number of data in inputfile

   30 continue
      if ( ierr2 .ne. 0 ) ierr = ierr + 1
      if ( ierr2 .eq. 3 ) call srstop(1)
      call check  ( cdummy , iwidth , 2      , ierr2  , ierr   )
      iwar = iwar + iwar2
      if ( timon ) call timstop( ithndl )
      return

!       Output formats

 2000 format ( /' ERROR: No substances have been specified.')
 2010 format (//' Factor between two clocks :  ',I10 )
 2020 format (  ' System clock in date-format  DDHHMMSS '   )
 2030 format (  ' Auxiliary timer in date-format DDHHMMSS '  )
 2040 format ( /,' The auxiliary timer is not equal to the system timer.',
     &          /' All the process fluxes will be scaled from the',
     &           ' auxiliary timer to the system timer.' )
 2050 format ( /' ERROR !!!! Auxiliary timer YYDDDHH-format ! ****'/
     &          ' This option is invalid in combination with '/
     &          ' system clock in DDHHMMSS format !')
 2060 format ( /' ERROR Auxiliary timer format not recognised :',A)
 2070 format (  ' System clock in date-format  YYDDDHH '    )
 2080 format (  ' Auxiliary timer in date-format YYDDDHH '   )
 2090 format ( /' ERROR !!!! Auxiliary timer DDHHMMSS-format ! ****'/
     &          ' This option is invalid in combination with '/
     &          ' system clock in YYDDDHH format !')
 2100 format ( /' ERROR System timer format not recognised :',A)
 2110 format ( /' ERROR !!!! Invalid integration option ! ****')
 2120 format ( /' Integration option :               ',F5.2)
 2130 format ( /' ERROR No in-active substances allowed for',
     &          ' fast-solver steady state scheme')
 2140 format ( /' ERROR: Absolute timer does not fit in timer format :',A,/
     &          ' Is your T0 setting in block #1 correct?'/,
     &          ' Allowed difference with T0 is usually ca. 68 years.' )
 2150 format ( /' ERROR: String is not recognised as a keyword and',
     &          ' it is not a valid absolute timer :',A)
 2155 format (  ' ERROR: Start time (',I10,') absolute timer is less than zero',
     &          ' or auxiliary timer is set before T0.'/
     &          '        This is not supported!' )
 2160 format (  ' ERROR, Stop time (',I10,') smaller than start time(',
     &                                I10,').' )
 2170 format ( ' Start of simulation :',I2,'Y-',I3,'D-',I2,'H-',I2,
     &           'M-',I2,'S.',
     &        /' End of simulation   :',I2,'Y-',I3,'D-',I2,'H-',I2,
     &           'M-',I2,'S.')
 2180 format (  ' Start of simulation :        ' ,I10 ,
     &         /' End of simulation   :        ' ,I10 )
 2190 format (  ' Maximum number of iterations:' ,I10 ,
     &         /' Stop iteration after rel. difference smaller than',
     &          ' 1.0E-',I2,'.')
 2200 format (  ' Selected option for time step size : ',I2)
 2210 format (  ' Integration time stepsize is :',
     &            I2,'Y-',I3,'D-',I2,'H-',I2,'M-',I2,'S.')
 2220 format (  ' Integration time stepsize is :' ,I9 )
 2230 format ( /' ERROR, invalid time step size:',I8 )
 2240 format (  ' Variable time step with number of breakpoints is ',i8 )
 2250 format (  /,' ERROR. allocating memory for variable timestep:',I4)
 2260 format (  ' Breakpoint          Timestep ',/)
 2270 format (    I2,'Y-',I3,'D-',I2,'H-',I2,'M-',I2,'S ',3X,
     &            I2,'Y-',I3,'D-',I2,'H-',I2,'M-',I2,'S.')
 2280 format (  ' Variable timestep. Information will be printed for ',
     &                                   'output option 4 or higher !' )
 2290 format (    I10,10X,I10 )
 2300 format ( /' ERROR',I10,' larger than start time:',I10 )
 2310 format ( /' ERROR invalid time step size:',I10)
 2320 format ( /' ERROR',I10,' smaller than ',I10,' descending order !')
 2330 format (  ' ERROR !!!! This option is not implemented !!!')
 2340 format ( /  I4,' monitoring points specified:' )
 2350 format (  /,' ERROR. allocating memory for monitor names:',I4)
 2360 format (  /,' ERROR. allocating memory for monitor segments:',I4)
 2370 format (    'Segment',I5 )
 2380 format (  /,' ERROR. observation ID not unique:',A)
 2390 format (  ' Segment number    Identification '  )
 2400 format (    I8,11X,A20 )
 2410 format ( /  I4,' monitoring points specified.'/
     &          ' Information will be printed for output option 3 ',
     &                                                'or higher !' )
 2420 format ( /' WARNING: Transects used without balance option specified,'
     &         /' Balances automaticaly switched on!')
 2430 format ( /' ERROR: DELWAQ start time:',I10,' not equal to DELPAR start time:',I10 )
 2440 format ( /' ERROR: DELWAQ stop time :',I10,' not equal to DELPAR stop time :',I10 )
 2450 format ( /' ERROR: DELWAQ time step :',I10,' not equal to DELPAR time step :',I10 )
 2460 format ( /' ERROR: DELWAQ time step is variable. DELPAR does not support variable step sizes.' )
 2470 format (  ' The following ',i3,' DELPAR substances are added as passive substances to DELWAQ.' )

      end
