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

      subroutine dlwq04 ( lun     , lchar   , filtype , nrftot  , nrharm  ,
     &                    ilflag  , dtflg1  , iwidth  , intsrt  , dtflg3  ,
     &                    vrsion  , ioutpt  , nsegdmp , isegdmp , nexcraai,
     &                    iexcraai, ioptraai, gridps  , ierr    , iwar    ,
     &                    has_hydfile       , nexch   )

!       Deltares Software Centre

!>\file
!>            Reads flow dimensions and pointers and all transport information
!>
!>            This routine reads:
!>               - the 3 dimensions of exchange surfaces in 3 directions
!>               - the 4th dimension of exchanges in the water bed
!>               - the number of additional diffusion arrays
!>               - per substance the array entry that applies (0 is none)
!>               - the number of additional velocity arrays
!>               - per substance the array entry that applies
!>               - the exchange surfaces table (from-to table)
!>               - information on the time series of dispersions
!>               - information on the time series of areas
!>               - information on the time series of flows
!>               - information on the time series of additional velocities
!>               - information on the time series of from- and to lengthes


!       Subroutines called : BOUND
!                            OPT0
!                            OPT1
!                            OPT2
!                            SCALE
!                            CHECK
!                            DHOPNF
!                            RDTOK1 tokenized data reading

!       Logical units      : LUN(27) = unit stripped DELWAQ input file
!                            LUN(29) = unit formatted output file
!                            LUN( 2) = unit intermediate file (system)
!                            LUN( 3) = unit intermediate file (harmonics)
!                            LUN( 4) = unit intermediate file (pointers)
!                            LUN( 7) = unit intermediate file (volumes)
!                            LUN( 8) = unit intermediate file ('to-from')
!                            LUN( 9) = unit intermediate file (dispersions
!                            LUN(10) = unit intermediate file (areas)
!                            LUN(11) = unit intermediate file (flows)
!                            LUN(12) = unit intermediate file (velocities)
!                            LUN(13) = unit intermediate file (lengths)

      use m_zoek
      use m_srstop
      use m_dhopnf
      use Grids        !   for the storage of contraction grids
      use rd_token     !   for the reading of tokens
      use pointr_mod
      use partmem
      use timers       !   performance timers
      use m_sysn          ! System characteristics


      implicit none

!     Parameters         :

!     kind           function         name                Descriptipon

      integer  ( 4), intent(inout) :: lun    (*)        !< array with unit numbers
      character( *), intent(inout) :: lchar  (*)        !< array with file names of the files
      integer  ( 4), intent(inout) :: filtype(*)        !< type of binary file
      integer  ( 4), intent(inout) :: nrftot (*)        !< number of function items
      integer  ( 4), intent(inout) :: nrharm (*)        !< number of harmonic items
      integer  ( 4), intent(  out) :: ilflag            !< length flag
      logical      , intent(in   ) :: dtflg1            !< 'date'-format 1st timescale
      integer  ( 4), intent(in   ) :: iwidth            !< width of the output file
      integer  ( 4), intent(in   ) :: intsrt            !< integration option
      logical      , intent(in   ) :: dtflg3            !< 'date'-format (F;ddmmhhss,T;yydddhh)
      real     ( 4), intent(in   ) :: vrsion            !< version number of this input
      integer  ( 4), intent(in   ) :: ioutpt            !< flag for more or less output
      integer  ( 4), intent(in   ) :: nsegdmp (*)       !< number of volumes in this monitoring area
      integer  ( 4), intent(inout) :: isegdmp (*)       !< computational volume numbers
      integer  ( 4), intent(in   ) :: nexcraai(*)       !< number of exchanges in this monitoring transect
      integer  ( 4), intent(in   ) :: iexcraai(*)       !< exchange area numbers of the transect
      integer  ( 4), intent(in   ) :: ioptraai(*)       !< option for the transects
      type(GridPointerColl)           GridPs            !< Collection of grid pointers
      integer  ( 4), intent(inout) :: ierr              !< cumulative error   count
      integer  ( 4), intent(inout) :: iwar              !< cumulative warning count
      logical      , intent(in)    :: has_hydfile       !< if true, much information comes from the hyd-file
      integer  ( 4), dimension(*), intent(in) :: nexch  !< nmber of exchanges from the hyd-file

!     COMMON BLOCK  :

!     integer :: noseg    !  number of elements
!     integer :: nolay    !  number of layers in the water
!     integer :: nseg2    !  number of bottom elements
!     integer :: nosys    !  number of active systems
!     integer :: nodisp   !  number of dispersion arrays
!     integer :: novelo   !  number of velocity arrays
!     integer :: noq1     !  number of exch. 1st direction
!     integer :: noq2     !  number of exch. 2nd direction
!     integer :: noq3     !  number of exch. 3rd direction
!     integer :: noq4     !  number of exch. bottom direction
!     integer :: noq      !  number of exchanges
!     integer :: nobnd    !  number of boundaries
!     integer :: jtrack   !  number of codiagonals
!     integer :: ndmpar   !  Number of dump area's for balance output
!     integer :: ndmpq    !  Number of exchanges of interest for balance
!     integer :: ndmps    !  Number of segments of interest for balance
!     integer :: ntdmpq   !  Number of times exchanges contribute to balance
!     integer :: ntdmps   !  Number of times segments contribute to balance
!     integer :: noraai   !  Number of raaien
!     integer :: ntraaq   !  Total number of times exchanges contribute to raai
!     integer :: nomat    !  Size of the fast solvers matrix
!     integer :: mmax     !  Number of columns in regular grid
!     integer :: nmax     !  Number of rows in regular grid
!     integer :: kmax     !  Number of layers in regular grid

!     Locals

      integer  ( 4)   nosss     !  number of volumes inclusive of bed volumes
      logical         volume    !  if true, computed volumes
      logical         disper    !  if true, dispersion
      real     ( 4)   adummy    !  real zero
      integer  ( 4)   idummy    !  integer zero
      character(255)  cdummy    !  dummy character space
      integer  ( 4)   idum      !  multi purpose dummy variable
      integer  ( 4)   ifact     !  factor between clocks ( 1 in the case of transport )
      integer  ( 4)   ierr2     !  local error count
      integer  ( 4)   iwar2     !  local warning count
      integer  ( 4)   nosegl    !  number of volumes per layer
      integer  ( 4)   noq12     !  noq1 + noq2 (number of horizontal exchanges)
      integer  ( 4)   noq34     !  noq3 + noq4 (number of vertical exchanges)
      integer  ( 4)   noqt      !  total number of exchanges (water and bed)
      integer  ( 4)   i, j, k   !  loop counters
      integer  ( 4)   ifound    !  help variable to find things
      integer  ( 4)   iopt      !  option for type of input (2 = tabular input)
      integer  ( 4)   iopt1     !  option for file type (1 = this file etc.)
      integer  ( 4)   itype     !  the type of token that was presented
      logical         regular   !  if .true. indicates presence of a regular grid

!     Local arrays

      character(20), allocatable :: dispnam(:)       !  dispersion names
      integer  ( 4), pointer     :: ipnt   (:,:)     !  room for the 'from-to' pointer table
      real     ( 4), allocatable :: rwork  (:,:)     !  room for tabular input option
      integer  ( 4)                 idisp  (nosys)   !  dispersion number per substance
      integer  ( 4)                 ivelo  (nosys)   !  velocity number per substance
      real     ( 4)                 factor ( 5 )     !  scale factor tabular input
      integer  ( 4), pointer     :: cellpnt(:)       !  backpointer noseg to mnmaxk
      integer  ( 4), pointer     :: flowpnt(:)       !  backpointer noq to 3*mnmaxk-mnmax
      real     ( 4)                 length (3,1)     !  lengthes per direction
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwq04", ithndl )

      nosss  = noseg + nseg2
      iposr  = 0
      volume = .false.
      adummy = 0.0
      idummy = 0
      ifact  = 1
      ierr2  = 0
      iwar2  = 0

!        Read exchange dimensions of the system (NOQ1,NOQ2,NOQ3)

      if ( has_hydfile ) then
         noq1 = nexch(1)
         noq2 = nexch(2)
         noq3 = nexch(3)
      else
         regular = .false.
         if ( gettoken( cdummy, noq1, itype, ierr2 ) .gt. 0 ) goto 100
         if ( itype .eq. 1 ) then
            if ( cdummy(1:12) .eq. 'REGULAR_GRID' ) then
               regular = .true.
               if ( gettoken( noq1, ierr2 ) .gt. 0 ) goto 100
            else
               ierr2 = 1
               goto 100
            endif
         endif
         if ( gettoken( noq2, ierr2 ) .gt. 0 ) goto 100
         if ( gettoken( noq3, ierr2 ) .gt. 0 ) goto 100
      endif

      noq = noq1 + noq2 + noq3

!        These 2 options use a regular grid with full matrix.

      if ( intsrt .eq. 19 .or. intsrt .eq. 20 .or. regular ) then
         nmax = noq1
         mmax = noq2
         kmax = noq3
         write ( lunut , 2000 ) nmax, mmax, kmax
         GridPs%Pointers(1)%nolay = kmax
         nolay = kmax
      else
         nmax = 0
         mmax = 0
         kmax = 0
         write ( lunut , 2010 ) noq1, noq2, noq3, noq

!        detect number of layers (only structured sigma or z model, otherwise KMAX=0)

         if ( nolay .le. 1 ) then
            if ( noq3 .gt. 0 ) then
               kmax = 0
               nosegl = noseg - noq3
               if ( nosegl .gt. 0 ) then
                  nolay = noseg/nosegl
                  if ( nolay*nosegl .eq. noseg ) then
                     kmax  = nolay
                  else
                     nolay = 1
                  endif
               endif
            else
               kmax = 1
               nolay  = 1
               nosegl = noseg
            endif
         else
            kmax = nolay
         endif
      endif
      if ( .not. alone ) then
         if ( noq .ne. noqp ) then
            write (lunut, 2020 ) noqp
            ierr = ierr + 1
         endif
      endif
      noq4 = 0
      if ( nseg2 .ne. 0 ) then
         noq4 = nseg2 + noseg/nolay
         noq4 = noq4 * 2
         write ( lunut , 2040 ) noq4
      endif

!        Read number of additional dispersion arrays NODISP

      if ( gettoken( nodisp, ierr2 ) .gt. 0 ) goto 100
      write ( lunut, 2050 ) nodisp
      idisp = 0
      if ( nodisp .gt. 0 ) then
         if ( vrsion .gt. 4.02 ) then      !    they all get a name and if blank a default name
            allocate ( dispnam(nodisp) )   !    'Dispersion nnnn'
            do i = 1, nodisp
               if ( gettoken( dispnam(i), ierr2 ) .gt. 0 ) goto 100
               if ( dispnam(i) .eq. ' ' ) write ( dispnam(i), 2060 ) i
               call ZOEK( dispnam(i), i-1, dispnam, 20, ifound )
               if ( ifound .gt. 0 ) then
                  write( lunut, 2070 ) dispnam(i)
                  ierr = ierr + 1
               endif
            enddo
            if ( ioutpt .ge. 2 ) then
               write ( lunut, 2080 ) ( i, dispnam(i), i=1,nodisp )
            else
               write ( lunut, 2090 )
            endif
            write ( lunut, *    )
            write ( lun(2) ) ( dispnam(i), i=1, nodisp)
            deallocate ( dispnam )
         endif
         do i = 1, nosys     !   read which dispersion array applies (0=none) for each subst.
            if ( gettoken( idisp(i), ierr2 ) .gt. 0 ) goto 100
            if ( idisp(i) .gt. nodisp) then
               write ( lunut , 2100 ) idisp(i), nodisp
               ierr = ierr + 1
            endif
         enddo
      endif

!        Read number of additional velocity arrays NOVELO in exactly
!                   the same way (could probably be better 1 code)

      if ( gettoken( novelo, ierr2 ) .gt. 0 ) goto 100
      write ( lunut , 2110 ) novelo
      ivelo = 0
      if ( novelo .gt. 0 ) then
         if ( vrsion .gt. 4.02 ) then      !
            allocate ( dispnam(novelo) )   !
            do i = 1, novelo
               if ( gettoken( dispnam(i), ierr2 ) .gt. 0 ) goto 100
               if ( dispnam(i) .eq. ' ' ) write ( dispnam(i), 2120 ) i
               call ZOEK( dispnam(i), i-1, dispnam, 20, ifound )
               if ( ifound .gt. 0 ) then
                  write( lunut, 2130 ) dispnam(i)
                  ierr = ierr + 1
               endif
            enddo
            if ( ioutpt .ge. 2 ) then
               write ( lunut, 2080 ) ( i, dispnam(i), i=1,novelo )
            else
               write ( lunut, 2090 )
            endif
            write ( lunut, *    )
            write ( lun(2) ) ( dispnam(i), i=1, novelo)
            deallocate ( dispnam )
         endif
         do i = 1, nosys     !   read which dispersion array applies (0=none) for each subst.
            if ( gettoken( ivelo(i), ierr2 ) .gt. 0 ) goto 100
            if ( ivelo(i) .gt. novelo) then
               write ( lunut , 2100 ) ivelo(i), novelo
               ierr = ierr + 1
            endif
         enddo
      endif
!           write a report if sensible and write binary file
      if ( (nodisp .gt. 0 .or. novelo .gt. 0) .and. ioutpt .ge. 2  )
     &          write ( lunut, 2140 ) ( i, idisp(i), ivelo(i), i = 1, nosys )
      write ( lun(2) ) idisp
      write ( lun(2) ) ivelo
!           a very obvious (and rude) check on correctness
      if ( noq1 .lt. 0 .or. noq2   .lt. 0 .or. noq3   .lt. 0 .or.
     &     noq  .eq. 0 .or. nodisp .lt. 0 .or. novelo .lt. 0 ) then
           write ( lunut , 2150 )
           ierr = ierr+1
      endif

!        Read option variable for input mode

      if ( has_hydfile ) then
         iopt1 = 0
      else
          if ( gettoken( iopt, ierr2 ) .gt. 0 ) goto 100
          write ( lunut , 2170 ) iopt
          noqt = noq
          if ( iopt .eq. 2 ) goto 10

!***************  first type of input ******************

!        Read exchange pointers

         if ( gettoken( iopt1, ierr2 ) .gt. 0 ) goto 100
         write ( lunut , 2180 )  iopt1

         if ( intsrt .eq. 19 .or. intsrt .eq. 20 .or. regular ) then  !        Regular grid
            call opt1 ( iopt1   , lun     , 8      , lchar  ,  filtype ,
     &                  dtflg1  , dtflg3  , 0      , ierr2  ,  iwar    ,
     &                  .false. )
            if ( ierr2  .gt. 0 ) goto 100
            noqt = noq4
            call pointr ( lun    , lchar  , noseg  , nmax   , mmax   ,
     &                    kmax   , noq    , noq1   , noq2   , noq3   ,
     &                    noqt   , nobnd  , ipnt   , intsrt , iopt1  ,
     &                    jtrack , ioutpt , iwidth , GridPs , cellpnt,
     &                    flowpnt, ierr   , iwar    )
         endif
      endif
      if ( has_hydfile .or.
     &     .not. ( intsrt .eq. 19 .or. intsrt .eq. 20 .or. regular ) ) then  ! Irregular grid/hyd-file
         call opt1 ( iopt1   , lun     , 44     , lchar  ,  filtype ,
     &               dtflg1  , dtflg3  , 0      , ierr2  ,  iwar    ,
     &               has_hydfile       )
         if ( ierr2  .gt. 0 ) goto 100
         noqt = noq  + noq4
         allocate ( ipnt(4,noqt) , stat = ierr2 )
         if ( ierr2 .ne. 0 ) then
            write ( lunut , 2160 ) ierr2, 4*noqt
            goto 100
         endif
         ipnt = 0
         call pointi ( lun    , lchar  , noseg  , noq       , noq1   ,
     &                 noq2   , noq3   , noqt   , nobnd     , ipnt   ,
     &                 intsrt , iopt1  , jtrack , filtype(44), ioutpt ,
     &                 GridPs , ierr   , iwar   )
      endif
      noq12 = noq1 + noq2
      noq34 = noq3 + noq4

!        set dump area structure

      call dmpare ( lun     , ndmpar  , ntdmps  , noqt    , nosss   ,
     &              nobnd   , ipnt    , ntdmpq  , ndmpq   , ndmps   ,
     &              noraai  , ntraaq  , nsegdmp , isegdmp , nexcraai,
     &              iexcraai, ioptraai, ierr    , iwar    )

!        calculate size of the fast solvers matrix

      if ( intsrt .eq. 15 .or. intsrt .eq. 16 .or.
     &     intsrt .eq. 17 .or. intsrt .eq. 18 .or.
     &     intsrt .eq. 21 .or. intsrt .eq. 22      ) then
         call dlwq0f ( noq1   , noq2   , noq34  , nosss  , ipnt   ,
     &                 nomat  )
         write ( lunut, 2190 ) nomat
      endif
      if ( associated(ipnt) ) then
          deallocate ( ipnt )
      endif

!        Read dispersions

      write ( lunut , 2200 )
      disper = .true.
      ierr2 = 0
      call opt0   ( lun    , 9      , noq1     , noq2     , noq3   ,
     &              nodisp , 1      , nrftot(3), nrharm(3), ifact  ,
     &              dtflg1 , disper , volume   , iwidth   , lchar  ,
     &              filtype, dtflg3 , vrsion   , ioutpt   , ierr2  ,
     &              iwar   , .false. )
      ierr = ierr + ierr2
      disper = .false.

!        Read areas

      write ( lunut , 2210 )
      ierr2 = 0
      call opt0   ( lun    , 10     , noq1     , noq2     , noq3   ,
     &              1      ,  1     , nrftot(4), nrharm(4), ifact  ,
     &              dtflg1 , disper , volume   , iwidth   , lchar  ,
     &              filtype, dtflg3 , vrsion   , ioutpt   , ierr2  ,
     &              iwar   , has_hydfile       )
      ierr = ierr + ierr2

!        Read flows

      write ( lunut , 2220 )
      ierr2 = 0
      call opt0   ( lun    , 11     , noq1     , noq2     , noq3   ,
     &              1      , 1      , nrftot(5), nrharm(5), ifact  ,
     &              dtflg1 , disper , volume   , iwidth   , lchar  ,
     &              filtype, dtflg3 , vrsion   , ioutpt   , ierr2  ,
     &              iwar   , has_hydfile       )
      ierr = ierr + ierr2
      if ( .not. alone ) then
         if ( lchar(11) .ne. fnamep(7) ) then
            write (lunut, 2225 ) fnamep(7)
            ierr = ierr + 1
         endif
      endif

!        Read velos

      if ( novelo .gt. 0 ) then
         write ( lunut , 2230 )
         ierr2 = 0
         call opt0   ( lun    , 12     , noq1     , noq2     , noq3   ,
     &                 novelo , 1      , nrftot(6), nrharm(6), ifact  ,
     &                 dtflg1 , disper , volume   , iwidth   , lchar  ,
     &                 filtype, dtflg3 , vrsion   , ioutpt   , ierr2  ,
     &                 iwar   , .false. )
         ierr = ierr + ierr2
      endif

!        Read length "to" and "from" surfaces

      write ( lunut , 2240 )

      if ( has_hydfile ) then
         ilflag = 1
      else
         if ( gettoken( ilflag, ierr2 ) .gt. 0 ) goto 100
         write ( lunut , 2250 ) ilflag
      endif
      select case ( ilflag )
         case ( 0 )
            write ( lunut , 2260 )
            idum   = 4
            write ( lun(2) ) idummy
            call opt2 ( 1      , length , 1      , 3      , 1      ,
     &                  iwidth , lun(2) , idum   , ierr2  )

         case ( 1 )
            write ( lunut , 2270 )
            write ( lun(2) ) idummy , ( adummy , k = 1,3 )
            ierr2 = 0
            call opt0   ( lun    , 13     , noq1     , noq2     , noq3   ,
     &                     2     , 1      , nrftot(7), nrharm(7), ifact  ,
     &                    dtflg1 , disper , volume   , iwidth   , lchar  ,
     &                    filtype, dtflg3 , vrsion   , ioutpt   , ierr2  ,
     &                    iwar   , has_hydfile       )

         case default
            write ( lunut , 2280 )
            ierr = ierr + 1

      end select
      goto 100

!***************  second type of input ******************

   10 continue
      allocate ( ipnt(4,noqt) , stat = ierr2 )
      if ( ierr2 .ne. 0 ) then
         write ( lunut , 2160 ) ierr2, 4*noqt
         goto 100
      endif
      ipnt = 0

      ilflag = 1
      if ( nodisp .lt. 1 ) then
           write ( lunut , 2290 ) nodisp
           ierr2 = 1
           goto 100
      endif
      if ( intsrt .eq. 19 .or. intsrt .eq. 20 ) then
           write ( lun(29) , 2300 ) intsrt
           ierr2 = 1
           goto 100
      endif
      allocate ( rwork(5,noq) , stat = ierr2 )
      if ( ierr2 .ne. 0 ) then
         write ( lunut , 2310 ) ierr2, 5*noq
         goto 100
      endif

      if ( gettoken( iopt1, ierr2 ) .gt. 0 ) goto 100
      write ( lunut , 2320 ) iopt1
      if ( iopt1 .eq. 0 ) then
           write ( lunut , 2280 )
           ierr2 = 1
           goto 100
      endif
      idum = 0

      call opt1 ( iopt1   , lun     , idum    , lchar   , filtype ,
     &            dtflg1  , dtflg3  , 0       , ierr2   , iwar    ,
     &            .false. )
      if ( ierr2  .gt. 0 ) goto 100

      do k = 1, 4
         if ( gettoken( factor(k), ierr2 ) .gt. 0 ) goto 100
      enddo

      do j = 1, noq
         do i = 1, 4
            if ( gettoken( ipnt (i,j), ierr2 ) .gt. 0 ) goto 100
         enddo
         do i = 1, 5
            if ( gettoken( rwork(i,j), ierr2 ) .gt. 0 ) goto 100
         enddo
      enddo

      write ( lunut , 2330 ) (  factor( i )   , i = 1, 4 )
      write ( lunut , 2340 )
      write ( lunut , 2350 ) ( (ipnt ( i, j ) , i = 1, 4 ) ,
     &                         (rwork( i, j ) , i = 1, 5 ) , j = 1, noq )

!       calculate number of boundaries and bandwith of matrix

      call bound  ( lun    , noseg  , noq    , noqt   , intsrt ,
     &              ioutpt , GridPs , nobnd  , jtrack , ipnt   ,
     &              ierr   , iwar   )

!        set dump area structure

      call dmpare ( lun     , ndmpar  , ntdmps  , noq     , noseg   ,
     &              nobnd   , ipnt    , ntdmpq  , ndmpq   , ndmps   ,
     &              noraai  , ntraaq  , nsegdmp , isegdmp , nexcraai,
     &              iexcraai, ioptraai, ierr    , iwar    )

!        calculate size of the fast solvers matrix

      if ( intsrt .eq. 15 .or. intsrt .eq. 16 .or.
     &     intsrt .eq. 17 .or. intsrt .eq. 18 .or.
     &     intsrt .eq. 21 .or. intsrt .eq. 22      ) then
         call dlwq0f ( noq1   , noq2   , noq34  , nosss  , ipnt   ,
     &                 nomat  )
         write ( lunut, 2190 ) nomat
      endif

      factor(5) = factor(4)
      call scale ( rwork , factor , noq  , 5 )

      write ( lun(2) ) idummy , ( adummy , k = 1,3 )
      write ( lun(2) ) idummy , ( adummy , k = 1,3 )

      call dhopnf  ( lun(8) , lchar(8) , 8      , 1     , ierr2 )
      if ( ierr2 .ne. 0 ) goto 100
      if ( noq1 .gt. 0 ) write( lun(8) )( ipnt(:,i) , i =       1, noq1  )
      if ( noq2 .gt. 0 ) write( lun(8) )( ipnt(:,i) , i = noq1 +1, noq12 )
      if ( noq3 .gt. 0 ) write( lun(8) )( ipnt(:,i) , i = noq12+1, noq   )
      close ( lun(8) )

      call dhopnf  ( lun( 9) , lchar( 9) , 9      , 1     , ierr2 )
      if ( ierr2 .ne. 0 ) goto 100
      write ( lun( 9) ) idummy, ( rwork(1,i), ( adummy, k=1,nodisp-1 ) , i=1,noq )
      close ( lun( 9) )

      call dhopnf  ( lun(10) , lchar(10) , 10     , 1     , ierr2 )
      if ( ierr2 .ne. 0 ) goto 100
      write ( lun(10) ) idummy, ( rwork(2,i) , i=1,noq )
      close ( lun(10) )

      call dhopnf  ( lun(11) , lchar(11) , 11     , 1     , ierr2 )
      if ( ierr2 .ne. 0 ) goto 100
      write ( lun(11) ) idummy, ( rwork(3,i) , i=1,noq )
      close ( lun(11) )

      if ( novelo .gt. 0 ) then
         call dhopnf  ( lun(12) , lchar(12) , 12     , 1     , ierr2 )
         if ( ierr2 .ne. 0 ) goto 100
         write ( lun(12) ) idummy,( (adummy,k=1,novelo) , i=1,noq)
         close ( lun(12) )
      endif

      call dhopnf  ( lun(13) , lchar(13) , 13     , 1     , ierr2 )
      if ( ierr2 .ne. 0 ) goto 100
      write ( lun(13) ) idummy,(rwork(4,i),rwork(5,i), i=1,noq )
      close ( lun(13) )

      deallocate( ipnt , rwork )
      ierr2 = 0

!       here ends the alternative input

  100 continue

!       check the layers/3D model information:
!       - is it a 3D model?
!       - do we have consistency?

      if ( noq3 /= 0 ) then
          if ( nolay == 1 ) then
              iwar = iwar + 1
              write( lunut, 3000 ) noseg, noq3, noseg-noq3
              write( lunut, 3005 )
              write( *, '(1x,a)' ) 'WARNING: inconsistency if 3D model',
     &                             '         check .lst file'
          else
              write( lunut, 3010 ) nolay
          endif
      endif

      if ( ierr2 .gt. 0 ) ierr = ierr + 1
      if ( ierr2 .eq. 3 ) call srstop(1)
      call check  ( cdummy , iwidth , 4      , ierr2  , ierr   )
      if ( timon ) call timstop( ithndl )
      return

!       Output formats

 2000 format ( //,' Dimensions of the system :',
     &          /,' Number of gridcells 1st direction : ',I7,
     &          /,' Number of gridcells 2nd direction : ',I7,
     &          /,' Number of gridcells 3rd direction : ',I7 )
 2010 format ( //,' Dimensions of the system :',
     &          /,' Number of exchanges 1st direction : ',I7,
     &          /,' Number of exchanges 2nd direction : ',I7,
     &          /,' Number of exchanges 3rd direction : ',I7,
     &          /,' Total number of exchanges         : ',I7 )
 2020 format (  /,' ERROR. Total number of exchanges in Delpar differs: ',I10 )
 2040 format (    ' Nr of added bottom layer exchanges: ',I7 )
 2050 format (  /,' Number of dispersion arrays       : ' , I7 )
 2060 format (    'Dispersion ',I4 )
 2070 format (  /,' ERROR. dispersion ID not unique:',A)
 2080 format (  /,' Item nr:       names:',/
     &           (I6,10X,A20)                )
 2090 format (  /,' Names and assignments for substances are printed',
     &            ' for output option 2 and higher !' )
 2100 format (  /,' ERROR. Item number : ',I4, ' larger than maximum (',
     &              I4, ') !' )
 2110 format (  /,' Number of velocity arrays         : ' , I7 )
 2120 format (    'Velocity ',I4 )
 2130 format (  /,' ERROR. velocity ID not unique:',A)
 2140 format (    ' System dispersion velocity',/,
     &            '   nr.      nr.      nr.   ',/,
     &            (    I6,      I9,      I9   )  )
 2150 format (  /,' ERROR. One or more settings are invalid.')
 2160 format (  /,' ERROR. allocating memory for pointers:',I4,i10)
 2170 format (  /,' Input option                      : ' , I7 )
 2180 format (  /,' Option selected for pointers      : ',I7 )
 2190 format (  /,' Size of the big matrix for fast solvers is: ',I10)
 2200 format (  /,' Dispersion:' )
 2210 format (  /,' Area:' )
 2220 format (  /,' Flows:' )
 2225 format (  /,' ERROR: Flows for Delpar come from different file: ',A )
 2230 format (  /,' Velocities:' )
 2240 format (  /,' Lengths:' )
 2250 format (    ' Option for lengths          :',I7 )
 2260 format (    ' Lengths constant per direction all over the area.')
 2270 format (    ' Lengths variable over the area.')
 2280 format (  /,' ERROR, option not implemented' )
 2290 format (  /,' ERROR. Option incompatible with NODISP=',I4)
 2300 format (  /,' ERROR. Option incompatible with integration=',I4)
 2310 format (  /,' ERROR. allocating memory for input table:',I4)
 2320 format (  /,' Option selected for input    :',I7 )
 2330 format (  /,' Scale factor for dispersions :',E13.6 ,
     &          /,' Scale factor for areas       :',E13.6 ,
     &          /,' Scale factor for flows       :',E13.6 ,
     &          /,' Scale factor for lengths     :',E13.6 )
 2340 format (  /,' from   to fr-1 to+1  dispersion',
     &   '     surface        flow from-length   to-length')
 2350 format (   4I5,1P,5E12.4 )
 3000 format ( //,' WARNING: The model seems to be a 3D model, but:'
     &         ,/,'          Number of segments:           ',i10
     &         ,/,'          Number of vertical exchanges: ',i10
     &         ,/,'          Difference gives expected number of'
     &         ,/,'          segments per layer:           ',i10)
 3005 format (  /,'          - this is inconsistent'
     &         ,/,'          Note that the program will now assume one',
     &                       ' (1) layer!'
     &        ,//,'          You can specify the number of layers via'
     &         ,/,'          these keywords:'
     &        ,//,'          MULTIGRID ZMODEL NOLAY ... END_MULTIGRID')
 3010 format ( //,' Number of layers in the model:', I5)

      end
