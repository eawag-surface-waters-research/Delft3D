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

      subroutine dlwq08 ( lun    , lchar  , filtype, noseg  , notot  ,
     &                    syname , iwidth , vrsion , ioutpt , inpfil ,
     &                    gridps , ierr   , iwar   )

!       Deltares Software Centre

!>\file
!>          Reads initial conditions
!>
!>          This routine reads the initial conditions
!>          - MASS/M2 is an allowed keyword to indicate that ASCII input
!>            of passive substances is expressed in mass/m2
!>          - ASCII with defaults and overridings is the same since 1988
!>          - ASCII without defaults requires 1 keyword and values for
!>            all volumes per substance, so 12345*2.27 ; substance 1 etc.
!>            and 11522*0.0 823*3.14  ; for a passive substance in 15 layers.
!>            The simulation system (dryfld.f) migrates automatically the
!>            lowest value to the first active cell in a Z-layer model
!>          - Binary files not being a .map file are assumed to be in mass/gridcell
!>          - .map files are scanned for the presence of the mass/m2 token at
!>            the end of title string 3 by the simulation system (dlwqi0.f)
!>          - The simulation system only produces mass/m2 .map files for restart
!>            purposes any more.

!      Created            : April     1988 by Marjolein E. Sileon / Leo Postma

!      Modified           : April     1997 by R. Bruinsma: Tokenized input data file reading added
!                           July      2002 by Leo Postma : Call to Opt1 changed.
!                           April     2010 by Leo Postma : allocated array for the values
!                           September 2010 by Leo Postma : mass/m2 for non-transported substances
!                           February  2011 by Leo Postma : input order changed for no-default reading
!                                                          new rd_token implemented

!      Subroutines called : opt1    ( which file is it ? )
!                           opt2    ( read the data from an ASCII file )
!                           rdtok1  ( tokenized data reading )
!                           dhopnf  ( to open the binary intermediate file )
!                           check   ( to see of the group was read correctly )

!      Logical units      : lun(27) = unit DELWAQ input file
!                           lun(29) = unit formatted output file
!                           lun(18) = unit intermediate file (initials)

      use m_srstop
      use m_dhopnf
      use grids          ! for the storage of contraction grids
      use dlwq_data      ! for definition and storage of data
      use rd_token
      use timers         ! performance timers
      use string_module  ! string manipulation tools
      implicit none

!     Arguments           :

!     Kind                    Function         Name               Description

      integer           ( 4), intent(inout) :: lun  (*)      !< array with unit numbers
      character         ( *), intent(inout) :: lchar(*)      !< filenames
      integer           ( 4), intent(inout) :: filtype(*)    !< type of binary file
      integer           ( 4), intent(in   ) :: noseg         !< nr of computational volumes
      integer           ( 4), intent(in   ) :: notot         !< nr of delwaq + delpar state variables
      character         (20), intent(in   ) :: syname(notot) !< names of the substances
      integer           ( 4), intent(in   ) :: iwidth        !< width of the output file
      real              ( 4), intent(in   ) :: vrsion        !< version of input
      integer           ( 4), intent(in   ) :: ioutpt        !< option for extent of output
      type(inputfilestack)  , intent(inout) :: inpfil        !< input file strucure with include stack and flags
      type(gridpointercoll) , intent(in)    :: gridps        !< collection off all grid definitions
      integer           ( 4), intent(inout) :: ierr          !< cumulative error   count
      integer           ( 4), intent(inout) :: iwar          !< cumulative warning count

      integer, parameter :: STRING   =  1
      integer, parameter :: EXTASCII = -1, BINARY   = 0, THISFILE = 1
      integer, parameter :: NODEFAUL =  1, DEFAULTS = 2

      real(4), allocatable :: scales(:)             ! real workspace scale factors
      real(4), allocatable :: values(:,:)           ! real workspace values
      integer, allocatable :: iover (:)             ! integer space for overridings

      integer  (  4)  ierr2                         ! local error indicator
      integer  (  4)  itype                         ! 0 = all, 1 = string, 2 = integer, 3 = real
      character(255)  cdummy                        ! workspace for reading
      character(  4)  cext                          ! inital conditions file extention
      integer  (  4)  icopt1                        ! first file option (ASCII/Binary/external etc)
      integer  (  4)  icopt2                        ! constants with or without defaults
      logical         ldummy                        ! dummy variable
      integer  (  4)  ip                            ! location of the period in the file name
      integer  (  4)  i                             ! loop variable and dummy integer
      integer  (  4)  isys, iseg                    ! substances and volumes loop variables
      logical         masspm2                       ! is it mass per m2 ?
      logical         transp            ! input with a transposed matrix (per substance) ?
      logical         old_input         ! old or new input
      integer  (  4)  itime             ! time in map file
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwq08", ithndl )

!        Initialisations

      iposr   =  0                                  ! start at begin of the input line with reading
      lunut   = lun(29)
      ierr2   =  0
      masspm2 = .false.
      transp  = .false.

!        Let's see what comes, file option or a token

      if ( gettoken( cdummy, icopt1, itype, ierr2 ) .gt. 0 ) goto 10
      if ( itype .eq. STRING ) then
         if ( cdummy .eq. 'mass/m2' .or.
     &        cdummy .eq. 'MASS/M2' ) then
            masspm2 = .true.
            write ( lunut, 2030 )
            if ( gettoken( cdummy, icopt1, itype, ierr2 ) .gt. 0 ) goto 10
         elseif ( cdummy .ne. 'INITIALS') then
            write ( lunut, 2040 ) trim(cdummy)
            ierr2 = 3
            goto 10
         endif
      endif

      if ( itype .eq. STRING ) then
         if ( cdummy .eq. 'INITIALS') then
            icopt1 = THISFILE
            old_input = .false.
         else
            write ( lunut, 2040 ) trim(cdummy)
            ierr2 = 3
            goto 10
         endif
      else
         old_input = .true.
      endif

!        The file option

      write ( lunut , 2000 ) icopt1
      if ( icopt1 .ne. EXTASCII .and. icopt1 .ne. BINARY   .and.
     &     icopt1 .ne. THISFILE       ) then
         write ( lunut , 2020 )
         goto 10
      endif

!        Get the input file name

      call opt1   ( icopt1  , lun     , 18      , lchar   , filtype ,
     &              ldummy  , ldummy  , 0       , ierr2   , iwar    ,
     &              .false. )
      if ( ierr2  .gt. 0 ) goto 10
      if ( icopt1 .eq. BINARY ) then
         ip = scan ( lchar(18), '.', back = .true. )              ! look for the file type
         cext = lchar(18)(ip:ip+3)
         call str_lower(cext)
         if ( cext .eq. '.map' .or. cext .eq. '.rmp' .or.
     &        cext .eq. '.rm2' ) then                             ! if .rmp or .rm2 (Sobek) or .map, it is a map-file
            call dhopnf  ( lun(18) , lchar(18) , 18    , 2     , ierr2 )
            read ( lun(18) ) cdummy(1:160)                        ! read title of simulation
            close ( lun(18) )
            if ( cdummy(114:120) .eq. 'mass/m2' .or.
     &           cdummy(114:120) .eq. 'MASS/M2' ) then            !  at end of third line ...
               write ( lunut , 2070 )
            else if ( masspm2 ) then
               write ( lunut , 2080 )
               ierr = ierr + 1
            else
               write ( lunut , 2090 )
               iwar = iwar + 1
            endif
         else if ( masspm2 ) then
            write ( lunut , 2100 )
            ierr = ierr + 1
         else
            write ( lunut , 2110 )
            iwar = iwar + 1
         endif
         goto 10
      endif

!        Make the file a .map file instead of the previous .wrk file

      ip = scan ( lchar(18), '.', back=.true. )
      if ( ip .eq. 0 ) then
         lchar(18) = trim(lchar(18))//'.map'
      else
         lchar(18)(ip:ip+3) = '.map'
      endif
      call DHOPNF  ( lun(18) , lchar(18) , 18    , 1     , ierr2 )
      if ( ierr2 .gt. 0 ) goto 10

!        Write the .map header

      if ( masspm2 ) then
         cdummy(  1: 40) = 'Initial conditions file                 '
         cdummy( 41: 80) = 'inactive substances are in mass/m2      '
         cdummy( 81:120) = 'this is the deciding keyword ==> mass/m2'
         cdummy(121:160) = 'there is no time string in this file    '
      else
         cdummy(  1: 40) = 'Initial conditions file                 '
         cdummy( 41: 80) = 'inactive substances are in mass/gridcell'
         cdummy( 81:120) = '                                        '
         cdummy(121:160) = 'there is no time string in this file    '
      endif
      write ( lun(18) ) cdummy(1:160), notot, noseg
      write ( lun(18) ) ( syname(i), i = 1, notot )

      if ( old_input ) then
!                see how the data comes

         if ( gettoken( cdummy, icopt2, itype, ierr2 ) .gt. 0 ) goto 10
         if ( itype .eq. STRING ) then
            if ( cdummy .eq. 'TRANSPOSE' ) then
               transp = .true.
               write ( lunut, 2060 )
               if ( gettoken( icopt2, ierr2 ) .gt. 0 ) goto 10
            else
               write ( lunut, 2040 ) trim(cdummy)
               ierr2 = 3
               goto 10
            endif
         endif
         write ( lunut , 2010 ) icopt2
         if ( icopt2 .ne. NODEFAUL .and. icopt2 .ne. DEFAULTS ) then
            write ( lunut , 2020 )
            goto 10
         endif

!           Get the data and write it to unit 18

         write ( lun(18) ) 0
         if ( transp ) then
            allocate ( values(noseg,notot) )
            call opt2  ( icopt2 , values , notot  , noseg  , 1      ,
     &                   iwidth , 0      , ioutpt , ierr2  )
            write ( lun(18) ) ( values(i,:),i=1,noseg )
         else
            allocate ( values(notot,noseg) )
            call opt2  ( icopt2 , values , noseg  , notot  , notot  ,
     &                   iwidth , 0      , ioutpt , ierr2  )
            write ( lun(18) ) values
         endif
         close ( lun(18) )
         if ( ierr2 .gt. 0 ) goto 10

      else

         allocate ( values(notot,noseg), stat=ierr2 )
         if ( ierr2 .ne. 0 ) then
            write( lunut, * ) 'ERROR allocating memory for initials'
            ierr2 = 3
            goto 10
         endif
         push = .true.
         call read_initials ( lun    , lchar  , filtype, inpfil, notot ,
     &                        syname , iwidth , ioutpt , gridps, noseg ,
     &                        values , ierr2  , iwar   )
         itime = 0
         write(lun(18)) itime,values
         close ( lun(18) )
         if ( ierr2 .gt. 0 ) goto 10

      endif

      ierr2 = 0
   10 if ( ierr2 .gt. 0 ) ierr = ierr + 1
      if ( ierr2 .gt. 0 ) write ( lunut, 2050 )
      if ( ierr2 .eq. 3 ) call SRSTOP(1)
      if ( old_input ) then
         call check  ( cdummy , iwidth , 8      , ierr2  , ierr   )
      endif
      if ( timon ) call timstop( ithndl )
      return

!       Output formats

 2000 format (  /,' Option selected for initials    :',I4 )
 2010 format (    ' Second option for initials      :',I4 )
 2020 format (  /,' ERROR, option not implemented')
 2030 format (  /,' Initials for passive substances are in mass/m2' )
 2040 format (  /,' ERROR, keyword not supported: ',A )
 2050 format (    ' ERROR reading input!' )
 2060 format (  /,' Block of input data is ordered per substance' )
 2070 format (  /,' Binary initials file is .map file with bed substances in mass/m2!' )
 2080 format (  /,' ERROR: initials file is .map file with bed substances in mass/gridcell rather than mass/m2!' )
 2090 format (  /,' WARNING: Binary initials file is .map file with bed substances in mass/gridcell!' )
 2100 format (  /,' ERROR: Binary initials file is assumed to have bed substances in mass/gridcell rather than mass/m2!' )
 2110 format (  /,' WARNING: Binary initials file is assumed to have bed substances in mass/gridcell!' )

      end
