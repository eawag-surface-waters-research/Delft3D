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

      subroutine opt0   ( lun    , is     , noql1  , noql2  , noql3  ,
     &                    ndim2  , ndim3  , nrftot , nrharm , ifact  ,
     &                    dtflg1 , disper , volume , iwidth , lchar  ,
     &                    filtype, dtflg3 , vrsion , ioutpt , ierr   ,
     &                    iwar   , dont_read   )

!       Deltares Software Centre

!>\file
!>                          Reads a block with constant or time variable data
!>\par  Description:
!>                          This is a main data aquisition sub system, it is
!>                          the only call to read:
!>                          - volumes ( in dlwq03 )
!>                          - additional dispersions ( in dlwq04 )
!>                          - additional velocities ( in dlwq04 )
!>                          - areas ( in dlwq04 )
!>                          - flows ( in dlwq04 )
!>                          - mixing lengthes ( in dlwq04 )
!>                          - old style open boundaries ( in dlwq05 )
!>                          - old style waste loads ( in dlwq06 )
!>                          Dlwq07 is a sort of dedicated verion of this routine
!>                          to read parameters and functions and segment functions\n
!>                          Dlwq08 is a sort of dedicated verion of this routine
!>                          to read initial conditions

!     Created            : May   1988  by M.E. Sileon / Leo Postma

!     Modified           : April 1996  by Leo Postma : Support version 4.90 for bounds and wastes
!                        : April 1997  by R. Bruinsma: Tokenized input data file reading added
!                          July  2002  by Leo Postma : Call to Opt1 changed. File option -4
!                                                      allowed for composite hydrodynamics
!                          May   2011  by Leo Postma : Fortran90 look and feel

!     Subroutines called : opt1    get & open include file
!                          opt2    read constants ( << (include) file)
!                          opt3    read time dep  ( << (include) file)
!                          dhopnf  open file

!     Functions called   : gettok  tokenized data file reading

!     Logical units      : lun(27) = unit stripped DELWAQ input file
!                          lun(28) = stripped workfile
!                          lun(29) = unit formatted output file
!                          lun( 2) = unit intermediate file (system)
!                          lun( 3) = unit intermediate file (harmos)
!                          lun( 4) = unit intermediate file (pointers)
!                          lun(is) = unit intermediate file (items)

      use m_dhopnf
      use timers       !   performance timers
      use rd_token
      use m_sysn          ! System characteristics


      implicit none

!     Parameters

!     kind           function         name           Descriptipon

      integer  ( 4), intent(inout) :: lun    (*)    !< array with unit numbers
      integer  ( 4), intent(in   ) :: is            !< entry in lun for this call
      integer  ( 4), intent(in   ) :: noql1         !< number of exchanges 1st direction
      integer  ( 4), intent(in   ) :: noql2         !< number of exchanges 2nd direction
      integer  ( 4), intent(in   ) :: noql3         !< number of exchanges 3rd direction
      integer  ( 4), intent(in   ) :: ndim2         !< number of items per block
      integer  ( 4), intent(in   ) :: ndim3         !< number of scale factors
      integer  ( 4), intent(inout) :: nrftot        !< number of functions
      integer  ( 4), intent(inout) :: nrharm        !< number of harmonics
      integer  ( 4), intent(in   ) :: ifact         !< factor between time scales
      logical      , intent(in   ) :: dtflg1        !< 'date'-format 1st time scale
      logical      , intent(in   ) :: disper        !< .true. then dispersion
      integer      , intent(inout) :: volume        !< if 1 then volume ( out: 0 = computed volumes )
      integer  ( 4), intent(in   ) :: iwidth        !< width of the output file
      character( *), intent(inout) :: lchar  (*)    !< array with file names of the files
      integer  ( 4), intent(inout) :: filtype(*)    !< type of binary file
      logical      , intent(in   ) :: dtflg3        !< 'date'-format (F;ddmmhhss,T;yydddhh)
      real     ( 4), intent(in   ) :: vrsion        !< version number of the input file
      integer  ( 4), intent(in   ) :: ioutpt        !< how extensive is output ?
      integer  ( 4), intent(inout) :: ierr          !< cumulative error count
      integer  ( 4), intent(inout) :: iwar          !< cumulative warning count
      logical      , intent(in)    :: dont_read     !< do not actually read tokens, if true, the information is already provided


      logical        bound       !  if .true. then boundary call
      logical        waste       !  if .true. then waste call
      logical        skip        !  if .true. then waste call with skip
      integer        iopt1       !  first  option ( type of file e.g. 0 = binary file )
      integer        iopt2       !  second option ( 1,2 = constant, 3 = time varying )
      integer        ndim1       !  sum of input in 3 directions
      integer        ndtot       !  total size of matrix (ndim1*ndim2)
      integer        ierr2       !  local error flag
      integer        itype       !  to identify the data type read
      integer        idummy      !  work integer ( = 0 )
      real           adummy      !  work real    ( = 0.0 )
      character(128) cdummy      !  work character
      integer        k           !  loop counter
      real           disp(3,1)   !  dispersions in 3 directions
      real(4), allocatable :: values(:,:) ! read buffer for the values
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "opt0", ithndl )

      idummy =   0
      adummy = 0.0
      ndim1  = noql1+ noql2+ noql3
      ndtot  = ndim1*ndim2
      bound = .false.
      waste = .false.
      skip  = .false.

      select case ( ierr )
         case ( -1 )
            bound = .true.
            waste = .false.
            skip  = .false.
         case ( -2 )
            bound = .false.
            waste = .true.
            skip  = .false.
         case ( :-3 )
            bound = .false.
            waste = .true.
            skip  = .true.
      end select
      ierr  = 0
      if ( skip ) goto 10

!        Read first option, write zero dispersion if OPT1=0

      if ( dont_read ) then
         iopt1 = -2
         if ( is == 13 ) then
            iopt1 = 0 ! Ugly hack, all other files are time-dependent
         endif
      else
         if ( gettoken( cdummy, iopt1, itype, ierr2 ) .gt. 0 ) goto 50
         if ( itype .eq. 1 ) then
            if ( volume .ne. 1 ) then
               write ( lunut, 2070 ) cdummy
               ierr2 = 1
               goto 50
            else
               if ( cdummy .eq. 'FRAUD' ) then
                  volume = -1
                  write ( lunut, 2080 )
                  if ( gettoken( iopt1, ierr2 ) .gt. 0 ) goto 50
               else
                  write ( lunut, 2090 ) cdummy
                  ierr2 = 1
                  goto 50
               endif
            endif
         endif
      endif

      write ( lunut , 2000 ) iopt1
      call opt1   ( iopt1   , lun     , is      , lchar   , filtype ,
     &              dtflg1  , dtflg3  , ndtot   , ierr2   , iwar    ,
     &              dont_read                                       )
      if ( ierr2 .gt. 0 ) goto 50

!        Binary file, option = -2 (or sequence of binary files option = -4)
!                           everything is block function, except volume

      if ( iopt1 .eq. -2 .or. iopt1 .eq. -4 ) then
         nlines    = nlines + ndim1*ndim2*2
         npoins    = npoins + ndim1 + 3
         nrftot    = ndim1*ndim2
         nrharm    = 0
         if ( volume .eq. 1 ) then
            write( lun(4) ) (  k , k=1,ndim1 ) , ( idummy , k=1,3 )
         else
            write( lun(4) ) ( -k , k=1,ndim1 ) , ( idummy , k=1,3 )
         endif
         iopt1 = 0
      endif

!        Dispersion in three directions if DISPER, return if NODISP=0

      if ( disper ) then
         if ( iopt1 .eq. 0 ) then                            ! binary file, then
            write ( lun(2) ) idummy , ( adummy , k=1,3 )     ! no fixed dispersions
         else
            write ( lun(2) ) idummy
            call opt2 ( 1      , disp   , 1      , 3      , 3      ,
     &                  iwidth , lun(2) , ioutpt , ierr2  )
            if ( ierr2 .gt. 0 ) goto 50
            if ( ndim2 .eq. 0 ) goto 9999
         endif
      endif

      if ( iopt1 .eq. 0 ) goto 9999                          ! binary file, we are ready

!        Read second option, set volume flag if OPT2 > 3 AND VOLUME

   10 if ( gettoken( iopt2, ierr2 ) .gt. 0 ) goto 50
      write ( lunut , 2010 ) iopt2
      if ( volume .eq. 1 .and. iopt2 .gt. 3 ) then                  ! Computed volumes !!
         volume = 0
         iopt2 = iopt2 - 3
      endif

!        Get the data

      select case ( iopt2 )
         case ( 1, 2 )              !   Constants with and without defaults in three directions
            allocate ( values( ndim2, max(noql1,noql2,noql3) ) )
            call dhopnf ( lun(is) , lchar(is) , is    , 1     , ierr2 )
            write ( lun(is) ) idummy
            if ( noql1 .gt. 0 ) write ( lunut , 2030 )
            call opt2 ( iopt2  , values , noql1  , ndim2  , ndim3  ,
     &                  iwidth , lun(is), ioutpt , ierr2  )
            if ( ierr2 .gt. 0 ) goto 50

            if ( noql2 .gt. 0 ) write ( lunut , 2040 )
            call opt2 ( iopt2  , values , noql2  , ndim2  , ndim3  ,
     &                  iwidth , lun(is), ioutpt , ierr2  )
            if ( ierr2 .gt. 0 ) goto 50

            if ( noql3 .gt. 0 .and. noql3 .ne. ndim1 ) write ( lunut , 2050 )
            call opt2 ( iopt2  , values , noql3  , ndim2  , ndim3  ,
     &                  iwidth , lun(is), ioutpt , ierr2      )
            close ( lun(is) )
            if ( ierr2 .gt. 0 ) goto 50

         case ( 3 )                 !   Time varying data
            ierr2 = 0
            if ( bound ) ierr2 = -1
            if ( waste ) ierr2 = -2
            call opt3 ( lun    , lchar  , is     , ndim1  , ndim2  ,
     &                  ndim3  , ifact  , dtflg1 , dtflg3 , nrftot ,
     &                  nrharm , iwidth , ioutpt , ierr2  )
            if ( ierr2 .gt. 0 ) goto 50

         case default
            write ( lunut , 2020 )
            goto 50

      end select
 9999 if (timon) call timstop( ithndl )
      return

   50 ierr = ierr+1
      if (timon) call timstop( ithndl )
      return

!       Output formats

 2000 format (  /,' First  selected option   : ',I7 )
 2010 format (    ' Second selected option   : ',I7 )
 2020 format (  /,' ERROR. Option not implemented !!!!!!')
 2030 format (  /,' First  direction:')
 2040 format (  /,' Second direction:')
 2050 format (  /,' Third  direction:')
 2060 format (  /,' ERROR the number of values (',I7,') exceeds the maximum (',I7,').',
     *          /,' The maximum is limited by REAL array space',
     *          /,' Consult your system manager to obtain at least',I7,
     *          /,' words of additional storage.' )
 2070 format (  /,' ERROR. No character string allowed: ',A )
 2080 format (    ' Keyword FRAUD found for fraudulent computations.' )
 2090 format (  /,' ERROR. This keyword is not allowed here: ',A )

      end
