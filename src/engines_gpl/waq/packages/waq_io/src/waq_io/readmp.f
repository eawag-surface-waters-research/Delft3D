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

      subroutine readmp ( lun    , lchar  , filtype, duname , nsegdmp,
     &                    isegdmp, dmpbal , ndmpar , ntdmps , ioutpt ,
     &                    ierr   , iwar   )

!       Deltares Software Centre

!>\file
!>                        Reads monitoring areas
!>
!>           - Reads an option\n
!>             This can be:
!>             - -1 for an external ASCII file, superfluous because of INCLUDE support
!>             - 1 for continuation on this file
!>             - 2 to indicate that no monitoring areas are used
!>           - The amount of monitoring areas
!>           - Per monitoring area:
!>             - the name (20 characters) of the area
!>             - optionally balance option NO_BALANCE/BALANCE(default)
!>             - the number of volumes included in the area
!>             - that many volume numbers

!     Created           : March 1995 by Jan van Beek

!     Modified          : April 1997 by R. Bruinsma: Tokenized data file reading added
!                         July  2002 by Leo Postma : Call to Opt1 changed.
!                         May   2011 by Leo Postma : Fortran-90 look and feel

!     Subroutine called : OPT1   - to open an external file
!                         ZOEK   - to searchs strings

!     Logical units     : LUN(27) = unitnumber stripped DELWAQ input file
!                         LUN(29) = unitnumber formatted output file

      use m_zoek
      use rd_token     !   for the reading of tokens
      use timers       !   performance timers

      implicit none

!     parameters

!     kind           function         name                Descriptipon

      integer  ( 4), intent(in   ) :: lun    (*)        !< array with unit numbers
      character( *), intent(inout) :: lchar  (*)        !< array with file names of the files
      integer  ( 4), intent(inout) :: filtype(*)        !< type of binary file
      character(20), pointer       :: duname (:)        !< name of monitoring areas
      integer  ( 4), pointer       :: nsegdmp(:)        !< number of volumes per monitoring area
      integer  ( 4), pointer       :: isegdmp(:)        !< volumes numbers per monitoring area
      integer  ( 4), pointer       :: dmpbal(:)         !< balance option (flag) per monitoring area
      integer  ( 4), intent(  out) :: ndmpar            !< number of monitoring areas
      integer  ( 4), intent(  out) :: ntdmps            !< total number of volumes in monitoring areas
      integer  ( 4), intent(in   ) :: ioutpt            !< flag for more or less output
      integer  ( 4), intent(inout) :: ierr              !< error   count
      integer  ( 4), intent(inout) :: iwar              !< cumulative warning count

!     local variables

      integer             idopt1           ! First option number monitoring areas
      integer             ierr2            ! Local error flag
      integer             max_ntdmps       ! Size of isegdmp
      integer, pointer :: isegdmp_2(:)     ! Help pointer for array expansion
      integer             ierr_alloc       ! Error indicator for allocations
      integer             nseg             ! Number of volumes per monitoring area
      logical             ldummy           ! Dummy logical
      integer             id               ! Loop variable over all monitoring areas
      integer             k                ! General loop variable
      integer             ifound           ! Help variable for string search
      character(len=256)::option           ! balance option
      integer             itype            ! type of the returned token
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "readmp", ithndl )

!     Read file option

      if ( gettoken( idopt1, ierr2 ) .gt. 0 ) goto 20
      select case ( idopt1 )
         case ( :-2 )
            write ( lunut, 2000 )  idopt1
            write ( lunut, 2010 )
            goto 20
         case ( -1 )                     ! old style <other ASCII file>
            write ( lunut, 2000 )  idopt1
            call opt1   ( idopt1  , lun     , 0       , lchar   , filtype ,
     &                    ldummy  , ldummy  , 0       , ierr2   , iwar    ,
     &                    .false. )
            if ( ierr2 .gt. 0 ) goto 20
            if ( gettoken( ndmpar, ierr2 ) .gt. 0 ) goto 20
         case ( 0 )                      ! new style (October 2012) no dump areas
            write ( lunut , 2020 )       ! old style would have produced an error
            ndmpar = 0
            ntdmps = 0
            goto 30
         case ( 1 )                      ! old style <this input file> or new style 1 area
            if ( gettoken( option, ndmpar, itype, ierr2 ) .gt. 0 ) goto 20
            if ( itype .eq. 1 ) then     ! character so: new style, 1 area
               push = .true.
               ndmpar = idopt1
            else
               write ( lunut, 2000 )  idopt1
            endif
         case ( 2 )                      ! old style <no dump areas> or new style 2 areas
            if ( gettoken( option, ndmpar, itype, ierr2 ) .gt. 0 ) goto 20
            push = .true.                ! look to see what will be next
            if ( itype .eq. 1 ) then     ! a string, so first dump-ID from 2 areas
               ndmpar = idopt1
            else                         ! an integer, so 2 meant <not used>
               write ( lunut, 2000 )  idopt1
               write ( lunut, 2020 )
               ndmpar = 0
               ntdmps = 0
               goto 30
            endif
         case default                    ! new input processing
            ndmpar = idopt1

      end select

!        Write number of dump areas, allocate arrays

      write( lunut, 2030 ) ndmpar
      ntdmps = 0
      allocate ( duname(ndmpar), stat=ierr_alloc )
      if ( ierr_alloc .ne. 0 ) then
         write ( lunut , 2380 ) ierr_alloc
         goto 20
      endif
      allocate ( nsegdmp(ndmpar), isegdmp(ndmpar), dmpbal(ndmpar), stat=ierr_alloc )
      if ( ierr_alloc .ne. 0 ) then
         write ( lunut , 2390 ) ierr_alloc
         goto 20
      endif
      max_ntdmps = ndmpar

!        Read specification of the dump areas

      if ( ioutpt .lt. 2 ) write ( lunut , 2040 )
      if ( ioutpt .eq. 2 ) write ( lunut , 2050 )
      do 10 id = 1 , ndmpar
         if ( gettoken( duname(id), ierr2 ) .gt. 0 ) goto 20
         if ( gettoken( option, nseg  , itype, ierr2 ) .gt. 0 ) goto 20
         if ( itype .eq. 1 ) then                    ! character
            if ( option .eq. 'BALANCE' ) then
               dmpbal(id) = 1
            elseif ( option .eq. 'NO_BALANCE' ) then
               dmpbal(id) = 0
            else
               write(lunut,2420) trim(option)
               goto 20
            endif
            if ( gettoken( nseg      , ierr2 ) .gt. 0 ) goto 20
         else
            dmpbal(id) = 1
         endif
         if ( ntdmps + nseg .gt. max_ntdmps ) then
            max_ntdmps = 2*(ntdmps+nseg)
            allocate ( isegdmp_2(max_ntdmps),stat=ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then
               write ( lunut , 2400 ) ierr_alloc
               goto 20
            endif
            isegdmp_2(1:ntdmps) = isegdmp(1:ntdmps)
            deallocate(isegdmp)
            isegdmp => isegdmp_2
         endif
         do k = 1, nseg
            if ( gettoken( isegdmp(ntdmps+k), ierr2 ) .gt. 0 ) goto 20
         enddo
         if ( duname(id) .eq. ' ' ) write(duname(id), '(''Observation-id'',i6)' ) id

      ! check if name is unique

         do k = 1 , id-1
            call ZOEK( duname(id), 1, duname(k:), 20, ifound )
            if ( ifound .gt. 0 ) then
               write( lunut, 2410 ) duname(id)
               ierr = ierr + 1
            endif
         enddo

         if ( ioutpt .ge. 2 ) then
            write( lunut, 2060 ) id , duname(id), nseg
            if ( dmpbal(id) .eq. 0 ) then
               write(lunut,2430)
            endif
            if ( ioutpt .ge. 3 ) then
               write( lunut, 2070 )
               write( lunut, 2080 ) ( k, isegdmp(ntdmps+k), k=1,nseg )
            endif
         endif

         ntdmps  = ntdmps + nseg
         nsegdmp(id) = nseg

   10 continue
      goto 30

!     Error handling

   20 ierr = ierr+1
   30 if (timon) call timstop( ithndl )
      return

 2000 format ( / ,' Dump areas:',
     &         / ' option selected for input :' , I5 )
 2010 format ( / ' ERROR, option for dump areas not implemented !!!!')
 2020 format (   ' Dump areas not used.')
 2030 format ( / ' Number of dump areas :',I5)
 2040 format (   ' Information on dump areas will only be printed ',
     &                             'for output option 2 and higher !' )
 2050 format (   ' Composition of dump areas will only be printed ',
     &                             'for output option 3 and higher !' )
 2060 format ( / ' Dump area :',I5,' : ',A20,
     &         / ' Number of segments in area :',I15)
 2070 format (   '          number   segment')
 2080 format (   2I15 )
 2380 format (  /,' ERROR. allocating memory for monitor names:',I8)
 2390 format (  /,' ERROR. allocating memory for monitor segments:',I8)
 2400 format (  /,' ERROR reallocating memory for monitor segments:',I8)
 2410 format (  /,' ERROR. observation ID not unique:',A)
 2420 FORMAT (  /,' ERROR, unrecognised keyword for dump area:',A)
 2430 FORMAT (    ' Dump area is excluded from mass balance output')
      end
