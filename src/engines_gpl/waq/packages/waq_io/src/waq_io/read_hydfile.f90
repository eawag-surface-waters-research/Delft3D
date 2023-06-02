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

subroutine read_hydfile( lunout, hydfile, lchar, noseg, nexch, ierr )
use m_dhpath


!   Deltares Software Centre

!>\File
!>               Reads the hyd-file and extracts relevant information

!   Global declarations

    implicit none

!   declaration of arguments

    integer               , intent(in)       :: lunout       !< unit number for reporting
    character(len=*)      , intent(in)       :: hydfile      !< name of the hyd-file to read
    character(len=*)      , intent(inout)    :: lchar(*)     !< filenames
    integer  ( 4)         , intent(out)      :: noseg        !< number of segments
    integer  ( 4), dimension(*), intent(out) :: nexch        !< number of exchanges
    integer               , intent(out)      :: ierr         !< error code

!   local variables

    character(len=400)                    :: line
    character(len=400)                    :: path
    character(len=20)                     :: cdummy
    character(len=20), dimension(10)      :: keyword
    integer,           dimension(10)      :: fileno
    integer                               :: i, ierr2, lunin, idxlga, idxgeom, pathlen

    integer                               :: nx, ny, nosegl, nolay, noq1, noq2, noq3
    character(len=4)                      :: identifier
    character(len=len(lchar))             :: grid_file

!
!   Read the various file names
!   Note:
!   The subroutine fffind checks the times in the files and we do not know the
!   number of items yet. So either we need to make that routine more complex
!   or exploit the simple structure of the hyd-file.
!
    keyword(1:8) = ['volumes-file        ', 'areas-file          ', 'flows-file          ', 'pointers-file       ', &
                    'lengths-file        ', 'attributes-file     ', 'grid-indices-file   ', 'waqgeom-file        ']
    fileno(1:8)  = [7                     , 10                    , 11                    , 44                    , &
                    13                    , 40                    , 6                     , 46                    ]

    idxlga  = -1
    idxgeom = -1

    call dhpath( hydfile, path, pathlen )

    open( newunit = lunin, file = hydfile, status = 'old', iostat = ierr )
    if ( ierr /= 0 ) then
        write( lunout, '(a,a)' ) 'ERROR: Hyd-file does not exist or could not be opened - ', trim(hydfile)
        return
    endif

    do
        read( lunin, '(a)', iostat = ierr2 ) line

        if ( ierr2 < 0 ) then
            exit
        endif
        if ( ierr2 > 0 ) then
            ierr = ierr2
            write( lunout, '(a,a)' ) 'ERROR: Reading hyd-file failed - ', trim(hydfile)
            return
        endif

        do i = 1,8
            if ( index( line, keyword(i) ) > 0 ) then
                read( line, *, iostat = ierr ) cdummy, lchar(fileno(i))
                if ( ierr2 > 0 ) then
                    ierr = ierr2
                    write( lunout, '(a,a)' ) 'ERROR: Reading hyd-file failed - ', trim(hydfile)
                    write( lunout, '(a,a)' ) '       Line: ', trim(line)
                    return
                endif

                lchar(fileno(i)) = path(1:pathlen) // lchar(fileno(i))

                if ( i == 7 ) then  ! LGA file
                    idxlga = fileno(i)
                endif
                if ( i == 8 ) then  ! WAQGEOM file
                    idxgeom = fileno(i)
                endif

                exit
            endif
        enddo

        if ( index( line, 'number-horizontal-exchanges' ) > 0 ) then
            read( line, * ) cdummy, nexch(1)
        endif
        if ( index( line, 'number-vertical-exchanges' ) > 0 ) then
            read( line, * ) cdummy, nexch(3)
        endif
        if ( index( line, 'number-water-quality-segments-per-layer' ) > 0 ) then
            read( line, * ) cdummy, nosegl
        endif
        if ( index( line, 'number-water-quality-layers' ) > 0 ) then
            read( line, * ) cdummy, nolay
        endif
        if ( index( line, 'grid-coordinates-file' ) > 0 ) then
            read( line, * ) cdummy, grid_file
        endif
    enddo

    noseg = nosegl * nolay

    close( lunin )

    !
    ! Read the number of grid cells:
    ! - LGRID file
    ! - WAQGEOM file
    !
    if ( idxgeom > 0 ) then
        !
        ! Retrieved via keywords
        !
        noseg = nosegl * nolay
    elseif ( idxlga > 0 ) then
        open( newunit = lunin, file = lchar(idxlga), access = 'stream', iostat = ierr2 )

        if ( ierr2 /= 0 ) then
            ierr = 1
            write( lunout, '(a,a)' ) 'ERROR: LGA-file does not exist or could not be opened - ', trim(lchar(8))
            return
        endif

        !
        ! Check that it is not a NetCDF 3/4 file -- UNTRIM
        !
        read( lunin, iostat = ierr2 ) identifier
        if ( identifier(1:3) == 'CDF' .or. identifier(2:4) == 'HDF' ) then
            !
            ! We have a hyd-file from UNTRIM, so use the other file name
            !
            idxgeom = fileno(8)
            lchar(idxgeom) = grid_file
        else
            rewind( lunin )
            read( lunin, iostat = ierr2 ) nx, ny, nosegl, nolay, nexch(1), nexch(2), nexch(3)

            if ( ierr2 /= 0 ) then
                ierr = 1
                write( lunout, '(a,a)' ) 'ERROR: Header of LGA-file could not be read - ', trim(lchar(8))
                return
            endif
        endif

        noseg = nosegl * nolay
        close( lunin )
    else
        close( lunin )
        write( lunout, '(a,a)' ) 'ERROR: Hyd-file does not contain the name for a grid file - ', trim(hydfile)
        ierr = 1
        return
    endif
end subroutine read_hydfile
