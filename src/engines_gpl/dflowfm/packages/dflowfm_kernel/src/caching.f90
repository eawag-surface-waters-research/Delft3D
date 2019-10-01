!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2019.!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

! $Id$
! $HeadURL$

! NOTES:
! - Observation points can be "moving" - does that have influence on the data that need to be cached?
! - How about the global index to these items?
!


!> Manages the caching file - store and retrieve the grid-based information.
module unstruc_caching
    use precision
    use m_observations, only: numobs, xobs, yobs, locTpObs, kobs
    use md5_checksum

    implicit none

    logical, private :: cache_success

    character(len=20), dimension(10), private :: section = ['OBSERVATIONS        ', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890']
    integer, parameter, private :: key_obs = 1

    double precision, dimension(:), allocatable, private :: cache_xobs
    double precision, dimension(:), allocatable, private :: cache_yobs
    integer, dimension(:), allocatable, private          :: cache_locTpObs
    integer, dimension(:), allocatable, private          :: cache_kobs

    character(len=30), private :: version_string = "D-Flow FM, cache file, 1.0"
    character(len=14), private :: md5current

contains
logical function cacheRetrieved()
    cacheRetrieved = cache_success
end function cacheRetrieved

!> Load the information from the caching file - if any
subroutine loadCachingFile( filename, netfile )
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: netfile

    integer :: lun
    integer :: ierr
    integer :: number
    character(len=30) :: version_file
    character(len=20) :: key
    character(len=14) :: md5checksum
    logical :: okay
    logical :: success

    cache_success = .false.

    open( newunit = lun, file = trim(filename) // ".cache", status = "old", access = "stream", iostat = ierr )

    !
    ! Apparently there is no caching file, so return without further processing
    !
    if ( ierr /= 0 ) then
        return
    endif

    !
    ! Load the version number and the MD5 checksum - useable at all?
    !
    read( lun, iostat = ierr ) version_file, md5checksum

    if ( ierr /= 0 ) then
        return
    endif

    if ( version_file /= version_string ) then
        !
        ! As there is no history of versions yet, the version in the file
        ! should match exactly
        !
        return
    endif

    !
    ! Determine the MD5 checksum for the network file - it must match the
    ! checksum in the cache file
    !
    call md5file( netfile, md5current, success )

    if ( md5checksum /= md5current .or. .not. success ) then
        close( lun ) 
        return
    endif


    !
    ! Load the observation points:
    ! Copy the node numbers when successful
    !
    read( lun, iostat = ierr ) key, number

    if ( ierr /= 0 .or. key /= section(key_obs) ) then
        close( lun )
        return
    endif

    allocate( cache_xobs(number), cache_yobs(number), cache_kobs(number), cache_locTpObs(number) )

    read( lun, iostat = ierr ) cache_xobs      ; okay = ierr == 0
    read( lun, iostat = ierr ) cache_yobs      ; okay = okay .and. ierr == 0
    read( lun, iostat = ierr ) cache_locTpObs  ; okay = okay .and. ierr == 0
    read( lun, iostat = ierr ) cache_kobs      ; okay = okay .and. ierr == 0

    if ( .not. okay ) then
        close( lun )
        return
    endif

    !
    ! All cached values were loaded, so all is well
    !
    close( lun )
    cache_success = .true.

end subroutine loadCachingFile

!> Store the grid-based information in the caching file
subroutine storeCachingFile( filename )
    character(len=*) :: filename

    integer :: lun
    integer :: ierr

    cache_success = .false.

    open( newunit = lun, file = trim(filename) // ".cache", access = "stream", status = "old", action = 'read',  iostat = ierr )
    if ( ierr == 0 ) then
        close( lun, status = "delete" )
    endif
    open( newunit = lun, file = trim(filename) // ".cache", access = "stream" )

    !
    ! Store version string and checksum (already determined at start-up)
    !
    write( lun ) version_string, md5current

    !
    ! Store the observation points
    !
    write( lun ) section(key_obs), numobs
    write( lun ) xobs, yobs, locTpObs, kobs

    !
    ! We are done, so close the file
    !
    close( lun )

end subroutine storeCachingFile

subroutine copyCachedObservations( success )
    logical, intent(out) :: success

    success = .false.
    if ( cache_success ) then
        !
        ! Check the number of observations
        !
        if ( numobs /= size(cache_xobs) ) then
            return
        endif
        !
        ! Check that the coordinates and the type are identical to the cached values
        !
        if ( all( cache_xobs == xobs ) .and. all( cache_yobs == yobs ) .and. all( cache_locTpObs == locTpObs ) ) then
            success = .true.
            kobs    = cache_kobs
        endif
    endif
end subroutine copyCachedObservations

end module unstruc_caching
