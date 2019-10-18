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
                                                            'FIXED WEIRS         ', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890']
    integer, parameter, private :: key_obs = 1
    integer, parameter, private :: key_fixed_weirs = 2

    double precision, dimension(:), allocatable, private :: cache_xobs
    double precision, dimension(:), allocatable, private :: cache_yobs
    double precision, dimension(:), allocatable, private :: cache_xpl_fixed
    double precision, dimension(:), allocatable, private :: cache_ypl_fixed
    double precision, dimension(:), allocatable, private :: cache_dsl_fixed
    integer, dimension(:), allocatable, private          :: cache_locTpObs
    integer, dimension(:), allocatable, private          :: cache_kobs
    integer, dimension(:), allocatable, private          :: cache_ilink_fixed
    integer, dimension(:), allocatable, private          :: cache_ipol_fixed

    character(len=30), private :: version_string = "D-Flow FM, cache file, 1.0"
    character(len=14), private :: md5current

contains
!> Check that the caching file contained compatible information
logical function cacheRetrieved()
    cacheRetrieved = cache_success
end function cacheRetrieved

!> Load the information from the caching file - if any
subroutine loadCachingFile( filename, netfile )
    character(len=*), intent(in) :: filename      !< Name of MDU file (used to construct the name of the caching file)
    character(len=*), intent(in) :: netfile       !< Full name of the network file

    integer :: lun
    integer :: ierr
    integer :: number, number_links
    character(len=30) :: version_file
    character(len=20) :: key
    character(len=14) :: md5checksum
    logical :: okay
    logical :: success

    cache_success = .false.
    return

    !
    ! Allocate the arrays to zero length
    !
    allocate( cache_xobs(0), cache_yobs(0), cache_xpl_fixed(0), cache_ypl_fixed(0), cache_dsl_fixed(0), &
              cache_locTpObs(0), cache_kobs(0), cache_ilink_fixed(0), cache_ipol_fixed(0) )

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

    deallocate( cache_xobs, cache_yobs, cache_locTpObs, cache_kobs )

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
    ! Load the information on the fixed weirs:
    ! Copy the node numbers when successful
    !
    read( lun, iostat = ierr ) key, number, number_links

    if ( ierr /= 0 .or. key /= section(key_fixed_weirs) ) then
        close( lun )
        return
    endif

    deallocate( cache_xpl_fixed, cache_ypl_fixed, cache_ilink_fixed, cache_ipol_fixed, cache_dsl_fixed )

    allocate( cache_xpl_fixed(number), cache_ypl_fixed(number) )
    allocate( cache_ilink_fixed(number_links), cache_ipol_fixed(number_links), cache_dsl_fixed(number_links) )

    read( lun, iostat = ierr ) cache_xpl_fixed   ; okay = ierr == 0
    read( lun, iostat = ierr ) cache_ypl_fixed   ; okay = okay .and. ierr == 0
    read( lun, iostat = ierr ) cache_ilink_fixed ; okay = okay .and. ierr == 0
    read( lun, iostat = ierr ) cache_ipol_fixed  ; okay = okay .and. ierr == 0
    read( lun, iostat = ierr ) cache_dsl_fixed   ; okay = okay .and. ierr == 0

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
    character(len=*) :: filename            !< Name of the MDU file (to construct the name of the caching file)

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
    if ( numobs > 0 ) then
        write( lun ) xobs, yobs, locTpObs, kobs
    endif

    !
    ! Store the fixed weirs data
    !
    write( lun ) section(key_fixed_weirs), size(cache_xpl_fixed), size(cache_ilink_fixed)

    if ( size(cache_xpl_fixed) > 0 ) then
        write( lun ) cache_xpl_fixed, cache_ypl_fixed, cache_ilink_fixed, cache_ipol_fixed, cache_dsl_fixed
    endif

    !
    ! We are done, so close the file
    !
    close( lun )

end subroutine storeCachingFile

!> Copy the cached network information for observation points
subroutine copyCachedObservations( success )
    logical, intent(out) :: success             !< The cached information was compatible if true

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

!> Copy the cached information on fixed weirs
subroutine copyCachedFixedWeirs( npl, xpl, ypl, number_links, iLink, iPol, dSL, success )
    integer, intent(in)                         :: npl                !< Number of vertices in the polygons making up the weirs
    double precision, dimension(:), intent(in)  :: xpl                !< X-coordinates of the vertices for the weirs
    double precision, dimension(:), intent(in)  :: ypl                !< Y-coordinates of the vertices for the weirs

    integer, intent(out)                        :: number_links       !< Number of links that was cached
    double precision, dimension(:), intent(out) :: dSL                !< Distances along the links that were cached
    integer, dimension(:), intent(out)          :: iLink              !< Cached lInkage information
    integer, dimension(:), intent(out)          :: iPol               !< Cached polygon information
    logical, intent(out)                        :: success            !< The cached information was compatible if true

    success = .false.
    if ( cache_success ) then
        !
        ! Check the number of coordinate pairs
        !
        if ( npl /= size(cache_xpl_fixed) ) then
            return
        endif
        !
        ! Check that the coordinates are identical to the cached values
        !
        if ( all( cache_xpl_fixed == xpl(1:npl) ) .and. all( cache_ypl_fixed == ypl(1:npl) ) ) then
            success      = .true.
            number_links = size(cache_iLink_fixed)
            iLink(1:number_links) = cache_iLink_fixed
            iPol(1:number_links)  = cache_iPol_fixed
            dSL(1:number_links)   = cache_dSL_fixed
        endif
    endif
end subroutine copyCachedFixedWeirs

!> cacheFixedWeirs:
!>     The arrays for fixed weirs are partly local - they do not reside in a
!>     module, so explicitly store them when we have the actual data
!
subroutine cacheFixedWeirs( npl, xpl, ypl, number_links, iLink, iPol, dSL )
    integer, intent(in)                        :: npl             !< Number of vertices in the polygons making up the weirs
    integer, intent(in)                        :: number_links    !< Number of links that is to be cached
    double precision, dimension(:), intent(in) :: xpl             !< X-coordinates of the vertices for the weirs
    double precision, dimension(:), intent(in) :: ypl             !< Y-coordinates of the vertices for the weirs
    double precision, dimension(:), intent(in) :: dSL             !< Distances along the links that are to be cached
    integer, dimension(:), intent(in)          :: iLink           !< LInkage information to be cached
    integer, dimension(:), intent(in)          :: iPol            !< Polygon information to be cached

    cache_xpl_fixed   = xpl(1:npl)
    cache_ypl_fixed   = ypl(1:npl)
    cache_iLink_fixed = iLink(1:number_links)
    cache_iPol_fixed  = iPol(1:number_links)
    cache_dSL_fixed   = dSL(1:number_links)
end subroutine cacheFixedWeirs

end module unstruc_caching
