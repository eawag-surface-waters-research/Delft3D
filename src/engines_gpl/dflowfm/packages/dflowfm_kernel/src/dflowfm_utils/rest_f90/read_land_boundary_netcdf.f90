!> Read land boundary from world vector shoreline files (NetCDF format)
!! Global Self-consistent Hierarchical High-resolution Shorelines (GSHHS)
!! http://opendap.deltares.nl/thredds/catalog/opendap/noaa/gshhs/catalog.html
!! Directly stored in m_landboundary module variables.
subroutine read_land_boundary_netcdf(filename)
    use M_landboundary
    USE M_MISSING
    use netcdf

    implicit none

    character(len=*), intent(in)  :: fileName

    double precision, dimension(:), allocatable :: x_lan, y_lan
    integer, dimension(:), allocatable          :: k
    integer :: i

    integer :: status, istart, istop
    integer :: id_nc, id_lon, id_lat,id_npts, id_sep, id_k
    integer :: npts, nsep
    integer :: max_vertex, n_vertex
    logical :: succes

!    write(msgtxt,'("Reading file: ",a)') trim(get_basename( get_filename(filename) ) )

    ! Read the land boundary geometry from netcdf-file.

    succes = .false.

    status = nf90_open(trim(filename), NF90_NOWRITE, ncid=id_nc)
!    if (status .ne. nf90_noerr) msgtxt = nf90_strerror(status)

    status = nf90_inq_dimid(id_nc, "npoints", id_npts)
    status = nf90_inquire_dimension(id_nc, id_npts, len=npts)
    status = nf90_inq_dimid(id_nc, "segment_separators", id_sep)
    status = nf90_inquire_dimension(id_nc, id_sep, len=nsep)

    !call setTotalSteps(pbar, 5+nsep)

    allocate(k(nsep))
    status = nf90_inq_varid(id_nc, "k", id_k)
    status = nf90_get_var(id_nc, id_k, k, count=(/ nsep /))
    status = nf90_inq_varid(id_nc, "lon", id_lon)
!    if (status .ne. nf90_noerr) msgtxt = nf90_strerror(status)

    status = nf90_inq_varid(id_nc, "lat", id_lat)
!    if (status .ne. nf90_noerr) msgtxt = nf90_strerror(status)

    ! Determine largest landboundary segment
    max_vertex = 0
    do i = 1, nsep-1
        max_vertex = max(max_vertex, k(i+1)-k(i)-1 )
    enddo
    !allocate(x_lan(npts))
    !allocate(y_lan(npts))
    call increaselan(npts)
    !call setProgress(pbar, 5)

    status = nf90_get_var(id_nc, id_lon, xlan, count=(/ npts /))
    status = nf90_get_var(id_nc, id_lat, ylan, count=(/ npts /))

    !call multiFeatureSetCapacity(polylines, nsep-1+10)
    do i = 1, nsep
       ! if (mod(i,100)==0) call setProgress(pbar, 5+i)
        !istart   = k(i  ) + 1
        !istop    = k(i+1) - 1
        !n_vertex = istop -istart + 1

        ! Replace NetCDF NaNs by our dmiss vals at segment separator positions, that's all.
        xlan(k(i)) = dmiss
        ylan(k(i)) = dmiss

        !
        !polyline  = newpolyline()
        !call addPointArray( polyline, n_vertex, x_lan(istart:istop), y_lan(istart:istop))
        !
        !call multifeatureaddfeature(polylines, polyline)
    enddo
    MXLAN = npts
    status = nf90_close(id_nc)

    !call setProgress(pbar, nsep)
    !call free(pbar)

    deallocate(k)
    !deallocate(x_lan)
    !deallocate(y_lan)

    if (status==0) then
        succes = .true.
    endif
end subroutine read_land_boundary_netcdf
