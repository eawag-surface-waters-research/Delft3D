subroutine wrirst(tim)
    use m_flow
    use m_flowtimes
    use m_observations
    use unstruc_netcdf
    use unstruc_model
    use unstruc_files , only: defaultFilename
    implicit none
    double precision, intent(in) :: tim

    ! locals
    integer, save      :: irstfile = 0
    integer            :: ierr
    character(len=256) :: filnam

    if (irstfile == 0) then
        filnam = defaultFilename('rst', timestamp=tim ) ! dble(floor(tim+.5d0)))
        ierr   = unc_create(filnam , 0, irstfile)
        if (ierr /= nf90_noerr) then
            call mess(LEVEL_WARN, 'Could not create rst file.')
            irstfile = 0
        end if
    endif

    if (irstfile .ne. 0) then
        call unc_write_rst_filepointer(irstfile,tim)
    endif

    ierr = unc_close(irstfile) ! Do more than flushing: close the file, it is not needed anymore

 end subroutine wrirst
