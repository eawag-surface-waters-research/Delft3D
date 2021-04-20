subroutine wricom(tim)
    use m_flow
    use m_flowtimes
    use m_observations
    use unstruc_netcdf
    use unstruc_model
    use unstruc_files , only: defaultFilename
    implicit none
    double precision, intent(in) :: tim

    ! locals

    type(t_unc_mapids), save :: comids
    integer                  :: ierr
    character(len=256), save :: filnam
    character(len=256)       :: msg
    logical                  :: file_exists

    ! When leaving netcdf-file open and using nf90_sync:
    ! Data did not appear during debugging
    ! This problem was solved by closing/opening the file everytime
    !
    if (comids%ncid/=0 .and. jawave==3) then
       !
       ! Existing/ongoing communication via com file:
       ! com file already exists
       !
       ierr = nf90_open(filnam, NF90_WRITE, comids%ncid)
    elseif (comids%ncid==0 .and. jawave==3) then
        !
        ! No communication yet via com file:
        ! Check whether com file exists
        !
        filnam = defaultFilename('com')
        md_wavefile = filnam
        inquire(file=filnam,exist=file_exists)
        if ( file_exists ) then
            write(msg,'(3a)') "File '",trim(filnam), "' already exists. Assuming that it contains valid WAVE information. FLOW data will be added."
            call mess(LEVEL_WARN, trim(msg))
            ierr = nf90_open(filnam, NF90_WRITE, comids%ncid)
        else
            ! No com file yet. Create a new one and write FLOW parameters
            !
            ierr = unc_create(filnam , 0, comids%ncid)
            if (ierr /= nf90_noerr) then
                call mess(LEVEL_WARN, 'Could not create com file.')
                comids%ncid = 0
            endif
        endif
    endif

    if (comids%ncid /= 0) then
        call unc_write_map_filepointer(comids%ncid,tim, 2)
    endif

    ierr = nf90_close(comids%ncid) ! Flush file
 end subroutine wricom
