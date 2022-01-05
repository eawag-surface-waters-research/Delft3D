subroutine ddc_version(lunrep)

    ! print version to report file

    integer, intent(in)      :: lunrep        ! unit number report file

    ! local variables

    character(len=120)       :: idstr         ! identification string
    character(len=20)        :: rundat        ! date and time string
    integer                  :: lennam        ! length of a string (dummy here)

    ! set version

    call getfullversionstring_DDCOUPLE(idstr)
    k = len_trim(idstr)

    ! write credentials to report file

    call dattim(rundat)
    write(lunrep, *)
    write(lunrep, '(a)') idstr(5:k)
    write(lunrep, *)
    write(lunrep,'(2a)') ' execution start: ',rundat
    write(lunrep,*)

    return
end subroutine
