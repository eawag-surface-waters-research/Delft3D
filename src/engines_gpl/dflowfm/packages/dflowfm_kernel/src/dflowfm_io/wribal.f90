!> Writes the current water balance quantities to file.
!! File format is ascii, one time per line, all quantities in columns.
subroutine wribal(tim)
    use m_flowtimes
    use m_flow
    use unstruc_files, only: defaultFilename

    implicit none

    double precision   :: tim

    ! locals
    integer, save      :: ibal = 0
    integer            :: ierr, ntbal
    character(len=256) :: nam

    if (ibal /= 0 .and. dnt == 1) then  ! volerr, volerrcum
        call doclose(ibal)
        ibal = 0
    end if

    if (ibal == 0) then
        nam  = defaultFilename('bal')
        call newfil(ibal, nam)

        write(ibal,'(a)') '* column 1  : Time (min) '
        write(ibal,'(a)') '* column 2  : Volume rain in       (m3) '
        write(ibal,'(a)') '* column 3  : Volume evap out      (m3) '
        write(ibal,'(a)') '* column 4  : Volume bnd  in       (m3) '
        write(ibal,'(a)') '* column 5  : Volume bnd  out      (m3) '
        write(ibal,'(a)') '* column 6  : Volume grw  in       (m3) '
        write(ibal,'(a)') '* column 7  : Volume grw  out      (m3) '
        write(ibal,'(a)') '* column 8  : Volume flow          (m3) '
        write(ibal,'(a)') '* column 9  : Volume flow ini      (m3) '
        write(ibal,'(a)') '* column 10 : Volume grw           (m3) '
        write(ibal,'(a)') '* column 11 : Volume grw  ini      (m3) '
        write(ibal,'(a)') '* column 12 : Flux rain in         (m3/s)'
        write(ibal,'(a)') '* column 13 : Flux evap out        (m3/s)'
        write(ibal,'(a)') '* column 14 : Flux bnd  in         (m3/s)'
        write(ibal,'(a)') '* column 15 : Flux bnd  out        (m3/s)'
        write(ibal,'(a)') '* column 16 : Flux grw  in         (m3/s)'
        write(ibal,'(a)') '* column 17 : Flux grw  out        (m3/s)'
        write(ibal,'(a)') '* in means into surface water, out means out of surface water'

        write(ibal,'(a)') 'BL01'
        ntbal = 1 + int(Tstop_user - Tstart_user) / Ti_xls
        write(ibal,'(i0, a)') ntbal, '   17'

    end if

    write(ibal,'(100(F20.4))') time1/60d0,                                                                          &
    vinraincum, voutevacum, vinbndcum, voutbndcum, vingrwcum, voutgrwcum, vol1tot, vol1ini, volgrw, volgrwini,      &
    qinrain   , qouteva   , qinbnd   , qoutbnd   , qingrw   , qoutgrw

end subroutine wribal
