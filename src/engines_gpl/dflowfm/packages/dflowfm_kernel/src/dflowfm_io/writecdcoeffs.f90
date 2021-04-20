 subroutine writeCdcoeffs()
 use unstruc_model
 use m_wind
 use m_waves
 use m_flow,     only : jawave
 use m_flowgeom, only : ndx
 implicit none

 integer          :: msgbu, k
 double precision :: uwi, Cd10, fetchL, fetchD, hsig, tsig
 double precision, allocatable :: hwavsav(:), twavsav(:)

 call newfil(msgbu, trim(getoutputdir())//trim(md_ident)//'_Cdwcoeff.tek')

 write (msgbu, '(a)')    '* Wind Cdcoefficient relation : '
 if (icdtyp == 1) then
    write (msgbu, '(a)') '* Constant'
 else if (icdtyp == 2 ) then
    write (msgbu, '(a)') '* Smith and Banks 2 breakpoints'
 else if (icdtyp == 3 ) then
    write (msgbu, '(a)') '* Smith and Banks like 3 breakpoints'
 else if (icdtyp == 4) then
    write (msgbu, '(a)') '* Charnock 1955 (1 parameter)'
 else if (icdtyp == 5) then
    write (msgbu, '(a)') '* Hwang 2005, wave frequency dependent (fixed parameters + wave period)'
 else if (icdtyp == 6) then
    write (msgbu, '(a)') '* Wuest 2003 & Smith en Banke (fixed parameters)'
 else if (icdtyp == 7) then
    write (msgbu, '(a)') '* Hans Hersbach, July 2010, ECMWF fit (CHarnock plus viscous term), (e.g. Charnock=0.018 and alfvisc=0.11)'
 endif

 write (msgbu, '(a)')     '* column 1 : Wind speed               (m/s) '
 write (msgbu, '(a)')     '* column 2 : Resulting Cd coefficient (   ) '
 if (jawave > 0) then
    write (msgbu, '(a)')  '* column 3 : Hwav                     (m  ) '
    write (msgbu, '(a)')  '* column 4 : Twav                     (s  ) '
    write (msgbu, '(a)')  '61  4'
 else
    write (msgbu, '(a)')  '61  2'
 endif

 if (jawave > 0) then
    fetchL = 20000d0
    fetchD = 4d0
    allocate ( hwavsav(ndx), twavsav(ndx) )
    hwavsav = hwav ; twavsav = twav
 endif

 uwi = 0.1d0
 if (jawave > 0) then
    call hurdlestive (Uwi, fetchL, fetchD, Hsig, Tsig)
    hwav = hsig ; twav = tsig
 endif
 call setcdwcoefficient(uwi, Cd10, 1)
 if (jawave > 0) then
    write(msgbu, '(4F14.6)') uwi, Cd10, hsig, tsig
 else
    write(msgbu, '(2F14.6)') uwi, Cd10
 endif

 uwi = 0.2d0
 if (jawave > 0) then
    call hurdlestive (Uwi, fetchL, fetchD, Hsig, Tsig)
    hwav = hsig ; twav = tsig
 endif
 call setcdwcoefficient(uwi, Cd10, 1)
 if (jawave > 0) then
    write(msgbu, '(4F14.6)') uwi, Cd10, hsig, tsig
 else
    write(msgbu, '(2F14.6)') uwi, Cd10
 endif

 do k = 1, 28
    uwi = uwi + 0.2d0
    if (jawave > 0) then
       call hurdlestive (Uwi, fetchL, fetchD, Hsig, Tsig)
       hwav = hsig ; twav = tsig
    endif
    call setcdwcoefficient(uwi, Cd10, 1)
    if (jawave > 0) then
       write(msgbu, '(4F14.6)') uwi, Cd10, hsig, tsig
    else
       write(msgbu, '(2F14.6)') uwi, Cd10
    endif
 enddo

 do k = 1, 24
    uwi = uwi + 1.0d0
    if (jawave > 0) then
       call hurdlestive (Uwi, fetchL, fetchD, Hsig, Tsig)
       hwav = hsig ; twav = tsig
    endif
    call setcdwcoefficient(uwi, Cd10, 1)
    if (jawave > 0) then
       write(msgbu, '(4F14.6)') uwi, Cd10, hsig, tsig
    else
       write(msgbu, '(2F14.6)') uwi, Cd10
    endif
 enddo

 do k = 1, 7
    uwi = uwi + 10d0
    if (jawave > 0) then
       call hurdlestive (Uwi, fetchL, fetchD, Hsig, Tsig)
       hwav = hsig ; twav = tsig
    endif
    call setcdwcoefficient(uwi, Cd10, 1)
    if (jawave > 0) then
       write(msgbu, '(4F14.6)') uwi, Cd10, hsig, tsig
    else
       write(msgbu, '(2F14.6)') uwi, Cd10
    endif
 enddo

 call doclose(msgbu)

 if (jawave > 0) then
    hwav = hwavsav ; twav = twavsav
    deallocate (hwavsav, twavsav)
 endif

 end subroutine writeCdcoeffs
