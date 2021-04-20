  subroutine DISPFORMscale(value,fmt,NDEC)
  implicit none
  integer :: n1
  integer :: n2
  integer :: n3 ! nr of digits behind decimal dot
  integer :: ndec
  double precision :: value
  character fmt*(*)

  fmt='(f10.3)'

  if (value .eq. 0d0) then
     fmt='(f3.1)'
     return
  endif

  n1 = int(log10(abs(value)))

  if (n1 .lt. 6 .and. n1 .gt. 0) then
     n2 = min(9,n1 + 3)
     n3 = 9 - n2
  else if (n1 .ge. -5 .and. n1 .lt. 0) then
     n3 = 6
  else if ( n1 .eq. 0) then
     n3 = 6
  else
     fmt ='(e10.3)'
     return
  endif

  IF (NDEC .GT. 0) then
     n3 = min(n3, NDEC) ! try ndec, but only if it fits
  end if

  write (fmt(6:6),'(i1)') n3
  return
  end
