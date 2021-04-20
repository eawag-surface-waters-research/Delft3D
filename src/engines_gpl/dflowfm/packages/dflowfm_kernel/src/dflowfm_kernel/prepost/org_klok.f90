  subroutine org_klok(cpu)  ! for true performance monitoring, wallclock gives more meaningfull information than cpuclock
  implicit none
  INTEGER, DIMENSION(8) :: IV
  double precision      :: cpu

  CALL DATE_AND_TIME(VALUES=IV)

  cpu = 3600*iv(5) + 60*iv(6) + iv(7) + 0.001d0*iv(8)

  end subroutine org_klok
