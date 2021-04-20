  subroutine NEWklok(cpu)
  implicit none
  double precision :: cpu
  real :: currentcpu

  call cpu_time(currentcpu)
  cpu = currentcpu

  end subroutine NEWklok
