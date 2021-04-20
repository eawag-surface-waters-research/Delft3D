  LOGICAL FUNCTION RECHTSAF_active(K1,K2,K3)

  use m_netw
  use geometry_module, only : duitpl
  use m_sferic, only: jsferic

  implicit none
  integer :: K1, K2, K3

  double precision :: sig

  rechtsaf_active = .false.

  call duitpl(xk(k1), yk(k1), xk(k2), yk(k2), xk(k2), yk(k2), xk(k3), yk(k3), sig, jsferic)
  if (sig < 0) then
      rechtsaf_active = .true.
  else
      rechtsaf_active = .false.
  endif

  return
  end FUNCTION RECHTSAF_active
