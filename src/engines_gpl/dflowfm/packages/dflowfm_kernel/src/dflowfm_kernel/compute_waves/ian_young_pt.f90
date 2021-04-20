 SUBROUTINE ian_young_pt(U10,x,d,Hsig,Tsig)
  use m_physcoef
  IMPLICIT NONE
  double precision , INTENT(IN)  :: d,U10,x
  double precision , INTENT(OUT) :: Hsig, Tsig
  double precision               :: E,fp
  double precision               :: delta, XX, A1, B1, epsilon, nu, A2, B2, ta1, ta2
  double precision, external     :: tanhsafe
  XX=ag*x/U10**2             ! non-dim fetch
  delta=ag*d/U10**2          ! non-dim depth
                             ! calculate nondimensional energy
  B1=3.13e-3*XX**0.57
  A1=0.493*delta**0.75
  ta1=tanhsafe(A1)
  epsilon=3.64e-3*(tA1*tanhsafe(B1/ta1))**1.74
                             ! calculate nondimensional frequency
  B2=5.215e-4*XX**0.73
  A2=0.331*delta**1.01
  ta2=tanhsafe(A2)
  nu=0.133*(tA2*tanhsafe(B2/tA2))**(-0.37);
  E =U10**4*epsilon/ag**2    ! total energy from non-dim energy
  Hsig=4*SQRT(E)             ! significant wave height
  fp=nu*ag/U10               ! peak freq from non-dim freq, Hz
  Tsig=1d0/fp
 END SUBROUTINE ian_young_pt
