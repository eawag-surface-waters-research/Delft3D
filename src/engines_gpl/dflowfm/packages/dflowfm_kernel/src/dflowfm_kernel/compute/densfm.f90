double precision function densfm(sal,temp)
use m_physcoef
use m_flow
double precision           :: sal, temp
double precision, external :: rho_Eckart, rho_Unesco

if (idensform == 0) then               ! Uniform density
    densfm = rhomean
    return
else if (abs(idensform) == 1) then     ! Carl Henry Eckart, 1958
    densfm = rho_Eckart(sal,temp)
else if (abs(idensform) == 2) then     ! Unesco
    densfm = rho_Unesco(sal,temp)
else if (abs(idensform) == 3) then     ! Baroclinic instability
    densfm = 1025d0 + 0.78d0*(sal - 33.73d0)
else if (abs(idensform) == 4) then     ! Test baroclni pressure term 'dicht.mdu'
    densfm = 2d0*rhomean
else if (abs(idensform) == 5) then     ! For Deltares flume experiment IJmuiden , Kees Kuipers saco code 1
    densfm = 999.904d0          + 4.8292d-2*temp - 7.2312d-3*temp**2 + &
             2.9963d-5*temp**3  + 7.6427d-1*sal  -                     &
             3.1490d-3*sal*temp + 3.1273d-5*sal*temp**2
endif
end function densfm
