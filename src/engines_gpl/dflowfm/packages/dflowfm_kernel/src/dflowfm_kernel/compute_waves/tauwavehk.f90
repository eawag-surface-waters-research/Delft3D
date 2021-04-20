 subroutine tauwavehk(Hrms, Tsig, Depth, Uorbi, rlabd, ust)
 use m_flow, only: plotlin, rhog, rhomean, jased
 use m_sferic
 use m_waves, only : gammax, jauorb

 implicit none
 double precision           :: Hrms, Tsig, Depth, uorbi, Tauw, hrm, ust
 integer                    :: k, jatauw = 2
 double precision           :: hk, sh2hk,hksh2,rn,asg,ew,sxx,syy,sxy,syx,dtau,shs, h2k, cp, cg, omeg
 double precision           :: dsk2, rk, rkx, rky, astar, fw, cgcp, rk2cgcp,  cgcp5, arms, rlabd

 double precision, external :: tanhsafe, sinhsafe, sinhsafei
 integer :: ndraw
 COMMON /DRAWTHIS/ ndraw(50)


 if (depth < 0.1d0 .or. Tsig == 0) then
    Uorbi = 0d0 ; rlabd = 0d0 ; ust = 0d0
 else
    call getwavenr(depth,tsig,rk)
    hrm    = min( Hrms,gammax*depth )
    arms   = 0.5d0*hrm
    omeg   = twopi/tsig
    shs    = sinhsafei(rk*depth)
    uorbi  = omeg*arms*shs                        !omeg*(0.5*hsig)
    if (jauorb==0) then              ! for consistency with old d3d convention
       uorbi = uorbi*sqrt(pi)/2d0
    end if
    ust    = 0.5d0*omeg*arms*arms/depth
    rlabd  = twopi/rk
 endif

 return

 if (ndraw(28) > 40) then
    omeg    = twopi/tsig                        ! omega
    cp      = omeg/rk                           ! fase velocity
    hk      = rk*depth                          ! kh
    sh2hk   = sinhsafei(2d0*hk)                 ! 1/sinh(2hk)
    hksh2   = hk*sh2hk                          ! kh/sinh(2kh)
    cgcp    = 0.5d0 + hksh2                     ! cg/cp
    cg      = cp*cgcp                           ! group velocity
    asg     = 0.5d0*hrms                        ! rms wave amplitude
    ew      = 0.5d0*rhog*asg*asg                ! wave energy
    !ustokes(z) =       rk*omeg*asg*asg*exp(2d0*rk*z)                         ! Vertical Stokes drift profile deep water
    !ustokes    =    0.5d0*omeg*asg*asg/depth                                 ! deep water vertical averaged
    !ustokes(z) = 0.5d0*rk*omeg*asg*asg*cosh(2d0*rk*(z+depth)/sinh2(rk*depth) ! Stokes drift profile  (5) Monismithetal2007.pdf

    Sxx     = ew*(0.5d0 + 2d0*hksh2)            ! radiation stress in wave dir
    Syy     = ew*hksh2                          ! radiation stress perpendicular to wave dir

    rk2cgcp = rk*rk*cgcp                        ! or, Wikipedia
    cgcp5   = cgcp - 0.5d0
    Sxx     = ew*( rkx*rkx/rk2cgcp + cgcp5 )
    Syy     = ew*( rky*rky/rk2cgcp + cgcp5 )
    Syx     = ew*( rkx*rky/rk2cgcp + 0d0   )
    Sxy     = Syx

    ! standard deviation or RMS of sine wave a*sin(om*t) : 0.5*sqrt(2)*a
    ! Hsig or HRMS is equal to 4 times RMS
    ! Hsig = 4*0.5*sqrt(2)*a = 2*sqrt(2)*a = 2.8*a

 endif

 end subroutine tauwavehk
