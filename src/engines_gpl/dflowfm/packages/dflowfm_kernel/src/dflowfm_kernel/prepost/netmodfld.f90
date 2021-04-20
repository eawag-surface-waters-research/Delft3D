!> network field move
!!   Is is assumed that there is a backup copy of the grid.
subroutine netmodfld(xp,yp,zp,kp)
 use m_netw
 use m_grid
 use m_alloc
 use m_missing
 use m_wearelt
 use m_sferic
 use geometry_module, only: dbdistance

 implicit none

 double precision           :: xp, yp, zp     !< coordinates that determine the influenced region

 integer                    :: kp             !< center point index

 double precision           :: Dx0, Dy0, rsx, xn, yn, dist, frac
 double precision           :: xcen, ycen
 double precision, external :: getDx, getDy

 integer                    :: i

 xcen = xk(kp)
 ycen = yk(kp)

 Dx0 = xp - xcen
 Dy0 = yp - ycen

 rsx = max(dsix, sqrt(Dx0*Dx0 + Dy0*Dy0))

 do i=1,numk
    xn     = xk(i)
    yn     = yk(i)
! intentional not in sferical coordinates
    dist  = sqrt( (xn-xcen)**2 + (yn-ycen)**2 )
    frac  = 0.5 * (1+ cos(min(max(dist/rsx,-1d0),1d0) * pi))

    xk(i) = xk(i) + Dx0*frac
    yk(i) = yk(i) + Dy0*frac
 end do


end subroutine netmodfld
