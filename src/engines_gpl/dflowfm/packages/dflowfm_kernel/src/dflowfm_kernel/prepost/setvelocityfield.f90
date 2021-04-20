 Subroutine setvelocityfield()
 use m_flow
 use m_flowgeom
 implicit none
 integer          :: k,k1,k2,L
 double precision :: xx,yy,ux,uy,yyy,uuu, ykmx

 uy = -0.5d0
 ux =  0.5d0*sqrt(3d0)

 ykmx = 100d0 ! 0d0

 do k = 1,ndx
    xx     = xz(k)
    yy     = yz(k) ! ykmx - yz(k)

    if (iuvfield == 1) then                          ! kwadratic horizontal
       ucx(k) = yy*yy
       ucy(k) = 0
    else if (iuvfield == 2) then                     ! kwadratic 30 degrees
       yyy    = -uy*xx + ux*yy
       uuu    = yyy*yyy
       ucx(k) = uuu*ux
       ucy(k) = uuu*uy
    else if (iuvfield == 3) then                     ! circular
       ucx(k) = -yy
       ucy(k) =  xx
    else if (iuvfield == 4) then                     ! linear horizontal
       ucx(k) =  yy
       ucy(k) =  0
    else if (iuvfield == 5) then                     ! linear 30 degrees
       yyy    = -uy*xx + ux*yy
       uuu    = yyy
       ucx(k) = uuu*ux
       ucy(k) = uuu*uy
    else if (iuvfield == 6) then                     ! random
       ucx(k) = 2 + sin(0.1d0*k)
       ucy(k) =     cos(1.5d0*k)
    endif

 enddo
 do L = 1,lnx
    k1 = ln(1,L) ; k2 = ln(2,L)
    u1(L)  = ( (1d0-acl(L))*ucx(k1) + acl(L)*ucx(k2) )*csu(L)  +   &  ! reversed acl weighting
             ( (1d0-acl(L))*ucy(k1) + acl(L)*ucy(k2) )*snu(L)
 enddo

 u0 = u1
 s0 = s1

 call setcornervelocities()

 end subroutine setvelocityfield
