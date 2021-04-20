 double precision function dprodin(x1,y1,x2,y2,x3,y3,x4,y4)    ! inner product of two segments
 use m_missing
 use m_sferic
 use geometry_module, only: getdx, getdy, sphertoCart3D
 implicit none
 double precision :: x1,y1,x2,y2,x3,y3,x4,y4
 double precision :: dx1,dy1,dx2,dy2

 double precision, dimension(4) :: xx, yy, zz
 double precision               :: dz1, dz2

 if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
    call sphertocart3D(x1, y1, xx(1), yy(1), zz(1))
    call sphertocart3D(x2, y2, xx(2), yy(2), zz(2))
    call sphertocart3D(x3, y3, xx(3), yy(3), zz(3))
    call sphertocart3D(x4, y4, xx(4), yy(4), zz(4))

    dx1 = xx(2)-xx(1)
    dy1 = yy(2)-yy(1)
    dz1 = zz(2)-zz(1)

    dx2 = xx(4)-xx(3)
    dy2 = yy(4)-yy(3)
    dz2 = zz(4)-zz(3)

    dprodin = dx1*dx2 + dy1*dy2 + dz1*dz2
 else

    dx1 = getdx(x1,y1,x2,y2,jsferic)
    dx2 = getdx(x3,y3,x4,y4,jsferic)

    dy1 = getdy(x1,y1,x2,y2,jsferic)
    dy2 = getdy(x3,y3,x4,y4,jsferic)

    dprodin = (dx1*dx2 + dy1*dy2)
 end if

 return
 end function dprodin
