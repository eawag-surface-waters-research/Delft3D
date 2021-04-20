 subroutine sincosdis(x1,y1,x2,y2,s,c,d)    ! get sin, cos, length of a line segment
 use m_missing
 use m_sferic, only: jsferic
 use geometry_module, only: getdx, getdy
 implicit none
 double precision :: x1,y1,x2,y2,s,c,d
 double precision :: dx1,dy1,dx2,dy2

 dx1 = getdx(x1,y1,x2,y2,jsferic)
 dy1 = getdy(x1,y1,x2,y2,jsferic)
 d   = sqrt(dx1*dx1 + dy1*dy1)
 if (d > 0d0) then
    s  = dy1/d
    c  = dx1/d
 else
    s  = 0d0
    c  = 0d0
 endif
 end subroutine sincosdis
