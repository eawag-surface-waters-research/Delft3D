 subroutine setwor_rai(xs1,ys1,xs2,ys2,xw1,yw1,xw2,yw2)
 use m_raaitek
 implicit none
 real             :: xs1,ys1,xs2,ys2
 double precision :: xw1,yw1,xw2,yw2
 call viewport(xs1,ys1,xs2,ys2)
 call setwor  (xw1,yw1,xw2,yw2)
 xs1m = xs1
 ys1m = ys1
 xs2m = xs2
 ys2m = ys2
 xw1m = xw1
 yw1m = yw1
 xw2m = xw2
 yw2m = yw2
 end subroutine setwor_rai
