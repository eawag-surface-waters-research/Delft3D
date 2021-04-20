 subroutine htext_rai(val,x,y,xx,zz,ihv)
 use m_raaitek
 implicit none
 double precision  :: val,x,y,xx,zz
 double precision  :: fx, fy, xa, ya
 integer           :: ihv
 fx = xs2m-xs1m
 fy = ys2m-ys1m
 if (ihv == 1) then
    xa = fx*(x-xx-xw1m)/(xw2m-xw1m) + xs1m
    ya = fy*(y   -yw1m)/(yw2m-yw1m) + ys1m
    call movabs(xa,ya)
    xa = fx*(x+xx-xw1m)/(xw2m-xw1m) + xs1m
    call lnabs (xa,ya)
    xa = fx*(x-11d0*xx-xw1m)/(xw2m-xw1m) + xs1m
 else if (ihv == 2) then
    xa = fx*(x   -xw1m)/(xw2m-xw1m) + xs1m
    ya = fy*(y-zz-yw1m)/(yw2m-yw1m) + ys1m
    call movabs(xa,ya)
    ya = fy*(y+zz-yw1m)/(yw2m-yw1m) + ys1m
    call lnabs (xa,ya)
    xa = fx*(x-5d0*xx-xw1m)/(xw2m-xw1m) + xs1m
    ya = fy*(y-3d0*zz-yw1m)/(yw2m-yw1m) + ys1m
 endif
 call htext(val,xa,ya)
 end subroutine htext_rai
