 subroutine hydraulicallysmooth(umod,h,sqcf)
 use m_physcoef
 use m_flow
 implicit none
 integer :: L
 double precision :: umod,h,sqcf
 double precision :: r, rv = 123.8d0, e = 8.84d0 , eps = 1d-2, s, sd, er, ers


 r    = umod*h/viskin                                      ! Local re-number:
 r    = max(r,0.001d0)
 er   = e*r
 if (r.lt.rv) then                                         ! Viscous sublayer:
     s   = sqrt(r)
 else

     s   = 12d0                                            ! In log-layer; initial trial for s:
100  continue
     sd  = s
     ers = max(er/sd, 1.0001d0)
     s   = log(ers)/vonkar

     if (abs(sd-s).gt.(eps*s)) then
         go to 100                                         ! Convergence criterium:
     endif
 endif

 if (s > 0d0) then
    sqcf = 1d0/s
 else
    sqcf = 0d0
 endif

 end subroutine hydraulicallysmooth
