subroutine getq1(hg,d,h1,h2,q)                   ! energiebehoud bovenstrooms
implicit none                                   ! bepaal q gegeven hg,h1,h2
double precision :: hg, d, h1, h2, q
double precision :: g, t, r, tr

g  = 9.81d0
t  = 2d0*g*(h1-h2)
r  = 1d0/hg**2 - 1d0/(h1+d)**2
tr = t/r
if (tr .gt. 0) then
     q = sqrt(tr)
else
     q = h1*sqrt(g*h1)
endif
end subroutine getq1
