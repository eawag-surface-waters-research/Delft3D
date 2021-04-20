subroutine getq2(hg,d,h2,h3,q)                  ! momentumbehoud benedenstrooms
implicit none                                   ! bepaal q gegeven a,h2,h3
double precision :: hg,d,h2,h3,q
double precision :: g, t, r, tr
g  = 9.81d0
t  = 0.5d0*g*(h3**2 - h2**2)
r  = 1d0/hg - 1d0/h3
tr = t/r
q  = sqrt(tr)
end subroutine getq2
