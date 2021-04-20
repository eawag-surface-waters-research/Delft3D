subroutine getq3(hg,d,a,h1,h2,q)                  ! momentumbehoud bovenstrooms
implicit none                                   ! bepaal q gegeven a,hg,h1,h2
double precision :: hg,d,a,h1,h2,q
double precision :: g, t, r, tr, h2d
g   = 9.81d0
t   = g*a*(h2 - h1)
r   = 1.0/h1 - 1d0/hg
tr  = t/r
q   = sqrt(tr)
end subroutine getq3
