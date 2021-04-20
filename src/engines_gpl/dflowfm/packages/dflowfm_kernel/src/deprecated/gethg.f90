subroutine gethg(q,a,h1,h2,hg)
implicit none
double precision :: q,a,h1,h2,hg, g = 9.81d0
hg = h1*q**2 / (q**2 - g*a**h1*(h2-h1)  )
end subroutine gethg
