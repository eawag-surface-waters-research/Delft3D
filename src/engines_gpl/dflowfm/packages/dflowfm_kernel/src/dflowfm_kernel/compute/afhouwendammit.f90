 subroutine afhouwendammit()
 use m_ship
 implicit none
 integer          :: n, i, j
 double precision :: sx1, sy1, sx2, sy2, eas, easm, frc

 ! kinetic e = potential e
 ! 0.5*m*u*u = 0.5*eas*dx*dx, u   = 5m/s, dx = 10 m indeuking => eas = deadw   potential energy = kinetic energy
 ! 0.5*m*u*u = 0.5*frc*u*dx   frc = mu/dx                                      friction labour  = kinetic energy

 do n = 1,nshiptxy

    eas = 0.25d0*deadw(n) ; easm = 0.5d0*eas
    frc = 0.5d0*deadw(n)
    fextx(n) = 0d0 ; fexty(n) = 0d0 ; fextm(n) = 0d0

    sx1 = 0.9d0; sy1 = 0d0
    call shipcoor(n,sx1,sy1,sx2,sy2)  ! midvoor
    call inkade(sx2,sy2,i,j)
    if (i == 1) then
       fextx(n)  = fextx(n)  + eas*(xmxs - sx2)
       fricxi(n) = fricxi(n) + frc
       fextm(n)  = fextm(n) - easm*(xmxs - sx2)*(sy2 - shy(n))
    endif

    if (i == -1) then
       fextx(n)  = fextx(n) + eas*(xmns - sx2)
       fricxi(n) = fricxi(n) + frc
       fextm(n)  = fextm(n) - easm*(xmns - sx2)*(sy2 - shy(n))
    endif

    if (j == 1) then
       fexty(n)  = fexty(n) + eas*(ymxs - sy2)
       fricyi(n) = fricyi(n) + frc
       fextm(n)  = fextm(n) + easm*(ymxs - sy2)*(sx2 - shx(n))
    endif

    if (j == -1) then
       fexty(n)  = fexty(n) + eas*(ymns - sy2)
       fricyi(n) = fricyi(n) + frc
       fextm(n)  = fextm(n) + easm*(ymns - sy2)*(sx2 - shx(n))
    endif


    sx1 = 0.9d0; sy1 = 1d0
    call shipcoor(n,sx1,sy1,sx2,sy2)  ! linksvoor
    call inkade(sx2,sy2,i,j)
    if (i == 1) then
       fextx(n)  = fextx(n) + eas*(xmxs - sx2)
       fricxi(n) = fricxi(n) + frc
       fextm(n)  = fextm(n) - easm*(xmxs - sx2)*(sy2 - shy(n))
    endif

    if (i == -1) then
       fextx(n)  = fextx(n) + eas*(xmns - sx2)
      fricxi(n)  = fricxi(n) + frc
        fextm(n) = fextm(n) - easm*(xmns - sx2)*(sy2 - shy(n))
    endif

    if (j == 1) then
       fexty(n)  = fexty(n) + eas*(ymxs - sy2)
       fricyi(n) = fricyi(n) + frc
       fextm(n)  = fextm(n) + easm*(ymxs - sy2)*(sx2 - shx(n))
    endif

    if (j == -1) then
       fexty(n)  = fexty(n) + eas*(ymns - sy2)
       fricyi(n) = fricyi(n) + frc
       fextm(n)  = fextm(n) + easm*(ymns - sy2)*(sx2 - shx(n))
    endif


    sx1 = 0.9d0; sy1 = -1d0
    call shipcoor(n,sx1,sy1,sx2,sy2)  ! rechtsvoor
    call inkade(sx2,sy2,i,j)
    if (i == 1) then
       fextx(n)  = fextx(n) + eas*(xmxs - sx2)
       fricxi(n) = fricxi(n) + frc
       fextm(n)  = fextm(n) - easm*(xmxs - sx2)*(sy2 - shy(n))
    endif

    if (i == -1) then
       fextx(n)  = fextx(n) + eas*(xmns - sx2)
       fricxi(n) = fricxi(n) + frc
       fextm(n)  = fextm(n) - easm*(xmns - sx2)*(sy2 - shy(n))
    endif

    if (j == 1) then
       fexty(n)  = fexty(n) + eas*(ymxs - sy2)
       fricyi(n) = fricyi(n) + frc
       fextm(n)  = fextm(n) + easm*(ymxs - sy2)*(sx2 - shx(n))
    endif

    if (j == -1) then
       fexty(n)  = fexty(n) + eas*(ymns - sy2)
       fricyi(n) = fricyi(n) + frc
       fextm(n)  = fextm(n) + easm*(ymns - sy2)*(sx2 - shx(n))
    endif


    sx1 = -1d0; sy1 = 1d0
    call shipcoor(n,sx1,sy1,sx2,sy2)  ! linksachter
    call inkade(sx2,sy2,i,j)
    if (i == 1) then
       fextx(n)  = fextx(n) + eas*(xmxs - sx2)
       fricxi(n) = fricxi(n) + frc
       fextm(n)  = fextm(n) - easm*(xmxs - sx2)*(sy2 - shy(n))
    endif

    if (i == -1) then
       fextx(n)  = fextx(n) + eas*(xmns - sx2)
       fricxi(n) = fricxi(n) + frc
       fextm(n)  = fextm(n) - easm*(xmns - sx2)*(sy2 - shy(n))
    endif

    if (j == 1) then
       fexty(n)  = fexty(n) + eas*(ymxs - sy2)
       fricyi(n) = fricyi(n) + frc
       fextm(n)  = fextm(n) + easm*(ymxs - sy2)*(sx2 - shx(n))
    endif

    if (j == -1) then
       fexty(n)  = fexty(n) + eas*(ymns - sy2)
       fricyi(n) = fricyi(n) + frc
       fextm(n)  = fextm(n) + easm*(ymns - sy2)*(sx2 - shx(n))
    endif


    sx1 = -1d0; sy1 = -1d0
    call shipcoor(n,sx1,sy1,sx2,sy2)  ! rechtsachter
    call inkade(sx2,sy2,i,j)
    if (i == 1) then
       fextx(n)  = fextx(n) + eas*(xmxs - sx2)
       fricxi(n) = fricxi(n) + frc
       fextm(n)  = fextm(n) - easm*(xmxs - sx2)*(sy2 - shy(n))
    endif

    if (i == -1) then
       fextx(n)  = fextx(n) + eas*(xmns - sx2)
       fricxi(n) = fricxi(n) + frc
       fextm(n)  = fextm(n) - easm*(xmns - sx2)*(sy2 - shy(n))
    endif

    if (j == 1) then
       fexty(n)  = fexty(n) + eas*(ymxs - sy2)
       fricyi(n) = fricyi(n) + frc
       fextm(n)  = fextm(n) + easm*(ymxs - sy2)*(sx2 - shx(n))
    endif

    if (j == -1) then
       fexty(n)  = fexty(n) + eas*(ymns - sy2)
       fricyi(n) = fricyi(n) + frc
       fextm(n)  = fextm(n) + easm*(ymns - sy2)*(sx2 - shx(n))
    endif

 enddo

 end subroutine afhouwendammit
