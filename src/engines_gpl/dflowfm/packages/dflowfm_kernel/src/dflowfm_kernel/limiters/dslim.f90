 double precision function dslim(d1,d2,limtyp)
 implicit none
 double precision d1, d2                             ! voorslope, naslope
 integer limtyp
 double precision :: dminmod, dvanleer, dkoren,dcentral,dcminmod, dsuperbee
 double precision :: dlimiter,dlimitercentral

 ! In order to translate psi to limiter, you have to multiply the psi function with ds2
 ! e.g. lax wendroff central: psi=1, dslimiter=d2

 if (limtyp .eq. 0) then
    dslim = 0
 else if (limtyp .eq. 1) then                        ! codering guus, met voorslope
    dslim = d1*dminmod(d1,d2)
 else if (limtyp .eq. 2) then                        ! codering guus, met voorslope
    dslim = d1*dvanleer(d1,d2)
 else if (limtyp .eq. 3) then                        ! codering guus, met voorslope
    dslim = d1*dkoren(d1,d2)
 else if (limtyp .eq. 4) then                        ! monotonized central no division
    dslim = dcentral(d1,d2)
 else if (limtyp .eq. 5) then                        ! monotonized central Sander with division
    dslim = dlimiter(d1,d2,limtyp) * d2
 else if (limtyp .eq. 6) then                        ! monotonized central Sander with division, upwind slope ds1 at central cel
    dslim = dlimitercentral(d1,d2,limtyp)
  else if (limtyp .eq. 11) then                      ! standaard codering
    dslim = d2*dminmod(d1,d2)
 else if (limtyp .eq. 12) then                       ! standaard codering
    dslim = d2*dvanleer(d1,d2)
 else if (limtyp .eq. 13) then                       ! standaard codering
    dslim = d2*dkoren(d1,d2)
 else if (limtyp .eq. 14) then                       ! monotonized central, == 4
    dslim = dcentral(d2,d1)
 else if (limtyp .eq. 15) then                       ! minmod central
    dslim = dcminmod(d2,d1)
 else if (limtyp .eq. 20) then                       ! leftbiased, beam&warming
    dslim = d1
 else if (limtyp .eq. 21) then                       ! central
    dslim = d2
 else if (limtyp .eq. 22) then                       ! superbee
    dslim = dsuperbee(d1,d2)
 else
    dslim = 0d0
 endif
 return
end function dslim
