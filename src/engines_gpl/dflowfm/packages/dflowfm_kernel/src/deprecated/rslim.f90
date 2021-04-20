 double precision function rslim(d1,d2,limtyp)
 implicit none
 double precision :: d1, d2
 double precision :: rminmod,rvanleer,rkoren,rcentral
 integer :: limtyp

 if (limtyp .eq. 0) then
    rslim = 0
 else if (limtyp .eq. 1) then                        ! codering guus, met voorloper
    rslim = d1*rminmod(d1,d2)
 else if (limtyp .eq. 2) then                        ! codering guus, met voorloper
    rslim = d1*rvanleer(d1,d2)
 else if (limtyp .eq. 3) then                        ! codering guus, met voorloper
    rslim = d1*rkoren(d1,d2)
 else if (limtyp .eq. 4) then                        ! monotonized central
    rslim = rcentral(d1,d2)
 endif
 return
 end function rslim
