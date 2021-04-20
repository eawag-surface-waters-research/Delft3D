subroutine getseg1D(hpr,wu2,dz,ai,frcn,ifrctyp, wid,ar,conv,perim,jaconv)  ! copy of above routine dressed out for 1D
 implicit none
 double precision, intent (in)  ::     hpr,wu2,dz,ai,frcn
 double precision, intent (out) ::                                 wid,ar,conv,perim        !
 integer,          intent (in)  ::                         ifrctyp                   ,jaconv
 double precision  :: d83 = 2.666666d0, d16 = 0.166666d0 , d23 = 0.666666d0, d43= 1.333333d0
 double precision  :: tt, hp2, hrad, Cz, cman, per, hav
 double precision  :: d38 = 0.375d0 , d113 = 3.666666d0 , d311 = 0.27272727d0, hpr83, hp283, d14 = 0.25d0
 integer           :: jac, L

 ! for jaconv >= 1, this routine gets 1D conveyance
 ! this constant value, (1+(dz/dy)**2)**0.25 is computed once and is volume cell based instead of link based

 if (ai < 1d-3) then
    wid = wu2
    ar  = wid * hpr
 else if (hpr < dz) then
    wid = wu2 * hpr / dz
    ar  = 0.5d0*wid*hpr
 else
    wid = wu2
    hp2 = hpr - dz
    ar  = wid*0.5d0*(hpr + hp2)
 endif

 if (jaconv == 0) then
    return
 else if (frcn == 0d0) then
    conv = 0d0 ;  return
 else if (jaconv == 1) then                       ! hydraulic radius type

    if (ai < 1d-3) then
       perim = wid
    else if (hpr < dz) then
       perim = sqrt(wid*wid + hpr*hpr)
    else
       perim = sqrt(wid*wid + (hpr-hp2)*(hpr-hp2) )
    endif

 else if (jaconv >= 2) then                       ! 1D analytic conveyance type
    if (ifrctyp == 1) then
       cman = frcn
    else
       if (ai < 1d-3) then
          hav = hpr
       else if (hpr < dz) then
          hav = 0.5d0*hpr
       else
          hav = hpr -0.5d0*dz
       endif
       call getcz(hav, frcn, ifrctyp, Cz, L)
       cman = hav**d16/Cz
    endif

    if (ai < 1d-3 ) then                       ! see sysdoc 5 1D conveyance
       conv = ( ar *hpr**d23             ) / ( cman                     )
    else if (hpr < dz) then
       conv = ( d38*hpr**d83             ) / ( cman*ai*(1d0+ai*ai)**d14 )
    else
       conv = ( d38*(hpr**d83-hp2**d83)  ) / ( cman*ai*(1d0+ai*ai)**d14 )
    endif

 endif
 end subroutine getseg1D
