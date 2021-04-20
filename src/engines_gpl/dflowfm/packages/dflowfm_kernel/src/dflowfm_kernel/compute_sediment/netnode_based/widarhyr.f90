subroutine widarhyr(hpr,dz,wu2,wid,ar,hyr)
 use m_flow, only :  slotw2D
 implicit none
 double precision :: hpr,dz,wu2,wid,ar,hyr
 double precision :: per, hp2
 if (dz/wu2 < 1d-3) then
    wid = wu2 ; wid = wid + slotw2D
    ar  = wid * hpr
    hyr = hpr
 else if (hpr < dz) then
    wid = wu2 * hpr / dz ; wid = wid + slotw2D
    ar  = 0.5d0*wid*hpr
    per = sqrt(hpr*hpr + wid*wid)
    hyr = ar/per
 else
    wid = wu2 ; wid = wid + slotw2D ! wid = max(wid, slotw2d)
    hp2 = hpr - dz
    ar  = wid*0.5d0*(hpr + hp2)
    per = sqrt(dz*dz + wid*wid)
    hyr = ar/per
 endif
end subroutine widarhyr
