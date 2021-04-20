subroutine widar(hpr,dz,wu2,wid,ar)
 use m_flow, only :  slotw2D
 implicit none
 double precision :: hpr,dz,wu2,wid,ar,hyr
 double precision :: per, hp2
 if (dz/wu2 < 1d-3) then
    wid = wu2 ; wid = wid + slotw2D
    ar  = wid * hpr
 else if (hpr < dz) then
    wid = wu2 * hpr / dz ; wid = wid + slotw2D
    ar  = 0.5d0*wid*hpr
 else
    wid = wu2 ; wid = wid + slotw2D
    hp2 = hpr - dz
    ar  = wid*0.5d0*(hpr + hp2)
 endif
end subroutine widar
