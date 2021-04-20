 subroutine getlinkareawid2D(L,wu2,dz,ai,hpr,ar,wid)
 use m_flow, only : slotw2D

 implicit none
 integer         , intent(in ) :: L
 double precision, intent(in ) :: wu2,dz,ai,hpr
 double precision, intent(out) :: ar,wid
 double precision              :: hp2

 if (ai < 1d-3) then
 ! if (dz == 0d0) then
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

 end subroutine getlinkareawid2D
