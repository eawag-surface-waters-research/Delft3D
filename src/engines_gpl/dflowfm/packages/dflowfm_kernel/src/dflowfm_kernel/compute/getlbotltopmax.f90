 subroutine getLbotLtopmax(LL,Lb,Ltx)
! Variation on getLbotLtop. Always returns the maximum possible layer range in stead of the actual range.
 use m_flow
 use m_flowgeom
 implicit none
 integer :: LL,Lb,Ltx
 if (kmx == 0) then
    Lb = LL
    if (hu(LL) > 0) then
        Ltx = LL
    else
        Ltx = 0
    endif
 else
    Lb = Lbot(LL) ; Ltx = Lbot(LL) + kmxL(LL) - 1
 endif
 end subroutine getLbotLtopmax
