 subroutine getLbotLtop(LL,Lb,Lt)
 use m_flow
 use m_flowgeom
 implicit none
 integer :: LL,Lb,Lt
 if (kmx == 0) then
    Lb = LL
    if (hu(LL) > 0) then
        Lt = LL
    else
        Lt = 0
    endif
 else
    Lb = Lbot(LL) ; Lt = Ltop(LL)
 endif
 end subroutine getLbotLtop
