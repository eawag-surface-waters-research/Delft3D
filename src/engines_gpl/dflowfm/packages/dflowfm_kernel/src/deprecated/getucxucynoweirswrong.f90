 subroutine getucxucynoweirswrong(ku, ucxku, ucyku, ischeme)  !wrong
 use m_flow
 use m_flowgeom
 implicit none

 integer           :: ku, LL, L, Ls, ischeme
 double precision  :: ucxku, ucyku, wwx, wwy, ww, wwt, ac1, wwxt, wwyt, ux, uy

 ucxku = 0d0 ; ucyku = 0d0; wwt = 0d0; wwxt = 0d0 ; wwyt = 0d0
 do LL = 1,nd(ku)%lnx
    Ls = nd(ku)%ln(LL); L = iabs(Ls)
    if ( iadv(L) < 21 .or. iadv(L) > 25 ) then  ! no weir
       if (Ls < 0) then
          ac1 = acL(L)
       else
          ac1 = 1d0 - acL(L)
       endif
       ww    = ac1*dx(L)*wu(L)
       if (ischeme == 3) then
          wwx   = csu(L)*ww
          wwy   = snu(L)*ww
          ucxku = ucxku + wwx*u0(L)
          ucyku = ucyku + wwy*u0(L)
       else
          wwx   = abs(csu(L))*ww
          wwy   = abs(snu(L))*ww
          ux    = csu(L)*u0(L)
          uy    = snu(L)*u0(L)
          ucxku = ucxku + ux*wwx
          ucyku = ucyku + uy*wwy
       endif
       wwxt = wwxt + abs(wwx)
       wwyt = wwyt + abs(wwy)
    endif
 enddo

 if (wwxt > 0d0) ucxku = ucxku / wwxt
 if (wwyt > 0d0) ucyku = ucyku / wwyt

 end subroutine getucxucynoweirswrong
