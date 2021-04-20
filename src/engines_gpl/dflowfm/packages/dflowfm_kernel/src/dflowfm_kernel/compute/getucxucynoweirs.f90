 subroutine getucxucynoweirs( ku, ucxku, ucyku, ischeme )
 use m_flow
 use m_flowgeom
 use m_sferic, only: jasfer3D
 implicit none

 integer           :: ku, LLL, LL, L, Ls, ischeme, n12

 double precision  :: ucxku, ucyku, ww, ac1, huweir, hunoweir, wl, wlno, at, cs, sn, fac

 double precision, external :: lin2nodx, lin2nody

 ucxku = 0d0  ; ucyku = 0d0
 huweir = 0d0 ; hunoweir = 0d0; wl = 0d0 ; wlno = 0d0; at = 0d0

 do LL = 1,nd(ku)%lnx
    Ls = nd(ku)%ln(LL); L = iabs(Ls)
    if (iadv(L) < 21 .or. iadv(L) > 29) then ! .ne. structures
       hunoweir = hunoweir + wu(L)*hu(L)
       wlno     = wlno     + wu(L)
    endif
 enddo
 if (wlno > 0d0 ) hunoweir = hunoweir/wlno

 do LL = 1,nd(ku)%lnx
    Ls = nd(ku)%ln(LL); L = iabs(Ls)
    if (Ls < 0) then
       ac1 = acL(L)
       n12 = 1
    else
       ac1 = 1d0 - acL(L)
       n12 = 2
    endif
    ww = ac1*dx(L)*wu(L)
    cs = ww*csu(L) ; sn = ww*snu(L)
    at = at + ww
    if (iadv(L) < 21 .or. iadv(L) > 29) then ! .ne. structures
       if (jasfer3D == 0) then
          ucxku = ucxku + cs*u0(L)
          ucyku = ucyku + sn*u0(L)
       else
          ucxku = ucxku + lin2nodx(L,n12,cs,sn)*u0(L)
          ucyku = ucyku + lin2nody(L,n12,cs,sn)*u0(L)
       endif
    else
       fac   = 1d0
       if (hunoweir > 0d0) fac = min(1d0, hu(L) / hunoweir )
       if (jasfer3D == 0) then
          ucxku = ucxku + cs*u0(L)*fac
          ucyku = ucyku + sn*u0(L)*fac
       else
          ucxku = ucxku + lin2nodx(L,n12,cs,sn)*u0(L)*fac
          ucyku = ucyku + lin2nody(L,n12,cs,sn)*u0(L)*fac
       endif
    endif
 enddo
 ucxku = ucxku/ba(ku)
 ucyku = ucyku/ba(ku)

 end subroutine getucxucynoweirs
