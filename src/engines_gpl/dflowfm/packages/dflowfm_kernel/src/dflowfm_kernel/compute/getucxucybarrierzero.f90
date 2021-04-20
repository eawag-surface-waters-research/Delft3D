! =================================================================================================
! =================================================================================================
  subroutine getucxucybarrierzero ( Lf, ku, ucxku, ucyku )
 use m_flow
 use m_flowgeom
 implicit none

 integer           :: ku, L, LL, Ls, n12, Lf
 double precision  :: ucxku, ucyku, ww, ac1, cs, sn
 double precision, external :: lin2nodx, lin2nody

 ucxku = 0d0  ; ucyku = 0d0

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
    if( L /= Lf ) then
       ucxku = ucxku + lin2nodx(L,n12,cs,sn)*u1(L)
       ucyku = ucyku + lin2nody(L,n12,cs,sn)*u1(L)
    endif
 enddo
 ucxku = ucxku/ba(ku)
 ucyku = ucyku/ba(ku)

 end subroutine getucxucybarrierzero
