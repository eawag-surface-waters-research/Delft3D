 subroutine addbarocn(n)
 use m_flowgeom
 use m_flow

 implicit none
 integer, intent(in) :: n

 integer             :: k,kb,kt
 double precision    :: rhosw(0:kmxx) ! rho at pressure point layer interfaces
 double precision    :: fzu , fzd, alf, pu, pd, gr, dzz, rvn

 call getkbotktop(n,kb,kt)
 ! if (kt < kb) return
 if (zws(kt) - zws(kb-1) < epshu) then
     grn(kb:kt)  = 0d0
     rvdn(kb:kt) = 1d-10
     return
 endif

 if (kt > kb) then
    do k = kb, kt-1
       fzu           = (zws(k+1) - zws(k)) / (zws(k+1) - zws(k-1)) ; fzd = 1d0 - fzu
       rhosw(k-kb+1) = fzu*rho(k+1) + fzd*rho(k) - rhomean
    enddo
    rhosw(0)       = 2d0*(rho(kb) - rhomean) - rhosw(1)
    rhosw(kt-kb+1) = 2d0*(rho(kt) - rhomean) - rhosw(kt-kb)
 else
    rhosw(0)       = rho(kb) - rhomean
    rhosw(1)       = rhosw(0)
 endif

 grn(kt)  = 0d0
 rvdn(kt) = 0d0
 pd       = 0d0
 rvn      = 0d0
 do k = kt, kb, -1
    dzz     = zws(k) - zws(k-1)
    rvn     = rvn + 0.5d0*( rhosw(k-kb+1) + rhosw(k-kb) ) * dzz
    rvdn(k) = rvn
    alf = rhosw(k-kb) - rhosw(k-kb+1)
    pu  = pd
    pd  = pu + rhosw(k-kb+1)*dzz + 0.5d0*alf*dzz
    gr  = pu*dzz + 0.5d0*rhosw(k-kb+1)*dzz*dzz + alf*dzz*dzz/6d0             ! your left  wall
    grn(k) = gr
 enddo

 end subroutine addbarocn
