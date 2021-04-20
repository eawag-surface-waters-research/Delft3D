Subroutine setequilibriumsedimentbnds(nbnd,n4,kbnd,kban,i01)
use m_flow
use m_flowgeom
use m_sediment
implicit none

integer           :: nbnd, kban(2,nbnd) , kbnd(n4,nbnd), i01, n4

integer           :: k, kb, ki, L, LL, Lb, Lt, j
double precision  :: hsk
double precision  :: seq  (mxgr)                !< sed equilibrium transport rate (kg/m/s) , dimension = mxgr
double precision  :: wse  (mxgr)                !< effective fall velocity (m/s)           , dimension = mxgr, ws*crefa=wse*seq



do k  = 1,nbnd                                  ! set equilibrium boundary conditions for open flow bnds, types z and u
   kb = kbnd(1,k)
   ki = kbnd(2,k)
   LL = kbnd(3,k)

   if (q1(LL) < 0) then
      bl(kb) = bl(ki)                            ! copy internal bottom level to outflow bnd level
      if (jaceneqtr == 1) then
         do j = 1,mxgr
            grainlay(j,kb) = grainlay(j,ki)
         enddo
      endif
   endif

   if (jaceneqtr == 1) then
      call getequilibriumtransportrates(ki, seq, wse, mxgr, hsk)                            ! get based on cellcentre
   else
      call getequilibriumtransportrates2(LL, kban(1,k), kban(2,k), seq, wse, mxgr, hsk, i01) ! get based on 2 netnodes
   endif
   call getLbotLtop(LL,Lb,Lt)
   do L  = Lb,Lt
      kb = ln(1,L) ; ki = ln(2,L)
      do j = 1,mxgr
         if (q1(L) > 0) then
            sed(j,kb) = seq(j)                      ! inflow ,  equilibrium boundary condition
         else
            sed(j,kb) = sed(j,ki)                   ! outflow
         endif
      enddo
   enddo
enddo

End Subroutine setequilibriumsedimentbnds
