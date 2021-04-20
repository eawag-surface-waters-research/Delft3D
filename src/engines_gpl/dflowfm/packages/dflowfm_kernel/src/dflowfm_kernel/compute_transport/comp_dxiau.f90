! compute Au/Dx for diffusive flux
subroutine comp_dxiAu()
   use m_flowgeom, only: ln, Lnx, dxi, wu
   use m_flow, only: hs, zws, kmx, Au
   use m_transport, only : dxiAu, jalimitdtdiff
   use timers

   implicit none

   integer :: k1, k2
   integer :: LL, L, Lb, Lt

   integer(4) ithndl /0/
   if (timon) call timstrt ( "comp_dxiAu", ithndl )

   if ( jalimitdtdiff.eq.0 ) then
      if ( kmx.eq.0 ) then
         do L=1,Lnx
            dxiAu(L) = dxi(L)*Au(L)
         end do
      else
         do LL=1,Lnx
            call getLbotLtop(LL,Lb,Lt)
            do L=Lb,Lt
               dxiAu(L) = dxi(LL)*Au(L)
            end do
         end do
      end if
   else
      if ( kmx.eq.0 ) then
         do L=1,Lnx
            k1 = ln(1,L)
            k2 = ln(2,L)
            dxiAu(L) = dxi(L)*wu(L) * min(hs(k1), hs(k2))
         end do
      else
         do LL=1,Lnx
            call getLbotLtop(LL,Lb,Lt)
            do L=Lb,Lt
               k1 = ln(1,L)
               k2 = ln(2,L)
               dxiAu(L) = dxi(LL)*wu(LL) * min(zws(k1)-zws(k1-1),zws(k2)-zws(k2-1))
            end do
         end do
      end if
   end if

   if (timon) call timstop( ithndl )
   return
end subroutine comp_dxiAu
