subroutine comp_horfluxtot()
   use m_flowgeom, only: Lnx
   use m_flow, only: Lbot, Ltop, kmx, Lnkx
   use m_transport, only: ISED1, ISEDN, fluxhor, fluxhortot, sinksetot, sinkftot
   use m_flowtimes, only: dts
   use timers

   implicit none

   integer :: LL, L, Lb, Lt
   integer :: j

   integer(4) ithndl /0/
   if (timon) call timstrt ( "comp_horfluxtot", ithndl )

   if ( kmx<1 ) then
      do L=1,Lnx
         do j=ISED1, ISEDN
            fluxhortot(j,L) = fluxhortot(j,L) + fluxhor(j,L) * dts
         end do
      end do
   else
      do LL=1,Lnx
         Lb = Lbot(LL)
         Lt = Ltop(LL)
         do L=Lb,Lt
            do j=ISED1, ISEDN
               fluxhortot(j,L) = fluxhortot(j,L) + fluxhor(j,L) * dts
            end do
         end do
      end do
   end if

   if (timon) call timstop( ithndl )
end subroutine comp_horfluxtot
