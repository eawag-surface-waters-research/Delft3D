!> apply tracer boundary conditions
subroutine apply_tracer_bc()
   use m_transport
   use m_meteo
   use m_flowgeom, only: ln
   use m_flow, only: kmxd, q1, kmxL
   use timers
   implicit none

   character (len=NAMTRACLEN)    :: tracnam

   double precision :: valtop

   integer :: itrac, iconst
   integer :: k, kk, ki, kb
   integer :: L, LL, Lb, Lt

   integer(4) ithndl /0/
   if (timon) call timstrt ( "apply_tracer_bc", ithndl )

!  loop over the tracer boundary conditions
   do itrac=1,numtracers
      iconst = itrac2const(itrac)
      do k=1,nbndtr(itrac)
         LL = bndtr(itrac)%k(3,k)
         call getLbotLtop(LL,Lb,Lt)
         kb = 0
         do L = Lb,Lt
            kb = ln(1,L)
            ki = ln(2,L)
            if ( q1(L).gt.0 ) then  ! inflow
               kk = kmxd*(k-1)+L-Lb+1
               constituents(iconst,kb) = bndtr(itrac)%z(kk)
            else                    ! outflow
               constituents(iconst,kb) = constituents(iconst,ki)
            end if
         end do

         if ( kb.gt.0 ) then
            valtop = constituents(iconst,kb)

            do L=Lt+1,Lb+kmxL(LL)-1
               kb = ln(1,L)
               ki = ln(2,L)
               if ( q1(Lt).gt.0d0 ) then
                  constituents(iconst,kb) = valtop
               else
                  constituents(iconst,kb) = constituents(iconst,ki)
               end if
            end do
         end if
      end do
   end do

   if (timon) call timstop( ithndl )
   return
end subroutine apply_tracer_bc
