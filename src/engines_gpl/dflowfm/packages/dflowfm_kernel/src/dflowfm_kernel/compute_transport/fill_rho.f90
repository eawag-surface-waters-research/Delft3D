subroutine fill_rho()
   use m_transport
   use m_flowgeom
   use m_flow
   use m_sediment
   use m_transport
   use m_sferic
   use m_flowtimes , only : dnt
   use timers

   implicit none

   integer          :: kk, k, kb, kt
   double precision :: dvoli, dtol=1d-8

   integer(4) ithndl /0/
   if (timon) call timstrt ( "fill_rho", ithndl )

   do k=1,Ndkx
      constituents(1,k) = rho(k)
   enddo

!  sources
   do kk=1,Ndx
      call getkbotktop(kk,kb,kt)
      do k=kb,kt
         dvoli = 1d0/max(vol1(k),dtol)
         const_sour(1,k) = - rho(k) * sq(k) * dvoli
         const_sink(1,k) = 0d0
      end do
   enddo

   if (timon) call timstop( ithndl )
   return
end subroutine fill_rho
