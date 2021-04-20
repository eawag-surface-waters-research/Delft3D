subroutine extract_rho()
   use m_transport
   use m_flow
   use m_sediment
   use m_transport
   use timers

   implicit none

   integer :: k

   integer(4) ithndl /0/
   if (timon) call timstrt ( "extract_rho", ithndl )

   do k=1,Ndkx
      rho(k) = constituents(1,k)
      if ( ISALT.ne.0 ) then
         constituents(ISALT,k) = sa1(k)
      endif
   enddo

   if (timon) call timstop( ithndl )
   return
end subroutine extract_rho
