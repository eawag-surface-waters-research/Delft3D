!> count number of 2D links and 1D endpoints
subroutine count_links(mx1Dend, Nx)
   use network_data, only: numL, numL1D, kn, lne, nmk
   implicit none

   integer, intent(out) :: mx1Dend  !< number of 1D endpoints
   integer, intent(out) :: Nx       !< number of 2D links and 1D endpoints

   integer              :: k1, k2, L

   mx1Dend = 0                                        ! count MAX nr of 1D endpoints
   do L = 1,numl1D
      if ( kn(3,L) == 1 .or. kn(3,L) == 6) then       ! zeker weten
         k1 = kn(1,L) ; k2 = kn(2,L)
         if (nmk(k1) == 1 .and. nmk(k2) == 2 .and. lne(1,L) < 0 .or. &
             nmk(k2) == 1 .and. nmk(k1) == 2 .and. lne(2,L) < 0 ) then
             mx1Dend = mx1Dend + 1
         endif
      endif
   enddo


   Nx = numL + mx1Dend

   return
end subroutine count_links
