!> find common neighboring cell of two links (0: no cell found)
integer function icommon(L1, L2)
   use network_data, only: lnn, lne
   implicit none

   integer, intent(in) :: L1, L2 !< link numbers

   integer :: icell, i, j, kk

   icommon = 0

   do i=1,lnn(L1)
      do j=1,lnn(L2)
         if ( lne(i,L1).eq.lne(j,L2) ) then
            icommon = lne(i,L1)
            exit
         end if
      end do
   end do

   return
end function icommon
