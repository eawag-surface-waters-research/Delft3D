subroutine sum_const(iter, vol1)
   use m_transport
   use m_flowgeom, only: Ndx
   use m_flow, only: Ndkx
   implicit none

   integer,                           intent(in) :: iter
   double precision, dimension(Ndkx), intent(in) :: vol1

   double precision, dimension(NUMCONST)         :: sum

   integer                                       :: kk, k, kb, kt
   integer                                       :: j

   sum = 0d0

   do kk=1,Ndx
      call getkbotktop(kk,kb,kt)
      do k=kb,kt
         do j=1,NUMCONST
            sum(j) = sum(j) + vol1(k)*constituents(j,k)
         end do
      end do
   end do

   write(6,"(I5, ':', $)") iter
   do j=1,NUMCONST
      write(6,"(E25.15, $)") sum(j)
   end do
   write(6,*)


   return
end subroutine sum_const
