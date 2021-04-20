!> computer cell-centered gradient from cell-centered data
!>   note: since we use wcx1 etc., it is assumed that normal components at the closed boundary are zero
   subroutine comp_gradC(val, gradx, grady)
      use m_flowgeom, only: ln, Lnx, Ndx, Dxi, wcx1, wcy1, wcx2, wcy2
      implicit none

      double precision, dimension(Ndx), intent(in)  :: val  !< cell-centered data
      double precision, dimension(Ndx), intent(out) :: gradx !< x-component of cell-centered gradient vector
      double precision, dimension(Ndx), intent(out) :: grady !< y-component of cell-centered gradient vector

      double precision                              :: DvalDn

      integer                                       :: k1, k2, L

      gradx = 0d0
      grady = 0d0

      do L=1,Lnx
         k1 = ln(1,L)
         k2 = ln(2,L)
         DvalDn = (val(k2) - val(k1))*Dxi(L)   ! outward positive for k1

         gradx(k1) = gradx(k1) + wcx1(L) * DvalDn
         grady(k1) = grady(k1) + wcy1(L) * DvalDn
         gradx(k2) = gradx(k2) + wcx2(L) * DvalDn
         grady(k2) = grady(k2) + wcy2(L) * DvalDn
      end do

      return
   end subroutine comp_gradC
