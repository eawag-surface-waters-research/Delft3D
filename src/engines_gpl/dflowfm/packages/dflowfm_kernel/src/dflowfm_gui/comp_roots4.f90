!> solves the quartic equation Ax^4+Bx^3+Cx^2+Dx+E=0
subroutine comp_roots4(coeffs,x)
   use m_missing
   use Solve_Real_Poly

   implicit none

   double precision, dimension(5), intent(in)  :: coeffs          !< coefficient vector (A,B,C,D,E)
   double precision, dimension(4), intent(out) :: x               !< roots

   double precision, dimension(4)              :: re, im          !< real and imaginairy parts of zeros

   double precision                            :: rhs

   logical                                     :: Lfail

   double precision                            :: dtol = 1d-12

   integer                                     :: i, j, ndegree

   x = DMISS

   Lfail = .true.
!
!   ndegree = 4
!   do i=1,4
!      if ( abs(coeffs(i)).gt.dtol ) exit
!      ndegree = ndegree-1
!   end do
!
!   if ( ndegree.ge.1 ) then
!      call rpoly(coeffs(5-ndegree:5), ndegree, re(1:ndegree), im(1:ndegree), Lfail)
!   end if

   do i=4,1,-1
      ndegree = i
      if ( abs(coeffs(5-ndegree)).lt.dtol ) cycle
      call rpoly(coeffs(5-ndegree:5), ndegree, re(1:ndegree), im(1:ndegree), Lfail)
      exit
!      if ( .not.Lfail ) exit
   end do

   if ( Lfail .and. ndegree.gt.0 ) then
      return
   end if

!   do i=1,ndegree
!      if ( abs(im(i)).lt.dtol ) then
!         x(i) = re(i)
!      end if
!   end do

!   check validity of roots
!    do i=1,ndegree
!      rhs=0d0
!      do j=ndegree,0,-1
!         rhs = rhs + coeffs(5-j)*re(i)**j
!      end do
!      if ( abs(rhs).lt.dtol ) then
!         x(i) = re(i)
!      end if
!    end do



   do i=1,ndegree
      if ( abs(im(i)).lt.1d-4 ) then
         x(i) = re(i)
      end if
   end do

end subroutine comp_roots4
