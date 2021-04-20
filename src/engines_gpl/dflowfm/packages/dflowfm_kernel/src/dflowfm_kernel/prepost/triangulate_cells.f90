!> convert quadrilaterals, pentagons and hexagons to triangles
subroutine triangulate_cells()
   use m_netw
   use m_inverse_map
   use unstruc_colors

   implicit none

   integer                            :: k, k0, k1, kk, Lnew, N

   do k=1,nump                ! loop over the cells
      N = netcell(k)%n
      if ( N.lt.4 ) cycle
!     make the triangles by connecting the 3rd, 4th, etc. node to the first one
      k0 = netcell(k)%nod(1)
      do kk=3,N-1
         k1 = netcell(k)%nod(kk)
         call newlink(k0, k1, Lnew)
      end do
   end do

end subroutine triangulate_cells
