!> check if a cell is close to a land boundary segment
subroutine cellcrossedbyland(k, jstart, jend, jland, jacross)
   use m_netw
   use m_landboundary
   use m_missing
   use geometry_module, only: cross
   use m_sferic, only: jsferic

   implicit none

   integer, intent(in)          :: k              !< cell number
   integer, intent(in)          :: jstart, jend   !< start and end point of land boundary segment respectively
   integer, intent(inout)       :: jland          !< point in land boundary that is (in:) visited first (out:) found
   integer, intent(out)         :: jacross        !< crossed (1) or not (0)

   double precision             :: rL

   double precision             :: x1, y1, x2, y2, x3, y3, x4, y4, sL, sm, xcr, ycr, crp

   integer                      :: j, kk, L, k1, k2

   jacross = 0

kklp:do kk=1,netcell(k)%N
      L = netcell(k)%lin(kk)
!      call linkcrossedbyland(L, jstart, jend, 0, jland, jacross)

      do j=jstart,jend-1
         k1 = kn(1,L)
         x1 = xk(k1)
         y1 = yk(k1)
         k2 = kn(2,L)
         x2 = xk(k2)
         y2 = yk(k2)
         x3 = xlan(j)
         y3 = ylan(j)
         x4 = xlan(j+1)
         y4 = ylan(j+1)

         call cross(x1, y1, x2, y2, x3, y3, x4, y4, jacross,sL,sm,xcr,ycr,crp, jsferic, dmiss)

         if ( jacross.eq.1 ) exit kklp
      end do
   end do kklp

   return
end subroutine cellcrossedbyland
