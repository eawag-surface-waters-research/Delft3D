!> mark the cells that are crossed by the polygon
   subroutine mark_cells_crossed_by_poly(ksize,kmask)
      use m_netw

      implicit none

      integer,                    intent(in) :: ksize  !< size of kmask array

      integer, dimension(ksize),  intent(in) :: kmask  !< original node mask, with new nodes set to 1

      integer, allocatable, dimension(:)     :: Lmask

      integer                                :: k, kk, k1, k2, L, N, lnn_orig

      logical                                :: Lcrossedcell

   !  allocate node mask arrays
      if ( allocated(cellmask) ) deallocate(cellmask)
      allocate(Lmask(numL),cellmask(nump))

   !  make the linkmask
      Lmask = 0
      do L=1,numL
         k1 = kn(1,L)
         k2 = kn(2,L)
         if ( k1.lt.1 .or. k2.lt.1 .or. k1.gt.numk .or. k2.gt.numk) cycle
         if ( kmask(k1).ne.kmask(k2) ) then
            Lmask(L) = 1
         else
            Lmask(L) = 0
         end if
      end do

   !  make the cellmask
      cellmask = 0
      do k=1,nump
         N = netcell(k)%N
         do kk=1,N
            L = netcell(k)%lin(kk)
            if ( Lmask(L).eq.1 ) then
               cellmask(k) = 1
               exit
            end if
         end do
      end do

   !  set lnn to 0 (deactivated, not a member of a crossed cell), 1 (member of one crossed cell) or 2 (member of two adjacent crossed cells)
      do L=1,numL
         lnn_orig = lnn(L)
         if ( lnn_orig.ge.1 ) then
            k1 = lne(1,L)
            if ( cellmask(k1).eq.1 ) then
               lnn(L) = 1
               lne(1,L) = k1
            else
               lnn(L) = 0
            end if
            if ( lnn_orig.eq.2 ) then
               k2 = lne(2,L)
               if ( cellmask(k2).eq.1 ) then
                  lnn(L) = lnn(L)+1
                  lne(lnn(L),L) = k2
               else
                  continue
               end if
            end if
         end if
      end do

   !  deallocate
      deallocate(Lmask, cellmask)

      return

   end subroutine mark_cells_crossed_by_poly
