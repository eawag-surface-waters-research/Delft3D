   subroutine init_1dinfo()
   use m_flowgeom, only: lnx1D, ln, ndx
   use m_sediment, only: stmpar
   use m_fm_erosed, only: link1, link1sign, link1_initialized

   integer :: k1
   integer :: k2
   integer :: L

   if (.not. stmpar%morpar%mornum%pure1d) return
   if (link1_initialized) return

   ! if (isassociated(link1)) deallocate(link1, link1sign) ! if link1 were associated then link1_initailized is true and this statement isn't reached
   allocate(link1(ndx), link1sign(ndx))

   ! we define the node as the begin/end point of the first link connected to it
   link1(:) = 0
   link1sign(:) = 0
   do L = 1,lnx1D
       k1 = ln(1,L)
       k2 = ln(2,L)
       if (link1(k1) == 0) then
           link1(k1) = L
           link1sign(k1) = -1
       endif
       if (link1(k2) == 0) then
           link1(k2) = L
           link1sign(k2) = 1
       endif
   enddo
   link1_initialized = .true.
   end subroutine init_1dinfo
