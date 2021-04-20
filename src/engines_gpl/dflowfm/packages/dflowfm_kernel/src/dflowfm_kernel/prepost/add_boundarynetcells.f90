!  increase netcell admin. to include boundary nodes (safety)
   subroutine add_boundarynetcells()

      use network_data
      use m_flowgeom
      use gridoperations

      implicit none

      integer nump_store
      integer k

  !   store nump
      nump_store = nump

  !   need to store nump1d2d netcells in "increasenetcells", not only nump
      nump = nump1d2d
      call increasenetcells(Ndx,1.0,.true.)
      do k=nump1d2d+1,Ndx
         netcell(k)%n = 0   ! safety
      end do

  !   restore nump
      nump = nump_store

      return
   end subroutine add_boundarynetcells
