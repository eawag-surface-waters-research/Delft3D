!  remove a netcell
   subroutine removecell(xp,yp)
      use m_netw
      use m_missing, only: jins, dmiss
      use geometry_module, only: pinpok
      use gridoperations

      implicit none

      integer, save                               :: NEEDFINDCELLS=1

      double precision,                intent(in) :: xp, yp             !< coordinates of input point

      integer                                     :: k, in

      if ( nump.lt.1 ) NEEDFINDCELLS=1

      if ( NEEDFINDCELLS.ne.0 .or. netstat.ne.NETSTAT_OK ) then
         call findcells(100)
         call makenetnodescoding()
         NEEDFINDCELLS = 0
      end if

!     (re)allocate
      if ( allocated(cellmask) ) deallocate(cellmask)
      allocate(cellmask(nump))
      cellmask = 0

      !  find the cell
      in = 0
      do k = 1,nump
         if ( netcell(k)%N.lt.1 ) cycle
         call pinpok(xp, yp, netcell(k)%N, xk(netcell(k)%nod), yk(netcell(k)%nod), in, jins, dmiss)
         if ( in.gt.0 ) exit
      end do

      if ( in.eq.0 ) then  ! no cell found
         call qnerror('removecell: no cell found', ' ', ' ')
         goto 1234
      end if

!     mask cell
      cellmask(k) = 1

!     remove masked cells
      call remove_masked_netcells()

 1234 continue

!     deallocate
      deallocate(cellmask)

      return
   end subroutine removecell
