!>  find cells that are directly and indirectly connected to cell k
   subroutine find_surrounding_cells(kcell, nmax, ndirect, nindirect, kdirect, kindirect, kne)
      use m_netw

      implicit none

      integer,                  intent(in)    :: kcell      !< cell number
      integer,                  intent(in)    :: nmax       !< array size
      integer,                  intent(out)   :: ndirect    !< number of   directly connected cells (=netcell()%N)
      integer,                  intent(out)   :: nindirect  !< number of indirectly connected cells
      integer, dimension(nmax), intent(out)   :: kdirect    !< directly connected cells
      integer, dimension(nmax), intent(out)   :: kindirect  !< indirectly connected cells
      integer, dimension(2,nmax), intent(out) :: kne        !< two indirectly connected cells that are adjacent to a directly connected cell

      integer                               :: nneighbors

      integer                              :: i, j, kk, kkk, kkkk, kcell1, kcell2, k1, L

      ndirect   = 0
      kdirect   = 0
      nindirect = 0
      kindirect = 0
      kne       = 0

!     find the directly connected cells
 kklp:do kk=1,netcell(kcell)%N
         L = netcell(kcell)%lin(kk)

         if ( lnn(L).lt.2 ) cycle

         kcell2 = lne(1,L) + lne(2,L) - kcell   ! other cell

!        check and see if the cell is already administered
         do kkk=1,ndirect
            if ( kcell2.eq.kdirect(kkk) ) cycle kklp
         end do

         ndirect = ndirect+1
         if ( ndirect.gt.nmax ) then
            ndirect = ndirect-1
            call qnerror('find_surrounding_cells: array too small', ' ', ' ')
            return
         end if
         kdirect(ndirect) = kcell2
      end do kklp

!     find the cells indirectly connected cells
      do kk=1,netcell(kcell)%N
         k1 = netcell(kcell)%nod(kk)
         do kkk=1,nmk(k1)
            L = nod(k1)%lin(kkk)
        ilp:do i=1,lnn(L)
               kcell2 = lne(i,L)
!              check and see if the cell is new
               if ( kcell2.eq.kcell ) cycle ilp

               do kkkk=1,ndirect
                  if ( kcell2.eq.kdirect(kkkk) ) cycle ilp
               end do

               do kkkk=1,nindirect
                  if ( kcell2.eq.kindirect(kkkk) ) cycle ilp
               end do

!              add new cell
               nindirect = nindirect + 1
               if ( nindirect.gt.nmax ) then
                  nindirect = nindirect-1
                  call qnerror('find_surrounding_cells: array size too small', ' ', ' ')
                  return
               end if
               kindirect(nindirect) = kcell2
            end do ilp
         end do
      end do


!     find the adjacent cells
      do i=1,ndirect
         kcell1 = kdirect(i)
         do j=1,netcell(kcell1)%N
            L = netcell(kcell1)%lin(j)
            if ( lnn(L).lt.2 ) cycle
            kcell2 = lne(1,L) + lne(2,L) - kcell1
!           check and see if this cell is administered
            do kk=1,ndirect
               if ( kdirect(kk).eq.kcell2 ) then
                  if ( kne(1,i).eq.0 ) then
                     kne(1,i) = -kcell2
                     kcell2 = -1234
                  else
                     kne(2,i) = -kcell2
                     kcell2 = -1234
                  end if
               end if
            end do

            if ( kcell2.eq.-1234 ) cycle

            do kk=1,nindirect
               if ( kindirect(kk).eq.kcell2 ) then
                  if ( kne(1,i).eq.0 ) then
                     kne(1,i) = kcell2
                  else
                     kne(2,i) = kcell2
                  end if
               end if
            end do

         end do
      end do

      return
   end subroutine find_surrounding_cells
