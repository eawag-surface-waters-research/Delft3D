!> delete cell by merging all its nodes and update administration
subroutine killcell(xp,yp)
   use m_netw
   use m_missing, only: dmiss, jins
   use geometry_module, only: pinpok
   use gridoperations

   implicit none

   double precision,                intent(in) :: xp, yp             !< coordinates of input point

   integer, parameter                          :: NMAX=100           !< array size
   integer                                     :: ndirect            !< number of directly connected cells
   integer                                     :: nindirect          !< number of indirectly connected cells
   integer, dimension(NMAX)                    :: kdirect            !< directly connected cells, i.e. cells sharing a link with cell k
   integer, dimension(NMAX)                    :: kindirect          !< indirectly connected cells, i.e. cells sharing a node, but not a link, with cell k
   integer, dimension(2,NMAX)                  :: kne                !< left and right neighboring (in)direct cell that neighbors the directly connected cells

   integer                                     :: k, kk, k1
   integer                                     :: in, ja

   integer, save                               :: NEEDFINDCELLS=1

   if ( nump.lt.1 ) NEEDFINDCELLS=1

   if ( NEEDFINDCELLS /= 0 .or. netstat /= NETSTAT_OK ) then
      call findcells(100)
      call makenetnodescoding()
      NEEDFINDCELLS = 0
   end if

!  find the cell
   in = 0
   do k = 1,nump
      if ( netcell(k)%N.lt.1 ) cycle
      call pinpok(xp, yp, netcell(k)%N, xk(netcell(k)%nod), yk(netcell(k)%nod), in, jins, dmiss)
      if ( in.gt.0 ) exit
   end do

   if ( in.eq.0 ) then  ! no cell found
      call qnerror('killcell: no cell found', ' ', ' ')
      return
   end if

!   write(6,*) 'Cell ', k, 'N=', netcell(k)%N

!  get the connected cells
   call find_surrounding_cells(k, NMAX, ndirect, nindirect, kdirect, kindirect, kne)


!  whipe out previous net image
!   call teknet(0,ja)
   do kk=1,netcell(k)%N
      call teknode(netcell(k)%nod(kk),0)
   end do

!  delete cell and update administration
   k1 = netcell(k)%nod(1)
   call deletecell(k, ndirect, nindirect, kdirect, kindirect, kne, .true., ja)

!   call pfiller(xk(netcell(k)%nod), yk(netcell(k)%nod), netcell(k)%N, ncolhl, 30)

!   call teknet(ncolhl,ja)
   if ( netcell(k)%N.eq.0 ) then ! cell removed: draw remaining node and links connected to it
      call teknode(k1,1)
   else                          ! cell not removed: draw whole cell and links connected to it
      do kk=1,netcell(k)%N
         call teknode(netcell(k)%nod(kk),1)
      end do
   end if

   return
end subroutine killcell
