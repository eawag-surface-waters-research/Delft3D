module m_particles
   integer                                      :: japart       !< particles (1) or not (0)
                   
   integer                                      :: Npart        !< number of particles
   double precision,  dimension(:), allocatable :: xpart, ypart !< coordinates of particles, dim(Npart)
   double precision,  dimension(:), allocatable :: dtremaining  !< remaining time, dim(Npart)
   integer,           dimension(:), allocatable :: kpart        !< cell (flownode) number, dim(Npart)
!   integer,           dimension(:), allocatable :: Lpart        !< edge (netlink)  number, dim(Npart)
!   character(len=40), dimension(:), allocatable :: namepart     !< name of particle, dim(Npart)
   
   integer,           dimension(:), allocatable :: numzero      !< number of consecutive (sub)times a particle was not displaces within a time-step
   
   integer                                      :: jatracer     !< add tracer with particle concentration (1) or not (0)
   double precision                             :: starttime    !< start time
   double precision                             :: timestep     !< time step (>0) or every computational timestep
   double precision                             :: timelast     !< last time of particle update
   double precision                             :: timenext     !< next time of particle update
   character(len=22), parameter                 :: PART_TRACERNAME = 'particle_concentration' !< particle tracer name
   integer                                      :: part_iconst  !< particle tracer constituent number
   
   double precision, dimension(:), allocatable  :: sbegin      !< water level at begin of time interval, dim(Ndx)
   double precision, dimension(:), allocatable  :: qpart       !< cummulative fluxes from begin of time interval, dim(Lnx)
end module m_particles

module m_partrecons
   double precision,  dimension(:), allocatable :: qe           !< fluxes at all edges, including internal
   double precision,  dimension(:), allocatable :: u0x, u0y, alpha !< reconstruction of velocity fiels in cells, dim(numcells)   
end module m_partrecons
   
module m_partfluxes
   integer,          dimension(:),   allocatable :: iflux2link   !< sparse storage of edge-based flux to flowlink-based flux (prescribed), flowlinks, dim(jflux2link(numedges+1)-1)
   integer,          dimension(:),   allocatable :: jflux2link   !< sparse storage of edge-based flux to flowlink-based flux (prescribed), startpointer, dim(numedges+1)
   double precision, dimension(:),   allocatable :: Aflux2link   !< sparse storage of edge-based flux to flowlink-based flux (prescribed), coefficients, dim(jflux2link(numedges+1)-1)
end module m_partfluxes

module m_partmesh
!  mesh data
   integer                                       :: numnodes    !< number of nodes, >= numk
   integer                                       :: numedges    !< number of edges, >= numL
   integer                                       :: numcells    !< number of cells, >= nump
   integer                                       :: numorigedges  !< number of original "non-internal" edges (numL)
                                                 
   integer,          dimension(:,:), allocatable :: edge2node   !< edge-to-node, dim(2,numedges)
   integer,          dimension(:,:), allocatable :: edge2cell   !< edge-to-cell, dim(2,numedges)

   double precision, dimension(:),   allocatable :: xnode       !< x-coordinate of nodes, dim(numnodes)
   double precision, dimension(:),   allocatable :: ynode       !< y-coordinate of nodes, dim(numnodes)
                                                 
   double precision, dimension(:),   allocatable :: xzwcell     !< x-coordinate of cell c/g, dim(numcells)
   double precision, dimension(:),   allocatable :: yzwcell     !< y-coordinate of cell c/g, dim(numcells)
   double precision, dimension(:),   allocatable :: areacell    !< area of cell, dim(numcells)
   
   integer,          dimension(:),   allocatable :: icell2edge   !< sparse storage of cell-to-edge, data, dim(jcell2edge(numcells+1)-1)
   integer,          dimension(:),   allocatable :: jcell2edge   !< sparse storage of cell-to-edge, startpointer, dim(numcells+1)
   
   integer,          dimension(:),   allocatable :: edge2link    !< edge to "flowlink" (>0), no flowlink (0), or new inner link (<0), dim(numedges)
!   integer,          dimension(:),   allocatable :: nod2cell     !< "flownode" to cell (>0), first new "inner" triangle (<0), dim(numcells), note: numcells can be too large for array dimension
   integer,          dimension(:),   allocatable :: cell2nod     !< cell to "flownode" (>0), new triangle (<0), dim(numcells), note: numcells can be too large for array dimension
   
   double precision, dimension(:),   allocatable :: dnx          !< x-coordinate of edge normal vector (positive outward for edge2cell(1,:), positive inward for edge2cell(2,:)), dim(numedges)
   double precision, dimension(:),   allocatable :: dny          !< y-coordinate of edge normal vector (positive outward for edge2cell(1,:), positive inward for edge2cell(2,:)), dim(numedges)
   double precision, dimension(:),   allocatable :: w            !< edge width, dim(numedges)
   
   integer,                            parameter :: MAXSUBCELLS=10
end module m_partmesh

!> update positions of particles
subroutine update_particles(q,s0,s1,Dt)
!   use m_flow
   use m_particles
   use m_wearelt
!   use m_flowtimes, only: dts, time1
   use m_flowgeom, only: Ndx, Lnx
   use m_transport, only: numconst, constituents
   implicit none
   
   double precision, dimension(Lnx), intent(in) :: q  !< fluxes
   double precision, dimension(Ndx), intent(in) :: s0 !< water levels at start of time interval
   double precision, dimension(Ndx), intent(in) :: s1 !< water levels at end of time interval
   double precision,                 intent(in) :: Dt !< time interval
   
   integer :: i, k
   integer :: numremaining ! number of remaining particles to be updated
   integer :: iter
   integer :: ierror
   
   integer, parameter :: MAXITER = 1000 ! maximum number of substeps
   
   if ( japart.ne.1 ) return
   
   ierror = 1
      
!  reconstruct velocity field
   call reconst_vel(q, s0, s1, ierror)
   if ( ierror.ne.0 ) goto 1234
   
!  set remaining time to time step
   dtremaining = Dt
!   Lpart = 0
   
   numzero = 0
   do iter=1,MAXITER
!     update particles in cells
      call update_particles_in_cells(numremaining, ierror)
      if ( ierror.ne.0 ) goto 1234
      
      if ( numremaining.eq.0 ) then
!         write(6,*) 'iter=', iter
         exit
      end if
   end do
   
   
!  check for remaining particles
   if ( numremaining.gt.0 ) then
 !    plot remaining particles
      do i=1,Npart
         if ( dtremaining(i).gt.0d0 .and. kpart(i).gt.0 ) then
            call cirr(xpart(i),ypart(i), 211)
         end if
      end do
      
      call qnerror('update_particles: iter>MAXITER', ' ', ' ')
      
      goto 1234
   end if
   
   if ( jatracer.eq.1 ) then
!     udpate particle concentration   
      call comp_concentration(s1,numconst,part_iconst,constituents)
   end if
   
   ierror = 0
1234 continue
   
   return
end subroutine update_particles
   
!> update positions of particles within triangles
subroutine update_particles_in_cells(numremaining, ierror)
   use m_particles
   use m_partrecons
   use m_partmesh
   implicit none
   
   integer,        intent(out) :: numremaining !< number of remaining particles to be updated
   integer,        intent(out) :: ierror       !< error (1) or not (0)
   
   integer                     :: ipart
   integer                     :: i, k, k1, k2, L, Lf
   integer                     :: ja
   integer                     :: Lexit
   
   double precision            :: d, d1, un
   double precision            :: t, tex, dt
   double precision            :: ux0, uy0, cs, sn
   double precision            :: xn, yn, rl
   double precision            :: dvar
   
   logical                     :: isboundary
   
   double precision, parameter :: DTOL = 1d-4
   double precision, parameter :: DTOLd  = 1d-4
   double precision, parameter :: DTOLun = 1d-4
   
   integer,          parameter :: MAXNUMZERO = 10
   
   ierror = 1
   
   numremaining = 0
   
   do ipart=1,Npart
!     check if this particle needs to be updated
      if ( dtremaining(ipart).eq.0d0 .or. kpart(ipart).lt.1 ) cycle
!     get cell (flownode) particle in in
      k = kpart(ipart)
      
!     compute exit time <= dtremaining
      tex = dtremaining(ipart)
      
      Lexit = 0   ! exit edge (flowlink)
      
 !    compute velocity at current position     
      ux0 = u0x(k) + alpha(k)*(xpart(ipart)-xzwcell(k))
      uy0 = u0y(k) + alpha(k)*(ypart(ipart)-yzwcell(k))
      
!     loop over edges (netlinks) of cells
      do i=jcell2edge(k),jcell2edge(k+1)-1
         L = icell2edge(i)   ! edge
         
         k1 = edge2node(1,L)
         k2 = edge2node(2,L)
        
         cs = dnx(L)
         sn = dny(L)
         if ( edge2cell(2,L).eq.k ) then
            cs = -cs
            sn = -sn
         end if
         
!        check for boundary edge
         isboundary = ( edge2cell(1,L).eq.0 .or. edge2cell(2,L).eq.0 )
         
!        compute normal distance to edge
         if ( isboundary ) then ! boundary: add tolerance
            call dlinedis2(xpart(ipart),ypart(ipart),xnode(k1)+cs*DTOLd,ynode(k1)+sn*DTOLd,xnode(k2)+cs*DTOLd,ynode(k2)+sn*DTOLd,ja,d,xn,yn,rl)
         else
            call dlinedis2(xpart(ipart),ypart(ipart),xnode(k1),ynode(k1),xnode(k2),ynode(k2),ja,d,xn,yn,rl)
         end if
         
!        check inside or outside triangle 
         if ( (xn-xpart(ipart))*cs + (yn-ypart(ipart))*sn.lt.-DTOLd .and. rL.ge.0d0 .and. rL.le.1d0 .and. .not.isboundary ) then
!           outside triangle
            tex = 0d0
            Lexit = L
            exit
         else
!           inside triangle

   !        compute normal velocity to edge (outward positive)
            un =  ux0*cs + uy0*sn
         
            if ( un.gt.DTOLun*d ) then   ! normal velocity does not change sign: sufficient to look at u0.n
   !           compute exit time for this edge: ln(1+ d/un alpha) / alpha
               dvar = alpha(k)*d/un
               if ( dvar.gt.-1d0) then
                  t = d/un
                  if ( abs(dvar).ge.DTOL ) then
                     t = t * log(1d0+dvar)/dvar
                  end if
               else
                  t = huge(1d0)
               end if  
            
   !           update exit time/edge (flowlink)      
               if ( t.le.tex .and. t.ge.0d0 ) then
                  tex = t
                  Lexit = L
               end if
            else
               continue
            end if
         
         end if
      end do
         
       if ( dtremaining(ipart).eq.0d0 ) then
         continue
      end if
         
!     compute timestep in cell (flownode)
      dt = min(dtremaining(ipart), tex)
         
!     update particle
      if ( abs(alpha(k)).lt.DTOL ) then
         dvar = dt
      else
         dvar = (exp(alpha(k)*dt)-1d0)/alpha(k)
      end if
      
      xpart(ipart) = xpart(ipart) + dvar * ux0
      ypart(ipart) = ypart(ipart) + dvar * uy0
      
      dtremaining(ipart) = dtremaining(ipart) - dt
!      Lpart(ipart) = Lexit
      
      if ( dt.eq.0d0 ) then
         numzero(ipart) = numzero(ipart) + 1
      end if
      
      if ( numzero(ipart).gt.MAXNUMZERO ) then
!        disable particle that is not moving
         kpart(ipart) = 0
         dtremaining(ipart) = 0d0
      end if
         
!     proceed to neighboring cell (if applicable)
      if ( Lexit.gt.0 ) then
         numremaining = numremaining + 1  ! number of remaining particles for next substep
         if ( edge2cell(1,Lexit).gt.0 .and. edge2cell(2,Lexit).gt.0 ) then   ! internal edge (netlink)
            kpart(ipart) = edge2cell(1,Lexit) + edge2cell(2,Lexit) - k
            
            if ( kpart(ipart).eq.0 ) then
               continue
            end if
         else  ! on boundary
            kpart(ipart) = 0
         end if
      else
!        safety check
         if ( dtremaining(ipart).ne.0d0 ) then
            ierror = 1
            call qnerror('update_particles_in_cells: dtremaining <> 0', ' ', ' ')
            goto 1234
         end if
      end if
         
   end do
   
   ierror = 0
1234 continue
   
   return
end subroutine update_particles_in_cells


!> reconstruct velocity in cells
subroutine reconst_vel(q, s0, s1, ierror)
   use m_flowgeom, only: Ndx, Lnx, bl  !, lne2ln  !, csu, snu, wu  !, xu, yu
   use m_flowparameters, only: epshs
   use m_partrecons
   use m_partmesh
   implicit none
   
   double precision, dimension(Lnx), intent(in)  :: q    ! flowlink-based discharge (m3/s)
   double precision, dimension(Ndx), intent(in)  :: s0   ! flownode-based water level (m) at begin of interval
   double precision, dimension(Ndx), intent(in)  :: s1   ! flownode-based water level (m) at end of interval
   
   integer,                          intent(out) :: ierror
   
   integer,                          parameter   :: N = 3
   
   double precision, dimension(N,N)              :: Amat ! matrix
   double precision, dimension(N)                :: rhs
   
   integer                                       :: i, icell, j, k, L, Lf
   integer                                       :: k1, k2
   integer                                       :: ja
   
   double precision                              :: cs, sn, wwu, h0, h1, h, dfac
   
   double precision, parameter                   :: DTOL=1d-10
   
   double precision, external                    :: dbdistance
   
   ierror = 1
   
!  get fluxes at all edges, including internal
   call set_fluxes(Lnx, q, qe)
   
!  initialize   
   u0x = 0d0
   u0y = 0d0
   alpha = 0d0

!  loop over internal cells
   do icell=1,numcells
!     check for triangles
!      if ( jcell2edge(k+1)-jcell2edge(k).ne.3 ) then
!         cycle
!      end if
   
!     get flownode number (for s, bl)
      k = iabs(cell2nod(icell))

!     fill system for (ux,uy) = (ux0, uy0) + alpha (x-x0, y-y0)
      
!     loop over edges (netlinks)
      i = 0
      do j=jcell2edge(icell),jcell2edge(icell+1)-1
         i = i+1
         L = icell2edge(j) ! netlink
         
         k1 = edge2node(1,L)
         k2 = edge2node(2,L)
         
         cs = dnx(L)
         sn = dny(L)
         
         wwu = w(L)
            
!        add to system
         Amat(i,1) = cs
         Amat(i,2) = sn
         Amat(i,3) = (0.5d0*(xnode(k1)+xnode(k2))-xzwcell(icell))*cs + (0.5d0*(ynode(k1)+ynode(k2))-yzwcell(icell))*sn
         
!        u.n = q / ( h wu )
         h0 = s0(k)-bl(k)
         h1 = s1(k)-bl(k)
         if ( abs(h1-h0).gt.DTOL ) then
            if ( h0.gt.epshs .or. h1.gt.epshs ) then
               h = (h1-h0)/(log(h1)-log(h0))
            else
               h = 0d0
            end if
         else
            h = 0.5d0*(h0+h1)
         end if
         
         if ( h.gt.epshs ) then
            rhs(i) = qe(L) / ( h*wwu )
         else
         
            rhs(i) = 0d0
         end if
      end do
      
!     solve system
      call gaussj(Amat,3,N,rhs,1,1)
      
      u0x(icell)   = rhs(1)
      u0y(icell)   = rhs(2)
      alpha(icell) = rhs(3)
      
!     BEGIN DEBUG
!      if ( k.eq.16 ) then
!         write(6,*) (u0x(k) + alpha(k)*(xu(Lf)-xz(k))) * csu(Lf) +  &
!            (u0y(k) + alpha(k)*(yu(Lf)-yz(k))) * snu(Lf), q(Lf) / ( (s(k)-bl(k))*wu(Lf) )
!      end if
!     END DEBUG
   end do
   
   ierror = 0
!  error handling
1234 continue

   return
end subroutine reconst_vel


!> draw particles in GUI
subroutine tekpart()
   use m_particles
   use m_wearelt
   use unstruc_display
   implicit none
   
   integer :: i
   
   if ( Npart.lt.1 .or. ndrawpart.eq.1 ) return

   call setcol(31)
   do i=1,Npart
      call movabs(xpart(i),ypart(i))
      call cir(rcir)
   end do
   
   return
end subroutine tekpart

!> set pointer
subroutine part_setmesh()
   use network_data, only: kn, xk, yk, xzw, yzw, numk, numL, nump, netcell, lnn, lne
   use m_flowgeom, only: lne2ln, ba
   use m_alloc
   use m_missing
   use m_partmesh
   implicit none
   
   integer                        :: i, node1, node2, icell, j, k, L, N
   integer                        :: im1, ip1, Lm1, Lp1
   integer                        :: newnode, newedge
   integer                        :: isign, ja, k1, k2
   integer                        :: jaswap
                                  
   integer                        :: numnontris
   integer                        :: numaddedges
   
   double precision, dimension(3) :: xv, yv
                                  
   double precision, external     :: dbdistance
   
   numnodes = numk
   numedges = numL
   numcells = nump
   
!  count number of non-triangles and additional (internal) edges
   numnontris=0
   numaddedges=0
   do k=1,nump
      N = netcell(k)%N
      if ( N.gt.3 ) then
         numnontris = numnontris+1
         numaddedges = numaddedges+N
      end if
   end do
   
!  compute sizes
   numnodes = numk + numnontris                 ! add centers of non-triangles
   numedges = numL + numaddedges                ! add internal edges of non-triangles
   numcells = nump - numnontris + numaddedges   ! add internal triangles of non-triangles
   
!  (re)allocate   
   call realloc_partmesh()
   
!  nodes   
   do k=1,numk
      xnode(k) = xk(k)
      ynode(k) = yk(k)
   end do

!  edges
   do L=1,numL
      edge2node(1,L) = kn(1,L)
      edge2node(2,L) = kn(2,L)
      if ( lne2ln(L).ge.0 ) then
         edge2link(L) = lne2ln(L)
      else
!        not a flowlink number, see flow_geominit
         continue
      end if
   end do
   
!  set jcell2edge startpointers and add new triangles
   icell=0
   jcell2edge(1) = 1
   do k=1,nump
      N = netcell(k)%N
      if ( N.eq.3 ) then
         icell = icell+1
         jcell2edge(icell+1) = jcell2edge(icell) + 3
!         nod2cell(k) = icell
         cell2nod(icell) = k
      else if ( N.gt.3 ) then
         do j=1,N
            icell = icell+1
            jcell2edge(icell+1) = jcell2edge(icell) + 3
!            if ( j.eq.1 ) nod2cell(k) = -icell
            cell2nod(icell) = -k
         end do
      else  ! should not happen
         continue
      end if
   end do
   
!  allocate jcell2edge
   N = jcell2edge(numcells+1)-1
   call realloc(icell2edge, N, fill=0, keepExisting=.false.)
   
!  copy netcell data into cell2edge and add new triangles (nodes, edges)
   icell = 0
   newnode = numk
   numorigedges = numL
   newedge = numorigedges
   do k=1,nump
      N = netcell(k)%N
      if ( N.eq.3 )then
         icell = icell+1   ! add existing triangle
         i = 0
         do j=jcell2edge(icell),jcell2edge(icell+1)-1
            i = i+1
            L = netcell(k)%lin(i)
            icell2edge(j) = L
         end do
         xzwcell(icell) = xzw(k)
         yzwcell(icell) = yzw(k)
         areacell(icell) = ba(k)
      else
!        add node
         newnode = newnode+1
         xnode(newnode) = xzw(k)
         ynode(newnode) = yzw(k)
         
         xv(1) = xzw(k)
         yv(1) = yzw(k)
         
         do i=1,N ! add new edges and triangles
            im1 = i-1; if ( im1.lt.1 ) im1=im1+N
            ip1 = i+1; if ( ip1.gt.N ) ip1=ip1-N
            L = netcell(k)%lin(i)
            call find_common_node(netcell(k)%lin(im1),L,node1)
            call find_common_node(netcell(k)%lin(ip1),L,node2)
            
            icell = icell+1
!           add edges
            newedge = newedge+1
            edge2node(1,newedge) = newnode
            edge2node(2,newedge) = node1
            edge2link(newedge) = -1
            
!           add edges to cell2edge
            j = jcell2edge(icell)
            icell2edge(j) = newedge
            icell2edge(j+1) = L
            icell2edge(j+2) = newedge+1
            if ( i.eq.N ) then
               icell2edge(j+2) = icell2edge(j+2) - N
            end if
            
!           compute mass center
            xv(2) = xk(node1)
            yv(2) = yk(node1)
            xv(3) = xk(node2)
            yv(3) = yk(node2)
            call comp_masscenter(3, xv, yv, xzwcell(icell), yzwcell(icell), areacell(icell), ja)
            
 !           call cirr(xzwcell(k), yzwcell(k), 31)
         end do
      end if
   end do
   
!  set edge2cell
   edge2cell = 0
   do icell=1,numcells
      do j=jcell2edge(icell),jcell2edge(icell+1)-1
         L = icell2edge(j)
         if ( edge2cell(1,L).eq.0 ) then
            edge2cell(1,L) = icell
         else
            edge2cell(2,L) = icell
         end if
      end do
   end do
   
!  fix orientation of edge2cell
   do L=1,numL ! exclude new edges
      k1 = edge2cell(1,L)
      if ( k1.eq.0 ) cycle
      
      k1 = iabs(cell2nod(k1))
      
      jaswap = 0
      
      if ( lnn(L).eq.1 ) then  !     boundaries: inward normal
         jaswap = 1
      else if ( k1.eq.lne(2,L) .and. lnn(L).gt.1 ) then
         jaswap = 1
      end if
      
      if ( jaswap.eq.1 ) then
         icell = edge2cell(1,L)
         edge2cell(1,L) = edge2cell(2,L)
         edge2cell(2,L) = icell
      end if
   end do
   
!  nx, ny, w
   do L=1,numedges
      k1 = edge2node(1,L)
      k2 = edge2node(2,L)
      
      k = edge2cell(1,L)   ! outward positive
      
      isign = 1
      if ( k.le.0 ) then
         k = edge2cell(2,L)   ! inward positive
         isign = -1
      end if
      
      if ( k.eq.0 ) cycle  ! isolated edge
      
!     compute normal vector (outward positive)
      call normaloutchk(xnode(k1),ynode(k1),xnode(k2),ynode(k2),xzwcell(k),yzwcell(k),dnx(L),dny(L),ja)
      
      if ( isign.eq.-1 ) then
         dnx(L) = -dnx(L)
         dny(L) = -dny(L)
      end if
      
!     compute width of edge (netlink)
      w(L) = dbdistance(xnode(k1),ynode(k1),xnode(k2),ynode(k2))
   end do
   
   
   return
end subroutine part_setmesh

!> set all fluxes, including internal
subroutine set_fluxes(Lnx,q,qe)
   use m_partmesh
   use m_partfluxes
   implicit none
   
   integer,                               intent(in)  :: Lnx
   double precision, dimension(Lnx),      intent(in)  :: q
   
   double precision, dimension(numedges), intent(out) :: qe
   
   integer                                            :: j, L, Lf
   
   do L=1,numedges
      qe(L) = 0d0
      do j=jflux2link(L),jflux2link(L+1)-1
         Lf = iflux2link(j)
         if ( Lf.gt.0 ) then
            qe(L) = qe(L) + Aflux2link(j)*q(Lf)
         end if
      end do
   end do
   
!   double precision, dimension(:),        allocatable :: dum
   
!   integer                                            :: i, j, k, k1, k2
!   integer                                            :: icL, icR, n1, n2
!   integer                                            :: L, L1, L2, L3, Lf, N
   
!   double precision                                   :: sumq, sumarea, circ, dcirc, dlen(MAXSUBCELLS), sumlen, dq
   
!   qe = 0d0
!   
!!  set non-internal fluxes   
!   do L=1,numedges
!      Lf = edge2link(L) ! flow link
!      if ( Lf.gt.0 ) then
!         qe(L) = q(Lf)
!      end if
!   end do
!   
!!  compute flux balances of original non-triangular cells (internal fluxes are still 0d0)
!   allocate(dum(numcells))
!   dum = 0d0
!   do L=1,numedges
!      k1 = edge2cell(1,L)
!      k2 = edge2cell(2,L)
!      
!      if ( k1.gt.0 ) dum(k1) = dum(k1) - qe(L)
!      if ( k2.gt.0 ) dum(k2) = dum(k2) + qe(L)
!   end do
!   
!!  set internal fluxes
!   k = 1 ! cell number
!   do while ( k.le.numcells )
!      
!      if ( cell2nod(k).lt.0 ) then  ! new triangles
!!        count number of sub-triangles
!         N = 0
!         sumq = 0d0
!         sumarea = 0d0
!         do while ( cell2nod(k+N).eq.cell2nod(k) )
!            sumq = sumq + dum(k+N)
!            sumarea = sumarea + areacell(k+N)
!            N = N+1
!            if ( k+N.gt.numcells ) exit
!         end do
!         
!         if ( N.eq.0 ) then
!            k = k+1
!            cycle
!         end if
!         
!         sumq = sumq/sumarea  ! div(q)
!         
!!        loop over all sub-triangles
!         circ = 0d0  ! circulation
!         sumlen = 0d0  ! summed area of triangles
!         j = jcell2edge(k)
!         do i=1,N
!!           get edge numbers     
!            L1 = icell2edge(j+(i-1)*3)   ! "left" internal
!            L2 = icell2edge(j+(i-1)*3+1) ! edge of original non-triangle
!            L3 = icell2edge(j+(i-1)*3+2) ! "right" internal
!            
!            if ( i.eq.1 ) then
!               qe(L1) = 0d0
!            end if
!            
!!           determine "right" internal flux from flux balance with other two fluxes
!            qe(L3) = sumq*areacell(k)  ! outward normal assumed (corrected later)
!            
!            if ( edge2cell(1,L1).eq.k ) then ! outward normal
!               qe(L3) = qe(L3) - qe(L1)
!            else  ! inward normal
!               qe(L3) = qe(L3) + qe(L1)
!            end if
!            
!            if ( edge2cell(1,L2).eq.k ) then ! outward normal
!               qe(L3) = qe(L3) - qe(L2)
!            else  ! inward normal
!               qe(L3) = qe(L3) + qe(L2)
!            end if
!            
!!           account for orientation            
!            if ( edge2cell(2,L3).eq.k ) then ! outward normal
!               qe(L3) = -qe(L3)
!            end if
!            
!!           add to circulation
!            icL = edge2cell(1,L3)
!            icR = edge2cell(2,L3)
!            dlen(i) = 0.25d0*(areacell(icL)+areacell(icR))/w(L3)
!            sumlen = sumlen + dlen(i)/w(L3)
!            if ( icR.eq.k ) then
!               dlen(i) = -dlen(i)
!            end if
!            
!            dcirc = qe(L3)/w(L3)*dlen(i)
!            circ = circ + dcirc
!            
!!           update cell number
!            k = k+1  
!         end do
!         
!!        correct for circulation
!         dq = -circ/sumlen
!         do i=1,N
!            L3 = icell2edge(j+(i-1)*3+2) ! "right" internal
!            if ( dlen(i).gt.0d0 ) then
!               qe(L3) = qe(L3) + dq
!            else
!               qe(L3) = qe(L3) - dq
!            end if
!         end do
!      else
!         k = k+1
!      end if
!   end do
!   
!   if ( allocated(dum) ) deallocate(dum)
   
    return
end subroutine set_fluxes


!> compute mapping from prescribed (at flowlinks) to all fluxes (at all edges, including "internal")
subroutine comp_fluxcoeffs()
   use m_partmesh
   use m_partfluxes
   use m_alloc
   use unstruc_messages
   implicit none
   
   integer                                  :: N         ! number of subtriangles
                     
   integer,          dimension(MAXSUBCELLS) :: L1        ! "internal" edges
   integer,          dimension(MAXSUBCELLS) :: L2        ! original "non-internal" edges
   integer,          dimension(MAXSUBCELLS) :: icell     ! subcells
   integer,          dimension(MAXSUBCELLS) :: isign1    ! orientation of "internal: edges L1, all cw or 
   integer,          dimension(MAXSUBCELLS) :: isign2    ! orientation of "non-internal: edges L2, outward normal
   double precision, dimension(MAXSUBCELLS) :: circ      ! weight of edge L1 in computation of circulation
   
   double precision, dimension(MAXSUBCELLS,MAXSUBCELLS) :: A, B
   
   double precision                         :: areasum, dlen
   
   integer                                  :: i, im1, ip1, j, j2, k, L, L3, Lf
                                     
   integer, external                        :: icommonval
   
!  allocate
   call realloc_partfluxes()
!   call realloc(jflux2link, numedges+1, fill=0, keepExisting=.false.)
   
!  set startpointers
   jflux2link(1) = 1
   L = 1
   do while ( L.le.numedges )
      Lf = edge2link(L)
      if ( Lf.gt.0 ) then
!        original flowlink
         jflux2link(L+1) = jflux2link(L) + 1
         
!        proceed to next edge         
         L = L+1
      else if ( L.gt.numorigedges ) then
!        internal edge: find number of subtriangels
         N = 0
         do while ( icommonval(edge2cell(:,L+N), edge2cell(:,L+N+1)).ne.0 )
            N = N+1
            if ( L+N.ge.numedges ) exit
         end do
         
         if ( N.gt.0 ) then
!           check connection first and last subtriangle
            if ( icommonval(edge2cell(:,L), edge2cell(:,L+N)).ne.0 ) then
               N = N+1
               
               do i=1,N
                  jflux2link(L+1) = jflux2link(L) + N
   !              proceed to next edge  
                  L = L+1
               end do
            else
               call mess(LEVEL_ERROR, 'comp_fluxcoeffs: numbering error')
            end if
         
         else  ! should not happen
            call mess(LEVEL_ERROR, 'comp_fluxcoeffs: numbering error')
         end if
         
         if ( L.ge.numedges ) exit
      else  ! other
         jflux2link(L+1) = jflux2link(L)
!        proceed to next edge  
         L = L+1
      end if
   end do
   
!  reallocate
   call realloc(iflux2link, jflux2link(numedges+1)-1, fill=0, keepExisting=.false.)
   call realloc(Aflux2link, jflux2link(numedges+1)-1, fill=0d0, keepExisting=.false.)
   
!  fill iflux2link (first in edge-numbers) and compute coefficients
   L = 1
   do while ( L.le.numedges )
      j=jflux2link(L)
      N = jflux2link(L+1)-j
      
      if ( N.eq.1 ) then
!        original "non-internal" link      
         iflux2link(j) = L
         Aflux2link(j) = 1d0
!        proceed to next edge  
         L = L+1
      else if ( N.gt.1 ) then
!        "internal" link
         do i=1,N
            ip1 = i+1; if ( ip1.gt.N ) ip1 = ip1-N
            L1(i) = L+i-1
            L3 = L1(i)+ip1
            
!           find subcell            
            icell(i) = icommonval(edge2cell(:,L+i-1), edge2cell(:,L+ip1-1))
            
!           find original edge
            j2 = jcell2edge(icell(i))+1   ! order of cell edges: "left" internal (L1), orginal (L2), "right internal (L3)
            L2(i) = icell2edge(j2)
            
!           get orientation of "internal" edges (i-dir positive)
!           note: will always be (/ -1, 1, 1, ... /), due to generation of edge2cells in part_setmesh
            isign1(i) = 1
            if ( edge2cell(1,L1(i)).eq.icell(i) ) isign1(i) = -1
            
!           get orientation of original "non-internal" edges (outward normal)
            isign2(i) = 1
            if ( edge2cell(2,L2(i)).eq.icell(i) ) isign2(i) = -1
         end do
         
!        compute summed area and circulation weights
         areasum = 0d0
         do i=1,N
            im1 = i-1; if ( im1.lt.1 ) im1=im1+N
            areasum = areasum + areacell(icell(i))
            dlen = 0.25*(areacell(icell(im1))+areacell(icell(i)))/w(L1(i))
            circ(i) = dlen/w(L1(i))
         end do
         
!        build system: A qe = B qlink
         A = 0d0
         B = 0d0
!        continuity equations
         do i=1,N-1
            A(i,i)   = -isign1(i)   ! outward
            A(i,i+1) =  isign1(i+1) ! outward
            B(i,i)   = -isign2(i)   ! outward, rhs
            do k=1,N
               B(i,k) = B(i,k) + areacell(icell(i))*isign2(k)/areasum
            end do
         end do
!        circulation
         do k=1,N
            A(N,k) = circ(k) * isign1(k)
         end do
         
!        invert matrix: qe = inv(A) B qlink
         call gaussj(A,N,MAXSUBCELLS,B,N,MAXSUBCELLS)
         
!        fill data
         do i=1,N
            iflux2link(j:j+N-1) = L2(1:N)
            Aflux2link(j:j+N-1) = B(i,1:N)
            
!           proceed to next edge  
            j = j+N
            L = L+1
         end do
      else
!        closed boundary: proceed to next edge
         L = L+1
      end if
   end do
   
!  convert to link number
   do j=1,jflux2link(numedges+1)-1
      L  = iflux2link(j)
      Lf = edge2link(L)
      if ( Lf.gt.0 ) then
         iflux2link(j) = Lf
      else  ! closed boundary
         iflux2link(j) = 0
      end if
   end do
   
   return
end subroutine comp_fluxcoeffs

!> find common value of two-dimensional arrays i1 and i2
!>   it is assumed there is at most one common value
integer function icommonval(i1, i2)
   implicit none
   
   integer, dimension(2), intent(in)  :: i1
   integer, dimension(2), intent(in)  :: i2
   
   icommonval = 0
   if ( i1(1).eq.i2(1) .or. i1(1).eq.i2(2) ) then
      icommonval = i1(1)
   else if ( i1(2).eq.i2(1) .or. i1(2).eq.i2(2) ) then
      icommonval = i1(2)
   end if
   
   return
end function icommonval

!> plot mesh admin
subroutine tekpartmesh()
   use m_particles
   use m_partmesh
   use m_partrecons
   use m_wearelt
   use unstruc_display
   implicit none
   
   character(len=32) :: text
   
   double precision :: xL, yL, xL1, yL1
   
   integer          :: i, k, k1, k2, L
   
   if ( japart.ne.1 .or. ndrawpart.eq.1 ) return
   
!  edges   
   do L=1,numedges
      k1 = edge2node(1,L)
      k2 = edge2node(2,L)
      
      xL = 0.5d0*(xnode(k1)+xnode(k2))
      yL = 0.5d0*(ynode(k1)+ynode(k2))
      xL1 = xL + rcir*dnx(L)
      yL1 = yL + rcir*dny(L)
      
      call movabs(xnode(k1),ynode(k1))
      call lnabs(xnode(k2),ynode(k2))
      
      call movabs(xL,yL)
      call lnabs(xL1,yl1)
!      call hitext(L,xL1,yL1)
      write(text,"(I0, '(', F0.1, ')')") L, qe(L)
      call drawtext(real(xL1),real(yL1),trim(text))
   end do
   
!  cells
   do k=1,numcells
      call hitext(k,xzwcell(k),yzwcell(k))
   end do
   
!  particle cell numbers
   do i=1,Npart
!      call hitext(i,xpart(i)+rcir,ypart(i))
!      call hitext(kpart(i),xpart(i)+rcir,ypart(i))
      write(text,"(I0, '(', I0, ')')") i, kpart(i)
      call drawtext(real(xpart(i)+rcir),real(ypart(i)),trim(text))
   end do
   
   return
end subroutine tekpartmesh

logical function get_japart()
   use m_particles
   implicit none
   
   get_japart = ( japart.eq.1 )
   
   return
end function get_japart

!> add particles
subroutine add_particles(Nadd, xadd, yadd, jareplace)
   use m_particles
   use m_alloc
   use m_wearelt
   implicit none
   
   integer,                           intent(in)  :: Nadd       !< number of particles to be added
   double precision, dimension(Nadd), intent(in)  :: xadd       !< x-coordinates of particles to be added
   double precision, dimension(Nadd), intent(in)  :: yadd       !< y-coordinates of particles to be added
   integer,                           intent(in)  :: jareplace  !< replace existing but disabled particles(1) or not (0)
   
   integer,          dimension(:),    allocatable :: kadd   !< cell numbers
   
   integer                                        :: i, ipoint, Nsize
   integer                                        :: ierror
   integer                                        :: Npartnew
   integer                                        :: Nreplace
   
   if ( japart.ne.1 ) return
   
!  get number of existing particles that may be replaced
   if ( jareplace.eq.1 ) then
      Nreplace = 0
      do i=1,Npart
         if ( kpart(i).eq.0 ) then
            Nreplace = Nreplace+1
         end if
      end do
   else
      Nreplace = 0
   end if
   
!  new number of particles
   Npartnew = Npart + max(Nadd-Nreplace,0)
   
!  get current array sizes
   if ( allocated(xpart) ) then
      Nsize = size(xpart)
   else
      Nsize = 0
   end if
   
!  realloc
   if ( Npartnew.gt.Nsize ) then
!     compute new array size   
      Nsize = int(1.2*dble(Npartnew)) + 1
      call realloc_particles(Nsize, .true., ierror)
   end if
   
!  get new particle cell number
   allocate(kadd(Nadd))
   kadd = 0
   call part_findcell(Nadd,xadd,yadd,kadd,ierror)
   
!  fill data
   if ( jareplace.eq.1 ) then
      ipoint = 1
   else
      ipoint = Npart+1
   end if

   do i=1,Nadd
      if ( ipoint.le.Npart ) then
         do while ( kpart(ipoint).ne.0 )
            ipoint = ipoint+1
         end do
      end if
      
      xpart(ipoint) = xadd(i)
      ypart(ipoint) = yadd(i)
      kpart(ipoint) = kadd(i)
!      write(namepart(ipoint), "('added_particle ', I0)") i
      Npart = Npart+1
      
!     plot      
      call setcol(31)
      call movabs(xpart(ipoint),ypart(ipoint))
      call cir(rcir)
      
!     advance pointer
      ipoint = ipoint+1
   end do
   
!  deallocate
   if ( allocated(kadd) ) deallocate(kadd)
   
   return
end subroutine add_particles



!> find in which cells particles are located
subroutine part_findcell(Npart, xpart, ypart, kpart, ierror)
   use m_partmesh
   use unstruc_messages
   use m_kdtree2
   implicit none
   
   integer,                            intent(in)  :: Npart    !< number of particles
   double precision, dimension(Npart), intent(in)  :: xpart    !< particle x-coordinates
   double precision, dimension(Npart), intent(in)  :: ypart    !< particle x-coordinates
   integer,          dimension(Npart), intent(out) :: kpart    !< cell numbers
                                                   
   integer                           , intent(out) :: ierror   !< error (1) or not (0)
   
   type(kdtree_instance)                           :: kdtree
   
   double precision, dimension(3)                  :: xv, yv
   
   double precision                                :: dmaxsize
   double precision                                :: R2search
   
   integer                                         :: i, ip1, j, k, knode, L, Lp1, N, NN
   integer                                         :: inside
   
   double precision, external                      :: dbdistance
   
   ierror = 1
   
!  build kdtree   
   call build_kdtree(kdtree, Npart, xpart, ypart, ierror)
   if ( ierror.ne.0 ) then
      goto 1234
   end if
   
   if ( Npart.lt.1 ) then
      ierror = 0
      goto 1234
   end if
   
   kpart = 0
   
!  loop over cells
   do k=1,numcells
!     check cell size
      N = jcell2edge(k+1)-jcell2edge(k)
      if ( N.ne.3 ) then
         call mess(LEVEL_ERROR, 'part_findcell: non-triangle')
         goto 1234
      end if
      
!     get cell polygon
      i=0
      do j = jcell2edge(k),jcell2edge(k+1)-1
         i = i+1
         L = icell2edge(j)
         ip1 = i+1; if ( ip1.gt.3 ) ip1=ip1-3
         Lp1 = icell2edge(j-i+ip1)
!        find common node of L and Lp1
         if ( edge2node(1,L).eq.edge2node(1,Lp1) .or. edge2node(1,L).eq.edge2node(2,Lp1) ) then
            knode = edge2node(1,L)
         else if ( edge2node(2,L).eq.edge2node(1,Lp1) .or. edge2node(2,L).eq.edge2node(2,Lp1) ) then
            knode = edge2node(2,L)
         else  ! should not happen
            continue
         end if
         xv(i) = xnode(knode)
         yv(i) = ynode(knode)
      end do
      
!     fill query vector
      call make_queryvector_kdtree(kdtree,xzwcell(k),yzwcell(k))
      
!     compute maximum flowcell dimension
      dmaxsize = 0d0
      do i=1,N
         ip1=i+1; if ( ip1.gt.N ) ip1=ip1-N
         dmaxsize = max(dmaxsize, dbdistance(xv(i),yv(i),xv(ip1),yv(ip1)))
      end do
      
!     determine square search radius
      R2search = 1.1d0*dmaxsize**2  ! 1.1d0: safety

!     count number of points in search area
      NN = kdtree2_r_count(kdtree%tree,kdtree%qv,R2search)

      if ( NN.eq.0 ) cycle ! no particles found
      
!     reallocate if necessary
      call realloc_results_kdtree(kdtree,NN)
     
!     find nearest NN samples
      call kdtree2_n_nearest(kdtree%tree,kdtree%qv,NN,kdtree%results)

!     check if samples are in cell
      do i=1,NN
         j = kdtree%results(i)%idx
         call pinpok(xpart(j), ypart(j), 3, xv, yv, inside)
         
         if ( inside.eq.1 ) then
            if ( kpart(j).eq.0 ) then
               kpart(j) = k
            end if
         end if
      end do
   end do
   
   ierror = 0

1234 continue
   
!  deallocate
   if ( kdtree%itreestat.ne.ITREE_EMPTY ) call delete_kdtree2(kdtree)
   
   return
end subroutine part_findcell

!> copy samples to particles
subroutine copy_sam2part()
   use m_particles
   use m_samples
   implicit none

   character(len=255) :: dum
   
   integer :: i
   integer :: ierror
   
   if ( NS.lt.1 ) return
   
   if ( japart.ne.1 ) then
      dum = ' '
      call ini_part(0, dum, 0,0d0,0d0)
   end if
   
   call add_particles(Ns, xs, ys, 0)
   
   call delsam(0)
   
end subroutine copy_sam2part


!> (re)allocate
subroutine realloc_particles(Nsize, LkeepExisting, ierror)
   use m_particles
   use m_alloc
   use m_missing
   implicit none
   
   integer, intent(in)  :: Nsize          !< array sizes
   logical, intent(in)  :: LkeepExisting  !< keep existing data (1) or not (0)
   integer, intent(out) :: ierror         !< error (1) or not
   
   ierror = 1
   
!  reallocate   
   call realloc(xpart, Nsize, keepExisting=LkeepExisting, fill=DMISS)
   call realloc(ypart, Nsize, keepExisting=LkeepExisting, fill=DMISS)
   call realloc(dtremaining, Nsize, keepExisting=LkeepExisting, fill=0d0)
   call realloc(kpart, Nsize, keepExisting=LkeepExisting, fill=0)
!   call realloc(Lpart, Nsize, keepExisting=LkeepExisting, fill=0)
!   call realloc(namepart, Nsize, keepExisting=LkeepExisting, fill='')
   
   call realloc(numzero, Nsize, keepExisting=LkeepExisting, fill=0)
   numzero = 0
   
   ierror = 0
1234 continue
   
   return
end subroutine realloc_particles

!> deallocate particle data
subroutine dealloc_particles()
   use m_particles
   implicit none
   
   if ( allocated(xpart)       ) deallocate(xpart)
   if ( allocated(ypart)       ) deallocate(ypart)
   if ( allocated(dtremaining) ) deallocate(dtremaining)
   if ( allocated(kpart)       ) deallocate(kpart)
!   if ( allocated(Lpart)       ) deallocate(Lpart)
!   if ( allocated(namepart)    ) deallocate(namepart)
   
   if ( allocated(numzero)     ) deallocate(numzero)
   
   Npart = 0
   
   return
end subroutine dealloc_particles


!> (re)allocate partmesh data
subroutine realloc_partmesh()
   use m_partmesh
   use m_alloc
   use m_missing
   implicit none
   
   integer :: N
   
   call realloc(edge2node, (/2, numedges/), fill=0, keepExisting=.false.)
   call realloc(edge2cell, (/2, numedges/), fill=0, keepExisting=.false.)
   call realloc(xnode, numnodes, fill=0d0, keepExisting=.false.)
   call realloc(ynode, numnodes, fill=0d0, keepExisting=.false.)
   
   call realloc(xzwcell, numcells, fill=DMISS, keepExisting=.false.)
   call realloc(yzwcell, numcells, fill=DMISS, keepExisting=.false.)
   call realloc(areacell, numcells, fill=DMISS, keepExisting=.false.)
   
   call realloc(dnx, numedges, fill=DMISS, keepExisting=.false.)
   call realloc(dny, numedges, fill=DMISS, keepExisting=.false.)
   call realloc(w, numedges, fill=DMISS, keepExisting=.false.)
   
   call realloc(edge2link, numedges, fill=0, keepExisting=.false.)
!   call realloc(nod2cell, numcells, fill=0, keepExisting=.false.)
   call realloc(cell2nod, numcells, fill=0, keepExisting=.false.)
   
   call realloc(jcell2edge, numcells+1, fill=1, keepExisting=.false.)
   N = jcell2edge(numcells+1)-1
   call realloc(icell2edge, N, fill=0, keepExisting=.false.)
   
   return
end subroutine realloc_partmesh

!> deallocate particle mesh data
subroutine dealloc_partmesh()
   use m_partmesh
   implicit none
   
   if ( allocated(edge2node ) ) deallocate(edge2node )
   if ( allocated(edge2cell ) ) deallocate(edge2cell )
   if ( allocated(xnode     ) ) deallocate(xnode     )
   if ( allocated(ynode     ) ) deallocate(ynode     )
   
   if ( allocated(xzwcell   ) ) deallocate(xzwcell   )
   if ( allocated(yzwcell   ) ) deallocate(yzwcell   )
   if ( allocated(areacell  ) ) deallocate(areacell  )
   
   if ( allocated(dnx       ) ) deallocate(dnx       )
   if ( allocated(dny       ) ) deallocate(dny       )
   if ( allocated(w         ) ) deallocate(w         )
   
   if ( allocated(edge2link ) ) deallocate(edge2link )
!  if ( allocated(nod2cell  ) ) deallocate(nod2cell  )
   if ( allocated(cell2nod  ) ) deallocate(cell2nod  )
   
   if ( allocated(icell2edge) ) deallocate(icell2edge)
   if ( allocated(jcell2edge) ) deallocate(jcell2edge)
   
   
   return
end subroutine dealloc_partmesh


!> (re)allocate flux coefficients
subroutine realloc_partfluxes()
   use m_partmesh
   use m_partfluxes
   use m_alloc
   use m_missing
   implicit none
   
   integer :: N
   
   call realloc(jflux2link, numedges+1, keepExisting=.false., fill=1)
   N = jflux2link(numedges+1)-1
   call realloc(iflux2link, N,  keepExisting=.false., fill=0)
   call realloc(Aflux2link, N,  keepExisting=.false., fill=0d0)
   
   return
end subroutine realloc_partfluxes

!> deallocate flux_coeffs
subroutine dealloc_partfluxes()
   use m_partfluxes
   implicit none
   
   if ( allocated(iflux2link) ) deallocate(iflux2link)
   if ( allocated(jflux2link) ) deallocate(jflux2link)
   if ( allocated(Aflux2link) ) deallocate(Aflux2link)
end subroutine dealloc_partfluxes

!> (re)allocate flux coefficients et cetera
subroutine realloc_partrecons()
   use m_partmesh
   use m_partrecons
   use m_alloc
   use m_missing
   implicit none
   
   call realloc(qe, numedges, keepExisting=.false., fill=DMISS)
   
   call realloc(u0x, numcells, keepExisting=.false., fill=DMISS)
   call realloc(u0y, numcells, keepExisting=.false., fill=DMISS)
   call realloc(alpha, numcells, keepExisting=.false., fill=DMISS)
   return
end subroutine realloc_partrecons

!> deallocate flux_coeffs
subroutine dealloc_partrecons()
   use m_partrecons
   implicit none
   
   if ( allocated(qe) ) deallocate(qe)
   
   if ( allocated(u0x) ) deallocate(u0x)
   if ( allocated(u0y) ) deallocate(u0y)
   if ( allocated(alpha) ) deallocate(alpha)
   
   return
end subroutine dealloc_partrecons

!> initialize particles
subroutine ini_part(japartfile, partfile, jatracer_loc, starttime_loc, timestep_loc)
   use m_particles
   use m_samples
   use m_flow, only: s1
   use m_transport, only: constituents, numconst
   use m_flowtimes, only: time1
   use m_missing
   implicit none
   
   integer,            intent(in) :: japartfile    !< use particle file (1) or not (0)
   character(len=255), intent(in) :: partfile      !< particle file
   integer,            intent(in) :: jatracer_loc  !< add tracer (1) or not (0)
   double precision,   intent(in) :: starttime_loc !< start time (>0) or not (0)
   double precision,   intent(in) :: timestep_loc  !< time step (>0) or every computational time step (0)
   
   integer             :: minp
   logical             :: lexist
   integer             :: iconst
   integer             :: ierror
   
!  deallocate
   call dealloc_partmesh()
   call dealloc_partfluxes()
   call dealloc_partrecons()
   call dealloc_particles()
   call dealloc_summedfluxes()
   
   timenext = 0d0
   timelast = DMISS
      
!  add particle tracer (when tracers are initialized)
   if ( jatracer_loc.eq.1 ) then
      jatracer = 1
   end if
   
!  start time
   if ( starttime_loc.gt.0d0 ) then
      starttime = starttime_loc
      timenext = starttime
   end if
   
!  time step
   if ( timestep_loc.gt.0d0 ) then
      timestep = timestep_loc
   end if
   
   if ( japartfile.eq.1 ) then
      japart = 0
      if ( len_trim(partfile).gt.0 ) then
   !     read initial samples from inputfile  
         inquire(FILE = trim(partfile), exist = lexist)
         if ( lexist ) then
            call oldfil(minp, partfile)
            call savesam()
            call reasam(minp, 0)
            japart = 1
         end if
      end if
   else  ! initialize only
      japart = 1
   end if
   
   if ( japart.eq.1 ) then
!     set pointers with mesh connectivity etc.
      call part_setmesh()
      
!     set flux coeffecients            
      call comp_fluxcoeffs()
      
      call realloc_partrecons()
      
      if ( Ns.gt.0 ) then
         call add_particles(Ns, xs, ys, 0)
         
         call delsam(0)
      else
         Npart = 0
      end if
      
      if ( jatracer.eq.1 ) then
!        REMARK: tracer with particle concentration is ALSO updated by transport module (not necessary)      
         call add_tracer(PART_TRACERNAME, part_iconst)
!        compute concentration (overwrite update by transport module)
         call comp_concentration(s1,numconst,part_iconst,constituents)
      end if
   
      if ( timestep.gt.0d0 ) then
         call alloc_summedfluxes()
      end if
   end if
   
   return
end subroutine ini_part

!> compute concentrations of particles (parts per unit volume) in flownodes
subroutine comp_concentration(s, nconst, iconst, c)
   use m_particles
   use m_partmesh
   use m_flowgeom, only : Ndx, ba, bl
   implicit none
   
   double precision, dimension(Ndx),        intent(in)  :: s      !< water level
   integer,                                 intent(in)  :: nconst !< number of constituents
   integer,                                 intent(in)  :: iconst !< particle tracer constituent number
   double precision, dimension(Nconst,Ndx), intent(out) :: c      !< constituents
   
   integer :: i, k
   
   do i=1,Ndx
      c(iconst,i) = 0d0
   end do
   
!  count number of particles per cell   
   do i=1,Npart
      k = kpart(i)
      if ( k.eq.0 ) cycle
      
      k = iabs(cell2nod(k))
      
      c(iconst,k) = c(iconst,k) + 1
   end do
   
!  compute depth concentration (parts per unit volume)
   do k=1,Ndx
      c(iconst,k) = c(iconst,k) / (ba(k)*(s(k)-bl(k)))
   end do
   
   return
end subroutine comp_concentration

!> allocate summed fluxes
subroutine alloc_summedfluxes()
   use m_particles
   use m_flowgeom, only: Ndx, Lnx
   use m_alloc
   implicit none
   
   call realloc(sbegin, Ndx, fill=0d0, keepExisting=.false.)
   call realloc(qpart, Lnx, fill=0d0, keepExisting=.false.)
   
   return
end subroutine alloc_summedfluxes

!> deallocate summed fluxes
subroutine dealloc_summedfluxes()
   use m_particles
   implicit none
   
   if ( allocated(sbegin) ) deallocate(sbegin)
   if ( allocated(qpart)  ) deallocate(qpart)
   
   return
end subroutine dealloc_summedfluxes

subroutine part_sumfluxes()
   use m_particles
   use m_partmesh
   use m_flowgeom, only: Lnx
   use m_flow, only: q1
   use m_flowtimes, only: dts
   implicit none
   
   integer :: L
   
   do L=1,Lnx
      qpart(L) = qpart(L) + q1(L)*dts
   end do
   
   return
end subroutine part_sumfluxes

!> update particles or add to summed fluxes
subroutine update_part()
   use m_particles
   use m_flowtimes
   use m_flow
   use m_missing
   implicit none
   
   if ( japart.ne.1 ) return

   if ( time0.ge.starttime ) then
   
      if ( timestep.le.0d0 ) then   ! update particles every computational time step
         call update_particles(q1,s0,s1,dts)
      else
      
!        check if timestep has been started
         if ( timelast.eq.DMISS ) then
!           start particle timestep
            timelast = time0
            timenext = time0+timestep
            sbegin = s0
            qpart = 0d0
         end if
         
!        sum fluxes of this computational time step         
         call part_sumfluxes()
         
         if ( time1.ge.timenext ) then
!           finish particle timestep         
            qpart = qpart/(time1-timelast)
            call update_particles(qpart, sbegin, s1, time1-timelast)
            
!           start new particle timestep
            timelast = time1
            timenext = time1 + timestep
            sbegin = s1
            qpart = 0d0
         end if
      end if
   end if
   
   return
end subroutine update_part