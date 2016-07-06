!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id$
!  $HeadURL$
!!--description-----------------------------------------------------------------
! This module prepares the structured grid into an unstructured data structure
!   and writes it to a UGRID compliant NetCDF file    
!!--pseudo code and references--------------------------------------------------
! Dependencies:
!   io_netcdf module to write to UGRID compliant NetCDF file    
!!--declarations----------------------------------------------------------------
module m_write_waqgeom_curvilinear
    implicit none
    integer, parameter   :: missing_value = -999
    type :: edge_t
        integer               :: type    = 0 !< Can be UNC_EDGETYPE_INTERNAL_CLOSED, UNC_EDGETYPE_INTERNAL, UNC_EDGETYPE_BND or UNC_EDGETYPE_BND_CLOSED
        integer               :: nmelm   = 0 !< Count of neighbouring elements (0, 1, or 2)
        integer, dimension(2) :: vertex  = missing_value !< an edge is a straight line between two vertices
        integer, dimension(2) :: element = missing_value !< an edge separates zero, one or two elements; their indices are stored here
    end type
    type :: element_t
        integer                        :: nvertex = 0       !< number of vertices for this element
        integer, dimension(:), pointer :: vertex  => null() !< 
        integer, dimension(:), pointer :: edge    => null() !< 
    end type
    type :: waq_polygon_t
        integer                        :: nvertex
        integer, dimension(:), pointer :: vertex  => null()
        integer, dimension(:), pointer :: edge !< edge number
    end type waq_polygon_t
contains    
    subroutine wrwaqgeomcl ( meta     , lundia, nmax   , mmax   , kmax   , & 
                             flow_kmax, nlb   , nub    , mlb    , mub    , &
                             xcor     , ycor  , xz     , yz     , dep    , &
                             kcs      , kcu   , kcv    , sferic , aggre  , &
                             isaggr   , nto   , nambnd , mnbnd)
    use precision

    use netcdf
    use io_ugrid
    use m_aggregate_waqgeom
!      
    implicit none
!
!   Global variables
!
    type(ug_meta), intent(in) :: meta
    integer   , intent(in) :: lundia
    integer(4), intent(in) :: nmax       !!  Dimension of first index in 2d arrays
    integer(4), intent(in) :: mmax       !!  Dimension of second index in 2d arrays
    integer(4), intent(in) :: kmax       !!  number of waq layers
    integer(4), intent(in) :: flow_kmax  !!  number of flow layers
    integer(4), intent(in) :: nlb        !!  Lower bound of all n dimensions
    integer(4), intent(in) :: nub        !!  Upper bound of all n dimensions
    integer(4), intent(in) :: mlb        !!  Lower bound of all m dimensions
    integer(4), intent(in) :: mub        !!  Upper bound of all m dimensions
    integer   , intent(in) :: nto        !!  Number of open boundaries (tidal openings)
    real(fp), dimension(nlb:nub,mlb:mub), intent(in) :: xcor !!  Array with x-values corners
    real(fp), dimension(nlb:nub,mlb:mub), intent(in) :: ycor !!  Array with y-values corners
    real(fp), dimension(nlb:nub,mlb:mub), intent(in) :: xz   !!  Array with x-values zeta point
    real(fp), dimension(nlb:nub,mlb:mub), intent(in) :: yz   !!  Array with y-values zeta point
    real(fp), dimension(nlb:nub,mlb:mub), intent(in) :: dep  !!  Array with depth-values at corners
    integer , dimension(nlb:nub,mlb:mub), intent(in) :: kcs  !!   
    integer , dimension(nlb:nub,mlb:mub), intent(in) :: kcu  !!  
    integer , dimension(nlb:nub,mlb:mub), intent(in) :: kcv  !!  
    logical, intent(in) :: sferic
    integer, intent(in) :: aggre                             !! INPUT   0 means no-aggregation active cells only
                                                             !<         1 means aggregation
    integer, intent(in) :: isaggr(nmax*mmax*kmax)      !!  grid aggregation pointer
    !                                                        
    character(20), intent(in) :: nambnd(  nto)               !!  names of the open boundaries
    integer      , intent(in) :: mnbnd(7,nto)                !!  indices of the open boundaries

!   ugrid types
    type(t_crs), target   :: crs
    type(t_ug_meshids)    :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
    type(t_ug_meshgeom)   :: meshgeom !< Mesh geometry to be written to the NetCDF file.
    type(t_ug_meshgeom)   :: aggregated_meshgeom !< Mesh geometry to be written to the NetCDF file.

!
!           Local variables
!
    integer i, j                    !!  loop counters
    integer, external :: newunit

    integer :: ierr
    character(len=256) :: filename
    integer :: inode, master
    integer :: igeomfile
    integer :: max_vertex
    integer :: nr_nodes, nr_edges
    integer :: nr_flowlinks
    integer :: m, n
    real(fp) :: xymiss
    
    real(hp), dimension(:), pointer :: xn, yn, zn ! coordinates of nodes
    real(hp), dimension(:), pointer :: xe, ye ! coordinates of the middle of the edge (ie u-punt)
    real(hp), dimension(:), pointer :: xf, yf ! coordinates of the mass centre of the elements
    real(hp), dimension(:,:,:), pointer :: open_bnd
    integer , dimension(:,:), pointer :: seg_nr
    integer , dimension(:), pointer :: node_mask, flow_vol, edge_mask, node_tmp
    integer , dimension(:), pointer :: nr_bnd_cells
    integer , dimension(:,:), pointer :: netlink, netelem
    integer , dimension(:,:), pointer :: flowlink, elemlink
    integer , dimension(:), pointer :: iapnt !< Aggregation pointer
    integer , dimension(:), pointer :: edge_type(:) !< Edge type variable to be written to the NetCDF file.
    integer , dimension(:), pointer :: aggregated_edge_type(:) !< Aggregated edge type variable to be written to the NetCDF file.
    integer :: elm_i, elm1, elm2, i1, j1, k
    integer :: nr_elems, nr_bnd_elm, cellindex, pointindex
    integer :: lunbnd, md, nd, mu, nu, m_dir, n_dir, total_bnd_cells
    integer :: max_bnd_cells
    logical :: found

    type(edge_t), dimension(:), pointer :: edge => null()
    type(waq_polygon_t), dimension(:), pointer  :: waq_polygon
!
!! executable statements -------------------------------------------------------
!
    allocate(waq_polygon(mmax*nmax)) ! maximum number of aggregated cells, reached when there is no aggregation
    allocate(node_mask((nmax-1)*(mmax-1)))
    allocate(node_tmp((nmax-1)*(mmax-1)))
    allocate(flow_vol(mmax*nmax))
    allocate(edge_mask(mmax*nmax))
    
    node_tmp = 0
    flow_vol = 0 
    edge_mask = 0

    max_vertex = 0
    nr_edges = 0
    nr_bnd_elm = 0

    nr_elems = 0
    do m = 1, mmax
        do n = 1, nmax
            if (kcs(n, m) == 1) then
                if (kcu(n  ,m-1)==0 .and. kcu(n  ,m  )==0 .and. &
                    kcv(n-1,m  )==0 .and. kcv(n  ,m  )==0) then ! do not count active cells defined with four thin dams
                else
                    ! Valid cell found
                    nr_elems = nr_elems + 1
                    cellindex = func(m, n, nmax)
                    flow_vol(cellindex) = nr_elems
                end if
            end if
        end do
    end do        
     
    nr_nodes = 0
    node_mask = 0
    xymiss = 0.0
    do m = 1, mmax-1
        do n = 1, nmax-1
            if (kcs(n,m  )==1 .or. kcs(n+1,m  )==1 .or. &
                kcs(n,m+1)==1 .or. kcs(n+1,m+1)==1) then
                ! Valid point found
                nr_nodes = nr_nodes + 1
                pointindex = func(m, n, nmax-1)
                node_mask(pointindex) = nr_nodes
            end if
        end do
    end do    
    !
    ! Determine the waq-polygons, not yet aggregated
    !
    call determine_elem_polygons(node_mask, mmax, nmax, flow_vol, waq_polygon, edge, nr_elems, nr_edges, max_vertex)
    !
    allocate(netelem    (4, nr_elems)) ! needed to write nc-file
    netelem = 0
    elm_i = 0
    do m = 2, mmax-1 ! outer columns have kcs==0
        do n = 2, nmax-1 ! outer rows have kcs==0
            if (kcs(n, m) == 1) then
                if (kcu(n  ,m-1)==0 .and. kcu(n  ,m  )==0 .and. &
                    kcv(n-1,m  )==0 .and. kcv(n  ,m  )==0) then ! do not count active cells defined with four thin dams
                else
                    ! Valid cell found
                    elm_i = elm_i + 1
                    netelem(1, elm_i) = node_mask(func(m-1, n-1, nmax-1))
                    netelem(2, elm_i) = node_mask(func(m  , n-1, nmax-1))
                    netelem(3, elm_i) = node_mask(func(m  , n  , nmax-1))
                    netelem(4, elm_i) = node_mask(func(m-1, n  , nmax-1))
                end if
            end if
        end do
    end do        
    !
    ! NetNode
    !
    allocate(xn(nr_nodes))
    allocate(yn(nr_nodes))
    allocate(zn(nr_nodes))
    nr_nodes = 0
    do m = 1, mmax-1
        do n = 1, nmax-1
            if (kcs(n,m  )==1 .or. kcs(n+1,m  )==1 .or. &
                kcs(n,m+1)==1 .or. kcs(n+1,m+1)==1) then
                !Valid point found
                nr_nodes = nr_nodes + 1
                xn(nr_nodes) = xcor(n,m)
                yn(nr_nodes) = ycor(n,m)
                zn(nr_nodes) =-dep (n,m)
            end if
        end do
    end do    
    !
    ! Netlink
    !
    allocate(netlink(2, nr_edges))
    allocate(elemlink(2, nr_edges)) ! adjacent cells
    allocate(edge_type(nr_edges)) 
    netlink = missing_value
    elemlink = missing_value

    do i = 1, nr_edges
         netlink(1,i) = edge(i)%vertex(1)
         netlink(2,i) = edge(i)%vertex(2)
         elemlink(1, i) = edge(i)%element(1)
         elemlink(2, i) = edge(i)%element(2)
         edge_type(i) = edge(i)%type
    end do
    !
    ! FlowLink
    !
    if (nr_bnd_elm > 0) then
        !  add nr_elems to the virtual (negative) boundary cells number 
        do i = 1, mmax*nmax ! size(cell_mask)
            if (flow_vol(i) < 0) then
                flow_vol(i) = abs(flow_vol(i)) + nr_elems
            endif 
        end do 
    end if
    allocate(flowlink(2, 2*mmax*nmax)) ! u- and v-flowlinks
    allocate(xe(2*mmax*nmax))
    allocate(ye(2*mmax*nmax))
    flowlink = 0
    nr_flowlinks = 0
    do m = 1, mmax-1
        do n = 1, nmax-1
            ! u-flowlink
            elm1 = func(m  ,n  , nmax)
            elm2 = func(m+1,n  , nmax)
            elm1 = flow_vol(elm1)
            elm2 = flow_vol(elm2)
            if (elm1 /= elm2) then
                if (elm1<0) elm1 = abs(elm1) + nr_elems
                if (elm2<0) elm2 = abs(elm2) + nr_elems
                if (kcu(n,m) == 1 ) then
                    nr_flowlinks = nr_flowlinks + 1
                    flowlink (1, nr_flowlinks) = elm1
                    flowlink (2, nr_flowlinks) = elm2
                    xe(nr_flowlinks) = 0.5*(xcor(n-1,m) + xcor(n,m))
                    ye(nr_flowlinks) = 0.5*(ycor(n-1,m) + ycor(n,m))
                endif
            endif
            ! v-flowlink
            elm1 = func(m  ,n  , nmax)
            elm2 = func(m  ,n+1, nmax)
            elm1 = flow_vol(elm1)
            elm2 = flow_vol(elm2)
            if (elm1 /= elm2) then
                if (elm1<0) elm1 = abs(elm1) + nr_elems
                if (elm2<0) elm2 = abs(elm2) + nr_elems
                if (kcv(n, m) == 1) then
                    nr_flowlinks = nr_flowlinks + 1
                    flowlink (1, nr_flowlinks) = elm1
                    flowlink (2, nr_flowlinks) = elm2
                    xe(nr_flowlinks) = 0.5*(xcor(n,m-1) + xcor(n,m))
                    ye(nr_flowlinks) = 0.5*(ycor(n,m-1) + ycor(n,m))
                end if
            end if
        end do
    end do    
    !
    ! Mass centre of elements
    !
    allocate(xf(nr_elems))
    allocate(yf(nr_elems))
    elm_i = 0
    do m = 2, mmax-1 ! outer columns have kcs==0
        do n = 2, nmax-1 ! outer rows have kcs==0
            if (kcs(n, m) == 1) then
                if (kcu(n  ,m-1)==0 .and. kcu(n  ,m  )==0 .and. &
                    kcv(n-1,m  )==0 .and. kcv(n  ,m  )==0) then ! do not count active cells defined with four thin dams
                else
                    ! Valid cell found
                    elm_i = elm_i + 1
                    xf(elm_i) = xz(n,m)
                    yf(elm_i) = yz(n,m)
                end if
            end if
        end do
    end do            
!   
! Write the boundary file
! 
    total_bnd_cells = 0
    max_bnd_cells = 0
    allocate(nr_bnd_cells(nto))
    do i = 1, nto
        m_dir = max(mnbnd(1,i),mnbnd(3,i)) - min(mnbnd(1,i),mnbnd(3,i)) + 1
        n_dir = max(mnbnd(4,i),mnbnd(4,i)) - min(mnbnd(2,i),mnbnd(2,i)) + 1
        nr_bnd_cells(i) = max(m_dir, n_dir)
        total_bnd_cells = total_bnd_cells + max(m_dir, n_dir)
        max_bnd_cells = max(max_bnd_cells, max(m_dir, n_dir))
    end do
    allocate(open_bnd(4,nto,max_bnd_cells))
    allocate(seg_nr(max_bnd_cells, nto))
    open_bnd = 0
    seg_nr = 0
    nr_bnd_cells = 0
    do m = 1,  mmax
        do n = 1, nmax
            if (kcs(n, m) == 2) then ! boundary element
                found = .false.
                nr_bnd_elm = nr_bnd_elm + 1
                cellindex = func(m, n, nmax)
                flow_vol(cellindex) = -nr_bnd_elm
                md = max(1   ,m-1)
                nd = max(1   ,n-1)
                mu = min(mmax,m+1)
                nu = min(nmax,n+1)
                if (kcs(nd,m) == 1) then
                   ! which boundary: upper boundary
          nto_loop1: do i = 1, nto
                        do i1 = min(mnbnd(1,i),mnbnd(3,i)), max(mnbnd(1,i),mnbnd(3,i))
                            do j1 = min(mnbnd(2,i),mnbnd(4,i)),  max(mnbnd(2,i),mnbnd(4,i))
                                if (m == i1 .and. n == j1) then
                                    found = .true.
                                    exit nto_loop1
                                end if
                            end do
                        end do
                    end do nto_loop1
                    if (found) then
                        nr_bnd_cells(i) = nr_bnd_cells(i)+1
                        seg_nr(nr_bnd_cells(i),i) = flow_vol(cellindex)
                        open_bnd(1, i, nr_bnd_cells(i)) = xcor(n-1,m-1)
                        open_bnd(2, i, nr_bnd_cells(i)) = ycor(n-1,m-1)
                        open_bnd(3 ,i, nr_bnd_cells(i)) = xcor(n-1,m  )
                        open_bnd(4, i, nr_bnd_cells(i)) = ycor(n-1,m  )
                    endif
                else if (kcs(nu,m  ) == 1) then
                   ! which boundary: lower boundary
          nto_loop2: do i = 1, nto
                        do i1 = min(mnbnd(1,i),mnbnd(3,i)), max(mnbnd(1,i),mnbnd(3,i))
                            do j1 = min(mnbnd(2,i),mnbnd(4,i)),  max(mnbnd(2,i),mnbnd(4,i))
                                if (m == i1 .and. n == j1) then
                                    found = .true.
                                    exit nto_loop2
                                end if
                            end do
                        end do
                    end do nto_loop2
                    if (found) then
                        nr_bnd_cells(i) = nr_bnd_cells(i)+1
                        seg_nr(nr_bnd_cells(i),i) = flow_vol(cellindex)
                        open_bnd(1, i, nr_bnd_cells(i)) = xcor(n  ,m-1)
                        open_bnd(2, i, nr_bnd_cells(i)) = ycor(n  ,m-1)
                        open_bnd(3 ,i, nr_bnd_cells(i)) = xcor(n  ,m  )
                        open_bnd(4, i, nr_bnd_cells(i)) = ycor(n  ,m  )
                    endif
                else if (kcs(n  ,md) == 1) then
                   ! which boundary: right boundary
          nto_loop3: do i = 1, nto
                        do i1 = min(mnbnd(1,i),mnbnd(3,i)), max(mnbnd(1,i),mnbnd(3,i))
                            do j1 = min(mnbnd(2,i),mnbnd(4,i)),  max(mnbnd(2,i),mnbnd(4,i))
                                if (m == i1 .and. n == j1) then
                                    found = .true.
                                    exit nto_loop3
                                end if
                            end do
                        end do
                    end do nto_loop3
                    if (found) then
                        nr_bnd_cells(i) = nr_bnd_cells(i)+1
                        seg_nr(nr_bnd_cells(i),i) = flow_vol(cellindex)
                        open_bnd(1, i, nr_bnd_cells(i)) = xcor(n-1,m-1)
                        open_bnd(2, i, nr_bnd_cells(i)) = ycor(n-1,m-1)
                        open_bnd(3 ,i, nr_bnd_cells(i)) = xcor(n  ,m-1)
                        open_bnd(4, i, nr_bnd_cells(i)) = ycor(n  ,m-1)
                    endif
                else if (kcs(n  ,mu) == 1) then
                   ! which boundary: left boundary
          nto_loop4: do i = 1, nto
                        do i1 = min(mnbnd(1,i),mnbnd(3,i)), max(mnbnd(1,i),mnbnd(3,i))
                            do j1 = min(mnbnd(2,i),mnbnd(4,i)),  max(mnbnd(2,i),mnbnd(4,i))
                                if (m == i1 .and. n == j1) then
                                    found = .true.
                                    exit nto_loop4
                                end if
                            end do
                        end do
                    end do nto_loop4
                    if (found) then
                        nr_bnd_cells(i) = nr_bnd_cells(i)+1
                        seg_nr(nr_bnd_cells(i),i) = flow_vol(cellindex)
                        open_bnd(1, i, nr_bnd_cells(i)) = xcor(n-1,m  )
                        open_bnd(2, i, nr_bnd_cells(i)) = ycor(n-1,m  )
                        open_bnd(3 ,i, nr_bnd_cells(i)) = xcor(n  ,m  )
                        open_bnd(4, i, nr_bnd_cells(i)) = ycor(n  ,m  )
                    end if
                endif
                
            end if
        end do
    end do
        
    lunbnd = newunit()
    filename = 'com-' // trim(meta%modelname) // '.bnd'
    open(lunbnd, file= trim(filename))

    if (nto > 0) then
        write(lunbnd, '(i0.0)') nto
        do i = 1, nto
            write(lunbnd, '(a)') nambnd(i)
            write(lunbnd, '(i0.0)') nr_bnd_cells(i)
            do j = 1, nr_bnd_cells(i)
                write(lunbnd, '(i0.0, 4es25.17)') seg_nr(j,i), (open_bnd(k,i,j), k=1,4)
            end do 
        end do
    else
        write(lunbnd, '(i2)') nto
    endif
    close(lunbnd)  
!
!===============================================================================
! Write the waqgeom netcdf file
!===============================================================================
!   
    !
    inode = 0
    master = 0
    ierr = 0
    filename = 'com-' // trim(meta%modelname) //'_waqgeom.nc' ! Should be equal to the name given in the hyd-file (that file is written in the routine wrwaqhyd)
    !
    ! create or open the file
    !
    ierr = nf90_create(filename, 0, igeomfile); 
    call nc_check_err(lundia, ierr, "creating file", filename)
    if (ierr/=0) goto 9999
    ierr = ug_addglobalatts(igeomfile, meta)
    call nc_check_err(lundia, ierr, "global attributes", filename)
    if (ierr/=0) goto 9999
    !
    ! Coordinates
    !
    crs%is_spherical = sferic
    
    ierr = ug_new_meshgeom(meshgeom)
    
    meshgeom%meshName = 'mesh2d'
    meshgeom%dim = 2
    meshgeom%crs => crs

    meshgeom%numNode = nr_nodes
    meshgeom%nodex => xn 
    meshgeom%nodey => yn 
    meshgeom%nodez => zn 
    
    meshgeom%numedge = nr_edges
    meshgeom%edgex => xe 
    meshgeom%edgey => ye 

    meshgeom%edge_nodes => netlink
    meshgeom%edge_faces => elemlink
    
    meshgeom%numFace = nr_elems 
    meshgeom%facex => xf
    meshgeom%facey => yf
    
    meshgeom%face_nodes => netelem 
    
    allocate(iapnt(nr_elems))
    do m = 1, mmax
        do n = 1, nmax
            cellindex = func(m, n, nmax)
            elm_i = flow_vol(cellindex)
            if (elm_i > 0) then
                iapnt(elm_i) = isaggr(cellindex)
            end if
        end do
    end do        
       
       
    if (aggre==1) then
        call aggregate_ugrid_geometry(meshgeom, aggregated_meshgeom, edge_type, aggregated_edge_type, iapnt)
        meshgeom = aggregated_meshgeom
        edge_type => aggregated_edge_type
    end if
    
    !datalocs = UG_LOC_NODE + UG_LOC_EDGE + UG_LOC_FACE ! todo: error in IO_UGRID module, you have to define UG_LOC_FACE if dim==2

    ierr = ug_write_mesh_struct(igeomfile, meshids, meshgeom)
    call nc_check_err(lundia, ierr, "writing mesh", filename)
    ! Write edge type variable (this is an extra variable that is not part of the UGRID standard).
    call write_edge_type_variable(igeomfile, meshids, meshgeom%meshName, edge_type)
    call nc_check_err(lundia, ierr, "writing mesh", filename)
    !
9999 continue
    ierr = nf90_sync(igeomfile); 
    call nc_check_err(lundia, ierr, "sync file", filename)
    ierr = nf90_close(igeomfile); 
    call nc_check_err(lundia, ierr, "closing file", filename)
    !
    deallocate(xn)
    deallocate(yn)
    deallocate(zn)
    deallocate(netlink)
    deallocate(flowlink)
    deallocate(xe)
    deallocate(ye)
    deallocate(xf)
    deallocate(yf)
    deallocate(node_mask)
    deallocate(node_tmp)
    deallocate(flow_vol)
    deallocate(edge_mask)
    deallocate(nr_bnd_cells)
    deallocate(open_bnd)
    deallocate(seg_nr)
    
end subroutine wrwaqgeomcl

!> Determine the waq_polygons from waq_vol with the same volume-number
subroutine determine_elem_polygons(node_mask, mmax, nmax, elem_vol, elem_polygon, edge, nr_elems, nr_edges, max_vertex)
    use m_alloc
    
    integer, dimension(:), intent(in) :: node_mask
    integer, intent(in) :: mmax, nmax
    integer, dimension(:), intent(in) :: elem_vol    !!  grid aggregation pointer
    type(waq_polygon_t), dimension(:), pointer :: elem_polygon
    type(edge_t)       , dimension(:), pointer :: edge => null()
    integer, intent(out) :: nr_elems, nr_edges, max_vertex

    integer , dimension(:,:), allocatable :: kcd
    integer :: i, j, iinc, jinc, idim, m, n, k
    integer :: p1, p2, elm1, elm2, i_edge, i_pol, edge_number
    integer :: istat
    logical :: closed
    
    allocate(edge(2*mmax*nmax)) ! equal to number u- and v-points
    allocate(kcd(mmax, nmax)) ! mask array for depth points (i.e. cell corners)
    i_pol = 0
    
    
1000    continue        ! start search again

    kcd = 0 ! mask for the depth-points
    i_pol = i_pol+1
    allocate(elem_polygon(i_pol)%vertex(4)) ! no-aggregation
    allocate(elem_polygon(i_pol)%edge(4))
    do m = 2, mmax
        do n = 2, nmax
            k = func(m, n, nmax)
            if (elem_vol(k)/=i_pol) cycle 
            if (i_pol > 1) then
                idim = elem_polygon(i_pol-1)%nvertex ! make it the size of the previous polygon
                call reallocP(elem_polygon(i_pol)%vertex, idim, fill=0, stat=istat)
                call reallocP(elem_polygon(i_pol)%edge  , idim, fill=0, stat=istat)
            endif
            elem_polygon(i_pol)%nvertex  = 0
            elem_polygon(i_pol)%vertex  = 0

            closed = .false.
            i = m-1
            j = n
            iinc = 1
            jinc = 0
            i_edge = 0

            p1 = func(i,j-1,nmax-1)
            elem_polygon(i_pol)%nvertex = elem_polygon(i_pol)%nvertex + 1
            elem_polygon(i_pol)%vertex(elem_polygon(i_pol)%nvertex) = node_mask(p1)
            kcd(i  ,j-1) = 1

            do while (.not.closed)
                i = i+iinc
                j = j+jinc
                elm1 = func(i  , j  , nmax)
                elm2 = func(i  , j-1, nmax)
                if (iinc == 1 .and. elem_vol(elm1) == i_pol .and. elem_vol(elm1)/=elem_vol(elm2)) then ! straight lower boundary
                    p1 = func(i  , j-1, nmax-1)
                    elem_polygon(i_pol)%nvertex = elem_polygon(i_pol)%nvertex + 1
                    if (elem_polygon(i_pol)%nvertex > size(elem_polygon(i_pol)%vertex)) then
                        call reallocP(elem_polygon(i_pol)%vertex, elem_polygon(i_pol)%nvertex, fill=0, stat=istat)
                        call reallocP(elem_polygon(i_pol)%edge  , elem_polygon(i_pol)%nvertex, fill=0, stat=istat)
                    endif
                    elem_polygon(i_pol)%vertex(elem_polygon(i_pol)%nvertex) = node_mask(p1)
                    i_edge = i_edge+1
                    p1 = node_mask(p1)
                    p2 = elem_polygon(i_pol)%vertex(elem_polygon(i_pol)%nvertex - 1)
                    edge_number = which_edge(p2, p1, elem_vol(elm1), elem_vol(elm2), nr_edges, edge)
                    elem_polygon(i_pol)%edge(i_edge) = edge_number
                    kcd(i, j-1) = 1
                end if
                elm1 = func(i  , j  , nmax)
                elm2 = func(i  , j+1, nmax)
                if (iinc == -1 .and. elem_vol(elm1) == i_pol .and. elem_vol(elm1)/=elem_vol(elm2)) then ! straight upper boundary
                    p1 = func(i-1, j  ,nmax-1)
                    elem_polygon(i_pol)%nvertex = elem_polygon(i_pol)%nvertex + 1
                    if (elem_polygon(i_pol)%nvertex > size(elem_polygon(i_pol)%vertex)) then
                        call reallocP(elem_polygon(i_pol)%vertex, elem_polygon(i_pol)%nvertex, fill=0, stat=istat)
                        call reallocP(elem_polygon(i_pol)%edge  , elem_polygon(i_pol)%nvertex, fill=0, stat=istat)
                    endif
                    elem_polygon(i_pol)%vertex(elem_polygon(i_pol)%nvertex) = node_mask(p1)
                    i_edge = i_edge+1
                    p1 = node_mask(p1)
                    p2 = elem_polygon(i_pol)%vertex(elem_polygon(i_pol)%nvertex - 1)
                    edge_number = which_edge(p2, p1, elem_vol(elm1), elem_vol(elm2), nr_edges, edge)
                    elem_polygon(i_pol)%edge(i_edge) = edge_number
                    kcd(i-1, j) = 1
                end if
                elm1 = func(i  , j  , nmax)
                elm2 = func(i+1, j  , nmax)
                if (jinc == 1 .and. elem_vol(elm1) == i_pol .and. elem_vol(elm1)/=elem_vol(elm2)) then ! straight right boundary
                    p1 = func(i  ,j  ,nmax-1)
                    elem_polygon(i_pol)%nvertex = elem_polygon(i_pol)%nvertex + 1
                    if (elem_polygon(i_pol)%nvertex > size(elem_polygon(i_pol)%vertex)) then
                        call reallocP(elem_polygon(i_pol)%vertex, elem_polygon(i_pol)%nvertex, fill=0, stat=istat)
                        call reallocP(elem_polygon(i_pol)%edge  , elem_polygon(i_pol)%nvertex, fill=0, stat=istat)
                    endif
                    elem_polygon(i_pol)%vertex(elem_polygon(i_pol)%nvertex) = node_mask(p1)
                    i_edge = i_edge+1
                    p1 = node_mask(p1)
                    p2 = elem_polygon(i_pol)%vertex(elem_polygon(i_pol)%nvertex - 1)
                    edge_number = which_edge(p2, p1, elem_vol(elm1), elem_vol(elm2), nr_edges, edge)
                    elem_polygon(i_pol)%edge(i_edge) = edge_number
                    kcd(i, j)= 1
                end if
                elm1 = func(i  , j  , nmax)
                elm2 = func(i-1, j  , nmax)
                if (jinc == -1 .and. elem_vol(elm1) == i_pol .and. elem_vol(elm1)/=elem_vol(elm2)) then ! straight left boundary
                    if (kcd(i-1, j-1) /= 1) then
                        p1 = func(i-1,j-1,nmax-1)
                        elem_polygon(i_pol)%nvertex = elem_polygon(i_pol)%nvertex + 1
                        if (elem_polygon(i_pol)%nvertex > size(elem_polygon(i_pol)%vertex)) then
                            call reallocP(elem_polygon(i_pol)%vertex, elem_polygon(i_pol)%nvertex, fill=0, stat=istat)
                            call reallocP(elem_polygon(i_pol)%edge  , elem_polygon(i_pol)%nvertex, fill=0, stat=istat)
                        endif
                        elem_polygon(i_pol)%vertex(elem_polygon(i_pol)%nvertex) = node_mask(p1)
                        i_edge = i_edge+1
                        p1 = node_mask(p1)
                        p2 = elem_polygon(i_pol)%vertex(elem_polygon(i_pol)%nvertex - 1)
                        edge_number = which_edge(p2, p1, elem_vol(elm1), elem_vol(elm2), nr_edges, edge)
                        elem_polygon(i_pol)%edge(i_edge) = edge_number
                        kcd(i-1, j-1) = 1
                    else
                        p1 = func(i-1,j-1,nmax-1)
                        i_edge = i_edge+1
                        p1 = node_mask(p1)
                        p2 = elem_polygon(i_pol)%vertex(elem_polygon(i_pol)%nvertex)
                        edge_number = which_edge(p2, p1, elem_vol(elm1), elem_vol(elm2), nr_edges, edge)
                        elem_polygon(i_pol)%edge(i_edge) = edge_number
                        i_edge = 0
                        closed = .true.
                        goto 1000
                    end if
                end if
                !
                ! Detect corner
                !    
                if (.not.closed) then
                    if (iinc == 1) then
                        elm1 = func(i  , j  , nmax)
                        elm2 = func(i+1, j  , nmax)
                        if ((elem_vol(elm1) == i_pol .or. elem_vol(elm2) == i_pol) .and. elem_vol(elm1)/=elem_vol(elm2)) then ! turn to left
                            iinc = 0
                            jinc = 1
                            j = j-1
                        else 
                            elm1 = func(i  , j-1, nmax)
                            elm2 = func(i+1, j-1, nmax)
                            if ((elem_vol(elm1) == i_pol .or. elem_vol(elm2) == i_pol) .and. elem_vol(elm1)/=elem_vol(elm2)) then ! turn to right
                                iinc = 0
                                jinc = -1
                                i = i+1
                            end if
                        end if
                    else if (iinc == -1) then
                        elm1 = func(i  , j  , nmax)
                        elm2 = func(i-1, j  , nmax)
                        if ((elem_vol(elm1) == i_pol .or. elem_vol(elm2) == i_pol) .and. elem_vol(elm1)/=elem_vol(elm2)) then ! turn to left
                            iinc = 0
                            jinc = -1
                            j = j+1
                        else 
                            elm1 = func(i-1, j+1, nmax)
                            elm2 = func(i  , j+1, nmax)
                            if ((elem_vol(elm1) == i_pol .or. elem_vol(elm2) == i_pol)  .and. elem_vol(elm1)/=elem_vol(elm2)) then ! turn to right
                                iinc = 0
                                jinc = 1
                                i = i-1
                            end if
                        end if
                    else if (jinc == 1) then
                        elm1 = func(i  , j  , nmax)
                        elm2 = func(i  , j+1, nmax)
                        if ((elem_vol(elm1) == i_pol .or. elem_vol(elm2) == i_pol) .and. elem_vol(elm1)/=elem_vol(elm2)) then ! turn to left
                            iinc = -1
                            jinc = 0
                            i = i+1
                        else 
                            elm1 = func(i+1, j+1, nmax)
                            elm2 = func(i+1, j  , nmax)
                            if ((elem_vol(elm1) == i_pol .or. elem_vol(elm2) == i_pol) .and. elem_vol(elm1)/=elem_vol(elm2)) then ! turn to right
                                iinc = 1
                                jinc = 0
                                j = j+1
                            end if
                        end if
                    else if (jinc == -1) then
                        elm1 = func(i  , j  , nmax)
                        elm2 = func(i  , j-1, nmax)
                        if ((elem_vol(elm1) == i_pol .or. elem_vol(elm2) == i_pol) .and. elem_vol(elm1)/=elem_vol(elm2)) then ! turn to left
                            iinc = 1
                            jinc = 0
                            i = i-1
                        else 
                            elm1 = func(i-1, j  , nmax)
                            elm2 = func(i-1, j-1, nmax)
                            if ((elem_vol(elm1) == i_pol .or. elem_vol(elm2) == i_pol) .and. elem_vol(elm1)/=elem_vol(elm2)) then ! turn to right
                                iinc = -1
                                jinc = 0
                                j = j-1
                            end if
                        end if
                    end if
                end if
            end do
        end do
    end do
    !
    nr_elems = i_pol-1
    max_vertex = 0
    do i = 1, nr_elems
        max_vertex = max(max_vertex, elem_polygon(i)%nvertex)
    end do
    
    deallocate(kcd)
end subroutine determine_elem_polygons


integer function  which_edge(p1, p2, elm1, elm2, nr_edges, edge) result(res)
   use io_netcdf
   implicit none
    
    integer, intent(in)  :: p1
    integer, intent(in)  :: p2
    integer, intent(in)  :: elm1
    integer, intent(in)  :: elm2
    integer, intent(out) :: nr_edges
    type(edge_t), dimension(:), pointer, intent(out) :: edge
        
    integer :: i

    ! Check whether edge already exists.
    do i = nr_edges, 1, -1
        if (edge(i)%vertex(1) == p1 .and. edge(i)%vertex(2) == p2 .or. &
            edge(i)%vertex(1) == p2 .and. edge(i)%vertex(2) == p1) then
            res = i
            return
        end if
    end do        

    nr_edges = nr_edges+1
    edge(nr_edges)%vertex(1) = p1
    edge(nr_edges)%vertex(2) = p2
    edge(nr_edges)%element(1) = elm1
    edge(nr_edges)%element(2) = elm2
    edge(nr_edges)%nmelm = 2
    if (elm1==0 .or. elm2==0) then
        edge(nr_edges)%nmelm = 1
        edge(nr_edges)%type = UNC_EDGETYPE_BND_CLOSED
            if (elm1==0) then
                edge(nr_edges)%element(1) = missing_value
            else
                edge(nr_edges)%element(2) = missing_value
            endif
    else if (elm1<0 .or. elm2<0) then
        edge(nr_edges)%type = UNC_EDGETYPE_BND
    else
        edge(nr_edges)%type = UNC_EDGETYPE_INTERNAL
    endif
! can't determine if it is UNC_EDGETYPE_INTERNAL_CLOSED here
    
    res = nr_edges
end function which_edge

integer function func(i, j, nmax) 
    integer i, j, nmax
    func = j + (i-1)*nmax
end function func

end module m_write_waqgeom_curvilinear
