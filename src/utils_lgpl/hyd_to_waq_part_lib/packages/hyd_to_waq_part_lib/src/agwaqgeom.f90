!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2017.                                
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
!> Aggregates the given mesh geometry using the given aggregation table.
!!--pseudo code and references--------------------------------------------------
! Dependencies:
!   io_netcdf module for derived type t_ug_meshgeom
!!--declarations----------------------------------------------------------------
module m_aggregate_waqgeom
    use MessageHandling
    
    implicit none

contains    
!> Aggregates the given mesh geometry and edge type array using the given aggregation table.
!! The mesh aggregation algorithm removes edges, but preserves the order of the edges. The edge type of a given edge stays the same.
!! So if the edges in the un-aggregated mesh are ordered (first flow links, then closed edges),
!! then the edges in the aggregated mesh will still be ordered (first flow links, then closed edges).
!!
!! since array pointers will become disassociated, possibly causing memory leaks.
function aggregate_ugrid_geometry(input, output, input_edge_type, output_edge_type, face_mapping_table) result(success)
    use io_ugrid
    use m_alloc

    implicit none

    type(t_ug_meshgeom), intent(in)                 :: input !< The mesh geometry to be aggregated.
    type(t_ug_meshgeom), intent(inout)              :: output !< Aggregated mesh geometry.
    integer, dimension(:), intent(in)               :: input_edge_type !< The edge type array to be aggregated.
    integer, dimension(:), allocatable, intent(out) :: output_edge_type !< Aggregated edge type array.
    integer, dimension(:), intent(in)               :: face_mapping_table !< Mapping table flow cells -> waq cells.
    logical                                         :: success !< Result status, true if successful.

    character(len=255)                       :: message !< Temporary variable for writing log messages.
    integer, parameter                       :: missing_value = -999
    integer, dimension(:), allocatable       :: node_mapping_table, reverse_node_mapping_table, reverse_edge_mapping_table !< Mapping tables.
    integer                                  :: input_edge_count, output_edge_count, output_node_count, output_face_count, max_nodes_per_face, node_count !< Counters.
    integer                                  :: i, j, input_edge, output_edge, input_node, output_node, output_face !< Counters.
    integer, dimension(2)                    :: faces !< Helper array.
    integer, dimension(:,:), allocatable     :: input_edge_output_faces !< Helper array.
    integer, dimension(:), allocatable       :: face_edge_count, nodes !< Helper arrays.
    double precision                         :: area !< Output of subroutine comp_masscenter (not used here).
    integer                                  :: counterclockwise !< Output of subroutine comp_masscenter (not used here).

    success = .false.


    ! 1. Determine output edge_faces and edge_nodes.
    ! Apply face mapping table to edge faces.
    input_edge_count = input%numEdge
    call realloc(input_edge_output_faces, (/ 2, input_edge_count /), fill=missing_value)
    do input_edge = 1,input_edge_count
        do i = 1,2
            if (input%edge_faces(i, input_edge) /= missing_value) then
                input_edge_output_faces(i, input_edge) = face_mapping_table(input%edge_faces(i, input_edge))
            end if
        end do ! i
    end do ! input_edge
    ! Create edge mapping table and output edge_faces and edge_nodes.
    call realloc(reverse_edge_mapping_table, input_edge_count)
    call reallocP(output%edge_faces, (/ 2, input_edge_count /))
    call reallocP(output%edge_nodes, (/ 2, input_edge_count /))
    output_edge = 0
    do input_edge = 1,input_edge_count
        ! If edge points to the same aggregated face on either side, then edge is not needed anymore in the aggregated mesh.
        if (input_edge_output_faces(1, input_edge) /= input_edge_output_faces(2, input_edge)) then ! Edge that should stay.
            ! The remaining output edges have a different numbering.
            output_edge = output_edge + 1
            reverse_edge_mapping_table(output_edge) = input_edge
            output%edge_faces(1:2, output_edge) = input_edge_output_faces(1:2, input_edge)
            output%edge_nodes(1:2, output_edge) = input%edge_nodes(1:2, input_edge)
        end if
    end do
    output_edge_count = output_edge
    if (output_edge_count < 3) then
        call mess(LEVEL_ERROR, 'Edge count in aggregated mesh < 3. Mesh will not be aggregated.')
        return
    end if
    ! At this point edges have been renumbered automatically from input edge numbers to output edge numbers.
    ! Truncate arrays.
    call realloc(reverse_edge_mapping_table, output_edge_count, keepExisting=.true.)
    call reallocP(output%edge_faces, (/ 2, output_edge_count /), keepExisting=.true.)
    call reallocP(output%edge_nodes, (/ 2, output_edge_count /), keepExisting=.true.)


    ! 2. Determine output edge coordinates and types.
    call reallocP(output%edgex, output_edge_count)
    call reallocP(output%edgey, output_edge_count)
    call realloc(output_edge_type, output_edge_count)
    do output_edge = 1,output_edge_count
        output%edgex(output_edge) = input%edgex(reverse_edge_mapping_table(output_edge))
        output%edgey(output_edge) = input%edgey(reverse_edge_mapping_table(output_edge))
        ! Edge z coordinates are unknown.
        output_edge_type(output_edge) = input_edge_type(reverse_edge_mapping_table(output_edge))
    end do


    ! 3. Create node mapping table.
    call realloc(node_mapping_table, input%numNode, fill=missing_value)
    ! All nodes that are present in output edge_nodes should remain, all other nodes are not needed anymore in the aggregated mesh.
    ! First create mask of remaining nodes in node_mapping_table.
    do output_edge = 1,output_edge_count
        node_mapping_table(output%edge_nodes(1:2, output_edge)) = 1
    end do
    output_node_count = count(node_mapping_table == 1)
    if (output_node_count < 3) then
        call mess(LEVEL_ERROR, 'Node count in aggregated mesh < 3. Mesh will not be aggregated.')
        return
    end if
    ! Change mask into mapping table.
    call realloc(reverse_node_mapping_table, output_node_count)
    output_node = 0
    do input_node = 1,input%numNode
        if (node_mapping_table(input_node) == 1) then ! Node that should stay.
            ! The remaining output nodes have a different numbering.
            output_node = output_node + 1
            node_mapping_table(input_node) = output_node
            reverse_node_mapping_table(output_node) = input_node
        end if
    end do
    ! Renumber input node numbers to output node numbers in output edge_nodes, using node_mapping_table.
    do output_edge = 1,output_edge_count
        output%edge_nodes(1, output_edge) = node_mapping_table(output%edge_nodes(1, output_edge))
        output%edge_nodes(2, output_edge) = node_mapping_table(output%edge_nodes(2, output_edge))
    end do


    ! 4. Determine output node coordinates.
    call reallocP(output%nodex, output_node_count)
    call reallocP(output%nodey, output_node_count)
    call reallocP(output%nodez, output_node_count)
    do output_node = 1,output_node_count
        output%nodex(output_node) = input%nodex(reverse_node_mapping_table(output_node))
        output%nodey(output_node) = input%nodey(reverse_node_mapping_table(output_node))
        output%nodez(output_node) = input%nodez(reverse_node_mapping_table(output_node))
    end do


    ! 5. Determine output face_edges.
!    ! Convert output edge_faces to a flat table with two columns: edges column and faces column.
!    call realloc(edges_column, output_edge_count * 2)
!    call realloc(faces_column, output_edge_count * 2)
!    forall (i = 1:output_edge_count*2)
!        edges_column(i) = (i + 1) / 2
!    end forall
!    faces_column = reshape(output_edge_faces, (/ output_edge_count * 2 /))
!    ! Sort table on faces column.
!    ! TODO use quicksort? AK
!    qsort(faces_column, sorted_faces_column, sorted_indices)
!    sorted_edges_column = edges_column(sorted_indices)
    ! This code assumes that output faces are numbered 1, 2, 3, etc. without gaps.
    ! TODO remove -1, -2, etc. by making temp pointer to first part of face_mapping_table
    output_face_count = maxval(face_mapping_table)
    if (output_face_count < 1) then
        call mess(LEVEL_ERROR, 'Face count in aggregated mesh < 1. Mesh will not be aggregated.')
        return
    end if
    ! Count edges for each face.
    call realloc(face_edge_count, output_face_count, fill=0)
    do output_edge = 1,output_edge_count
        faces = output%edge_faces(1:2, output_edge)
        ! Add 1 edge for both faces.
        do i=1,2
            if (faces(i) == missing_value) then
                cycle
            end if

            face_edge_count(faces(i)) = face_edge_count(faces(i)) + 1
        end do ! i
    end do ! output_edge
    do output_face = 1,output_face_count
        if (face_edge_count(output_face) < 3) then
            write(message, *) 'Face edge count in aggregated mesh < 3 for face ', output_face, '. Mesh will not be aggregated.'
            call mess(LEVEL_ERROR, trim(message))
            return
        end if
    end do
    ! Determine max_nodes_per_face.
    max_nodes_per_face = maxval(face_edge_count)
    ! Determine nodes, edges and faces for each output face.
    call reallocP(output%face_edges, (/ max_nodes_per_face, output_face_count /), fill=missing_value)
    ! Re-use face_edge_count array to put edges in the next available spot in the output%face_edges array.
    face_edge_count = 0
    do output_edge = 1,output_edge_count
        faces = output%edge_faces(1:2, output_edge)
        do i = 1,2
            if (faces(i) == missing_value) then
                cycle
            end if

            ! Keep track of current number of edges for this face.
            face_edge_count(faces(i)) = face_edge_count(faces(i)) + 1
            ! Put current edge in the next available spot in output%face_edges for this face.
            output%face_edges(face_edge_count(faces(i)), faces(i)) = output_edge
        end do ! i
    end do ! output_edge
    ! At this point the edges for each face are in random order.


    ! 6. Sort edges for each face in counter clockwise order.
    ! At the same time store sorted nodes of sorted edges in output%face_nodes array.
    call reallocP(output%face_nodes, (/ max_nodes_per_face, output_face_count /), fill=missing_value)
    do output_face = 1,output_face_count
        ! Sort edges for current output face.
        call sort_edges(output_face, output%face_edges(1:face_edge_count(output_face), output_face), output%face_nodes(1:face_edge_count(output_face), output_face), &
                input%edge_nodes, input%face_nodes, input%edge_faces, face_mapping_table, reverse_edge_mapping_table, node_mapping_table, output%edge_nodes)
    end do


    ! 7. Determine output face_links.
    call reallocP(output%face_links, (/ max_nodes_per_face, output_face_count /), fill=missing_value)
    do output_face = 1,output_face_count
        ! Get output faces that are adjacent to the current output_face.
        call get_adjacent_faces(output_face, output%face_edges, output%edge_faces, output%face_links(1:face_edge_count(output_face), output_face))
    end do


    ! 8. Determine output face coordinates.
    ! Here calculate the cell centroids (cell "centers of mass").
    call realloc(nodes, max_nodes_per_face)
    call reallocP(output%facex, output_face_count)
    call reallocP(output%facey, output_face_count)
    do output_face = 1,output_face_count
        node_count = face_edge_count(output_face)

        ! Reset nodes.
        nodes = missing_value
        nodes(1:node_count) = output%face_nodes(1:node_count, output_face)

        ! Note that passed xs and ys arrays are larger than the passed polygon size (extra elements are not used in subroutine comp_masscenter).
        call comp_masscenter(node_count, output%nodex(nodes(1:node_count)), output%nodey(nodes(1:node_count)), &
                output%facex(output_face), output%facey(output_face), area, counterclockwise)
        ! Face z coordinates are unknown.
    end do


    ! Store remaining output variables in output mesh geometry.
    output%meshName = trim(input%meshName)//'_agg'
    output%dim = input%dim

    output%numNode = output_node_count
    output%numEdge = output_edge_count
    output%numFace = output_face_count

    !TODO deallocate temporary arrays

    success = .true.

end function aggregate_ugrid_geometry

!> Sorts the given edges of the current face in counter clockwise order.
!! At the same time stores the sorted nodes of the current face in the given nodes array.
!! In this subroutine input means "from the un-aggregated mesh" and output means "from the aggregated mesh".
subroutine sort_edges(current_face, edges, nodes, input_edge_nodes, input_face_nodes, input_edge_faces, face_mapping_table, reverse_edge_mapping_table, node_mapping_table, output_edge_nodes)

    implicit none

    integer, intent(in)                  :: current_face !< Current face.
    integer, dimension(:), intent(inout) :: edges !< Edges of the current face.
    integer, dimension(:), intent(out)   :: nodes !< Array to store the nodes of the current face.
    integer, dimension(:,:), intent(in)  :: input_edge_nodes, input_face_nodes, input_edge_faces, output_edge_nodes !< Connectivity arrays.
    integer, dimension(:), intent(in)    :: face_mapping_table, reverse_edge_mapping_table, node_mapping_table !< Mapping tables.

    character(len=255)    :: message !< Temporary variable for writing log messages.
    integer               :: first_node, current_node, number_of_edges, k, i !< Counters.
    integer, dimension(2) :: next_nodes !< Helper array.
    logical               :: found

    ! Start with the edge that happens to be listed first, this will stay in the first position.
    ! First sort the two nodes of the first edge in CCW order, so that all subsequent edges will also be sorted in CCW order.
    next_nodes = sort_first_two_nodes(current_face, edges(1), input_edge_nodes, input_face_nodes, input_edge_faces, face_mapping_table, reverse_edge_mapping_table, node_mapping_table)

    first_node = next_nodes(1)
    current_node = next_nodes(2)
    nodes(1) = first_node
    number_of_edges = size(edges)
    do k = 2,number_of_edges
        nodes(k) = current_node

        ! Error if arrive at the first edge and there are still un-used edges leftover.
        if (current_node == first_node) then
            write(message, *) 'For face ', current_face, ' there are unconnected edges in aggregated mesh. &
                    &This can happen if the aggregated cell consists of cells that are not connected or if the aggregated cell is shaped like a ring. Mesh will not be aggregated.'
            call mess(LEVEL_ERROR, trim(message))
            return
        end if

        ! Get next neighbor edge, i.e. another edge that is connected to the current node.
        found = .false.
        do i = k,number_of_edges
            next_nodes = output_edge_nodes(1:2, edges(i))

            if (next_nodes(1) == current_node) then
                found = .true.
                if (i /= k) then
                    call swap(edges(i), edges(k))
                end if
                ! Continue with node on the other side of next edge.
                current_node = next_nodes(2)
                exit
            else if (next_nodes(2) == current_node) then
                found = .true.
                if (i /= k) then
                    call swap(edges(i), edges(k))
                end if
                ! Continue with node on the other side of next edge.
                current_node = next_nodes(1)
                exit
            end if
        end do ! i

        if (.not. found) then
            write(message, *) 'For face ', current_face, ' cannot find edge connected to node ', current_node, ' of edge ', edges(k-1), ' in aggregated mesh. Mesh will not be aggregated.'
            call mess(LEVEL_ERROR, trim(message))
            return
        end if
    end do ! k

    ! Error if last edge is not connected to first edge.
    if (current_node /= first_node) then
        write(message, *) 'For face ', current_face, ' node ', current_node, ' of last edge ', edges(number_of_edges), &
                ' is not connected to node ', first_node, ' of first edge ', edges(1), ' in aggregated mesh. Mesh will not be aggregated.'
        call mess(LEVEL_ERROR, trim(message))
        return
    end if

end subroutine sort_edges

!> The given edge in the aggregated mesh has two nodes. The returned array contains these two nodes sorted in CCW order,
!! i.e. in the same order as these two nodes would be encountered when traversing the nodes of the given face in CCW order.
!! The order will be opposite for the two faces that the given edge connects, therefore the given face is also needed as input.
!! In this subroutine input means "from the un-aggregated mesh" and output means "from the aggregated mesh".
function sort_first_two_nodes(output_face, output_edge, input_edge_nodes, input_face_nodes, input_edge_faces, face_mapping_table, reverse_edge_mapping_table, node_mapping_table) result(sorted_output_nodes)
    use m_alloc

    implicit none

    integer, intent(in)                 :: output_face !< Current face.
    integer, intent(in)                 :: output_edge !< First edge of the current face.
    integer, dimension(:,:), intent(in) :: input_edge_nodes, input_face_nodes, input_edge_faces !< Connectivity arrays.
    integer, dimension(:), intent(in)   :: face_mapping_table, reverse_edge_mapping_table, node_mapping_table !< Mapping tables.

    character(len=255)                 :: message !< Temporary variable for writing log messages.
    integer, parameter                 :: missing_value = -999
    integer                            :: input_edge, input_face, max_nodes_per_face, nodes_per_face, node, next_node, previous_node
    integer, dimension(2)              :: input_nodes, input_faces
    integer, dimension(:), allocatable :: nodes
    integer                            :: i !< Counter.
    logical                            :: sorted
    integer, dimension(2)              :: sorted_output_nodes !< The two nodes of the first edge of the current face in sorted order.
    integer                            :: input_face1, input_face2


    ! Get input edge, nodes and faces that correspond to the given output edge.
    input_edge = reverse_edge_mapping_table(output_edge)
    input_nodes = input_edge_nodes(1:2, input_edge)
    input_faces = input_edge_faces(1:2, input_edge)

    ! Get the input face of the input edge that is part of the given output face.
    input_face1=-1
    input_face2=-1
    if (input_faces(1) /= missing_value) input_face1 = face_mapping_table(input_faces(1))
    if (input_faces(2) /= missing_value) input_face2 = face_mapping_table(input_faces(2))
    if (input_face1 == output_face) then
        input_face = input_faces(1)
    else if (input_face2 == output_face) then
        input_face = input_faces(2)
    else
        write(message, *) 'Cannot find input face for output face ', output_face, ' and output edge ', output_edge, ' in un-aggregated mesh. Mesh will not be aggregated.'
        call mess(LEVEL_ERROR, trim(message))
        return
    end if

    ! Get input nodes of input face.
    max_nodes_per_face = size(input_face_nodes(:, input_face))
    call realloc(nodes, max_nodes_per_face)
    nodes = input_face_nodes(:, input_face)
    ! Determine nodes_per_face.
    nodes_per_face = max_nodes_per_face
    do i = 1,max_nodes_per_face
        if (nodes(i) == missing_value) then
            nodes_per_face = i - 1
            exit
        end if
    end do
    if (nodes_per_face < 3) then
        call mess(LEVEL_ERROR, 'Nodes per face in un-aggregated mesh < 3. Mesh will not be aggregated.')
        return
    end if

    ! Sort input_nodes.
    ! Find input nodes in input face nodes of input face and sort the input nodes in the same order as they are found in input face nodes.
    sorted = .false.
    do i = 1,nodes_per_face
        node = nodes(i)

        if (node == input_nodes(1)) then
            next_node = nodes(modulo(i, nodes_per_face) + 1)
            if (next_node == input_nodes(2)) then
                sorted = .true.
                exit
            end if

            previous_node = nodes(modulo(i + nodes_per_face - 2, nodes_per_face) + 1)
            if (previous_node == input_nodes(2)) then
                call swap(input_nodes(1), input_nodes(2))
                sorted = .true.
                exit
            end if

            write(message, *) 'Cannot find node ', input_nodes(2), ' of face ', input_face, ' in face nodes in un-aggregated mesh. Mesh will not be aggregated.'
            call mess(LEVEL_ERROR, trim(message))
            return
        end if
    end do
    if (.not. sorted) then
        write(message, *) 'Cannot find node ', input_nodes(1), ' of face ', input_face, ' in face nodes in un-aggregated mesh. Mesh will not be aggregated.'
        call mess(LEVEL_ERROR, trim(message))
        return
    end if

    ! Get output nodes corresponding to sorted input nodes.
    sorted_output_nodes = node_mapping_table(input_nodes)

    deallocate(nodes)

end function sort_first_two_nodes

!> All faces that are adjacent to the given face are stored in the given array adjacent_faces.
!! The length of the given array adjacent_faces must be equal to the number of edges of the given face.
subroutine get_adjacent_faces(face, face_edges, edge_faces, adjacent_faces)

    implicit none

    integer, intent(in)                 :: face !< Input face.
    integer, dimension(:,:), intent(in) :: face_edges !< Face edge connectivity.
    integer, dimension(:,:), intent(in) :: edge_faces !< Edge face connectivity.
    integer, dimension(:), intent(out)  :: adjacent_faces !< Output array.

    integer               :: edge, i
    integer, dimension(2) :: faces

    ! Determine faces that are adjacent to the current face.
    do i = 1,size(adjacent_faces)
        edge = face_edges(i, face)

        ! Store neighboring face for this edge.
        ! Note that some face links can be out_of_mesh (i.e. missing value).
        faces = edge_faces(1:2, edge)
        ! Of the two faces, one is the given face and the other is the neighboring face (or missing value).
        if (faces(1) == face) then
            adjacent_faces(i) = faces(2)
        else ! If faces(2) == face
            adjacent_faces(i) = faces(1)
        end if
    end do ! i

end subroutine get_adjacent_faces

!> Swap the values of the given integers a and b.
subroutine swap(a, b)

    implicit none

    integer, intent(inout) :: a, b !< Integers to swap.

    integer :: temp

    temp = a
    a = b
    b = temp

end subroutine swap

!> compute area and mass center of polygon
subroutine comp_masscenter(N, xin , y, xcg, ycg, area, jacounterclockwise)

   implicit none

   integer,                        intent(in)    :: N        !< polygon size
   double precision, dimension(N), intent(in)    :: xin, y   !< polygon coordinates
   double precision,               intent(out)   :: xcg, ycg !< polygon mass center coordinates
   double precision,               intent(out)   :: area     !< polygon area
   integer,                        intent(out)   :: jacounterclockwise  !< counterclockwise (1) or not (0)

   integer                                       :: jsferic = 0 ! xy pair is in : 0=cart, 1=sferic coordinates (no taken into account yet)

   double precision, dimension(N) :: x  ! Copy of xin, with possibly periodic fixes.
   double precision                              :: dsx, dsy, xc, yc, dcos, xds, fac, x0, y0, x1, dx0, dx1, dy0, dy1
   double precision                              :: xdum

   integer                                       :: i, ip1

   double precision, external                    :: getdx, getdy

   double precision, parameter                   :: dtol=1d-8
   double precision, parameter                   :: ra = 6378137d0    ! earth radius (m)
   double precision                              :: pi
   double precision                              :: dg2rd             ! degrees to radians
   pi    = acos(-1d0)
   dg2rd = pi/180d0

   area = 0d0
   xcg  = 0d0
   ycg  = 0d0
   jacounterclockwise = 1

   if ( N.lt.1 ) goto 1234

   x = xin

!  set reference point (furthest away from poles)   
   x0 = minval(x(1:N))
   y0 = y(1)
   do i=2,N
      if ( abs(y(i)).lt.abs(y0) ) then
         y0 = y(i)
      end if
   end do

   !  fix for periodic, spherical coordinates
   if ( jsferic.eq.1 ) then
      x1 = maxval(x(1:N))
      if ( x1-x0.gt.180d0 ) then
!        determine cutline
         xdum = x1-180d0
         do i=1,N
            if ( x(i).lt.xdum ) then
               x(i) = x(i) + 360d0
            end if
         end do
         x0 = minval(x(1:N))
      end if
   end if

   do i=1,N
      ip1 = i+1; if ( ip1.gt.N ) ip1=ip1-N

      
      call getdxdy(x0,y0,x(i),y(i), dx0,dy0)
      call getdxdy(x0,y0,x(ip1),y(ip1), dx1, dy1)
      xc = 0.5d0*(dx0 + dx1)
      yc = 0.5d0*(dy0 + dy1) 
      
      call getdxdy(x(i), y(i), x(ip1), y(ip1), dx0, dy0) 
      dsx = dy0 ; dsy = -dx0
      
      xds  = xc*dsx+yc*dsy
      area = area + 0.5d0*xds
      xcg  = xcg  + xds * xc
      ycg  = ycg  + xds * yc
   end do

!  for clockwise oriented cells, the normal will be inward, and consequently the area negative
!  it must stay negative in the computation of the cell center (xcg,ycg)
   area = sign(max(abs(area),dtol),area)

   fac = 1d0/(3d0*area)

   xcg = fac * xcg
   ycg = fac * ycg

   if ( JSFERIC.ne.0 ) then
      ycg = ycg / (Ra*dg2rd)
      xcg = xcg / (Ra*dg2rd*cos((ycg+y0)*dg2rd))
   end if

   xcg = xcg + x0
   ycg = ycg + y0
   
!  output cell orientation
   if ( area.gt.0d0 ) then
      jacounterclockwise = 1
   else
      jacounterclockwise = 0
   end if
   
!  fix for inward normals (clockwise oriented cells)   
   area = abs(area)

1234 continue

   return
end subroutine comp_masscenter

 subroutine getdxdy(x1,y1,x2,y2,dx,dy)
 implicit none
 double precision :: x1, y1, x2, y2, dx, dy, dx2, dy2, dum
 integer :: jsferic = 0 ! xy pair is in : 0=cart, 1=sferic coordinates (no taken into account yet)
 double precision, external :: getdx, getdy
 if (Jsferic == 1) then 
  
!    if (jasferdistance == 1) then  ! this is a fix 
!       call sferdistance(x1,y1,x2,y1,dx)  
!       if (x2 < x1) dx = -dx
!       call sferdistance(x1,y1,x1,y2,dy)
!       if (y2 < y1) dy = -dy
!     else
!       dx = getdx(x1,y1,x2,y2)  
!       dy = getdy(x1,y1,x2,y2) 
!    endif
    
 else
    dx = x2-x1
    dy = y2-y1
 endif
 
 end subroutine getdxdy

!
!------------------------------------------------------------------------------
end module m_aggregate_waqgeom
