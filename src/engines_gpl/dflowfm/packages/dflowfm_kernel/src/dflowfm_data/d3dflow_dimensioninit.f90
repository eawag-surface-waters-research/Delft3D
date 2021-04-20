subroutine D3Dflow_dimensioninit()
    use m_flowgeom
    use grid_dimens_module
    use m_partitioninfo, only: jampi, idomain, iglobal_s, my_rank
    use m_flow !, only: ndkx, lnkx
    use network_data, only: xk, yk
    ! use m_cell_geometry, ony: xz, yz, ndx
    implicit none

    integer :: istart ! start index into cell2node array
    integer :: istat  ! status flag
    integer :: L      ! loop variable
    integer :: nm     ! spatial loop variable
    integer :: nnod   ! number of nodes per face

    ! Construct a default griddim struct
    call simplegrid_dimens(griddim, ndx, 1)
    griddim%mmax                 = ndxi !! this should be nmax to be consistent, but mmax is also used in trachytopes ...
    griddim%nmmax                = ndxi !! Why not pass ndxi in simplegrid_dimens call? reduces length of celltype to ndxi is this a problem?
    griddim%meshtype             = MESH_UNSTRUCTURED
    griddim%parttype             = PARTITION_NONCONT

    allocate(griddim%ncellnodes(ndx), stat=istat)
    if (istat==0) allocate(griddim%indexnode1(ndx), stat=istat)
    if (istat==0) then
       istart = 1
       do nm = 1,ndxi
          nnod = size(nd(nm)%nod)
          griddim%indexnode1(nm) = istart
          griddim%ncellnodes(nm) = nnod
          istart = istart + nnod
          !
          if (jampi == 1) then
             griddim%nmglobal(nm) = iglobal_s(nm)
             if (idomain(nm) == my_rank) then
                griddim%celltype(nm) = 1 ! Internal cells
             else
                griddim%celltype(nm) = -1 ! Ghost cells
             endif
          else
             griddim%nmglobal(nm) = nm
             griddim%celltype(nm) = 1 ! Internal cells
          endif
       enddo
       griddim%celltype(ndxi+1:ndx) = 2 ! Boundary cells
    endif
    if (istat==0) allocate(griddim%cell2node(istart-1), stat=istat)
    if (istat==0) then
       istart = 1
       do nm = 1,ndxi
          nnod = size(nd(nm)%nod)
          do L = 1,nnod
              griddim%cell2node(istart) = nd(nm)%nod(L)
              istart = istart + 1
          enddo
       enddo
    endif

    ! generate table for boundary mirroring of input
    allocate(griddim%nmbnd(ndx-ndxi,2))
    do L = lnxi+1, lnx
       griddim%nmbnd(LN(1, L)-ndxi,1) = LN(1, L)  ! point outside net
       griddim%nmbnd(LN(1, L)-ndxi,2) = LN(2, L)  ! point inside net
    enddo

    griddim%xz                   => xz
    griddim%yz                   => yz
    griddim%xnode                => xk
    griddim%ynode                => yk

end subroutine D3Dflow_dimensioninit
