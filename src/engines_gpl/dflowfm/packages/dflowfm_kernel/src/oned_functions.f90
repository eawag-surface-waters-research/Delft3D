module m_oned_functions
   private

   public set_1d_roughnesses
   public set_1d_indices_in_network
   public save_1d_nrd_vars_in_stm

   type, public :: t_gridp2cs
      integer :: num_cross_sections
      integer, allocatable, dimension(:) :: cross
   end type

   type(t_gridp2cs), allocatable, dimension(:) :: gridpoint2cross

   contains

   !> IFRCUTP and FRCu are filled, using 1D roughness values from Network structure 
   subroutine set_1d_roughnesses()
      use m_flow, only: frcu, ifrcutp
      use unstruc_channel_flow
      use m_spatial_data
      use m_branch

      implicit none

      integer :: L, i, k
      integer :: ibr
      type(t_branch), pointer                 :: pbr
      double precision, dimension(:), pointer :: cpar
      integer,          dimension(:), pointer :: rgh_type
      integer,          dimension(:), pointer :: fun_type
      integer,          dimension(9)          :: rgh_mapping

      if (network%brs%Count > 0) then
         ! RGH_TYPE is similar to IFRCUTP, only with different type numbers
         ! Dflow1D also supports water level or discharge dependent roughness parameters (FUN_TYPE )
         rgh_mapping = -1
         rgh_mapping(R_Chezy         ) = 0
         rgh_mapping(R_Manning       ) = 1
         rgh_mapping(R_WhiteColebrook) = 3

         rgh_type => network%rgs%rough(1)%rgh_type_pos
         fun_type => network%rgs%rough(1)%fun_type_pos
         cpar     => network%spData%quant(network%rgs%rough(1)%spd_pos_idx)%values
         do ibr = 1, network%brs%Count
            pbr => network%brs%branch(ibr)
            do i = 1, pbr%uPointsCount
               L = pbr%lin(i)
               k = pbr%points(1) -1 + i
               ifrcutp(L) = rgh_mapping(rgh_type(ibr))
               if ( (fun_type(ibr) == R_FunctionConstant) .and. (ifrcutp(L) >=0) ) then
                  frcu(L) = cpar(k)
               else
                  call setmessage(LEVEL_FATAL, '1D roughness type on branch '// trim(pbr%name) //', '//pbr%id//' is not available in D-FlowFM')
                  ifrcutp(L) = 0
                  frcu(L)    = 45d0
               endif
            enddo
         enddo
      endif

   end subroutine set_1d_roughnesses

   !> Sets the flowgeom link and node numbers of the computational grid 
   !! into the 1D network structure for branches, retention nodes, 
   !! cross sections and structures, etc. 
   subroutine set_1d_indices_in_network()
      use m_sediment
      use m_flowgeom
      use m_cross_helper
      
      default_width = wu1DUNI
      
      call set_linknumbers_in_branches()
      call set_retention_grid_numbers()
      
      if (jased > 0 .and. stm_included) then
         ! 
         call set_cross_sections_to_gridpoints()
      endif
      call set_structure_indices()
      
   end subroutine set_1d_indices_in_network

   !> set the flowgeom linknumbers and node numbers in the branches
   subroutine set_linknumbers_in_branches()
   
      use unstruc_channel_flow
      use m_flowgeom
      use m_sediment
      use messageHandling

      implicit none

      integer :: L
      integer :: ibr
      integer :: nbr, upointscount, pointscount
      integer :: storageCount
      integer :: i, j, jpos, linkcount
      integer :: k1, k2, igrid
      integer :: c1, c2
      integer :: storage_count
      type(t_branch), pointer                 :: pbr
      type(t_storage), pointer                :: pstor
      integer, dimension(:), pointer          :: lin
      integer, dimension(:), pointer          :: grd
      double precision, dimension(:), pointer :: offset
      type(t_offset2cross), pointer           :: gpnt2cross(:)                   !< list containing cross section indices per u-location
      type (t_CrossSection), pointer          :: cross1, cross2
      double precision, parameter             :: eps_crs = 1d0                      !< accuracy for determining a cross section lies on a grid point

      nbr = network%brs%count
      do ibr = 1, nbr
         pbr => network%brs%branch(ibr)
         lin => pbr%lin
         grd => pbr%grd
         L = lin(1)
         k1  =  ln(1,L)
         pbr%FromNode%gridNumber = k1
         upointscount = pbr%uPointsCount
         do i = 1, uPointsCount
            L = lin(i)
            k1 = ln(1,L)
            grd(i) = k1
         enddo
         k2 = ln(2,lin(upointscount))
         pbr%tonode%gridnumber = k2
         grd(upointscount+1) = k2
      enddo
   end subroutine set_linknumbers_in_branches

   !> Set the node numbers from flowgeom in the retention structure
   subroutine set_retention_grid_numbers()
   
      use unstruc_channel_flow
      use m_flowgeom
      use m_sediment
      use messageHandling

      implicit none

      integer :: L
      integer :: ibr
      integer :: nbr, upointscount, pointscount
      integer :: storageCount
      integer :: i, j, jpos, linkcount
      integer :: k1, k2, igrid
      integer :: c1, c2
      integer :: storage_count
      type(t_branch), pointer                 :: pbr
      type(t_storage), pointer                :: pstor
      integer, dimension(:), pointer          :: lin
      integer, dimension(:), pointer          :: grd
      double precision, dimension(:), pointer :: offset
      type(t_offset2cross), pointer           :: gpnt2cross(:)                   !< list containing cross section indices per u-location
      type (t_CrossSection), pointer          :: cross1, cross2
      double precision, parameter             :: eps_crs = 1d0                      !< accuracy for determining a cross section lies on a grid point


      storageCount = network%storS%count
      do i = 1, storageCount
         pstor => network%storS%stor(i)
         if (pstor%branch_index <= 0) then
            pstor%gridPoint = network%nds%node(pstor%node_index)%gridNumber
         else
            pbr => network%brs%branch(pstor%branch_index)
            pstor%gridPoint = pbr%grd(pstor%local_grid_index)
         endif
      enddo
   end subroutine set_retention_grid_numbers
   
   !> For sediment transport on each node a cross section is required
   !! This subroutine checks this requirement and generates a 
   subroutine set_cross_sections_to_gridpoints()
   
      use unstruc_channel_flow
      use m_flowgeom
      use m_sediment
      use messageHandling

      implicit none

      integer :: L
      integer :: ibr
      integer :: nbr, upointscount, pointscount
      integer :: storageCount
      integer :: i, j, jpos, linkcount
      integer :: k1, k2, igrid
      integer :: c1, c2
      integer :: storage_count
      type(t_branch), pointer                 :: pbr
      type(t_storage), pointer                :: pstor
      integer, dimension(:), pointer          :: lin
      integer, dimension(:), pointer          :: grd
      double precision, dimension(:), pointer :: offset
      type(t_offset2cross), pointer           :: gpnt2cross(:)                   !< list containing cross section indices per u-location
      type (t_CrossSection), pointer          :: cross1, cross2
      double precision, parameter             :: eps_crs = 1d0                      !< accuracy for determining a cross section lies on a grid point


      ! cross sections (in case of sediment transport every gridpoint requires a unique
      ! cross section)
      if (jased > 0 .and. stm_included) then
         allocate(gridpoint2cross(ndxi))
         gpnt2cross => network%adm%gpnt2cross
         do i = 1, ndxi
            gridpoint2cross(i)%num_cross_sections = 0
         enddo

         ! allocate space for local cross section numbers on connection nodes
         do i = 1, network%nds%count
            k1 = network%nds%node(i)%gridNumber
            linkcount = nd(k1)%lnx
            allocate(gridpoint2cross(k1)%cross(linkcount))
            gridpoint2cross(k1)%num_cross_sections = linkcount
         enddo
         
         igrid = 0
         do ibr = 1, nbr
            pbr => network%brs%branch(ibr)
            lin => pbr%lin
            grd => pbr%grd
            offset => pbr%gridPointsOffsets
            pointscount = pbr%gridPointsCount
            do i = 1, pointscount
               igrid = igrid+1
               k1 = grd(i)
               if (i==1 .or. i==pointscount) then
                  ! search for correct location
                  if (i==1) then 
                     L = lin(1)
                  else
                     L = lin(pointscount-1)
                  endif
                  do j = 1,nd(k1)%lnx
                     if (L == iabs(nd(k1)%ln(j))) then
                        jpos = j
                     endif
                  enddo
               else
                  allocate(gridpoint2cross(k1)%cross(1))
                  gridpoint2cross(k1)%num_cross_sections = 1
                  jpos = 1
               endif
               c1 = gpnt2cross(igrid)%c1
               c2 = gpnt2cross(igrid)%c2
               cross1 => network%crs%cross(c1)
               cross2 => network%crs%cross(c2)
               if (abs(offset(i) - cross1%location) < eps_crs) then
                  gridpoint2cross(k1)%cross(jpos) = c1
               elseif (abs(offset(i) - cross2%location) < eps_crs) then
                  gridpoint2cross(k1)%cross(jpos) = c2
               else
                  msgbuf = 'Grid point '//trim(pbr%gridPointIDs(i))//' has no cross section. This is a requirement for sediment transport'
                  call err_flush()
               endif
            enddo
         enddo
      endif
   end subroutine set_cross_sections_to_gridpoints
      
   ! function to store variables related to the nodal relation variables
   subroutine save_1d_nrd_vars_in_stm
      use m_branch
      use m_node
      use m_sediment, only: sedtra, stmpar
      use unstruc_channel_flow
      use morphology_data_module, only : t_nodefraction, t_noderelation
      use string_module

      implicit none

      integer :: inod, ibr, iFrac, iNodeRel, directionLink = 0
      type(t_branch), pointer :: pbr
      type(t_node)  , pointer :: pnod
      type(t_nodefraction), pointer          :: pFrac
      type(t_noderelation),pointer           :: pNodRel

      if (network%brs%Count > 0) then
          do iFrac = 1, stmpar%nrd%nFractions
              pFrac => stmpar%nrd%nodefractions(iFrac)
              do iNodeRel = 1, pFrac%nNodeRelations
                  pNodRel => pFrac%noderelations(iNodeRel)
                  do ibr = 1, network%brs%Count
                      pbr => network%brs%branch(ibr)
                      if (pNodRel%node == pbr%fromNode%id) then
                          pNodRel%nodeIdx = pbr%fromNode%gridnumber
                      endif
                      if (pNodRel%node == pbr%toNode%id) then
                          pNodRel%nodeIdx = pbr%toNode%gridnumber
                      endif
                      if (pNodRel%BranchIn == pbr%id) then
                          if (pNodRel%node == pbr%fromNode%id) then
                              pNodRel%BranchInLn = pbr%lin(pbr%upoints(1))    ! (negative = at start of branch)
                          elseif (pNodRel%node == pbr%toNode%id) then
                              pNodRel%BranchInLn = pbr%lin(pbr%upoints(2))     ! (positive = at end of branch)
                          endif
                      endif
                      if (pNodRel%BranchOut1 == pbr%id) then
                          if (pNodRel%node == pbr%fromNode%id) then
                              pNodRel%BranchOut1Ln = pbr%lin(pbr%upoints(1))  ! (negative = at start of branch)
                          elseif (pNodRel%node == pbr%toNode%id) then
                              pNodRel%BranchOut1Ln = pbr%lin(pbr%upoints(2))   ! (positive = at end of branch)
                          endif
                      endif
                      if (pNodRel%BranchOut2 == pbr%id) then
                          if (pNodRel%node == pbr%fromNode%id) then
                              pNodRel%BranchOut2Ln = pbr%lin(pbr%upoints(1))  ! (negative = at start of branch)
                          elseif (pNodRel%node == pbr%toNode%id) then
                              pNodRel%BranchOut2Ln = pbr%lin(pbr%upoints(2))   ! (positive = at end of branch)
                          endif
                      endif
                  enddo
              enddo
          enddo
      endif

   end subroutine save_1d_nrd_vars_in_stm

   !> 
   subroutine set_structure_indices()
   end subroutine set_structure_indices

end module m_oned_functions