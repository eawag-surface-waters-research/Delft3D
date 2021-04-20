!> inverse-map smoother in orthogonalisenet
module m_inverse_map

   type tops                                                           !< operator type
      double precision, allocatable, dimension(:, :) :: Az             !< cell-center coefficient matrix; node-to-cell
      double precision, allocatable, dimension(:, :) :: Gxi, Geta      !< netcell-gradient coefficient matrices; node-to-link
      double precision, allocatable, dimension(:)    :: Divxi, Diveta  !< netnode-gradient coefficient matrices; link-to-node
      double precision, allocatable, dimension(:)    :: Jxi, Jeta      !< netnode-gradient coefficient matrices; node-to-node
      double precision, allocatable, dimension(:)    :: ww2            !< weights in Laplacian smoother
   end type

   type tadm                                                           !< administration type (per node)
      integer                                        :: Ncell          !< number of netcells connected to the center node
      integer,          allocatable, dimension(:)    :: icell          !< netcells connected to node k0
      integer                                        :: nmk
      integer                                        :: nmk2
      integer,          allocatable, dimension(:)    :: kk2            ! local node administration
      integer,          allocatable, dimension(:,:)  :: kkc            ! position of netcell nodes in kk2 array
   end type

   type ttop                                                           !< topology array type (for unique topologies)
      integer,          allocatable, dimension(:)    :: nmk, nmk2      !< stored number of links and nodes in stencil resp.
      double precision, allocatable, dimension(:,:)  :: xi, eta        !< stored node coordinates (xi, eta)
   end type

!---------------------------
!  administration
!---------------------------
   integer                                       :: nmkx               !< maximum number of links connected to center node k0
   integer, save                                 :: nmkx2=4            !< maximum number of nodes in stencil
   integer, allocatable, dimension(:)            :: nmk2               !< number of nodes in stencil
   integer, allocatable, dimension(:,:)          :: kk2                !< node administration; first row, i.e. kk2(1,:), points to center nodes

!---------------------------
!  unique topologies
!---------------------------
   integer                                        :: numtopo           !< number of unique topologies
   integer,          allocatable, dimension(:)    :: ktopo             !< index in topology array

!--------------------------------------------
!  parameters
!--------------------------------------------
   integer,          parameter                    :: M=6               !< maximum number of nodes per netcell

end module m_inverse_map
