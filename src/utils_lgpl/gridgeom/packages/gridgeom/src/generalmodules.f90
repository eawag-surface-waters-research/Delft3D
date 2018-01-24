   !modules from modules.f90
   module m_dimens
   implicit none
   integer                       :: MMAX_old = 3, NMAX_old = 3
   integer                       :: KMAX, LMAX, KNX, MXB

   contains

   function m_dimens_destructor() result (ierr)

   integer ierr
   MMAX_old = 3
   NMAX_old = 3
   KMAX = 0
   LMAX = 0
   KNX   = 0
   MXB  = 0

   ierr = 0
   end function m_dimens_destructor

   end module m_dimens

   module m_landboundary
   implicit none
   double precision, allocatable :: XLAN (:), YLAN(:), ZLAN(:)
   integer, allocatable          :: NCLAN(:)
   integer                       :: MXLAN, MAXLAN

   ! SPvdP: segments
   integer                              :: MXLAN_loc        ! actual MXLAN
   integer                              :: Nlanseg          ! number of land boundary segments
   integer, allocatable, dimension(:,:) :: lanseg_startend  ! segment start and end indices,          dim(2,Nlanseg)
   integer, allocatable, dimension(:)   :: lanseg_map       ! node to land boundary segment mapping,  dim(numk)

   integer                              :: jleft, jright    !< outer land boundary segments in projection

   double precision                     :: rLleft, rLright  !< fractional location of the projected outer nodes (min and max) on the land boundary segment

   double precision                     :: DCLOSE_bound = 5d0 !< close-to-landboundary tolerance, netbound only, measured in number of meshwidths
   double precision                     :: DCLOSE_whole = 1d0 !< close-to-landboundary tolerance, whole network, measured in number of meshwidths

   double precision                     :: DCLOSE = 1d0       ! close-to-landboundary tolerance, measured in number of meshwidths

   logical                              :: Ladd_land = .true. ! add land boundary between land boundary segments that are close to each other
   end module m_landboundary

   module m_missing
   implicit none
   double precision                  :: dmiss           = -999d0      !
   double precision                  :: xymis           = -999d0      !
   double precision                  :: dxymis          = -999d0
   !double precision                 :: ieee_negative_inf = -1.7976931348623158e+308 ! IEEE standard for the maximum negative value
   integer                           :: intmiss         = -2147483647 ! integer fillvlue
   integer                           :: imiss           = -999        ! cf_dll missing value 
   integer                           :: LMOD, KMOD                    ! TBV READDY, LC gui related variables can go to unstruc_display
   integer                           :: jins            = 1
   integer                           :: jadelnetlinktyp = 0
   end module m_missing


   module m_sferic
   implicit none
   integer                           :: jsferic = 0        ! xy pair is in : 0=cart, 1=sferic coordinates
   integer                           :: jsfertek= 0        ! drawn in 0=cart, 1=stereografisch
   integer                           :: jasfer3D = 0      ! 0 = org, 1 = sqrt(dx2+dy2+dz2), 2= greatcircle
   integer                           :: jglobe  = 0       ! if (jsferic==1) do we need extra tests for 360-0 transgression
   double precision                  :: pi                ! pi
   double precision                  :: twopi             ! 2pi
   double precision                  :: dg2rd             ! degrees to radians
   double precision                  :: rd2dg             ! and vice versa
   double precision                  :: ra = 6378137d0    ! earth radius (m)
   double precision                  :: omega             ! earth angular velocity (rad/s)
   double precision                  :: fcorio            ! 2omegasinfi
   double precision                  :: anglat = 0d0      ! 26.0     ! dubai 52.5     ! angle of latitude  (horizontal)
   double precision                  :: anglon = 0d0      ! 26.0     ! dubai 52.5     ! angle of longitude (vertical)
   double precision                  :: dy2dg             ! from dy in m to lat in degrees
   double precision                  :: csphi             ! cosphi of latest requested

   double precision, parameter       :: dtol_pole = 1d-4   ! pole tolerance in degrees
   end module m_sferic

   module m_polygon
   implicit none
   double precision, allocatable  :: XPL (:), YPL (:), ZPL (:), XPH(:), YPH(:), ZPH(:), DZL(:), DZR(:), DCREST(:), DTL(:), DTR(:), DVEG(:)
   integer, allocatable           :: IWEIRT(:)
   integer                        :: NPL, NPH, MAXPOL, MP, MPS, jakol45 = 0
   character(len=64), allocatable :: nampli(:) ! Names of polylines, set in reapol,
   ! not shifted/updated during editpol.
   double precision               :: dxuni=40d0  ! uniform spacing

   integer                        :: MAXPOLY=1000 ! will grow if needed
   double precision, allocatable  :: xpmin(:), ypmin(:), xpmax(:), ypmax(:), zpmin(:), zpmax(:)
   integer                        :: Npoly
   integer,          allocatable  :: iistart(:), iiend(:)
   integer,          allocatable  :: ipsection(:)
   end module m_polygon

   !
   ! Stores the coordinates of the cells
   !
   module m_cell_geometry
   ! TODO: UNST-1705: LC: I want ndx2d and ndx back into m_flowgeom, as these are flowgeom and not netgeom. Only findcells and update_cell_circumcenters need a change first.
   integer, target                       :: ndx2d          !< [-] Number of 2D flow cells (= NUMP). {"rank": 0}
   integer, target                       :: ndx            !< [-] Number of flow nodes (internal + boundary). {"rank": 0}
   double precision, allocatable, target :: xz (:)     !< [m/degrees_east] waterlevel point / cell centre, x-coordinate (m) {"location": "face", "shape": ["ndx"]}
   double precision, allocatable         :: xz0(:)     !< backup of xz
   double precision, allocatable, target :: yz (:)     !< [m/degrees_north] waterlevel point / cell centre, y-coordinate (m) {"location": "face", "shape": ["ndx"]}
   double precision, allocatable         :: yz0(:)     !< backup of yz
   double precision, allocatable, target :: ba (:)     !< [m2] bottom area, if < 0 use table in node type {"location": "face", "shape": ["ndx"]}
   double precision, allocatable         :: ba0(:)     ! Backup of ba
   ! TODO: UNST-1705: LC: the above variables used to be automatically available in the dflowfm BMI, via the JSON annotated documentation string, this is now broken, needs fixing.

   end module m_cell_geometry


   module M_afmeting
   implicit none
   double precision :: RLENGTH, RWIDTH, RTHICK, RDIAM, RLMIN
   integer :: JVAST, MC, NC, K0, LFAC
   end module M_afmeting

   !> Orthogonalisation settings, both for regular grids and unstructured nets.
   module m_ggeo_orthosettings
   implicit none
   integer          :: ITATP = 25  !< Nr. of outer    iterations in grid/net orthogonalisation.
   integer          :: ITBND = 1   !< Nr. of boundary iterations in grid/net orthogonalisation. (within ITATP)
   integer          :: ITIN  = 25  !< Nr. of inner    iterations in grid/net orthogonalisation. (within ITBND)
   !! Also used within transfinite regular grid generation.
   integer          :: JAPROJECT = 1 !< Project nodes back to boundary (2: yes, all, 1:yes, net bounds only, 0:no)
   double precision :: ATPF = 0.95d0  !< Factor (0.<=ATPF<=1.) between grid smoothing and grid ortho resp.
   double precision :: ATPF_B = 1d0 !< minimum ATPF on the boundary
   double precision :: circumormasscenter = 1d0          !< 1.0 = circumcentre,      0.0 = masscentre, 1.0 -> 0.0 : weighted
   double precision :: smoothorarea    = 1d0   !< Factor between smoother (1d0) and area-homogenizer (0d0)
   integer          :: adapt_method    = 1     !< Mesh-adaptation method; 0: Winslow, 1: arc-length, 2: harmonic map
   double precision :: adapt_beta      = 0.0d0 !< Mesh-refinement factor; between 0d0 and 1d0
   integer          :: adapt_niter_u   = 0     !< number of smoothing iterations of `solution` u in adaptation
   integer          :: adapt_niter_G   = 4     !< number of smoothing iterations of monitor matrix G in adaptation
   double precision :: ortho_pure      = 0.5d0   !< curvi-linear-like (0d0) or pure (1d0) orthogonalisation

   end module m_ggeo_orthosettings

   module m_WEARELT
   double precision :: XMIN,YMIN,XMAX,YMAX,X1,Y1,X2,Y2,RCIR,CR,DSIX
   END module m_WEARELT
