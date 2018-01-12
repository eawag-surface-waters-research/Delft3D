   module m_ggeo_missing
   implicit none
   double precision                  :: dmiss           = -999d0      !
   double precision                  :: xymis           = -999d0      !
   double precision                  :: dxymis          = -999d0
   !double precision                 :: ieee_negative_inf = -1.7976931348623158e+308 ! IEEE standard for the maximum negative value
   integer                           :: intmiss         = -2147483647 ! integer fillvlue
   integer                           :: LMOD, KMOD                    ! TBV READDY
   integer                           :: jins            = 1
   integer                           :: jadelnetlinktyp = 0
   end module m_ggeo_missing


   module m_ggeo_dimens
   implicit none
   integer                       :: MMAX_old = 3, NMAX_old = 3
   integer                       :: KMAX, LMAX, KNX, MXB
   
   contains
   
   function m_ggeo_dimens_destructor() result (ierr)
   
   integer ierr
   MMAX_old = 3
   NMAX_old = 3
   KMAX = 0
   LMAX = 0
   KNX   = 0
   MXB  = 0
   
   ierr = 0
   end function m_ggeo_dimens_destructor

   end module m_ggeo_dimens

   module m_ggeo_polygon
   implicit none
   double precision, allocatable  :: XPL (:), YPL (:), ZPL (:), XPH(:), YPH(:), ZPH(:), DZL(:), DZR(:), DCREST(:), DTL(:), DTR(:), DVEG(:)
   integer, allocatable           :: IWEIRT(:)
   integer                        :: NPL, NPH, MAXPOL, MP, MPS, jakol45 = 0
   character(len=64), allocatable :: nampli(:) ! Names of polylines, set in reapol,
   ! not shifted/updated during editpol.

   end module m_ggeo_polygon

   MODULE M_GGEO_TRIANGLE
   implicit none
   double precision, ALLOCATABLE :: XCENT(:), YCENT(:)
   INTEGER, ALLOCATABLE :: INDX(:,:)
   INTEGER, ALLOCATABLE :: EDGEINDX(:,:)
   INTEGER, ALLOCATABLE :: TRIEDGE(:,:)
   INTEGER              :: NUMTRI
   INTEGER              :: NUMTRIINPOLYGON
   INTEGER              :: NUMEDGE
   INTEGER, PARAMETER   :: ITYPE = 2 ! 1 = ORIGINAL FORTRAN ROUTINE, 2 = NEW C ROUTINE

   integer                       :: jagetwf = 0    ! if 1, also assemble weightfactors and indices in:
   INTEGER, ALLOCATABLE          :: indxx(:,:)     ! to be dimensioned by yourselves 3,*
   double precision, ALLOCATABLE :: wfxx (:,:)

   double precision     :: TRIANGLEMINANGLE =  5d0 ! MINIMUM ANGLE IN CREATED TRIANGLES  IF MINANGLE > MAXANGLE: NO CHECK
   double precision     :: TRIANGLEMAXANGLE =  150 ! MAXIMUM ANGLE IN CREATED TRIANGLES
   double precision     :: TRIANGLESIZEFAC  =  1.0 ! TRIANGLE SIZEFACTOR, SIZE INSIDE VS AVERAGE SIZE ON POLYGON BORDER

   TYPE T_NODI
      INTEGER              :: NUMTRIS       ! total number of TRIANGLES ATtached to this node
      INTEGER, allocatable :: TRINRS(:)     ! numbers of ATTACHED TRIANGLES
   END TYPE T_NODI

   TYPE (T_NODI), DIMENSION(:), ALLOCATABLE :: NODE          !

   !integer, dimension(:,:), allocatable :: trinods ! triangle nodes, dim(3,numtri)
   integer, dimension(:,:), allocatable :: LNtri  ! triangles connected to edges, dim(2,numedges)

   integer                              :: IDENT   ! identifier
   integer, dimension(:),   allocatable :: imask   ! mask array for triangles

   END MODULE M_GGEO_TRIANGLE

   !-----------------------------------------------------!
   !mflow geom
   !-----------------------------------------------------!
   !> in m_ggeo_flowgeom: nd and ln apply to waterlevel nodes and links
   !! in m_netw    : nod and lin apply to 'grid' or 'net' nodes and links
   module m_ggeo_flowgeom
   implicit none
   ! node (s) related : dim=ndx
   type tnode                                          !< node administration
      integer                         :: lnx            !< max nr of links attached to this node
      integer, allocatable            :: ln (:)         !< linknrs attached to this node, >0: to this flownode, <0: from this flownode

      integer, allocatable            :: nod(:)         !< Mapping to net nodes
      double precision, allocatable   :: x  (:)         !< for now, this is only for quick/aligned plotting, the corners of a cell
      double precision, allocatable   :: y  (:)         !< for now, this is only for quick/aligned plotting, the corners of a cell
   end type tnode

   double precision                  :: bamin          !< minimum 2D cell area
   double precision                  :: bamin1D        !< minimum cell area 1d nodes
   double precision                  :: dxmin=1d-3     !< minimum link length 1D (m)
   double precision                  :: dxmin1D        !< minimum link length 1D (m)
   double precision                  :: dxmin2D        !< minimum link length 2D (m)

   double precision                  :: wu1DUNI        !< uniform 1D profile width
   double precision                  :: hh1DUNI        !< uniform 1D profile height

   integer                           :: ja1D2Dinternallinktype = 1

   ! Flow node numbering:
   ! 1:ndx2D, ndx2D+1:ndxi, ndxi+1:ndx1Db, ndx1Db:ndx
   ! ^ 2D int ^ 1D int      ^ 1D bnd       ^ 2D bnd ^ total
   integer, target                   :: ndx2d          !< [-] Number of 2D flow cells (= NUMP). {"rank": 0}
   integer, target                   :: ndxi           !< [-] Number of internal flowcells  (internal = 2D + 1D ). {"rank": 0}
   integer, target                   :: ndx1db         !< [-] Number of flow nodes incl. 1D bnds (internal 2D+1D + 1D bnd). {"rank": 0}
   integer, target                   :: ndx            !< [-] Number of flow nodes (internal + boundary). {"rank": 0}
   type (tnode),     allocatable     :: nd(:)          !< (ndx) flow node administration
   integer,          allocatable     :: kcs(:)         !< node code permanent
   integer,          allocatable, target :: kfs(:)     !< [-] node code flooding {"shape": ["ndx"]}
   integer,          allocatable, target :: kfst0(:)   !< [-] node code flooding {"shape": ["ndx"]}
   double precision, allocatable, target :: ba (:)     !< [m2] bottom area, if < 0 use table in node type {"location": "face", "shape": ["ndx"]}
   double precision, allocatable         :: ba0(:)     ! Backup of ba
   double precision, allocatable     :: bai(:)         !< inv bottom area (m2), if < 0 use table in node type
   double precision, allocatable, target :: bl(:)      !< [m] bottom level (m) (positive upward) {"location": "face", "shape": ["ndx"]}
   double precision, allocatable, target :: xz (:)     !< [m/degrees_east] waterlevel point / cell centre, x-coordinate (m) {"location": "face", "shape": ["ndx"]}
   double precision, allocatable         :: xz0(:)     !< backup of xz
   double precision, allocatable, target :: yz (:)     !< [m/degrees_north] waterlevel point / cell centre, y-coordinate (m) {"location": "face", "shape": ["ndx"]}
   double precision, allocatable         :: yz0(:)     !< backup of yz
   double precision, allocatable     :: aif(:)         !< cell based skewness ai factor sqrt(1+(dz/dy)**2) = abed/asurface
   !< so that cfu=g(Au/conveyance)**2 = g*aif*(Au/convflat)**2
   !< convflat is flat-bottom conveyance
   double precision, allocatable     :: aifu(:)        !< bed skewness at u point (Lnx)
   double precision, allocatable     :: bz(:)          !< averaged bed level at cell center (Ndx)

   ! link (u) related : dim = lnx
   ! Flow link numbering:
   ! 1:lnx1d, lnx1d+1:lnxi, lnxi+1:lnx1Db, lnx1Db+1:lnx
   ! ^ 1D int ^ 2D int      ^ 1D bnd       ^ 2D bnd ^ total
   integer, target                   :: lnx1D          !< [-] nr of 1D flow links (so first 1D, next 2D, next boundaries). {"rank": 0}
   integer, target                   :: lnxi           !< [-] nr of flow links (internal, 1D+2D    ). {"rank": 0}
   integer, target                   :: lnx1Db         !< [-] nr of flow links including 1D bnds (internal, 1D+2D, boundary: only 1D. 2D bnd behind it). {"rank": 0}
   integer, target                   :: lnx            !< [-] nr of flow links (internal + boundary). First we have 1D links, next 2D links, next boundary links (first 1D, then 2D). {"rank": 0}
   integer,          allocatable, target   :: ln    (:,:)    !< [-] 1D link (2,*) node   administration, 1=nd1,  2=nd2   linker en rechter celnr {"shape": [2, "lnkx"]}
   integer,          allocatable, target   :: lncn  (:,:)    !< [-] 2D link (2,*) corner administration, 1=nod1, 2=nod2  linker en rechter netnr {"shape": [2, "lnkx"]}
   integer,          allocatable     :: kcu   (:)      !< link code, 1=1D link, 2=2D link, -1= bc 1D, -2=bc 2D, 3=2D parall wall, 4=1D2Dlink, 5=Pump
   integer,          allocatable, target :: iadv(:)    !< [-] type of advection for this link {"location": "edge", "shape": ["lnx"]}
   double precision, allocatable     :: teta  (:)      !< link teta (m)
   integer,          allocatable     :: klnup (:,:)    !< link upwind cell pointer if q> 0 use (1:3,L), else (4:6,L)
   double precision, allocatable, target :: dx    (:)      !< [m] link length (m) {"location": "edge", "shape": ["lnx"]}
   double precision, allocatable     :: dxi   (:)      !< inverse dx
   double precision, allocatable, target :: wu(:)      !< [m] link initial width (m), if < 0 pointer to convtab {"location": "edge", "shape": ["lnx"]}
   double precision, allocatable     :: wui   (:)      !< inverse link initial width (m), if < 0 pointer to convtab
   double precision, allocatable     :: prof1D (:,:)   !< dim = (3,lnx1D) 1= 1D prof width, 2=1D profile height, 3=proftyp, or: if 1,2< 0, pointers to prof 1,2, then 3=alfa1
   integer,          allocatable     :: jaduiktmp(:)  !< temparr
   double precision, allocatable, target     :: bob   (:,:)    !< [m] left and right inside lowerside tube (binnenkant onderkant buis) HEIGHT values (m) (positive upward) {"location": "edge", "shape": [2, "lnx"]}
   integer,          allocatable     :: ibot  (:)      !< local ibedlevtype for setting min or max network depths (temporary, result goes to bobs)

   double precision, allocatable     :: acl   (  :)    !< left dx fraction, alfacl
   double precision, allocatable     :: acn   (:,:)    !< 2,L left and right wu fraction
   double precision, allocatable     :: xu    (:)      !< velocity point x (m)
   double precision, allocatable     :: yu    (:)      !< velocity point y (m)
   double precision, allocatable     :: blu   (:)      !< velocity point bottom level positive up (m)
   double precision, allocatable     :: csu   (:)      !< cosine comp of u0, u1
   double precision, allocatable     :: snu   (:)      !< sine   comp of u0, u1
   double precision, allocatable     :: wcl   (:,:)    !< link weights (2,lnx) for center scalar , 1,L for k1, 2,L for k2 Ln
   double precision, allocatable     :: wcLn  (:,:)    !< link weights (2,lnx) for corner scalar , 1,L for k3, 2,L for k4 Lncn
   double precision, allocatable     :: wcx1(:)        !< link weights (lnx) for corner velocities k3
   double precision, allocatable     :: wcy1(:)        !< link weights (lnx) for corner velocities k3
   double precision, allocatable     :: wcx2(:)        !< link weights (lnx) for corner velocities k4
   double precision, allocatable     :: wcy2(:)        !< link weights (lnx) for corner velocities k4
   double precision, allocatable     :: wcnx3(:)       !< link weights (lnx) for corner velocities k3
   double precision, allocatable     :: wcny3(:)       !< link weights (lnx) for corner velocities k3
   double precision, allocatable     :: wcnx4(:)       !< link weights (lnx) for corner velocities k4
   double precision, allocatable     :: wcny4(:)       !< link weights (lnx) for corner velocities k4

   double precision, allocatable     :: csb(:,:)       !< cosine orientation from left/right neighboring flownode to flowlink, left/right as ln
   double precision, allocatable     :: snb(:,:)       !< sine   orientation from left/right neighboring flownode to flowlink, left/right as ln

   double precision, allocatable     :: csbn(:,:)      !< cosine orientation from left/right netnode to flowlink, left/right as lncn
   double precision, allocatable     :: snbn(:,:)      !< sine   orientation from left/right netnode to flowlink, left/right as lncn

   double precision, allocatable     :: slnup (:,:)    !< link upwind cell weight, if q> 0 use (1:3,L), else (4:6,L)
   double precision, allocatable     :: csbup (:,:)    !< cosine orientation from upwind cell to flowlink
   double precision, allocatable     :: snbup (:,:)    !< sine   orientation from upwind cell to flowlink

   double precision, allocatable     :: csbw(:,:)      !< cosine orientation from left/right flowlink to wall (netlink), left/right as in walls(10,:) (left), walls(11,:) (right)
   double precision, allocatable     :: snbw(:,:)      !< sine   orientation from left/right flowlink to wall (netlink), left/right as in walls(10,:) (left), walls(11,:) (right)

   double precision, allocatable     :: csbwn(:)       !< cosine orientation from flownode to wall (netlink)
   double precision, allocatable     :: snbwn(:)       !< sine   orientation from flownode to wall (netlink)

   integer,          allocatable     :: ln2lne(:)      !< flowlink to netlink nr dim = lnx
   integer,          allocatable     :: lne2ln(:)      !< netlink to flowlink nr dim = numL


   ! cell corner related, the links attached to a cell corner
   type tcorn                                          !< corner administration
      integer                         :: lnx            !< max nr of links attached to this corner
      integer, allocatable            :: ln (:)         !< linknrs attached to this corner
      integer                         :: nwx            !< nr of walls attached
      integer, allocatable            :: nw(:)          !< wallnrs attached to this corner

   end type tcorn                                      !< corner administration

   type(tcorn)     , allocatable     :: cn  (:)        !< cell cornerpoints, (in counting order of nod)
   double precision, allocatable     :: ucnx(:)        !< cell corner velocity, global x-dir (m/s)
   double precision, allocatable     :: ucny(:)        !< cell corner velocity, global y-dir (m/s) (in m_ggeo_flowgeom...)
   double precision, allocatable, target :: vort(:)        !< [s-1] vorticity at netnodes {"shape": ["ndx"], "comment": "Currently not available, is nowhere allocated nor filled."}


   ! fixed wall related, may be expanded to closed internal walls later for now, dim=(7,*)
   integer                            :: mxwalls            !< max nr of walls
   double precision, allocatable      :: walls(:,:)     !< 1,* : inside waterlevel point (node)
   !! 2,* : first  cornerpoint
   !! 3,* : second cornerpoint
   !! 4,* : flow link 1 attached to first  cornerpoint
   !! 5,* : flow link 2 attached to second cornerpoint
   !! 6,* : stress contribution to link 1
   !! 7,* : stress contribution to link 1
   integer                            :: nwcnx          !< max nr of cornerpoints to which walls are attached
   integer,          allocatable      :: nwalcnw(:,:)   !< pointer to those walls, 1 = left wall, 2 =right wall

   ! closed wall corner (netnode) related
   integer                            :: nrcnw          !< nr of cn points attached to 2 closed walls
   integer         , allocatable      ::  kcnw (:)      !< closed corner point nr k, reference to net nodes
   real            , allocatable      :: cscnw (:)      !< closed corner alignment cos (1:nrcnw)
   real            , allocatable      :: sncnw (:)      !< closed corner alignment sin (1:nrcnw)
   real            , allocatable      :: sfcnw (:)      !< closed corner partial slip sf = u*/u  (based on walls average)

   ! branch related :
   type tbranch                                        !< this is a branch type
      integer                         :: nx             !< with nx links and nx + 1 nodes in it
      integer, allocatable            :: ln (:)         !< successive flow linknrs
   end type tbranch

   integer                           :: mxflowbr       !< max nr of flow branches
   type(tbranch), allocatable        :: flowbr(:)      !< this is a list of flow branches


   integer, allocatable              :: Lbnd1D(:)      !< for prof1D, boundary links refer to regular attached 1D links

   ! 1D endnode related
   integer                           :: mx1Dend        !< nr of 1D endnodes
   integer,          allocatable     :: n1Dend(:)      !< node nrs of 1D endnodes


   ! netnode/flownode  related, dim = mxban
   double precision, allocatable     :: banf  (:)     !< horizontal netnode/flownode area (m2)
   double precision, allocatable     :: ban  (:)      !< horizontal netnode          area (m2)
   integer         , allocatable     :: nban  (:,:)   !< base area pointers to banf, 1,* = netnode number, 2,* = flow node number
   integer                           :: mxban         !< max dim of ban


   ! useful parameters :
   double precision                  :: rrtol            !< relative cellsize factor in search tolerance ()
   double precision, allocatable     :: xyen(:,:)        !< temp boundary opposite point (end of EdgeNormal) (replaces ebtol tolerance)
   integer                           :: jarenumber       !< renumberFlowNodes
   integer                           :: jaFlowNetChanged !< To enforce various net(link)-related init routines after renumbering


   ! JRE Stuff related to setting up wave directional grid
   integer                                     :: ntheta          !< Number of wave direction bins
   double precision                            :: thetamax        !< upper limit wave directional sector
   double precision                            :: thetamin        !< lower limit wave directional sector
   double precision                            :: thetanaut       !< nautical convention or not
   double precision                            :: dtheta          !< directional resolution
   double precision                            :: theta0          !< mean theta-grid direction
   double precision, allocatable               :: thetabin(:)           !< bin-means of theta-grid

   ! Villemonte calibration coefficients :
   double precision                            :: VillemonteCD1 = 1.0d0      !< default for VillemonteCD1 = 1
   double precision                            :: VillemonteCD2 = 10.0d0     !< default for VillemonteCD2 = 10

   ! Debug parameter
   integer                                     :: jabatch  = 0       !< dobatch
   integer                                     :: cmd_icgsolver = 4  !< save commandline icgsolver
   end module m_ggeo_flowgeom

   module M_ggeo_afmeting
   implicit none
   double precision :: RLENGTH, RWIDTH, RTHICK, RDIAM, RLMIN
   integer :: JVAST, MC, NC, K0, LFAC
   end module M_ggeo_afmeting

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

module m_ggeo_WEARELT
   double precision :: XMIN,YMIN,XMAX,YMAX,X1,Y1,X2,Y2,RCIR,CR,DSIX
   END module m_ggeo_WEARELT 
   
