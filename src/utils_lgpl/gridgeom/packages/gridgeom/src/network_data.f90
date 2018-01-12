module network_ggeo_data 

  use m_ggeo_dimens
  !LC use m_ggeo_landboundary
  use m_ggeo_polygon, only: NPL
  implicit none
  
  !> Type tnod describes connectivity for a net node (connected net links).
  !!
  !! For each link kn(1:2, L)==(/ k1, k2 /) there exist two entries:\n
  !! nod(k1)%lin(..) = L\n
  !! nod(k2)%lin(..) = L
  type tnod
     integer, ALLOCATABLE          :: lin(:)          !< Link nrs (==index in kn(:,L)).
  end type tnod

  type (tnod), allocatable         :: nod (:)         !< (numk) Net node connectivity.
  type (tnod), allocatable         :: nod0(:)         !< Backup for nod.


  !> Type tface describes a 'netcell', a cell with net nodes as vertices.
  type tface
    integer                        :: n               !< nr of nodes
    integer, allocatable           :: nod(:)          !< node nrs
    integer, allocatable           :: lin(:)          !< link nrs, kn(1 of 2,netcell(n)%lin(1)) =  netcell(n)%nod(1)
  end type tface
  type (tface), allocatable        :: netcell(:)      !< (nump1d2d) 1D&2D net cells (nodes and links)
  type (tface), allocatable        :: netcell0(:)     ! backup of netcell
  integer,  allocatable            :: cellmask(:)     !< (nump) Mask array for net cells

  double precision, allocatable, target :: xzw(:)  !< [m] centre of gravity {"shape": ["nump"]}
  double precision, allocatable         :: xzw0(:)    ! Backup of xzw
  double precision, allocatable, target :: yzw(:)  !< [m] centre of gravity {"shape": ["nump"]}
  double precision, allocatable         :: yzw0(:)    ! Backup of yzw


  ! Net node related
  double precision, allocatable, target :: xk(:) !< [-] Net node x coordinate {"shape": ["numk"]}
  double precision, allocatable, target :: yk(:) !< [-] Net node y coordinate {"shape": ["numk"]}
  double precision, allocatable, target :: zk(:) !< [-] Net node z coordinate {"shape": ["numk"]}
  double precision, allocatable    :: XK0(:), YK0(:), ZK0(:) !< Backup for xk, etc.
  double precision, allocatable    :: XK1(:), YK1(:), ZK1(:) !< Work array for xk, etc.
  real            , allocatable    :: RNOD(:)                !< Placeholder for node values to be displayed.

  real, allocatable    :: netlinkpath_xk(:), netlinkpath_yk(:), netlinkpath_zk(:)
  integer :: numpath
  integer, allocatable :: netlinkpath_end(:)

  integer,  allocatable            :: NMK (:)         !< (numk) Nr. of neighbouring netnodes for each netnode (ubound for nod(k)%lin).
  integer,  allocatable            :: KC  (:)         !< (numk) Mask array for net nodes.
  integer,  allocatable            :: NMK0(:)         !< Backup for nmk.
  integer,  allocatable            :: KC0 (:)         !< Backup for kc.
  integer,  allocatable            :: NB  (:)         !< (numk) Node codes (corner/boundary/internal classification)

  ! Net link related :
  integer,  allocatable, target    :: kn(:,:)         !< [-] Net links: kn(1,:)=from-idx, kn(2,:)=to-idx, kn(3,:)=net link type (0/1/2/3/4) {"shape": [3, "numl"]}
  integer,  allocatable            :: KN0(:,:)        !< Backup for kn.
  integer,  allocatable            :: LC(:)           !< (numl) Mask array for net links.
  integer,  allocatable            :: LC0 (:)         !< Backup for lc.
  real   , allocatable             :: RLIN(:)         !< (numl) Placeholder for link values to be displayed.
  double precision, allocatable    :: xe(:), ye(:)    !< (numl) Edge (link) center coordinates.
  integer,  allocatable            :: KTRI (:)  , KTON(:)  , KBT (:)

  ! Edge (and cell) related :      ! there are more edges than flow links .....
  integer, allocatable             :: lne(:,:)        !< (2,numl) Edge administration 1=nd1 , 2=nd2, rythm of kn
                                                      !! flow nodes between/next to which this net link lies.
  integer, allocatable             :: lne0(:,:)       ! backup of lne
  integer, allocatable             :: LNN(:)          !< (numl) Nr. of cells in which link participates (ubound for non-dummy values in lne(:,L))
  integer, allocatable             :: LNN0(:)
  integer                          :: NUMK0
  integer                          :: numk             !< [-] Nr. of net nodes. {"shape": []}
  integer                          :: NUML0, NUML     !< Total nr. of net links. In link arrays: 1D: 1:NUML1D, 2D: NUML1D+1:NUML
  integer                          :: NUML1D          !< Nr. of 1D net links.
  integer                          :: NUMP0, NUMP     !< Nr. of 2d netcells.
  integer                          :: nump1d2d        !< nr. of 1D and 2D netcells (2D netcells come first)
  integer                          :: nump1d2d0       !< nr. of 1D and 2D netcells (2D netcells come first)
  integer                          :: KN3TYP = 2      !< Default netlink type (1D/2D).
  integer                          :: jconn = 0

  integer                          :: LNUMK = 0, LNUML = 0

  integer,  allocatable            :: ilin(:)           !< (numl) HELPARRAY for netw2curv
  integer,  allocatable            :: jlin(:)           !< (numl) HELPARRAY for netw2curv
  integer, allocatable             :: linkcross(:,:)    !< (2,nlinkcross) Helparray, pairs of net links that cross each other.
  integer                          :: nlinkcross        !< Nr. of crossing net links detected.
  integer, allocatable             :: linkbadqual(:)    !< (nlinkbadortho+nlinktoosmall) Net link nrs with a bad flow link orthogonality (>cosphiutrsh) or too short (<removesmalllinktrsh*...).
                                                        !! 1:nlinkbadortho for badortho links
                                                        !! nlinkbadortho+1:nlinkbadortho+nlinktoosmall for too short flow links.
  integer                          :: nlinkbadortho = 0 !< Nr. of net links with bad orthogonality detected.
  integer                          :: nlinktoosmall = 0 !< Nr. of net links with too small flow links across them.

  integer                          :: netflow = 2   ! 1=net, 2=flow

  integer                          :: JOCHECKNET = 0

  double precision                 :: zkUNI    = -5d0                   !< Uniform bottom level       (m)
  
!  integer                          :: jacenterinside = 1                !< Force cell center inside or not: 1 = inside, on edge ,  0 = true circumcenter
  double precision                 :: dcenterinside = 1d0               !< Force cell center inside cell with factor dcenterinside, 1: confined by cell edges, 0: at mass center

  double precision                 :: removesmalllinkstrsh = 1d-1       !< 0.0 = remove no links ,  0.1 = remove links smaller than 0.1 sqrt(ba)
                                                                        !< used for removelinks, but *also* in geominit: no flow link created if dx < dxtrsh

  integer                          :: maxfaceallow = 4                  !< Nr. of faces allowed in removesmallflowlinks

  INTEGER                          :: NUMITCOURANT = 0                  !< Nr. of smooth. iter. in Courant network (need samples).

  double precision                 :: SMALLESTSIZEINCOURANT = 100d0     !< Smallest cellsize generated in Courant network.

  double precision                 :: TRIAREAREMFRAC = 0.2d0            !< Triangle to be removed if AREA < AV. ADJACAEREA.

  integer                          :: linmin = 0, linmax = 0, nodmin= 0, nodmax = 0, netcelmax = 0, netcelmin=0

  integer                          :: jathindams = 0                    !< For quick check whether any of the kn(3,:)==0

  integer                          :: M13QUAD = 0                       !< Quad refinement dir 0 = both, 1 =this, -1 = that.

  double precision                 :: cosphiutrsh = 0.5d0               !< No flow network generated if cosphiu > cospiutrsh

  double precision                 :: CORNERCOS   = 0.25d0              !< threshold in makenetnodescoding cornerpoints

  double precision                 :: TOOCLOSE = 0.001d0                !< Network points closer than tooclose are merged

  double precision                 :: CONNECT1DEND = 200d0              !< Merge 1D endpoint ti closest branch point

  double precision                 :: Unidx1D = 100d0                   !< Uniform 1D dx in copylandboundaryto1Dnetw

  integer                          :: makeorthocenters = 0              !< shift from circumcentre to orthocentre (acts as a maxiter)

  double precision                 :: xkmin, xkmax , ykmin, ykmax

! 1d NET BRANCHES
  type tNETbr                                        !< this is a NET branch type
   integer                         :: nx             !< with nx links and nx + 1 nodes in it
   integer, allocatable            :: ln (:)         !< successive NET linknrs
!  parallel: this branch may belong to a larger globally connected branch, distributed in multiple domains
   integer                         :: iconn           !< connected-branch number this branch belongs to
   double precision                :: doff            !< offset length of this branch in global branch
  end type tNETBR

  INTEGER                          :: MXNETBR = 0
  type (tNETBR), allocatable       :: NETBR(:)       !< FOR 1d BRANCHES

  INTEGER, ALLOCATABLE             :: IBN(:), LIB(:), K1BR(:), NRLB(:)  ! TEMPORARY

  integer                          :: lasttopology = 0                     ! network checker fingerprint
  !  UNSWAN course
  integer                          :: jaswan = 0


!  network administration status
  integer, parameter               :: NETSTAT_OK    = 0 !< Network administration up-to-date
  integer, parameter               :: NETSTAT_CELLS_DIRTY = 1 !< Network administration needs findcells call.
  integer                          :: netstat = NETSTAT_CELLS_DIRTY

! keep circumcenters before orthogonalization in case of quadtree meshes
  integer                          :: keepcircumcenters = 0    !< keep circumcenter (1) or not (0)
  
! for dry/illegal/cutcells (mesh generation related only)
  character(len=255)               :: dryptsfile = ''
  
   contains
   
   function network_ggeo_data_destructor() result (ierr)
   
   integer ierr, k

   ! deallocate all arrays
   if (ALLOCATED(nod0)) then
      do k= 1, SIZE(nod0)
         if ( allocated(nod0(k)%lin) ) deallocate(nod0(K)%LIN)
      enddo
      deallocate(nod0)
   endif
   
   if (ALLOCATED(nod)) then
      do k= 1, SIZE(nod)
         if ( allocated(nod(k)%lin) ) deallocate(nod(K)%LIN)
      enddo
      deallocate(nod)
   endif
      
   if(allocated(cellmask)) deallocate(cellmask)
   if(allocated(xzw))      deallocate(xzw)
   if(allocated(xzw0))     deallocate(xzw0)
   if(allocated(yzw))      deallocate(yzw)
   if(allocated(yzw0))     deallocate(yzw0)
   
   if(allocated(xk)) deallocate(xk)
   if(allocated(yk)) deallocate(yk)
   if(allocated(zk)) deallocate(zk)
   if(allocated(XK0)) deallocate(XK0)
   if(allocated(YK0)) deallocate(YK0)
   if(allocated(ZK0)) deallocate(ZK0)
   
   if(allocated(XK1)) deallocate(XK1)
   if(allocated(YK1)) deallocate(YK1)
   if(allocated(ZK1)) deallocate(ZK1)
   if(allocated(RNOD)) deallocate(RNOD)
   
   if(allocated(netlinkpath_xk)) deallocate(netlinkpath_xk)
   if(allocated(netlinkpath_yk)) deallocate(netlinkpath_yk)
   if(allocated(netlinkpath_zk)) deallocate(netlinkpath_zk)
   if(allocated(netlinkpath_end)) deallocate(netlinkpath_end)
   
   if(allocated(NMK)) deallocate(NMK)
   if(allocated(KC)) deallocate(KC)
   if(allocated(NMK0)) deallocate(NMK0)
   if(allocated(KC0)) deallocate(KC0)
   if(allocated(NB)) deallocate(NB)
   
   if(allocated(kn)) deallocate(kn)
   if(allocated(KN0)) deallocate(KN0)   
   if(allocated(LC)) deallocate(LC)
   if(allocated(LC0)) deallocate(LC0)
   if(allocated(RLIN)) deallocate(RLIN)
   if(allocated(xe)) deallocate(xe)
   if(allocated(ye)) deallocate(ye)
   if(allocated(KTRI)) deallocate(KTRI)
   if(allocated(KTON)) deallocate(KTON)
   if(allocated(KBT)) deallocate(KBT)
   if(allocated(lne)) deallocate(lne)
   
   if(allocated(lne0)) deallocate(lne0)
   if(allocated(LNN)) deallocate(LNN)   
   if(allocated(LNN0)) deallocate(LNN0)
   if(allocated(ilin)) deallocate(ilin)
   if(allocated(jlin)) deallocate(jlin)
   if(allocated(linkcross)) deallocate(linkcross)
   if(allocated(linkbadqual)) deallocate(linkbadqual)
   
   if(allocated(NETBR)) deallocate(NETBR)
   if(allocated(IBN)) deallocate(IBN)
   if(allocated(LIB)) deallocate(LIB)
   if(allocated(K1BR)) deallocate(K1BR)  
   if(allocated(NRLB)) deallocate(NRLB)
   
   !if(allocated(KN))   deallocate(KN)
   !if(allocated(LC))   deallocate(LC)
   !if(allocated(RLIN)) deallocate(RLIN)
   !
   !if(allocated(XK)) deallocate(XK)
   !if(allocated(YK)) deallocate(YK)
   !if(allocated(ZK)) deallocate(ZK)
   !
   !if(allocated(RNOD)) deallocate(RNOD)
   !
   !if(allocated(XK0)) deallocate(XK0) 
   !if(allocated(YK0)) deallocate(YK0)    
   !if(allocated(ZK0)) deallocate(ZK0) 
   !
   !if(allocated(NMK))  deallocate(NMK)        
   !if(allocated(KC))   deallocate(KC)       
   !if(allocated(NMK0)) deallocate(NMK0)        
   !if(allocated(KC0))  deallocate(KC0)       
   !if(allocated(NB))   deallocate(NB)     
   
   ! default initialize all variables
   NUMK0     = 0
   numk      = 0            
   NUML0     = 0
   NUML      = 0     
   NUML1D    = 0         
   NUMP0     = 0
   NUMP      = 0     
   nump1d2d  = 0      
   nump1d2d0 = 0      
   KN3TYP    = 2      
   jconn     = 0
   LNUMK     = 0
   LNUML     = 0
   nlinkcross = 0
   nlinkbadortho = 0 
   nlinktoosmall = 0
   netflow = 2  
   JOCHECKNET = 0
   zkUNI    = -5d0                   
   dcenterinside = 1d0               
   removesmalllinkstrsh = 1d-1       
   maxfaceallow = 4                  
   NUMITCOURANT = 0                  
   SMALLESTSIZEINCOURANT = 100d0     
   TRIAREAREMFRAC = 0.2d0            
   linmin = 0
   linmax = 0
   nodmin= 0
   nodmax = 0
   netcelmax = 0
   netcelmin=0
   jathindams = 0                    
   M13QUAD = 0                       
   cosphiutrsh = 0.5d0               
   CORNERCOS   = 0.25d0             
   TOOCLOSE = 0.001d0                
   CONNECT1DEND = 200d0              
   Unidx1D = 100d0                   
   makeorthocenters = 0             
   xkmin = 0
   xkmax = 0
   ykmin = 0
   ykmax = 0
   MXNETBR = 0
   lasttopology = 0 
   jaswan = 0
   netstat = NETSTAT_CELLS_DIRTY
   keepcircumcenters = 0
   
   ! return error
   ierr = 0
   
   end function network_ggeo_data_destructor
  
   end module network_ggeo_data
   