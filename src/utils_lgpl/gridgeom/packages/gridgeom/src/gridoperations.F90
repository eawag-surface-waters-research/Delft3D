   !LC TO DO: PASS CALL-BACK  FUNCTION FOR INTERACTER MESSAGES
   module gridoperations

   implicit none

   !new functions
   public :: make1D2Dinternalnetlinks
   public :: ggeo_convert
   public :: ggeo_convert_1d_arrays
   public :: ggeo_get_links_count
   public :: ggeo_get_links
   public :: ggeo_create_edge_nodes
   public :: ggeo_deallocate
   public :: ggeo_edge_nodes_count

   !from net.f90
   public :: RESTORE
   public :: SAVENET
   public :: INCREASENETW
   public :: increasenetcells
   public :: alreadycell
   public :: SETNEWPOINT
   public :: CROSSED2d_BNDCELL
   public :: OTHERNODE
   public :: OTHERNODECHK
   public :: SETNODADM
   public :: INIALLOCnetcell
   public :: update_cell_circumcenters
   public :: FINDCELLS
   public :: FINDTRIS
   public :: FINDQUADS
   public :: FINDPENTAS
   public :: FINDHEXAS
   public :: iscounterclockwise
   public :: RECHTSAF
   public :: CONNECTDBN
   public :: CONNECTDB
   public :: ADDLINKTONODES
   public :: SETNODLIN
   public :: CHKLINSIZTONODE
   public :: GIVENEWNODENUM
   public :: DRIETWEE
   public :: TWEEDRIE
   public :: DVIEW
   public :: INCELLS
   public :: sort_links_ccw
   public :: get_cellpolygon
   
   ! rest.f90
   public ::INVIEW
   public ::DINVIEW
   
   ! unstruct.F90
   public :: getcellsurface
   public :: getcellweightedcenter

   private

   contains

   !-----------------------------------------------------------------!
   ! net.f90
   !-----------------------------------------------------------------!

   !> Restore variables with backup data
   SUBROUTINE RESTORE()
   use network_data
   implicit none
   integer :: k, ls, ls0, NODSIZ, IERR

   IF ( NUMK0.EQ.0 ) RETURN

   XK (1:NUMK0)  = XK0 (1:NUMK0)
   YK (1:NUMK0)  = YK0 (1:NUMK0)
   ZK (1:NUMK0)  = ZK0 (1:NUMK0)

   NMK(1:NUMK0)  = NMK0(1:NUMK0)
   KC (1:NUMK0)  = KC0 (1:NUMK0)

   KN(:,1:NUML0) = KN0(:,1:NUML0)
   LC(  1:NUML0) = LC0(  1:NUML0)

   NODSIZ = SIZE(NOD)

   DO K = 1,NUMK0
      LS0 = SIZE(NOD0(K)%LIN )  ! LS0 = NMK0(K)
      IF (LS0 .GE. 1) THEN
         ! IF (.NOT. ASSOCIATED(NOD(K)%LIN) ) THEN
         IF (ALLOCATED(NOD(K)%LIN) ) THEN
            LS = SIZE(NOD(K)%LIN )
         ELSE
            LS = 0
         ENDIF
         IF (LS .LT. LS0) THEN
            IF (LS .GE. 1) DEALLOCATE (NOD(K)%LIN )
            ALLOCATE   (NOD(K)%LIN(LS0), STAT = IERR )
            NOD(K)%LIN = 0
         ENDIF
         NOD(K)%LIN(1:LS0) = NOD0(K)%LIN(1:LS0)
      ENDIF
   ENDDO

   NUMK = NUMK0
   NUML = NUML0

   !  need findcells
   netstat = NETSTAT_CELLS_DIRTY
   RETURN
   END SUBROUTINE RESTORE

   SUBROUTINE SAVENET()
   use network_data
   implicit none
   integer :: ierr
   integer :: k, KX, LS, LS0, LX, NN

   if (numk == 0) return

   KX = NUMK
   IF (ALLOCATED(nod0)) THEN
      DO K= 1, SIZE(NOD0)
         if ( allocated(nod0(k)%lin) ) DEALLOCATE(NOD0(K)%LIN)
      ENDDO
      DEALLOCATE(NOD0)
      ALLOCATE ( NOD0(KX) , stat = ierr )
      !CALL AERR('NOD0(KX)', IERR, KX)
   ENDIF

   if (allocated(xk0)) deallocate(xk0,yk0,zk0)
   allocate ( XK0(KX), YK0(KX), ZK0(KX) , STAT=IERR)
   !call aerr('XK0(KX), YK0(KX), ZK0(KX)', IERR, 3*kx)

   if (allocated (KC0) ) deallocate ( KC0 )
   ALLOCATE( KC0(KX), STAT=IERR)

   if (allocated (nmk0) ) deallocate ( NMK0 )
   ALLOCATE( NMK0(KX), STAT=IERR)

   XK0 (1:NUMK) = XK (1:NUMK)
   YK0 (1:NUMK) = YK (1:NUMK)
   ZK0 (1:NUMK) = ZK (1:NUMK)
   KC0( 1:NUMK) = KC (1:NUMK)
   NMK0(1:NUMK) = NMK(1:NUMK)

   IF (ALLOCATED(LC0)) DEALLOCATE(KN0 ,LC0)
   LX = NUML
   ALLOCATE (KN0(3,LX), LC0(LX), STAT=IERR)

   KN0(:,1:NUML) = KN(:,1:NUML)
   LC0(  1:NUML) = LC(  1:NUML)

   DO K   = 1,NUMK
      LS  = NMK(K) ! SIZE(NOD (K)%LIN )
      IF (LS .GE. 1) THEN
         !       IF (.NOT. ASSOCIATED(NOD0(K)%LIN) ) THEN
         IF (.NOT. ALLOCATED(NOD0(K)%LIN) ) THEN
            LS0 = 0
         ELSE
            LS0 = SIZE(NOD0(K)%LIN )
         ENDIF
         IF (LS0 .LT. LS) THEN
            IF (LS0 .GE. 1 .and. allocated(NOD0(K)%LIN)) DEALLOCATE (NOD0(K)%LIN )
            ALLOCATE   (NOD0(K)%LIN(LS) ) ; NOD0(K)%LIN = 0
         ENDIF
         NOD0(K)%LIN(1:LS) = NOD(K)%LIN(1:LS)
      ENDIF
   ENDDO

   NUMK0 = NUMK
   NUML0 = NUML
   RETURN
   END SUBROUTINE SAVENET

   !> Increase the number of net links
   SUBROUTINE INCREASENETW(K0,L0) ! TODO AFMAKEN
   !LC removed use m_netw
   use network_data
   use m_alloc
   use m_missing, only : xymis, dmiss

   implicit none
   integer :: ierr
   integer :: k
   integer :: knxx
   INTEGER :: K0, L0

   if (K0 < KMAX .and. L0 < LMAX) RETURN

   CALL SAVENET()

   IF (KMAX <= K0) THEN
      KMAX = K0 + 100000   ! 2 KAN WEG
      IF (allocated(nod)) then
         do k = 1,size(nod)
            if ( allocated(nod(k)%lin) ) deallocate (nod(k)%lin)
         enddo
         deallocate(nod, xk, yk, zk, kc, nmk, rnod)
      end if
      ALLOCATE ( NOD(KMAX) , STAT = IERR)
      CALL AERR('NOD(KMAX)', IERR, KMAX )
      ALLOCATE ( XK (KMAX), YK (KMAX), ZK (KMAX), KC (KMAX), NMK (KMAX), RNOD(KMAX) , STAT=IERR   )
      CALL AERR('XK (KMAX), YK (KMAX), ZK (KMAX), KC (KMAX), NMK (KMAX), RNOD(KMAX)', IERR, 7*KMAX)

      DO K = 1,KMAX
         IF (K .LE. SIZE(NMK0) ) THEN
            KNXX = MAX(NMK0(K),KNX)
         ELSE
            KNXX = KNX
         ENDIF
         ALLOCATE(NOD(K)%LIN(KNXX) , STAT=IERR) ;
         NOD(K)%LIN = 0
      ENDDO

      NMK = 0 ; KC = 1 ; XK = XYMIS ; YK = XYMIS ; ZK = dmiss
   ENDIF

   IF (LMAX <= L0) THEN
      LMAX = L0 + 3*100000
      IF (SIZE(LC) > 0 .and. allocated(kn)) THEN
         DEALLOCATE(KN ,LC , RLIN)
      ENDIF
      ALLOCATE (KN (3,LMAX), LC (LMAX), STAT=IERR) ; KN = 0 ; LC = 0 ! TODO: AvD: catch memory error
      ALLOCATE (RLIN (LMAX), STAT=IERR)
   ENDIF

   CALL RESTORE()

   END SUBROUTINE INCREASENETW

   !> Increases the global netcell array to a new size.
   !! Will not shrink the array. Specify a growfac > 1.0 to grow in bigger chunks.
   !!
   !! Example:
   !! do
   !!     call increasenetcells(NUMP+1, 1.2, .true.)
   subroutine increasenetcells(numpx, growfac, keepExisting)
   use network_data
   use m_alloc
   implicit none
   integer,          intent(in) :: numpx        !< New maximum size for netcell array.
   real,             intent(in) :: growfac      !< When growing, resize by additional factor growfac*numpx (e.g. 1.2)
   logical,          intent(in) :: keepExisting !< Restore existing data after reallocate, otherwise leave undefined.

   integer :: p, ierr, n0
   integer :: numpx0 !< Current size of netcell
   integer :: numpx1 !< Actual size of to-be-increased netcell
   if (allocated(netcell)) then
      numpx0 = size(netcell)
   else
      numpx0 = 0
   end if

   if (numpx0 >= numpx) return ! Array is still large enough

   numpx1 = max(numpx, ceiling(numpx*growfac)) ! Grow a bigger chunk at once

   ! 1: SAVE
   if (nump > 0 .and. keepExisting) then ! NOTE: Only create backup if nump > 0 (not numpx0 > 0)

      if(allocated(netcell_sav)) then
         !          deallocate netcell_sav
         do p=1,ubound(netcell_sav,1)
            if ( allocated(netcell_sav(p)%nod) ) deallocate(netcell_sav(p)%nod)
            if ( allocated(netcell_sav(p)%lin) ) deallocate(netcell_sav(p)%lin)
         end do
         deallocate(netcell_sav)
      end if

      allocate(netcell_sav(nump), stat = ierr)
      CALL AERR('netcell_sav(nump)', ierr, nump)

      do p=1,nump
         n0 = netcell(p)%n
         if (n0 <= 0) then
            cycle
         end if

         allocate(netcell_sav(p)%nod(n0), netcell_sav(p)%lin(n0), stat = ierr)
         !CALL AERR('netcell_sav(p)%nod(n0), netcell_sav(p)%lin(n0)', ierr, 2*n0)

         netcell_sav(p)%n   = netcell(p)%n
         netcell_sav(p)%nod = netcell(p)%nod
         netcell_sav(p)%lin = netcell(p)%lin

         deallocate(netcell(p)%nod, netcell(p)%lin)
      end do
   end if

   if (allocated(netcell)) then
      deallocate(netcell)
      CALL AERR('netcell', 0, -numpx0)
   end if

   allocate(netcell(numpx1), stat = ierr)
   CALL AERR('netcell(numpx1)', ierr, numpx1)

   ! 2: RESTORE
   if (nump > 0 .and. keepExisting) then ! NOTE: Only restore backup if nump > 0 (not numpx0 > 0)
      do p=1,nump
         n0 = netcell_sav(p)%n
         if (n0 <= 0) then
            cycle
         end if

         allocate(netcell(p)%nod(n0), netcell(p)%lin(n0), stat = ierr)
         !CALL AERR('netcell(p)%nod(n0), netcell(p)%lin(n0)', ierr, 2*n0)

         netcell(p)%n   = netcell_sav(p)%n
         netcell(p)%nod = netcell_sav(p)%nod
         netcell(p)%lin = netcell_sav(p)%lin

         deallocate(netcell_sav(p)%nod, netcell_sav(p)%lin)
      end do

      deallocate(netcell_sav)
      CALL AERR('netcell_sav', 0, -nump)
   end if

   end subroutine increasenetcells

   !> check and see if the links already form a cell
   logical function alreadycell(N, K, L)
   !use m_netw
   use network_data
   implicit none

   integer,               intent(in) :: N  !< number of links and nodes
   integer, dimension(N), intent(in) :: K  !< node set
   integer, dimension(N), intent(in) :: L  !< link set

   integer                           :: i, j
   integer                           :: kL, kR, LL

   integer                           :: num

   integer                           :: knod, kcom

   integer, dimension(N)             :: icell

   integer, dimension(N)             :: dum

   !   search for a link that:
   !     -is a member of the link set,
   !     -bounds two cells which are adjacent to the links in the link set, and
   !     -connects two nodes that are members of the node set
   !
   !   this link will be an internal link in the polygon formed by the links in the link set, hence these links do not form a new cell

   alreadycell = .false.

   do i=1,N
      if ( lnn(L(i)).eq.2 ) return
   end do

   icell = lne(1, L)

   do i=1,N
      if ( lnn(L(i)).lt.1 ) cycle

      do j = 1,netcell(icell(i))%N
         LL = netcell(icell(i))%lin(j)

         if ( lnn(LL).ne.2 ) cycle  ! this also excludes all members of the link set

         kL = lne(1,LL)
         kR = lne(2,LL)

         !        check if both kL and kR are members of the cell set
         dum = 0
         where( icell.eq.kL ) dum = 1
         if ( sum(dum).eq.0 ) cycle   ! kL not a member
         dum = 0
         where( icell.eq.kR ) dum = 1
         if ( sum(dum).eq.0 ) cycle   ! kR not a member

         !        check if the link connects nodes in the node set
         dum = 0
         where( K.eq.kn(1,LL) ) dum =1
         if ( sum(dum).eq.0 ) cycle   ! first node of link not a member
         dum = 0
         where( K.eq.kn(2,LL) ) dum =1
         if ( sum(dum).eq.0 ) cycle   ! second node of link not a member

         alreadycell = .true.
         return
      end do
   end do

   !   check for nodes inside the polygon - to be done

   !   see if a cell contains only triangles that share a node
   kcom = 0
   !  if ( N.eq.4 ) then ! quads only
   do i=1,N
      LL = L(i)

      if ( lnn(LL).lt.1 ) then
         kcom = 0
         exit
      end if

      if ( netcell(icell(i))%N.ne.3 ) then
         kcom = 0
         exit ! triangles only
      end if

      !       find the node of the triangle not on the link
      knod = sum(netcell(icell(i))%nod(1:3)) - kn(1,LL) - kn(2,LL)

      if ( kcom.eq.0 ) then  ! set common node
         kcom = knod
      else if ( knod.ne.kcom ) then  ! check with common node
         kcom = 0
         exit
      end if
   end do
   !  end if

   ! common node found
   if ( kcom.ne.0 ) then
      alreadycell = .true.
      return
   end if


   end function alreadycell

   !> TODO: Document me
   SUBROUTINE SETNEWPOINT(XP,YP,ZP,K1)

   use network_data
   use m_missing, only : dmiss, xymis

   implicit none
   integer :: jav
   integer :: jview
   double precision :: xyz
   double precision :: XP, YP, ZP
   integer :: K1

   COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
   CALL GIVENEWNODENUM(K1)
   CALL TWEEDRIE(XP,YP,XK(K1),YK(K1),ZK(K1))
   IF (JVIEW .EQ. 1) THEN
      ZK(K1) = dmiss ! AvD: Was changed from XYZ to dmiss. TODO: What about other views. Used at all?
   ELSE IF (JVIEW .EQ. 2) THEN
      XK(K1) = XYZ
   ELSE IF (JVIEW .EQ. 3) THEN
      YK(K1) = XYZ
   ENDIF
   IF (KC(K1) .EQ. 0) KC(K1) = 1
   RETURN
   END SUBROUTINE SETNEWPOINT

   SUBROUTINE CROSSED2d_BNDCELL(NML, XP1, YP1, XP2, YP2 , NC1)
   !use m_netw
   use network_data
   use m_missing, only : dmiss
   use geometry_module, only : crossinbox
   use m_sferic, only: jsferic, jasfer3D

   implicit none
   INTEGER          :: NML, NC1
   DOUBLE PRECISION :: XP1, YP1, XP2, YP2

   INTEGER          :: L, JACROS, K1, K2
   DOUBLE PRECISION :: SL, SM, XCR, YCR, CRP, slm

   NC1 = 0
   slm = 1d9
   DO L  = 1,NML
      K1 = KN(1,L) ; K2 = KN(2,L)
      if ( k1.lt.1 .or. k2.lt.1 ) cycle   ! SPvdP: safety
      IF (LNN(L) == 1) THEN       ! LINK MET 1 BUURCEL
         IF (KN(3,L) == 2) THEN
            CALL CROSSinbox (XP1, YP1, XP2, YP2, XK(K1), YK(K1), XK(K2), YK(K2), jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)
            if (jacros == 1) then
               if (sl < slm) then
                  NC1 = LNE(1,L)
                  slm = sl
               endif
            end if
         ENDIF
      ENDIF
   ENDDO

   END SUBROUTINE CROSSED2d_BNDCELL

   SUBROUTINE OTHERNODE(K1,L1,K2)

   use network_data
   implicit none
   integer :: K1, L1, K2

   integer :: ka

   KA = KN(1,L1)
   K2 = KN(2,L1)
   IF (KA .EQ. K1) RETURN
   K2 = KA
   RETURN
   END SUBROUTINE OTHERNODE

   SUBROUTINE OTHERNODECHK(K1,L1,K2)
   use network_data
   implicit none

   integer :: K1, L1, K2

   K2 = 0

   IF (KN(3,L1) .NE. 2 .and. KN(3,L1) .NE. 0) RETURN


   IF (K1 == KN(1,L1)) THEN
      IF (LC(L1) == 1) RETURN
      K2 =  KN(2,L1)
      RETURN
   ENDIF

   IF (LC(L1) == -1) RETURN
   K2 = KN(1,L1)

   RETURN
   END SUBROUTINE OTHERNODECHK

   SUBROUTINE SETNODADM(JACROSSCHECK_)

   use network_data

   use mathconsts, only: degrad_hp
   use m_ec_triangle, only: triangleminangle
   use geometry_module, only: getdx, getdy, dcosphi, cross
   use m_missing, only : dmiss, dxymis
   use m_sferic, only: pi, jsferic, jasfer3D
   use MessageHandling
   use m_alloc

   implicit none
   INTEGER               :: JACROSSCHECK_ !< remove crossed 2D links (1), or not (0), output permutation array (+10)

   double precision :: crp, e, e1
   integer          :: jacros, mout
   integer          :: k, k1, k12, k2, k22, k3, KI, ka, kb, kk, L, L1, L2, LL, LLL, LI, LTOT, ls, JA
   INTEGER          :: jDupLinks, jOverlapLinks, jSmallAng, maxlin
   double precision :: sl, sm, xcr, ycr

   INTEGER, ALLOCATABLE          ::  KC2(:), KN2(:,:), KCK(:)
   double precision, allocatable :: arglin(:)
   integer, allocatable          :: linnrs(:), inn(:)
   double precision              :: phi, dx, dy, dmaxcosp, dcosp, costriangleminangle, phi0

   double precision :: X(4), Y(4)

   double precision, dimension(:), allocatable :: Lperm_new

   integer :: jacrosscheck ! remove 2D crossing netlinks (1) or not (0)
   integer :: japermout ! output permutation array (1) or not (0)

   jacrosscheck = jacrosscheck_
   japermout = 0
   if ( jacrosscheck.ge.10 ) then
      japermout    = 1
      jacrosscheck = jacrosscheck - 10
   end if

   IF (NUML == 0) RETURN

   E = 1E-6 ; E1 = 1-E

   if ( japermout.eq.1 ) then
      !     allocate permutation array
      call realloc(Lperm, numL, keepExisting=.false., fill=0)
      do L=1,numL
         Lperm(L) = L
      end do
      allocate(Lperm_new(numL))
   end if


   IF (JACROSSCHECK == 1) THEN
      LL = 0
      DO L=NUML,1,-1
         K1 = KN(1,L)  ; K2 = KN(2,L); K3 = KN(3, L)
         if (k3 .NE. 2) then
            cycle ! 1D links mogen blijven
         endif

         IF (DINVIEW(XK(K1),YK(K1),ZK(K1)) .OR. DINVIEW(XK(K2),YK(K2),ZK(K2)) ) THEN
            DO LLL = MAX(1,L-1), 1 ,-1
               KA = KN(1,LLL) ; KB = KN(2,LLL)
               ! If interfaces share same node, no further action:
               if (k1 == ka .or. k1 == kb .or. k2 == ka .or. k2 == kb ) cycle
               X(1) = XK(K1)
               Y(1) = YK(K1)
               X(2) = XK(K2)
               Y(2) = YK(K2)
               X(3) = XK(KA)
               Y(3) = YK(KA)
               X(4) = XK(KB)
               Y(4) = YK(KB)
               CALL CROSS(XK(K1), YK(K1), XK(K2), YK(K2), XK(KA), YK(KA), XK(KB), YK(KB), JACROS,SL,SM,XCR,YCR,CRP,jsferic, dmiss)
               IF (SL > E .AND. SL < E1 .AND. SM > E .AND. SM < E1 ) THEN
                  KN(1,L) = 0; KN(2,L) = 0 ; KN(3, L) = -1; EXIT
               ENDIF
            ENDDO
         ENDIF
      ENDDO
   ENDIF

100 continue

   ALLOCATE(KCK(NUMK) )
   if ( .not.allocated(kc) ) then
      allocate(kc(numk))
      kc  = 0
      kck = 0
   else
      KCK(1:NUMK) = KC(1:NUMK)            ! STORE ORG KC
      kc = 0
   end if

   ALLOCATE(KN2(3,NUML)) ; KN2 = 0     ! RESERVE KN


   L2 = 0 ; L1 = 0
   jathindams = 0
   DO L=1,NUML                                                   ! LINKS AANSCHUIVEN, 1d EERST
      K1 = KN(1,L)  ; K2 = KN(2,L) ; K3 = KN(3,L)
      if (k3 == 0) then
         jathindams = 1
      end if
      IF (K1 .NE. 0 .AND. K2 .NE. 0 .AND. K1 .NE. K2 ) THEN
         JA = 1
         IF (XK(K1) == DMISS .OR. XK(K2) == DMISS) THEN          ! EXTRA CHECK: ONE MISSING
            JA = 0
         ELSE IF (XK(K1) == XK(K2) .AND. YK(K1) == YK(K2) ) THEN !            : OR BOTH EQUAL
            JA = 0
         ENDIF
         IF (JA == 1) THEN
            IF (K3 == 0 .or. K3 == 2) THEN
               L2 = L2 + 1
               KN2(1,L2)  = K1 ; KN2(2,L2)  = K2 ; KN2(3,L2)  = K3
               if ( japermout.eq.1 ) then
                  Lperm_new(numL-L2+1) = Lperm(L) ! fill 2D links from the back of the temp. array
               end if
            ELSE IF (K3 == 1 .OR. K3 > 2) THEN
               L1 = L1 + 1
               KN(1,L1) = K1 ; KN(2,L1) = K2 ; KN(3,L1) = K3
               if ( japermout.eq.1 ) then
                  Lperm_new(L1) = Lperm(L) ! fill 1D links from the start of the temp. array
               end if
            ENDIF
            KC(K1)   = 1  ; KC(K2)   = 1
         ENDIF
      ENDIF
   ENDDO

   if ( japermout.eq.1 ) then
      !     copy 1D and flip 2D values from the temp. to the permutation array
      do L=1,L1
         Lperm(L) = Lperm_new(L)
      end do
      do L=1,L2
         Lperm(L1+L) = Lperm_new(numL-L+1)
      end do
   end if

   NUML1D = L1
   NUML   = L1 + L2

   DO L   = 1,L2
      LL  = NUML1D + L
      KN(:, LL) = KN2(:,L)              ! 2D na 1D
   ENDDO

   ALLOCATE (KC2(NUMK) )
   KK = 0
   DO K = 1,NUMK                        ! NODES AANSCHUIVEN
      IF (KC(K) .NE. 0 ) THEN
         KK = KK + 1
         KC (KK) = K                    ! HIER KWAM IE VANDAAN
         XK (KK) = XK(K)
         YK (KK) = YK(K)
         ZK (KK) = ZK(K)
         KCK(KK) = KCK(K)               ! COPY ORG KC
         KC2(K)  = KK                   ! EN HIER GAAT IE NAARTOE
      ENDIF
   ENDDO
   NUMK = KK


   DO L   = 1,NUML
      K1  = KN(1,L)  ; K2  = KN(2,L) ; K3 = KN(3,L)
      K12 = KC2(K1)  ; K22 = KC2(K2)
      KN2(1,L) = K12 ; KN2(2,L) = K22; KN2(3,L) = K3
   ENDDO

   KN(:,1:NUML)  = KN2(:,1:NUML)       ! TERUGZETTEN
   KC(1:NUMK)    = KCK(1:NUMK)

   DEALLOCATE (KC2, KN2, KCK)          ! WEGWEZEN

   NMK = 0
   DO L   = 1,NUML                     ! TEL LINKS PER NODE
      K1  = KN(1,L)  ; K2  = KN(2,L)
      NMK(K1) = NMK(K1) + 1
      NMK(K2) = NMK(K2) + 1
   ENDDO

   DO K = 1,NUMK                       ! ALLOCEER RUIMTE
      IF (NMK(K) > 0) THEN
         !call REALLOC(NOD(K)%LIN, NMK(K), keepexisting = .false. )
         if (allocated(NOD(K)%LIN)) then
            deallocate(NOD(K)%LIN)
         endif
         allocate( NOD(K)%LIN(nmk(k)) )
      ENDIF
   ENDDO

   NMK = 0
   jDupLinks = 0
   lnk:DO L=1,NUML                         ! EN ZET NODEADMIN (+check/reset dubbele links)
      K1 = KN(1,L) ;
      K2 = KN(2,L) ;
      DO LL = 1,NMK(K1) ! Check all previously added links
         if (KN(1,NOD(K1)%LIN(LL)) == K2 .or. KN(2,NOD(K1)%LIN(LL)) == K2) then
            KN(1,L) = 0
            KN(2,L) = 0
            jDupLinks = 1
            cycle lnk ! Jump to next outer L-loop
         end if
      ENDDO
      NMK(K1) = NMK(K1) + 1
      NMK(K2) = NMK(K2) + 1
      NOD(K1)%LIN(NMK(K1)) = L
      NOD(K2)%LIN(NMK(K2)) = L
   END DO lnk
   if (jDupLinks /= 0) then
      goto 100 ! Er waren duplicate links: opnieuw aanschuiven
   endif



   ! New cross check (two-smallangle check) is always performed
   jOverlapLinks = 0
   costriangleminangle = cos(triangleminangle*degrad_hp)
   if ( triangleminangle > 0 ) then
      lnl:do L=1,NUML
         k1 = kn(1,L) ; k2 = kn(2,L) ; k3 = kn(3,L)

         if (k3 >= 1 .and. k3 <= 6 ) cycle

         jSmallAng = 1
         do ki=1,2 ! Consider links of both nodes in link L
            if (jSmallAng /= 1) exit ! First or second end node did not have small angle between links

            if (ki == 2) then ! Check second node
               k1 = k2 ; k2 = kn(1,L)
            end if

            dmaxcosp = -huge(dmaxcosp)
            do LI=1,NMK(k1)
               LL = NOD(k1)%lin(LI)
               if (LL == L) cycle ! No self-check

               call othernode(k1, LL, kb)
               if (kb == 0) then      ! hk: dit kan hier toch nooit voorkomen?
                  cycle              ! Incomplete link?
               endif

               dcosp = dcosphi(xk(k1), yk(k1), xk(k2), yk(k2), xk(k1), yk(k1), xk(kb), yk(kb), jsferic, jasfer3D, dxymis)
               dmaxcosp = max(dmaxcosp, dcosp)
            end do
            if (dmaxcosp > costriangleminangle) then
               jSmallAng = 1
            else
               jSmallAng = 0
            end if
         end do

         if (jSmallAng == 1) then ! Disable original link L
            kn(1,L) = 0
            kn(2,L) = 0
            jOverlapLinks = 1
            write(msgbuf, '(a,i8, a)') 'Removed link', L, ', because of tiny angles at endpoints.'
            call msg_flush()
            ! cycle lnl ! Jump to next outer L-loop ben je al?
         end if
      end do lnl
   end if

   if (jOverlapLinks /= 0) then
      goto 100 ! Er waren overlapping links: opnieuw aanschuiven
   end if


   ! Sort nod%lin in counterclockwise order
   maxlin = maxval(nmk(1:numk))
   allocate(linnrs(maxlin), arglin(maxlin), inn(maxlin))
   do k=1,numk
      call sort_links_ccw(k, maxlin, linnrs, arglin, inn)
   end do
   deallocate(linnrs, arglin, inn)

   ! Reset small link count for linkbadqual (net link based).
   ! Will only be recomputed in flow_geominit.
   nlinkbadortho = 0
   nlinktoosmall = 0
   nlinkcross    = 0

   ! call trace_netlink_polys()

   if ( japermout.eq.1 ) then
      if ( allocated(Lperm_new) ) deallocate(Lperm_new)
   end if

   !  netcell administration out of date
   netstat = NETSTAT_CELLS_DIRTY
   END SUBROUTINE SETNODADM

   SUBROUTINE INIALLOCnetcell()
   use network_data
   use m_alloc
   implicit none

   integer :: ierr, nx

   NUMP  = 0

   nx = max(1,int(1.5*NUMK))
   call increasenetcells(nx, 1.0, .false.)
   netcell(:)%N = 0

   if (allocated(lnn) ) deallocate(lnn)
   allocate ( lnn(numl) , stat=ierr  )
   call aerr('lnn(numl)', ierr, numl )
   LNN  = 0

   if (allocated(lne) ) deallocate(lne)
   allocate(  lne(2,numl) , stat=ierr )
   call aerr('lne(2,numl)', ierr, 2*numl )
   lne  = 0                                            ! array = 0
   RETURN

   END SUBROUTINE INIALLOCnetcell

   subroutine update_cell_circumcenters()

   use network_data
   use m_alloc
   use m_sferic, only:  jsferic, jasfer3D, dtol_pole
   use m_cell_geometry

   implicit none

   integer :: n, numc, ierr
   double precision :: zzz

   ! Compute (circum)center coordinates now already.
   ! nump is in same rythm as  (future) ndx2d
   if (nump > 0) then
      !     if ( keepcircumcenters.eq.1 ) call qnerror('updating circumcenter', ' ', ' ')
      ! If ndx>nump, probably already some 1D stuff present.
      ! We can safely ignore it here, but won't, because this saves some
      ! realloc costs for xz, yz in flow_geominit.
      numc = max(ndx,nump)
      if (numc > size(xz)) then
         call realloc(xz, numc, stat=ierr, keepExisting=.false.)
         call aerr('xz(numc)',IERR, numc)
         call realloc(yz, numc, stat=ierr, keepExisting=.false.)
         call aerr('yz(numc)',IERR, numc)
      end if
      if (numc > size(xzw)) then
         call realloc(xzw, numc, stat=ierr, keepExisting=.false.)
         call aerr('xzw(numc)',IERR, numc)
         call realloc(yzw, numc, stat=ierr, keepExisting=.false.)
         call aerr('yzw(numc)',IERR, numc)
      end if
      if (numc > size(ba)) then
         call realloc(ba, numc, stat=ierr, keepExisting=.false.)
         call aerr('ba(numc)',IERR, numc)
      endif

      do n = 1,nump                                      ! get cell center coordinates 2D
         CALL GETCELLWEIGHTEDCENTER(n, xz(n) , yz(n) , zzz)
         call getcellsurface(n, ba(n), xzw(n), yzw(n))
         ! call cirr( xzw(n), yzw(n), 211 )
      end do
   end if
   end subroutine update_cell_circumcenters

   !> Finds 2D cells in the unstructured net.
   !! Optionally within a polygon mask.
   ! Resets netcell data and also computes circumcenters in xz (flowgeom)
   SUBROUTINE FINDCELLS(JP)
   
   use network_data
   use geometry_module, only: dbpinpol
   use m_polygon,  only: NPL, xpl, ypl, zpl
   use m_missing,  only: dmiss, jins
   use m_cell_geometry
   use m_sferic

   implicit none
   integer, intent(in) :: JP !< Type of cells to find (unfolded: 3: triangle, etc. up to 6=hexa, 0 = all; folded: code+100; no new nodemask (nonzero values will be used as mask here): code+1000, no sednodadm: code+10000, output link permutation array: code+100000)

   integer, allocatable, dimension(:) :: kc_sav  ! save of kc

   integer :: ik
   integer :: k
   integer :: k1
   integer :: k2
   integer :: l
   integer :: jafold, jakeepmask, jasetnodadm, japermout
   integer :: jp_

   jp_ = jp

   ! determine if (setnodm) has to output link permutation array
   if ( jp_.ge.100000) then
      jp_ = jp_-100000
      japermout = 1
   else
      japermout = 0
   end if

   ! determine if setnodm has to be called
   if ( jp_.ge.10000 ) then
      jp_ = jp_-10000
      jasetnodadm = 0
   else
      jasetnodadm = 1
   end if

   ! determine if the nodemask has to be made
   if ( jp_.ge.1000 ) then
      jp_ = jp_-1000
      jakeepmask = 1
   else
      jakeepmask = 0
   end if

   ! determine if folded cells have to be accounted for
   if ( jp_.ge.100 ) then
      jp_ = jp_-100
      jafold = 1
   else
      jafold = 0
   end if

   if ( jasetnodadm.eq.1 ) then
      if ( japermout.eq.1 ) then
         CALL SETNODADM(10)
      else
         CALL SETNODADM(0)
      end if
   end if

   CALL INIALLOCnetcell()

   LC   =  0
   IK   = -1

   if ( jakeepmask.ne.1 ) then
      KC   =  0
      DO K = 1,NUMK
         CALL DBPINPOL(xk(k), yk(k), ik, dmiss, JINS, NPL, xpl, ypl, zpl)
         IF (IK > 0) THEN
            KC(K) = IK
         ENDIF
      ENDDO
   end if

   IF (JP_ .EQ. 0) THEN
      CALL FINDTRIS(0)
      CALL FINDQUADS(0)
      CALL FINDPENTAS(0)
      CALL FINDHEXAS(0)
      if ( jafold.eq.1 ) then
         CALL FINDQUADS(1)
         CALL FINDTRIS(1)
         CALL FINDPENTAS(1)
         CALL FINDHEXAS(1)
      end if
   ELSE IF (JP_ .EQ. 3) THEN
      CALL FINDTRIS(0)
      if ( jafold.eq.1 ) CALL FINDTRIS(1)
   ELSE IF (JP_ .EQ. 4) THEN
      CALL FINDQUADS(0)
      if ( jafold.eq.1 ) CALL FINDQUADS(1)
   ELSE IF (JP_ .EQ. 5) THEN
      CALL FINDPENTAS(0)
      if ( jafold.eq.1 ) CALL FINDPENTAS(1)
   ELSE IF (JP_ .EQ. 6) THEN
      CALL FINDHEXAS(0)
      if ( jafold.eq.1 ) CALL FINDHEXAS(1)
   ELSE IF (JP_ .EQ. 11)THEN
      CALL FINDPENTAS(0)
      CALL FINDHEXAS(0)
      if ( jafold.eq.1 ) then
         CALL FINDPENTAS(1)
         CALL FINDHEXAS(1)
      end if
   ENDIF

   IF (NPL < 3) THEN
      !     LC = 1; KC = 1 ! SPvdP: this gives problems in orthogonalisenet
      LC = 1
      if ( jakeepmask.ne.1 ) then
         KC = 1
      end if
   ELSE
      DO L = 1, NUML
         K1 = KN(1,L) ; K2 = KN(2,L)
         IF (KC(K1) == 1 .or. KC(K2) == 1) LC(L) = 1
      ENDDO
   ENDIF

   call update_cell_circumcenters()

   nump1d2d = nump   ! there are no 1D cells yet, safety

   !  If one chooses to add find1dcells to findcells in future, this is how it may look like.
   !    Note however, that:
   !      -lne now has negative entries, which causes problems in various 2d-only subroutines, like orthonogalisenet, at the moment
   !      -kc is detroyed
   !    For these reasons, find1dcells is not included here
   !
   !! find 1D cells, will destroy kc
   !  allocate(kc_sav(numk))
   !  kc_sav = kc(1:numk)
   !
   !  call find1dcells()    ! will destroy kc
   !
   !! restore kc
   !  kc(1:numk) = kc_sav(1:numk)
   !  deallocate(kc_sav)

   NDX2D = NUMP                                        ! NR OF 2d CELLS=NUMP

   lasttopology = numk + numl


   ! set network status
   netstat = NETSTAT_OK

   RETURN
   END SUBROUTINE FINDCELLS

   SUBROUTINE FINDTRIS(jafold)

   use network_data
   use m_afmeting
   use m_alloc
   use m_sferic


   integer, intent(in) :: jafold  !< find folded cells (1), or not (0)

   integer :: ierr
   integer :: k1
   integer :: k2
   integer :: k3
   integer :: k4
   integer :: kk
   integer :: kkk
   integer :: kkkk
   integer :: kmod
   integer :: l
   integer :: ll
   integer :: lll
   integer :: i
   integer :: kr(3), Lr(3)
   integer :: kkk_, kkkk_, nmkmax

   !CALL READYY ('FIND TRIS', 0d0)

   nmkmax = 1
   if ( jafold.eq.1 ) nmkmax = 1000

   KMOD = max(1,NUMK/100)
   DO K1 = 1,NUMK

      !IF (MOD(K1,KMOD) == 1) CALL READYY ('FIND TRIS',dble(K1)/dble(NUMK))

      IF (KC(K1) .EQ. 1) THEN

         kklp:DO KK = 1,NMK(K1)
            L  = NOD(K1)%LIN(KK)
            IF (LNN(L) .GE. 2) CYCLE
            CALL OTHERNODECHK(K1,L,K2); IF (K2 == 0) CYCLE

            kkk = 1
            do while ( nod(k2)%lin(kkk).ne.L )
               kkk=kkk+1
            end do
            DO KKK_ = 1,min(NMK(K2),nmkmax)
               kkk = kkk-1
               if ( kkk.lt.1       ) kkk=kkk+nmk(k2)
               if ( kkk.gt.nmk(k2) ) kkk=kkk-nmk(k2)

               LL  = NOD(K2)%LIN(KKK)  ; IF (LL .EQ. L) CYCLE
               IF (LNN(LL) .GE. 2) CYCLE
               CALL OTHERNODECHK(K2,LL,K3) ; IF (K3 == 0) CYCLE
               IF ( RECHTSAF(K1,K2,K3) ) CYCLE
               IF (K3 .NE. K1) THEN

                  kkkk = 1
                  do while ( nod(k3)%lin(kkkk).ne.LL )
                     kkkk=kkkk+1
                  end do
                  DO KKKK_ = 1,min(NMK(K3),nmkmax)
                     kkkk = kkkk-1
                     if ( kkkk.lt.1       ) kkkk=kkkk+nmk(k3)
                     if ( kkkk.gt.nmk(k3) ) kkkk=kkkk-nmk(k3)

                     LLL  = NOD(K3)%LIN(KKKK) ; IF (LLL .EQ. LL .OR. LLL .EQ. L) CYCLE
                     IF (LNN(LLL) .GE. 2) CYCLE
                     CALL OTHERNODECHK(K3,LLL,K4)  ; IF (K4 == 0) CYCLE
                     IF ( RECHTSAF(K2,K3,K4) ) CYCLE
                     IF (K4 .EQ. K1) THEN  ! TRI GEVONDEN
                        IF (LNN(L)>1 .OR. LNN(LL)>1 .OR. LNN(LLL)>1)  EXIT
                        !                    call setcol(31) ! red
                        !                    call rcirc(xk(k1),yk(k1))
                        !                    call setcol(204) ! green
                        !                    call rcirc(xk(k2),yk(k2))
                        !                    call setcol(211) ! blue
                        !                    call rcirc(xk(k3),yk(k3))
                        !  SPvdP: check and see if cell already exist
                        if ( lnn(L).gt.0 .and. lnn(LL).gt.0 .and. lnn(LLL).gt.0 ) then
                           if ( lne(1,L).eq.lne(1,LL) .and. lne(1,L).eq.lne(1,LLL) ) then
                              cycle
                           else   ! more expensive check
                              kr(1)=k1; kr(2)=k2; kr(3)=k3
                              Lr(1)=L; Lr(2)=LL; Lr(3)=LLL
                              if ( alreadycell(3, kr, Lr) ) cycle

                              !                         do not allow folded cells when all links already have neighboring cells
                              if ( kkk_.ne.1 .or. kkkk_.ne.1 ) then
                                 cycle
                              end if
                           end if
                        end if

                        kr(1)=k1; kr(2)=k2; kr(3)=k3
                        if ( .not.iscounterclockwise(3, kr) ) cycle

                        !CALL ALREADYTRI(K1,K2,K3,JA); IF (JA > 0) EXIT
                        call increasenetcells(NUMP+1, 1.2, .true.)
                        NUMP = NUMP + 1
                        call realloc(netcell(NUMP)%NOD, 3, stat=ierr, keepExisting=.false.)
                        call realloc(netcell(NUMP)%LIN, 3, stat=ierr, keepExisting=.false.)
                        netcell(NUMP)%N = 3
                        netcell(NUMP)%NOD(1) = K1
                        netcell(NUMP)%NOD(2) = K2
                        netcell(NUMP)%NOD(3) = K3
                        netcell(NUMP)%LIN(1) = L
                        netcell(NUMP)%LIN(2) = LL
                        netcell(NUMP)%LIN(3) = LLL
                        LNN(L)            = LNN(L)   + 1
                        LNN(LL)           = LNN(LL)  + 1
                        LNN(LLL)          = LNN(LLL) + 1
                        LNE(LNN(L),L)     = NUMP
                        LNE(LNN(LL),LL)   = NUMP
                        LNE(LNN(LLL),LLL) = NUMP
                        ! SPvdP: linkmask deactivated with the purpose to find folded cells; check if cells already exist instead
                        !                    LC(L)             = 1 ; IF (KN(1,L)   == K2) LC(L)   = -1
                        !                    LC(LL)            = 1 ; IF (KN(1,LL)  == K3) LC(LL)  = -1  ! AvD: re-enabled, in line with new rgfgrid
                        !                    LC(LLL)           = 1 ; IF (KN(1,LLL) == K1) LC(LLL) = -1  !

                        !                    cell found and administered: proceed
                        cycle kklp
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO kklp
      ENDIF
   ENDDO

   !CALL READYY ( 'FIND TRIS', -1d0 )

   RETURN

   END SUBROUTINE FINDTRIS

   SUBROUTINE FINDQUADS(jafold)
   !LC use m_netw
   use network_data
   use m_afmeting
   use m_alloc
   implicit none

   integer, intent(in) :: jafold  !< find folded cells (1), or not (0)

   double precision :: af
   integer :: ierr
   integer :: k1
   integer :: k2
   integer :: k3
   integer :: k4
   integer :: k5
   integer :: kk
   integer :: kkk
   integer :: kkkk
   integer :: kkkkk
   integer :: kmod
   integer :: l
   integer :: ll
   integer :: lll
   integer :: llll
   integer :: kr(4), Lr(4)
   integer :: kkk_, kkkk_, kkkkk_, nmkmax

   !CALL READYY('FIND QUADS',0d0)

   nmkmax = 1
   if ( jafold.eq.1 ) nmkmax = 1000

   KMOD = max(1,NUMK/100)
   DO K1 = 1,NUMK

      if (mod(k1,KMOD) == 1) then
         af = dble(k1) /dble(numk)
         !CALL READYY('FIND QUADS',AF)
      endif


      IF (KC(K1) .EQ. 1) THEN

         kklp:DO KK = 1,NMK(K1)
            L   = NOD(K1)%LIN(KK)
            IF (LNN(L) .GE. 2) CYCLE
            CALL OTHERNODECHK(K1,L,K2); IF (K2 == 0) CYCLE

            kkk = 1
            do while ( nod(k2)%lin(kkk).ne.L )
               kkk=kkk+1
            end do
            DO KKK_ = 1,min(NMK(K2),nmkmax)
               kkk = kkk-1
               if ( kkk.lt.1       ) kkk=kkk+nmk(k2)
               if ( kkk.gt.nmk(k2) ) kkk=kkk-nmk(k2)

               LL  = NOD(K2)%LIN(KKK)  ; IF (LL .EQ. L) CYCLE
               IF (LNN(LL) .GE. 2) CYCLE
               CALL OTHERNODECHK(K2,LL,K3); IF (K3 == 0) CYCLE
               IF ( RECHTSAF(K1,K2,K3) ) CYCLE
               IF (K3 .NE. K1) THEN

                  kkkk = 1
                  do while ( nod(k3)%lin(kkkk).ne.LL )
                     kkkk=kkkk+1
                  end do
                  DO KKKK_ = 1,min(NMK(K3),nmkmax)
                     kkkk = kkkk-1
                     if ( kkkk.lt.1       ) kkkk=kkkk+nmk(k3)
                     if ( kkkk.gt.nmk(k3) ) kkkk=kkkk-nmk(k3)

                     LLL  = NOD(K3)%LIN(KKKK) ; IF (LLL .EQ. LL .OR. LLL .EQ. L) CYCLE
                     IF (LNN(LLL) .GE. 2) CYCLE
                     CALL OTHERNODECHK(K3,LLL,K4); IF (K4 == 0) CYCLE
                     IF ( RECHTSAF(K2,K3,K4) ) CYCLE
                     IF (K4 .NE. K2) THEN

                        kkkkk = 1
                        do while ( nod(k4)%lin(kkkkk).ne.LLL )
                           kkkkk=kkkkk+1
                        end do
                        DO KKKKK_ = 1,min(NMK(K4),nmkmax)
                           kkkkk = kkkkk-1
                           if ( kkkkk.lt.1       ) kkkkk=kkkkk+nmk(k4)
                           if ( kkkkk.gt.nmk(k4) ) kkkkk=kkkkk-nmk(k4)

                           LLLL  = NOD(K4)%LIN(KKKKK)
                           IF (LLLL .EQ. LLL .OR. LLLL .EQ. LL .OR. LLLL .EQ. L) CYCLE
                           IF (LNN(LLLL) .GE. 2) CYCLE
                           CALL OTHERNODECHK(K4,LLLL,K5) ; IF (K5 == 0) CYCLE
                           IF ( RECHTSAF(K3,K4,K5) ) CYCLE
                           IF (K5 .EQ. K1) THEN  ! PANEEL GEVONDEN
                              IF (LNN(L)>1 .OR. LNN(LL)>1 .OR. LNN(LLL)>1 .OR. LNN(LLLL)>1)  EXIT

                              !  SPvdP: check and see if cell already exist
                              if ( lnn(L).gt.0 .and. lnn(LL).gt.0 .and. lnn(LLL).gt.0 .and. lnn(LLLL).gt.0 ) then
                                 if ( lne(1,L).eq.lne(1,LL) .and. lne(1,L).eq.lne(1,LLL) .and. lne(1,L).eq.lne(1,LLLL) ) then
                                    cycle
                                 else   ! more expensive check
                                    kr(1)=k1; kr(2)=k2; kr(3)=k3; kr(4)=k4
                                    Lr(1)=L; Lr(2)=LL; Lr(3)=LLL; Lr(4)=LLLL
                                    if ( alreadycell(4, kr, Lr) ) cycle
                                    !                               do not allow folded cells when all links already have neighboring cells
                                    if ( kkk_.ne.1 .or. kkkk_.ne.1 .or. kkkkk_.ne.1 ) then
                                       cycle
                                    end if
                                 end if
                              end if

                              !CALL ALREADYQUAD(K1,K2,K3,K4,JA) ; IF (JA > 0 ) EXIT

                              kr(1)=k1; kr(2)=k2; kr(3)=k3; kr(4)=k4
                              if ( .not.iscounterclockwise(4, kr) ) cycle

                              call increasenetcells(NUMP+1, 1.2, .true.)
                              NUMP = NUMP + 1
                              call realloc(netcell(NUMP)%NOD, 4, stat=ierr, keepExisting=.false.)
                              call realloc(netcell(NUMP)%LIN, 4, stat=ierr, keepExisting=.false.)
                              netcell(NUMP)%N = 4
                              netcell(NUMP)%NOD (1)  = K1
                              netcell(NUMP)%NOD (2)  = K2
                              netcell(NUMP)%NOD (3)  = K3
                              netcell(NUMP)%NOD (4)  = K4
                              netcell(NUMP)%LIN(1)   = L
                              netcell(NUMP)%LIN(2)   = LL
                              netcell(NUMP)%LIN(3)   = LLL
                              netcell(NUMP)%LIN(4)   = LLLL
                              LNN(L)              = LNN(L)    + 1
                              LNN(LL)             = LNN(LL)   + 1
                              LNN(LLL)            = LNN(LLL)  + 1
                              LNN(LLLL)           = LNN(LLLL) + 1
                              LNE(LNN(L),L)       = NUMP
                              LNE(LNN(LL),LL)     = NUMP
                              LNE(LNN(LLL),LLL)   = NUMP
                              LNE(LNN(LLLL),LLLL) = NUMP
                              ! SPvdP: linkmask deactivated with the purpose to find folded cells; check if cells already exist instead
                              !                           LC(L)               = 1 ; IF (KN(1,L)    == K2) LC(L)    = -1
                              !                           LC(LL)              = 1 ; IF (KN(1,LL)   == K3) LC(LL)   = -1
                              !                           LC(LLL)             = 1 ; IF (KN(1,LLL)  == K4) LC(LLL)  = -1
                              !                           LC(LLLL)            = 1 ; IF (KN(1,LLLL) == K1) LC(LLLL) = -1

                              !                       cell found and administered: proceed
                              cycle kklp

                           ENDIF
                        ENDDO
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO kklp
      ENDIF
   ENDDO

   !CALL READYY('FIND QUADS',-1d0)

   RETURN
   END SUBROUTINE FINDQUADS

   SUBROUTINE FINDPENTAS(jafold)

   use network_data
   use m_afmeting
   use m_alloc
   implicit none

   integer, intent(in) :: jafold  !< find folded cells (1), or not (0)

   integer :: ierr
   integer :: k1
   integer :: k2
   integer :: k3
   integer :: k4
   integer :: k5
   integer :: k6
   integer :: kk
   integer :: kkk
   integer :: kkkk
   integer :: kkkkk
   integer :: kkkkkk
   integer :: kmod
   integer :: l
   integer :: ll
   integer :: lll
   integer :: llll
   integer :: lllll
   integer :: kr(5), Lr(5)
   integer :: kkk_, kkkk_, kkkkk_, kkkkkk_, nmkmax
   
   !CALL READYY ('FINDPENTAS',0d0)

   nmkmax = 1
   if ( jafold.eq.1 ) nmkmax = 1000

   KMOD = max(1,NUMK/100)
   DO K1 = 1,NUMK

      !IF (MOD(K1,KMOD) == 1) CALL READYY ('FINDPENTAS',dble(K1)/dble(NUMK))

      IF (KC(K1) == 1) THEN

         kklp:DO KK = 1,NMK(K1)
            L  = NOD(K1)%LIN(KK)
            IF (LNN(L) .GE. 2) CYCLE
            CALL OTHERNODECHK(K1,L,K2); IF (K2 == 0) CYCLE

            kkk = 1
            do while ( nod(k2)%lin(kkk).ne.L )
               kkk=kkk+1
            end do
            DO KKK_ = 1,min(NMK(K2),nmkmax)
               kkk = kkk-1
               if ( kkk.lt.1       ) kkk=kkk+nmk(k2)
               if ( kkk.gt.nmk(k2) ) kkk=kkk-nmk(k2)

               LL  = NOD(K2)%LIN(KKK)
               IF (LL .EQ. L) CYCLE
               IF (LNN(LL) .GE. 2) CYCLE
               CALL OTHERNODECHK(K2,LL,K3); IF (K3 == 0) CYCLE
               IF ( RECHTSAF(K1,K2,K3) ) CYCLE
               IF (K3 .NE. K1) THEN

                  kkkk = 1
                  do while ( nod(k3)%lin(kkkk).ne.LL )
                     kkkk=kkkk+1
                  end do
                  DO KKKK_ = 1,min(NMK(K3),nmkmax)
                     kkkk = kkkk-1
                     if ( kkkk.lt.1       ) kkkk=kkkk+nmk(k3)
                     if ( kkkk.gt.nmk(k3) ) kkkk=kkkk-nmk(k3)

                     LLL  = NOD(K3)%LIN(KKKK)
                     IF (LLL .EQ. LL .OR. LLL .EQ. L) CYCLE
                     IF (LNN(LLL) .GE. 2) CYCLE
                     CALL OTHERNODECHK(K3,LLL,K4); IF (K4 == 0) CYCLE
                     IF ( RECHTSAF(K2,K3,K4) ) CYCLE
                     IF (K4 .NE. K2 .AND. K4 .NE. K1) THEN

                        kkkkk = 1
                        do while ( nod(k4)%lin(kkkkk).ne.LLL )
                           kkkkk=kkkkk+1
                        end do
                        DO KKKKK_ = 1,min(NMK(K4),nmkmax)
                           kkkkk = kkkkk-1
                           if ( kkkkk.lt.1       ) kkkkk=kkkkk+nmk(k4)
                           if ( kkkkk.gt.nmk(k4) ) kkkkk=kkkkk-nmk(k4)

                           LLLL  = NOD(K4)%LIN(KKKKK)
                           IF (LLLL .EQ. LLL .OR. LLLL .EQ. LL .OR. LLLL .EQ. L) CYCLE
                           IF (LNN(LLLL) .GE. 2) CYCLE
                           CALL OTHERNODECHK(K4,LLLL,K5) ; IF (K5 == 0) CYCLE
                           IF ( RECHTSAF(K3,K4,K5) ) CYCLE
                           IF (K5 .NE. K3 .AND. K5 .NE. K2 .AND. K5 .NE. K1) THEN

                              kkkkkk = 1
                              do while ( nod(k5)%lin(kkkkkk).ne.LLLL )
                                 kkkkkk=kkkkkk+1
                              end do
                              DO KKKKKK_ = 1,min(NMK(K5),nmkmax)
                                 kkkkkk = kkkkkk-1
                                 if ( kkkkkk.lt.1       ) kkkkkk=kkkkkk+nmk(k5)
                                 if ( kkkkkk.gt.nmk(k5) ) kkkkkk=kkkkkk-nmk(k5)

                                 LLLLL  = NOD(K5)%LIN(KKKKKK)
                                 IF (LLLLL .EQ. LLLL .OR. LLLLL .EQ. LLL .OR. LLLLL .EQ. LL .OR. LLLLL .EQ. L) CYCLE
                                 IF (LNN(LLLLL) .GE. 2) CYCLE
                                 CALL OTHERNODECHK(K5,LLLLL,K6); IF (K6 == 0) CYCLE
                                 IF ( RECHTSAF(K4,K5,K6) ) CYCLE
                                 IF (K6 .EQ. K1) THEN  ! PENTA GEVONDEN
                                    IF (LNN(L)>1 .OR. LNN(LL)>1 .OR. LNN(LLL)>1 .OR.     &
                                       LNN(LLLL)>1 .OR. LNN(LLLLL) > 1 )  EXIT

                                    !  SPvdP: check and see if cell already exist
                                    if ( lnn(L).gt.0 .and. lnn(LL).gt.0 .and. lnn(LLL).gt.0 .and.  &
                                       lnn(LLLL).gt.0  .and. lnn(LLLLL).gt.0 ) then
                                    if ( lne(1,L).eq.lne(1,LL) .and. lne(1,L).eq.lne(1,LLL) .and.  &
                                       lne(1,L).eq.lne(1,LLLL) .and. lne(1,L).eq.lne(1,LLLLL) ) then
                                    cycle
                                    else   ! more expensive check
                                       kr(1)=k1; kr(2)=k2; kr(3)=k3; kr(4)=k4; kr(5)=k5
                                       Lr(1)=L; Lr(2)=LL; lr(3)=LLL; Lr(4)=LLLL; Lr(5)=LLLLL
                                       if ( alreadycell(5, kr, Lr) ) cycle
                                       !                                  do not allow folded cells when all links already have neighboring cells
                                       if ( kkk_.ne.1 .or. kkkk_.ne.1 .or. kkkkk_.ne.1 .or. kkkkkk_.ne.1 ) then
                                          cycle
                                       end if
                                    end if
                                    end if

                                    !CALL ALREADYPENTA(K1,K2,K3,K4,K5,JA) ; IF (JA > 0) EXIT

                                    kr(1)=k1; kr(2)=k2; kr(3)=k3; kr(4)=k4; kr(5)=k5
                                    if ( .not.iscounterclockwise(5, kr) ) cycle

                                    call increasenetcells(NUMP+1, 1.2, .true.)
                                    NUMP = NUMP + 1
                                    call realloc(netcell(NUMP)%NOD, 5, stat=ierr, keepExisting=.false.)
                                    call realloc(netcell(NUMP)%LIN, 5, stat=ierr, keepExisting=.false.)
                                    netcell(NUMP)%N = 5
                                    netcell(NUMP)%NOD(1)     = K1
                                    netcell(NUMP)%NOD(2)     = K2
                                    netcell(NUMP)%NOD(3)     = K3
                                    netcell(NUMP)%NOD(4)     = K4
                                    netcell(NUMP)%NOD(5)     = K5
                                    netcell(NUMP)%LIN(1)     = L
                                    netcell(NUMP)%LIN(2)     = LL
                                    netcell(NUMP)%LIN(3)     = LLL
                                    netcell(NUMP)%LIN(4)     = LLLL
                                    netcell(NUMP)%LIN(5)     = LLLLL
                                    LNN(L)                = LNN(L)     + 1
                                    LNN(LL)               = LNN(LL)    + 1
                                    LNN(LLL)              = LNN(LLL)   + 1
                                    LNN(LLLL)             = LNN(LLLL)  + 1
                                    LNN(LLLLL)            = LNN(LLLLL) + 1
                                    LNE(LNN(L),L)         = NUMP
                                    LNE(LNN(LL),LL)       = NUMP
                                    LNE(LNN(LLL),LLL)     = NUMP
                                    LNE(LNN(LLLL),LLLL)   = NUMP
                                    LNE(LNN(LLLLL),LLLLL) = NUMP
                                    ! SPvdP: linkmask deactivated with the purpose to find folded cells; check if cells already exist instead
                                    !                              LC(L)                 = 1 ; IF (KN(1,L)     == K2) LC(L)     = -1
                                    !                              LC(LL)                = 1 ; IF (KN(1,LL)    == K3) LC(LL)    = -1
                                    !                              LC(LLL)               = 1 ; IF (KN(1,LLL)   == K4) LC(LLL)   = -1
                                    !                              LC(LLLL)              = 1 ; IF (KN(1,LLLL)  == K5) LC(LLLL)  = -1
                                    !                              LC(LLLLL)             = 1 ; IF (KN(1,LLLLL) == K1) LC(LLLLL) = -1

                                    !                       cell found and administered: proceed
                                    cycle kklp

                                 ENDIF
                              ENDDO
                           ENDIF
                        ENDDO
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO kklp
      ENDIF
   ENDDO

   !CALL READYY ('FINDPENTAS', -1d0)

   RETURN

   END SUBROUTINE FINDPENTAS

   SUBROUTINE FINDHEXAS(jafold)

   use network_data
   use m_afmeting
   use m_alloc
   use m_sferic

   implicit none

   integer, intent(in) :: jafold  !< find folded cells (1), or not (0)

   integer :: ierr
   integer :: k1
   integer :: k2
   integer :: k3
   integer :: k4
   integer :: k5
   integer :: k6
   integer :: k7
   integer :: kk
   integer :: kkk
   integer :: kkkk
   integer :: kkkkk
   integer :: kkkkkk
   integer :: kkkkkkk
   integer :: kmod
   integer :: l
   integer :: ll
   integer :: lll
   integer :: llll
   integer :: lllll
   integer :: llllll
   integer :: kr(6), Lr(6)
   integer :: kkk_, kkkk_, kkkkk_, kkkkkk_, kkkkkkk_, nmkmax

   !LC LOGICAL RECHTSAF
   !LC logical :: alreadycell
   !LC logical :: iscounterclockwise

   !  CALL READYY ('FINDHEXAS', 0d0)

   nmkmax = 1
   if ( jafold.eq.1 ) nmkmax = 1000

   KMOD = max(1,NUMK/100)
   DO K1 = 1,NUMK
      !IF (MOD(K1,KMOD) == 1) CALL READYY ('FINDHEXAS',dble(K1)/dble(NUMK))

      IF (KC(K1) == 1) THEN

         kklp:DO KK = 1,NMK(K1)
            L  = NOD(K1)%LIN(KK)
            IF (LNN(L) .GE. 2) CYCLE
            CALL OTHERNODECHK(K1,L,K2); IF (K2 ==0) CYCLE

            kkk = 1
            do while ( nod(k2)%lin(kkk).ne.L )
               kkk=kkk+1
            end do
            DO KKK_ = 1,min(NMK(K2),nmkmax)
               kkk = kkk-1
               if ( kkk.lt.1       ) kkk=kkk+nmk(k2)
               if ( kkk.gt.nmk(k2) ) kkk=kkk-nmk(k2)

               LL  = NOD(K2)%LIN(KKK)
               IF (LL .EQ. L) CYCLE
               IF (LNN(LL) .GE. 2) CYCLE
               CALL OTHERNODECHK(K2,LL,K3); IF (K3 ==0) CYCLE
               IF ( RECHTSAF(K1,K2,K3) ) CYCLE
               IF (K3 .NE. K1) THEN

                  kkkk = 1
                  do while ( nod(k3)%lin(kkkk).ne.LL )
                     kkkk=kkkk+1
                  end do
                  DO KKKK_ = 1,min(NMK(K3),nmkmax)
                     kkkk = kkkk-1
                     if ( kkkk.lt.1       ) kkkk=kkkk+nmk(k3)
                     if ( kkkk.gt.nmk(k3) ) kkkk=kkkk-nmk(k3)


                     LLL  = NOD(K3)%LIN(KKKK)
                     IF (LLL .EQ. LL .OR. LLL .EQ. L) CYCLE
                     IF (LNN(LLL) .GE. 2) CYCLE
                     CALL OTHERNODECHK(K3,LLL,K4); IF (K4 ==0) CYCLE
                     IF ( RECHTSAF(K2,K3,K4) ) CYCLE
                     IF (K4 .NE. K2 .AND. K4 .NE. K1) THEN

                        kkkkk = 1
                        do while ( nod(k4)%lin(kkkkk).ne.LLL )
                           kkkkk=kkkkk+1
                        end do
                        DO KKKKK_ = 1,min(NMK(K4),nmkmax)
                           kkkkk = kkkkk-1
                           if ( kkkkk.lt.1       ) kkkkk=kkkkk+nmk(k4)
                           if ( kkkkk.gt.nmk(k4) ) kkkkk=kkkkk-nmk(k4)

                           LLLL  = NOD(K4)%LIN(KKKKK)
                           IF (LLLL .EQ. LLL .OR. LLLL .EQ. LL .OR. LLLL .EQ. L) CYCLE
                           IF (LNN(LLLL) .GE. 2) CYCLE
                           CALL OTHERNODECHK(K4,LLLL,K5); IF (K5 ==0) CYCLE
                           IF ( RECHTSAF(K3,K4,K5) ) CYCLE
                           IF (K5 .NE. K3 .AND. K5 .NE. K2 .AND. K5 .NE. K1) THEN

                              kkkkkk = 1
                              do while ( nod(k5)%lin(kkkkkk).ne.LLLL )
                                 kkkkkk=kkkkkk+1
                              end do
                              DO KKKKKK_ = 1,min(NMK(K5),nmkmax)
                                 kkkkkk = kkkkkk-1
                                 if ( kkkkkk.lt.1       ) kkkkkk=kkkkkk+nmk(k5)
                                 if ( kkkkkk.gt.nmk(k5) ) kkkkkk=kkkkkk-nmk(k5)

                                 LLLLL  = NOD(K5)%LIN(KKKKKK)
                                 IF (LLLLL .EQ. LLLL .OR. LLLLL .EQ. LLL .OR. LLLLL .EQ. LL .OR. LLLLL .EQ. L) CYCLE
                                 IF (LNN(LLLLL) .GE. 2) CYCLE
                                 CALL OTHERNODECHK(K5,LLLLL,K6); IF (K6 ==0) CYCLE
                                 IF ( RECHTSAF(K4,K5,K6) ) CYCLE
                                 IF (K6 .NE. K4 .AND. K6 .NE. K3 .AND. K6 .NE. K2 .AND. K6 .NE. K1) THEN

                                    kkkkkkk = 1
                                    do while ( nod(k6)%lin(kkkkkkk).ne.LLLLL )
                                       kkkkkkk=kkkkkkk+1
                                    end do
                                    DO KKKKKKK_ = 1,min(NMK(K6),nmkmax)
                                       kkkkkkk = kkkkkkk-1
                                       if ( kkkkkkk.lt.1       ) kkkkkkk=kkkkkkk+nmk(k6)
                                       if ( kkkkkkk.gt.nmk(k6) ) kkkkkkk=kkkkkkk-nmk(k6)

                                       LLLLLL  = NOD(K6)%LIN(KKKKKKK)
                                       IF (LLLLLL .EQ. LLLLL .OR. LLLLLL .EQ. LLLL .OR.   &
                                          LLLLLL .EQ. LLL .OR. LLLLLL .EQ. LL .OR. LLLLLL .EQ. L) CYCLE
                                       IF (LNN(LLLLLL) .GE. 2) CYCLE
                                       CALL OTHERNODECHK(K6,LLLLLL,K7); IF (K7 ==0) CYCLE
                                       IF ( RECHTSAF(K5,K6,K7) ) CYCLE
                                       IF (K7 .EQ. K1) THEN  ! HEXA GEVONDEN
                                          IF (LNN(L)>1 .OR. LNN(LL)>1 .OR. LNN(LLL)>1 .OR.      &
                                             LNN(LLLL)>1 .OR. LNN(LLLLL)>1 .OR. LNN(LLLLLL)>1)  EXIT

                                          !  SPvdP: check and see if cell already exist
                                          if ( lnn(L).gt.0 .and. lnn(LL).gt.0 .and. lnn(LLL).gt.0 .and.  &
                                             lnn(LLLL).gt.0 .and. lnn(LLLLL).gt.0 .and. lnn(LLLLLL).gt.0 ) then
                                          if ( lne(1,L).eq.lne(1,LL) .and. lne(1,L).eq.lne(1,LLL) .and.  &
                                             lne(1,L).eq.lne(1,LLLL) .and. lne(1,L).eq.lne(1,LLLLL) .and. lne(1,L).eq.lne(1,LLLLLL) ) then
                                          cycle
                                          else   ! more expensive check
                                             kr(1)=k1; kr(2)=k2; kr(3)=k3; kr(4)=k4; kr(5)=k5; kr(6)=k6
                                             Lr(1)=L; Lr(2)=LL; lr(3)=LLL; Lr(4)=LLLL; Lr(5)=LLLLL; Lr(6)=LLLLLL
                                             if ( alreadycell(6, kr, Lr) ) cycle
                                             !                                       do not allow folded cells when all links already have neighboring cells
                                             if ( kkk_.ne.1 .or. kkkk_.ne.1 .or. kkkkk_.ne.1 .or. kkkkkk_.ne.1 .or. kkkkkkk_.ne.1 ) then
                                                cycle
                                             end if
                                          end if
                                          end if

                                          !CALL ALREADYHEXA(K1,K2,K3,K4,K5,K6,JA) ; IF (JA > 0) EXIT

                                          kr(1)=k1; kr(2)=k2; kr(3)=k3; kr(4)=k4; kr(5)=k5; kr(6)=k6
                                          if ( .not.iscounterclockwise(6, kr) ) cycle

                                          call increasenetcells(NUMP+1, 1.2, .true.)
                                          NUMP = NUMP + 1
                                          call realloc(netcell(NUMP)%NOD, 6, stat=ierr, keepExisting=.false.)
                                          call realloc(netcell(NUMP)%LIN, 6, stat=ierr, keepExisting=.false.)
                                          netcell(NUMP)%N = 6
                                          netcell(NUMP)%NOD(1)       = K1
                                          netcell(NUMP)%NOD(2)       = K2
                                          netcell(NUMP)%NOD(3)       = K3
                                          netcell(NUMP)%NOD(4)       = K4
                                          netcell(NUMP)%NOD(5)       = K5
                                          netcell(NUMP)%NOD(6)       = K6
                                          netcell(NUMP)%LIN(1)       = L
                                          netcell(NUMP)%LIN(2)       = LL
                                          netcell(NUMP)%LIN(3)       = LLL
                                          netcell(NUMP)%LIN(4)       = LLLL
                                          netcell(NUMP)%LIN(5)       = LLLLL
                                          netcell(NUMP)%LIN(6)       = LLLLLL
                                          LNN(L)                  = LNN(L)      + 1
                                          LNN(LL)                 = LNN(LL)     + 1
                                          LNN(LLL)                = LNN(LLL)    + 1
                                          LNN(LLLL)               = LNN(LLLL)   + 1
                                          LNN(LLLLL)              = LNN(LLLLL)  + 1
                                          LNN(LLLLLL)             = LNN(LLLLLL) + 1
                                          LNE(LNN(L),L)           = NUMP
                                          LNE(LNN(LL),LL)         = NUMP
                                          LNE(LNN(LLL),LLL)       = NUMP
                                          LNE(LNN(LLLL),LLLL)     = NUMP
                                          LNE(LNN(LLLLL),LLLLL)   = NUMP
                                          LNE(LNN(LLLLLL),LLLLLL) = NUMP

                                          ! SPvdP: linkmask deactivated with the purpose to find folded cells; check if cells already exist instead
                                          !                                   LC(L)                   = 1 ; IF (KN(1,L)      == K2) LC(L)      = -1
                                          !                                   LC(LL)                  = 1 ; IF (KN(1,LL)     == K3) LC(LL)     = -1
                                          !                                   LC(LLL)                 = 1 ; IF (KN(1,LLL)    == K4) LC(LLL)    = -1
                                          !                                   LC(LLLL)                = 1 ; IF (KN(1,LLLL)   == K5) LC(LLLL)   = -1
                                          !                                   LC(LLLLL)               = 1 ; IF (KN(1,LLLLL)  == K6) LC(LLLLL)  = -1
                                          !                                   LC(LLLLLL)              = 1 ; IF (KN(1,LLLLLL) == K1) LC(LLLLLL) = -1

                                          !                                  cell found and administered: proceed
                                          cycle kklp
                                       ENDIF
                                    ENDDO
                                 ENDIF
                              ENDDO
                           ENDIF
                        ENDDO
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO kklp
      ENDIF
   ENDDO

   !CALL READYY ('FINDHEXAS', -1d0)

   RETURN
   END SUBROUTINE FINDHEXAS

   ! check if cell is counterclockwise
   logical function iscounterclockwise(N, K)
   !LC use m_netw

   use network_data
   use m_missing,  only: dmiss
   use geometry_module, only: comp_masscenter
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   integer,               intent(in) :: N  !< number of links and nodes
   integer, dimension(N), intent(in) :: K  !< node set

   integer,                parameter :: MMAX = 10
   double precision, dimension(MMAX) :: xv, yv

   double precision                  :: xdum, ydum

   double precision                  :: darea
   integer                           :: jacounterclockwise          ! counterclockwise (1) or not (0)
   integer                           :: i, ip1, kk, kkp1

   iscounterclockwise = .true.

   !
   !    darea = 0d0
   !    do i=1,N
   !       ip1 = i+1; if ( ip1.gt.N ) ip1=ip1-N
   !       kk   = K(i)
   !       kkp1 = K(ip1)
   !       darea = darea + xk(kk) * (yk(kkp1)-yk(kk)) - yk(kk) * (xk(kkp1)-xk(kk))
   !    end do
   !    darea = 0.5d0*darea   ! not really necessary
   !
   !    if ( darea.le.0d0 ) then
   !       iscounterclockwise = .false.
   !    end if

   do i=1,N
      kk = K(i)
      xv(i) = xk(kk)
      yv(i) = yk(kk)
   end do

   call comp_masscenter(N, xv, yv, xdum, ydum, darea, jacounterclockwise, jsferic, jasfer3D, dmiss)
   if ( jacounterclockwise.eq.1 ) then
      iscounterclockwise = .true.
   else
      iscounterclockwise = .false.
   end if

   return
   end function iscounterclockwise

   LOGICAL FUNCTION RECHTSAF(K1,K2,K3)
   use network_data
   implicit none
   integer :: K1, K2, K3

   logical, external :: rechtsaf_active

   double precision :: sig

   rechtsaf = .false.
   return

   rechtsaf = RECHTSAF_active(K1,K2,K3)

   return

   end FUNCTION RECHTSAF

   SUBROUTINE CONNECTDBN(K1,K2,LNU)
   implicit none
   integer :: K1, K2, LNU
   if (k1 == k2) return
   CALL CONNECTDB(K1,K2,lnu)
   CALL ADDLINKTONODES(K1,K2,LNU)
   RETURN
   END SUBROUTINE CONNECTDBN

   SUBROUTINE CONNECTDB(K1,K2,lnu) ! fast version without refinement

   use network_data
   implicit none
   integer :: K1, K2, LNU

   integer :: l

   DO L = NUML,1,-1
      IF (KN(1,L) .EQ. K1 .AND. KN(2,L) .EQ. K2 .OR.    &
         KN(1,L) .EQ. K2 .AND. KN(2,L) .EQ. K1 ) THEN
      ! CALL CONFRM('POINTS ALREADY CONNECTED, CONTINUE', JA)
      ! IF (JA .NE. 1) RETURN
      LNU = L
      RETURN
      ENDIF
   ENDDO

   LNU  = 0
   DO L = NUML,1,-1
      IF (KN(1,L) .EQ. 0) THEN
         LNU = L
         EXIT
      ENDIF
   ENDDO

   IF (LNU == 0) THEN ! NO FREE NR
      NUML = NUML + 1
      LNU  = NUML
      IF (NUML >= LMAX) THEN
         CALL INCREASENETW(NUMK,NUML)
      ENDIF
   ENDIF

   kn(1,lnu) = k1 ; kn(2,lnu) = k2 ; kn(3,lnu) = KN3TYP  ! cheap version only to be used icm setnodadm
   ! mark link as active
   lc(lnu) = 1

   RETURN
   END SUBROUTINE CONNECTDB

   SUBROUTINE ADDLINKTONODES(KL,KR,LNU)
   use network_data
   implicit none
   integer :: KL, KR, LNU

   KN(1,LNU) = KL
   KN(2,LNU) = KR
   NMK(KL)   = NMK(KL) + 1
   NMK(KR)   = NMK(KR) + 1
   CALL SETNODLIN(KL,NMK(KL),LNU)
   CALL SETNODLIN(KR,NMK(KR),LNU)
   IF (KC(KL) .EQ. 0) KC(KL) = 1
   IF (KC(KR) .EQ. 0) KC(KR) = 1
   RETURN

   END SUBROUTINE ADDLINKTONODES

   SUBROUTINE SETNODLIN(K,LK,L)
   !LC use m_netw
   use network_data
   implicit none
   integer :: K, LK, L

   CALL CHKLINSIZTONODE(K)
   NOD(K)%LIN(LK) = L
   RETURN

   END SUBROUTINE SETNODLIN

   SUBROUTINE CHKLINSIZTONODE(KK)
   use network_data
   implicit none
   integer :: KK

   integer :: ierr
   integer :: knxk

   INTEGER, ALLOCATABLE :: IH(:)
   if ( allocated(nod(kk)%lin) ) then   ! SPvdP: nod(kk)%lin may not have been allocated
      KNXK = SIZE(NOD(KK)%LIN)
      IF (NMK(KK) .GT. KNXK) THEN
         ALLOCATE (IH(KNXK),STAT=IERR)
         IH(1:KNXK) = NOD(KK)%LIN(1:KNXK)
         DEALLOCATE (NOD(KK)%LIN)
         ALLOCATE   (NOD(KK)%LIN(KNXK+1),STAT=IERR) ; NOD(KK)%LIN = 0
         NOD(KK)%LIN(1:KNXK) = IH(1:KNXK)
         DEALLOCATE(IH)
      ENDIF
   else
      ALLOCATE (NOD(KK)%LIN(1),STAT=IERR) ; NOD(KK)%LIN = 0
   end if
   RETURN
   END SUBROUTINE CHKLINSIZTONODE

   SUBROUTINE GIVENEWNODENUM(KNU)
   !LC use m_netw
   use network_data
   implicit none
   integer :: KNU

   integer :: kx
   integer :: lx

   IF ( NUMK == SIZE(KC) ) THEN
      KX = 1.2*NUMK ; LX = 1.2*NUML
      CALL INCREASENETW(KX, LX)
   ENDIF
   NUMK = NUMK + 1
   KNU  = NUMK
   RETURN
   END SUBROUTINE GIVENEWNODENUM

   SUBROUTINE DRIETWEE(XD,YD,ZD,X,Y,Z)
   implicit none
   integer :: jav
   integer :: jview
   double precision :: xyz
   DOUBLE PRECISION XD,YD,ZD,X,Y,Z
   COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
   IF (JVIEW .EQ. 1) THEN        ! NORMAL
      X = XD
      Y = YD
      Z = ZD
   ELSE IF (JVIEW .EQ. 2) THEN   ! FROM LEFT
      X = ZD
      Y = YD
      Z = XD
   ELSE IF (JVIEW .EQ. 3) THEN   ! FROM TOP
      X = XD
      Y = -ZD
      Z = YD
   ELSE IF (JVIEW .EQ. 4) THEN
      !    CALL DVIEW(XD,YD,-ZD,X,Y,Z)
      CALL DVIEW(XD,YD,-ZD,X,Y,Z)
   ENDIF
   RETURN
   END SUBROUTINE DRIETWEE

   SUBROUTINE TWEEDRIE(X,Y,XD,YD,ZD)
   implicit none
   integer :: jav
   integer :: jview
   double precision :: xyz
   double precision :: X,Y,XD,YD,ZD
   COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
   IF (JVIEW .EQ. 1) THEN
      XD = X
      YD = Y
      ZD = XYZ
   ELSE IF (JVIEW .EQ. 2) THEN
      ZD = X
      YD = Y
      XD = XYZ
   ELSE IF (JVIEW .EQ. 3) THEN
      XD = X
      ZD = -Y
      YD = XYZ
   ELSE IF (JVIEW .EQ. 4) THEN
      !    CALL DVIEW(XD,YD,ZD,X,Y,Z)  ! MOET NOG INVERS MAKEN
      XD = X
      YD = Y
      ZD = XYZ
   ENDIF

   RETURN
   END SUBROUTINE TWEEDRIE

   SUBROUTINE DVIEW(XD,YD,ZD,X,Y,Z)
   use m_missing
   implicit none
   double precision :: ce
   integer :: i
   double precision :: vs
   double precision :: x0s
   double precision :: y0s
   ! GEEF perspectievische COORDINATEN
   ! xD,yD,zD                             :coordinaten te tekenen punt
   ! x0s,y0s                              :waar op scherm ligt kijklijn
   ! X,Y,Z                                :scherm coordinaten
   ! Vs                                   :viewing matrix na viema

   DOUBLE PRECISION XD,YD,ZD,X,Y,Z
   COMMON /VIEWMAT/ VS(4,4), X0S, Y0S
   DIMENSION CE(4)
   ! use z as zd temporarily (zet to zero when zd==dmiss)
   if (zd == dmiss) then
      z = 0
   else
      z = zd
   end if
   DO I = 1,3
      CE(I) = VS(I,1)*XD + VS(I,2)*YD + VS(I,3)*Z + VS(I,4)
   ENDDO
   Z  = CE(3)
   IF (Z .LT. 0) THEN
      Z = dmiss
   ELSE
      X = CE(1)/Z  + X0S
      Y = CE(2)/Z  + Y0S
   ENDIF
   END SUBROUTINE DVIEW

   SUBROUTINE INCELLS(XA,YA,KIN)
   !use m_netw
   use network_data
   use geometry_module, only: pinpok
   use m_missing, only : jins, dmiss

   implicit none
   double precision :: xa
   double precision :: ya
   integer :: kin

   integer :: in
   integer :: k
   integer :: k1
   integer :: n
   integer :: nn
   double precision :: XH(6), YH(6)
   KIN = 0
   DO K = 1,NUMP
      NN = netcell(K)%N
      DO N = 1,NN
         K1 = netcell(K)%NOD(N)
         XH(N) = XK(K1) ; YH(N) = YK(K1)
      ENDDO
      CALL PINPOK(XA, YA , NN, XH, YH, IN, jins, dmiss)
      IF (IN == 1) THEN
         KIN = K
         RETURN
      ENDIF
   ENDDO
   END SUBROUTINE INCELLS

   !> sort links in nod%lin counterclockwise (copy-paste from setnodadm)
   subroutine sort_links_ccw(k,maxlin,linnrs,arglin,inn)
   !LC use m_netw
   use network_data
   use m_sferic
   use geometry_module, only: getdxdy, dcosphi, getdx, getdy
   use sorting_algorithms, only: indexx

   implicit none

   integer,          intent(in)    :: k                           !< node number
   integer,          intent(in)    :: maxlin                      !< array size


   double precision, intent(inout) :: arglin(maxlin)              ! dummy array
   integer,          intent(inout) :: linnrs(maxlin), inn(maxlin) ! dummy arrays

   integer                         :: k1, k2, L, LL

   integer                         :: jDupLinks, jOverlapLinks, jSmallAng
   double precision                :: sl, sm, xcr, ycr, phi0

   double precision                :: phi, dx, dy, dmaxcosp, dcosp, costriangleminangle

   do L=1,NMK(K)
      K1 = KN(1,nod(K)%lin(L)); K2 = KN(2,nod(K)%lin(L))
      if (K2 == K) then
         K2 = K1
         K1 = K
      end if

      !dx = getdx(xk(k1), yk(k1), xk(k2), yk(k2))
      !dy = getdy(xk(k1), yk(k1), xk(k2), yk(k2))
      call getdxdy(xk(k1), yk(k1), xk(k2), yk(k2),dx,dy,jsferic)
      if (abs(dx) < 1d-14 .and. abs(dy) < 1d-14) then
         if (dy < 0) then
            phi = -pi/2
         else
            phi = pi/2
         end if
      else
         phi = atan2(dy, dx)
      end if
      if ( L.eq.1 ) then
         phi0 = phi
      end if

      arglin(L) = phi-phi0
      if ( arglin(L).lt.0d0 ) arglin(L) = arglin(L) + 2d0*pi
   end do

   call indexx(nmk(k), arglin(1:nmk(k)), inn(1:nmk(k)))

   linnrs(1:nmk(k)) = nod(k)%lin(1:nmk(k))
   do L=1,nmk(k)
      nod(k)%lin(L) = linnrs(inn(L))
   end do

   return
   end subroutine sort_links_ccw

   !> get netcell polygon that is safe for periodic, spherical coordinates and poles
   subroutine get_cellpolygon(n, Msize, nn, xv, yv, LnnL, Lorg, zz)
   use network_data
   use m_missing, only : dmiss
   use m_sferic
   implicit none

   integer,                            intent(in)  :: n      !< cell number
   integer,                            intent(in)  :: Msize  !< array size
   integer,                            intent(out) :: nn     !< polygon size
   double precision, dimension(Msize), intent(out) :: xv, yv !< polygon coordinates
   integer,          dimension(Msize), intent(out) :: LnnL   !< original link LnnL
   integer,          dimension(Msize), intent(out) :: Lorg   !< original link number (>0) or added link (0)
   double precision,                   intent(out) :: zz     !< polygon-averaged value

   integer,          dimension(Msize)              :: kpole
   integer                                         :: num, numz, m, mp1, mp2, k1, k2, k3

   !  initialization
   xv   = 0d0
   yv   = 0d0
   LnnL = 0
   Lorg = 0
   zz   = 0d0
   nn   = netcell(n)%n

   if ( nn.lt.3 ) then
      return  ! safety
   end if

   !  check for poles
   kpole = 0
   if ( jsferic.eq.1 ) then
      do m=1,nn
         k1 = netcell(n)%nod(m)
         if ( abs(abs(yk(k1))-90d0).lt.dtol_pole ) then
            kpole(m) = 1
         end if
      end do
   end if

   zz = 0d0
   num = 0 ! number of nodes in polygon
   numz = 0
   do m  = 1,nn
      !num       = num+1
      !k1        = netcell(n)%NOD(m)
      !xv(num)   = xk(k1)
      !yv(num)   = yk(k1)
      !zz        = zz + zk(k1)
      !lnnl(num) = LNN(netcell(n)%lin(m))

      mp1 = m+1; if ( mp1.gt.nn ) mp1=mp1-nn
      k1 = netcell(n)%nod(m)
      k2 = netcell(n)%nod(mp1)

      if ( kpole(m).eq.1 .and. kpole(mp1).ne.1 ) then
         num = num+1
         xv(num) = xk(k2)
         yv(num) = yk(k1)
         if ( zk(k1).ne.DMISS ) then
            numz = numz+1
            zz = zz + zk(k1)
         end if
         lnnl(num) = LNN(netcell(n)%lin(m))
         Lorg(num) = m
      else if (  kpole(m).ne.1 .and. kpole(mp1).eq.1 ) then
         num = num+1
         xv(num) = xk(k1)
         yv(num) = yk(k1)
         if ( zk(k1).ne.DMISS ) then
            numz = numz+1
            zz   = zz + zk(k1)
         end if
         lnnl(num) = LNN(netcell(n)%lin(m))
         Lorg(num) = m

         !        add dummy link on pole ("unmerge")
         num = num+1
         xv(num) = xk(k1)
         yv(num) = yk(k2)
         if ( zk(k2).ne.DMISS ) then
            numz = numz+1
            zz   = zz + zk(k2)
         end if
         !         lnnl(num) = LNN(netcell(n)%lin(m))
         lnnl(num) = 1  ! fictitious boundary netlink

         !        use already existing dummy link if possible
         mp2 = mp1+1; if ( mp2.gt.nn) mp2=mp2-nn
         if ( kpole(mp2).eq.1 ) then
            Lorg(num) = mp1
         else  ! add dummy link
            Lorg(num) = 0
         end if
      else if ( kpole(m).ne.1 .and. kpole(mp1).ne.1 ) then
         num       = num+1
         k1        = netcell(n)%NOD(m)
         xv(num)   = xk(k1)
         yv(num)   = yk(k1)
         if ( zk(k1).ne.DMISS ) then
            numz = numz+1
            zz   = zz + zk(k1)
         end if
         lnnl(num) = LNN(netcell(n)%lin(m))
         Lorg(num) = m
      end if
   enddo
   nn = num

   if (numz.eq.0) then
      zz = DMISS
   else
      zz = zz / numz
   endif

   !  check periodicity
   if ( jsferic.eq.1 ) then
      do m=1,nn
         mp1 = m+1; if ( mp1.gt.nn ) mp1=mp1-nn
         if ( xv(mp1)-xv(m).gt.180d0 ) then
            xv(mp1) = xv(mp1) - 360d0
         else if ( xv(mp1)-xv(m).lt.-180d0 ) then
            xv(mp1) = xv(mp1) + 360d0
         end if
      end do
   end if

   return
   end subroutine get_cellpolygon

   !-----------------------------------------------------------------!
   ! rest.f90
   !-----------------------------------------------------------------!

   LOGICAL FUNCTION INVIEW(X,Y)
   use m_WEARELT
   use m_missing, only: dmiss
   implicit none
   double precision :: x
   double precision :: y
   !     ZIT IK IN ZOOMGEBIED? NULLEN EN DEFAULTS NIET

   IF (               X .NE. dmiss .AND.     &
      X .GT. X1 .AND. X .LT. X2 .AND.             &
      Y .GT. Y1 .AND. Y .LT. Y2     ) THEN
   INVIEW = .TRUE.
   ELSE
      INVIEW = .FALSE.
   ENDIF
   RETURN
   END FUNCTION INVIEW

   LOGICAL FUNCTION DINVIEW(XD,YD,ZD)
   implicit none
   double precision :: x
   double precision :: y
   double precision :: z
   DOUBLE PRECISION XD,YD,ZD
   CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
   DINVIEW = INVIEW(X,Y)
   RETURN
   END FUNCTION DINVIEW

   !-----------------------------------------------------------------!
   ! unstruct.F90
   !-----------------------------------------------------------------!

   !> Computes the bottom area of a cell and the center of mass coordinates.
   subroutine getcellsurface( n, ba, xzwr, yzwr) ! bottom area of cell nr n                       ! todo : sferic

   !lc use m_netw
   use network_data
   use m_missing, only : dmiss
   use geometry_module, only: comp_masscenter
   use m_sferic

   implicit none
   double precision :: ba, xzwr, yzwr
   integer          :: n

   ! locals
   integer          :: nn

   integer,                parameter :: MMAX=10   ! maximum cell polygon size
   double precision, dimension(MMAX) :: xh, yh    ! cell polygon node coordinates
   integer,          dimension(MMAX) :: LnnL      ! cell polygon link Lnn (not used here)
   integer,          dimension(MMAX) :: Lorg      ! cell polygon link number (not used here)
   double precision                  :: zz
   integer                           :: jaccw     ! counterclockwise (1) or not (0) (not used here)

   call get_cellpolygon(n,Mmax,nn,xh,yh,LnnL,Lorg,zz)
   call comp_masscenter(nn, xh , yh, xzwr, yzwr, ba, jaccw,  jsferic, jasfer3D, dmiss)

   end subroutine getcellsurface

   !> computes the cell-weighted center
   subroutine getcellweightedcenter(n, xz, yz, zz)

   use m_ggeo_orthosettings
   use m_missing,  only: jins, dmiss, dxymis
   use geometry_module, only : getcircumcenter, comp_circumcenter3D, comp_masscenter
   use network_data, only: netcell, xk, yk, zk, dcenterinside
   use m_sferic

   implicit none
   double precision   :: xz, yz, zz
   integer            :: n

   integer,                parameter :: MMAX = 10
   double precision, dimension(MMAX) :: xv, yv
   integer,          dimension(MMAX) :: Lorg
   integer,          dimension(MMAX) :: LnnL
   integer                           :: nn
   integer                           :: jaccw  ! counterclockwise (1) or not (0) (not used here)
   integer                           :: i, k

   double precision                  :: ba, xzw, yzw

   if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
      nn = netcell(n)%N
      do i=1,nn
         k = netcell(n)%nod(i)
         xv(i) = xk(k)
         yv(i) = yk(k)
      end do

      call comp_circumcenter3D(nn, xv, yv, xz, yz, jsferic, dmiss)
   else
      !   get the cell polygon that is safe for periodic, spherical coordinates, inluding poles
      call get_cellpolygon(n,Mmax,nn,xv,yv,LnnL,Lorg,zz)
      call getcircumcenter(nn, xv, yv, lnnl, xz, yz, jsferic, jasfer3D, jglobe, jins, dmiss, dxymis, dcenterinside)
   end if

   if (circumormasscenter .ne. 1d0) then
      !   update with cell mass center
      call comp_masscenter(nn, xv, yv, xzw, yzw, ba, jaccw, jsferic, jasfer3D, dmiss)

      xz = circumormasscenter*xz + (1d0-circumormasscenter)*xzw
      yz = circumormasscenter*yz + (1d0-circumormasscenter)*yzw
   endif
   ! CALL CIRR(XZ,YZ,31)

   end subroutine getcellweightedcenter


   !-----------------------------------------------------------------!
   ! Library public functions
   !-----------------------------------------------------------------!
   function make1D2Dinternalnetlinks() result(ierr)

   use m_cell_geometry, only: xz, yz
  use network_data
  use m_alloc
  use m_missing, only:  dmiss, dxymis, jadelnetlinktyp
  use geometry_module, only: dbdistance, normalout
  use m_sferic, only: jsferic, jasfer3D

   implicit none

   integer          :: K1, K2, K3, L, NC1, NC2, JA, KK2(2), KK, NML
   integer          :: i, ierr, k, kcell
   DOUBLE PRECISION :: XN, YN, XK2, YK2, WWU

   call SAVENET()
   call findcells(0)

   KC = 2
   DO L = 1,NUML  ! FLAG TO 1 ANY NODE TOUCHED BY SOMETHING 1D
      K1  = KN(1,L) ; K2  = KN(2,L); K3 = KN(3,L)
      IF (K3 .NE. 4 .AND. K3 .NE. 2 .AND. K3 .NE. 0) THEN ! only for yet-isolated 1D channels with KN(3,L)==1
         KC(K1) = 1 ; KC(K2) = 1
      ENDIF
   ENDDO

   if (jadelnetlinktyp .ne. 0) then
      kn3typ = jadelnetlinktyp
   else
      kn3typ = 3
   endif

   NML  = NUML
   DO K = 1,NUMK

      IF (NMK(K) > 0) THEN ! == 2 .or. ) THEN

         IF (KC(K) == 1) THEN
            NC1 = 0
            CALL INCELLS(XK(K), YK(K), NC1)
            IF (NC1 > 1) THEN
               CALL SETNEWPOINT(XZ(NC1),YZ(NC1),ZK(K) ,NC2)
               call connectdbn(NC2, K, L)
               KN(3,L) = kn3typ
            ELSE
               DO KK = 1, min(2, NMK(K))
                  L  = NOD(K)%LIN(KK)
                  KK2(KK) = KN(1,L) + KN(2,L) - K
               ENDDO
               K1 = KK2(1) ;
               IF(NMK(K) == 1) THEN
                  K2 = K
               ELSE
                  K2 = KK2(2)
               ENDIF

               CALL normalout(XK(K1), YK(K1), XK(K2), YK(K2) , XN, YN, jsferic, jasfer3D, dmiss, dxymis)

               WWU = 5D0*DBDISTANCE(XK(K1), YK(K1), XK(K2), YK(K2), jsferic, jasfer3D, dmiss )

               XK2 = XK(K) + XN*WWU
               YK2 = YK(K) + YN*WWU
               CALL CROSSED2d_BNDCELL(NML, XK(K), YK(K), XK2, YK2, NC1)

               IF (NC1 > 1) THEN
                  CALL SETNEWPOINT(XZ(NC1),YZ(NC1),ZK(K) ,NC2)
                  call connectdbn(NC2, K, L)
                  KN(3,L) = kn3typ
               ENDIF

               XK2 = XK(K) - XN*WWU
               YK2 = YK(K) - YN*WWU
               CALL CROSSED2d_BNDCELL(NML, XK(K), YK(K), XK2, YK2, NC1)

               IF (NC1 > 1) THEN
                  CALL SETNEWPOINT(XZ(NC1),YZ(NC1),ZK(K) ,NC2)
                  call connectdbn(NC2, K, L)
                  KN(3,L) = 3
               ENDIF

            ENDIF

         ENDIF

      ENDIF

   ENDDO

   CALL SETNODADM(0)

   end function make1D2Dinternalnetlinks

   !< converter function
   function ggeo_convert(meshgeom) result(ierr)

   use meshdata
   use network_data
   use m_missing, only : dmiss

   implicit none
   type(t_ug_meshgeom), intent(in)      :: meshgeom
   integer                              :: numk_last, numl_last, ierr, numk_read, l

   ierr = 0

   numk_last = LNUMK
   numl_last = LNUML
   numk_read = meshgeom%numnode

   !Prepare net vars for new data and fill with values from file, increases nod, xk, yk, zk, kn if needed

   call increasenetw(numk_last + meshgeom%numnode, numl_last + meshgeom%numedge)
   XK(numk_last+1:numk_last+numk_read) = meshgeom%nodex(:)
   YK(numk_last+1:numk_last+numk_read) = meshgeom%nodey(:)
   ZK(numk_last+1:numk_last+numk_read) = dmiss

   do l=1,meshgeom%numedge
      ! Append the netlink table, and also increment netnode numbers in netlink array to ensure unique ids.
      kn(1:2,numl_last+l) = numk_last + meshgeom%edge_nodes(1:2,l) !to calculate in 1D!
      kn(3,  numl_last+l) = meshgeom%dim
   end do

   !Increase the number of links
   NUML = numl_last + meshgeom%numedge
   NUMK = numk_last + meshgeom%numnode

   LNUMK = NUMK
   LNUML = NUML

   end function ggeo_convert

   function ggeo_deallocate() result (ierr)

   use network_data
   use m_dimens

   integer ierr
   ierr = network_data_destructor()
   ierr = m_dimens_destructor()

   end function

   !< get the number of created links
   function ggeo_get_links_count(nlinks) result(ierr)

   use network_data

   integer, intent(inout)  :: nlinks
   integer                 :: l, ierr

   ierr = 0
   nlinks = 0
   do l=1,NUML
      if(kn(3,l).eq.3) then
         nlinks = nlinks + 1
      end if
   end do

   end function ggeo_get_links_count

   !< get the links
   function ggeo_get_links(arrayfrom, arrayto)  result(ierr)

   use network_data

   integer, intent(inout):: arrayfrom(:), arrayto(:)
   integer :: ierr, nlinks, l, nc

   ierr     = 0
   nlinks   = 0

   do l=1,numl
      if(kn(3,l).eq.3) then
         nlinks = nlinks + 1
         nc = 0
         call incells(xk(kn(1,l)), yk(kn(1,l)), nc)
         if (nc < 1) then
            ierr = -1
            return
         endif
         arrayfrom(nlinks) = nc  !2d cell
         arrayto(nlinks)   = kn(2,l)  !1dlink
      end if
   end do

   end function ggeo_get_links

   !< create meshgeom from array
   function ggeo_convert_1d_arrays(nodex, nodey, branchoffset, branchlength, branchidx, sourcenodeid, targetnodeid, meshgeom, startindex) result(ierr)

   use meshdata
   use m_alloc

   double precision, intent(in)         :: nodex(:), nodey(:), branchoffset(:), branchlength(:)
   integer, intent(in)                  :: branchidx(:), sourcenodeid(:), targetnodeid(:), startindex
   type(t_ug_meshgeom), intent(inout)   :: meshgeom
   integer                              :: ierr, numedge, nbranches

   ierr = 0
   nbranches = size(sourcenodeid)
   meshgeom%dim = 1
   meshgeom%numnode =  size(nodex,1)
   allocate(meshgeom%nodex(meshgeom%numnode))
   allocate(meshgeom%nodey(meshgeom%numnode))
   allocate(meshgeom%branchidx(size(branchidx,1)))

   ierr = ggeo_edge_nodes_count(sourcenodeid, targetnodeid, meshgeom%numnode, numedge)
   meshgeom%numedge = numedge
   allocate(meshgeom%edge_nodes(2, meshgeom%numedge))

   !Assign the node coordinates
   meshgeom%nodex      =  nodex
   meshgeom%nodey      =  nodey
   meshgeom%branchidx =  branchidx

   !Calculate the edge_nodes
   ierr = ggeo_create_edge_nodes(meshgeom%branchidx, branchoffset, sourcenodeid, targetnodeid, meshgeom%edge_nodes, branchlength, startindex)

   end function ggeo_convert_1d_arrays

   function ggeo_edge_nodes_count(sourcenodeid, targetnodeid, numnode, numedge) result(ierr)

   integer, intent(in)     :: sourcenodeid(:), targetnodeid(:), numnode
   integer, intent(inout)  :: numedge
   integer                 :: ierr, i, k, nnetworknodes, noverlaps, nbranches
   integer, allocatable    :: connectedBranches(:)

   ierr = 0
   nbranches = size(sourcenodeid)
   ! calculate the number of edge nodes, considering the overlaps
   ! (if more nodes are shared, then we have less edge nodes)
   nnetworknodes = max(maxval(sourcenodeid),maxval(targetnodeid))
   allocate(connectedBranches(nnetworknodes))
   connectedBranches = 0
   do i = 1, nnetworknodes
      !for each node count how many branches are sharing it
      do k = 1, nbranches
         if ((targetnodeid(k).eq.i).or.(sourcenodeid(k).eq.i)) then
            connectedBranches(i) = connectedBranches(i) + 1
         endif
      enddo
   enddo
   noverlaps = sum(connectedBranches) - nnetworknodes
   numedge = numnode - (nBranches - noverlaps)

   end function ggeo_edge_nodes_count


   !< Algorithm to calculate the edgenodes array. The only assumption made here is that the mesh nodes are written consecutively,
   !< in the same direction indicated by the sourcenodeid and the targetnodeid arrays (e.g.
   !< 1  -a-b-c-> 2 and not 1 -c-a-b-> 2 where 1 and 2 are network nodes and a, b, c are mesh nodes )
   function ggeo_create_edge_nodes(branchidx, branchoffset, sourcenodeid, targetnodeid, edgenodes, branchlength, startindex) result(ierr) !edge_nodes

   integer, intent(in)          :: branchidx(:), sourcenodeid(:), targetnodeid(:), startindex
   double precision, intent(in) :: branchoffset(:),branchlength(:)
   integer, intent(inout)       :: edgenodes(:,:)
   integer, allocatable         :: meshnodemapping(:),internalnodeindexses(:)
   integer                      :: nnetworknodes, nBranches,nmeshnodes, ierr , k , n, br, st, en, kk, firstvalidarraypos

   ierr = 0
   firstvalidarraypos = 0

   if (startindex.eq.0) then
      firstvalidarraypos = 1
   endif

   !Build mesh mapping: assuming not overlapping mesh nodes
   nnetworknodes = max(maxval(sourcenodeid),maxval(targetnodeid)) + firstvalidarraypos
   nmeshnodes = size(branchidx)
   nbranches = size(sourcenodeid)
   allocate(meshnodemapping(nnetworknodes))
   allocate(internalnodeindexses(nmeshnodes))

   !map the mesh nodes
   meshnodemapping = -1
   do br = 1, nbranches
      do n=1, nmeshnodes
         if ((abs(branchoffset(n)).le.1e-6).and.(branchidx(n)+firstvalidarraypos.eq.br)) then
            meshnodemapping(sourcenodeid(br)+firstvalidarraypos) = n
         endif
         if ((abs(branchoffset(n)-branchlength(br)).le.1e-6).and.(branchidx(n)+firstvalidarraypos.eq.br)) then
            meshnodemapping(targetnodeid(br)+firstvalidarraypos) = n
         endif
      enddo
   enddo

   k = 0
   do br = 1, nbranches
      if ((meshnodemapping(sourcenodeid(br)+firstvalidarraypos).eq.-1).or.(meshnodemapping(targetnodeid(br)+firstvalidarraypos).eq.-1)) then
         ierr = -1
         return
      endif
      ! starting and ending nodes
      st =  meshnodemapping(sourcenodeid(br)+firstvalidarraypos)
      en =  meshnodemapping(targetnodeid(br)+firstvalidarraypos)
      !the nodes between
      kk =  0
      do n=1, nmeshnodes
         if(branchidx(n)+firstvalidarraypos.eq.br.and.(n.ne.st).and.(n.ne.en)) then
            kk = kk + 1
            internalnodeindexses(kk) = n
         endif
      enddo
      !connect the start node to the first internal node
      k = k + 1
      edgenodes(1,k) = st
      edgenodes(2,k) = internalnodeindexses(1)
      !connect end node to iternal
      do n =1, kk - 1
         k = k +1
         edgenodes(1,k) = internalnodeindexses(n)
         edgenodes(2,k) = internalnodeindexses(n)  + 1
      enddo
      !connect the last internal node to the end node
      k = k + 1
      edgenodes(1,k) = internalnodeindexses(kk)
      edgenodes(2,k) = en
   enddo

   end function ggeo_create_edge_nodes

   end module gridoperations