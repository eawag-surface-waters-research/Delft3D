   module gridoperations

   !use module (should not be used)

   implicit none
   
   !public functions
   public :: make1D2Dinternalnetlinks
   public :: ggeo_convert
   public :: ggeo_convert_1d_arrays
   public :: ggeo_get_links_count
   public :: ggeo_get_links
   public :: ggeo_create_edge_nodes
   
   !All subroutines are made private (we do not expose them for now)
   private
   
   contains
   
   !-----------------------------------------------------------------!
   ! net.f90
   !-----------------------------------------------------------------!
   
   !> Restore variables with backup data
   SUBROUTINE RESTORE()
   !LC use m_netw
   use network_ggeo_data
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
      LS0 = NMK0(K)
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

   !> Save variables in the back ups
   SUBROUTINE SAVENET()
   !LC use m_netw
   use network_ggeo_data
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
   use m_ggeo_missing
   !LC removed use m_netw
   use network_ggeo_data
   !LC
   use m_alloc
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

      NMK = 0 ; KC = 1 ; XK = XYMIS ; YK = XYMIS ; ZK = zkUNI
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
   use network_ggeo_data
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
      allocate(netcell0(nump), stat = ierr)
      CALL AERR('netcell0(nump)', ierr, nump)

      do p=1,nump
         n0 = netcell(p)%n
         if (n0 <= 0) then
            cycle
         end if

         allocate(netcell0(p)%nod(n0), netcell0(p)%lin(n0), stat = ierr)
         !CALL AERR('netcell0(p)%nod(n0), netcell0(p)%lin(n0)', ierr, 2*n0)

         netcell0(p)%n   = netcell(p)%n
         netcell0(p)%nod = netcell(p)%nod
         netcell0(p)%lin = netcell(p)%lin

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
         n0 = netcell0(p)%n
         if (n0 <= 0) then
            cycle
         end if

         allocate(netcell(p)%nod(n0), netcell(p)%lin(n0), stat = ierr)
         !CALL AERR('netcell(p)%nod(n0), netcell(p)%lin(n0)', ierr, 2*n0)

         netcell(p)%n   = netcell0(p)%n
         netcell(p)%nod = netcell0(p)%nod
         netcell(p)%lin = netcell0(p)%lin

         deallocate(netcell0(p)%nod, netcell0(p)%lin)
      end do

      deallocate(netcell0)
      CALL AERR('netcell0', 0, -nump)
   end if

   end subroutine increasenetcells

   !> check and see if the links already form a cell
   logical function alreadycell(N, K, L)
   !use m_netw
   use network_ggeo_data
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
   !LC use m_netw
   use network_ggeo_data
   use m_ggeo_missing
   implicit none
   integer :: jav
   integer :: jview
   double precision :: xyz
   double precision :: XP, YP, ZP
   integer :: K1

   !LC old common block for viewing, it can go
   !COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4

   JVIEW     = 1
   JAV       = 3
   XYZ       = 0

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
   use network_ggeo_data
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
            CALL CROSSinbox (XP1, YP1, XP2, YP2, XK(K1), YK(K1), XK(K2), YK(K2), jacros, SL, SM, XCR, YCR, CRP)
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
   !LC use m_netw
   use network_ggeo_data
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
   use network_ggeo_data
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

   SUBROUTINE SETNODADM(JACROSSCHECK)
   !LC use m_netw
   use network_ggeo_data
   use m_ggeo_missing
   use m_ggeo_sferic, only: pi, dg2rd
   !LC use unstruc_messages
   use M_GGEO_TRIANGLE, only: triangleminangle

   implicit none
   INTEGER               :: JACROSSCHECK

   double precision :: crp, e, e1
   integer          :: jacros, mout
   integer          :: k, k1, k12, k2, k22, k3, KI, ka, kb, kk, L, L1, L2, LL, LLL, LI, LTOT, ls, JA
   INTEGER          :: jDupLinks, jOverlapLinks, jSmallAng, maxlin
   double precision :: sl, sm, xcr, ycr

   INTEGER, ALLOCATABLE          ::  KC2(:), KN2(:,:), KCK(:)
   double precision, allocatable :: arglin(:)
   integer, allocatable          :: linnrs(:), inn(:)
   !LC   LOGICAL                       :: DINVIEW
   double precision              :: getdx, getdy !LC , dcosphi
   double precision              :: phi, dx, dy, dmaxcosp, dcosp, costriangleminangle, phi0

   double precision :: X(4), Y(4)


   IF (NUML == 0) RETURN

   E = 1E-6 ; E1 = 1-E

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
               CALL CROSS(XK(K1), YK(K1), XK(K2), YK(K2), XK(KA), YK(KA), XK(KB), YK(KB), JACROS,SL,SM,XCR,YCR,CRP)
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
            ELSE IF (K3 == 1 .OR. K3 > 2) THEN
               L1 = L1 + 1
               KN(1,L1) = K1 ; KN(2,L1) = K2 ; KN(3,L1) = K3
            ENDIF
            KC(K1)   = 1  ; KC(K2)   = 1
         ENDIF
      ENDIF
   ENDDO

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
   costriangleminangle = cos(triangleminangle*dg2rd)
   if ( triangleminangle > 0 ) then
      lnl:do L=1,NUML
         k1 = kn(1,L) ; k2 = kn(2,L) ; k3 = kn(3,L)

         if (k3 == 1) cycle

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

               dcosp = dcosphi(xk(k1), yk(k1), xk(k2), yk(k2), xk(k1), yk(k1), xk(kb), yk(kb))
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
            !LC write(msgbuf, '(a,i8, a)') 'Removed link', L, ', because of tiny angles at endpoints.'
            !LC call msg_flush()
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

   !  netcell administration out of date
   netstat = NETSTAT_CELLS_DIRTY
   END SUBROUTINE SETNODADM

   SUBROUTINE INIALLOCnetcell()
   !LC use m_netw
   use network_ggeo_data
   !LC
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
   use network_ggeo_data
   use m_ggeo_flowgeom
   use m_alloc
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
   !LC use m_netw
   use network_ggeo_data
   use m_ggeo_tpoly, only: dbpinpol
   !LC use m_ggeo_flowgeom
   implicit none
   integer, intent(in) :: JP !< Type of cells to find (unfolded: 3: triangle, etc. up to 6=hexa, 0 = all; folded: code+100; no new nodemask (nonzero values will be used as mask here): code+1000, no sednodadm: code+10000)

   integer, allocatable, dimension(:) :: kc_sav  ! save of kc

   integer :: ik
   integer :: k
   integer :: k1
   integer :: k2
   integer :: l
   integer :: jafold, jakeepmask, jasetnodadm
   integer :: jp_

   jp_ = jp

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
      CALL SETNODADM(0)
   end if

   CALL INIALLOCnetcell()

   LC   =  0
   IK   = -1

   if ( jakeepmask.ne.1 ) then
      KC   =  0
      DO K = 1,NUMK
         CALL DBPINPOL(xk(k), yk(k), ik)
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

   !LC:
   !NDX2D = NUMP                             ! NR OF 2d CELLS=NUMP

   lasttopology = numk + numl


   ! set network status
   netstat = NETSTAT_OK

   RETURN
   END SUBROUTINE FINDCELLS

   SUBROUTINE FINDTRIS(jafold)
   !use m_netw
   use network_ggeo_data
   USE M_ggeo_afmeting
   use m_alloc
   implicit none

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
   !LC LOGICAL RECHTSAF
   !LC logical :: alreadycell
   !LC logical :: iscounterclockwise

   !LC CALL READYY ('FIND TRIS', 0d0)

   nmkmax = 1
   if ( jafold.eq.1 ) nmkmax = 1000

   KMOD = max(1,NUMK/100)
   DO K1 = 1,NUMK

      !LC IF (MOD(K1,KMOD) == 1) CALL READYY ('FIND TRIS',dble(K1)/dble(NUMK))

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

   !LC CALL READYY ( 'FIND TRIS', -1d0 )

   RETURN
   END SUBROUTINE FINDTRIS

   SUBROUTINE FINDQUADS(jafold)
   !LC use m_netw
   use network_ggeo_data
   USE M_ggeo_afmeting
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
   !LC LOGICAL RECHTSAF
   !LC logical :: alreadycell
   !LC logical :: iscounterclockwise


   !LC CALL READYY('FIND QUADS',0d0)

   nmkmax = 1
   if ( jafold.eq.1 ) nmkmax = 1000

   KMOD = max(1,NUMK/100)
   DO K1 = 1,NUMK

      if (mod(k1,KMOD) == 1) then
         af = dble(k1) /dble(numk)
         !LC CALL READYY('FIND QUADS',AF)
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

   !LC CALL READYY('FIND QUADS',-1d0)

   RETURN
   END SUBROUTINE FINDQUADS

   SUBROUTINE FINDPENTAS(jafold)
   !LC use m_netw
   use network_ggeo_data
   USE M_ggeo_afmeting
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
   !LC LOGICAL RECHTSAF
   !LC logical :: alreadycell
   !LC logical :: iscounterclockwise

   !LC CALL READYY ('FINDPENTAS',0d0)

   nmkmax = 1
   if ( jafold.eq.1 ) nmkmax = 1000

   KMOD = max(1,NUMK/100)
   DO K1 = 1,NUMK

      !LC IF (MOD(K1,KMOD) == 1) CALL READYY ('FINDPENTAS',dble(K1)/dble(NUMK))

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

   !LC CALL READYY ('FINDPENTAS', -1d0)

   RETURN
   END SUBROUTINE FINDPENTAS

   SUBROUTINE FINDHEXAS(jafold)
   !LC use m_netw
   use network_ggeo_data
   USE M_ggeo_afmeting
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
   use network_ggeo_data
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

   call comp_masscenter(N, xv, yv, xdum, ydum, darea, jacounterclockwise)
   if ( jacounterclockwise.eq.1 ) then
      iscounterclockwise = .true.
   else
      iscounterclockwise = .false.
   end if

   return
   end function iscounterclockwise

   LOGICAL FUNCTION RECHTSAF(K1,K2,K3)
   use network_ggeo_data
   implicit none
   integer :: K1, K2, K3

   double precision :: sig

   rechtsaf = .false.
   return

   call duitpl(xk(k1), yk(k1), xk(k2), yk(k2), xk(k2), yk(k2), xk(k3), yk(k3), sig)
   if (sig < 0) then
      rechtsaf = .true.
   else
      rechtsaf = .false.
   endif

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
   use network_ggeo_data
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
   use network_ggeo_data
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
   use network_ggeo_data
   implicit none
   integer :: K, LK, L

   CALL CHKLINSIZTONODE(K)
   NOD(K)%LIN(LK) = L
   RETURN
   END SUBROUTINE SETNODLIN

   SUBROUTINE CHKLINSIZTONODE(KK)
   !LC use m_netw
   use network_ggeo_data
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
   use network_ggeo_data
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

   !LC for viewing, it can go COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4

   JVIEW     = 1
   JAV       = 3
   XYZ       = 0

   !LC COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
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
      !LC     CALL DVIEW(XD,YD,-ZD,X,Y,Z)
   ENDIF
   RETURN
   END SUBROUTINE DRIETWEE

   SUBROUTINE TWEEDRIE(X,Y,XD,YD,ZD)
   implicit none
   integer :: jav
   integer :: jview
   double precision :: xyz
   double precision :: X,Y,XD,YD,ZD
   !LC COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4

   JVIEW     = 1
   JAV       = 3
   XYZ       = 0

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
   use m_ggeo_missing
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

   SUBROUTINE INDEXX(N,ARRIN,INDX)
   implicit none
   integer :: i
   integer :: indxt
   integer :: ir
   integer :: j
   integer :: l
   double precision :: q
   integer :: N
   double precision :: ARRIN(N)
   integer :: INDX(N)
   DO J=1,N
      INDX(J)=J
   ENDDO
   IF (N == 1) RETURN
   L=N/2+1
   IR=N
10 CONTINUE
   IF(L.GT.1)THEN
      L=L-1
      INDXT=INDX(L)
      Q=ARRIN(INDXT)
   ELSE
      INDXT=INDX(IR)
      Q=ARRIN(INDXT)
      INDX(IR)=INDX(1)
      IR=IR-1
      IF(IR.EQ.1)THEN
         INDX(1)=INDXT
         RETURN
      ENDIF
   ENDIF
   I=L
   J=L+L
20 IF(J.LE.IR)THEN
      IF(J.LT.IR)THEN
         IF(ARRIN(INDX(J)).LT.ARRIN(INDX(J+1)))J=J+1
      ENDIF
      IF(Q.LT.ARRIN(INDX(J)))THEN
         INDX(I)=INDX(J)
         I=J
         J=J+J
      ELSE
         J=IR+1
      ENDIF
      GO TO 20
   ENDIF
   INDX(I)=INDXT
   GO TO 10
   END SUBROUTINE INDEXX

   SUBROUTINE INCELLS(XA,YA,KIN)
   !use m_netw
   use network_ggeo_data
   use m_ggeo_tpoly, only: pinpok
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
      CALL PINPOK(XA, YA , NN, XH, YH, IN)
      IF (IN == 1) THEN
         KIN = K
         RETURN
      ENDIF
   ENDDO
   END SUBROUTINE INCELLS

   !> sort links in nod%lin counterclockwise (copy-paste from setnodadm)
   subroutine sort_links_ccw(k,maxlin,linnrs,arglin,inn)
   !LC use m_netw
   use network_ggeo_data
   use m_ggeo_sferic

   implicit none

   integer,          intent(in)    :: k                           !< node number
   integer,          intent(in)    :: maxlin                      !< array size


   double precision, intent(inout) :: arglin(maxlin)              ! dummy array
   integer,          intent(inout) :: linnrs(maxlin), inn(maxlin) ! dummy arrays

   integer                         :: k1, k2, L, LL

   integer                         :: jDupLinks, jOverlapLinks, jSmallAng
   double precision                :: sl, sm, xcr, ycr, phi0

   double precision                :: getdx, getdy, dcosphi
   double precision                :: phi, dx, dy, dmaxcosp, dcosp, costriangleminangle

   do L=1,NMK(K)
      K1 = KN(1,nod(K)%lin(L)); K2 = KN(2,nod(K)%lin(L))
      if (K2 == K) then
         K2 = K1
         K1 = K
      end if

      !dx = getdx(xk(k1), yk(k1), xk(k2), yk(k2))
      !dy = getdy(xk(k1), yk(k1), xk(k2), yk(k2))
      call getdxdy(xk(k1), yk(k1), xk(k2), yk(k2),dx,dy)
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

   !> compute area and mass center of polygon, in two-dimensional space or three-dimensional space depending on "jsferic" and "jasfer3D"
   subroutine comp_masscenter(N, xin , y, xcg, ycg, area, jacounterclockwise)
   use m_ggeo_sferic

   implicit none

   integer,                        intent(in)    :: N        !< polygon size
   double precision, dimension(N), intent(in)    :: xin, y     !< polygon coordinates
   double precision,               intent(out)   :: xcg, ycg !< polygon mass center coordinates
   double precision,               intent(out)   :: area     !< polygon area
   integer,                        intent(out)   :: jacounterclockwise  !< counterclockwise (1) or not (0)

   if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
      call comp_masscenter3D(N, xin , y, xcg, ycg, area, jacounterclockwise)
   else
      call comp_masscenter2D(N, xin , y, xcg, ycg, area, jacounterclockwise)
   end if

   return
   end subroutine comp_masscenter

   !> compute area and mass center of polygon in two-dimensional space
   subroutine comp_masscenter2D(N, xin , y, xcg, ycg, area, jacounterclockwise)
   use m_ggeo_sferic

   implicit none

   integer,                        intent(in)    :: N        !< polygon size
   double precision, dimension(N), intent(in)    :: xin, y     !< polygon coordinates
   double precision,               intent(out)   :: xcg, ycg !< polygon mass center coordinates
   double precision,               intent(out)   :: area     !< polygon area
   integer,                        intent(out)   :: jacounterclockwise  !< counterclockwise (1) or not (0)

   double precision, dimension(N) :: x  ! Copy of xin, with possibly periodic fixes.
   double precision                              :: dsx, dsy, xc, yc, dcos, xds, fac, x0, y0, x1, dx0, dx1, dy0, dy1
   double precision                              :: xdum

   integer                                       :: i, ip1

   !LC double precision, external                    :: getdx, getdy

   double precision, parameter                   :: dtol=1d-8

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

      ! xc = 0.5d0*(getdx(x0,y0,x(i),y(i)) + getdx(x0,y0,x(ip1),y(ip1)))
      ! yc = 0.5d0*(getdy(x0,y0,x(i),y(i)) + getdy(x0,y0,x(ip1),y(ip1)))


      call getdxdy(x(i), y(i), x(ip1), y(ip1), dx0, dy0)
      dsx = dy0 ; dsy = -dx0

      !dsx =  getdy(x(i), y(i), x(ip1), y(ip1))
      !dsy = -getdx(x(i), y(i), x(ip1), y(ip1))

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
   end subroutine comp_masscenter2D

   !> compute area and mass center of polygon
   subroutine comp_masscenter3D(N, x, y, xcg, ycg, area, jacounterclockwise)
   use m_ggeo_sferic

   implicit none

   integer,                        intent(in)    :: N        !< polygon size
   double precision, dimension(N), intent(in)    :: x, y     !< polygon coordinates
   double precision,               intent(out)   :: xcg, ycg !< polygon mass center coordinates
   double precision,               intent(out)   :: area     !< polygon area
   integer,                        intent(out)   :: jacounterclockwise  !< counterclockwise (1) or not (0)

   !   double precision, dimension(N)                :: x  ! Copy of xin, with possibly periodic fixes.

   double precision, dimension(N)                :: xx, yy, zz ! 3D coordinates

   double precision, dimension(4,4)              :: A
   double precision, dimension(4)                :: rhs

   double precision, dimension(N)                :: DvolDx, DvolDy, DvolDz

   double precision                              :: xx0, yy0, zz0, alpha
   double precision                              :: xxcg, yycg, zzcg
   double precision                              :: dvol, vol, voli
   double precision                              :: Jx, Jy, Jz
   double precision                              :: Rai
   double precision                              :: sx, sy, sz
   double precision                              :: xmin, xmax

   integer                                       :: i, ip1, iter


   !   double precision, external                    :: getdx, getdy

   integer,          parameter                   :: MAXITER=100
   double precision, parameter                   :: dtol=1d-8
   double precision, parameter                   :: deps=1d-8
   double precision, parameter                   :: onesixth = 0.166666666666666667d0

   area = 0d0
   xcg = 0d0
   ycg = 0d0
   jacounterclockwise = 1

   if ( N.lt.1 ) goto 1234

   do i=1,N
      call sphertocart3D(x(i), y(i), xx(i), yy(i), zz(i))
   end do

   Rai = 1d0/Ra

   !  first iterate
   xx0 = 0
   yy0 = 0
   zz0 = 0
   do i=1,N
      xx0 = xx0 + xx(i)
      yy0 = yy0 + yy(i)
      zz0 = zz0 + zz(i)
   end do
   xx0 = xx0/N
   yy0 = yy0/N
   zz0 = zz0/N
   alpha = 0.75d0

   !  Newton iterations
   do iter=1,MAXITER

      !     compute volume
      vol = 0d0
      do i=1,N
         ip1 = i+1; if ( ip1.gt.N ) ip1=ip1-N

         DvolDx(i) = onesixth * ( yy(i)*zz(ip1) - zz(i)*yy(ip1))
         DvolDy(i) = onesixth * ( zz(i)*xx(ip1) - xx(i)*zz(ip1))
         DvolDz(i) = onesixth * ( xx(i)*yy(ip1) - yy(i)*xx(ip1))

         dvol = DvolDx(i)*xx0 + DvolDy(i)*yy0 + DvolDz(i)*zz0
         vol = vol + dvol
      end do

      voli = 1d0/vol

      A    = 0d0
      rhs  = 0d0
      Jx = (0.25d0-alpha)*xx0 !*vol
      Jy = (0.25d0-alpha)*yy0 !*vol
      Jz = (0.25d0-alpha)*zz0 !*vol
      do i=1,N
         ip1 = i+1; if ( ip1.gt.N ) ip1=ip1-N

         dvol = (DvolDx(i)*xx0 + DvolDy(i)*yy0 + DvolDz(i)*zz0) * voli  ! *vol

         Jx = Jx + 0.25d0*dvol*(xx(i)+xx(ip1))
         Jy = Jy + 0.25d0*dvol*(yy(i)+yy(ip1))
         Jz = Jz + 0.25d0*dvol*(zz(i)+zz(ip1))

         xxcg = 0.25d0*(xx0+xx(i)+xx(ip1))
         yycg = 0.25d0*(yy0+yy(i)+yy(ip1))
         zzcg = 0.25d0*(zz0+zz(i)+zz(ip1))

         A(1,1) = A(1,1) + xxcg * dvoldx(i)
         A(1,2) = A(1,2) + xxcg * dvoldy(i)
         A(1,3) = A(1,3) + xxcg * dvoldz(i)

         A(2,1) = A(2,1) + yycg * dvoldx(i)
         A(2,2) = A(2,2) + yycg * dvoldy(i)
         A(2,3) = A(2,3) + yycg * dvoldz(i)

         A(3,1) = A(3,1) + zzcg * dvoldx(i)
         A(3,2) = A(3,2) + zzcg * dvoldy(i)
         A(3,3) = A(3,3) + zzcg * dvoldz(i)
      end do

      A(1,1) = voli*A(1,1) + 0.25-alpha
      A(1,2) = voli*A(1,2)
      A(1,3) = voli*A(1,3)
      A(1,4) = -xx0*Rai

      A(2,1) = voli*A(2,1)
      A(2,2) = voli*A(2,2) + 0.25-alpha
      A(2,3) = voli*A(2,3)
      A(2,4) = -yy0*Rai

      A(3,1) = voli*A(3,1)
      A(3,2) = voli*A(3,2)
      A(3,3) = voli*A(3,3) + 0.25-alpha
      A(3,4) = -zz0*Rai

      A(4,1) = -xx0*Rai
      A(4,2) = -yy0*Rai
      A(4,3) = -zz0*Rai
      A(4,4) = 0d0

      rhs(1) = -Jx   ! *dvoli
      rhs(2) = -Jy   ! *dvoli
      rhs(3) = -Jz   ! *dvoli
      rhs(4) = -0.5*(Ra**2 - (xx0**2+yy0**2+zz0**2))*Rai

      !     solve system
      call gaussj(A,4,4,rhs,1,1) ! rhs contains solution

      !     update coordinates of centerpoint
      xx0 = xx0 + rhs(1)
      yy0 = yy0 + rhs(2)
      zz0 = zz0 + rhs(3)
      alpha = alpha + rhs(4)*Rai

      !     check convergence
      if ( rhs(1)**2 + rhs(2)**2 + rhs(3)**2 + rhs(4)**2 .lt. deps ) then
         exit
      end if

   end do

   !  check convergence
   if ( iter.ge.MAXITER ) then
      !LC call qnerror('comp_masscenter: no convergence', ' ', ' ')
   end if

   !  compute area
   Area = 0d0
   do i=1,N
      ip1 = i+1; if ( ip1.gt.N ) ip1=ip1-N
      sx = 0.5d0*( (yy(i)-yy0) * (zz(ip1)-zz0) - (zz(i)-zz0) * (yy(ip1)-yy0) )
      sy = 0.5d0*( (zz(i)-zz0) * (xx(ip1)-xx0) - (xx(i)-xx0) * (zz(ip1)-zz0) )
      sz = 0.5d0*( (xx(i)-xx0) * (yy(ip1)-yy0) - (yy(i)-yy0) * (xx(ip1)-xx0) )

      Area = Area + sqrt(sx**2+sy**2+sz**2)

   end do

   !   write(6,*) Area*(Ra/3d0*voli)

   call Cart3Dtospher(xx0,yy0,zz0,xcg,ycg,maxval(x(1:N)))

   !  output cell orientation
   if ( vol.gt.0d0 ) then
      jacounterclockwise = 1
   else
      jacounterclockwise = 0
   end if

   !!  fix for inward normals (clockwise oriented cells)
   !   area = abs(area)

1234 continue

   return
   end subroutine comp_masscenter3D

   !> get netcell polygon that is safe for periodic, spherical coordinates and poles
   subroutine get_cellpolygon(n, Msize, nn, xv, yv, LnnL, Lorg, zz)
   use m_ggeo_sferic
   use m_ggeo_missing
   use network_ggeo_data
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

   !> compute circumcenter using 3D coordinates
   subroutine comp_circumcenter3D(N, xv, yv, xz, yz)
   use m_ggeo_sferic
   use network_ggeo_data,   only: dcenterinside
   implicit none
   integer, intent(in)              :: N            !< Nr. of vertices
   double precision, intent(inout)  :: xv(N), yv(N) !< Coordinates of vertices (may be changed to avoid alloc overhead)
   double precision, intent(out)    :: xz, yz       !< Circumcenter coordinates

   double precision, dimension(N)   :: xx, yy, zz

   double precision, dimension(N)   :: ttx, tty, ttz      ! tangential vector in 3D coordinates
   double precision, dimension(N)   :: xxe, yye, zze      ! edge midpoint in 3D coordinates
   double precision, dimension(N)   :: ds                 ! edge lengths

   double precision, dimension(4,4) :: A           ! matrix
   double precision, dimension(4)   :: rhs         ! right-hand side
   double precision, dimension(4)   :: update      ! update of (xxc,yyc,zzc,lambda)

   double precision                 :: xxc, yyc, zzc      ! circumcenter in 3D coordinates
   double precision                 :: lambda             ! Lagrange multiplier to enforce circumcenter on the sphere

   double precision                 :: dsi, dinpr
   double precision                 :: xmin, xmax

   integer                          :: i, ip1

   integer                          :: iter

   double precision, parameter      :: dtol=1d-8    ! tolerance for ignoring edges
   double precision, parameter      :: deps=1d-8    ! convergence tolerance (relative)

   integer,          parameter      :: MAXITER=100

   !  compute 3D coordinates and first iterate of circumcenter in 3D coordinates and Lagrange multiplier lambda
   xxc = 0d0
   yyc = 0d0
   zzc = 0d0
   do i=1,N
      call sphertocart3D(xv(i), yv(i), xx(i), yy(i), zz(i))
      xxc = xxc + xx(i)
      yyc = yyc + yy(i)
      zzc = zzc + zz(i)
   end do
   lambda = 0d0
   xxc = xxc/N
   yyc = yyc/N
   zzc = zzc/N

   !  compute tangential vectors and edge midpoints, edge i is from nodes i to i+1, and convergence tolerance
   do i=1,N
      ip1 = i+1; if ( ip1.gt.N) ip1=ip1-N

      !     tangential vector
      ttx(i) = xx(ip1)-xx(i)
      tty(i) = yy(ip1)-yy(i)
      ttz(i) = zz(ip1)-zz(i)

      ds(i) = sqrt(ttx(i)**2 + tty(i)**2 + ttz(i)**2)

      if ( ds(i).lt.dtol ) cycle

      dsi = 1d0/ds(i)

      ttx(i) = ttx(i) * dsi
      tty(i) = tty(i) * dsi
      ttz(i) = ttz(i) * dsi

      !     edge midpoint
      xxe(i) = 0.5d0*(xx(i)+xx(ip1))
      yye(i) = 0.5d0*(yy(i)+yy(ip1))
      zze(i) = 0.5d0*(zz(i)+zz(ip1))
   end do


   !  Newton iterations
   do iter=1,MAXITER

      !     build system
      A   = 0d0
      rhs = 0d0
      do i=1,N
         if ( ds(i).lt.dtol ) cycle ! no contribution

         !        add to upper triangular part and right-hand side
         A(1,1) = A(1,1) + ttx(i)*ttx(i)
         A(1,2) = A(1,2) + ttx(i)*tty(i)
         A(1,3) = A(1,3) + ttx(i)*ttz(i)

         A(2,2) = A(2,2) + tty(i)*tty(i)
         A(2,3) = A(2,3) + tty(i)*ttz(i)

         A(3,3) = A(3,3) + ttz(i)*ttz(i)

         dinpr = (xxc-xxe(i))*ttx(i) + (yyc-yye(i))*tty(i) + (zzc-zze(i))*ttz(i)

         rhs(1) = rhs(1) - dinpr*ttx(i)
         rhs(2) = rhs(2) - dinpr*tty(i)
         rhs(3) = rhs(3) - dinpr*ttz(i)
      end do

      if ( jsferic.eq.1 ) then
         !        add contribution of constraint
         A(1,1) = A(1,1) - 2d0*lambda
         A(2,2) = A(2,2) - 2d0*lambda
         A(3,3) = A(3,3) - 2d0*lambda

         A(1,4) = -2d0*xxc
         A(2,4) = -2d0*yyc
         A(3,4) = -2d0*zzc

         A(4,4) = 0d0

         rhs(1) = rhs(1) + 2d0 * lambda * xxc
         rhs(2) = rhs(2) + 2d0 * lambda * yyc
         rhs(3) = rhs(3) + 2d0 * lambda * zzc
         rhs(4) = xxc**2 + yyc**2 + zzc**2 - Ra**2
      else  ! no constraints, enforce lambda=0
         A(1,4) = 0d0
         A(2,4) = 0d0
         A(3,4) = 0d0
         A(4,4) = 1d0

         rhs(4) = 0d0
      end if

      !     use symmetry of matrix
      A(2,1) = A(1,2)

      A(3,1) = A(1,3)
      A(3,2) = A(2,3)

      A(4,1) = A(1,4)
      A(4,2) = A(2,4)
      A(4,3) = A(3,4)

      !     solve system
      call gaussj(A,4,4,rhs,1,1) ! rhs contains solution

      !     update circumcenter and Lagrange multiplier
      xxc = xxc + rhs(1)
      yyc = yyc + rhs(2)
      zzc = zzc + rhs(3)
      lambda = lambda + rhs(4)


      !     check convergence
      if ( rhs(1)**2 + rhs(2)**2 + rhs(3)**2 .lt. deps ) then
         exit
      end if
   end do

   !  check convergence
   if ( iter.ge.MAXITER ) then
      !LC call qnerror('comp_circumcenter3D: no convergence', ' ', ' ')
   end if

   !  project circumcenter back to spherical coordinates
   call Cart3Dtospher(xxc,yyc,zzc,xz,yz,maxval(xv(1:N)))

   return
   end subroutine comp_circumcenter3D

   !-----------------------------------------------------------------!
   ! rest.f90
   !-----------------------------------------------------------------!
   !> Checks whether lines 1-2 and 3-4 intersect.
   !! @param[in] x1,y1,x2,y2,x3,y3,x4,y4 x- and y-coords of line endpoints.
   !! @param[out] jacros 1 if lines cross (intersect), 0 if not.
   !! @param[out] sl lambda in [0,1] on line segment 1-2 (outside [0,1] if no intersection). Unchanged if no intersect!!
   !! @param[out] sm lambda in [0,1] on line segment 3-4 (outside [0,1] if no intersection). Unchanged if no intersect!!
   !! @param[out] xcr,ycr x-coord. of intersection point.
   SUBROUTINE CROSS(x1, y1, x2, y2, x3, y3, x4, y4, JACROS,SL,SM,XCR,YCR,CRP)
   use m_ggeo_missing
   implicit none
   double precision, intent(inout) :: crp !< crp (in)==-1234 will make crp (out) non-dimensional
   double precision :: det
   double precision :: eps
   integer :: jacros, jamakenondimensional
   double precision :: sl
   double precision :: sm
   double precision, intent(in) :: x1, y1, x2, y2, x3, y3, x4, y4
   double precision :: x21, y21, x31, y31, x43, y43, xcr, ycr
   !LC double precision, external :: getdx, getdy



   !     safety check on crp (in)
   if ( isnan(crp) ) then
      crp = 0d0
   end if

   ! Set defaults for no crossing at all:
   jamakenondimensional = 0
   if ( abs(crp+1234d0).lt.0.5d0 ) then
      jamakenondimensional = 1
      crp = 0d0
   endif

   JACROS = 0
   EPS    = 0.00001d0
   SL     = DMISS
   SM     = DMISS
   !     SL     = LABDA TUSSEN 0 EN 1 OP EERSTE PAAR
   !     Sm     = LABDA TUSSEN 0 EN 1 OP TWEEDE PAAR

   call getdxdy(x1,y1,x2,y2,x21,y21)
   call getdxdy(x3,y3,x4,y4,x43,y43)
   call getdxdy(x1,y1,x3,y3,x31,y31)

   !X21 =  getdx(x1,y1,x2,y2)
   !Y21 =  getdy(x1,y1,x2,y2)
   !X43 =  getdx(x3,y3,x4,y4)
   !Y43 =  getdy(x3,y3,x4,y4)
   !X31 =  getdx(x1,y1,x3,y3)
   !Y31 =  getdy(x1,y1,x3,y3)

   DET =  X43*Y21 - Y43*X21

   !     SPvdP: make eps have proper dimension
   EPS = max(EPS*MAXVAL((/ X21,Y21,X43,Y43,X31,Y31 /)), tiny(0d0))
   IF (ABS(DET) .LT. EPS) THEN
      RETURN
   ELSE
      SM = (Y31*X21 - X31*Y21) / DET
      IF (ABS(X21) .GT. EPS) THEN
         SL = (SM*X43 + X31) / X21
      ELSE IF (ABS(Y21) .GT. EPS) THEN
         SL = (SM*Y43 + Y31) / Y21
      ELSE
         SL   = 0d0
      ENDIF
      IF (SM .GE. 0d0 .AND. SM .LE. 1d0 .AND. &
         SL .GE. 0d0 .AND. SL .LE. 1d0) THEN
      JACROS = 1
      ENDIF
      XCR    = X1 + SL*(X2-X1)
      YCR    = Y1 + SL*(Y2-Y1)
      if ( jamakenondimensional.eq.1 ) then  ! make crp non-dimensional (for spline2curvi)
         CRP    = -DET / ( sqrt(x21**2+y21**2) * sqrt(x43**2 + y43**2) + 1d-8 )
      else
         CRP    = -DET
      end if
   ENDIF
   RETURN
   END SUBROUTINE CROSS

   SUBROUTINE CROSSinbox (x1, y1, x2, y2, x3, y3, x4, y4, JACROS,SL,SM,XCR,YCR,CRP)  ! only if overlap
   use m_ggeo_missing
   implicit none
   double precision, intent(inout) :: crp ! crp (in)==-1234 will make crp (out) non-dimensional
   integer                         :: jacros
   double precision, intent(in)    :: x1, y1, x2, y2, x3, y3, x4, y4
   double precision, intent(out)   :: SL,SM,XCR,YCR
   double precision                :: x1min, x1max, y1min, y1max, x3min, x3max, y3min, y3max

   ! Set defaults for no crossing at all:
   JACROS = 0

   x1min = min(x1,x2); x1max = max(x1,x2)
   y1min = min(y1,y2); y1max = max(y1,y2)
   x3min = min(x3,x4); x3max = max(x3,x4)
   y3min = min(y3,y4); y3max = max(y3,y4)

   if (x1max < x3min) return
   if (x1min > x3max) return
   if (y1max < y3min) return
   if (y1min > y3max) return

   call CROSS (x1, y1, x2, y2, x3, y3, x4, y4, JACROS,SL,SM,XCR,YCR,CRP)

   RETURN
   END SUBROUTINE CROSSinbox
   
   double precision function getdx(x1,y1,x2,y2)
   use m_ggeo_sferic
   implicit none
   double precision :: x1, y1, x2, y2
   double precision :: xx1, yy1, xx2, yy2
   double precision :: diff1, diff2, dy, r, dx2
   double precision, external :: getdy

   if (jsferic == 1) then

      ! fix for poles
      diff1 = abs(abs(y1)-90d0)
      diff2 = abs(abs(y2)-90d0)
      if ( (diff1.le.dtol_pole .and. diff2.gt.dtol_pole) .or. &
         (diff1.gt.dtol_pole .and. diff2.le.dtol_pole) ) then
      getdx = 0d0
      return
      end if

      xx1   = x1
      xx2   = x2
      if      (xx1 - xx2 >  180d0) then
         xx1 = xx1 - 360d0
      else if (xx1 - xx2 < -180d0) then
         xx1 = xx1 + 360d0
      endif
      xx1   = xx1*dg2rd
      xx2   = xx2*dg2rd
      yy1   = y1 *dg2rd
      yy2   = y2 *dg2rd
      csphi = dcos(0.5d0*(yy1+yy2))
      getdx = ra*csphi*(xx2-xx1)
   else
      getdx = x2-x1
   endif
   end function getdx

   double precision function getdy(x1,y1,x2,y2)
   use m_ggeo_sferic
   implicit none
   double precision :: x1, y1, x2, y2
   double precision :: xx1, yy1,yy2

   if (jsferic == 1) then
      yy1   = y1*dg2rd
      yy2   = y2*dg2rd
      getdy = ra*(yy2-yy1)
   else
      getdy = y2-y1
   endif
   end function getdy

   subroutine getdxdy(x1,y1,x2,y2,dx,dy)
   use m_ggeo_sferic
   implicit none
   double precision :: x1, y1, x2, y2, dx, dy, dx2, dy2, dum
   !LC double precision, external :: getdx, getdy
   !double precision :: getdx, getdy
   if (Jsferic == 1) then
      dx = getdx(x1,y1,x2,y2)
      dy = getdy(x1,y1,x2,y2)
   else
      dx = x2-x1
      dy = y2-y1
   endif

   end subroutine getdxdy

   !> Normalized inner product of two segments
   !! NOTE that parallel lines may produce abs(dcosphi)=1+O(10^-16) > 1
   !! in Debug builds, crashes subsequent acos calls! (not in Release)
   double precision function dcosphi(x1,y1,x2,y2,x3,y3,x4,y4)
   use m_ggeo_missing
   use m_ggeo_sferic
   implicit none
   double precision :: x1,y1,x2,y2,x3,y3,x4,y4
   double precision :: dx1,dy1,dx2,dy2,r1,r2

   double precision, dimension(4) :: xx, yy, zz
   double precision                :: dz1, dz2

   !LC double precision, external :: getdx, getdy

   if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
      call sphertocart3D(x1, y1, xx(1), yy(1), zz(1))
      call sphertocart3D(x2, y2, xx(2), yy(2), zz(2))
      call sphertocart3D(x3, y3, xx(3), yy(3), zz(3))
      call sphertocart3D(x4, y4, xx(4), yy(4), zz(4))

      dx1 = xx(2)-xx(1)
      dy1 = yy(2)-yy(1)
      dz1 = zz(2)-zz(1)
      r1  = dx1**2 + dy1**2 + dz1**2

      dx2 = xx(4)-xx(3)
      dy2 = yy(4)-yy(3)
      dz2 = zz(4)-zz(3)
      r2  = dx2**2 + dy2**2 + dz2**2

      if ( r1.eq.0d0 .or. r2.eq.0d0 ) then
         dcosphi = dxymis
      else
         dcosphi = (dx1*dx2 + dy1*dy2 + dz1*dz2)/sqrt(r1*r2)
      endif
   else
      !call getdxdy(x1,y1,x2,y2,dx1,dy1)
      !call getdxdy(x3,y3,x4,y4,dx2,dy2)

      dx1 = getdx(x1,y1,x2,y2)
      dx2 = getdx(x3,y3,x4,y4)

      dy1 = getdy(x1,y1,x2,y2)
      dy2 = getdy(x3,y3,x4,y4)

      r1  = dx1*dx1+dy1*dy1
      r2  = dx2*dx2+dy2*dy2

      if (r1 == 0d0 .or. r2 == 0d0) then
         dcosphi = dxymis
      else
         dcosphi = (dx1*dx2 + dy1*dy2)/sqrt(r1*r2)
      endif

   end if

   dcosphi = max( min(dcosphi,1d0) , -1d0)

   return
   end function dcosphi

   !> compute distance from (x1,y1) to (x2,y2)
   double precision function dbdistance(x1,y1,x2,y2)                  ! distance point 1 -> 2
   use m_ggeo_missing
   use m_ggeo_sferic
   implicit none
   double precision, intent(in) :: x1, y1, x2, y2
   ! locals
   double precision             :: ddx, ddy, rr
   double precision             :: xx1, yy1, zz1, xx2, yy2, zz2
   !LC double precision, external   :: getdx, getdy

   if ( x1.eq.DMISS .or. x2.eq.DMISS .or. y1.eq.DMISS .or. y2.eq.DMISS ) then
      dbdistance = 0d0
      return
   end if

   if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
      call sphertocart3D(x1,y1,xx1,yy1,zz1)
      call sphertocart3D(x2,y2,xx2,yy2,zz2)
      dbdistance = sqrt( (xx2-xx1)**2 + (yy2-yy1)**2 + (zz2-zz1)**2 )
   else
      ddx = getdx(x1,y1,x2,y2)
      ddy = getdy(x1,y1,x2,y2)
      rr  = ddx*ddx + ddy*ddy
      if (rr == 0d0) then
         dbdistance = 0d0
      else
         dbdistance = sqrt(rr)
      endif
   endif

   end function dbdistance
   

   !> Normalized vector in direction 1 -> 2, in the orientation of (xu,yu)
   subroutine normalin(x1,y1,x2,y2,xn,yn,xu,yu)
   use m_ggeo_sferic
   use m_ggeo_missing
   implicit none
   double precision :: x1, y1, x2, y2, xn, yn, xu, yu
   ! locals
   double precision :: ddx, ddy, rr
   !LC double precision :: getdx, getdy

   double precision, dimension(3) :: xx1
   double precision, dimension(3) :: xx2
   double precision, dimension(3) :: xxu
   double precision, dimension(3) :: elambda
   double precision, dimension(3) :: ephi

   double precision :: lambda, phi

   if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
      !    call qnerror('normalin: reference probably not set', ' ', ' ')

      !   compute 3D coordinates
      call sphertoCart3D(x1,y1,xx1(1),xx1(2),xx1(3))
      call sphertoCart3D(x2,y2,xx2(1),xx2(2),xx2(3))

      !   compute base vectors in reference point
      lambda = xu*dg2rd
      phi    = yu*dg2rd
      elambda = (/ -sin(lambda),                    cos(lambda), 0d0 /)
      ephi    = (/ -sin(phi)*cos(lambda), -sin(phi)*sin(lambda), cos(phi) /)

      !   project vector in local base
      ddx = sum((xx2-xx1)*elambda)
      ddy = sum((xx2-xx1)*ephi)
   else
      ddx = getdx(x1,y1,x2,y2)
      ddy = getdy(x1,y1,x2,y2)
   end if

   rr  = ddx*ddx + ddy*ddy
   if (rr == 0d0) then
      xn  = dxymis
      yn  = dxymis
   else
      rr  = sqrt(rr)
      xn  = ddx / rr
      yn  = ddy / rr
   endif
   end subroutine normalin

   !> Creates the relative unit normal vector to edge 1->2
   !!
   !! Vector is of unit length in Cartesian world.
   !! Vector is almost unit length in spherical world, but its
   !! x-component is scaled 1/cos(phi) such that in later uses:
   !! (theta_B, theta_A) = (theta_A, phi_A) + alpha*(theta_n, phi_n)
   !! the vectors 1->2 and A->B are perpendicular in Cartesian world,
   !! not in spherical world. NOTE: the LENGTH of A->B in Cartesian
   !! world implictly contains the earth radius and dg2rd already,
   !! so make sure your alpha is in degrees.
   subroutine normalout(x1,y1,x2,y2,xn,yn)             ! normals out edge 1  2
   use m_ggeo_missing
   use m_ggeo_sferic
   implicit none
   double precision :: x1, y1, x2, y2, xn, yn
   ! locals
   double precision :: ddx, ddy, rr
   !LC double precision :: getdx, getdy

   double precision, dimension(3) :: xx1
   double precision, dimension(3) :: xx2
   double precision, dimension(3) :: xxu
   double precision, dimension(3) :: elambda
   double precision, dimension(3) :: ephi

   double precision :: xu, yu
   double precision :: lambda, phi

   if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
      !   get local coordinates w.r.t. (xn,yn)
      call half(x1,y1,x2,y2,xn,yn)

      !   compute 3D coordinates
      call sphertoCart3D(x1,y1,xx1(1),xx1(2),xx1(3))
      call sphertoCart3D(x2,y2,xx2(1),xx2(2),xx2(3))

      !   compute midpoint
      xxu = 0.5d0*(xx1+xx2)
      call Cart3Dtospher(xxu(1),xxu(2),xxu(3),xu,yu,max(x1,x2))

      !   compute base vectors at midpoint
      lambda = xu*dg2rd
      phi    = yu*dg2rd
      elambda = (/ -sin(lambda),                    cos(lambda), 0d0 /)
      ephi    = (/ -sin(phi)*cos(lambda), -sin(phi)*sin(lambda), cos(phi) /)

      !   project vector in local base
      ddx = sum((xx2-xx1)*elambda)
      ddy = sum((xx2-xx1)*ephi)
   else
      ddx = getdx(x1,y1,x2,y2)
      ddy = getdy(x1,y1,x2,y2)
   end if

   rr  = ddx*ddx + ddy*ddy
   if (rr == 0d0) then
      xn  = dxymis
      yn  = dxymis
   else
      rr  =  sqrt(rr)
      xn  =  ddy / rr
      yn  = -ddx / rr
   endif
   if (jsferic == 1 .and. jasfer3D.eq.0) then
      xn = xn / cos(dg2rd*0.5d0*(y1+y2) )
      yn = yn
   endif

   end subroutine normalout

   SUBROUTINE DUITPL(X1,Y1,X2,Y2,X3,Y3,X4,Y4,RU)
   implicit none
   double precision :: X1,Y1,X2,Y2,X3,Y3,X4,Y4,RU
   double precision :: X12, y12, x34, y34
   !LC double precision :: GETDX, GETDY
   X12 = GETDX(X1,Y1,X2,Y2)
   Y12 = GETDY(X1,Y1,X2,Y2)
   X34 = GETDX(X3,Y3,X4,Y4)
   Y34 = GETDY(X3,Y3,X4,Y4)
   RU  = X12*Y34 - Y12*X34
   RU  = SIGN(1d0,RU)
   RETURN
   END SUBROUTINE DUITPL

   SUBROUTINE GAUSSJ(A,N,NP,B,M,MP)

   implicit none

   integer          :: n,np,m,mp
   double precision :: a,b

   integer          :: ipiv, indxr, indxc, i, j, k, L, LL, irow, icol
   double precision :: big, dum, pivinv

   !      PARAMETER (NMAX=50)
   !      DIMENSION A(NP,NP),B(NP,MP),IPIV(NMAX),INDXR(NMAX),INDXC(NMAX)

   DIMENSION A(NP,NP),B(NP,MP),IPIV(NP),INDXR(NP),INDXC(NP) ! SPvdP: set NMAX to N
   DO 11 J=1,N
      IPIV(J)=0
11 CONTINUE
   DO 22 I=1,N
      BIG=0.
      DO 13 J=1,N
         IF(IPIV(J).NE.1)THEN
            DO 12 K=1,N
               IF (IPIV(K).EQ.0) THEN
                  IF (ABS(A(J,K)).GE.BIG)THEN
                     BIG=ABS(A(J,K))
                     IROW=J
                     ICOL=K
                  ENDIF
               ELSE IF (IPIV(K).GT.1) THEN
                  WRITE(*,*) 'Singular matrix'
               ENDIF
12          CONTINUE
         ENDIF
13    CONTINUE
      IPIV(ICOL)=IPIV(ICOL)+1
      IF (IROW.NE.ICOL) THEN
         DO 14 L=1,N
            DUM=A(IROW,L)
            A(IROW,L)=A(ICOL,L)
            A(ICOL,L)=DUM
14       CONTINUE
         DO 15 L=1,M
            DUM=B(IROW,L)
            B(IROW,L)=B(ICOL,L)
            B(ICOL,L)=DUM
15       CONTINUE
      ENDIF
      INDXR(I)=IROW
      INDXC(I)=ICOL
      IF (A(ICOL,ICOL).EQ.0.) WRITE(*,*) 'Singular matrix'
      PIVINV=1./A(ICOL,ICOL)
      A(ICOL,ICOL)=1.
      DO 16 L=1,N
         A(ICOL,L)=A(ICOL,L)*PIVINV
16    CONTINUE
      DO 17 L=1,M
         B(ICOL,L)=B(ICOL,L)*PIVINV
17    CONTINUE
      DO 21 LL=1,N
         IF(LL.NE.ICOL)THEN
            DUM=A(LL,ICOL)
            A(LL,ICOL)=0.
            DO 18 L=1,N
               A(LL,L)=A(LL,L)-A(ICOL,L)*DUM
18          CONTINUE
            DO 19 L=1,M
               B(LL,L)=B(LL,L)-B(ICOL,L)*DUM
19          CONTINUE
         ENDIF
21    CONTINUE
22 CONTINUE
   DO 24 L=N,1,-1
      IF(INDXR(L).NE.INDXC(L))THEN
         DO 23 K=1,N
            DUM=A(K,INDXR(L))
            A(K,INDXR(L))=A(K,INDXC(L))
            A(K,INDXC(L))=DUM
23       CONTINUE
      ENDIF
24 CONTINUE
   RETURN
   END subroutine GAUSSJ

   ! compute coordinates (xu, yu) halfway between (x1,y1) and (x2,y2)
   subroutine half(x1,y1,x2,y2,xu,yu)
   use m_ggeo_sferic
   implicit none

   double precision, intent(in)  :: x1, y1
   double precision, intent(in)  :: x2, y2
   double precision, intent(out) :: xu, yu

   double precision              :: xx1, yy1, zz1, xx2, yy2, zz2

   if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
      call sphertoCart3D(x1,y1,xx1,yy1,zz1)
      call sphertoCart3D(x2,y2,xx2,yy2,zz2)
      call Cart3Dtospher(0.5d0*(xx1+xx2),0.5d0*(yy1+yy2),0.5d0*(zz1+zz2),xu,yu,max(x1,x2))
   else
      xu = 0.5d0*(x1+x2)
      yu = 0.5d0*(y1+y2)
   end if

   return
   end subroutine half

   LOGICAL FUNCTION INVIEW(X,Y)
   USE m_ggeo_missing
   use m_ggeo_WEARELT
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
   !LC LOGICAL INVIEW
   DOUBLE PRECISION XD,YD,ZD
   CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
   DINVIEW = INVIEW(X,Y)
   RETURN
   END FUNCTION DINVIEW

   !-----------------------------------------------------------------!
   ! unstruct.F90
   !-----------------------------------------------------------------!

   !> circumcenter of a polygon defined by set of vertices.
   !! See also getcellcircumcenter
   subroutine GETCIRCUMCENTER( nn, xv, yv, lnnl, xz, yz)
   use m_ggeo_sferic
   use network_ggeo_data,   only: dcenterinside
   use m_ggeo_tpoly, only: pinpok
   implicit none
   integer, intent(in)             :: nn             !< Nr. of vertices
   double precision, intent(inout) :: xv(nn), yv(nn) !< Coordinates of vertices (may be changed to avoid alloc overhead)
   integer,          intent(in)    :: lnnl(nn)       !< Local lnn codes for all netlinks between vertices.
   double precision, intent(out)   :: xz, yz         !< Circumcenter coordinates

   ! locals
   double precision :: xzw, yzw                        ! zwaartepunt
   double precision :: xn, yn                          ! normal out
   double precision :: dis
   integer          :: m,k,k1,k2
   double precision :: xz2, yz2                        ! only for help 4 corners
   !LC double precision :: xe3,ye3,xe1,ye1,xe2,ye2,tex,tey,dp,dotp, &
   !                    xccf,yccf,xccc,yccc,xcccf,ycccf,xccfo,yccfo,alf
   double precision :: xe3,ye3,xe1,ye1,xe2,ye2,tex,tey,dp, &
      xccf,yccf,xccc,yccc,xcccf,ycccf,xccfo,yccfo,alf

   integer, parameter :: MMAX=10

   double precision :: xh(MMAX), yh(MMAX)
   double precision :: xr(MMAX), yr(MMAX), SL,SM,XCR,YCR,CRP
   double precision :: eps = 1d-3, xcc3, ycc3, xf, xmx, xmn
   !LC double precision :: getdx, getdy
   double precision :: dfac
   integer          :: jacros, in, m2, nintlinks ! nr of internal links = connected edges
   logical          :: isnan

   !LC double precision, external :: dbdistance

   ! integer,          parameter     :: N6=6
   ! double precision, dimension(N6) :: xhalf, yhalf

   double precision, parameter      :: dtol=1d-4

   xzw = 0d0 ; yzw = 0d0

   if (jsferic == 1) then ! jglobe                 ! regularise sferic coordinates
      xmx = maxval(xv(1:nn))
      xmn = minval(xv(1:nn))
      if (xmx - xmn > 180d0) then
         do m  = 1,nn
            if ( xmx - xv(m) > 180d0) then
               xv(m) = xv(m) + 360d0
            endif
         enddo
      endif
   endif

   do m  = 1,nn
      xzw   = xzw + xv(m)
      yzw   = yzw + yv(m)
   enddo

   xzw = xzw / nn
   yzw = yzw / nn

   !--------------------------
   ! test
   ! if ( nn.gt.N6 ) then
   !    call qnerror('getcircumcenter: nn>N6', ' ', ' ')
   !    stop
   ! end if
   ! xhalf(1:nn) = 0.5d0*(xv(1:nn)+(/ xv(2:nn), xv(1) /))
   ! yhalf(1:nn) = 0.5d0*(yv(1:nn)+(/ yv(2:nn), yv(1) /))
   ! call comp_circumcenter(nn, xv, yv, xhalf, yhalf, xz, yz)
   ! goto 1234
   ! end test
   !--------------------------
   ! if (nn == 333) then
   if (nn == 3 .and. jglobe == 0 ) then ! for triangles
      call circumcenter3(nn, xv, yv, xz, yz )
   else
      ! default case
      if (jsferic == 1) then
         eps = 9d-10 ! 111km = 0-e digit.
      endif

      xccf = xzw
      yccf = yzw
      alf  = 0.1d0

      if (jsferic == 1) then
         xf  = 1d0/dcos( dg2rd*yzw )
      endif

      nintlinks = 0
      do m  = 1,nn
         if ( lnnl(m) == 2) then
            nintlinks = nintlinks + 1
         endif
      enddo

      if (nintlinks > 1 .or. nn.eq.3) then                ! nn.eq.3: always for triangles
         do k = 1,100                                     ! Zhang, Schmidt and Perot 2002, formula A3
            xccfo = xccf
            yccfo = yccf
            do m  = 1,nn
               if ( lnnl( m ) == 2 .or. nn.eq.3) then     ! nn.eq.3: always for triangles
                  xe1= xv(m)
                  ye1= yv(m)
                  m2 = m + 1; if (m == nn) m2 = 1
                  xe2= xv(m2)
                  ye2= yv(m2)
                  ! If two subsequent corners are on top of each other, see them as one.
                  if (xe1 == xe2 .and. ye1 == ye2) then
                     cycle
                  end if
                  xe3= 0.5d0*(xe1+xe2)
                  ye3= 0.5d0*(ye1+ye2)
                  call normalin(xe1,ye1,xe2,ye2,tex,tey,xe3,ye3)
                  xcc3 =  getdx(xe3,ye3,xccf,yccf)
                  ycc3 =  getdy(xe3,ye3,xccf,yccf)
                  dp   = -alf*dotp(xcc3,ycc3,tex,tey)  ! - sign not present in given formula
                  if (jsferic == 1) then
                     dp   = rd2dg*dp/ra
                     xccf = xccf + tex*dp*xf           ! even erbijblijven voor beste resultaat
                     yccf = yccf + tey*dp
                  else
                     xccf = xccf + tex*dp
                     yccf = yccf + tey*dp
                  endif
                  ! dp   = -alf*dotp(xccf - xe3,yccf - ye3, tex, tey)  ! - sign not present in given formula
                  ! call cirr(xccf,yccf,31)
                  ! call waitesc()
               endif
            enddo
            if (k > 1 .and. abs(xccf-xccfo) < eps .and. abs(yccf-yccfo) < eps) then
               m = 1
               exit
            endif
         enddo

         xz = xccf
         yz = yccf

      else

         xz = xzw
         yz = yzw

      endif
   endif

1234 continue

   ! if (jsferic == 1) then ! jglobe   ! regularisatie tbv tidal force routine
   !    if ( xz < -180d0 ) then
   !         xz = xz + 360d0
   !    endif
   ! ENDIF

   if ( dcenterinside .le. 1d0 .and. dcenterinside.ge.0d0 ) then
      if ( nn.le.3 ) then ! triangles
         dfac = 1d0
      else
         dfac = dcenterinside
      end if

      do m=1,nn
         xh(m) = dfac*xv(m)+(1-dfac)*xzw
         yh(m) = dfac*yv(m)+(1-dfac)*yzw
      end do

      call pinpok(xz,yz,nn,xh,yh,in)                    ! circumcentre may not lie outside cell
      if (in == 0) then
         do m  = 1,nn
            m2 = m + 1; if (m == nn) m2 = 1
            call CROSS(xzw, yzw, xz, yz, xh(m ), yh(m ), xh(m2), yh(m2),&
               JACROS,SL,SM,XCR,YCR,CRP)

            if (jacros == 1) then
               !               xz = 0.5d0*( xh(m) + xh(m2) ) ! xcr
               !               yz = 0.5d0*( yh(m) + yh(m2) ) ! ycr
               xz = xcr
               yz = ycr

               exit
            endif
         enddo
      endif
   endif


   end subroutine GETCIRCUMCENTER

   !> computes dot product of two two-dimensional vectors defined by (x1,y1) and (x2,y2) respectively
   double precision function dotp(x1,y1,x2,y2)         ! dot produkt
   implicit none
   double precision :: x1, y1, x2, y2
   dotp = x1*x2 + y1*y2
   end function dotp

   !> compute circumcenter of a triangle
   subroutine circumcenter3(nn, x, y, xz, yz )             ! of triangle n                      ! todo : sferic
   use m_ggeo_sferic
   implicit none
   integer          :: nn
   double precision :: x(nn), y(nn), xz, yz, xf, phi
   !LC double precision :: getdx, getdy


   ! locals

   double precision :: z,den,dx2,dx3,dy2,dy3


   dx2 = x(2)-x(1)
   dx3 = x(3)-x(1)

   dy2 = y(2)-y(1)
   dy3 = y(3)-y(1)

   dx2 = getdx( x(1),y(1),x(2),y(2) )
   dy2 = getdy( x(1),y(1),x(2),y(2) )

   dx3 = getdx( x(1),y(1),x(3),y(3) )
   dy3 = getdy( x(1),y(1),x(3),y(3) )

   den = dy2*dx3-dy3*dx2
   if (den .ne. 0) then
      z=(dx2*(dx2-dx3)+dy2*(dy2-dy3))/den
   else
      ! call qnerror('coinciding points',' ',' ')
      z = 0d0
   endif
   if (jsferic == 1) then
      phi = (y(1)+y(2)+y(3))/3d0
      xf  = 1d0/dcos( dg2rd*phi )
      xz  = x(1) + xf*0.5d0*(dx3-z*dy3)*rd2dg/ra
      yz  = y(1) +    0.5d0*(dy3+z*dx3)*rd2dg/ra
   else
      xz = x(1) + 0.5d0*(dx3-z*dy3)
      yz = y(1) + 0.5d0*(dy3+z*dx3)
   endif

   end subroutine circumcenter3

   !> Computes the bottom area of a cell and the center of mass coordinates.
   subroutine getcellsurface ( n, ba, xzwr, yzwr )                 ! bottom area of cell nr n                       ! todo : sferic
   !lc use m_netw
   use network_ggeo_data
   use m_ggeo_sferic
   use m_ggeo_missing
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
   call comp_masscenter(nn, xh , yh, xzwr, yzwr, ba, jaccw)
   end subroutine getcellsurface

   !> computes the cell-weighted center
   subroutine getcellweightedcenter(n, xz, yz, zz)
   use m_ggeo_orthosettings
   use network_ggeo_data, only: netcell, xk, yk, zk
   use m_ggeo_sferic, only: jsferic, jasfer3D
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

      call comp_circumcenter3D(nn, xv, yv, xz, yz)
   else
      !   get the cell polygon that is safe for periodic, spherical coordinates, inluding poles
      call get_cellpolygon(n,Mmax,nn,xv,yv,LnnL,Lorg,zz)
      call getcircumcenter(nn, xv, yv, lnnl, xz, yz)
   end if

   if (circumormasscenter .ne. 1d0) then
      !   update with cell mass center
      call comp_masscenter(nn, xv, yv, xzw, yzw, ba, jaccw)

      xz = circumormasscenter*xz + (1d0-circumormasscenter)*xzw
      yz = circumormasscenter*yz + (1d0-circumormasscenter)*yzw
   endif
   ! CALL CIRR(XZ,YZ,31)
   end subroutine getcellweightedcenter
   
   !------------------------------------------------------------------
   ! From polygon.F90
   !------------------------------------------------------------------
   
   
   

   !-----------------------------------------------------------------!
   ! Library public functions
   !-----------------------------------------------------------------!

   ! make1D2Dinternalnetlinks
   function make1D2Dinternalnetlinks() result(ierr)

   use m_ggeo_flowgeom, only: xz, yz
   use network_ggeo_data
   implicit none

   integer          :: K1, K2, K3, L, NC1, NC2, JA, KK2(2), KK, NML
   integer          :: i, ierr, k, kcell
   DOUBLE PRECISION :: XN, YN, XK2, YK2, WWU

   ierr = 0

   call SAVENET()
   call findcells(0)

   KC = 2
   DO L = 1,NUML  ! FLAG TO 1 ANY NODE TOUCHED BY SOMETHING 1D
      K1  = KN(1,L) ; K2  = KN(2,L); K3 = KN(3,L)
      IF (K3 .NE. 4 .AND. K3 .NE. 2 .AND. K3 .NE. 0) THEN ! only for yet-isolated 1D channels with KN(3,L)==1
         KC(K1) = 1 ; KC(K2) = 1
      ENDIF
   ENDDO

   NML  = NUML
   DO K = 1,NUMK

      IF (NMK(K) == 2) THEN

         IF (KC(K) == 1) THEN
            NC1 = 0
            CALL INCELLS(XK(K), YK(K), NC1)
            IF (NC1 > 1) THEN
               CALL SETNEWPOINT(XZ(NC1),YZ(NC1),ZK(K) ,NC2)
               call connectdbn(NC2, K, L)
               KN(3,L) = 3
            ELSE
               DO KK = 1,2
                  L  = NOD(K)%LIN(KK)
                  KK2(KK) = KN(1,L) + KN(2,L) - K
               ENDDO
               K1 = KK2(1) ; K2 = KK2(2)
               CALL normalout(XK(K1), YK(K1), XK(K2), YK(K2) , XN, YN )

               WWU = 5D0*DBDISTANCE(XK(K1), YK(K1), XK(K2), YK(K2) )

               XK2 = XK(K) + XN*WWU
               YK2 = YK(K) + YN*WWU
               CALL CROSSED2d_BNDCELL(NML, XK(K), YK(K), XK2, YK2, NC1)

               IF (NC1 > 1) THEN
                  CALL SETNEWPOINT(XZ(NC1),YZ(NC1),ZK(K) ,NC2)
                  call connectdbn(NC2, K, L)
                  KN(3,L) = 3
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
   use network_ggeo_data
   use m_ggeo_missing

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
   
   !< get the number of created links
   function ggeo_get_links_count(nlinks) result(ierr)
   
   use network_ggeo_data
      
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
   
   use network_ggeo_data
      
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
   function ggeo_convert_1d_arrays(nodex, nodey, nBranches, branchid, meshgeom) result(ierr)
   
   use meshdata
   use m_alloc
   
   integer, intent(in)                  :: branchid(:), nBranches
   double precision, intent(in)         :: nodex(:), nodey(:)
   type(t_ug_meshgeom), intent(inout)   :: meshgeom
   integer                              :: ierr, i, k, idxstart, idxbr, cbranchid, idxend
   
   ierr = 0
   
   !The last computational node of a branch overlaps with the starting one
   meshgeom%dim = 1
   meshgeom%numnode =  size(nodex,1)
   meshgeom%numedge = meshgeom%numnode - nBranches

   allocate(meshgeom%nodex(meshgeom%numnode))
   allocate(meshgeom%nodey(meshgeom%numnode))
   allocate(meshgeom%edge_nodes(2, meshgeom%numedge))
   allocate(meshgeom%branchids(size(branchid,1)))

   !Assign the node coordinates
   meshgeom%nodex = nodex
   meshgeom%nodey = nodey
   meshgeom%branchids = branchid

   !Calculate the edge_node assuming discretization points are consecutive within the same branch.
   cbranchid       = meshgeom%branchids(1)
   idxstart       = 1
   idxbr          = 1
   idxend         = 1
   k              = 0
   do while (idxbr<=nBranches)
       do i = idxstart + 1, meshgeom%numnode
           if (meshgeom%branchids(i).ne.cbranchid) then
               cbranchid = meshgeom%branchids(i)
               idxend = i - 1;
               exit
           endif
           if (i == meshgeom%numnode) then
               idxend = i;
           endif
       enddo
       do i = idxstart, idxend - 1
           k = k +1
           meshgeom%edge_nodes(1,k) = i
           meshgeom%edge_nodes(2,k) = i + 1
       enddo
       idxstart    = idxend + 1
       idxbr       = idxbr + 1
   enddo
   
   end function ggeo_convert_1d_arrays
   
   !< Calculate the edge_node assuming discretization points are consecutive within the same branch.
   function ggeo_create_edge_nodes(nBranches, nNodes, branchids, edgenodes) result(ierr) !edge_nodes
      
   integer, intent(in)      :: nBranches, nNodes, branchids(:)
   integer, intent(inout)   :: edgenodes(:,:)
   integer                  :: ierr, cbranchid, idxstart, idxbr, idxend, i, k
   
   ierr = 0
  
   cbranchid      = branchids(1)
   idxstart       = 1
   idxbr          = 1
   idxend         = 1
   k              = 0
   do while (idxbr<=nBranches)
       do i = idxstart + 1, nNodes
           if (branchids(i).ne.cbranchid) then
               cbranchid = branchids(i)
               idxend = i - 1;
               exit
           endif
           if (i == nNodes) then
               idxend = i;
           endif
       enddo
       do i = idxstart, idxend - 1
           k = k +1
           edgenodes(1,k) = i
           edgenodes(2,k) = i + 1
       enddo
       idxstart    = idxend + 1
       idxbr       = idxbr + 1
   enddo
   
   end function ggeo_create_edge_nodes
   
   end module gridoperations