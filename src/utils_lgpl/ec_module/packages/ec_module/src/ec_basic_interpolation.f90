   !LC NOTES
   !MODK, csphi is not memorized anymore   
   !NTRANSFORMCOEF is substituted by 25 (hardcoded)
   
   !Global modules
   module m_ec_triangle           ! original name : m_triangle 
   implicit none
   double precision, ALLOCATABLE :: XCENT(:), YCENT(:)
   INTEGER, ALLOCATABLE          :: INDX(:,:)
   INTEGER, ALLOCATABLE          :: EDGEINDX(:,:)
   INTEGER, ALLOCATABLE          :: TRIEDGE(:,:)
   INTEGER                       :: NUMTRI
   INTEGER                       :: NUMTRIINPOLYGON
   INTEGER                       :: NUMEDGE
   INTEGER, PARAMETER            :: ITYPE = 2 ! 1 = ORIGINAL FORTRAN ROUTINE, 2 = NEW C ROUTINE

   integer                       :: jagetwf = 0    ! if 1, also assemble weightfactors and indices in:
   INTEGER, ALLOCATABLE          :: indxx(:,:)     ! to be dimensioned by yourselves 3,*
   double precision, ALLOCATABLE :: wfxx (:,:)

   double precision              :: TRIANGLEMINANGLE =  5d0 ! MINIMUM ANGLE IN CREATED TRIANGLES  IF MINANGLE > MAXANGLE: NO CHECK
   double precision              :: TRIANGLEMAXANGLE =  150 ! MAXIMUM ANGLE IN CREATED TRIANGLES
   double precision              :: TRIANGLESIZEFAC  =  1.0 ! TRIANGLE SIZEFACTOR, SIZE INSIDE VS AVERAGE SIZE ON POLYGON BORDER

   TYPE T_NODI
      INTEGER                    :: NUMTRIS       ! total number of TRIANGLES ATtached to this node
      INTEGER, allocatable       :: TRINRS(:)     ! numbers of ATTACHED TRIANGLES
   END TYPE T_NODI

   TYPE (T_NODI), DIMENSION(:), ALLOCATABLE :: NODE          !

   !integer, dimension(:,:), allocatable :: trinods ! triangle nodes, dim(3,numtri)
   integer, dimension(:,:), allocatable :: LNtri  ! triangles connected to edges, dim(2,numedges)

   integer                              :: IDENT   ! identifier
   integer, dimension(:),   allocatable :: imask   ! mask array for triangles

   END MODULE m_ec_triangle


   MODULE m_ec_interpolationsettings
   implicit none
   integer, parameter              :: INTP_INTP = 1
   integer, parameter              :: INTP_AVG  = 2
   INTEGER                         :: INTERPOLATIONTYPE            ! 1 = TRIANGULATION/BILINEAR INTERPOLATION 2= CELL AVERAGING
   INTEGER                         :: JTEKINTERPOLATIONPROCESS     ! TEKEN INTERPOLATION PROCESS YES/NO 1/0
   INTEGER                         :: IAV                          ! AVERAGING METHOD, 1 = SIMPLE AVERAGING, 2 = CLOSEST POINT, 3 = MAX, 4 = MIN, 5 = INVERSE WEIGHTED DISTANCE, 6 = MINABS, 7 = KDTREE
   INTEGER                         :: NUMMIN                       ! MINIMUM NR OF POINTS NEEDED INSIDE CELL TO HANDLE CELL
   DOUBLE PRECISION, parameter     :: RCEL_DEFAULT = 1.01d0        ! we need a default
   DOUBLE PRECISION                :: RCEL                         ! RELATIVE SEARCH CELL SIZE, DEFAULT 1D0 = ACTUAL CELL SIZE, 2D0=TWICE AS LARGE
   INTEGER                         :: Interpolate_to               ! 1=bathy, 2=zk, 3=s1, 4=Zc
   DOUBLE PRECISION                :: percentileminmax             ! if non zero, take average of highest or lowest percentile

   contains

   !> set default interpolation settings
   subroutine default_interpolationsettings()
   implicit none

   INTERPOLATIONTYPE        = INTP_INTP      ! 1 = TRIANGULATION/BILINEAR INTERPOLATION 2= CELL AVERAGING
   JTEKINTERPOLATIONPROCESS = 0              ! TEKEN INTERPOLATION PROCESS YES/NO 1/0
   IAV                      = 1              ! AVERAGING METHOD, 1 = SIMPLE AVERAGING, 2 = CLOSEST POINT, 3 = MAX, 4 = MIN, 5 = INVERSE WEIGHTED DISTANCE, 6 = MINABS, 7 = KDTREE
   NUMMIN                   = 1              ! MINIMUM NR OF POINTS NEEDED INSIDE CELL TO HANDLE CELL
   RCEL                     = RCEL_DEFAULT   ! RELATIVE SEARCH CELL SIZE, DEFAULT 1D0 = ACTUAL CELL SIZE, 2D0=TWICE AS LARGE
   Interpolate_to           = 2              ! 1=bathy, 2=zk, 3=s1, 4=Zc
   percentileminmax         = 0d0
   return
   end subroutine default_interpolationsettings


   END MODULE m_ec_interpolationsettings

   !---------------------------------------------------------------------------!
   !---------------------------------------------------------------------------!
   !   m_ec_basic_interpolation
   !---------------------------------------------------------------------------!
   !---------------------------------------------------------------------------!

   module m_ec_basic_interpolation
   
   use precision
   use MessageHandling, only: msgbox, mess, LEVEL_ERROR

   interface triinterp2
      module procedure triinterp2_dbldbl
      module procedure triinterp2_realdbl
      module procedure triinterp2_realreal
   end interface triinterp2

   private
   
   public   ::  bilin_interp
   public   ::  TRIINTfast
   public   ::  AVERAGING2
   public   ::  dlaun
   public   ::  comp_x_dxdxi
   public   ::  bilin_interp_loc
   public   ::  triinterp2

   contains

   !---------------------------------------------------------------------------!
   !   Triinterp
   !---------------------------------------------------------------------------!

   subroutine triinterp2_dbldbl(XZ, YZ, BL, NDX, JDLA,&
                        XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef, kcc)
   implicit none
   !
   ! Parameters
   real(hp), intent(in)            :: XZ(:)
   real(hp), intent(in)            :: YZ(:)
   real(hp), intent(inout)         :: BL(NDX)
   integer , intent(in)            :: NDX
   integer , intent(in)            :: JDLA
   integer , intent(in)            :: NS
   integer , intent(in)            :: jins
   integer , intent(in)            :: jasfer3D
   integer , intent(in)            :: NPL
   integer , intent(in)            :: MXSAM
   integer , intent(in)            :: MYSAM
   real(hp), intent(in)            :: XS(:)
   real(hp), intent(in)            :: YS(:)
   real(hp), intent(in)            :: ZS(:)
   real(hp), intent(in)            :: dmiss
   integer                         :: jakdtree
   integer                         :: jsferic, jakc
   
   real(hp), intent(in)            :: XPL(:)
   real(hp), intent(in)            :: YPL(:)
   real(hp), intent(in)            :: ZPL(:)
   real(hp), intent(in)            :: transformcoef(:)
   
   integer , intent(in), optional  :: kcc(:) 
   
   !
   ! Locals
   integer :: i
   integer :: in_unit
   integer :: out_unit
   !
   ! Body
   !   jsferic = 0
  
   !   ! assign 'missing value' to all elements of dRes
   !   BL = -999d0

   if (ndx < 1) return

   jakc = 0
   if (present(kcc)) then 
      jakc = 1
   endif   
   
   
   jakdtree = 1

   if ( MXSAM.gt.0 .and. MYSAM.gt. 0 ) then  ! bi-linear interpolation
      if (jakc == 0) then 
         call bilin_interp(NDX, XZ, YZ, BL, dmiss, XS, YS, ZS, MXSAM, MYSAM, XPL, YPL, ZPL, NPL, jsferic)
      else   
         call bilin_interp(NDX, XZ, YZ, BL, dmiss, XS, YS, ZS, MXSAM, MYSAM, XPL, YPL, ZPL, NPL, jsferic, kcc)
      endif    
   else  ! Delauny
      if (jakc == 0) then  
         call TRIINTfast(XS,YS,ZS,NS,1,XZ,YZ,BL,Ndx,JDLA, jakdtree, jsferic, Npl, jins, dmiss, jasfer3D, XPL, YPL, ZPL, transformcoef)
      else
         call TRIINTfast(XS,YS,ZS,NS,1,XZ,YZ,BL,Ndx,JDLA, jakdtree, jsferic, Npl, jins, dmiss, jasfer3D, XPL, YPL, ZPL, transformcoef,kcc)
      endif
   
   end if

   !   in_unit = 10 
   !   open (unit=in_unit,file="in.txt",action="write",status="replace")
   !   write (in_unit,*) NS
   !   do i=1, size(ZS)
   !      write (in_unit,*) XS(i),YS(i),ZS(i)
   !   end do 
   !   
   !   out_unit = 20
   !   open (unit=out_unit,file="out.txt",action="write",status="replace")      
   !   write (out_unit,*) NDX, JDLA 
   !   do i=1, size(BL)
   !      write (out_unit,*) XZ(i),YZ(i),BL(i)
   !   end do 
   ! close (out_unit)

   end subroutine triinterp2_dbldbl



   subroutine triinterp2_realdbl(XZ, YZ, BL, NDX, JDLA,&
                        XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)
   implicit none
   !
   ! parameters
   real(sp), intent(in)            :: XZ(NDX)
   real(sp), intent(in)            :: YZ(NDX)
   real(hp), intent(inout)         :: BL(NDX)
   integer , intent(in)            :: NDX
   integer , intent(in)            :: JDLA
   integer , intent(in)            :: NS
   integer , intent(in)            :: jins
   integer , intent(in)            :: jasfer3D
   integer , intent(in)            :: NPL
   integer , intent(in)            :: MXSAM
   integer , intent(in)            :: MYSAM
   real(hp), intent(in)            :: XS(:)
   real(hp), intent(in)            :: YS(:)
   real(hp), intent(in)            :: ZS(:)
   real(hp), intent(in)            :: dmiss
   integer                         :: jakdtree
   integer                         :: jsferic
   real(hp), intent(in)            :: XPL(:)
   real(hp), intent(in)            :: YPL(:)
   real(hp), intent(in)            :: ZPL(:)
   real(hp), intent(in)            :: transformcoef(:)
   !
   ! locals
   integer                              :: ierror
   real(hp), dimension (:), allocatable :: xz_dbl
   real(hp), dimension (:), allocatable :: yz_dbl
   !
   ! body
   allocate (xz_dbl(NDX), stat=ierror)
   allocate (yz_dbl(NDX), stat=ierror)

   xz_dbl = dble(xz)
   yz_dbl = dble(yz)
   call triinterp2_dbldbl(xz_dbl, yz_dbl, bl, NDX, JDLA,&
                        XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)
   deallocate (xz_dbl, stat=ierror)
   deallocate (yz_dbl, stat=ierror)
   end subroutine triinterp2_realdbl


   subroutine triinterp2_realreal(XZ, YZ, BL, NDX, JDLA,&
                        XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)
   implicit none
   !
   ! parameters
   real(sp), intent(in)            :: XZ(NDX)
   real(sp), intent(in)            :: YZ(NDX)
   real(sp), intent(inout)         :: BL(NDX)
   integer , intent(in)            :: NDX
   integer , intent(in)            :: JDLA
   integer , intent(in)            :: NS
   integer , intent(in)            :: jins
   integer , intent(in)            :: jasfer3D
   integer , intent(in)            :: NPL
   integer , intent(in)            :: MXSAM
   integer , intent(in)            :: MYSAM
   real(hp), intent(in)            :: XS(:)
   real(hp), intent(in)            :: YS(:)
   real(hp), intent(in)            :: ZS(:)
   real(hp), intent(in)            :: dmiss
   integer                         :: jakdtree
   integer                         :: jsferic
   real(hp), intent(in)            :: XPL(:)
   real(hp), intent(in)            :: YPL(:)
   real(hp), intent(in)            :: ZPL(:)
   real(hp), intent(in)            :: transformcoef(:)
   !
   ! locals
   integer                              :: ierror
   real(hp), dimension (:), allocatable :: xz_dbl
   real(hp), dimension (:), allocatable :: yz_dbl
   real(hp), dimension (:), allocatable :: bl_dbl
   !
   ! body
   allocate (xz_dbl(NDX), stat=ierror)
   allocate (yz_dbl(NDX), stat=ierror)
   allocate (bl_dbl(NDX), stat=ierror)

   xz_dbl = dble(xz)
   yz_dbl = dble(yz)
   bl_dbl = dble(bl)
   call triinterp2_dbldbl(xz_dbl, yz_dbl, bl_dbl, NDX, JDLA,&
                        XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)
   bl = real(bl_dbl)
   deallocate (xz_dbl, stat=ierror)
   deallocate (yz_dbl, stat=ierror)
   deallocate (bl_dbl, stat=ierror)
   end subroutine triinterp2_realreal



   SUBROUTINE TRIINTfast(XS, YS, ZS, NS,NDIM,X,Y,Z,NXY,JDLA,jakdtree, jsferic, NH, jins, dmiss, jasfer3D, &
                         XH, YH, ZH, transformcoef,kc)
   use m_ec_triangle
   use m_ec_interpolationsettings
   use geometry_module, only: dbpinpol
   ! use unstruc_colors
   ! use m_ec_kdtree2, only: ITREE_EMPTY, ITREE_DIRTY
   use mathconsts,  only: degrad_hp 
   use physicalconsts, only: earth_radius
   use kdtree2Factory
   use MessageHandling

   implicit none
   integer, intent(inout) :: jakdtree !< use kdtree (1) or not (0)
   double precision :: af
   integer :: i, IERR, k, inhul, intri, jdla, jslo
   integer :: n, in, nxx, nxy
   integer :: NDIM  !< sample vector dimension
   integer :: naf
   integer :: nbf
   integer :: ncf
   integer :: ncol
   integer :: ndraw
   integer :: nrfind
   integer :: ns, n2
   integer :: idim

   double precision :: rd
   double precision :: slo(NDIM)
   double precision :: xmaxs
   double precision :: xmins
   double precision :: xp, yp, zp(NDIM), xpmin, xpmax,ypmin, ypmax
   double precision :: ymaxs
   double precision :: ymins
   double precision :: XS(ns), YS(ns), ZS(NDIM,ns), X(nxy), Y(nxy), Z(NDIM,nxy)
   integer, optional:: kc(nxy) 
   double precision :: XL(3),YL(3)
   double precision, allocatable :: xx(:), yy(:) , zz(:,:)
   integer         , allocatable :: ind(:)

   double precision, dimension(:),   allocatable :: xs1, ys1      ! for store/restore of xs, ys
   double precision, dimension(:,:), allocatable :: zs1           ! for store/restore of zs
   
   double precision                              :: xsmin, ysmin, xsmax, ysmax  ! bounding box corner coordinates

   integer                                       :: indf(3)
   double precision                              :: wf(3)

   integer                                       :: NS1           ! for store/restore of NS

   integer                                       :: jadum, ierror

   logical                                       :: Ldeleteddata  ! for store/restore of xs, ys and zs

   integer                                       :: nh, jins, jsferic
   double precision, intent(in)                  :: dmiss    
   integer,          intent(in)                  :: jasfer3D
   double precision                              :: XH(nh), YH(nh), ZH(nh), transformcoef(:)
   integer                                       :: KMOD, jakc

   type(kdtree_instance) :: sampletree

   !LC: gui related COMMON /DRAWTHIS/  ndraw(50)

   integer                                      :: numsam, ii, k1, jakdtree2
   double precision                             :: R2Search, dist2, cof1, cof2

   jakdtree2 = jakdtree
   
   jakc = 0
   if (present(kc)) jakc = 1
   
   
   if ( jakdtree.eq.1 ) then
      !       enforce generation of kdtree
      treeglob%itreestat = ITREE_EMPTY
   end if

   !     JSLO=1, OOK SLOPES RD4
   IF (NS .LE. 2) RETURN
   JSLO = 0

   Ldeleteddata = .false.

   jdla = 1   ! later, maybe remove this if you want to avoid dlauny every time. This is not working now.
   IF (JDLA == 1) THEN

      IF (Nh > 2 ) THEN ! polygon size reduction

         xpmin = minval(xh(1:nh)) ; xpmax = maxval(xh(1:nh))
         ypmin = minval(yh(1:nh)) ; ypmax = maxval(yh(1:nh))
         rd = 0.2*(xpmax-xpmin)   ; xpmin = xpmin - rd ; xpmax = xpmax + rd
         rd = 0.2*(ypmax-ypmin)   ; ypmin = ypmin - rd ; ypmax = ypmax + rd

         !            CALL SAVESAM()   ! xs, ys, zs not necessarilly sample data

         !           store original data
         allocate(xs1(NS), ys1(NS), zs1(NDIM,NS))
         NS1 = NS
         do i=1,NS
            xs1(i) = xs(i)
            ys1(i) = ys(i)
            do k=1,NDIM
               zs1(k,i) = zs(k,i)
            end do
         end do
         Ldeleteddata = .true.

         n2   = 0
         idim = 1 ! DMISS check on first sample dimension only
         in   = -1
         DO K = 1,Ns
            if (jins == 1) then
               if ( xs(k) > xpmin .and. xs(k) < xpmax .and. ys(k) > ypmin .and. ys(k) < ypmax .and. zs(idim,k).ne.DMISS ) then
                  n2 = n2 + 1; xs(n2) = xs(k) ; ys(n2) = ys(k) ; zs(1:NDIM,n2) = zs(1:NDIM,k)
               endif
            else
               if (zs(idim,k).ne.DMISS ) then
                  n2 = n2 + 1; xs(n2) = xs(k) ; ys(n2) = ys(k) ; zs(1:NDIM,n2) = zs(1:NDIM,k)
               endif
            endif
         ENDDO


         nxx  = 0        ! count net/grid size
         DO K = 1,Nxy
            if (jins == 1) then
               if ( x(k) > xpmin .and. x(k) < xpmax .and. y(k) > ypmin .and. y(k) < ypmax ) then
                  nxx = nxx + 1
               endif
            else
               nxx = nxx + 1
            endif
         ENDDO
         allocate (xx (nxx), yy(nxx), zz(NDIM,nxx) )
         allocate (ind(nxx)) ; ind = 0

         nxx  = 0 ; in = -1  ! net/grid size reduction
         DO K = 1,Nxy
            if ( jakc == 1 ) then 
               if (kc(k) == 0) then
                  cycle
               endif   
            endif            
            call dbpinpol(x(k), y(k), in, dmiss, JINS, nh, XH, YH, ZH)
            if (in == 1) then
               nxx = nxx + 1
               ind(nxx) = k
               xx (nxx) = x(k); yy(nxx) = y(k); zz(1:NDIM,nxx) = z(1:NDIM,k)
            endif
         ENDDO
      ELSE
         NXX = NXY; N2 = NS
      ENDIF

      !        determine sample bounding box
      xsmin = minval(xs,mask=xs.ne.DMISS)
      xsmax = maxval(xs,mask=xs.ne.DMISS)
      ysmin = minval(ys,mask=ys.ne.DMISS)
      ysmax = maxval(ys,mask=ys.ne.DMISS)

      ierr = 1
      if ( jakdtree.ne.1 ) then
         CALL DLAUN(XS,YS,N2,1,IERR)
      else
         call dlaun(xs,ys,N2,3,ierr)   ! generate edgeindex and triedge
      end if
      if ( IERR.ne.0 ) then
         goto 1234
      end if
      JDLA = 0
   ENDIF

   IF (NUMTRI .LT. 1) THEN
      RETURN
   ENDIF

   !if ( numtri.lt.40 ) then
   !   jakdtree=0
   !end if

   NCOL = 14
   IF (Jtekinterpolationprocess .NE. 0) THEN
      DO I = 1,NUMTRI

         NAF   = INDX(1,I)
         NBF   = INDX(2,I)
         NCF   = INDX(3,I)
         XL(1) = XS(NAF)
         XL(2) = XS(NBF)
         XL(3) = XS(NCF)

         YL(1) = YS(NAF)
         YL(2) = YS(NBF)
         YL(3) = YS(NCF)

         !LC: GUI RELATED CALL TEKTRI(XL,YL,NCOLDN)
      ENDDO
   ENDIF

   R2search = 0d0
   if( transformcoef(6) /= dmiss ) R2search = transformcoef(6)**2
   if ( jakdtree2 == 1 .and. R2search.gt.0d0 ) then             ! build kd-tree for sample points
      sampletree%itreestat = ITREE_EMPTY
      call build_kdtree(sampletree, n2,xs,ys,ierror, jsferic, dmiss)
      if ( ierror /= 0 ) then
         if ( sampletree%itreestat /= ITREE_EMPTY ) call delete_kdtree2(sampletree)
         jakdtree2 = 0
      end if
   end if

   KMOD = MAX(1,NXX/100)
   DO N = 1,NXx

      IF (Jtekinterpolationprocess == 0 .AND. MOD(N,KMOD) == 0 ) THEN
         AF = dble(N-1) / dble(NXX)
         !LC CALL READYY('TRIANGLE INTERPOLATION',AF)
      ENDIF


      idim = 1 ! DMISS check on first sample dimension only
      if (nh > 2) then
         XP = xx(N)
         YP = yy(N)
         RD = zz(idim,N)
      else
         if ( jakc == 1 ) then 
            if (kc(n) == 0) then
               cycle
            endif   
         endif            

         XP = x(N)
         YP = y(N)
         RD = z(idim,N)
      endif


      INTRI = 0
      ! For triangulation, only consider points that are inside the sample bounding box
      if ( xp.ge.xsmin .and. xp.le.xsmax .and. yp.ge.ysmin .and. yp.le.ysmax ) then
         IF (RD .EQ. dmiss) then
            if ( jakdtree.eq.1 ) then
               jadum = 0
               !if ( N.eq.974271 ) jadum = 1
               !if ( N.eq.21 ) jadum = 1
               !if ( N.eq.263 ) jadum = 1
               !if ( N.eq.5839 .or. N.eq.5935 ) jadum = 1
               !if ( N.eq.31 .or. N.eq.169 .or. N.eq.360 ) jadum = 1
               !if ( N.eq.8081 ) jadum = 1
               !if ( N.eq.7797 ) jadum = 1
               !if ( N.eq.341 ) jadum =
               !if ( N.eq.9 ) jadum = 1
               call findtri_kdtree(XP,YP,ZP,XS,YS,ZS,N2,NDIM,NRFIND,INTRI,JSLO,SLO, & 
                                   Jtekinterpolationprocess,jadum,ierror,indf,wf, dmiss, jsferic, jins, jasfer3D)
               if ( ierror.ne.0 ) then
                  !                    deallocate
                  if ( treeglob%itreestat.ne.ITREE_EMPTY ) call delete_kdtree2(treeglob)
                  if ( allocated(imask) ) deallocate(imask)
                  if ( allocated(LNtri) ) deallocate(LNtri)
                  !                    disable kdtree
                  jakdtree = 0
               end if
            end if

            if ( jakdtree.ne.1 ) then              
               CALL FINDTRI(XP,YP,ZP,XS,YS,ZS,N2,NDIM,NRFIND,INTRI,JSLO,SLO, & 
                            Jtekinterpolationprocess,indf,wf, dmiss, jsferic, jins)
            end if

            IF (INTRI .EQ. 1) THEN
               if (nh > 2) then
                  Zz(:,N) = ZP
               else
                  z(:,n)  = zp
               endif

               if (jagetwf == 1) then
                  indxx(1:3,n) = indf(1:3)
                  wfxx (1:3,n) = wf (1:3)
               endif

            ENDIF ! (INTRI .EQ. 1)
         ENDIF ! (XP .NE. XYMIS .AND. ... .AND. YP .LE. YMAXS )
      endif


      !!!!!!!!!! give it another try with nearest neighbour opr inverse distance.
      if (intri == 0 .and. R2search.gt.0d0) then
      
!  this part is probably not prepared for spherical coordinates (and "jseric" isn't put to "0" temporarily either)
         call mess(LEVEL_ERROR, 'triintfast: smallest distance search not prepared for spherical coordinates, see UNST-1720')
      
         if (RD == dmiss) then
            if( jakdtree2 == 1 ) then
               call make_queryvector_kdtree(sampletree, xp, yp, jsferic)
               numsam = kdtree2_r_count( sampletree%tree, sampletree%qv, R2search )
               if ( numsam.gt.0 ) then
                  call realloc_results_kdtree(sampletree, numsam)
                  call kdtree2_n_nearest(sampletree%tree,sampletree%qv,numsam,sampletree%results)
               end if

               do i = 1,idim
                  ii = 0
                  cof1 = 0d0
                  cof2 = 0d0
                  do k = 1,numsam
                     k1 = sampletree%results(k)%idx
                     if( abs( xp - xs(k1) ) < 1d-6 .and. abs( yp - ys(k1) ) < 1d-6 ) then
                        ii = 1
                        z(i,n) = zs(i,k1)
                        exit
                     endif
                     dist2 = ( xp - xs(k1) )**2 +  ( yp - ys(k1) )**2
                     cof1 = cof1 + zs(i,k1) / dist2
                     cof2 = cof2 + 1d0 / dist2
                  enddo
                  if( ii == 0 .and. numsam > 0 ) then
                     z(i,n) = cof1 / cof2
                  end if
               enddo

            else
               do i = 1,idim
                  ii = 0
                  cof1 = 0d0
                  cof2 = 0d0
                  do k = 1,n2
                     if( abs( xp - xs(k) ) < 1d-6 .and. abs( yp - ys(k) ) < 1d-6 ) then
                        ii = 1
                        z(i,n) = zs(i,k)
                        exit
                     endif
                     dist2 = ( xp - xs(k) )**2 +  ( yp - ys(k) )**2
                     cof1 = cof1 + zs(i,k) / dist2
                     cof2 = cof2 + 1d0 / dist2
                  enddo
                  if( ii == 0 ) then
                     z(i,n) = cof1 / cof2
                  end if
               enddo
            endif
         endif ! RDMISS

      end if ! intri ==0

   ENDDO ! DO N = 1,NXx

   !LC IF (Jtekinterpolationprocess == 0) CALL READYY('TRIANGLE INTERPOLATION',-1d0)

   if (nh > 2) then
      do k = 1,nxx
         if ( jakc == 1 ) then 
            if (kc(k) == 0) then
               cycle
            endif   
         endif            
         do idim=1,NDIM
            z(idim,ind(k)) = zz(idim,k)
         end do
      enddo
      deallocate (xx, yy, zz, ind)
      !         CALL RESTORESAM()
   endif

1234 continue

   !     deallocate
   if ( jakdtree.eq.1 ) then
      if ( treeglob%itreestat   .ne. ITREE_EMPTY ) call delete_kdtree2(treeglob  )
      if ( sampletree%itreestat .ne. ITREE_EMPTY ) call delete_kdtree2(sampletree)
      if ( allocated(imask) ) deallocate(imask)
      if ( allocated(LNtri) ) deallocate(LNtri)
      if ( allocated(edgeindx) ) deallocate(edgeindx)
      if ( allocated(triedge) ) deallocate(triedge)
      if ( allocated(indx) ) deallocate(indx)
   end if

   !     restore original data
   if ( Ldeleteddata ) then
      NS = NS1
      do i=1,NS
         xs(i) = xs1(i)
         ys(i) = ys1(i)
         do k=1,NDIM
            zs(k,i) = zs1(k,i)
         end do
      end do
      deallocate(xs1, ys1, zs1)
   end if

   RETURN
   END subroutine triintfast


   SUBROUTINE DLAUN(XS,YS,NS,jatri,ierr)
   USE m_ec_triangle
   use m_alloc
   !LC: use unstruc_messages
   implicit none
   integer, intent(out) :: ierr
   integer              :: maxtri
   integer              :: nh
   integer              :: ns
   integer, intent(in)  :: jatri !< Type of DLaun triangulation: 1: just triangulate,
   !! 3: also produce node-edge-triangle mapping tables
   !! for use in Triangulatesamplestonetwork.
   integer :: nsm
   double precision :: trisize

   PARAMETER (NH = 1)   ! SPvdP: too small if jatri.eq.0
   double precision :: XS(ns), YS(ns)

   double precision :: XH(NH), YH(NH)

   integer, allocatable, dimension(:) :: idum

   !LC call mess(LEVEL_INFO,'Starting Delaunay triangulation...')

   !     check memory
   allocate ( idum(50*Ns) ,stat=ierr)     ! probably not enough
   call aerr('idum(50*Ns)',ierr,-50*Ns)

   if ( ierr.ne.0 ) then
      call msgbox('', 'dlaun: out of memory', LEVEL_ERROR)
      ! TODO: SvdP: consider adding 'call mess' to stop the simulation.
      !         if ( allocated(idum) ) deallocate(idum)  ! gives an error
      return
   end if

   deallocate(idum)


   NUMTRI = 0
   NUMEDGE = 0
   IF (NS .LT. 3) RETURN

   if (jatri /= 1 .and. jatri /= 3) then
      !LC call mess(LEVEL_INFO, 'Invalid jatri value in DLAUN:', jatri)
      return
   end if

   numtri = -1
   do while ( numtri.lt.0 )
      NSM    = 6*NS + 10
      IF (ALLOCATED (INDX) ) DEALLOCATE (INDX)
      ALLOCATE   (INDX(3,NSM),STAT=IERR)
      CALL AERR ('INDX(3,NSM)',IERR,INT(3*NSM))

      if (jatri==3) then
         call realloc(EDGEINDX, (/ 2,2*NSM /), keepExisting=.false., fill=0, stat=ierr)
         call aerr('edgeindx(2,NSM)', ierr, int(2*NSM))
         call realloc(TRIEDGE, (/ 3,NSM /), keepExisting=.false., fill=0, stat=ierr)
         call aerr('triedge(3,NSM)', ierr, int(3*NSM))
      else
         call realloc(EDGEINDX, (/ 2,1 /), keepExisting=.false., fill=0, stat=ierr)
         call realloc(TRIEDGE, (/ 3,1 /), keepExisting=.false., fill=0, stat=ierr)
      end if
      MAXTRI = NSM !?

      numtri = NSM ! Input value should specify max nr of triangles in indx.
      CALL TRICALL(jatri,XS,YS,NS,INDX,NUMTRI,EDGEINDX,NUMEDGE,TRIEDGE,XH,YH,NH,trisize)
      if ( numtri.lt.0 ) nsm = -numtri
   end do

   !LC call mess(LEVEL_INFO,'done')

   END SUBROUTINE DLAUN

   !>    find triangle for interpolation with kdtree
   !>       will initialize kdtree and triangulation connectivity
   subroutine findtri_kdtree(XP,YP,ZP,XS,YS,ZS,NS,NDIM,NRFIND,INTRI,JSLO,SLO,JATEK,jadum,ierror,ind, wf, dmiss, jsferic, jins, jasfer3D)
   use m_ec_triangle
   use kdtree2Factory
   !LC use MessageHandling
   use mathconsts, only: degrad_hp
   use physicalconsts, only: earth_radius
   use geometry_module, only: pinpok, cross, pinpok3D, cross3D, ave3D
   use m_alloc
   
   
   implicit none

   double precision, intent(in)                :: xp, yp    !< node coordinates
   double precision, intent(out)               :: zp(NDIM)  !< node values
   integer,          intent(in)                :: NS        !< number of samples
   integer,          intent(in)                :: NDIM      !< sample vector dimension
   double precision, intent(in)                :: xs(NS), ys(NS), zs(NDIM,NS) !< sample coordinates and values
   integer,          intent(out)               :: NRFIND    !< triangle index
   integer,          intent(out)               :: intri     !< in triangle (1) or not (0)
   integer,          intent(in)                :: jslo      !< get slope (1) or not (0)
   double precision, intent(out)               :: slo(NDIM) !< slope
   integer,          intent(in)                :: JATEK     !< draw to screen (1) or not (0)
   integer,          intent(in)                :: jadum     !< debug (1) or not (0)
   integer,          intent(out)               :: ierror    !< error (1) or not (0)
   integer,          intent(out)               :: ind(3)    !<
   double precision, intent(out)               :: wf(3)     !<


   double precision, dimension(:), allocatable :: xx, yy !< triangle circumcenter coordinates, dim(numtri)

   double precision, dimension(3)      :: xv, yv    ! triangle node coordinates
   double precision, dimension(NDIM,3) :: zv        ! triangle node vectors


   double precision                    :: xz, yz    ! triangle circumcenter
   double precision                    :: SL,SM,XCR,YCR,CRP

   integer                             :: NN        ! number of nearest points
   integer                             :: IDIM      ! dimensionality

   integer                             :: i, inod, inod1, inod2, inod3, inod4, ii, iii, iip1, inext, j, k
   integer                             :: iothertri, iiothertri, inodother, numtrisnew, jacros, iothertriangle
   integer                             :: jsferic_store, ierr
   integer                             :: iedge, k1, k2, numsearched

   double precision, parameter         :: dtol = 1d-8
   integer,          parameter         :: MAXTRICON=100
   integer,          parameter         :: INIT_TRIS_PER_NODE=6

   double precision, external          :: dcosphi
   double precision, intent(in)        :: dmiss
   integer,          intent(in)        :: jins
   integer                             :: jsferic
   integer,          intent(in)        :: jasfer3D
   
   double precision, parameter         :: dfac = 1.000001d0  ! enlargement factor for pinpok3D

   ierror = 1

   NRFIND = 0

   if ( numtri.le.1 ) then
      !            call qnerror('findtri_kdtree: numtri<=1', ' ', ' ')
      goto 1234
   end if

   ! store jsferic
   jsferic_store = jsferic

   if ( treeglob%itreestat /= ITREE_OK ) then

      !           INITIALIZE kdetree2
      !LC call mess(LEVEL_INFO,'Determining triangle connectivity and computing polygons and circumcenters...')

      allocate(xx(numtri), yy(numtri), stat=ierr)
      call aerr('xx(numtri), yy(numtri)', ierr, 2*numtri)
      allocate(imask(numtri), stat=ierr)
      call aerr('imask(numtri)', ierr, numtri)
      imask = 0
      IDENT = 0

      allocate(LNtri(2,numedge), stat=ierr)
      call aerr('LNtri(2,numedge)', ierr, 2*numedge)
      LNtri = 0

      !           dlaun is not spherical proof: set global jsferic for circumcenter
      jsferic = 0

      do i=1,numtri
         do ii=1,3
            !                 generate edge-triangle connectivity
            iedge = triedge(ii,i)
            if ( LNtri(1,iedge).eq.0 ) then
               LNtri(1,iedge) = i
            else
               LNtri(2,iedge) = i
            end if

            inod = indx(ii,i)

            !                 get triangle polygon
            xv(ii) = xs(inod)
            yv(ii) = ys(inod)
            zv(1:NDIM,ii) = zs(1:NDIM,inod)
         end do

         !              compute triangle circumcenter
         !call circumcenter3(3, xv, yv, xx(i), yy(i))
         if ( jasfer3D.eq.1 ) then
            call ave3D(3,xv,yv, xx(i), yy(i),jsferic_store,jasfer3D)
         else
            xx(i) = sum(xv(1:3)) / 3d0
            yy(i) = sum(yv(1:3)) / 3d0
         end if
      end do

      !        restore jsferic
      jsferic = jsferic_store

      !LC call mess(LEVEL_INFO, 'done')
      
      call build_kdtree(treeglob, numtri, xx, yy, ierror,jsferic, dmiss)

      !           deallocate
      deallocate(xx, yy)

      if ( ierror.ne.0 ) then
         goto 1234
      end if
   end if

   if ( jadum.eq.1 ) then
      continue
   end if

   !        update identifier
   IDENT=IDENT+1

   !        fill query vector
   call make_queryvector_kdtree(treeglob,xp,yp,jsferic)

   !        get first triangle
   call kdtree2_n_nearest(treeglob%tree,treeglob%qv,1,treeglob%results)
   inext = treeglob%results(1)%idx

   !        perform a line search
   numsearched = 1
   i = 0

   mainloop:do while ( NRFIND.eq.0 .and. numsearched.le.2*NUMTRI .and. numsearched.gt.0 )

      inext = treeglob%results(1)%idx

      numsearched = 0

!!      !        check current triangle
!      do ii=1,3
!         inod = indx(ii,inext)
!         xv(ii) = xs(inod)
!         yv(ii) = ys(inod)
!         zv(1:NDIM,ii) = zs(1:NDIM,inod)
!      end do
!
!      !        get cell centroid
!      if ( jasfer3D.eq.1 ) then
!         call ave3D(3,xv,yv,xz,yz,jsferic,jasfer3D)
!      else
!         xz = sum(xv(1:3)) / 3d0
!         yz = sum(yv(1:3)) / 3d0
!      end if

      do while ( inext.gt.0 .and. inext.le.numtri .and. numsearched.le.2*NUMTRI )   ! numsearched: safety
         i = inext

         !           check current triangle
         do ii=1,3
            inod = indx(ii,i)
            xv(ii) = xs(inod)
            yv(ii) = ys(inod)
            zv(1:NDIM,ii) = zs(1:NDIM,inod)
         end do
         
!        get a point in the cell
         if ( jasfer3D.eq.1 ) then
            call ave3D(3,xv,yv,xz,yz,jsferic,jasfer3D)
         else
            xz = sum(xv(1:3)) / 3d0
            yz = sum(yv(1:3)) / 3d0
         end if

         if ( jadum.eq.1 ) then
            !LC call tektri(xv,yv,31)
            !LC call cirr(xz,yz,31)
         end if

         if ( imask(i).eq.IDENT ) then
            intri = 0
         else
            numsearched = numsearched+1
            if ( jasfer3D.eq.0 ) then
               call pinpok(xp,yp,3,xv,yv,intri, jins, dmiss)
            else
               call pinpok3D(xp,yp,3,xv,yv,intri, dmiss, jins, 1, 1, dfac=dfac, xz=xz, yz=yz)
            end if
            
            imask(i) = IDENT
         end if

         if ( intri.eq.1 ) then
            NRFIND = i
            exit mainloop
         end if

         !           dlaun is not spherical proof: set global jsferic
         jsferic = 0

         !           proceed to next triangle, which is adjacent to the edge that is cut by the line from the current triangle to the query point
!         if ( jasfer3D.eq.1 ) then
!            call ave3D(3,xv,yv,xz,yz,jsferic_store,jasfer3D)
!         else
!            xz = sum(xv(1:3)) / 3d0
!            yz = sum(yv(1:3)) / 3d0
!         end if
         inext = 0

         if ( jadum.eq.1 ) then
            !LC call setcol(221)
            !LC call movabs(xz,yz)
            !LC call lnabs(xp,yp)
            !LC call setcol(31)
         end if

         do ii=1,3
            iedge=triedge(ii,i)
            if ( LNtri(2,iedge).eq.0 ) cycle

            iothertriangle = LNtri(1,iedge) + LNtri(2,iedge) - i
            if ( imask(iothertriangle).eq.IDENT ) then
               cycle
            end if

            k1 = edgeindx(1,iedge)
            k2 = edgeindx(2,iedge)
            if ( jasfer3D.eq.0 ) then
               call CROSS(xz, yz, xp, yp, xs(k1), ys(k1), xs(k2), ys(k2), JACROS,SL,SM,XCR,YCR,CRP, jsferic, dmiss)
            else
               call cross3D(xz, yz, xp, yp, xs(k1), ys(k1), xs(k2), ys(k2), jacros, sL, sm, xcr, ycr, jsferic_store, dmiss)
            end if

            !              use tolerance
            if ( jacros.eq.0 ) then
               if ( sm.eq.dmiss .or. sl.eq.dmiss ) then
                  !                    triangle with small area: sm and sl have not been computed
                  jacros = 1
               else IF (SM .GE. 0d0-dtol .AND. SM .LE. 1d0+dtol .AND. &
                  SL .GE. 0d0-dtol .AND. SL .LE. 1d0+dtol) THEN
               JACROS = 1
               ENDIF
            end if

            if ( jadum.eq.1 .and. jacros.eq.1 ) then
               !LC call movabs(xs(k1),ys(k1))
               !LC call lnabs(xs(k2),ys(k2))
               call msgbox('', '', LEVEL_ERROR)
               ! TODO: SvdP: consider adding 'call mess' to stop the simulation.
            end if

            if ( jacros.eq.1 ) then  ! only select this edge if it has a second adjacent triangle
               inext = iothertriangle
               exit
            end if
         end do

         !        restore jsferic
         jsferic = jsferic_store

         !if ( jadum.eq.1 ) then
         !   call tektri(xv,yv,0)
         !end if

      end do

   end do mainloop

   if ( intri.eq.0 .and. inext.gt.0 ) then
      if ( numsearched.gt.10 ) write(6,"('numsearched= ', I0)") numsearched
      !            call qnerror('findtri_kdtree: error', ' ', ' ')
   end if

   if (intri .eq. 1) then
      if ( jasfer3D.eq.0 ) then
         call linear(xv, yv, zv, NDIM, xp, yp, zp, JSLO, SLO, JATEK, wf, dmiss, jsferic)
      else
         call linear3D(xv, yv, zv, NDIM, xp, yp, zp, JSLO, SLO, JATEK, wf, dmiss, jsferic)
      end if
      do k = 1,3
         ind(k) = indx(k,nrfind)
      enddo

   endif

   ierror = 0
1234 continue

   return
   end subroutine findtri_kdtree


   SUBROUTINE FINDTRI(XP,YP,ZP,XS,YS,ZS,NS,NDIM,NRFIND,INTRI,JSLO,SLO,JATEK, ind, wf, dmiss, jsferic, jins)
   USE m_ec_triangle
   use geometry_module, only: pinpok
   
   implicit none
   integer :: intri, jatek, jslo
   integer :: k, k1, k2, numit
   integer :: nrfind, nroldfind, interval
   integer :: ns
   integer :: NDIM   !< sample vector dimension
   integer :: idim, ind(3)
   double precision :: slo(NDIM)
   double precision :: xp
   double precision :: xtmax
   double precision :: xtmin
   double precision :: yp
   double precision :: ytmax
   double precision :: ytmin
   double precision :: zp(NDIM)
   double precision :: XS(NS), YS(NS), ZS(NDIM,NS), XT(3),YT(3),ZT(NDIM,3), wf(3)
   double precision, intent(in) :: dmiss
   integer, intent(in) :: jsferic, jins

   integer          :: ik1, ik2, ik3

   DATA NROLDFIND /0/

   ZP       = dmiss
   INTRI    = 0
   interval = 2 ; numit = 0; nrfind = 0
5  CONTINUE
   numit    = numit + 1
   interval = 5*interval

   K1 = MAX(1     ,NROLDFIND-interval)
   K2 = MIN(NUMTRI,NROLDFIND+interval)

   DO K = K1,K2
      ik1 = INDX(1,K)
      ik2 = INDX(2,K)
      ik3 = INDX(3,K)
      XT(1) = XS(ik1)
      XT(2) = XS(ik2)
      XT(3) = XS(ik3)
      YT(1) = YS(ik1)
      YT(2) = YS(ik2)
      YT(3) = YS(ik3)
      XTMAX = MAX(XT(1),MAX( XT(2),XT(3) ) )
      YTMAX = MAX(YT(1),MAX( YT(2),YT(3) ) )
      XTMIN = MIN(XT(1),MIN( XT(2),XT(3) ) )
      YTMIN = MIN(YT(1),MIN( YT(2),YT(3) ) )
      IF (XP .GE. XTMIN .AND. XP .LE. XTMAX .AND.   &
         YP .GE. YTMIN .AND. YP .LE. YTMAX) THEN
      CALL PINPOK(XP,YP,3,XT,YT,INTRI, jins, dmiss)
      IF (INTRI .EQ. 1) THEN
         NRFIND    = K
         NROLDFIND = NRFIND
         do idim=1,NDIM
            ZT(idim,1) = ZS(idim,INDX(1,K))
            ZT(idim,2) = ZS(idim,INDX(2,K))
            ZT(idim,3) = ZS(idim,INDX(3,K))
         end do
         CALL LINEAR (XT, YT, ZT, NDIM, XP, YP, ZP, JSLO, SLO, JATEK, wf, dmiss, jsferic)
         ind(1) = ik1
         ind(2) = ik2
         ind(3) = ik3
         RETURN
      ENDIF
      ENDIF
   ENDDO
   if (k1 == 1 .and. k2 == numtri) then
      NROLDFIND = numtri / 2
      return
   endif
   IF (NRfind == 0) THEN
      GOTO 5
   ENDIF
   RETURN
   END subroutine FINDTRI


   SUBROUTINE LINEAR ( X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, JATEK, wf, dmiss, jsferic)
   
   use geometry_module, only: getdx, getdy
   
   ! use unstruc_colors
   implicit none
   double precision :: a11
   double precision :: a12
   double precision :: a21
   double precision :: a22
   double precision :: a31
   double precision :: a32
   double precision :: b1
   double precision :: b2
   double precision :: det
   double precision :: dum
   integer :: jatek
   integer :: jslo
   integer :: ncol
   integer :: ndraw
   double precision :: r3
   double precision :: rlam
   double precision :: rmhu
   double precision :: x1
   double precision :: x2
   double precision :: x3
   double precision :: xmax
   double precision :: xmin
   double precision :: xn
   double precision :: xp
   double precision :: xy
   double precision :: y1
   double precision :: y2
   double precision :: y3
   double precision :: ymax
   double precision :: ymin
   double precision :: yn
   double precision :: yp
   double precision :: z3
   double precision :: zn

   integer          :: idim
   integer          :: NDIM   !< sample vector dimension
   double precision :: X(3),Y(3),Z(NDIM,3), wf(3)
   double precision :: zp(NDIM)
   double precision :: slo(NDIM)
   double precision, intent(in) :: dmiss
   integer, intent(in) :: jsferic

   !COMMON /DRAWTHIS/  ndraw(50)


   ZP  = dmiss
   A11 = getdx(x(1),y(1),x(2),y(2),jsferic)   ! X(2) - X(1)
   A21 = getdy(x(1),y(1),x(2),y(2),jsferic)   ! Y(2) - Y(1)
   A12 = getdx(x(1),y(1),x(3),y(3),jsferic)   ! X(3) - X(1)
   A22 = getdy(x(1),y(1),x(3),y(3),jsferic)   ! Y(3) - Y(1)
   B1  = getdx(x(1),y(1),xp  ,yp , jsferic)   ! XP   - X(1)
   B2  = getdy(x(1),y(1),xp  ,yp , jsferic)   ! YP   - Y(1)

   DET  =   A11 * A22 - A12 * A21
   IF (ABS(DET) .LT. 1E-12) THEN		! Jan Mooiman 07-01-2015
      RETURN
   ENDIF

   RLAM = ( A22 * B1  - A12 * B2) / DET
   RMHU = (-A21 * B1  + A11 * B2) / DET
   wf(3) = rmhu
   wf(2) = rlam
   wf(1) = 1d0 - rlam - rmhu

   ZP   = Z(:,1) + RLAM * (Z(:,2) - Z(:,1)) + RMHU * (Z(:,3) - Z(:,1))

   IF (JATEK .EQ. 1) THEN
      !LC CALL ISOCOL(ZP(1),NCOL)
      !LC CALL KCIR(XP,YP,ZP(1))
      !LC CALL TEKTRI(X,Y,NCOL)
      IF (MAX(ABS(A21),ABS(A22)) .GT. 500) THEN
         DUM = 0
      ENDIF
   ENDIF

   IF (JSLO .EQ. 1) THEN
      do idim = 1,NDIM
         A31 = Z(idim,2) - Z(idim,1)
         A32 = Z(idim,3) - Z(idim,1)
         X3 =  (A21*A32 - A22*A31)
         Y3 = -(A11*A32 - A12*A31)
         Z3 =  (A11*A22 - A12*A21)
         R3 =  SQRT(X3*X3 + Y3*Y3 + Z3*Z3)
         IF (R3 .NE. 0) THEN
            XN = X3/R3
            YN = Y3/R3
            ZN = Z3/R3
            XY = SQRT(XN*XN + YN*YN)
            IF (ZN .NE. 0) THEN
               SLO(idim) = ABS(XY/ZN)
            ELSE
               SLO(idim) = dmiss
            ENDIF
         ELSE
            SLO(idim) = dmiss
         ENDIF
      end do
   ENDIF
   RETURN
   END SUBROUTINE LINEAR
   
   
   subroutine linear3D(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, JATEK, w, dmiss, jsferic)
      use geometry_module
      use MessageHandling
      implicit none

      integer,                             intent(in)     :: NDIM       !< sample vector dimension
      double precision, dimension(3),      intent(in)     :: x, y
      double precision, dimension(NDIM,3), intent(in)     :: z
      double precision,                    intent(in)     :: xp, yp
      double precision,                    intent(out)    :: zp(NDIM)
      integer,                             intent(in)     :: jslo       !< not supported
      double precision, dimension(NDIM),   intent(out)    :: slo(NDIM)
      integer,                             intent(in)     :: jatek      !< not supported
      double precision, dimension(3),      intent(out)    :: w
      double precision,                    intent(in)     :: dmiss
      integer,                             intent(in)     :: jsferic
                                       
      double precision, dimension(3)                      :: xx1, xx2, xx3, xxp
      double precision, dimension(3)                      :: s123, rhs
                                                          
      double precision, dimension(3,3)                    :: A
      
      double precision                                    :: D
                                                          
      integer                                             :: idim, i
                                                          
      double precision, parameter                         :: dtol = 0d0
      
      slo = DMISS
      if ( jslo.eq.1 ) then
         call mess(LEVEL_ERROR, 'linear3D: jslo=1 not supported')
      end if

!     get 3D coordinates of the points
      call sphertocart3D(x(1), y(1), xx1(1), xx1(2), xx1(3))
      call sphertocart3D(x(2), y(2), xx2(1), xx2(2), xx2(3))      
      call sphertocart3D(x(3), y(3), xx3(1), xx3(2), xx3(3))     
      call sphertocart3D(xp, yp, xxp(1), xxp(2), xxp(3))             
      
!     get (double) area vector
      s123 = vecprod(xx2-xx1,xx3-xx1)
      
      D = sqrt( inprod(s123,s123) )
      
      if ( D.gt.dtol ) then
!        build system:
!           gradz . (x2-x1) = z2-z1
!           gradz . (x3-x1) = z3-z1
!           gradz . ((x2-x1) X (x3-x1)) = 0
         
         A(1,:) = xx2-xx1
         A(2,:) = xx3-xx1
         A(3,:) = s123
         rhs = 0d0   ! not used
         
!        compute inverse
         call gaussj(A,3,3,rhs,1,1)
         
!        compute weights
         w(2) = inprod(xxp-xx1, A(:,1))
         w(3) = inprod(xxp-xx1, A(:,2))
         w(1) = 1d0 - w(2) - w(3)
         
!        interpolate
         do idim=1,NDIM
            zp(idim) = w(1) * z(idim,1) + w(2) * z(idim,2) + w(3) * z(idim,3)
         end do
      else
         zp = DMISS
!!        enable output problematic polygon to stdout:
!         write(6,*) xp, yp
!         write(6,*) 'L001'
!         write(6,*) 3, 2
!         do i=1,3
!            write(6,*) x(i), y(i)
!         end do
         call mess(LEVEL_ERROR, 'linear3D: area too small')
      end if
      
      return
   end subroutine linear3D
   
   !---------------------------------------------------------------------------!
   !   bilin_interp
   !---------------------------------------------------------------------------!

   !> bilinear interpolation of structed sample data at points
   subroutine bilin_interp(Nc, xc, yc, zc, dmiss, XS, YS, ZS, MXSAM, MYSAM, XPL, YPL, ZPL, NPL, jsferic, kc)

   implicit none

   integer,                         intent(in)  :: Nc       !< number of points to be interpolated
   double precision, dimension(Nc), intent(in)  :: xc, yc   !< point coordinates
   double precision, dimension(Nc), intent(out) :: zc       !< interpolated point values
   double precision                             :: xi, eta
   double precision, intent(in)                 :: dmiss
   
   double precision, intent(in)                 ::  XS(:), YS(:), ZS(:)
   double precision, intent(in)                 ::  XPL(:),YPL(:), ZPL(:)
   integer, intent(in)                          ::  MXSAM, MYSAM, NPL, jsferic
   integer, intent(in), optional                ::  kc(nc) 
   
   integer               :: ierror
   integer               :: k, jakc

   jakc = 0
   if (present(kc)) jakc = 1
   
   ierror = 1

   if ( MXSAM.eq.0 .or. MYSAM.eq.0 ) then
      call msgbox('', 'bilin_interp: sample data is unstructured', LEVEL_ERROR)
      ! TODO: SvdP: consider adding 'call mess' to stop the simulation.
      goto 1234
   end if

   xi  = 0d0
   eta = 0d0

   do k=1,Nc
      if ( zc(k).eq.DMISS ) then
         if (jakc == 0) then 
            call bilin_interp_loc(MXSAM, MYSAM, MXSAM, MYSAM, 1, XS, YS, ZS, xc(k), yc(k), xi, eta, zc(k), ierror, dmiss, jsferic)
         else if (kc(k) == 1) then
            call bilin_interp_loc(MXSAM, MYSAM, MXSAM, MYSAM, 1, XS, YS, ZS, xc(k), yc(k), xi, eta, zc(k), ierror, dmiss, jsferic)
         endif   
      end if
   end do

   ierror = 0

   !  error handling
1234 continue

   return
   end subroutine bilin_interp


   !> bilinear interpolation between nodes
   subroutine bilin_interp_loc(Nxmax, Nymax, Nx, Ny, NDIM, x, y, z, xp, yp, xi, eta, zp, ierror, dmiss, jsferic)
   
   use geometry_module, only: getdxdy
   
   implicit none

   integer,                                    intent(in)    :: Nxmax, Nymax !< node array size
   integer,                                    intent(in)    :: Nx, Ny   !< actual sizes
   integer,                                    intent(in)    :: NDIM     !< sample vector dimension
   double precision, dimension(Nxmax,Nymax),   intent(in)    :: x, y     !< node coordinates
   double precision, dimension(NDIM,Nxmax,Nymax), intent(in) :: z        !< node values
   double precision,                           intent(in)    :: xp, yp   !< interpolant coordinates
   double precision,                           intent(inout) :: xi, eta  !< interpolant index coordinates (in: first iterate)
   double precision, dimension(NDIM),          intent(out)   :: zp       !< interpolant value
   integer,                                    intent(out)   :: ierror   !< error (1) or not (0)

   double precision                                          :: x1, y1
   double precision, dimension(NDIM)                         :: z1
   double precision, dimension(2,2)                          :: DxDxi, DxiDx
   double precision                                          :: Dx, Dy   ! residual (in Cartesian coordinates always)
   double precision                                          :: Dxi, Deta
   double precision                                          :: eps, epsprev
   double precision                                          :: det

   integer                                                   :: iter, ierror_loc

   double precision, parameter                               :: dtol    = 1d-6
   double precision, parameter                               :: dmaxincrease = 100
   integer,          parameter                               :: MAXITER = 1000
   double precision, parameter                               :: dmindif = 1d-2
   double precision, intent(in)                              :: dmiss
   integer, intent(in)                                       :: jsferic

   ierror = 1

   zp = DMISS

   !  set realistic start values
   xi  = min(max(xi, 0d0),dble(Nx)-1d0)
   eta = min(max(eta,0d0),dble(Ny)-1d0)

   !  Newton iterations
   eps = 1d99
   epsprev = 2d0*eps
   do iter=1,MAXITER
      call comp_x_DxDxi(Nxmax, Nymax, Nx, Ny, NDIM, x, y, z, xi, eta, x1, y1, z1, DxDxi, ierror_loc, dmiss, jsferic)

      if ( ierror_loc.ne.0 ) goto 1234

      !     compute residual
      !Dx = getdx(x1,y1,xp,yp)
      !Dy = getdy(x1,y1,xp,yp)
      call getdxdy(x1,y1,xp,yp,dx,dy, jsferic)

      epsprev= eps
      eps = sqrt(Dx**2 + Dy**2)

      if ( eps.lt.dtol .or. (epsprev-eps).lt.dmindif*epsprev ) exit

      !     invert Jacobian matrix
      det = DxDxi(1,1)*DxDxi(2,2) - DxDxi(2,1)*DxDxi(1,2)
      if ( abs(det).lt.1d-9 ) goto 1234

      DxiDx(1,1) =  DxDxi(2,2)/det
      DxiDx(1,2) = -DxDxi(1,2)/det
      DxiDx(2,1) = -DxDxi(2,1)/det
      DxiDx(2,2) =  DxDxi(1,1)/det

      !     compute (xi,eta)-increment
      Dxi  = DxiDx(1,1) * Dx + DxiDx(1,2) * Dy
      Deta = DxiDx(2,1) * Dx + DxiDx(2,2) * Dy

      !     limit (xi,eta)-increment
      if ( Dxi  .gt.  dmaxincrease ) Dxi  =  dmaxincrease
      if ( Dxi  .lt. -dmaxincrease ) Dxi  = -dmaxincrease
      if ( Deta .gt.  dmaxincrease ) Deta =  dmaxincrease
      if ( Deta .lt. -dmaxincrease ) Deta = -dmaxincrease

      xi  = xi  + Dxi
      eta = eta + Deta

      !     set realistic values
      xi  = min(max(xi, 0d0),dble(Nx)-1d0)
      eta = min(max(eta,0d0),dble(Ny)-1d0)
   end do

   if ( eps.gt.dtol ) then
      !      call qnerror('bilin_interp_loc: no convergence', ' ', ' ')
      goto 1234
   end if

   !  set interpolated node value
   zp = z1

   ierror = 0

   !  error handling
1234 continue

   return
   end subroutine bilin_interp_loc

   !> bilinear interpolation of node coordinates and Jacobian matrix
   subroutine comp_x_DxDxi(Ncx, Ncy, Nx, Ny, NDIM, x, y, z, xi, eta, x1, y1, z1, DxDxi, ierror, dmiss, jsferic)

   use geometry_module, only: getdx, getdy
   
   implicit none

   integer,                                   intent(in)  :: Ncx, Ncy !< node array sizes
   integer,                                   intent(in)  :: Nx, Ny   !< actual sizes
   integer,                                   intent(in)  :: NDIM     !< sample vector dimension
   double precision, dimension(Ncx,Ncy),      intent(in)  :: x, y     !< node coordinates
   double precision, dimension(NDIM,Ncx,Ncy), intent(in)  :: z        !< node values
   double precision,                          intent(in)  :: xi, eta  !< interpolant index coordinates
   double precision,                          intent(out) :: x1, y1   !< interpolant coordinates
   double precision, dimension(NDIM),         intent(out) :: z1       !< interpolant value
   double precision, dimension(2,2),          intent(out) :: DxDxi    !< Jacobian matrix
   integer,                                   intent(out) :: ierror   !< error (1) or not (0)

   double precision                                       :: xiL, etaL, xiL1, etaL1

   integer                                                :: i0, i1, j0, j1, k
   double precision, intent(in)                           :: dmiss 
   integer                                                ::jsferic

   !double precision, external                             :: getdx, getdy

   ierror = 1

   if ( Nx.lt.2 .or. Ny.lt.2 ) goto 1234

   !  get the cell indices
   i0 = max(min(int(xi)+1, Nx-1), 1)
   i1 = i0+1
   j0 = max(min(int(eta)+1, Ny-1), 1)
   j1 = j0+1

   !  compute local index coordinates
   xiL  = xi  - dble(i0-1)
   etaL = eta - dble(j0-1)

   xiL1  = 1d0 - xiL
   etaL1 = 1d0 - etaL

   !  check if all values are valid
   if ( x(i0,j0).eq.DMISS .or. x(i1,j0).eq.DMISS .or. x(i0,j1).eq.DMISS .or. x(i1,j1).eq.DMISS .or.   &
      y(i0,j0).eq.DMISS .or. y(i1,j0).eq.DMISS .or. y(i0,j1).eq.DMISS .or. y(i1,j1).eq.DMISS ) then
   goto 1234
   end if

   !  bilinear interpolation of node coordinates
   x1 = ( xiL1*x(i0,j0) + xiL*x(i1,j0) )*etaL1 + ( xiL1*x(i0,j1) + xiL*x(i1,j1) )*etaL
   y1 = ( xiL1*y(i0,j0) + xiL*y(i1,j0) )*etaL1 + ( xiL1*y(i0,j1) + xiL*y(i1,j1) )*etaL
   do k=1,NDIM
      if ( z(k,i0,j0).eq.DMISS .or. z(k,i1,j0).eq.DMISS .or. z(k,i0,j1).eq.DMISS .or. z(k,i1,j1).eq.DMISS ) then
         z1(k) = DMISS
      else
         z1(k) = ( xiL1*z(k,i0,j0) + xiL*z(k,i1,j0) )*etaL1 + ( xiL1*z(k,i0,j1) + xiL*z(k,i1,j1) )*etaL
      end if
   end do

   !  Jacobian matrix
   DxDxi(1,1) = etaL1*getdx(x(i0,j0),y(i0,j0),x(i1,j0),y(i1,j0),jsferic) +   &
      etaL *getdx(x(i0,j1),y(i0,j1),x(i1,j1),y(i1,j1),jsferic)
   DxDxi(1,2) = xiL1 *getdx(x(i0,j0),y(i0,j0),x(i0,j1),y(i0,j1),jsferic) +   &
      xiL  *getdx(x(i1,j0),y(i1,j0),x(i1,j1),y(i1,j1),jsferic)

   DxDxi(2,1) = etaL1*getdy(x(i0,j0),y(i0,j0),x(i1,j0),y(i1,j0),jsferic) +   &
      etaL *getdy(x(i0,j1),y(i0,j1),x(i1,j1),y(i1,j1),jsferic)
   DxDxi(2,2) = xiL1 *getdy(x(i0,j0),y(i0,j0),x(i0,j1),y(i0,j1),jsferic) +   &
      xiL  *getdy(x(i1,j0),y(i1,j0),x(i1,j1),y(i1,j1),jsferic)

   ierror = 0
   !  error handling
1234 continue
   return
   end subroutine comp_x_DxDxi
   

    !---------------------------------------------------------------------------!
    !   averaging2
    !---------------------------------------------------------------------------!
    
    
    !> interpolate/average sample vector data in a polygon (e.g. a netcell)
    !>   note: M_samples is not used
    !>         XS and YS are the sample coordinates, dim(NS)
    !>         ZSS contains a NDIM-dimensional vector for each of the NS samples, dim(NDIM,NS)
    
    SUBROUTINE AVERAGING2(NDIM,NS,XS,YS,ZSS,IPSAM,XC,YC,ZC,NX,XX,YY,N6,NNN,jakdtree_, &
                          dmiss, jsferic, jasfer3D, JINS, NPL, xpl, ypl, zpl) ! WERKT ALLEEN VOOR CELL REGIONS, DIE ZITTEN IN XX EN YY

    use m_ec_interpolationsettings
    use kdtree2Factory
    use geometry_module, only: pinpok, dbdistance, dbpinpol
    use sorting_algorithms, only: indexx

    IMPLICIT NONE
    INTEGER,                              INTENT(IN)    :: NDIM                 ! sample vector dimension
    INTEGER,                              INTENT(IN)    :: NS                   ! number of samples
    DOUBLE PRECISION, DIMENSION(NS),      INTENT(IN)    :: XS, YS               ! sample coordinates
    DOUBLE PRECISION, DIMENSION(NDIM,NS), INTENT(IN)    :: ZSS                  ! sample values
    INTEGER,          DIMENSION(NS),      INTENT(IN)    :: IPSAM                ! sample permutation array (increasing x-coordinate)
    INTEGER,                              INTENT(IN)    :: NX, N6               ! number of polygons and maximum polygon size
    DOUBLE PRECISION,                     INTENT(IN)    :: XC(NX), YC(NX)       ! polygon center coordinates
    DOUBLE PRECISION,                     INTENT(INOUT) :: ZC(NDIM,NX)          ! ZC not initialized here
    DOUBLE PRECISION,                     INTENT(IN)    :: XX(N6,NX), YY(N6,NX) ! polygon coordinates
    INTEGER,                              INTENT(IN)    :: NNN(NX)              ! polygon sizes
    integer,                              intent(in)    :: jakdtree_            ! use kdtree (1) or not (0)

    DOUBLE PRECISION, ALLOCATABLE     :: XH(:), YH(:)
    DOUBLE PRECISION, DIMENSION(NDIM) :: HP, RHP
    DOUBLE PRECISION   :: XLOW, XHIH, YLOW, YHIH, AF, RMIN2, WALL, DIS2, WEIGHT, XP, YP, XDUM
    INTEGER            :: N,K,NN,MODIN, NLOWX, NHIHX, NUMXY, IFIRS, ILAST, INHUL
    INTEGER            :: IVAR
    INTEGER            :: K_, k_start, k_end

    integer            :: japrogressbar, jadoen, in, numsam

    character(len=128) :: txt

    double precision   :: R2search, t0, t1, t2, rnn

    integer            :: ierror

    integer            :: jakdtree = 0   ! use kdtree (1) or not (0)
    integer, parameter :: jatimer  = 0   ! output timings (1) or not (0)


    double precision, allocatable :: zz(:)
    integer         , allocatable :: kkin(:)
    integer                       :: nin, ki, n1, n2, n12, num
    
    
    double precision, intent(in)            :: dmiss
    integer, intent(in)                     :: jsferic, jasfer3D, NPL, JINS
    double precision, intent(in)            :: XPL(:), YPL(:), ZPL(:)
    integer                                 :: i, in_unit, out_unit 
    
    INTEGER :: NCOLNOW
    
    !COMMON /COLNOW/ NCOLNOW

    ! default/no samples in cell
    ! ZC = DMISS
    ! hk : do not switch off please

    jakdtree = jakdtree_

    japrogressbar = 1

    if ( percentileminmax > 0d0) then
       allocate(zz(ns), kkin(ns) )
    endif

    if ( jtekinterpolationprocess.eq.1 .or. Nx.lt.100 ) then
       japrogressbar = 0
    end if

    ALLOCATE (XH(N6), YH(N6) )

    !IF ( japrogressbar.eq.1 ) THEN
    !   CALL READYY('GRIDCELL AVERAGING', AF)
    !ENDIF

    !if ( jatimer.eq.1 ) then
    !   call klok(t0)
    !   t1 = t0
    !end if

    MODIN = MAX(1.0,REAL(NX)/100.0)
    in = -1
    DO N = 1,NX

       JADOEN = 0
       do ivar=1,NDIM
          if ( ZC(ivar,N) == DMISS ) THEN
             JADOEN = 1
          endIF
       enddo
       if (jadoen == 0 .or. NNN(N) == 0) cycle ! Skip undefined cells (0 corners)

       if (npl > 0) then
          CALL DBPINPOL( XC(N), YC(N), in, dmiss, JINS, NPL, xpl, ypl, zpl)
          if (in == 0) then
             cycle
          endif
       endif

       !IF ( japrogressbar.eq.1 )THEN
       !   IF (MOD(MODIN,N) .EQ. 0) THEN
       !      AF = DBLE(N) / DBLE(NX)
       !      CALL READYY('GRIDCELL AVERAGING', AF)
       !   ENDIF
       !ENDIF

       NN   = NNN(N)
       DO K = 1,NN
          XH(K) = XX(K,N)
          YH(K) = YY(K,N)
       ENDDO
       DO K = 1,NN
          XH(K) = RCEL*XH(K) + (1D0-RCEL)*XC(N)
          YH(K) = RCEL*YH(K) + (1D0-RCEL)*YC(N)
       ENDDO


       XLOW = MINVAL(XH(1:NN)) ; XHIH = MAXVAL(XH(1:NN))
       YLOW = MINVAL(YH(1:NN)) ; YHIH = MAXVAL(YH(1:NN))

       !    check for periodic coordinates
       !    it is assumed that the user has provided sufficient sample overlap
       if ( jsferic.eq.1 ) then

          if ( xhih-xlow.gt.180d0) then
             xdum = 0.5d0*(xlow+xhih)

             do k=1,NN
                if ( xh(k).lt.xdum ) then
                   xh(k) = xh(k) + 360d0
                end if
             end do
             XLOW = MINVAL(XH(1:NN)) ; XHIH = MAXVAL(XH(1:NN))

             !          BEGIN DEBUG
             !           call tekpoly(nn,xh,yh,31)
             !           call toemaar()
             !          END DEBUG

          end if
       end if

       if ( jakdtree.eq.0 ) then
          CALL LOCATE(XS,NS,IPSAM,XLOW,NLOWX); IF (NLOWX == 0) NLOWX = 1
          CALL LOCATE(XS,NS,IPSAM,XHIH,NHIHX)
          k_start = NLOWX
          k_end   = min(NHIHX+1,NS)
       else   ! kdtree
          !       compute cell-bounding circle radius
          R2search = 0d0
          do k=1,NN
             R2search = max(R2search,dbdistance(xc(N),yc(N),xh(k),yh(k),jsferic, jasfer3D, dmiss)**2)
          end do

          !       find all samples in the cell-bounding circle
          call make_queryvector_kdtree(treeglob,xc(N),yc(N), jsferic)

          !       count number of points in search area
          numsam = kdtree2_r_count(treeglob%tree,treeglob%qv,R2search)

          !       set number of points to be queried
          k_start = 1
          k_end   = numsam

          if ( numsam.gt.0 ) then
             !          resize results array if necessary
             call realloc_results_kdtree(treeglob,numsam)

             !          find samples
             call kdtree2_n_nearest(treeglob%tree,treeglob%qv,numsam,treeglob%results)
          end if
       end if

       HP    = 0
       NUMXY = 0
       RMIN2 = dbdistance(XHIH, yhih,xlow,ylow, jsferic, jasfer3D, dmiss)
       IFIRS = 0
       WALL  = 0
       nin   = 0
       sam:DO K_ = k_start,k_end
          if ( jakdtree.ne.1 ) then
             k  = ipsam(k_)
          else
             k = treeglob%results(k_)%idx
          end if

          do ivar=1,NDIM
             if ( zss(ivar,k).eq.DMISS ) cycle sam
          end do

          IF (YS(K) .GE. YLOW .AND. YS(K) .LE. YHIH) THEN
             CALL PINPOK(XS(K),YS(K),NN,XH,YH,INHUL, jins, dmiss)
             IF (INHUL .EQ. 1) THEN
                do ivar=1,NDIM
                   IF (IAV .EQ. 1) THEN
                      NUMXY  = NUMXY + 1
                      HP(IVAR) = HP(IVAR) + ZSS(IVAR,K)
                   ELSE IF (IAV .EQ. 2) THEN
                      DIS2 = dbdistance(XS(K),YS(K),XC(N),YC(N), jsferic, jasfer3D, dmiss)
                      IF (DIS2 .LT. RMIN2) THEN
                         RMIN2 = DIS2
                         HP(IVAR) = ZSS(IVAR,K)
                         NUMXY = 1
                      ENDIF
                   ELSE IF (IAV .LE. 4 .OR. IAV .EQ. 6) THEN
                      IF (IFIRS .EQ. 0) THEN
                         IFIRS = 1
                         HP(IVAR) = ZSS(IVAR,K)
                         NUMXY = 1
                      ENDIF
                      IF (IAV .EQ. 3) THEN
                         HP(IVAR) = MAX(HP(IVAR),ZSS(IVAR,K))
                         if (percentileminmax > 0d0 .and. ivar == 1) then
                            nin = nin + 1 ; kkin(nin) = k
                         endif
                      ELSE IF (IAV .EQ. 4) THEN
                         HP(IVAR) = MIN(HP(IVAR),ZSS(IVAR,K))
                         if (percentileminmax > 0d0 .and. ivar == 1) then
                            nin = nin + 1 ; kkin(nin) = k
                         endif
                      ELSE IF (IAV .EQ. 6) THEN
                         HP(IVAR) = MIN(ABS(HP(IVAR)),ABS(ZSS(IVAR,K)))
                      ENDIF
                   ELSE IF (IAV .EQ. 5) THEN
                      NUMXY  = NUMXY + 1
                      DIS2 = dbdistance(XS(K),YS(K),XC(N),YC(N), jsferic, jasfer3D, dmiss)
                      DIS2   = MAX(0.01,DIS2)
                      WEIGHT = 1/DIS2
                      WALL   = WALL + WEIGHT
                      HP(IVAR) = HP(IVAR) + WEIGHT*ZSS(IVAR,K)
                   ENDIF
                end do ! do ivar=1,NDIM
             ENDIF
          ENDIF
       ENDDO sam

       RHP = DMISS
       IF (IAV .EQ. 1 .OR. IAV .EQ. 5) THEN
          IF (NUMXY .GE. NUMMIN) THEN
             IF (IAV .EQ. 1) THEN
                RHP = HP / REAL(NUMXY)
             ELSE IF (IAV .EQ. 5) THEN
                RHP = HP/WALL
             ENDIF
             IF (JTEKINTERPOLATIONPROCESS >  0) THEN  ! plot first variable only
                !CALL KCIR(XP,YP,RHP(1))
                !CALL DISPF2(XH,YH,NN,NN,NCOLNOW)
                !CALL LNABS(XH(1),YH(1))
             ENDIF
          ENDIF
       ELSE IF (NUMXY .GE. 1) THEN
          RHP = HP
          if ((iav == 3 .or. iav == 4) .and. percentileminmax > 0d0 .and. ndim == 1) then  ! compute percentile
             do nn = 1,nin
                zz(nn) = zss( 1,kkin(nn) )
             enddo
             call indexx(nin,zz,kkin)
             rnn   = 0 ; rhp(1) = 0d0; num = nint(0.01d0*percentileminmax*nin)
             if (iav == 4) then
                n1 = 1   ; n2 =  num           ; n12 = 1
             else
                n1 = nin ; n2 =  nin - num + 1 ; n12 = -1
             endif
             do nn = n1, n2, n12
                rnn = rnn + 1d0
                rhp(1) = rhp(1) + zz(kkin(nn))
             enddo
             if (rnn > 0) then
                rhp(1) = rhp(1) / rnn
             endif
          endif
          IF (JTEKINTERPOLATIONPROCESS > 0) THEN
             !CALL KCIR(XP,YP,RHP(1))
             !CALL DISPF2(XH,YH,NN,NN,NCOLNOW)
             !CALL LNABS(XH(1),YH(1))
          ENDIF
       ENDIF

       do ivar=1,NDIM
          IF (RHP(ivar) .NE. DMISS) THEN
             ZC(ivar,N) = RHP(ivar)
          ENDIF
       end do

       !if ( jatimer.eq.1 ) then
       !   call klok(t2)
       !   if ( t2-t1.gt.10d0 ) then
       !      write(txt, "('averaging2: ', F0.2, ' seconds passed, N= ', I0, ' of ', I0)") t2-t0, N, NS
       !      !LC call mess(LEVEL_INFO, trim(txt))
       !      t1 = t2
       !   end if
       !end if

    ENDDO

    DEALLOCATE (XH, YH)

    !LC IF ( japrogressbar.eq.1 ) CALL READYY('GRIDCELL AVERAGING', -1d0)

    ! LC
    !! output message
    !if ( jatimer.eq.1 ) then
    !   call klok(t1)
    !
    !   txt = ''
    !   write(txt, "('averaged ', I0, ' values in ', I0, ' samples in ', F0.2, ' seconds.')") NX, NS, t1-t0
    !    call mess(LEVEL_INFO, trim(txt))
    !end if

    if ( percentileminmax > 0d0) then
       deallocate( zz, kkin )
    endif

    !!Print results to file
    !in_unit = 10
    !open (unit=in_unit,file="in.txt",action="write",status="replace")
    !write (in_unit,*) NS
    !do i=1, NS
    !   write (in_unit,*) XS(i),YS(i),ZSS(1,i)
    !end do
    !
    !out_unit = 20
    !open (unit=out_unit,file="out.txt",action="write",status="replace")
    !write (out_unit,*) NX
    !do i=1, size(XC)
    !   write (out_unit,*) XC(i),YC(i),ZC(1,i)
    !end do

    !close (out_unit)

  END SUBROUTINE AVERAGING2
                          
   SUBROUTINE LOCATE(XX,N,IPERM,X,J)
   INTEGER          :: N, J
   integer, dimension(N), intent(in) :: IPERM !< permutation array (increasing xx)
   DOUBLE PRECISION :: XX(N), X

   INTEGER          :: JL, JU, JM

   JL=0
   JU=N+1
10 IF(JU-JL.GT.1)THEN
     JM=(JU+JL)/2
     IF((XX(IPERM(N)).GT.XX(IPERM(1))).EQV.(X.GT.XX(IPERM(JM))))THEN
       JL=JM
     ELSE
       JU=JM
     ENDIF
     GO TO 10
   ENDIF
   J=JL
   RETURN
   END SUBROUTINE LOCATE

   end module m_ec_basic_interpolation
