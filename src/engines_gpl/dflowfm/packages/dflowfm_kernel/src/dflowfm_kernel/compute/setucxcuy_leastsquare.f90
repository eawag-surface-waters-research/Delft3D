module m_setucxcuy_leastsquare
   
   double precision, dimension(:,:,:), allocatable :: AtWAiAtW ! Matrix on each flow node
   integer         , dimension(:)    , allocatable :: ireconstu
   integer         , dimension(:)    , allocatable :: ireconstz
 
   contains
   
   ! ==============================================================================================
   ! ==============================================================================================
   subroutine reconst2ndini ()
      use m_flow
      use m_flowgeom
      use m_netw
        
      implicit none
      integer                                :: i, j, k, k1, k2, kk, L, L1, L2, L3, m, mm, n1, n2, nn, ierr
      integer                                :: L1a, L2a, L3a, k3, nmax
      double precision                       :: cof0, xwall, ywall, scale, cs, sn, Deltxu, Deltyu
      integer, parameter                     :: mmax = 64
      double precision, dimension(9,9)       :: AtWA, AtWAi
      double precision, dimension(9,mmax)    :: AtW
      double precision, dimension(mmax,9)    :: Amat
      double precision, dimension(mmax)      :: Wmat
      double precision, dimension(9)         :: svec
      double precision, dimension(9,9)       :: smat
      integer, dimension(lnx)                :: LDone
      integer, dimension(mxwalls)            :: LwDone
      double precision                       :: condnr, sum
      double precision, dimension(9)         :: cvec, dvec
      double precision, dimension(9,9)       :: Rmat, Rmati, Qmatt, RiQt
      logical :: sing
      
      if (allocated(AtWAiAtW)) deallocate(AtWAiAtW)
      allocate(AtWAiAtW(9,mmax,lnx), stat = ierr) ; AtWAiAtW = 0d0
      if (allocated(ireconstu)) deallocate(ireconstu)
      allocate(ireconstu(lnx)) ; ireconstu = 0
      if (allocated(ireconstz)) deallocate(ireconstz)
      allocate(ireconstz(ndx)) ; ireconstz = 0

      scale = 1d0
      ireconstu = 0
      ireconstz = 0

      do L = 1,lnxi
         m = 0
         AtWA = 0d0 ; AtWAi = 0d0 ; AtW = 0d0
         Amat = 0d0
         Wmat = 0d0
         LDone = 0
         LwDone = 0
         k1 = ln(1,L) ; k2 = ln(2,L)
         
         do L1 = 1,nd(k1)%nwx
            L1a = abs(nd(k1)%nw(L1))
            if (LwDone(L1a) == 1) cycle
            m = m + 1
            n1 = walls(2, L1a)
            n2 = walls(3, L1a)
            xwall = (xk(n1) + xk(n2)) * 0.5d0
            ywall = (yk(n1) + yk(n2)) * 0.5d0
            Deltxu = xwall - xu(L)
            Deltyu = ywall - yu(L)
            cs = - walls(8,L1a)
            sn  =  walls(7,L1a)
            Amat(m,1) = cs
            Amat(m,2) = sn
            Amat(m,3) = cs * Deltxu
            Amat(m,4) = sn * Deltxu
            Amat(m,5) = cs * Deltyu
            Amat(m,6) = sn * Deltyu
            Amat(m,7) = cs * Deltxu * Deltyu
            Amat(m,8) = sn * Deltxu * Deltyu
            cof0 = sqrt( Deltxu**2 + Deltyu**2 )
            if (cof0 > 1d-10) then 
               Wmat(m) = 1d0 / cof0
            else
               Wmat(m) = 1d0
            endif
            LwDone(L1a) = 1
         enddo
         
         do L2 = 1,nd(k2)%nwx
            L2a = abs(nd(k2)%nw(L2))
            if (LwDone(L2a) == 1) cycle
            m = m + 1
            n1 = walls(2, L2a)
            n2 = walls(3, L2a)
            xwall = (xk(n1) + xk(n2)) * 0.5d0
            ywall = (yk(n1) + yk(n2)) * 0.5d0
            Deltxu = xwall - xu(L)
            Deltyu = ywall - yu(L)
            cs = - walls(8,L2a)
            sn  =  walls(7,L2a)
            Amat(m,1) = cs
            Amat(m,2) = sn
            Amat(m,3) = cs * Deltxu
            Amat(m,4) = sn * Deltxu
            Amat(m,5) = cs * Deltyu
            Amat(m,6) = sn * Deltyu
            Amat(m,7) = cs * Deltxu * Deltyu
            Amat(m,8) = sn * Deltxu * Deltyu
            cof0 = sqrt( Deltxu**2 + Deltyu**2 )
            if (cof0 > 1d-10) then 
               Wmat(m) = 1d0 / cof0
            else
               Wmat(m) = 1d0
            endif
            LwDone(L2a) = 1
         enddo

         do L1 = 1,nd(k1)%lnx
            L1a = abs(nd(k1)%ln(L1))
            if (L1a == L) cycle
            k3 = ln(1,L1a) + ln(2,L1a) - k1
            do L3 = 1,nd(k3)%lnx
               L3a = abs(nd(k3)%ln(L3))
               if (LDone(L3a) == 1) cycle 
               m = m + 1
               cs = csu(L3a)
               sn = snu(L3a)
               Deltxu = xu(L3a) - xu(L)
               Deltyu = yu(L3a) - yu(L)
               Amat(m,1) = cs
               Amat(m,2) = sn
               Amat(m,3) = cs * Deltxu
               Amat(m,4) = sn * Deltxu
               Amat(m,5) = cs * Deltyu
               Amat(m,6) = sn * Deltyu
               Amat(m,7) = cs * Deltxu * Deltyu
               Amat(m,8) = sn * Deltxu * Deltyu
               cof0 = sqrt(Deltxu**2 + Deltyu**2)
               if (cof0 > 1.0d-10) then
                  Wmat(m) = 1d0 / cof0
               else
                  Wmat(m) = 1d0
               endif
               LDone(L3a) = 1
            enddo
            do L3 = 1,nd(k3)%nwx
               L3a = abs(nd(k3)%nw(L3))
               if (LwDone(L3a) == 1) cycle
               m = m + 1
               n1 = walls(2,L3a)
               n2 = walls(3,L3a)
               xwall = (xk(n1) + xk(n2)) * 0.5d0
               ywall = (yk(n1) + yk(n2)) * 0.5d0
               Deltxu = xwall - xu(L)
               Deltyu = ywall - yu(L)
               cs = - walls(8,L3a)
               sn  =  walls(7,L3a)
               Amat(m,1) = cs
               Amat(m,2) = sn
               Amat(m,3) = cs * Deltxu
               Amat(m,4) = sn * Deltxu
               Amat(m,5) = cs * Deltyu
               Amat(m,6) = sn * Deltyu
               Amat(m,7) = cs * Deltxu * Deltyu
               Amat(m,8) = sn * Deltxu * Deltyu
               cof0 = sqrt( Deltxu**2 + Deltyu**2 )
               if (cof0 > 1d-10) then
                  Wmat(m) = 1d0 / cof0
               else
                  Wmat(m) = 1d0
               endif
               LwDone(L3a) = 1
            enddo
         enddo
         
         do L2 = 1,nd(k2)%lnx
            L2a = abs(nd(k2)%ln(L2))
            if (L2a == L) cycle
            k3 = ln(1,L2a) + ln(2,L2a) - k2
            do L3 = 1,nd(k3)%lnx
               L3a = abs(nd(k3)%ln(L3))
               if (LDone(L3a) == 1) cycle 
               m = m + 1
               cs = csu(L3a)
               sn = snu(L3a)
               Deltxu = xu(L3a) - xu(L)
               Deltyu = yu(L3a) - yu(L)
               Amat(m,1) = cs
               Amat(m,2) = sn
               Amat(m,3) = cs * Deltxu
               Amat(m,4) = sn * Deltxu
               Amat(m,5) = cs * Deltyu
               Amat(m,6) = sn * Deltyu
               Amat(m,7) = cs * Deltxu * Deltyu
               Amat(m,8) = sn * Deltxu * Deltyu
               cof0 = sqrt(Deltxu**2 + Deltyu**2)
               if (cof0 > 1d-10) then
                  Wmat(m) = 1d0 / cof0
               else
                  Wmat(m) = 1d0
               endif
               LDone(L3a) = 1
            enddo
            do L3 = 1,nd(k3)%nwx
               L3a = abs(nd(k3)%nw(L3))
               if (LwDone(L3a) == 1) cycle
               m = m + 1
               n1 = walls(2,L3a)
               n2 = walls(3,L3a)
               xwall = (xk(n1) + xk(n2)) * 0.5d0
               ywall = (yk(n1) + yk(n2)) * 0.5d0
               Deltxu = xwall - xu(L)
               Deltyu = ywall - yu(L)
               cs = - walls(8,L3a)
               sn =   walls(7,L3a)
               Amat(m,1) = cs
               Amat(m,2) = sn
               Amat(m,3) = cs * Deltxu
               Amat(m,4) = sn * Deltxu
               Amat(m,5) = cs * Deltyu
               Amat(m,6) = sn * Deltyu
               Amat(m,7) = cs * Deltxu * Deltyu
               Amat(m,8) = sn * Deltxu * Deltyu
               cof0 = sqrt( Deltxu**2 + Deltyu**2 )
               if (cof0 > 1d-10) then
                  Wmat(m) = 1d0 / cof0
               else
                  Wmat(m) = 1d0
               endif
               LwDone(L3a) = 1
            enddo
         enddo
            
         nmax = 8
         if (m < 8 .and. m >= 6) then
            nmax = 6
            print *, 'm < 8 .and. m >= 6', L
            ireconstu(L) = 1
            cycle
         elseif (m < 6) then
            print *, 'm < 6', m, L
            ireconstu(L) = 1
            cycle
         endif

         do j = 1,m
            do i = 1,nmax
               AtW(i,j) = Amat(j,i) * Wmat(j)
            enddo
         enddo
         do j = 1,nmax
            do i = 1,nmax
               AtWA(i,j) = 0d0
               do mm = 1,m
                  AtWA(i,j) = AtWA(i,j) + AtW(i,mm) * Amat(mm,j)
               enddo
            enddo
         enddo
         !
         ! === Lagrangian multiplier
         !
         nmax = nmax + 1
         AtWA(nmax,:) = 0d0
         AtWA(:,nmax) = 0d0
         AtWA(nmax,1) = csu(L)
         AtWA(nmax,2) = snu(L)
         AtWA(1,nmax) = csu(L)
         AtWA(2,nmax) = snu(L)
         m = m + 1
         AtW(nmax,:) = 0d0
         Atw(:,m)    = 0d0
         Atw(nmax,m) = 1d0
            
            
            !AtWAi = AtWA
            !svec  = 0d0
            !smat  = 0d0
            !call svdcmp( AtWAi, nmax, nmax, nmax, nmax, svec, smat )  ! here, AtWAi is not the inverse, AtWAi is used replacing of U
            !condnr = maxval(svec) / minval(svec)
            !!if (condnr > 1000d0) then
            !if( L==2152 ) then
            !   print * , 'Condition number is large', L, condnr
            !   pause
            !endif
            
            
            !Rmat = AtWA
            !cvec = 0.0d0
            !dvec = 0.0d0
            !call qrdcmp( Rmat, nmax, nmax, cvec, dvec, sing )
            !do i = 1,nmax
            !   do j = 1,nmax
            !      Qmatt(i,j) = 0.0d0
            !   enddo
            !   Qmatt(i,i) = 1.0d0
            !enddo
            !do k = 1,nmax-1
            !   if( cvec(k) .ne. 0.0d0 ) then
            !      do j = 1,nmax
            !         sum = 0.0d0
            !         do i = k,nmax
            !            sum = sum + Rmat(i,k) * Qmatt(i,j)
            !         enddo
            !         sum = sum / cvec(k)
            !         do i = k,nmax
            !            Qmatt(i,j) = Qmatt(i,j) - sum * Rmat(i,k)
            !         enddo
            !      enddo
            !   endif
            !enddo
            !do i = 1,nmax
            !   Rmat(i,i) = dvec(i)
            !   do j = 1,i-1
            !      Rmat(i,j) = 0.0d0
            !   enddo
            !enddo
            !Rmati = Rmat
            !svec = 0d0  ! dummy vector
            !call gaussj( Rmati, nmax, nmax, svec, 1, 1 )
            !do j = 1,nmax
            !   do i = 1,nmax
            !      RiQt(i,j) = 0.0d0
            !      do kk = 1,nmax
            !         RiQt(i,j) = RiQt(i,j) + Rmati(i,kk) * Qmatt(kk,j)
            !      enddo
            !   enddo
            !enddo
            !do j = 1,m
            !   do i = 1,nmax
            !      AtWAiAtW(i,j,k) = 0.0d0
            !      do kk = 1,nmax
            !         AtWAiAtW(i,j,k) = AtWAiAtW(i,j,k) + RiQt(i,kk) * AtW(kk,j)
            !      enddo
            !   enddo
            !enddo
            !
           
         AtWAi = AtWA
         svec = 0d0  ! dummy vector
         call gaussj1( AtWAi, nmax, nmax, svec, 1, 1, ierr )
         if( ierr /= 0 ) then
            ireconstu(L) = 1
            !print*,L,'singulare'
            !pause
            cycle
         endif
            
         do j = 1,m
            do i = 1,nmax
               AtWAiAtW(i,j,L) = 0d0
               do kk = 1,nmax
                  AtWAiAtW(i,j,L) = AtWAiAtW(i,j,L) + AtWAi(i,kk) * AtW(kk,j)
               enddo
            enddo
         enddo
      enddo

   end subroutine reconst2ndini
   
   ! ==============================================================================================
   ! ==============================================================================================
   subroutine reconst2nd ()
      use m_flow
      use m_flowgeom
      use m_netw
        
      implicit none
      integer                                :: i, j, k, k1, k2, L, L1, L2, L3, m, n, n1, nn
      integer                                :: L1a, L2a, L3a, L3b, L3t, k3, LL1, LL1a, nmax, LL, Lb, Lt, kk1, kk2, La
      double precision                       :: cof0, cofx, cofy, xwall, ywall, scale, cs, sn, cof1, cof2, uu, vv
      integer, parameter                     :: mmax = 64
      double precision, dimension(mmax)      :: bvec
      double precision, dimension(9)         :: xvec
      integer, dimension(lnx)                :: LDone
      integer, dimension(mxwalls)            :: LwDone
      double precision, dimension(:), allocatable :: uxu, uyu
        
      double precision :: x1, x2, x3, y1, y2, y3
      integer :: n2, L4
        
      if( .not. allocated( uxu ) ) then
         allocate( uxu(lnkx), uyu(lnkx) ) ;  uxu= 0d0 ; uyu = 0d0
      endif
      
      ucx = 0d0 ; ucy = 0d0
        
      if (kmx == 0) then  !2D
         do L = 1,lnxi
            if (hu(L) < epshu) then
               v(L) = 0d0
               cycle
            endif
            
            k1 = ln(1,L) ; k2 = ln(2,L)
            if (ireconstu(L) == 1) then
               call perotnode2d (k1)
               call perotnode2d (k2)
               v(L) = (1d0-acl(L))*(-snu(L)*ucx(k1)+csu(L)*ucy(k1))+acl(L)*(-snu(L)*ucx(k2)+csu(L)*ucy(k2))
               ireconstz(k1) = 1
               ireconstz(k2) = 1
               cycle
            endif
            m = 0
            bvec   = 0d0
            LDone  = 0
            LwDone = 0
            
            do L1 = 1,nd(k1)%nwx
               L1a = abs(nd(k1)%nw(L1))
               if (LwDone(L1a) == 1) cycle
               m = m + 1
               bvec(m) = 0d0
               LwDone(L1a) = 1
            enddo
            
            do L2 = 1,nd(k2)%nwx
               L2a = abs(nd(k2)%nw(L2))
               if (LwDone(L2a) == 1) cycle
               m = m + 1
               bvec(m) = 0d0
               LwDone(L2a) = 1
            enddo

            do L1 = 1,nd(k1)%lnx
               L1a = abs(nd(k1)%ln(L1))
               if (L1a == L) cycle
               k3 = ln(1,L1a) + ln(2,L1a) - k1
               do L3 = 1,nd(k3)%lnx
                  L3a = abs(nd(k3)%ln(L3))
                  if (LDone(L3a) == 1) cycle 
                  m = m + 1
                  bvec(m) = u1(L3a)
                  LDone(L3a) = 1
               enddo
               do L3 = 1,nd(k3)%nwx
                  L3a = abs(nd(k3)%nw(L3))
                  if (LwDone(L3a) == 1) cycle
                  m = m + 1
                  bvec(m) = 0d0
                  LwDone(L3a) = 1
               enddo
            enddo

            do L2 = 1,nd(k2)%lnx
               L2a = abs(nd(k2)%ln(L2))
               if (L2a == L) cycle
               k3 = ln(1,L2a) + ln(2,L2a) - k2
               do L3 = 1,nd(k3)%lnx
                  L3a = abs(nd(k3)%ln(L3))
                  if (LDone(L3a) == 1) cycle 
                  m = m + 1
                  bvec(m) = u1(L3a)
                  LDone(L3a) = 1
               enddo
               do L3 = 1,nd(k3)%nwx
                  L3a = abs(nd(k3)%nw(L3))
                  if (LwDone(L3a) == 1) cycle
                  m = m + 1
                  bvec(m) = 0d0
                  LwDone(L3a) = 1
               enddo
            enddo

            nmax = 8
            if (m < 8 .and. m >= 6) then
               nmax = 6
            endif
            !
            ! === Lagrangian multiplier
            !
            nmax = nmax + 1
            m = m + 1
            bvec(m) = u1(L)
            
            do i = 1,2 !nmax
               xvec(i) = 0d0
               do j = 1,m
                  xvec(i) = xvec(i) + AtWAiAtW(i,j,L) * bvec(j)
               enddo
            enddo
            uxu(L) = xvec(1)
            uyu(L) = xvec(2)
            !dudxL = xvec(3)
            !dvdxL = xvec(4)
            !dudyL = xvec(5)
            !dvdyL = xvec(6)
            v(L) = - snu(L) * uxu(L) + csu(L) * uyu(L)
         enddo
         do L = lnxi+1,lnx
            uxu(L) = csu(L) * u1(L)
            uyu(L) = snu(L) * u1(L)
         enddo  
  
      else   ! 3D =================================
         
         do L = 1,lnxi
            call getLbotLtop (L, Lb, Lt)
            k1 = ln(1,L) ; k2 = ln(2,L)
            do LL = Lb,Lt
               if (ireconstu(L) == 1) then
                  kk1 = ln(1,LL) ; kk2 = ln(2,LL)
                  call perotnode3d( k1, kk1 )
                  call perotnode3d( k2, kk2 )
                  v(LL) = (1d0-acl(L))*(-snu(L)*ucx(kk1)+csu(L)*ucy(kk1))+acl(L)*(-snu(L)*ucx(kk2)+csu(L)*ucy(kk2))
                  ireconstz(k1) = 1
                  ireconstz(k2) = 1
                  cycle
               endif
               m = 0
               bvec   = 0d0
               LDone  = 0
               LwDone = 0
               
               do L1 = 1,nd(k1)%nwx
                  L1a = abs(nd(k1)%nw(L1))
                  if (LwDone(L1a) == 1) cycle
                  m = m + 1
                  bvec(m) = 0d0
                  LwDone(L1a) = 1
               enddo

               do L2 = 1,nd(k2)%nwx
                  L2a = abs(nd(k2)%nw(L2))
                  if (LwDone(L2a) == 1) cycle
                  m = m + 1
                  bvec(m) = 0d0
                  LwDone(L2a) = 1
               enddo

               do L1 = 1,nd(k1)%lnx
                  L1a = abs(nd(k1)%ln(L1))
                  if (L1a == L) cycle
                  k3 = ln(1,L1a) + ln(2,L1a) - k1
                  do L3 = 1,nd(k3)%lnx
                     L3a = abs(nd(k3)%ln(L3))
                     if (LDone(L3a) == 1) cycle 
                     m = m + 1
                     call getLbotLtop (L3a, L3b, L3t)
                     bvec(m) = u1(L3b+LL-Lb)
                     LDone(L3a) = 1
                  enddo
                  do L3 = 1,nd(k3)%nwx
                     L3a = abs(nd(k3)%nw(L3))
                     if (LwDone(L3a) == 1) cycle
                     m = m + 1
                     bvec(m) = 0d0
                     LwDone(L3a) = 1
                  enddo
               enddo

               do L2 = 1,nd(k2)%lnx
                  L2a = abs(nd(k2)%ln(L2))
                  if (L2a == L) cycle
                  k3 = ln(1,L2a) + ln(2,L2a) - k2
                  do L3 = 1,nd(k3)%lnx
                     L3a = abs(nd(k3)%ln(L3))
                     if (LDone(L3a) == 1) cycle 
                     m = m + 1
                     call getLbotLtop (L3a, L3b, L3t)
                     bvec(m) = u1(L3b+LL-Lb)
                     LDone(L3a) = 1
                  enddo
                  do L3 = 1,nd(k3)%nwx
                     L3a = abs(nd(k3)%nw(L3))
                     if (LwDone(L3a) == 1) cycle
                     m = m + 1
                     bvec(m) = 0d0
                     LwDone(L3a) = 1
                  enddo
               enddo

               nmax = 8
               if (m < 8 .and. m >= 6) then
                  nmax = 6
               endif
               !
               ! === Lagrangian multiplier
               !
               nmax = nmax + 1
               m = m + 1
               bvec(m) = u1(LL)
            
               do i = 1,2 !nmax
                  xvec(i) = 0d0
                  do j = 1,m
                     xvec(i) = xvec(i) + AtWAiAtW(i,j,L) * bvec(j)
                  enddo
               enddo
               uxu(LL) = xvec(1)
               uyu(LL) = xvec(2)
               !dudxL = xvec(3)
               !dvdxL = xvec(4)
               !dudyL = xvec(5)
               !dvdyL = xvec(6)
               v(LL) = - snu(L) * uxu(LL) + csu(L) * uyu(LL)
            enddo
         enddo
         do L = lnxi+1,lnx
            call getLbotLtop (L, Lb, Lt)
            do LL = Lb,Lt
               uxu(LL) = csu(L) * u1(LL)
               uyu(LL) = snu(L) * u1(LL)
            enddo
         enddo  
      endif
      
      ucx = 0d0
      ucy = 0d0
      call linkstocenterstwodoubles(ucx, uxu)
      call linkstocenterstwodoubles(ucy, uyu)
      
      
      do L = 1,lnxi
         if( hu(L) < epshu ) then
            v(L) = 0d0
            cycle
         endif
         k1 = ln(1,L) ; k2 = ln(2,L)
         if( ireconstu(L) == 1 ) then
            call perotnode2d( k1 )
            call perotnode2d( k2 )
            v(L) = acl(L)*(-snu(L)*ucx(k1)+csu(L)*ucy(k1))+(1d0-acl(L))*(-snu(L)*ucx(k2)+csu(L)*ucy(k2))
            ireconstz(k1) = 1
            ireconstz(k2) = 1
            cycle
         endif
      enddo

      !if (kmx == 0) then
      !   do L = 1,lnx
      !      if( ireconstu(L) == 1 ) cycle
      !      k1 = ln(1,L) ; k2 = ln(2,L)
      !      if( ireconstz(k1) == 0 ) then
      !         ucx(k1) = ucx(k1) + uxu(L)*wcL(1,L)
      !         ucy(k1) = ucy(k1) + uyu(L)*wcL(1,L)
      !      endif
      !      if( ireconstz(k2) == 0 ) then
      !         ucx(k2) = ucx(k2) + uxu(L)*wcL(2,L)
      !         ucy(k2) = ucy(k2) + uyu(L)*wcL(2,L)
      !      endif
      !   enddo
      !else  !3D
      !   ucx = 0d0 ; ucy = 0d0
      !   do LL = 1,lnx
      !      if( ireconstu(LL) == 1 ) cycle
      !      call getLbotLtop(LL,Lb,Lt)
      !      n1 = ln(1,LL) ; n2 = ln(2,LL)
      !      do L = Lb,Lt
      !         k1 = ln(1,L) ; k2 = ln(2,L)
      !         if( ireconstz(n1) == 0 ) then
      !            ucx(k1) = ucx(k1) + uxu(L)*wcL(1,LL)
      !            ucy(k1) = ucy(k1) + uyu(L)*wcL(1,LL)
      !         endif
      !         if( ireconstz(n2) == 0 ) then
      !            ucx(k2) = ucx(k2) + uxu(L)*wcL(2,LL)
      !            ucy(k2) = ucy(k2) + uyu(L)*wcL(2,LL)
      !         endif
      !      enddo
      !   enddo
      !endif
      
   
      
      !!!do L = 1,lnx
      !!!   if( ireconstu(L) == 1 ) cycle
      !!!   k1 = ln(1,L) ; k2 = ln(2,L)
      !!!   if( ireconstz(k1)  == 0 ) then
      !!!      cof0 =  acl(L) * dx(L)
      !!!      ucx(k1) = ucx(k1) + ( 1d0 - wu(L) * cof0  / ba(k1) ) * uxu(L)
      !!!      ucy(k1) = ucy(k1) + ( 1d0 - wu(L) * cof0  / ba(k1) ) * uyu(L)
      !!!   endif
      !!!   if( ireconstz(k2) == 0 ) then 
      !!!      cof0 = (1d0 - acl(L)) * dx(L)
      !!!      ucx(k2) = ucx(k2) + ( 1d0 - wu(L) * cof0  / ba(k2) ) * uxu(L)
      !!!      ucy(k2) = ucy(k2) + ( 1d0 - wu(L) * cof0  / ba(k2) ) * uyu(L)
      !!!   endif
      !!!enddo
      !!!do L = 1,mxwalls
      !!!   k1 = walls(1,L)
      !!!   if( ireconstz(k1) == 1 ) then
      !!!      continue
      !!!      cycle
      !!!   endif
      !!!   cs = - walls(8,L)
      !!!   sn =   walls(7,L)
      !!!   cof0 = 2d0 * walls(12,L)
      !!!   cof1 = 1d0 - walls(9,L) * cof0  / ba(k1)
      !!!   cof2 = ( 1d0 - cof1 * sn**2 ) * ( 1d0 - cof1 * cs**2 ) - cof1**2 * sn**2 * cs**2
      !!!   uu = ucx(k1)
      !!!   vv = ucy(k1)
      !!!   ucx(k1) = uu * ( 1d0 - cof1 * cs**2 ) - vv * cof1 * sn * cs
      !!!   ucy(k1) = vv * ( 1d0 - cof1 * sn**2 ) - uu * cof1 * sn * cs 
      !!!enddo
      
      
      
      
        do k = 1,ndxi
        !    if( size( nd(k)%nod ) == 4 ) then
        !       if ( nd(k)%lnx == size( nd(k)%nod ) ) then
        !       L1 = abs( nd(k)%ln(1) )
        !       L2 = abs( nd(k)%ln(2) )
        !       L3 = abs( nd(k)%ln(3) )
        !       L4 = abs( nd(k)%ln(4) )
        !       ucx(k) = ( uxu(L1) + uxu(L2) + uxu(L3) + uxu(L4) ) * 0.25d0 
        !       ucy(k) = ( uyu(L1) + uyu(L2) + uyu(L3) + uyu(L4) ) * 0.25d0 
        !        !print *,'A nontriangular cell', k
        !        !pause
        !        !cycle
        !       endif
        !    endif
            
            !!!    L1 = abs( nd(k)%ln(1) )
            !!!    L2 = abs( nd(k)%ln(2) )
            !!!    L3 = abs( nd(k)%ln(3) )
               !!!if ( size( nd(k)%nod ) == 3 ) then
               !!!if( ireconstz(k) == 1 ) cycle
               !!!ucx(k) = 0d0
               !!!ucy(k) = 0d0
               !!!do LL = 1,nd(k)%lnx
               !!!   L = nd(k)%ln(LL)
               !!!   La = abs(L)
               !!!   if( L > 0 ) then
               !!!      cof0 =  acl(La) * dx(La)
               !!!   else
               !!!      cof0 = (1d0 - acl(La)) * dx(La)
               !!!   endif
               !!!   ucx(k) = ucx(k) + ( 1d0 - wu(La) * cof0  / ba(k) ) * uxu(La)
               !!!   ucy(k) = ucy(k) + ( 1d0 - wu(La) * cof0  / ba(k) ) * uyu(La)
               !!!enddo
               !!!endif
            !!!    x1 = xu(L1) - xz(k) ; y1 = yu(L1) - yz(k)
            !!!    x2 = xu(L2) - xz(k) ; y2 = yu(L2) - yz(k)
            !!!    x3 = xu(L3) - xz(k) ; y3 = yu(L3) - yz(k)
            !!!    cof0 = x1 * ( y2 - y3 ) + x2 * ( y3 - y1 ) + x3 * ( y1 - y2 )
            !!!    ucx(k) = uxu(L1) * ( x2 * y3 - x3 * y2 ) + uxu(L2) * ( x3 * y1 - x1 * y3 ) + uxu(L3) * ( x1 * y2 - x2 * y1 )
            !!!    ucy(k) = uyu(L1) * ( x2 * y3 - x3 * y2 ) + uyu(L2) * ( x3 * y1 - x1 * y3 ) + uyu(L3) * ( x1 * y2 - x2 * y1 )
            !!!    ucx(k) = ucx(k) / cof0
            !!!    ucy(k) = ucy(k) / cof0
        !    else
        !        !L1 = abs( nd(k)%ln(1) )
        !        !L2 = abs( nd(k)%ln(2) )
        !        !x1 = xu(L1) - xz(k) ; y1 = yu(L1) - yz(k)
        !        !x2 = xu(L2) - xz(k) ; y2 = yu(L2) - yz(k)
        !        !do nn = 1,size(nd(k)%nod)
        !        !    n = nd(k)%nod(nn)
        !        !    do LL1 = 1,cn(n)%nwx
        !        !        L3 = abs(cn(n)%nw(LL1))
        !        !        n2 = walls(2,L3) + walls(3,L3) - n
        !        !        x3 = ( xk(n) + xk(n2) ) * 0.5d0 - xz(k)
        !        !        y3 = ( yk(n) + yk(n2) ) * 0.5d0 - yz(k)
        !        !    enddo
        !        !enddo
        !        !cof0 = x1 * ( y2 - y3 ) + x2 * ( y3 - y1 ) + x3 * ( y1 - y2 )
        !        !ucx(k) = uxu(L1) * ( x2 * y3 - x3 * y2 ) + uxu(L2) * ( x3 * y1 - x1 * y3 )
        !        !ucy(k) = uyu(L1) * ( x2 * y3 - x3 * y2 ) + uyu(L2) * ( x3 * y1 - x1 * y3 )
        !        !ucx(k) = ucx(k) / cof0
        !        !ucy(k) = ucy(k) / cof0

        !    
        enddo
        ucxq = ucx
        ucyq = ucy
 
   end subroutine reconst2nd
   
! =================================================================================================
! =================================================================================================
   subroutine perotnode2d( k )
      use m_flow
      use m_flowgeom
        
      implicit none
      integer       :: k, L, L1, La

      ucx(k) = 0d0
      ucy(k) = 0d0
      do L1 = 1,nd(k)%lnx
         L = nd(k)%ln(L1) ; La = abs( L )
         if (L < 0) then
            ucx(k) = ucx(k) + wcx1(La) * u1(La)
            ucy(k) = ucy(k) + wcy1(La) * u1(La)
         else
            ucx(k) = ucx(k) + wcx2(La) * u1(La)
            ucy(k) = ucy(k) + wcy2(La) * u1(La)
         endif
      enddo

   end subroutine perotnode2d

   ! ==============================================================================================
   ! ==============================================================================================
   subroutine perotnode3d (kk, k)
      use m_flow
      use m_flowgeom
        
      implicit none
      integer, intent(in) :: kk, k
      integer             :: kb, kt, L, L1, La, Lb, Lt, LL

      call getkbotktop (kk, kb, kt)
      ucx(k) = 0d0
      ucy(k) = 0d0
      do L1 = 1,nd(kk)%lnx
         L = nd(kk)%ln(L1)
         La = abs(L)
         call getLbotLtop (La, Lb, Lt)
         LL = Lb + k - kb
         if (L < 0) then
            ucx(k) = ucx(k) + wcx1(La) * u1(LL)
            ucy(k) = ucy(k) + wcy1(La) * u1(LL)
         else
            ucx(k) = ucx(k) + wcx2(La) * u1(LL)
            ucy(k) = ucy(k) + wcy2(La) * u1(LL)
         endif
      enddo

   end subroutine perotnode3d
   
   ! ==============================================================================================
   ! ==============================================================================================
   subroutine gaussj1 (a, n, np, b, m, mp, ierr)
   
      implicit none
      integer                            :: m, mp, n, np, ierr
      integer                            :: i, icol, irow, j, k, l, ll 
      integer, dimension(np)             :: indxc, indxr, ipiv
      double precision                   :: big, dum, pivinv
      double precision, dimension(np,np) :: a
      double precision, dimension(np,mp) :: b

      ierr = 0
      do j = 1,n
         ipiv(j) = 0
      enddo
      do i = 1,n
         big = 0d0
         do j = 1,n
            if (ipiv(j) /= 1) then
               do k = 1,n
                  if (ipiv(k) == 0) then
                     if (abs(a(j,k)) >= big ) then
                        big = abs( a(j,k) )
                        irow = j
                        icol = k
                     endif
                  elseif( ipiv(k) > 1 ) then
                     print*, 'singular matrix in gaussj'
                     ierr = 1
                     return
                  endif
               enddo
            endif
         enddo
         ipiv(icol) = ipiv(icol) + 1
         if( irow /= icol ) then
            do l = 1,n
               dum = a(irow,l)
               a(irow,l) = a(icol,l)
               a(icol,l) = dum
            enddo
            do l = 1,m
               dum = b(irow,l)
               b(irow,l) = b(icol,l)
               b(icol,l) = dum
            enddo
         endif
         indxr(i) = irow
         indxc(i) = icol
         if ( abs( a(icol,icol) ) < 1d-10 ) then
            print*, 'singular matrix in gaussj'
            ierr = 1
            return
         endif
         pivinv = 1d0 / a(icol,icol)
         a(icol,icol) = 1d0
         do l = 1,n
            a(icol,l) = a(icol,l) * pivinv
         enddo
         do l=1,m
            b(icol,l) = b(icol,l) * pivinv
         enddo
         do ll = 1,n
            if (ll /= icol) then
               dum = a(ll,icol)
               a(ll,icol) = 0d0
               do l = 1,n
                  a(ll,l) = a(ll,l) - a(icol,l) * dum
               enddo
               do l=1,m
                  b(ll,l) = b(ll,l) - b(icol,l) * dum
               enddo
            endif
         enddo
      enddo
      do l = n,1,-1
         if (indxr(l) /= indxc(l)) then
            do k = 1,n
               dum = a(k,indxr(l))
               a(k,indxr(l)) = a(k,indxc(l))
               a(k,indxc(l)) = dum
            enddo
         endif
      enddo
        
   end subroutine gaussj1

   ! ==============================================================================================
   ! ==============================================================================================
   subroutine qrdcmp (a, n, np, c, d, sing)
 
      implicit none
      integer                            :: n,np
      integer                            :: i, j, k
      double precision                   :: scale,sigma,sum,tau
      double precision, dimension(np,np) :: a
      double precision, dimension(n)     :: c, d
      logical sing

      sing = .false.
      do k = 1,n-1
         scale = 0d0
         do i = k,n
            scale = max( scale, abs( a(i,k) ) )
         enddo
         if (scale == 0d0) then
            sing = .true.
            c(k) = 0d0
            d(k) = 0d0
         else
            do i = k,n
               a(i,k) = a(i,k) / scale
            enddo
            sum = 0d0
            do i = k,n
               sum = sum + a(i,k)**2
            enddo
            sigma = sign( sqrt(sum), a(k,k) )
            a(k,k) = a(k,k) + sigma
            c(k) = sigma * a(k,k)
            d(k) = - scale * sigma
            do j = k+1,n
               sum = 0d0
               do i=k,n
                  sum = sum + a(i,k) * a(i,j)
               enddo
               tau = sum / c(k)
               do i = k,n
                  a(i,j) = a(i,j) - tau * a(i,k)
               enddo
            enddo
         endif
      enddo
      d(n) = a(n,n)
      if (d(n) == 0d0) sing = .true.

   end subroutine qrdcmp
 
   
end module m_setucxcuy_leastsquare