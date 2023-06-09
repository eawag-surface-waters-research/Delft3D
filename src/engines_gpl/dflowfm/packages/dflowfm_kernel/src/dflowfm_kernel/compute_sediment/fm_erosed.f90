!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

   subroutine fm_erosed()
   !!--description-----------------------------------------------------------------
   !!
   !!    Function: Computes sediment fluxes at the bed using
   !!              the Partheniades-Krone formulations.
   !!              Arrays SOURSE and SINKSE are filled and added
   !!              to arrays SOUR and SINK
   !!              Computes bed load transport for sand sediment
   !!              Arrays SBUU and SBVV are filled.
   !!              Computes vertical sediment diffusion coefficient
   !!              Array SEDDIF is filled
   !!              Includes wave asymmetry effects on sand bed-load
   !!              transport
   !!              Bed slope effects computed at the U and V velocity
   !!              points
   !! Method used: Attention: pointer ll for 'standard' FLOW
   !!              arrays is shifted with lstart
   !!
   !!
   !!--pseudo code and references--------------------------------------------------
   ! NONE
   !!--declarations----------------------------------------------------------------
   use precision
   use mathconsts, only: pi
   use bedcomposition_module
   use morphology_data_module
   use sediment_basics_module
   use m_physcoef, only: ag, vonkar, sag, ee, backgroundsalinity, backgroundwatertemperature, vismol
   use m_sediment, only: stmpar, sedtra, stm_included, mtd, jatranspvel, sbcx_raw,sbcy_raw,sswx_raw,sswy_raw,sbwx_raw,sbwy_raw
   use m_flowgeom, only: bl, lnxi, lnx, ln, dxi, ndx, csu, snu, wcx1, wcx2, wcy1, wcy2, acl, nd, csu, snu, wcl, wu_mor
   use m_flow, only: s0, s1, u1, kmx, zws, hs, &
      iturbulencemodel, z0urou, ifrcutp, hu, spirint, spiratx, spiraty, u_to_umain, qa, frcu_mor, javeg,jabaptist,cfuhi, epshu, taubxu, epsz0
   use m_flowtimes, only: julrefdat, dts, time1
   use unstruc_files, only: mdia
   use unstruc_channel_flow, only: network, t_branch, t_node, nt_LinkNode
   use message_module, only: write_error
   use MessageHandling, only: LEVEL_INFO, LEVEL_FATAL, mess, setmessage
   use m_transport, only: ised1, constituents, isalt, itemp
   use dfparall
   use m_alloc
   use m_missing
   use m_physcoef, only: frcuni, ifrctypuni
   use m_turbulence, only: vicwws, turkinepsws, rhowat
   use m_flowparameters, only: jasal, jatem, jawave, epshs, jasecflow, jasourcesink, v2dwbl, flowWithoutWaves
   use m_fm_erosed
   use m_bedform
   use m_xbeach_data
   use m_waves
   use m_xbeach_paramsconst
   use m_tables, only: interpolate
   use m_partitioninfo
   use compbsskin_module, only: compbsskin, get_alpha_fluff
   !
   implicit none
   !
   real(fp)                                       :: eps = 1.0e-6_fp
   logical                                        :: scour = .false.
   logical                                        :: ubot_from_com != .true. !! promoted approach, so only option in FM
   logical                                        :: flmd2l = .false.
   logical                                        :: wave

   integer                              , pointer :: iunderlyr
   real(prec)       , dimension(:,:)    , pointer :: bodsed
   type(t_nodefraction)                 , pointer :: pFrac
   type(t_noderelation)                 , pointer :: pNodRel
   type(t_node)                         , pointer :: pnod

   !
   ! Local parameters
   !
   integer, parameter :: kmax2d = 20
   !
   ! Global variables
   !
   integer                                        :: ltur
   !
   !
   ! Local variables
   !
   integer                       :: i
   integer                       :: iFrac
   integer                       :: inod
   integer                       :: istat
   integer                       :: ised
   integer                       :: j
   integer                       :: k
   integer                       :: k2d
   integer                       :: kbed
   integer                       :: kmaxsd
   integer                       :: l
   integer                       :: ll
   integer                       :: lstart
   integer                       :: nm
   logical                       :: error
   integer                       :: klc
   integer                       :: kmaxlc
   integer                       :: k1, k2, k3
   logical                       :: suspfrac  ! includes suspended transport via advection-diffusion equation
   logical                       :: javegczu
   real(fp)                      :: afluff
   real(fp)                      :: aks_ss3d
   real(fp)                      :: caks
   real(fp)                      :: caks_ss3d
   real(fp)                      :: chezy
   real(fp)                      :: conc2d
   real(fp)                      :: delr
   real(fp)                      :: di50
   real(fp)                      :: difbot
   real(fp)                      :: drho
   real(fp)                      :: dtmor
   real(fp)                      :: fracf
   real(fp)                      :: maxslope
   real(fp)                      :: grkg
   real(fp)                      :: grm2
   real(fp)                      :: grlyrs
   real(fp)                      :: h0
   real(fp)                      :: h1
   real(fp)                      :: rc
   real(fp)                      :: mfltot
   real(fp)                      :: salinity
   real(fp)                      :: sinktot
   real(fp)                      :: sourfluff
   real(fp)                      :: spirintnm   ! local variable for spiral flow intensity
   real(fp)                      :: taks
   real(fp)                      :: taks0
   real(fp)                      :: tauadd
   real(fp)                      :: tdss      ! temporary variable for dss
   real(fp)                      :: temperature
   real(fp), dimension(max(kmx,1))      :: thicklc
   real(fp), dimension(max(kmx,1))      :: siglc
   real(fp)                      :: thick0
   real(fp)                      :: thick1
   real(fp)                      :: trsedeq   ! temporary variable for rsedeq
   real(fp)                      :: tsd
   real(fp)                      :: tsigmol   ! temporary variable for sigmol
   real(fp)                      :: twsk
   real(fp)                      :: ulocal
   real(fp)                      :: ubed
   real(fp)                      :: umean
   real(fp)                      :: ustarc
   real(fp)                      :: utot
   real(fp)                      :: vbed
   real(fp)                      :: velb
   real(fp)                      :: velm
   real(fp)                      :: vlocal
   real(fp)                      :: vmean
   real(fp)                      :: z0rou
   real(fp)                      :: zvelb
   real(fp)                      :: poros
   real(fp)                      :: wstau                 ! dummy for erosilt
   real(fp), dimension(:), allocatable :: evel            ! erosion velocity [m/s]
   real(fp), dimension(0:kmax2d) :: dcww2d
   real(fp), dimension(0:kmax2d) :: sddf2d
   real(fp), dimension(0:kmax2d) :: ws2d
   real(fp), dimension(kmax2d)   :: rsdq2d
   real(fp), dimension(kmax2d), save :: sig2d = &
      (/ -0.0874, -0.2472, -0.3797, -0.4897, -0.5809, -0.6565, -0.7193, &
      & -0.7713, -0.8145, -0.8503, -0.8800, -0.9046, -0.9250, -0.9419, -0.9560,&
      & -0.9676, -0.9773, -0.9854, -0.9920, -0.9975 /)

   real(fp), dimension(kmax2d), save :: thck2d = &
      (/ 0.1747, 0.1449, 0.1202, 0.0997, 0.0827, 0.0686, 0.0569, 0.0472, &
      & 0.0391, 0.0325, 0.0269, 0.0223, 0.0185, 0.0154, 0.0127, 0.0106, 0.0088,&
      & 0.0073, 0.0060, 0.0050 /)

   real(fp), dimension(max(kmx,1))     :: concin3d
   real(fp), dimension(kmax2d)         :: concin2d
   character(256)                      :: errmsg
   double precision                    :: zcc, maxdepfrac
   double precision                    :: ubot
   integer                             :: ierr, kk, Lf, kmxvel, kb, kt
   integer                             :: Ldir
   double precision, allocatable       :: dzdx(:), dzdy(:), u1_tmp(:), ucxq_tmp(:), ucyq_tmp(:)
   double precision, allocatable       :: z0rouk(:), z0curk(:), deltas(:), ua(:), va(:)
   double precision                    :: dzdn, dzds
   double precision                    :: z0u, czu
   double precision                    :: facCheck
   integer                             :: nrd_idx
   double precision                    :: expQ
   double precision                    :: expW
   double precision                    :: facQ
   double precision                    :: facW
   double precision                    :: qb1d, wb1d, sb1d
   double precision                    :: sbrratio, qbrratio, Qbr1, Qbr2
   !
   real(fp), dimension(:), allocatable :: localpar        !< local array for sediment transport parameters
   real(fp), dimension(:), allocatable :: qb_out          !< sum of outgoing discharge at 1d node
   real(fp), dimension(:), allocatable :: width_out       !< sum of outgoing main channel widths
   real(fp), dimension(:,:), allocatable :: sb_in         !< sum of incoming sediment transport at 1d node
   integer, dimension(:,:,:), allocatable :: sb_dir       !< direction of transport at node (nnod, lsedtot, nbr) (-1 = incoming or no transport, +1 = outgoing)
   integer, dimension(:), allocatable :: branInIDLn       !< ID of Incoming Branch (If there is only one) (nnod)
   !
   !! executable statements -------------------------------------------------------
   !
   !   exit the routine immediately if sediment transport (and morphology) is not included in the simulation
   !
   error = .false.
   if (.not.stm_included) return
   ubot_from_com = jauorbfromswan>0
   !
   ! Allocate memory
   allocate(dzdx(1:ndx), dzdy(1:ndx), stat=istat)
   if (istat == 0) allocate(localpar (npar), stat = istat)
   if (istat == 0) allocate(ua(1:ndx), va(1:ndx), stat=istat)
   if (istat == 0) allocate(z0rouk(1:ndx), z0curk(1:ndx), deltas(1:ndx), stat=istat)
   if (istat == 0) allocate(qb_out(network%nds%Count), stat = istat)
   if (istat == 0) allocate(width_out(network%nds%Count), stat = istat)
   if (istat == 0) allocate(sb_in(network%nds%Count, lsedtot), stat = istat)
   if (istat == 0) allocate(sb_dir(network%nds%Count, lsedtot, network%nds%maxnumberofconnections), stat = istat)
   if (istat == 0) allocate(branInIDLn(network%nds%Count), stat = istat)

   localpar = 0.0_fp
   ua = 0d0; va = 0d0; z0rouk = 0d0; z0curk=0d0
   qb_out = 0d0; width_out = 0d0; sb_in = 0d0; sb_dir = -1
   BranInIDLn = 0

   if ((istat == 0) .and. (.not. allocated(u1_tmp))) allocate(u1_tmp(1:lnx), ucxq_tmp(1:ndx), ucyq_tmp(1:ndx), stat=ierr)

   if (istat /= 0) then
      error = .true.
      write(errmsg,'(a)') 'fm_erosed::error allocating memory.'
      call mess(LEVEL_FATAL, errmsg)
   endif
   !
   wave = (jawave>0) .and. .not. flowWithoutWaves
   !
   ! Mass conservation; s1 is updated before entering fm_erosed
   !
   if (varyingmorfac) then
      call updmorfac(stmpar%morpar, time1/3600.0_fp, julrefdat)
   endif
   !
   ! Reset some arrays before next iteration
   spirintnm = 0.0_fp
   ust2 = 0.0_fp
   !
   ! Use Eulerian velocities if jatranspvel > 0
   !
   u1_tmp = u1 * u_to_umain

   if (jatranspvel > 0 .and. jawave > 0 .and. .not. flowWithoutWaves) then
      u1_tmp = u1 - ustokes
      call setucxucy_mor (u1_tmp)
   else
   !   Calculate cell centre velocities ucxq, ucyq
      if (maxval(u_to_umain) /= 1d0 .or. minval(u_to_umain) /= 1d0) then
         call setucxucy_mor (u1_tmp)
      endif
   endif
   ucxq_tmp = ucxq_mor
   ucyq_tmp = ucyq_mor
   call init_1dinfo()
   call setucxqucyq_mor(u1_tmp, ucxq_tmp, ucyq_tmp)
   !
   if (jawave>2) then
      if ((.not. (jawave==4 .or. jawave==3 .or. jawave==6)) .or. flowWithoutWaves) then
         ktb=0d0     ! no roller turbulence
      else
         do k=1, ndx
            call rollerturbulence(k)  ! sets ktb values
         end do
      end if
   endif
   !
   ! Determine total thickness of the mud layers
   ! to be used in computation of skin friction
   ! (Soulsby&Clarke 2004, EstProc report TR137)
   ! will be used in compbsskin.f90
   !
   if (bsskin) then
      call detthcmud(stmpar%morlyr, thcmud)
   endif
   !
   ! Initialisation:
   ! reset sediment sources and sinks
   !     set default kmxsed layer
   !     set kfsed
   !
   lstart = ised1 - 1
   !
   ! Reset Sourse and Sinkse arrays for all (l,nm)
   !
   do k = 1, ndx
      call getkbotktop(k, kb, kt)
      kmxsed(k,:)  = kb
   end do
   sinkse  = 0.0_fp
   sourse  = 0.0_fp
   sour_im = 0.0_fp
   ! source and sink terms fluff layer
   if (iflufflyr>0) then
      sinkf = 0.0_fp
      sourf = 0.0_fp
   endif
   !
   ! Reset Sediment diffusion arrays for (l,nmk)
   !
   seddif  = 0.0_fp
   rca     = 0.0_fp
   !
   ! Reset Bed Shear Ratio for all nm and l = 1:lsedtot
   !
   taurat = 0.0_fp
   !
   ! Set zero bedload transport for all nm and l = 1:lsedtot
   !
   sbcx   = 0.0_fp
   sbcy   = 0.0_fp
   e_sbcn = 0.0_fp
   e_sbct = 0.0_fp
   sbwx   = 0.0_fp
   sbwy   = 0.0_fp
   e_sbwn = 0.0_fp
   e_sbwt = 0.0_fp
   sswx   = 0.0_fp
   sswy   = 0.0_fp
   e_sswn = 0.0_fp
   e_sswt = 0.0_fp
   sxtot  = 0.0_fp
   sytot  = 0.0_fp
   rsedeq = 0.0_fp

   ! Set ltur
   ltur = 0
   if (kmx>0) then
      select case (iturbulencemodel)
         case (0,1,2)
            ltur = 0
         case (3,4)
            ltur = 2
      end select
   end if

   do nm = 1, ndx
      if ((s1(nm) - bl(nm)) > sedthr) then
         kfsed(nm) = 1
      else
         kfsed(nm) = 0
      endif
   enddo
   !
   ! Determine fractions of all sediments the top layer and
   ! compute the mud fraction.
   !
   if (lsedtot > 1) then
      call getfrac(stmpar%morlyr,frac      ,anymud    ,mudcnt    , &
         & mudfrac      ,1         ,ndx)
   endif

   ! 3D:
   ! Calculate cell centre velocity components and magnitude
   ! based on velocity in the bottom computational layer
   ! Note: uses downwind velocity at any internal point,
   ! uses internal velocity at any open boundary, uses
   ! half of internal velocity in direction of any
   ! closed boundary or dry point.
   !
   javegczu = (javeg==1 .and. jabaptist>1 .and. kmx==0 )
   !
   do k = 1,ndx                            ! This interpolation is done by considering constant waterdepth per each flow-cell
      h1 = s1(k) - bl(k)                   ! To ensure to get the same results from interpolation based on constant frcu and ifrcutp in the cell centre
                                           ! with considering hs
      if (nd(k)%lnx==0) then
         z0curk(k) = 1d-5                  ! safety if nd(k)%lnx==0. Happens sometimes in case of thin dams
         cycle
      endif

      do LL = 1,nd(k)%lnx
         Lf = nd(k)%ln(LL)
         L = abs( Lf )
         if (javegczu) then 
            if (cfuhi(L)>0d0) then         ! use bed contribution of baptist>1
               czu = 1d0/(cfuhi(L)*max(hu(L),epshu))
               czu = sqrt(czu*ag)
            else
               call getcz(hu(L), frcuni, ifrctypuni, czu, L)
            endif
         else
            if (frcu_mor(L)>0) then
               call getcz(hu(L), frcu_mor(L), ifrcutp(L), czu, L)
            else
               call getcz(hu(L), frcuni, ifrctypuni, czu, L)
            end if
         endif
         !
         z0u = hu(L)*exp(-vonkar*czu/sag - 1d0)         ! differs from delft3d
         if( Lf < 0 ) then
            z0curk(k) = z0curk(k)+wcl(1,L)*z0u
         else
            z0curk(k) = z0curk(k)+wcl(2,L)*z0u
         endif
      enddo
      z0curk(k)=max(epsz0,z0curk(k))
   enddo
   !
   z0rouk = 0d0; taub = 0d0; dzdx=0d0; dzdy=0d0
   do L=1,lnx
      k1=ln(1,L); k2=ln(2,L)
      z0rouk(k1) = z0rouk(k1)+wcl(1,L)*z0urou(L)   
      z0rouk(k2) = z0rouk(k2)+wcl(2,L)*z0urou(L)
      taub(k1)   = taub(k1)+wcl(1,L)*taubxu(L)        
      taub(k2)   = taub(k2)+wcl(2,L)*taubxu(L)        
   end do
   !
   if (kmx > 0) then            ! 3D
      deltas = 0.05d0
      maxdepfrac = 0.05
      if (jawave>0 .and. v2dwbl>0) then
         deltas = 0d0
         do L=1,lnx
            k1=ln(1,L); k2=ln(2,L)
            deltas(k1) =  deltas(k1) + wcl(1,L)*wblt(L)
            deltas(k2) =  deltas(k2) + wcl(2,L)*wblt(L)
         end do
         maxdepfrac = 0.5d0                        ! cases where you want 2D velocity above the wbl, make sure 2nd criterion applies
      endif
      zcc = 0d0

      do kk = 1, ndx
         call getkbotktop(kk,kb,kt)
         do k = kb, kt
            zcc  = 0.5d0*(zws(k-1)+zws(k))         ! cell centre position in vertical layer admin, using absolute height
            kmxvel = k
            if (zcc>=(bl(kk)+maxdepfrac*hs(kk)) .or. zcc>=(bl(kk)+deltas(kk))) then
               exit
            endif
         enddo

         uuu(kk)   = ucxq_tmp(kmxvel)                  ! discharge based cell centre velocities
         vvv(kk)   = ucyq_tmp(kmxvel)
         umod(kk)  = sqrt(uuu(kk)*uuu(kk) + vvv(kk)*vvv(kk))
         zumod(kk) = zcc-bl(kk)
      end do

      ! If secondary flow, then we consider the bed shear stress magnitude as computed in 3D,
      ! but the direction as computed by the 1DV solution of the secondary flow. Here the
      ! near bed vector is projected back to the original depth averaged direction of the flow.
      if (jasecflow > 0) then
         do kk = 1, ndx
            uuu(kk) = spiratx(kk)*umod(kk)
            vvv(kk) = spiraty(kk)*umod(kk)
         enddo
      end if

   else
      do kk = 1, ndx
         uuu(kk)   = ucxq_mor(kk)
         vvv(kk)   = ucyq_mor(kk)
         umod(kk)  = sqrt(uuu(kk)*uuu(kk) + vvv(kk)*vvv(kk))
         zumod(kk) = hs_mor(kk)/ee
      enddo
   end if
   !
   ! set velocities to zero if not active point for transport
   !
   do nm = 1, ndx
      if (kfsed(nm) == 0) then
         uuu  (nm) = 0.0_fp
         vvv  (nm) = 0.0_fp
         umod (nm) = 0.0_fp
         zumod(nm) = 0.0_fp
      endif
   end do
   !
   ! Get the reduction factor if thickness of sediment at bed is less than
   ! user specified threshold. Also get maximum erosion source SRCMAX
   ! (used for cohesive sediments).
   !
   dtmor = dts * morfac
   !
   call getfixfac(stmpar%morlyr, 1        , ndx     , lsedtot, &                    ! Update underlayer bookkeeping system for erosion/sedimentation
                & ndx          , fixfac   , ffthresh  )
   !
   ! Set fixfac to 1.0 for tracer sediments and adjust frac
   !
   istat = bedcomp_getpointer_integer(stmpar%morlyr, 'IUnderLyr', iunderlyr)        ! iunderlayer=1: mixed, 2: layer bookkeeping
   if (ffthresh>0.0_hp .or. iunderlyr/=1) then
      srcmax = 1.0e+10_fp
   elseif (iunderlyr==1) then
      istat = bedcomp_getpointer_realprec(stmpar%morlyr,'bodsed',bodsed)
      do l = 1, lsed
         if (ffthresh<1.0e-10_fp) then
            !
            ! Compute SRCMAX (only used for cohesive sediments)
            !
            do nm = 1, ndx
               !
               ! If user-specified THRESH is <= 0.0, the erosion flux is effectively not limited by FIXFAC since ffthresh is 1e-10
               ! but by the amount of sediment that is available
               !
               !srcmax(nm, l) = bodsed(l, nm)*cdryb(l)/dtmor
               srcmax(nm, l) = bodsed(l, nm)/dtmor
            enddo
         endif
         !
         if (sedtrcfac(l)>0.0_fp) then
            grkg = 1.0_fp / (rhosol(l)*pi*sedd50(l)**3/6.0_fp) ! Number of grains per kg
            grm2 = 0.5_fp / (pi*sedd50(l)**2) ! Number of grains per m^2 -- Not quite correct: maximum area of grain is pi*r^2 not pi*d^2, using porosity factor of 0.5
            do nm = 1, ndx
               fixfac(nm, l) = 1.0_fp
               grlyrs = bodsed(l, nm) * grkg / grm2 ! Number of grain layers
               frac(nm, l) = min(max(0.0_fp, grlyrs), 1.0_fp)*sedtrcfac(l)
            enddo
         endif
      enddo
   endif
   !
   ! in case of multiple (non-mud) fractions, the following quantities
   ! --- that are initialized in INISED --- may be time-dependent and
   ! they must be updated here or after updating the bed levels in
   ! BOTT3D.
   !
   if (lsedtot-nmudfrac > 1) then    ! for all non-cohesive suspended fractions
      !
      ! calculate arithmetic mean sediment diameter Dm
      ! calculate geometric mean sediment diameter Dg
      ! calculate percentiles Dxx
      !
      call compdiam(frac      ,sedd50    ,sedd50    ,sedtyp    ,lsedtot   , &
         & logsedsig ,nseddia   ,logseddia ,ndx       ,1         , &
         & ndx       ,xx        ,nxx       ,max_mud_sedtyp, min_dxx_sedtyp, &
         & sedd50fld ,dm        ,dg        ,dxx       ,dgsd      )
      !
      ! determine hiding & exposure factors
      !
      call comphidexp(frac      ,dm        ,ndx       ,lsedtot   , &
         & sedd50    ,hidexp    ,ihidexp   ,asklhe    , &
         & mwwjhe    ,1         ,ndx      )

   !endif
   !
   ! TODO UNST-5545 adapt compsandfrac
   !if (lsedtot > nmudfrac .and. ((lsedtot - nmudfrac > 1) .or. (nmudfrac > 0))) then
      !
      ! compute sand fraction
      !
      call compsandfrac(frac, sedd50, ndx, lsedtot, sedtyp, &
                      & max_mud_sedtyp, sandfrac, sedd50fld, &
                      & 1, ndx)   
   endif   
   !
   ! compute normal component of bed slopes at edges    (e_xxx refers to edges)

   dzdx = 0d0; dzdy = 0d0

   do L = 1, lnx
      ! Get the bottom slope components in the cell centres; keep these, needed later on
      ! Bottom slopes are positive on downsloping parts, cf bedbc2004.f90 and info from Bert Jagers
      ! So bl(k1)-bl(k2) instead of other way round
      k1 = ln(1,L); k2 = ln(2,L)
      dzdx(k1) = dzdx(k1) - wcx1(L)*(bl(k2)-bl(k1))*dxi(L)
      dzdy(k1) = dzdy(k1) - wcy1(L)*(bl(k2)-bl(k1))*dxi(L)
      dzdx(k2) = dzdx(k2) - wcx2(L)*(bl(k2)-bl(k1))*dxi(L)
      dzdy(k2) = dzdy(k2) - wcy2(L)*(bl(k2)-bl(k1))*dxi(L)
   enddo
   !   boundary conditions:
   !      dz/dn = 0
   do L=Lnxi+1,Lnx
      k1 = ln(1,L)
      k2 = ln(2,L)
      !     project in normal and tangential direction
      dzdn =  dzdx(k2) * csu(L) + dzdy(k2) * snu(L)
      dzds = -dzdx(k2) * snu(L) + dzdy(k2) * csu(L)
      !     apply boundary conditions in normal and tangential direction
      dzdn = -dzdn
      !      dzds =  dzds  ! for completeness
      !     project back to Cartesian coordinate directions
      dzdx(k1) = dzdn * csu(L) - dzds * snu(L)
      dzdy(k1) = dzdn * snu(L) + dzds * csu(L)
   end do

   !   Note: at closed boundaries, effectively dzdn=0 is applied

   do L = 1, lnx
      ! Interpolate back to links
      k1 = ln(1,L); k2 = ln(2,L)
      !       e_dzdn(L) = acl(L)*(csu(L)*dzdx(k1) + snu(L)*dzdy(k1)) + (1d0-acl(L))*(csu(L)*dzdx(k2) + snu(L)*dzdy(k2))
      e_dzdn(L) = -dxi(L)*(bl(ln(2,L))-bl(ln(1,L)))                                                              ! more accurate near boundaries
      e_dzdt(L) = acl(L)*(-snu(L)*dzdx(k1) + csu(L)*dzdy(k1))+(1d0-acl(L))*(-snu(L)*dzdx(k2) + csu(L)*dzdy(k2))  ! affected near boundaries due to interpolation
   end do
   !
   !================================================================
   !    Start of sand part
   !================================================================
   !
   ! Start of main loop over sediment fractions for suspended sediment
   ! sources, sinks, equilibrium concentrations and vertical diffusion
   ! coefficients, and bed-load transport vector components at water
   ! level points
   !
   do nm = 1, ndx
      !
      ! do not calculate sediment sources, sinks, and bed load
      ! transport in areas with very shallow water.
      !
      if ((s1(nm)-bl(nm))<=epshs) cycle     ! dry
      !
      call getkbotktop(nm, kb, kt)
      if (kfsed(nm) == 0) then              ! shallow but not dry, ie ]epshs sedthresh]
         !
         ! Very shallow water:
         ! set sediment diffusion coefficient
         ! and set zero equilibrium concentrations
         !
         if (kmx>0) then              ! 3D only
            ! at layer interfaces, but not at bed and surface  ! to check...
            do l = 1,lsed
               do k = kb, kt-1
                  !seddif(l, k) = max(vicwws(k),dicoww)
                  seddif(l, k) = vicwws(k)    ! background dico is added in solve_vertical
               enddo
            enddo
            !
            rsedeq(nm,:) = 0d0
         endif
         cycle
      endif
      !
      ! kfsed(nm) == 1
      !
      h0   = max(0.01_fp, s0(nm) - bl(nm))
      h1   = max(0.01_fp, s1(nm) - bl(nm))

      kmaxlc = kmx
      if (kmx>0) then
         !
         ! 3D CASE
         !
         kbed    = kb                                   ! okay, this is safe for Z-layers
         thicklc = 0.0_fp
         klc     = 1
         do k = kt,kb,-1                                ! counts from surface to bottom
            thicklc(klc)   = (zws(k)-zws(k-1))/h1       ! depth fraction, this works for z layers. If only sigma: m_flow::laycof can be used
            klc=klc+1
         enddo
         siglc   = 0.0_fp
         kmaxlc = klc-1                                 ! needed for z layers eventually. For sigma, equals kmx
         siglc(1) = -0.5_fp*thicklc(1)
         do klc = 2, kmaxlc
            siglc(klc) = siglc(klc-1) - 0.5_fp*(thicklc(klc) + thicklc(klc-1))
         enddo
      else                       ! 2D
         kbed    = nm            ! okay, kbed index 2D nodes from now on
         kmaxlc  = 1
         thicklc = 1.0_fp
      endif
      !
      ! Compute absolute value maximum slope for erosilt
      ! Slope taken from link, similar to Delft 3D
      !
      maxslope = 0.0_fp
      do Lf = 1, nd(nm)%lnx
         L = abs(nd(nm)%ln(Lf))
         maxslope = max(maxslope, abs(e_dzdn(L)))
      end do
      !
      ! Compute depth-averaged velocity components at cell centre, discharge based cc velocities
      !
      umean = ucxq_tmp(nm)      ! ok, calculated in getucxucyandsoon
      vmean = ucyq_tmp(nm)
      velm = sqrt(umean**2+vmean**2)
      !
      ubed = ucxq_tmp(kbed)
      vbed = ucyq_tmp(kbed)
      velb = sqrt(ubed**2 + vbed**2)
      if (kmaxlc>1) then               ! 3D only
         zvelb = 0.5_fp*thicklc(kmaxlc)*h1
      else
         zvelb = h1/ee
      endif
      !
      if (jawave > 0 .and. .not. flowWithoutWaves) then
         ubot = uorb(nm)        ! array uitgespaard
      else
         ubot = 0d0
      end if
      !
      ! Calculate total (possibly wave enhanced) roughness
      !
      if (jawave > 0 .and. .not. flowWithoutWaves) then
         z0rou = max(epsz0,z0rouk(nm))
      else ! currents only
         z0rou = z0curk(nm)       ! currents+potentially trachy
      end if
      !
      chezy = sag * log(h1/ee/z0rou) / vonkar                          ! consistent with getczz0
      !
      ! bed shear stress as used in flow, or
      ! skin fiction following Soulsby; "Bed shear stress under
      ! combined waves and currents on rough and smooth beds"
      ! Estproc report TR137, 2004
      !
      if (bsskin) then
         !
         ! Compute bed stress resulting from skin friction
         !
         if (iflufflyr>0) then
            afluff = get_alpha_fluff(iflufflyr, lsed, nm, mfluff(:,nm), stmpar%trapar, stmpar%sedpar)
         else
            afluff = 0d0
         endif
         !
         call compbsskin(umean, vmean, h1, wave, uorb(nm), twav(nm), &
                          & phiwav(nm), thcmud(nm), mudfrac(nm), taub(nm), &
                          & rhowat(kbed), vismol, stmpar%sedpar, afluff)
      endif
      !
      ustarc = umod(nm)*vonkar/log(1.0_fp + zumod(nm)/max(z0rou,1d-5))

      ! To be in line with rest of FM, this should be 
      !ustarc = umod(nm)*vonkar/log(zumod(nm)/z0rou - 1d0)
      !
      !if (scour) then
      !
      tauadd = 0d0
      !
      ! Compute effective depth averaged velocity
      !
      utot  = ustarc * chezy / sag
      ulocal= utot * uuu(nm) / (umod(nm)+eps)
      vlocal= utot * vvv(nm) / (umod(nm)+eps)
      !
      ! sa0 and tem0 have no real meaning, sa1 and tem1 before transport are at the old time-level,
      ! while after transport they are at the new time-level
      if (jasal > 0) then
         salinity = constituents(isalt, kbed)                            ! r0(nm, kbed, lsal)
      else
         salinity = backgroundsalinity
      endif
      if (jatem > 0) then
         temperature =  constituents(itemp,kbed)                         ! r0(nm, kbed, ltem)
      else
         temperature = backgroundwatertemperature
      endif
      !
      taks0 = 0d0
      !
      ! Calculate Van Rijn's reference height
      !
      if (iopkcw==1) then            !  iopkcw: options to calculate curr related roughness height
         rc = 30.d0*z0curk(nm)       ! 33?
      else
         rc = rdc
      endif
      taks0 = max(aksfac*rc, 0.01d0*h1)
      !
      if (jawave>0 .and. .not. flowWithoutWaves) then
         if (twav(nm)>0d0) then
            delr  = 0.025d0
            taks0 = max(0.5d0*delr, taks0)
         end if
      endif
      !
      ! Limit maximum aks to 20% of water depth
      ! (may be used when water depth becomes very small)
      !
      taks0 = min(taks0, 0.2d0*h1)
      !
      ! Input parameters are passed via dll_reals/integers/strings-arrays
      !
      if (max_reals < MAX_RP) then
         write(errmsg,'(a)') 'fm_erosed::Insufficient space to pass real values to transport routine.'
         call mess(LEVEL_FATAL, errmsg)
         error = .true.
         return
      endif
      dll_reals(RP_TIME ) = real(time1     ,hp)
      dll_reals(RP_EFUMN) = real(ulocal    ,hp)
      dll_reals(RP_EFVMN) = real(vlocal    ,hp)
      dll_reals(RP_EFVLM) = real(utot      ,hp)
      dll_reals(RP_UCHAR) = real(uuu(nm)   ,hp)
      dll_reals(RP_VCHAR) = real(vvv(nm)   ,hp)
      dll_reals(RP_VELCH) = real(umod(nm)  ,hp)
      dll_reals(RP_ZVLCH) = real(zumod(nm) ,hp)
      dll_reals(RP_DEPTH) = real(h1        ,hp)
      dll_reals(RP_CHEZY) = real(chezy     ,hp)
      if (wave) then
         dll_reals(RP_HRMS ) = real(hwav(nm)     ,hp)
         dll_reals(RP_TPEAK) = real(twav(nm)     ,hp)
         dll_reals(RP_TETA ) = real(phiwav(nm)   ,hp)
         dll_reals(RP_RLAMB) = real(rlabda(nm)   ,hp)
         dll_reals(RP_UORB ) = real(uorb(nm)     ,hp)
         if (jawave>2) then
            dll_reals(RP_KWTUR) = real(ktb(nm)      ,hp)
         else
            dll_reals(RP_KWTUR) = real(0.0_hp      ,hp)      ! array not allocated for fetch length models (choice of HK)
         endif
      else
         dll_reals(RP_HRMS ) = 0.0_hp
         dll_reals(RP_TPEAK) = 0.0_hp
         dll_reals(RP_TETA ) = 0.0_hp
         dll_reals(RP_RLAMB) = 0.0_hp
         dll_reals(RP_UORB ) = 0.0_hp
         dll_reals(RP_KWTUR) = 0.0_hp
      endif
      dll_reals(RP_D10MX) = real(dxx(nm,i10)    ,hp)
      dll_reals(RP_D15MX) = real(dxx(nm,i15)    ,hp)
      dll_reals(RP_D90MX) = real(dxx(nm,i90)    ,hp)
      dll_reals(RP_MUDFR) = real(mudfrac(nm)    ,hp)
      dll_reals(RP_RHOWT) = real(rhowat(kbed)   ,hp) ! Density of water
      dll_reals(RP_SALIN) = real(salinity       ,hp)
      dll_reals(RP_TEMP ) = real(temperature    ,hp)
      dll_reals(RP_GRAV ) = real(ag             ,hp)
      dll_reals(RP_VICML) = real(vismol         ,hp)
      dll_reals(RP_TAUB ) = real(taub(nm)       ,hp)
      dll_reals(RP_UBED ) = real(ubed           ,hp)
      dll_reals(RP_VBED ) = real(vbed           ,hp)
      dll_reals(RP_VELBD) = real(velb           ,hp)
      dll_reals(RP_ZVLBD) = real(zvelb          ,hp)
      dll_reals(RP_VNKAR) = real(vonkar         ,hp)
      dll_reals(RP_Z0CUR) = real(z0curk(nm)      ,hp) !   potentially with trachytopes
      dll_reals(RP_Z0ROU) = real(z0rou          ,hp)
      dll_reals(RP_DG   ) = real(dg(nm)         ,hp)
      dll_reals(RP_DM   ) = real(dxx(nm,i50)    ,hp) ! d50 mixture, not dm; following Van Rijn 2007c
      dll_reals(RP_SNDFR) = real(sandfrac(nm)   ,hp)
      dll_reals(RP_DGSD ) = real(dgsd(nm)       ,hp)
      if (iturbulencemodel > 2 .and. kmx>0 ) then
         dll_reals(RP_KTUR ) = real(turkinepsws(1,kb),hp)     ! 1=k, 2=e
      endif
      dll_reals(RP_UMEAN) = real(umean     ,hp)
      dll_reals(RP_VMEAN) = real(vmean     ,hp)
      dll_reals(RP_VELMN) = real(velm      ,hp)
      dll_reals(RP_USTAR) = real(ustarc    ,hp)
      dll_reals(RP_BLCHG) = real(dzbdt(nm) ,hp)   ! for dilatancy
      dll_reals(RP_DZDX)  = real(dzdx(nm)  ,hp)   ! for dilatancy
      dll_reals(RP_DZDY)  = real(dzdy(nm)  ,hp)   ! for dilatancy
      !

      if (max_integers < MAX_IP) then
         write(errmsg,'(a)') 'fm_erosed::Insufficient space to pass integer values to transport routine.'
         call mess(LEVEL_FATAL, errmsg)
         error = .true.
         return
      endif
      dll_integers(IP_NM   ) = nm
      !
      if (max_strings < MAX_SP) then
         write(errmsg,'(a)') 'fm_erosed::Insufficient space to pass strings to transport routine.'
         call mess(LEVEL_FATAL, errmsg)
         error = .true.
         return
      endif
      !
      ! total mass in fluff layer
      !
      mfltot = 0.0_fp
      if (iflufflyr>0) then
         do l = 1, lsed
            mfltot = mfltot + max(0.0_fp,mfluff(l,nm))
         enddo
      endif
      !
      do l = 1, lsedtot
         ll = lstart + l
         !
         ! Copy the globally defined l-dependent parameters of array par to localpar.
         ! All nm-/l-based redefinitions of these parameters are performed
         ! on localpar, thus ensuring that the global array par is not
         ! messed up with specific, nm-/l-dependent data.
         !
         do i = 1, npar
            j = stmpar%trapar%iparfld(i,l)
            if (j>0) then
                localpar(i) = stmpar%trapar%parfld(nm,j)
            else
                localpar(i) = par(i,l)
            endif
         enddo
         !
         ! fraction specific quantities
         !
         dll_reals(RP_HIDEX)    = real(hidexp(nm,l) ,hp)
         dll_reals(RP_RHOSL)    = real(rhosol(l) ,hp)
         dll_integers(IP_ISED ) = l
         dll_strings(SP_USRFL)  = dll_usrfil(l)
         !
         if (.not.has_bedload(tratyp(l))) then
            !
            ! sediment transport governed by erosion and deposition fluxes
            !
            dll_reals(RP_D50  ) = 0.0_hp
            dll_reals(RP_DSS  ) = 0.0_hp
            dll_reals(RP_DSTAR) = 0.0_hp
            dll_reals(RP_SETVL) = real(ws(kb, l)  ,hp)
            !
            if (kmx > 0) then
               klc = 0
               wslc   = 0.0_fp
               do kk = kt, kb-1, -1                  ! should follow sigma conventions
                  wslc(klc)   = ws(kk, l)
                  klc=klc+1
               enddo
            else
               klc = 1
               wslc(klc)   = ws(nm, l)
            end if
            !
            ! Fluff layer parameters
            !
            fracf   = 0.0_fp
            if (iflufflyr>0) then
               if (mfltot>0.0_fp) fracf = max(0.0_fp,mfluff(l,nm))/mfltot
            endif
            !
            kmaxsd        = 1                       ! for mud fractions kmaxsd points to the grid cell at the bottom of the water column 
            thick0        = max(thicklc(kmaxsd) * h0, epshs)
            thick1        = max(thicklc(kmaxsd) * h1, epshs)
            !
            call erosilt(thicklc        ,kmaxlc       , wslc        , mdia          , &
                       & thick1         ,thick1       , fixfac(nm,l), srcmax(nm, l) , &                         ! mass conservation
                       & frac(nm,l)     ,oldmudfrac   , flmd2l      , iform(l)      , &
                       & npar           ,localpar     ,max_integers , max_reals     , &
                       & max_strings    ,dll_function(l),dll_handle(l), dll_integers, &
                       & dll_reals      ,dll_strings  ,iflufflyr    , mfltot        , &
                       & fracf          ,maxslope     ,wetslope     , &
                       & error          ,wstau        , sinktot     , sourse(nm,l)  , sourfluff)
            if (error) then
               write(errmsg,'(a)') 'fm_erosed::erosilt returned an error. Check your inputs.'
               call mess(LEVEL_FATAL, errmsg)
            end if
            !
            if (iflufflyr>0) then
               if (iflufflyr==2) then
                  sinkf(l,nm)  = sinktot*(1.0_fp - depfac(l,nm))
                  sinkse(nm,l) = sinktot*depfac(l,nm)
               else
                  sinkf(l,nm)  = sinktot
                  sinkse(nm,l) = 0.0_fp
               endif
               !
               sourf(l,nm)  = sourfluff
            else
               sinkse(nm,l) = sinktot
               sourse(nm,l) = sourse(nm,l) + sourfluff
            endif
            !
            if (kmx>0) then
               !
               ! For 3D model set sediment diffusion coefficient
               ! NOTE THAT IF ALGEBRAIC TURBULENCE MODEL IS USED THEN WAVES
               ! ONLY AFFECT THE VERTICAL TURBULENT MIXING VIA THE ENHANCED BED
               ! ROUGHNESS
               !
               klc    = 0
               do k = kt, kb-1, -1
                  !seddif(l, k) = max(vicwws(k),dicoww)
                  seddif(l, k) = vicwws(k)
                  klc=klc+1
               enddo
            endif
            !
            ! l runs from 1 to lsedtot, kmxsed is defined for 1:lsed
            ! The first lsed fractions are the suspended fractions (including cohesive ones),
            ! so this works
            !
            kmxsed(nm, l) = kb ! to check
            cycle
         endif
         !
         ! sediment transport governed by bedoad vector and reference concentration
         !
         suspfrac = has_advdiff(tratyp(l))
         !
         tsd  = -999.0_fp
         di50 = sedd50(l)
         if (di50 < 0.0_fp) then
            !  Space varying sedd50 specified in array sedd50fld:
            !  Recalculate dstar, tetacr and taucr for each nm,l - point
            di50     = sedd50fld(nm)
            drho     = (rhosol(l)-rhowat(kbed)) / rhowat(kbed)
            dstar(l) = di50 * (drho*ag/vismol**2)**0.3333_fp
            if (dstar(l) < 1.0_fp) then
               if (iform(l) == -2) then
                  tetacr(l) = 0.115_fp / (dstar(l)**0.5_fp)
               else
                  tetacr(l) = 0.24_fp / dstar(l)
               endif
            elseif (dstar(l) <= 4.0_fp) then
               if (iform(l) == -2) then
                  tetacr(l) = 0.115_fp / (dstar(l)**0.5_fp)
               else
                  tetacr(l) = 0.24_fp / dstar(l)
               endif
            elseif (dstar(l)>4.0_fp .and. dstar(l)<=10.0_fp) then
               tetacr(l) = 0.14_fp  / (dstar(l)**0.64_fp)
            elseif (dstar(l)>10.0_fp .and. dstar(l)<=20.0_fp) then
               tetacr(l) = 0.04_fp  / (dstar(l)**0.1_fp)
            elseif (dstar(l)>20.0_fp .and. dstar(l)<=150.0_fp) then
               tetacr(l) = 0.013_fp * (dstar(l)**0.29_fp)
            else
               tetacr(l) = 0.055_fp
            endif
            taucr(l) = factcr * (rhosol(l)-rhowat(kbed)) * ag * di50 * tetacr(l)
         endif
         !
         if (suspfrac) then
            tsigmol = 1d0            ! molecular PS = 1d0
            tdss    = dss(nm, l)
            twsk    = ws(kb, l)      ! was kb-1, should be same in 3D (see fallve)
         else
            !
            ! use dummy values for bedload fractions
            !
            tsigmol =  1.0_fp
            tdss    =  di50
            twsk    =  0.0_fp
         endif
         !
         ! NONCOHESIVE fraction specific quantities
         !
         dll_reals(RP_D50  ) = real(di50    ,hp)
         dll_reals(RP_DSS  ) = real(tdss    ,hp)
         dll_reals(RP_DSTAR) = real(dstar(l),hp)
         dll_reals(RP_SETVL) = real(twsk    ,hp) ! Settling velocity near bedlevel
         !
         ! Calculate bed porosity for dilatancy
         !
         poros = 1d0-cdryb(l)/rhosol(l)
         dll_reals(RP_POROS) = real(poros   ,hp)
         !
         localpar(1) = ag
         localpar(2) = rhowat(kbed) ! rhow
         localpar(3) = rhosol(l)
         localpar(4) = (rhosol(l)-rhowat(kbed)) / rhowat(kbed)
         localpar(5) = 1.0E-6     ! laminar viscosity of water
         localpar(6) = di50
         !
         ! SWITCH 2DH/3D SIMULATIONS
         !
         if (kmaxlc > 1) then
            !
            ! 3D CASE
            !
            concin3d = 0.0_fp
            if (suspfrac) then
               !
               ! Fill local 1dv arrays with settling velocity, diffusivity and concentration.
               !
               klc    = 0
               dcwwlc = 0.0_fp
               wslc   = 0.0_fp
               do kk = kt, kb-1, -1                        ! sigma convention
                  !dcwwlc(klc) = max(vicwws(kk),dicoww)     ! maximalisation is safety
                  dcwwlc(klc) = vicwws(kk)                 !  background is added in solve_vertical
                  wslc(klc)   = ws(kk, l)
                  klc=klc+1
               enddo
               !
               klc    = 1
               do kk = kt, kb, -1
                  concin3d(klc) = max(0.0_fp , constituents(ll,kk))
                  klc=klc+1
               enddo
            endif
            taks = 0.0_fp
            !
            ! Solve equilibrium concentration vertical and
            ! integrate over vertical
            !
            if (jasecflow > 0) then                          ! secondary flow in 3D
               spirintnm = spirint(nm)
            endif
            !
            call eqtran(siglc           ,thicklc       ,kmaxlc      ,wslc      ,ltur      , &
                      & frac(nm,l)      ,tsigmol       ,dcwwlc      ,mdia      ,taucr(l)  , &
                      & bfmpar%rksr(nm) ,3             ,jasecflow   ,spirintnm ,suspfrac  , &
                      & tetacr(l)       ,concin3d      , &
                      & dzdx(nm)        ,dzdy(nm)      ,ubot        ,tauadd    ,sus       , &
                      & bed             ,susw          ,bedw        ,espir     ,wave      , &
                      & scour           ,ubot_from_com              ,camax     ,eps       , &
                      & iform(l)        ,npar          ,localpar    ,max_integers,max_reals , &
                      & max_strings     ,dll_function(l) ,dll_handle(l) ,dll_integers,dll_reals , &
                      & dll_strings     , &
                      & taks            ,caks          ,taurat(nm,l),sddflc    ,rsdqlc    , &
                      & kmaxsd          ,conc2d        ,sbcx(nm,l)  ,sbcy(nm,l),sbwx(nm,l) , &
                      & sbwy(nm,l)      ,sswx(nm,l)    ,sswy(nm,l)  ,tdss      ,caks_ss3d , &
                      & aks_ss3d        ,ust2(nm)      ,tsd         ,error     )
            !
            if (error) then
               write(errmsg,'(a)') 'fm_erosed::eqtran in 3D returned an error. Check your inputs.'
               call mess(LEVEL_FATAL, errmsg)
            end if
            !
            if (suspfrac) then
               aks(nm, l) = taks
               dss(nm, l) = tdss
               !
               ! Copy results into arrays
               !
               kmxsed(nm, l) = kb + kmaxlc-kmaxsd
               !
               klc=0
               do kk = kt, kb-1, -1
                  seddif(l, kk) = sddflc(klc)
                  klc           = klc + 1
               enddo
               rsedeq(nm, l)    = rsdqlc(kmaxsd)
               !
               thick0 = max(thicklc(kmaxsd) * h0, epshu)
               thick1 = max(thicklc(kmaxsd) * h1, epshu)
               thick1 =thicklc(kmaxsd) * h1
               !
               call soursin_3d  (h1                ,thick1         ,thick1             ,              &      ! thick1 iso thick0 mass conservation
                              &  siglc(kmaxsd)     ,thicklc(kmaxsd),constituents(ll,kmxsed(nm,l))    , &
                              &  vismol            ,tsigmol        ,seddif(l, kmxsed(nm,l)-1),        &
                              &  rhosol(l)         ,caks_ss3d      ,ws(kmxsed(nm,l),l)      ,         &
                              &  aks_ss3d          ,sourse(nm,l)   ,sour_im(nm,l)      ,              &
                              &  sinkse(nm,l) )
               !
               ! Impose relatively large vertical diffusion
               ! coefficients for sediment in layer interfaces from
               ! bottom of reference cell downwards, to ensure little
               ! gradient in sed. conc. exists in this area.

               difbot = 10.0_fp * ws(kmxsed(nm,l)-1,l) * thick1
               do kk = kb-1, kmxsed(nm,l)-1
                  seddif(l, kk) = difbot
               enddo
            endif ! suspfrac
         else
            !
            ! kmaxlc = 1
            ! 2D CASE (Numerical approximation)
            !
            if (suspfrac) then
               !
               ! Fill local 1dv arrays with fall velocity and
               ! diffusivity
               !
               do k2d = 0, kmax2d
                  ws2d(k2d)   = ws(kb, l)
                  dcww2d(k2d) = 0.0_fp        ! to check
               enddo
            endif
            trsedeq =  0.0_fp
            taks = taks0
            !
            if (jasecflow > 0) then                          ! secondary flow in 2D
               spirintnm = spirint(nm)
            endif
            !
            ! Solve equilibrium concentration vertical and
            ! integrate over vertical; compute bedload
            ! transport excluding slope effects, but including spiral flow
            !
            ltur = 0
            !
            call eqtran(sig2d   ,thck2d        ,kmax2d       ,ws2d      ,ltur      , &
                      & frac(nm,l)     ,tsigmol       ,dcww2d       ,mdia      ,taucr(l)  , &
                      & bfmpar%rksr(nm),2             ,jasecflow    ,spirintnm ,suspfrac  , &
                      & tetacr(l)      ,concin2d      , &
                      & dzdx(nm)       ,dzdy(nm)      ,ubot         ,tauadd    ,sus       , &
                      & bed            ,susw          ,bedw         ,espir     ,wave      , &
                      & scour          ,ubot_from_com ,camax        ,eps       , &
                      & iform(l)       ,npar          ,localpar      ,max_integers ,max_reals , &
                      & max_strings    ,dll_function(l),dll_handle(l) ,dll_integers ,dll_reals , &
                      & dll_strings    , &
                      & taks           ,caks          ,taurat(nm,l) ,sddf2d    ,rsdq2d    , &
                      & kmaxsd         ,trsedeq       ,sbcx(nm,l)   ,sbcy(nm,l),sbwx(nm,l) , &
                      & sbwy(nm,l)     ,sswx(nm,l)    ,sswy(nm,l)   ,tdss      ,caks_ss3d , &
                      & aks_ss3d       ,ust2(nm)      ,tsd          ,error     )

            if (error) then
               write(errmsg,'(a)') 'fm_erosed::eqtran in 2D returned an error. Check your inputs.'
               call mess(LEVEL_FATAL, errmsg)
            end if

            if (suspfrac) then
               aks   (nm, l)       = taks
               dss   (nm, l)       = tdss
               rsedeq(nm, l)       = trsedeq
               kmxsed(nm, l)       = kbed        ! nm or 1  when sedthr not passed
               !
               ! Galappatti time scale and source and sink terms
               !
               call soursin_2d(umod(nm)      ,ustarc        ,h0            ,h1        , &
                             & ws(kb,l)      ,tsd           ,trsedeq       ,factsd,    &
                             & sourse(nm,l)  ,sour_im(nm,l) ,sinkse(nm,l)  )
            endif ! suspfrac
         endif ! kmaxlc = 1
         if (suspfrac) then
            rca(nm, l) = caks * rhosol(l)
         endif
      enddo ! next sediment fraction
      ua(nm) = real(dll_reals(RP_UAU), fp)
      va(nm) = real(dll_reals(RP_VAU), fp)
   enddo ! next nm point
   !
   ! Distribute velocity asymmetry to links
   !
   do L = 1, lnxi
      k1 = ln(1,L); k2=ln(2,L)
      uau(L) = (acL(L)*ua(k1) + (1d0-acL(L))*ua(k2)) * csu(L) +   &
         (acL(L)*va(k1) + (1d0-acL(L))*va(k2)) * snu(L)
   end do
   !
   do L = lnxi+1, lnx   ! Boundaries: neumann
      k = ln(2,L)
      uau(L) = ua(k)*csu(L) + va(k)*snu(L)
   end do
   !
   ! Reduce the source and sink terms to avoid large bed level changes
   !
   call fm_red_soursin()

   do l = 1,lsedtot                                   ! this is necessary for next calls to upwbed
      if (has_bedload(tratyp(l))) then
         do nm = 1, ndx
            sxtot(nm, l) = sbcx(nm, l) + sbwx(nm, l) + sswx(nm, l)
            sytot(nm, l) = sbcy(nm, l) + sbwy(nm, l) + sswy(nm, l)
         enddo
      endif
   enddo
   !
   if (stmpar%morpar%moroutput%rawtransports) then
      sbcx_raw = sbcx; sbcy_raw = sbcy;    ! save transports before upwinding and bed slope effects
      sbwx_raw = sbwx; sbwy_raw = sbwy;    ! to compare with analytical solutions
      sswx_raw = sswx; sswy_raw = sswy;
   endif
   !
   ! Upwind scheme for bed load and wave driven transport
   ! Convert sand bed load transport to velocity points using upwind scheme
   !
   if (bed > 0.0_fp) then
      !
      ! Upwind bed load transport
      !
      call fm_upwbed(lsedtot, sbcx, sbcy, sxtot, sytot, e_sbcn, e_sbct)
   endif
   !
   if (bedw>0.0_fp .and. jawave > 0 .and. .not. flowWithoutWaves) then
      !
      ! Upwind wave-related bed load load transports
      !
      call fm_upwbed(lsedtot, sbwx, sbwy, sxtot, sytot, e_sbwn, e_sbwt)
   endif
   !
   if (susw>0.0_fp .and. jawave > 0 .and. .not. flowWithoutWaves) then
      !
      ! Upwind wave-related suspended load transports
      !
      call fm_upwbed(lsedtot, sswx, sswy, sxtot, sytot, e_sswn, e_sswt)
   endif
   !
   ! Bed-slope and sediment availability effects for
   ! current-related bed load transport
   !
   if (bed > 0.0_fp) then
      call fm_adjust_bedload(e_sbcn, e_sbct, .true.)
   endif
   !
   ! Determine incoming discharge and transport at nodes
   !
   qb_out = 0d0; width_out = 0d0; sb_in = 0d0; sb_dir = 1
   BranInIDLn = 0
   do inod = 1, network%nds%Count
      pnod => network%nds%node(inod)
      if (pnod%numberofconnections > 1) then
         k3 = pnod%gridnumber ! TODO: Not safe in parallel models (check gridpointsseq as introduced in UNST-5013)
         do j=1,nd(k3)%lnx
            L = iabs(nd(k3)%ln(j))
            Ldir = sign(1,nd(k3)%ln(j))
            !
            wb1d = wu_mor(L)
            !
            if (u1(L)*Ldir < 0d0) then
               ! Outgoing discharge
               qb1d = -qa(L)*Ldir  ! replace with junction advection: to do WO
               width_out(inod) = width_out(inod) + wb1d
               qb_out(inod)    = qb_out(inod) + qb1d
               do ised = 1, lsedtot
                  sb_dir(inod, ised, j) = -1           ! set direction to outgoing
               enddo
            else
               ! Incoming discharge
               if (branInIDLn(inod) == 0) then
                  branInIDLn(inod) = L
               else
                  branInIDLn(inod) = -444               ! multiple incoming branches
               endif
            endif
         enddo
      endif
   enddo
   !
   ! Apply nodal relations to transport
   !
   do inod = 1, network%nds%Count
      pnod => network%nds%node(inod)
      if (pnod%numberofconnections == 1) cycle
      if (pnod%nodeType == nt_LinkNode) then  ! connection node
         k1 = pnod%gridnumber ! TODO: Not safe in parallel models (check gridpointsseq as introduced in UNST-5013)
         do j=1,nd(k1)%lnx
            L = iabs(nd(k1)%ln(j))
            Ldir = sign(1,nd(k1)%ln(j))
            !
            wb1d = wu_mor(L)
            do ised = 1, lsedtot
               sb1d = e_sbcn(L, ised) * Ldir  ! first compute all outgoing sed. transport.
               ! this works for one incoming branch TO DO: WO
               if (sb_dir(inod, ised, j) == -1) then
                  sb_in(inod, ised) = sb_in(inod, ised) + max(-wb1d*sb1d, 0.0_fp)  ! outgoing transport is negative
               endif
            enddo
         enddo
      endif
   enddo
   !
   ! Determining sediment redistribution
   !
   ! loop over sediment fractions
   do ised = 1, lsedtot

      ! mor%nrd%nFractions = or 1 (One for All Fractions) or lsedtot (One for Every Fraction)
      iFrac = min(ised, stmpar%nrd%nFractions)

      pFrac => stmpar%nrd%nodefractions(iFrac)

      do inod = 1, network%nds%Count
         pnod => network%nds%node(inod)
         if (pnod%nodeType == nt_LinkNode) then  ! connection node

            facCheck = 0.d0

            if (pnod%numberofconnections == 1) cycle


            ! loop over branches and determine redistribution of incoming sediment
            k3 = pnod%gridnumber ! TODO: Not safe in parallel models (check gridpointsseq as introduced in UNST-5013)
            do j=1,nd(k3)%lnx
               L = iabs(nd(k3)%ln(j))
               Ldir = sign(1,nd(k3)%ln(j))
               qb1d = -qa(L)*Ldir
               wb1d = wu_mor(L)

               ! Get Nodal Point Relation Data
               nrd_idx = get_noderel_idx(inod, pFrac, pnod%gridnumber, branInIDLn(inod), pnod%numberofconnections)

               pNodRel => pFrac%noderelations(nrd_idx)

               if (sb_dir(inod, ised, j) == -1) then ! is outgoing

                  if (qb_out(inod) > 0.0_fp) then

                     if (pNodRel%Method == 'function') then

                        expQ = pNodRel%expQ
                        expW = pNodRel%expW

                        facQ = (qb1d / qb_out(inod))**expQ
                        facW = (wb1d / width_out(inod))**expW

                        facCheck = facCheck + facQ * facW

                        e_sbcn(L,ised) = -Ldir * facQ * facW * sb_in(inod, ised) / wu_mor(L)

                     elseif (pNodRel%Method == 'table') then

                        facCheck = 1.0d0

                        if (L == pNodRel%BranchOut1Ln) then
                           Qbr1 = qb1d
                           Qbr2 = qb_out(inod) - qb1d
                        elseif (L == pNodRel%BranchOut2Ln) then
                           Qbr1 = qb_out(inod) - qb1d
                           Qbr2 = qb1d
                        else
                           call SetMessage(LEVEL_FATAL, 'Unknown Branch Out (This should never happen!)')
                        endif

                        QbrRatio = Qbr1 / Qbr2

                        SbrRatio = interpolate(pNodRel%Table, QbrRatio)

                        if (L == pNodRel%BranchOut1Ln) then
                           e_sbcn(L,ised) = -Ldir * SbrRatio * sb_in(inod, ised) / (1 + SbrRatio) / wu_mor(L)
                           e_sbct(L,ised) = 0.0
                        elseif (L == pNodRel%BranchOut2Ln) then
                           e_sbcn(L,ised) = -Ldir * sb_in(inod, ised) / (1 + SbrRatio) / wu_mor(L)
                           e_sbct(L,ised) = 0.0
                        endif


                     else
                        call SetMessage(LEVEL_FATAL, 'Unknown Nodal Point Relation Method Specified')
                     endif

                  else
                     e_sbcn(L,ised) = 0.0_fp
                     e_sbct(L,ised) = 0.0
                  endif

               endif

            enddo    ! Branches

            ! Correct Total Outflow
            if ((facCheck /= 1.0_fp) .and. (facCheck > 0.0_fp)) then
               ! loop over branches and correct redistribution of incoming sediment
               do j=1,nd(k3)%lnx
                  L = iabs(nd(k3)%ln(j))
                  if (sb_dir(inod, ised, j) == -1) then
                     e_sbcn(L,ised) = e_sbcn(L,ised)/facCheck
                  endif
               enddo    ! Branches
            endif
         endif
      enddo      ! Nodes

   enddo    ! Fractions

   !
   ! Bed-slope and sediment availability effects for
   ! wave-related bed load transport
   !
   if (bedw>0.0_fp .and. jawave > 0 .and. .not. flowWithoutWaves) then
      call fm_adjust_bedload(e_sbwn, e_sbwt,.false.)
   endif
   !
   ! Sediment availability effects for
   ! wave-related suspended load transport
   !
   if (susw>0.0_fp .and. jawave > 0 .and. .not. flowWithoutWaves) then
      call fm_adjust_bedload(e_sswn, e_sswt, .false.)
   endif
   !
   if (duneavalan) then
      call duneaval(error)         ! only on current related bed transport
      if (error) then
         write(errmsg,'(a)') 'fm_erosed::duneavalan returned an error. Check your inputs.'
         call mess(LEVEL_FATAL, errmsg)
      end if
   end if
   !
   ! Summation of current-related and wave-related transports on links
   !
   e_sbn = 0d0
   e_sbt = 0d0
   do l = 1,lsedtot
      if (has_bedload(tratyp(l))) then
         do nm = 1, lnx
            e_sbn(nm, l) = e_sbcn(nm, l) + e_sbwn(nm, l) + e_sswn(nm, l)
            e_sbt(nm, l) = e_sbct(nm, l) + e_sbwt(nm, l) + e_sswt(nm, l)
         enddo
      endif
   enddo
   !
   ! Update sourse fluxes due to sand-mud interaction
   !
   allocate(evel(lsed), stat=istat)
   do nm = 1, ndx
      if (pmcrit(nm)<0.0_fp) cycle
      if (mudfrac(nm)<=0.0_fp .or. mudfrac(nm)>=1.0_fp) cycle
      !
      ! compute erosion velocities
      !
      evel = 0.0_fp
      do l = 1, lsed
         ll = lstart + l
         kmaxsd = kmxsed(nm,l)              ! meaning of kmaxsd changes here!
         if (frac(nm,l)>0.0_fp)  evel(l) = (sourse(nm,l) - sour_im(nm,l)*constituents(ll,kmaxsd))/(cdryb(l)*frac(nm,l))
      enddo
      !
      ! recompute erosion velocities
      !
      call sand_mud(lsed, evel, frac(nm,:), mudfrac(nm), sedtyp, pmcrit(nm))
      !
      ! recompute erosion fluxes
      ! only explicit part of erosion flux is changed
      ! (is effectively the same as changing the equilibrium concentration)
      !
      do l = 1, lsed
         ll = ised1 - 1 + l
         kmaxsd = kmxsed(nm,l)
         sourse(nm,l) = frac(nm,l)*cdryb(l)*evel(l) + sour_im(nm,l)*constituents(ll,kmaxsd)
      enddo
   enddo
   deallocate(evel, stat=istat)
   !
   ! Add implicit part of source term to sinkse
   !
   do l = 1, lsed
      do nm = 1, ndx
         sinkse(nm, l) = sinkse(nm, l) + sour_im(nm, l)
      enddo
   enddo
   !
   if (jasourcesink==0) then
      sourse=0d0
      sinkse=0d0
   elseif (jasourcesink==1) then
      !
   elseif (jasourcesink==2) then
      sinkse=0d0
   elseif (jasourcesink==3) then
      sourse=0d0
   endif
   !

   deallocate(dzdx, dzdy, stat = istat)
   if (istat == 0) deallocate(localpar, stat = istat)
   if (istat == 0) deallocate(qb_out, stat = istat)
   if (istat == 0) deallocate(width_out, stat = istat)
   if (istat == 0) deallocate(sb_in, stat = istat)
   if (istat == 0) deallocate(sb_dir, stat = istat)
   if (istat == 0) deallocate(BranInIDLn, stat = istat)

   if (istat /= 0) then
      error = .true.
      write(errmsg,'(a)') 'fm_erosed::error deallocating memory.'
      call mess(LEVEL_FATAL, errmsg)
   endif

   end subroutine fm_erosed
