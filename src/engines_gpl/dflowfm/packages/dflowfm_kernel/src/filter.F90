! filter

#include "blasfm.h"
   
!> initialize filter
subroutine ini_filter(jafilter, filterorder, jacheckmonitor, ierr)
   use m_flowgeom, only: Lnx, ln, ln2lne, nd, lne2ln, dx, wu, ba, ban, lncn, xu, yu, csu, snu
   use network_data, only: lne, nmk, kn, nod, nb, kc
   use m_flow, only: layertype, Lnkx, kmx
   use m_filter
   use m_solver
   use m_alloc
   use unstruc_messages
   use unstruc_model, only: md_netfile
   use dfm_error
   use m_alloc
   use m_partitioninfo, only: jampi
   implicit none
   
   integer,               intent(in)    :: jafilter     !< explicit (1), implicit (2), or no filter (0)
   integer,               intent(in)    :: filterorder  !< filter order
   integer,               intent(inout) :: jacheckmonitor   !< compute and output "checkerboard" monitor (1) or not (0)
   integer,               intent(out)   :: ierr         !< error (1) or not (0)
   
   
!   integer,          dimension(:), allocatable :: iLvec  !< vector Laplacian in CRS format, startpointers
!   integer,          dimension(:), allocatable :: jLvec  !< vector Laplacian in CRS format, row numbers
!   double precision, dimension(:), allocatable :: ALvec  !< vector Laplacian in CRS format, matrix entries
   
!   integer, dimension(:), allocatable :: num
   
!   integer, dimension(:), allocatable :: dum
   integer, dimension(:), allocatable :: iwork
   
!   integer, parameter                 :: LENFILNAM = 128
!   
!   character(len=LENFILNAM)           :: FNAM
   
   double precision                   :: dfacDiv, dfacCurl, dfac, val
                                      
   integer                            :: kk, k
   integer                            :: L, L2, Lf, Lf2, LL2
   integer                            :: nn, n
   integer                            :: numtot
                                      
   integer                            :: istart, iend
   integer                            :: i, ipoint, j
   
   integer                            :: len
   
   integer                            :: ierror
   
!  clean up pre-existing filter
   call dealloc_filter()
   
   if ( jafilter.ne.0 .or. jacheckmonitor.eq.1 ) then
      if ( kmx.gt.1 ) then
         call realloc(checkmonitor, kmx, keepExisting=.false., fill=0d0)
         if ( jampi.eq.1 ) then
            call realloc(workin, kmx+1, keepExisting=.false., fill=0d0)
            call realloc(workout, kmx+1, keepExisting=.false., fill=0d0)
         end if
         jacheckmonitor = 1
      else
         jacheckmonitor = 0
      end if
   end if
   
   if ( jafilter.eq.0 ) then
      itype = 0
      order = 1  ! safety
      
      return
   end if
   
   itype = jafilter
   order = filterorder
   
   ierr = 1
   
!  check for sigma-layers
   if ( layertype.ne.1 ) then
      call mess(LEVEL_ERROR, 'filter: only sigma layers supported')
      
      goto 1234
   end if
   
!  construct vector Laplacian
!  boundary conditions: u.n = 0, n.du/ds = 0
   
!  set node mask: 1 internal 2D, other elsewhere
   kc = 1
   call MAKENETNODESCODING()
   
!  get upper bound for number of non-zero entries per row
   call realloc(num, Lnx, keepExisting=.false., fill=0)
   
!  loop over flowlinks
   do Lf=1,Lnx
!     Div-part: loop over left,right neighboring cell   
      do kk=1,2
!        get cell number
         k = ln(kk,Lf)
         if ( k.eq.0 ) cycle

!        add to upper bound
         num(Lf) = num(Lf) + nd(k)%lnx
      end do
      
!     Curl-part: get netlink
      L = iabs(ln2lne(Lf))
!     loop over left,right netnode
      do nn=1,2
!        get netnode number
         n = kn(nn,L)
         
!        add to upper bound
         num(Lf) = num(Lf) + nmk(n)
      end do
   end do
   
!  allocate and construct startpointers with upper bound
   call realloc(iLvec,Lnx+1,keepExisting=.false.)
   iLvec(1)=1
   do Lf=1,Lnx
      iLvec(Lf+1) = iLvec(Lf) + num(Lf)
   end do
   
!  allocate CRS with upper bound
   numtot = iLvec(Lnx+1)-1
   call realloc(jLvec,numtot,fill=0,keepExisting=.false.)
   call realloc(ALvec,numtot,fill=0d0,keepExisting=.false.)
   
!  construct row numbers and fill matrix
   num = 0
!  loop over edges
   do Lf=1,Lnx
!     get start- and endpointer
      istart = iLvec(Lf)
      iend = iLvec(Lf+1)-1
         
!     Div-part: loop over left, right neighboring cell
      dfacDiv = 1d0/Dx(Lf)
      do kk=1,2
!        account for orientation
         dfacDiv = -dfacDiv
         
         k = ln(kk,Lf)
         
         if ( k.eq.0 ) cycle
         
         dfac = dfacDiv/ba(k)
         
!        loop over edges of cell that are flowlinks
         do LL2=1,nd(k)%lnx
            Lf2 = iabs(nd(k)%ln(LL2))
            if ( Lf2.eq.0 ) then
               call mess(LEVEL_ERROR, 'ini_filter: zero link number')
               goto 1234
            end if
            
            if ( k.eq.ln(1,Lf2) ) then
               val = dfac*wu(Lf2)
            else if ( k.eq.ln(2,Lf2) ) then
               val = -dfac*wu(Lf2)
            else
               call mess(LEVEL_ERROR, 'ini-filter: error in div')
            end if
            
!           add element on row
            call add_rowelem(jLvec(istart),ALvec(istart),iend-istart+1,Lf2,val,num(Lf))
         end do
      end do
      
!     Curl-part: loop over left, right netnode
      dfacCurl = -1d0/wu(Lf)
      do nn=1,2
!        account for orientation
         dfacCurl = -dfacCurl
         
!        get netnode number
         n = lncn(nn,Lf)
         
!        boundary condition: curl u = 0
         if ( nb(n).ne.1 ) cycle
         
         if ( n.eq.0 ) then
            call mess(LEVEL_ERROR, 'ini_filter: zero node number')
            goto 1234
         end if
         
         dfac = dfacCurl/(ban(n))
         
!        loop over attached flowlinks
         do LL2=1,nmk(n)
!           get netlink number
            L2 = nod(n)%lin(LL2)
!           get flowlink number
            Lf2 = lne2ln(L2)
            
            if ( Lf2.le.0 ) cycle ! boundary
            
            if ( n.eq.lncn(2,Lf2) ) then
               val = dfac*Dx(Lf2)
            else if( n.eq.lncn(1,Lf2) ) then
               val = -dfac*Dx(Lf2)
            else
               call mess(LEVEL_ERROR, 'ini_filter: error in curl')
            end if
            
!           add row element
            call add_rowelem(jLvec(istart),ALvec(istart),iend-istart+1,Lf2,val,num(Lf))
         end do
      end do
   end do
   
!  remove zeros
   ipoint=1
   istart=iLvec(1)
   do Lf=1,Lnx
      do i=istart,iLvec(Lf+1)-1
         if ( jLvec(i).ne.0 ) then
            jLvec(ipoint) = jLvec(i)
            ALvec(ipoint) = ALvec(i)
            ipoint=ipoint+1
         else
            exit
         end if
      end do
      istart = iLvec(Lf+1)
      iLvec(Lf+1) = ipoint
   end do
   
   N = iLvec(Lnx+1)-1
   call realloc(jLvec,N,keepExisting=.true.)
   call realloc(ALvec,N,keepExisting=.true.)
   
!  prepare solver
   if ( filterorder.eq.1 ) then

   else if ( filterorder.eq.2 .or. filterorder.eq.3 ) then
      allocate(iwork(Lnx))
      call amub_countonly(Lnx,Lnx,ALvec,jLvec,iLvec,ALvec,jLvec,iLvec,iwork,N)
   else
!     not supported
      call mess(LEVEL_ERROR, 'filter: order not supported')
      goto 1234
   end if
   
!  default solver settings
   call SolverSettings(solver_filter, Lnx, N)
   
!  allocate solver arrays
   call allocSolver(solver_filter, ierror)
   if ( ierror.ne.0 ) goto 1234
   
   if ( filterorder.eq.1 ) then
      solver_filter%ia = iLvec
      solver_filter%ja = jLvec
   else if ( filterorder.eq.2 .or. filterorder.eq.3 ) then
!     allocate Lvec2
      call realloc(ALvec2, N, keepExisting=.false., fill=0d0)

!     compute biharmonic operator
      call amub(Lnx, Lnx, 1, ALvec,jLvec,iLvec, ALvec,jLvec,iLvec, ALvec2, solver_filter%ja,solver_filter%ia,N,iwork,ierror)
      if ( ierror.ne.0 ) goto 1234
   end if
   
!  safety: check if diagonal entry exists
Lp:do Lf=1,Lnx
      do j=solver_filter%ia(Lf),solver_filter%ia(Lf+1)-1
         if ( j.eq.Lf ) then
!           diagonal element found
            exit Lp
         end if
      end do
   
!     no diagonal element found
      call mess(LEVEL_ERROR, 'filterfuru: diagonal entry not filled')
      goto 1234
         
   end do Lp
   
!  allocate other arrays
   call realloc(sol, Lnx, keepExisting=.false., fill=0d0)
   call realloc(ustar, Lnkx, keepExisting=.false., fill=0d0)
   call realloc(eps, Lnx, keepExisting=.false., fill=0d0)
   call realloc(Deltax, Lnx, keepExisting=.false., fill=0d0)
   
!  get typical mesh width
   call get_Deltax()
   
   if ( itype.eq.1 ) then
      call realloc(dtmaxeps, Lnx, keepExisting=.false., fill=0d0)
   
!     get maximum time step divided by filter coefficient
      call get_dtmaxeps()
   end if

   if ( jadebug.eq.1 ) then
   
!     get netfile basename
      len = index(md_netfile, '_net')-1

      if ( len.lt.1 .or. len+6.gt.LENFILNAM ) then
         call qnerror('write domains: net filename error', ' ', ' ')
         goto 1234
      end if
   
      FNAM = md_netfile(1:len) // '_flt.m'
      call realloc(num, Lnx+1)
      call realloc(dum, Lnx)
      dum = 1
      do Lf=1,Lnx+1
         num(Lf) = Lf
      end do

      call writematrix(FNAM, Lnx, iLvec, jLvec, ALvec, 'L', 0)
      call writematrix(FNAM, Lnx, solver_filter%ia, solver_filter%ja, ALvec2, 'L2', 1)
      call writematrix(FNAM, Lnx, num, dum, xu, 'xu', 1)
      call writematrix(FNAM, Lnx, num, dum, yu, 'yu', 1)
      call writematrix(FNAM, Lnx, num, dum, csu, 'csu', 1)
      call writematrix(FNAM, Lnx, num, dum, snu, 'snu', 1)
   end if
   
   ierr = 0
1234 continue
   

!  deallocate
!   if ( allocated(num) ) deallocate(num)
!   if ( allocated(dum) ) deallocate(dum)
!   if ( allocated(iLvec) ) deallocate(iLvec)
!   if ( allocated(jLvec) ) deallocate(jLvec)
!   if ( allocated(ALvec) ) deallocate(ALvec)
   if ( allocated(iwork) ) deallocate(iwork)
   
   return
end subroutine ini_filter


! clean-up filter
subroutine dealloc_filter
   use m_filter
   implicit none
   
   call deallocSolver(solver_filter)
   
   if ( allocated(ALvec2) ) deallocate(ALvec2)
   if ( allocated(iLvec) ) deallocate(iLvec)
   if ( allocated(jLvec) ) deallocate(jLvec)
   if ( allocated(ALvec) ) deallocate(ALvec)
   if ( allocated(sol) )   deallocate(sol)
   if ( allocated(ustar) ) deallocate(ustar)
   if ( allocated(eps)   ) deallocate(eps)
   if ( allocated(Deltax)) deallocate(Deltax)
   
   if ( allocated(dtmaxeps) ) deallocate(dtmaxeps)
   if ( allocated(checkmonitor) ) deallocate(checkmonitor)
   if ( allocated(workin) ) deallocate(workin)
   if ( allocated(workout) ) deallocate(workout)
   
   if ( allocated(num) ) deallocate(num)
   if ( allocated(dum) ) deallocate(dum)
   
   return
end subroutine dealloc_filter

!> add element to row
subroutine add_rowelem(jA,A,N,j,val,num)
   use unstruc_messages
   implicit none
   
   integer,                        intent(in)    :: N      !< array length
   integer, dimension(N),          intent(inout) :: jA     !< rownumber array
   double precision, dimension(N), intent(inout) :: A      !< matrix values
   integer,                        intent(in)    :: j      !< rownumber to be inserted
   double precision,               intent(in)    :: val    !< value to be inserted
   integer,                        intent(inout) :: num    !< number of nonzero entries
   
   integer :: i, i2
   
   
   do i=1,N
      if ( jA(i).eq.0 ) then
!        no non-zero array members remaining
         jA(i) = j
         num = num+1
         A(i) = val
         exit
      else if ( jA(i).eq.j ) then
!        entry already exists
         A(i) = A(i) + val
         exit
      else if ( jA(i).gt.j ) then
!        insert entry
         do i2=N,i+1,-1
            jA(i2) = jA(i2-1)
            A(i2) = A(i2-1)
         end do
         jA(i) = j
         A(i) = val
         num = num+1
         exit
      else if ( i.eq.N ) then
!        check for suffucient array size
         call mess(LEVEL_ERROR, 'insert_rownumber: error')
      else
!        proceed to next array member
      end if
   end do
   
   return
end subroutine add_rowelem

!> compute filter predictor (sigma only)
!>  (I-Delta t F) u^* = u^n
subroutine comp_filter_predictor()
   use m_filter
   use m_flowgeom, only: Lnx, Dx
   use m_flow, only: layertype, kmx, fu, ru, u0, hu, plotlin, adve
   use m_flowtimes, only: Dts
   use m_physcoef, only: ag
   use unstruc_messages
   use m_saad, only: jasafe   ! for amux
   implicit none
   
   double precision            :: fac, dsign
   double precision            :: dt
                               
   integer                     :: klay, LL, L
   integer                     :: Lb, Lt
   integer                     :: i, j
   integer                     :: it, Nt
                               
   integer                     :: iters
                               
   integer                     :: japrecond
                              
   integer                     :: jasafe_store
                               
   integer                     :: ierror   ! error (1) or not (0)
   
   double precision, parameter :: facmax = 0.9d0 ! safety factor for maximum allowed sub time step
   
   if ( itype.eq.0 ) return
   
   ierror = 1
   
!  store jasafe
   jasafe_store = jasafe
   
!  force thread safe
   jasafe = 1
   
   dsign = 1d0   ! sign of Lvec2 in matrix
   if ( itype.eq.1 ) then
      dsign = -dsign
   end if
   
   if ( jadebug.eq.1 ) then
      open(2345, file='debug.txt')
   end if
   
   japrecond = 1
   
!  loop over layers
   
   solver_filter%A = 0d0
   ustar = 0d0
   do klay=1,kmx
         
!     get filter coefficient
      call get_filter_coeff(klay)
      
!     compute number of sub time steps and sub time step
      Nt = 1
      dt = dts
      
      if ( itype.eq.1 ) then
!        compute sub time step
         dt = huge(1d0)
         do LL=1,Lnx
            dt = min(dt, dtmaxeps(LL)/max(eps(LL),1e-10))
         end do
         
         dt = facmax * dt
         
         if ( dt.lt.dts ) then
!           get number of sub time steps
            Nt = 1 + floor(dts/dt)
!            write(6,"(I4, ':', I4)") klay, Nt
            dt = dts/Nt
         else
            dt = dts
         end if
      end if
      
!     construct matrix
      do LL=1,Lnx
         call getLbotLtop(LL, Lb, Lt)
!        get 3D link index (sigma only)
         L = Lb + klay-1
         
!        fill right-hand side
         if ( itype.eq.3 ) then
            solver_filter%rhs(LL) = u0(L) - adve(L)*Dt
         else
            solver_filter%rhs(LL) = u0(L)
         end if
         
         if ( order.eq.1 ) then
            fac = -eps(LL) * Dt * dsign
         else if ( order.eq.2 ) then
            fac = eps(LL) * Dt * dsign
         else
            fac = eps(LL) * Dt * dsign
         end if
         plotlin(L) = eps(LL)
            
!!        BEGIN DEBUG
!         if ( itype.eq.1 ) then
!            plotlin(L) = dts/(dtmaxeps(LL)/max(eps(LL),1e-10))
!         else
!            plotlin(L) = 1d0
!         end if
!!        END DEBUG
            
         
!        loop over columns
         do i=solver_filter%ia(LL),solver_filter%ia(LL+1)-1
!           get column number
            j = solver_filter%ja(i)
            
!           add scaled biharmonic operator
            if ( order.eq.1 ) then
               solver_filter%A(i) = fac*ALvec(i)
            else if ( order.eq.2 .or. order.eq.3 ) then
               solver_filter%A(i) = fac*ALvec2(i)
            end if
            
!           add diagonal entry
            if ( j.eq.LL ) then
               solver_filter%A(i) = solver_filter%A(i) + 1d0
            end if
         end do
      end do
      
      if ( jadebug.eq.1 ) then
         call writematrix(FNAM, Lnx, num, dum, solver_filter%rhs, 'rhs', 1)
         call writematrix(FNAM, Lnx, solver_filter%iA, solver_filter%jA, solver_filter%A, 'Fmat', 1)
         jadebug = 0
      end if
      
      if ( itype.eq.1 ) then
!        explicit filter
!        sub time steps
         do it=1,Nt
            call amux(Lnx, solver_filter%rhs, sol, solver_filter%A, solver_filter%jA, solver_filter%iA)
            if ( it.lt.Nt ) then
               solver_filter%rhs = sol
            end if
         end do
      else if ( itype.eq.2 .or. itype.eq.3 ) then
!        solve system for r (reuse preconditioner)
         call solveSystem(solver_filter,sol,japrecond,iters,ierror)
         japrecond = 0
         if ( ierror.ne.0 ) goto 1234
      else
!        unsupported option
         goto 1234
      end if
      
!     copy layer data back to 3D arrays
      do LL=1,Lnx
         call getLbotLtop(LL, Lb, Lt)
!        get 3D link index (sigma only)
         L = Lb + klay-1
         
!        fill layer data
         ustar(L) = sol(LL)
      end do
   end do
   
   ierror = 0
   
1234 continue

!  restore jasafe
   jasafe = jasafe_store

   if ( ierror.ne.0 ) then
      call mess(LEVEL_ERROR, 'filter_furu: error')
      continue
   end if
   
   if ( jadebug.eq.1 ) then
      close(2345)
   end if
   
   return
end subroutine comp_filter_predictor

!> get filter coefficient (sigma only)
subroutine get_filter_coeff(klay)
   use m_flowgeom, only: Lnx, ln, nd, acL, wcx1, wcx2, wcy1, wcy2, csu, snu, Dx, ba
   use m_flow, only: q1, vol1, kmx, vicLu
   use m_filter, only: iLvec, jLvec, ALvec, jadebug, eps, order, Deltax
   implicit none
   
   integer,          intent(in)  :: klay   !< layer number
   
   double precision              :: eps1   ! first-order filter coefficient
   double precision              :: eps2   ! second-order filter coefficient
   double precision              :: eps3   ! third-order filter coefficient
   
   double precision              :: dsign
   double precision              :: Q
   double precision              :: wcx, wcy, w, alpha
   double precision              :: volu
!   double precision              :: Deltax   !< typical mesh width
   double precision              :: vicouv   !< typical viscosity
   double precision              :: maxeps
   double precision              :: dinpr
   
   integer                       :: LL
   integer                       :: Lb, Lt, L
   integer                       :: LL1, iLL1, L1
   integer                       :: kk
   integer                       :: k1, k2
   integer                       :: i, ik, iL, j
   
   integer                       :: ierror
   
   ierror = 1
   
   do LL=1,Lnx
!     get 3D link number (sigma only)
      call getLbotLtop(LL,Lb,Lt)
      L = Lb+klay-1
      
!     compute first-order filter coefficient
      eps1 = 0d0
!      Deltax = Dx(LL)
      vicouv = vicLu(L)
      
!     get advection volume
      k1 = ln(1,L)
      k2 = ln(2,L)
      
      volu  = acL(LL)*vol1(k1) + (1d0-acL(LL))*vol1(k2)
            
!     loop over left/right neighboring cell
      do ik=1,2
         kk=ln(ik,LL)
         
         alpha = acL(LL)
         if ( ik.eq.2 ) then
            alpha = 1d0-acL(LL)
         end if
         
!        loop over links in cell
         do iL=1,nd(kk)%lnx
            iLL1 = nd(kk)%ln(iL)
            LL1 = iabs(iLL1)
            
!           exclude self
            if ( LL1.eq.LL ) then
               cycle
            end if
            
!           update typical viscosity
!            vicouv = max(vicouv, vicLu(L))
            
!           get orientation of link
            dsign = 1d0 ! outward
            wcx = wcx1(LL1)
            wcy = wcy1(LL1)
            if ( ln(2,LL1).eq.kk) then
               dsign = -1d0   ! inward
               wcx = wcx2(LL1)
               wcy = wcy2(LL1)
            end if
            
!           get 3D link number (sigma only)
            call getLbotLtop(LL1,Lb,Lt)
            L1 = Lb+klay-1
            
!           get outward positive flux
            Q = q1(L1)*dsign
            
!           outflowing only
            if ( Q.le.0d0 ) then
               cycle
            else
               continue
            end if
            
!           compute weight of this link (L1) in advection of link L
            w = (wcx*csu(LL)+wcy*snu(LL)) * alpha / volu
            
            w = w*Q
            
!           compare with vector Laplacian
            do i=iLvec(LL),ILvec(LL+1)-1
               j=jLvec(i)
               if ( j.eq.LL1 ) exit
            end do
            
            if ( j.ne.LL1 ) then ! safety
               goto 1234
            end if
            
            if ( ALvec(i).ne.0d0 ) then
               eps1 = max(eps1, w/ALvec(i))
            else
               continue
            end if
            
            if ( w*ALvec(i).gt.0d0 .and. Q.lt.0d0 ) then
               continue
            end if
            
         end do
      end do
      
!     BEGIN DEBUG
!      Deltax = 1d0
!     END DEBUG
      
!     compute third-order filter coefficient from first-order filter coefficient
      eps3 = 0.25 * Deltax(LL)**2 * eps1
      
!     compute second-order filter coefficient
      eps2 = max(eps3, 0.25 * Deltax(LL)**2 * vicouv)
      
      
!     BEGIN DEBUG
!      eps2 = max(eps3, 0.25 * Deltax**2 * max(vicouv,1d0))
!     END DEBUG
      
      if ( order.eq.1 ) then
         eps(LL) = eps1
      else if ( order.eq.2 ) then
         eps(LL) = eps2
      else if ( order.eq.3 ) then
         eps(LL) = eps3
      end if
   end do
   
!!  BEGIN DEBUG
!   maxeps = 0d0
!   do LL=1,Lnx
!      maxeps = max(maxeps, eps(LL))
!   end do
!   
!   eps = maxeps
!!  END DEBUG
   
   ierror = 0
1234 continue
   
   return
end subroutine get_filter_coeff

!> get maximum filter time step mulitplied with filter coefficient
subroutine get_dtmaxeps()
   use m_flowgeom, only: Lnx
   use m_filter
   implicit none
   
   double precision              :: diag, offdiag
   
   integer                       :: i, j
   integer                       :: L
   
   integer                       :: ierror   ! error (1) or not (0)
   
   ierror = 1
   
   dtmaxeps = huge(1d0)
   
   if ( order.eq.2 .or. order.eq.3 ) then
      do L=1,Lnx
         diag = 0d0
         offdiag = 0d0
         
         do i=solver_filter%ia(L),solver_filter%ia(L+1)-1
!           get column index
            j = solver_filter%ja(i)
            
            if ( j.eq.L ) then
!              get diagonal entry
               diag = -ALvec2(i)
            else
!              sum off diagonal entries
               offdiag = offdiag + abs(ALvec2(i))
            end if
         end do
            
         if ( offdiag.gt.0d0 ) then
!           update maximum time step
            dtmaxeps(L) = min(dtmaxeps(L), 1d0/offdiag)
            dtmaxeps(L) = min(dtmaxeps(L), 2d0/(-diag + offdiag))
         else
!           error
            goto 1234
         end if
      end do
   end if
   
   ierror = 0
1234 continue
   
   return
end subroutine get_dtmaxeps

!> determine typical mesh width
subroutine get_Deltax()
   use m_flowgeom, only: Dx, csu, snu, Lnx
   use m_filter
   implicit none
   
   double precision            :: dinpr
                               
   integer                     :: L, L1
   integer                     :: j
   
   double precision, parameter :: dtol = 1d-8
   
   do L=1,Lnx
      Deltax(L) = Dx(L)
      
!     get other links in stencil
      do j=solver_filter%ia(L),solver_filter%ia(L+1)-1
         L1 = solver_filter%ja(j)
         
!        exclude self
         if ( L1.eq.L ) cycle
         
!        account for orientation
         dinpr = abs(csu(L)*csu(L1) + snu(L)*snu(L1))
         
!        update typical mesh width
         if ( dinpr.gt.dtol ) then
            Deltax(L) = min(Deltax(L),  Dx(L1)/dinpr)
         end if
      end do
   end do
   
   return
end subroutine get_Deltax

!> compute "checkerboard" mode monitor
subroutine comp_checkmonitor()
   use m_flowgeom, only: Lnx, Dx, wu, ba, ln
   use m_flow, only: qw, kmx
   use m_turbulence, only: ln0
   use m_filter
   use m_partitioninfo
   implicit none
   
   double precision                              :: area
   integer                                       :: kk1, kk2, k1, k2
   integer                                       :: Ll, L, Lb, Lt
   integer                                       :: klay
   integer                                       :: jaghost, idmn_link
   
   checkmonitor = 0d0
   area = 0d0
   
   do LL=1,Lnx
!     get neighboring 2D cells
      kk1 = ln(1,LL)
      kk2 = ln(2,LL)
      
      
      if ( jampi.eq.1 ) then
!        determine if link is ghost or not
         call link_ghostdata(my_rank,idomain(kk1),idomain(kk2),jaghost,idmn_link)
         if ( jaghost.eq.1 ) then
!           exclude ghost links
            cycle
         end if
      end if
      
      call getLbotLtop(LL,Lb,Lt)
      do klay=1,kmx
!        get 3D link number (sigma only)
         L = Lb+klay-1
         
!        get neighboring 3D cells
         k1 = ln0(1,L)
         k2 = ln0(2,L)
         
!        add to monitor
         checkmonitor(klay) = checkmonitor(klay) + abs( qw(k2)/ba(kk2) - qw(k1)/ba(kk1)) * 0.5d0*wu(LL)
      end do
      area = area + 0.5d0*Dx(LL)*wu(LL)
   end do
   
   if ( jampi.eq.1 ) then
      workin(1:kmx) = checkmonitor(1:kmx)
      workin(kmx+1) = area
      call reduce_double_sum(kmx+1,workin,workout)
      checkmonitor(1:kmx) = workout(1:kmx)
      area = workout(kmx+1)
   end if
   
   do klay=1,kmx
      checkmonitor(klay) = checkmonitor(klay) / area
   end do
   
   return
end subroutine comp_checkmonitor