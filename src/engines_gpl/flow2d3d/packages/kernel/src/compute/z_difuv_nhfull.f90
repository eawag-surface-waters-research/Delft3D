subroutine z_difuv_nhfull(lundia   ,nst       ,icx       ,icy       ,j         , &
                        & nmmaxj   ,nmmax     ,kmax      ,lstsci     ,norow    , nocol   , &
                        & irocol   ,kcs       ,kcu       ,kcv       ,kfs       , &
                        & kfsmin   ,kfsmax    ,kfsmx0    ,kfumin    ,kfumx0    , &
                        & kfvmin   ,kfvmx0    ,kfsz1     ,kfuz1     ,kfvz1     , &
                        & qxk      ,qyk       ,qzk       ,u         ,v         , &
                        & guv      ,gvu       ,gsqs      ,rbnd      ,sigdif    , &
                        & sigmol   ,dicuv     ,vicww     ,r0        ,r1        , &
                        & sour     ,sink      ,aak       ,bbk       ,cck       , &
                        & aakl     ,bbkl      ,cckl      ,ddkl      ,dzs0      ,dzs1      , &
                        & dzu0     ,dzv0      ,areau     ,areav     ,volum0    , &
                        & volum1   ,guu       ,gvv       ,bruvai    , &
                        & gdp      )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!!--description-----------------------------------------------------------------
!
! Computes transport in the u, v and w-direction.
! Implicit w-direction, explicit in u- and v-direction.
! Sinks are treated implicitly and sources explicitly
! A special approach is used for the horizontal diffusion to avoid artificial
! creeping.
! - Special approach for top layer.
! - Horizontal Advection in U-direction :
!      Van Leer 2, non linear approach (explicit)
! - Horizontal Advection in V-direction :
!      Van Leer 2, non linear approach (explicit)
! - Vertical Advection, first order upwind (impl)
! - Horizontal Diffusion :
!     2D and 3D: explicit, along Z-planes
! - Option: horizontal diffusion strictly horizontal using special filter
! - Vertical Diffusion : implicitly
! - Sources are integrated explicitly.
! - Sinks are integrated implicitly.
!
! COMMENT:
! For the Thatcher Harlemann boundaries the boundary points for outflow
! are reflected from the inner points; for inflow the boundary conditions are
! used (see also thahbc.for).
!
! NOTE:
! AREAU/V were set using DZU0/V0 in Z_SUD as required by Z_DIFHOR

!
!!--pseudo code and references--------------------------------------------------
!
! Written by Sebastian Ullmann
!
! "A comparison of two 3D shallow water models using sigma coordinates and
!  z-coordinates in the vertical direction."
! Bijvelds, Van Kester and Stelling.
!
!!--declarations----------------------------------------------------------------
    use precision
    use timers
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
    include 'flow_steps_f.inc'
    real(fp) , pointer :: vicmol
    real(fp) , pointer :: xlo
    real(fp) , pointer :: hdt
    logical  , pointer :: nonhyd
!
! Global variables
!
    integer                                                               :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                               :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                               :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                                               :: kmax   !  Description and declaration in iidim.f90
    integer                                                               :: lstsci !  Description and declaration in iidim.f90
    integer                                                               :: lundia !  Description and declaration in inout.igs
    integer                                                               :: nmmax  !  Description and declaration in dimens.igs
    integer                                                               :: nmmaxj !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: nocol  !  Description and declaration in iidim.f90
    integer                                                 , intent(in)  :: norow  !  Description and declaration in iidim.f90
    integer                                                               :: nst
    integer , dimension(5, norow+nocol)                     , intent(in)  :: irocol !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kcs    !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcu    !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcv    !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfs    !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmax !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmin !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmx0 !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfumin !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfumx0 !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfvmin !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfvmx0 !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: kfsz1  !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: kfuz1  !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: kfvz1  !  Description and declaration in iidim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gsqs   !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guu    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guv    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvu    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvv    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: bruvai !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: vicww  !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: qzk    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: aak    !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: bbk    !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: cck    !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)                    :: dicuv  !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: areau  !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: areav  !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzs0   !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzs1   !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: volum0 !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: volum1 !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: dzu0   !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: dzv0   !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: qxk    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: qyk    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: u
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: v
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: aakl   !!  Internal work array, lower diagonal tridiagonal matrix, implicit coupling of concentration in (N,M,K) with concentration in (N,M,K-1)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: bbkl   !!  Internal work array, main diagonal  tridiagonal matrix, implicit coupling of concentration in (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: cckl   !!  Internal work array, upper diagonal tridiagonal matrix, implicit coupling of concentration in (N,M,K) with concentration in (N,M,K+1)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: ddkl   !!  Internal work array, diagonal space at (N,M,K,L)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: r0     !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: r1     !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in)  :: sink   !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in)  :: sour   !  Description and declaration in rjdim.f90
    real(fp), dimension(kmax, max(lstsci, 1), 2, norow+nocol),intent(in)  :: rbnd   !  Description and declaration in rjdim.f90
    real(fp), dimension(lstsci)                                           :: sigdif !  Description and declaration in rjdim.f90
    real(fp), dimension(lstsci)                             , intent(in)  :: sigmol !  Description and declaration in rjdim.f90
!
! Local variables
!
    integer            :: ddb, m
    integer            :: ic
    integer            :: icxy
    integer            :: k
    integer            :: kfw
    integer            :: kfsum
    integer            :: kmin
    integer            :: ksm
    integer            :: l
    integer            :: mf, nf
    integer            :: mink
    integer            :: ml, nl
    integer            :: n
    integer            :: ndm
    integer            :: nhystp
    integer            :: nm
    integer            :: nmd
    integer            :: nmf, nfm
    integer            :: nml, nlm
    integer            :: nmlu, nlum
    integer            :: nmu
    integer            :: nmuu
    integer            :: num
    integer            :: nuum
    real(fp)           :: adza
    real(fp)           :: adzc
    real(fp)           :: bi
    real(fp)           :: cfl
    real(fp)           :: ddzc
    real(fp)           :: delz
    real(fp)           :: difiwe
    real(fp)           :: diz1
    real(fp)           :: timest
    real(fp)           :: flux
    real(fp)           :: qzw
    real(fp)           :: r00
    real(fp), external :: reddic
    real(fp)           :: rmax
    real(fp)           :: rmin
    real(fp)           :: rscal
    real(fp)           :: sqrtbv
!
!! executable statements -------------------------------------------------------
!
    vicmol  => gdp%gdphysco%vicmol
    xlo     => gdp%gdturcoe%xlo
    nonhyd  => gdp%gdprocs%nonhyd
    hdt     => gdp%gdnumeco%hdt
    !
    if (lstsci == 0) goto 9999
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    if (nonhyd) then
       timest = 2.0_fp * hdt
    else
       timest = hdt
    endif
    do l = 1, lstsci
       do nm = 1, nmmax
          if (kfs(nm) == 1) then
             do k = 1, kmax
                aak (nm, k)    = 0.0_fp
                bbk (nm, k)    = 0.0_fp
                cck (nm, k)    = 0.0_fp
                aakl(nm, k, l) = 0.0_fp
                bbkl(nm, k, l) = 0.0_fp
                cckl(nm, k, l) = 0.0_fp
                ddkl(nm, k, l) = 0.0_fp
                bbkl(nm, k, l) = 1.0_fp
             enddo
          endif
       enddo
       !
       ! system of difference equations
       !
       do nm = 1, nmmax
          if (kfs(nm) == 1) then
             kmin = min(kfsmax(nm), kfsmx0(nm))
             if (kfsmax(nm)==kfsmx0(nm) .or. kmax==1) then
                do k = kfsmin(nm), kfsmax(nm)
                   bbkl(nm, k, l) = volum1(nm, k)/timest
                   aakl(nm, k, l) = 0.0_fp
                   cckl(nm, k, l) = 0.0_fp
                   ddkl(nm, k, l) = volum0(nm, k)*r0(nm, k, l)/timest
                enddo
             elseif (kfsmax(nm) > kfsmx0(nm)) then
                do k = kfsmin(nm), kmin
                   bbkl(nm, k, l) = volum1(nm, k)/timest
                   aakl(nm, k, l) = 0.0_fp
                   cckl(nm, k, l) = 0.0_fp
                   ddkl(nm, k, l) = volum0(nm, k)*r0(nm, k, l)/timest
                enddo
                do k = kmin + 1, kfsmax(nm)
                   bbkl(nm, kmin, l) = bbkl(nm, kmin, l) + volum1(nm, k)/timest
                   aakl(nm, k, l) = 0.0_fp
                   bbkl(nm, k, l) = 1.0_fp
                   cckl(nm, k, l) = 0.0_fp
                   ddkl(nm, k, l) = 0.0_fp
                enddo
             else
                do k = kfsmin(nm), kmin
                   bbkl(nm, k, l) = volum1(nm, k)/timest
                   aakl(nm, k, l) = 0.0_fp
                   cckl(nm, k, l) = 0.0_fp
                   ddkl(nm, k, l) = volum0(nm, k)*r0(nm, k, l)/timest
                enddo
                do k = kmin + 1, kfsmx0(nm)
                   bbkl(nm, k, l) = 1.0_fp
                   aakl(nm, k, l) = 0.0_fp
                   cckl(nm, k, l) = 0.0_fp
                   ddkl(nm, k, l) = 0.0_fp
                   ddkl(nm, kmin, l) = ddkl(nm, kmin, l) + volum0(nm, k)*r0(nm, k, l)/timest
                enddo
             endif
          endif
       enddo
    enddo
    do nm = 1, nmmax
       !
       ! HORIZONTAL ADVECTION IN X-DIRECTION
       !
       nmd = nm - icx
       nmu = nm + icx
       nmuu = nmu + icx
       if (kfs(nm)*kfs(nmu) /= 0) then
          kmin = max(kfumin(nm), 1)
          do k = kmin, kmax
             cfl = u(nm,k) * timest / gvu(nm)
             if (qxk(nm,k) > 0.0_fp) then
                do l = 1, lstsci
                   rmax = max(r0(nmd,k,l) , r0(nmu,k,l))
                   rmin = min(r0(nmd,k,l) , r0(nmu,k,l))
                   if (  kfsz1(nmd, k)*kfsz1(nmu, k) == 0    &
                       & .or. r0(nm,k,l)             >= rmax &
                       & .or. r0(nm,k,l)             <  rmin  ) then
                      r00 = r0(nm,k,l)
                   else
                      r00 = r0(nm,k,l)                                 &
                          & + (1.0_fp-cfl) * (r0(nm ,k,l)-r0(nmd,k,l)) &
                          &                * (r0(nmu,k,l)-r0(nm ,k,l)) &
                          &                / (r0(nmu,k,l)-r0(nmd,k,l))
                   endif
                   flux = qxk(nm,k) * r00
                   if (kcs(nm) == 1) then
                      ddkl(nm,k,l) = ddkl(nm,k,l) - flux
                   endif
                   if (kcs(nm+icx) == 1) then
                      ddkl(nm+icx,k,l) = ddkl(nm+icx,k,l) + flux
                   endif
                enddo
             else
                do l = 1, lstsci
                   rmax = max(r0(nm,k,l) , r0(nmuu,k,l))
                   rmin = min(r0(nm,k,l) , r0(nmuu,k,l))
                   if (  kfsz1(nm, k)*kfsz1(nmuu, k) == 0    &
                       & .or. r0(nmu,k,l)            >= rmax &
                       & .or. r0(nmu,k,l)            <  rmin  ) then
                      r00 = r0(nmu,k,l)
                   else
                      r00 = r0(nmu,k,l)                                 &
                          & + (1.0_fp+cfl) * (r0(nm ,k,l)-r0(nmu ,k,l)) &
                          &                * (r0(nmu,k,l)-r0(nmuu,k,l)) &
                          &                / (r0(nm ,k,l)-r0(nmuu,k,l))
                   endif
                   flux = qxk(nm,k) * r00
                   if (kcs(nm) == 1) then
                      ddkl(nm,k,l) = ddkl(nm,k,l) - flux
                   endif
                   if (kcs(nm+icx) == 1) then
                      ddkl(nm+icx,k,l) = ddkl(nm+icx,k,l) + flux
                   endif
                enddo
             endif
          enddo
       endif
       !
       ! HORIZONTAL ADVECTION IN Y-DIRECTION
       !
       ndm = nm - icy
       num = nm + icy
       nuum = num + icy
       if (kfs(nm)*kfs(num) /= 0) then
          kmin = max(kfvmin(nm) , 1)
          do k = kmin, kmax
             cfl = v(nm,k) * timest / guv(nm)
             if (qyk(nm,k) > 0.0_fp) then
                do l = 1, lstsci
                   rmax = max(r0(ndm,k,l) , r0(num,k,l))
                   rmin = min(r0(ndm,k,l) , r0(num,k,l))
                   if (  kfsz1(ndm,k)*kfsz1(num,k) == 0    &
                       & .or. r0(nm,k,l)           >= rmax &
                       & .or. r0(nm,k,l)           <  rmin  ) then
                      r00 = r0(nm,k,l)
                   else
                      r00 = r0(nm,k,l)                                 &
                          & + (1.0_fp-cfl) * (r0(nm ,k,l)-r0(ndm,k,l)) &
                          &                * (r0(num,k,l)-r0(nm ,k,l)) &
                          &                / (r0(num,k,l)-r0(ndm,k,l))
                   endif
                   flux = qyk(nm,k) * r00
                   if (kcs(nm) == 1) then
                      ddkl(nm,k,l) = ddkl(nm,k,l) - flux
                   endif
                   if (kcs(nm+icy) == 1) then
                      ddkl(nm+icy,k,l) = ddkl(nm+icy,k,l) + flux
                   endif
                enddo
             else
                do l = 1, lstsci
                   rmax = max(r0(nm,k,l) , r0(nuum,k,l))
                   rmin = min(r0(nm,k,l) , r0(nuum,k,l))
                   if (  kfsz1(nm,k)*kfsz1(nuum,k) == 0    &
                       & .or. r0(num,k,l)          >= rmax &
                       & .or. r0(num,k,l)          <  rmin  ) then
                      r00 = r0(num,k,l)
                   else
                      r00 = r0(num,k,l)                                 &
                          & + (1.0_fp+cfl) * (r0(nm ,k,l)-r0(num ,k,l)) &
                          &                * (r0(num,k,l)-r0(nuum,k,l)) &
                          &                / (r0(nm ,k,l)-r0(nuum,k,l))
                   endif
                   flux = qyk(nm,k) * r00
                   if (kcs(nm) == 1) then
                      ddkl(nm,k,l) = ddkl(nm,k,l) - flux
                   endif
                   if (kcs(nm+icy) == 1) then
                      ddkl(nm+icy,k,l) = ddkl(nm+icy,k,l) + flux
                   endif
                enddo
             endif
          enddo
       endif
    enddo
    !
    ! summation of fluxes for cells which do not have a horizontal neighbour cell
    !
    do l = 1, lstsci
       do nm = 1, nmmax
          if (kfs(nm) /= 0) then
             kmin = min(kfsmax(nm) , kfsmx0(nm))
             do k = kmin+1, kmax
                ddkl(nm,kmin,l) = ddkl(nm,kmin,l) + ddkl(nm,k,l)
             enddo
          endif
       enddo
    enddo
    !
    ! diffusion in horizontal direction (explicit)
    !
    call z_difhor_nhfull(j         ,nmmaxj    ,kmax      ,lstsci    ,nmmax     , &
                       & icx       ,icy       ,kcs       ,kfuz1     ,kfvz1     , &
                       & kfsz1     ,kfumin    ,kfumx0    ,kfvmin    ,kfvmx0    , &
                       & dicuv     ,sigdif    ,dzu0      ,dzv0      ,areau     , &
                       & areav     ,guv       ,gvu       ,r0        ,ddkl      , &
                       & gdp      )
    do l = 1, lstsci
       !
       ! SOURCES AND SINK TERMS
       !
       !
       ! SINKS ARE TREATED IMPLICITLY
       !
       do nm = 1, nmmax
          if (kfs(nm)*kcs(nm) == 1) then
             if (kfsmax(nm)==kfsmx0(nm) .or. kmax==1) then
                do k = kfsmin(nm), kfsmax(nm)
                   bbkl(nm,k,l) = bbkl(nm,k,l) + sink(nm,k,l)
                   ddkl(nm,k,l) = ddkl(nm,k,l) + sour(nm,k,l)
                enddo
             else
                kmin = min(kfsmax(nm) , kfsmx0(nm))
                do k = kfsmin(nm), kmin
                   bbkl(nm,k,l) = bbkl(nm,k,l) + sink(nm,k,l)
                   ddkl(nm,k,l) = ddkl(nm,k,l) + sour(nm,k,l)
                enddo
                if (kfsmax(nm) > kfsmx0(nm)) then
                   do k = kmin+1, kfsmax(nm)
                      bbkl(nm,kmin,l) = bbkl(nm,kmin,l) + sink(nm,k,l)
                      ddkl(nm,kmin,l) = ddkl(nm,kmin,l) + sour(nm,k,l)
                   enddo
                endif
             endif
          endif
       enddo
       do nm = 1, nmmax
          ksm = min(kfsmx0(nm) , kfsmax(nm))
          if (kfs(nm) == 1) then
            do k = kfsmin(nm), ksm-1
               if (k==kfsmin(nm) .or. k==ksm-1) then
                  kfw = 1
               else
                  kfw = 0
               endif
               !
               ! ADVECTION IN VERTICAL DIRECTION; W*DU/DZ
               !
               ! second order central
               !
               if (kfs(nm) == 1) then
                  qzw = qzk(nm,k)
                  if (qzw<0.0_fp) then
                     adza = 0.5_fp * qzw * (1-kfw)
                     adzc = 0.5_fp * qzw * (1+kfw)
                  else
                     adza = 0.5_fp * qzw * (1+kfw)
                     adzc = 0.5_fp * qzw * (1-kfw)
                  endif
                  aakl(nm,k+1,l) = aakl(nm,k+1,l) - adza
                  bbkl(nm,k+1,l) = bbkl(nm,k+1,l) - adzc
                  bbkl(nm,k  ,l) = bbkl(nm,k  ,l) + adza
                  cckl(nm,k  ,l) = cckl(nm,k  ,l) + adzc
               endif
            enddo
          endif
       enddo
       !
       ! DIFFUSION IN VERTICAL DIRECTION
       !
       do nm = 1, nmmax
          ksm = min(kfsmx0(nm) , kfsmax(nm))
          if (kfs(nm) == 1) then
            do k = kfsmin(nm), ksm-1
               delz = max(0.1_fp, 0.5_fp*(dzs1(nm,k)+dzs1(nm,k+1)))
               !
               ! Internal wave contribution
               !
               sqrtbv = max(0.0_fp, bruvai(nm,k))
               sqrtbv = sqrt(sqrtbv)
               difiwe = 0.2_fp * sqrtbv * xlo**2
               !
               ! dicoww-restriction is moved from TURCLO to here (in reddic)
               ! vicww is used instead of dicww
               !
               diz1 = vicmol/sigmol(l) + reddic(difiwe+vicww(nm,k)/sigdif(l),gdp)
               ddzc = gsqs(nm) * diz1 / delz
               aakl(nm,k+1,l) = aakl(nm,k+1,l) - ddzc
               bbkl(nm,k+1,l) = bbkl(nm,k+1,l) + ddzc
               bbkl(nm,k  ,l) = bbkl(nm,k  ,l) + ddzc
               cckl(nm,k  ,l) = cckl(nm,k  ,l) - ddzc
            enddo
          endif
       enddo
       !
       ! set values in open boundary points (in part. for y-direction)
       !
       do nm = 1, nmmax
          if (kcs(nm) == 2) then
             do k = kfsmin(nm) , kfsmax(nm)
                ddkl(nm,k,l) = r0(nm,k,l)
                aakl(nm,k,l) = 0.0_fp
                bbkl(nm,k,l) = 1.0_fp
                cckl(nm,k,l) = 0.0_fp
             enddo
          endif
       enddo
       !
       ! IMPLEMENTATION OF BOUNDARY CONDITIONS IN X-DIRECTION
       !
       do ic = 1, norow
          n    = irocol(1,ic)
          mf   = irocol(2,ic) - 1
          ml   = irocol(3,ic)
          nmf  = (n+ddb)*icy + (mf+ddb)*icx - icxy
          nml  = (n+ddb)*icy + (ml+ddb)*icx - icxy
          nmlu = nml + icx
          if (kcu(nmf) == 1) then
             do k = kfsmin(nmf), kfsmax(nmf)
                ddkl(nmf,k,l) = rbnd(k,l,1,ic)
                aakl(nmf,k,l) = 0.0_fp
                bbkl(nmf,k,l) = 1.0_fp
                cckl(nmf,k,l) = 0.0_fp
             enddo
          endif
          if (kcu(nml) == 1) then
             do k = kfsmin(nmlu), kfsmax(nmlu)
                ddkl(nmlu,k,l) = rbnd(k,l,2,ic)
                aakl(nmlu,k,l) = 0.0_fp
                bbkl(nmlu,k,l) = 1.0_fp
                cckl(nmlu,k,l) = 0.0_fp
             enddo
          endif
       enddo
       !
       ! IMPLEMENTATION OF BOUNDARY CONDITIONS IN Y-DIRECTION
       !
       do ic = norow+1, norow+nocol
          m    = irocol(1,ic)
          nf   = irocol(2,ic) - 1
          nl   = irocol(3,ic)
          !
          ! WATCH OUT: icx and icy are swapped
          nfm  = (m+ddb)*icx + (nf+ddb)*icy - icxy
          nlm  = (m+ddb)*icx + (nl+ddb)*icy - icxy
          nlum = nlm + icy
          if (kcv(nfm) == 1) then
             do k = kfsmin(nfm), kfsmax(nfm)
                ddkl(nfm,k,l) = rbnd(k,l,1,ic)
                aakl(nfm,k,l) = 0.0_fp
                bbkl(nfm,k,l) = 1.0_fp
                cckl(nfm,k,l) = 0.0_fp
             enddo
          endif
          if (kcv(nlm) == 1) then
             do k = kfsmin(nlum), kfsmax(nlum)
                ddkl(nlum,k,l) = rbnd(k,l,2,ic)
                aakl(nlum,k,l) = 0.0_fp
                bbkl(nlum,k,l) = 1.0_fp
                cckl(nlum,k,l) = 0.0_fp
             enddo
          endif
       enddo
       !
       !   set concentrations in temporary dry points
       !
       do nm = 1, nmmax
          if (kfs(nm)==0 .and. kcs(nm)==1) then
             do k = kfsmin(nm), kmax
                r1(nm,k,l) = r0(nm,k,l)
             enddo
          endif
       enddo
       do nm = 1, nmmax
          if (kcs(nm) == 3) then
             !
             ! left hand-side is now set by Delft3D-FLOW instead of the mapper
             !
             do k = 1, kmax
                aakl(nm,k,l) = 0.0_fp
                bbkl(nm,k,l) = 1.0_fp
                cckl(nm,k,l) = 0.0_fp
                ddkl(nm,k,l) = r0(nm,k,l)
             enddo
          else
             !
             !***SCALE ROWS OF MATRIX/RIGHT HAND SIDE VECTOR
             !
             do k = 1, kmax
                rscal        = 1.0_fp / bbkl(nm,k,l)
                aakl(nm,k,l) = aakl(nm,k,l) * rscal
                bbkl(nm,k,l) = 1.0_fp
                cckl(nm,k,l) = cckl(nm,k,l) * rscal
                ddkl(nm,k,l) = ddkl(nm,k,l) * rscal
             enddo
          endif
       enddo
       !
       ! D3dFlow_Build_ADI_Conc: poke the coupling equations into system
       !
       nhystp = nxtstp(d3dflow_build_adi_conc,gdp)
       !
       ! DD code added end
       !
       !
       ! SOLUTION PROCEDURE SYSTEM OF EQUATIONS FOR ADVECTION
       !
       do nm = 1, nmmax
          if (kfs(nm) /= 0) then
             mink = kfsmin(nm)
             kmin = min(kfsmx0(nm) , kfsmax(nm))
             bi = 1.0_fp / bbkl(nm,mink,l)
             cckl(nm,mink,l) = cckl(nm,mink,l) * bi
             ddkl(nm,mink,l) = ddkl(nm,mink,l) * bi
             do k = mink+1, kmin
                bi             = 1.0_fp / (bbkl(nm,k,l)-aakl(nm,k,l)*cckl(nm,k-1,l))
                cckl(nm,k,l) = cckl(nm,k,l) * bi
                ddkl(nm,k,l) = (ddkl(nm,k,l)-aakl(nm,k,l)*ddkl(nm,k-1,l)) * bi
             enddo
          endif
       enddo
       !
       ! DD code added:
       !
       !
       ! (re)solve system of equations
       !
  111 continue
       gdp%dd%difuiter = gdp%dd%difuiter + 1
       !
       ! DD code added end
       !
       !
       ! back sweep
       !
       do nm = 1, nmmax
          nmd = nm - icx
          ndm = nm - 1
          if (kfs(nm) /= 0) then
             mink          = kfsmin(nm)
             kmin          = min(kfsmx0(nm) , kfsmax(nm))
             r1(nm,kmin,l) = ddkl(nm,kmin,l)
             do k = kmin-1, mink, -1
                r1(nm,k,l) = ddkl(nm,k,l) - cckl(nm,k,l)*r1(nm,k+1,l)
             enddo
             if (kfsmx0(nm) > kfsmax(nm)) then
                do k = kmin+1, kmax
                   kfsum =   kfuz1(nm,k) + kfuz1(nmd,k) &
                         & + kfvz1(nm,k) + kfvz1(ndm,k)
                   if (kfsum == 0) then
                      r1(nm,k,l) = 0.0_fp
                   endif
                enddo
             elseif (kfsmx0(nm) < kfsmax(nm)) then
                do k = kmin+1, kmax
                   r1(nm,k,l) = r1(nm,kmin,l)
                   kfsum =   kfuz1(nm,k) + kfuz1(nmd,k) &
                         & + kfvz1(nm,k) + kfvz1(ndm,k)
                   if (k>kfsmax(nm) .and. kfsum == 0) then
                      r1(nm,k,l) = 0.0_fp
                   endif
                enddo
             else
             endif
          endif
       enddo
       !
       ! DD code added:
       !
       !
       ! D3dFlow_Solve_ADI_Conc: Check for convergence
       !
       nhystp = nxtstp(d3dflow_solve_adi_conc,gdp)
       if (nhystp == d3dflow_solve_adi_conc) goto 111
       !
       ! DD code added end
       !
    enddo
 9999 continue
    do l = 1, lstsci
       do nm = 1, nmmax
          if (kfs(nm) /= 0) then
             do k = kfsmin(nm), kfsmax(nm)
                if (abs(r1(nm,k,l)-r0(nm,k,l) ) > 10.0_fp) then
                  call nm_to_n_and_m(nm, n, m, gdp)
                  write (lundia,'(a,5i5,2f10.3)') 'z_difu', &
                        & nm,m,n,k,l,r1(nm,k,l),r0(nm,k,l)
                endif
             enddo
          endif
       enddo
    enddo
end subroutine z_difuv_nhfull
