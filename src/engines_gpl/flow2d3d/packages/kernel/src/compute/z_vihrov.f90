subroutine z_vihrov(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                  & icy       ,kcs45     ,kcs       ,kfu       ,kfv       , &
                  & kfs       ,u0        ,v         ,vicuv     ,vnu2d     , &
                  & gud       ,guu       ,gvd       ,gvu       ,gvz       , &
                  & ddk       ,rxx       ,rxy       ,kfuz1     ,kfvz1     , &
                  & kfsz1     ,kfumin    ,kfumax    ,gdp       )
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
!  $Id$
!  $HeadURL$
!!--description-----------------------------------------------------------------
!
! Z-layer model
! - roughness (partial slip) of rigid walls
! - blockage flow by rigid sheets
! - 2D turbulence model at depth points
! - computes shear stress for HLES-model
!   Special approximation pressure term, based
!   on limiter to avoid artificial flow.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: z0
    real(fp)               , pointer :: z0v
    real(fp)               , pointer :: vonkar
    integer                , pointer :: iro
    integer                , pointer :: irov
    real(fp)               , pointer :: hdt
!
! Global variables
!
    integer                                        , intent(in) :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                          !!  then computation proceeds in the X-
                                                                          !!  dir. If icx=1 then computation pro-
                                                                          !!  ceeds in the Y-dir.
    integer                                        , intent(in) :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                        , intent(in) :: j      !!  Begin pointer for arrays which have
                                                                          !!  been transformed into 1D arrays.
                                                                          !!  Due to the shift in the 2nd (M-)
                                                                          !!  index, J = -2*NMAX + 1
    integer                                        , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                        , intent(in) :: nmmax  !  Description and declaration in dimens.igs
    integer                                        , intent(in) :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: kcs45
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                   :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                   :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfumax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                   :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: kfsz1  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: kfuz1  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: kfvz1  !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gud    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvd    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvz    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: vnu2d  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ddk    !!  Internal work array, diagonal space
                                                                          !!  at (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rxx    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rxy    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: v      !!  V-velocities at new time level
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)              :: vicuv  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: k      ! Loop variable 
    integer :: ki     ! Mask value 
    integer :: ndm    ! NM - ICY 
    integer :: ndmd   ! NM - ICX - ICY
    integer :: nm     ! Loop variable 
    integer :: nmd    ! NM - ICX 
    integer :: nmu    ! NM + ICX 
    integer :: num    ! NM + ICY 
    integer :: numu   ! NM + ICX + ICY 
    real(fp):: dy     ! Distance in Y dir. in depth point 
    real(fp):: dx     ! Distance in X dir. in depth point 
    real(fp):: s
    real(fp):: vi     ! Viscosity in U-point 
    real(fp):: vicmax
!
!! executable statements -------------------------------------------------------
!
    z0       => gdp%gdphysco%z0
    z0v      => gdp%gdphysco%z0v
    vonkar   => gdp%gdphysco%vonkar
    iro      => gdp%gdphysco%iro
    irov     => gdp%gdphysco%irov
    hdt      => gdp%gdnumeco%hdt
    !
    ! Set upper bound for VICUV in coupling points
    ! maximum value based on CFL stability criterion (see also DETVIC)
    !
    if (irov>0) then
       do nm = 1, nmmax
          nmd = nm - icx
          if (kfs(nm)==1 .and. kcs(nm)==3) then
             do k = 1, kmax
                dx =  gvz(nm)
                dy =  0.5 * ( guu(nm) + guu(nmd) )
                vicmax = 1.0/dx**2 + 1.0/dy**2
                vicmax = 0.9/(4.0*hdt*vicmax)
                if (vicuv(nm, k)>=vicmax) then
                   vicuv(nm, k) = vicmax
                endif
             enddo
          endif
       enddo
    endif
    !
    !
    ! initialisation of work array's rxy and rxx
    !
    do nm = 1, nmmax
       do k = 1, kmax
          rxy(nm, k) = 0.0
          rxx(nm, k) = 0.0
       enddo
    enddo
    if (irov==1) then
       !
       ! roughness rigid walls (partial slip IROV = 1)
       ! NOT YET APPLIED TO 45 degrees staircase situation
       !
       do nm = 1, nmmax
          num = nm + icy
          nmu = nm + icx
          numu = nm + icx + icy
          !
          ! rxy in depth point
          !
          do k = 1, kmax
             if (kfuz1(nm, k)==1 .or. kfuz1(num, k)==1) then
                ki = kfuz1(num, k) + 2*kfvz1(nmu, k) + 4*kfuz1(nm, k)           &
                   & + 8*kfvz1(nm, k)
                if (ki==15) then
                   !
                   ! rxy no walls
                   !
                   vi = 0.25*(vicuv(nm, k) + vicuv(nmu, k) + vicuv(num, k)      &
                      & + vicuv(numu, k)) + vnu2d(nm)
                   rxy(nm, k) = vi*((u0(num, k) - u0(nm, k))/gud(nm) + (v(nmu, k&
                              & ) - v(nm, k))/gvd(nm))
                elseif (ki==12 .or. ki==6 .or. ki==4) then
                   !
                   ! rxy for u(nm); rough wall, flow below wall
                   !
                   s = vonkar*u0(nm, k)/log((0.5*guu(nm) + z0v)/z0v)
                   rxy(nm, k) = -s*abs(s)
                elseif (ki==9 .or. ki==3 .or. ki==1) then
                   !
                   ! rxy for u(num); rough wall, flow above wall
                   !
                   s = vonkar*u0(num, k)/log((0.5*guu(num) + z0v)/z0v)
                   rxy(nm, k) = s*abs(s)
                else
                endif
             endif
          enddo
       enddo
    !
    ! roughness rigid walls (no slip IROV = 2 and Z0V = 0.)
    !
    elseif (irov==2) then
       do nm = 1, nmmax
          ndm = nm - icy
          num = nm + icy
          nmu = nm + icx
          numu = nm + icx + icy
          !
          ! rxy in depth point
          !
          do k = 1, kmax
             if (kfuz1(nm, k)==1 .or. kfuz1(num, k)==1) then
                ki = kfuz1(num, k) + 2*kfvz1(nmu, k) + 4*kfuz1(nm, k)           &
                   & + 8*kfvz1(nm, k)
                !
                ! rxy no walls
                !
                if (ki==15) then
                   vi = 0.25*(vicuv(nm, k) + vicuv(nmu, k) + vicuv(num, k)      &
                      & + vicuv(numu, k)) + vnu2d(nm)
                   rxy(nm, k) = vi*((u0(num, k) - u0(nm, k))/gud(nm) + (v(nmu, k&
                              & ) - v(nm, k))/gvd(nm))
                elseif (ki==12 .or. ki==6 .or. ki==4) then
                   !
                   ! rxy for u(nm); rough wall, flow below wall
                   !
                   dy = guu(nm)
                   vi = 0.5*(vicuv(nm, k) + vicuv(nmu, k))                      &
                      & + 0.5*(vnu2d(nm) + vnu2d(ndm))
                   rxy(nm, k) = vi*( - u0(nm, k))/dy/2
                elseif (ki==9 .or. ki==3 .or. ki==1) then
                   !
                   ! rxy for u(num); rough wall, flow above wall
                   !
                   dy = guu(num)
                   vi = 0.5*(vicuv(num, k) + vicuv(numu, k))                    &
                      & + 0.5*(vnu2d(nm) + vnu2d(num))
                   rxy(nm, k) = vi*(u0(num, k))/dy/2
                else
                endif
             endif
          enddo
       enddo
    !
    ! roughness rigid walls (free slip IROV = 3 and Z0V = 0.)
    !
    elseif (irov==3) then
       do nm = 1, nmmax
          num = nm + icy
          nmu = nm + icx
          numu = nm + icx + icy
          !
          ! rxy in depth point
          !
          do k = 1, kmax
             if (kfuz1(nm, k)==1 .or. kfuz1(num, k)==1) then
                ki = kfuz1(num, k) + 2*kfvz1(nmu, k) + 4*kfuz1(nm, k)           &
                   & + 8*kfvz1(nm, k)
                !
                ! rxy no walls
                !
                if (ki==15) then
                   vi = 0.25*(vicuv(nm, k) + vicuv(nmu, k) + vicuv(num, k)      &
                      & + vicuv(numu, k)) + vnu2d(nm)
                   rxy(nm, k) = vi*((u0(num, k) - u0(nm, k))/gud(nm) + (v(nmu, k&
                              & ) - v(nm, k))/gvd(nm))
                endif
             endif
          enddo
       enddo
    else
    endif
    !
    !
    ! rxx in zeta point (active point, on open boundary rxx = 0.)
    !
    do nm = 1, nmmax
       ndm = nm - icy
       nmd = nm - icx
       ndmd = nm + icx + icy
       do k = kfumin(nm), kfumax(nm)
          if ( (kcs(nm)==1 .or. kcs(nm)==3) .and. kfsz1(nm, k)==1 ) then
             vi = vicuv(nm, k) + 0.25*(vnu2d(nm) + vnu2d(nmd) + vnu2d(ndm)         &
                & + vnu2d(ndmd))
             rxx(nm, k) = 2*vi*(u0(nm, k) - u0(nmd, k))/gvz(nm)
          endif
       enddo
    enddo
    !
    ! roughness rigid walls (partial slip)
    ! explicit term
    !
    do nm = 1, nmmax
       nmu = nm + icx
       ndm = nm - icy
       do k = kfumin(nm), kfumax(nm)
          ddk(nm, k) = ddk(nm, k) + (rxx(nmu, k) - rxx(nm, k))/gvu(nm)          &
                     & + (rxy(nm, k) - rxy(ndm, k))/guu(nm)
       enddo
    enddo
end subroutine z_vihrov
