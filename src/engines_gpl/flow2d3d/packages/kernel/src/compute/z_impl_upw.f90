subroutine z_impl_upw(nmmax     ,kmax      ,icx       ,icy       ,kcs     , &
                    & kcscut    ,kfu       ,kfuz1     ,kfumin    ,kfumax  , &
                    & kfvz1     ,u0        ,v1        ,guu       ,gvu     , &
                    & gvd       ,guz       ,gsqiu     ,bdx       ,bux     , &
                    & bbk       ,bdy       ,ddk       ,buy       ,gdp     )
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
!!--pseudo code and references--------------------------------------------------
! NONE
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
    !
!
! Global variables
!
    integer                                                        :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X- dir. If icx=1 then computation pro ceeds in the Y-dir.
    integer                                                        :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                        :: kmax   !  Description and declaration in iidim.f90
    integer                                                        :: nmmax  !  Description and declaration in dimens.igs
    integer  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kcs    !  Description and declaration in iidim.f90
    integer  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfu    !  Description and declaration in iidim.f90
    integer  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfumax !  Description and declaration in iidim.f90
    integer  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfumin !  Description and declaration in iidim.f90
    integer  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(in) :: kcscut !  Description and declaration in iidim.f90
    integer  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: kfuz1  !  Description and declaration in iidim.f90
    integer  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: kfvz1  !  Description and declaration in iidim.f90
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in) :: gsqiu  !  Description and declaration in rjdim.f90
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: guu    !  Description and declaration in rjdim.f90
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in) :: guz    !  Description and declaration in rjdim.f90
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: gvd    !  Description and declaration in rjdim.f90
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: gvu    !  Description and declaration in rjdim.f90
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: bdx    !!  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                             !!  with layer velocity in (N,M-1,K)
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: bdy    !!  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                             !!  with layer velocity in (N-1,M,K)
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: bux    !!  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                             !!  with layer velocity in (N,M+1,K)
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: buy    !!  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                             !!  with layer velocity in (N+1,M,K)
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: bbk    !!  Internal work array, coefficient layer velocity in (N,M,K) implicit part
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: ddk    !!  Internal work array, diagonal space at (N,M,K)
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: u0     !  Description and declaration in rjdim.f90
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: v1     !  Description and declaration in rjdim.f90
!
! Local variables
!
    integer            :: idifd
    integer            :: idifu
    integer            :: k
    integer            :: kenm
    integer            :: ndm
    integer            :: ndmd
    integer            :: ndmu
    integer            :: nm
    integer            :: nmd
    integer            :: nmu
    integer            :: num
    integer            :: numd
    integer            :: numu
    real(fp)           :: dgeta
    real(fp)           :: dgvnm
    real(fp)           :: fac
    real(fp)           :: geta
    real(fp)           :: gksi
    real(fp)           :: gsqi
    real(fp)           :: uuu
    real(fp)           :: vvv
    real(fp)           :: uvdgdy
    real(fp)           :: vvdgdx
!
!! executable statements -------------------------------------------------------
!
    fac = 1.0_fp
    !
    do nm = 1, nmmax
       if (kfu(nm)==1) then
          nmd   = nm - icx
          ndm   = nm - icy
          ndmd  = nm - icx - icy
          nmu   = nm + icx
          num   = nm + icy
          numu  = nm + icx + icy
          ndmu  = nm + icx - icy
          numd  = nm - icx + icy
          gksi  = gvu(nm)
          geta  = guu(nm)
          dgeta = guz(nmu) - guz(nm)
          dgvnm = gvd(nm)  - gvd(ndm)
          gsqi  = gsqiu(nm)
          !
          do k = kfumin(nm), kfumax(nm)
             if (kfuz1(nm, k)==1 .and. kcs(nm)*kcs(nmu)>0) then
                vvv   = 0.25_fp*(v1(ndm, k) + v1(ndmu, k) + v1(nm, k) + v1(nmu, k))
                uuu   = u0(nm, k)
                idifd = kfvz1(ndm, k)*kfvz1(ndmu, k)*kfuz1(ndm, k)
                idifu = kfvz1(nm, k)*kfvz1(nmu, k)*kfuz1(num, k)
                !
                ! For 1:n stair case (cut-cell) boundary:
                ! - check dgvnm
                ! - reset geta
                ! - reset vvv
                !
                if (kcscut(nm, k)==1 .or. kcscut(nmu, k)==1) then
                   kenm = max(1, kfvz1(nm, k) + kfvz1(ndm, k) + kfvz1(ndmu, k)  &
                        & + kfvz1(nmu, k))
                   vvv  = v1(nm, k)*kfvz1(nm, k) + v1(ndm, k)*kfvz1(ndm, k)      &
                        & + v1(ndmu, k)*kfvz1(ndmu, k) + v1(nmu, k)*kfvz1(nmu, k)
                   vvv  = vvv/kenm
                endif
                !
                ! CURVATURE TERM DUE TO CONVECTION IN U-DIRECTION
                !
                uvdgdy = vvv*gsqi*dgvnm
                !
                ! CURVATURE TERM DUE TO ADVECTION IN V-DIRECTION
                !
                vvdgdx = vvv*gsqi*dgeta
                !
                ! Advection in U- and V-direction
                ! First order upwind, implicit
                ! Only in this half time step using operator splitting,
                ! therefore factor 2.0_fp in discretisation
                !
                if (uuu >= 0.0_fp) then
                   bbk(nm, k) = bbk(nm, k) + fac*kfuz1(nmd, k)*uuu/gksi
                   bdx(nm, k) = bdx(nm, k) - fac*kfuz1(nmd, k)*uuu/gksi
                   ddk(nm, k) = ddk(nm, k) - fac*kfuz1(nmd, k)*uuu*uvdgdy
                else
                   bbk(nm, k) = bbk(nm, k) - fac*kfuz1(nmu, k)*uuu/gksi
                   bux(nm, k) = bux(nm, k) + fac*kfuz1(nmu, k)*uuu/gksi
                   ddk(nm, k) = ddk(nm, k) - fac*kfuz1(nmu, k)*uuu*uvdgdy
                endif
                if (vvv >= 0.0_fp) then
                   bbk(nm, k) = bbk(nm, k) + fac*idifd*vvv/geta
                   bdy(nm, k) = bdy(nm, k) - fac*idifd*vvv/geta
                   ddk(nm, k) = ddk(nm, k) + fac*0.5_fp*idifd*vvv*vvdgdx
                else
                   bbk(nm, k) = bbk(nm, k) - fac*idifu*vvv/geta
                   buy(nm, k) = buy(nm, k) + fac*idifu*vvv/geta
                   ddk(nm, k) = ddk(nm, k) + fac*0.5_fp*idifu*vvv*vvdgdx
                endif                
             endif
          enddo
       endif
    enddo
end subroutine z_impl_upw
