subroutine z_dengra(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                  & icy       ,kfsz1     ,kfumin    ,kfumax    ,kfvmin    , &
                  & kfvmax    ,rho       ,gvu       ,guv       ,drhodx    , &
                  & drhody    ,dzu0      ,dzv0      ,gdp       )
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
! Computes densities from temperature, salinity and
! equation of state.
! Equation of state following Eckart, (C. Eckart,
! The equation of state of water and sea water at
! low temperatures and pressures, American Journal
! of Science, april 1958)
! Fixed Layer Approach
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
!
! Global variables
!
    integer, intent(in)            :: icx
    integer, intent(in)            :: icy
    integer         :: j
    integer, intent(in)            :: kmax !  Description and declaration in iidim.f90
    integer, intent(in)            :: nmmax !  Description and declaration in dimens.igs
    integer         :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfumax !  Description and declaration in iidim.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfumin !  Description and declaration in iidim.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfvmax !  Description and declaration in iidim.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfvmin !  Description and declaration in iidim.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: kfsz1 !  Description and declaration in iidim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: guv !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: gvu !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) :: drhodx !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) :: drhody !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: dzu0 !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: dzv0 !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: rho !  Description and declaration in rjdim.f90
!
! Local variables
!
    integer :: iken
    integer :: ikenup
    integer :: k
    integer :: kup
    integer :: nm
    integer :: nmu
    integer :: num
!
!! executable statements -------------------------------------------------------
!
    !
    do nm = 1, nmmax
       nmu = nm + icx
       num = nm + icy
       if (kfumin(nm)<=kfumax(nm)) then
          drhodx(nm, kfumax(nm)) = 0.
          do k = kfumax(nm), kfumin(nm), -1
             kup = k + 1
             if (k==kfumax(nm)) then
                iken = kfsz1(nm, k)*kfsz1(nmu, k)
                drhodx(nm, k) = iken*dzu0(nm, k)*.5*(rho(nmu, k) - rho(nm, k))  &
                              & /gvu(nm)
             else
                ikenup = kfsz1(nm, kup)*kfsz1(nmu, kup)
                iken = kfsz1(nm, k)*kfsz1(nmu, k)
                drhodx(nm, k) = drhodx(nm, kup) + ikenup*.5*dzu0(nm, kup)       &
                              & *(rho(nmu, kup) - rho(nm, kup))/gvu(nm)         &
                              & + iken*.5*dzu0(nm, k)*(rho(nmu, k) - rho(nm, k))&
                              & /gvu(nm)
             endif
          enddo
       endif
       !
       if (kfvmin(nm)<=kfvmax(nm)) then
          drhody(nm, kfvmax(nm)) = 0.
          do k = kfvmax(nm), kfvmin(nm), -1
             kup = k + 1
             if (k==kfvmax(nm)) then
                iken = kfsz1(num, k)*kfsz1(nm, k)
                drhody(nm, k) = iken*dzv0(nm, k)*.5*(rho(num, k) - rho(nm, k))  &
                              & /guv(nm)
             else
                ikenup = kfsz1(nm, kup)*kfsz1(num, kup)
                iken = kfsz1(nm, k)*kfsz1(num, k)
                drhody(nm, k) = drhody(nm, kup) + ikenup*.5*dzv0(nm, kup)       &
                              & *(rho(num, kup) - rho(nm, kup))/guv(nm)         &
                              & + iken*.5*dzv0(nm, k)*(rho(num, k) - rho(nm, k))&
                              & /guv(nm)
             endif
          enddo
       endif
    enddo
end subroutine z_dengra
