subroutine shrwav(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                & icy       ,dfu       ,deltau    ,tp        ,rlabda    , &
                & hu        ,kfu       ,sig       ,ddk       ,thick     , &
                & gdp       )
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
! NONE
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
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
    integer                                         , intent(in) :: icx     !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir.
                                                                            !!  If icx=1 then computation proceeds in the Y-dir.
    integer                                                      :: icy     !!  Increment in the Y-dir. (see ICX)
    integer                                                      :: j       !!  Begin pointer for arrays which have been transformed into 1D arrays.
                                                                            !!  Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                         , intent(in) :: kmax    !  Description and declaration in iidim.f90
    integer                                         , intent(in) :: nmmax   !  Description and declaration in dimens.igs
    integer                                                      :: nmmaxj  !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfu     !  Description and declaration in iidim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: deltau  !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: dfu     !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: hu      !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: rlabda  !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: tp      !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: ddk     !!  Internal work array, diagonal space at (N,M,K)
    real(fp), dimension(kmax)                                    :: sig     !  Description and declaration in rjdim.f90
    real(fp), dimension(kmax)                       , intent(in) :: thick   !  Description and declaration in rjdim.f90
!
!
! Local variables
!
    integer    :: k
    integer    :: nm
    integer    :: nmu
    real(fp)   :: deltuu
    real(fp)   :: dfuu
    real(fp)   :: ku
    real(fp)   :: omega
    real(fp)   :: rzza
    real(fp)   :: rzzb
    real(fp)   :: thsum
    real(fp)   :: tpu
    real(fp)   :: vdist
!
!
!! executable statements -------------------------------------------------------
!
    !
    ! Initialisation
    !
    thsum = 0.
    !
    do k = 1, kmax
       nmu = icx
       thsum = thsum + thick(k)
       do nm = 1, nmmax
          nmu = nmu + 1
          if (kfu(nm)==1) then
             !
             ! Added shear stress in wave boundary layer due to streaming
             !
             if (tp(nm)>0. .and. tp(nmu)>0.) then
                tpu = 0.5*(tp(nm) + tp(nmu))
                omega = 2.*pi/tpu
                !
                ! Deltau is defined at velocity points
                !
                deltuu = deltau(nm)
                ku = 2.*pi/(0.5*(rlabda(nm) + rlabda(nmu)))
                !
                ! Dfu is defined at velocity points
                !
                dfuu = dfu(nm)
                !
                ! Detmine distance from bottom
                !
                vdist = 1. - thsum
                if (abs(deltuu)>1.E-7) then
                   rzza = (dfuu*ku/omega)*(deltuu - vdist)/deltuu
                   rzzb = (dfuu*ku/omega)*(max(deltuu - vdist + thick(k), 0.0_fp))  &
                        & /deltuu
                   !
                   ! Add term for streaming
                   !
                   if (vdist<=deltuu) then
                      ddk(nm, k) = ddk(nm, k) + (rzzb - rzza)/(thick(k)*hu(nm))
                   endif
                endif
             endif
          endif
       enddo
    enddo
end subroutine shrwav
