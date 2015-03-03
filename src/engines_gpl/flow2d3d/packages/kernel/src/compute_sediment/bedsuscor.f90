subroutine bedsuscor(kfu, hu, fluxu, guu, u1, dzu1, &
                   & kcs, kfsed, aks, rca, r1, fixfac, thick, sig, &
                   & nm, nmu, l, ll, kmax, k1, k2, dk, sucor, gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2015.                                
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
!    Function: Computes suspended sediment transport correction
!              vector for sand sediment fractions. This is the
!              opposite of the computed suspended transport below
!              the reference height aksu
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    integer                                            , intent(in)  :: kmax
    integer                                            , intent(in)  :: k2     !  index of top-most layer
    integer                                            , intent(in)  :: k1     !  index of bottom-most layer
    integer                                            , intent(in)  :: dk     !  layer number order (+1 = layer 1 near bottom, -1 = layer 1 near surface)
    integer                                            , intent(in)  :: l
    integer                                            , intent(in)  :: ll
    integer                                            , intent(in)  :: nm
    integer                                            , intent(in)  :: nmu
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kfsed  !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in)  :: dzu1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, *), intent(in)  :: fluxu
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in)  :: u1
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, *)      , intent(in)  :: aks
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, *)      , intent(in)  :: fixfac
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, *), intent(in)  :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, *)      , intent(in)  :: rca
    real(fp), dimension(*)                             , intent(in)  :: thick
    real(fp), dimension(*)                             , intent(in)  :: sig
    real(fp)                                           , intent(out) :: sucor
!
! Local variables
!
    integer  :: k
    integer  :: ka
    logical  :: zmodel
    real(fp) :: aksu
    real(fp) :: apower
    real(fp) :: cavg
    real(fp) :: cavg1
    real(fp) :: cavg2
    real(fp) :: ceavg
    real(fp) :: cflux
    real(fp) :: cumflux
    real(fp) :: dz
    real(fp) :: dzup
    real(fp) :: r1avg
    real(fp) :: z
    real(fp) :: zktop
!
!! executable statements -------------------------------------------------------
!
    zmodel = dk==1
    if ((kfu(nm)*kfsed(nm)*kfsed(nmu)) /= 0) then
       cumflux = 0.0_fp
       if (kcs(nmu) == 3 .or. kcs(nmu) == -1) then
          aksu = aks(nm, l)
       elseif (kcs(nm) == 3 .or. kcs(nm) == -1) then
          aksu = aks(nmu, l)
       else
          aksu = (aks(nm, l)+aks(nmu, l)) / 2.0_fp
       endif
       !
       ! work up through layers accumulating the suspended transport
       ! in the layers fully below aksu
       !
       zktop = 0.0_fp
       ka = 0
       do k = k1,k2,dk
          if (zmodel) then
             dz = dzu1(nm,k)
          else
             dz = thick(k)*hu(nm)
          endif
          zktop = zktop + dz
          !
          ! if layer containing aksu
          !
          if (aksu <= zktop) then
             ka = k
             if (k/=k2) then
                if (zmodel) then
                   dzup = dzu1(nm,k+dk)
                else
                   dzup = thick(k+dk)*hu(nm)
                endif
             endif
             ! the contribution of this layer is computed below
             exit
          else
             cumflux = cumflux + fluxu(nm,k,ll)
          endif
       enddo
       !
       ! add the flux below the reference height in layer ka.
       !
       k = ka
       if (k==0) then
          ! aksu larger than water depth, so all done
       elseif (k==k2) then
          ! aksu is located in top layer; use simple flux
          ! approximation assuming uniform flux
          cumflux = cumflux + fluxu(nm,k,ll)*(aksu - zktop + dz)/dz
       else
          ! aksu is located in a layer below the top layer
          !
          ! Get reference concentration at aksu
          !
          if (kcs(nmu) == 3 .or. kcs(nmu) == -1) then
             ceavg = rca(nm,l)
          elseif (kcs(nm) == 3 .or. kcs(nm) == -1) then
             ceavg = rca(nmu,l)
          else
             ceavg = (rca(nm,l)+rca(nmu,l)) / 2.0_fp
          endif
          !
          ! Get concentration in layer above this layer
          !
          r1avg = (r1(nm, k+dk, ll) + r1(nmu, k+dk, ll)) / 2.0_fp
          !
          ! If there is a significant concentration gradient, and significant
          ! reference concentration
          !
          if (ceavg>r1avg*1.1_fp .and. ceavg>0.05_fp) then
             !
             ! Compute Rouse number based on reference concentration and
             ! concentration of the layer above it. Make sure that Rouse number
             ! differs significantly from 1, and that it is not too large.
             ! Note: APOWER = - Rouse number
             !
             ! The Rouse profile equation 
             !
             !            [ a*(H-z) ]^R
             ! c(z) = c_a*[ ------- ]
             !            [ z*(H-a) ]
             !
             ! is here approximated by
             !
             ! c(z) = c_a*(a/z)^R = c_a*(z/a)^-R
             !
             z = zktop + dzup/2.0_fp
             apower = log(max(r1avg/ceavg,1.0e-5_fp)) / log(z/aksu)
             if (apower>-1.05_fp .and. apower<=-1.0_fp) then
                apower = -1.05_fp
             elseif (apower>=-1.0_fp .and. apower<-0.95_fp) then
                apower = -0.95_fp
             else
             endif
             apower  = min(max(-10.0_fp , apower), 10.0_fp)
             !
             ! Compute the average concentration cavg between the reference
             ! height a and the top of the current layer (bottom of layer above) z.
             !         /zktop                           zktop                       zktop
             ! cavg = | c(z) dz = c_a/(-R+1)*(z/a)^(-R+1)*a | = c_a/(-R+1)*a^R*z^(-R+1) |
             !       /a                                     a                           a
             !
             dz     = zktop - aksu
             cavg1   = (ceavg/(apower+1.0_fp)) * aksu**(-apower)
             cavg2   = zktop**(apower+1.0_fp) - aksu**(apower+1.0_fp)
             cavg    = cavg1 * cavg2 / dz
             !
             ! The corresponding effective suspended load flux is
             !
             cflux   = u1(nm,k)*cavg*dz*guu(nm)
             !
             ! Increment the correction by the part of the suspended load flux
             ! that is in excess of the flux computed above, but never opposite.
             !
             if (fluxu(nm,k,ll)>0.0_fp .and. cflux>0.0_fp) then
                 cumflux = cumflux + max(0.0_fp, fluxu(nm,k,ll)-cflux)
             elseif (fluxu(nm,k,ll)<0.0_fp .and. cflux<0.0_fp) then
                 cumflux = cumflux + min(fluxu(nm,k,ll)-cflux, 0.0_fp)
             else
                 cumflux = cumflux + fluxu(nm,k,ll)
             endif
          endif
       endif
       sucor = -cumflux / guu(nm)
       !
       ! bedload will be reduced in case of sediment transport
       ! over a non-erodible layer (no sediment in bed) in such
       ! a case, the suspended sediment transport vector must
       ! also be reduced.
       !
       if ((sucor > 0.0_fp .and. kcs(nm)==1) .or. kcs(nmu)/=1) then
          sucor = sucor*fixfac(nm, l)
       else
          sucor = sucor*fixfac(nmu, l)
       endif
    else
       sucor = 0.0_fp
    endif
end subroutine bedsuscor
