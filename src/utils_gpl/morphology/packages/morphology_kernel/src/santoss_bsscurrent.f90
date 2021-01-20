subroutine santoss_bsscurrent(i2d3d, g, d, d50, d90, delta, unet, ang, &
               & zref, rh, rl, unet_delwblt, delwblt, sc, scx, scy)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2021.                                
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
!   The SANTOSS practical sand transport model, version 2.08  
!   Computations of the orbital motions in the nearshore morphodynamical model
!   using the formula of Abreu et al. (2010)
!
!   Computation of the bed shear stresses using the method of Ribberink (1998)
!   Acceleration-skewness method by Van der A (2009) 
!   Part 1: computation friction factor + current velocity at edge boundary layer
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts, only: degrad
    implicit none
!
! call variables
!
    integer                         , intent(in)    :: i2d3d
    real(fp)                        , intent(in)    :: g
    real(fp)                        , intent(in)    :: d
    real(fp)                        , intent(in)    :: d50
    real(fp)                        , intent(in)    :: d90
    real(fp)                        , intent(in)    :: delta
    real(fp)                        , intent(in)    :: unet
    real(fp)                        , intent(in)    :: ang
    real(fp)                        , intent(in)    :: zref
    real(fp)                        , intent(in)    :: rh
    real(fp)                        , intent(in)    :: rl
!
    real(fp)                        , intent(out)   :: unet_delwblt
    real(fp)                        , intent(out)   :: delwblt
    real(fp)                        , intent(out)   :: sc
    real(fp)                        , intent(out)   :: scx
    real(fp)                        , intent(out)   :: scy
!
! local variables
!
    integer                 :: j
    real(fp)                :: mu
    real(fp)                :: p_corr
    real(fp)                :: ksc1
    real(fp)                :: z0c1
    real(fp)                :: fc1
    real(fp)                :: theta1
    real(fp)                :: theta2
    real(fp)                :: ksc
    real(fp)                :: z0c
    real(fp)                :: fc
    real(fp)                :: fcc
    real(fp)                :: theta
    real(fp)                :: ustarc
    real(fp), dimension(100) :: dum
!
!! executable statements -------------------------------------------------------
!
!   initialize local variables
    mu = 6.0_fp !in combination with deltas sheetflow layer thickness in sfltd99

!
!   current alone part i and ii
!
    ksc1=3.0_fp*d90
    z0c1=ksc1/30.0_fp

    ! current fricion factor assuming logarithmic current profile [-]
    if (comparereal(unet,0.0_fp)==0) then
        fc1=0.0_fp
    elseif (i2d3d == 2) then ! 2d
        fc1=2.0_fp*(0.4_fp/(log(d/z0c1)-1.0_fp+z0c1/d))**2
    else ! 3d
        fc1=2.0_fp*(0.4_fp/log(zref/z0c1))**2
    endif

!   initial total bed shear stress [-] - note that for current only conditions 
!   hiding/exposure is not accounted for since no such conditions in the database. 
!   can apply zeta factor as above or other approaches, e.g. Day, 1980
    theta1=0.5_fp*fc1*unet**2/(delta*g*d50)

!     - mobile bed roughness
!     - friction coefficient for waves and for current
!     - mean magnitude of bed shear stress

    j=1
    dum(j)=theta1

!   additional roughness if wave averaged total Shiels >1 [m]
    if (d50 <= 0.00015_fp) then
        ksc=max(ksc1,d50*(mu+6.0_fp*(dum(j)-1.0_fp)))
    elseif (d50 >= 0.00020_fp) then
        ksc=max(ksc1,d50*(1.0_fp+6.0_fp*(dum(j)-1.0_fp)))
    else
        ksc=max(ksc1,d50*(mu+(d50-0.00015_fp)*(1.0_fp-mu)/ &
                    & (0.00020_fp-0.00015_fp)+6.0_fp*(dum(j)-1.0_fp)))
    endif
    z0c=ksc/30.0_fp
    if (i2d3d == 2) then ! 2d
        fcc=2.0_fp*(0.4_fp/(log(d/z0c)-1.0_fp+z0c/d))**2
    else ! 3d
        fcc=2.0_fp*(0.4_fp/log(zref/z0c))**2
    endif

!   total bed shear stress [-]
    theta2=0.5_fp*fcc*unet**2/(delta*g*d50)
    j=j+1
    dum(j)=theta2

!   while loop to find theta with 0.001 difference and 100 iterations
!   as stop criteria.
    do while (abs(dum(j)-dum(j-1)) > 0.001_fp .and. j < 100)
        if (d50 <= 0.00015_fp) then
            ksc=max(ksc1,d50*(mu+6.0_fp*(dum(j)-1.0_fp)))
        elseif (d50 >= 0.00020_fp) then
            ksc=max(ksc1,d50*(1.0_fp+6.0_fp*(dum(j)-1.0_fp)))
        else
            ksc=max(ksc1,d50*(mu+(d50-0.00015_fp)*(1.0_fp-mu)/ &
                        & (0.00020_fp-0.00015_fp)+6.0_fp*(dum(j)-1.0_fp)))
        endif
        z0c=ksc/30.0_fp
        
        if (i2d3d == 2) then ! 2d
            fcc=2.0_fp*(0.4_fp/(log(d/z0c)-1.0_fp+z0c/d))**2
        else ! 3d
            fcc=2.0_fp*(0.4_fp/log(zref/z0c))**2
        end if
        j=j+1
        dum(j)=0.5_fp*fcc*unet**2/(delta*g*d50)
    enddo
    theta=dum(j)  !total shields 

!
!   (ii) bed shear stress for only currents: tc, tt and x,y components stx, sty
!
    ustarc=sqrt(0.5_fp*fcc)*unet      ! friction velocity [m/s]

!   standard, fixed level above the bed of 0.1 m for net current	
    delwblt = 0.1_fp;

!   net current strength at this standard vertical level [m/s]
    unet_delwblt=(ustarc/0.4_fp)*log(delwblt/z0c)

    if (comparereal(unet_delwblt,0.0_fp)==0) then
        fc=0.0_fp
    else
        ! current friction factor corresponding to unet_delwblt such that bed shear stress stays the same
        fc=fcc*unet**2/unet_delwblt**2
    endif

    sc = 0.5_fp*fc*unet_delwblt**2/(delta*g*d50)
    scx = sc * cos(ang*degrad)
    scy = sc * sin(ang*degrad)
end subroutine santoss_bsscurrent
