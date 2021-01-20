subroutine santoss_bss1(i2d3d, g, d, d50, d90, delta, aw, uw, &
               & unet, zref, rh, rl, uwc, uwt, ang, uc, ut, &
               & theta, ksw, ksc, fc, fw, fcw, unet_delwblt, alpha, delwblt)
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
    use mathconsts, only:degrad
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
    real(fp)                        , intent(in)    :: aw
    real(fp)                        , intent(in)    :: uw
    real(fp)                        , intent(in)    :: unet
    real(fp)                        , intent(in)    :: zref
    real(fp)                        , intent(in)    :: rh
    real(fp)                        , intent(in)    :: rl
    real(fp)                        , intent(in)    :: uwc
    real(fp)                        , intent(in)    :: uwt
    real(fp)                        , intent(in)    :: ang
!
    real(fp)                        , intent(inout) :: uc
    real(fp)                        , intent(inout) :: ut
!
    real(fp)                        , intent(out)   :: theta
    real(fp)                        , intent(out)   :: ksw
    real(fp)                        , intent(out)   :: ksc
    real(fp)                        , intent(out)   :: fc
    real(fp)                        , intent(out)   :: fw
    real(fp)                        , intent(out)   :: fcw
    real(fp)                        , intent(out)   :: unet_delwblt
    real(fp)                        , intent(out)   :: alpha
    real(fp)                        , intent(out)   :: delwblt
!
! local variables
!
    integer                 :: j
    real(fp)                :: fw_wblt
    real(fp)                :: wblt_sett
    real(fp)                :: mu
    real(fp)                :: ksw1
    real(fp)                :: fw1
    real(fp)                :: p_corr
    real(fp)                :: ksc1
    real(fp)                :: z0c1
    real(fp)                :: fc1
    real(fp)                :: theta1
    real(fp)                :: theta2
    real(fp), dimension(102):: dum
    real(fp)                :: z0c
    real(fp)                :: fcc
    real(fp)                :: ustarc
    real(fp)                :: nu_corr
!
!! executable statements -------------------------------------------------------
!
    mu = 6.0_fp !in combination with deltas sheetflow layer thickness in sfltd99
!
!   (iii) bed roughness (mobile or ripples) and friction coefficient for waves + current
!   aw = horizontal excursion amplitude of the free-stream orbital flow (regular waves)
!

!   initial roughness sheet flow regime [m]
    ksw1=d50

!   wave friction formula of swart (1974) [-]
    if (ksw1/aw < 0.63) then
        fw1=exp(5.213*(ksw1/aw)**0.194-5.977)
    else
        fw1=0.3
    endif

!   initial current roughness [m]
    ksc1=3.*d90
    z0c1=ksc1/30.

!   current fricion factor assuming logarithmic current profile [-]
    if (unet == 0.) then
        fc1=0.
    elseif (i2d3d == 2) then ! 2d
        fc1=2.*(0.4/(log(d/z0c1)-1.+z0c1/d))**2.
    else ! 3d
        fc1=2.*(0.4/log(zref/z0c1))**2.
    endif

!   initial wave-averaged total bed shear stress [-] 
    theta1=0.5*fc1*unet**2./(delta*g*d50)+0.25*fw1*uw**2./(delta*g*d50)

!    - mobile bed roughness
!    - friction coefficient for waves and for current
!    - mean magnitude of bed shear stress

!   start of shields loop
    j=1    
    dum(j)=theta1

!   additional roughness if wave-averaged total Shields > 1 [m]
    if (d50<=0.00015_fp) then
        ksw=max(ksw1,d50*(mu+6.0_fp*(dum(j)-1.0_fp)))
        ksc=max(ksc1,d50*(mu+6.0_fp*(dum(j)-1.0_fp)))
    elseif (d50>=0.00020_fp) then
        ksw=max(ksw1,d50*(1.0_fp+6.0_fp*(dum(j)-1.0_fp)))
        ksc=max(ksc1,d50*(1.0_fp+6.0_fp*(dum(j)-1.0_fp)))
     else
        ksw=max(ksw1,d50*(mu+(d50-0.00015_fp)*(1.0_fp-mu)/ &
                    & (0.00020_fp-0.00015_fp)+6.0_fp*(dum(j)-1.0_fp)))
        ksc=max(ksc1,d50*(mu+(d50-0.00015_fp)*(1.0_fp-mu)/ &
                    & (0.00020_fp-0.00015_fp)+6.0_fp*(dum(j)-1.0_fp)))
    endif
    z0c=ksc/30.0_fp

    if (ksw/aw < 0.63_fp) then
        fw=exp(5.213_fp*(ksw/aw)**0.194_fp-5.977_fp)
    else
        fw=0.3_fp
    endif

    if (comparereal(unet,0.0_fp)==0) then
        fcc=0.0_fp
    elseif (i2d3d == 2) then ! 2d
        fcc=2.0_fp*(0.4_fp/(log(d/z0c)-1.0_fp+z0c/d))**2
    else ! 3d
        fcc=2.0_fp*(0.4_fp/log(zref/z0c))**2
    endif

!   wave-averaged total bed shear stress [-] 
    theta2=0.5_fp*fcc*unet**2/(delta*g*d50) + 0.25*fw*uw**2/(delta*g*d50)
    j=j+1
    dum(j)=theta2

!   while loop to find theta with 0.001 difference and 100 iterations
!   as stop criteria.
    do while (abs(dum(j)-dum(j-1)) > 0.001_fp .and. j<100)
        if (d50 <= 0.00015_fp) then
            ksw=max(ksw1,d50*(mu+6.0_fp*(dum(j)-1.0_fp)))
            ksc=max(ksc1,d50*(mu+6.0_fp*(dum(j)-1.0_fp)))
        elseif (d50 >= 0.00020_fp) then
            ksw=max(ksw1,d50*(1.0_fp+6.0_fp*(dum(j)-1.0_fp)))
            ksc=max(ksc1,d50*(1.0_fp+6.0_fp*(dum(j)-1.0_fp)))
        else
            ksw=max(ksw1,d50*(mu+(d50-0.00015_fp)*(1.0_fp-mu)/ &
                        & (0.00020_fp-0.00015_fp)+6.0_fp*(dum(j)-1.0_fp)))
            ksc=max(ksc1,d50*(mu+(d50-0.00015_fp)*(1.0_fp-mu)/ &
                        & (0.00020_fp-0.00015_fp)+6.0_fp*(dum(j)-1.0_fp)))
        endif
        z0c=ksc/30.0_fp
        if (ksw/aw < 0.63_fp) then
            fw=exp(5.213_fp*(ksw/aw)**0.194_fp - 5.977_fp)
        else
            fw=0.3_fp
        end if
        if (comparereal(unet,0.0_fp) == 0) then
            fcc=0.0_fp
        elseif (i2d3d == 2) then ! 2d
            fcc=2.0_fp*(0.4_fp/(log(d/z0c)-1.0_fp+z0c/d))**2
        else ! 3d
            fcc=2.0_fp*(0.4_fp/log(zref/z0c))**2
        end if
        j=j+1
        dum(j)=0.5_fp*fcc*unet**2/(delta*g*d50) + 0.25_fp*fw*uw**2/(delta*g*d50)
    end do
!   total Shields [-]
    theta=dum(j)

    if (comparereal(rh,0.0_fp) /= 0) then
        ! rippled bed roughness [m]
        p_corr = 0.4_fp ! correction factor p. used for form roughness ripples
        ksw=ksw+rh*rh/rl*p_corr
        ksc=ksc+rh*rh/rl*p_corr
        z0c=ksc/30.0_fp
        if (ksw/aw < 0.63_fp) then
            fw=exp(5.213_fp*(ksw/aw)**0.194_fp - 5.977_fp)
        else
            fw=0.3_fp
        endif
        if (comparereal(unet,0.0_fp) == 0) then
            fcc=0.0_fp
        elseif (i2d3d == 2) then ! 2D
            fcc=2.0_fp*(0.4_fp/(log(d/z0c)-1.0_fp+z0c/d))**2
        else ! 3D
            fcc=2.0_fp*(0.4_fp/log(zref/z0c))**2
        endif 
    endif

    delwblt = min(max(ksw*0.27_fp*(aw/ksw)**0.67_fp,0.01_fp),0.2_fp)
!   friction velocity [m/s]
    ustarc=sqrt(0.5_fp*fcc)*unet
!   net current strength at edge boundary layer [m/s]
    unet_delwblt=ustarc/0.4_fp*log(delwblt/z0c)
!   correction factor weighing waves and current
    nu_corr=1.0_fp
!   relative current strength [-]
    alpha=nu_corr*unet_delwblt/(nu_corr*unet_delwblt+uw)

    if (comparereal(unet_delwblt,0.0_fp) == 0) then
        fc=0.0_fp
        fcw=fw !combined wave-current friction factor [-]
        uc=uwc
        ut=uwt
    else
!       current friction factor corresponding to unet_delwblt such that bed shear stress stays the same
        fc=fcc*unet**2/unet_delwblt**2
!       combined wave-current friction coefficient fcw using formula Madsen & Grant (1976)
        fcw =alpha*fc+(1.0_fp-alpha)*fw
    endif
    uc=sqrt((uwc+unet_delwblt*cos(ang*degrad))**2 + &
              & (unet_delwblt*sin(ang*degrad))**2)
    ut=sqrt((unet_delwblt*cos(ang*degrad)-uwt)**2+ &
          & (unet_delwblt*sin(ang*degrad))**2)

!   addition for the special case of current stronger than uwc or uwt, 19/4/19, jjvdwerf
    if ((unet_delwblt*cos(ang*degrad)-uwt) >= 0.0_fp) then
        ut=sqrt(0.001_fp**2+(unet_delwblt*sin(ang*degrad))**2)
    else
        if ((uwc + unet_delwblt*cos(ang*degrad)) <= 0.0_fp) then
            uc=sqrt(0.001_fp**2+(unet_delwblt*sin(ang*degrad))**2)
        endif
    endif
end subroutine santoss_bss1