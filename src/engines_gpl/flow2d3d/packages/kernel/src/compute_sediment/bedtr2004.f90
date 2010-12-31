subroutine bedtr2004(u2dh      ,d50       ,d90       ,h1        ,rhosol    , &
                   & tp        ,teta      ,lundia    ,uon       ,uoff      , &
                   & ubw       ,taucr     ,delm      ,ra        ,z0cur     , &
                   & fc1       ,fw1       ,dstar     ,drho      ,phicur    , &
                   & qbcu      ,qbcv      ,qbwu      ,qbwv      ,qswu      , &
                   & qswv      ,tetacr    ,aks       ,fsilt     ,sig       , &
                   & thick     ,concin    ,kmax      ,deltas    ,ws        , &
                   & rksrs     ,dzduu     ,dzdvv     ,rhow      , &
                   & ag        ,bedw      ,pangle    ,fpco      ,susw      , &
                   & dclay     ,wave      ,eps       ,subiw     ,error     )
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
! Compute bed load transport according to Van Rijn
! Note: Formulation used depends on presence of waves in the
! simulation.
! If no waves then use traditional Van Rijn formulation
! If waves then use new parameterization which
! includes wave asymetry
! Note: The two methods are known to give different results
! (order factor 2) for situations without waves
! Van Rijn (1993,2000)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    implicit none
!
! Global variables
!
    integer                  , intent(in)  :: lundia   !  Description and declaration in inout.igs
    integer                  , intent(in)  :: kmax     !  Description and declaration in inout.igs
    real(fp)                 , intent(in)  :: aks
    real(fp)                 , intent(in)  :: d50
    real(fp)                 , intent(in)  :: d90
    real(fp)                 , intent(in)  :: delm
    real(fp)                 , intent(in)  :: drho
    real(fp)                 , intent(in)  :: dstar
    real(fp)                 , intent(in)  :: dzduu    !  Description and declaration in rjdim.f90
    real(fp)                 , intent(in)  :: dzdvv    !  Description and declaration in rjdim.f90
    real(fp)                 , intent(in)  :: fc1
    real(fp)                 , intent(in)  :: fsilt
    real(fp)                 , intent(in)  :: fw1
    real(fp)                 , intent(in)  :: h1
    real(fp)                 , intent(in)  :: phicur
    real(fp)                 , intent(out) :: qbcu
    real(fp)                 , intent(out) :: qbcv
    real(fp)                 , intent(out) :: qbwu
    real(fp)                 , intent(out) :: qbwv
    real(fp)                 , intent(out) :: qswu
    real(fp)                 , intent(out) :: qswv
    real(fp)                 , intent(in)  :: ra
    real(fp)                 , intent(in)  :: rhosol   !  Description and declaration in rjdim.f90
    real(fp)                 , intent(in)  :: teta     !  Description and declaration in rjdim.f90
    real(fp)                 , intent(in)  :: tetacr   !  Description and declaration in rjdim.f90
    real(fp)                 , intent(in)  :: taucr
    real(fp)                 , intent(in)  :: tp       !  Description and declaration in rjdim.f90
    real(fp)                 , intent(in)  :: u2dh
    real(fp)                 , intent(in)  :: ubw
    real(fp)                 , intent(in)  :: uon  
    real(fp)                 , intent(in)  :: uoff 
    real(fp)                 , intent(in)  :: z0cur
    real(fp)                 , intent(in)  :: rksrs
    real(fp)                 , intent(in)  :: ws
    real(fp)                 , intent(in)  :: deltas
    real(fp), dimension(kmax), intent(in)  :: sig      !  Description and declaration in rjdim.f90
    real(fp), dimension(kmax), intent(in)  :: thick    !  Description and declaration in rjdim.f90
    real(fp), dimension(kmax), intent(in)  :: concin
    real(fp)                 , intent(in)  :: rhow
    real(fp)                 , intent(in)  :: ag
    real(fp)                 , intent(in)  :: bedw
    real(fp)                 , intent(in)  :: pangle
    real(fp)                 , intent(in)  :: fpco
    real(fp)                 , intent(in)  :: susw
    real(fp)                 , intent(in)  :: dclay
    logical                  , intent(in)  :: wave
    real(fp)                 , intent(in)  :: eps
    integer                  , intent(in)  :: subiw
    logical                  , intent(out) :: error
!
! Local variables
!
    integer       :: ntime
    integer       :: ii
    integer       :: k
    real(fp)      :: uau
    real(fp)      :: uav
    real(fp)      :: acw
    real(fp)      :: betacw
    real(fp)      :: ceavg
    real(fp)      :: cosphiwav
    real(fp)      :: dif_aks
    real(fp)      :: dif_upp
    real(fp)      :: dtt
    real(fp)      :: dzdn
    real(fp)      :: dzds
    real(fp)      :: fac1
    real(fp)      :: fac2
    real(fp)      :: fac_slp
    real(fp)      :: fcw1
    real(fp)      :: phiwav
    real(fp)      :: plead1
    real(fp)      :: plead2
    real(fp)      :: pi_tfor
    real(fp)      :: pi_tback
    real(fp)      :: p2
    real(fp)      :: phi
    real(fp)      :: qbtu
    real(fp)      :: qbtv
    real(fp)      :: rc
    real(fp)      :: rrr2
    real(fp)      :: sbt
    real(fp)      :: sinphiwav
    real(fp)      :: tau1t
    real(fp)      :: tback
    real(fp)      :: tfor
    real(fp)      :: time
    real(fp)      :: tt
    real(fp)      :: vrdelm
    real(fp)      :: vrra
    real(fp)      :: ua
    real(fp)      :: ubtotu
    real(fp)      :: ubtotv
    real(fp)      :: udt
    real(fp)      :: udt1
    real(fp)      :: udt2
    real(fp)      :: dudt
    real(fp)      :: ustar1t
    real(fp)      :: utvec
    real(fp)      :: uut
    real(fp)      :: uvt
    real(fp)      :: vcr
    real(fp)      :: veff
    real(fp)      :: rpower
    real(fp)      :: z
    real(fp)      :: ceavgtmp
    character(80) :: message
!
!! executable statements -------------------------------------------------------
!
    error = .false.
    !
    ! CALCULATE BED LOAD TRANSPORT
    ! Based on intra-wave near bed velocity signal of Isobe-Horikawa
    ! Implementation of TR2004
    !
    phiwav    = teta * degrad
    cosphiwav = cos(phiwav)
    sinphiwav = sin(phiwav)
    rc        = 30.0_fp * z0cur
    ceavg     = 0.0_fp
    !
    ! Make assumptions for friction angle
    !
    phi = 30.0_fp * degrad
    !
    !   BED LOAD TRANSPORT (instantaneous and averaging)
    !
    if (delm < ra/30.0_fp) then
       vrra = 0.0_fp
    else
       !
       ! No distinction between 2DH and 3D yet
       ! in 2DH vrdelm is okay
       ! in 3D better to base vrdelm on uuu and vvv (includes effects of streaming etc.)
       ! in 3D interpolate velocity to delm
       !
       vrdelm = u2dh   * log(30.0_fp*delm/ra) / (-1.0_fp+log(30.0_fp*h1  /ra))
       vrra   = vrdelm * log(30.0_fp*aks /rc) /          log(30.0_fp*delm/rc)
    endif
    tfor = 0.0_fp
    if(ubw > 0.0_fp) then
        tfor = uoff / (uon+uoff) * tp
        tback    = tp - tfor
        pi_tfor  = pi / tfor
        pi_tback = pi / tback
    endif
    betacw   = 0.25_fp * ((-1.0_fp+log(30.0_fp*h1/rc))/log(30.0_fp*aks/rc))**2
    acw      = abs(u2dh) / max(1.0e-6_fp , (abs(ubw)+abs(u2dh)))
    fcw1     = acw*betacw*fc1 + (1.0_fp-acw)*fw1
    ubtotu   = vrra * cos(phicur)
    ubtotv   = vrra * sin(phicur)
    !
    ! bed slope effects now included (see further below) 
    !
    if (wave .and. tp>1.0_fp .and. bedw>0.0_fp) then
       ntime = subiw
    else
       ntime = 1
    endif
    dtt    = tp / real(ntime,fp)
    fac1   = 0.5_fp * rhow * fcw1
    fac2   = 0.5_fp * d50  * rhosol / (dstar**0.3_fp)
    qbtu   = 0.0_fp
    qbtv   = 0.0_fp
    qbcu   = 0.0_fp
    qbcv   = 0.0_fp
    !
    ! phase lead in shear stress according to Nielsen (1992) now included
    !
    if (pangle > 0.0_fp) then
       plead1 = cos(pangle*degrad)
       plead2 = sin(pangle*degrad)
    endif
    !
    do ii = 1,ntime
       time = real(ii-1,fp) * dtt
       if (ubw>0.0_fp .and. tp>1.0_fp) then
          if (time < tfor) then
             udt = uon * sin(pi*time/tfor)
             if (pangle > 0.0_fp) then
               udt2 = uon * sin(pi_tfor*(time+dtt))
               udt1 = uon * sin(pi_tfor*(time-dtt))
             endif
          else
             udt = -uoff * sin(pi_tback*(time-tfor))
             if (pangle > 0.0_fp) then
               udt2 = -uoff * sin(pi_tback*(time+dtt-tfor))
               udt1 = -uoff * sin(pi_tback*(time-dtt-tfor))
             endif
          endif
          if (pangle > 0.0_fp) then
             udt = plead1*udt + plead2*tp*(udt2-udt1)/(4.0_fp*pi*dtt)
          endif
       else
          udt  = 0.0_fp
          dudt = 0.0_fp
       endif
       uut     = udt*cosphiwav + ubtotu
       uvt     = udt*sinphiwav + ubtotv
       utvec   = sqrt(max(1.0e-4_fp ,(uut**2 +uvt**2)))
       !
       ! bed slope effects on critical shear stress
       ! using Dey (2003?) as modified by Van Rijn (App in Report Z4056)
       ! (approximation where schocklitsch and Leitner factors are combined)
       !
       dzds    =  dzduu*uut/utvec + dzdvv*uvt/utvec
       !
       !! beta = atan(dzds) = longitudinal slope angle
       !! positive values refer to downsloping beds
       !! negative values refer to upsloping beds
       !
       dzdn    =  abs(dzduu*uvt/utvec + dzdvv*uut/utvec)
       !
       !! gamma = atan(dzdn) = lateral (transverse) slope angle
       !
       fac_slp = max((1.0_fp-(atan(dzds)/phi)),0.001_fp)**0.75_fp * max((1.0_fp-(atan(dzdn)/phi)),0.001_fp)**0.37_fp
       tau1t   = fac1 * utvec * utvec
       rrr2    = max(min(0.8_fp+0.2_fp*((tau1t/(fac_slp*taucr)-0.8_fp)/1.2_fp) , 1.0_fp) , 0.8_fp )
       tt      = max(0.0001_fp , (tau1t-rrr2*fac_slp*taucr)/(fac_slp*taucr))
       ustar1t = (tau1t/rhow)**0.5_fp
       sbt     = fsilt * fac2 * ustar1t * tt
       qbtu    = qbtu + uut/utvec*sbt
       qbtv    = qbtv + uvt/utvec*sbt
       !
       ! Current-related transport only
       !
       qbcu    = qbcu + ubtotu/utvec*sbt
       qbcv    = qbcv + ubtotv/utvec*sbt
    enddo
    !
    qbcu = qbcu/real(ntime,fp)
    qbcv = qbcv/real(ntime,fp)
    qbwu = qbtu/real(ntime,fp) - qbcu
    qbwv = qbtv/real(ntime,fp) - qbcv
    !
    if (susw>0.0_fp .and. tp>1.0_fp) then
       !
       ! Calculate critical (depth averaged) velocity
       !
       if (d50 >= dclay) then
          vcr = 5.75_fp * sqrt(drho*ag*d50*tetacr) * log10(4.0_fp*h1/d90)
       else
          write(message,'(a,e12.4,a)') 'd50 < ', dclay, ' not allowed'
          call prterr(lundia, 'P004', trim(message))
          error = .true.
          return
       endif
       !
       ! Suspended transport qswu/v due to waves, oriented in wave
       ! direction
       ! Expression modified by Walstra (2006)
       !
       ! STEP 1
       ! Integrate concentration between aks and 3*deltas
       !
       do k = kmax, 1, -1
          dif_aks  = aks/h1           - (1.0_fp+sig(k)-thick(k)/2.0_fp)
          dif_upp  = 3.0_fp*deltas/h1 - (1.0_fp+sig(k)-thick(k)/2.0_fp)
          if (dif_aks<=thick(k) .and. dif_aks>=0.0_fp) then
             !
             ! k-layer contains aks (take part above)
             !
             if (concin(k-1)<1.0e-6_fp .or. concin(k)<1.0e-6_fp) then
                ceavg    = ceavg + concin(k)*(1.0_fp-dif_aks/thick(k))*thick(k)*h1
             else
                rpower   = log(concin(k-1)/concin(k)) / log(  (h1*(1.0_fp+sig(k  ))*(h1-h1*(1.0_fp+sig(k-1)))) &
                &                                           / (h1*(1.0_fp+sig(k-1))*(h1-h1*(1.0_fp+sig(k  )))) )
                z        = ((1.0_fp+sig(k)+0.5_fp*thick(k))*h1 + aks) / 2.0_fp
                ceavgtmp = concin(k) * ((h1*(1.0_fp+sig(k))*(h1-z))/(z*(h1-h1*(1.0_fp+sig(k)))))**rpower
                ceavg    = ceavg + ceavgtmp*(1.0_fp-dif_aks/thick(k))*thick(k)*h1
             endif
          elseif (dif_aks<=0.0_fp .and. dif_upp>=0.0_fp) then
             !
             ! layer between aks and 3*deltas
             !
             ceavg = ceavg + concin(k)*thick(k)*h1
          elseif (dif_upp <= thick(k)) then
             !
             ! k-layer contains 3*deltas (take part below)
             !
             ceavg = ceavg + concin(k)*dif_upp*h1
             exit
          endif
       enddo
       !
       ! STEP 2
       ! Multiply integrated transports with orbital & drift velocity component
       !
       veff = sqrt(u2dh*u2dh+uon*uon) - vcr
       !
       ! Added phase function
       !
       if (fpco == 0.0_fp) then
          p2 = -tanh(100.0_fp*(rksrs/(ws*tp)-0.1_fp))
       else
          p2 = fpco
       endif
       !    
       if (veff > eps) then
          !
          ! Gamma has been reduced from 0.2 to 0.1 in TR2004
          !
          ua    = 0.1_fp * p2 * (uon**4-uoff**4) / (uon**3+uoff**3)
          uau   = ua * cosphiwav
          uav   = ua * sinphiwav
          !
          qswu = uau * ceavg
          qswv = uav * ceavg
       else
          qswu = 0.0_fp
          qswv = 0.0_fp
       endif
    else
       !
       ! SUSW == 0
       !
       qswu = 0.0_fp
       qswv = 0.0_fp
    endif
end subroutine bedtr2004
