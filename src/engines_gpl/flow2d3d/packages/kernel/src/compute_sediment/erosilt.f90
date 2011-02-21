subroutine erosilt(l        ,thick    ,rhowat   ,rlabda   ,vicmol   , &
                 & kmax     ,hrms     ,uorb     ,tp       ,teta     ,ws       , &
                 & wstau    ,entr     ,dicww    ,seddif   ,lundia   ,rhosol   , &
                 & nm       ,h0       ,h1       ,z0rou    ,tauadd   ,um       , &
                 & vm       ,uuu      ,vvv      ,taub     ,salinity ,temperature, &
                 & error    ,ag       ,vonkar   ,fixfac   , &
                 & frac     ,sinkse   ,sourse   ,oldmudfrac, flmd2l , tcrdep  , &
                 & tcrero   ,eropar   ,timsec   ,iform    , &
                 & numintpar,numrealpar,numstrpar,dllfunc ,dllhandle, &
                 & intpar   ,realpar  ,strpar   )
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
!    Function: Computes sediment fluxes at the bed using
!              the Partheniades-Krone formulations.
!              Arrays SOURSE and SINKSE are filled
!              Array seddif id filled with dicww for mud
! Method used:
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
    !
    include 'sedparams.inc'
    !
    integer                                                   , intent(in)  :: kmax
    integer                                                   , intent(in)  :: nm
    integer                                                   , intent(in)  :: l
    integer                                                                 :: lundia   !  Description and declaration in inout.igs
    real(fp)                                                  , intent(in)  :: salinity
    real(fp)                                                  , intent(in)  :: temperature
    real(fp)                                                  , intent(in)  :: vicmol
    real(fp)                                                  , intent(in)  :: entr
    real(fp)                                                  , intent(in)  :: hrms
    real(fp)                                                  , intent(in)  :: rlabda
    real(fp)                                                  , intent(in)  :: teta
    real(fp)                                                  , intent(in)  :: tp
    real(fp)                                                  , intent(in)  :: uorb
    real(fp)  , dimension(0:kmax)                             , intent(in)  :: ws
    real(fp)                                                  , intent(out) :: wstau
    real(fp)  , dimension(0:kmax)                             , intent(in)  :: dicww
    real(fp)                                                  , intent(in)  :: rhowat !  Description and declaration in rjdim.f90
    real(fp)  , dimension(0:kmax)                             , intent(out) :: seddif
    real(fp)                                                  , intent(in)  :: rhosol
    real(fp)  , dimension(kmax)                               , intent(in)  :: thick
    real(fp)                                                  , intent(in)  :: h0
    real(fp)                                                  , intent(in)  :: h1
    real(fp)                                                  , intent(in)  :: z0rou
    real(fp)                                                  , intent(in)  :: tauadd
    real(fp)                                                  , intent(in)  :: um
    real(fp)                                                  , intent(in)  :: uuu
    real(fp)                                                  , intent(in)  :: vm
    real(fp)                                                  , intent(in)  :: vvv
    real(fp)                                                  , intent(in)  :: taub
    logical                                                   , intent(out) :: error
    real(fp)                                                  , intent(in)  :: ag
    real(fp)                                                  , intent(in)  :: vonkar
    real(fp)                                                  , intent(in)  :: fixfac
    real(fp)                                                  , intent(in)  :: frac
    real(fp)                                                  , intent(out) :: sinkse
    real(fp)                                                  , intent(out) :: sourse
    real(fp)                                                  , intent(in)  :: timsec
    logical                                                   , intent(in)  :: oldmudfrac
    logical                                                   , intent(in)  :: flmd2l
    real(fp)                                                  , intent(in)  :: tcrdep
    real(fp)                                                  , intent(in)  :: tcrero
    real(fp)                                                  , intent(in)  :: eropar
    integer                                                   , intent(in)  :: iform
    integer, dimension(numintpar)   , intent(inout):: intpar
    integer                         , intent(in)   :: numintpar
    integer                         , intent(in)   :: numrealpar
    integer                         , intent(in)   :: numstrpar
    real(hp), dimension(numrealpar) , intent(inout):: realpar
    character(256), dimension(numstrpar), intent(inout):: strpar
    character(256)                  , intent(in)   :: dllfunc
    integer                         , intent(in)   :: dllhandle
!
! Local variables
!
    integer  :: k
    integer  :: kn
    real(fp) :: sour
    real(fp) :: sink
    real(fp) :: taum
    real(fp) :: thick0
    real(fp) :: thick1

    ! Interface to dll is in High precision!
    !
    real(fp)          :: chezy
    real(fp)          :: sag
    real(fp)          :: ee
    real(hp)          :: sink_dll
    real(hp)          :: sour_dll
    real(fp)          :: tauba
    integer           :: ierror
    integer, external :: perf_function_erosilt
    character(256)    :: errmsg
    character(256)    :: message     ! Contains message from
!
!! executable statements ------------------
!
    ee     = exp(1.0_fp)
    sag    = sqrt(ag)
    error  = .false.
    !
    ! Calculate total (possibly wave enhanced) roughness
    !
    chezy = sag * log( 1.0_fp + h1/max(1.0e-8_fp,ee*z0rou) ) / vonkar
    !
    tauba = sqrt(taub**2 + tauadd**2)
    !
    thick0 = thick(kmax) * h0
    thick1 = thick(kmax) * h1
    !
    ! Bed transport following Partheniades and Krone
    ! but in case of fluid mud, source term is determined by
    ! fluid mud part (sourmu). Information is passed via entr()
    ! maximum erosion is sediment available at bed (ignores sediment
    ! settling during the current morphological timestep)
    ! In case of fluid mud the maximum erosion is determined in sourmu
    ! of the fluid mud module. So ignore this check when fluid mud.
    ! Also, taum is not required in the formulation since whether or not
    ! and how much entrainment occurs is entirely handled by the sourmu
    ! routine.
    !
    ! For 3D model set sediment diffusion coefficient
    ! NOTE THAT IF ALGEBRAIC OR K-L TURBULENCE MODEL IS USED THEN WAVES
    ! ONLY AFFECT THE VERTICAL TURBULENT MIXING VIA THE ENHANCED BED
    ! ROUGHNESS
    !
    if (kmax > 1) then
       do k = 1, kmax
          seddif(k) = dicww(k)
       enddo
    endif
    !
    ! calculation both for mud and floc
    !
    if (flmd2l) then
       !
       ! maximum erosion is sediment available at bed
       ! (ignores sediment settling during the current morphological timestep)
       !
       sour = entr
       if (tcrdep > 0.0) then
          sink = max(0.0_fp , 1.0-tauba/tcrdep)
       else
          sink = 0.0
       endif
    else
       if (iform == -1) then
          !
          ! Default Partheniades-Krone formula
          !
          taum = max(0.0_fp, tauba/tcrero - 1.0)
          sour = eropar * taum
          if (tcrdep > 0.0) then
             sink = max(0.0_fp , 1.0-tauba/tcrdep)
          else
             sink = 0.0
          endif
       elseif (iform == 15) then
          !
          ! User defined formula in DLL
          ! Input parameters are passed via realpar/intpar/strpar-arrays
          !
          if (numrealpar < 30) then
             write(errmsg,'(a,a,a)') 'Insufficient space to pass real values to transport routine.'
             call prterr (lundia,'U021', trim(errmsg))
             error = .true.
             return
          endif
          realpar( 1) = real(timsec ,hp)
          realpar( 2) = real(um     ,hp)
          realpar( 3) = real(vm     ,hp)
          realpar( 4) = real(sqrt(um*um + vm*vm),hp)
          realpar( 5) = real(uuu    ,hp)
          realpar( 6) = real(vvv    ,hp)
          realpar( 7) = real(sqrt(uuu*uuu + vvv*vvv),hp)
          if (kmax>1) then
             realpar( 8) = real(h1*thick(kmax)/2.0_fp,hp)
          else
             realpar( 8) = real(h1/ee,hp)
          endif
          realpar( 9) = real(h1     ,hp)
          realpar(10) = real(chezy  ,hp)
          realpar(11) = real(hrms  ,hp)
          realpar(12) = real(tp    ,hp)
          realpar(13) = real(teta  ,hp)
          realpar(14) = real(rlabda,hp)
          realpar(15) = real(uorb  ,hp)
          realpar(16) = 0.0_hp !real(di50   ,hp)
          realpar(17) = 0.0_hp !real(dss    ,hp)
          realpar(18) = 0.0_hp !real(dstar  ,hp)
          realpar(19) = 0.0_hp !real(d10    ,hp)
          realpar(20) = 0.0_hp !real(d90    ,hp)
          realpar(21) = 0.0_hp !real(mudfrac,hp)
          realpar(22) = 1.0_hp !real(hidexp ,hp)
          realpar(23) = real(ws(kmax)  ,hp) ! Vertical velocity near bedlevel
          realpar(24) = real(rhosol    ,hp)
          realpar(25) = real(rhowat    ,hp) ! Density of water
          realpar(26) = real(salinity,hp)
          realpar(27) = real(temperature,hp)
          realpar(28) = real(ag        ,hp)
          realpar(29) = real(vicmol    ,hp)
          realpar(30) = real(tauba     ,hp)
          !
          ! Initialisation of output variables of user defined transport formulae
          !
          sink_dll    = 0.0_hp
          sour_dll    = 0.0_hp
          message     = ' '
          !
          ! psem/vsem is used to be sure this works fine in DD calculations
          !
          call psemlun
          ierror = perf_function_erosilt(dllhandle       , dllfunc           , &
                                         intpar          , numintpar         , &
                                         realpar         , numrealpar        , &
                                         strpar          , numstrpar         , &
                                         sink_dll        , sour_dll          , &
                                         message)
          call vsemlun
          if (ierror /= 0) then
             write(errmsg,'(a,a,a)') 'Cannot find function "',trim(dllfunc),'" in dynamic library.'
             call prterr (lundia,'U021', trim(errmsg))
             error = .true.
             return
          endif
          if (message /= ' ') then
             write (lundia,'(a,a,a)') '*** ERROR Message from user defined erosion/deposition formulae ',trim(dllfunc),' :'
             write (lundia,'(a,a  )') '          ', trim(message)
             write (lundia,'(a    )') ' '
             error = .true.
             return
          endif
          !
          ! Output parameters
          !
          sour    = real(sour_dll,fp)
          sink    = real(sink_dll,fp)
       endif
    endif
    !
    wstau         = ws(kmax) * sink
    if (.not.flmd2l) then
       if (oldmudfrac) then
          sour = fixfac * sour
       else
          sour = fixfac * frac * sour
       endif
    endif
    sourse = sour / thick0
    sinkse = wstau / thick1
end subroutine erosilt
