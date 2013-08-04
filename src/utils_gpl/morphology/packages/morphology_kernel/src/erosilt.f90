subroutine erosilt(thick    ,kmax      ,ws        ,lundia   , &
                 & thick0   ,thick1    ,fixfac    ,srcmax   , &
                 & frac     ,oldmudfrac,flmd2l    ,iform    , &
                 & par      ,numintpar ,numrealpar,numstrpar, &
                 & dllfunc  ,dllhandle ,intpar    ,realpar  , &
                 & strpar   , &
! output:
                 & error    ,wstau     ,sinkse    ,sourse   )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                
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
! Method used:
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sediment_basics_module
    use morphology_data_module, only: RP_TAUB
    use message_module, only: write_error
    !
    implicit none
    !
    integer                             , intent(in)    :: iform
    integer                             , intent(in)    :: numintpar
    integer                             , intent(in)    :: numrealpar
    integer                             , intent(in)    :: numstrpar
    integer                             , intent(in)    :: kmax
    integer                                             :: lundia   !  Description and declaration in inout.igs
    integer       , dimension(numintpar), intent(inout) :: intpar
    integer(pntrsize)                   , intent(in)    :: dllhandle
    real(fp)       , dimension(0:kmax)  , intent(in)    :: ws
    real(fp)                            , intent(out)   :: wstau
    real(fp)       , dimension(kmax)    , intent(in)    :: thick
    real(fp)                            , intent(in)    :: thick0
    real(fp)                            , intent(in)    :: thick1
    real(fp)                            , intent(in)    :: fixfac
    real(fp)                            , intent(in)    :: srcmax
    real(fp)                            , intent(in)    :: frac
    real(fp)     , dimension(30)        , intent(inout) :: par
    real(fp)                            , intent(out)   :: sinkse
    real(fp)                            , intent(out)   :: sourse
    real(hp)     , dimension(numrealpar), intent(inout) :: realpar
    logical                             , intent(out)   :: error
    logical                             , intent(in)    :: oldmudfrac
    logical                             , intent(in)    :: flmd2l
    character(256), dimension(numstrpar), intent(inout) :: strpar
    character(256)                      , intent(in)    :: dllfunc
!
! Local variables
!
    integer  :: k
    real(fp) :: sour
    real(fp) :: sink
    real(fp) :: taub
    real(fp) :: taum
    real(fp) :: entr
    real(fp) :: tcrdep
    real(fp) :: tcrero
    real(fp) :: eropar
    !
    ! Interface to dll is in High precision!
    !
    real(hp)                    :: sink_dll
    real(hp)                    :: sour_dll
    integer(pntrsize)           :: ierror_ptr
    integer(pntrsize), external :: perf_function_erosilt
    character(1024)             :: errmsg
    character(256)              :: message     ! Contains message from user dll
!
!! executable statements ------------------
!
    error  = .false.
    !
    ! Calculate total (possibly wave enhanced) roughness
    !
    taub   = real(realpar(RP_TAUB), fp)
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
    ! calculation both for mud and floc
    !
    if (flmd2l) then
       entr   = par(11)
       tcrdep = par(12)
       !
       ! maximum erosion is sediment available at bed
       ! (ignores sediment settling during the current morphological timestep)
       !
       sour = entr
       if (tcrdep > 0.0_fp) then
          sink = max(0.0_fp , 1.0_fp-taub/tcrdep)
       else
          sink = 0.0
       endif
    else
       if (iform == -3) then
          eropar = par(11)
          tcrdep = par(12)
          tcrero = par(13)
          !
          ! Default Partheniades-Krone formula
          !
          taum = max(0.0_fp, taub/tcrero - 1.0_fp)
          sour = eropar * taum
          if (tcrdep > 0.0_fp) then
             sink = max(0.0_fp , 1.0_fp-taub/tcrdep)
          else
             sink = 0.0_fp
          endif
       elseif (iform == 15) then
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
          ierror_ptr = 0
          ierror_ptr = perf_function_erosilt(dllhandle       , dllfunc           , &
                                             intpar          , numintpar         , &
                                             realpar         , numrealpar        , &
                                             strpar          , numstrpar         , &
                                             sink_dll        , sour_dll          , &
                                             message)
          call vsemlun
          if (ierror_ptr /= 0) then
             errmsg = 'Cannot find function "'//trim(dllfunc)//'" in dynamic library.'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
          if (message /= ' ') then
             errmsg = 'Message from user defined erosion/deposition formulae '//trim(dllfunc)//' :'
             call write_error(errmsg, unit=lundia)
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
       else
          write (errmsg,'(a,i,a)') 'Invalid transport formula ',iform,' for mud fraction.'
          call write_error(errmsg, unit=lundia)
       endif
    endif
    !
    wstau         = ws(kmax) * sink
    !
    if (.not.flmd2l) then
       if (oldmudfrac) then
          sour = fixfac * sour
       else
          sour = fixfac * frac * sour
       endif
    endif
    !
    sour   = min(sour, srcmax)
    !
    sourse = sour / thick0
    sinkse = wstau / thick1
end subroutine erosilt
