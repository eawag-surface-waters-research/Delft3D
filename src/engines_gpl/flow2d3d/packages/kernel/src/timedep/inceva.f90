subroutine inceva(timnow    ,evaint    ,j         ,nmmaxj    ,nmmax     , &
                & evap      ,gdp       )
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
!    Function: Determine increments and update the current time
!              dependent value for rain/evaporation data (if inceva
!              = true) which model depends on value KEVA
! Method used: At each time step (if INTEVA=true) the increment
!              values (stored in D"value") are added to update "value"
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
    integer           , pointer :: it0eva
    integer           , pointer :: it1eva
    real(fp)          , pointer :: dt
    integer           , pointer :: luneva
    real(fp)          , pointer :: evapor
    real(fp)          , pointer :: devapo
    real(fp)          , pointer :: precip
    real(fp)          , pointer :: dpreci
    real(fp)          , pointer :: train
    real(fp)          , pointer :: dtrain
!
! Global variables
!
    integer                                                     :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays.
                                                                          !!  Due to the shift in the 2nd (M-)index, J = -2*NMAX + 1
    integer                                       , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                     :: nmmaxj !  Description and declaration in dimens.igs
    real(fp)                                                    :: timnow !!  Current timestep (multiples of dt)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: evap   !  Description and declaration in esm_alloc_real.f90
    character(1)                                  , intent(in)  :: evaint !  Description and declaration in tricom.igs
!
! Local variables
!
    integer    :: nm
    logical    :: first       ! Flag = TRUE in case a time-dependent file is read for the 1st time 
    logical    :: inteva      ! Interpolation method between consecutive data: 
                              ! N = No interpolation. Y = Linear interpolation. 
!
!
!! executable statements -------------------------------------------------------
!
!
    evapor      => gdp%gdheat%evapor
    devapo      => gdp%gdheat%devapo
    precip      => gdp%gdheat%precip
    dpreci      => gdp%gdheat%dpreci
    train       => gdp%gdheat%train
    dtrain      => gdp%gdheat%dtrain
    luneva      => gdp%gdluntmp%luneva
    dt          => gdp%gdexttim%dt
    it0eva      => gdp%gdinttim%it0eva
    it1eva      => gdp%gdinttim%it1eva
    !
    first = .false.
    if (evaint == 'Y') then
       inteva = .true.
    else
       inteva = .false.
    endif
    !
    ! Update rainfall/evaporation module time dependent data for TIMNOW > IT1EVA
    ! For INTTEVA = .false. (block function) define new working values before reading for new time
    !
    if (timnow > real(it1eva,fp)) then
       it0eva = it1eva
       if (.not. inteva) then
          precip = dpreci
          evapor = devapo
          train  = dtrain
          !
          ! Update evaporation (block function)
          !
          do nm = 1, nmmax
             evap(nm) = devapo
          enddo
       endif
       !
       ! Read new time dependent input
       !
       call updeva(luneva, timnow, dt, inteva, first, gdp)
       !
    endif
    !
    ! For interpolation INTEVA = .true. update data with step value
    !
    if (inteva) then
       precip = precip + dpreci
       evapor = evapor + devapo
       train  = dtrain + train
       !
       ! Update evaporation (step interpolation)
       !
       do nm = 1, nmmax
          evap(nm) = evapor
       enddo
    endif
end subroutine inceva
