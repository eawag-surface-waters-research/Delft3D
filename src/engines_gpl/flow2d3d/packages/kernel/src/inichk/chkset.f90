subroutine chkset(lundia    ,error     ,sferic    ,method    ,trasol    , &
                & dischy    ,solver    ,disctr    ,ktemp     , &
                & keva      ,iphisi    ,gdp       )
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
! Checks the combination of Domain Decomposition with various program modes
! Checks the combination of Z-model with various program modes
! The combination Domain Decomposition and Z-model is allowed
! Dredge is only allowed in combination with 3D morphology
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
    integer                       , pointer :: lsec
    integer                       , pointer :: lstsci
    integer                       , pointer :: ltur
    integer                       , pointer :: ltur2d
    integer                       , pointer :: nto
    integer                       , pointer :: ntoq
    integer                       , pointer :: ndro
    logical                       , pointer :: multi
    character(8)                  , pointer :: dpsopt
    character(8)                  , pointer :: dpuopt
    character(6)                  , pointer :: momsol
    logical                       , pointer :: drogue
    logical                       , pointer :: temp
    logical                       , pointer :: dredge
    logical                       , pointer :: wave
    logical                       , pointer :: iweflg
    logical                       , pointer :: struct
    logical                       , pointer :: sedim
    logical                       , pointer :: htur2d
    logical                       , pointer :: mudlay
    logical                       , pointer :: couplemod
    logical                       , pointer :: zmodel
    logical                       , pointer :: roller
    logical                       , pointer :: bubble
    integer                       , pointer :: numdomains
    integer                       , pointer :: nummappers
    integer                       , pointer :: rtcmod
    integer                       , pointer :: nrcmp
    logical                       , pointer :: flcut
    logical                       , pointer :: fl45
    logical                       , pointer :: waqol
!
! Global variables
!
    integer     , intent(in)  :: iphisi      !  Description and declaration in inttim.igs
    integer     , intent(in)  :: keva        !  Description and declaration in tricom.igs
    integer     , intent(in)  :: ktemp       !  Description and declaration in tricom.igs
    integer                   :: lundia      !  Description and declaration in inout.igs
    logical     , intent(out) :: error       !!  Flag=TRUE if an error is encountered
    logical     , intent(in)  :: sferic      !  Description and declaration in tricom.igs
    character(*), intent(in)  :: dischy      !  Description and declaration in tricom.igs
    character(*), intent(in)  :: disctr      !  Description and declaration in tricom.igs
    character(*)              :: method      !  Description and declaration in tricom.igs
    character(*), intent(in)  :: solver      !  Description and declaration in tricom.igs
    character(*)              :: trasol      !  Description and declaration in tricom.igs
!
! Local variables
!
    integer        :: ierror                 ! Error counter
    integer        :: iwarn                  ! Warning counter
    character(100) :: errtxt
!
!! executable statements -------------------------------------------------------
!
    lsec                => gdp%d%lsec
    lstsci              => gdp%d%lstsci
    ltur                => gdp%d%ltur
    ltur2d              => gdp%d%ltur2d
    nto                 => gdp%d%nto
    ntoq                => gdp%d%ntoq
    ndro                => gdp%d%ndro
    multi               => gdp%gdmorpar%multi
    dpsopt              => gdp%gdnumeco%dpsopt
    dpuopt              => gdp%gdnumeco%dpuopt
    momsol              => gdp%gdnumeco%momsol
    drogue              => gdp%gdprocs%drogue
    temp                => gdp%gdprocs%temp
    dredge              => gdp%gdprocs%dredge
    wave                => gdp%gdprocs%wave
    iweflg              => gdp%gdprocs%iweflg
    struct              => gdp%gdprocs%struct
    sedim               => gdp%gdprocs%sedim
    htur2d              => gdp%gdprocs%htur2d
    mudlay              => gdp%gdprocs%mudlay
    couplemod           => gdp%gdprocs%couplemod
    zmodel              => gdp%gdprocs%zmodel
    roller              => gdp%gdprocs%roller
    bubble              => gdp%gdprocs%bubble
    numdomains          => gdp%gdprognm%numdomains
    nummappers          => gdp%gdprognm%nummappers
    rtcmod              => gdp%gdrtc%rtcmod
    nrcmp               => gdp%gdtfzeta%nrcmp
    flcut               => gdp%gdtmpfil%flcut
    fl45                => gdp%gdtmpfil%fl45
    waqol               => gdp%gdwaqpar%waqol
    !
    ierror = 0
    iwarn  = 0
    !
    ! For checking functionality combinations:
    ! Must DD be specified as nummappers>=1 or numdomains>1?
    ! The behaviour around a DD boundary is the same when comparing one domain
    ! with an internal DD boundary with two domains. So use nummappers>=1.
    !
    if (nummappers >= 1) then
       !
       ! errors
       !
       if (iweflg) then
          call prterr(lundia    ,'M001'    ,'Internal Wave option'          ,gdp       )
          ierror = ierror+ 1
       endif
       if (mudlay) then
          call prterr(lundia    ,'M001'    ,'Fluid Mud'          ,gdp       )
          ierror = ierror+ 1
       endif
       if (solver(1:2)=='gs') then
          call prterr(lundia    ,'M001'    ,'Gauss Seidel solver',gdp       )
          ierror = ierror+ 1
       endif
       !
       ! warnings
       !
       if (roller) then
          call prterr(lundia    ,'M002'    ,'Roller model'       ,gdp       )
          iwarn = iwarn + 1
       endif
       if (ltur2d>0) then
          call prterr(lundia    ,'M002'    ,'Q2E 2D Turb.model'  ,gdp       )
          iwarn = iwarn + 1
       endif
       if (nrcmp>0) then
          call prterr(lundia    ,'M002'    ,'Tide generating forces'        ,gdp       )
          iwarn = iwarn + 1
       endif
       if (lsec>0) then
          call prterr(lundia    ,'M002'    ,'Secondary Flow'     ,gdp       )
          iwarn = iwarn + 1
       endif
       if (ndro>0) then
          call prterr(lundia    ,'M002'    ,'Drogues' ,gdp       )
          iwarn = iwarn + 1
       endif
       if (disctr(1:4)=='expl') then
          call prterr(lundia    ,'M002'    ,'Explicit method'    ,gdp       )
          iwarn = iwarn + 1
       endif
       if (wave) then
          call prterr(lundia    ,'M002'    ,'Wave effect'        ,gdp       )
          iwarn = iwarn + 1
       endif
       if (sferic) then
          call prterr(lundia    ,'M002'    ,'Spherical coordinate'          ,gdp       )
          iwarn = iwarn + 1
       endif
       if (sedim) then
          call prterr(lundia    ,'M002'    ,'3D Morphology'      ,gdp       )
          iwarn = iwarn + 1
       endif
       if (momsol /= 'cyclic') then
          call prterr(lundia    ,'M002'    ,'Momentum solver other than cyclic'  ,gdp       )
          iwarn = iwarn + 1
       endif
       if (couplemod) then
          call prterr(lundia    ,'M002'    ,'Online Couple'      ,gdp       )
          iwarn = iwarn + 1
       endif
    endif
    !
    ! Z-MODEL case
    !
    if (zmodel) then
       !
       ! errors
       !
       if (rtcmod == dataFromRTCToFLOW) then
          call prterr(lundia    ,'Z011'    ,'Real Time Control'  ,gdp       )
          ierror = ierror+ 1
       endif
       if (couplemod) then
          call prterr(lundia    ,'Z011'    ,'Online Couple'      ,gdp       )
          ierror = ierror+ 1
       endif
       if (sedim) then
          call prterr(lundia    ,'Z011'    ,'3D Morphology'      ,gdp       )
          ierror = ierror+ 1
       endif
       if (iweflg) then
          call prterr(lundia    ,'Z011'    ,'Internal Wave option'          ,gdp       )
          ierror = ierror+ 1
       endif
       if (mudlay) then
          call prterr(lundia    ,'Z011'    ,'Fluid Mud'          ,gdp       )
          ierror = ierror+ 1
       endif
       if (solver(1:2)=='gs') then
          call prterr(lundia    ,'Z011'    ,'Gauss Seidel solver',gdp       )
          ierror = ierror+ 1
       endif
       if (ltur2d>0) then
          call prterr(lundia    ,'Z011'    ,'Q2E 2D Turb.model'  ,gdp       )
          ierror = ierror+ 1
       endif
       if (lsec>0) then
          call prterr(lundia    ,'Z011'    ,'Secondary Flow'     ,gdp       )
          errtxt = 'Use Sigma version of Delft3D-FLOW instead'
          call prterr(lundia    ,'U021'    ,errtxt    ,gdp       )
          ierror = ierror+ 1
       endif
       if (disctr(1:4)=='expl') then
          call prterr(lundia    ,'Z011'    ,'Explicit method'    ,gdp       )
          ierror = ierror+ 1
       endif
       if (wave) then
          call prterr(lundia    ,'Z011'    ,'Wave effect'        ,gdp       )
          ierror = ierror+ 1
       endif
       if (roller) then
          call prterr(lundia    ,'Z011'    ,'Roller model'       ,gdp       )
          ierror = ierror+ 1
       endif
       !
       ! warnings
       !
       if (method(1:3)/='adi') then
          call prterr(lundia    ,'Z011'    ,method    ,gdp       )
          method = 'adi          '
          errtxt = 'ADI numerical method will be applied instead'
          call prterr(lundia    ,'U190'    ,errtxt    ,gdp       )
          iwarn = iwarn + 1
       endif
       if (ktemp/=5 .and. ktemp>0) then
          call prterr(lundia    ,'Z012'    ,'Heat model other than option 5',gdp       )
          iwarn = iwarn + 1
       endif
       if (keva>0) then
          call prterr(lundia    ,'Z012'    ,'Evaporation model'  ,gdp       )
          iwarn = iwarn + 1
       endif
       if (nrcmp>0) then
          call prterr(lundia    ,'Z012'    ,'Tide generating forces'        ,gdp       )
          iwarn = iwarn + 1
       endif
       if (ntoq>0) then
          call prterr(lundia    ,'Z012'    ,'Q-H boundary'       ,gdp       )
          iwarn = iwarn + 1
       endif
       if (ndro>0) then
          call prterr(lundia    ,'Z012'    ,'Drogues' ,gdp       )
          iwarn = iwarn + 1
       endif
       if (sferic) then
          call prterr(lundia    ,'Z012'    ,'Spherical coordinate'          ,gdp       )
          iwarn = iwarn + 1
       endif
       if (htur2d) then
          call prterr(lundia    ,'Z012'    ,'HLES'    ,gdp       )
          iwarn = iwarn + 1
       endif
       if (struct) then
          call prterr(lundia    ,'Z012'    ,'Structures'         ,gdp       )
          iwarn = iwarn + 1
       endif
    else
       !
       ! SIGMA-MODEL case
       !
       !
       ! errors
       !
       if (fl45 .or. flcut) then
          errtxt = 'Combination of sigma-model and cut cells is not available'
          call prterr(lundia ,'U021' ,errtxt ,gdp )
          ierror = ierror+ 1
       endif
       !
       ! warnings
       !
       if (bubble) then
          errtxt = 'Combination of sigma-model and bubble screens has not yet been tested'
          call prterr(lundia ,'Z013' ,errtxt ,gdp )
          iwarn = iwarn + 1
       endif
    endif
    !
    !
    !
    if (dredge) then
       !
       ! errors
       !
       if (.not. sedim) then
          call prterr(lundia    ,'P004'    ,'Dredging is only allowed in combination with 3D Morphology'  ,gdp       )
          ierror = ierror+ 1
       endif
    endif
    !
    !
    !
    if (multi) then
       call prterr(lundia, 'U190', 'Mormerge: Mass balance only satisfied for all runs together', gdp)
       iwarn = iwarn + 1
    endif
    !
    !
    !
    if (dpsopt=='DP' .and. dpuopt=='MEAN') then
       call prterr(lundia, 'P004', 'The combination of dpsopt=DP and dpuopt=MEAN is invalid', gdp)
       ierror = ierror+ 1
    endif
    !
    !
    !
    if (waqol) then
       if (sedim) then
          call prterr(lundia, 'P004', 'Morphology/sediments can not be switched on in both FLOW and WAQ', gdp)
          ierror = ierror+ 1
       endif
    endif
    !
    !
    !
    if (iphisi > 0 .and. lstsci > 7) then
       !
       ! The output to ascii file can be written for a maximum of 7 constituents (prthis.f90)
       !
       call prterr(lundia, 'P004', 'Writing to a tri-prt (ascii) file is not supported when using more than 7 constituents', gdp) 
       ierror = ierror+ 1
    endif
    !
    !
    !
    if (parll) then
       !
       ! errors
       !
       if (drogue) then
          errtxt = 'Drogues/walking monitor points are not available in parallel computations'
          call prterr(lundia ,'U021' ,errtxt ,gdp )
          ierror = ierror+ 1
       endif
    endif
    !
    !
    !
    if (ierror> 0) then
       error = .true.
    endif
end subroutine chkset
