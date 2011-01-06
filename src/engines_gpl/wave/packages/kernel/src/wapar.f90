subroutine wapar(hrm       ,dir       ,deph      ,tp        ,fxhis     , &
               & fyhis     ,dish      ,wavel     ,wavek     ,ldep      , &
               & option    ,fx        ,fy        ,qbsli     ,dismax    , &
               & corht     ,swdis     ,grav      )
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
!     Input:
!     -------
!     Hrm,DIR,DEPH,TP,FXHIS,FYHIS,DISH,QBH,WAVEL,WAVEK,LDEP,OPTION
!     LDEP   : logical variable, .true. when depth or waveheight is too small
!     OPTION : 1: wave forces based on gradients, 2: from dissipation
!
!     Output:
!     --------
!     FX,FY,QBSLI
!     FX,FY : wave forces based on gradients(1) or dissipation(2)
!             Unit: N/m2
!     QBSLI : 'actual' fraction breaking waves, Unit [-]
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
    !
! Common variables
    real            ::  pi, twopi, wort2, gamma
    common /const /     pi, twopi, wort2, gamma
!
! Global variables
!
    integer                        :: option
    logical, intent(in)            :: corht
    logical, intent(in)            :: ldep
    logical, intent(in)            :: swdis
    real   , intent(in)            :: deph
    real   , intent(in)            :: dir
    real   , intent(in)            :: dish
    real   , intent(in)            :: dismax
    real                           :: fx
    real   , intent(in)            :: fxhis
    real                           :: fy
    real   , intent(in)            :: fyhis
    real                           :: grav
    real   , intent(in)            :: hrm
    real   , intent(out)           :: qbsli
    real   , intent(in)            :: tp
    real   , intent(in)            :: wavek
    real   , intent(in)            :: wavel
!
! Local variables
!
    integer :: choice
    real    :: factor
    real    :: fmax
    real    :: frc
    real    :: ftot
    real    :: tr_angle
!
!! executable statements -------------------------------------------------------
!
    choice = 0
    if (corht) then
       choice = 1
    endif
    option = 2
    if (.not.swdis) then
       option = 1
    endif
    if (ldep) then
       fx    = 0.0
       fy    = 0.0
       qbsli = 0.0
    else
       if (option == 1) then
          !
          !! Determine wave forces based on gradients radiation stresses
          !
          fx = fxhis
          fy = fyhis
       elseif (option == 2) then
          !
          ! Determine wave forces based on dissipation
          !
          frc      = dish*tp/wavel
          !
          tr_angle = 0.0174533*dir
          fx       = cos(tr_angle)*frc
          fy       = sin(tr_angle)*frc
          if (choice == 1) then
             ftot = fx*fx + fy*fy
             if (ftot>0.0) then
                ftot   = sqrt(ftot)
                fmax   = dismax/sqrt(grav*deph)
                factor = min(fmax/ftot, 1.E0)
                fx     = factor*fx
                fy     = factor*fy
             endif
          endif
       else
       endif
    endif
end subroutine wapar
