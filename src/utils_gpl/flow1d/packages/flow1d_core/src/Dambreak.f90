module m_Dambreak
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2018.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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
!-------------------------------------------------------------------------------

    use m_GlobalParameters
    use m_struc_helper

   implicit none

   public ComputeDambreak

 type, public :: t_dambreak
    double precision :: start_location_x   
    double precision :: start_location_y    
    integer          :: algorithm            
    double precision :: crestlevelini       
    double precision :: breachwidthini     
    double precision :: crestlevelmin       
    double precision :: dischargecoeff     
    double precision :: f1                     
    double precision :: f2                   
    double precision :: ucrit                 
    double precision :: t0 

    ! Stored values
    double precision :: phase
    double precision :: struwi

 end type

   private

contains

subroutine ComputeDambreak(dambreak, s1m1, s1m2, u0, time0, time1, dt, dpu)


    type(t_dambreak), pointer      :: dambreak
    double precision, intent(in)   :: s1m1
    double precision, intent(in)   :: s1m2
    double precision, intent(in)   :: u0
    double precision, intent(in)   :: time0
    double precision, intent(in)   :: time1
    double precision, intent(in)   :: dt

    !output
    double precision, intent(inout):: dpu

    !locals
    double precision :: smax
    double precision :: smin
    double precision :: struwi
    double precision :: crl
    double precision :: eps = 1.0d-5
    double precision :: t0
    double precision :: width


    double precision ::f1
    double precision ::f2 
    double precision ::uc

    double precision ::hmx
    double precision ::hmn
    double precision ::qkennelijkgeschat

    t0 = dambreak%t0
    dambreak%phase = max(0.0, dambreak%phase)
    if (t0 == 0.0) then
       t0 = -1
    endif


    smax = max(s1m1, s1m2)
    smin = min(s1m1, s1m2)

    if (dambreak%algorithm == 1 .or. dambreak%algorithm == 0) then

       crl    = max(dambreak%crestlevelmin,  &
                    dambreak%crestlevelini -dpu/ dambreak%breachwidthini )  !hk: dpu = area??
       struwi = dpu / (dambreak%crestlevelini - crl)                    !hk: Confirmed

   elseif (dambreak%algorithm == 2) then                             !.and. .not. infuru

       crl    = max(dambreak%crestlevelmin, dambreak%crestlevelini - dpu/dambreak%breachwidthini )

       if (dambreak%phase <2) then

          if (crl <=dambreak%crestlevelmin+eps) then 
             t0 = time0
             dambreak%phase = dambreak%phase + 1
          endif 
        
       endif

       if (crl>=(dambreak%crestlevelmin+eps) .and. dambreak%phase < 2 .and. ((dambreak%crestlevelini - crl).gt.eps)) then
          struwi = dpu/(dambreak%crestlevelini - crl)
       else
          ! Use formula
          f1 = dambreak%f1
          f2 = dambreak%f2
          uc = dambreak%ucrit


          ! hk: nu af te vangen voor situatie geen stroming 19828
          hmx = max(0d0,smax - crl)
          hmn = max(0d0,smin - crl)
          qkennelijkgeschat = (gravity*(hmx - hmn))**1.5d0
          ! hk: nu af te vangen voor situatie geen stroming 19828

          if (time1-T0 > 0 .and. (.not.isnan(u0)) .and. dabs(u0) > uc) then                                         ! width = delta width
             width =  (f1*f2/dlog(10D0))*(qkennelijkgeschat/(uc*uc))          &
                     *(1.0/(1.0 + (f2*gravity*(time1-T0)/(3600.0d0*uc))))        &   
                     *(dt/3600.0d0)
          else
             width = 0.0d0
          endif

          struwi = dambreak%struwi + max(0.0, width)
          dpu = struwi*(dambreak%crestlevelini - dambreak%crestlevelmin)
      endif
   endif
    
   dambreak%t0     = t0      ! hk: store t0 
   dambreak%struwi = struwi  ! gets the structure width 

end subroutine ComputeDambreak

end 