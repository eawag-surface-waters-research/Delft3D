!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2021.                                
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
module horton
   implicit none	

   !! Horton is based on in/decrease of infiltration between min and max value.
   integer, parameter :: HORTON_CAPSTAT_NOCHANGE = 0 !< No change in infiltration state
   integer, parameter :: HORTON_CAPSTAT_DECREASE = 1 !< Infiltration in decreasing mode
   integer, parameter :: HORTON_CAPSTAT_RECOVERY = 2 !< Infiltration in recovery/increasing mode
   contains 
   
   !> Computes infiltration capacity as defined by Horton equations.
   !!
   !! Infiltration capacity defined in mm/hr, decrease and recovery rate in 1/hr.
   !! Typical timestep used in application is 1 minute (i.e. much smaller than 1 hour),
   !! otherwise computation of infiltration volume (in mm) should be more refined
   !! (using integral of capacity function, depending on state recovery or decrease).
   
   function infiltration_horton_formula(n, MinInfCap, MaxInfCap, DecreaseRate, RecoveryRate, PreviousInfCap, NewInfCap, &
                                       TimestepSize, Dt, InitialStorage, Rainfall, InfCapState, InfiltrationMM) result(ierr)
      use dhydrology_error
      
      integer,                    intent(in   ) :: n                  !< Array length (grid cell count)
      double precision,           intent(in   ) :: MinInfCap(n)       !< Minimum infiltration capacity (mm/hr)
      double precision,           intent(in   ) :: MaxInfCap(n)       !< Maximum infiltration capacity (mm/hr)
      double precision,           intent(in   ) :: DecreaseRate(n)    !< Decrease rate (1/hr)
      double precision,           intent(in   ) :: RecoveryRate(n)    !< Recovery rate (1/hr)
      double precision,           intent(in   ) :: PreviousInfCap(n)  !< Last infiltration capacity (mm/hr)
      double precision,           intent(  out) :: NewInfCap(n)       !< New infiltration capacity (mm/hr)
      double precision,           intent(in   ) :: TimestepSize       !< Timestep size (s)
      double precision,           intent(inout) :: Dt(n)              !< Time in hours since start of decreasing/recovery mode (hr)
      double precision,           intent(in   ) :: InitialStorage(n)  !< Initial storage (=storage at start of timestep) (m)
      double precision,           intent(in   ) :: Rainfall(n)        !< Rainfall in current timestep (or more precise: additional ground rainfall, so minus interception)
      integer,                    intent(  out) :: InfCapState(n)     !< Infiltration capacity state; (one of HORTON_CAPSTAT_(NOCHANGE|RECOVERY|INCREASE))
      double precision, optional, intent(  out) :: InfiltrationMM(n)  !< Infiltration amount (mm)
      integer                                   :: ierr               !< Result status, DHYD_NOERR if successful.
      
      ! local
      double precision, allocatable   :: Dt1(:)
      integer, parameter              :: NrSecondsPerHour = 3600
      double precision                :: RFrac
      double precision, allocatable   :: ratio(:)
      integer                         :: i
      
      ierr = DHYD_NOERR

      allocate(Dt1(n))
      allocate(ratio(n))


      RFRAC = TimestepSize / NrSecondsPerHour

      do i = 1, n
         DT1(i)   = DT(i)  + RFRAC
         if(MaxInfCap(i) <= MinInfCap(i)) then
         !      constant infiltration capacity; infiltration state not changed
            InfCapState(i) = HORTON_CAPSTAT_NOCHANGE
         else if (PreviousInfCap(i) >= MaxInfCap(i)) then
         !      Previous infiltration capacity is at maximum value; now decrease
            InfCapState(i) = HORTON_CAPSTAT_DECREASE
            if (DT(i) > RFRAC .and. PreviousInfCap(i) >= MaxInfCap(i)) then
               DT1(i) = RFRAC
               DT(i)  = 0d0
            end if
         else if(PreviousInfCap(i) <= MinInfCap(i)) then
         !      Previous infiltration capacity is at minimum value, now increase
            InfCapState(i) = HORTON_CAPSTAT_RECOVERY
            if(DT(i) > RFRAC .and. PreviousInfCap(i) <= MinInfCap(i)) then
               DT1(i) = RFRAC
               DT(i)  = 0d0
            end if
         end if
      enddo
      ! additional checks: decrease of infiltration capacity as long as there is storage and/or rain
      !                    recovery of infiltration capacity as soon as storage is empty and no rain
      ! note: not just simple  ft = fc + (f0-fc)*exp (-kt)
      !       trick required for computation of t for intermediate switch from decrease to recovery or vice versa, before reaching max. or min.

      Ratio = -1d0

      do i = 1, n
         if(InfCapState(i) == HORTON_CAPSTAT_DECREASE .and. InitialStorage(i) <= 0d0 .and. Rainfall(i) <= 0d0) then
            ! state is decrease, but no storage and no rain anymore: switch to recovery
            InfCapState(i) = HORTON_CAPSTAT_RECOVERY
            Ratio(i) = (PreviousInfCap(i)-MaxInfcap(i)) / (MinInfCap(i)- MaxInfCap(i))
            if(ratio(i) > 0d0) then
               DT1(i) = -1/RecoveryRate (i)* log(ratio(i))  + RFRAC
            else if(ratio (i)<= 0d0) then
               DT1(i) = 9999d0
            end if
            DT(i)  = max(0d0, DT1(i) - RFRAC)
            else if(InfCapState(i) == HORTON_CAPSTAT_RECOVERY .and. (InitialStorage(i) > 0d0 .or. Rainfall(i) > 0d0) ) then
            ! state is recovery, but storage or rain: switch to decrease
            InfCapState(i) = HORTON_CAPSTAT_DECREASE
            ratio(i) = (PreviousInfCap(i)-MinInfCap(i)) / (MaxInfCap(i) - MinInfCap(i))
            if (ratio(i) > 0d0) then
               DT1(i) = -1/DecreaseRate(i) * log(ratio(i))  + RFRAC
            else if (ratio (i)<= 0d0) then
               DT1(i) = 9999d0
            end if
            DT(i) = max(0d0, DT1(i) - RFRAC)
         end if
      enddo

      do i = 1, n
         if (InfCapState(i) == HORTON_CAPSTAT_NOCHANGE) then
         !     do nothing, unchanged
         else if(InfCapState(i) == HORTON_CAPSTAT_DECREASE) then
         !  infiltration capacity is decreasing
            NewInfCap(i) = MinInfCap(i) + (MaxInfCap(i) - MinInfCap(i))  * exp(-1d0*DecreaseRate(i) * DT1(i))
         else if (InfCapState(i) == HORTON_CAPSTAT_RECOVERY) then
         !  infiltration capacity is recovering
            NewInfCap(i) = MaxInfCap(i) - (MaxInfCap(i) - MinInfCap(i)) * exp(-1d0*RecoveryRate(i) * DT1(i))
         end if
      enddo
      NewInfCap = min(NewInfCap, MaxInfCap) 
      NewInfCap = max(NewInfCap, MinInfCap)

      DT = DT1
      if (present(InfiltrationMM)) then
         InfiltrationMM = NewInfCap * TimeStepSize / NrSecondsPerHour
      end if

   end function infiltration_horton_formula
end module horton