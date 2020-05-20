!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2020.                                
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
      
      ierr = DHYD_NOERR

      allocate(Dt1(n))
      allocate(ratio(n))


      RFRAC = TimestepSize / NrSecondsPerHour
      DT1   = DT  + RFRAC

      where(MaxInfCap <= MinInfCap)
      !      constant infiltration capacity; infiltration state not changed
         InfCapState = HORTON_CAPSTAT_NOCHANGE
      else where(PreviousInfCap >= MaxInfCap)
      !      Previous infiltration capacity is at maximum value; now decrease
         InfCapState = HORTON_CAPSTAT_DECREASE
         where(DT > RFRAC .and. PreviousInfCap >= MaxInfCap)
            DT1 = RFRAC
            DT  = 0
         end where
      else where(PreviousInfCap <= MinInfCap)
      !      Previous infiltration capacity is at minimum value, now increase
         InfCapState = HORTON_CAPSTAT_RECOVERY
         where(DT > RFRAC .and. PreviousInfCap <= MinInfCap)
            DT1 = RFRAC
            DT  = 0
         end where
      end where

      ! additional checks: decrease of infiltration capacity as long as there is storage and/or rain
      !                    recovery of infiltration capacity as soon as storage is empty and no rain
      ! note: not just simple  ft = fc + (f0-fc)*exp (-kt)
      !       trick required for computation of t for intermediate switch from decrease to recovery or vice versa, before reaching max. or min.

      Ratio = -1.
      where(InfCapState == HORTON_CAPSTAT_DECREASE .and. InitialStorage <= 0 .and. Rainfall <= 0)
         ! state is decrease, but no storage and no rain anymore: switch to recovery
         InfCapState = HORTON_CAPSTAT_RECOVERY
         Ratio = (PreviousInfCap-MaxInfcap) / (MinInfCap - MaxInfCap)
         where(ratio > 0)
            DT1 = -1/RecoveryRate * log(ratio)  + RFRAC
         else where(ratio <= 0)
            DT1 = 9999.
         end where
         DT  = max(0.0, DT1 - RFRAC)
      else where (InfCapState == HORTON_CAPSTAT_RECOVERY .and. (InitialStorage > 0 .or. Rainfall > 0) )
         ! state is recovery, but storage or rain: switch to decrease
         InfCapState = HORTON_CAPSTAT_DECREASE
         ratio = (PreviousInfCap-MinInfCap) / (MaxInfCap - MinInfCap)
         where(ratio > 0)
            DT1 = -1/DecreaseRate * log(ratio)  + RFRAC
         else where (ratio <= 0)
            DT1 = 9999.
         end where
         DT = max(0.0, DT1 - RFRAC)
      end where

      where(InfCapState == HORTON_CAPSTAT_NOCHANGE)
      !     do nothing, unchanged
      else where(InfCapState == HORTON_CAPSTAT_DECREASE)
      !     infiltration capacity is decreasing
         NewInfCap = MinInfCap + (MaxInfCap - MinInfCap) * exp(-1*DecreaseRate * DT1)
      else where(InfCapState == HORTON_CAPSTAT_RECOVERY)
      !     infiltration capacity is recovering
         NewInfCap = MaxInfCap - (MaxInfCap - MinInfCap) * exp(-1*RecoveryRate * DT1)
      end where

      NewInfCap = min(NewInfCap, MaxInfCap)
      NewInfCap = max(NewInfCap, MinInfCap)

      DT = DT1
      if (present(InfiltrationMM)) then
         InfiltrationMM = NewInfCap * TimeStepSize / NrSecondsPerHour
      end if

   end function infiltration_horton_formula
end module horton