!----- AGPL --------------------------------------------------------------------
!                                                                         
!  Copyright (C)  Stichting Deltares, 2017-2020.                          
!                                                                          
!  The program is free software: you can redistribute it and/or modify             
!  it under the terms of the GNU Affero General Public License as 
!  published by the Free Software Foundation version 3.          
!                                                                
!  The program  is distributed in the hope that it will be useful,        
!  but WITHOUT ANY WARRANTY; without even the implied warranty of    
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      
!  GNU Affero General Public License for more details.               
!                                                       
!  You should have received a copy of the GNU Affero General Public License
!  along with the program.  If not, see <http://www.gnu.org/licenses/>.    
!                                                                                     
!  Stichting Deltares                                                 
!  P.O. Box 177                                               
!  2600 MH Delft, The Netherlands
!                                               
!-------------------------------------------------------------------------------

module horton
   implicit none	

   contains 
   
   function infiltration_horton_formula(n, MinInfCap, MaxInfCap, DecreaseRate, RecoveryRate, PreviousInfCap, NewInfCap, &
                                       TimestepSize, Dt, InitialStorage, Rainfall, InfCapStatus, InfiltrationMM) result(ierr)
      use dhydrology_error
      
      integer,          intent(in   )  :: n
      double precision, intent(in   )  :: MinInfCap(n), MaxInfCap(n), DecreaseRate(n), RecoveryRate(n), PreviousInfCap(n)
      double precision, intent(inout)  :: Dt(n)
      double precision, intent(in   )  :: InitialStorage(n), Rainfall(n)
      double precision, intent(  out)  :: NewInfCap(n)      
      integer,          intent(  out)  :: InfCapStatus(n)
      double precision, intent(  out)  :: InfiltrationMM(n)
      integer                          :: ierr
      
      ! local
      double precision                 :: Dt1(n)
      integer                          :: TimestepSize, NrSecondsPerHour
      double precision                 :: RFrac, ratio(n)

      ! Input data:
      !    n  = array dimension
      !    MinInfCap = minimum infiltration capacity (mm/hr)
      !    MaxInfCap = maximum infiltration capacity (mm/hr)
      !    DecreaseRate = decrease rate (1/hr)
      !    RecoveryRate = recovery rate (1/hr)
      !    PreviousInfCap= last infiltration capacity (mm/hr)
      !    TimestepSize  = timestep size in seconds
      !    Dt  (I/O)     = time in hours since start of decreasing/recovery mode
      !    InitialStorage= initial storage (=storage at start of timestep)
      !    Rainfall      = rainfall in current timestep (or more precise: additional ground rainfall, so minus interception)
      ! Output:
      !    NewInfCap    = new infiltration capacity (mm/hr)
      !    InfiltrationMM = infiltration in mm
      !    InfCapStatus = infiltration capacity status; 0=no change, 1=decrease, 2=recovery
      !    Dt  (I/O)    = time in hours since start of decreasing/recovery mode


      ! Compute infiltration capacity as defined by Horton equations

      ! Note: infiltration capacity defined in mm/hr, decrease and recovery rate in 1/hr
      ! Note: typical timestep used in application is 1 minute (i.e. much smaller than 1 hour)
      ! Note: otherwise computation of infiltration volume (in mm) should be more refined (using integral of capacity function, depending on status recovery or decrease)
      
      ierr = DHYD_NOERR
      NrSecondsPerHour = 3600
      RFRAC = Dble(TimestepSize) / Dble(NrSecondsPerHour)
      DT1   = DT  + RFRAC

      where(MaxInfCap <= MinInfCap)
      !      constant infiltratiecapacity; infiltration status not changed
         InfCapStatus = 0
      else where(PreviousInfCap >= MaxInfCap)
      !      Previous infiltration capacity is at maximum value; now decrease
         InfCapStatus = 1
         where(DT > RFRAC .and. PreviousInfCap >= MaxInfCap)
            DT1 = RFRAC
            DT  = 0
         end where
      else where(PreviousInfCap <= MinInfCap)
      !      Previous infiltration capacity is at minimum value, now increase
         InfCapStatus = 2
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
      where(InfCapStatus == 1 .and. InitialStorage <= 0 .and. Rainfall <= 0)
         ! status is decrease, but no storage and no rain anymore: switch to recovery
         InfCapStatus = 2
         Ratio = (PreviousInfCap-MaxInfcap) / (MinInfCap - MaxInfCap)
         where(ratio > 0)
            DT1 = -1/RecoveryRate * log(ratio)  + RFRAC
         else where(ratio <= 0)
            DT1 = 9999.
         end where
         DT  = max(0.0, DT1 - RFRAC)
      else where (InfCapStatus == 2 .and. (InitialStorage > 0 .or. Rainfall > 0) )
         ! status is recovery, but storage or rain: switch to decrease
         InfCapStatus = 1
         ratio = (PreviousInfCap-MinInfCap) / (MaxInfCap - MinInfCap)
         where(ratio > 0)
            DT1 = -1/DecreaseRate * log(ratio)  + RFRAC
         else where (ratio <= 0)
            DT1 = 9999.
         end where
         DT = max(0.0, DT1 - RFRAC)
      end where

      where(InfCapStatus == 0)
      !     do nothing, unchanged
      else where(InfCapStatus == 1)
      !     infiltration capacity is decreasing
         NewInfCap = MinInfCap + (MaxInfCap - MinInfCap) * exp(-1*DecreaseRate * DT1)
      else where(InfCapStatus == 2)
      !     infiltration capacity is recovering
         NewInfCap = MaxInfCap - (MaxInfCap - MinInfCap) * exp(-1*RecoveryRate * DT1)
      end where

      NewInfCap = min(NewInfCap, MaxInfCap)
      NewInfCap = max(NewInfCap, MinInfCap)

      InfiltrationMM = NewInfCap * TimeStepSize / NrSecondsPerHour

   end function infiltration_horton_formula
end module horton