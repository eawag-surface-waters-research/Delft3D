   module interception_module

   contains

   !> Please document rainfall_interception_modrutr, return an error message.
   function rainfall_interception_modrut(n, Precipitation, PotEvap, CanopyStorage, CanopyGapFraction, Cmax,NetInterception,&
      ThroughFall, StemFlow,LeftOver, Interception) result(ierr)

   implicit none 
   
   ! inputs
   integer, intent(in)             :: n
   double precision, intent(in)    :: Precipitation(n), PotEvap(n), CanopyGapFraction(n), Cmax(n)
   double precision, intent(inout) :: CanopyStorage(n)
   double precision, intent(out)   :: NetInterception(n), ThroughFall(n), StemFlow(n),LeftOver(n), Interception(n)
   
   ! locals
   double precision                :: pt(n), Pfrac(n), DD(n), dC(n), D(n)
   integer                         :: ierr  

   ierr = 0
   
   pt = 0.1 * CanopyGapFraction
   ! Amount of P that falls on the canopy
   Pfrac = (1 - CanopyGapFraction - pt * Precipitation)
   ! S cannot be larger than Cmax, no gravity drainage below that
   where(CanopyStorage > Cmax)
      DD = CanopyStorage - Cmax
   elsewhere
      DD = 0.
   end where

   CanopyStorage = CanopyStorage - DD

   ! Add the precipitation that falls on the canopy to the store
   CanopyStorage = CanopyStorage + Pfrac

   ! Now do the Evap, make sure the store does not get negative
   where(CanopyStorage > PotEvap)
      dC = -1. * PotEvap
   elsewhere
      dC = -1. * CanopyStorage
   end where

   CanopyStorage = CanopyStorage + dC

   LeftOver = PotEvap + dC ! Amount of evap not used

   ! Now drain the canopy storage again if needed...
   where(CanopyStorage > Cmax)
      D = CanopyStorage - Cmax
   elsewhere
      D = 0.0
   end where

   CanopyStorage = CanopyStorage - D

   ! Calculate throughfall
   ThroughFall = DD + D + CanopyGapFraction * Precipitation
   StemFlow = Precipitation * pt

   ! Calculate interception, this is NET Interception
   NetInterception = Precipitation - ThroughFall - StemFlow
   Interception = -dC


   end function rainfall_interception_modrut


   !!> Please document rainfall_interception_modrut_type2, return an error message.
   !function rainfall_interception_modrut_type2(Precipitation, PotEvap, CanopyStorage, CanopyGapFraction, Cmax, n,NetInterception,&
   !   ThroughFall, StemFlow,LeftOver, Interception)  result(ierr)
   !
   !implicit none 
   !   
   !
   !integer                         :: ierr  
   !
   !
   !
   !end function rainfall_interception_modrut_type2
   
   
   end module interception_module
