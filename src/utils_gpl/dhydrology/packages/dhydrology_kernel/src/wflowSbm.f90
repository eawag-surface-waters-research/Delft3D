   !> DFlowFM will call the concept, for now is coded in Python
   function interceptionSbm(timeStep, n, Precipitation, PotEvap, CanopyStorage, CanopyGapFraction, Cmax,NetInterception,&
      ThroughFall, StemFlow,LeftOver, Intercep) result(ierr)

   use interception

   implicit none
   double precision, intent(in)    :: timeStep
   integer, intent(in)             :: n
   double precision, intent(in)    :: Precipitation(n), PotEvap(n), CanopyGapFraction(n), Cmax(n)
   double precision, intent(inout) :: CanopyStorage(n)
   double precision, intent(out)   :: NetInterception(n), ThroughFall(n), StemFlow(n),LeftOver(n), Intercep(n)
   double precision                :: pt(n), Pfrac(n), DD(n), dC(n), D(n)
   integer                         :: ierr

   ierr = -1

   if (timestep < 100) then
       ierr = rainfall_interception_modrut(Precipitation, PotEvap, CanopyStorage, CanopyGapFraction, Cmax, n, NetInterception, &
         ThroughFall, StemFlow, LeftOver, Intercep)
   else
      !call rainfall_interception_modrut_type2()
   end if

   end function interceptionSbm