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