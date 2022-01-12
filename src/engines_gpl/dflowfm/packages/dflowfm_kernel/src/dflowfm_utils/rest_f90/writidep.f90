!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2022.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! $Id$
! $HeadURL$

!> output tide potential as samples
subroutine writidep(time)
   use m_flowgeom
   use m_flow
   use m_partitioninfo
   use m_samples
   implicit none

   double precision, intent(in) :: time

   character(len=256)           :: dateandtime

   integer                      :: k
   integer                      :: jaRestoreSam
   integer                      :: itid

   dateandtime = '_'
   call maketime(dateandtime(2:), time)

   if ( jampi.eq.0 ) then
      call newfil(itid, 'tide_potential' // trim(dateandtime) // '.xyz')
   else
      call newfil(itid, 'tide_potential' // trim(dateandtime) // '_' // trim(sdmn) // '.xyz')
   end if

   jaRestoreSam = 0
   if ( Ns.gt.0 ) then
      call savesam()
      jaRestoreSam = 1
   end if

   call increasesam(Ndx)
   NS = Ndx

   do k=1,Ndx
      xs(k) = xz(k)
      ys(k) = yz(k)
      zs(k) = tidep(1,k)
   end do

   call wrisam(itid)

   call doclose(itid)

   if ( jaRestoreSam.eq.1 ) then
      call restoresam()
   else
      call delsam(0)
   end if

   return
end subroutine writidep
