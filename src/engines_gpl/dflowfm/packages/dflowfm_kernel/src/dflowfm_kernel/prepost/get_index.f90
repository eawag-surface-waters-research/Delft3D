!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

! 
! 

!> get the cross splines that have valid grid height
subroutine get_index(ncs, isvalid, ndx, idx)
   implicit none

   integer,                 intent(in)  :: ncs  !< number of cross splines
   integer, dimension(ncs), intent(in)  :: isvalid !< valid (>=0) or not (<0)
   integer,                 intent(out) :: ndx  !< number of valid cross splines
   integer, dimension(ncs), intent(out) :: idx  !< valid cross splines

   integer                              :: i

   ndx = 0
   do i=1,ncs
      if ( isvalid(i).ge.0 ) then
         ndx = ndx+1
         idx(ndx) = i
      end if
   end do

   return
end subroutine
