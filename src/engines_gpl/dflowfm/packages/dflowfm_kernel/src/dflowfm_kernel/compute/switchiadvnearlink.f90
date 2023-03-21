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

subroutine switchiadvnearlink(L)
use m_flowgeom
use m_flow
implicit none
integer :: L, k1, k2, kk,LL, iadv1, iadv2

k1 = ln(1,L) ; k2 = ln(2,L)

if (iadvec==0) then
   iadv1 = 0
   iadv2 = 0
elseif (u0(L) > 0) then
   iadv1 = 8  ! piaczek incoming upwind
   iadv2 = 0  ! noadv downstream
else if (u0(L) < 0) then
   iadv1 = 0
   iadv2 = 8
else ! == (now safe for grid direction)
   iadv1 = 8
   iadv2 = 8
end if

do kk  = 1,nd(k1)%lnx
   LL = iabs( nd(k1)%ln(kk) )
   if ( iadv(LL) .ne. 22 .and. (kcu(LL) == 1 .or. kcu(LL) == 2)) then ! Only for regular 1D or 2D.
        iadv(LL) = iadv1
   endif
enddo
do kk  = 1,nd(k2)%lnx
   LL = iabs( nd(k2)%ln(kk) )
   if ( iadv(LL) .ne. 22 .and. (kcu(LL) == 1 .or. kcu(LL) == 2)) then ! Only for regular 1D or 2D.
        iadv(LL) = iadv2
   endif
enddo

end subroutine switchiadvnearlink
