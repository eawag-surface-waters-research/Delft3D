!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2021.                                
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

!> Toplevel setnodadm routine wraps:
!! * original setnodadm(), for network_data administration.
!! * update_flow1d_admin(), to remove any net links from
!!   the flow1d::network administration, if they were also
!!   removed from network_data in the first step.
subroutine setnodadm(jacrosscheck_)
   use gridoperations
   use m_network
   use m_save_ugrid_state, only: contactnlinks, contactnetlinks
   use network_data
   use unstruc_channel_flow

   integer, intent(in   ) :: jacrosscheck_ !< Whether or not to remove any crossing netlinks.

   integer :: L, LL, Lnew


   call setnodadm_grd_op(10+jacrosscheck_)

   ! Update netlink numbers for all 1d2d contacts, after netlinks may have been permuted:
   if (contactnlinks > 0) then
      do LL=1,contactnlinks
         L = contactnetlinks(LL)
         Lnew = Lperminv(L)
         contactnetlinks(LL) = Lnew
      end do
   end if

   if (lc(1) /=0) then
      call update_flow1d_admin(network, lc)
   endif

end subroutine setnodadm
