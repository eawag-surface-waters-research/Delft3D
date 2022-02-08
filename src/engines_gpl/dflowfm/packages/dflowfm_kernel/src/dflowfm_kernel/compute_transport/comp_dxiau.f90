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

! compute Au/Dx for diffusive flux
subroutine comp_dxiAu()                          ! or: setdxiau
   use m_flowgeom , only : ln, Lnx, dxi, wu, Lnxi
   use m_flow     , only : hs, zws, kmx, Au, hu, jadiffusiononbnd
   use m_transport, only : dxiAu, jalimitdtdiff
   use timers

   implicit none

   integer :: k1, k2
   integer :: LL, L, Lb, Lt
   integer(4) ithndl /0/

   if (timon) call timstrt ( "comp_dxiAu", ithndl )

   if ( kmx.eq.0 ) then
      do L=1,Lnx
         dxiAu(L) = dxi(L)*Au(L)
      end do
   else
      do LL=1,Lnx
         call getLbotLtop(LL,Lb,Lt)
         do L=Lb,Lt
            dxiAu(L) = dxi(LL)*Au(L)
         end do
      end do
   end if

   if (jadiffusiononbnd == 0) then  
      do LL=lnxi+1, lnx
         call getLbotLtop(LL,Lb,Lt)
         do L=Lb,Lt
            dxiAu(L) = 0d0 
         enddo
      enddo
   endif

   if (timon) call timstop( ithndl )
   return
end subroutine comp_dxiAu
