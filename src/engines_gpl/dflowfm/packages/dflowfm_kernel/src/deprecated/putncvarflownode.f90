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

subroutine putncvarflownode( ncid, idq, fnod, kx, nx, itim, jint)
use netcdf
use unstruc_netcdf
use m_flow, only: work1
implicit none

double precision, intent(in)     :: fnod(*)
integer,          intent(in)     :: ncid  ! file unit
integer,          intent(in)     :: idq   ! quantity id
integer,          intent(in)     :: kx, nx, itim, jint


integer                          :: ierr, kb, kt, k, kk

if (kx > 0) then
   do kk=1,nx
      call getkbotktop(kk,kb,kt)
      if (jint == 1) kb = kb - 1
      do k = kb,kt
         work1(k-kb+1,kk) = fnod(k)
      enddo
   enddo
   ierr = nf90_put_var(ncid, idq, work1(1:kx,1:nx), start=(/ 1, 1, itim /), count=(/ kx, nx, 1 /))
else
   ierr = nf90_put_var(ncid, idq, work1(1:kx,1:nx), start=(/ 1, 1, itim /), count=(/ kx, nx, 1 /))
endif
end subroutine putncvarflownode
