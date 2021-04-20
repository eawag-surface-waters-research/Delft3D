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

subroutine gettaus(typout)

use m_flowgeom
use m_flow
use m_alloc

implicit none
integer, intent (in)       ::  typout   !< type of setting, 1: set czs and taus, 2: just set czs:

double precision           ::  taucurc  !< local variable for taucurrent
double precision           ::  czc      !< local variable for chezy
integer                    ::  ierr     !< Error code
integer                    ::  n        !< Counter

if (.not. allocated(czs) ) then
    call realloc(czs,  ndxi,  keepExisting = .false., fill = 0d0, stat = ierr)
else if (size(czs) < ndxi) then
    call realloc(czs,  ndxi,  keepExisting = .false., fill = 0d0, stat = ierr)
endif
if (typout == 1) then
    if (.not. allocated(taus) ) then
        call realloc(taus,  ndxi,  keepExisting = .false., fill = 0d0, stat = ierr)
    else if (size(taus) < ndxi) then
        call realloc(taus,  ndxi,  keepExisting = .false., fill = 0d0, stat = ierr)
    endif
endif

do n = 1,ndxi
   call gettau(n,taucurc,czc)
   czs(n) = czc
   if (typout == 1) then
       taus(n) = taucurc
   endif
enddo
end subroutine gettaus
