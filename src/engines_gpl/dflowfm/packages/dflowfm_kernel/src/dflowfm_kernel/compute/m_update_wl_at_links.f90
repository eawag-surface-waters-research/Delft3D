!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2022-2022.
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
module m_update_wl_at_links
   implicit none
   private

   public :: update_wl_at_links

   contains

!> update module array sul
!! first make sure the size is correct
!! then fill using getblu_from_bob
subroutine update_wl_at_links()
   use m_flow, only : sul, hu
   use m_flowgeom, only : lnx
   use precision, only : hp
   use m_alloc, only : realloc, aerr

   implicit none

   integer :: L, iup, ierr
   real(kind=hp) :: blu

   call realloc(sul, lnx, stat=ierr)
   call aerr('sul(lnx)', ierr, lnx)

   iup = 0
   do L = 1, lnx
      call getblu_from_bob(L, iup, blu)
      sul(L) = blu + hu(L)
   end do
end subroutine update_wl_at_links

end module m_update_wl_at_links
