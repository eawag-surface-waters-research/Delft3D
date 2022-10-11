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
!> Module for using volume tables at 1d nodes for the computation of the total volume of water in a node.
module m_StorageTable
   use messageHandling
   use m_GlobalParameters
   use m_VolumeTables
   use m_flowgeom
   use m_flowparameters
   use unstruc_channel_flow, only: tableincrement

implicit none
private
  
   public generateVolumeTableData
   public getBedToplevel

contains
  
!> Generate the total volumes/surfaces (TODO: and the totalised volumes/surfaces for each branch).
subroutine generateVolumeTableData(volumes, surfaces, voltb, voltbOnlinks, bedlevel, increment, numpoints, numlevels, nodes, links)

   use unstruc_channel_flow
   double precision, dimension(:),  intent(  out)     :: volumes         !< Aggregated volumes per level 
   double precision, dimension(:),  intent(  out)     :: surfaces        !< Aggregated areas per level
   type(T_voltable), dimension(:),  intent(in   )     :: voltb            !< Volume tables on grid points
   type(T_voltable), dimension(:,:),intent(in   )     :: voltbOnLinks     !< Volume tables on links
   double precision,                intent(in   )     :: bedlevel        !< Bed level of the model
   double precision,                intent(in   )     :: increment       !< Requested increment
   integer,                         intent(in   )     :: numpoints       !< number of 1d grid points also the size of voltb
   integer,                         intent(in   )     :: numlevels       !< number of levels in volume, surface and levels
   integer, optional,dimension(:),  intent(in   )     :: nodes           !< mask array for defining a sub set on nodes.  
   integer, optional,dimension(:),  intent(in   )     :: links           !< mask array for defining a sub set on flow links.  

   integer           :: j, i, ipoint, L

   volumes = 0d0
   if (present(nodes)) then
      do j = 1, numpoints
         ipoint = nodes(j)
         call AddVolumeAndSurface(volumes, surfaces, voltb(ipoint), bedlevel, increment, numlevels)
      enddo
      call ComputeSurface(volumes, surfaces, increment, numlevels)
   else if (present(links)) then
      do j = 1, numpoints
        L = links(j)
        call AddVolumeAndSurface(volumes, surfaces, voltbOnLinks(1, L), bedlevel, increment, numlevels)
        call AddVolumeAndSurface(volumes, surfaces, voltbOnLinks(2, L), bedlevel, increment, numlevels)
      enddo
      call ComputeSurface(volumes, surfaces, increment, numlevels)
   endif

end subroutine generateVolumeTableData

!> Add the volumes and surfaces for this volume table to the aggregated volumes and surfaces
subroutine AddVolumeAndSurface(vol, sur, voltb, bedlevel, increment, numlevels)
double precision, dimension(:), intent(inout) :: vol              !< Aggregated volumes
double precision, dimension(:), intent(inout) :: sur              !< Aggregated surfaces
type(t_voltable),               intent(in   ) :: voltb             !< Volume table
integer,                        intent(in   ) :: numlevels        !< Number of levels in the aggregated data
double precision,               intent(in   ) :: bedlevel         !< Bed level
double precision,               intent(in   ) :: increment        !< Requested increment   

integer :: i
double precision :: waterlevel, help

do i = 2, numlevels
  waterlevel = bedlevel + (i-1)*increment
  ! Compute 1d volumes, using volume tables (still excl. 1D2D contributions)
  help = voltb%getVolume (waterlevel)
  vol(i)  = vol(i) +  voltb%getVolume (waterlevel)
  help = voltb%getSurface(waterlevel)
  sur(i) = sur(i) + voltb%getSurface(waterlevel)
enddo
!sur(numlevels) = sur(numlevels) + voltb%getSurface(waterlevel)

end subroutine 

!> derive the surface areas from the volume table
subroutine ComputeSurface(vol, sur, increment, numlevels)
double precision, dimension(:), intent(inout) :: vol              !< Aggregated volumes
double precision, dimension(:), intent(inout) :: sur              !< Aggregated surfaces
integer,                        intent(in   ) :: numlevels        !< Number of levels in the aggregated data
double precision,               intent(in   ) :: increment        !< Requested increment   

integer :: i
double precision :: waterlevel

do i = 2, numlevels
   sur(i-1) = (vol(i)-vol(i-1))/increment
enddo
end subroutine ComputeSurface


!> Determine the lowest level and the highest level in the model.
subroutine getBedToplevel(voltb, numpoints, toplevel, bedlevel)
   integer,          intent(in   ) :: numpoints
   type(t_voltable), pointer, dimension(:), intent(in) :: voltb     !< volume table for individual 
   double precision, intent(  out) :: toplevel       !< Highest level in the model, where cross sections or storage nodes are defined.
   double precision, intent(  out) :: bedlevel       !< Lowest level in the model, where cross sections or storage nodes are defined.

   integer :: n
   toplevel = voltb(1)%topheight + voltb(1)%bedlevel
   bedlevel = voltb(1)%bedlevel
   
   do n = 1, numpoints
      toplevel = max(toplevel, voltb(n)%topheight + voltb(n)%bedlevel)
      bedlevel = min(bedlevel, voltb(n)%bedlevel)
   enddo

end subroutine getBedToplevel

end module m_StorageTable