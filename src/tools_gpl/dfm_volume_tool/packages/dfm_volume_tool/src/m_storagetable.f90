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
  
   public generateVolumeTableDataOnGridpoints
   public generateVolumeTableDataOnLinks
   public getBedToplevel
   public calculateDeadStorage

contains
  
!> Generate the total volumes/surfaces (TODO: and the totalised volumes/surfaces for each branch).
subroutine generateVolumeTableDataOnGridPoints(volumes, surfaces, voltb, bedlevel, increment, numpoints, numlevels, nodes)

   use unstruc_channel_flow
   double precision, dimension(:),  intent(  out)     :: volumes         !< Aggregated volumes per level 
   double precision, dimension(:),  intent(  out)     :: surfaces        !< Aggregated areas per level
   type(T_voltable), dimension(:),  intent(in   )     :: voltb           !< Volume tables on grid points
   double precision,                intent(in   )     :: bedlevel        !< Bed level of the model
   double precision,                intent(in   )     :: increment       !< Requested increment
   integer,                         intent(in   )     :: numpoints       !< number of 1d grid points also the size of voltb
   integer,                         intent(in   )     :: numlevels       !< number of levels in volume, surface and levels
   integer,          dimension(:),  intent(in   )     :: nodes           !< mask array for defining a sub set on nodes.  

   integer           :: j, i, ipoint, L

   volumes = 0d0

   do j = 1, numpoints
     ipoint = nodes(j)
     call AddVolumeAndSurface(volumes, surfaces, voltb(ipoint), bedlevel, increment, numlevels)
   enddo
   
   call ComputeSurface(volumes, surfaces, increment, numlevels)

end subroutine generateVolumeTableDataOnGridPoints

!> Generate the total volumes/surfaces (TODO: and the totalised volumes/surfaces for each branch).
subroutine generateVolumeTableDataOnLinks(volumes, surfaces, voltbOnlinks, bedlevel, increment, numpoints, numlevels, links)

   use unstruc_channel_flow
   double precision, dimension(:),  intent(  out)     :: volumes         !< Aggregated volumes per level 
   double precision, dimension(:),  intent(  out)     :: surfaces        !< Aggregated areas per level
   type(T_voltable), dimension(:,:),intent(in   )     :: voltbOnLinks   !< Volume tables on links
   double precision,                intent(in   )     :: bedlevel       !< Bed level of the model
   double precision,                intent(in   )     :: increment      !< Requested increment
   integer,                         intent(in   )     :: numpoints       !< number of 1d grid points also the size of voltb
   integer,                         intent(in   )     :: numlevels       !< number of levels in volume, surface and levels
   integer,          dimension(:),  intent(in   )     :: links           !< mask array for defining a sub set on flow links.  

   integer           :: j, i, ipoint, L

   volumes = 0d0

   do j = 1, numpoints
     L = links(j)
     call AddVolumeAndSurface(volumes, surfaces, voltbOnLinks(1, L), bedlevel, increment, numlevels)
     call AddVolumeAndSurface(volumes, surfaces, voltbOnLinks(2, L), bedlevel, increment, numlevels)
   enddo
   
   call ComputeSurface(volumes, surfaces, increment, numlevels)

end subroutine generateVolumeTableDataOnLinks

!> Add the volumes and surfaces for this volume table to the aggregated volumes and surfaces
subroutine AddVolumeAndSurface(vol, sur, voltb, bedlevel, increment, numlevels)
double precision, dimension(:), intent(inout) :: vol              !< Aggregated volumes
double precision, dimension(:), intent(inout) :: sur              !< Aggregated surfaces
type(t_voltable),               intent(in   ) :: voltb            !< Volume table
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

!> Calculate the dead storage using boundary conditions, turn off levels of pumping stations and the geometry of the network.
subroutine calculateDeadStorage(wl_deadstorage, network, bndvalues, inslevtube, bndindex, ln2nd, numlinks, numpoints, numboundaries)

   use m_flowgeom
   use m_network
   use m_1d_structures
   use m_Pump

   type(t_network),                  intent(in)       :: network         !< Network definition of the model.
   double precision, dimension(:),   intent(inout)    :: wl_deadstorage  !< On output this array contains the minimal water levels, after the model is 
                                                                         !< drained.
   double precision, dimension(:),   intent(in   )    :: bndvalues       !< Boundary condition values. See ZBNDZ in D-FlowFM.
   double precision, dimension(:,:), intent(in   )    :: inslevtube      !< Inside level tubes. See BOB in D-FlowFM.
   integer,          dimension(:,:), intent(in   )    :: bndindex        !< Index numbers for boundary condtions. See KBNDZ in D-FfowFM.
   integer,          dimension(:,:), intent(in   )    :: ln2nd           !< Link nodes to node numbers. See LN in D-FfowFM.
   integer,                          intent(in   )    :: numpoints       !< Number of 1d points in volume tables.
   integer,                          intent(in   )    :: numlinks        !< Number of 1d internal links
   integer,                          intent(in   )    :: numboundaries   !< Number of water level boundaries
   
   
   integer :: numberOfChanges
   integer :: i, n, k1, k2, kb, L, j
   integer :: itpbn
   integer :: istru
   integer :: suctionSideNode
   integer :: nstages
   type(t_structure), pointer, dimension(:)  :: structs
   integer, pointer, dimension(:)            :: indices
   type(t_pump), pointer                     :: pump

   ! Set water levels to large value
   wl_deadstorage = huge(1d0)

   ! Set the boundary conditions
   
   do n  = 1, numboundaries                              
      kb      = bndindex(1,n)
      k2      = bndindex(2,n)
      L       = bndindex(3,n)
      itpbn   = bndindex(4,n)
      if (     itpbn == 1) then                        ! waterlevelbnd
         wl_deadstorage(k2) = bndvalues(n)
         wl_deadstorage(kb) = bndvalues(n)
      endif
   enddo

   ! Set the waterlevel at the suction side of a pump to the turn off level.
   ! Also set the bob of this flow link to 0.5d0 * huge
   if (network%sts%count > 0) then
      structs => Network%sts%struct
      indices => Network%sts%pumpIndices
      do i = 1, Network%sts%numPumps
         istru = indices(i)
         pump => structs(istru)%pump
         L = structs(istru)%linknumbers(1)
         if (pump%direction * pump%capacity(1) > 0) then
            suctionsideNode = ln2nd(1,L)
         else
            suctionsideNode = ln2nd(2,L)
         end if

         nstages = pump%nrstages
         do j = 1, nstages
            wl_deadstorage(suctionsideNode) = min(wl_deadstorage(suctionsideNode), pump%ss_offlevel(j))
         enddo
      enddo
   endif

   ! Now iterate over all flow links in order to find the lowest level in the model
   numberOfChanges = numpoints
   do while (numberOfChanges > 0)
      numberOfChanges = 0
      do L = 1, numlinks
         do k1 = 1, 2
            if (k1==1) then
               k2 = 2
            else
               k2 = 1
            endif
            if (wl_deadstorage(ln2nd(k1,L)) > max(inslevtube(1,L), inslevtube(2,L), wl_deadstorage(ln2nd(k2,L)))) then
               numberOfChanges = numberOfChanges + 1
               wl_deadstorage(ln2nd(k1,L)) = max(inslevtube(1,L), inslevtube(2,L), wl_deadstorage(ln2nd(k2,L)))
            endif
         enddo
      enddo
      continue
   enddo

end subroutine calculateDeadStorage

end module m_StorageTable