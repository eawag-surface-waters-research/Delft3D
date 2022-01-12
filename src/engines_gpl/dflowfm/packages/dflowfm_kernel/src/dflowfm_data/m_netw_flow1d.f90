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

!> Wrapper module for loading a flow1d network into dflowfm state.
module m_netw_flow1d
use m_network
use network_data
use unstruc_channel_flow
use m_missing
use m_profiles

implicit none

contains

!> Reads a 1D model from a *.md1d ini file.
!! This routine is still used for Morphology model with network in INI-File (Willem Ottevanger)
!! This subroutine does not support parallel models.
subroutine load_network_from_flow1d(filenames, found_1d_network)
   use m_flow1d_reader
   use m_flowgeom
   use m_globalParameters
   use m_cross_helper
   use unstruc_messages
   use messagehandling
   type(t_filenames), intent(inout) :: filenames !< Name of 1d files to read from.
   logical, intent(out)            :: found_1d_network

   type(t_branch), pointer :: pbr
   type(t_node), pointer :: pnod
   integer :: istat, minp, ifil, inod, ibr, ngrd, k, L, k1, k2
   type (t_structure), pointer :: pstru
   integer :: nstru, i
   double precision, dimension(2) :: tempbob
   character(len=255) :: filename
   integer :: threshold_abort_current

   filename = filenames%onednetwork

   ! Check on Empty File Name
   if (len_trim(filename) <= 0) then
      found_1d_network = .false.
      return
   endif

   ! MessageHandling has already been set up via initMessaging() earlier.
   threshold_abort_current = threshold_abort
   threshold_abort = LEVEL_FATAL
   call read_1d_mdu(filenames, network, found_1d_network)
   if (.not. found_1d_network) then
      network%numk = 0
      network%numl = 0
      network%loaded = .false.
      return
   else
       network%loaded = .true.
   endif

   call admin_network(network, numk, numl)

   call read_1d_attributes(filenames, network)

   call initialize_1dadmin(network, network%l1d)

   numk = 0
   numl = 0
   do inod = 1, network%nds%Count
      pnod => network%nds%node(inod)
      numk = numk+1
      pnod%gridNumber = numk
      xk(numk) = pnod%x
      yk(numk) = pnod%y
      zk(numk) = dmiss
   enddo

   do ibr = 1, network%brs%Count
      pbr => network%brs%branch(ibr)

      ! first step add coordinates and bed levels to nodes
      ngrd = pbr%gridPointsCount
      pbr%grd(1) = pbr%FromNode%gridNumber ! TODO: Not safe in parallel models (check gridpointsseq as introduced in UNST-5013)
      do k = 2, ngrd-1
         numk = numk+1
         pbr%grd(k) = numk
         xk(numk) = pbr%Xs(k)
         yk(numk) = pbr%Ys(k)
         zk(numk) = dmiss
      enddo
      pbr%grd(ngrd) = pbr%toNode%gridNumber

      ! second step create links
      do k = 1, ngrd-1
         numl = numl+1
         kn(1,numl) = pbr%grd(k)
         kn(2,numl) = pbr%grd(k+1)
         kn(3,numl) = 1
      enddo

   enddo

   network%numk = numk
   network%numl = numl

   ! fill bed levels from values based on links
   do L = 1, network%numl
      tempbob = getbobs(network, L)
      if (tempbob(1) > 0.5d0* huge(1d0)) tempbob(1) = dmiss
      if (tempbob(2) > 0.5d0* huge(1d0)) tempbob(2) = dmiss

      k1 = kn(1,L)
      k2 = kn(2,L)
      if (zk(k1) == dmiss) then
         zk(k1) = tempbob(1)
      endif
      if (zk(k2) == dmiss) then
         zk(k2) = tempbob(2)
      endif
      zk(k1) = min(zk(k1),tempbob(1))
      zk(k2) = min(zk(k2),tempbob(2))
   enddo

   ! TODO: Once dflowfm's own 1D and the flow1d code are aligned, the following switch should probably disappear.
   jainterpolatezk1D = 0
   threshold_abort = threshold_abort_current

end subroutine load_network_from_flow1d

end module m_netw_flow1d
