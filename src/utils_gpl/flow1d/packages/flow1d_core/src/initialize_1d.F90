module initialize_1d
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2019.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------
!  $Id$
!  $HeadURL$
!-------------------------------------------------------------------------------

!   implicit none
!   private
!   
!   public initDpu
!   public compute_volumes_1d
!   public computeCross1d
!   
!   contains
!   
!   subroutine initDpu(network)
!      use m_network
!      
!      type(t_network), intent(inout) :: network
!      
!      integer :: linksCount
!      integer :: l
!      integer :: icrs1, icrs2
!      double precision :: f
!      
!      ! each branch has 1 link less than gridpoints
!      linksCount = network%brs%gridpointsCount - network%brs%count
!      
!      do l = 1, size(network%adm%lin2local)
!         if (network%adm%lin2local(l) <= 0) then
!            cycle
!         endif
!         
!         icrs1 = network%adm%line2cross(l)%c1
!         icrs2 = network%adm%line2cross(l)%c2
!         f = network%adm%line2cross(l)%f
!      enddo
!      
!   end subroutine initDpu
!   
!   subroutine compute_volumes_1d(network, dps, ll, levelscount, sinc, vol1d)
!      use m_network
!      
!      type(t_network), intent(in)        :: network
!      double precision, intent(in)       :: dps                   !< depth in node (pressure point)
!      integer, intent(in)                :: ll(:)                 !< links connected to pressure point
!      integer, intent(in)                :: levelscount           !< number of levels to be computed above dps
!      double precision, intent(in)       :: sinc                  !< increment for volume table
!      double precision, intent(out)      :: vol1d(0:levelscount)  !< output volume table in node
!
!      integer :: inc
!      integer :: index
!      integer :: l
!      integer :: loc
!      double precision :: lvl1
!      double precision :: hh
!      double precision :: area1d
!      double precision :: dx
!      double precision :: wetPerimeter, flowWidth, conveyance, u1 = 0d0, cz
!      type(t_branch), pointer :: pbr
!
!      vol1d = 0d0
!      loc = ubound(vol1d,1)
!      
!      if (levelscount > ubound(vol1d,1)) then
!         continue
!      endif
!      
!      do inc=1,levelscount
!         lvl1 = dfloat(inc)*sinc
!         do index=1,size(ll)
!            l=iabs(ll(index))
!            if (l<=0) cycle
!            if (l > size(network%adm%lin2ibr)) then
!               continue
!            endif
!            if (l > size(network%adm%lin2local)) then
!               continue
!            endif
!            
!            if (network%adm%lin2ibr(l) > 0 ) then
!               ! calculate area1d for water depth of hsija
!               
!               loc = network%adm%lin2local(l)
!               if (network%adm%lin2ibr(l) < 1 .or. network%adm%lin2ibr(l) > network%brs%count) then
!                  continue
!               endif
!               
!               call computeCross1d(network, l, hh, u1, cz, area1d, wetPerimeter, flowWidth, conveyance) 
!               pbr=>network%brs%branch(network%adm%lin2ibr(l))
!               if (network%adm%lin2point(l) > size(pbr%dx)) then
!                  continue
!               endif
!               
!               dx = pbr%dx(network%adm%lin2point(l))
!               vol1d(inc) = vol1d(inc) + area1d*dx*0.5d0
!            endif
!         enddo
!      enddo
!   end subroutine compute_volumes_1d
!
!
!!> computes 1d cross section data based on water depth hh
!subroutine computeCross1d(network, l, hh, u1, cz, area, wetPerimeter, flowWidth, conveyance)
!
!   use modelglobaldata
!   use m_globalParameters
!   use m_network
!   
!   implicit none
!   type(t_network), intent(in)     :: network
!   double precision, intent(out)   :: area
!   double precision, intent(in)    :: hh
!   double precision, intent(in)    :: u1
!   double precision, intent(out)   :: wetperimeter
!   double precision, intent(out)   :: flowWidth     ! typically WIDTH1D(l)
!   double precision, intent(out)   :: conveyance
!   double precision, intent(inout) :: cz
!   integer, intent(in)             :: l
!
!   type(t_administration_1d)       :: adm
!   
!   adm = network%adm
!   
!   if(adm%line2cross(l)%c1 >0) then
!      cz = 0d0          
!      call GetCSParsFlow(network%crs%cross(adm%line2cross(l)%c1), network%crs%cross(adm%line2cross(l)%c2), adm%line2cross(l)%f, &
!                         hh, u1, cz, area, wetPerimeter, flowWidth, conveyance)
!      if (hh < thresholdDry) then
!         conveyance = adm%minconv(adm%lin2local(l))
!         flowWidth = adm%minwidth1d(adm%lin2local(l))
!      endif
!   endif
!
!end subroutine computeCross1d
!!--------------------------------------------------------------------
!
!subroutine initialize_lineadministration()
!
!
!
!end subroutine initialize_lineadministration

end module initialize_1d
   