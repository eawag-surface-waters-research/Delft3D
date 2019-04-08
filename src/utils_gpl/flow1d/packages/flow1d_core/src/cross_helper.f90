module m_cross_helper
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

   use m_network
   use M_newcross
   use m_CrossSections
   use m_tables
   
   implicit none
   
   public getHighest1DlevelGP
   
   public getBobs
   
   public getWetFlowAreaGP
   public getWetTotalAreaGP
   
   public getFlowWidthGP
   public getTotalWidthGP
   
   public getWetPerimeterGP
   
   public getCrossFlowDataGP
   public getCrossFlowSectionGP
   public getCrossTotalDataGP
   
   public getSummerDikeGP
   public getSummerDikeGPData
   public resetSummerDike
   
   public getChezyFromYZ
   
   public getCrossFlowData_on_link
   public getCrossTotalData_on_link
   
   public getConveyance
   public getCrossDischarge

   integer, public, parameter :: CSH_DEPTH = 0
   integer, public, parameter :: CSH_LEVEL  = 1
   
   double precision, public :: default_width
   
   private
   
contains

   double precision function getWetFlowAreaGP(network, igrid, water, DepthOrLevel)
   
      type(t_network), intent(in)        :: network
      integer, intent(in)                :: igrid
      double precision, intent(in)       :: water
      integer, optional                  :: DepthOrLevel
   
      type (t_CrossSection), pointer     :: cross1
      type (t_CrossSection), pointer     :: cross2 
      double precision                   :: factor

      double precision                   :: bob_grid_point
      double precision                   :: dpt
      double precision                   :: czdum
      double precision                   :: flowArea
      double precision                   :: wetPerimeter
      double precision                   :: flowWidth
      double precision                   :: conv

      getWetFlowAreaGP = 0.0d0
      
      cross1 => network%crs%cross(network%adm%gpnt2cross(igrid)%c1)
      cross2 => network%crs%cross(network%adm%gpnt2cross(igrid)%c2)
      factor =  network%adm%gpnt2cross(igrid)%f
      
      if (present(DepthOrLevel)) then
         if (DepthOrLevel ==  CSH_LEVEL) then     
            bob_grid_point = getBob(cross1, cross2, factor)
            dpt = bob_grid_point + water
         else
            dpt = water
         endif
      else
         dpt = water
      endif
      
      czdum = 0.0d0
      
      call GetCSParsFlow(cross1, cross2, factor, dpt, 0.0d0, czdum, flowArea, wetPerimeter, flowWidth, conv)
      
      getWetFlowAreaGP = flowArea

   end function getWetFlowAreaGP

   double precision function getHighest1DlevelGP(network, igrid)
   
      type(t_network), intent(in)        :: network
      integer, intent(in)                :: igrid
    
      type (t_CrossSection), pointer     :: cross1
      type (t_CrossSection), pointer     :: cross2 
      double precision                   :: factor

      cross1 => network%crs%cross(network%adm%gpnt2cross(igrid)%c1)
      cross2 => network%crs%cross(network%adm%gpnt2cross(igrid)%c2)
      factor =  network%adm%gpnt2cross(igrid)%f
      
      getHighest1DlevelGP = getHighest1Dlevel(cross1, cross2, factor)

   end function getHighest1DlevelGP

   double precision function getWetTotalAreaGP(network, igrid, water, DepthOrLevel)
   
      ! Get Wetted Area at GridPoint
      type(t_network), intent(inout)        :: network
      integer, intent(in)                :: igrid
      double precision, intent(in)       :: water
      integer, optional                  :: DepthOrLevel
   
      type (t_CrossSection), pointer     :: cross1
      type (t_CrossSection), pointer     :: cross2 
      double precision                   :: factor

      double precision                   :: bob_grid_point
      double precision                   :: dpt
      double precision                   :: totalArea
      double precision                   :: totalWidth
      logical                            :: hysteresis(2) 

      getWetTotalAreaGP = 0.0d0
      
      cross1 => network%crs%cross(network%adm%gpnt2cross(igrid)%c1)
      cross2 => network%crs%cross(network%adm%gpnt2cross(igrid)%c2)
      factor =  network%adm%gpnt2cross(igrid)%f
                
      if (present(DepthOrLevel)) then
         if (DepthOrLevel ==  CSH_LEVEL) then     
            bob_grid_point = getBob(cross1, cross2, factor)
            dpt = bob_grid_point + water
         else
            dpt = water
         endif
      else
         dpt = water
      endif
      hysteresis = network%adm%hysteresis_for_summerdike(:,igrid)
      call GetCSParsTotal(cross1, cross2, factor, dpt, totalArea, totalWidth, CS_TYPE_PREISMAN, hysteresis)
      
      getWetTotalAreaGP = totalArea

   end function getWetTotalAreaGP

   double precision function getFlowWidthGP(network, igrid, water, DepthOrLevel)
   
      type(t_network), intent(in)        :: network
      integer, intent(in)                :: igrid
      double precision, intent(in)       :: water
      integer, optional                  :: DepthOrLevel
   
      type (t_CrossSection), pointer     :: cross1
      type (t_CrossSection), pointer     :: cross2 
      double precision                   :: factor

      double precision                   :: bob_grid_point
      double precision                   :: dpt
      double precision                   :: czdum
      double precision                   :: flowArea
      double precision                   :: wetPerimeter
      double precision                   :: flowWidth
      double precision                   :: conv

      getFlowWidthGP = 0.0d0
      
      cross1 => network%crs%cross(network%adm%gpnt2cross(igrid)%c1)
      cross2 => network%crs%cross(network%adm%gpnt2cross(igrid)%c2)
      factor =  network%adm%gpnt2cross(igrid)%f
                
      if (present(DepthOrLevel)) then
         if (DepthOrLevel ==  CSH_LEVEL) then     
            bob_grid_point = getBob(cross1, cross2, factor)
            dpt = bob_grid_point + water
         else
            dpt = water
         endif
      else
         dpt = water
      endif
      
      czdum = 0.0d0
      
      call GetCSParsFlow(cross1, cross2, factor, dpt, 0.0d0, czdum, flowArea, wetPerimeter, flowWidth, conv)
      
      getFlowWidthGP = flowWidth

   end function getFlowWidthGP

   double precision function getTotalWidthGP(network, igrid, water, DepthOrLevel)
   
      type(t_network), intent(inout)     :: network
      integer, intent(in)                :: igrid
      double precision, intent(in)       :: water
      integer, optional                  :: DepthOrLevel
   
      type (t_CrossSection), pointer     :: cross1
      type (t_CrossSection), pointer     :: cross2 
      double precision                   :: factor

      double precision                   :: bob_grid_point
      double precision                   :: dpt
      double precision                   :: totalArea
      double precision                   :: totalWidth
      logical                            :: hysteresis(2) 

      getTotalWidthGP = 0.0d0
      
      cross1 => network%crs%cross(network%adm%gpnt2cross(igrid)%c1)
      cross2 => network%crs%cross(network%adm%gpnt2cross(igrid)%c2)
      factor =  network%adm%gpnt2cross(igrid)%f
                
      if (present(DepthOrLevel)) then
         if (DepthOrLevel ==  CSH_LEVEL) then     
            bob_grid_point = getBob(cross1, cross2, factor)
            dpt = bob_grid_point + water
         else
            dpt = water
         endif
      else
         dpt = water
      endif
      
      hysteresis = network%adm%hysteresis_for_summerdike(:,igrid)
      call GetCSParsTotal(cross1, cross2, factor, dpt, totalArea, totalWidth, CS_TYPE_PREISMAN, hysteresis)
      
      getTotalWidthGP = totalWidth

   end function getTotalWidthGP

   double precision function getWetPerimeterGP(network, igrid, water, DepthOrLevel)
   
      type(t_network), intent(in)        :: network
      integer, intent(in)                :: igrid
      double precision, intent(in)       :: water
      integer, optional                  :: DepthOrLevel
   
      type (t_CrossSection), pointer     :: cross1
      type (t_CrossSection), pointer     :: cross2 
      double precision                   :: factor

      double precision                   :: bob_grid_point
      double precision                   :: dpt
      double precision                   :: czdum
      double precision                   :: flowArea
      double precision                   :: wetPerimeter
      double precision                   :: flowWidth
      double precision                   :: conv

      getWetPerimeterGP = 0.0d0
      
      cross1 => network%crs%cross(network%adm%gpnt2cross(igrid)%c1)
      cross2 => network%crs%cross(network%adm%gpnt2cross(igrid)%c2)
      factor =  network%adm%gpnt2cross(igrid)%f
                
      if (present(DepthOrLevel)) then
         if (DepthOrLevel ==  CSH_LEVEL) then     
            bob_grid_point = getBob(cross1, cross2, factor)
            dpt = bob_grid_point + water
         else
            dpt = water
         endif
      else
         dpt = water
      endif
      
      czdum = 0.0d0
      
      call GetCSParsFlow(cross1, cross2, factor, dpt, 0.0d0, czdum, flowArea, wetPerimeter, flowWidth, conv)
      
      getWetPerimeterGP = wetPerimeter

   end function getWetPerimeterGP

   subroutine getCrossFlowDataGP(network, igrid, water, DepthOrLevel, flowArea, flowWidth, wetPerimeter)
   
      type(t_network), intent(in)              :: network
      integer, intent(in)                      :: igrid
      double precision, intent(in)             :: water
      integer, optional, intent(in)            :: DepthOrLevel
      double precision, optional, intent(out)  :: flowArea
      double precision, optional, intent(out)  :: flowWidth
      double precision, optional, intent(out)  :: wetPerimeter
   
      type (t_CrossSection), pointer     :: cross1
      type (t_CrossSection), pointer     :: cross2 
      double precision                   :: factor

      double precision                   :: bob_grid_point
      double precision                   :: dpt
      double precision                   :: area
      double precision                   :: width
      double precision                   :: perimeter
      double precision                   :: czdum
      double precision                   :: conv

      cross1 => network%crs%cross(network%adm%gpnt2cross(igrid)%c1)
      cross2 => network%crs%cross(network%adm%gpnt2cross(igrid)%c2)
      factor =  network%adm%gpnt2cross(igrid)%f
                
      if (present(DepthOrLevel)) then
         if (DepthOrLevel ==  CSH_LEVEL) then     
            bob_grid_point = getBob(cross1, cross2, factor)
            dpt = bob_grid_point + water
         else
            dpt = water
         endif
      else
         dpt = water
      endif
      
      czdum = 0.0d0
      
      call GetCSParsFlow(cross1, cross2, factor, dpt, 0.0d0, czdum, area, perimeter, width, conv)
      
      if (present(flowArea))     flowArea = area
      if (present(flowWidth))    flowWidth = width
      if (present(wetPerimeter)) wetPerimeter = perimeter

   end subroutine getCrossFlowDataGP
   
   subroutine getCrossFlowSectionGP(network, igrid, isec, water, DepthOrLevel, flowArea, flowWidth, wetPerimeter)
   
      type(t_network), intent(in)              :: network
      integer, intent(in)                      :: igrid
      integer, intent(in)                      :: isec
      double precision, intent(in)             :: water
      integer, optional, intent(in)            :: DepthOrLevel
      double precision, optional, intent(out)  :: flowArea
      double precision, optional, intent(out)  :: flowWidth
      double precision, optional, intent(out)  :: wetPerimeter
   
      type (t_CrossSection), pointer           :: cross1
      type (t_CrossSection), pointer           :: cross2 
      double precision                         :: factor

      double precision                   :: bob_grid_point
      double precision                   :: dpt
      
      double precision                   :: area
      double precision                   :: width
      double precision                   :: perimeter

      double precision                   :: area1
      double precision                   :: width1
      double precision                   :: perimeter1
      double precision                   :: area2
      double precision                   :: width2
      double precision                   :: perimeter2

      cross1 => network%crs%cross(network%adm%gpnt2cross(igrid)%c1)
      cross2 => network%crs%cross(network%adm%gpnt2cross(igrid)%c2)
      factor =  network%adm%gpnt2cross(igrid)%f
      
      if (present(DepthOrLevel)) then
         if (DepthOrLevel ==  CSH_LEVEL) then     
            bob_grid_point = getBob(cross1, cross2, factor)
            dpt = bob_grid_point + water
         else
            dpt = water
         endif
      else
         dpt = water
      endif

      call GetTabFlowSectionFromTables(dpt, cross1, isec, area1, width1, perimeter1)
      call GetTabFlowSectionFromTables(dpt, cross2, isec, area2, width2, perimeter2)
      
      area      = (1.0d0 - factor) * area1      + factor * area2
      width     = (1.0d0 - factor) * width1     + factor * width2
      perimeter = (1.0d0 - factor) * perimeter1 + factor * perimeter2
      
      if (present(flowArea))     flowArea = area
      if (present(flowWidth))    flowWidth = width
      if (present(wetPerimeter)) wetPerimeter = perimeter

   end subroutine getCrossFlowSectionGP
   
   subroutine getCrossFlowData_on_link(network, ilink, depth, flowArea, flowWidth, wetPerimeter, conveyance, cz, af_sub, perim_sub, cz_sub)
   
      type(t_network), intent(in)              :: network
      integer, intent(in)                      :: ilink
      double precision, intent(in)             :: depth
      double precision, optional, intent(out)  :: flowArea
      double precision, optional, intent(out)  :: flowWidth
      double precision, optional, intent(out)  :: wetPerimeter
      double precision, optional, intent(out)  :: conveyance
      double precision, optional, intent(inout):: cz
      double precision, optional, intent(out)  :: af_sub(3)
      double precision, optional, intent(out)  :: perim_sub(3)
      double precision, optional, intent(out)  :: cz_sub(3)
      
      type (t_CrossSection), pointer     :: cross1
      type (t_CrossSection), pointer     :: cross2 
      double precision                   :: factor

      double precision                   :: area
      double precision                   :: width
      double precision                   :: perimeter
      double precision                   :: czdum
      double precision                   :: conv
      double precision                   :: af_sub_local(3)
      double precision                   :: perim_sub_local(3)
      double precision                   :: cz_sub_local(3)
      double precision                   :: cz_

      if (network%adm%line2cross(ilink)%c1 <= 0) then
         ! no cross section defined on branch, use default definition
         area = default_width* depth
         perimeter = default_width + 2*depth
         if (present(cz)) then
            cz_ = cz
         else
            cz_ = 60d0
         end if

         if (present(flowArea))     flowArea = area
         if (present(flowWidth))    flowWidth = default_width
         if (present(wetPerimeter)) wetPerimeter = perimeter
         if (present(conveyance))   conveyance = cz_* area * sqrt(area/perimeter)
         if (present(af_sub   )) then
            af_sub    = 0d0
            af_sub(1) = area
         endif
         if (present(perim_sub)) then
            perim_sub    = 0d0
            perim_sub(1) = perimeter
         endif
         if (present(cz_sub   )) then
            cz_sub    = 0d0
            cz_sub(1) = cz_
         endif
         return
      endif

      cross1 => network%crs%cross(network%adm%line2cross(ilink)%c1)
      cross2 => network%crs%cross(network%adm%line2cross(ilink)%c2)
      factor =  network%adm%line2cross(ilink)%f
                
      
      czdum = 0d0
      
      call GetCSParsFlow(cross1, cross2, factor, depth, 0.0d0, czdum, area, perimeter, width, conv, &
                         af_sub_local, perim_sub_local, cz_sub_local)
      
      if (present(flowArea))          flowArea = area
      if (present(flowWidth))         flowWidth = width
      if (present(wetPerimeter))      wetPerimeter = perimeter
      if (present(conveyance))        conveyance = conv
      if (present(af_sub   ))   af_sub    = af_sub_local   
      if (present(perim_sub))   perim_sub = perim_sub_local
      if (present(cz_sub   ))   cz_sub    = cz_sub_local   
      if (present(cz       ))   cz        = czdum

   end subroutine getCrossFlowData_on_link
   
   subroutine getCrossTotalData_on_link(network, ilink, depth, totalArea, totalWidth, calculationOption)
   
      type(t_network), intent(inout)           :: network
      integer, intent(in)                      :: ilink
      double precision, intent(in)             :: depth
      double precision, intent(out)  :: totalArea
      double precision, intent(out)  :: totalWidth
                                                        !> type of total area computation, possible values:\n
                                                        !! CS_TYPE_PREISMAN  Ordinary total area computation, with possible Preisman lock on top\n
                                                        !! CS_TYPE_PLUS      Total area for only the expanding part of the cross section (Nested Newton method)\n
                                                        !! CS_TYPE_MIN       Total area for only the narrowing part of the cross section (Nested Newton method)
      integer, intent(in)               :: calculationOption 

      type (t_CrossSection), pointer     :: cross1
      type (t_CrossSection), pointer     :: cross2 
      double precision                   :: factor

      if (network%adm%line2cross(ilink)%c1 <= 0) then
         ! no cross section defined on branch, use default definition
         totalArea  = sl*depth
         totalWidth = sl
         return
      endif
      
      cross1 => network%crs%cross(network%adm%line2cross(ilink)%c1)
      cross2 => network%crs%cross(network%adm%line2cross(ilink)%c2)
      factor =  network%adm%line2cross(ilink)%f
 
      call GetCSParsTotal(cross1, cross2, factor, depth, TotalArea, TotalWidth, calculationOption, network%adm%hysteresis_for_summerdike(:,ilink))
      
   end subroutine getCrossTotalData_on_link
   
   function getbobs(network, ilink) result(res)
      type(t_network), intent(in) :: network
      integer, intent(in) :: ilink
      double precision, dimension(2) ::res
      
      type (t_CrossSection), pointer     :: cross1
      type (t_CrossSection), pointer     :: cross2 
      double precision :: dx
      double precision :: dxlocal
      double precision :: distancelocal
      double precision :: factor
      double precision :: linkpos

      if (network%adm%line2cross(ilink)%c1 < 0) then
         ! no cross section on this branch
         res = huge(1d0)
         return
      endif

      cross1 => network%crs%cross(network%adm%line2cross(ilink)%c1)
      cross2 => network%crs%cross(network%adm%line2cross(ilink)%c2)
      
      if (network%adm%line2cross(ilink)%c1 == network%adm%line2cross(ilink)%c2) then 
          res(1) = getBob(cross1)
          res(2) = res(1)
      else
          dxlocal = 0.5d0*getdeltax(network, ilink) 
          distancelocal = cross2%chainage - cross1%chainage
          dx = dxlocal/distancelocal
          linkpos = network%adm%line2cross(ilink)%f
          factor = linkpos - dx
          res(1) = getBob(cross1, cross2, factor)      
          factor = linkpos + dx
          res(2) = getBob(cross1, cross2, factor)      
      endif    
   end function getbobs   
   
   double precision function getdeltax(network, ilink)
      type(t_network), intent(in) :: network
      integer, intent(in) :: ilink
      
      integer :: ibr, ll
      
      ibr = network%adm%lin2ibr(ilink)
      ll  = network%adm%lin2point(ilink)
      getdeltax = network%brs%branch(ibr)%dx(ll)
   end function getdeltax

   subroutine getCrossTotalDataGP(network, igrid, water, hysteresis, DepthOrLevel, totalArea, totalWidth)
   
      type(t_network), intent(inout)              :: network
      integer, intent(in)                      :: igrid
      double precision, intent(in)             :: water
      integer, optional, intent(in)            :: DepthOrLevel
      double precision, optional, intent(out)  :: totalArea
      double precision, optional, intent(out)  :: totalWidth
      logical, intent(inout)                   :: hysteresis(2)
      
      type (t_CrossSection), pointer     :: cross1
      type (t_CrossSection), pointer     :: cross2 
      double precision                   :: factor

      double precision                   :: bob_grid_point
      double precision                   :: dpt
      double precision                   :: area
      double precision                   :: width

      cross1 => network%crs%cross(network%adm%gpnt2cross(igrid)%c1)
      cross2 => network%crs%cross(network%adm%gpnt2cross(igrid)%c2)
      factor =  network%adm%gpnt2cross(igrid)%f
                
      if (present(DepthOrLevel)) then
         if (DepthOrLevel ==  CSH_LEVEL) then     
            bob_grid_point = getBob(cross1, cross2, factor)
            dpt = bob_grid_point + water
         else
            dpt = water
         endif
      else
         dpt = water
      endif
      
      call GetCSParsTotal(cross1, cross2, factor, dpt, area, width, CS_TYPE_PREISMAN, hysteresis)
      
      if (present(totalArea))  totalArea = area
      if (present(totalWidth)) totalWidth = width

   end subroutine getCrossTotalDataGP
   
   subroutine getSummerDikeGP(network, igrid, water, sdArea, sdWidth, doFlow, DepthOrLevel, hysteresis)
   
      ! Get Wetted Area at GridPoint
      type(t_network), intent(in)              :: network
      integer, intent(in)                      :: igrid
      double precision, intent(in)             :: water
      double precision, intent(out)            :: sdArea
      double precision, intent(out)            :: sdWidth
      logical, intent(in)                      :: doFlow
      integer, optional, intent(in)            :: DepthOrLevel
      logical, intent(inout)                   :: hysteresis 
   
      type (t_CrossSection), pointer     :: cross1
      type (t_CrossSection), pointer     :: cross2 
      double precision                   :: factor

      double precision                   :: bob_grid_point
      double precision                   :: dpt
      
      cross1 => network%crs%cross(network%adm%gpnt2cross(igrid)%c1)
      cross2 => network%crs%cross(network%adm%gpnt2cross(igrid)%c2)
      factor =  network%adm%gpnt2cross(igrid)%f
                
      if (present(DepthOrLevel)) then
         if (DepthOrLevel ==  CSH_LEVEL) then     
            bob_grid_point = getBob(cross1, cross2, factor)
            dpt = bob_grid_point + water
         else
            dpt = water
         endif
      else
         dpt = water
      endif
      
      call interpolateSummerDike(cross1, cross2, factor, dpt, sdArea, sdWidth, doFlow, hysteresis)
      
   end subroutine getSummerDikeGP  

   subroutine getSummerDikeGPData(network, igrid, summerdike)
   
      ! Get Summer Dike Data at GridPoint
      type(t_network), intent(in)                 :: network
      integer, intent(in)                         :: igrid
      type (t_summerdike), pointer, intent(inout) :: summerdike
   
      type (t_CrossSection), pointer     :: cross1
      type (t_CrossSection), pointer     :: cross2 
      double precision                   :: f
      
      type (t_summerdike), pointer       :: summerdike1
      type (t_summerdike), pointer       :: summerdike2
      
      double precision                   :: shift1
      double precision                   :: shift2

      cross1 => network%crs%cross(network%adm%gpnt2cross(igrid)%c1)
      cross2 => network%crs%cross(network%adm%gpnt2cross(igrid)%c2)
      f      =  network%adm%gpnt2cross(igrid)%f
      
      allocate(summerdike)
      summerdike1 => cross1%tabDef%summerdike
      summerdike2 => cross2%tabDef%summerdike
            
      shift1 = cross1%shift
      shift2 = cross2%shift
            
      if (associated(summerdike1) .and. associated(summerdike2)) then
         summerdike%crestLevel = (1.0d0 - f) * (summerdike1%crestLevel + shift1) + f * (summerdike2%crestLevel + shift2)
         summerdike%baseLevel  = (1.0d0 - f) * (summerdike1%baseLevel + shift1)  + f * (summerdike2%baseLevel + shift2)
         summerdike%flowArea   = (1.0d0 - f) * summerdike1%flowArea   + f * summerdike2%flowArea
         summerdike%totalArea  = (1.0d0 - f) * summerdike1%totalArea   + f * summerdike2%totalArea
      elseif (associated(summerdike1) .and. .not. associated(summerdike2)) then
         summerdike%crestLevel = (1.0d0 - f) * (summerdike1%crestLevel + shift1) + f * cross2%bedlevel
         summerdike%baseLevel  = (1.0d0 - f) * (summerdike1%baseLevel + shift1)  + f * cross2%bedlevel
         summerdike%flowArea   = (1.0d0 - f) * summerdike1%flowArea
         summerdike%totalArea  = (1.0d0 - f) * summerdike1%totalArea
      elseif (.not. associated(summerdike1) .and. associated(summerdike2)) then
         summerdike%crestLevel = (1.0d0 - f) * cross1%bedlevel + f * (summerdike2%crestLevel + shift2)
         summerdike%baseLevel  = (1.0d0 - f) * cross1%bedlevel + f * (summerdike2%baseLevel + shift2)
         summerdike%flowArea   = f * summerdike2%flowArea
         summerdike%totalArea  = f * summerdike2%totalArea
      else
         ! Not any Summer Dike Data
         deallocate(summerdike)
         summerdike => null()
      endif
     
   end subroutine getSummerDikeGPData

   double precision function getChezyFromYZ(cru, network, ilink, di, r0, qi)
 
      type(t_crsu), intent(in)          :: cru
      type(t_network), intent(in)       :: network
      integer, intent(in)               :: ilink
      double precision, intent(in)      :: di
      double precision, intent(in)      :: r0
      double precision, intent(in)      :: qi
   
      type (t_CrossSection), pointer     :: cross1
      type (t_CrossSection), pointer     :: cross2 
      double precision                   :: factor

      if (cru%conveyType == CS_VERT_SEGM) then

         if (di <= cru%hu(cru%nru)) then

            ! Normal Interpolation
            if (qi < 0.0d0 .and. allocated(cru%cz2) ) then
               getChezyFromYZ = interpolate(cru%hu, cru%cz2, cru%nru, di)
            else
               getChezyFromYZ = interpolate(cru%hu, cru%cz1, cru%nru, di)
            endif

         else

            ! Get and interpolate earlier calculated Chezy value
         
            cross1 => network%crs%cross(network%adm%line2cross(ilink)%c1)
            cross2 => network%crs%cross(network%adm%line2cross(ilink)%c2)
            factor =  network%adm%line2cross(ilink)%f
        
            getChezyFromYZ = (1.0d0 - factor) * cross1%convTab%chezy_act + factor * cross2%convTab%chezy_act
            
         endif

      else
         !rad = interpolate(cru%hu, cru%af, cru%nru, di)/interpolate(cru%hu, cru%pf, cru%nru, di)
         getChezyFromYZ = GetChezy(nint(cru%cz1(1)), cru%cz2(1), r0, di, 0d0)
      endif
   
   end function getChezyFromYZ

   subroutine resetSummerDike(network, igrid)
   
      type(t_network), intent(inout)     :: network
      integer, intent(in)                :: igrid

      logical, pointer :: hysteresis(:)

      network%adm%hysteresis_for_summerdike(:,igrid) = .true.

   end subroutine resetSummerDike

! =================================================================================================
! =================================================================================================
   subroutine getConveyance(network, dpt, u1L, q1L, s1L, L, perim_sub, flowarea_sub, conv, cz_sub, cz, flowArea, wetPerimeter)
      use m_CrossSections     , only: t_CSType, CS_TABULATED, CS_YZ_PROF
      
      implicit none
      type(t_network), intent(in)    :: network
      double precision               :: dpt, u1L, q1L, s1L
      double precision, intent(out)  :: conv
      integer                        :: i , L, n
      double precision, dimension(3), intent(in) :: flowarea_sub, perim_sub
      double precision, dimension(3), intent(out) :: cz_sub
      type(t_CSType), pointer        :: cross
      double precision, intent(out)  :: cz
      double precision, parameter    :: eps = 1d-3               !< accuracy parameter for determining wetperimeter == 0d0
      double precision               :: r
      double precision, intent(in)   :: flowArea
      double precision, intent(in)   :: wetPerimeter
      integer                        :: igrid
      integer                        :: ibranch
      logical                        :: compute_conveyance  
      
      n = network%adm%line2cross(L)%c1
      if ( n <= 0) then
         ! no cross section defined on L
         compute_conveyance = .true.
      else
         cross => network%crs%cross(n)%tabdef
         igrid   = network%adm%lin2grid(L)
         ibranch = network%adm%lin2ibr(L)
         ! for YZ profiles CalcCSParsFlow computes the conveyance
         compute_conveyance = cross%crosstype /= CS_YZ_PROF
      endif
      
      if (compute_conveyance) then
         igrid   = network%adm%lin2grid(L)
         ibranch = network%adm%lin2ibr(L)
         conv = 0d0
         if (perim_sub(1)> 0.0d0) then
            do i = 1, 3
               if (perim_sub(i) > eps .and. flowarea_sub(i) > 0.0d0) then
                  r = flowarea_sub(i)/perim_sub(i)
                  cz_sub(i) =  getFrictionValue(network%rgs, network%spData, ibranch, i, igrid, s1L, q1L, u1L, r, dpt)
                  conv = conv + cz_sub(i) * flowarea_sub(i) * sqrt(flowarea_sub(i) / perim_sub(i))
               else
                  cz_sub(i) = 0
               endif
            enddo
         endif
         ! compute average chezy 
         cz = conv/(flowArea*sqrt(flowArea/wetPerimeter))
         ! criteria to satisfy the criteria  in normup i.e cz(m)*cz(m)*wet
      
      endif

   end subroutine getConveyance

   
   subroutine getCrossDischarge(perim_sub, flowarea_sub, cz_sub, q1_local, q_sub)
      ! Get discharges per reachsubsegment - based on plqsec() by J.Kuipers 
      implicit none
      double precision, dimension(3), intent(in) :: flowarea_sub, cz_sub, perim_sub
      double precision,               intent(in) :: q1_local
      double precision, dimension(3), intent(out) :: q_sub  
      double precision, dimension(3)             :: r_sub  
      integer                                    :: isec
      double precision                           :: acrtot
      double precision                           :: qacrtot
      double precision, parameter                :: eps_perim = 1d-3               ! accuracy wetted perimeter == 0d0
      double precision, parameter                :: eps_acrtot = 1d-10            ! accuracy weighted area parameter == 0d0
      double precision, parameter                :: eps_area = 1d-6               ! accuracy area == 0d0
      
      acrtot = 0.d0
      do isec = 1,3
          if (perim_sub(isec) > eps_perim) then
              r_sub(isec) = flowarea_sub(isec)/perim_sub(isec)
              acrtot = acrtot+flowarea_sub(isec)*cz_sub(isec) * sqrt(r_sub(isec))
          endif
      enddo
      if (acrtot > eps_acrtot) then
         qacrtot = q1_local/acrtot
         q_sub = 0.d0
         do isec = 1, 3
            if (flowarea_sub(isec) > eps_area) then
               q_sub(isec) = flowarea_sub(isec)*cz_sub(isec) *sqrt(r_sub(isec))*qacrtot
            endif
         enddo
      endif      
   end subroutine getCrossDischarge
   
   
end module m_cross_helper
