module m_Culvert
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

   use m_CrossSections
   use m_tables
   use MessageHandling
   use m_GlobalParameters

   implicit none

   private

   public ComputeCulvert
   public dealloc
   
   interface dealloc
      module procedure deallocCulvert
   end interface dealloc
   
   type, public :: t_culvert
      integer                         :: culvertType ! ST_CULVERT, ST_SIPHON or ST_INV_SIPHON
      double precision                :: leftlevel
      double precision                :: rightlevel
      type(t_crosssection), pointer   :: pcross => null()              
      integer                         :: crosssectionnr      
      integer                         :: allowedflowdir
      double precision                :: length
      double precision                :: inletlosscoeff
      double precision                :: outletlosscoeff
      logical                         :: has_valve
      double precision                :: inivalveopen
      type(t_table), pointer          :: losscoeff => null()
      
      ! Bend Loss for Siphons
      double precision                :: bendlosscoeff

      ! Levels for Normal Siphon
      double precision                :: turnonlevel
      double precision                :: turnofflevel
      logical                         :: is_siphon_on = .false.

   end type

contains

   subroutine deallocCulvert(culvert)
      ! Modules

      implicit none
      ! Input/output parameters
      type(t_culvert), pointer   :: culvert

      ! Local variables

      ! Program code
      if (associated(culvert) ) then
         if (associated(culvert%lossCoeff))           deallocate(culvert%lossCoeff)
         culvert%lossCoeff => null()
         deallocate(culvert)
      endif
      
      culvert => null()
      
   end subroutine deallocCulvert
                              
   subroutine ComputeCulvert(culvert, fum, rum, aum, dadsm, kfum, cmustr, s1m1, s1m2, qm,  &
                             q0m, u1m, u0m, dxm, dt, bobgrm1, bobgrm2, wetdown, state, infuru)
      
      implicit none
      !
      ! Global variables
      !
      type(t_culvert), pointer                     :: culvert
      integer, intent(out)                         :: kfum
      double precision, intent(out)                :: aum
      double precision, intent(out)                :: dadsm
      double precision, intent(out)                :: fum
      double precision, intent(inout)              :: q0m
      double precision, intent(inout)              :: qm
      double precision, intent(out)                :: rum
      double precision, intent(out)                :: cmustr
      double precision, intent(inout)              :: u0m
      double precision, intent(inout)              :: u1m
      double precision, intent(in)                 :: s1m2         !< left waterlevel s(m)          sleft
      double precision, intent(in)                 :: s1m1         !< right waterlevel s(m+1)       sright
      double precision, intent(in)                 :: dxm
      double precision, intent(in)                 :: dt
      double precision, intent(in)                 :: bobgrm1
      double precision, intent(in)                 :: bobgrm2
      double precision, intent(in)                 :: wetdown
      integer, intent(inout)                       :: state
      logical, intent(in)                          :: infuru
         
      ! Local variables
      type(t_CrossSection)           :: CrossSection
      integer                        :: allowedflowdir
      integer                        :: dir

      logical                        :: IsCulvert
      logical                        :: IsSiphon
      logical                        :: IsInvertedSiphon

      double precision               :: smax             !< zeta_1 (upstream water level)
      double precision               :: smin             !< zeta_2 (downstream water level)
      logical                        :: firstafterdry
      logical                        :: isfreeflow
      logical                        :: belowBottom
      double precision               :: bu
      double precision               :: cmus
      double precision               :: cu
      double precision               :: d00
      double precision               :: d11
      double precision               :: dc                  !< hc_2 critical depth
      double precision               :: culvertCrest
      double precision               :: inflowCrest         !< zc_1 (at upstream water level)
      double precision               :: outflowCrest        !< zc_2 (at downstream water level)
      double precision               :: du
      double precision               :: fr
      double precision               :: uest
      double precision               :: gl_thickness
      double precision               :: dummy
      double precision               :: dpt                 !< upstream water depth
      double precision               :: openingfac
      double precision               :: inivalveopen
      double precision               :: chezyCulvert
      double precision               :: culvertChezy
      double precision               :: chezyValve
      double precision               :: wArea               !< upstream wet area (no valve)
      double precision               :: wPerimiter          !< upstream wet perimeter  (no valve)
      double precision               :: wWidth              !< upstream wet surface width (no valve)
      double precision               :: valveArea           !< upstream wet area
      double precision               :: valvePerimiter      !< upstream wet perimeter
      double precision               :: valveWidth          !< upstream wet surface width
      double precision               :: hydrRadius
      double precision               :: culvertArea
      double precision               :: valveloss
      double precision               :: exitloss
      double precision               :: frictloss
      double precision               :: totalLoss

      ! Culvert Type
      if (culvert%culvertType == ST_SIPHON) then
         IsCulvert        = .false.
         IsSiphon         = .true.
         IsInvertedSiphon = .false.
      elseif (culvert%culvertType == ST_INV_SIPHON) then
         IsCulvert        = .false.
         IsSiphon         = .false.
         IsInvertedSiphon = .true.
      else
         IsCulvert        = .true.
         IsSiphon         = .false.
         IsInvertedSiphon = .false.
      endif

      ! Find the flow direction
      if (s1m1 > s1m2) then
         smax = s1m1
         smin = s1m2
         dir  = 1
         outflowCrest = culvert%rightlevel
         inflowCrest  = culvert%leftlevel
      else
         smax = s1m2
         smin = s1m1
         dir  = -1
         outflowCrest = culvert%leftlevel
         inflowCrest  = culvert%rightlevel
      endif
      culvertCrest = max(outflowCrest, inflowCrest) 

      ! Chack on Flow Direction
      allowedFlowDir = culvert%allowedflowdir
      if ((allowedFlowDir == 3) .or. &
          (dir == 1  .and. allowedFlowDir == 2) .or. &
          (dir == -1 .and. allowedFlowDir == 1)) then
         kfum  = 0
         fum   = 0.0d0
         rum   = 0.0d0
         u1m   = 0.0d0
         u0m   = 0.0d0
         qm    = 0.0d0
         q0m   = 0.0d0
         state = 0
         return
      endif

      CrossSection = culvert%pcross
      gl_thickness = getGroundLayer(CrossSection)

      ! Check on Valve
      if (culvert%has_valve .and. ((culvert%inivalveopen - gl_thickness) < thresholdDry)) then
         kfum  = 0
         fum   = 0.0d0
         rum   = 0.0d0
         u1m   = 0.0d0
         u0m   = 0.0d0
         qm    = 0.0d0
         q0m   = 0.0d0
         state = 0
         return
      endif

      ! Check on flooding or drying with treshold
      firstafterdry = .false.

      if (IsCulvert) then
      
         ! Check if structure is lower than bottom
         belowBottom = .false.
         if ((bobgrm1 > Culvert%leftlevel + gl_thickness) .or.      &
             (bobgrm2 > Culvert%rightlevel + gl_thickness)) then
            belowBottom = .true.
         endif
         
         if (((smax - culvertCrest - gl_thickness) < thresholdDry) .or.               &
             (belowBottom .and. (dir == 1) .and. ((s1m1 - bobgrm1) < thresholdDry))   &
             .or.                                                                     &
             (belowBottom .and. (dir == -1) .and. ((s1m2 - bobgrm2) < thresholdDry))) then
            kfum = 0
         elseif (((smax - culvertCrest - gl_thickness) > thresholdFlood) .and.               &
                 ((belowBottom .and. (dir == 1) .and. ((s1m1 - bobgrm1) > thresholdFlood))   &
                 .or.                                                                        &
                 (belowbottom .and. (dir == -1) .and. ((s1m2 - bobgrm2) > thresholdFlood)))) then
            if (kfum == 0) then
               firstafterdry = .true.
            endif
            kfum = 1
         elseif (((smax - culvertCrest - gl_thickness) > thresholdFlood) .and. (.not. belowBottom)) then
            if (kfum == 0) then
               firstafterdry = .true.
            endif
            kfum = 1
         endif

         if (kfum==0) then 
            kfum  = 0
            fum   = 0.0d0
            rum   = 0.0d0
            u1m   = 0.0d0
            u0m   = 0.0d0
            qm    = 0.0d0
            q0m   = 0.0d0
            state = 0
            return
         endif
      
      else
      
         if ( ((smax - culvertCrest - gl_thickness) < thresholdDry) .or.                             &
              (IsInvertedSiphon .and. (dir == 1)  .and. ((s1m1 - bobgrm1) < thresholdDry)) .or.     &
              (IsInvertedSiphon .and. (dir == -1) .and. ((s1m2 - bobgrm2) < thresholdDry))) then
            kfum = 0
         elseif ( ((smax - culvertCrest - gl_thickness) > thresholdFlood) .and.                            &
                  ((IsInvertedSiphon .and. (dir == 1)  .and. ((s1m1 - bobgrm1) > thresholdFlood)) .or.    &
                   (IsInvertedSiphon .and. (dir == -1) .and. ((s1m2 - bobgrm2) > thresholdFlood)))) then
            if (kfum == 0) then
               firstafterdry = .true.
            endif
            kfum = 1
         endif

         if (IsInvertedSiphon .and. kfum == 0) then
            fum   = 0.0d0
            rum   = 0.0d0
            u1m   = 0.0d0
            u0m   = 0.0d0
            qm    = 0.0d0
            q0m   = 0.0d0
            state = 0
            return
         endif

      endif

      !     First find out the critical depth that can be used in free flow equations
      !     pjo, 13-04-2000, ars 4952, when flow direction changes, critical
      !     depth is taken as zero.
      ! if (firstafterdry .or. (dble(dir) * qm <= 0.0d0)) then Simplification does not give same result
      if (firstafterdry .or.                       &
          ((dir == -1) .and. (qm > 0.0d0)) .or.    &
          ((dir == 1)  .and. (qm < 0.0d0))) then
         dc = 0.0d0 
      else
         dc = GetCriticalDepth(qm, CrossSection)
      endif

      if (IsSiphon) then
      
         ! Siphon
         ! Now find out whether the upstream water level is greater than the
         ! crest level + characteristic height of siphon
         ! and check on the flow direction of the siphon -> can be only positive
         if ( (smax - (inflowCrest + CrossSection%charHeight) < -ThresholdSiphon * CrossSection%charHeight) .or. &
              (s1m1 < s1m2)) then
            kfum = 0
         elseif ((smax - (inflowCrest + CrossSection%charHeight)) > thresholdFlood) then
            kfum = 1
         else
         endif
      
         if (kfum == 0) then
            fum = 0.0d0
            rum = 0.0d0
            u1m = 0.0d0
            qm  = 0.0d0
            q0m = 0.0d0
            return
         endif

         ! Siphon
         ! Check for switch on or off of siphon
         if (s1m2 <= culvert%turnonlevel) then
            culvert%is_siphon_on = .true.
         elseif (s1m2 >= culvert%turnofflevel) then
            culvert%is_siphon_on = .false.
         else
         endif

         ! Check if siphon is in operation
         if (.not. culvert%is_siphon_on) then
            kfum = 0
            fum = 0.0d0
            rum = 0.0d0
            u1m = 0.0d0
            qm  = 0.0d0
            q0m = 0.0d0
            return
         endif

      endif

      ! Calculate cross-section values in culvert
      if (IsCulvert) then
         dpt = smax - inflowCrest
      else
         ! Siphon always runs full
         dpt = CrossSection%charHeight
      endif
      
      chezyCulvert = 0.0d0
      call GetCSParsFlow(CrossSection, dpt, u0m, chezyCulvert, wArea, wPerimiter, wWidth, dummy)     
                  
      ! Valve Loss
      if (culvert%has_valve .and. (culvert%inivalveopen < dpt)) then
         inivalveopen = culvert%inivalveopen
         chezyValve = 0.0d0
         call GetCSParsFlow(CrossSection, inivalveopen, u0m, chezyValve, valveArea, valvePerimiter, valveWidth, dummy)     
         openingfac = (inivalveopen - gl_thickness) / (CrossSection%charHeight - gl_thickness)
      else
         openingfac = 2.0d0     ! >> 1, so not influenced by valve
      endif
      
      if (openingfac >= 1.0d0) then
         hydrRadius  = wArea / wPerimiter
         culvertArea  = wArea
         culvertChezy = chezyCulvert
         valveloss = 0.0d0
      else
         valveloss = interpolate(culvert%lossCoeff, openingfac)
         hydrRadius  = valveArea / (valvePerimiter + valveWidth)
         culvertArea  = valveArea
         culvertChezy = chezyValve
      endif
      
      !Friction Loss
      frictloss = 2.0d0 * gravity * culvert%length / (culvertChezy * culvertChezy * hydrRadius)            ! culvert friction established
      
      ! Check if flow is free flow or submerged
      if (smin >= (outflowCrest + gl_thickness + dc)) then  

         ! Submerged Flow
         isfreeflow = .false.

         exitloss = culvert%outletlosscoeff * (max((1.0d0 - culvertArea / wetdown), 0.0d0))**2
         exitloss = max(exitloss, 0.0d0)

      else
      
         ! Free Flow
         isfreeflow = .true.
         exitloss = 0.0d0
         
      endif
      
      totalLoss = exitloss + frictloss + culvert%inletlosscoeff + valveloss
      
      if (.not. IsCulvert) then        ! Is a Siphon
         totalLoss = totalLoss + culvert%bendlosscoeff
      endif
      
      totalLoss = max(totalLoss, 0.01d0)
      
      cmus = 1.0d0 / sqrt(totalLoss)
      cmus = min(cmus, 1.0d0)    ! Limit to maximum of 1.0
 
      cmustr = cmus
      aum    = culvertArea
      dadsm  = wWidth

      if (infuru) then
      
         if (isCulvert) then
         
            uest = u1m

         else

            if (abs(u1m) < 1.0d-8)then

               ! Estimate velocity through siphon
               ! Because the velocity is used explicitly an estimation of u is necessary to prevent 
               ! overshoot situations.

               if (isfreeflow) then
                  uest = cmus * sqrt(2 * gravity * (smax - outflowCrest - gl_thickness - dc))
               else
                  uest = cmus * sqrt(2  *gravity * (smax - smin))
               endif
            else
               uest = u1m        
            endif   
         
         endif

         if (isfreeflow) then
         
            if (dir==1) then
               d11 = s1m1 - outflowCrest - gl_thickness - dc
            else
               d11 = s1m2 - outflowCrest - gl_thickness - dc
            endif
            
            d00 = max(1.0d-10, smax - smin)
            
            cu = cmus * cmus * 2.0d0 * gravity * d11 / (dxm * d00)
            
         else
         
            cu = cmus * cmus * 2.0d0 * gravity / dxm
            
         endif
         
         fr = abs(uest) / dxm
         
         bu = 1.0d0 / dt + fr
         du = u0m / dt
         
         fum = cu / bu
         rum = du / bu
         
      endif
      
      if (isfreeflow) then
         state = 5
      else
         state = 6
      endif
    
   end subroutine ComputeCulvert

end module m_Culvert


