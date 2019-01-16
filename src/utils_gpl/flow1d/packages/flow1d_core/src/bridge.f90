module m_Bridge
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
   
   ! Modules
   use m_GlobalParameters
   use m_CrossSections

   implicit none

   public ComputeBridge

   type, public :: t_bridge
      double precision              :: bedLevel
      double precision              :: pillarwidth
      double precision              :: formfactor
      integer                       :: allowedflowdir  ! 0 all directions
                                                       ! 1 only positive flow
                                                       ! 2 only negative flow
                                                       ! 3 no flow allowed
      logical                       :: useOwnCrossSection
      type(t_crosssection), pointer :: pcross => null()              
      integer                       :: crosssectionnr
      integer                       :: bedFrictionType
      integer                       :: groundFrictionType
      double precision              :: bedFriction
      double precision              :: groundFriction
      double precision              :: length
      double precision              :: inletlosscoeff
      double precision              :: outletlosscoeff
   end type

   private

contains

   subroutine ComputeBridge(bridge, glThick, fum, rum, aum, dadsm, wtm, wtgm, kfum, convm,             &
                            cmustr, s1m1, s1m2, qm, q0m, u1m, u0m, dxm, dt,                            &
                            wetup, widthup, wperimup, convup, bobup, wetdown, infuru)
      !=======================================================================
      !                       Deltares
      !                One-Two Dimensional Modelling System
      !                           S O B E K
      !
      ! Subsystem:          Flow Module
      !
      ! Programmer:         J Dhondia
      !
      ! Module:             bridge (BRIDGE)
      !
      ! Module description:Computes coefficients for momentum equation in bridge
      !                    (uses four types of bridges)
      !
      !
      !     update information
      !     person                    date
      !
      !     Dhondia                   01-08-2001
      !
      !
      !
      !     coefficients for momentum equation in wet bridge point
      !

      implicit none
      !
      ! Global variables
      !
      type(t_bridge), pointer, intent(in)       :: bridge
      double precision                          :: glThick
      integer, intent(inout)                    :: kfum
      double precision, intent(out)             :: aum
      double precision, intent(out)             :: convm
      double precision, intent(out)             :: dadsm
      double precision, intent(out)             :: wtm
      double precision, intent(out)             :: wtgm
      double precision, intent(out)             :: cmustr
      double precision, intent(out)             :: fum
      double precision, intent(inout)           :: q0m
      double precision, intent(out)             :: qm
      double precision, intent(out)             :: rum
      double precision, intent(in)              :: u0m
      double precision, intent(inout)           :: u1m
      double precision, intent(in)              :: s1m2
      double precision, intent(in)              :: s1m1
      double precision, intent(in)              :: dxm
      double precision, intent(in)              :: dt
      double precision, intent(in)              :: wetup
      double precision, intent(in)              :: widthup
      double precision, intent(in)              :: wperimup
      double precision, intent(in)              :: convup
      double precision, intent(in)              :: bobup
      double precision, intent(in)              :: wetdown
      logical, intent(in)                       :: infuru
      !
      !
      ! Local variables
      !
      integer                                   :: dir
      integer                                   :: allowedFlowDir
      
      double precision                          :: cmus
      double precision                          :: smax
      double precision                          :: smin
      double precision                          :: gl_thickness
      double precision                          :: crestLevel
      double precision                          :: depth
      double precision                          :: chezyBridge
      double precision                          :: wArea
      double precision                          :: wPerimiter
      double precision                          :: wWidth
      double precision                          :: hydrRadius
      double precision                          :: dummy
      double precision                          :: frictloss
      double precision                          :: exitLoss
      double precision                          :: totalLoss
      double precision                          :: cu
      double precision                          :: fr
      double precision                          :: bu
      double precision                          :: du

      ! Initializing at declaration is not enough....
      cmus         = 1.0d0
      gl_thickness = 0.0d0
      chezyBridge  = 0.0d0
      wArea        = 0.0d0
      wPerimiter   = 0.0d0
      wWidth       = 0.0d0
      hydrRadius   = 0.0d0
      dummy        = 0.0d0
      frictloss    = 0.0d0
      exitLoss     = 0.0d0
      totalLoss    = 0.0d0
      cu           = 0.0d0
      fr           = 0.0d0
      bu           = 0.0d0
      du           = 0.0d0

      ! Initialize with flow
      kfum = 1
      
      ! Find the flow direction
      if (s1m1 > s1m2) then
         smax = s1m1
         smin = s1m2
         dir  = 1
      else
         smax = s1m2
         smin = s1m1
         dir  = -1
      endif

      allowedFlowDir = bridge%allowedflowdir
      if ((allowedFlowDir == 3) .or. &
          (dir == 1  .and. allowedFlowDir == 2) .or. &
          (dir == -1 .and. allowedFlowDir == 1)) then
         kfum = 0
         if (infuru) then
            fum = 0.0d0
            rum = 0.0d0
         endif
         !        same as weir
         u1m = 0.0d0
         qm  = 0.0d0
         q0m = 0.0d0
         return
      endif
      
      if (.not. bridge%useOwnCrossSection) then

         ! Pillar Bridge; wetted profile at upstream side
         convm = convup
         aum   = wetup
         dadsm = widthup
         wtm   = wperimup

         if (bridge%pillarwidth > 1.0d-5) then

            depth = smax - bobup  ! Already corrected for Ground Layer and positive
      
            if (glThick > 1.0d-5) then
               wtgm = wtgm - bridge%pillarwidth
               if (wtgm <= 0.0d0) kfum = 0
            elseif (wtm > 1.0d-5) then
               wtm = wtm - bridge%pillarwidth
               if (wtm <= 0.0d0) kfum = 0
            endif
         
            aum = aum - bridge%pillarwidth * depth
            if (aum <= 0.0d0) kfum = 0

            if (dadsm > 1.0d-5) then
               dadsm = dadsm - bridge%pillarwidth   !hk: Only true if pillar length equals link length
               if (dadsm <= 0.0d0) kfum = 0
            endif
            
            if (kfum == 0) then
               if (infuru) then
                  fum = 0.0
                  rum = 0.0
               endif
               !        same as weir
               u1m = 0.0d0
               qm  = 0.0d0
               q0m = 0.0d0
               return
            endif

            ! Upstream wetted area - wetted area under the bridge would give wetted area for pillars
            if ((wetup - aum) > 0.0d0) then
               cmus = cmus / dsqrt(bridge%formfactor * (wetup - aum) / wetup)
            endif
       
         endif
      
      else

         ! Abutment Bridge 33, Fixed Bed Bridge 34 and
         ! Soil Bed Bridge 35
         
         ! Check on drying with dead band and groundlayer
         gl_thickness = getGroundLayer(bridge%pcross)
      
         crestLevel = bridge%bedlevel

         if ((smax - crestLevel - gl_thickness) < thresholdDry) then
            kfum = 0
         elseif ((smax - crestLevel - gl_thickness) > thresholdFlood) then
            kfum = 1
         endif
         if (kfum == 0) then
            if (infuru) then
               fum = 0.0
               rum = 0.0
            endif
            !        same as weir
            u1m = 0.0d0
            qm  = 0.0d0
            q0m = 0.0d0
            return
         endif

         ! Initialize = bridge%pcross
         depth = smax - crestLevel
         call GetCSParsFlow(bridge%pcross, depth, u0m, chezyBridge, wArea, wPerimiter, wWidth, dummy)     

         ! Friction Loss
         hydrRadius = wArea / wPerimiter
         frictLoss = 2.0d0 * gravity * bridge%length / (chezyBridge * chezyBridge * hydrRadius)

         ! Exit Loss
         exitLoss = bridge% outletlosscoeff * ((max((1.0d0 - wArea / wetdown), 0.0d0))**2)
         exitLoss = max(exitLoss, 0.0d0)
         
         totalLoss = bridge%inletlosscoeff + frictLoss + exitLoss
         totalLoss = max(totalLoss, 0.01d0)
         
         cmus = 1.0d0 / sqrt(totalLoss)
         cmus = min(cmus, 1.0d0)    ! Limit to maximum of 1.0

         aum   = wArea
         dadsm = wWidth

      endif

      cmustr = cmus

      if (infuru) then
      
         cu = cmustr * cmustr * 2  *gravity / dxm
         fr = abs(u0m) / dxm
         bu = 1.0d0 / dt + fr
         du = u0m / dt
         fum = cu / bu
         rum = du / bu
      endif
      
   end subroutine ComputeBridge
      
end module m_Bridge
