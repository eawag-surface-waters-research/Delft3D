module m_struc_helper
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

   implicit none

   private
   
   public UpAndDownstreamParameters
   public furu_iter

contains

   subroutine UpAndDownstreamParameters(s1ml, s1mr, alm, arm, qtotal, velheight, &
                                        rholeft, rhoright, crest, hu, hd, uu, ud, flowdir)
      !!--declarations----------------------------------------------------------------
      use m_GlobalParameters
      implicit none
      !
      ! Global variables
      !
      logical, intent(in)            :: velheight
      double precision, intent(in)   :: s1ml
      double precision, intent(in)   :: s1mr
      double precision, intent(in)   :: alm
      double precision, intent(in)   :: arm
      double precision, intent(in)   :: qtotal
      double precision, intent(in)   :: crest
      double precision               :: hd
      double precision               :: hu
      double precision               :: rholeft
      double precision               :: rhoright
      double precision               :: flowdir
      double precision               :: ud
      double precision               :: uu
      !
      !
      ! Local variables
      !
      double precision               :: eld
      double precision               :: elu
       double precision               :: temp
      !
      !
      !! executable statements -------------------------------------------------------
      !
      rholeft = 1000.0D0
      rhoright = 1000.0D0
      !
      !if (relax>0.D0) then
      !   hu = s1ml * relax + (1.0D0 - relax) * s2ml
      !   hd = s1mr * relax + (1.0D0 - relax) * s2mr
      !endif
      hu = s1ml
      hd = s1mr
      !
      if (velheight) then
         if (alm < 1.0D-6) then
            uu = 0.D0
         else
            uu = qtotal / alm
         endif
         if (arm < 1.0D-6) then
            ud = 0.0D0
         else
            ud = qtotal / arm
         endif
      else
         !        Velocity head will be neglected so make velicities zero
         uu = 0.0D0
         ud = 0.0D0
      endif
      !
      !     Calculate discharge ratio of the last 2 successive iteration steps
      !
      if (hu>crest) then
         elu = hu + (uu * uu) / (2.0d0 * gravity)
      else
         elu = hu
      endif
      if (hd>crest) then
         eld = hd + (ud * ud) / (2.0d0 * gravity)
      else
         eld = hd
      endif

      elu = (elu - crest) * rholeft
      eld = (eld - crest) * rhoright
      !
      if (elu>=eld) then
         flowdir = 1.0d0
      else
         flowdir = -1.0d0
      endif
      !
      !     Water levels & Velocities for reverse flow
      !
      if (flowdir < 0.0d0) then
         !
         temp = hu
         hu = hd
         hd = temp
         !
         temp = uu
         uu = ud
         ud = temp
      endif
       
   end subroutine UpAndDownstreamParameters
          
   subroutine furu_iter(fum, rum, s1m2, s1m1, u1m, qL, aum, fr, cu, rhsc, dxdt, dx_struc, hu, lambda, Cz)
      !!--description-----------------------------------------------------------------
      ! NONE
      !!--pseudo code and references--------------------------------------------------
      ! NONE
      !!--declarations----------------------------------------------------------------
      !=======================================================================
      !                       Deltares
      !                One-Two Dimensional Modelling System
      !                           S O B E K
      !
      ! Subsystem:          Flow Module
      !
      ! Programmer:         J. Noort
      !
      ! Module:             furu_iter (FURU_ITER)
      !
      ! Module description: coefficients for momentum equation in wet weir point
      !
      !
      !     update information
      !     person                    date
      !
      !
      !
      !     Include Pluvius data space
      !
      use m_GlobalParameters

      implicit none
      !
      ! Global variables
      !
      !
      double precision, intent(out)    :: fum
      double precision, intent(out)    :: rum
      double precision, intent(in)     :: fr
      double precision, intent(in)     :: cu
      double precision, intent(in)     :: rhsc
      double precision, intent(in)     :: s1m2
      double precision, intent(in)     :: s1m1
      double precision, intent(in)     :: qL
      double precision, intent(in)     :: aum
      double precision, intent(inout)  :: u1m
      double precision, intent(in)     :: dxdt
      !
      ! Local variables
      !
      !
      double precision                 :: bu
      double precision                 :: du
      double precision                 :: dxfrL

      !
      !! executable statements -------------------------------------------------------
      !
      dxfrL = 0d0
      if (lambda ==0d0 .and. Cz > 0.1d0) then
         dxfrl = dx_struc*gravity/(Cz*Cz*hu)
      endif
      
      bu   = dxdt + (1+dxfrL) * fr
      du   = (strucalfa  * qL / max(aum, 1.0d-4) + (1 - strucalfa) * u1m) * dxdt + rhsc
      fum  = cu / bu
      rum  = du / bu
      u1m  = rum - fum * (s1m2 - s1m1)
      
   end subroutine furu_iter

end module m_struc_helper
