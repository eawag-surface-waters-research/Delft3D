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

   !> Determine up and downstream parameters, depending on flow direction for structures
   subroutine UpAndDownstreamParameters(s1ml, s1mr, alm, arm, qtotal, velheight, &
                                        rholeft, rhoright, crest, hu, hd, uu, ud, flowdir)
      !!--declarations----------------------------------------------------------------
      use m_GlobalParameters
      implicit none
      !
      ! Global variables
      !
      logical, intent(in)             :: velheight     !< Indicates whether the velocity height is taken into account or if the water level is used
      double precision, intent(in)    :: s1ml          !< Water level at geometrical left side
      double precision, intent(in)    :: s1mr          !< Water level at geometrical right side
      double precision, intent(in)    :: alm           !< FLow area at geometrical left side
      double precision, intent(in)    :: arm           !< FLow area at geometrical right side
      double precision, intent(in)    :: qtotal        !< Total discharge through flow link (in case of compound structures this might be larger than the
                                                       !< discharge through the actual structure
      double precision, intent(in)    :: crest         !< crest level
      double precision, intent(  out) :: hd            !< downstream water level
      double precision, intent(  out) :: hu            !< upstream water level
      double precision, intent(  out) :: rholeft       !< water density at left side of structure
      double precision, intent(  out) :: rhoright      !< water density at right side of structure
      double precision, intent(  out) :: flowdir       !< flow direction 1 positive direction, -1 negative direction 
      double precision, intent(  out) :: ud            !< downstream velocity
      double precision, intent(  out) :: uu            !< upstream velocity
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
   
   !> Calculate FU and RU
   subroutine furu_iter(fuL, ruL, s1k2, s1k1, u1L, qL, auL, fr, cu, rhsc, dxdt, dx_struc, hu, lambda, Cz)
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
      double precision, intent(out)             :: fuL      !< Fu component of momentum equation
      double precision, intent(out)             :: ruL      !< Right hand side component of momentum equation
      double precision, intent(in)              :: fr       !< Structure velocity (u_s)
      double precision, intent(in)              :: cu       !< coefficient for calculating fuL = cu/bu
      double precision, intent(in)              :: rhsc     !< right hand side term in structure equation
      double precision, intent(in)              :: s1k2     !< water level s1(k2)
      double precision, intent(in)              :: s1k1     !< water level s1(k1)
      double precision, intent(in)              :: qL       !< discharge through structure
      double precision, intent(in)              :: auL      !< Flow area of structure
      double precision, intent(inout)           :: u1L      !< Flow velocity through structure
      double precision, intent(in)              :: dxdt     !< dx/dt
      double precision, intent(in), optional    :: Cz       !< Chezy value
      double precision, intent(in), optional    :: lambda   !< extra resistance
      double precision, intent(in), optional    :: hu       !< upstream water level
      double precision, intent(in), optional    :: dx_struc !< crest length 
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
      if (present(dx_struc)) then
         if (lambda ==0d0 .and. Cz > 0d0) then
            dxfrl = dx_struc*gravity/(Cz*Cz*hu)
         endif
      endif
      
      bu   = dxdt + (1+dxfrL) * fr
      du   = (strucalfa  * qL / max(auL, 1.0d-4) + (1 - strucalfa) * u1L) * dxdt + rhsc
      fuL  = cu / bu
      ruL  = du / bu
      u1L  = ruL - fuL * (s1k2 - s1k1)
      
   end subroutine furu_iter

end module m_struc_helper
