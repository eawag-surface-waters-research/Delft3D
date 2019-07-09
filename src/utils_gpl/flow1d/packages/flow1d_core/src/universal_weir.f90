module m_Universal_Weir
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
   
   use m_GlobalParameters
   use m_struc_helper
   use MessageHandling
   use m_CrossSections
   
   implicit none
 
   public realloc
   public dealloc
   public ComputeUniversalWeir

   type, public :: t_uni_weir
      integer                                      :: yzcount
      double precision, allocatable, dimension(:)  :: y
      double precision, allocatable, dimension(:)  :: z
      double precision                             :: crestlevel
      double precision                             :: dischargecoeff
      integer                                      :: allowedflowdir
      double precision                             :: freesubmergedfactor
   end type

   interface dealloc
      module procedure deallocUniWeir
   end interface dealloc

   private

   contains
 
   subroutine deallocUniWeir(uniweir)
      ! Modules

      implicit none
      
      ! Input/output parameters
      type(t_uni_weir), pointer   :: uniweir

      ! Local variables

      ! Program code
      if (associated(uniweir) ) then
         if (allocated(uniweir%y))        deallocate(uniweir%y)
         if (allocated(uniweir%z))        deallocate(uniweir%z)
         deallocate(uniweir)
      endif
      
      uniweir => null()
      
   end subroutine deallocUniWeir
   
   subroutine ComputeUniversalWeir(uniweir, fum, rum, aum, dadsm, kfum, s1m1, s1m2, &
                                   qm, q0m, u1m, u0m, dxm, dt, infuru)
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
      ! Programmer:         P.J. van Overloop
      !
      ! Module:             weiruni (WEIR UNIversal)
      !
      ! Module description: coefficients for momentum equation in wet weir
      !                     universal point
      !
      !
      !     update information
      !     person                    date
      !
      !
      ! coefficients for momentum equation in wet weir point
      !
      !     Include Pluvius data space
      !

      implicit none
      !
      ! Global variables
      !
      type(t_uni_weir), pointer, intent(in)          :: uniweir
      integer, intent(out)                           :: kfum
      double precision, intent(out)                  :: aum
      double precision, intent(out)                  :: dadsm
      double precision, intent(out)                  :: fum
      double precision, intent(inout)                :: q0m
      double precision, intent(out)                  :: qm
      double precision, intent(out)                  :: rum
      double precision, intent(in)                   :: u0m
      double precision, intent(inout)                :: u1m
      double precision, intent(in)                   :: s1m2
      double precision, intent(in)                   :: s1m1
      double precision, intent(in)                   :: dxm
      double precision, intent(in)                   :: dt
      logical, intent(in)                            :: infuru
      !
      !
      ! Local variables
      !
      integer                        :: dir
      integer                        :: allowedflowdir
      double precision               :: cmus
      double precision               :: dqdh1
      double precision               :: dqdh2
      double precision               :: dwddavg
      double precision               :: qflow
      double precision               :: qflowmax
      double precision               :: qflowmin
      double precision               :: sinc
      double precision               :: smax
      double precision               :: smax1
      double precision               :: smin
      double precision               :: smin1
      double precision               :: switchfactor
      double precision               :: wetaavg
      !
      !
      !! executable statements -------------------------------------------------------
      !
      !
      !
      !     Find the flow direction
      if (s1m1  > s1m2) then
         smax = s1m1 
         smin = s1m2
         dir  = 1
      else
         smax = s1m2
         smin = s1m1 
         dir  = -1
      endif
      !
      ! ARS 11952 PJvO 20040309
      allowedflowdir = uniweir%allowedflowdir 
      if ((allowedflowdir == 3) .or. (dir == 1 .and. allowedflowdir == 2) .or. (dir == -1 .and. allowedflowdir == 1)) then
         kfum = 0
         fum  = 0.0d0
         rum  = 0.0d0
         u1m  = 0.0d0
         qm   = 0.0d0
         q0m  = 0.0d0
         return
      endif
      
      !
      !     Check on flooding or drying with treshold
      if ((smax - uniweir%crestlevel) < thresholdDry) then
         kfum = 0
      elseif ((smax - uniweir%crestlevel) > thresholdFlood) then
         kfum = 1
      else
      endif
      if (kfum == 0) then
         if (infuru) then
            fum = 0.0
            rum = 0.0
         endif
         !        same as weir
         u1m = 0.0
         qm = 0.0
         q0m = 0.0
         return
      endif
      !
      !     Switchfactor is the transition factor between free and
      !     submerged flow (0.667 for a broad crested weir)
      !     switchfactor is taken as a fixed value according to
      !     description of Bos
      switchfactor = 0.667d0
      !
      !     In subroutine linearizeweiruni the discharge Q of the
      !     universal weir is computed
      !
      call linearizeweiruni(uniweir, switchfactor, smax, smin, cmus, wetaavg, dwddavg, qflow)

      aum = wetaavg
      dadsm = dwddavg
      !
      if (infuru) then
         !
         !        The subroutine linearizeweiruni is called again twice with a
         !        small incremental on the water levels to compute the
         !        derivatives dQ/dh1 and dQ/dh2
         !
         sinc = 1.0d-4
         smax1 = smax + sinc
         call linearizeweiruni(uniweir, switchfactor, smax1, smin, cmus, wetaavg, dwddavg, qflowmax)
         smin1 = smin - sinc
         call linearizeweiruni(uniweir, switchfactor, smax, smin1, cmus, wetaavg, dwddavg, qflowmin)
         if (dir==1) then
            dqdh1 = (qflowmax - qflow)/(sinc)
            dqdh2 = (qflowmin - qflow)/(-sinc)
         else
            dqdh1 = -(qflowmin - qflow)/(-sinc)
            dqdh2 = -(qflowmax - qflow)/(sinc)
            qflow = -qflow
         endif
         !
         call uniweir_furu(s1m1, s1m2, qflow, dqdh1, dqdh2, dxm, dt, aum, fum, rum, u1m, u0m, q0m, qm)
      !
      endif
                          
                          
   end subroutine ComputeUniversalWeir

   subroutine linearizeweiruni(uniweir, switchfactor, smax, smin, cmus, wetaavg, dwddavg, qflow)
      !
      !=======================================================================
      !                       Deltares
      !                One-Two Dimensional Modelling System
      !                           S O B E K
      !
      ! Subsystem:          Flow Module
      !
      ! Programmer:         P.J. van Overloop
      !
      ! Module:             linearizeweiruni (LINEARIZE WEIR UNIversal)
      !
      ! Module description: The discharge Q and the partial derivatives dQ/dh1
      !                     and dQ/dh2 are computed with numerical derivative

      implicit none
      !
      ! Global variables
      !
      type(t_uni_weir), pointer, intent(in)          :: uniweir
      double precision                               :: cmus
      double precision                               :: dwddavg
      double precision                               :: qflow
      double precision, intent(in)                   :: smax
      double precision, intent(in)                   :: smin
      double precision, intent(in)                   :: switchfactor
      double precision                               :: wetaavg
      !
      !
      ! Local variables
      !
      logical                                :: isfreeflow
      double precision                       :: dpt
      double precision                       :: mulfactor
      double precision                       :: crestlevel
      double precision                       :: warea
      double precision                       :: dwdd
      double precision                       :: lowestcrestlevel
      double precision                       :: cmuoriginal
      double precision                       :: h1
      double precision                       :: hcrit
      double precision                       :: vkm
      double precision                       :: dzb
      double precision                       :: qsect

      integer                                :: isect

      !
      !
      !! executable statements -------------------------------------------------------
      !
      cmus    = 0.0
      Wetaavg = 0.0
      dwddavg = 0.0
      qflow   = 0.0

      lowestcrestlevel = uniweir%crestlevel
      cmuoriginal      = uniweir%dischargecoeff

      cmuoriginal=1d0 ! jira 19171

      !     Loop over number of sections
      !     Each section corresponds with the number line parts in the
      !     y-z-profile
      
      do isect = 1, uniweir%yzcount - 1

      !        Crestlevel is the lowest of the z-points in the section

         crestlevel = lowestcrestlevel + min(uniweir%z(isect), uniweir%z(isect + 1))
         !           level onderkant    laagste level isect                           

         if (smax .gt. crestlevel) then

            ! H1 is the upstream water level above the crest of
            ! the subsection
            h1 = smax - crestlevel

            ! VKM is the drowning factor
            vkm = switchfactor

            ! check for rectangular or sloping sections
            ! dzb is the height of the slope
            dzb = dabs(uniweir%z(isect) - uniweir%z(isect + 1))

            if (dzb .gt.1d-5) then

               if (h1 .lt. 1.25d0 * dzb) then
                  vkm = 0.8d0
               else
                  vkm = switchfactor + 1.0d0 / 6.0d0 * dzb / h1
               endif
               
            endif
            
            hcrit = vkm * h1 + crestlevel
            
            ! discharge coefficient for all types of sections
            ! (rectangular and sloping) is taken as 1.0
            mulfactor  = 1.0d0
            
            if (smin .lt. hcrit) then
      
               ! free flow
               dpt = hcrit - lowestcrestlevel
               isfreeflow  = .true.
               
            else
      
               ! submerged flow
               dpt = max(smin - lowestcrestlevel, hcrit - lowestcrestlevel) 
               isfreeflow = .false.
               
            endif

            call wetdimuni(dpt, warea, dwdd, uniweir, isect)

            warea    = warea * uniweir%dischargecoeff  ! cmu correction in au, jira 19171
            wetaavg  = wetaavg +  warea
            dwddavg  = dwddavg + dwdd
            cmus     = cmus + mulfactor * warea

            if (isfreeflow) then
               ! free flow
               qsect = cmuoriginal * mulfactor * warea * dsqrt(2.d0 * gravity * (1 - vkm) * (smax - crestlevel))
            else
               ! submerged flow
               qsect = cmuoriginal * mulfactor * warea * dsqrt(2.d0 * gravity * (smax - smin))
            endif
            qflow = qflow + qsect 
         endif
      enddo
      
      ! get the cmus factor (by diving it by the total area)
      if (wetaavg > 0) then
         cmus = cmuoriginal * cmus / wetaavg
      endif
       
   end subroutine linearizeweiruni

   subroutine uniweir_furu(s1m1, s1m2, qstru, qdh1, qdh2, dxm, dt, aum, fum, rum, u1m, u0m, q0m, qm)
   
      use  m_struc_helper
   
      implicit none
      !
      ! Local parameters
      !

      !
      ! Global variables
      !
      double precision, intent(in)    :: s1m1
      double precision, intent(in)    :: s1m2
      double precision, intent(in)    :: qdh1
      double precision, intent(in)    :: qdh2
      double precision, intent(in)    :: qstru
      double precision, intent(in)    :: dxm
      double precision, intent(in)    :: dt
      double precision, intent(in)    :: aum
      double precision, intent(inout) :: fum
      double precision, intent(inout) :: rum
      double precision, intent(inout) :: u1m
      double precision, intent(in)    :: u0m
      double precision, intent(in)    :: q0m
      double precision, intent(out)   :: qm
      !
      !
      ! Local variables
      !
      double precision               :: cu
      double precision               :: dxdt
      double precision               :: fr
      double precision               :: fuast
      double precision               :: rhsc
      double precision               :: ustru
      !
      !
      !! executable statements -------------------------------------------------------
      !
      !
      !=======================================================================
      !                      Deltares
      !                One-Two Dimensional Modelling System
      !                           S O B E K
      !
      ! Subsystem:          Flow Module
      !
      ! Programmer:         J.Kuipers
      !
      ! Module:             FLTSFURU (FLow Tabulated Structure, calculate FU and RU)
      !
      ! Module description: The linearization coefficients FU and RU are
      !                     calculated for the tabulated structure
      !
      !
      ! Parameters:
      ! NR NAME            IO DESCRIPTION
      !  1 formno           I  Flow condition of general structure:
      !                         0 : closed or dry
      !                         1 : open
      !  3 h1               I  water level at the left  of the structure
      !  4 h2               I  water level at the right of the structure
      !  2 m                I  Grid index of structure
      !  6 qdh1             I  (dQ/dh1) determined for approximation function
      !  7 qdh2             I  (dQ/dh2) determined for approximation function
      !  5 qstru            I  resulting Q value for given h1 and h2
      !=======================================================================
      !     Include Pluvius data space
      !     Declaration of parameters:
      !     Declaration of local variables:
      !

      dxdt = dxm / dt
      
      fuast = (qdh1 - qdh2) * 0.5D0 / aum
      cu = gravity
      ustru = qstru / aum
      rhsc = cu * (s1m1 - s1m2)
      fr = cu / fuast
      rhsc = -rhsc + fr * ustru
      
      call furu_iter(fum, rum, s1m2, s1m1, u1m, q0m, aum, fr, cu, rhsc, dxdt)

      qm = aum * u1m
      
   end subroutine uniweir_furu

   subroutine wetdimuni(dpt, warea, dwdd, uniweir, isect)
   
      !=======================================================================
      !                       Deltares
      !                One-Two Dimensional Modelling System
      !                           S O B E K
      !
      ! Subsystem:          Flow Module
      !
      ! Programmer:         J.J. Noort
      !
      ! Module:             WETDIMUNI (computes wet area and width for given segment of 
      !                                cross section)
      !
      ! Module description: The wetted area, and width of a tabulated yz profile
      !     is computed 
      !
      !
      !     update information
      !     person                    date
      !
      !
      implicit none
      !
      ! Global variables
      !
      type(t_uni_weir), pointer      :: uniweir
      integer, intent(in)            :: isect
      double precision, intent(in)   :: dpt
      double precision, intent(out)  :: warea
      double precision, intent(out)  :: dwdd
      !
      !
      ! Local variables
      !
      integer                        :: j
      double precision               :: v
      double precision               :: y1
      double precision               :: y2
      double precision               :: zlower
      double precision               :: zlvl
      double precision               :: zmax
      double precision               :: zmin
      double precision               :: zupper
      !
      !
      !! executable statements -------------------------------------------------------
      !
      !
      !
      !
      zmin =  1.0D+6
      zmax = -1.0D+6
      do j = 1, uniweir%yzcount
         zmin = min(zmin, uniweir%z(j))
         zmax = max(zmax, uniweir%z(j))
      enddo

      zlvl = zmin + dpt

      y1 = uniweir%y(isect)
      y2 = uniweir%y(isect + 1)
      zlower = uniweir%z(isect)
      zupper = uniweir%z(isect + 1)
      !
      if ((zupper <= zlvl) .and. (zlower <= zlvl)) then

         warea = (y2 - y1) * (zlvl - zlower + zlvl - zupper) / 2.0d0
         dwdd  = (y2 - y1)

      elseif ((zupper <= zlvl) .and. (zlower >= zlvl)) then
      
         v = (zlvl - zupper) / (zlower - zupper)
         warea = v  *(y2 - y1) * (zlvl - zupper) / 2.0d0
         dwdd  = v * (y2 - y1)
         
      elseif ((zupper > zlvl) .and. (zlower < zlvl)) then
      
         v = (zlvl - zlower) / (zupper - zlower)
         warea = v * (y2 - y1) * (zlvl - zlower) / 2.0d0
         dwdd  = v * (y2 - y1)
         
      endif

   end subroutine wetdimuni

   
end module m_Universal_Weir
