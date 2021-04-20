!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2021.                                
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

   subroutine wave_comp_stokes_velocities()
   use m_flowparameters
   use m_flowgeom
   use m_flow, only: hu, huvli, hs
   use m_physcoef, only: sag
   use m_waves
   use m_partitioninfo
   implicit none

   double precision :: Mu, Mv, hminlwi, massflux_max, mnorm, mangle          ! link-based and link-oriented wave-induced volume fluxes
   double precision, allocatable :: mx(:), my(:)

   integer :: k1, k2, L, k
   integer :: ierror ! error (1) or not (0)

   ierror = 1

   if (.not.(allocated(mx))) then
      allocate(mx(1:ndx), my(1:ndx), stat=ierror)
   end if

   ! hminlwi = 1d0/hminlw
   ustokes = 0d0
   vstokes = 0d0
   mx      = 0d0
   my      = 0d0

   do k = 1,ndx
      massflux_max = 1d0/8d0*sag*(max(hs(k),0d0)**1.5)*gammax**2
      mnorm  = min(sqrt(mxwav(k)**2+mywav(k)**2), massflux_max)
      mangle = atan2(mywav(k), mxwav(k))
      mx(k)  = mnorm*dcos(mangle)
      my(k)  = mnorm*dsin(mangle)
   end do

   if (jampi>0) then
      call update_ghosts(ITYPE_SALL, 1, ndx, mx, ierror)
      call update_ghosts(ITYPE_SALL, 1, ndx, my, ierror)
   endif

   do L=1,Lnxi
      if ( hu(L).gt.epshu ) then
         k1 = ln(1,L); k2 = ln(2,L)

         Mu =    acL(L) *(csu(L)*(Mx(k1)) + snu(L)*(My(k1))) + &
            (1d0-acL(L))*(csu(L)*(Mx(k2)) + snu(L)*(My(k2)))

         Mv =    acL(L) *(-snu(L)*(Mx(k1)) + csu(L)*(My(k1))) + &
            (1d0-acL(L))*(-snu(L)*(Mx(k2)) + csu(L)*(My(k2)))

         ustokes(L) = Mu * huvli(L)
         vstokes(L) = Mv * huvli(L)
      else
         ustokes(L) = 0d0
         vstokes(L) = 0d0
      end if
   end do

   do L=lnxi+1,lnx                   ! Randen: Neumann
      if (hu(L)>epshu)  then
         k1 = ln(1,L) ! buiten
         k2 = ln(2,L) ! binnen
         Mx(k1) = Mx(k2);  My(k1) = My(k2)

         Mu =    acL(L) *(csu(L)*(Mx(k1)) + snu(L)*(My(k1))) + &
            (1d0-acL(L))*(csu(L)*(Mx(k2)) + snu(L)*(My(k2)))

         Mv =    acL(L) *(-snu(L)*(Mx(k1)) + csu(L)*(My(k1))) + &
            (1d0-acL(L))*(-snu(L)*(Mx(k2)) + csu(L)*(My(k2)))

         ustokes(L) = Mu * huvli(L)
         vstokes(L) = Mv * huvli(L)
      else
         ustokes(L) = 0d0
         vstokes(L) = 0d0
      end if
   end do

   if (jampi>0) then
      call update_ghosts(ITYPE_U, 1, lnx, ustokes, ierror)
      call update_ghosts(ITYPE_U, 1, lnx, vstokes, ierror)
   !   call update_ghostboundvals(ITYPE_U, 1, lnx, ustokes, 0, ierror)
   !   call update_ghostboundvals(ITYPE_U, 1, lnx, vstokes, 0, ierror)
   endif

   ierror = 0
1234 continue
   return
   end subroutine wave_comp_stokes_velocities
