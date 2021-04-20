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

! JRE TODO: ustokes values used here are in 3D from previous timestep (updateverticalprofile)
! Potential solution is to separate this in subroutine to use here and in updatevp. You will lose point operation speed
   subroutine setwavmubnd()
   use m_flowgeom
   use m_flowparameters
   use m_flowexternalforcings
   use m_flow, only: hu, huvli, wavmubnd, kmx
   use m_waves
   implicit none

   double precision :: ac1, ac2

   integer          :: kb, ki, L, n, LL, Lb, Lt
   double precision :: hminlwi

   hminlwi = 1d0/hminlw

   !  wavmubnd is defined on the whole mesh, but has non-zero values at the open boundaries only
   wavmubnd = 0d0

   do n=1,nbndu
      kb = kbndu(1,n)
      ki = kbndu(2,n)
      L  = kbndu(3,n)
      ! interpolate cell-centered mass fluxes to flow links
      if (hu(L) < epshu) cycle
      if (kmx==0) then
         ac1 = acl(L)
         ac2 = 1d0-ac1
         wavmubnd(L) = (ac1*mxwav(kb) + ac2*mxwav(ki)) * csu(L) + &
                       (ac1*mywav(kb) + ac2*mywav(ki)) * snu(L)

         wavmubnd(L) = wavmubnd(L) * min(huvli(L),hminlwi)
      else
         call getLbotLtop(L,Lb,Lt)
         do LL=Lb,Lt
            wavmubnd(LL) = ustokes(LL)
            wavmubnd(L)  = wavmubnd(L) + ustokes(LL)*(hu(LL)-hu(LL-1))  ! needed for check below
         enddo
         wavmubnd(L) = wavmubnd(L)/max(hu(L),epshu)
      endif
   end do

   do n=1,nbndz
      if ( kbndz(4,n).eq.5 ) then   ! riemann boundaries
         kb = kbndz(1,n)
         ki = kbndz(2,n)
         L  = kbndz(3,n)
         if (hu(L) < epshu) cycle
         if ( wavmubnd(L).ne.0d0 ) cycle
         if (kmx==0) then
            ! interpolate cell-centered mass fluxes to flow links
            ac1 = acl(L)
            ac2 = 1d0-ac1
            wavmubnd(L) = (ac1*mxwav(kb) + ac2*mxwav(ki)) * csu(L) + &
               (ac1*mywav(kb) + ac2*mywav(ki)) * snu(L)

            wavmubnd(L) = wavmubnd(L) * min(huvli(L),hminlwi)
         else
            call getLbotLtop(L,Lb,Lt)
            do LL=Lb,Lt
               wavmubnd(LL) = ustokes(LL)
               wavmubnd(L)  = wavmubnd(L) + ustokes(LL)*(hu(LL)-hu(LL-1))
            enddo
            wavmubnd(L) = wavmubnd(L)/max(hu(L),epshu)
         endif
      end if
   end do

   !  normal-velocity boundaries
   do n=1,nbndn
      kb = kbndn(1,n)
      ki = kbndn(2,n)
      L  = kbndn(3,n)
      if (hu(L) < epshu) cycle
      if ( wavmubnd(L).ne.0d0 ) cycle
      if (kmx==0) then
         ! interpolate cell-centered mass fluxes to flow links
         ac1 = acl(L)
         ac2 = 1d0-ac1
         wavmubnd(L) = (ac1*mxwav(kb) + ac2*mxwav(ki)) * csu(L) + &
            (ac1*mywav(kb) + ac2*mywav(ki)) * snu(L)

         wavmubnd(L) = wavmubnd(L) * min(huvli(L),hminlwi)
      else
         call getLbotLtop(L,Lb,Lt)
         do LL=Lb,Lt
            wavmubnd(LL) = ustokes(LL)
            wavmubnd(L)  = wavmubnd(L) + ustokes(LL)*(hu(LL)-hu(LL-1))
         enddo
         wavmubnd(L) = wavmubnd(L)/max(hu(L),epshu)
      endif
   end do

   !  tangential-velocity boundaries: not needed to define mass fluxes

   return
   end subroutine setwavmubnd
