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

   !> subroutine to compute wave forces from SWAN output
   subroutine setwavfu()
   use unstruc_messages
   use MessageHandling
   use m_flowparameters
   use m_flowgeom
   use m_flow, only: hu, huvli, wavfu, wavfv, rhomean,kmx,rho
   use m_waves
   use m_physcoef, only: sag
   implicit none

   integer          :: mout
   integer          :: L,LL,Lb,Lt
   double precision :: wavfx, wavfy, wavfbx, wavfby
   double precision :: wavfu_loc, wavfbu_loc, twavL, hwavL, wavfuL, cc
   double precision :: wavfv_loc, wavfbv_loc, wavfmag, wavfbmag,wavfang, wavfbang
   double precision :: fmax, ac1, ac2, hminlwi, rhoL, zw

   integer          :: k1, k2

   hminlwi = 1d0/hminlw
   wavfu = 0d0

   if (kmx==0) then
      do L = 1,lnx
         if (hu(L) < epshu) cycle
         if (L > lnx1D) then
            k1 = ln(1,L) ; k2 = ln(2,L)
            ac1 = acl(L)
            ac2 = 1d0-ac1

            wavfx = ac1*sxwav(k1) + ac2*sxwav(k2)
            wavfy = ac1*sywav(k1) + ac2*sywav(k2)

            wavfbx = ac1*sbxwav(k1) + ac2*sbxwav(k2)
            wavfby = ac1*sbywav(k1) + ac2*sbywav(k2)

            twavL = ac1*twav(k1)   + ac2*twav(k2)

            ! projection in face-normal direction
            wavfu_loc  = wavfx*csu(L)  + wavfy*snu(L)
            wavfv_loc  = -wavfx*snu(L)  + wavfy*csu(L)
            wavfbu_loc = wavfbx*csu(L) + wavfby*snu(L)
            wavfbv_loc = -wavfbx*snu(L) + wavfby*csu(L)

            ! limit forces
            fmax       = facmax*hu(L)**1.5 / max(0.1d0, twavL)

            ! Should be done on the vector norm, nt separate comps
            wavfmag = min(sqrt(wavfu_loc*wavfu_loc + wavfv_loc*wavfv_loc),fmax)
            wavfbmag = min(sqrt(wavfbu_loc*wavfbu_loc + wavfbv_loc*wavfbv_loc),fmax)
            wavfang  = atan2(wavfv_loc,wavfu_loc)
            wavfbang  = atan2(wavfbv_loc,wavfbu_loc)    ! necessary?
            wavfu_loc = wavfmag*cos(wavfang)
            wavfv_loc = wavfmag*sin(wavfang)
            wavfbu_loc = wavfbmag*cos(wavfbang)
            wavfbv_loc = wavfbmag*sin(wavfbang)

            wavfu(L) = wavfu_loc + wavfbu_loc
            wavfv(L) = wavfv_loc + wavfbv_loc
         endif
         wavfu(L) = wavfu(L) * min(huvli(L), hminlwi) / rhomean       ! Dimensions [m/s^2]
         wavfv(L) = wavfv(L) * min(huvli(L), hminlwi) / rhomean       ! Dimensions [m/s^2]
      enddo
   else  ! kmx>0
      do LL=1, lnx
         if (hu(LL)<epshu) cycle
         call getLbotLtop(LL, Lb, Lt)
         if (Lt<Lb) cycle
         k1 = ln(1,LL); k2 = ln(2,LL)
         ac1 = acL(LL); ac2 = 1d0-ac1
         !
         hwavL = ac1*hwav(k1) + ac2*hwav(k2)
         twavL = ac1*twav(k1) + ac2*twav(k2)
         fmax  = facmax*hu(LL)**1.5 / max(0.1d0, twavL)
         rhoL  = ac1*rho(ln(1,Lt)) + ac2*rho(ln(2,Lt))
         !
         ! Surface force, linear decrease over 0.5*hrms
         !
         wavfx     = ac1*sxwav(k1) + ac2*sxwav(k2)
         wavfy     = ac1*sywav(k1) + ac2*sywav(k2)
         wavfu_loc = csu(LL)*wavfx + snu(LL)*wavfy
         wavfuL    = 4d0*sign(min(abs(wavfu_loc), fmax), wavfu_loc)/max(hwavL,gammax*hminlw) ! to check
         zw=0.5*(hu(Lt)-hu(Lt-1))    ! center top layer
         do L=Lt,Lb+1,-1
            if (zw<=0.5*hwavL) then
               wavfu(L) = wavfuL*(1d0-2d0*zw/hwavL)/rhoL/hu(LL)*(hu(L)-hu(L-1))/hu(LL)
               zw = zw + 0.5*(hu(L)-hu(L-1)) + 0.5*(hu(L-1)-hu(L-2))
            elseif (zw>0.5*hwavL .and. hu(L)>(hu(LL)-0.5*hwavL)) then                      ! partial layer
               cc = 0.5*(hu(L)+(hu(LL)-0.5*hwavL))                                         ! replaced layer center
               wavfu(L) = wavfuL*(1d0-2d0*zw/hwavL)/rhoL/hu(LL)*(hu(L)-cc)/hu(LL)          ! contribution over partial layer
            endif
         enddo
         !
         ! Body forces, uniform over depth
         !
         wavfx     = ac1*sbxwav(k1) + ac2*sbxwav(k2)
         wavfy     = ac1*sbywav(k1) + ac2*sbywav(k2)
         wavfu_loc = csu(LL)*wavfx + snu(LL)*wavfy
         do L=Lb,Lt
            rhoL  = ac1*rho(ln(1,L)) + ac2*rho(ln(2,L))
            wavfu(L) = wavfu(L) + sign(min(abs(wavfu_loc), fmax),wavfu_loc)/rhoL/hu(LL)/hu(LL)*(hu(L)-hu(L-1))   ! integral equals wavfu_loc/rho/hu
         enddo
      enddo
   endif

   end subroutine setwavfu
