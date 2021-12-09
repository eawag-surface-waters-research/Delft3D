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

   subroutine wave_makeplotvars
   use m_waves, only: ustokes, vstokes, ust_mag, fwav_mag, taubxu, taux_cc, tauy_cc
   use m_xbeach_data, only: Fx_cc, Fy_cc
   use m_flowparameters, only: jawave
   use m_flow
   use m_flowgeom

   implicit none

   integer           :: ierror
   integer           :: L, LL, Lb, Lt, k1, k2
   double precision :: ust_mag_u

   ust_mag=0d0
   fwav_mag=0d0
   taux_cc=0d0
   tauy_cc=0d0

   do L = 1, lnx   ! safe for 3D
      k1=ln(1,L);k2=ln(2,L)
      taux_cc(k1) = taux_cc(k1)+wcx1(L)*taubxu(L)
      taux_cc(k2) = taux_cc(k2)+wcx2(L)*taubxu(L)
      tauy_cc(k1) = tauy_cc(k1)+wcy1(L)*taubxu(L)
      tauy_cc(k2) = tauy_cc(k2)+wcy2(L)*taubxu(L)
      call getLbotLtop(L, Lb, Lt)
      do LL = Lb,Lt
         k1=ln(1,LL);k2=ln(2,LL)
         ust_mag_u = sqrt(ustokes(LL)*ustokes(LL) + vstokes(LL)*vstokes(LL))
         ust_mag(k1) = ust_mag(k1)+wcl(1,L)*ust_mag_u
         ust_mag(k2) = ust_mag(k2)+wcl(2,L)*ust_mag_u
      end do
   end do

   if (jawave==3 .or. jawave==6) then
      do L=1,lnx
         k1=ln(1,L);k2=ln(2,L)
         fwav_mag(k1) = fwav_mag(k1)+wcl(1,L)*wavfu(L)*rhomean*hu(L)
         fwav_mag(k2) = fwav_mag(k2)+wcl(2,L)*wavfu(L)*rhomean*hu(L)
      enddo
   end if

   if (jawave==4) then
      fwav_mag = sqrt(Fx_cc*Fx_cc + Fy_cc*Fy_cc)
   endif
   ierror = 0
1234 continue
   return

   end subroutine wave_makeplotvars
