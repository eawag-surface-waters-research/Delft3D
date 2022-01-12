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

!> compute riemann boundary reference state
   subroutine riemann_setmean()
      use m_flow, only: s1, u1
      use m_flowgeom, only: bl
      use m_flowexternalforcings
      use m_flowtimes, only: dts
      use m_physcoef, only: ag
      implicit none

      double precision :: dfac, dfac1
      double precision :: h

      integer          :: n, kb, k2, L, itpbn

      double precision :: Tref

      if ( nbndz.gt.0 ) then
         do n=1,nbndz
            itpbn   = kbndz(4,n)
            Tref    = dble(kbndz(6,n))  ! integer, but can get away with it, nobody uses fractional seconds..
            dfac  = max(min(dts/Tref, 1d0), 0d0)
            dfac1 = 1d0 - dfac

            if ( itpbn.eq.5 ) then
               kb      = kbndz(1,n)
               k2      = kbndz(2,n)
               L       = kbndz(3,n)

               k2 = kbndz(2,n)

               h = s1(k2) - bl(k2)
               zbndz0(n) = dfac1*zbndz0(n) + dfac*(s1(k2) - sqrt(h/ag)*u1(L))
            end if
         end do
      end if

      return
   end subroutine riemann_setmean
