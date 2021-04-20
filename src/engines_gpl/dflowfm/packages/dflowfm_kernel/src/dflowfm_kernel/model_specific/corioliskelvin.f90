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

   subroutine corioliskelvin(tim)
    use m_netw
    use m_flowgeom
    use m_flow
    use m_sferic
    use unstruc_display
    implicit none

    integer          :: k, L, k1, k2
    double precision :: tim,s1k, xx, yy, samp, ux, uy, dif, alf, cs, sn, aer, dep, r0, x0, y0, Rossby, rr, sgh

    if (tim == 0d0) then
       call inisferic()
    endif

    dep    = sini-zkuni
    sgh    = sqrt(ag/dep)
    Rossby = sqrt(ag*dep) / fcorio
    r0     = 0.5d0*(xkmax-xkmin)
    x0     = 0.5d0*(xkmax+xkmin)
    y0     = 0.5d0*(ykmax+ykmin)
    samp   = 0.05d0

    call statisticsnewstep()

    do k   = 1,ndx
       yy  = yz(k)  - y0
       xx  = xz(k)  - x0
       rr  = dsqrt(xx*xx + yy*yy)
       cs  = xx/rr
       sn  = yy/rr

       aer = samp*exp((rr-r0)/Rossby)
       s1k = aer*cs

       if (tim == 0) then
          s1(k)  = max( bl(k), s1k) ; s0(k) = s1(k)
          ucx(k)  = -s1k*sgh*sn
          ucy(k)  =  s1k*sgh*cs
       endif

       dif = abs(s1(k) - s1k)
       call statisticsonemorepoint(dif)
    enddo

    if (tim == 0) then
       do L  = 1,Lnx
          k1 = ln(1,L) ; k2 = ln(2,L)
          u1(L) = 0.5d0*(ucx(k1) + ucx(k2))*csu(L) + 0.5d0*(ucy(k1) + ucy(k2))*snu(L)
          u0(L) = u1(L)
       enddo
    endif

    call statisticsfinalise()
    end subroutine corioliskelvin
