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

 subroutine addbarocL(LL,Lb,Lt)
 use m_flowgeom
 use m_flow
 use m_flowtimes

 implicit none
 integer, intent(in) :: LL,Lb,Lt

 integer             :: L, k1, k2, k1t, k2t
 double precision    :: gradpu(kmxx), rhovol(kmxx), gr3, barocL, ft

 do L = Lb,Lt
    k1 = ln(1,L) ; k1t = k1
    k2 = ln(2,L) ; k2t = k2
    if (L == Lt) then
       k1t = ktop(ln(1,LL)) ; k2t = ktop(ln(2,LL))
    endif
    rhovol(L-Lb+1) = 0.5d0*( zws(k1t) - zws(k1-1) + zws(k2t) - zws(k2-1) )*dx(LL)*0.5d0*(rho(k1) + rho(k2)) ! write in rvdn
    gr3            = 0.5d0*( rvdn(k1) + rvdn(k2) )*(zws(k1-1) - zws(k2-1))
    gradpu(L-Lb+1) = grn(k1) - grn(k2) + gr3
    if (L > Lb ) then
       gradpu(L-Lb) = gradpu(L-Lb)     - gr3            ! ceiling of ff# downstairs neighbours
    endif
 enddo

 ! this last piece is identical to addbaroc2, that will be removed at some moment
 if (jabaroctimeint == 3) then                                           ! original AB implementation

    do L = Lb, Lt
       if (rhovol(L-Lb+1) > 0d0) then
           barocl    = ag*gradpu(L-Lb+1)/rhovol(L-Lb+1)                  !
           adve(L)   = adve(L) - 1.5d0*barocl + 0.5d0*dpbdx0(L)
           dpbdx0(L) = barocL
        endif
    enddo

 else if (abs(jabaroctimeint) == 4) then                                 ! AB + better drying flooding

    ft = 0.5d0*dts/dtprev
    do L = Lb, Lt
       if (rhovol(L-Lb+1) > 0d0) then
           barocl  = ag*gradpu(L-Lb+1)/rhovol(L-Lb+1)
           if (dpbdx0(L) .ne. 0d0) then
               adve(L)   = adve(L) - (1d0+ft)*barocl + ft*dpbdx0(L)
           else
               adve(L)   = adve(L) - barocl
           endif
           dpbdx0(L) = barocL
       endif
    enddo

    do L = Lt+1,Lb+kmxL(LL)-1
       dpbdx0(L) = 0d0
    enddo

 else

    do L = Lb, Lt
        if (rhovol(L-Lb+1) > 0d0) then
           barocl  = ag*gradpu(L-Lb+1)/rhovol(L-Lb+1)                     !  Explicit
           adve(L) = adve(L) - barocl
       endif
    enddo

 endif

 end subroutine addbarocL
