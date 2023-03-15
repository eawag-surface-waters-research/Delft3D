!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

! $Id: qucperipiaczekteta.f90 142549 2023-02-16 12:28:37Z buwalda $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/trunk/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/qucperipiaczekteta.f90 $

subroutine QucPeripiaczekteta(n12,L,ai,ae,volu,iad)  ! sum of (Q*uc cell IN centre upwind normal) at side n12 of link L
 use m_flow                                          ! advect the cell center velocities (dimension: m4/s2)
 use m_flowgeom
 use m_flowtimes                                     ! leaving the cell = +
 use m_sferic
 use m_nod2lin, only: lin2nodx, lin2nody, nod2linx, nod2liny
 implicit none
 integer :: n12,L,iad                                ! for link L,
 double precision ai, ae, volu

 ! locals
 integer :: LL, LLL, LLLL                            ! for links LL,
 integer :: k12, kup, ja                             ! relevant node, 1 or 2, L/R
 double precision :: cs, sn, ucin, cfl, tet, ucinx, uciny

 integer :: nn12

 ai = 0d0 ; ae = 0d0
 cs      = csu(L)
 sn      = snu(L)

 k12     = ln(n12,L)
 do LL   = 1, nd(k12)%lnx                            ! loop over all attached links
    LLL  = nd(k12)%ln(LL)
    LLLL = iabs(LLL)

    if ( qa(LLLL) .ne. 0d0) then                     !

       ja = 0
       if (iad == 3) then
          ja = 1                                     ! all in odd schemes
       else if ( LLL*qa(LLLL) > 0d0 ) then
          ja = 1                                     ! incoming only otherwise
       endif

       if (ja == 1) then

          cfl  = abs(qa(LLLL))* dts/volu
          if (nd(k12)%lnx ==3) cfl=1.4d0*cfl
          if (cfl > 0) then
             tet  = max(0d0, 1d0 - 1d0/cfl  )
             if (jasfer3D == 1) then
                nn12 = 1; if ( LLL.gt.0 ) nn12 = 2
                ucinx = lin2nodx(LLLL,nn12,ucxu(LLLL),ucyu(LLLL))
                uciny = lin2nody(LLLL,nn12,ucxu(LLLL),ucyu(LLLL))
                ucin  = nod2linx(L,n12,ucinx,uciny)*cs + nod2liny(L,n12,ucinx,uciny)*sn - u1(L)*(1d0-tet)
             else
                ucin = ucxu(LLLL)*cs + ucyu(LLLL)*sn - u1(L)*(1d0-tet)
             endif

             if (LLL > 0) then                             ! incoming link
                ae = ae - qa(LLLL)*ucin
                ai = ai + qa(LLLL)*tet
             else
                ae = ae + qa(LLLL)*ucin
                ai = ai - qa(LLLL)*tet
             endif
          endif

       endif

    endif

 enddo

 end subroutine Qucperipiaczekteta
