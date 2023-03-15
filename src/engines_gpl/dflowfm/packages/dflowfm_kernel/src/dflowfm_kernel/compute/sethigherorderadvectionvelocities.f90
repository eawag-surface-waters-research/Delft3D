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

   ! $Id: sethigherorderadvectionvelocities.f90 142549 2023-02-16 12:28:37Z buwalda $
   ! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/trunk/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/sethigherorderadvectionvelocities.f90 $
   
   Subroutine sethigherorderadvectionvelocities()
   use m_flowgeom
   use m_flow
   use m_sferic
   use m_flowtimes
   use unstruc_messages
   use m_nod2lin
   use m_limiters, only: dslim
   use m_set_HO_advec_velocity

   implicit none

   integer  L, LL , Lb, Lt

   if (limtypmom < 1 ) return

   if (kmx == 0) then
      if (jasfer3D == 0) then
         !$OMP PARALLEL DO
         do L  = 1,lnx                                                    ! upwind (supq) + limited high order (dsq)
            if (qa (L) > 0) then
               call setvelocity_pos(ucxu(L), ucyu(L), L, L, L)
            endif
            if (qa (L) < 0) then
               call setvelocity_neg(ucxu(L), ucyu(L), L, L, L)
            endif
         enddo 
         !$OMP END PARALLEL DO
      else
         !$OMP PARALLEL DO
         do L  = 1,lnx                                                    ! upwind (supq) + limited high order (dsq)
            if (qa (L) > 0) then
               call setvelocity_pos_Jasfer3D(ucxu(L), ucyu(L), L, L, L)
            endif
            if (qa (L) < 0) then
               call setvelocity_neg_Jasfer3D(ucxu(L), ucyu(L), L, L, L)
            endif
         enddo
         !$OMP END PARALLEL DO
      endif
   else
      if (jasfer3D == 0) then
         !$OMP PARALLEL DO PRIVATE(LL, Lb, Lt)
         do L  = 1,lnx                                                    ! upwind (supq) + limited high order (dsq)
            if (qa(L) .ne. 0d0) then
               call getLbotLtop(L,Lb,Lt)
               do LL = Lb,Lt
                  if (qa (LL) > 0) then
                     call setvelocity_pos(ucxu(LL), ucyu(LL), L, LL, Lb)
                  else
                     call setvelocity_neg(ucxu(LL), ucyu(LL), L, LL, Lb)
                  endif
               enddo
            endif
         enddo  ! horizontal
         !$OMP END PARALLEL DO
      else
         !$OMP PARALLEL DO PRIVATE(LL, Lb, Lt)
         do L  = 1,lnx                                                    ! upwind (supq) + limited high order (dsq)
            if (qa(L) .ne. 0d0) then
               call getLbotLtop(L,Lb,Lt)
               do LL = Lb,Lt
                  if (qa (LL) > 0) then
                     call setvelocity_pos_Jasfer3D(ucxu(LL), ucyu(LL), L, LL, Lb)
                  else
                     call setvelocity_neg_Jasfer3D(ucxu(LL), ucyu(LL), L, LL, Lb)
                  endif
               enddo
            endif
         enddo  ! horizontal
         !$OMP END PARALLEL DO
      endif
   endif

   end subroutine sethigherorderadvectionvelocities
