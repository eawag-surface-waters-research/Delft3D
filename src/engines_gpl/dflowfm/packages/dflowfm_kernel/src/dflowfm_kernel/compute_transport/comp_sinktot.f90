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

!subroutine update_constituents_RK3
!   use m_flowgeom,   only: Ndx, Ndxi, Lnxi, Lnx, ln, nd  ! static mesh information
!   use m_flow,       only: Ndkx, Lnkx, u1, q1, au, qw, zws, sq, sqi, vol1, kbot, ktop, Lbot, Ltop,  kmxn, kmxL, kmx, viu, vicwws, plotlin, jalts
!   use m_flowtimes,  only: dts, ja_timestep_auto
!   use m_turbulence, only: sigdifi
!   use m_physcoef,   only: dicoww, vicouv, difmolsal
!   use m_transport
!   use m_flowparameters, only: limtypsa, limtyptm, limtypsed, cffachor, cffacver
!   use m_alloc
!   use m_partitioninfo
!   use m_timer
!   use unstruc_messages
!   implicit none
!
!   integer :: jarhoonly
!
!   integer :: ierror
!
!   integer                                               :: limtyp  !< limiter type (>0), or first-order upwind (0)
!   double precision                                      :: dvoli
!   double precision                                      :: dts_store
!
!   integer                                               :: k, L, j, numconst_store
!   integer                                               :: istep
!   integer                                               :: numstepssync
!
!   double precision, dimension(:,:), allocatable         :: constituents0, constituents1, constituents2
!   double precision, dimension(2)                        :: cffac_store
!
!
!   if ( NUMCONST.eq.0 ) return  ! nothing to do
!
!   ierror = 1
!
!   limtyp = max(limtypsa, limtyptm, limtypsed)
!
!   call fill_constituents()
!
!   call get_dtmax()
!
!   allocate(constituents0(NUMCONST,Ndkx))
!   allocate(constituents1(NUMCONST,Ndkx))
!   allocate(constituents2(NUMCONST,Ndkx))
!
!!  store
!   dts_store = dts
!   cffac_store = (/ cffachor, cffacver /)
!   cffachor = 0
!   cffacver = 0
!
!!  no local time-stepping
!   nsubsteps = 1
!   ndeltasteps = 1
!   numnonglobal = 0
!
!   jaupdate = 1
!
!   fluxhor = 0d0  ! not necessary
!
!   if ( kmx.gt.0 ) then
!      fluxver = 0d0
!   end if
!
!   do istep=1,3
!      sumhorflux = 0d0
!
!      if ( istep.eq.1 ) then
!         constituents0 = constituents
!         dts = dts_store
!      else if ( istep.eq.2 ) then
!         constituents1 = constituents
!         dts = 0.25*dts_store
!      else if ( istep.eq.3 ) then
!         constituents2 = constituents
!         dts = 2d0/3d0*dts_store
!      end if
!
!!     compute horizontal fluxes, explicit part
!      call comp_fluxhor3D(NUMCONST, limtyp, Ndkx, Lnkx, u1, q1, au, sqi, vol1, kbot, Lbot, Ltop,  kmxn, kmxL, constituents, difsedu, sigdifi, viu, vicouv, nsubsteps, jaupdate, jaupdatehorflux, ndeltasteps, fluxhor)
!
!      call starttimer(IDEBUG)
!      call comp_sumhorflux(NUMCONST, kmx, Lnkx, Ndkx, Lbot, Ltop, fluxhor, sumhorflux)
!      call stoptimer(IDEBUG)
!
!      if ( kmx.gt.1 ) then
!         call comp_fluxver(  NUMCONST, limtyp, thetavert, Ndkx, kmx, zws, qw, kbot, ktop, constituents, nsubsteps, jaupdate, ndeltasteps, fluxver)
!      end if
!
!      if ( istep.eq.2 ) then
!         constituents = 0.75*constituents0 + 0.25*constituents1
!      else if ( istep.eq.3 ) then
!         constituents = 1d0/3d0*constituents0 + 2d0/3d0*constituents2
!      end if
!
!      if ( kmx.le.1 ) then
!         call solve_2D(NUMCONST, Ndkx, Lnkx, vol1, kbot, ktop, Lbot, Ltop, sumhorflux, fluxver, const_sour, const_sink, nsubsteps, jaupdate, ndeltasteps, constituents, rhs)
!      else
!         call solve_vertical(NUMCONST, ISED1, ISEDN, limtyp, thetavert, Ndkx, Lnkx, kmx,    &
!                             zws, qw, vol1, kbot, ktop, Lbot, Ltop,                     &
!                             sumhorflux, fluxver, const_sour, const_sink,                   &
!                             difsedw, sigdifi, vicwws, nsubsteps, jaupdate, ndeltasteps, constituents, &
!                             a, b, c, d, e, sol, rhs)
!      end if
!
!
!   end do
!
!   if ( jampi.gt.0 ) then
!      if ( jatimer.eq.1 ) call starttimer(IUPDSALL)
!      if ( kmx.lt.1 ) then ! 2D
!         call update_ghosts(ITYPE_Sall, NUMCONST, Ndx, constituents, ierror)
!      else                 ! 3D
!         call update_ghosts(ITYPE_Sall3D, NUMCONST, Ndkx, constituents, ierror)
!      end if
!      if ( jatimer.eq.1 ) call stoptimer(IUPDSALL)
!   end if
!
!   call extract_constituents()
!
!   ierror = 0
!1234 continue
!
!!  restore
!   dts = dts_store
!   cffachor = cffac_store(1)
!   cffacver = cffac_store(2)
!
!   if ( allocated(constituents0) ) deallocate(constituents0)
!   if ( allocated(constituents1) ) deallocate(constituents1)
!   if ( allocated(constituents2) ) deallocate(constituents2)
!
!   return
!end subroutine update_constituents_RK3
subroutine comp_sinktot()
   use m_transport
   use m_flow, only: vol1, kmx, ndkx
   use m_flowgeom, only: ndx
   use m_flowtimes, only: dts
   use m_sediment
   use timers

   implicit none

   integer   :: k, j, kb, kt, ll

   integer(4) ithndl /0/
   if (.not. stm_included) return
   if (mxgr == 0) return
   if (timon) call timstrt ( "comp_sinktot", ithndl )

   if (kmx<1) then    ! 2D
      do k=1,ndx
         do j=ISED1,ISEDN
            ll = j-ISED1+1
            sinksetot(j,k) = sinksetot(j,k) + vol1(k)*sedtra%sinkse(k,ll)*constituents(j,k)*dts
            if (stmpar%morpar%flufflyr%iflufflyr > 0) then
               sinkftot(j,k)  = sinkftot(j,k) + vol1(k)*stmpar%morpar%flufflyr%sinkf(ll,k)*constituents(j,k)*dts
            endif
         enddo
      enddo
   else               ! 3D
      do k=1,ndx
         do j=ISED1,ISEDN
            ll = j-ISED1+1
            sinksetot(j,k) = sinksetot(j,k) + vol1(sedtra%kmxsed(k,ll))*sedtra%sinkse(k,ll) *constituents(j,sedtra%kmxsed(k,ll))*dts
            if (stmpar%morpar%flufflyr%iflufflyr > 0) then
               sinkftot(j,k)  = sinkftot(j,k) +  vol1(sedtra%kmxsed(k,ll))*stmpar%morpar%flufflyr%sinkf(ll,k)*constituents(j,sedtra%kmxsed(k,ll))*dts
            endif
         enddo
      enddo
   endif

   if (timon) call timstop( ithndl )
end subroutine comp_sinktot
