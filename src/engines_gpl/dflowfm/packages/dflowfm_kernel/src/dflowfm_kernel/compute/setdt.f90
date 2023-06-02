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

! 
! 

subroutine setdt()
   use m_partitioninfo
   use m_flowparameters, only: jawave
   use m_xbeach_data,    only: swave, instat
   use m_flowtimes
   use m_flow,           only: kkcflmx
   use m_timer
   use unstruc_display,  only: jaGUI
   use m_sediment,       only: jased, stm_included, stmpar, jamorcfl, jamormergedtuser
   use m_fm_erosed,      only: duneavalan
   use m_mormerge
   implicit none

   double precision :: dtsc_loc
   integer          :: nsteps
   integer          :: jareduced

!  compute CFL-based maximum time step and limiting flownode/time step, per subomdain
   call setdtorg(jareduced) ! 7.1 2031

   dtsc_loc = dtsc

!  globally reduce time step
   if ( jampi.eq.1 .and. jareduced.eq.0 ) then
!     globally reduce dts (dtsc may now be larger)
      if ( jatimer.eq.1 ) call starttimer(IMPIREDUCE)
      call reduce_double_min(dts)
      if ( jatimer.eq.1 ) call stoptimer(IMPIREDUCE)
   end if

   ! morphological timestep reduction
   if (stm_included  .and. jamorcfl>0) then
      if (time1 > tstart_user + stmpar%morpar%tmor * tfac) then
         call fm_mor_maxtimestep()
      endif
   endif

   if ( jawave.eq.4 .and. swave.eq.1 ) then
      if (.not.(trim(instat)=='stat' .or. trim(instat)=='stat_table')) then
         call xbeach_wave_maxtimestep()
      endif
   end if

   if (jased.eq.4 .and. stm_included) then
      if (duneavalan) then
         call setdtmaxavalan(dts)
      endif
   end if

   !  globally reduce time step
   if ( jampi.eq.1 ) then
      !     globally reduce dts (dtsc may now be larger)
      if ( jatimer.eq.1 ) call starttimer(IMPIREDUCE)
      call reduce_double_min(dts)
      if ( jatimer.eq.1 ) call stoptimer(IMPIREDUCE)
   end if

   dtsc = dts

!  account for user time step
   if (ja_timestep_auto >= 1) then
      if (dts > dtfacmax*dtprev) then
          dts = dtfacmax*dtprev
          nsteps = ceiling((time_user-time0) / dts)
          ! New timestep dts would be rounded down to same dtprev (undesired, so use nsteps-1)
          if (1000*dtprev > time_user-time0) then
             nsteps = ceiling((time_user-time0) / dts)
             if (nsteps == ceiling((time_user-time0) / dtprev)) then
                 nsteps = max(1,nsteps - 1)
             end if
             dts = (time_user-time0) / dble(nsteps)
             ! dtmax is always leading.
             if (dts > dt_max .or. dts > dtsc) then ! Fall back to smaller step anyway.
                dts    = (time_user-time0) / dble(nsteps+1)
             end if
          endif

      else
          ! dts = min (dts, dt_max) ! hk: commented out, already done this 15 lines above

          ! Fit timestep dts so that we will exactly reach time_user in future steps.
          !if ( time0+dts.ge.time_user ) then
          !   dts = min(dts, time_user-time0)
          !else
          ! NOTE: when the model has an extremely small timestep, nsteps gets an integer overflow,
          ! then becomes negative, so the max below sets nsteps=1, violating the dtmax requirement. (UNST-1926)
             nsteps = max(1,ceiling((time_user-time0) / dts))
             dts = ( time_user-time0 ) / dble(nsteps)
          !end if
      endif
   else
      dts = dt_max
      dtsc = 0d0    ! SPvdP: safety, was undefined but could be used later
      kkcflmx = 0   ! SPvdP: safety, was undefined but could be used later
   endif

   if (stm_included .and. jased>0) then
      if (stmpar%morpar%multi .and. jamormergedtuser==0 .and. my_rank == 0 ) then
         call put_get_time_step(stmpar%morpar%mergehandle, dts)
      endif
   endif

   call timestepanalysis(dtsc_loc)

   dti = 1d0/dts
   
   if ( jaGUI.eq.1 ) then
      call tekcflmx()
   endif

end subroutine setdt
