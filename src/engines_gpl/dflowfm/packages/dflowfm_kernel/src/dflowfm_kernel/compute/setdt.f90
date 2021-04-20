subroutine setdt()
   use m_partitioninfo
   use m_flowparameters, only: jawave
   use m_xbeach_data,    only: swave, instat
   use m_flowtimes
   use m_flow,           only: kkcflmx
   use m_timer
   use unstruc_display,  only: jaGUI
   use m_sediment,       only: jased, stm_included, stmpar, jamorcfl, jamormergedtuser
   implicit none

   double precision :: dtsc_loc
   double precision :: dim_real

   integer          :: nsteps
   integer          :: jareduced

!  compute CFL-based maximum time step and limiting flownode/time step, per subomdain
   call setdtorg(jareduced) ! 7.1 2031

   dtsc_loc = dtsc
   dim_real = 1.0d0

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
     call setdtmaxavalan(dts)
   end if

   dti = 1d0/dts
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
      if (stmpar%morpar%multi .and. jamormergedtuser==0) then
         call putarray (stmpar%morpar%mergehandle,dim_real,1)
         call putarray (stmpar%morpar%mergehandle,dts,1)
         call getarray (stmpar%morpar%mergehandle,dts,1)
      endif
   endif

   call timestepanalysis(dtsc_loc)

   if ( jaGUI.eq.1 ) then
      call tekcflmx()
   endif

end subroutine setdt
