!> set field oriented boundary conditions
subroutine flow_setexternalforcings(tim, l_initPhase, iresult)
   use m_timer
   use timers
   use m_flowtimes
   use m_flowgeom
   use m_flow
   use m_sferic
   use timespace
   use m_missing
   use m_structures
   use m_meteo
   use m_trachy, only : itimtt
   use m_calbedform
   use m_bedform
   use dfm_error
   use m_calibration, only: calibration_backup_frcu
   use unstruc_channel_flow
   use m_pump
   use m_Dambreak
   use m_flowexternalforcings
   use m_partitioninfo
   use time_class
   use m_longculverts
   use unstruc_messages

   implicit none

   double precision, intent(in)    :: tim !< Time in seconds
   type(c_time)                    :: ecTime !< Time in EC-module
   logical                         :: l_initPhase, first_time_wind
   integer,          intent(out)   :: iresult !< Integer error status: DFM_NOERR==0 if succesful.

   double precision :: timmin
   double precision :: tem_dif
   integer          :: k, L, i, k1, k2, iFirst, iLast
   logical          :: l_set_frcu_mor = .false.

   logical, external :: flow_initwaveforcings_runtime, flow_trachy_needs_update
   character(len=255) :: tmpstr
   type(tEcItem), pointer :: itemPtr !< Item under consideration, for right order of wind items

   ! variables for processing the pump with levels, SOBEK style
   integer                               :: n, ierr, istru, structInd
   double precision, allocatable         :: results(:,:)
   double precision, allocatable         :: waterLevelsLeft(:), waterLevelsRight(:),normalVelocity(:)
   logical                               :: foundtempforcing

   iresult = DFM_EXTFORCERROR
   call timstrt('External forcings', handle_ext)

   timmin = tim/60d0   ! talking to Meteo1 is in minutes

   success = .true.

   if (allocated(patm)) then
      ! To prevent any pressure jumps at the boundary, set (initial) patm in interior to PavBnd.
      ! May of course be overridden later by spatially varying patm values.
      patm = PavBnd
   end if

   if (jawind == 1 .or. japatm > 0) then   ! setwind
      if (allocated(wx)) then              ! initialize all winds to zero
        wx = 0.d0
      end if
      if (allocated(wy)) then
        wy = 0.d0
      end if
      if (allocated(wcharnock)) then
        wcharnock = 0.d0
      end if
      if (allocated(ec_pwxwy_x)) then
        ec_pwxwy_x = 0.d0
      end if
      if (allocated(ec_pwxwy_y)) then
        ec_pwxwy_y = 0.d0
      end if

      first_time_wind = (id_last_wind < 0)
      if (first_time_wind) then
         iFirst = 1
         iLast = ecInstancePtr%nItems
      else
         iFirst = id_first_wind
         iLast = id_last_wind
      endif
      do i=iFirst, iLast
         itemPtr => ecInstancePtr%ecItemsPtr(i)%ptr
         ! Retrieve wind's x- and y-component for ext-file quantity 'windxy'.
         if (itemPtr%id == item_windxy_x .and. item_windxy_y /= ec_undef_int) then
            success = ec_gettimespacevalue(ecInstancePtr, item_windxy_x, irefdate, tzone, tunit, tim)
         ! Retrieve wind's p-, x- and y-component for ext-file quantity 'airpressure_windx_windy'.
         else if (itemPtr%id == item_apwxwy_p .and. item_apwxwy_x /= ec_undef_int .and. item_apwxwy_y /= ec_undef_int) then
            if (item_apwxwy_c /= ec_undef_int) then
               success = ec_gettimespacevalue(ecInstancePtr, 'airpressure_windx_windy_charnock', tim)
            else
               success = ec_gettimespacevalue(ecInstancePtr, 'airpressure_windx_windy', tim)
            endif
            if (success) then
               ! FM performs an additional spatial interpolation:
               do L  = 1,lnx ! i
                  k1 = ln(1,L) ; k2 = ln(2,L)
                  wx(L) = wx(L) + 0.5d0*( ec_pwxwy_x(k1) + ec_pwxwy_x(k2) )
                  wy(L) = wy(L) + 0.5d0*( ec_pwxwy_y(k1) + ec_pwxwy_y(k2) )
                  if (allocated(ec_pwxwy_c)) then
                     wcharnock(L) = wcharnock(L) + 0.5d0*( ec_pwxwy_c(k1) + ec_pwxwy_c(k2) )
                  endif
               enddo
            end if
         ! Retrieve wind's x-component for ext-file quantity 'windx'.
         else if (itemPtr%id == item_windx) then
            success = ec_gettimespacevalue(ecInstancePtr, item_windx, irefdate, tzone, tunit, tim)
         ! Retrieve wind's y-component for ext-file quantity 'windy'.
         else if (itemPtr%id == item_windy) then
            success = ec_gettimespacevalue(ecInstancePtr, item_windy, irefdate, tzone, tunit, tim)
         ! Retrieve wind's p-component for ext-file quantity 'atmosphericpressure'.
         else if (itemPtr%id == item_atmosphericpressure) then
            success = ec_gettimespacevalue(ecInstancePtr, item_atmosphericpressure, irefdate, tzone, tunit, tim)
         else
            cycle  ! avoid updating id_first_wind and id_last_wind
         endif
         if (.not. success) goto 888
         if (first_time_wind) then
            id_first_wind = min(i, id_first_wind)
            id_last_wind  = max(i, id_last_wind)
         endif
      enddo

      if (item_atmosphericpressure /= ec_undef_int) then
         do k = 1,ndx
            if (patm(k) == dmiss) patm(k) = 101325d0
         enddo
      endif

      if (jawave == 1 .or. jawave == 2) then
         call tauwavefetch(tim)
      endif
   endif

   if (jawind > 0) then
      if (jawindspeedfac > 0) then
         do L = 1,lnx
            if (windspeedfac(L) .ne. dmiss) then
                wx(L) = wx(L) *windspeedfac(L)
                wy(L) = wy(L) *windspeedfac(L)
            endif
         enddo
      endif
      call setwindstress()
   endif

!   !$OMP  PARALLEL SECTIONS   &
!   !$OMP REDUCTION(.AND.:success)


!   !$OMP SECTION

    if (jatem > 1) then

       ! Update arrays rhum, tair and clou in a single method call.
       ! Nothing happens in case quantity 'humidity_airtemperature_cloudiness' has never been added through ec_addtimespacerelation.
       select case (itempforcingtyp)
       case (1)
          success = success .and. ec_gettimespacevalue(ecInstancePtr, 'humidity_airtemperature_cloudiness', tim)
       case (2)
          success = success .and. ec_gettimespacevalue(ecInstancePtr, 'humidity_airtemperature_cloudiness_solarradiation', tim)
       case (3)
          success = success .and. ec_gettimespacevalue(ecInstancePtr, 'dewpoint_airtemperature_cloudiness', tim)
       case (4)
          success = success .and. ec_gettimespacevalue(ecInstancePtr, 'dewpoint_airtemperature_cloudiness_solarradiation', tim)
       end select

       foundtempforcing = (itempforcingtyp >= 1 .and. itempforcingtyp <= 4)

       if (btempforcingtypH) then
           success = success .and. ec_gettimespacevalue(ecInstancePtr, item_humidity, irefdate, tzone, tunit, tim)
           foundtempforcing = .true.
       endif
       if (btempforcingtypA) then
           success = success .and. ec_gettimespacevalue(ecInstancePtr, item_airtemperature, irefdate, tzone, tunit, tim)
           foundtempforcing = .true.
       endif
       if (btempforcingtypS) then
           success = success .and. ec_gettimespacevalue(ecInstancePtr, item_solarradiation, irefdate, tzone, tunit, tim)
           foundtempforcing = .true.
       endif
       if (btempforcingtypC) then
           success = success .and. ec_gettimespacevalue(ecInstancePtr, item_cloudiness, irefdate, tzone, tunit, tim)
           foundtempforcing = .true.
       endif
       if (btempforcingtypL) then
           success = success .and. ec_gettimespacevalue(ecInstancePtr, item_longwaveradiation, irefdate, tzone, tunit, tim)
           foundtempforcing = .true.
       endif

       if (.not. foundtempforcing ) then
            call mess(LEVEL_WARN,'No humidity, airtemperature, cloudiness and solar radiation forcing found, setting temperature model [physics:Temperature] = 1 (Only transport)')
            jatem = 1
       endif
   endif

!   !$OMP SECTION

   ! Get wave parameters within this parallel section:
   call ecTime%set(tim)
   if (jawave == 3) then
      !
      ! This part must be skipped during initialization
      if (.not.l_initPhase) then
         ! Finally the delayed external forcings can be initialized
         success = flow_initwaveforcings_runtime()
         if (allocated (hwavcom) ) then
            ! Don't make them zero: ecGetValues might do nothing
            !hwav = 0.0
            success = success .and. ecGetValues(ecInstancePtr, item_hrms, ecTime)
         endif
         if (allocated (twav) ) then
            ! Don't make them zero: ecGetValues might do nothing
            !twav = 0.0
            success = success .and. ecGetValues(ecInstancePtr, item_tp, ecTime)
         endif
         if (allocated (phiwav) ) then
            ! Don't make them zero: ecGetValues might do nothing
            !phiwav = 0.0
            success = success .and. ecGetValues(ecInstancePtr, item_dir, ecTime)
         endif
         if (allocated (sxwav) ) then
            ! Don't make them zero: ecGetValues might do nothing
            !sxwav = 0.0
            success = success .and. ecGetValues(ecInstancePtr, item_fx, ecTime)
         endif
         if (allocated (sywav) ) then
            ! Don't make them zero: ecGetValues might do nothing
            !sywav = 0.0
            success = success .and. ecGetValues(ecInstancePtr, item_fy, ecTime)
         endif
         if (allocated (sbxwav) ) then
            ! Don't make them zero: ecGetValues might do nothing
            !sxwav = 0.0
            success = success .and. ecGetValues(ecInstancePtr, item_wsbu, ecTime)
         endif
         if (allocated (sbywav) ) then
            ! Don't make them zero: ecGetValues might do nothing
            !sywav = 0.0
            success = success .and. ecGetValues(ecInstancePtr, item_wsbv, ecTime)
         endif
         if (allocated (mxwav) ) then
            ! Don't make them zero: ecGetValues might do nothing
            !mxwav = 0.0
            success = success .and. ecGetValues(ecInstancePtr, item_mx, ecTime)
         endif
         if (allocated (mywav) ) then
            ! Don't make them zero: ecGetValues might do nothing
            !mywav = 0.0
            success = success .and. ecGetValues(ecInstancePtr, item_my, ecTime)
         endif
         if (allocated (dsurf) ) then
            success = success .and. ecGetValues(ecInstancePtr, item_dissurf, ecTime)
         endif
         if (allocated (dwcap) ) then
            success = success .and. ecGetValues(ecInstancePtr, item_diswcap, ecTime)
         endif
         if (allocated (uorbwav) ) then
            success = success .and. ecGetValues(ecInstancePtr, item_ubot, ecTime)
         endif
      endif
      if (.not. success) then
         !
         ! success = .false. : Most commonly, WAVE data has not been written to the com-file yet:
         ! - Print a warning
         ! - Continue with the calculation
         ! - Just try it the next timestep again
         ! - success must be set to .true., otherwise the calculation is aborted
         !
         message = dumpECMessageStack(LEVEL_WARN,callback_msg)
         success=.true.
      end if
      !
      ! SWAN data used via module m_waves
      !    Data from FLOW 2 SWAN: s1 (water level), bl (bottom level), ucx (vel. x), ucy (vel. y), FlowElem_xcc, FlowElem_ycc, wx, wy
      !          NOTE: all variables defined @ cell circumcentre of unstructured grid
      !                different from Delft3D. There all variables are defined on the velocity points.
      !    Data from SWAN 2 FLOW:  wavefx, wavefy, hrms (or 0.5*sqrt(2)*hm0), rtp, tp/tps/rtp, phi (= wavedirmean), Uorb, wlen
      !          NOTE:
      !                not necessary are; tmean (Tm01), urms, wavedirpeak
      !
      !   JRE: moved this part to step_reduce, update needed at compu dt
      !
      !if( kmx == 0 ) then
      !   call wave_comp_stokes_velocities()
      !   call wave_uorbrlabda()                       ! hwav gets depth-limited here
      !   call tauwave()
      !   if ( jaGUI.eq.1 ) then                                          ! this part is for online visualisation
      !      if (ntek > 0) then
      !         if (mod(int(dnt_user),ntek) .eq. 0) then
      !            call wave_makeplotvars()
      !         end if
      !      endif
      !   endif
      !   ! wavfu: wave force at links, to be used in the advection equation
      !   call setwavfu()
      !   call setwavmubnd()
      !end if
   endif

!   !$OMP SECTION

!   !$OMP SECTION

   ! Retrieve rainfall for ext-file quantity 'rainfall'.
   if (jarain > 0) then
      if (item_rainfall /= ec_undef_int) then
         success = success .and. ec_gettimespacevalue(ecInstancePtr, 'rainfall', tim)
      endif
      if (item_rainfall_rate /= ec_undef_int) then
         success = success .and. ec_gettimespacevalue(ecInstancePtr, 'rainfall_rate', tim)
      endif
   endif

!   !$OMP SECTION

   if (ncdamsg > 0) then
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_damlevel, irefdate, tzone, tunit, tim, zcdam)
   endif

!   !$OMP SECTION

   if (ncgensg > 0) then
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_generalstructure, irefdate, tzone, tunit, tim, zcgen)
      call update_zcgen_widths_and_heights() ! TODO: replace by Jan's LineStructure from channel_flow
   endif

!   !$OMP SECTION

   if (npumpsg > 0) then
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_pump, irefdate, tzone, tunit, tim, qpump)
   endif
   if (network%sts%numPumps > 0) then
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_pump_capacity, irefdate, tzone, tunit, tim)
   endif

   !   !$OMP SECTION

   if (network%sts%numCulverts > 0) then
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_culvert_valveOpeningHeight, irefdate, tzone, tunit, tim)
   endif

   !   !$OMP SECTION

   if (network%sts%numWeirs > 0) then
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_weir_crestLevel, irefdate, tzone, tunit, tim)
   endif

   !   !$OMP SECTION

   if (network%sts%numOrifices > 0) then
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_orifice_crestLevel, irefdate, tzone, tunit, tim)
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_orifice_gateLowerEdgeLevel, irefdate, tzone, tunit, tim)
   endif

   !   !$OMP SECTION

   if (network%sts%numGeneralStructures > 0) then
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_general_structure_crestLevel, irefdate, tzone, tunit, tim)
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_general_structure_gateLowerEdgeLevel, irefdate, tzone, tunit, tim)
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_general_structure_crestWidth, irefdate, tzone, tunit, tim)
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_general_structure_gateOpeningWidth, irefdate, tzone, tunit, tim)
   endif

   if (nlongculvertsg > 0) then
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_longculvert_valve_relative_opening, irefdate, tzone, tunit, tim)
   endif

   !   !$OMP SECTION

   if (nvalv > 0) then
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_valve1D, irefdate, tzone, tunit,tim)
   endif

!   !$OMP SECTION

   if (jatidep > 0 .or. jaselfal > 0) then
      call flow_settidepotential(timmin)
   endif

!   !$OMP SECTION

   if (numlatsg > 0) then
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_lateraldischarge, irefdate, tzone, tunit, tim) ! 'lateral(_)discharge'
   endif

!   !$OMP END PARALLEL SECTIONS

   !Pump with levels, outside OpenMP region
   if (nPumpsWithLevels > 0) then
      call update_pumps_with_levels()
   endif

   if (numsrc > 0) then
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_discharge_salinity_temperature_sorsin, irefdate, tzone, tunit, tim)
   endif

   if (jasubsupl > 0) then
      if (.not. sdu_first) then
         ! preserve the previous 'bedrock_surface_elevation' for computing the subsidence/uplift rate
         subsupl_tp = subsupl
      endif
      if (item_subsiduplift /= ec_undef_int) then
         success = success .and. ec_gettimespacevalue(ecInstancePtr, 'bedrock_surface_elevation', tim)
      endif
      if (sdu_first) then
         ! preserve the first 'bedrock_surface_elevation' field as the initial field
         subsupl_tp = subsupl
         subsupl_t0 = subsupl
         sdu_first  = .false.
      endif
   endif

   call timstop(handle_ext)

   if (.not. success) then
      goto 888
   end if

   if (jatem > 1 .and. jaheat_eachstep == 0) then
      call heatu(timmin/60d0)                                  ! from externalforcings
   endif

   if (bfm_included) then
      if (bfmpar%lfbedfrm) then
          call fm_calbf()            ! JRE TODO: see with which timestep we update this?
       end if
   end if

   if (bfmpar%lfbedfrmrou)  then     ! .true. if van rijn 2004 or trachy contains ripple roughness
      call fm_calksc()
   end if

   if ((jacali == 1) .and. l_initphase) then
      ! Make backup of roughness factor after initialisation of frcu
      call calibration_backup_frcu()
   endif

   if (jatrt == 1) then
       if (flow_trachy_needs_update(time1)) then
           call flow_trachyupdate()                            ! perform a trachy update step
           l_set_frcu_mor = .true.
       end if
   end if

   if (jacali == 1) then
       ! update calibration definitions and factors on links
       call calibration_update()
       l_set_frcu_mor = .true.
   end if

   if (stm_included) then
       if ((jased>0) .and. l_set_frcu_mor) then
           if (jamd1dfile == 0) then
               call set_frcu_mor(1)     !otherwise frcu_mor is set in getprof_1d()
           endif
           call set_frcu_mor(2)
       endif
   endif

   ! Update nudging temperature (and salinity)
   if (item_nudge_tem /= ec_undef_int .and. janudge > 0 ) then ! .and. .not.l_initphase) then
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_nudge_tem, irefdate, tzone, tunit, tim)
!      tmpstr = dumpECMessageStack(LEVEL_INFO, callback_msg)
   endif

   iresult = DFM_NOERR
   return ! return with success

   ! Error handling:
888 continue
   iresult = DFM_EXTFORCERROR
   write(tmpstr,'(f22.11)') tim
   call mess(LEVEL_WARN, 'Error while updating meteo/structure forcing at time=' // trim(tmpstr))
   tmpstr = dumpECMessageStack(LEVEL_WARN,callback_msg)
end subroutine flow_setexternalforcings
