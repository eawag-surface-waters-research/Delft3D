 !> Resets the current flow- and time-state, but keeps al active parameter settings.
 !! To be called upon flow_modelinit().
 !! Upon program startup and loading of new model/MDU, call resetFullFlowModel() instead.
 subroutine resetFlow()
 use m_wind
 use m_flow
 use m_flowexternalforcings
 use m_flowparameters
 use m_statistics
 use m_flowgeom
 use m_flowtimes
 use waq
 use m_waves
 use m_hydrology_data
 use m_sobekdfm
 use m_save_ugrid_state, only: reset_save_ugrid_state
 use m_longculverts, only: reset_longculverts
 implicit none

    ! Only reset counters and other scalars, allocatables should be
    ! automatically reset elsewhere (e.g., allocateandset*, flow_geominit)

    call reset_wind()

    call reset_waves()

    call reset_sobekdfm()

    ! Reset some flow (rest is done in flow_geominit())
    call reset_flowgeom()

    call reset_flowexternalforcings()

    call reset_longculverts()

    call reset_flowtimes()

    ! call reset_flowparameters()

    call reset_flow()

    call reset_waq()

    call reset_movobs()

    call reset_statistics()

    if ( jawave.eq.4 ) then
       call xbeach_reset()
    end if

    call reset_save_ugrid_state()

    call reset_sedtra()

    call reset_hydrology_data()

 end subroutine resetFlow
