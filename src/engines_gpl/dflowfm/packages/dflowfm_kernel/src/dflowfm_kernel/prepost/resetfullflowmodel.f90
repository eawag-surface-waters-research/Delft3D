 !> Resets the current flow- and time-state, as well as all related (phys) parameters.
 !! To be called prior to loading a new MDU and upon program startup.
 subroutine resetFullFlowModel()
 use timers
 use m_wind
 use m_physcoef
 use m_turbulence
 use m_flow
 use m_flowexternalforcings
 use m_flowparameters
 use m_flowgeom
 use m_flowtimes
 use m_samples
 use unstruc_netcdf
 use unstruc_model
 use unstruc_display
 use m_observations
 use m_monitoring_crosssections
 use m_thindams
 use m_fixedweirs
 use m_sediment
 use m_trachy
 use m_hydrology_data
 use m_kml_parameters
 use m_structures
 use m_longculverts, only: default_longculverts
 use m_heatfluxes
 use m_ec_interpolationsettings
 use unstruc_channel_flow
 use m_sobekdfm
 use m_waves, only: default_waves
 use m_save_ugrid_state
 use  m_xbeach_avgoutput, only: default_xbeach_avgoutput
 use m_ship
 use unstruc_caching
 use m_subsidence
 use m_sferic, only : default_sferic
 implicit none

    ! Only reset counters and other scalars, allocatables should be
    ! automatically reset elsewhere (e.g., allocateandset*, flow_geominit)


    call init_unstruc_netcdf()

    call default_caching()

    ! TODO: UNST-487: Add default_fourier + reset
    call resetModel()

    call default_kml_parameters()

    call default_physcoef()

    call default_sferic()

    call default_grw()

    call default_wind()

    call default_waves()

    call default_sobekdfm()

    call dealloc(network) ! flow1d

    call default_heatfluxes()

    call default_sediment()  ! stm_included not defined yet

    call default_subsupl()

    call default_trachy()

    call default_hydrology_data()

    call default_fm_wq_processes()

    call default_turbulence()

    call default_flowgeom()

    call default_flowexternalforcings()

    call default_channel_flow()

    call default_structures()

    call default_longculverts()

    call default_flowtimes()

    call default_flowparameters()

    call default_flow()

    call default_interpolationsettings()

    call default_xbeach_avgoutput()

    call default_save_ugrid_state()

    !Reset samples:
    ns = 0

    ! Reset observations and cross sections
    call deleteObservations()
    call delCrossSections()
    call delThinDams()
    call delFixedWeirs()

 end subroutine resetFullFlowModel
