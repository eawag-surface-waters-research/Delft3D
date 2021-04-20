 !> Initializes all administration encessary for writing output to his-files.
 !! That is: snap observation stations to flow cells, cross sections to flow links.
 !! And bookkeeping for time series output on structures.
 subroutine flow_obsinit()
 use m_observations, only: init_valobs
 use m_structures
 implicit none
    call crosssections_on_flowgeom()
    call runupgauges_on_flowgeom()

    call obs_on_flowgeom(0)

!   for the following, it is assumed that the moving observation stations have been initialized (in flow_initexternalforcings)
    call init_valobs()   ! (re)initialize work array and set pointers for observation stations

    call updateValuesOnObservationStations() ! and fill first value

    call init_structure_hisvalues()

 end subroutine flow_obsinit
