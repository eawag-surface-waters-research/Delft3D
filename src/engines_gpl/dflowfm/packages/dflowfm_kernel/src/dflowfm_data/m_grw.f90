 module m_grw
 use m_hydrology_data
 integer                               :: jagrw                !< include ground water
 double precision, allocatable         :: sgrw0(:)             !< ground water level start
 double precision, allocatable         :: sgrw1(:)             !< ground water level end of timestep
 double precision, allocatable         :: pgrw (:)             !< pressure and plotting of sgrw
 double precision, allocatable         :: h_unsat(:)           !< initial height unsaturated zone
 double precision, allocatable         :: bgrw(:)              !< initial height unsaturated zone


 !  TODO: UNST-3763: Use named parameter constant for jaintercept2D
 integer                               :: jaintercept2D        !< 1 = uniform, 2 = spatially variable
 double precision                      :: Hinterceptionlayer   !< (DEPRECATED) thickness of interception layer in  (m) only if infiltrationmodel == 1
 double precision                      :: Conductivity         !< non dimensionless K conductivity   saturated (m/s), Q = K*A*i (m3/s)
 double precision                      :: Unsatfac             !< reduction factor for conductivity in unsaturated zone

 double precision                      :: h_aquiferuni         !< uniform height of carrying layer
 double precision                      :: h_unsatini           !< initial level groundwater is bedlevel - h_unsatini
 double precision                      :: sgrwini              !< initial level groundwater. If specified, h_unsatini wiil not be used
 double precision                      :: bgrwuni              !< initial level groundwater. If specified, h_unsatini wiil not be used
 double precision                      :: h_capillair          !< Capillary rising height (m)
 double precision                      :: h_transfer           !< uniform thickness (numerical) transfer zone grw <-> openw

 double precision                      :: porosgrw             !< porosity of soil = Vair / (Vsoil+Vair)  , or,
                                                                 !< porosity of soil = (Rhoparticle - Rhobulk) / Rhoparticle
                                                                 !< e.g.
contains

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_grw() instead.
subroutine default_grw()
   jagrw             = 0       !< include ground water
   jaintercept2D     = 0       !< 1 = uniform, 2 = spatially variable
   !Hinterceptionlayer          !< thickness of interception layer in  (m) only if infiltrationmodel == 1
   Conductivity      = 0d-4    !< non dimensionless K conductivity   saturated (m/s), Q = K*A*i (m3/s)
   Unsatfac          = 1.0d0   !< reduction factor for conductivity in unsaturated zone

   h_aquiferuni      = 20d0    !< uniform height of carrying layer
   h_unsatini        = 0.2     !< initial level groundwater is bedlevel - h_unsatini
   sgrwini           = -999d0  !< initial level groundwater. If specified, h_unsatini wiil not be used
   bgrwuni           = -999d0  !< initial level groundwater. If specified, h_unsatini wiil not be used
   h_capillair       = 0.5     !< Capillary rising height (m)
   h_transfer        = 0.1d0   !< uniform thickness (numerical) transfer zone grw <-> openw

   porosgrw          = 0.25d0  !< porosity of soil = Vair / (Vsoil+Vair)  , or,
                               !< porosity of soil = (Rhoparticle - Rhobulk) / Rhoparticle
    ! Remaining of variables is handled in reset_grw()
    call reset_grw()
end subroutine default_grw

!> Resets only groundwater variables intended for a restart of flow simulation.
!! Upon loading of new model/MDU, call default_grw() instead.
subroutine reset_grw()
end subroutine reset_grw

 end module m_grw
