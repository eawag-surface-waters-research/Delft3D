module m_samples_refine     ! used in refinecellsandfaces2 and in sample paths
   integer                                         :: NDIM=5             !< sample vector dimension
   double precision, allocatable, dimension(:,:,:) :: zss                !< sample data [zs, direction vector x-component, direction vector y-component, refinement criterion, ridge distance], dim(NDIM,MXSAM,MYSAM)

   integer                                         :: Nsamplesmooth  = 0     !< number of sample smoothing iterations
   integer                                         :: Nsamplesmooth_last = -1 !< last number of sample smoothing iterations
   integer                                         :: MAXLEVEL       = 10    !< maximum number of refinement levels
   double precision                                :: threshold      = 1d2   !< typical obstacle height in grid refinement
   double precision                                :: thresholdmin   = 1d0   !< minimum obstacle height grid refinement
   double precision                                :: hmin           = 1d3   !< minimum cell size
   integer                                         :: jadirectional  = 0     !< directional refinement (1) or not (0)

   integer, parameter                              :: iHesstat_OK    = 0     !< sample Hessians up-to-date
   integer, parameter                              :: iHesstat_DIRTY = 1     !< sample Hessians out-of-date
   integer                                         :: iHesstat       = 0     !< sample Hessians up-to-date (0) or not (1)

   integer, parameter                              :: ITYPE_RIDGE       = 1     !< critetrion based on ridge-detection
   integer, parameter                              :: ITYPE_WAVECOURANT = 2     !< critetrion based on wave Courant number
   integer, parameter                              :: ITYPE_MESHWIDTH   = 3     !< criterion based on maximum mesh width
   integer                                         :: irefinetype       = ITYPE_WAVECOURANT     !< refinement criterion type
   integer                                         :: jaconnect         = 1     !< connect hanging nodes (1) or not (0)
   double precision                                :: Dt_maxcour        = 240d0 !< maximum time-step in courant grid
   double precision                                :: Dx_mincour        = 100d0 !< minimum edge length in courant grid
   double precision                                :: dminsampledist    = 0d0   !< minimum sample distance
   integer                                         :: jaoutsidecell     = 1     !< take samples outside cell into account (1) or not (0)
   integer                                         :: numrefcycles=0, numrefcyc=0     !< max and act nr of non interactive cycles
end module m_samples_refine
