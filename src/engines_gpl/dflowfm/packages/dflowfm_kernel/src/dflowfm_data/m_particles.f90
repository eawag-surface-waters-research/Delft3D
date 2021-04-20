module m_particles
   integer                                        :: japart       !< particles (1) or not (0)

   integer                                        :: Npart        !< number of particles
   double precision,  dimension(:),   allocatable :: xpart, ypart !< coordinates of particles, dim(Npart)
   double precision,  dimension(:),   allocatable :: zpart        !< z-coordinates of particles, dim(Npart), for spherical models
   double precision,  dimension(:),   allocatable :: dtremaining  !< remaining time, dim(Npart)
   integer,           dimension(:),   allocatable :: kpart        !< cell (flownode) number, dim(Npart)
!   integer,           dimension(:),   allocatable :: Lpart        !< edge (netlink)  number, dim(Npart)
!   character(len=40), dimension(:),   allocatable :: namepart     !< name of particle, dim(Npart)
   integer,           dimension(:),   allocatable :: iglob        !< global number, dim(Npart)
   integer                                        :: Nglob        !< maximum global number
   integer                                        :: NpartOut     !< number of particle tracks in output

   integer                                        :: Nrpart       !< number of particles to be released
   integer                                        :: irpart       !< current number of particle to be released
   double precision,  dimension(:),   allocatable :: trpart       !< timing of to be released particles, dim(Npart)
   double precision,  dimension(:),   allocatable :: xrpart       !< x-coordinates of to be released particles, dim(Npart)
   double precision,  dimension(:),   allocatable :: yrpart       !< y-coordinates of to be released particles, dim(Npart)
   double precision,  dimension(:),   allocatable :: zrpart       !< z-coordinates of to be released particles, dim(Npart), for spherical models

   integer,           dimension(:),   allocatable :: numzero      !< number of consecutive (sub)times a particle was not displaces within a time-step

   integer                                        :: jatracer     !< add tracer with particle concentration (1) or not (0)
   double precision                               :: starttime    !< start time
   double precision                               :: timestep     !< time step (>0) or every computational timestep
   double precision                               :: timelast     !< last time of particle update
   double precision                               :: timenext     !< next time of particle update
   double precision                               :: timepart     !< time of particles
   character(len=22), parameter                   :: PART_TRACERNAME = 'particle_concentration' !< particle tracer name
   integer                                        :: part_iconst  !< particle tracer constituent number
   integer                                        :: threeDtype       !< depth averaged or 2D (0), free surface (1)

   double precision, dimension(:),    allocatable :: sbegin      !< water level at begin of time interval, dim(Ndx)
   double precision, dimension(:),    allocatable :: qpart       !< cummulative fluxes from begin of time interval, dim(Lnx)

   double precision, dimension(:),    allocatable :: qfreesurf       !< free surface flux (for threeDtype=1)

!   integer :: mfile
end module m_particles
