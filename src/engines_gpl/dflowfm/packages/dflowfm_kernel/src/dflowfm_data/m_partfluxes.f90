module m_partfluxes
   integer,          dimension(:),    allocatable :: iflux2link   !< sparse storage of edge-based flux to flowlink-based flux (prescribed), flowlinks, dim(jflux2link(numedges+1)-1)
   integer,          dimension(:),    allocatable :: jflux2link   !< sparse storage of edge-based flux to flowlink-based flux (prescribed), startpointer, dim(numedges+1)
   double precision, dimension(:),    allocatable :: Aflux2link   !< sparse storage of edge-based flux to flowlink-based flux (prescribed), coefficients, dim(jflux2link(numedges+1)-1)
end module m_partfluxes
