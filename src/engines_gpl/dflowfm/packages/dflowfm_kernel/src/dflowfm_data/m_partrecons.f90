module m_partrecons
   double precision,  dimension(:),   allocatable :: qe           !< fluxes at all edges, including internal
   double precision,  dimension(:),   allocatable :: u0x, u0y, alpha !< reconstruction of velocity fiels in cells, dim(numcells)
   double precision,  dimension(:),   allocatable :: u0z          !< reconstruction of velocity fiels in cells, dim(numcells), for spherical models

   integer,           dimension(:),   allocatable :: ireconst    !< sparse storage of velocity reconstructin, edges,        dim(jreconst(numcells+1)-1)
   integer,           dimension(:),   allocatable :: jreconst    !< sparse storage of velocity reconstructin, startpointer, dim(numcells+1)
   double precision,  dimension(:,:), allocatable :: Areconst    !< sparse storage of velocity reconstructin, [ucx, ucy, (ucz,) alpha] dim(jreconst(3 (4), numcells+1)-1)
end module m_partrecons
