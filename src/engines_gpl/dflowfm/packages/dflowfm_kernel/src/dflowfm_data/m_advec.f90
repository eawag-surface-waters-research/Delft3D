module m_advec
   use m_solver

   integer                                     :: jaoutput   = 1 !< output matrices to file (1) or not (0)

   type(tsolver)                               :: solver_advec

   integer,          dimension(:), allocatable :: iI, jI
   double precision, dimension(:), allocatable :: aI        !< interpolation and projection matrix in CRS format

   integer,          dimension(:), allocatable :: iR, jR
   double precision, dimension(:), allocatable :: aR        !< reconstruction matrix in CRS format

   integer,          dimension(:), allocatable :: iC, jC
   double precision, dimension(:), allocatable :: aC        !< collocated discretization matrix in CRS format

   integer,          dimension(:), allocatable :: iW, jW
   double precision, dimension(:), allocatable :: aW        !< work matrix in RCS format
   integer,          dimension(:), allocatable :: iwork

   double precision, dimension(:,:), allocatable :: dfluxfac   !< flux(L) = dfluxfac(1,L) * something(ln(1,L)) + dfluxfac(2,L) * something(ln(2,L)), positive from ln(1,L) to ln(2,L)
end module
