 module m_jacobi                                        ! arrays needed for solving jacobi
 implicit none
 integer                           :: ndxjac   = 0      ! nr. of nodes already allocated for jacobi should be ndx
 integer                           :: lnxjac   = 0      ! nr. of links already allocated for jacobi should be lnx
 integer                           :: itmxjac  = 6666   ! max nr. of iterations in solve-jacobi
 double precision                  :: epsjac   = 1d-13  ! epsilon waterlevels jacobi method (maximum)
 double precision, allocatable     :: rr    (:)         ! resid
 double precision, allocatable     :: db    (:)         ! solving related, right-hand side
 double precision, allocatable     :: bbi   (:)         ! solving related, diagonal
 end module m_jacobi
