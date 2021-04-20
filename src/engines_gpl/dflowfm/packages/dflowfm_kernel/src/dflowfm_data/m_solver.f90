module m_solver
   type tsolver
!     matrix
      integer                                       :: numrows               !< number of rows
      integer                                       :: numnonzeros           !< number of non-zero entries
      double precision, dimension(:),   allocatable :: a                     !< matrix entries,   dim(numnonzeros)
      integer,          dimension(:),   allocatable :: ia                    !< matrix pointer,   dim(numrows+1)
      integer,          dimension(:),   allocatable :: ja                    !< matrix row index, dim(numnonzeros)

!     right-hand side
      double precision, dimension(:),   allocatable :: rhs

!     preconditioner
      integer                                       :: numnonzerosprecond    !< number of non-zero entries in preconditioning matrix
      double precision, dimension(:),   allocatable :: alu                   !< precond. matrix entries,   dim(numnonzerosprecond)
      integer,          dimension(:),   allocatable :: ju                    !< precond. matrix pointer,   dim(numrows)
      integer,          dimension(:),   allocatable :: jlu                   !< precond. matrix row index, dim(numnonzerosprecond)

!     work array
      integer                                       :: nwork
      double precision, dimension(:),   allocatable :: work                  !< work array, dimension(nwork)     !< size of work array
      integer,          dimension(:),   allocatable :: jw                    !< nonzero indicater, dim(2*nrows)

!     settings
      integer,          dimension(16)               :: ipar
      double precision, dimension(16)               :: fpar
      double precision                              :: alpha
      integer                                       :: lfil
      double precision                              :: tol
      double precision                              :: eps
      integer                                       :: jabcgstab
   end type tsolver
end module m_solver
