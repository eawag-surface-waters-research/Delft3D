!> default solver settings
   subroutine SolverSettings(solver, numrows, numnonzeros)
      use m_solver
      implicit none

      type(tsolver), intent(inout) :: solver       !< solver
      integer,       intent(in)    :: numrows      !< number of rows
      integer,       intent(in)    :: numnonzeros  !< number of nonzero elements

      solver%numrows            = numrows
      solver%numnonzeros        = numnonzeros
      solver%numnonzerosprecond = 120*numrows
      solver%nwork              = 2*solver%numnonzerosprecond

         !!   ipar(1) = 0               ! initialized in "itaux"
      solver%ipar(2) = 1               ! no (0), left (1), right (2), both (3) precond
      solver%ipar(3) = 1               ! stopping criteria
      solver%ipar(4) = solver%nwork    ! number of elems in array 'wk'
      solver%ipar(5) = 10              ! size of Krylov subspace in GMRES and variants
      solver%ipar(6) = 1000            ! max number of mat-vec multiplies

      solver%fpar(1) = 0.0D-16         ! relative tolerance ('exact' solve, except
      solver%fpar(2) = 1.0d-14         ! absolute tolerance

      solver%lfil  = 3
      solver%alpha = 1d0
      solver%tol   = 0.50D-2

      solver%jabcgstab = 1
   end subroutine SolverSettings
