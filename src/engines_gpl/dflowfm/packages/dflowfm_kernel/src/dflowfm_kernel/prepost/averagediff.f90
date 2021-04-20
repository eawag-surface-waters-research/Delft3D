      !> Computes the average segment size at polyline points.
      !! by averaging between left and right neighbouring points at each point.
      SUBROUTINE averageDiff(DPL, DDX, NPL)
      implicit none
      DOUBLE PRECISION, intent(in)  :: DPL(NPL) !< Accumulated distance at each point
      double precision, intent(out) :: DDX(NPL) !< Output average segment size.
      integer :: npl                            !< Nr. of polyline points.

      integer :: n

      DDX      = 0D0
      DDX(1)   = 1d0*( DPL(2)   - DPL(1)     )
      DDX(NPL) = 1d0*( DPL(NPL) - DPL(NPL-1) )

      DO N = 2, NPL-1
         DDX(N) = 0.5D0*( DPL(N+1) - DPL(N-1) )
      ENDDO

      END SUBROUTINE averageDiff
