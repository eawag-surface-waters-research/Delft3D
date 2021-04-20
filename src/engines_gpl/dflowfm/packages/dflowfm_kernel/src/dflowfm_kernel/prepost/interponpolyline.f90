      !> Performs linear interpolation between two values along a polyline.
      !! The interpolation is done along a polyline at the distances
      !! measured along the consecutive polyline segments.
      SUBROUTINE interpOnPolyline(DPL, DXS, NPL, DXS1, DXS2)
      implicit none
      double precision, intent(in)  :: DPL(NPL) !< Accumulated distance at each point.
      double precision, intent(out) :: DXS(NPL) !< Interpolated values of dxs1--dxs2 on polyline points.
      double precision, intent(in)  :: dxs1     !< Value at first polyline point.
      double precision, intent(in)  :: dxs2     !< Value at last polyline point.
      integer :: npl

      double precision :: f
      double precision :: f1
      integer :: n

      IF (NPL .LE. 1) RETURN

      DO N = 1,NPL
         F = DPL(N) / DPL(NPL) ; F1 = 1-F
         DXS(N) = F1*DXS1 + F*DXS2
      ENDDO

      END SUBROUTINE interpOnPolyline
