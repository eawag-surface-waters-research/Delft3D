      !> Maps a list of distances to a list of points.
      !! The points are placed onto a polyline at the distances measured along
      !! the consecutive polyline segments.
      SUBROUTINE mapToPolyline(XHO, YHO, DPL, NO, XH, YH, DPLA, NPL) ! HAAL HUIDIGE PUNTEN OP
      implicit none
      DOUBLE PRECISION, intent(in)  :: XHO(NO), YHO(NO) !< Polyline points.
      double precision, intent(in)  :: DPL(NO)          !< Accumulated segment sizes along polyline.
      integer,          intent(in)  :: NO               !< Nr. of polyline points.
      double precision, intent(out) :: XH(NPL), YH(NPL) !< Output points interpolated on polyline.
      double precision, intent(in)  :: DPLA(NPL)        !< Desired distances for all points.
      integer,          intent(in)  :: npl              !< Nr. of points to be interpolated.

      integer :: ja
      integer :: n

      DO N = 1, NPL
         CALL interpolateOnPolyline(XHO,YHO,YHO,DPL,NO,XH(N),YH(N),YH(N),DPLA(N),JA)
      ENDDO

      END SUBROUTINE mapToPolyline
