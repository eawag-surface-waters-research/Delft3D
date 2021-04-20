      SUBROUTINE INTDXSTRI(XH,YH,DXS,NPH,JDLA)
      use m_missing
      use m_samples
      use m_sferic, only: jsferic, jasfer3D
      use m_polygon, only: NPL, xpl, ypl, zpl
      use m_ec_basic_interpolation, only: triinterp2
      use m_flowexternalforcings, only: transformcoef

      implicit none
      DOUBLE PRECISION :: XH(NPH), YH(NPH), DXS(NPH)
      integer :: nph, jdla

      double precision :: dxsav
      integer :: n
      integer :: nn

      DXS = DXYMIS

      CALL triinterp2(XH,YH,DXS,NPH,JDLA, &
                      XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)

      NN = 0
      DO N = 1,NPH
         IF (DXS(N) .NE. DXYMIS) THEN
            DXSAV = DXSAV + DXS(N); NN = NN + 1
         ENDIF
      ENDDO

      IF (NN < NPH) THEN   ! TODO, LINEAR INTER- AND EXTRAPOLATION
         DXSAV = DXSAV / NN
         DO N  = 1, NPH
            IF (DXS(N) == DXYMIS) THEN
                DXS(N) = DXSAV
            ENDIF
         ENDDO
      ENDIF

      END SUBROUTINE INTDXSTRI
