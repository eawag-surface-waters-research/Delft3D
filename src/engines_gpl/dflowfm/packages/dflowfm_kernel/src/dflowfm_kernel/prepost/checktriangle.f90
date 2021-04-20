   SUBROUTINE CHECKTRIANGLE(N,JA,phimin,phimax)

   use m_samples
   use m_ec_triangle
   use m_sferic
   use geometry_module, only: dcosphi
   use m_missing, only : dmiss, dxymis

   implicit none
   double precision :: phimin,phimax
   integer          :: n,ja

   integer :: k0, k1, k2, n0, n2, nn
   DOUBLE PRECISION :: X0, Y0, X1, Y1, X2, Y2, COSPHI, PHI
   IF ( TRIANGLEMINANGLE >= TRIANGLEMAXANGLE ) RETURN
   JA = 1
   phimin = 1d3 ; phimax = 0d0
   DO NN = 1,3
      N0 = NN - 1; IF (N0 < 1) N0 = N0 + 3
      N2 = NN + 1; IF (N2 > 3) N2 = N2 - 3
      K0 = INDX(N0,N) ; K1 = INDX(NN,N) ; K2 = INDX(N2,N)
      X0 = XS(K0) ; Y0 = YS(K0)
      X1 = XS(K1) ; Y1 = YS(K1)
      X2 = XS(K2) ; Y2 = YS(K2)
      COSPHI = DCOSPHI(X1,Y1,X0,Y0,X1,Y1,X2,Y2, jsferic, jasfer3D, dxymis)
      PHI    = ACOS(min(max(COSPHI,-1d0),1d0))*RD2DG
      phimin = min(phimin, phi)
      phimax = max(phimax, phi)
      IF (PHI < TRIANGLEMINANGLE .OR. PHI > TRIANGLEMAXANGLE ) THEN     ! TOO SHARP
         JA = 0
      ENDIF
   ENDDO
   RETURN
   END SUBROUTINE CHECKTRIANGLE
