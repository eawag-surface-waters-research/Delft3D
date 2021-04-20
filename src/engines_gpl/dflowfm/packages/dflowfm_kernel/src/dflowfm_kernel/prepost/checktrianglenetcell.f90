   SUBROUTINE CHECKTRIANGLEnetcell(N,JA,phimin,phimax)
   USE M_netw
   USE M_SFERIC
   use m_ec_triangle
   use m_missing, only : dxymis
   use geometry_module, only: dcosphi

   implicit none
   double precision :: phimin,phimax
   integer          :: n,ja

   integer :: k0, k1, k2, n0, n2, nn
   DOUBLE PRECISION :: X0, Y0, X1, Y1, X2, Y2, COSPHI, PHI
   JA = 1
   phimin = 1d3 ; phimax = 0d0
   DO NN = 1,3
      N0 = NN - 1; IF (N0 < 1) N0 = N0 + 3
      N2 = NN + 1; IF (N2 > 3) N2 = N2 - 3
      K0 = netcell(n)%nod(n0)
      K1 = netcell(n)%nod(nn)
      K2 = netcell(n)%nod(n2)
      ! k0 = INDX(N0,N) ; K1 = INDX(NN,N) ; K2 = INDX(N2,N)
      X0 = Xk(K0) ; Y0 = Yk(K0)
      X1 = Xk(K1) ; Y1 = Yk(K1)
      X2 = Xk(K2) ; Y2 = Yk(K2)
      COSPHI = DCOSPHI(X1,Y1,X0,Y0,X1,Y1,X2,Y2, jsferic, jasfer3D, dxymis )
      PHI    = ACOS(min(max(COSPHI,-1d0),1d0))*RD2DG
      phimin = min(phimin, phi)
      phimax = max(phimax, phi)
      IF (PHI < TRIANGLEMINANGLE .OR. PHI > TRIANGLEMAXANGLE ) THEN     ! TOO SHARP
         JA = 0
      ENDIF
   ENDDO
   RETURN
   END SUBROUTINE CHECKTRIANGLEnetcell
