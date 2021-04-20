     SUBROUTINE PAKTIJ(T,mmax, nmax, TH,imax,I1,I2,J1,J2,NUM)
       implicit none
!     Haal lijn uit array en geef aantal niet nul NUM
     integer :: mmax, nmax, imax, i1, i2, j1, j2, num
      double precision :: T(MMAX,NMAX),TH(IMAX)
      integer :: i, j, k, ji1
      TH = 0d0
      K   = 0
      JI1 = 0
      DO 10 I  = I1,I2
         DO 10 J  = J1,J2
         IF (T(I,J) .NE. 0) THEN
            K     = K + 1
            TH(K) = T(I,J)
         ENDIF
    10 CONTINUE
      NUM = K
      RETURN
      END subroutine paktij
