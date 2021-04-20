     !> Checks spline points in X and Y.
     !! Counts the number of splines and the maximum length and moves all
     !! All splines with <=1 point are reset and moved to the back.
     SUBROUTINE CHECKSPL(X, Y, mmax, nmax, MCS, NCS)
      USE m_missing
      implicit none
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)
      integer :: mmax, nmax, mcs, ncs

      integer :: numspl, numpx, numpi, numpj, i, j, k

!    5CONTINUE
      NUMSPL  = 0
      NUMPX   = 0
      DO 10 I = 1,MMAX-1
         CALL NUMPold(X,mmax, nmax, I,NUMPI)
         IF (NUMPI .LE. 1) THEN
            DO 15 K = 1,NMAX
               X(I,K) = XYMIS
               Y(I,K) = XYMIS
    15      CONTINUE
            DO 20 J = I+1,MMAX
               CALL NUMPold(X,mmax, nmax, J,NUMPJ)
               IF (NUMPJ .GT. 1) THEN
                  CALL CHAROW(X,mmax, nmax,  J, J-1, NMAX)
                  CALL CHAROW(Y,mmax, nmax,  J, J-1, NMAX)
               ENDIF
    20      CONTINUE
         ELSE IF (NUMPI .GE. 2) THEN
            NUMPX  = MAX(NUMPX,NUMPI)
            NUMSPL = NUMSPL + 1
         ENDIF
    10 CONTINUE
      MCS = NUMSPL
      NCS = NUMPX
      RETURN
      END subroutine checkspl
