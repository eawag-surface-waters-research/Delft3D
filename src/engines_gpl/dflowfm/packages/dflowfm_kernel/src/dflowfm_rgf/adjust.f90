      SUBROUTINE ADJUST( X, Y, mmax, nmax, MC, NC)
      USE m_missing
      implicit none
      integer :: mmax, nmax, mc, nc
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)
! TODO: Z not present, no filling with dmiss [AvD]
!     schuif data naar links en of beneden en geef nieuwe MC,NC

      integer :: i, j, ifirst, jfirst
      double precision, allocatable :: XH(:,:), YH(:,:)
      allocate(xh(MMAX,NMAX), YH(MMAX,NMAX))

      xh = x
      yh = y
      x = xymis
      y = xymis

      IFIRST = 0
      DO 10 I = 1,MC
         DO 10 J = 1,NC
            IF (XH(I,J) .NE. XYMIS .AND. IFIRST .EQ. 0) IFIRST = I
    10 CONTINUE

      JFIRST = 0
      DO 20 J = 1,NC
         DO 20 I = 1,MC
            IF (XH(I,J) .NE. XYMIS .AND. JFIRST .EQ. 0) JFIRST = J
    20 CONTINUE

      IF (IFIRST .EQ. 0 .OR. JFIRST .EQ. 0) THEN
         MC = 0
         NC = 0
      ELSE
         IFIRST = IFIRST - 1
         JFIRST = JFIRST - 1
         DO 30 I = 1,MC-IFIRST
            DO 30 J = 1,NC-JFIRST
               X(I,J) = XH(I+IFIRST,J+JFIRST)
               Y(I,J) = YH(I+IFIRST,J+JFIRST)
    30   CONTINUE
         CALL NUMS(X,mmax, nmax, MC,NC)
      ENDIF

      deallocate(xh, yh)
      RETURN
      END
