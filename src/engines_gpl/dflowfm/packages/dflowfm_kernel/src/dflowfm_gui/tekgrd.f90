      SUBROUTINE TEKgrd(XC, YC, MMAX, NMAX, m1,n1,m2,n2,NCOL,MET,key,MC)
      implicit none
      integer :: mmax, nmax, m1, n1, m2, n2, ncol, met, key, mc
      DOUBLE PRECISION :: XC(MMAX,NMAX), YC(MMAX,NMAX), xlist(nmax), ylist(nmax)

      integer :: i, j, kmax, ja


      IF (MET .EQ. 0 .OR. MC == 0) RETURN
      JA = 0

      CALL SETCOL(NCOL)
      IF (MET .EQ. 2 .OR. MET .EQ. 4) CALL IGRLINETYPE(1)

      KMAX = 8
      DO J = N1,N2
        IF (MOD (J,10) .EQ. 0) CALL HALT2(JA)
        IF (JA .EQ. 1) THEN
           IF (MET .EQ. 2 .OR. MET .EQ. 4) CALL IGRLINETYPE(0)
           RETURN
        ENDIF

        CALL JGRLINE8(Xc(M1,J),Yc(M1,J),M2-M1+1)
      ENDDO

      DO I = M1,M2
        IF (MOD (I,10) .EQ. 0) CALL HALT2(JA)
        IF (JA .EQ. 1) THEN
           IF (MET .EQ. 2 .OR. MET .EQ. 4) CALL IGRLINETYPE(0)
           RETURN
        ENDIF

        xlist(1:N2-N1+1) = xc(i,N1:N2)
        ylist(1:N2-N1+1) = yc(i,N1:N2)
        CALL JGRLINE8(xlist,ylist,N2-N1+1)
      ENDDO

      IF (MET .EQ. 2 .OR. MET .EQ. 4) CALL IGRLINETYPE(0)
      IF (MET .EQ. 5) THEN
         CALL TEKnumnetcells(0)
      ENDIF

      END subroutine tekgrd
