   SUBROUTINE RELINK()

   use m_netw
   use m_ec_triangle
   use gridoperations
   use m_polygon
   use gridoperations

   use m_ec_basic_interpolation, only: dlaun

   implicit none

   double precision :: af
   integer :: ierr
   integer :: ja
   integer :: k
   integer :: k1
   integer :: k1l
   integer :: k2
   integer :: k2l
   integer :: ki
   integer :: l
   integer :: ll
   integer :: n
   integer :: n1
   integer :: n2
   integer :: new
   integer :: nn

   INTEGER, ALLOCATABLE :: KIN(:)
   double precision, ALLOCATABLE :: X(:), Y(:)

   ALLOCATE ( KIN(NUMK), X(NUMK), Y(NUMK) , STAT=IERR)
   CALL AERR('KIN(NUMK), X(NUMK), Y(NUMK)', IERR, 3*NUMK)

   CALL DSELECTINP(XK,YK,NUMK,KIN)

   KI = 0
   DO K = 1,NUMK
      IF (KIN(K) > 0) THEN
         KI = KI + 1
         X(KI)   = XK(K)
         Y(KI)   = YK(K)
         KIN(KI) = K
      ENDIF
   ENDDO


   CALL READYY('TRIANGULATING', 0d0)

   CALL DLAUN(X,Y,KI,1,ierr)

   CALL READYY('TRIANGULATING', 0.3d0)

   CALL DELLINKSINPOL()

   L = NUML
   DO N = 1,NUMTRI
      AF = 0.3d0 + 0.7d0*dble(N)/dble(NUMTRI)
      CALL READYY('TRIANGULATING', AF)


      JA = 1
      ! CALL CHECKTRIANGLE(N,JA)
      IF (JA == 0) THEN
         CYCLE
      ENDIF
      DO NN = 1,3
         N1 = NN ; N2 = N1 + 1 ; IF (N1 == 3) N2 = 1
         K1 = INDX(N1,N) ; K2 = INDX(N2,N)
         K1 = KIN(K1)    ; K2 = KIN(K2)

         NEW = 1
         DO LL  = NUML, 1, -1
            K1L = KN(1,LL) ; K2L = KN(2,LL)
            IF (K1 .EQ. K1L .AND. K2 .EQ. K2L .OR.    &
                K2 .EQ. K1L .AND. K1 .EQ. K2L ) THEN
                NEW = 0 ; EXIT
            ENDIF
         ENDDO

         IF (NEW .EQ. 0) CYCLE

         L = L + 1 ;
         IF (L > LMAX) THEN
            CALL INCREASENETW(INT(1.2*NUMK), INT(1.2*NUML) )
         ENDIF
         NUML = L
         KN(1,L) = K1 ; KN(2,L) = K2

      ENDDO
   ENDDO

   DEALLOCATE (KIN)

   CALL SETNODADM(1)

   CALL READYY('TRIANGULATING', -1d0)

   RETURN
   END SUBROUTINE RELINK
