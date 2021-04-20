   SUBROUTINE ADDMAZE(X,Y,Z,N,JAFIVE)    ! FOR FLOW GRIDS
   use m_netw
   use gridoperations

   implicit none
   double precision :: X(N), Y(N), Z(N)
   integer :: N, nn
   integer :: k
   integer :: k2
   integer :: lnu
   INTEGER KK(8), JAFIVE

   DO K = 1,N
      CALL ISNODEDB( KK(k), X(k), Y(k))
      if (kk(k) == 0) then
          numk   = numk + 1
          XK(numk) = X(K) ; YK(numk) = Y(K) ; ZK(numk)  = Z(K) ; KC(numk) = 1; kk(k) = numk
      endif
    enddo

   DO K  = 1,N
      K2 = K+1 ; IF (K .EQ. N) K2 = 1
      CALL CONNECTDB(kk(k),kk(k2),lnu)
   ENDDO

   IF (JAFIVE == 1) THEN
      CALL CONNECTDB(kk(2),kk(4),lnu)
      CALL CONNECTDB(kk(2),kk(5),lnu)
   ENDIF

   RETURN
   END SUBROUTINE ADDMAZE
