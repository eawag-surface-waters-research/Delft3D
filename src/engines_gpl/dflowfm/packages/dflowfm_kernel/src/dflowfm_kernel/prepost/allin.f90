   SUBROUTINE ALLIN(N,JA)
   use m_netw
   implicit none
   integer :: n
   integer :: ja

   integer :: k
   integer :: i
   integer :: nn
   JA = 1
   NN = netcell(N)%N
   DO I = 1,NN
      K = netcell(N)%NOD(I)
      IF ( KC(K) .NE. 1) THEN
         JA = 0 ; RETURN
      ENDIF
   ENDDO
   END SUBROUTINE ALLIN
