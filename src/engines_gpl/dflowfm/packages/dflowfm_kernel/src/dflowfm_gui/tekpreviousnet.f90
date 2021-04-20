      SUBROUTINE TEKPREVIOUSNET(NCOL)
      use m_netw
      implicit none
      integer :: NCOL

      integer :: k
      integer :: k1
      integer :: k2
      integer :: l
      integer :: ndraw

      COMMON /DRAWTHIS/ ndraw(50)
      IF (NDRAW(16) .LE. 0) RETURN
      CALL SETCOL(NCOL)
      DO L = 1,NUML0
         K1 = KN0(1,L)
         K2 = KN0(2,L)
         IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
            CALL DMOVABS( XK0(K1),YK0(K1),ZK0(K1) )
            CALL  DLNABS( XK0(K2),YK0(K2),ZK0(K2) )
         ENDIF
      ENDDO
      END SUBROUTINE TEKPREVIOUSNET
