   SUBROUTINE DSELECTINP(X,Y,N,KIN)
   USE M_POLYGON
   use m_missing, only: dmiss, jins
   use geometry_module, only: dpinpok
   implicit none
   integer :: N
   DOUBLE PRECISION :: X(N), Y(N), ZK
   INTEGER          :: KIN(N)

   integer :: in
   integer :: k
   double precision :: xmaxp
   double precision :: xminp
   double precision :: ymaxp
   double precision :: yminp
   ZK = 1D0

   IF (NPL < 3) THEN
      KIN  = 1
   ELSE
      CALL MINMAXPOL(XMINp, YMINp, XMAXp, YMAXp)
      DO K  = 1,N
         IN = 0
         IF (X(K) >= XMINp .AND. X(K) <= XMAXp .AND. Y(K) >= YMINp .AND. Y(K) <= YMAXp ) THEN
            CALL DPINPOK(X(K), Y(K), ZK, NPL, XPL, YPL, IN, jins, dmiss)
         ENDIF
         KIN(K) = IN
      ENDDO
   ENDIF
   END SUBROUTINE DSELECTINP
