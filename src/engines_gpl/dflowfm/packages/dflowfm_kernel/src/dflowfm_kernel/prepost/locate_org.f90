!> original locate
  SUBROUTINE LOCATE_ORG(XX,N,X,J)
   INTEGER          :: N, J
   DOUBLE PRECISION :: XX(N), X

   INTEGER          :: JL, JU, JM

   JL=0
   JU=N+1
10 IF(JU-JL.GT.1)THEN
     JM=(JU+JL)/2
     IF((XX(N).GT.XX(1)).EQV.(X.GT.XX(JM)))THEN
       JL=JM
     ELSE
       JU=JM
     ENDIF
     GO TO 10
   ENDIF
   J=JL
   RETURN
   END
