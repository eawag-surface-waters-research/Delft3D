      SUBROUTINE SPLINTXY(X,Y,X2,Y2,N,T,XT,YT)
      implicit none
      !USE DIMENS
      integer :: n
      double precision :: T
      double precision :: X(N), Y(N), X2(N), Y2(N)
      double precision :: xt, yt

      CALL SPLINT(X,X2,N,T,XT)
      CALL SPLINT(Y,Y2,N,T,YT)
      RETURN
      END
