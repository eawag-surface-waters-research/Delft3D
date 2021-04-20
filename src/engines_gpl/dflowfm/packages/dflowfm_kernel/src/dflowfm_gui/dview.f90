   SUBROUTINE DVIEW(XD,YD,ZD,X,Y,Z)
   use m_missing
   implicit none
   double precision :: ce
   integer :: i
   double precision :: vs
   double precision :: x0s
   double precision :: y0s
   ! GEEF perspectievische COORDINATEN
   ! xD,yD,zD                             :coordinaten te tekenen punt
   ! x0s,y0s                              :waar op scherm ligt kijklijn
   ! X,Y,Z                                :scherm coordinaten
   ! Vs                                   :viewing matrix na viema

   DOUBLE PRECISION XD,YD,ZD,X,Y,Z
   COMMON /VIEWMAT/ VS(4,4), X0S, Y0S
   DIMENSION CE(4)
   ! use z as zd temporarily (zet to zero when zd==dmiss)
   if (zd == dmiss) then
      z = 0
   else
      z = zd
   end if
   DO I = 1,3
      CE(I) = VS(I,1)*XD + VS(I,2)*YD + VS(I,3)*Z + VS(I,4)
   ENDDO
   Z  = CE(3)
   IF (Z .LT. 0) THEN
      Z = dmiss
   ELSE
      X = CE(1)/Z  + X0S
      Y = CE(2)/Z  + Y0S
   ENDIF
   END SUBROUTINE DVIEW
