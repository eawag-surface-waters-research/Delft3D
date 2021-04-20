      SUBROUTINE TEKADMIN(X,Y,I,J)
      implicit none
      integer :: i
      integer :: j
      integer :: l
      double precision :: x
      double precision :: y
      CHARACTER TEX*11
      IF (I .LE. 9) THEN
         WRITE(TEX(1:1) ,'(I1)') I
         L = 2
      ELSE IF (I .LE. 99) THEN
         WRITE(TEX(1:2) ,'(I2)') I
         L = 3
      ELSE IF (I .LE. 999) THEN
         WRITE(TEX(1:3) ,'(I3)') I
         L = 4
      ELSE
         WRITE(TEX(1:4) ,'(I4)') I
         L = 5
      ENDIF
      WRITE(TEX(L:L),'(A)') ','
      IF (J .LE. 9) THEN
         WRITE(TEX(L+1:L+1) ,'(I1)') J
         L = L + 1
      ELSE IF (J .LE. 99) THEN
         WRITE(TEX(L+1:L+2) ,'(I2)') J
         L = L + 2
      ELSE IF (J .LE. 999) THEN
         WRITE(TEX(L+1:L+3) ,'(I3)') J
         L = L + 3
      ELSE
         WRITE(TEX(L+1:L+4) ,'(I4)') J
         L = L + 4
      ENDIF
      CALL DRAWTEXT(real(X),real(Y), TEX(1:L))
      RETURN
      END
