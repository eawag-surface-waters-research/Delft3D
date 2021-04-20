      SUBROUTINE HITEXT(IVAL,X,Y)
      implicit none
      integer :: ival
      integer :: l
      integer :: ncolnow
      double precision :: x
      double precision :: y
!     INTEGER grafisch scherm in current color
      CHARACTER TEX*8
      COMMON /COLNOW/ NCOLNOW
      IF (NCOLNOW .GE. 0) THEN
         IF (abs(IVAL) < 100) THEN
            WRITE(TEX,'(I3)') IVAL
         ELSE IF (abs(IVAL) < 10000) THEN
            WRITE(TEX,'(I5)') IVAL
         ELSE
            WRITE(TEX,'(I8)') IVAL
         ENDIF
         L = len_trim(TEX)
         CALL DRAWTEXT(real(X),real(Y), TEX(1:L))
      ENDIF
      RETURN
      END
