  SUBROUTINE JGTEXT(TEX,X,Y,NCOL,WIC,HIC,JAHOOG) ! grafische tekst, grafische posities, met kleurblokjes ERONDER
    use unstruc_colors
  implicit none
  double precision :: hic, WIC
  integer :: jahoog
  integer :: ncol
  integer :: ndraw
  double precision :: x
  double precision :: xa
  double precision :: xb
  double precision :: xp
  double precision :: y
  double precision :: ya
  double precision :: yb
  double precision :: yp
  CHARACTER TEX*(*)
  COMMON /DRAWTHIS/  ndraw(50)

  CALL SETCOL(KLTEX)
  CALL DRAWTEXT(real(X),real(Y),TEX)
  CALL GETPOS(XP,YP)

  XA = XP + 0.3d0*WIC
  YA = YP - 0.8d0*HIC + JAHOOG*HIC
  XB = XA + 1.3d0*WIC
  YB = YA + 0.7d0*HIC

  IF (NCOL .NE. 0) THEN
     CALL SETCOL(NCOL)
     IF (JAHOOG .EQ. 0) THEN
        CALL FBOXnop(XA,YA,XB,YB)
        CALL SETCOL(KLTEX)
        CALL  BOXnop(XA,YA,XB,YB)
     ELSE
        CALL FBOXnop(XA,YA,XB,YB)
     ENDIF
  ENDIF
  RETURN
  END
