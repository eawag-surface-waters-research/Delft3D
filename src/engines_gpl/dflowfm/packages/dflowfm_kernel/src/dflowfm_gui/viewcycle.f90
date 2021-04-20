  SUBROUTINE VIEWCYCLE(KEY)
  implicit none
  double precision :: deltx
  double precision :: delty
  double precision :: deltz
  double precision :: dscr
  integer :: jav
  integer :: jview
  double precision :: wpqr
  double precision :: xyz
  double precision :: zfac
  double precision :: zupw
  integer :: KEY
  COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
  COMMON /PERSPX/ WPQR,DELTX,DELTY,DELTZ,ZFAC,DSCR,ZUPW
  JVIEW = JVIEW + 1
  IF (JVIEW .GT. JAV) JVIEW = 1
  KEY   = 3
  RETURN
  END SUBROUTINE VIEWCYCLE
