  SUBROUTINE TEKBOTTOM(MET)
  use m_wearelt
  implicit none
  double precision :: dz
  integer :: i
  integer :: jav
  integer :: jview
  integer :: k
  integer :: k1
  integer :: k2
  integer :: nz
  double precision :: uf
  double precision :: vf
  double precision :: wd
  double precision :: wf
  double precision :: xyz
  double precision :: ybot
  double precision :: ytop
  integer :: MET
  COMMON /FLOWSTUFF/ UF, VF, WF, YBOT, YTOP
  COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
  DOUBLE PRECISION XD,YD,ZD,XX1,XX2,ZZ1,ZZ2
  CALL SETCOL(160)
  IF (MET .EQ. 1) RETURN

  WD  = 1000
  XX2 = WD/2
  XX1 = -XX2
  ZZ2 = WD/2
  ZZ1 = -ZZ2
  DZ = 0
  NZ = 1

  IF (JVIEW .GE. 3) THEN
      NZ = 11
      DZ = WD / (NZ-1)
  ENDIF

  IF (MET .EQ. 2) THEN
     K1 = 1
     K2 = 2
  ELSE IF (MET .EQ. 3) THEN
     K1 = 2
     K2 = 2
  ELSE IF (MET .EQ. 4) THEN
     K1 = 1
     K2 = 1
  ENDIF

  YD = YTOP
  CALL SETCOL(128) ! (112)
  DO K = K1,K2
     IF (K .EQ. 2) THEN
        YD = YBOT
        CALL SETCOL(89) ! 128)
     ENDIF
     XD  = XX1
     ZD  = ZZ1
     DO I = 1,NZ
        CALL DMOVABS( XX1, YD, ZD)
        CALL DLNABS ( XX2, YD, ZD)
        CALL DMOVABS(  XD, YD, ZZ1)
        CALL DLNABS (  XD, YD, ZZ2)
        ZD = ZD + DZ
        XD = XD + DZ
     ENDDO
  ENDDO

  RETURN
  END SUBROUTINE TEKBOTTOM
