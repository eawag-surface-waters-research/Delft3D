  SUBROUTINE ADDBLOCK(X,Y,Z,JANET)
  use gridoperations
  implicit none
  integer :: ja
  integer :: jav
  integer :: jview
  integer :: k
  integer :: n
  double precision :: xyz
  double precision :: X(4), Y(4), Z
  integer :: JANET

  COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
  INTEGER KK(8)
  DO K = 1,8
     N = K
     IF (K .EQ. 5) THEN
        Z = Z + 1D0
     ENDIF
     IF (K .GE. 5) N = K - 4
     CALL ISNODE2(KK(K), X(N), Y(N), Z)
     IF (KK(K) .LE. 0) THEN
        CALL GIVENEWNODENUM(KK(K))
        XYZ = Z
        CALL SETPOINT(X(N),Y(N),Z,KK(K))
     ENDIF
  ENDDO
  CALL ADDELEM(KK(1),KK(2),JA)
  CALL ADDELEM(KK(2),KK(3),JA)
  CALL ADDELEM(KK(3),KK(4),JA)
  CALL ADDELEM(KK(4),KK(1),JA)

  CALL ADDELEM(KK(5),KK(6),JA)
  CALL ADDELEM(KK(6),KK(7),JA)
  CALL ADDELEM(KK(7),KK(8),JA)
  CALL ADDELEM(KK(8),KK(5),JA)

  CALL ADDELEM(KK(1),KK(5),JA)
  CALL ADDELEM(KK(2),KK(6),JA)
  CALL ADDELEM(KK(3),KK(7),JA)
  CALL ADDELEM(KK(4),KK(8),JA)

  CALL ADDELEM(KK(1),KK(3),JA)
  CALL ADDELEM(KK(5),KK(7),JA)
  CALL ADDELEM(KK(4),KK(5),JA)
  CALL ADDELEM(KK(3),KK(6),JA)
  CALL ADDELEM(KK(4),KK(7),JA)
  CALL ADDELEM(KK(1),KK(6),JA)
  RETURN
  END SUBROUTINE ADDBLOCK
