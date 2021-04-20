  SUBROUTINE REFINECELLSONLY()
  use m_netw
  USE M_POLYGON
  use gridoperations
  implicit none

  integer :: ja
  integer :: k
  integer :: k1
  integer :: kp
  integer :: lnu
  integer :: n
  integer :: nn

  DOUBLE PRECISION :: XL, YL, ZL = 0D0

  CALL FINDCELLS(0)

  DO N  = 1,NUMP
     CALL ALLIN(N,JA)
     IF (JA == 0) CYCLE
     CALL GETAVCOR    (N,XL,YL,ZL)
     CALL dSETNEWPOINT(XL,YL,KP)
     NN = netcell(N)%N
     DO K  = 1,NN
        K1 = netcell(N)%NOD(K)
        CALL CONNECTDB(KP,K1,LNU)
     ENDDO
  ENDDO

  CALL SETNODADM(0)

  END SUBROUTINE REFINECELLSONLY
