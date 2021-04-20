  SUBROUTINE NEWLINK(K1,K2,LNU) ! no checks
  use m_netw
  use unstruc_colors
  use gridoperations
  implicit none
  integer :: K1, K2, LNU

  NUML = NUML + 1
  IF (NUML >= LMAX) THEN
     CALL INCREASENETW(NUMK,NUML)
  ENDIF
  KN(1,NUML) = K1
  KN(2,NUML) = K2
  KN(3,NUML) = KN3TYP

  LNU = NUML

  CALL TEKLINK(NUML,NCOLDN)

  END SUBROUTINE NEWLINK
