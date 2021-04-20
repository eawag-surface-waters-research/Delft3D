  SUBROUTINE REFINELINES()
  use m_netw
  USE M_GRIDSETTINGS
  use m_missing, only: dmiss, jins
  use geometry_module, only: pinpok
  implicit none

  integer :: INL
  integer :: k1
  integer :: k2
  integer :: l
  integer :: lnu
  double precision :: a0, r0, XX, YY, ZZ

  IF (MFAC .LE. 1) RETURN

  DO L  = 1,NUML
     K1 = KN(1,L)
     K2 = KN(2,L)
     XX = 0.5D0*( XK(K1) + XK(K2) )
     YY = 0.5D0*( YK(K1) + YK(K2) )
     ZZ = 0.5D0*( ZK(K1) + ZK(K2) )

     CALL PINPOK( XX, YY, NPL, XPL, YPL, INL, jins, dmiss)
     IF (INL .EQ. 1) THEN

        CALL DELELEM(K1,K2,LNU)
        CALL CONNECT(K1,K2,mFAC,A0, R0)

     ENDIF

  ENDDO

  RETURN
  END SUBROUTINE REFINELINES
