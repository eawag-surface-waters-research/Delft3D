  SUBROUTINE ISNODE(KP, XP, YP, ZP)

  use m_netw
  use m_wearelt
  use m_missing
  use m_sferic
  use m_sferzoom

  implicit none

  integer :: KP
  double precision :: XP, YP, ZP

  integer :: ll
  double precision :: xkk, ykk, zkk, rcx,rcy,dis
  integer :: K, KPREV

  IF (KP < 0) THEN
     KPREV = IABS(KP)
  ELSE
     KPREV = 0
  ENDIF

  if (jsfertek > 0) then
       rcy = cr*dyh*ra*dg2rd
     ! call setrcirxy(xp,yp,rcx,rcy)
  endif

  KP = 0
  ZP = dmiss
  DO K = 1,NUMK
     if (jsfertek > 0) then
        call dbdistancehk(xk(k),yk(k),xp,yp,dis)
        if (dis< rcy) then
           kp = k
        endif
     else
        IF (ABS(XK(k)-XP) .LT. rcir .AND. ABS(YK(k)-YP) .LT. rcir) THEN
            KP = K
        endif
     endif
     if (kp > 0) then
         CALL DISPNODE(KP)
         ZP  = ZK(kp)
!         XYZ = ZKK
         RETURN
     ENDIF
  ENDDO

  END SUBROUTINE ISNODE
