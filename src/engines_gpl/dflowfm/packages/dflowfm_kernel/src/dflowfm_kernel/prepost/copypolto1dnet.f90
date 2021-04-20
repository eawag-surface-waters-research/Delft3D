  SUBROUTINE COPYPOLTo1Dnet()
  use m_polygon
  USE M_netw
  USE M_MISSING
  use network_data, only: kn3typ
  implicit none

  integer :: k, L, kn3o

  kn3o = kn3typ ; kn3typ = 1

  ! CALL INCREASENETW(NUMK+NPL, NUML+NPL-1)
  DO K = 2,NPL

     if (xpl(k) .ne. dmiss .and. xpl(K-1) .ne. dmiss) then
         call addnetlink(xpl(k-1), ypl(k-1), xpl(k), ypl(k), L)
     endif

  ENDDO

  kn3typ = kn3o
  CALL DELPOL()
  RETURN
  END SUBROUTINE COPYPOLTo1Dnet
