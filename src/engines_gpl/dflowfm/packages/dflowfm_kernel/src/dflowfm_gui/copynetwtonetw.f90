 subroutine copynetwtonetw()
 use M_MAPPROPARAMETERS
 use m_netw
 use m_missing
 use m_polygon, only: NPL, xpl, ypl, zpl
 use geometry_module, only: dbpinpol
 use gridoperations

 implicit none
 integer :: in, k, n, L, k0, L0, numkn, numLn, ja


 call savenet()

 call converparameters(ja)

 KC = 0 ; in = -1
 numkn = 0 ; numLn = 0
 do n  = 1,numk
    CALL DBPINPOL(XK(n), YK(n), IN, dmiss, JINS, NPL, xpl, ypl, zpl)
    IF (IN  == 1) THEN
       numkn = numkn + 1
       KC(n) = numkn
    ENDIF
 ENDDO

 do L = 1,numl
    if (kc(kn(1,L)) > 0 .and. kc(kn(2,L)) > 0) then
        numLn = numLn + 1
    endif
 enddo

 K0 = numk ; L0 = numL
 CALL INCREASENETW(K0+NUMKN, L0 + NUMLN)

 KC = 0 ; in = -1          ! redo kc after increasenetw
 numkn = 0 ; numLn = 0
 do n  = 1,numk
    CALL DBPINPOL(XK(n), YK(n), IN, dmiss, JINS, NPL, xpl, ypl, zpl)
    IF (IN  == 1) THEN
       numkn = numkn + 1
       KC(n) = numkn
    ENDIF
 ENDDO

 numLn = 0
 do L  = 1,numl
    if (kc(kn(1,L)) > 0 .and. kc(kn(2,L)) > 0) then
        numLn       = numLn + 1
        kn(1,numLn+L0) = kc( kn(1,L) ) + numk
        kn(2,numLn+L0) = kc( kn(2,L) ) + numk
        kn(3,numLn+L0) =     kn(3,L)
    endif
 enddo

 do n  = 1,numk
    IF (kc(n) > 0) THEN
       xk( kc(n) + numk ) = xk(n) + deltx
       yk( kc(n) + numk ) = yk(n) + delty
       zk( kc(n) + numk ) = zk(n)
    ENDIF
 ENDDO


 numL = L0 + numLn
 numk = K0 + NUMKN

 end subroutine copynetwtonetw
