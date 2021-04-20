 subroutine copynetnodestosam(jarnod)

 use m_samples
 use m_netw
 use m_missing
 use m_polygon, only: NPL, xpl, ypl, zpl
 use geometry_module, only: dbpinpol

 implicit none
 integer :: in, k, n, jarnod
 real    :: r

 in = -1
 k  = ns

 KC = 0
 do n = 1,numk
    if (jarnod == 1) then
       r = rnod(n)
    else
       r = zk(n)
    endif

    if (r .ne. dmiss) then
       CALL DBPINPOL(XK(n), YK(n), IN, dmiss, JINS, NPL, xpl, ypl, zpl)
       IF (IN == 1) THEN
          KC(N) = 1
          K     = K + 1
       ENDIF
    ENDIF
 ENDDO

 CALL INCREASESAM(k)

 K = NS
 do n = 1,numk
    IF (KC(N) == 1) THEN
       k = k + 1
       xs(k) = xk(n) ; ys(k) = yk(n)
       if (jarnod == 1) then
          zs(k) = rnod(n)
       else
          zs(k) = zk(n)
    endif
    endif
 enddo
 ns = k

end subroutine copynetnodestosam
