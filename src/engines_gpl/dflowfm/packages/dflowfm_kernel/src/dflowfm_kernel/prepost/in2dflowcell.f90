 subroutine in2Dflowcell(xp,yp,k)                      ! is this point in a 2Dflowcell

 use m_flowgeom
 use m_flow
 use m_flowexternalforcings
 use geometry_module, only: pinpok
 use m_missing, only: jins, dmiss

 implicit none

 double precision  :: xp, yp
 integer           :: k

 ! locals
 integer           :: n, nn, in, kb, L
 double precision  :: dxx, dyy, r

 k = 0
 do n = 1,ndx2D
     nn = size( nd(n)%x )
     IF (NN > 2) THEN
        call PINPOK (Xp, Yp, Nn, nd(n)%x, nd(n)%y, IN, jins, dmiss)
        if (in == 1) then
           k = n
           return
        endif
     ENDIF
 enddo
 end subroutine in2Dflowcell
