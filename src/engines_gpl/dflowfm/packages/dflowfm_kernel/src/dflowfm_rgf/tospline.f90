       SUBROUTINE TOSPLINE(XX, YY, XV, YV)
       USE M_SPLINES
       implicit none

       double precision :: XX, YY, XV, YV

       double precision :: XI(maxsplen), XI2(maxsplen), YI(maxsplen), YI2(maxsplen)
       double precision :: TV, DIS
       integer :: IN, NUMPI

       IN = 1 ! Pick first spline
       CALL NUMP(IN, NUMPI)
       TV = NUMPI/2d0
       CALL GETIJ (XSP, XI, maxspl, maxsplen, maxsplen, IN, IN,  1, NUMPI)
       CALL GETIJ (YSP, YI, maxspl, maxsplen, maxsplen, IN, IN,  1, NUMPI)
       CALL SPLINE(XI,NUMPI,XI2)
       CALL SPLINE(YI,NUMPI,YI2)
       CALL DISMIN(XI,XI2,YI,YI2,XX,YY,NUMPI, DIS,TV,XV,YV)
       RETURN
       END subroutine tospline
