SUBROUTINE getwavenrqn(DEPTH,Period, RK)
use m_sferic
implicit none
double precision :: PERIOD,DEPTH,RK
double precision :: OMEGAS, GR, RLAB0, DEP2PI,  RLAB1, rlab2, criter
OMEGAS = TWOPI / PERIOD
GR     = 9.81
RLAB0  = TWOPI*GR / OMEGAS**2
DEP2PI = TWOPI*DEPTH
RLAB1  = RLAB0*SQRT( TANH(DEP2PI / RLAB0) )

10 CONTINUE
   RLAB2  = RLAB0*TANH(DEP2PI/RLAB1)
   CRITER = (RLAB2 - RLAB1) / RLAB1
   IF (ABS(CRITER) .LT. 0.001) THEN
      RK = TWOPI/RLAB2
      RETURN
   ELSE
      RLAB1 = RLAB2
      GOTO 10
   ENDIF

end SUBROUTINE getwavenrqn
