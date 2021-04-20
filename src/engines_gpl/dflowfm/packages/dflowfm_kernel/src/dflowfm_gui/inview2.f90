  LOGICAL FUNCTION INVIEW2(X,Y,XX,YY)
  USE M_MISSING
  use m_wearelt
  implicit none
  double precision :: x,y,xx,yy

  ! ZIT IK IN ZOOMGEBIED? NULLEN EN DEFAULTS NIET, IN WERELDCOORD

  INVIEW2 = .FALSE.
  IF (X .NE. XYMIS) THEN
     CALL dPROJECT(X,Y,XX,YY,1)
     IF (XX .GT. X1 .AND. XX .LT. X2 .AND.  &
         YY .GT. Y1 .AND. YY .LT. Y2     ) THEN
        INVIEW2 = .TRUE.
     ENDIF
  ELSE
     XX = XYMIS
     YY = XYMIS
  ENDIF
  RETURN
  END FUNCTION INVIEW2
