  LOGICAL FUNCTION INVIEW(X,Y)
  ! ZIT IK IN ZOOMGEBIED? NULLEN EN DEFAULTS NIET, IN WERELDCOORD inview3
  use m_wearelt
  use m_missing
  doubleprecision :: x,y,xx,yy
  INVIEW = .FALSE.
  IF (X .NE. XYMIS) THEN
     CALL dPROJECT(X,Y,XX,YY,1)
     IF (XX .GT. X1 .AND. XX .LT. X2 .AND. YY .GT. Y1 .AND. YY .LT. Y2     ) THEN
        INVIEW = .TRUE.
     ENDIF
  ENDIF
  RETURN
  END
