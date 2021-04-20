     SUBROUTINE delgrd(KEY,JASAVE,jadelpol)
!    delete grid
     use m_grid
     use m_missing
     use m_polygon, only: NPL, xpl, ypl, zpl
     use geometry_module, only: dbpinpol

     implicit none
     integer                :: inhul, ja, i, j
     integer, intent(in)    :: jasave, jadelpol
     integer, intent(inout) :: key
     double precision       :: xi, yi

     inhul = -1

     IF (JASAVE .EQ. 1) CALL SAVEgrd()
     KEY = 3
     IF (NPL .LE. 2) THEN
        IF (NPL .GE. 1) THEN
           CALL CONFRM('NO POLYON, SO DELETE all GRID POINTS ? ',JA)
           IF (JA .EQ. 0) THEN
              KEY = 0
           ELSE
              XC = 0d0 ; YC = 0d0; MC = 0 ; NC = 0
           ENDIF
        ELSE
           XC = 0d0 ; YC = 0d0; MC = 0 ; NC = 0
        ENDIF
        RETURN
     ENDIF

     DO 10 I = 1,MC
        DO 10 J = 1,NC
           IF (Xc(I,J) .NE. DXYMIS) THEN
              CALL dbpinpol(Xc(i,j), yc(i,j), INHUL, dmiss, JINS, NPL, xpl, ypl, zpl)
              IF (INHUL .EQ. 1) Xc(I,J) = XYMIS
           ENDIF
   10 CONTINUE

!      CALL ADJUST(X, Y, MC, NC, WW1, WW2)
     if (jadelpol == 1) call delpol()
     RETURN
     END SUBROUTINE delgrd
