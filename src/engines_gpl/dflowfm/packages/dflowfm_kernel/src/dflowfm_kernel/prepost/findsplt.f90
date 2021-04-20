      SUBROUTINE FINDSPLT(X,Y,X2,Y2,MMAX,MFAC,MCS,TS,DS,XS,YS,JA)
      implicit none
      integer :: ja
      integer :: mcs
      integer :: mfac
      integer :: mmax
      DOUBLE PRECISION :: X(MMAX), Y(MMAX), X2(MMAX), Y2(MMAX), TS, DS, XS, YS
      DOUBLE PRECISION :: TA, XA, TB, XB, YA, YB, DMF, DB, DA, DX, DY
!     TS is de administratieve start zoekindex tussen 0 en MCS
!     DS is de te zoeken afstand vanaf punt TS
      JA  = 1
      DMF = 0.5d0 / dble(MFAC)
      DB  = 0
      DA  = 0
      TA  = TS
      CALL SPLINT(X,X2,MCS,TA,XA)
      CALL SPLINT(Y,Y2,MCS,TA,YA)
   10 CONTINUE
         TB = TA + DMF
         TB = MIN(TB,dble(MCS-1))
         CALL SPLINT(X,X2,MCS,TB,XB)
         CALL SPLINT(Y,Y2,MCS,TB,YB)
         DX  = XB - XA
         DY  = YB - YA
         DB  = DB + SQRT(DX*DX + DY*DY)
         IF (TB .LT. MCS-1) THEN
            IF (DB .LT. DS) THEN
               TA = TB
               DA = DB
               XA = XB
               YA = YB
            ELSE
               TS = TA + DMF*(DS-DA)/(DB-DA)
               CALL SPLINT(X,X2,MCS,TS,XS)
               CALL SPLINT(Y,Y2,MCS,TS,YS)
               JA = 1
               RETURN
            ENDIF
         ELSE
            JA = 0
            RETURN
         ENDIF
      GOTO 10
      END SUBROUTINE FINDSPLT
