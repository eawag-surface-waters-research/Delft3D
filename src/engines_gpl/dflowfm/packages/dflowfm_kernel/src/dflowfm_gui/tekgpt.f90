      SUBROUTINE TEKGPT(      X,      Y,     mmax, nmax, MC,     NC, &
                            MP,     NP,   NCOL,   RD1)
!     TEKEN GRIDLIJNEN UITKOMEND OP DIT PUNT
      use m_missing
      use m_wearelt
      implicit none
      integer :: mmax, nmax, mc, nc, mp, np, ncol
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX), RD1(MMAX,NMAX)


      double precision :: XP, YP
      integer :: MPU, MPD, NPU, NPD, ncolcir
      XP = X(MP,NP)
      IF (XP .EQ. XYMIS) RETURN
      YP = Y(MP,NP)
      CALL MOVABS(XP,YP)
      CALL SETCOL(NCOL)
      MPU = MP + 1
      MPD = MP - 1
      NPU = NP + 1
      NPD = NP - 1
      IF (MPU .LE. MC) THEN
         IF (X(MPU,NP) .NE. XYMIS) THEN
            CALL MOVABS(X(MPU,NP),Y(MPU,NP))
            CALL LNABS(XP,YP)
         ENDIF
      ENDIF
      IF (MPD .GE. 1) THEN
         IF (X(MPD,NP) .NE. XYMIS) THEN
            CALL MOVABS(X(MPD,NP),Y(MPD,NP))
            CALL LNABS(XP,YP)
         ENDIF
      ENDIF
      IF (NPU .LE. NC) THEN
         IF (X(MP,NPU) .NE. XYMIS) THEN
            CALL MOVABS(X(MP,NPU),Y(MP,NPU))
            CALL LNABS(XP,YP)
         ENDIF
      ENDIF
      IF (NPD .GE. 1) THEN
         IF (X(MP,NPD) .NE. XYMIS) THEN
            CALL MOVABS(X(MP,NPD),Y(MP,NPD))
            CALL LNABS(XP,YP)
         ENDIF
      ENDIF
      CALL SETXOR(0)
      IF (RD1(MP,NP) .NE. DMISS) THEN
         CALL ISOCOL(RD1(MP,NP),NCOLCIR)
         CALL CIR(RCIR)
         CALL SETCOL(0)
         CALL PTABS(XP,YP)
      ENDIF
      CALL SETXOR(1)
      RETURN
      END subroutine tekgpt
