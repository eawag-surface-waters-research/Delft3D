      SUBROUTINE TEKGRPT(      X,      Y,     mmax, nmax, MC,     NC,  MP,     NP,   NCOL        )
!     TEKEN GRIDLIJNEN UITKOMEND OP DIT PUNT
      use m_missing
      implicit none
      integer :: mmax, nmax, mc, nc, mp, np, ncol
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)

      double precision :: xp, yp
      integer :: mpu, mpd, npu, npd

      CALL SETCOL(NCOL)
      IF (MP .EQ. 0) RETURN
      XP = X(MP,NP)
      YP = Y(MP,NP)
      IF (XP .EQ. 0) RETURN
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
      RETURN
      END
