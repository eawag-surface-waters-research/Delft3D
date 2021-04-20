      SUBROUTINE  DXYB(      X,      Y,     mmax, nmax, MC,            &
                            NC,     II,     JJ,     IN,                &
                            JN,   DXY0                )
      use m_missing
      use geometry_module, only: dbdistance
      use m_sferic, only: jsferic, jasfer3D

      implicit none
      integer :: mmax, nmax, mc, nc, ii, jj, in, jn
      double precision :: dxy0
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)

      integer :: num
      double precision :: XU, YU, XD, YD, dxy1
      NUM  = 0
      DXY0 = 0

      IF (II+IN .LE. MC .AND. JJ+JN .LE. NC) THEN
         XU = X(II+IN,JJ+JN)
         IF (XU .NE. XYMIS) THEN
            YU   = Y(II+IN,JJ+JN)
            dxy0 = dbdistance(X(II,JJ),Y(II,JJ),XU,YU,jsferic, jasfer3D, dmiss)
            NUM  = NUM + 1
         ENDIF
      ENDIF

      IF (II-IN .GE. 1 .AND. JJ-JN .GE. 1) THEN
         XD = X(II-IN,JJ-JN)
         IF (XD .NE. XYMIS) THEN
            YD   = Y(II-IN,JJ-JN)
            dxy1 = dbdistance(X(II,JJ),Y(II,JJ),XD,YD,jsferic, jasfer3D, dmiss)
            NUM  = NUM + 1
            DXY0 = (DXY0 + DXY1) / dble(NUM)
         ENDIF
      ENDIF

      RETURN
      END subroutine dxyb
