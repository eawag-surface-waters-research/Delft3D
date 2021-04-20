    SUBROUTINE POLYLINE(XR,YR,N)
    use unstruc_opengl
    implicit none
    integer :: n, I
    real xr(N), yr(N)

    IF (InOpenGLRendering) THEN
      CALL MOVABSNOP(dble(XR(1)),dble(YR(1)))
      DO 10 I = 2,N
         call LNABSNOP(dble(XR(I)),dble(YR(I)))
      10 CONTINUE
    ELSE
         CALL IGRPOLYLINE(XR,YR,N)
    ENDIF

    END SUBROUTINE
