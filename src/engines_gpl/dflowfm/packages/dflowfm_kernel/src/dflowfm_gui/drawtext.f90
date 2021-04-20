    SUBROUTINE DRAWTEXT(X,Y,TEX)
    use unstruc_opengl
    implicit none
    real :: x, y
    CHARACTER TEX*(*)

    IF (InOpenGLRendering) THEN
        CALL RenderText(X,Y,TEX)
    ELSE
        CALL IGRCHAROUT(X,Y,TEX)
    ENDIF
    END SUBROUTINE
