    SUBROUTINE PFILLERCORE(XR,YR,N)
    use unstruc_opengl
    implicit none
    integer :: n
    real xr(N), yr(N)

    IF (InOpenGLRendering) THEN
        CALL FillPolygon(xr,yr,n)
    ELSE
        if (n .le. 4) then
            call igrpolygonsimple(xr,yr,n)
        else
            CALL IGrPolygoncomplex(Xr,Yr,N)
        endif
    ENDIF

    END SUBROUTINE
