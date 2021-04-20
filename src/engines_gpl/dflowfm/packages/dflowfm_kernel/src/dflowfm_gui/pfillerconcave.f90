    SUBROUTINE PFILLERconcave(X,Y,N_,NCOL,NCLR)
    use unstruc_opengl
    implicit none
    integer :: N_
    integer :: nclr
    integer :: ncol
    integer :: ncolnow
    integer :: ndraw
    double precision :: X(N_), Y(N_)
    COMMON /DRAWTHIS/ ndraw(50)
    COMMON /COLNOW/ NCOLNOW

    integer :: N

    integer, parameter :: NMAX = 128
    real xr(NMAX), yr(NMAX)

    CALL SETCOL(NCOL)

!   safety
    N = min(N_, NMAX)

    xr(1:N) = x(1:N)
    yr(1:N) = y(1:N)

    CALL IGrPolygoncomplex(Xr,Yr,N)

    IF (.NOT. InOpenGLRendering .AND. (NCLR .NE. NCOL .or. ndraw(10) .ne. 0)) then
        CALL realPolygon(Xr,Yr,N,NCLR)
    ENDIF

    RETURN
    END
