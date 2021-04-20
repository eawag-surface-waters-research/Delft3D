    SUBROUTINE PFILLER(X,Y,N_,NCOL,NCLR)
    use unstruc_opengl
    use m_sferic

    implicit none
    integer :: N_
    integer :: nclr
    integer :: ncol, i, n
    integer :: ncolnow
    integer :: ndraw
    double precision :: X(N_), Y(N_), xx, yy
    COMMON /DRAWTHIS/ ndraw(50)
    COMMON /COLNOW/ NCOLNOW

    integer, parameter :: NMAX = 128
    real xr(NMAX), yr(NMAX)

    CALL SETCOL(NCOL)

!   safety
    N = min(N_, NMAX)

    if (jsfertek == 1) then
       do i = 1,n
          call dproject(x(i), y(i), xx, yy, 1)
          xr(i) = xx ; yr(i) = yy
       enddo
    else
       xr(1:N) = x(1:N)
       yr(1:N) = y(1:N)
    endif

    CALL PFILLERCORE(xr,yr,N)

    IF (.NOT. InOpenGLRendering .AND. (NCLR .NE. NCOL .or. ndraw(10) .ne. 0)) then
        CALL realPolygon(Xr,Yr,N,NCLR)
    ENDIF

    RETURN
   END
