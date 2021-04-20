 !> make structured triangular mesh from curvlinear grid
 subroutine maketrigrid()
    use m_grid
    use m_sferic, only: jsferic, jasfer3D
    use geometry_module, only :half
    use m_missing
    implicit none

    double precision, dimension(:,:), allocatable :: x, y ! original grid coordinates

    integer                                       :: i,j
    integer                                       :: M, N ! original grid dimensions
    integer                                       :: Mnew, Nnew ! new grid dimensions

    integer                                       :: orient
    integer                                       :: ja

    if ( MC*NC.eq.0 ) return  ! nothing to do

    call savegrd()

    orient = 1

!   whipe out grid
    call cleargrid()

    do
!      allocate and copy original grid
       if ( orient.eq.1 ) then
          M = MC
          N = NC
          allocate(x(M,N))
          allocate(y(M,N))
          do j=1,N
             do i=1,M
                x(i,j) = xc(i,j)
                y(i,j) = yc(i,j)
             end do
          end do
       else if ( orient.eq.2 ) then
          M = NC
          N = MC
          allocate(x(M,N))
          allocate(y(M,N))
          do j=1,N
             do i=1,M
                x(i,j) = xc(MC-j+1,i)
                y(i,j) = yc(MC-j+1,i)
             end do
          end do
       else if ( orient.eq.3 ) then
          M = MC
          N = NC
          allocate(x(M,N))
          allocate(y(M,N))
          do j=1,N
             do i=1,M
                x(i,j) = xc(MC-i+1,NC-j+1)
                y(i,j) = yc(MC-i+1,NC-j+1)
             end do
          end do
       else
          M = NC
          N = MC
          allocate(x(M,N))
          allocate(y(M,N))
          do j=1,N
             do i=1,M
                x(i,j) = xc(j,NC-i+1)
                y(i,j) = yc(j,NC-i+1)
             end do
          end do
       end if

!      compute new grid dimensions
       Mnew = 2*M
       Nnew = N

!      increase grid
       MC = Mnew
       NC = Nnew
       call increasegrid(MC,NC)
       xc = DMISS
       yc = DMISS

!      odd j-lines: copy
       do j=1,N,2
          do i=1,M
!            even i-lines
             xc(2*i,j) = x(i,j)
             yc(2*i,j) = y(i,j)

!            odd i-lines
             xc(2*i-1,j) = xc(2*i,j)
             yc(2*i-1,j) = yc(2*i,j)
          end do
       end do

!      even j-lines: shift
       do j=2,N,2
!         boundaries
          xc(1,j) = x(1,j)
          yc(1,j) = y(1,j)

          xc(2*M,j) = x(M,j)
          yc(2*M,j) = y(M,j)

          do i=1,M-1
!            even i-lines
             call half(x(i,j),y(i,j),x(i+1,j),y(i+1,j),xc(2*i,j),yc(2*i,j),jsferic,jasfer3D)

!            odd i-lines
             xc(2*i+1,j) = xc(2*i,j)
             yc(2*i+1,j) = yc(2*i,j)
          end do
       end do

!      deallocate
       if ( allocated(x) ) deallocate(x)
       if ( allocated(y) ) deallocate(y)

!      plot grid
       call tekgrid(i)

       ja = 0
       call confrm('Shift orientation?', ja)
       if ( ja.eq.1 ) then
!         whip out grid
          call cleargrid()

          call restoregrd()

          orient = orient+1; if ( orient.gt.4 ) orient=orient-4
       else
          exit
       end if

    end do

    call confrm('Are you satisfied?', ja)
    if ( ja.ne.1 ) then
!      whip out grid
       call cleargrid()

       call restoregrd()

!      plot grid
       call tekgrid(i)
    end if

    return
 end subroutine maketrigrid
