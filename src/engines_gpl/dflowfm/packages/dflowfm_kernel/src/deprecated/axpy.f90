 !> speed test: compute z = Ax+y
 !> note: no communcation included in parallel runs, only test speed
 subroutine axpy(Mglob,Nglob)
    use m_partitioninfo
    use unstruc_messages
    use m_timer

    implicit none

    integer,                          intent(in)  :: Mglob  !> global size of x
    integer,                          intent(in)  :: Nglob  !> global size of y

    double precision, dimension(:),   allocatable :: x, y, z
    double precision, dimension(:,:), allocatable :: A

    integer                                       :: irun
    integer                                       :: ierror
    integer                                       :: M, N, i, j

    ierror = 1

!   compute array sizes based on number of subdomains
    M = Mglob
    N = Nglob

!   allocate
    allocate(x(M))
    allocate(y(N))
    allocate(z(N))
    allocate(A(N,M))

!   fill matrix A and vector x
     do j=1,M
        do i=1,N
          call random_number(A(i,j))
        end do
        call random_number(x(j))
      end do

    ! fill vecor y
    do i=1,N
        call random_number(y(i))
    end do

    call starttimer(IAXPY)

    ! z = Ax + y
    do i=1,N
       z(i) = y(i)
       do j=1,M
          z(i) = z(i) + A(i,j)*x(j)
       end do
    end do

    call stoptimer(IAXPY)

    ierror = 0

1234 continue

!   deallocate
    if ( allocated(x) ) deallocate(x)
    if ( allocated(y) ) deallocate(y)
    if ( allocated(A) ) deallocate(A)

 end subroutine axpy
