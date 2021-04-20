 subroutine setcentertolinkorientations()
    use m_flowgeom
    use network_data, only: xk, yk
    use m_sferic
    use m_alloc
    use unstruc_messages
    use geometry_module, only :half, spher2locvec
    use m_missing, only : dmiss

    implicit none

    double precision               :: xL, yL

    integer                        :: i, k, k3, k4
    integer                        :: L

    integer                        :: ierr

    double precision, parameter    :: dtol = 1d-8

    if ( allocated (csb) ) deallocate(csb)
    if ( allocated (snb) ) deallocate(snb)

    if ( jsferic.eq.0 .or. jasfer3D.eq.0 ) return

    allocate ( csb(2,Lnx) , stat  = ierr) ; csb = 1d0
    call aerr('csb(2,Lnx)', ierr, 2*Lnx)
    allocate ( snb(2,Lnx) , stat  = ierr) ; snb = 0d0
    call aerr('snb(2,Lnx)', ierr, 2*Lnx)

    do L=1,Lnx
       if ( L.eq.12 ) then
          continue
       end if
       k3    = lncn(1,L)
       k4    = lncn(2,L)

!      compute flowlink midpoint coordinates (xL,yL)
       call half(xk(k3),yk(k3),xk(k4),yk(k4),xL,yL, jsferic, jasfer3D)

       do i=1,2
          k = ln(i,L)

!         compute orientation w.r.t. link mid point
          call spher2locvec(xz(k),yz(k),1,(/xL/),(/yL/),(/1d0/),(/0d0/),csb(i,L),snb(i,L),jsferic, jasfer3D, dmiss)
       end do
    end do

    return
 end subroutine setcentertolinkorientations
