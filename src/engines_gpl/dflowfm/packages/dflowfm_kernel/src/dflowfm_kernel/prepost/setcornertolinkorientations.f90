 subroutine setcornertolinkorientations()
    use m_flowgeom
    use network_data, only: xk, yk
    use m_sferic
    use m_alloc
    use unstruc_messages
    use m_missing, only : dmiss
    use geometry_module, only :half, spher2locvec
    implicit none

    double precision               :: xL, yL

    integer                        :: i, k, k3, k4
    integer                        :: L

    integer                        :: ierr

    double precision, parameter    :: dtol = 1d-8

    if ( allocated (csbn) ) deallocate(csbn)
    if ( allocated (snbn) ) deallocate(snbn)

    if ( jsferic.eq.0 .or. jasfer3D.eq.0 ) return

    allocate ( csbn(2,Lnx) , stat  = ierr) ; csbn = 1d0
    call aerr('csbn(2,Lnx)', ierr, 2*Lnx)
    allocate ( snbn(2,Lnx) , stat  = ierr) ; snbn = 0d0
    call aerr('snbn(2,Lnx)', ierr, 2*Lnx)

    do L=1,Lnx
       k3    = lncn(1,L)
       k4    = lncn(2,L)

!      compute flowlink midpoint coordinates (xL,yL)
       call half(xk(k3),yk(k3),xk(k4),yk(k4),xL,yL,  jsferic, jasfer3D)


       if ( yk(k3).eq.90d0 .or. yk(k4).eq.90d0 ) then
          continue
       end if

!      compute orientation w.r.t. link mid point
       call spher2locvec(xk(k3),yk(3),1,(/xL/),(/yL/),(/1d0/),(/0d0/),csbn(1,L),snbn(1,L), jsferic, jasfer3D, dmiss)
       call spher2locvec(xk(k4),yk(4),1,(/xL/),(/yL/),(/1d0/),(/0d0/),csbn(2,L),snbn(2,L), jsferic, jasfer3D, dmiss)
    end do

    return
 end subroutine setcornertolinkorientations
