 !> divide segment 1-3 (between latitudes y1 and y3) by y2,
 !> such that aspect ratios of the segments 1-2 and 2-3 are equal:
 !>   |y2-y1| / cos((y1+y2)/2) = |y3-y2| / cos((y2+y3)/2)
 subroutine comp_middle_latitude(y1_,y3_,y2,ierr)
    use m_sferic
    implicit none

    double precision, intent(in)  :: y1_
    double precision, intent(in)  :: y3_
    double precision, intent(out) :: y2
    integer,          intent(out) :: ierr

    double precision              :: y1, y3
    double precision              :: y2min, y2max
    double precision              :: A, dAdy2

    integer                       :: iter

    double precision, parameter   :: dtol = 1d-8
    double precision, parameter   :: deps = 1d-16
    integer,          parameter   :: MAXITER = 1000

    y2 = 0.5d0*(y1_+y3_)

    if ( jsferic.eq.0 .or. y1_.eq.y3_ .or. jamidlat == 0) then
        ierr = 0
       return
    end if

    y1 = dg2rd*y1_
    y3 = dg2rd*y3_

    y2max = 0.5d0*pi - deps
    y2min = -y2max

    ierr = 1

    y2 = 0.5d0*(y1+y3)
    do iter=1,MAXITER
       A = abs(y3-y2) * cos(0.5d0*(y1+y2)) - abs(y2-y1) * cos(0.5d0*(y2+y3))

       if ( abs(A)<dtol ) then
          ierr = 0
          exit
       end if

       dAdy2 = -sign(1d0,y3-y2) * cos(0.5d0*(y1+y2)) - 0.5d0*abs(y3-y2) * sin(0.5d0*(y1+y2)) -  &
                sign(1d0,y3-y2) * cos(0.5d0*(y2+y3)) + 0.5d0*abs(y2-y1) * sin(0.5d0*(y2+y3))
       y2 = y2 - A/dAdy2
       y2 = min(max(y2,y2min),y2max)
    end do

    if ( ierr.ne.0 ) then
!      error
       y2 = 0.5d0*(y1_+y3_)
    else
       y2 = y2/dg2rd
    end if

    return
 end subroutine comp_middle_latitude
