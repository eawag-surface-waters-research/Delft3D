    !> Checks whether a polyline point is at the start or end of a polyline.
    !!
    !! Multiple polylines are stored in one large array, separated by dmiss.
    !! To know whether point at index L1 is a start/end point of one of these
    !! polylines, check on a neighbouring dmiss.
    logical function ispolystartend( X, Y, N, MAXPOL, ipoi) result(res)
    use m_missing
    implicit none
        integer,          intent(in) :: MAXPOL !< Length of polyline coordinate arrays.
        double precision, intent(in) :: X(MAXPOL), Y(MAXPOL) !< Entire polyline coordinate arrays
        integer,          intent(in) :: N      !< Index of last filled polyline point (npol<=maxpol)
        integer,          intent(in) :: ipoi   !< Index of polyline point to be checked.

        ! First check invalid input
        if (ipoi <= 0 .or. ipoi > n .or. n > maxpol) then
            res = .false.
            return
        end if

        ! Next, check on trivial end points
        if (ipoi == 1 .or. ipoi == n) then
            res = .true.
            return
        end if

        ! Generic case: somewhere in middle of poly, check on dmiss.
        res = (x(ipoi-1) == dmiss .or. x(ipoi+1) == dmiss)

    end function ispolystartend
