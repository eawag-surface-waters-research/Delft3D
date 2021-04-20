!> copy spline to resampled polygon
subroutine copySplinesToFinePol(numk)
   USE M_SPLINES
   use m_polygon
   use m_missing

   implicit none

    integer, intent(in) :: numk  !< resample factor

    integer :: i, k, m, numpi, Numnew, ierror
    double precision :: tn, xk, yk, xh2(500), yh2(500)

!    NUMK  = 11
    do m = 1,mcs
        CALL NUMP(m,NUMPI)

        IF (NUMPI .GT. 1) THEN
            Numnew = 1+(NUMPI-1)*numk
            if ( NPL.gt.0 .and. xpl(max(NPL,1)).ne.DMISS ) then
               call increasepol(Numnew+2, 1)
               NPL = NPL+1
               xpl(NPL) = DMISS
            else
               call increasepol(Numnew+1,1)
            end if

            do
               call sample_spline(NUMPI, xsp(m,1:NUMPI), ysp(m,1:NUMPI), numk-1, Numnew, xpl(NPL+1:NPL+Numnew), ypl(NPL+1:NPL+Numnew), ierror)
               if ( ierror.eq.2 ) then
                  call increasepol(Numnew+1,1)
               else
                  exit
               end if
            end do
            NPL = NPL + Numnew
        ENDIF

!       add DMISS
        NPL = NPL+1
        xpl(NPL) = DMISS
        ypl(NPL) = DMISS
        zpl(NPL) = DMISS
    enddo
end subroutine copySplinesToFinePol
