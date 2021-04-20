 !> add polygon to global polygons
 subroutine addtopol(XCRA,YCRA,NCRA)
    use m_polygon
    use m_alloc
    use m_missing
    implicit none

    integer, intent(in) :: NCRA
    double precision, dimension(NCRA), intent(in) :: XCRA, YCRA

    integer :: i

    if ( NCRA.le.0 ) return

    call increasepol(NPL+NCRA+1,1)

    if ( NPL.gt.0 .and. NCRA.gt.0 ) then
       NPL = NPL+1
       xpl(NPL) = DMISS
       ypl(NPL) = DMISS
       zpl(NPL) = DMISS
    end if

    do i=1,NCRA
       NPL = NPL+1
       xpl(NPL) = XCRA(i)
       ypl(NPL) = YCRA(i)
       zpl(NPL) = DMISS
    end do

   return
 end subroutine addtopol
