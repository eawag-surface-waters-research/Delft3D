      !> Writes active cross sections to a polyline file.
      subroutine wricrs(mpol)
      use m_crosssections
      use m_polygon
      use m_missing
      implicit none
      integer :: mpol,i

      call savepol()
      call copycrosssectionstopol()
!      npl = 0 ! Write traced polygons instead of original plis
!      do i=1,ncrs
!        xpl(npl+1:npl+crs(i)%len+1)=crs(i)%xk(1:crs(i)%len+1)
!        ypl(npl+1:npl+crs(i)%len+1)=crs(i)%yk(1:crs(i)%len+1)
!        npl = npl+crs(i)%len+2
!        xpl(npl) = dmiss
!        ypl(npl) = dmiss
!      end do
!      if (ncrs>0) npl = npl - 1 ! remove last separator
      call wripol(mpol)
      call restorepol()

      end subroutine wricrs
