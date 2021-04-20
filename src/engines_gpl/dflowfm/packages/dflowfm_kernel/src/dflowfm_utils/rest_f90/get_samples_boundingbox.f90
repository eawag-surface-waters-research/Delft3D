!>    determine sample bounding box
      subroutine get_samples_boundingbox()
         use m_samples
         use m_missing
         implicit none

         integer :: i

         xsammin =  huge(1d0)
         xsammax = -huge(1d0)
         ysammin =  huge(1d0)
         ysammax = -huge(1d0)

         do i=1,NS
            if ( xs(i).ne.DMISS .and. ys(i).ne.DMISS .and. zs(i).ne.DMISS ) then
               xsammin = min(xsammin,xs(i))
               xsammax = max(xsammax,xs(i))
               ysammin = min(ysammin,ys(i))
               ysammax = max(ysammax,ys(i))
            end if
         end do

         return
      end subroutine get_samples_boundingbox
