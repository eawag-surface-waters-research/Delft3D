!>    plot dots
      subroutine plotdots()
         use m_plotdots
         use unstruc_colors, only: ncolhl
         use unstruc_display
         implicit none
         integer                    :: i

         if ( Ndrawdots.ne.2 ) return

         do i=1,numdots
            call cirr(xdots(i), ydots(i), colnumber(i))
         end do

         return
      end subroutine
