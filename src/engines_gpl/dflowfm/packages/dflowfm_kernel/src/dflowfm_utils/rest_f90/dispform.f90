      subroutine DISPFORM (value,fmt)
      implicit none
      integer :: n1
      integer :: n2
      double precision :: value
      character fmt*(*)

      fmt='(f9.3)'

      if (value .eq. 0.0) then
         fmt='(f9.5)'
         return
      endif

      n1 = int(log10(abs(value)))

      if (n1 .le. 6 .and. n1 .gt. 0) then
         n2 = min(9,n1 + 3)
         write (fmt(5:5),'(i1)') 9 - n2
      else if (n1 .ge. -5 .and. n1 .lt. 0) then
         write (fmt(5:5),'(i1)') 6
      else if ( n1 .eq. 0) then
         write (fmt(5:5),'(i1)') 6
      else
         fmt ='(e9.3)'
      endif

      return
      end
