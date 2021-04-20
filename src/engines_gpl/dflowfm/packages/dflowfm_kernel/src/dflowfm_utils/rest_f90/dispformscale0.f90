      subroutine DISPFORMscale0(value,fmt)
      implicit none
      integer :: n1
      integer :: n2
      integer :: ndec
      double precision :: scalesize
      double precision :: value
      double precision :: xsc
      double precision :: ysc
      character fmt*(*)
      COMMON /SCALEPOS/ XSC,YSC,SCALESIZE,NDEC

      fmt='(f9.3)'

      if (value .eq. 0.0) then
         fmt='(f3.1)'
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
      IF (NDEC .GT. 0) write (fmt(5:5),'(i1)') NDEC
      return
      end
