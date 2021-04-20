subroutine smooth_samples_from_GUI()
   use m_samples
   implicit none

   integer :: N

!  check if samples are structured
   if ( MXSAM*MYSAM.ne.NS ) then
      call qnerror('Samples are not structured', ' ', ' ')
      goto 1234
   end if

   N = 0
   call getint('Number of smoothing iterations', N)

   call savesam()
   call smooth_samples(MXSAM, MYSAM, NS, 1, N, zs, zs)

1234 continue

   return
end subroutine smooth_samples_from_GUI
