 subroutine inworld(xx) ! shifts x coordinates in world window, only call if jsferic == 1
   use m_missing
   use m_sferic
   double precision :: xx
   if (xx .ne. dmiss) then
      do while (xx < xwleft)
         xx = xx + 360d0
      enddo
      do while (xx >  xwleft + 360d0)
         xx = xx - 360d0
      enddo
   endif
   end subroutine inworld
