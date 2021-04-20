!> copy samples to particles
subroutine copy_sam2part()
   use m_particles
   use m_samples
   implicit none

   character(len=255) :: dum

   integer, dimension(1) :: idum

   integer :: i
   integer :: ierror

   if ( NS.lt.1 ) return

   if ( japart.ne.1 ) then
      dum = ' '
      call ini_part(0, dum, dum, 0,0d0,0d0,0)
   end if

   call add_particles(Ns, xs, ys, 0, 1)

   call delsam(0)

end subroutine copy_sam2part
