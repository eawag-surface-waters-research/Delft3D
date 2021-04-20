!> add released particles
subroutine add_particles_from_release_file(time0)
   use m_particles
   double precision, intent(in) :: time0       !< current   julian (s) of s0
   logical :: adding

   if (irpart.eq.0 .or. irpart.gt.Nrpart) return

   do while (irpart.lt.Nrpart)
      if (trpart(irpart)*60.0d0.gt.time0) exit
      call add_particles(1, xrpart(irpart), yrpart(irpart), 0, 1)
      irpart = irpart + 1
   end do
end subroutine add_particles_from_release_file
