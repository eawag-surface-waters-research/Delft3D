logical function get_japart()
   use m_particles
   implicit none

   get_japart = ( japart.eq.1 )

   return
end function get_japart
