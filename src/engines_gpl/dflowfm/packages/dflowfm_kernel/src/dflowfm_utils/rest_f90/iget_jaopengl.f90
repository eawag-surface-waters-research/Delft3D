!< get jaopengl module variable
integer function iget_jaopengl()
   use unstruc_opengl, only: jaopengl
   implicit none

   iget_jaopengl = jaopengl

   return
end function iget_jaopengl
