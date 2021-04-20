!< set jaopengl module variable
subroutine iset_jaopengl(jaopengl_loc)
   use unstruc_opengl, only: jaopengl
   use unstruc_model,  only: md_jaopengl
   implicit none

   integer, intent(in) :: jaopengl_loc  !< value to be set to jaopengl

   if ( md_jaopenGL.eq.-1 ) then
      jaopengl = jaopengl_loc
   else
      jaopengl = md_jaopengl
   end if

   return
end subroutine iset_jaopengl
