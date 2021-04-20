   SUBROUTINE WRIRSTfileold(tim)
   use m_flowtimes
   use unstruc_model
   use m_flow
   use m_flowgeom
   use unstruc_files, only: defaultFileName

   implicit none
   double precision    :: tim
   integer :: mout

   call newfil(mout, defaultFileName('xyz', timestamp=tim))

   call wrirstold(mout)

   end subroutine wrirstfileold
