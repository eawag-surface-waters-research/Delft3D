   SUBROUTINE read_restart_from_map(filename,ierr)
   use unstruc_netcdf, only : unc_read_map
   implicit none
   character(len=*),  intent(in)  :: filename
   integer,           intent(out) :: ierr !< Error status (DFM_NOERR==0 is successful)

   call unc_read_map(filename, ierr)

   END SUBROUTINE read_restart_from_map
