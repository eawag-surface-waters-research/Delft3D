 ! Delete dry points from netgeom based on drypoints files and grid enclosure file
 subroutine delete_dry_points_and_areas()
   use unstruc_model, only: md_dryptsfile, md_encfile
   use gridoperations, only: update_cell_circumcenters
   implicit none

   call delete_drypoints_from_netgeom(md_dryptsfile, 0, 0)
   call delete_drypoints_from_netgeom(md_encfile, 0, -1)
!   call delete_drypoints_from_netgeom(md_cutcelllist, 0, 0)

   ! for issue UNST-3381, compute circumcenter after deleting dry areas
   ! TODO: UNST-3436 must be done as a better solution
   if (len_trim(md_dryptsfile) > 0 .or. len_trim(md_encfile) > 0) then
      call update_cell_circumcenters()
   end if

   return
 end subroutine delete_dry_points_and_areas
