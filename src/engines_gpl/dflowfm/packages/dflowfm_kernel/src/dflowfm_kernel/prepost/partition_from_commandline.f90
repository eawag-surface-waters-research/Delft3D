!>  perform partitioning from command line
subroutine partition_from_commandline(fnam, md_Ndomains, md_jacontiguous, md_icgsolver, md_pmethod, md_dryptsfile, md_encfile, md_genpolygon, md_partugrid, md_partseed)

   use network_data
   use m_partitioninfo
   use m_polygon
   use dfm_error
   use gridoperations


   implicit none

   character(len=255), intent(in) :: fnam             !< filename
   integer,            intent(in) :: md_Ndomains      !< number of subdomains, Metis (>0) or polygon (0)
   integer,            intent(in) :: md_jacontiguous  !< contiguous domains, Metis (1) or not (0)
   integer,            intent(in) :: md_icgsolver     !< intended solver
   integer,            intent(in) :: md_pmethod       !< partition method: K-way (=1, default), Recursive Bisection(=2), Mesh-dual(=3)
   character(len=255), intent(in) :: md_dryptsfile    !< dry points file
   character(len=255), intent(in) :: md_encfile       !< Enclosure file to clip outer parts from the grid *.pol
   integer,            intent(in) :: md_genpolygon    !< make partition file (1) or not (0)
   integer,            intent(in) :: md_partugrid     !< write partitioning in ugrid format (1) or not (0)
   integer,            intent(in) :: md_partseed      !< User defined random seed, passed to METIS'option "SEED". Useful for reproducible partitionings, but only used when /= 0.

   integer                        :: jacells
   integer                        :: japolygon

   integer                        :: ierr = 0

   if ( md_genpolygon.eq.1 ) then
      jacells = 0
      japolygon = 1
   else
      jacells = 1
      japolygon = 0
   end if

   if ( netstat.eq.NETSTAT_CELLS_DIRTY ) then
      call preparecells(fnam, 0, 0, ierr)
   end if
   if (ierr /= DFM_NOERR) then
     call findcells(0)
     call find1dcells()
   end if
   netstat = NETSTAT_OK

!  delete dry points and dry areas
   call delete_dry_points_and_areas()

   if ( nump1d2d.lt.1 ) return

   call cosphiunetcheck(1)

   if ( md_Ndomains.gt.0 ) then ! use METIS
      call partition_METIS_to_idomain(md_Ndomains, md_jacontiguous, md_pmethod, md_partseed)
!     generate partitioning polygons
      Ndomains = md_Ndomains
      if ( japolygon.eq.1 ) then
         call generate_partition_pol_from_idomain(ierr)
      endif
   else if ( NPL.gt.1 ) then ! use polygons
!     generate partitioning polygons
      call generate_partitioning_from_pol()
   end if

   if ( ndomains.gt.1 ) then
      call partition_write_domains(trim(fnam),md_icgsolver,jacells,japolygon,md_partugrid)
   end if

   return
end subroutine partition_from_commandline
