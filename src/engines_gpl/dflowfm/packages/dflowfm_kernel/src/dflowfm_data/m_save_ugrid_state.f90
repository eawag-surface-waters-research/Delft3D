!> Save data for later writing of Ugrid
module m_save_ugrid_state

   use meshdata
   use m_hash_search

   type(t_ug_meshgeom)                                :: meshgeom1d
   character(len=ug_idsLen),allocatable               :: nbranchids(:), nnodeids(:), nodeids(:)
   character(len=ug_idsLongNamesLen), allocatable     :: nbranchlongnames(:), nnodelongnames(:), nodelongnames(:)
   character(len=255)                                 :: network1dname, mesh2dname, mesh1dname, contactname !MAXSTRLEN = 255
   character(len=ug_idsLen), allocatable              :: mesh1dNodeIds(:)
   integer, allocatable, dimension(:)                 :: mesh1dUnmergedToMerged(:)
   !integer, allocatable, dimension(:)                 :: mesh1dMergedToUnMerged(:)
   integer                                            :: numMesh1dBeforeMerging
   integer, allocatable                               :: contactnetlinks(:) !< netlink number for each contact
   integer                                            :: contactnlinks      !< Total number of links in all mesh contacts (typically we'll have one mesh contact with many netlinks part of it)

   type(t_hashlist)                                   :: hashlist_contactids!< Hash list for quick search for contact ids.

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_save_ugrid_state() instead.
contains
   subroutine default_save_ugrid_state()
      implicit none
      call reset_save_ugrid_state()
      network1dname = 'network1d'
      mesh1dname    = 'mesh1d'
      mesh2dname    = 'mesh2d'
      contactname   = 'contacts'
      numMesh1dBeforeMerging = 0
   end subroutine default_save_ugrid_state

   !> Resets only waves variables intended for a restart of flow simulation.
   !! Upon loading of new model/MDU, call default_save_ugrid_state() instead.
   subroutine reset_save_ugrid_state()
      implicit none
      if (allocated(mesh1dNodeIds)) deallocate(mesh1dNodeIds)
      if (allocated(mesh1dUnmergedToMerged)) deallocate(mesh1dUnmergedToMerged)
      !if (allocated(mesh1dMergedToUnMerged)) deallocate(mesh1dMergedToUnMerged)

      if (allocated(contactnetlinks)) deallocate(contactnetlinks)
      contactnlinks = 0
      call dealloc(hashlist_contactids)
   end subroutine reset_save_ugrid_state

   end module m_save_ugrid_state
