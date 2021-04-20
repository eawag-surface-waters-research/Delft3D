!  update geometry data that may have been incorrectly computed in the ghost area
   subroutine update_geom(iphase)
      use m_partitioninfo
      use m_flowgeom
      use unstruc_channel_flow
      use m_crosssections
      use m_cross_helper

      implicit none

      integer, intent(in) :: iphase ! phase, 0 (all), 1 (first) or 2 (second)
      integer :: ierror

      if ( iphase.eq.0 .or. iphase.eq.1 ) then
         call update_ghosts        (ITYPE_SALL, 1, Ndx, xz, ierror)
         call update_ghostboundvals(ITYPE_SALL, 1, Ndx, xz, 1, ierror)  ! safety: check
         call update_ghosts        (ITYPE_SALL, 1, Ndx, yz, ierror)
         call update_ghostboundvals(ITYPE_SALL, 1, Ndx, yz, 0, ierror)
      end if

      if (iphase.eq.0 .or. iphase.eq.2 ) then
         call update_ghosts        (ITYPE_SALL, 1, Ndx, bl, ierror)
         call update_ghostboundvals(ITYPE_SALL, 1, Ndx, bl, 0, ierror)
      end if

      return
   end subroutine update_geom
