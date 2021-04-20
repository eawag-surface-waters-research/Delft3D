   ! update the water levels for the bed level change caused by subsidence/uplift
   subroutine subsupl_update_s1()
      use m_subsidence, only: sdu_update_s1, sdu_blp
      use m_flowgeom, only: ndx, bl
      use m_flow, only: s1
      use m_flowparameters, only: epshs
      use MessageHandling, only: mess, LEVEL_ERROR

      implicit none

      integer           :: k !< face/cell index

      if (sdu_update_s1 == 1) then
         ! update s1 in all cells
         do k = 1,ndx
            ! adjust water level for subsidence/uplift
            s1(k) = s1(k) + (bl(k) - sdu_blp(k))
         enddo
      else
         ! update s1 only in dry cells
         do k = 1,ndx
            if ( (s1(k) - sdu_blp(k)) < epshs) then
               ! adjust water level in dry areas for subsidence/uplift
               s1(k) = s1(k) + (bl(k) - sdu_blp(k))
            else
               ! avoid negative depths in case of drying due to uplift
               s1(k) = max(s1(k), bl(k))
            endif
         enddo
      endif
   end subroutine subsupl_update_s1
