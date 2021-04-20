!> snap network meshlines to nearest land boundary
subroutine nettoland()

   use M_netw
   use M_MISSING
   use m_observations
   use gridoperations

   implicit none

   integer :: ja

   call findcells(100)
   call makenetnodescoding()


   ja = 1
   call confrm('Do you want to snap net boundary to the land boundary only?', ja)
   if ( ja.eq.1) then
      call find_nearest_meshline(2) ! net boundaries only
      call snap_to_landboundary()
   else
      ja = 1
      call confrm('Do you want to snap inner net to the land boundary too?', ja)
      if ( ja.eq.1) then
         call find_nearest_meshline(3)
         call snap_to_landboundary()
      else
         ja = 1
         call confrm('Do you want to snap all net to the land boundary?', ja)
         if ( ja.eq.1) then
            call find_nearest_meshline(4)
            call snap_to_landboundary()
         end if
      end if
   end if

   if ( ja.eq.1 ) then
      call confrm('Are you satisfied?', ja)
      if ( ja.ne.1 ) call restore()
   end if

   return
end subroutine nettoland
