!> deallocate splineprops array
subroutine deallocate_splineprops()
   use m_spline2curvi

   implicit none

   integer :: ispline

   if ( .not.allocated(splineprops) ) return

   do ispline=1,ubound(splineprops,1)
      if ( allocated(splineprops(ispline)%ics) )       deallocate(splineprops(ispline)%ics)
      if ( allocated(splineprops(ispline)%Lorient) )   deallocate(splineprops(ispline)%Lorient)
      if ( allocated(splineprops(ispline)%t) )         deallocate(splineprops(ispline)%t)
      if ( allocated(splineprops(ispline)%cosphi) )    deallocate(splineprops(ispline)%cosphi)
      if ( allocated(splineprops(ispline)%hL) )        deallocate(splineprops(ispline)%hL)
      if ( allocated(splineprops(ispline)%hR) )        deallocate(splineprops(ispline)%hR)
      if ( allocated(splineprops(ispline)%NsubL) )     deallocate(splineprops(ispline)%NsubL)
      if ( allocated(splineprops(ispline)%NsubR) )     deallocate(splineprops(ispline)%NsubR)
   end do

   deallocate(splineprops)

   return
end subroutine deallocate_splineprops
