!> copy curvigrid boundaries to polygon(s)
subroutine copycurvigridboundstopol()
   use network_data
   use m_grid
   use m_polygon
   use gridoperations

   implicit none

   integer :: ierror

   ierror = 1

!  save net and curvigrid
   call SAVENET()
   call savegrd()

!  delete net
   call zeronet()

   call gridtonet()
   call copynetboundstopol(1, 1, 0, 1)

   ierror = 0
1234 continue

!  resore net and curvigrid
   call restore()
   call restoregrd()

   return
end subroutine copycurvigridboundstopol
