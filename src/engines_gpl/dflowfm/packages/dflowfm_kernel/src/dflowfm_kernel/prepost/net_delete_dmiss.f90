!> delete missing values part of network
subroutine net_delete_DMISS()
   use m_netw
   use m_missing
   use gridoperations

   implicit none

   integer :: k

   if ( netstat.ne.netstat_OK ) call findcells(0)

   do k=1,numk
      if ( zk(k).eq.DMISS ) then
         call delnode(k)
      end if
   end do

   call setnodadm(0)

   netstat = NETSTAT_CELLS_DIRTY

   return
end subroutine net_delete_DMISS
