!> Toplevel setnodadm routine wraps:
!! * original setnodadm(), for network_data administration.
!! * update_flow1d_admin(), to remove any net links from
!!   the flow1d::network administration, if they were also
!!   removed from network_data in the first step.
subroutine setnodadm(jacrosscheck_)
   use gridoperations
   use m_network
   use m_save_ugrid_state, only: contactnlinks, contactnetlinks
   use network_data
   use unstruc_channel_flow

   integer, intent(in   ) :: jacrosscheck_ !< Whether or not to remove any crossing netlinks.

   integer :: L, LL, Lnew


   call setnodadm_grd_op(10+jacrosscheck_)

   ! Update netlink numbers for all 1d2d contacts, after netlinks may have been permuted:
   if (contactnlinks > 0) then
      do LL=1,contactnlinks
         L = contactnetlinks(LL)
         Lnew = Lperminv(L)
         contactnetlinks(LL) = Lnew
      end do
   end if

   if (lc(1) /=0) then
      call update_flow1d_admin(network, lc)
   endif

end subroutine setnodadm
