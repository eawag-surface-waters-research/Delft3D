   !> fill initial salinity and temperature with nudge variables
   subroutine set_saltem_nudge()
      use m_flowgeom
      use m_flow, only: sa1, kmxn
      use m_transport
      use m_nudge
      use m_missing
      implicit none

      integer :: k, kk, KB, KT

      do kk=1,Ndx
         call getkbotktop(kk,kb,kt)
         do k=kb,kt
            if ( ITEMP.gt.0 .and. nudge_tem(k).ne.DMISS ) then
               constituents(ITEMP,k) = nudge_tem(k)
            end if

            if ( ISALT.gt.0 .and. nudge_sal(k).ne.DMISS ) then
               constituents(ISALT,k) = nudge_sal(k)
               sa1(k) = constituents(ISALT,k)
            end if
         end do

         do k = kt+1, kb + kmxn(kk) - 1
            if ( ITEMP.gt.0) constituents(ITEMP,k) = constituents(ITEMP,kt)
            if ( ISALT.gt.0) constituents(ISALT,k) = constituents(ISALT,kt)
            if ( ISALT.gt.0) sa1(k)                = constituents(ISALT,kt)
         enddo

      end do

   end subroutine set_saltem_nudge
