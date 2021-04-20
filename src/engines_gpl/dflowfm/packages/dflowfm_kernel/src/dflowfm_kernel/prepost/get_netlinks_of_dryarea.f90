! =================================================================================================
! =================================================================================================
   subroutine get_netlinks_of_dryarea()
      use network_data   , only: numl, lne
      use m_flowexternalforcings, only: kdryarea, nDryLinks

      implicit none
      integer :: L, k1, k2

      if (allocated(kdryarea) ) deallocate( kdryarea )
      allocate( kdryarea(numl) ) ; kdryarea = 0

      nDryLinks = 0
      do L = 1,numl
         k1 = lne(1,L) ; k2 = lne(2,L)
         if (k1 > 0 .and. k2 > 0) cycle
         if (k1 <= 0 .and. k2 <= 0) cycle
         nDryLinks = nDryLinks + 1
         kdryarea(nDryLinks) = L
      enddo

   end subroutine get_netlinks_of_dryarea
