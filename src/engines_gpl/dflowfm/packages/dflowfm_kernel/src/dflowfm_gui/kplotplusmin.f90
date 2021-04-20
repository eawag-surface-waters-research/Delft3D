     SUBROUTINE KPLOTPLUSMIN(IPM)
      USE M_FLOWGEOM
      USE M_FLOW
      use m_xbeach_data, only: itheta_view
      implicit none
      integer :: IP, IPM, NRLAY

      if (kmx >= 1) then

         ip = ipm
         if (kplotfrombedorsurface .ne. 1) then
            ip = -1*ipm
         endif

         KPLOT = KPLOT+ip
         kplot = max(1,min(kplot,kmx))

         CALL TEXTFLOW()
      else if ( jawave.eq.4 ) then
         itheta_view = max(min(itheta_view + sign(1,ipm), ntheta), 1)
      end if
     END SUBROUTINE KPLOTPLUSMIN
