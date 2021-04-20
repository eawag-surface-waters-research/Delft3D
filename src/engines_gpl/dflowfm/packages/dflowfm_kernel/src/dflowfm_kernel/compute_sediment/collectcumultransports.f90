   subroutine collectcumultransports()
   use m_flowtimes, only:dts
   use m_flowgeom
   use m_fm_erosed

   implicit none

   integer            :: k, l
   double precision   :: dtmor_

   ! cumulative transports
   dtmor_ = dts*morfac
   do l = 1, lsedtot
      do k = 1,ndx
         sbxcum(k,l) = sbxcum(k,l) + (sbcx(k,l) + sbwx(k,l)) * dtmor_
         sbycum(k,l) = sbycum(k,l) + (sbcy(k,l) + sbwy(k,l)) * dtmor_
         ssxcum(k,l) = ssxcum(k,l) + (sscx(k,l) + sswx(k,l)) * dtmor_
         ssycum(k,l) = ssycum(k,l) + (sscy(k,l) + sswy(k,l)) * dtmor_
      enddo
   enddo

   end subroutine
