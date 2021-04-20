subroutine furusobekstructures()
use m_flow
use m_flowgeom
use m_strucs
implicit none
integer :: ng, n, L, Ls, LL, Lb, Lt
double precision :: zup, bup, a, fac

logical :: firstiter=.true. , jarea= .false.

firstiter = .true.
jarea     = .false.

do ng = 1, ncgensg      ! loop over generalstruc signals, sethu
   do n  = L1cgensg(ng), L2cgensg(ng)
      L  = kcgen(3,n)
      if (kcgen(1,n) == ln(2,L)) then
         Ls = -L ! Flow link has opposite orientation to structure's orientation.
      else
         Ls = L
      end if

      if (hu(L) > 0d0) then ! hu is above lowest sill
         call flgsfm( n, ng, Ls, firstiter , jarea )
      endif
      if (kmx > 0) then
         call getLbotLtop(L,Lb,Lt)
         do LL = Lb, Lt
            fu(LL) = fu(L) ; ru(LL) = ru(L)
            au(LL) = au(L)*( hu(LL)-hu(LL-1) ) / ( hu(Lt)-hu(Lb-1) )
         enddo
      endif
   enddo
enddo

end subroutine furusobekstructures
