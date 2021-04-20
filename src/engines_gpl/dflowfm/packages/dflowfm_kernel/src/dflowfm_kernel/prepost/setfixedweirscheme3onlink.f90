subroutine setfixedweirscheme3onlink(L)
use m_flowgeom
use m_flow
implicit none
integer :: L, nn, n12,kk,LL

teta(L) = 1d0

if (iadv(L) .ne. 24 .and. iadv(L) .ne. 25) then                      ! no change in advection for Tabellenboek and Villemonte
   do nn  = 1,2
      n12 = ln(nn,L)
      do kk  = 1,nd(n12)%lnx                                         ! and flag non-21 links to perot incoming only
          LL = iabs( nd(n12)%ln(kk) )
          if ( iadv(LL) < 21 .or. iadv(LL) > 25) then
               iadv(LL) = 4
          endif
          teta(LL) = 1d0
      enddo
   enddo
endif

end subroutine setfixedweirscheme3onlink
