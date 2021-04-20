 subroutine tekprofpoint()
 use m_flowgeom
 use m_flow
 use unstruc_display
 implicit none
 integer :: k, nn
 if (klprof > 0 .and. nplot.gt.0 ) then
     call cirr(xz(nplot), yz(nplot), klprof)

     ! k    = nplot
     ! nn   = size( nd(k)%x )
     ! call PFILLER(nd(k)%x, nd(k)%y, nn, klprof, klprof)
 endif
 end subroutine tekprofpoint
