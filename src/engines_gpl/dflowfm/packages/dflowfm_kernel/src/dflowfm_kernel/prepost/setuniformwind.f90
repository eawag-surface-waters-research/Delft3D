 subroutine setuniformwind()
 use m_wind
 use m_sferic
 use m_flowgeom
 implicit none
 double precision :: wdir

 jawind = 2
 wdir   = (90d0 - winddir)*dg2rd

 if (.not. allocated(wx) ) then
     allocate (  wx(lnx), wy(lnx) )
 endif

 wx   = windsp*cos(wdir)
 wy   = windsp*sin(wdir)
 call setwindstress()
 end subroutine setuniformwind
