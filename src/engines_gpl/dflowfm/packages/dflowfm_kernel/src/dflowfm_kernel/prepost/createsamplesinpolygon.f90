   SUBROUTINE CREATESAMPLESINPOLYGON()
   use m_polygon
   use m_missing
   use m_samples
   use geometry_module, only: get_startend   ! zijn er nog meer startends zodat dit afgeschermd moet worden?

   integer :: jpoint, jstart,jend,jadoall, nplsav
   double precision, allocatable :: xplsav(:), yplsav(:)

   allocate(xplsav(npl) , yplsav(npl)) ; xplsav = xpl(1:npl) ; yplsav = ypl(1:npl) ; nplsav = npl

   jpoint = 1; jadoall = 0
   do while ( jpoint.lt.NPLsav )

      !get subpolyline
      call get_startend(NPLsav-jpoint+1,xplsav(jpoint:NPLsav),yplsav(jpoint:NPLsav),jstart,jend, dmiss)
      xpl(1:jend-jstart+1) = xplsav(jstart+jpoint-1:jend+jpoint-1)
      ypl(1:jend-jstart+1) = yplsav(jstart+jpoint-1:jend+jpoint-1)
      npl = jend-jstart+1

      if (nplsav > jend) then
         jadoall = 1
      endif

      jstart = jstart+jpoint-1
      jend   = jend+jpoint-1
      jpoint = jend+2

      call CREATESAMPLESINPOLYGON2()

      if (jadoall == 1) then
         call Triangulatesamplestonetwork(1)
      endif

   enddo

   deallocate (xplsav, yplsav)

   END SUBROUTINE CREATESAMPLESINPOLYGON
