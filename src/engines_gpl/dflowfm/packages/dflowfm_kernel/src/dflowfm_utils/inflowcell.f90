 subroutine inflowcell(xp,yp,k,jaoutside, iLocTp)                      ! is this point in a flowcell

 use m_flowgeom
 use m_GlobalParameters, only: INDTP_1D, INDTP_2D, INDTP_ALL
 use m_flow
 use m_flowexternalforcings
 use geometry_module, only: pinpok
 use m_missing, only: jins, dmiss

 implicit none

 double precision  :: xp, yp
 integer           :: k, jaoutside
 integer,                         intent(in)     :: iLocTp      !< Node type, one of INDTP_1D/2D/ALL.

 ! locals
 integer           :: n, nn, in, kb, L, nstart, nend
 double precision  :: dxx, dyy, r

 ! define the searching range, this is especially for the purpose of snapping obs to 1D, 2D or 1D+2D flownodes.
 ! For other purpose it should stay as before
 select case(iLocTp)
   case (INDTP_ALL)
      nstart = 1
      nend   = ndxi
   case(INDTP_1D) ! 1d flownodes coordinates
      nstart = ndx2D+1
      nend   = ndxi
   case(INDTP_2D) ! 2d flownodes coordinates
      nstart = 1
      nend   = ndx2D
 end select

 k = 0
 do n = nstart,nend
     nn = size( nd(n)%x )
     IF (NN > 2) THEN
        call PINPOK (Xp, Yp, Nn, nd(n)%x, nd(n)%y, IN, jins, dmiss)
        if (in == 1) then
           k = n
           return
        endif
     ENDIF
 enddo

 if (jaoutside == -1) then ! do not look at open boundaries
    return
 endif

 do n   = 1, nbndz
    kb  = kbndz(1,n)
    L   = kbndz(3,n)
    dxx = xp - xz(kb)
    dyy = yp - yz(kb)
    r   = sqrt(dxx*dxx + dyy*dyy)
    if (r < 0.3d0*dx(L) ) then
       k = kb
       return
    endif
 enddo

   end subroutine inflowcell
