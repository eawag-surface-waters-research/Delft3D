 subroutine tekspw(vfac,met)
 use m_flowgeom
 use m_spiderweb
 use m_wind
 implicit none
 double precision :: vfac, shft
 integer          :: met

 integer          :: mx, nx, i, j, L

 shft  = 0d0
 mx    = size(spw,2)
 nx    = size(spw,3)
 if (sum(xu(:)) .lt. 0) then
     shft       = 1d0
 end if
 if (mx.ne.0 .and. nx.ne.0) then
    do i = 1,mx-1
       do j = 1,nx
          call setcol(221)
          call arrowsxy( spw(1,i,j) - shft*360d0, spw(2,i,j), spw(3,i,j) , spw(4,i,j), 0.05*VFAC)
       enddo
    enddo
 endif
 if (allocated(wx)) then
    do L  = 1,lnxi
       call setcol(224)
       call arrowsxy( xu(L) , yu(L) , wx(L) , wy(L), 0.05*VFAC)
    enddo
 endif

 end subroutine tekspw
