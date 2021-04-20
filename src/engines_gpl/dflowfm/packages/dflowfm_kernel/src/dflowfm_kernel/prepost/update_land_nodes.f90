! updates zk value at specified net node index using diven delta
! TODO: extend it to multiple indices
subroutine update_land_nodes(node_index, new_zk)
  use network_data
  use m_missing
  use m_polygon
  use m_flowgeom
  use m_flow
  use unstruc_display
  use m_sediment
  implicit none
  integer, intent(in) :: node_index
  double precision, intent(in) :: new_zk

  ! locals
  integer :: kk, k, n, nn, ncol, j, i
  double precision :: old_zk

  if (ndx == 0) return

  k = node_index

  if (npl > 2) then
    old_zk = zk(k)
    zk(k) = new_zk
    if (jaceneqtr == 2 .and.jased > 0) then
        do j = 1,mxgr
            grainlay(j, k ) = max(0d0, grainlay(j, k ) + (old_zk - new_zk)/mxgr)
        enddo
    endif
    call isocol(zk(k), ncol)
    call movabs(xk(k), yk(k))
    call hlcir2(rcir, ncol, 30)
  else
    old_zk = zk(k)
    zk(k) = new_zk
    if (jaceneqtr == 2 .and.jased > 0) then
    do j = 1,mxgr
        grainlay(j, k ) = max(0d0, grainlay(j, k) + (old_zk - new_zk)/mxgr)
    enddo
    endif
    call isocol(zk(k),ncol)
    call movabs(xk(k),yk(k))
    call hlcir2(rcir,ncol,30)
  endif

  ! NOTE: update of bobs/bl/s1/hs is now in subroutine on_land_change().
  !       Should be called separately on call-site!

end subroutine update_land_nodes
