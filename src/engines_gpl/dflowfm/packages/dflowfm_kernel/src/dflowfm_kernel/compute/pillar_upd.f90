  ! =================================================================================================
  ! =================================================================================================
  subroutine pillar_upd()
    use m_flowexternalforcings, only: Cpil
    use m_flowgeom            , only: lnx, ln, dx
    use m_flow                , only: u1, v, advi
    use m_flowparameters      , only: japillar
    implicit none
    integer          :: L, k1, k2
    double precision :: CpilL, uv

    if (japillar == 1) then
      do L = 1,lnx
        k1 = ln(1,L)
        k2 = ln(2,L)
        CpilL = ( Cpil(k1) + Cpil(k2) ) * 0.5d0
        uv = sqrt( u1(L) * u1(L) + v(L) * v(L) )
        advi(L) = advi(L) + CpilL * uv / dx(L)
      enddo
    else if (japillar == 3) then
      do L = 1,lnx
        if (Cpil(L) == 0d0) cycle
        CpilL = Cpil(L)
        uv = sqrt( u1(L) * u1(L) + v(L) * v(L) )
        advi(L) = advi(L) + CpilL * uv / dx(L)
      enddo
    endif

  end subroutine pillar_upd
