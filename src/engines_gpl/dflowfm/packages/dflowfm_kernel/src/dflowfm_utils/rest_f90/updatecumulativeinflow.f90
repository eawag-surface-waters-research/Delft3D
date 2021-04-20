! update m_wind::vextcum(:) with the realized inflow from m_wind::qextreal(:)
subroutine updateCumulativeInflow(deltat)
    use m_wind
    use m_flowgeom, only : ndx

    integer :: k
    double precision, intent(in) :: deltat ! dt of current timestep

    if (jaQext == 0) return

    do k = 1, ndx
        vextcum(k) = vextcum(k) + qextreal(k)*deltat
    enddo

end subroutine updateCumulativeInflow
