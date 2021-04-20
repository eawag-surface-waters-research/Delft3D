 module m_nudge
    double precision, allocatable, target :: nudge_tem(:) !< 3D temperature for nudging
    double precision, allocatable, target :: nudge_sal(:) !< 3D salinity for nudging
    double precision, allocatable         :: nudge_time(:)   !< nudge relaxation time
    double precision, allocatable         :: nudge_rate(:)   !< nudge relaxation time, 1/days
    double precision, parameter           :: NUDGE_RATE_UNIT_TO_SECI = 1d0/(24d0 * 3600d0)
 end module
