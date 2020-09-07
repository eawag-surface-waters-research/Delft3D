subroutine dredge_d3d4(dps, s1, timhr, nst, gdp)
!!--declarations----------------------------------------------------------------
    use precision
    use m_dredge, only: dredge
    use dredge_comm, only: dredgecommunicate
    use dredge_data_module, only: dredge_type
    use bedcomposition_module, only: updmorlyr
    use morstatistics, only: morstats
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    type (gd_bedformpar)      , pointer :: gdbedformpar
    type (dredge_type)        , pointer :: gddredge
    type (sedtra_type)        , pointer :: gderosed
    type (bedcomp_data)       , pointer :: gdmorlyr
    type (morpar_type)        , pointer :: gdmorpar
    type (message_stack)      , pointer :: messages
    real(fp)                  , pointer :: hdt
    integer                   , pointer :: numdomains
    integer                   , pointer :: lundia
    integer                   , pointer :: julday
    integer                   , pointer :: nmlb
    integer                   , pointer :: nmub
    integer   , dimension(:)  , pointer :: kfsed
    integer                   , pointer :: nmmax
    integer                   , pointer :: lsedtot
    real(fp)  , dimension(:)  , pointer :: cdryb
    real(fp)  , dimension(:,:), pointer :: dbodsd

!
! Global variables
!
    integer                                                    , intent(in)  :: nst     !< time step number
    real(fp)                                                   , intent(in)  :: timhr   !< flow time in hours
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: s1      !< water level
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                             :: dps     !< bed level - positive down
!
! Local variables
!
    logical                                  :: error
    real(fp)                                 :: morhr
    logical                                  :: spinup
    real(fp), dimension(:), allocatable      :: dz_dummy
    integer                                  :: istat
!
!! executable statements -------------------------------------------------------
!
    gdbedformpar        => gdp%gdbedformpar
    gddredge            => gdp%gddredge
    gderosed            => gdp%gderosed
    gdmorlyr            => gdp%gdmorlyr
    gdmorpar            => gdp%gdmorpar
    messages            => gdp%messages
    
    hdt                 => gdp%gdnumeco%hdt
    numdomains          => gdp%gdprognm%numdomains
    lundia              => gdp%gdinout%lundia
    julday              => gdp%gdinttim%julday
    nmlb                => gdp%d%nmlb
    nmub                => gdp%d%nmub
    
    nmmax               => gdp%d%nmmax
    lsedtot             => gdp%d%lsedtot
    kfsed               => gdp%gderosed%kfsed
    cdryb               => gdp%gdsedpar%cdryb
    dbodsd              => gdp%gderosed%dbodsd

    morhr = real(gdmorpar%morft * 24.0_hp, fp)
    spinup = nst < gdmorpar%itmor
    call dredge(nmmax, lsedtot, spinup, cdryb, dps, -1.0_fp, &
              & dbodsd, kfsed, s1, timhr, morhr, gddredge, error, &
              & dredgecommunicate, gdbedformpar%duneheight, gdmorpar, hdt, numdomains, lundia, &
              & julday, nmlb, nmub, gderosed, gdmorlyr, messages)
    !
    ! Update sediment administration for dumping only
    ! dbodsd is filled (kg/m^2 sediment added to a cell)
    !
    if (.not.error .and. gdmorpar%cmpupd) then
       allocate(dz_dummy(nmlb:nmub), stat=istat)   ! no actual bed update, unlike updmorlyr in fm_erosed.f90
       if (gdmorpar%moroutput%morstats) then
           call morstats(gderosed, gdmorpar, dbodsd, nmlb, nmub, lsedtot)
       endif   
       if (updmorlyr(gdmorlyr, dbodsd, dz_dummy, messages) /= 0) then
           call writemessages(messages, lundia)
           error = .true.
       endif
       deallocate(dz_dummy, stat=istat)
    endif

    if (error) call d3stop(1, gdp)
end subroutine dredge_d3d4
