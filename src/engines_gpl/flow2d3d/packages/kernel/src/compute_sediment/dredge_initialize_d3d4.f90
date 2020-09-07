subroutine dredge_initialize_d3d4(gdp)
!!--declarations----------------------------------------------------------------
    use dfparall, only: parll, inode, nproc
    use dredge_comm, only: dredgecommunicate
    use dredge_data_module, only: dredge_type
    use m_dredge_initialize, only: dredge_initialize
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer            , pointer :: lundia
    type (dredge_type) , pointer :: gddredge
!
! Global variables
!
! NONE
!
! Local variables
!
    logical                                  :: dredge_parll
    logical                                  :: error
    integer                        , pointer :: dredge_domainnr
    integer                        , pointer :: dredge_ndomains
!
!! executable statements -------------------------------------------------------
!
    gddredge            => gdp%gddredge
    lundia              => gdp%gdinout%lundia
    dredge_domainnr     => gddredge%dredge_domainnr
    dredge_ndomains     => gddredge%dredge_ndomains
    
    if (parll) then
       dredge_parll    = .true.
       dredge_domainnr = inode
       dredge_ndomains = nproc
    elseif (gdp%gdprognm%numdomains > 1) then
       dredge_parll    = .true.
       call start_dd_dredgecommunication (dredge_domainnr, dredge_ndomains)
       dredge_domainnr = dredge_domainnr+1
    else
       dredge_parll    = .false.
       dredge_domainnr = 1
       dredge_ndomains = 1
    endif
    !
    call dredge_initialize(gddredge, dredge_domainnr, dredge_ndomains, lundia, error, dredgecommunicate)
    !
    if (error) call d3stop(1, gdp)
end subroutine dredge_initialize_d3d4
