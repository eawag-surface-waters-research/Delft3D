subroutine enloss(ag        ,d1        ,eweir     ,hkruin    ,hov       , &
                & qunit     ,qvolk     ,toest     ,vov       , &
                & ewben     ,wsbov     ,wsben     ,dte       , &
                & dtefri    ,iflagweir , &
                & crestl    ,rmpbov    ,rmpben    ,veg      )
!-------------------------------------------------------------------------------
!  Original URL: https://svn.oss.deltares.nl/repos/delft3d/trunk/src/engines_gpl/flow2d3d/packages/kernel/src/compute/enloss.f90
!!--description-----------------------------------------------------------------
!
! Function: Determines additional energy loss due to weir.
!           Energy loss dependent on velocity perpendicular
!           to weir (Schonfeld, 1955).
!           Subroutine based on subroutine ENLOSS in WAQUA.
!           Energyloss based on Carnot relation for
!           decellerated flow VOV < 0.25 m/s
!           or on "Tables" VOV > 0.5 m/s or
!           average for 0.25 < VOV < 0.50 m/s.
!           In 2003 the improved formulations described in
!           "Beoordeling nieuwe overlaatroutines WAQUA"
!           WL-report Z3063, have been implemented and tested.
!
!!--pseudo code and references--------------------------------------------------
!
! "Weergave van extra energieverlies in RIVCUR."
!  (J.H.A. Wybenga, report Deltares Q779 voor RWS/RIZA, 1989).
! "Energieverliezen door overlaten: een gewijzigde berekeningsprocedure voor
!  WAQUA-rivieren versie."
!  (H. Vermaas, verslag onderzoek Deltares Q92, 1987)
! "Beoordeling nieuwe overlaatroutines WAQUA"
!  (J. van Kester, verslag Z3063, juli 2001)
!
!!--declarations----------------------------------------------------------------
    use m_flowgeom
    use precision
    implicit none
!
! Global variables
!
    real(fp)    , intent(in)    :: ag     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , intent(in)    :: d1     !!  Distance between crest and downstream depth
    real(fp)    , intent(out)   :: dte    !!  Subgrid energy loss due to weir
    real(fp)    , intent(in)    :: dtefri !!  Energy loss due to friction
    real(fp)    , intent(in)    :: ewben  !!  Energy height downstream
    real(fp)    , intent(in)    :: eweir  !!  Energy height at weir
    real(fp)    , intent(in)    :: hkruin !!  Crest height (downward positive).
    real(fp)    , intent(in)    :: hov    !!  Total water depth at crest weir
    real(fp)    , intent(in)    :: qunit  !!  Discharge at weir crest
    real(fp)    , intent(in)    :: qvolk  !!  Maximum discharge (super critical flow)
    real(fp)    , intent(in)    :: vov    !!  Velocity at crest of weir
    real(fp)    , intent(in)    :: wsben  !!  Downstream water level
    real(fp)    , intent(in)    :: wsbov  !!  Upstream water level
    character(4), intent(inout) :: toest  !!  State weir:
                                          !!  volk = perfect weir
                                          !!  onvo = imperfect weir
    integer    , intent(in)     :: iflagweir  !!  Flag to switch between Tabellenboek and Villemonte
    real(fp)   , intent(in)     :: crestl !!  crest length of weir
    real(fp)   , intent(in)     :: rmpben !!  ramp (talud) downstream of weir
    real(fp)   , intent(in)     :: rmpbov !!  ramp (talud) upstream of weir
    real(fp)   , intent(in)     :: veg    !!  Vegetation on weir
!
! Local variables
!
    real(fp) :: dtecar
    real(fp) :: dteonv
    real(fp) :: dtetab
    real(fp) :: dtevol
    real(fp) :: qqv
    double precision :: tabellenboek
    real(fp) :: theta
    real(fp) :: vilcd(1:2)  !! These parameters have to be read in
    real(fp) :: p, pref, qvolkvil, qweir, q2rat, cd0, cd0ref, sqfac, alfitp, re
    real(fp) :: ddive, vil1, vil2
!
!! executable statements -------------------------------------------------------
!
    if (iflagweir == 24) then    !! Tabellenboek
       !
       ! Determine energy loss for Tabellenboek
       !
       qqv = qunit/qvolk
       !
       ! Imperfect weir (sub critical flow)
       !
       if ((wsben + hkruin + d1)==0.0) then
          !
          ! Dry bed downstream, could perhaps also check on qunit==0.0
          !
          dteonv = 0.0
       elseif (abs(vov)<=0.25) then
          !
          ! Energy loss according Carnot law
          ! WSBEN+HKRUIN+D1 := H0 (S0+DPS)
          !
          dteonv = (vov - qunit/(wsben + hkruin + d1))**2/(2.*ag)
       elseif (abs(vov)>0.25 .and. abs(vov)<0.5) then
          !
          ! Weigthing between Carnot and Tables
          ! WSBEN+HKRUIN+D1 := H0 (S0+DPS)
          !
          dtecar = (vov - qunit/(wsben + hkruin + d1))**2/(2.*ag)
          dtetab = tabellenboek(d1, eweir, qunit, qvolk)
          theta = (abs(vov) - 0.25)/0.25
          dteonv = (1 - theta)*dtecar + theta*dtetab
       elseif (abs(vov)>=0.5) then
          !
          ! Energy loss according to Tables from Vermaas
          !
          dteonv = tabellenboek(d1, eweir, qunit, qvolk)
       else
       endif

    elseif (iflagweir == 25) then     !! Villemonte
       !
       ! Set Villemonte coefficients

       vilcd(1) = VillemonteCD1
       vilcd(2) = VillemonteCD2
       !
       ! Determine energy loss for Villemonte
       !
       ! Sieben2010 including vegetation:
       ! Noted that cd0_ref and P-ref are only used for determining
       ! whether (on)volkomen flow appears. Vegetation is excluded.
       ! cd0 and p are used to compute the energy loss, in which
       ! vegetation is included.

       alfitp = exp(-0.5d0*eweir/max(0.01d0,crestl))
       cd0ref = vilcd(1) *                                             &
                ( alfitp      * (1.0d0-0.25d0*exp(-0.5d0*rmpbov)) +          &
                  (1.-alfitp) * (0.8d0+0.65d0*exp(-0.1d0*rmpben)) )
       cd0    = cd0ref * (1.0d0 + veg/3.0d0)**(-1.5d0)

       !
       ! Sieben' formula of 3 February 2010:
       !
       ! pref = 3.0**3 / (4.0  * cd0**2) *                              &
       !      (1 + min(5.0d0,d1/eweir)*(1-exp(-rmpben/vilcd(2))))**2
       !
       ! Sieben' formula of 6 August 2010:
       !
       ddive = min(5.0d0,d1/eweir)
       vil1 = 1 + ddive * (1-exp(-rmpben/vilcd(2)))
       vil1 = 1.0 / (vil1**2)
       vil2 = (1 + ddive)
       vil2 = 1.0 / (vil2**2)
       pref = 3.0**3 / (4.0 * cd0**2) / (max(0.001d0,vil1 - vil2))

       p = (1.0 + veg/3.0)**3 / (1.0+2*veg) * pref

       qvolkvil = 2.0/3.0 * eweir * sqrt(2.0/3.0 * ag * eweir) * cd0

       sqfac = sqrt(max(0.0d0,1.0d0-max(0.0d0,ewben/eweir)**pref))
       qweir = qvolkvil * sqfac
       !
       ! determine energy loss for submerged weir flow with Sieben
       !
       q2rat = max(0.0d0, qunit**2/qvolkvil**2)
       if (q2rat .lt. 1.0d0) then
          re =  max(0.00000001d0, 1.0d0 -( 1.0d0-q2rat )**(1.0d0/p))
       else
          re = 1.
       endif

       q2rat  = sqfac
       dteonv = re * eweir
       qvolkvil = max(0.00000001d0, qweir)
       qqv = 0.
    endif

    !
    ! Determine energy loss for free weir flow
    !
    dtevol = wsbov - wsben - dtefri
    !
    ! Determine if it is free weir flow or submerged weir flow
    !
    if (iflagweir == 24) then    !! Tabellenboek
       !
       if (dtevol*qqv**2>=dteonv) then
           !
           ! It is a free weir flow
           !
           toest = 'volk'
        else
           !
           ! It is a submerged weir flow
           !
           toest = 'onvo'
       endif
    elseif (iflagweir == 25) then     !! Villemonte
        !
        if (q2rat .gt. 0.99d0) then
           !
           ! It is a free weir flow
           !
           toest = 'volk'
        else
           !
           ! It is a submerged weir flow
           !
           toest = 'onvo'
       endif
    endif
    !
    ! Energy loss
    !
    if (toest=='volk') then
       !
       ! It is a free weir flow
       !
       dte = dtevol
    elseif (toest=='onvo') then
       !
       ! It is a submerged weir flow
       !
       dte = dteonv
    else
    endif

!    if (iflagweir == 24) then         !! Tabellenboek
!       write(88,'(2a,10f8.4)')  'Tabellenboek  ', toest, dteonv, dtevol
!    elseif (iflagweir == 25) then     !! Villemonte
!       write(88,'(2a,10f8.4)')  'Villemonte  ', toest, dteonv, qweir, qunit, qvolkvil, q2rat, p
!    endif
end subroutine enloss
