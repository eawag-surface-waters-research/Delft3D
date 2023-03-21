!> \page morphology_lib Library of sediment transport formulations
!> \section formula_santoss SANTOSS practical sand transport model
!!
!! See: \ref santoss
!!

!> \anchor santoss
!! Main routine  of the SANTOSS practical sand transport model, version 2.08
!! Computations of the orbital motions in the nearshore morphodynamical model
!! using the formula of Abreu et al. (2010)
!!
subroutine santoss(numrealpar, realpar ,par ,npar, dzduu ,dzdvv , i2d3d, &
                 & sbcu, sbcv, sbwu, sbwv, sswu, sswv , &
                 & error, message)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2023.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!--declarations----------------------------------------------------------------
    use precision
    use morphology_data_module
    use mathconsts, only: sqrt2, degrad, raddeg

    implicit none
!
! arguments
!
    integer                         , intent(in)    :: i2d3d      !< switch 2D/3D (2 = 2D, 3 = 3D)
    integer                         , intent(in)    :: numrealpar !< number of parameters in realpar array
    integer                         , intent(in)    :: npar       !< number of parameters in par array
    real(fp)                        , intent(in)    :: dzduu      !< slope in m direction [m/m]
    real(fp)                        , intent(in)    :: dzdvv      !< slope in n direction [m/m]
!
    real(hp), dimension(numrealpar) , intent(in)    :: realpar    !< array of local conditions
    real(fp), dimension(30)         , intent(inout) :: par        !< in: transport formula parameters
                                                                  !! out: transport output variables
!
    real(fp)                        , intent(out)   :: sbcu       !< bed load due to currents, m component [m3/m/s]
    real(fp)                        , intent(out)   :: sbcv       !< bed load due to currents, n component [m3/m/s]
    real(fp)                        , intent(out)   :: sbwu       !< bed load due to waves, m component [m3/m/s]
    real(fp)                        , intent(out)   :: sbwv       !< bed load due to waves, n component [m3/m/s]
    real(fp)                        , intent(out)   :: sswu       !< susp load due to waves, m component [m3/m/s]
    real(fp)                        , intent(out)   :: sswv       !< susp load due to waves, n component [m3/m/s]
    character(len=256)              , intent(out)   :: message    !< error message
    logical                         , intent(out)   :: error      !< flag indicating whether an error occurred

!
! local variables for input parameters
!
      integer                  :: nm
      integer                  :: sw_effects ! surface wave effects 1 for on 0 for off
      integer                  :: as_effects ! wave asymetry effects 1 for on 0 for off
      integer                  :: pl_effects ! phase lag effects 1 for on 0 for off
      integer                  :: sl_effects ! slope effect oaaja in critical shear stress 1 for on 0 for off
      real(fp)                 :: ag
      real(fp)                 :: d
      real(fp)                 :: d50
      real(fp)                 :: d90
      real(fp)                 :: hrms
      real(fp)                 :: rhosol
      real(fp)                 :: rhowat
      real(fp)                 :: teta
      real(fp)                 :: timsec
      real(fp)                 :: tp
      real(fp)                 :: tanphi     ! tangens of angle of natural talud
      real(fp)                 :: u
      real(fp)                 :: umod
      real(fp)                 :: uorb
      real(fp)                 :: utot
      real(fp)                 :: uuu
      real(fp)                 :: v
      real(fp)                 :: vicmol
      real(fp)                 :: vvv
      real(fp)                 :: zumod
!
! local variables
!
      integer                  :: i
      integer                             :: istat
      integer                  :: j
      integer                  :: nt
      real(fp)                 :: ang        ! angle between net current direction and positive orbital velocity [deg]
      real(fp)                 :: alphas
      real(fp)                 :: alphar
      real(fp)                 :: aw
      real(fp)                 :: alpha
      real(fp)                 :: ang_cur
      real(fp)                 :: ang_ubot
      real(fp)                 :: a1
      real(fp)                 :: a2
      real(fp)                 :: a3
      real(fp)                 :: as
      real(fp)                 :: b
      real(fp)                 :: c1
      real(fp)                 :: delwblt
      real(fp)                 :: delta
      real(fp)                 :: dzbds_c
      real(fp)                 :: dzbds_t
      real(fp)                 :: dstar      ! dimensionless grain-size [-]
      real(fp)                 :: dstars     ! dimensionless grain size associated with representative grain size d50s [-]
      real(fp)                 :: d50s       ! representative grain size [m]
      real(fp)                 :: fc
      real(fp)                 :: fw
      real(fp)                 :: fcw
      real(fp)                 :: fsl_cr_c
      real(fp)                 :: fsl_cr_t
      real(fp)                 :: fcwc
      real(fp)                 :: fcwt
      real(fp)                 :: hw         ! wave height  [m]
      real(fp)                 :: ksw
      real(fp)                 :: ksc
      real(fp)                 :: k
      real(fp)                 :: k0
      real(fp)                 :: k1
      real(fp)                 :: km
      real(fp)                 :: l
      real(fp)                 :: l0
      real(fp)                 :: m1
      real(fp)                 :: n1
      real(fp)                 :: nr_timesteps
      real(fp)                 :: ocr
      real(fp)                 :: oc
      real(fp)                 :: occ
      real(fp)                 :: oct
      real(fp)                 :: ot
      real(fp)                 :: ott
      real(fp)                 :: otc
      real(fp)                 :: omega
      real(fp)                 :: pcr       ! critical phase lag parameter [-]
      real(fp)                 :: pc
      real(fp)                 :: pt
      real(fp)                 :: phi_ab
      real(fp)                 :: phicx
      real(fp)                 :: phitx
      real(fp)                 :: phicy
      real(fp)                 :: phity
      real(fp)                 :: qsx
      real(fp)                 :: qsy
      real(fp)                 :: qsu
      real(fp)                 :: qsv
      real(fp)                 :: r
      real(fp)                 :: rh
      real(fp)                 :: rl
      real(fp)                 :: r_ab
      real(fp)                 :: sfltc
      real(fp)                 :: sfltt
      real(fp)                 :: sflt
      real(fp)                 :: scr
      real(fp)                 :: scr_c
      real(fp)                 :: scr_t
      real(fp)                 :: sc
      real(fp)                 :: s1
      real(fp)                 :: sk
      real(fp)                 :: st
      real(fp)                 :: swc
      real(fp)                 :: swt
      real(fp)                 :: scx
      real(fp)                 :: scy
      real(fp)                 :: stx
      real(fp)                 :: sty
      real(fp)                 :: sc_sflt
      real(fp)                 :: st_sflt
      real(fp)                 :: swt_sflt
      real(fp)                 :: swcrepr
      real(fp)                 :: swtrepr
      real(fp)                 :: swc_sflt
      real(fp)                 :: screpr
      real(fp)                 :: strepr
      real(fp)                 :: scxrepr
      real(fp)                 :: scyrepr
      real(fp)                 :: stxrepr
      real(fp)                 :: styrepr
      real(fp)                 :: screpr_v
      real(fp)                 :: screpr_u
      real(fp)                 :: strepr_v
      real(fp)                 :: strepr_u
      real(fp)                 :: tc
      real(fp)                 :: tt
      real(fp)                 :: tcu
      real(fp)                 :: tcd
      real(fp)                 :: ttu
      real(fp)                 :: ttd
      real(fp), dimension(:), allocatable :: tw
      real(fp)                 :: theta
      real(fp)                 :: ustar
      real(fp)                 :: unet      ! magnitude of current velocity [umod] at reference level zumod [m/s]
      real(fp)                 :: urms      ! rootmeansquare orbital velocity [m/s]
      real(fp)                 :: uwc
      real(fp)                 :: uwt
      real(fp)                 :: uw
      real(fp)                 :: uwcrepr
      real(fp)                 :: uwtrepr
      real(fp), dimension(:), allocatable :: uorb_time_serie
      real(fp)                 :: ur
      real(fp)                 :: ucxrepr
      real(fp)                 :: ucyrepr
      real(fp)                 :: ucrepr
      real(fp)                 :: utxrepr
      real(fp)                 :: utyrepr
      real(fp)                 :: utrepr
      real(fp)                 :: ucx
      real(fp)                 :: ucy
      real(fp)                 :: utx
      real(fp)                 :: uty
      real(fp)                 :: uc
      real(fp)                 :: ut
      real(fp)                 :: unet_delwblt
      real(fp)                 :: uwmax
      real(fp)                 :: uwmin
      real(fp)                 :: wss
      real(fp)                 :: zref           ! reference level net current velocity [m]
!
!! executable statements -------------------------------------------------------
!
    nt = 200 ! must equal length of tw, uorb_time_serie, and ub_ts, ab_ts in santoss_orb
    allocate(tw             (nt), STAT = istat)
    allocate(uorb_time_serie(nt), STAT = istat)
    !
    d         = real(realpar(RP_DEPTH),fp)  ! d water depth [m]
    d50       = real(realpar(RP_D50)  ,fp)  ! sediment diameter of fraction [m]
    d90       = real(realpar(RP_D90MX),fp)  ! 90-percentile diameter of local sediment mixture [m]
    hrms      = real(realpar(RP_HRMS) ,fp)  ! wave height [m]
    tp        = real(realpar(RP_TPEAK),fp)  ! wave period [s]
    uorb      = real(realpar(RP_UORB) ,fp)  ! orbital velocity at the bed [m/s]
    teta      = real(realpar(RP_TETA) ,fp)  ! angle between wave dir and local grid orientation [deg]
    uuu       = real(realpar(RP_UCHAR),fp)  ! m component of characteristic velocity [m/s]
    vvv       = real(realpar(RP_VCHAR),fp)  ! n component of characteristic velocity [m/s]
    umod      = real(realpar(RP_VELCH),fp)  ! magnitude of characteristic velocity [m/s]
    zumod     = real(realpar(RP_ZVLCH),fp)  ! height above bed of characteristic velocity [m]
    ag        = real(realpar(RP_GRAV) ,fp)  ! gravitational acceleration [m/s2]
    vicmol    = real(realpar(RP_VICML),fp)  ! molecular viscosity of water [m2/s]
    rhosol    = real(realpar(RP_RHOSL),fp)  ! solid sediment density [kg/m3]
    rhowat    = real(realpar(RP_RHOWT),fp)  ! local water density [kg/m3]

    uwcrepr =  0.0_fp
    uwtrepr =  0.0_fp

    sw_effects = int(par(20))
    as_effects = int(par(21))
    pl_effects = int(par(22))
    sl_effects = int(par(23))

    error = .false.
!
! set some parameters.
!
    hw      = sqrt2*hrms              ! significant wave height [m]
    pcr     = 1.0_fp
    zref    = zumod                   ! zmud is computed in dwnvel; for 3d it is the centre of the lowest layer above deltas; for 2d it equals h/e
    tanphi  = tan(30.0_fp*degrad)     ! fixed the angle of repose of the natural talud to 30 degrees
!
!   determine the rms orbital velocity
!
!   uorb is computed in orbvel (roller) or setwav (SWAN); based on hrms, for a sine wave orbital time-series with uorb as amplitude corresponds to sqrt(2)*urms
    urms  = 0.5_fp*sqrt2*uorb
!
!   if there is no current and no waves the sand transport is zero
!
    if (comparereal(urms,0.0_fp) == 0 .and. comparereal(umod,0.0_fp) == 0) then
        qsu = 0.0_fp
        qsv = 0.0_fp
    else
!
!       current strength and angle between current and wave propagation direction
!
        unet = sqrt(uuu**2+vvv**2)
        ang_cur = atan2(vvv,uuu)*raddeg
        ang_ubot=teta

        if (ang_cur >= ang_ubot) then
            ang = ang_cur-ang_ubot
        else 
            ang = ang_cur-ang_ubot+360.0_fp
        endif
!
!       calculate velocity and acceleration parameters for the wave 
!       approximation of Abreu et al. (2010) by Reussink et al. (2012)
!
        call santoss_rrr12(ag, hw, tp, d, sk, as, phi_ab, r_ab, ur, km)
!
!       calculate wave time serie by Abreu et al. (2010)
!
        call santoss_abreu(hrms, km, d, r_ab, phi_ab, urms, tp, nt, tw, uorb_time_serie)
!
!       check wether it is current alone
!
        nr_timesteps = 0.0_fp
        uwmax = 0.0_fp
        uwmin = 0.0_fp
        do i = 1, nt
            uwmax = max(uwmax,uorb_time_serie(i))
            uwmin = min(uwmin,uorb_time_serie(i))
        enddo

        if (comparereal(uwmax,0.0_fp) == 0 .and. comparereal(uwmin,0.0_fp) == 0) then
            uw      =  0.0_fp
            aw      =  0.0_fp
            uwt     =  0.0_fp
            uwc     =  0.0_fp
            uwcrepr =  0.0_fp
            uwtrepr =  0.0_fp
            !
            ucxrepr =  uwcrepr + unet*cos(ang*degrad) ! x-component representative crest velocity   (wave+current)
            ucyrepr =            unet*sin(ang*degrad) ! y-component representative crest velocity   (wave+current)
            ucrepr=sqrt(ucxrepr**2+ucyrepr**2)        ! representative crest velocity   (wave+current)
            !
            utxrepr = -uwtrepr + unet*cos(ang*degrad) ! x-component representative trough velocity  (wave+current)
            utyrepr =            unet*sin(ang*degrad) ! y-component representative trough velocity  (wave+current)
            utrepr=sqrt(utxrepr**2+utyrepr**2)        ! representative trough velocity  (wave+current)
            !
            r = 0.5_fp
            b = 0.5_fp
        else
!
!           calculate orbital periods and velocities
!           for this first call set unet = 0.
!
            call santoss_orb(nt, as_effects, tw, uorb_time_serie, 0.0_fp, ang, tp, &
                     & rhowat, d, hw, aw, uw, uwc, uwt, uwcrepr, uwtrepr, &
                     & tc, tcu, tcd, tt, ttu, ttd, uc, ut, ucx, utx, ucy, uty, &
                     & ucxrepr, utxrepr, ucrepr, utrepr, b)
        endif
        delta = (rhosol-rhowat)/rhowat
!
!       calculate ripple height and length
!
        call santoss_ripple(d50, uwc, uwt, delta, ag, aw, rh, rl)
!
!       calculate shields parameter
!
!       first for the case of only a current near the bed
        if (comparereal(uwmax,0.0_fp)==0 .and. comparereal(uwmin,0.0_fp)==0) then
            call santoss_bsscurrent(i2d3d, ag, d, d50, d90, delta, unet, ang, &
                     & zref, rh, rl, unet_delwblt, delwblt, sc, scx, scy)
            screpr  = sc
            scxrepr = scx
            scyrepr = scy
            strepr  = 0.0_fp
            stxrepr = 0.0_fp
            styrepr = 0.0_fp
            st      = 0.0_fp
            stx     = 0.0_fp
            sty     = 0.0_fp
            swc     = 0.0_fp
            swt     = 0.0_fp
            tc      = tp
            tcu     = 0.0_fp
            tcd     = 0.0_fp
            tt      = 0.0_fp
            ttu     = 0.0_fp
            ttd     = 0.0_fp
            sw_effects=0
            as_effects=0
        else
!           define general friction factor (wave + current) and net velocity at edge boundary layer
!           using regular waves: so one friction factor and unet_delwblt for whole time serie.
!           first call to santoss_bss1 using aw, uw, uwc and uwt based on santoss_orb call with unet_delwblt=0
            call santoss_bss1(i2d3d, ag, d, d50, d90, delta, aw, uw, &
                     & unet, zref, rh, rl, uwc, uwt, ang, uc, ut, &
                     & theta, ksw, ksc, fc, fw, fcw, unet_delwblt, alpha, delwblt)

!           final definition of (representative) velocities and partial periods
            call santoss_orb(nt, as_effects, tw, uorb_time_serie, unet_delwblt, ang, tp, &
                     & rhowat, d, hw, aw, uw, uwc, uwt, uwcrepr, uwtrepr, &
                     & tc, tcu, tcd, tt, ttu, ttd, uc, ut, ucx, utx, ucy, uty, &
                     & ucxrepr, utxrepr, ucrepr, utrepr, b)
            r = uwc/(uwc+uwt)

!           final call to santoss_bss1 using aw, uw, uwc and uwt based on santoss_orb call with unet_delwblt/=0
            call santoss_bss1(i2d3d, ag, d, d50, d90, delta, aw, uw, &
                     & unet_delwblt, delwblt, rh, rl, uwc, uwt, ang, uc, ut, &
                     & theta, ksw, ksc, fc, fw, fcw, unet_delwblt, alpha, delwblt)

            ucxrepr = uwcrepr+unet_delwblt*cos(ang*degrad)     !x-component ucrest [m/s]
            ucyrepr = unet_delwblt*sin(ang*degrad)             !y-component ucrest [m/s]
            utxrepr = -uwtrepr+unet_delwblt*cos(ang*degrad)    !x-component utrough [m/s]
            utyrepr = unet_delwblt*sin(ang*degrad)             !y-component utrough [m/s]

            ! Shields based on maximum velocities: uwc, uwt, uc, ut
            call santoss_bss2(sw_effects, as_effects, ag, d, rhowat, rhosol, delta, &
                     & d50, d90, b, r, tp, uw, aw, uwc, uwt, uc, ut, unet_delwblt, &
                     & ang, delwblt, alpha, ksw, ksc, fw, fcw, tc, tt, tcu, tcd, &
                     & ttu, ttd, fc, sc, st, swc, swt, scx, scy, stx, sty, fcwc, fcwt)
            ! Shields based on representative velocities: uwcrepr, uwtrepr, ucrepr, utrepr
            call santoss_bss2(sw_effects, as_effects, ag, d, rhowat, rhosol, delta, &
                     & d50, d90, b, r, tp, uw, aw, uwcrepr, uwtrepr, ucrepr, utrepr, unet_delwblt, &
                     & ang, delwblt, alpha, ksw, ksc, fw, fcw, tc, tt, tcu, tcd, &
                     & ttu, ttd, fc, screpr, strepr, swcrepr, swtrepr, scxrepr, scyrepr, stxrepr, styrepr, fcwc, fcwt)
        endif
!
!       slope effect factors:
!          - fsl_cr_c and fsl_cr_t for correction on critical Shields stress
!            eqs. (3.27) and (3.28), Veen (2014)
!
        if (sl_effects==1.) then
            screpr_u = -scyrepr*sin(teta*degrad)+scxrepr*cos(teta*degrad)
            screpr_v =  scxrepr*sin(teta*degrad)+scyrepr*cos(teta*degrad)
            strepr_u = -styrepr*sin(teta*degrad)+stxrepr*cos(teta*degrad)
            strepr_v =  stxrepr*sin(teta*degrad)+styrepr*cos(teta*degrad)

            if (comparereal(screpr,0.0_fp)==0) then
                dzbds_c = 0.0_fp
            else
                dzbds_c = (screpr_u*-dzduu + screpr_v*-dzdvv)/screpr
            endif
            if (comparereal(strepr,0.0_fp)==0) then
                dzbds_t = 0.0_fp
            else
                dzbds_t = (strepr_u*-dzduu + strepr_v*-dzdvv)/strepr
            endif
            fsl_cr_c = sin(atan(tanphi)+atan(dzbds_c))/sin(atan(tanphi))
            fsl_cr_t = sin(atan(tanphi)+atan(dzbds_t))/sin(atan(tanphi))
        else
            fsl_cr_c = 1.0_fp
            fsl_cr_t = 1.0_fp
        endif

!       correction of the transport due to longitudinal and transverse slope in Delft3D
!
!       critical bed shear stress [-] (Soulsby, 1997, dynamics of marine sands)
        dstar = (delta*ag/vicmol**2)**(1.0_fp/3.0_fp)*d50
        scr = 0.3_fp/(1.0_fp+1.2_fp*dstar)+0.055_fp*(1.0_fp-exp(-0.02_fp*dstar))

!       effect of slope on critical shields stress
        scr_c = fsl_cr_c*scr
        scr_t = fsl_cr_t*scr

!       sheet-flow layer thickness [m]
        sc_sflt = sc
        st_sflt = st
        swc_sflt = swc
        swt_sflt = swt

!       calcuate max sheet flow layer thickness at crest and trough
        call santoss_sfltd99(d50, sc_sflt, st_sflt, swc_sflt, &
                 & swt_sflt, unet_delwblt, sfltc, sfltt)
        sflt = max(sfltc,sfltt)

!
!       fall velocity suspended sand [m/s] (Soulsby, 1997, dynamics of marine sands)
!
        d50s = 0.8_fp*d50
        dstars = (delta*ag/vicmol**2)**(1.0_fp/3.0_fp)*d50s
        wss=vicmol/d50s*(sqrt(10.36_fp**2+1.049_fp*dstars**3)-10.36_fp)

!
!       calculate transport rates
!
!       calibration constants SANTOSS report and ICCE paper
        n1 = 1.20_fp            ! power to the shields number    
        m1 = 10.9711_fp         ! multiplication factor 
        alphas = 8.2_fp         ! phase lag coefficient sheet-flow cases
        alphar = 8.2_fp         ! phase lag coefficient ripple cases   
        call santoss_core(pl_effects, sw_effects, ag, d50, d, hw, rhosol, rhowat, &
                 & delta, tp, r, b, tc, tt, tcu, tcd, ttu, ttd, sfltc, sfltt, &
                 & wss, rh, scr_c, scr_t, screpr, strepr, scxrepr, scyrepr, stxrepr, styrepr, &
                 & uwc, uwt, n1, m1, alphas, alphar, pcr, &
                 & pc, pt, oc, occ, oct, ot, ott, otc, phicx, phitx, &
                 & phicy, phity, qsx, qsy)
        qsu = -sin(teta*degrad)*qsy+cos(teta*degrad)*qsx
        qsv =  sin(teta*degrad)*qsx+cos(teta*degrad)*qsy
    endif

    ! check if qsu and qsv have a value
    if (isnan(qsu) .or. isnan(qsv)) then
        error = .true.
        message = 'qsu or qsv isnan in SANTOSS formula'
    endif
!
!   set the transport rates
!
    sbcu  = qsu*rhosol
    sbcv  = qsv*rhosol
    sbwu  = 0.0_fp
    sbwv  = 0.0_fp
    sswu  = 0.0_fp
    sswv  = 0.0_fp

    ! santoss specific output
    par     = -999.0_fp
    par( 1) = uwc
    par( 2) = uwt
    par( 3) = rh
    par( 4) = ksw
    par( 5) = ksc
    par( 6) = ucrepr
    par( 7) = utrepr
    par( 8) = fcwc
    par( 9) = fcwt
    par(10) = screpr
    par(11) = strepr
    par(12) = pc
    par(13) = pt
    par(14) = occ
    par(15) = otc
    par(16) = ott
    par(17) = oct
    par(18) = tc
    par(19) = tt
    par(20) = phicx
    par(21) = phitx
    par(22) = qsu
    par(23) = sk
    par(24) = as
    !
    deallocate(tw             , STAT = istat)
    deallocate(uorb_time_serie, STAT = istat)
end subroutine santoss
