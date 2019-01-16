module m_General_Structure
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2019.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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
!  $Id$
!  $HeadURL$
!-------------------------------------------------------------------------------
   
   use m_tables
   use m_struc_helper
   
   implicit none
   
   double precision, public    :: extra_resist_genstruc = 0d0

   public ComputeGeneralStructure

   type, public :: t_GeneralStructure ! see flgtar.f90
      double precision            :: widthleftW1
      double precision            :: levelleftZb1
      double precision            :: widthleftWsdl
      double precision            :: levelleftZbsl
      double precision            :: widthcenter
      double precision            :: levelcenter
      double precision            :: widthrightWsdr
      double precision            :: levelrightZbsr
      double precision            :: widthrightW2
      double precision            :: levelrightZb2
      double precision            :: gateheight
      double precision            :: gateheightintervalcntrl
      double precision            :: pos_freegateflowcoeff
      double precision            :: pos_drowngateflowcoeff
      double precision            :: pos_freeweirflowcoeff
      double precision            :: pos_drownweirflowcoeff
      double precision            :: pos_contrcoeffreegate
      double precision            :: neg_freegateflowcoeff
      double precision            :: neg_drowngateflowcoeff
      double precision            :: neg_freeweirflowcoeff
      double precision            :: neg_drownweirflowcoeff
      double precision            :: neg_contrcoeffreegate
      double precision            :: extraresistance
      double precision            :: stabilitycounter
      double precision            :: dynstrucfact
   end type


   private

contains

   subroutine computeGeneralStructure(GenStr, fuL, ruL, s_on_crest, auL, as1, as2, dadsL, kfuL, s1m1, s1m2, s2m1, s2m2, qL, &
                                      q0L, qtotalL, u1L, u0L, dxL, dt, firstiter, jarea, state)
      ! modules

      ! Global variables
      type(t_GeneralStructure), pointer, intent(in):: GenStr
      double precision, intent(out)                :: fuL
      double precision, intent(out)                :: ruL
      double precision, intent(out)                :: s_on_crest
      double precision, intent(inout)              :: auL
      double precision, intent(in)                 :: as1
      double precision, intent(in)                 :: as2
      double precision, intent(out)                :: dadsL
      integer, intent(out)                         :: kfuL
      double precision, intent(in)                 :: s1m1
      double precision, intent(in)                 :: s1m2
      double precision, intent(in)                 :: s2m1
      double precision, intent(in)                 :: s2m2
      double precision, intent(inout)              :: qL
      double precision, intent(in)                 :: q0L
      double precision, intent(in)                 :: qtotalL
      double precision, intent(inout)              :: u1L
      double precision, intent(in)                 :: u0L
      double precision, intent(in)                 :: dxL
      double precision, intent(in)                 :: dt
      logical, intent(in)                          :: firstiter
      logical, intent(in)                          :: jarea!    integer         :: il
      integer, intent(inout)                       :: state
      
!
!
! Local variables
!
      double precision :: fum
      double precision :: rum
      double precision :: alm
      double precision :: arm
      double precision :: s1ml
      double precision :: s1mr
      double precision :: s2ml
      double precision :: s2mr
      double precision :: qm
      double precision :: q0m
      double precision :: qtotal
      double precision :: u1m
      double precision :: u0m

      integer                        :: errornumber
      integer                        :: ker
      logical                        :: velheight
      double precision               :: abran
      character(8)                   :: string1
      double precision               :: cgd
      double precision               :: cgf
      double precision               :: crest
      double precision               :: cwd
      double precision               :: cwf
      double precision               :: dg
      double precision               :: ds
      double precision               :: ds1
      double precision               :: ds2
      double precision               :: hd
      double precision               :: hu
      double precision               :: lambda
      double precision               :: mugf
      double precision               :: relax
      double precision               :: rhoast
      double precision               :: rholeft
      double precision               :: rhoright
      double precision               :: strdamf
      double precision               :: flowDir
      double precision               :: ud
      double precision               :: uu
      double precision               :: w2
      double precision               :: wsd
      double precision               :: wstr
      double precision               :: zb2
      double precision               :: zs
!
!
!! executable statements -------------------------------------------------------
!

      alm  = as1
      arm  = as2
      s1ml = s1m1
      s1mr = s1m2
      s2ml = s2m1
      s2mr = s2m2
      qm   = qL
      q0m  = q0L
      qtotal = qtotalL
      u1m  = u1L
      u0m  = u0L
       
      crest = GenStr%levelcenter
      
      velheight = .true.
      !
      !
      relax = 1.0D0
      call UpAndDownstreamParameters(s1ml, s1mr, s2ml, s2mr, alm, arm, qtotal, velheight, &
                                     rholeft, rhoright, crest, hu, hd,uu, ud, flowDir, relax)
      !
      call flgtar(genstr, flowDir, zs, wstr, w2, wsd, zb2, dg, ds1, ds2, cgf, cgd,   &
                  cwf, cwd, mugf, lambda, strdamf)
      !
      if (firstiter) then
         if (nint(genstr%stabilitycounter)<20) then
            abran = min(alm, arm)
            if (auL > abran * 1.01d0) then
               genstr%stabilitycounter = genstr%stabilitycounter + 1.
               !              Area in general structure > area branch
               ker = 1
               errornumber = 120
               string1 = 'flgs'
!               call errmsg_sobek(ker, errornumber, string1, strucidnam(istru))
            endif
            if (dg<0.D0) then
               ker = 3
               errornumber = 125
               string1 = 'flgs'
!               call errmsg_sobek(ker, errornumber, string1, strucidnam(istru))
            endif
         endif
      endif
      !
      rhoast = rhoright/rholeft
      if (flowDir < 0.0) rhoast = 1.0d0 / rhoast
      !
      call flqhgs(fum, rum, u1m, u0m, dxL, dt, dadsL, kfuL, auL, qm, q0m, flowDir, &
                  hu, hd, uu, zs, wstr, w2, wsd, zb2, ds1, ds2, dg,                &
                  rhoast, cgf, cgd, cwf, cwd, mugf, lambda, strdamf, jarea, ds, state)
      !
      !TEMP = laatste statement
      s_on_crest = ds + crest     ! waterlevel on crest
      
      fuL = fum
      ruL = rum
      qL  = qm
      u1L = u1m
      
   end subroutine computeGeneralStructure

   subroutine flgtar(genstr, flowDir, zs, wstr, w2, wsd, zb2, dg, ds1, ds2, cgf,  &
                     cgd, cwf, cwd, mugf, lambda, strdamf)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use m_GlobalParameters
    implicit none
!
! Global variables
!
    type(t_GeneralStructure), pointer, intent(in):: GenStr
    double precision, intent(out)  :: cgd
    double precision, intent(out)  :: cgf
    double precision, intent(out)  :: cwd
    double precision, intent(out)  :: cwf
    double precision, intent(out)  :: dg
    double precision, intent(out)  :: ds1
    double precision, intent(out)  :: ds2
    double precision :: lambda
    double precision, intent(out)  :: mugf
    double precision :: strdamf
    double precision, intent(in)   :: flowDir
    double precision :: w2
    double precision, intent(out)  :: wsd
    double precision, intent(out)  :: wstr
    double precision :: zb2
    double precision :: zs
!
!
! Local variables
!
    double precision               :: help
    double precision               :: w1
    double precision               :: wsdl
    double precision               :: wsdr
    double precision               :: zb1
    double precision               :: zbsl
    double precision               :: zbsr
!
!
!! executable statements -------------------------------------------------------
!
    !
    !=======================================================================
    !                      Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:         J.Brouwer
    !
    ! Module:             FLGTAR (FLow get General sTructure ARguments)
    !
    ! Module description: Parameters for the general structure are extracted
    !                     from the structures module.
    !
    !
    ! Parameters:
    ! NR NAME              IO DESCRIPTION
    ! 13 cgd               O  Correction coefficient for drowned gate flow.
    ! 12 cgf               O  Correction coefficient for free gate flow.
    ! 15 cwd               O  Correction coefficient for drowned weir flow.
    ! 14 cwf               O  Correction coefficient for free weir flow.
    !  9 dg                O  Gate opening height.
    ! 10 ds1               O  Delta s1 general structure.
    ! 11 ds2               O  Delta s2 general structure.
    !  1 istru             I  Number of structure.
    ! 17 lambda            O  Extra resistance
    ! 16 mugf              O  Contraction coefficient for free gate flow.
    !  3 flowDir           I  Flow direction (+/-).
    !  6 w2                O  Width at right side of structure.
    !  7 wsd               O  Width structure right or left side.
    !  5 wstr              O  Width at centre of structure.
    !  8 zb2               O  Bed level at right side of structure.
    !  4 zs                O  Bed level at centre of structure.
    !=======================================================================
    !     Include Pluvius data space
    !
    !     Declaration of parameters:
    !
    !
    !     Declaration of local variables:
    !
    !
    !     Fetch parameters from structure info array
    !
    
    w1 = genstr%widthleftW1
    zb1 = genstr%levelleftZb1
    wsdl = genstr%widthleftWsdl
    zbsl = genstr%levelleftZbsl
    wstr = genstr%widthcenter
    zs = genstr%levelcenter
    wsdr = genstr%widthrightWsdr
    zbsr = genstr%levelrightZbsr
    w2 = genstr%widthrightW2
    zb2 = genstr%levelrightZb2
    dg = genstr%gateheight - zs
    lambda = genstr%extraresistance
    if (lambda< - 0.5D0) lambda = extra_resist_genstruc
    strdamf = genstr%dynstrucfact
    if (strdamf< - 0.5D0) strdamf = dynstructext
    !
    !     Determine cgf, cgd, cwf, cwd, mugf
    !     (flow direction dependent)
    !
    if (flowDir > 0.0D0) then
       cgf = genstr%pos_freegateflowcoeff
       cgd = genstr%pos_drowngateflowcoeff
       cwf = genstr%pos_freeweirflowcoeff
       cwd = genstr%pos_drownweirflowcoeff
       mugf = genstr%pos_contrcoeffreegate
    else
       cgf = genstr%neg_freegateflowcoeff
       cgd = genstr%neg_drowngateflowcoeff
       cwf = genstr%neg_freeweirflowcoeff
       cwd = genstr%neg_drownweirflowcoeff
       mugf = genstr%neg_contrcoeffreegate
    endif
    !
    !     Determine flow direction dependent parameters
    !
    if (flowDir > 0.0D0) then
       wsd = wsdr
       ds1 = zs - zbsr
       ds2 = zbsr - zb2
    else
       wsd = wsdl
       ds1 = zs - zbsl
       ds2 = zbsl - zb1
       help = w1
       w1 = w2
       w2 = help
       help = zb1
       zb1 = zb2
       zb2 = help
    endif
end subroutine flgtar

subroutine flqhgs(fum, rum, u1m, u0m, dxm, dt, dadsm, kfum, aum, qm, q0m, flowDir, &
                  hu, hd, uu, zs, wstr, w2, wsd, zb2, ds1, ds2,   &
                  dg, rhoast, cgf, cgd, cwf, cwd, mugf, lambda, strdamf,    &
                  jarea, ds, state)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use m_GlobalParameters
    implicit none
!
! Global variables
!
    integer, intent(out)            :: kfum
    logical, intent(in)             :: jarea
    double precision                :: aum
    double precision                :: fum
    double precision                :: rum
    double precision, intent(inout) :: u1m
    double precision, intent(in)    :: u0m
    double precision, intent(inout) :: qm
    double precision, intent(in)    :: q0m
    double precision                :: dxm
    double precision                :: dt
    double precision, intent(out)  :: dadsm
    double precision               :: cgd
    double precision               :: cgf
    double precision               :: cwd
    double precision, intent(in)   :: cwf
    double precision               :: dg
    double precision               :: ds
    double precision               :: ds1
    double precision               :: ds2
    double precision               :: hd
    double precision               :: hu
    double precision               :: lambda
    double precision               :: mugf
    double precision               :: rhoast
    double precision               :: strdamf
    double precision               :: flowDir
    double precision, intent(in)   :: uu
    double precision               :: w2
    double precision               :: wsd
    double precision               :: wstr
    double precision               :: zb2
    double precision               :: zs
    integer, intent(out)           :: state
!
!
! Local variables
!
    logical                        :: imag
    double precision               :: cgd2
    double precision               :: cgda
    double precision               :: cgfa
    double precision               :: cwfa
    double precision               :: dc
    double precision               :: dlim
    double precision               :: elu
    double precision               :: hd1
    double precision               :: hs1
    double precision               :: mugfa
    double precision               :: velhght
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !=======================================================================
    !                      Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:         J.Kuipers
    !
    ! Module:             FLQHGS (FLow QH relation for General Structure)
    !
    ! Module description: The QH-relationship for a general structure
    !                     will be transformed to a linearized equation
    !
    !                     In this subroutine for given upstream and down-
    !                     stream water levels and upstream velocity
    !                     the flow condition will be determined.
    !
    !
    ! Parameters:
    ! NR NAME              IO DESCRIPTION
    ! 16 cgd               I  Contraction coefficient for drowned gate flow
    ! 15 cgf               I  Contraction coefficient for gate flow
    ! 18 cwd               I  Contraction coefficient for drowned weir flow.
    ! 17 cwf               I  Contraction coefficient for free weir flow.
    ! 13 dg                I  Gate opening height.
    ! 22 ds                O  Water level immediately downstream the gate.
    ! 11 ds1               I  Delta s1 general structure.
    ! 12 ds2               I  Delta s2 general structure.
    !  4 hd                I  Downstream water level.
    !  3 hu                I  Upstream water level.
    ! 21 jarea             I  If True then claculate only area
    !  1 m                 I  Grid index of structure
    ! 20 lambda            I  Extra resistance
    ! 19 mugf              I  Vertical contraction coefficient for free
    !                         gate flow.
    ! 14 rhoast            I  Ratio of density right and left of structure
    !  2 flowDir           I  Flow direction (+1/-1).
    !  5 uu                I  Upstream velocity.
    !  8 w2                I  Width at right side of structure.
    !  9 wsd               I  Width structure right or left side.
    !  7 wstr              I  Width at centre of structure.
    ! 10 zb2               I  Bed level at right side of structure.
    !  6 zs                I  Bed level at centre of structure.
    !
    ! Subprogram calls:
    ! NAME     DESCRIPTION
    ! flccgs   FLow contraction coefficients for general structure
    ! flgsd2   FLow general structure depth sill 2nd order equation
    ! flgsd3   FLow general structure depth sill 3rd order equation
    ! flgsfuru FLow general structure calculate FU and RU
    !=======================================================================
    !     Include Pluvius data space
    !     Declaration of parameters:
    !     Declaration of local variables:
    !     Function declaration:
    !
    !     Compute upstream velocity height and energy level
    !
    velhght = uu*uu/(2.0D0*gravity)
    elu = hu + velhght
    hs1 = elu - zs
    !
    if (hs1<=0.0D0 .or. wstr<=0.0D0 .or. min(cgf, cgd, cwf, cwd)<=0. .or.       &
      & dg<.0001) then          !hk: or gate closed
       state = 0
    else
       !
       !        Compute critical water depth at the
       !        sill, dc and water depth at the sill,ds
       !
       dlim = hs1*(wstr/w2*2./3.*sqrt(2./3.))**(2.0/3.0)
       hd1 = max(hd, zb2 + dlim*0.9D0)
       !
       dc = 2.0D0/3.0D0*hs1
       !
       !        Calculate ds by solving third order algebraic equation
       !
       call flgsd3(wsd, wstr, zs, w2, zb2, ds1, ds2, elu, hd1, rhoast, cwd, ds, &
                 & lambda)
       !
       if (ds>=dc) then
          if (dg>=ds) then
             !
             !              - drowned weir -
             !
             state = 2
          else
             !
             !              - gate flow -
             !
             state = 3
             !
             !              adapt coefficients on basis of Ds & Cwd
             !
             call flccgs(dg, ds, cgd, cgf, cwd, mugf, cgda, cgfa, mugfa)
          endif
       else
          !
          !           Adapt Cwf coefficient
          !
          if (cwf<cwd) then
             if (GS_dpsequ(dc, 0.0D0, 1.0D-20)) then
                cwfa = cwf
             else
                cwfa = max(ds/dc*cwd, cwf)
             endif
          elseif (ds>0.0D0) then
             cwfa = min(dc/ds*cwd, cwf)
          else
             cwfa = cwf
          endif
          !
          if (dg>=dc) then
             !
             !              - free weir -
             !
             state = 1
             ds = dc
          else
             !
             !              - gate flow -
             !
             state = 3
             !
             !              adapt coefficients on basis of Dc & Cwf
             !
             call flccgs(dg, dc, cgd, cgf, cwfa, mugf, cgda, cgfa, mugfa)
          endif
       endif
       !
       !        In case of gate flow determine type of gate flow
       !        (drowned or free)
       !
       if (state==3) then
          dc = mugfa*dg
          !
          !           Cgd for second order equation = Cgd' * Mu'
          !
          cgd2 = cgda*mugfa
          !
          call flgsd2(wsd, wstr, zs, w2, zb2, dg, ds1, ds2, elu, hd1, rhoast,   &
                    & cgd2, imag, ds, lambda)
          !
          if (imag) then
             !
             !              - free gate -
             !
             state = 3
             ds = dc
          elseif (ds<=dc) then
             !
             !              - free gate -
             !
             state = 3
             !
             !             Adapt coefficients
             !
             if (cgda>cgfa) then
                if (.not.GS_dpsequ(dc, 0.0D0, 1.0D-20)) then
                   cgfa = max(ds/dc*cgda, cgfa)
                endif
             elseif (ds>0.0D0) then
                cgfa = min(dc/ds*cgda, cgfa)
             else
             endif
             ds = dc
          !TEM          WRITE (11,*) 'cgfa,mugfa',cgfa,mugfa
          else
             !
             !             - drowned gate -
             !
             state = 4
          endif
       endif
    !
    !
    endif
    !
    !TEM    WRITE (11,*) 'state,ds,dc,dg',state,ds,dc,dg
    !
    !       The flowe condition is known so calculate
    !       the linearization coefficients FU and RU
    !
    if (jarea) then
       call flgsarea(state, kfum, aum, hu, velhght, zs, ds, dg, wstr)
    !
    else
       call flgsfuru(fum, rum, u1m, u0m, aum, qm, q0m, dxm, dt, dadsm, kfum, state, &
                     flowDir, hu, hd, velhght, zs, ds, dg, dc, wstr,   &
                     cwfa, cwd, mugfa, cgfa, cgda, strdamf)
    endif
   end subroutine flqhgs

subroutine flgsd3(wsd, wstr, zs, w2, zb2, ds1, ds2, elu, hd, rhoast, cwd,   &
                & ds, lambda)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Local parameters
!
    double precision, parameter :: c23 = 2.0D0/3.0D0, c13 = 1.0D0/3.0D0
!
! Global variables
!
    double precision, intent(in)   :: cwd
    double precision, intent(out)  :: ds
    double precision, intent(in)   :: ds1
    double precision, intent(in)   :: ds2
    double precision, intent(in)   :: elu
    double precision, intent(in)   :: hd
    double precision, intent(in)   :: lambda
    double precision, intent(in)   :: rhoast
    double precision, intent(in)   :: w2
    double precision, intent(in)   :: wsd
    double precision, intent(in)   :: wstr
    double precision, intent(in)   :: zb2
    double precision, intent(in)   :: zs
!
!
! Local variables
!
    double precision               :: aw
    double precision               :: bw
    double precision               :: cw
    double precision               :: d2
    double precision               :: fac
    double precision               :: h2a
    double precision               :: h2b
    double precision               :: h2c
    double precision               :: hsl
    double precision               :: hulp
    double precision               :: hulp1
    double precision               :: p
    double precision               :: phi
    double precision               :: q
    double precision               :: r60
    double precision               :: term
    double precision               :: u
    double precision               :: v
!
!
!! executable statements -------------------------------------------------------
!
    !
    !=======================================================================
    !                      Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:         J.Brouwer/J.Kuipers
    !
    ! Module:             FLGSD3 (FLow Gen. Struct. Depth sill 3rd ord. eq.)
    !
    ! Module description: Compute water depth ds at the sill by solving a
    !                     third order algebraic equation.
    !
    !                     In case of drowned weir flow the water level at
    !                     the sill is required. The water depth is calcu-
    !                     lated in this routine.
    !
    !
    ! Parameters:
    ! NR NAME              IO DESCRIPTION
    ! 11 cwd               I  Correction coefficient for drowned weir flow.
    !  6 ds1               I  Delta s1 general structure.
    !  7 ds2               I  Delta s2 general structure.
    ! 12 ds                IO Water level immediately downstream the gate.
    !  8 elu               I  Upstream energy level.
    !  9 hd                I  Downstream water level.
    ! 13 lambda            I  Extra resistance in general structure.
    ! 10 rhoast            I  Downstream water density divided by upstream
    !                         water density.
    !  4 w2                I  Width at right side of structure.
    !  1 wsd               I  Width structure right or left side.
    !  2 wstr              I  Width at centre of structure.
    !  5 zb2               I  Bed level at right side of structure.
    !  3 zs                I  Bed level at centre of structure.
    !=======================================================================
    !
    !     Declaration of parameters:
    !
    !
    !     Declaration of local variables:
    !
    !
    !JK   LOGICAL uitput
    !JK   COMMON /UITPUT/uitput
    !
    !     Calculate Dw (=term), Aw, Bw and Cw according to appendix C of
    !     the design document River Rural integratietraject deel 3.
    !
    !JK   WRITE (11,*) 'FLGSD3'
    !JK   WRITE (11,*)      'wsd,wstr,zs ,w2 ,zb2,ds1 ,ds2 ,elu ,hd'    ,
    !JK  +                   wsd,wstr,zs ,w2 ,zb2,ds1 ,ds2 ,elu ,hd
    d2 = hd - zb2
    hsl = elu - zs
    !JK   WRITE (11,*)  'hsl',hsl
    term = ((4.0D0*cwd*cwd*rhoast*wstr*wstr)/(w2*d2))*(1.0D0 + lambda/d2)
    !
    aw = ( - term*hsl - 4.0D0*cwd*wstr + (1.0D0 - rhoast)                       &
       & *(w2/12.0D0 + wsd/4.0D0) + 0.5D0*(rhoast + 1.0D0)*(c13*w2 + c23*wsd))  &
       & /term
    !
    bw = (4.0D0*cwd*wstr*hsl + (1.0D0 - rhoast)                                 &
       & *((d2 + ds1)*(w2 + wsd)/6.D0 + ds1*wsd*c13) + 0.5D0*(rhoast + 1.0D0)   &
       & *((ds1 + ds2 - d2)*(c13*w2 + c23*wsd) + (c23*d2 + c13*ds1)             &
       & *w2 + (c13*d2 + c23*ds1)*wsd))/term
    !
    cw = ((1.0D0 - rhoast)*((d2 + ds1)**2*(w2 + wsd)/12.D0 + ds1**2*wsd/6.0D0)  &
       & + 0.5D0*(rhoast + 1.0D0)*(ds1 + ds2 - d2)                              &
       & *((c23*d2 + c13*ds1)*w2 + (c13*d2 + c23*ds1)*wsd))/term
    !
    !     Solve the equation ds**3 + aw*ds**2 + bw*ds +cw to get the water
    !     level at the sill
    !
    p = bw/3.0D0 - aw*aw/9.0D0
    q = aw*aw*aw/27.0D0 - aw*bw/6.0D0 + cw/2.0D0
    hulp = q*q + p*p*p
    !
    if (hulp<0.0D0) then
       p = abs(p)
       phi = acos(abs(q)/p/sqrt(p))/3.0D0
       r60 = acos(0.5D0)
       fac = sign(2.D0, q)*sqrt(p)
       h2a = -fac*cos(phi)
       h2b = fac*cos(r60 - phi)
       h2c = fac*cos(r60 + phi)
       ds = max(h2a, h2b, h2c) - aw/3.0D0
    else
       hulp = sqrt(hulp)
       hulp1 = -q + hulp
       if (abs(hulp1)<1E-6) then
          u = 0 ; v = 0
       else       ! hk: ook fix for Erwin, ARS 15132
          u = abs(hulp1)**c13*sign(1.0D0, hulp1)
          hulp1 = -q - hulp
          v = abs(hulp1)**c13*sign(1.0D0, hulp1)
       endif
       ds = u + v - aw/3.0D0
    endif
   end subroutine flgsd3
                
subroutine flccgs(dg, dsc, cgd, cgf, cw, mugf, cgda, cgfa, mugfa)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    double precision, intent(in)   :: cgd
    double precision, intent(out)  :: cgda
    double precision, intent(in)   :: cgf
    double precision, intent(out)  :: cgfa
    double precision, intent(in)   :: cw
    double precision :: dg
    double precision :: dsc
    double precision, intent(in)   :: mugf
    double precision, intent(out)  :: mugfa
!
!
! Local variables
!
!
!
!! executable statements -------------------------------------------------------
!
    !
    !=======================================================================
    !                      Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:         J.Brouwer/J.Kuipers
    !
    ! Module:             FLCCGS (FLow Corr. Coefficients for General Structure)
    !
    ! Module description: Correct coefficients for gate flow
    !
    !                     In the formulas for the gate and weir several
    !                     coefficients are applied. To avoid discontinuities
    !                     in the transition from weir to gate flow, the
    !                     correction coefficient cgd should be corrected.
    !
    !
    ! Parameters:
    ! NR NAME              IO DESCRIPTION
    !  3 cgd               I  Correction coefficient for drowned gate flow.
    !  7 cgda              O  Adapted correction coefficient for drowned
    !                         gate flow.
    !  4 cgf               I  Correction coefficient for free gate flow.
    !  8 cgfa              O  Adapted correction coefficient for free gate
    !                         flow.
    !  5 cw                I  Correction coefficient for weir flow.
    !  1 dg                I  Gate opening height.
    !  2 dsc               I  Depth at sill or critical depth.
    !  6 mugf              I  Contraction coefficient for free gate flow.
    !  9 mugfa             O  Adapted contraction coefficient for free gate
    !                         flow.
    !=======================================================================
    !
    !     Declaration of parameters:
    !
    !
    !     Logical function
    !
    !
    !     dsc contains ds or dc
    !
    if (.not.GS_dpsequ(dsc, 0.0D0, 1.D-20)) then
       !
       if (dg/dsc>mugf) then
          mugfa = dg/dsc
       else
          mugfa = mugf
       endif
       !
       if (cgd>cw) then
          if (GS_dpsequ(dg, 0.0D0, 1.0D-20)) then
             cgda = cgd
          else
             cgda = min(dsc/dg*cw, cgd)
          endif
       else
          cgda = max(dg/dsc*cw, cgd)
       endif
       !
       if (cgf>cw) then
          if (GS_dpsequ(dg, 0.0D0, 1.0D-20)) then
             cgfa = cgf
          else
             cgfa = min(dsc/dg*cw, cgf)
          endif
       else
          cgfa = max(dg/dsc*cw, cgf)
       endif
    !
    else
       mugfa = mugf
       cgda = cgd
       cgfa = cgf
    endif
end subroutine flccgs

subroutine flgsd2(wsd, wstr, zs, w2, zb2, dg, ds1, ds2, elu, hd, rhoast,    &
                & cgd, imag, ds, lambda)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Local parameters
!
    double precision, parameter :: c23 = 2.0D0/3.0D0, c13 = 1.0D0/3.0D0
!
! Global variables
!
    logical, intent(out)           :: imag
    double precision, intent(in)   :: cgd
    double precision, intent(in)   :: dg
    double precision, intent(out)  :: ds
    double precision, intent(in)   :: ds1
    double precision, intent(in)   :: ds2
    double precision, intent(in)   :: elu
    double precision, intent(in)   :: hd
    double precision, intent(in)   :: lambda
    double precision, intent(in)   :: rhoast
    double precision, intent(in)   :: w2
    double precision, intent(in)   :: wsd
    double precision, intent(in)   :: wstr
    double precision, intent(in)   :: zb2
    double precision, intent(in)   :: zs
!
!
! Local variables
!
    double precision               :: ag
    double precision               :: bg
    double precision               :: cg
    double precision               :: d2
    double precision               :: det
    double precision               :: hsl
    double precision               :: terma
    double precision               :: termb
!
!
!! executable statements -------------------------------------------------------
!
    !
    !=======================================================================
    !                      Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:         J.Brouwer/J.Kuipers
    !
    ! Module:             FLGSD2 (FLow Gen. Struct. Depth sill 2nd ord. eq.)
    !
    ! Module description: Compute water depth ds at the sill by a second
    !                     order algebraic equation.
    !
    !                     In case of drowned gate flow the water level at
    !                     the sill is required. The water depth is calcu-
    !                     lated in this routine.
    !
    !
    ! Parameters:
    ! NR NAME              IO DESCRIPTION
    ! 12 cgd               I  Correction coefficient for drowned gate flow.
    !  6 dg                I  Gate opening height.
    !  7 ds1               I  Delta s1 general structure.
    !  8 ds2               I  Delta s2 general structure.
    ! 14 ds                IO Water level immediately downstream the gate.
    !  9 elu               I  Upstream energy level.
    ! 10 hd                I  Downstream water level.
    ! 13 imag              O  Logical indicator, = TRUE when determinant of
    !                         second order algebraic equation less than
    !                         zero.
    ! 15 lambda            I  Extra resistance in general structure.
    ! 11 rhoast            I  Downstream water density divided by upstream
    !                         water density.
    !  4 w2                I  Width at right side of structure.
    !  1 wsd               I  Width structure right or left side.
    !  2 wstr              I  Width at centre of structure.
    !  5 zb2               I  Bed level at right side of structure.
    !  3 zs                I  Bed level at centre of structure.
    !=======================================================================
    !
    !     Declaration of parameters:
    !
    !
    !     Declaration of local variables:
    !
    !
    !JK   LOGICAL uitput
    !JK   COMMON /UITPUT/uitput
    !
    !     Calculate Ag, Bg and Cg according to appendix C of
    !     the design document River Rural integratietraject deel 3.
    !JK   WRITE  (11,*)  'IN FLGSD2 ----'
    !
    ag = (1.0D0 - rhoast)*(w2/12.0D0 + wsd/4.0D0) + 0.5D0*(rhoast + 1.0D0)      &
       & *(c13*w2 + c23*wsd)
    d2 = hd - zb2
    !
    terma = (4.0D0*rhoast*cgd*cgd*dg*dg*wstr*wstr)/(w2*d2)*(1.0D0 + lambda/d2)
    termb = 4.0D0*cgd*dg*wstr
    !
    bg = (1.0D0 - rhoast)*((d2 + ds1)*(w2 + wsd)/6.D0 + ds1*wsd*c13)            &
       & + 0.5D0*(rhoast + 1.0D0)                                               &
       & *((ds1 + ds2 - d2)*(c13*w2 + c23*wsd) + (c23*d2 + c13*ds1)             &
       & *w2 + (c13*d2 + c23*ds1)*wsd) + terma - termb
    !
    hsl = elu - zs
    !
    cg = (1.0D0 - rhoast)*((d2 + ds1)**2*(w2 + wsd)/12.D0 + ds1**2*wsd/6.0D0)   &
       & + 0.5D0*(rhoast + 1.0D0)*(ds1 + ds2 - d2)                              &
       & *((c23*d2 + c13*ds1)*w2 + (c13*d2 + c23*ds1)*wsd) - terma*hsl +        &
       & termb*hsl
    !
    det = bg*bg - 4.0D0*ag*cg
    if (det<0.0D0) then
       imag = .true.
    !JK      WRITE (11,*) 'Det=',det
    else
       imag = .false.
       ds = ( - bg + sqrt(det))/(2.0D0*ag)
    endif
end subroutine flgsd2

subroutine flgsarea(state, kfum, aum, hu, velhght, zs, ds, dg, wstr)

implicit none
!
! Global variables
!
    integer, intent(in)             :: state
    integer, intent(out)            :: kfum
    double precision, intent(inout) :: aum
    double precision, intent(in)    :: dg
    double precision, intent(in)    :: ds
    double precision, intent(in)    :: hu
    double precision, intent(in)    :: velhght
    double precision, intent(in)    :: wstr
    double precision, intent(in)    :: zs
!
!
! Local variables
!
    double precision               :: hs1
!
!
!! executable statements -------------------------------------------------------
!
      !=======================================================================
      ! Module:             FLGSAREA (FLow General Structure
      !                               calculate AREA thru structure)
      ! Module description: The area through the general structure will
      !                     be deermined.
      !                     The stage of the flow was already determined.
      !
      !
      ! Parameters:
      ! NR NAME              IO DESCRIPTION
      !  7 dg                I  Gate opening height.
      !  6 ds                I  Water level immediately downstream the gate.
      !  1 state            I  Flow condition of general structure:
      !                         0 : closed or dry
      !                         1 : free weir flow
      !                         2 : drowned weir flow
      !                         3 : free gate flow
      !                         4 : drowned gate flow
      !  3 hu                I  Upstream water level.
      !  2 m                 I  Grid index of structure
      !  4 velhght           I  Velocity height
      !  8 wstr              I  Width at centre of structure.
      !  5 zs                I  Bed level at centre of structure.
      !=======================================================================
      !     Include Pluvius data space
      !     Declaration of parameters:
      !     Declaration of local variables:
      !
      if (state==0) then
         !        closed or dry
         aum = 0.0
         kfum = 0
      else
         !
         !        Calculate upstream energy level w.r.t sill
         !
         hs1 = hu + velhght - zs
         kfum = 1
         !
         if (state==1) then
            !           free weir flow
            aum = wstr*hs1*2.0D0/3.0D0
         elseif (state==2) then
            !           drowned weir flow
            aum = wstr*ds
         elseif (state==3) then
            !           free gate flow
            aum = wstr*dg
         elseif (state==4) then
            !           drowned gate flow
            aum = wstr*dg
         else
         endif
      endif
   end subroutine flgsarea

       !> FLow General Structure calculate FU and RU \n
       !!\n
       !! The linearization coefficients FU and RU are
       !! calculated for the general structure.\n
       !! The stage of the flow was already determined.
   subroutine flgsfuru(fum, rum, u1m, u0m, aum, qm, q0m, dxm, dt, dadsm, kfum, state, &
                       flowDir, hu, hd, velhght, zs, ds, dg, dc, wstr,&
                       cwfa, cwd, mugfa, cgfa, cgda, strdamf)
   !!--declarations----------------------------------------------------------------
      use m_GlobalParameters
      use m_Weir
      implicit none
   !
   ! Local parameters
   !
      double precision, parameter :: relax = 0.0D0, alfa = 0.9D0
      !  7 dg                I  Gate opening height.
      !  6 ds                I  Water level immediately downstream the gate.
      !  1 state            I  Flow condition of general structure:
      !                         0 : closed or dry
      !                         1 : free weir flow
      !                         2 : drowned weir flow
      !                         3 : free gate flow
      !                         4 : drowned gate flow
      !  3 hu                I  Upstream water level.
      !  2 m                 I  Grid index of structure
      !  4 velhght           I  Velocity height
      !  8 wstr              I  Width at centre of structure.
      !  5 zs                I  Bed level at centre of structure.
      ! Parameters:
      ! NR NAME              IO DESCRIPTION
      ! 16 cgda              I  Contraction coefficient for drowned gate flow
      !                         (adapted)
      ! 15 cgfa              I  Contraction coefficient for gate flow
      !                         (adapted)
      ! 13 cwd               I  Contraction coefficient for drowned weir flow.
      ! 12 cwfa              I  Contraction coefficient for free weir flow.
      !                         (adapted)
      ! 10 dc                I  Critical water level (free gate flow)
      !  9 dg                I  Gate opening height.
      !  8 ds                I  Water level immediately downstream the gate.
      
   !
   ! Global variables
   !
                                                   !> Flow condition of general structure: \n
                                                   !! 0 : closed or dry\n
                                                   !! 1 : free weir flow\n
                                                   !! 2 : drowned weir flow\n
                                                   !! 3 : free gate flow\n
                                                   !! 4 : drowned gate flow\n
      integer, intent(in)            :: state     
      integer, intent(out)         :: kfum       !<
      double precision, intent(out)  :: fum        !<
      double precision, intent(out)  :: rum        !<
      double precision, intent(in)   :: u0m        !<
      double precision, intent(inout):: u1m        !<
      double precision, intent(inout):: qm         !<
      double precision, intent(in)   :: q0m        !<
      double precision, intent(inout):: aum        !<
      double precision, intent(out)  :: dadsm      !<
      double precision, intent(in)   :: dxm        !<
      double precision, intent(in)   :: dt         !<
      double precision, intent(in)   :: cgda       !<
      double precision, intent(in)   :: cgfa       !<
      double precision, intent(in)   :: cwd        !<
      double precision, intent(in)   :: cwfa       !<
      double precision, intent(in)   :: dc         !<
      double precision, intent(in)   :: dg         !<
      double precision, intent(in)   :: ds         !<
      double precision, intent(in)   :: hd         !< Downstream water level.
      double precision, intent(in)   :: hu         !< Upstream water level.
      double precision, intent(in)   :: mugfa      !< Vertical contraction coefficient for free gate flow (adapted)
      double precision, intent(in)   :: strdamf    !< 
      double precision, intent(in)   :: flowDir    !< Flow direction (+1/-1).
      double precision, intent(in)   :: velhght    !< Velocity height
      double precision, intent(in)   :: wstr       !< Width at centre of structure.
      double precision, intent(in)   :: zs         !< Bed level at centre of structure.
   !
   !
   ! Local variables
   !
      double precision               :: cu
      double precision               :: dh
      double precision               :: dsqrt
      double precision               :: dxdt
      double precision               :: hs1
      double precision               :: mu
      double precision               :: rhsc
      double precision               :: ustru
      double precision               :: su
      double precision               :: sd

      logical, external              :: iterfuru
   !
   !! executable statements -------------------------------------------------------
   !
       !
      if (state==0) then
         !        closed or dry
         kfum = 0
         fum = 0.0
         rum = 0.0
         u1m = 0.0
         qm = 0.0
         aum = 0.0
         return
      endif
      !
      !     Calculate upstream energy level w.r.t sill
      !
      hs1 = hu + velhght - zs
      !
      dxdt = strdamf*dxm/dt

         if (state==1) then
            !           free weir flow
            cu = cwfa**2*gravity/1.5D0
            !TEM        WRITE (11,*) cu,cwfa
            aum = wstr*hs1*2.0D0/3.0D0
            ustru = cwfa*dsqrt(gravity*2.0D0/3.0D0*hs1)
            rhsc = cu*(hd + velhght - zs)*flowDir
         elseif (state==2) then
            !           drowned weir flow
            cu = cwd**2*2.0D0*gravity
            aum = wstr*ds
            dh = max(hs1 - ds, 0.D0)
            ustru = cwd*dsqrt(gravity*2.0D0*dh)
            rhsc = cu*(hd + velhght - (ds + zs))*flowDir
         elseif (state==3) then
            !           free gate flow
            mu = mugfa*cgfa
            cu = mu**2*2.0D0*gravity
            aum = wstr*dg
            dh = max(hs1 - dc, 0.D0)
            ustru = mu*dsqrt(gravity*2.0D0*dh)
            rhsc = cu*(hd + velhght - (dc + zs))*flowDir
         elseif (state==4) then
            !           drowned gate flow
            mu = mugfa*cgda
            cu = mu**2*2.0D0*gravity
            aum = wstr*dg
            dh = max(hs1 - ds, 0.D0)
            ustru = mu*dsqrt(gravity*2.0D0*dh)
            rhsc = cu*(hd + velhght - (ds + zs))*flowDir
         endif
         
         dadsm = wstr
         !
         if (flowDir>0) then
             su = hu
             sd = hd
         else
             sd = hu
             su = hd
         endif
         
         call furu_iter(fum, rum, su, sd, u1m, u0m, q0m, aum, ustru, cu, rhsc, dxdt)

         qm = aum*u1m
   end subroutine flgsfuru
                       
logical function GS_dpsequ(dvar1, dvar2, eps)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    double precision, intent(in)   :: dvar1
    double precision, intent(in)   :: dvar2
    double precision, intent(in)   :: eps
!
!
!! executable statements -------------------------------------------------------
!
    !
    !=======================================================================
    !                      Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          General routines Module
    !
    ! Programmer:         J.Brouwer
    !
    ! Module:             DPSEQU (EQUal test with Double precision interval EPSilon)
    !
    ! Module description: Logical function to check if the difference be-
    !                     tween two double precision values is lower than a
    !                     defined interval epsilon.
    !
    !
    ! Parameters:
    ! NR NAME              IO DESCRIPTION
    !  0 dpsequ            O  -
    !  1 dvar1             I  Double precision variable.
    !  2 dvar2             I  Double precision variable.
    !  3 eps               I  Interval epsilon.
    !=======================================================================
    !
    !     Declaration of function:
    !
    !
    !     Declaration of parameters:
    !
    !
    GS_dpsequ = abs(dvar1 - dvar2)<eps
end function GS_dpsequ

end module m_General_Structure
