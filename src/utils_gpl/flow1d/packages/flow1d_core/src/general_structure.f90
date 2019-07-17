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
   double precision            :: eps = 1d-5

   public ComputeGeneralStructure
   public dealloc

   interface dealloc
      module procedure deallocGenstru
   end interface dealloc


   type, public :: t_GeneralStructure ! see flgtar.f90
      double precision                 :: wu1                           !< w_u1
      double precision                 :: zu1                           !< z_u1
      double precision                 :: wu2                           !< w_u2
      double precision                 :: zu2                           !< z_u2
      double precision                 :: ws                            !< crest width
      double precision                 :: zs                            !< crest level
      double precision                 :: wd1                           !< w_d1
      double precision                 :: zd1                           !< z_d1
      double precision                 :: wd2                           !< w_d2
      double precision                 :: zd2                           !< z_d2
      double precision                 :: gateLowerEdgeLevel            !< gate lower edge level
      double precision                 :: cgf_pos                       !< Positive free gate flow function 
      double precision                 :: cgd_pos                       !< Positive drowned gate flow function 
      double precision                 :: cwf_pos                       !< Positive free weir flow function 
      double precision                 :: cwd_pos                       !< Positive drowned weir flow function 
      double precision                 :: mugf_pos                      !< Positive flow contraction coefficient function 
      double precision                 :: cgf_neg                       !< Negative free gate flow function 
      double precision                 :: cgd_neg                       !< Negative drowned gate flow function 
      double precision                 :: cwf_neg                       !< Negative free weir flow function 
      double precision                 :: cwd_neg                       !< Negative drowned weir flow function 
      double precision                 :: mugf_neg                      !< Negative flow contraction coefficient function 
      double precision                 :: extraresistance               !< Extra resistance
      double precision                 :: gatedoorheight                !< height of the doors
      double precision                 :: gateopeningwidth              !< width between the doors
      double precision                 :: crestlength                   !< length of the crest for computing the extra resistance using bedfriction over the crest of the weir
      double precision, pointer        :: widthcenteronlink(:)          !< For each crossed flow link the the center width portion of this genstr. (sum(widthcenteronlink(1:numlink)) should equal widthcenter)
      double precision, pointer        :: gateclosedfractiononlink(:)   !< part of the link width that is closed by the gate
      double precision, pointer        :: fu(:,:)                       !< fu(1:3,L0) contains the partial computational value for fu
      double precision, pointer        :: ru(:,:)                       !< ru(1:3,L0) contains the partial computational value for ru
      double precision, pointer        :: au(:,:)                       !< au(1:3,L0) contains the partial computational value for au
      integer                          :: numlinks                      !< Nr of flow links that cross this generalstructure.
      logical                          :: velheight                     !< Flag indicates the use of the velocity height or not
   end type


   private

contains

   !> compute FU, RU and AU for general structure genstr
   subroutine computeGeneralStructure(genstr, L0, maxWidth, fuL, ruL, s_on_crest, auL, as1, as2, dadsL, kfuL, s1m1, s1m2, &
                                      qtotal, Cz, dxL, dt, jarea, state)
      ! modules

      ! Global variables
      type(t_GeneralStructure), pointer, intent(in):: genstr        !< Derived type containing general structure information
      double precision, intent(in)                 :: maxWidth      !< Maximal width of the structure. Normally the the width of the flowlink
      double precision, intent(out)                :: fuL           !< fu component of momentum equation
      double precision, intent(out)                :: ruL           !< Right hand side component of momentum equation
      double precision, intent(out)                :: s_on_crest    !< Water level on crest
      double precision, intent(inout)              :: auL           !< Flow area of structue opening
      double precision, intent(in)                 :: as1           !< (geometrical) upstream flow area.
      double precision, intent(in)                 :: as2           !< (geometrical) downstream flow area.
      double precision, intent(out)                :: dadsL         !< flow width of structure
      integer, intent(in)                          :: L0            !< local link index
      integer, intent(out)                         :: kfuL          !< Flag indicating whether the structure link is wet (=1) or not (=0)
      double precision, intent(in)                 :: s1m1          !< (geometrical) upstream water level
      double precision, intent(in)                 :: s1m2          !< (geometrical) downstream water level
      double precision, intent(in)                 :: qtotal        !< Total discharge (in case of a compound structure this is not equal to 
                                                                    !< the discharge through the structure)
      double precision, intent(in)                 :: Cz            !< Chezy value
      double precision, intent(in)                 :: dxL           !< length of the flow link
      double precision, intent(in)                 :: dt            !< time step
      logical, intent(in)                          :: jarea         !< Flag indicating only the flow area is required or the full 
      integer, intent(inout)                       :: state         !< Flow state of the structure
      
      !
      !
      ! Local variables
      !
      double precision :: alm
      double precision :: arm
      double precision :: s1ml
      double precision :: s1mr
      double precision :: qL

      logical                        :: velheight
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
      double precision               :: rhoast
      double precision               :: rholeft
      double precision               :: rhoright
      double precision               :: flowDir
      double precision               :: ud
      double precision               :: uu
      double precision               :: w2
      double precision               :: wsd
      double precision               :: wstr
      double precision               :: zb2
      double precision               :: zs
      double precision               :: zgate
      double precision               :: gatefraction
      double precision               :: dx_struc  
      double precision               :: dsL
      double precision               :: u1L           
      double precision, dimension(3) :: fu
      double precision, dimension(3) :: ru
      double precision, dimension(3) :: au
      !
      !
      !! executable statements -------------------------------------------------------
      !

      alm  = as1
      arm  = as2
      s1ml = s1m1
      s1mr = s1m2
      dsL   = s1m2 - s1m1 
       
      crest = genstr%zs
      dx_struc = genstr%crestlength
      
      velheight = genstr%velheight
      !
      call UpAndDownstreamParameters(s1ml, s1mr, alm, arm, qtotal, velheight, &
                                     rholeft, rhoright, crest, hu, hd,uu, ud, flowDir)
      !
      
      call flgtar(genstr, L0, maxWidth, flowDir, zs, wstr, w2, wsd, zb2, ds1, ds2, cgf, cgd,   &
                  cwf, cwd, mugf, lambda)
      !
      rhoast = rhoright/rholeft
      if (flowDir < 0.0) rhoast = 1.0d0 / rhoast
      !
      
      gatefraction = genstr%gateclosedfractiononlink(L0)
      
      fu = genstr%fu(:,L0) 
      ru = genstr%ru(:,L0) 
      au = genstr%au(:,L0) 
      if (gatefraction > eps) then
         ! calculate flow under gate
         dg = genstr%gateLowerEdgeLevel - zs

         u1L = ru(1) - fu(1)*dsL 
         qL = Au(1)*u1L

         call flqhgs(fu(1), ru(1), u1L, dxL, dt, dadsL, kfuL, au(1), qL, flowDir, &
                     hu, hd, uu, zs, wstr, w2, wsd, zb2, ds1, ds2, dg,                &
                     rhoast, cgf, cgd, cwf, cwd, mugf, lambda, Cz, dx_struc, jarea, ds, state)
         
         !calculate flow over gate
         dg = huge(1d0)
         zgate = genstr%gateLowerEdgeLevel+genstr%gatedoorheight
         u1L = ru(2) - fu(2)*dsL 
         qL = Au(2)*u1L

         call flqhgs(fu(2), ru(2), u1L, dxL, dt, dadsL, kfuL, au(2), qL, flowDir, &
                     hu, hd, uu, zgate, wstr, w2, wsd, zb2, ds1, ds2, dg,                &
                     rhoast, cgf, cgd, cwf, cwd, mugf, 0d0, 0d0, dx_struc, jarea, ds, state)
      endif
      
      if (gatefraction< 1d0 - eps) then
         ! calculate flow asif no door is present
         dg = huge(1d0)
         u1L = ru(3) - fu(3)*dsL 
         qL = Au(3)*u1L
         
         call flqhgs(fu(3), ru(3), u1L, dxL, dt, dadsL, kfuL, au(3), qL, flowDir, &
                     hu, hd, uu, zs, wstr, w2, wsd, zb2, ds1, ds2, dg,                &
                     rhoast, cgf, cgd, cwf, cwd, mugf, lambda, Cz, dx_struc, jarea, ds, state)
      endif
      
      auL =  gatefraction*(      au(1)+      au(2)) +(1d0-gatefraction)*      au(3)
      if (auL > 0d0) then
         fuL = (gatefraction*(fu(1)*au(1)+fu(2)*au(2)) +(1d0-gatefraction)*fu(3)*au(3))/auL
         ruL = (gatefraction*(ru(1)*au(1)+ru(2)*au(2)) +(1d0-gatefraction)*ru(3)*au(3))/auL
      endif
      genstr%fu(:,L0) = fu
      genstr%ru(:,L0) = ru
      genstr%au(:,L0) = au
      !TEMP = laatste statement
      s_on_crest = ds + crest     ! waterlevel on crest
      
   end subroutine computeGeneralStructure

   !> Compute coefficients for structure equation                                   
   subroutine flgtar(genstr, L0, maxWidth, flowDir, zs, wstr, w2, wsd, zb2, ds1, ds2, cgf,  &
                     cgd, cwf, cwd, mugf, lambda)
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
      type(t_GeneralStructure), pointer, intent(in):: genstr    !< Derived type containing general structure information
      integer,          intent(in   )  :: L0                    !< Internal link number
      double precision, intent(in   )  :: maxWidth              !<  Maximal width of the structure. Normally the the width of the flowlink
      double precision, intent(  out)  :: cgd                   !< Contraction coefficient for drowned gate flow
      double precision, intent(  out)  :: cgf                   !< Contraction coefficient for gate flow
      double precision, intent(  out)  :: cwd                   !< Contraction coefficient for drowned weir flow.
      double precision, intent(  out)  :: cwf                   !< Contraction coefficient for free weir flow.
      double precision, intent(  out)  :: ds1                   !< Delta s1 general structure.
      double precision, intent(  out)  :: ds2                   !< Delta s2 general structure.
      double precision, intent(  out)  :: lambda                !< Extra resistance
      double precision, intent(  out)  :: mugf                  !< Vertical contraction coefficient for free gate flow.
      double precision, intent(in   )  :: flowDir               !< Flow direction (+1/-1). 
      double precision, intent(  out)  :: w2                    !< Width at right side of structure.
      double precision, intent(  out)  :: wsd                   !< Width structure right or left side.
      double precision, intent(  out)  :: wstr                  !< Width at centre of structure.
      double precision, intent(  out)  :: zb2                   !< Bed level at right side of structure.
      double precision, intent(  out)  :: zs                    !< Bed level at centre of structure.
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
     
      wstr = min(maxWidth, genstr%widthcenteronlink(L0))
      
      if (genstr%numlinks == 1) then
         w1   = min(maxWidth, genstr%wu1   )
         wsdl = min(maxWidth, genstr%wu2 )
         wstr = min(maxWidth, genstr%ws   )
         wsdr = min(maxWidth, genstr%wd1)
         w2   = min(maxWidth, genstr%wd2  )
      else  ! Structure crosses more than one link: nonsensible to use single width left/right etc. 
            ! same for all links. Use center linkwidth instead (i.e., typically wu(Lf))
         w1   = wstr
         wsdl = wstr
         wstr = wstr
         wsdr = wstr
         w2   = wstr
      endif
      
      zs = genstr%zs
      zb1 = genstr%zu1
      zbsl = genstr%zu2
      zbsr = genstr%zd1
      zb2 = genstr%zd2
      lambda = genstr%extraresistance
      !
      !     Determine cgf, cgd, cwf, cwd, mugf
      !     (flow direction dependent)
      !
      if (flowDir > 0.0D0) then
         cgf = genstr%cgf_pos
         cgd = genstr%cgd_pos
         cwf = genstr%cwf_pos
         cwd = genstr%cwd_pos
         mugf = genstr%mugf_pos
      else
         cgf = genstr%cgf_neg
         cgd = genstr%cgd_neg
         cwf = genstr%cwf_neg
         cwd = genstr%cwd_neg
         mugf = genstr%mugf_neg
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

   !> FLow QH relation for General Structure
   subroutine flqhgs(fuL, ruL, u1L, dxL, dt, dadsm, kfuL, auL, qL, flowDir, &
                  hu, hd, uu, zs, wstr, w2, wsd, zb2, ds1, ds2,   &
                  dg, rhoast, cgf, cgd, cwf, cwd, mugf, lambda, Cz, dx_struc,  &
                  jarea, ds, state)
       use m_GlobalParameters
       implicit none
      !
      ! Global variables
      !
      integer, intent(out)            :: kfuL      !< Flag indicating whether the structure link is wet (=1) or not (=0)
      logical, intent(in)             :: jarea     !< Flag indicating only the flow area is required 
      double precision, intent(inout) :: auL       !< flow area
      double precision, intent(inout) :: fuL       !< fu component of momentum equation
      double precision, intent(inout) :: ruL       !< Right hand side component of momentum equation
      double precision, intent(inout) :: u1L       !< Flow velocity at current time step
      double precision, intent(inout) :: qL        !< Discharge through structure
      double precision, intent(in)    :: dxL       !< Length of flow link
      double precision, intent(in)    :: dt        !< Time step 
      double precision, intent(out)  :: dadsm      !< Flow width
      double precision, intent(in)   :: cgd        !< Contraction coefficient for drowned gate flow
      double precision, intent(in)   :: cgf        !< Contraction coefficient for gate flow
      double precision, intent(in)   :: cwd        !< Contraction coefficient for drowned weir flow.
      double precision, intent(in)   :: cwf        !< Contraction coefficient for free weir flow.
      double precision, intent(in)   :: dg         !< Gate opening height.
      double precision, intent(inout):: ds         !< Water level immediately downstream the gate.
      double precision, intent(in)   :: ds1        !< Delta s1 general structure.
      double precision, intent(in)   :: ds2        !< Delta s2 general structure.
      double precision, intent(in)   :: hd         !< Downstream water level.
      double precision, intent(in)   :: hu         !< Upstream water level.
      double precision, intent(in)   :: lambda     !< Extra resistance
      double precision, intent(in)   :: Cz         !< Chezy value
      double precision, intent(in)   :: mugf       !< Vertical contraction coefficient for free gate flow.
      double precision, intent(in)   :: rhoast     !< Ratio of density right and left of structure
      double precision, intent(in)   :: flowDir    !< Flow direction (+1/-1).
      double precision, intent(in)   :: uu         !< Upstream velocity.
      double precision, intent(in)   :: w2         !< Width at right side of structure.
      double precision, intent(in)   :: wsd        !< Width structure right or left side.
      double precision, intent(in)   :: wstr       !< Width at centre of structure.
      double precision, intent(in)   :: zb2        !< Bed level at right side of structure.
      double precision, intent(in)   :: zs         !< Bed level at centre of structure.
      integer, intent(out)           :: state      !< Flow state of the structure
      double precision, intent(in)   :: dx_struc   !< length of structure
      
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
         call flgsarea(state, kfuL, auL, hu, velhght, zs, ds, dg, wstr)
      !
      else
         call flgsfuru(fuL, ruL, u1L, auL, qL, dxL, dt, dadsm, kfuL, state, &
                       flowDir, hu, hd, velhght, zs, ds, dg, dc, wstr,   &
                       cwfa, cwd, mugfa, cgfa, cgda, dx_struc, lambda, Cz)
      endif
   end subroutine flqhgs


   !>  Compute water depth ds at the sill by solving a third order algebraic equation. \n
   !!  In case of drowned weir flow the water level atthe sill is required. The water 
   !!  depth is calculated in this routine.                  
   subroutine flgsd3(wsd, wstr, zs, w2, zb2, ds1, ds2, elu, hd, rhoast, cwd,   &
                & ds, lambda)
      implicit none
      !
      ! Local parameters
      !
      double precision, parameter :: c23 = 2.0D0/3.0D0, c13 = 1.0D0/3.0D0
      !
      ! Global variables
      !
      double precision, intent(in)   :: cwd      !< 
      double precision, intent(out)  :: ds       !< 
      double precision, intent(in)   :: ds1      !< 
      double precision, intent(in)   :: ds2      !< 
      double precision, intent(in)   :: elu      !< 
      double precision, intent(in)   :: hd       !< 
      double precision, intent(in)   :: lambda   !< 
      double precision, intent(in)   :: rhoast   !< 
      double precision, intent(in)   :: w2       !< 
      double precision, intent(in)   :: wsd      !< 
      double precision, intent(in)   :: wstr     !< 
      double precision, intent(in)   :: zb2      !< 
      double precision, intent(in)   :: zs       !< 
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
                
                
   !> FLow contraction coefficients for general structure.\n
   !! In the formulas for the gate and weir several coefficients are applied. 
   !! To avoid discontinuities in the transition from weir to gate flow, the
   !!correction coefficient cgd should be corrected.
   subroutine flccgs(dg, dsc, cgd, cgf, cw, mugf, cgda, cgfa, mugfa)
      implicit none
      !
      ! Global variables
      !
      double precision, intent(in)   :: cgd     !< Correction coefficient for drowned gate flow.
      double precision, intent(out)  :: cgda    !< Adapted correction coefficient for drowned gate flow.
      double precision, intent(in)   :: cgf     !< Correction coefficient for free gate flow.
      double precision, intent(out)  :: cgfa    !< Adapted correction coefficient for free gate flow.
      double precision, intent(in)   :: cw      !< Correction coefficient for weir flow.
      double precision, intent(in)   :: dg      !< Gate opening height.
      double precision, intent(in)   :: dsc     !< Depth at sill or critical depth.
      double precision, intent(in)   :: mugf    !< Contraction coefficient for free gate flow.
      double precision, intent(out)  :: mugfa   !< Adapted contraction coefficient for free gate flow.
      !
      !
      !! executable statements -------------------------------------------------------
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


   !> FLGSD2 (FLow Gen. Struct. Depth sill 2nd ord. eq.)\n
   !! Compute water depth ds at the sill by a second order algebraic equation.
   !! In case of drowned gate flow the water level at the sill is required. 
   !! The water depth is calculated in this routine.
   subroutine flgsd2(wsd, wstr, zs, w2, zb2, dg, ds1, ds2, elu, hd, rhoast,    &
                   & cgd, imag, ds, lambda)
      implicit none
      !
      ! Local parameters
      !
      double precision, parameter :: c23 = 2.0D0/3.0D0, c13 = 1.0D0/3.0D0
      !
      ! Global variables
      !
      logical, intent(out)           :: imag      !< Logical indicator, = TRUE when determinant of second order algebraic equation less than zero.
      double precision, intent(in)   :: cgd       !< Correction coefficient for drowned gate flow.
      double precision, intent(in)   :: dg        !< Gate opening height.
      double precision, intent(out)  :: ds        !< Water level immediately downstream the gate.
      double precision, intent(in)   :: ds1       !< Delta s1 general structure.
      double precision, intent(in)   :: ds2       !< Delta s2 general structure.
      double precision, intent(in)   :: elu       !< Upstream energy level.
      double precision, intent(in)   :: hd        !< Downstream water level.
      double precision, intent(in)   :: lambda    !< Extra resistance in general structure.
      double precision, intent(in)   :: rhoast    !< Downstream water density divided by upstream water density.
      double precision, intent(in)   :: w2        !< Width at right side of structure.
      double precision, intent(in)   :: wsd       !< Width structure right or left side.
      double precision, intent(in)   :: wstr      !< Width at centre of structure.
      double precision, intent(in)   :: zb2       !< Bed level at right side of structure.
      double precision, intent(in)   :: zs        !< Bed level at centre of structure.
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

   !> FLGSAREA (FLow General Structure calculate AREA thru structure)
   !! The area through the general structure will be determined.
   !! The stage of the flow was already determined.
   subroutine flgsarea(state, kfuL, auL, hu, velhght, zs, ds, dg, wstr)
   
      implicit none
      !
      ! Global variables
      !
      integer, intent(in)             :: state      !< Flow condition of general structure:
                                                  !< * 0 : closed or dry
                                                  !< * 1 : free weir flow
                                                  !< * 2 : drowned weir flow
                                                  !< * 3 : free gate flow
                                                  !< * 4 : drowned gate flow
      integer, intent(out)            :: kfuL       !< Flag indicating whether the structure link is wet (=1) or not (=0)
      double precision, intent(inout) :: auL        !< Flow area of structue opening
      double precision, intent(in)    :: dg         !< Gate opening height. 
      double precision, intent(in)    :: ds         !< Water level immediately downstream the gate.
      double precision, intent(in)    :: hu         !< Upstream water level. 
      double precision, intent(in)    :: velhght    !< Velocity height
      double precision, intent(in)    :: wstr       !< Width at centre of structure.
      double precision, intent(in)    :: zs         !< Bed level at centre of structure.
      !
      !
      ! Local variables
      !
      double precision               :: hs1
      !
      !
      !! executable statements -------------------------------------------------------
      !
      if (state==0) then
         !        closed or dry
         auL = 0.0
         kfuL = 0
      else
         !
         !        Calculate upstream energy level w.r.t sill
         !
         hs1 = hu + velhght - zs
         kfuL = 1
         !
         if (state==1) then
            !           free weir flow
            auL = wstr*hs1*2.0D0/3.0D0
         elseif (state==2) then
            !           drowned weir flow
            auL = wstr*ds
         elseif (state==3) then
            !           free gate flow
            auL = wstr*dg
         elseif (state==4) then
            !           drowned gate flow
            auL = wstr*dg
         else
         endif
      endif
   end subroutine flgsarea

   !> FLow General Structure calculate FU and RU \n
   !!\n
   !! The linearization coefficients FU and RU are
   !! calculated for the general structure.\n
   !! The stage of the flow was already determined.
   subroutine flgsfuru(fuL, ruL, u1L, auL, qL, dxL, dt, dadsm, kfuL, state, &
                       flowDir, hu, hd, velhght, zs, ds, dg, dc, wstr,&
                       cwfa, cwd, mugfa, cgfa, cgda, dx_struc, lambda, Cz)
      use m_GlobalParameters
      use m_Weir
      implicit none
      !
      ! Local parameters
      !
      double precision, parameter :: relax = 0.0D0, alfa = 0.9D0
      !
      ! Global variables
      !
      integer, intent(in)            :: state      !< Flow condition of general structure: \n
                                                   !< 0 : closed or dry\n
                                                   !< 1 : free weir flow\n
                                                   !< 2 : drowned weir flow\n
                                                   !< 3 : free gate flow\n
                                                   !< 4 : drowned gate flow\n
      integer, intent(out)           :: kfuL       !< Flag indicating whether the structure link is wet (=1) or not (=0)
      double precision, intent(out)  :: fuL        !< fu component of momentum equation
      double precision, intent(out)  :: ruL        !< Right hand side component of momentum equation
      double precision, intent(inout):: u1L        !< Flow velocity at current time step
      double precision, intent(inout):: qL         !< Discharge through structure
      double precision, intent(inout):: auL        !< flow area 
      double precision, intent(out)  :: dadsm      !< Flow width
      double precision, intent(in)   :: dxL        !< Length of flow link
      double precision, intent(in)   :: dt         !< Time step
      double precision, intent(in)   :: cgda       !< Contraction coefficient for drowned gate flow (adapted)
      double precision, intent(in)   :: cgfa       !< Contraction coefficient for gate flow (adapted)
      double precision, intent(in)   :: cwd        !< Contraction coefficient for drowned weir flow.
      double precision, intent(in)   :: cwfa       !< Contraction coefficient for free weir flow. (adapted)
      double precision, intent(in)   :: dc         !< Critical water level (free gate flow)
      double precision, intent(in)   :: dg         !< Gate opening height.
      double precision, intent(in)   :: ds         !< Water level immediately downstream the gate.
      double precision, intent(in)   :: hd         !< Downstream water level.
      double precision, intent(in)   :: hu         !< Upstream water level.
      double precision, intent(in)   :: mugfa      !< Vertical contraction coefficient for free gate flow (adapted)
      double precision, intent(in)   :: flowDir    !< Flow direction (+1/-1).
      double precision, intent(in)   :: velhght    !< Velocity height
      double precision, intent(in)   :: wstr       !< Width at centre of structure.
      double precision, intent(in)   :: zs         !< Bed level at centre of structure.
      double precision, intent(in)   :: lambda     !< extra resistance
      double precision, intent(in)   :: cz         !< Chezy value
      double precision, intent(in)   :: dx_struc   !< length of structure
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
         kfuL = 0
         fuL = 0.0
         ruL = 0.0
         u1L = 0.0
         qL = 0.0
         auL = 0.0
         return
      endif
      !
      !     Calculate upstream energy level w.r.t sill
      !
      hs1 = hu + velhght - zs
      !
      dxdt = dxL/dt

      if (state==1) then
         !           free weir flow
         cu = cwfa**2*gravity/1.5D0
         !TEM        WRITE (11,*) cu,cwfa
         auL = wstr*hs1*2.0D0/3.0D0
         ustru = cwfa*dsqrt(gravity*2.0D0/3.0D0*hs1)
         rhsc = cu*(hd + velhght - zs)*flowDir
      elseif (state==2) then
         !           drowned weir flow
         cu = cwd**2*2.0D0*gravity
         auL = wstr*ds
         dh = max(hs1 - ds, 0.D0)
         ustru = cwd*dsqrt(gravity*2.0D0*dh)
         rhsc = cu*(hd + velhght - (ds + zs))*flowDir
      elseif (state==3) then
         !           free gate flow
         mu = mugfa*cgfa
         cu = mu**2*2.0D0*gravity
         auL = wstr*dg
         dh = max(hs1 - dc, 0.D0)
         ustru = mu*dsqrt(gravity*2.0D0*dh)
         rhsc = cu*(hd + velhght - (dc + zs))*flowDir
      elseif (state==4) then
         !           drowned gate flow
         mu = mugfa*cgda
         cu = mu**2*2.0D0*gravity
         auL = wstr*dg
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
      
      call furu_iter(fuL, ruL, su, sd, u1L, qL, auL, ustru, cu, rhsc, dxdt, dx_struc, hu, lambda, Cz)

      qL = auL*u1L
   end subroutine flgsfuru


   !> DPSEQU (EQUal test with Double precision interval EPSilon)\n
   !! Logical function to check if the difference between two double 
   !! precision values is lower than a defined interval epsilon.
   logical function GS_dpsequ(dvar1, dvar2, eps)

      implicit none
      !
      ! Global variables
      !
      double precision, intent(in)   :: dvar1   !< Double precision variable.
      double precision, intent(in)   :: dvar2   !< Double precision variable.
      double precision, intent(in)   :: eps     !< Interval epsilon.
      !
      !
      !! executable statements -------------------------------------------------------
      !
      GS_dpsequ = abs(dvar1 - dvar2)<eps
   end function GS_dpsequ
   
   !> deallocate general structure pointer
   subroutine deallocGenstru(genstru)
      implicit none
      
      type(t_GeneralStructure), pointer, intent(inout) :: genstru !< pointer to general structure data type
      
      if (associated(genstru%widthcenteronlink       )) deallocate(genstru%widthcenteronlink       )
      if (associated(genstru%gateclosedfractiononlink)) deallocate(genstru%gateclosedfractiononlink)
      if (associated(genstru%fu                      )) deallocate(genstru%fu                    )
      if (associated(genstru%ru                      )) deallocate(genstru%ru                    )
      if (associated(genstru%au                      )) deallocate(genstru%au                    )
      deallocate(genstru)
   end subroutine deallocGenstru
   
end module m_General_Structure
