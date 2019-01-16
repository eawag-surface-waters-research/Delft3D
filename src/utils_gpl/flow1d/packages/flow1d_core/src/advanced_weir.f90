module m_Advanced_Weir
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
   
   use m_GlobalParameters
   use m_struc_helper
   
   implicit none

   public ComputeAdvancedWeir
   
   type, public :: t_advweir 
      double precision         :: crestlevel
      double precision         :: totwidth
      integer                  :: npiers
      double precision         :: pos_height
      double precision         :: pos_designhead
      double precision         :: pos_piercontractcoeff
      double precision         :: pos_abutcontractcoeff
      double precision         :: neg_height
      double precision         :: neg_designhead
      double precision         :: neg_piercontractcoeff
      double precision         :: neg_abutcontractcoeff
      double precision         :: dynstrucfact
   end type

   private

contains

   subroutine ComputeAdvancedWeir(advweir, fum, rum, aum, alm, arm, dadsm, kfum, s1m1, s1m2, s2m1, s2m2, &
                                  qm, q0m, qtotal, u1m, u0m, dxm, dt, area_only)
      !!--description-----------------------------------------------------------------
      ! NONE
      !!--pseudo code and references--------------------------------------------------
      ! NONE
      !!--declarations----------------------------------------------------------------
      !=======================================================================
      !                      Deltares
      !                One-Two Dimensional Modelling System
      !                           S O B E K
      !
      ! Subsystem:          Flow Module
      !
      ! Programmer:         J.Kuipers
      !
      ! Module:             FLAW (FLow structure Advanced Weir)
      !
      ! Module description: In subroutine FLAW the Q-H relation for the
      !                     advanced weir is defined.
      !
      !                     The following input parameters are available for
      !                     this type of structure:
      !
      !                     -   Level of crest Zs
      !                     -   Total net width Wn
      !                     -   Number of piers N
      !                     -   Heigth of upstream face P
      !                     -   Design head H0 of the weir
      !                     -   Pier contraction coefficient Kp
      !                     -   Abutment contraction coefficient Ka
      !
      !
      ! Parameters:
      ! NR NAME              IO DESCRIPTION
      !  7 area_only         I  If True then calculate only area
      !
      ! Subprogram calls:
      ! NAME    DESCRIPTION
      ! flawar    FLow get Advanced Weir ARguments
      ! flqhaw    FLow QH relation for Advanced Weir
      ! flawfuru  FLow Advanced Weir calculate FU and RU)

      implicit none
      !
      ! Local parameters
      !
      double precision, parameter :: twothird = 2.D0/3.D0
      !
      ! Global variables
      !
      type(t_advweir), pointer, intent(in)      :: advweir
      integer, intent(out)                      :: kfum
      double precision, intent(inout)           :: aum
      double precision, intent(in)              :: alm
      double precision, intent(in)              :: arm
      double precision, intent(out)             :: dadsm
      double precision, intent(out)             :: fum
      double precision, intent(in)              :: qtotal
      double precision, intent(inout)           :: q0m
      double precision, intent(out)             :: qm
      double precision, intent(out)             :: rum
      double precision, intent(in)              :: u0m
      double precision, intent(inout)           :: u1m
      double precision, intent(in)              :: s1m1
      double precision, intent(in)              :: s1m2
      double precision, intent(in)              :: s2m1
      double precision, intent(in)              :: s2m2
      double precision, intent(in)              :: dxm
      double precision, intent(in)              :: dt
      logical, intent(in)                       :: area_only

      !
      !
      ! Local variables
      !
      logical                        :: isdry
      logical                        :: velheight
      double precision               :: c1
      double precision               :: crest
      double precision               :: ct
      double precision               :: h0
      double precision               :: hd
      double precision               :: hdcr
      double precision               :: hs1
      double precision               :: hu
      double precision               :: ka
      double precision               :: kp
      double precision               :: n
      double precision               :: p
      double precision               :: relax
      double precision               :: rholeft
      double precision               :: rhoright
      double precision               :: strdamf
      double precision               :: flowdir
      double precision               :: ud
      double precision               :: uu
      double precision               :: velhght
      double precision               :: wact
      double precision               :: wn
      double precision               :: zs

      !    Velheight is always true for river structures
      velheight = .true.

      crest = advweir%crestlevel
      relax = 0.5d0

      call UpAndDownstreamParameters(s1m1, s1m2, s2m1, s2m2, alm, arm, qtotal, velheight, rholeft, rhoright, &
                                     crest, hu, hd, uu, ud, flowdir, relax)
                                     
      !
      !     Determine crest height, total net width and number of piers
      !     Determine P, H0, Kp and Ka (flow direction dependent)
      !
      call flawar(advweir, flowdir, h0, ka, kp, n, p, wn, zs, strdamf)
      !
      !     Compute upstream velocity height and energy level
      !
      velhght = uu * uu / (2.0D0 * gravity)
      hs1 = hu + velhght - zs
      hdcr = hd - zs

      if (hs1 < 1.0D-10) then
         ! weir is dry
         isdry = .true.
      else
         isdry = .false.
      endif

      if (area_only) then

         ! calculate wetted area above crest
         call flqhaw(area_only, hs1, hdcr, h0, ka, kp, n, p, wn, c1, ct, wact)

         ! calculate area thru structure
         if (isdry) then
            ! closed or dry
            kfum = 0
            aum  = 0.0d0
         else
            ! Calculate upstream energy level w.r.t sill
            kfum = 1
            aum = wact * max(hs1 * twothird, hdcr)
         endif
         
      else

         if (.not. isdry) then
         
            call flqhaw(area_only, hs1, hdcr, h0, ka, kp, n, p, wn, c1, ct, wact)
            
            ! Is weir closed? (width is zero)
            if (abs(wact - 0.0D0) < 1.0D-10) isdry = .true.
            
         endif

         ! Calculate linearization coefficient FU and RU
         call advweir_furu(isdry, flowdir, s1m1, s1m2, dxm, dt, aum, dadsm, fum, rum, &
                           u1m, u0m, q0m, qm, kfum, hu, hd, hs1, hdcr, wact, c1, ct, velhght, strdamf)

      endif

   end subroutine ComputeAdvancedWeir


   subroutine flawar(advweir, flowdir, h0, ka, kp, n, p, wn, zs, strdamf)
   
      implicit none
      !
      ! Global variables
      !
      type(t_advweir), pointer, intent(in)   :: advweir
      double precision, intent(out)          :: h0
      double precision, intent(out)          :: ka
      double precision, intent(out)          :: kp
      double precision, intent(out)          :: n
      double precision, intent(out)          :: p
      double precision                       :: strdamf
      double precision, intent(in)           :: flowdir
      double precision, intent(out)          :: wn
      double precision, intent(out)          :: zs
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
      ! Subsystem:          Flow module
      !
      ! Programmer:         J.Brouwer/J.Kuipers
      !
      ! Module:             FLAWAR (Flow get Advanced Weir Arguments)
      !
      ! Module description: Parameters for a advanced weir are extracted from
      !                     structure module
      !
      !
      ! Parameters:
      ! NR NAME              IO DESCRIPTION
      !  4 h0                O  Design head of the advanced weir.
      !  1 advweir           I  Advanced Weir Structure.
      !  5 ka                O  Abutment contraction coefficient.
      !  6 kp                O  Pier contraction coefficient.
      !  7 n                 O  Number of piers.
      !  8 p                 O  Height of upstream face P.
      !  3 flowdir           I  Flow direction (+/-).
      !  9 wn                O  Total net width.
      ! 10 zs                O  crest level
      !=======================================================================
      !     Include Pluvius data space
      !     Declaration of parameters:
      !
      !
      !     Determine crest height, total net width and number of piers
      !     -----------------------------------------------------------
      !

      zs = advweir%crestlevel
      wn = advweir%totwidth
      n  = advweir%npiers
      !
      !     Determine P, H0, Kp and Ka
      !     (flow direction dependent)
      !
      if (flowdir > 0.0D0) then
         p  = advweir%pos_height
         h0 = advweir%pos_designhead
         kp = advweir%pos_piercontractcoeff
         ka = advweir%pos_abutcontractcoeff
      else
         p  = advweir%neg_height
         h0 = advweir%neg_designhead
         kp = advweir%neg_piercontractcoeff
         ka = advweir%neg_abutcontractcoeff
      endif
      
      strdamf = advweir%dynstrucfact
      if (strdamf < -0.5d0) strdamf = dynstructext

   end subroutine flawar

   subroutine flqhaw(area_only, hs1, hdcr, h0, ka, kp, n, p, wn, c1, ct, wact)
   
      implicit none
      !
      ! Global variables
      !
      logical, intent(in)            :: area_only
      double precision, intent(out)  :: c1
      double precision, intent(out)  :: ct
      double precision, intent(in)   :: h0
      double precision, intent(in)   :: hdcr
      double precision, intent(in)   :: hs1
      double precision, intent(in)   :: ka
      double precision, intent(in)   :: kp
      double precision, intent(in)   :: n
      double precision, intent(in)   :: p
      double precision, intent(out)  :: wact
      double precision, intent(in)   :: wn
      !
      !
      ! Local variables
      !
      double precision               :: ch
      double precision               :: cnul
      double precision               :: corr
      double precision               :: cp
      double precision               :: x
      double precision               :: x1
      double precision               :: x12
      double precision               :: x13
      double precision               :: x2
      double precision               :: x3
      double precision               :: x4
      double precision               :: x5
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
      ! Programmer:         J.Brouwer / J.Kuipers
      !
      ! Module:             FLQHAW (FLow QH relation for Advanced Weir)
      !
      ! Module description: Subroutine FLQHAW defines the QH-relationship for
      !                     an advanced weir.
      !
      !                     In subroutine FLQHAW for given downstream and
      !                     upstream water levels the effective width and
      !                     the discharge coefficients c1 and ct of the
      !                     weir will be computed according to the specific
      !                     stage-discharge equation (QH-relation) for the
      !                     advanced weir.
      !
      !
      ! Parameters:
      ! NR NAME              IO DESCRIPTION
      ! 10 c1                O  discharge coefficient C1
      ! 11 ct                O  discharge coefficient Ct
      !  2 hs1               I  Upstream energy level w.r.t. crest
      !  3 hdcr              I  Downstream water level w.r.t. crest
      !  4 h0                I  Design head of the advanced weir.
      !  1 area_only         I  If True then calculate only area
      !  5 ka                I  Abutment contraction coefficient.
      !  6 kp                I  Pier contraction coefficient.
      !  7 n                 I  Number of piers.
      !  8 p                 I  Height of upstream face P.
      ! 12 wact              O  Actual width
      !  9 wn                I  Total net width.
      !=======================================================================

      wact = wn - 2.0D0 * (n * kp + ka) * hs1
      wact = max(wact, 0.0D0)
      !
      if (.not. area_only) then
         !
         x  = p / h0
         x2 = x * x
         x3 = x2 * x
         x4 = x2 * x2
         x5 = x3 * x2
         !
         !        Calculate basic correction coefficient -c0-
         !
         if (x < 2.0D0) then

            corr = -0.052D0 * x3 + 0.145D0 * x2 - 0.096D0 * x + 1.01D0
            
            cnul = (0.1256D0 * x5 - 1.0178D0 * x4 + 3.0D0 * x3 - 3.94D0 * x2 + 2.28D0 * x + 1.66D0) * corr / sqrt(2.0D0 * gravity)
             
         else
         
            cnul = 2.1549008D0 / sqrt(2.0D0 * gravity)
            
         endif
         !
         x1  = hs1 / h0
         x12 = x1 * x1
         x13 = x12 * x1
         !
         !        Correction coefficient for head / design head effects -ch-
         !
         if (x1 < 1.6D0) then
            ch = 0.1394D0 * x13 - 0.416D0 * x12 + 0.488D0 * x1 + 0.785D0
         else
            ch = 1.0718224D0
         endif
         !
         x2 = (hs1 - hdcr) / hs1
         !
         ct = ct_aweir(x2)
         !
         !        Correction coefficient for apron effect -cp-
         !
         cp = 1.0D0
         !
         c1 = cnul * ch * cp

      endif
      
   end subroutine flqhaw
   
   subroutine advweir_furu(isdry, flowdir, s1m1, s1m2, dxm, dt, aum, dadsm, fum, rum, u1m, u0m, q0m, qm, &
                           kfum, hu, hd, hs1, hdcr, wact, c1, ct, velhght, strdamf)
      implicit none
      !
      ! Local parameters
      !
      double precision, parameter :: twothird = 2.D0 / 3.D0
      double precision, parameter :: xlim = 0.005D0
      !
      ! Global variables
      !
      logical, intent(in)            :: isdry
      double precision, intent(in)    :: s1m1
      double precision, intent(in)    :: s1m2
      double precision, intent(in)    :: dxm
      double precision, intent(in)    :: dt
      double precision, intent(inout) :: aum
      double precision, intent(inout) :: dadsm
      double precision, intent(inout) :: fum
      double precision, intent(inout) :: rum
      double precision, intent(inout) :: u1m
      double precision, intent(in)    :: u0m
      double precision, intent(in)    :: q0m
      double precision, intent(out)   :: qm
      integer, intent(out)            :: kfum
      
      double precision, intent(in)   :: c1
      double precision, intent(in)   :: ct
      double precision, intent(in)   :: hd
      double precision, intent(in)   :: hdcr
      double precision, intent(in)   :: hs1
      double precision, intent(in)   :: hu
      double precision, intent(in)   :: strdamf
      double precision, intent(in)   :: flowdir
      double precision, intent(in)   :: velhght
      double precision, intent(in)   :: wact
      !
      !
      ! Local variables
      !
      double precision               :: areacor
      double precision               :: ctlim
      double precision               :: cu
      double precision               :: dsqrt
      double precision               :: dxdt
      double precision               :: muac
      double precision               :: submercor
      double precision               :: ustru
      double precision               :: x
      double precision               :: su
      double precision               :: sd
      double precision               :: rhsc
      integer, external              :: iterfuru
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
      ! Programmer:         J.Kuipers
      !
      ! Module:             FLAWURU (FLow Advanced Weir, calculate FU and RU)
      !
      ! Module description: The linearization coefficients FU and RU are
      !                     calculated for the advanced weir
      !
      !
      ! Parameters:
      ! NR NAME              IO DESCRIPTION
      !  9 c1                I  discharge coefficient C1
      ! 10 ct                I  discharge coefficient Ct
      !  1 isdry             I  Closed/Dry or Open
      !  5 hd                I  Downstream water level.
      !  7 hdcr              I  Adapted downstream water level w.r.t. crest.
      !  6 hs1               I  Upstream energy head w.r.t. crest
      !  4 hu                I  Upstream water level.
      !  2 m                 I  Grid index of structure
      !  3 teken             I  Flow direction (+1/-1).
      !  8 wact              I  Actual width at centre of structure.
      ! 11 velhght           I  Upstream velocity height
      !=======================================================================
      !     Include Pluvius data space
      !     Declaration of parameters:
      !     Declaration of local variables:
      !

      if (isdry) then
         ! Closed or dry
         kfum   = 0
         fum   = 0.0d0
         rum   = 0.0d0
         u1m   = 0.0d0
         qm    = 0.0d0
         aum   = 0.0d0
         return
      endif

      dxdt = strdamf * dxm / dt
      
      if (hdcr < hs1 * twothird) then
         areacor = 1.5D0
      else
         areacor = hs1 / hdcr
      endif
      x = (hs1 - hdcr) / hs1
      if (x > xlim) then
         submercor = ct * ct * hs1 / (hu - hd)
      else
         ctlim = ct_aweir(xlim)
         submercor = ctlim * ctlim / (xlim - velhght / hs1)
      endif
      !
      muac = c1 * areacor
      cu = 2.0D0 * muac * muac * gravity * submercor
      aum = wact * hs1 / areacor
      ustru = muac * ct * dsqrt(2.0D0 * gravity * hs1)
      dadsm = wact
         
      if (flowdir > 0.0d0) then
         su = hu
         sd = hd
      else
         sd = hu
         su = hd
      endif
         
      rhsc = 0.0d0
         
      call furu_iter(fum, rum, s1m2, s1m1, u1m, u0m, q0m, aum, ustru, cu, rhsc, dxdt)
      
      qm = aum * u1m
      
   end subroutine advweir_furu
   
   function ct_aweir(x)

      implicit none
      !
      ! Global variables
      !
      double precision :: ct_aweir
      double precision, intent(in)   :: x
      !
      !
      ! Local variables
      !
      double precision               :: ct
      double precision               :: ta
      double precision               :: ta2
      double precision               :: tb4
      double precision               :: x2
      !
      !
      !! executable statements -------------------------------------------------------
      !
      !
      !
      x2 = x
      if (x2 <= 1.0D-10) then
         ct = 0.0D0
      elseif (x2 <= 1.0D-2) then
         ct = x2 * 17.4546d0
      elseif (x2 <= 0.7D0) then
         ta = x2 - 0.7D0
         ta2 = ta * ta
         tb4 = ta2 * ta2
         ct = sqrt(1.0D0 - ta2 / 0.49D0) + 27.0D0 * tb4 * x2**1.5
      else
         ct = 1.0D0
      endif

      ct_aweir = max(ct, 1.0D-30)
      
   end function ct_aweir

end module m_Advanced_Weir
