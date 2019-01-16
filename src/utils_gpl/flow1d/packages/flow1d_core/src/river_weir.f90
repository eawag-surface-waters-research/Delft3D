module m_River_Weir
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
   use m_tables
   
   implicit none

   public ComputeRiverWeir
   public dealloc
   
   interface dealloc
      module procedure deallocRiverWeir
   end interface dealloc

   type, public :: t_riverweir
      double precision       :: crestlevel
      double precision       :: crestwidth
      double precision       :: pos_cwcoeff
      double precision       :: pos_slimlimit
      double precision       :: neg_cwcoeff
      double precision       :: neg_slimlimit
      double precision       :: dynstrucfact
      type(t_table), pointer :: pos_reducfact => null()
      type(t_table), pointer :: neg_reducfact => null()
   end type

   private

contains

   subroutine deallocRiverWeir(riverweir)
      ! Modules

      implicit none
      ! Input/output parameters
      type(t_riverweir), pointer   :: riverweir

      ! Local variables

      ! Program code
      if (associated(riverweir) ) then
         call dealloc(riverweir%pos_reducfact)
         call dealloc(riverweir%neg_reducfact)
         riverweir%pos_reducfact => null()
         riverweir%neg_reducfact => null()
         deallocate(riverweir)
      endif
      
      riverweir => null()
      
   end subroutine deallocRiverWeir
   
   subroutine ComputeRiverWeir(riverweir, fum, rum, aum, alm, arm, dadsm, kfum, s1m1, s1m2, s2m1, s2m2, &
                               qm, q0m, qtotal, u1m, u0m, fred, dxm, dt, area_only)
      !!--description-----------------------------------------------------------------
      ! NONE
      !!--pseudo code and references--------------------------------------------------
      ! NONE
      !!--declarations----------------------------------------------------------------
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
      ! Module:             FLSW (FLow structure Simple Weir)
      !
      ! Module description: In subroutine FLSW the Q-H relation for the
      !                     simple weir is defined.
      !
      !                     The following items can be controlled:
      !
      !                     Wstr = Width across flow section
      !                     Zs   = Crest level of weir
      !
      !
      ! Parameters:
      ! NR NAME              IO DESCRIPTION
      !  2 il                I  Grid point on left side of structure (lower
      !                         index).
      !  3 ir                I  Grid point on right side of structure (upper
      !                         index).
      !  7 jarea             I  If True then calculate only area
      !  1 m                 I  Grid index of structure
      !  4 istru             I  Number of structure.
      !  8 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
      !                         to maxtab. For a specific table number k and
      !                         function Y = f (X) the following definitions
      !                         exist:
      !                         (1,k) = Length of table k.
      !                         (2,k) = Start address X in table.
      !                         (3,k) = Start address Y in table.
      !                         (4,k) = Access method and period control: xy
      !                                 x = ctbnpf (0) : No period defined
      !                                 x = ctbpfu (1) : Period defined
      !                                 y = ctbico (0) : Continue interpltn
      !                                 y = ctbidi (1) : Discrete interpltn
      !  9 table             I  Contains drowned flow reduction curve.
      !
      ! Subprogram calls:
      ! NAME      DESCRIPTION
      ! flswar    FLow get Simple Weir ARguments
      ! flupdo    FLow UP- and DOwnstream near structure
      ! flswarea  FLow Simple Weir calculate AREA thru structure
      ! flqhsw    FLow QH relation for Simple Weir
      ! flqhswq   FLow QH relation for Simple Weir using Q
      ! flswfuru  FLow Simple Weir calculate FU and RU)
!=======================================================================

      implicit none
      !
      ! Local parameters
      !
      double precision, parameter :: twothird = 2.D0/3.D0
      !
      ! Global variables
      !
      type(t_riverweir), pointer, intent(in)    :: riverweir
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
      double precision, intent(inout)           :: fred
      double precision, intent(in)              :: dt
      logical, intent(in)                       :: area_only

      !
      !
      ! Local variables
      !
      logical                        :: isdry
      logical                        :: velheight
      double precision               :: crest
      double precision               :: cw
      double precision               :: fredhh
      double precision               :: hd
      double precision               :: hdcr
      double precision               :: hs1
      double precision               :: hs2
      double precision               :: hu
      double precision               :: relax
      double precision               :: rholeft
      double precision               :: rhoright
      double precision               :: slim
      double precision               :: strdamf
      double precision               :: flowdir
      double precision               :: ud
      double precision               :: uu
      double precision               :: velhght1
      double precision               :: velhght2
      double precision               :: wstr
      double precision               :: zs
      type(t_table), pointer     :: pred =>null()

      ! Velheight is always true for river structures
      velheight = .true.
      !
      crest = riverweir%crestlevel
      relax = min(0.5d0 + 0.5d0*fred/0.7d0, 1.0d0)
      !
      call UpAndDownstreamParameters(s1m1, s1m2, s2m1, s2m2, alm, arm, qtotal, velheight, rholeft, rhoright, &
                                     crest, hu, hd, uu, ud, flowdir, relax)

      call flswar(riverweir, flowdir, zs, wstr, cw, slim, pred, strdamf)

      !     Compute upstream and downstream velocity height and energy level
      velhght1 = uu * uu / (2.0D0 * gravity)
      hs1 = hu + velhght1 - zs
      velhght2 = ud * ud/ (2.0D0 * gravity)
      hs2 = hd + velhght2 - zs
      hdcr = hd - zs

      if (hs1 <= 0.0D0 .or. wstr <= 0.0D0) then
         ! Weir is dry or closed (width is zero)
         isdry = .true.
      else
         isdry = .false.
      endif

      if (area_only) then
         !
         !        calculate wetted area above crest
         !
     !    call flswarea(formno, m, hs1, hdcr, wstr)
         if (isdry) then
            ! Closed or dry
            kfum = 0
            aum  = 0.0d0
         else
            ! Calculate upstream energy level w.r.t sill
            kfum = 1
            aum  = wstr * max(hs1 * twothird, hdcr)
         endif

      else

         ! Calculate linearization coefficient FU and RU
         if (.not. isdry) then
            if (flowdir >= 0) then
               call flqhsw(hs1, hs2, slim, riverweir%pos_reducfact, fredhh)
            else
               call flqhsw(hs1, hs2, slim, riverweir%neg_reducfact, fredhh)
            endif

            fred = 0.5d0 * fredhh + 0.5d0 * fred

            if (flowdir >= 0) then
               call flswfuru(isdry, flowdir, s1m1, s1m2, dxm, dt, aum, dadsm, fum, rum, u1m, u0m, q0m, qm, &
                             kfum, hu, hd, hs1, hdcr, wstr, cw, fred, strdamf, riverweir%pos_reducfact)
            else
               call flswfuru(isdry, flowdir, s1m1, s1m2, dxm, dt, aum, dadsm, fum, rum, u1m, u0m, q0m, qm, &
                             kfum, hu, hd, hs1, hdcr, wstr, cw, fred, strdamf, riverweir%neg_reducfact)
            endif

         endif

      endif

      
   end subroutine ComputeRiverWeir

   subroutine flswar(riverweir, flowdir, zs, wstr, cw, slim, pred, strdamf)

      use m_GlobalParameters
    
      implicit none
      !
      ! Global variables
      !
      type(t_riverweir), pointer, intent(in)  :: riverweir
      type(t_table), pointer                  :: pred
      double precision, intent(out)           :: cw
      double precision, intent(out)           :: slim
      double precision                        :: strdamf
      double precision, intent(in)            :: flowdir
      double precision, intent(out)           :: wstr
      double precision, intent(out)           :: zs
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
      ! Module:             FLSWAR (FLow get Simple Weir ARguments)
      !
      ! Module description: Parameters for a simple weir are extracted from
      !                     module structures.
      !
      !
      ! Parameters:
      ! NR NAME              IO DESCRIPTION
      !  6 cw                O  Correction coefficient for weir flow.
      !  1 istru             I  Number of structure.
      !  7 slim              O  Submergence limit.
      !  3 flowdir           I  Flow direction (+/-).
      !  5 wstr              O  Width at centre of structure.
      !  4 zs                O  Bed level at centre of structure.
      !=======================================================================
      !     Include Pluvius data space
      !     Declaration of parameters:
      !
      !     Determine crest height and crest width
      !
    
      zs = riverweir%crestlevel
      wstr = riverweir%crestwidth
      !
      !     Determine Cw, Slim and table pointer
      !     (flow direction dependent)
      !
      if (flowdir >= 0.0D0) then
         cw   = riverweir%pos_cwcoeff
         slim = riverweir%pos_slimlimit
         pred => riverweir%pos_reducfact
      else
         cw   = riverweir%neg_cwcoeff
         slim = riverweir%neg_slimlimit 
         pred => riverweir%neg_reducfact
      endif
      
      strdamf = riverweir%dynstrucfact
      if (strdamf< -0.5D0) strdamf = dynstructext
      
   end subroutine flswar

   subroutine flqhsw(hs1, hs2, slim, pred, fred)

      implicit none
      !
      ! Global variables
      !
      double precision, intent(out)  :: fred
      double precision, intent(in)   :: hs1
      double precision, intent(in)   :: hs2
      double precision, intent(in)   :: slim

      type(t_table)              :: pred
      !
      !
      ! Local variables
      !
      double precision               :: sf
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
      ! Module:             FLQHSW (FLow QH relation for Simple Weir)
      !
      ! Module description: Subroutine FLQHSW defines the QH-relationship for
      !                     a simple weir.
      !
      !                     In subroutine FLQHSW for given upstream and down-
      !                     stream water levels the value of the drowned flow
      !                     reduction factor will be computed according to the
      !                     specific stage-discharge equation (QH-relation)
      !                     for the weir.
      !
      !                     Note: by definition the upstream side is defined
      !                     as the side on which the highest water level
      !                     exists.
      !
      !
      ! Parameters:
      ! NR NAME              IO DESCRIPTION
      !  7 fred              O  Drowned flow reduction factor
      !  1 hs1               I  Upstream energy head w.r.t. crest
      !  2 hs2               I  Downstream energy head w.r.t. crest
      !  5 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
      !                         to maxtab. For a specific table number k and
      !                         function Y = f (X) the following definitions
      !                         exist:
      !                         (1,k) = Length of table k.
      !                         (2,k) = Start address X in table.
      !                         (3,k) = Start address Y in table.
      !                         (4,k) = Access method and period control: xy
      !                                 x = ctbnpf (0) : No period defined
      !                                 x = ctbpfu (1) : Period defined
      !                                 y = ctbico (0) : Continue interpltn
      !                                 y = ctbidi (1) : Discrete interpltn
      !  3 slim              I  Submergence limit.
      !  6 table(ntabm)      I  Contains drowned flow reduction curve.
      !
      ! Subprogram calls:
      ! NAME    DESCRIPTION
      ! inttab  INTerpolate in TABle
      !=======================================================================
      !     Include Pluvius data space
      !     Declaration of parameters:
      !     Declaration of local variables:
      !
      sf = hs2/hs1
      !TEM  WRITE (11,*) 'sf',sf
      sf = min(max(sf, 0.0D0), 1.0D0)
      !
      if (sf<=slim) then
         fred = 1.0D0
      else
         !TEM      fred4 = 1.142533 - 64.59 * (sf - .867)**2
         !
         fred = interpolate(pred, sf)
      endif
      
   end subroutine flqhsw

   subroutine flswfuru(isdry, flowdir, s1m1, s1m2, dxm, dt, aum, dadsm, fum, rum, u1m, u0m, q0m, qm, &
                       kfum, hu, hd, hs1, hdcr, wstr, cw, fred, strdamf, pred)

      implicit none
      !
      ! Local parameters
      !
      double precision, parameter :: twothird = 2.0D0/3.0D0
      double precision, parameter :: fredl = 0.01D0
      !
      ! Global variables
      !
      logical, intent(in)             :: isdry
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

      double precision, intent(in)   :: cw
      double precision, intent(in)   :: fred
      double precision, intent(in)   :: hd
      double precision, intent(in)   :: hdcr
      double precision, intent(in)   :: hs1
      double precision, intent(in)   :: hu
      double precision, intent(in)   :: strdamf
      double precision, intent(in)   :: flowdir
      double precision, intent(in)   :: wstr
      type(t_table)                  :: pred
      !
      !
      ! Local variables
      !
      double precision               :: sf
      double precision               :: areacor
      double precision               :: cu
      double precision               :: dsqrt
      double precision               :: dxdt
      double precision               :: muac
      double precision               :: submercor
      double precision               :: ustru
      double precision               :: su
      double precision               :: sd
      double precision               :: rhsc

      logical, external              :: iterfuru
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
      ! Module:             FLSWURU (FLow Simple Weir, calculate FU and RU)
      !
      ! Module description: The linearization coefficients FU and RU are
      !                     calculated for the simple weir
      !
      !
      ! Parameters:
      ! NR NAME              IO DESCRIPTION
      ! 10 cw                I  Contraction coefficient for weir flow.
      !  1 isdry             I  Flow condition of general structure:
      !                         true : closed or dry
      !                         false : open
      ! 11 fred              I  Drowned flow reduction factor
      !  5 hd                I  Downstream water level.
      !  8 hdcr              I  Adapted downstream water level w.r.t. crest.
      !  6 hs1               I  Upstream energy head w.r.t. crest
      !  7 hs2               I  Downstream energy head w.r.t. crest
      !  4 hu                I  Upstream water level.
      !  2 m                 I  Grid index of structure
      !  3 flowdir           I  Flow direction (+1/-1).
      !  9 wstr              I  Width at centre of structure.
      !=======================================================================
 
      if (isdry) then
         ! Closed or dry
         kfum = 0
         fum = 0.0
         rum = 0.0
         u1m = 0.0
         qm = 0.0
         aum = 0.0
         return
      endif
      
      dxdt = strdamf*dxm/dt
      
         if (hdcr < twothird * hs1) then
            areacor = 1.0D0
         else
            areacor = twothird * hs1 / hdcr
         endif
         
         if (fred > fredl) then
            submercor = fred * hs1 / max(hu - hd, 0.0001d0)    ! hk: max = fix for Erwin, ARS 15132
         else
            call inttab_dp(pred%length, pred%interpoltype, pred%y, pred%x, fredl, sf, pred%stcount)
            submercor = fredl / (1.0d0 - sf)
         endif
         
         muac = cw * areacor
         cu = twothird * muac * muac * gravity * submercor * fred
         aum = wstr * hs1 * twothird / areacor
         ustru = muac * fred * dsqrt(gravity * twothird * hs1)
         
         dadsm = wstr
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
      
   end subroutine flswfuru

   subroutine inttab_dp(nxy, access, xar, yar, xs, ys, si)

      implicit none
      !
      ! Global variables
      !
      integer, intent(in)            :: access
      integer, intent(in)            :: nxy
      integer                        :: si
      double precision, intent(out)  :: ys
      double precision, intent(in)   :: xs
      double precision, dimension(nxy), intent(in) :: xar
      double precision, dimension(nxy), intent(in) :: yar
      !
      !
      ! Local variables
      !
      integer                        :: i
      integer                        :: ind
      integer                        :: perp1
      integer                        :: perp2
      integer                        :: sa
      logical                        :: period
      double precision               :: bgn
      double precision               :: dmod
      double precision               :: fween
      double precision               :: lt
      double precision               :: rint
      !
      !
      !! executable statements -------------------------------------------------------
      !
      !
      !=======================================================================
      !                       Deltares
      !                One-Two Dimensional Modelling System
      !                           S O B E K
      !
      ! Subsystem:          Flow Module
      !
      ! Programmer:         G. Stelling
      !
      ! Module:             INTTAB_DP (INTerpolate TABle data Double Precision)
      !                     This version returns the interpolates value in double precision
      !                     Where needed the call for INTTAB can be replaced by INTTAB_DP
      !
      ! Module description: Interpolation in tables which are stored in the standard
      !                     datastructure TABLE. si stores te table index
      !
      !
      !     update information
      !     person                    date
      !
      !
      !
      !     Declaration of parameters:
      !
      !
      !     Declaration of local variables:
      !
      !
      !     Period defined ?
      !
      period = access/10==1
      !     in  lowland and urban periodical functions are not used and therefore
      !     period is made as false
      if (period) then
         if (xar(2)<xar(nxy)) then
            !
            !           Increasing arguments
            perp1 = 2
            perp2 = nxy
         else
            !
            !           Decreasing arguments
            perp1 = nxy
            perp2 = 2
         endif
         fween = xar(perp1)
         bgn = xar(perp2) - xar(1)
         if (bgn<fween) then
            bgn = fween
         else
            !           interpolation itself is not periodical
            period = .false.
         endif
         if (xs>xar(perp2)) then
            !           periodical; x > 0
            lt = dmod(xs - bgn, xar(1)) + bgn
         elseif ((xs<xar(perp1)) .and. (bgn<fween + 1.0D-2)) then
            !           periodical; x < 0
            lt = dmod(xs - bgn, xar(1)) + bgn + xar(1)
         else
            !           run-in part (also for x < 0)
            lt = xs
         endif
         sa = 2
      else
         lt = xs
         sa = 1
      endif
      !
      !     to be on safe side
      if (si==0) si = sa
      !
      !
      !     For discrete interpolation add small number to find proper
      !     table point if xs=xar(i)
      !
      if (mod(access, 10)==1) then
         lt = lt*1.0000005d0
      endif
      !
      !     Index table and find X value
      !
      ind = 0
      if (xar(sa)<xar(nxy)) then
         if (lt<xar(si)) si = sa
         !
         !         do 100 i = sa, nxy
         do i = si, nxy
            if (lt>=xar(i)) then
               ind = i
            else
               exit
            endif
         enddo
      else
         if (lt<xar(si)) si = nxy
         !
         do i = si, sa, -1
            if (lt>=xar(i)) then
               ind = i
            else
               exit
            endif
         enddo
      endif
      !
      !     Check for continuous or discrete interpolation
      !
      if (mod(access, 10)==0) then
         !
         !        Interpolate in X using ind and find Y value
         !
         if (xar(sa)<xar(nxy)) then
            if (ind==nxy) then
               if (period) then
                  rint = (lt - xar(nxy))/(xar(sa) + xar(1) - xar(nxy))
                  ys = yar(nxy) + rint*(yar(sa) - yar(nxy))
               else
                  ys = yar(nxy)
               endif
            elseif (ind==0) then
               if (period) then
                  rint = (lt - xar(sa))/(xar(nxy) + xar(1) - xar(sa))
                  ys = yar(sa) + rint*(yar(nxy) - yar(sa))
               else
                  ys = yar(sa)
               endif
            else
               rint = (lt - xar(ind))/(xar(ind + 1) - xar(ind))
               ys = yar(ind) + rint*(yar(ind + 1) - yar(ind))
            endif
         elseif (ind==sa) then
            ys = yar(sa)
         elseif (ind==0) then
            ys = yar(nxy)
         else
            rint = (lt - xar(ind))/(xar(ind - 1) - xar(ind))
            ys = yar(ind) + rint*(yar(ind - 1) - yar(ind))
         endif
      !
      !        Find Y value using ind
      !
      elseif (xar(sa)<xar(nxy)) then
         if (ind==0) then
            ys = yar(sa)
         else
            ys = yar(ind)
         endif
      elseif (ind==0) then
         ys = yar(nxy)
      else
         ys = yar(ind)
      endif
      !
      !     store counter
      si = ind
   
   end subroutine inttab_dp

   
end module m_River_Weir
