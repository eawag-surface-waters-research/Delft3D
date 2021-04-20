!> Determines flow link' upwind/downwind parameters based on current velocities and water levels.
!! NOTE that this is purely for this flow link, independent of left-right orientation of the structure itself.
!! (Motivation: a single structure in 2D may be crossed by multiple flow links, with varying 1->2 orientation.)
subroutine flupdofm(m, il, ir, istru, velheight, rholeft, rhoright, crest, &
                    husb, hdsb, uu, ud, teken, relax)

use m_strucs
use m_flowgeom
use m_flow

implicit none
!
! Global variables
!
integer, intent(in)            :: m !< Flow link number, signed! If m < 0 then flow link is in opposite direction than structure left-right orientation.
integer, intent(in)            :: il,ir,istru
logical, intent(in)            :: velheight
double precision, intent(in)   :: crest, relax

double precision               :: hdsb
double precision               :: husb
double precision               :: rholeft
double precision               :: rhoright
double precision               :: teken
double precision               :: ud
double precision               :: uu

double precision               :: tem
!double precision               :: ucxku, ucyku
integer                        :: L, k, LL,iflip


! Parameters:
! NR NAME              IO DESCRIPTION
!  7 crest             I  crest.
! 10 hd                O  Downstream water level at t=(n+1).
!  9 husb                O  Upstream water level at t=(n+1).
!  2 il                I  Grid point on left side of structure (lower
!                         index).
!  3 ir                I  Grid point on right side of structure (upper
!                         index).
!  4 istru             I  structure number
!  1 m                 I  Index of velocity point, signed! If m < 0 then flow link is in opposite direction than structure left-right orientation.
!  5 rholeft           O  Density of diluted water on left grid point.
!  6 rhoright          O  Density of diluted water on left grid point.
! 13 teken             O  Flow direction (+1/-1).
! 12 ud                O  Downstream velocity.
! 11 uu                O  Upstream velocity.
!  8 velheight         I  True if velicity height will be taken into account

L    = abs(m)
iflip = max(0, sign(1,m)) ! iflip: 0 if flow link has same orientation as structure's left-right, 1 if opposite (because then the 'left' str point == ln(2,Lf))
if (relax .ne. 1d0) then
   husb = s1(il)*relax + (1.D0 - relax)*strhis2(9+iflip,istru) ! TODO: HK: strhis2 is not yet filled anywhere (no relaxation possible)
   hdsb = s1(ir)*relax + (1.D0 - relax)*strhis2(10-iflip,istru)
else
   husb = s1(il)
   hdsb = s1(ir)
endif
uu   = 0.D0
ud   = 0.D0
if (velheight) then
   uu = 0d0 ; ud = 0d0
   do k = 1,nd(il)%lnx
      LL = iabs( nd(il)%ln(k) )
      if (iadv(LL) .ne. 22) then  ! any non-structure point
         uu = max(uu, abs(u1(LL)) )
      endif
   enddo

   do k = 1,nd(ir)%lnx
      LL = iabs( nd(ir)%ln(k) )
      if (iadv(LL) .ne. 22) then  ! any non-structure point
         ud = max(ud, abs(u1(LL)) )
      endif
   enddo

   !uu = call getucxucynoweirs(il, ucxku, ucyku, 6)
   !uu = csu(L)*ucxku + snu(L)*ucyku
   !call getucxucynoweirs(ir, ucxku, ucyku, 6)
   !ud = csu(L)*ucxku + snu(L)*ucyku
endif

if (u1(L) > 0d0) then
   teken =  1d0
else if (u1(L) < 0d0) then
   teken = -1d0
else if (s1(iL) > s1(ir) ) then
   teken = 1d0
elseif (s1(iL) < s1(ir) ) then
   teken = -1d0
else ! s1(iL) == s1(ir)
   teken = -dble(sign(1,m)) ! account for orientation of flow link w.r.t. structure
endif

if (teken < 0) then
   tem   = hdsb ; hdsb = husb ; husb = tem
   tem   = ud   ; ud   = uu   ; uu   = tem
endif

end subroutine flupdofm
