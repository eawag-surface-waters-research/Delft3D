subroutine comp_sq(Ndkx, Lnkx, kbot, ktop, Lbot, Ltop, q1, qw, sq)
   use m_flowgeom, only: Ndx, Lnx, ln
   implicit none

   integer,                                intent(in)    :: Ndkx     !< total number of flownodes (dynamically changing)
   integer,                                intent(in)    :: Lnkx     !< total number of flowlinks (dynamically changing)
   integer,          dimension(Ndx),       intent(in)    :: kbot     !< flow-node based layer administration
   integer,          dimension(Ndx),       intent(in)    :: ktop     !< flow-node based layer administration
   integer,          dimension(Lnx),       intent(in)    :: Lbot     !< flow-link based layer administration
   integer,          dimension(Lnx),       intent(in)    :: Ltop     !< flow-link based layer administration
   double precision, dimension(Lnkx),      intent(in)    :: q1       !< flow-field discharges
   double precision, dimension(Ndkx),      intent(in)    :: qw       !< flow-field vertical discharges
   double precision, dimension(Ndkx),      intent(out)   :: sq       !< flux balance (inward positive)

   double precision                                      :: dum, sumsq

   integer                                               :: k1, k2, k, kk, L, LL

   sq = 0d0

!   write(6,*) q1(1714)-q1(1735)
!   write(6,*) qw(1736)-qw(1735)

!  vertical
   do kk=1,Ndx
      do k = kbot(kk),ktop(kk)-1
         sq(k)   = sq(k)   - qw(k)
         sq(k+1) = sq(k+1) + qw(k)
      end do

      if ( abs(qw(ktop(kk))).gt.1d-8 ) then
         continue
      end if

      if ( abs(qw(kbot(kk)-1)).gt.1d-8 ) then
         continue
      end if
   end do

!   write(6,"('after vertical: ',E)") sq(1736)

!  horizontal
   do LL=1,Lnx
      do L=Lbot(LL),Ltop(LL)
         k1 = ln(1,L)
         k2 = ln(2,L)
         sq(k1) = sq(k1) - q1(L)
         sq(k2) = sq(k2) + q1(L)
      end do
   end do

!   write(6,"('after horizontal: ',E)") sq(1736)

   sumsq = 0d0
   do kk=1,Ndx
      do k=kbot(kk),ktop(kk)
         sumsq = sumsq + sq(k)
      end do
   end do

!   write(6,*) sumsq

   return
end subroutine comp_sq
