!>    move probe:
!>      7 8 9
!>      4 5 6
!>      1 2 3
      subroutine moveprobe(idir, kk, xp, yp)
         use m_flowgeom
         use network_data, only: xzw, yzw
         implicit none

         integer,          intent(in)    :: idir   !< direction (see keys on keypad)
         integer,          intent(inout) :: kk     !< probed flownode number
         double precision, intent(inout) :: xp, yp !< probed flownode coordinates

         double precision                :: csdir, sndir !< direction vector components
         double precision                :: dum
         double precision                :: dmaxinprod
         double precision                :: cs, sn

         integer                         :: i, j, jj, k, k2, L, knext

         if ( kk.eq.0 .or. idir.eq.5 ) then
            call in_flowcell(xp, yp, KK)
         else
!           determine direction vector
            csdir = mod(idir-1,3) - 1d0
            sndir = int((idir-1)/3) - 1d0
            dum = sqrt(csdir**2+sndir**2)
            csdir = csdir / dum
            sndir = sndir / dum

!!           find next flownode
!            knext = 0
!            dmaxinprod = -huge(0d0)
!
!            do i=1,size(nd(kk)%nod)
!               k = nd(kk)%nod(i)
!               do j=1,cn(k)%lnx
!                  L = iabs(cn(k)%ln(j))
!                  do jj=1,2
!                     k2 = ln(jj,L)
!                     if ( k2.eq.kk ) cycle
!
!                     call getdxdy(xzw(kk),yzw(kk),xzw(k2),yzw(k2),cs,sn)
!                     dum = sqrt(cs**2+sn**2)
!                     cs = cs/dum
!                     sn = sn/dum
!
!                     dum = csdir*cs + sndir*sn
!
!                     if ( dum.gt.0d0 .and. dum.gt.dmaxinprod) then
!                        knext = k2
!                        dmaxinprod = dum
!                     end if
!
!                  end do
!               end do
!            end do

!           find next flownode
            knext = 0
            dmaxinprod = -huge(0d0)
            do i=1,nd(kk)%lnx
               L = nd(kk)%ln(i)
               if ( L.lt.0 ) then
                  dum = csdir*csu(-L) + sndir*snu(-L)
               else
                  dum = -(csdir*csu(L) + sndir*snu(L))
               end if
               if ( dum.gt.0d0 .and. dum.gt.dmaxinprod) then
                  knext = ln(1,iabs(L))+ln(2,iabs(L))-kk
                  dmaxinprod = dum
               end if
            end do

            if ( knext.ne.0 ) then
               kk = knext
               xp = xzw(kk)
               yp = yzw(kk)
            end if
         end if

         return
      end subroutine moveprobe
