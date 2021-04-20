!>  regularise spline2curvi grid
!>     note: there is an asymmetry, but this procedure is intended for regularisation only
subroutine regularise_spline2curvigrid()
   use m_grid
   use m_spline2curvi, only: dtolLR
   use m_missing
   use geometry_module, only: dbdistance
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   double precision                            :: xi
   double precision                            :: dhmax, dtolLR_bak

   integer                                     :: i, j, iL, iR, iter
   integer                                     :: ih

   integer                                     :: ierror

   double precision, parameter                 :: FAC = 1d-1   ! regularisation parameter

   call savegrd()

   ierror = 1

!  store settings
   dtolLR_bak = dtolLR

!  compute maximum mesh width and get dtolLR in the proper dimension
   dhmax = 0d0
   do i=1,mc
      do j=1,nc-1
         if ( xc(i,j).eq.DMISS .or. xc(i,j+1).eq.DMISS ) cycle
         dhmax = max(dhmax, dbdistance(xc(i,j),yc(i,j),xc(i,j),yc(i,j+1), jsferic, jasfer3D, dmiss))
      end do
   end do
   dtolLR = dtolLR*dhmax

   do j=1,nc
      i = 1
      do while ( i.le.mc )
         if ( xc(i,j).ne.DMISS .and. yc(i,j).ne.DMISS ) then
!           get neighboring nodes
            call get_LR(mc, xc(:,j), yc(:,j), i, iL, iR)

!           regularise grid on right hand side of this node (asymmetric)
            do ih = i+1, iR-1
               xi = dble(ih-i)/dble(iR-i) * FAC
               xc(ih,j) = (1d0-xi)*xc(i,j) + xi*xc(iR,j)
               yc(ih,j) = (1d0-xi)*yc(i,j) + xi*yc(iR,j)
            end do
         else  ! just advance pointer
            iR = i+1
         end if

         i = max(iR, i+1)
      end do
   end do

   ierror = 0
1234 continue

!  restore settings
   dtolLR = dtolLR_bak

   return
end subroutine regularise_spline2curvigrid
