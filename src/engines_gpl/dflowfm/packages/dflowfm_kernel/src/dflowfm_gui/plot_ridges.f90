!> plot the ridges
subroutine plot_ridges(ierror)

   use m_samples
   use m_samples_refine
   use m_missing
   use geometry_module, only: dbdistance

   implicit none

   integer, intent(out) :: ierror   !< error (1) or not (0)

   integer :: i, j, ip

   double precision :: Dx, Dy, dum, Dh, x0, y0, x1, y1, x2, y2

   double precision, external :: comp_sampleDh

   ierror = 1

   if ( iHesstat.ne.iHesstat_OK ) goto 1234

!  plot ridge
   do i=1,MXSAM
      do j=1,MYSAM
!        compute sample mesh width
         Dh = comp_sampleDh(i,j)

         ip = i+(j-1)*MXSAM

         if ( abs(zss(5,i,j)).gt.0.5d0*Dh .or. zss(4,i,j).gt.-1d-8 .or. zss(5,i,j).eq.DMISS ) cycle

         Dx =  zss(3,i,j)
         Dy = -zss(2,i,j)
         dum = Dh/sqrt(Dx**2+Dy**2+1d-16)
         Dx = Dx*dum
         Dy = Dy*dum

         call setcol(204)

         x0 = xs(ip)+zss(2,i,j)*zss(5,i,j)
         y0 = ys(ip)+zss(3,i,j)*zss(5,i,j)
         x1 = min(max(x0-Dx,xs(ip)-0.5d0*Dh), xs(ip)+0.5*Dh)
         y1 = min(max(y0-Dy,ys(ip)-0.5d0*Dh), ys(ip)+0.5*Dh)
         x2 = min(max(x0+Dx,xs(ip)-0.5d0*Dh), xs(ip)+0.5*Dh)
         y2 = min(max(y0+Dy,ys(ip)-0.5d0*Dh), ys(ip)+0.5*Dh)

         call movabs(x1,y1)
         call lnabs(x2,y2)
      end do
   end do

!   call qnerror(' ', ' ', ' ')

   ierror = 0
1234 continue

   return
end subroutine plot_ridges
