!>  network field rotate
!!     It is assumed that there is a backup copy of the grid.
subroutine netrotfld(xp,yp,zp,kp)
   use m_netw
   use m_grid
   use m_alloc
   use m_missing
   use m_wearelt
   use m_sferic
   use unstruc_colors, only: ncolhl
   use geometry_module, only: dbdistance

   implicit none

   double precision           :: xp, yp, zp     !< coordinates that determine the influenced region and rotation angle

   integer                    :: kp             !< center point index

   double precision           :: Dx0, Dy0, rsx, xn, yn, dist, frac
   double precision           :: Dalpha0, alpha, xcen, ycen
   double precision, external :: getDx, getDy

   integer                    :: i, ja, jac

   xcen = xk(kp)
   ycen = yk(kp)

   Dx0 = xp - xcen
   Dy0 = yp - ycen

   Dalpha0 = atan2(Dy0, Dx0)

   do
      rsx = max(dsix, sqrt(Dx0*Dx0 + Dy0*Dy0))

   !  whipe out previous net image
      ja = 0
      call teknet(0,ja)

      do i=1,numk
         xn     = xk(i)
         yn     = yk(i)
   !     intentional not in sferical coordinates
         dist  = sqrt( (xn-xcen)**2 + (yn-ycen)**2 )
         frac  = 0.5 * (1+ cos(min(max(dist/rsx,-1d0),1d0) * pi))

         alpha = Dalpha0*frac

         xk(i) = xcen + (xn-xcen)*cos(alpha) - (yn-ycen)*sin(alpha)
         yk(i) = ycen + (xn-xcen)*sin(alpha) + (yn-ycen)*cos(alpha)
      end do

      call teknet(ncolhl,ja)

      jac = 1
      call confrm('More? ', jac)
      if ( jac.eq.0 ) then
         call confrm('Flip rotation?', jac)
         if ( jac.eq.1 ) then
            Dalpha0 = -Dalpha0
         else
            exit
         end if
      end if
   end do

end subroutine netrotfld
