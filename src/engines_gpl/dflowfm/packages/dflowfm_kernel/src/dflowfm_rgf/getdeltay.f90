   subroutine getdeltay(y, dx0, dy0) ! find dy=dx*cos(y0+0.5*dy) newton iteration
   use m_sferic
   double precision ::  y, dx0, dy0, f, df, yd, c, s, phi
   integer :: k
   dy0 = dx0*cos(dg2rd*y)
   do k   = 1,5
      phi = dg2rd*(y+0.5*dy0) ; c = cos(phi) ; s = sqrt(1d0-c*c)
      f   = dy0 -             dx0*c
      df  = 1d0 + 0.5d0*dg2rd*dx0*s
      yd  = f/df
      dy0 = dy0 - yd
      if (yd < 1d-14) return
   enddo
   end
