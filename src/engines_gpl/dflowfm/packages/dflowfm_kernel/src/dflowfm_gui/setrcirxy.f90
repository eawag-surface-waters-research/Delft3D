      subroutine setrcirxy(x,y,rcx,rcy) ! determine x and y search tols on the spot where you click
      use m_wearelt
      use m_sferic
      use m_devices
      use m_sferzoom
      implicit none
      double precision :: x,y,rcx,rcy,xx,yy,xa,ya,rpx,rpy
      real :: xloc, yloc
      integer          :: nx,ny
      rcx = rcir ; rcy = rcir
      if (jsfertek .ge. 1) then
         call dPROJECT(x,y,xa,ya,2)

         rpy = 28*(y2-y1)/npy
         call dPROJECT(xa,ya+rpy,xx,yy,1)  ! you still have to project in
         call dbdistancehk(x,y,xx,yy,rcy)
         rcy = rcy*rd2dg/ra

         rpx = 28*(x2-x1)/npx
         call dPROJECT(xa+rpx,ya,xx,yy,1)
         call dbdistancehk(x,y,xx,yy,rcx)
         rcx = rcx*rd2dg/ra

         rcx = sqrt(rcx*rcx + rcy*rcy)
         rcy = rcx

       endif
      end
