!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2021.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! $Id$
! $HeadURL$

      subroutine weirexact()
      use unstruc_colors
      implicit none
      integer, parameter :: mx = 32
      double precision   :: dp(mx), s1(mx), s0(mx), u1(mx), u0(mx), q1(mx), q0(mx), se(mx), ue(mx), xe(mx), wh
      integer :: m, m0, m1
      double precision :: g, hexact, uexact, zexact, energy, z0, z1, z2, h0, h1, h2, &
                          fh0, fh1, fh2, fhx, zx, sem, uem, test0, he2, he1, test1, h00

      integer :: mmax

      do m = 1,mx
         xe(m) = 5 + (m-1)*10
      enddo

      mmax = mx-1
      m0   = 14
      m1   = 14
      wh   = 1.0
      dp   = wh ; dp(m0:m1) = 0.0
      q0   = 1.71
      s1(mx) = 0.0


      g=9.81
      hexact=(q0(1)**2/g)**(1.0/3.0)
      uexact=sqrt(g*hexact)
      zexact=hexact-minval(dp)


      energy=0.5*uexact**2+g*zexact

      ! energy=0.5*u1(1)*u1(1) + g*s1(1)

      z0=zexact
      z1=zexact+3.0
      h00=dp(1)+z0
      h1=dp(1)+z1
      fh0=0.5*(q0(1)/h00)**2+g*z0-energy
      fh1=0.5*(q0(1)/h1)**2+g*z1-energy
      z0=0.5
      z1=zexact+1.0
      z2=0.5*(z0+z1)
      do m=1,mmax
        if (m>=m0.and.m<=m1) then
          se(m)=zexact
          ue(m)=uexact
        else
          if (m>=m1) then
            z0=-dp(m)-0.1
            z1=z1+0.0001
          else
            z0=zexact
            z1=zexact+0.0001
          endif
        do
          h1=dp(m)+z1
          h0=dp(m)+z0
          fh1=0.5*(q0(1)/h1)**2+g*z1-energy
          fh0=0.5*(q0(1)/h0)**2+g*z0-energy
          if (fh1*fh0<0.0) then
            if (fh0>0.0) then
              fhx=fh0
              zx=z0
              fh0=fh1
              z0=z1
              z1=zx
              fh1=fhx
            endif
            exit
          endif
          z1=z1+0.0001
          z0=z0+0.0001
        enddo
10      z2=0.5*(z0+z1)
        h2=dp(m)+z2
        fh2=0.5*(q0(1)/h2)**2+g*z2-energy
        if (fh2<0.0) then
          z0=z2
          fh0=fh2
        else
          z1=z2
          fh1=fh2
        endif
        if (abs(fh2)>1.0d-6) goto 10
        se(m)=z2
        if (dp(m)+z2<0.0) then
          write(*,*) m,dp(m)+z2
          stop
        endif
        ue(m)=q1(m)/(dp(m)+se(m))
      endif
      enddo
      se(m1+1)=0.5*(se(m1)+se(m1+2))
      sem=s1(mmax+1)
      uem=q1(1)/(sem+dp(mmax+1))
      ue(mmax+1)=uem
      se(mmax+1)=sem
      h2=sem+dp(mmax+1)
      h1=se(mmax)+dp(mmax)
      energy=0.5*ue(mmax+1)**2+g*sem
      test0=0.5*g*h1*h2*(h1+h2)-q1(1)**2
      m=mmax+1
      do
      m=m-1
      z1=sem+0.1
      z0=z1-0.001
        do
          h1=dp(m)+z1
          h0=dp(m)+z0
          fh1=0.5*(q0(1)/h1)**2+g*z1-energy
          fh0=0.5*(q0(1)/h0)**2+g*z0-energy
          if (fh1*fh0<0.0) then
            if (fh0>0.0) then
              fhx=fh0
              zx=z0
              fh0=fh1
              z0=z1
              z1=zx
              fh1=fhx
            endif
            exit
          endif
          z1=z1-0.001
          z0=z0-0.001
        enddo
        do
          z2=0.5*(z0+z1)
          h2=dp(m)+z2
          fh2=0.5*(q0(1)/h2)**2+g*z2-energy
          if (fh2<0.0) then

            z0=z2
            fh0=fh2
          else
            z1=z2
            fh1=fh2
          endif
          if (abs(fh2)<1.0d-5) exit
        enddo
        sem=z2
        uem=q1(1)/(dp(m)+se(m))
        he2=dp(m)+sem
        he1=dp(m)+se(m)
        test1=g*0.5*he1*he2*(he1+he2)-q1(1)**2
        if (test1*test0>0.and.m>m1) then
          se(m)=sem
          ue(m)=uem
          cycle
        else
          exit
        endif
      enddo

      call setcol(NCOLANA)
      call movabs(xe(1), se(1)+wh)
      do m=2,mx
         call lnabs(xe(m), se(m) + wh)
      enddo
      return
      end subroutine weirexact
