!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
      module m_bleffpro
      
      implicit none

      contains


!
! **********************************************************************
! * Begin subroutine to calculate solar intensity and efficiency       *
! * matrix for the phytoplankton model bloom ii                        *
! * the program has been updated most recently in december 2015        *
! * to be part of delwaq/bloom as a subroutine                         *
! * the user may specify the photosynthetic curves tablelized          *
! **********************************************************************
!
      subroutine bleffpro(lunrep, lunblm, nuecog, npoint, power, effic, nz, zvec, fun, der) 
      
      implicit none 
      dimension power(51),solvec(51),effic(51,30),fun(51,30),der(51,30),
     1          sfirst(51),solar(51),time(51),cdf(51),freq(51),
     2          dens(51),tsol(51),tden(51),domf(51),rfirst(51,30),
     3          gfun(51),gder(51),zvec(51),daymul(24,30),dl(24)
      common/solrad/tsol,tden,freq,nval
      common/solval/delsol,solmax,day
      character*5 table
      character*265 inputfile
      character*265 outputfile
      integer lunblm, lunrep
      integer irc, npoint, i, j, k, nsp, nval, nz
      real*8 power, effic, solvec, time, solar, cdf, dens, freq, tsol, tden, domf
      real*8 rfirst, zvec, done, dneg, sfirst, gfun, gder, fun, der
      real*8 daymul, dl, delsol, solmax, day
!
      integer nuecog ! Number of groups
!
!  Read number of light intensity points
!  Read efficiency data for each ecogroup
!
      read (lunblm,*) npoint
!
!  Read in tables
!
   15 continue
      do 20 i=1,npoint
   20 read (lunblm,*) power(i),(effic(i,j),j=1,nuecog)
!
!  Read, integrate, and transform diurnal intensity distribution
!  Read number of points in solar radiation distribution
!
      read (lunblm,*) nsp
!
!  Read: solvec--solar radiation level; time--time of day
!
      read (lunblm,*) (solvec(k),time(k),k=1,nsp)
!
!  nval is the number of points desired in the transformed solar
!   radiation distribution function (max 50)
!
      nval=50
      call solcdf(solvec,time,nsp,nval,solar,cdf)
      call convrt(solar,cdf,nval,dens)
      call indist(nval,solar,dens,freq,tsol,tden)
!
!  Transform efficiency data
!
      call inteff(npoint,nuecog,domf,rfirst,effic,power)
!
!  Determine appropriate tabulation points for convolution
!  nz is the number of points desired in the convolution (max 51)
!  Write convolution points into data set
!
      nz=51
      call zval(domf,npoint,tsol,nval,zvec,nz)
!
!  Compute convolutions and their derivatives
!  Calculate constants and determine roots for each order
!
      done = 1.0d0
      dneg = -1.0d0
      do 80 j=1,nuecog
      do 60 k=1,npoint
   60 sfirst(k)=rfirst(k,j)
      call cvolve(tsol,freq,nval,done,domf,sfirst,npoint,dneg,zvec,
     1            gfun,nz)
      call cvolve(domf,sfirst,npoint,dneg,tsol,tden,nval,done,zvec,
     1            gder,nz)
      do 70 k=1,nz
      fun(k,j)=gfun(k)
   70 der(k,j)=gder(k)
   80 continue
      return
      end subroutine
!
!  Subroutine to adjust lower end of efficiency data and transform it
!  to a logarithmic form
!
      subroutine inteff(npoint,nuecog,domf,rfirst,effic,power)
      implicit none
      dimension effic(51,30),power(51),domf(51),rfirst(51,30),e(30)
      integer npoint, nuecog, i, j, i1
      real*8 power, p, effic, e, rfirst, domf
!
!  Move lower end of curve away from zero.
!
    1 if (power(1) .gt. 0.001*power(2)) go to 15
      p=0.001*power(2)
      call interm(power,effic,npoint,nuecog,p,e)
      do 12 j=1,nuecog
   12 effic(1,j)=e(j)
      power(1)=p
!
!  Transform intensity into its logarithmic form
!
   15 do 20 i=1,npoint
      i1=npoint-i+1
      do 18 j=1,nuecog
   18 rfirst(i,j)=effic(i1,j)
   20 domf(i)=-dlog(power(i1))
      return
      end
!
!  Subroutine to find cumulative distribution function
!
      subroutine solcdf(solvec,time,nsp,nval,solar,cdf)
      implicit none
      dimension solvec(51),time(51),cdf(51),solar(51),solna(51),
     1          solnb(51),timea(51),timeb(51),value(2)
      common/solval/delsol,solmax,day
      integer i, nsp, it, na, nb, j, jk, jkn, n, nval
      real*8 solmax, solvec, rn, delsol, day, time, solna, timea, solnb
      real*8 timeb, solar, cdf, rj, sol, value, dens, ri
!
!  Find maximum intensity
!
    1 solmax=solvec(1)
      do 5 i=1,nsp
    5 if (solvec(i) .gt. solmax) solmax=solvec(i)
!
!  SET CONSTANTS
!
      rn=nval-1
      delsol=solmax/rn
      day=time(nsp)-time(1)
!
!   SPLIT SOLAR RADIATION VECTOR INTO ASCENDING AND DESCENDING PARTS
!
      it=0
      i=1
   12 if (solvec(i) .eq. solmax) it=1
      solna(i)=solvec(i)
      timea(i)=time(i)
      if (it .eq. 1) go to 15
      i=i+1
      go to 12
   15 na=i
      nb=nsp-na+1
      do 18 j=1,nb
      jk=j-1
      jkn=nsp-jk
      solnb(j)=solvec(jkn)
   18 timeb(j)=time(jkn)
!
!  Calculate cdf for radiation function
!  Set endpoints of arrays
!
      solar(1)=0.0
      cdf(1)=0.0
!
!  Begin calculation of cdf
!
      do 20 j=2,nval
      rj=j-1
      sol=delsol*rj
      call interp(solna,timea,na,sol,value(1))
      call interp(solnb,timeb,nb,sol,value(2))
      cdf(j)=(value(1)-time(1)+time(nsp)-value(2))/day
   20 solar(j)=sol
      return
      end
!
!  Subroutine to convert cdf to probability density function
!
      subroutine convrt(solar,cdf,nval,dens)
      implicit none
      dimension solar(51),cdf(51),dens(51),value(2)
      common/solval/delsol,solmax,day
      integer i, n, nval, imin
      real*8 rj, sol, value, dens, ri, solc, delsol, soll, solu
      real*8 solar, cdf, solmax, day
!
!  Set constants and endpoints
!
    1 dens(1)=0.0
!
!  Iterate through intensity function
!  Determine density values
!
      n=nval-1
      do 10 i=2,n
      ri=i-1
      solc=delsol*ri
      if (i .eq. 2) go to 6
      value(1)=value(2)
      soll=solu
    4 solu=solc+0.5*delsol
      call interp(solar,cdf,nval,solu,value(2))
      go to 8
    6 soll=0.5*delsol
      call interp(solar,cdf,nval,soll,value(1))
      go to 4
    8 dens(i)=(value(2)-value(1))/delsol
   10 continue
      dens(nval)=(1.0-value(2))*2.0/delsol
      return
      end
!
!  Subroutine to integrate and normalize diurnal intensity
!  distribution, and compute mean intensity
!
      subroutine indist(nval,solar,dens,freq,tsol,tden)
      implicit none
      dimension solar(51),freq(51),dens(51),tsol(51),tden(51)
      integer i, nval
      real*8 solar, s, dens, d, freq, sbar, del, amult, tsol, tden
!
!  Move lower end of curve away from zero
!
      if (solar(1) .gt. 0.001*solar(2)) go to 15
      s=0.001*solar(2)
      call interp(solar,dens,nval,s,d)
      dens(1)=d
      solar(1)=s
!
!  Integrate to get cumulative distribution
!
   15 freq(1)=0.0
      sbar=0.0
      do 20 i=2,nval
      del=0.5*(solar(i)-solar(i-1))*(dens(i-1)+dens(i))
      freq(i)=freq(i-1)+del
   20 sbar=sbar+solar(i)*del
      amult=1.0/freq(nval)
      sbar=sbar*amult
!
!  Normalize distribution
!
      do 30 i=1,nval
      dens(i)=dens(i)*amult
      freq(i)=freq(i)*amult
      tsol(i)=dlog(solar(i)/sbar)
   30 tden(i)=solar(i)*dens(i)
      return
      end
!
!  Subroutine to determine z-values for convolutions
!
      subroutine zval(xvec,nx,yvec,ny,zvec,nz)
      implicit none
      dimension xvec(51),yvec(51),zvec(51),ix(51)
      integer i, nx, ix, nz1, nz, ny, n1, n2, j, k, imin, ixk
      real*8 rat, crat, xvec, yvec, zvec, smin, del 
      
      do 10 i=1,nx
   10 ix(i)=1
      nz1=nz-1
      rat=(nx*ny-1)/nz1
      crat=0.0
      zvec(1)=xvec(1)+yvec(1)
      ix(1)=2
      zvec(nz)=xvec(nx)+yvec(ny)
      n2=0
!
!  Loop through desired number of z-values
!
      do 20 i=2,nz1
      n1=n2+1
      crat=crat+rat
      n2=crat+0.5
!
!  Loop through next "rat" potential z-values in ascending order
!
      do 15 j=n1,n2
      smin=zvec(nz)+1.0
      imin=0
      do 12 k=1,nx
      if (ix(k) .gt. ny) go to 12
      ixk=ix(k)
      if (xvec(k)+yvec(ixk) .ge. smin) go to 12
      smin=xvec(k)+yvec(ixk)
      imin=k
   12 continue
   15 ix(imin)=ix(imin)+1
!
!  Fill in next actual z-value
!
   20 zvec(i)=smin
!
!  Adjust for duplicates
!
      do 30 i=2,nz
      if (zvec(i) .gt. zvec(i-1)) go to 30
      do 25 j=i,nz
      if (zvec(j) .gt. zvec(i-1)) go to 26
   25 continue
   26 del =(zvec(j)-zvec(i-1))/(j-i)
      do 27 k=i,j
   27 zvec(k)=zvec(k-1)+del
   30 continue
      return
      end
!
!  Subroutine to convolve the functions f(x) and g(y)
!
      subroutine cvolve(xvec,fofx,nx,ax,yvec,gofy,ny,ay,zvec,
     1                  fstarg,nz)
      implicit none
      dimension xvec(51),fofx(51),yvec(51),gofy(51),zvec(51),
     1          fstarg(51)
      integer i, j, nx, ny, nz, ix, iy
      real*8 xvec, yvec, zvec, fofx, fstarg, bot, top
      real*8 ex1, ax, ex2, ey1, ay, ey2, f2, f1, g2, gofy, g1, d, s
      real*8 tmp, x
!
!  Add a convenience point to f(x).
!
      xvec(nx+1)=xvec(nx)+1.0
      fofx(nx+1)=fofx(nx)
      do 50 i=1,nz
      fstarg(i)=0.0
      iy=2
      bot=yvec(1)
      ix=nx
      do 1 j=1,nx
      if (zvec(i)-xvec(ix) .gt. yvec(1)) go to 20
    1 ix=ix-1
!
!  G-domain lies entirely to the right of inverted f-domain
!  integral must be zero
!
      go to 50
!
!  Integrate over the overlapping parts of f- and g-domains
!
   20 top=dmin1(yvec(iy),zvec(i)-xvec(ix))
      ex1=dexp(ax*xvec(ix))
      ex2=dexp(ax*xvec(ix+1))
      ey1=dexp(ay*yvec(iy))
      ey2=dexp(ay*yvec(iy-1))
      f2=(fofx(ix+1)-fofx(ix))/(ex2-ex1)
      f1=fofx(ix)-f2*ex1
      g2=(gofy(iy-1)-gofy(iy))/(ey2-ey1)
      g1=gofy(iy)-g2*ey1
      d=ay-ax
      s=f1*g1*(top-bot)+f1*g2*(dexp(ay*top)-dexp(ay*bot))/ay
     2 -f2*g1*(dexp(ax*(zvec(i)-top))-dexp(ax*(zvec(i)-bot)))/ax
     3 +f2*g2*dexp(ax*zvec(i))*(dexp(d*top)-dexp(d*bot))/d
      fstarg(i)=fstarg(i)+s
!
!  Update intervals and stop if x(1) or y(ny) has been reached
!
      if (top .ge. yvec(iy)) iy=iy+1
!
      tmp = zvec(i) - xvec(ix) - 1.0d-60
      if (top .ge. tmp) ix=ix-1
      if (iy .gt. ny) go to 50
      if (ix .lt. 1) go to 50
      bot=top
      go to 20
   50 continue
      return
      end
!
!  Subroutine to perform linear interpolations
!
      subroutine interp(xvec,fofx,n,x,f)
!
!  Performs a single linear interpolation
!
      implicit none
      dimension xvec(51),fofx(51),fofxm(51,30),fm(30)
      integer i, n
      real*8 x, xvec, f, fofx, fofxm, alam, fm
!
!  Check whether x is too low or too high
!
      if (x .gt. xvec(1)) go to 20
      f=fofx(1)
      return
   20 if (x .lt. xvec(n)) go to 40
      f=fofx(n)
      return
   40 do 60 i=2,n
      if (x .le. xvec(i)) go to 80
   60 continue
   80 alam=(x-xvec(i-1))/(xvec(i)-xvec(i-1))
      f=alam*fofx(i)+(1.0-alam)*fofx(i-1)
      return
      end
!
!  Entry for multiple value functions
!
      subroutine interm(xvec,fofxm,n,nuecog,x,fm)
      implicit none
      dimension xvec(51),fofx(51),fofxm(51,30),fm(30)
      integer i, j, n, nuecog
      real*8 x, xvec, fm, fofxm, alam, fofx
!
!  Check whether x is too low or too high
!
      if (x .gt. xvec(1)) go to 120
      do 110 j=1,nuecog
  110 fm(j)=fofxm(1,j)
      return
  120 if (x .lt. xvec(n)) go to 140
      do 130 j=1,nuecog
  130 fm(j)=fofxm(n,j)
      return
  140 do 160 i=2,n
      if (x .le. xvec(i)) go to 180
  160 continue
  180 alam=(x-xvec(i-1))/(xvec(i)-xvec(i-1))
      do 190 j=1,nuecog
  190 fm(j)=alam*fofxm(i,j)+(1.0-alam)*fofxm(i-1,j)
      return
      end
      end module m_bleffpro
