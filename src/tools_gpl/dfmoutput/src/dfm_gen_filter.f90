!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2018.
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

!> DFM_GEN_FILTER : apply more general filter (compared to max13/max25)
!!

module dfm_gen_filter
   use netcdf
   use read_nc_histories
   use precision
   implicit none
   private
   public :: gen_filter

   contains

!> main routine to write maximum values based on a generic filter to file
subroutine gen_filter(filename, filename_out, field_name, intval, coefimpl, coefexpl)
   implicit none
   character(len=*) , intent(in) :: filename      !< input filename (NetCDF)
   character(len=*) , intent(in) :: field_name    !< input field name (e.g. 'waterlevel')
   character(len=*) , intent(in) :: filename_out  !< output filename (ascii)
   real, intent(in) :: coefimpl, coefexpl, intval

   integer :: ierr, i, nStations, ntimes, iunout
   real, allocatable :: hisdata(:,:), ySmooth(:), y(:)
   character(len=64), allocatable :: stations(:)
   integer, allocatable :: stats_index(:), list(:)
   logical, parameter :: debug_test = .false.

                           ierr = read_meta_data(filename, nStations)
   if (ierr == nf90_noerr) ierr = read_station_names(stations, 'station_name')
   if (ierr == nf90_noerr) ierr = read_data(hisdata, field_name)
   if (ierr == nf90_noerr) ierr = close_nc_his_file()

   ! small test
   if (debug_test) then
      ntimes = 20
      allocate(y(ntimes), ysmooth(ntimes))
      do i = 1, ntimes
         y(i) = real(i, hp) + 0.9_hp * sin(3.0_hp * real(i, hp))
      enddo
      call fourthOrderMono(y, intval, coefimpl, coefexpl, ySmooth)
      deallocate(y, ysmooth)
   endif

   if (ierr == nf90_noerr) then
      open (newunit=iunout, file=filename_out)
      ntimes = size(hisdata,1)
      allocate(ySmooth(nTimes))
      do i = 1, nStations
         call fourthOrderMono(hisdata(:,i), intval, coefimpl, coefexpl, ySmooth)
        !write(iunout,'(f8.3,x,a)') maxval(ySmooth), trim(stations(i))
        !write(iunout,*) maxval(ySmooth), ' ', trim(stations(i))
         write(iunout,*) stations(i), maxval(ySmooth)
         write(*,*) stations(i), maxval(ySmooth)
      enddo
      close(iunout)
   endif
end subroutine gen_filter

subroutine fourthOrderMono(y, intval, coefimpl, coefexpl, ySmooth)
   real, intent(in) :: y(:), coefimpl, coefexpl, intval
   real, intent(out) :: ySmooth(:)

   integer :: ndim,i,N,ii,NN
   real, parameter :: pi = 4.0 * atan(1.0)
   real :: ToDt, c4expl, c4impl, ca4expl, ca4impl, ddd
   real, allocatable :: v(:), M(:,:), termN(:), ySmooth2(:)
   integer, allocatable :: indx(:), interval(:)

   ndim = size(y)
   allocate(v(ndim), indx(ndim))
   allocate(M(ndim,ndim))
   M = 0.0

   N = nint(1.5 * intval)
   ToDt=intval/(2.0*pi)
   ToDt=ToDt*sqrt(sqrt(2.0)-1.0)

   c4expl=2.0*coefexpl
   c4impl=2.0*(coefimpl-ToDt**2)
   ca4expl=coefexpl**2
   ca4impl=coefimpl*(coefimpl - 2.0*ToDt**2)

   V(1)=1.0-2.0*c4expl+6.0*ca4expl
   V(2)=c4expl-4.0*ca4expl
   V(3)=ca4expl

   ! symmetry about M41,1 -->M41,0 added to M41,2 and M41,-1 added to M41,3
   M(1,1) = 1.0-2.0*c4impl + 6.0*(ca4impl+ToDt**4)
   M(1,2) = 2.0*c4impl -8.0*(ca4impl+ToDt**4)
   M(1,3) = 2.0*(ca4impl + ToDt**4)

   ! symmetry about M42,1 -->M42,0 added to M4,2
   M(2,1) = c4impl-4.0*(ca4impl+ToDt**4)
   M(2,2) = 1.0-2.0*c4impl+7.0*(ca4impl+ToDt**4)
   M(2,3) = c4impl-4.0*(ca4impl+ToDt**4)
   M(2,4) = ca4impl + ToDt**4

   do i=3, ndim-2
        ! inner discretization with two 2nd-order diffusive fluxes and two 4th order diffusive fluxes
        M(i,i-2)=ca4impl+ToDt**4
        M(i,i-1)=c4impl-4.0*(ca4impl+ToDt**4)
        M(i,i)=1.0-2.0*c4impl+6.0*(ca4impl+ToDt**4)
        M(i,i+1)=c4impl-4*(ca4impl+ToDt**4)
        M(i,i+2)=ca4impl+ToDt**4
   end do

   ! boundary procedure: apply only one 4th order diffusive flux, since both
   ! cannot be applied anymore (the two 2nd order diffusive fluxes that both
   ! can still be applied)
   M(ndim-1,ndim-3) = ca4impl+ToDt**4
   M(ndim-1,ndim-2) = c4impl-3.0*(ca4impl+ToDt**4)
   M(ndim-1,ndim-1) = 1.0-2.0*c4impl+3.0*(ca4impl+ToDt**4)
   M(ndim-1,ndim)   = c4impl-(ca4impl+ToDt**4)

   ! boundary procedure: apply no 4th order diffusive flux and only one 2nd
   ! order diffusive flux, since both cannot be applied anymore
   M(ndim,ndim-1) = c4impl
   M(ndim,ndim)   = 1.0-c4impl

   call ludcmp(M,ndim,ndim,indx,ddd)
   call lubksb(M,ndim,ndim,indx,V)

   ! divide weight for central element by 2, to allow double application below
   V(1) = 0.5_hp * V(1)

   ii = 0
   nn = ndim-N+1
   allocate(interval(nn+1-N))  ! a little bit too big
   do i = N, nn
      ii = ii + 1
      interval(ii) = i
   enddo

   allocate(termN(nn+1-N), ySmooth2(nn+1-N))
   do i = 0, N-1
      do ii = N, nn
         termN(ii+1-N) = V(i+1) * (y(interval(ii+1-N)+i) + y(interval(ii+1-N)-i))
      enddo
      if (i == 0) then
         ySmooth2 = termN
      else
         ySmooth2 = ySmooth2 + termN
      endif
   enddo

   ySmooth = y  ! todo: niet nodig
   do i = 1, ndim
      ii = i-(N-1)
      if (i < N .or. ii > size(interval)) then
          ySmooth(i) = 0.0;
      else
          ySmooth(i) = ySmooth2(ii);
      end if
   enddo

end subroutine fourthOrderMono

!! from http://www.me.udel.edu/meeg342/LU.f
   subroutine ludcmp(a, n, np, indx, d)
   integer, intent(in) :: n, np
   integer, intent(out) :: indx(n)
   real, intent(inout) :: a(np, np)
   real, intent(out) :: d

   real, parameter :: Tiny=1.0e-20
   integer :: i,imax,j,k
   real :: aamax, dum, sum, vv(np)

   d=1.0
   do i=1,n
      aamax=0.0
      do j=1,n
         if(abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
      enddo
      if(aamax == 0) then
         write(*,*) 'singular matrix in ludcmp'
         return
      endif
      vv(i)=1./aamax
   enddo

   do j=1,n
      do i=1,j-1
         sum=a(i,j)
         do k=1,i-1
            sum=sum-a(i,k)*a(k,j)
         enddo
         a(i,j)=sum
      enddo
      aamax=0.0
      do i=j,n
         sum=a(i,j)
         do k=1,j-1
            sum=sum-a(i,k)*a(k,j)
         enddo
         a(i,j)=sum
         dum=vv(i)*abs(sum)
         if(dum >= aamax)then
            imax=i
            aamax=dum
         endif
      enddo
      if(j /= imax)then
         do k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
         enddo
         d=-d
         vv(imax)=vv(j)
      endif
      indx(j)=imax
      if(a(j,j) == 0.0) a(j,j)=tiny
      if(j /= n)then
         dum=1.0/a(j,j)
         do i=j+1,n
            a(i,j)=a(i,j)*dum
         enddo
      endif
   enddo

   end subroutine ludcmp

   subroutine lubksb(a, n, np, indx, b)
   integer, intent(in) :: n, np, indx(n)
   real, intent(in) :: a(np, np)
   real, intent(inout) :: b(n)

   integer :: i, ii, j, ll
   real :: sum

   ii=0
   do i=1,n
      ll=indx(i)
      sum=b(ll)
      b(ll)=b(i)
      if(ii /= 0)then
         do j=ii,i-1
            sum=sum-a(i,j)*b(j)
         enddo
      else if(sum /= 0.0)then
         ii=i
      endif
      b(i)=sum
   enddo

   do i=n,1,-1
      sum=b(i)
      do j=i+1,n
         sum=sum-a(i,j)*b(j)
      enddo
      b(i)=sum/a(i,i)
   enddo

   end subroutine lubksb

end module dfm_gen_filter
