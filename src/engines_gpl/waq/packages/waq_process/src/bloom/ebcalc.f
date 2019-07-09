!!  Copyright (C)  Stichting Deltares, 2012-2019.
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

!
!  *********************************************************************
!  *         SUBROUTINE TO PERFORM SPECIAL INTERPOLATION               *
!  *********************************************************************
!
      subroutine ebcalc(x,f,fpr,numgr)

!      use bloom_data_dim
!      use bloom_data_arran   

      implicit none
      
      include 'blmdim.inc'
      include 'arran.inc'
      
      real(8)    :: x, f, fpr, ex, ei, ei1, alam, c0, c1
      integer    :: i, numgr
!
!  CHECK WHETHER X IS TOO LOW OR TOO HIGH
!
      if (x .gt. zvec(1)) go to 20
      f=fun(1,numgr)
      fpr=0.0
      return
   20 if (x .lt. zvec(nz)) go to 40
      f=fun(nz,numgr)
      fpr=0.0
      return
   40 do 60 i=2,nz
      if (x .le. zvec(i)) go to 80
   60 continue
   80 ex=dexp(-x)
      ei=dexp(-zvec(i))
      ei1=dexp(-zvec(i-1))
      alam=(ex-ei)/(ei1-ei)
      fpr=alam*der(i-1,numgr)+(1.0-alam)*der(i,numgr)
      c0=((ex/ei1)-1.0+x-zvec(i-1))/((ex/ei1)-1.0)
      c1=((ei1/ex)-1.0+zvec(i-1)-x)/((ei1/ex)-1.0)
      f=fun(i-1,numgr)-c0*fpr+c1*der(i-1,numgr)
      return
      end
