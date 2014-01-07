!!  Copyright(C) Stichting Deltares, 2012-2014.
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

!!  Note: The "part" engine is not yet Open Source, but still under
!!  development. This package serves as a temporary dummy interface for
!!  the references in the "waq" engine to the "part" engine.

      module typos

      use precision

      type pnt

         integer         in1
         integer         n1
         integer         m1
         integer         f1
         integer         i1

         integer         in2
         integer         n2
         integer         m2
         integer         f2
         integer         i2
      end type pnt
      type domain
         character(256)  name
         integer         nmax
         integer         mmax
         integer         moff
      end type domain
      type range
         character(256)  name
         integer         did
         integer         fn
         integer         fm
         integer         tn
         integer         tm
      end type range
      type boundp
         type ( range )  r1
         type ( range )  r2
      end type boundp

      type PlotGrid
         integer(ip) :: ztype

         integer(ip) :: mmap
         integer(ip) :: nmap
         real   (rp) :: xlow
         real   (rp) :: xhigh
         real   (rp) :: ylow
         real   (rp) :: yhigh
         real   (rp) :: zlow
         real   (rp) :: zhigh
         real   (rp) :: surf
         integer(ip), pointer :: nmcell(    :,:)
         real   (rp), pointer :: amap  (:,:,:,:)
         real   (rp), pointer :: atrack(  :,:,:)
         integer(ip), pointer :: nbin  (  :,:,:)
         integer(ip), pointer :: imask (    :,:)
      end type PlotGrid

      end module typos
