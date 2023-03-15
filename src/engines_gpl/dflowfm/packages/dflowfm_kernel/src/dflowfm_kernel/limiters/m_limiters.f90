   !----- AGPL --------------------------------------------------------------------
   !
   !  Copyright (C)  Stichting Deltares, 2017-2023.
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

   ! $Id: m_limiters.f90 142548 2023-02-16 11:25:43Z buwalda $
   ! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/trunk/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/limiters/m_limiters.f90 $


   module m_limiters

   implicit none
   contains

   elemental double precision function dminmod(d1,d2)                      ! twee maal vergroot vanwege acl
   implicit none
   double precision, intent(in) :: d1, d2
   if (d1*d2 > 0d0) then
      dminmod =min(1d0,d2/d1)
   else
      dminmod =0d0
   endif
   return
   end function dminmod

   elemental double precision function dvanleer(d1,d2)                     ! twee maal vergroot vanwege acl
   implicit none
   double precision, intent(in) :: d1, d2
   if (d1*d2 > 0d0) then
      dvanleer = 2d0*d2/(d1+d2)
   else
      dvanleer = 0d0
   endif
   return
   end function dvanleer

   elemental double precision function dkoren(d1,d2)                       ! nog naar kijken
   implicit none
   double precision, intent(in) :: d1, d2
   double precision r
   if (d1*d2 > 0d0) Then
      r=d2/d1
      dkoren=max(0d0,min(r+r,min((1d0+r+r)/3d0,2d0)))
   else
      dkoren=0d0
   endif
   return
   end function dkoren

   elemental double precision function dcminmod(d1,d2)                     ! basic minmod definition
   implicit none
   double precision, intent(in) :: d1, d2
   logical :: minabs
   if (d1*d2 > 0) then
      if (abs(d1) < abs(d2)) then
         dcminmod = d1
      else
         dcminmod = d2
      endif
   else
      dcminmod = 0d0
   endif

   end function dcminmod

   elemental double precision function dcminmod_nocheck(d1,d2)                     ! basic minmod definition
   implicit none
   double precision, intent(in) :: d1, d2
   integer :: minabs

   minabs = merge(1, 0, abs(d1) < abs(d2))
   dcminmod_nocheck = minabs*d1 + (1-minabs)*d2

   end function

   elemental double precision function dcentral(d1,d2)                     ! twee maal vergroot vanwege acl
   implicit none
   double precision, intent(in):: d1, d2
   
   double precision :: d3
   integer :: pos

   pos = merge(1,0,d1*d2 > 0d0)
   dcentral = pos*dcminmod_nocheck( (d1+d2)*0.5d0 , dcminmod_nocheck( 2d0*d1, 2d0*d2) )

   end function dcentral

   !> limiter function
   elemental double precision function dlimiter(d1,d2,limtyp)
   implicit none

   double precision, intent(in) :: d1, d2   !< left and right slopes
   integer         , intent(in) :: limtyp   !< first order upwind (0) or MC (>0)

   double precision             :: r
   double precision, parameter  :: dtol=1d-16

   double precision, parameter  :: TWO=2.0d0

   dlimiter = 0d0
   if (limtyp == 0)     return
   if ( d1*d2.lt.dtol ) return

   r = d1/d2    ! d1/d2

   !!     Monotinized Central
   dlimiter = max(0d0, min(TWO*r,TWO,0.5d0*(1d0+r)) )

   end function dlimiter

   elemental double precision function dlimitercentral(dc,d2,limtyp)  ! as dlimiter, now for central gradient instead of slope
   implicit none

   double precision, intent(in) :: dc, d2   !< central and right slopes
   integer         , intent(in) :: limtyp   !< first order upwind (0) or MC (>0)

   double precision             :: r, d1
   double precision, parameter  :: dtol=1d-16

   dlimitercentral = 0d0
   if (limtyp == 0)     return

   !  compute left slope (assume uniform mesh)
   d1 = 2d0*dc - d2

   if ( d1*d2.lt.dtol ) return

   r = d1/d2    ! d1/d2

   dlimitercentral = d2 * max(0d0, min(2d0*r,0.5d0*(1d0+r),2d0) ) !  Monotonized Central
   end function dlimitercentral

   elemental double precision function dsuperbee(ds1, ds2)
   implicit none
   double precision, intent(in)     :: ds1, ds2
   double precision                 :: r

   if (ds1*ds2>0d0) then
      r = ds1/ds2
      dsuperbee = max(0d0, min(2d0*r,1d0),min(r,2d0))
      dsuperbee = dsuperbee*ds2
   else
      dsuperbee = 0d0
   end if
   end function

   elemental double precision function dslim(d1,d2,limtyp)
   implicit none
   double precision, intent(in) :: d1, d2                             ! voorslope, naslope
   integer, intent(in) :: limtyp

   ! In order to translate psi to limiter, you have to multiply the psi function with ds2
   ! e.g. lax wendroff central: psi=1, dslimiter=d2

   select case(LIMTYP)
   case (4)                        ! monotonized central no division
      dslim = dcentral(d1,d2)
   case default
      dslim = 0
   case (1)                        ! codering guus, met voorslope
      dslim = d1*dminmod(d1,d2)
   case (2)                        ! codering guus, met voorslope
      dslim = d1*dvanleer(d1,d2)
   case (3)                        ! codering guus, met voorslope
      dslim = d1*dkoren(d1,d2)
   case (5)                        ! monotonized central Sander with division
      dslim = dlimiter(d1,d2,limtyp) * d2
   case (6)                        ! monotonized central Sander with division, upwind slope ds1 at central cel
      dslim = dlimitercentral(d1,d2,limtyp)
   case (11)                      ! standaard codering
      dslim = d2*dminmod(d1,d2)
   case (12)                       ! standaard codering
      dslim = d2*dvanleer(d1,d2)
   case (13)                       ! standaard codering
      dslim = d2*dkoren(d1,d2)
   case (14)                       ! monotonized central, == 4
      dslim = dcentral(d2,d1)
   case (15)                       ! minmod central
      dslim = dcminmod(d2,d1)
   case (20)                       ! leftbiased, beam&warming
      dslim = d1
   case (21)                       ! central
      dslim = d2
   case (22)                       ! superbee
      dslim = dsuperbee(d1,d2)
   end select

   end function dslim

   !> limited higher-order correction of vector data
   elemental subroutine dslimvec(ds1x, ds1y, ds2x, ds2y, csu, snu, limtyp, dsx, dsy)
   use m_flowparameters
   implicit none

   double precision, intent(in)  :: ds1x, ds1y   !< "voorslope" components
   double precision, intent(in)  :: ds2x, ds2y   !< "naslope" components
   double precision, intent(in)  :: csu, snu     !< orientation vector components
   integer,          intent(in)  :: limtyp       !< limiter type
   double precision, intent(out) :: dsx, dsy     !< correction components

   double precision              :: ds1n, ds1t   !< normal and tangential component, respectively
   double precision              :: ds2n, ds2t   !< normal and tangential component, respectively
   double precision              :: dsn, dst

   if ( jalimnor.eq.1 ) then
      ds1n =  csu*ds1x + snu*ds1y
      ds1t = -snu*ds1x + csu*ds1y

      ds2n =  csu*ds2x + snu*ds2y
      ds2t = -snu*ds2x + csu*ds2y

      dsn = 0d0
      dst = 0d0

      if (abs(ds2n)  > eps10 .and. abs(ds1n) > eps10) then
         dsn = dslim(ds1n, ds2n, limtyp)
      endif

      if (abs(ds2y)  > eps10 .and. abs(ds1y) > eps10) then
         dst =  dslim(ds1t, ds2t, limtyp)
      endif

      dsx = csu*dsn - snu*dst
      dsy = snu*dsn + csu*dst

   else
      dsx = dslim(ds1x, ds2x, limtyp)
      dsy = dslim(ds1y, ds2y, limtyp)
   end if

   return
   end subroutine dslimvec

   end module m_limiters
