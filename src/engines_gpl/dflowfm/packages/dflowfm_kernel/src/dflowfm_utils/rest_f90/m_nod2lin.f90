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

   ! $Id$
   ! $HeadURL$
   module m_nod2lin

   implicit none
   contains

   !>    return x-component in link coordinate frame of vector in node coordinate frame
   elemental double precision function nod2linx(L,i12,ux,uy)
   use m_flowgeom, only: csb, snb
   use m_sferic
   implicit none

   integer,          intent(in) :: L      !< flowlink number
   integer,          intent(in) :: i12    !< left (1) or right (2) neighboring cell
   double precision, intent(in) :: ux, uy !< vector components in flowlnode coordinate frame

   if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
      nod2linx = ux
   else
      nod2linx =  csb(i12,L) * ux + snb(i12,L) * uy
   end if

   return
   end function nod2linx

   !>    return y-component in link coordinate frame of a vector in node coordinate frame
   elemental double precision function nod2liny(L,i12,ux,uy)
   use m_flowgeom, only: csb, snb
   use m_sferic
   implicit none

   integer,          intent(in) :: L      !< flowlink number
   integer,          intent(in) :: i12    !< left (1) or right (2) neighboring cell
   double precision, intent(in) :: ux, uy !< vector components in flownode coordinate frame


   if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
      nod2liny = uy
   else
      nod2liny =  -snb(i12,L) * ux + csb(i12,L) * uy
   end if

   return
   end function nod2liny

   !>    return x-component in node coordinate frame of a vector in link coordinate frame
   elemental double precision function lin2nodx(L,i12,ux,uy)
   use m_flowgeom, only: csb, snb
   use m_sferic
   implicit none

   integer,          intent(in) :: L   !< flowlink number
   integer,          intent(in) :: i12 !< left (1) or right (2) neighboring cell
   double precision, intent(in) :: ux, uy !< vector components in flowlink coordinate frame

   if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
      lin2nodx = ux
   else
      lin2nodx =  csb(i12,L) * ux - snb(i12,L) * uy
   end if

   return
   end function lin2nodx

   !>    return y-component in node coordinate frame of a vector in link coordinate frame
   elemental double precision function lin2nody(L,i12,ux,uy)
   use m_flowgeom, only: csb, snb
   use m_sferic
   implicit none

   integer,          intent(in) :: L   !< flowlink number
   integer,          intent(in) :: i12 !< left (1) or right (2) neighboring cell
   double precision, intent(in) :: ux, uy !< vector components in flowlink coordinate frame

   if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
      lin2nody = uy
   else
      lin2nody =  snb(i12,L) * ux + csb(i12,L) * uy
   end if

   return
   end function lin2nody

   !>    return x-component in link coordinate frame of vector in "klnup"-node coordinate frame
   elemental double precision function nodup2linx(L,ib,ux,uy)
   use m_flowgeom, only: csbup, snbup
   use m_sferic
   implicit none

   integer,          intent(in) :: L      !< flowlink number
   integer,          intent(in) :: ib     !< stencil index  (1 (iup=1), 2 (iup=2), 3 (iup=4), or 4 (iup=5))
   double precision, intent(in) :: ux, uy !< vector components in flownode coordinate frame


   if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
      nodup2linx = ux
   else
      nodup2linx =  csbup(ib,L) * ux + snbup(ib,L) * uy
   end if

   return
   end function nodup2linx

   !>    return y-component in link coordinate frame of vector in "klnup"-node coordinate frame
   elemental double precision function nodup2liny(L,ib,ux,uy)
   use m_flowgeom, only: csbup, snbup
   use m_sferic
   implicit none

   integer,          intent(in) :: L      !< flowlink number
   integer,          intent(in) :: ib     !< stencil index (1 (iup=1), 2 (iup=2), 3 (iup=4), or 4 (iup=5))
   double precision, intent(in) :: ux, uy !< vector components in flownode coordinate frame


   if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
      nodup2liny = uy
   else
      nodup2liny = -snbup(ib,L) * ux + csbup(ib,L) * uy
   end if

   return
   end function nodup2liny

   end module