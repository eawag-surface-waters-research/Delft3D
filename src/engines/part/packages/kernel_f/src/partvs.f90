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

      subroutine partvs ( lun2   , itime  , nosubs , nopart , ivtset ,   &
                          ivtime , vsfour , vsfact , wpart  , wsettl ,   &
                          modtyp , nmax   , mmax   , lgrid3 , nolay  ,   &
                          npart  , mpart  , kpart  , nosegp , noseglp ,  &
                          rhopart, rhowatc  , sizep  , const  , rhow   )

      use precision

      implicit none

      integer  ( ip), intent(in   ) :: lun2                    
      integer  ( ip), intent(in   ) :: itime                   
      integer  ( ip), intent(in   ) :: nosubs                  
      integer  ( ip), intent(in   ) :: nopart                  
      integer  ( ip), intent(in   ) :: ivtset                  
      integer  ( ip), intent(in   ) :: ivtime(ivtset)          
      real     ( rp), intent(in   ) :: vsfour(6,nosubs,ivtset) 
      real     ( rp)                :: vsfact(6,nosubs)        
      real     ( rp), intent(in   ) :: wpart (  nosubs,nopart) 
      real     ( rp), intent(  out) :: wsettl(         nopart) 
      integer  ( ip), intent(in   ) :: modtyp                  
      integer  ( ip), intent(in   ) :: nosegp                  
      integer  ( ip), intent(in   ) :: noseglp                 
      integer  ( ip), intent(in   ) :: npart( nopart)          
      integer  ( ip), intent(in   ) :: mpart( nopart)          
      integer  ( ip), intent(in   ) :: kpart( nopart)          
      integer  ( ip), intent(in   ) :: nmax                  
      integer  ( ip), intent(in   ) :: mmax                  
      integer  ( ip), intent(in   ) :: lgrid3 (nmax,mmax)    
      integer  ( ip), intent(in   ) :: nolay                   
      real     ( rp), intent(in   ) :: rhopart (nosubs, nopart)
      real     ( rp), intent(in   ) :: rhowatc (nosegp)        
      real     ( rp), intent(in   ) :: sizep (nosubs,*)     
      real     ( rp), intent(in   ) :: const(*)             
      real     ( rp), intent(in   ) :: rhow                 

      wsettl (1) = 0.0
      return

      end subroutine
