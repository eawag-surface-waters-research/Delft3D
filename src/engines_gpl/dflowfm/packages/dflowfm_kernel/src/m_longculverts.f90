!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2020.                                
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

!> Module for long culvert data in a dflowfm model.
!! Long culverts are read from the structures.ini file(s), and converted into
!! new netlinks and prof1D definitions.
module m_longculverts
   use MessageHandling
   use m_missing

   private
   public realloc

   public loadLongCulvertsAsNetwork
   public finalizeLongCulvertsInNetwork
   public LongCulvertsToProfs
   public setFrictionForLongculverts
   public reduceFlowAreaAtLongculverts

   !> Type definition for longculvert data.
   type, public :: t_longculvert      
      character(len=IdLen)                           :: id
      integer                                        :: numlinks                   !< Number of links of the long culvert
      integer, dimension(:), allocatable             :: netlinks                   !< Net link numbers of the long culvert
      integer, dimension(:), allocatable             :: flowlinks                  !< Flow link numbers of the long culvert
      integer                                        :: ifrctyp         = imiss    !< Friction type 
      integer                                        :: allowed_flowdir            !< Allowed flowdir: 
                                                                                   !< 0 all directions
                                                                                   !< 1 only positive flow
                                                                                   !< 2 only negative flow
                                                                                   !< 3 no flow allowed 
      double precision                               :: friction_value  = dmiss    !< Friction value
      double precision, dimension(:), allocatable    :: bl                         !< Bed level on numlinks+1 points
      double precision                               :: width                      !< Width of the rectangular culvert
      double precision                               :: height                     !< Height of the rectangular culvert
      double precision                               :: valve_relative_opening     !< Relative valve opening: 0 = fully closed, 1 = fully open
   end type                              
   type(t_longculvert), dimension(:), allocatable, public     :: longculverts               !< Array containing long culvert data (size >= nlongculvertsg)              
   integer, public                                    :: nlongculvertsg             !< Number of longculverts               

   interface realloc
      module procedure reallocLongCulverts
   end interface


contains
   !> Loads the long culverts from a structures.ini file and
   !! creates extra netnodes+links for them.
   subroutine loadLongCulvertsAsNetwork(structurefile, jaKeepExisting, ierr)
       !use network_data
       use dfm_error
       use gridoperations, only: make1D2DLongCulverts
       use string_module, only: strcmpi
       use m_polygon
       use m_missing
       use m_Roughness
       use m_readstructures
       use messageHandling
       use properties
       use unstruc_channel_flow

       implicit none

       character(len=*),      intent(in   ) :: structurefile  !< File name of the structure.ini file.
       integer,               intent(in   ) :: jaKeepExisting !< Whether or not (1/0) to keep the existing already read long culverts. 
       integer,               intent(  out) :: ierr           !< Result status, DFM_NOERR in case of success.

       type(tree_data), pointer :: strs_ptr
       type(tree_data), pointer :: str_ptr
       character(len=IdLen) :: typestr
       character(len=IdLen) :: st_id
       character(len=IdLen) :: txt
       integer :: readerr, nstr, i, numcoords
       integer, allocatable, dimension(:)    :: links
       logical :: success
       integer :: istart
       integer :: nlongculvertsg0

       ierr = DFM_NOERR

       nlongculvertsg0 = nlongculvertsg ! Remember any old longculvert count

       if (jaKeepExisting == 0) then
          nlongculvertsg = 0
          if (allocated(longculverts)) then
             deallocate(longculverts)
          end if
       end if

       xpl = dmiss
       ypl = dmiss
       zpl = dmiss
       npl = 0

       ! Temporarily put structures.ini file into a property tree
       call tree_create(trim(structurefile), strs_ptr)
       call prop_inifile(structurefile, strs_ptr, readerr)
   ! check if file was successfully opened
       if ( readerr /= 0 ) then
          ierr = DFM_WRONGINPUT
          call mess(LEVEL_ERROR, 'Error opening file ''', trim(structurefile), ''' for loading the long culverts.')
       endif

       nstr = tree_num_nodes(strs_ptr)
       call realloc(longculverts, nlongculvertsg + nstr)
       do i=1,nstr
          str_ptr => strs_ptr%child_nodes(i)%node_ptr
       
          success = .true.
       
          if (.not. strcmpi(tree_get_name(str_ptr), 'Structure')) then
             ! Only read [Structure] blocks, skip any other (e.g., [General]).
             cycle
          end if
       
          typestr = ' '
          call prop_get_string(str_ptr, '', 'type',         typestr, success)
          if (.not. success .or. .not. strcmpi(typestr, 'longCulvert')) then
             cycle
          end if
       
          call prop_get(str_ptr, '',  'id', st_id, success)
          if (success) call prop_get(str_ptr, '', 'numCoordinates', numcoords, success)
          if (success) then
             nlongculvertsg = nlongculvertsg + 1
             longculverts(nlongculvertsg)%id = st_id
             longculverts(nlongculvertsg)%numlinks = numcoords-1
             allocate(longculverts(nlongculvertsg)%netlinks(numcoords-1))
             allocate(longculverts(nlongculvertsg)%flowlinks(numcoords-1))
             longculverts(nlongculvertsg)%flowlinks = -999
             allocate(longculverts(nlongculvertsg)%bl(numcoords))
             call increasepol(numcoords, 0)
             call prop_get(str_ptr, '', 'xCoordinates', xpl(npl+1:), numcoords, success)
             if (.not. success) then
                call SetMessage(LEVEL_ERROR, 'xCoordinates not found for long culvert: '// st_id )
             endif
             call prop_get(str_ptr, '', 'yCoordinates', ypl(npl+1:), numcoords, success)
             if (.not. success) then
                call SetMessage(LEVEL_ERROR, 'yCoordinates not found for long culvert: '// st_id )
             endif
             call prop_get(str_ptr, '', 'zCoordinates', zpl(npl+1:), numcoords, success)
             if (.not. success) then
                call SetMessage(LEVEL_ERROR, 'zCoordinates not found for long culvert: '// st_id )
             endif
             longculverts(nlongculvertsg)%bl = zpl(npl+1:npl+numcoords)
             npl = npl+numcoords+1 ! TODO: UNST-4328: success1 checking done later in readStructureFile().
             
             call prop_get(str_ptr, '', 'allowedFlowdir', txt, success)
             if (.not. success) then
                TXT = 'both'
             endif
             longculverts(nlongculvertsg)%allowed_flowdir = allowedFlowDirToInt(txt)
             
             call prop_get(str_ptr, '', 'width', longculverts(nlongculvertsg)%width, success)
             if (.not. success) then
                call SetMessage(LEVEL_ERROR, 'width not found for long culvert: '// st_id )
             endif
             call prop_get(str_ptr, '', 'height', longculverts(nlongculvertsg)%height, success)
             if (.not. success) then
                call SetMessage(LEVEL_ERROR, 'height not found for long culvert: '// st_id )
             endif
             call prop_get(str_ptr, '', 'frictionType', typestr, success)
             if (.not. success) then
                longculverts(nlongculvertsg)%ifrctyp = -999
             else
                call frictionTypeStringToInteger(typestr, longculverts(nlongculvertsg)%ifrctyp)
                call prop_get(str_ptr, '', 'frictionValue', longculverts(nlongculvertsg)%friction_value, success)
                if (.not. success) then
                   call SetMessage(LEVEL_ERROR, 'frictionValue not found for long culvert: '// st_id )
                endif
             endif
          
             call get_value_or_addto_forcinglist(str_ptr, 'valveRelativeOpening', longculverts(nlongculvertsg)%valve_relative_opening, st_id, &
                                    ST_LONGCULVERT,network%forcinglist, success)
             if (.not. success) then
                call SetMessage(LEVEL_ERROR, 'valveRelativeOpening not found for long culvert: '// st_id )
             endif
          else 
             call SetMessage(LEVEL_ERROR, 'numCoordinates not found for long culvert '//st_id)
          end if
       
       
       end do
       allocate(links(npl))
       call make1D2DLongCulverts(xpl, ypl, zpl, npl, links)
       
       istart = 1
       do i = nlongculvertsg0+1, nlongculvertsg
          longculverts(i)%netlinks = links(istart:istart+longculverts(i)%numlinks-1)
          istart = istart+longculverts(i)%numlinks+2
       enddo

       call tree_destroy(strs_ptr)

   end subroutine loadLongCulvertsAsNetwork

   
   !> Finalizes some necessary network administration after all long culverts have been read.
   !! Actual reading is done in other subroutine loadLongCulvertsAsNetwork().
   subroutine finalizeLongCulvertsInNetwork()
      use network_data

      integer :: Lnet, i, ilongc

      ! NOTE: IF setnodadm() is again called after this subroutine has completed, with more netlink permutations,
      !! Then the longculvert()%netlinks array is incorrect. This can be fixed if we change our approach
      !! to always using closeto1dnetlink() calls in the longCulvertsToProfs() subroutine, instead. For now, we are safe, though.

      call setnodadm(0)
      ! Netlink numbers have probably been permuted by setnodadm, so also update netlinks.
      do ilongc = 1, nlongculvertsg
         do i = 1, longculverts(ilongc)%numlinks
            ! Netlink numbers have probably been permuted after the initial long culvert reading, so also update netlinks.
            Lnet = Lperminv(longculverts(ilongc)%netlinks(i))
            longculverts(ilongc)%netlinks(i) = Lnet
         enddo
      enddo
   end subroutine finalizeLongCulvertsInNetwork


   !> Reallocates a given longculvert array to larger size.
   !! Any existing longculvert data is copied into the new array.
   subroutine reallocLongCulverts(lcs, newsize)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_longculvert), allocatable, intent(inout) :: lcs(:)  !< The existing longculvert array.
      integer,                          intent(in   ) :: newsize !< The desired new size.

      ! Local variables
      type(t_longculvert), allocatable :: oldlcs(:)
      integer :: oldsize, i

      ! Program code

      if (allocated(lcs)) then
         oldsize = size(lcs)
      else
         oldsize = 0
      end if

      if (newsize > oldsize) then
         allocate(oldlcs(oldsize))
         do i=1,oldsize
            oldlcs(i) = lcs(i)
         end do

         if (allocated(lcs)) then
            deallocate(lcs)
         end if
         allocate(lcs(newsize))
         do i=1,oldsize
            lcs(i) = oldlcs(i)
         end do
      endif

   end subroutine reallocLongCulverts


   !> Initializes the cross section administration for long culverts in prof1d and other relevant flow geometry arrays.
   !! * Sets netlink numbers and flowlink numbers.
   !> * Fills for the corresponding flow links the bedlevels, bobs and prof1d data.
   subroutine longculvertsToProfs()
      use network_data
      use m_flowgeom

      integer :: Lnet, Lf, i, ilongc, k1, k2

      do ilongc = 1, nlongculvertsg
         do i = 1, longculverts(ilongc)%numlinks
            if (longculverts(nlongculvertsg)%flowlinks(i) < 0) then
               ! Flow links have not yet been initialized, this is the first call.
               ! Netlink numbers have been set correctly in finalizeLongCulvertsInNetwork() already.
               Lnet = longculverts(ilongc)%netlinks(i)
               longculverts(ilongc)%flowlinks(i) = lne2ln(Lnet)
            endif
         enddo
      enddo
   
      do ilongc = 1, nlongculvertsg
         do i = 1, longculverts(ilongc)%numlinks
            Lf = longculverts(ilongc)%flowlinks(i)
            k1 = ln(1,Lf)
            k2 = ln(2,Lf)
            bob(1, Lf) = longculverts(ilongc)%bl(i)
            bob(2, Lf) = longculverts(ilongc)%bl(i+1)
            bl(k1) = min(bl(k1), bob(1,Lf))
            bl(k2) = min(bl(k2), bob(2,Lf))

            wu(Lf) = longculverts(ilongc)%width
            prof1D(1,Lf)  = wu(Lf)
            prof1D(2,Lf)  = longculverts(ilongc)%height
            prof1D(3,Lf)  =  2                                      ! for now, simple rectan
         enddo
      enddo
   
   end subroutine longculvertsToProfs
   
   !> Fill frcu and icrctyp for the corresponding flow link numbers of the long culverts
   subroutine setFrictionForLongculverts()
      use m_flow
      implicit none

      integer :: LL, ilongc, Lf

      do ilongc = 1, nlongculvertsg
         do LL = 1, longculverts(ilongc)%numlinks
            Lf = abs(longculverts(ilongc)%flowlinks(LL))
            if (longculverts(ilongc)%ifrctyp > 0) then
               ifrcutp(Lf) = longculverts(ilongc)%ifrctyp
            end if
            if (longculverts(ilongc)%friction_value > 0) then
               frcu(Lf) = longculverts(ilongc)%friction_value
            endif
         enddo
      enddo
   
   end subroutine setFrictionForLongculverts

   !> In case  the valve_relative_area < 1 the flow area
   !! at the first link is reduced by valve_relative_area
   subroutine reduceFlowAreaAtLongculverts()
      use m_flow

      implicit none

      integer i, L

      do i = 1, nlongculvertsg
         L = longculverts(i)%flowlinks(1)
         au(L) = longculverts(i)%valve_relative_opening * au(L)
      enddo

   end subroutine reduceFlowAreaAtLongculverts

end module m_longculverts
    