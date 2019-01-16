module m_read_roughness
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2019.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------
!  $Id$
!  $HeadURL$
!-------------------------------------------------------------------------------
   
   use m_Branch
   use m_GlobalParameters
   use m_read_table
   use m_hash_search
   use m_hash_list
   use m_network
   use m_readSpatialData
   use m_Roughness
   use m_spatial_data
   use properties
   use  string_module
   use messagehandling

   implicit none

   private

   public roughness_reader
   public read_roughness_cache
   public write_roughness_cache

contains

   subroutine roughness_reader(network, roughnessfiles, mapdir, md_ptr)
   
      type(t_network), intent(inout), target :: network
      character(len=*), intent(in)           :: mapdir, roughnessfiles
      type(tree_data), pointer, intent(in), optional   :: md_ptr

      type(t_RoughnessSet), pointer          :: rgs
      type(t_branchSet), pointer             :: brs
      type(t_spatial_dataSet) , pointer      :: spData
      
      character(len=1024)                    :: inputfiles
      integer                                :: i
      integer                                :: ifrst
      integer                                :: isemi
      integer                                :: count
      integer                                :: def_type
      logical                                :: success
      character(len=charLn)                  :: file
      double precision                       :: default

      integer                                :: ibin = 0
      character(len=Charln)                  :: binfile
      logical                                :: file_exist
      integer                                :: istat
      
      inputfiles = roughnessfiles
      default = 60d0
      def_type = 1
      def_type = 1
      if (present(md_ptr)) then
         call prop_get_double(md_ptr, 'GlobalValues', 'roughness', default, success)
         if (success) then
            call prop_get_integer(md_ptr, 'GlobalValues', 'roughnessType', def_type, success)
         endif
         if (.not. success) then
            def_type = R_Chezy
            default = 45
         endif
      endif
      
      binfile = 'Roughness.cache'
      inquire(file=binfile, exist=file_exist)
      if (doReadCache .and. file_exist) then
         open(newunit=ibin, file=binfile, status='old', form='unformatted', access='stream', action='read', iostat=istat)
         if (istat /= 0) then
            call setmessage(LEVEL_FATAL, 'Error opening Roughness Cache file')
            ibin = 0
         endif
         call read_roughness_cache(ibin, network)
         close(ibin)
         return
      endif
   
      rgs    => network%rgs
      brs    => network%brs
      spdata => network%spdata
      
      ! initialize hash_search
      ifrst = 1
      isemi = 1
      count = 1
      do while (len_trim(inputfiles(ifrst:)) > 0) 
         isemi = scan(inputfiles(ifrst:), ';')
         if (isemi ==0) then
            isemi = len_trim(inputfiles(ifrst:))+1
         endif
         isemi = ifrst+isemi - 1
         ifrst = isemi+1
         count = count+1
      enddo
   
      ! just to be sure save space for 3 default roughnesses.
      count = count+3
      call hashfill_init(rgs%hashlist, count)
      call realloc(rgs%hashlist%id_list, count)
   
      ! First three roughnesses are 'Main', 'FloodPlain1' and 'FloodPlain2' 
      rgs%count = 3
      call realloc(rgs)
      rgs%rough(1)%id = 'Main'
      rgs%rough(2)%id = 'FloodPlain1'
      rgs%rough(3)%id = 'FloodPlain2'
      success = .true.
      do i = 1, 3
         if (success) then
            success = hashsearch_or_add(rgs%hashlist, rgs%rough(i)%id) == i
            rgs%rough(i)%spd_pos_idx = 0
            rgs%rough(i)%spd_neg_idx = 0
            rgs%rough(i)%rgh_type_pos => null()
            rgs%rough(i)%rgh_type_neg => null()
            rgs%rough(i)%fun_type_pos => null()
            rgs%rough(i)%fun_type_neg => null()
         endif
      enddo
   
      if (.not. success) then
         call setmessage(LEVEL_FATAL, 'Internal error in roughness reader')
      endif
   
      ! now start reading individual files
      do while (len_trim(inputfiles) > 0) 
         isemi = scan(inputfiles, ';')
         if (isemi ==0) then
            isemi = len_trim(inputfiles)+1
         endif
         
         file = inputfiles(1:isemi-1)
         inputfiles = inputfiles(isemi+1:)
         if (len_trim(mapdir) > 0) then
            file = trim(mapdir)//file
         endif
            
         call remove_leading_spaces(trim(file))
         call read_roughnessfile(rgs, brs, spdata, file, default, def_type)
      enddo
   
      if (rgs%rough(1)%iSection == 1 .and. .not. associated(rgs%rough(1)%fun_type_pos)) then
         call setmessage(LEVEL_FATAL, 'Obligatory main roughness section for ZW cross sections is missing')
      elseif (rgs%rough(2)%iSection == 2 .and. .not. associated(rgs%rough(2)%fun_type_pos)) then
         call setmessage(LEVEL_FATAL, 'roughness section FloodPlain1 is missing, while at least one ZW cross section contains section Floodplain1')
      elseif (rgs%rough(3)%iSection == 3 .and. .not. associated(rgs%rough(3)%fun_type_pos)) then
         call setmessage(LEVEL_FATAL, 'roughness section FloodPlain2 is missing, while at least one ZW cross section contains section Floodplain2')
      endif
   end subroutine roughness_reader

   subroutine read_roughnessfile(rgs, brs, spdata, inputfile, default, def_type)
   
      type(t_roughnessSet), intent(inout)    :: rgs
      type(t_branchSet), intent(in)          :: brs
      type(t_spatial_dataSet), intent(inout) :: spdata
      character(len=charLn), intent(in)      :: inputfile
      double precision, intent(inout)        :: default
      integer, intent(inout)                 :: def_type
   
      integer                                :: istat
      integer                                :: count
      integer                                :: itype
      integer                                :: irgh
      integer                                :: ibr
      integer                                :: i
      integer                                :: nlev
      integer                                :: maxlevels
      integer                                :: isp
      logical                                :: flowdir
      logical                                :: success
      type(tree_data), pointer               :: tree_ptr
      type(t_roughness), pointer             :: rgh
      character(len=Idlen)                   :: sectionId
      character(len=Idlen)                   :: branchid
      double precision, allocatable          :: levels(:,:)
   
      integer, pointer, dimension(:)         :: rgh_type
      integer, pointer, dimension(:)         :: fun_type
   
      ! create and fill tree
      call tree_create(trim(inputfile), tree_ptr, maxlenpar)
      call prop_file('ini',trim(inputfile),tree_ptr,istat)
   
      ! Get section id
      call prop_get_string(tree_ptr, 'Content', 'sectionId', sectionId, success)
      if (.not. success) then
         call setmessage(LEVEL_FATAL, 'SectionId not found in roughness definition file: '//trim(inputfile))
      endif
      call prop_get_integer(tree_ptr, 'Content', 'globalType', def_type, success)
   
      irgh = hashsearch_or_add(rgs%hashlist, sectionId)
      if (irgh == rgs%count+1) then
         rgs%rough(irgh)%id           = sectionId
         rgs%rough(irgh)%spd_pos_idx  = 0
         rgs%rough(irgh)%spd_neg_idx  = 0
         rgs%rough(irgh)%rgh_type_pos => null()
         rgs%rough(irgh)%rgh_type_neg => null()
         rgs%rough(irgh)%fun_type_pos => null()
         rgs%rough(irgh)%fun_type_neg => null()
         rgs%count = irgh
         if (rgs%count > rgs%size) then
            call realloc(rgs)
         endif
      elseif (irgh > rgs%count+1) then
         call setmessage(LEVEL_FATAL, 'Internal error in roughness reader')
      endif

      rgh => rgs%rough(irgh)
      rgh%iSection = irgh
   
      flowDir = 0
      call prop_get_logical(tree_ptr, 'Content', 'flowDirection', flowdir, success)
   
      if (.not.flowdir) then
         if (associated(rgh%rgh_type_pos)) then
            call setmessage(LEVEL_FATAL, 'Internal error in roughness reader')
         endif
         allocate(rgh%rgh_type_pos(brs%count))
         allocate(rgh%fun_type_pos(brs%count))
         rgh_type => rgh%rgh_type_pos
         fun_type => rgh%fun_type_pos
      else
         if (associated(rgh%rgh_type_neg)) then
            call setmessage(LEVEL_FATAL, 'Internal error in roughness reader')
         endif
         allocate(rgh%rgh_type_neg(brs%count))
         allocate(rgh%fun_type_neg(brs%count))
         rgh_type => rgh%rgh_type_neg
         fun_type => rgh%fun_type_neg
      endif
      rgh_type = -1
      fun_type = -1
   
      count = 0
      if (associated(tree_ptr%child_nodes)) then
            count = size(tree_ptr%child_nodes)
      end if
   
      maxlevels = 1
      do i = 1, count
         call prop_get_integer(tree_ptr%child_nodes(i)%node_ptr, '', 'numLevels',nlev,success)
         if (success .and. nlev > maxlevels) then
            maxlevels = nlev
         endif
      enddo
      allocate(levels(maxlevels, brs%count))
   
      do i = 1, count
         itype = -1
         if (tree_get_name(tree_ptr%child_nodes(i)%node_ptr) .ne. 'branchproperties') then
            cycle
         endif
         call prop_get_integer(tree_ptr%child_nodes(i)%node_ptr, '', 'roughnessType',itype,success)
         call prop_get_string(tree_ptr%child_nodes(i)%node_ptr, '', 'branchId',branchid, success)
         ibr = hashsearch(brs%hashlist, branchid)
         if (ibr <= 0 .or. ibr > brs%count) then
            call setmessage(LEVEL_ERROR, 'Unknown branchid found ('//trim(branchid)//') in file: '//inputfile)
            cycle
         endif
      
         rgh_type(ibr) = itype
         call prop_get_integer(tree_ptr%child_nodes(i)%node_ptr, '', 'functionType',fun_type(ibr),success)
      enddo
   
      ! fill up missing branches, using branch orders
      call init_at_branches(brs, rgh_type, fun_type, def_type)
   
      ! Read spatial data
      call spatial_data_reader(isp, spdata, brs, inputfile, default, -def_type, .false.)
      if (.not.flowdir) then
         rgh%spd_pos_idx = isp
      else
         rgh%spd_neg_idx = isp
      endif
   
      deallocate(levels)
   
   end subroutine read_roughnessfile

   subroutine init_at_branches(brs, rgh_type, fun_type, def_type)
   
      type(t_branchset), intent(in) :: brs
      integer, dimension(:), intent(inout) :: rgh_type
      integer, dimension(:), intent(inout) :: fun_type
      integer, intent(in) :: def_type
      integer ibr

      do ibr = 1, brs%count
         if (rgh_type(ibr) < 0) then
            rgh_type(ibr) = def_type
            fun_type(ibr) = 0
         endif
      enddo
   
   end subroutine init_at_branches
 
   subroutine read_roughness_cache(ibin, network)
   
      type(t_network), intent(inout)  :: network
      integer, intent(in)             :: ibin
      
      integer                         :: i
      integer                         :: j
      integer                         :: tblCount

      type(t_Roughness), pointer      :: pRough
      integer                         :: nbrs
      logical                         :: hasPos
      logical                         :: hasNeg
      
      nbrs = network%brs%Count

      read(ibin) network%rgs%count
      network%rgs%growsby = network%rgs%count + 2
      call realloc(network%rgs)

      do i = 1, network%rgs%Count
      
         pRough => network%rgs%rough(i)
       
         read(ibin) pRough%id
         
         read(ibin) pRough%iSection
         
         read(ibin) hasPos
         if (hasPos) then
         
            allocate(pRough%rgh_type_pos(nbrs))
            allocate(pRough%fun_type_pos(nbrs))
         
            read(ibin) (pRough%rgh_type_pos(j), j = 1, nbrs)
            read(ibin) (pRough%fun_type_pos(j), j = 1, nbrs)

         endif
         
         read(ibin) hasNeg
         if (hasNeg) then
         
            allocate(pRough%rgh_type_neg(nbrs))
            allocate(pRough%fun_type_neg(nbrs))
         
            read(ibin) (pRough%rgh_type_neg(j), j = 1, nbrs)
            read(ibin) (pRough%fun_type_neg(j), j = 1, nbrs)

         endif

         read(ibin) pRough%spd_pos_idx
         read(ibin) pRough%spd_neg_idx
         
      enddo
      
      read(ibin) tblCount

      do i = 1, tblCount
         network%rgs%tables%Count = network%rgs%tables%Count + 1
         if (network%rgs%tables%Count > network%rgs%tables%Size) Then
            call realloc(network%rgs%tables)
         endif
         allocate(network%rgs%tables%tb(i)%table)
         call read_table_cache(ibin, network%rgs%tables%tb(i)%table)
      enddo

      call read_hash_list_cache(ibin, network%rgs%hashlist)
      
   end subroutine read_roughness_cache
   
   subroutine write_roughness_cache(ibin, network)

      type(t_network), intent(in)     :: network
      integer, intent(in)             :: ibin
      
      type(t_RoughnessSet)            :: rgs
      type(t_Roughness), pointer      :: pRough
      integer                         :: i
      integer                         :: j
      integer                         :: nbrs
      logical                         :: hasPos
      logical                         :: hasNeg
      
      rgs  = network%rgs
      nbrs = network%brs%Count
      
      write(ibin) rgs%Count

      do i = 1, rgs%Count
      
         pRough => rgs%rough(i)
       
         write(ibin) pRough%id
         
         write(ibin) pRough%iSection
         
         if (associated(pRough%rgh_type_pos)) then
            hasPos = .true.
            write(ibin) hasPos
            write(ibin) (pRough%rgh_type_pos(j), j = 1, nbrs)
            write(ibin) (pRough%fun_type_pos(j), j = 1, nbrs)
         else
            hasPos = .false.
            write(ibin) hasPos
         endif
         
         
         if (associated(pRough%rgh_type_neg)) then
            hasNeg = .true.
            write(ibin) hasNeg
            write(ibin) (pRough%rgh_type_neg(j), j = 1, nbrs)
            write(ibin) (pRough%fun_type_neg(j), j = 1, nbrs)
         else
            hasNeg = .false.
            write(ibin) hasNeg
         endif
         
         write(ibin) pRough%spd_pos_idx
         write(ibin) pRough%spd_neg_idx

      enddo

      write(ibin) rgs%tables%Count
      do i = 1, rgs%tables%Count
         call write_table_cache(ibin, rgs%tables%tb(i)%table)
      enddo
      
      call write_hash_list_cache(ibin, rgs%hashlist)

   end subroutine write_roughness_cache

end module m_read_roughness