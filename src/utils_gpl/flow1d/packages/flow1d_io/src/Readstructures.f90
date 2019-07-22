module m_readstructures
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

   use MessageHandling
   use string_module
   use m_network
   use m_CrossSections
   use m_1d_structures
   use m_Weir
   use m_Universal_Weir
   use m_Culvert
   use m_Bridge
   use m_pump
   use m_Orifice
   use m_General_Structure
   use m_ExtraResistance
   use m_Dambreak

   use properties
   use m_hash_list
   use m_hash_search
   use m_tables
   use m_read_table

   implicit none

   private

   public readStructures
   public read_structure_cache
   public write_structure_cache
   public readPump
   public readDambreak

   !> The file version number of the structure file format: d.dd, [config_major].[config_minor], e.g., 1.03
   !!
   !! Note: read config_minor as a 2 digit-number, i.e., 1.1 > 1.02 (since .1 === .10 > .02).
   !! Convention for format version changes:
   !! * if a new format is backwards compatible with old files, only
   !!   the minor version number is incremented.
   !! * if a new format is not backwards compatible (i.e., old files
   !!   need to be converted/updated by user), then the major version number
   !!   is incremented.
   
   ! Structure file current version: 1.00
   integer, parameter :: StructureFileMajorVersion = 2
   integer, parameter :: StructureFileMinorVersion = 0
   
   ! History structure file versions:

   ! 0.00 (pre-2019  ): Unversioned flow1d/SOBEK3 version of *.ini type structure file.
   ! 1.00 (2019-07-05): Consistent renaming,
   !                    * pumps: nrStages -> numStages, reductionFactorLevels -> numReductionLevels
   !                    * culverts: lossCoeffCount -> numLossCoeff
   !                    * universal weir: levelsCount -> numLevels

   contains

   !> Read the structure.ini file
   subroutine readStructures(network, structureFile)
      use m_GlobalParameters
      use m_1d_Structures
      implicit none
      
      type(t_network), intent(inout) :: network              !< Network pointer
      character*(*), intent(in)      :: structureFile        !< Name of the structure file

      logical                                                :: success
      type(tree_data), pointer                               :: md_ptr 
      integer                                                :: istat
      integer                                                :: numstr
      integer                                                :: i
      character(len=IdLen)                                   :: str_buf

      character(len=IdLen)                                   :: typestr
      character(len=IdLen)                                   :: structureID
      character(len=IdLen)                                   :: branchID
      
      double precision                                       :: Chainage
      integer                                                :: branchIdx
      logical                                                :: isPillarBridge
      logical                                                :: isInvertedSiphon

      integer                                                :: iCompound
      character(len=IdLen)                                   :: compoundName
      character(len=IdLen), allocatable, dimension(:)        :: compoundNames
      
      integer                                                :: iStrucType
      integer                                                :: istru
      type(t_structure), pointer                             :: pstru
      integer                                                ::  nweir
      integer                                                ::  nculvert
      integer                                                ::  norifice
      integer                                                ::  ngenstru
      integer                                                ::  nbridge
      integer                                                ::  ngate

      integer                       :: pos
      integer                       :: ibin = 0
      character(len=Charln)         :: binfile
      logical                       :: file_exist
      integer                       :: major, minor, ierr
      pos = index(structureFile, '.', back = .true.)
      binfile = structureFile(1:pos)//'cache'
      inquire(file=binfile, exist=file_exist)
      if (doReadCache .and. file_exist) then
         open(newunit=ibin, file=binfile, status='old', form='unformatted', access='stream', action='read', iostat=istat)
         if (istat /= 0) then
            call setmessage(LEVEL_FATAL, 'Error opening Structure Cache file')
            ibin = 0
         endif
         call read_structure_cache(ibin, network)
         close(ibin)
         ibin = 0
         return
      endif

      call tree_create(trim(structurefile), md_ptr, maxlenpar)
      call prop_file('ini',trim(structurefile),md_ptr,istat)
      
      ! check FileVersion
      ierr = 0
      major = 1
      minor = 0
      call prop_get_version_number(md_ptr, major = major, minor = minor, success = success)
      ! by exception, we backwards-support majorVersion=1 (for orifice and weir)
      if ((major /= StructureFileMajorVersion .and. major /= 1) .or. minor > StructureFileMinorversion) then
         write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported format of structure file detected in '''//trim(structurefile)//''': v', major, minor, '. Current format: v',StructureFileMajorVersion,StructureFileMinorVersion,'. Ignoring this file.'
         call warn_flush()
         ierr = 1
      end if

      if (ierr /= 0) then
         goto 999
      end if

      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if

      do i = 1, numstr
         
         if (tree_get_name(md_ptr%child_nodes(i)%node_ptr) .eq. 'structure') then
            
            if (network%sts%count+1 > network%sts%Size) then
               call realloc(network%sts)
            endif
            pstru => network%sts%struct(network%sts%count+1)
            ! Read Common Structure Data
            
            call prop_get(md_ptr%child_nodes(i)%node_ptr, 'structure', 'id', structureID, success)
            pstru%id = structureID
            if (.not. success) then
               write (msgbuf, '(a,i0,a)') 'Error Reading Structure #', i, ' from '''//trim(structureFile)//''', Id is missing.'
               call err_flush()
               cycle
            endif
            pstru%name = pstru%id
            call prop_get(md_ptr%child_nodes(i)%node_ptr, 'structure', 'name', pstru%name)
            if (success) call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'structure', 'branchid', branchID, success)
            if (success) call prop_get_double(md_ptr%child_nodes(i)%node_ptr, 'structure', 'chainage', pstru%chainage, success)

            pstru%numCoordinates = 0
            if (success) then
               pstru%ibran = hashsearch(network%brs%hashlist, branchID)
               if (pstru%ibran <= 0) then
                  msgbuf = 'Branchid '//trim(branchID)//' not found, check '//trim(pstru%id)
                  call err_flush()
                  success = .false.
                  cycle
               endif
            else 
               call prop_get(md_ptr%child_nodes(i)%node_ptr, 'structure', 'numCoordinates', pstru%numCoordinates, success)
               if (success) then
                  allocate(pstru%xCoordinates(pstru%numCoordinates), pstru%yCoordinates(pstru%numCoordinates))
                  call prop_get(md_ptr%child_nodes(i)%node_ptr, 'structure', 'xCoordinates', pstru%xCoordinates, &
                                pstru%numCoordinates, success)
                  if (success) call prop_get(md_ptr%child_nodes(i)%node_ptr, 'structure', 'yCoordinates', pstru%yCoordinates, &
                                pstru%numCoordinates, success)
               endif
            endif
            
            if (.not.success) then
               msgbuf = 'Location specification is missing for structure '//trim(pstru%id)//' in '//trim(structureFile)
               call warn_flush()
               cycle
            endif
            
            if (success) call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'structure', 'type', typestr, success)
            if (.not. success) then
               write (msgbuf, '(a,i0,a)') 'Error Reading Structure '''//trim(structureId)//''' from '''//trim(structureFile)//''', Type is missing.'
               call err_flush()
               cycle
            endif
      
            call prop_get_integer(md_ptr%child_nodes(i)%node_ptr, 'structure', 'compound', iCompound, success)
            if (.not. success) iCompound = 0
            
            if (iCompound > 0) then
            
               call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'structure', 'compoundName', compoundName, success)
               if (success .and. len_trim(compoundName) > 0) then
                  if (.not. allocated(compoundNames)) then
                     allocate(compoundNames(numstr))
                     compoundNames = ' '
                  endif
                  if (len_trim(compoundNames(iCompound)) <= 0) then
                     compoundNames(iCompound) = compoundName
                  endif
               else
                  write(str_buf, *) iCompound
                  call remove_leading_spaces(str_buf)
                  compoundName = 'Cmp'//trim(str_buf)
               endif
            else
               compoundName = ' '
            endif
            
            iStrucType = GetStrucType_from_string(typestr)
            pstru%type = iStrucType

            if (major ==1) then   
               ! support for version 1.0 structure files
               select case (iStrucType)
               case (ST_WEIR)
                  call readWeir(pstru%weir, md_ptr%child_nodes(i)%node_ptr, success)
               case (ST_UNI_WEIR)
                  call readUniversalWeir(pstru%uniweir, md_ptr%child_nodes(i)%node_ptr, success)
               case (ST_CULVERT)
                  call readCulvert(network, istru, md_ptr%child_nodes(i)%node_ptr, success)
               case (ST_BRIDGE)
                  call readBridge(network, istru, md_ptr%child_nodes(i)%node_ptr, isPillarBridge, success)
               case (ST_PUMP)
                  call readPump(pstru%pump, md_ptr%child_nodes(i)%node_ptr, structureId, network%forcinglist, success)
               case (ST_ORIFICE, ST_GATE)
                  call readOrifice(pstru%orifice, md_ptr%child_nodes(i)%node_ptr, success)
               case (ST_GENERAL_ST)
                  call readGeneralStructure_v100(pstru%generalst, md_ptr%child_nodes(i)%node_ptr, success)
               end select
            elseif (major ==2) then
               select case (iStrucType)
               case (ST_WEIR)
                  call readWeirAsGenstru(pstru%generalst, md_ptr%child_nodes(i)%node_ptr, structureID, network%forcinglist, success)
               !case (ST_UNI_WEIR)
               !   call readUniversalWeir(pstru%uniweir, md_ptr%child_nodes(i)%node_ptr, success)
               case (ST_CULVERT)
                  call readCulvert(network, istru, md_ptr%child_nodes(i)%node_ptr, success)
               !case (ST_BRIDGE)
               !   call readBridge(network, istru, md_ptr%child_nodes(i)%node_ptr, isPillarBridge, success)
               case (ST_PUMP)
                  call readPump(pstru%pump, md_ptr%child_nodes(i)%node_ptr, structureId, network%forcinglist, success)
               case (ST_ORIFICE, ST_GATE)
                  call readOrificeAsGenstru(pstru%generalst, md_ptr%child_nodes(i)%node_ptr, structureID, network%forcinglist, success)
               case (ST_GENERAL_ST)
                  call readGeneralStructure(pstru%generalst, md_ptr%child_nodes(i)%node_ptr, structureID, network%forcinglist, success)
                case default
                  call setmessage(LEVEL_ERROR,  trim(typestr)//' not implemented for version 2.00, see '//trim(pstru%id))
               end select
            endif
            
            if (.not. success) then
               call SetMessage(LEVEL_FATAL, 'Error Reading Structure '''//trim(pstru%id)//'''')
            else 
               network%sts%Count = network%sts%Count + 1
               call incStructureCount(network%sts, iStrucType)
            endif
         
         endif

         
      end do
      
      ! Set counters for number of weirs, culverts, etc
      network%sts%numweirs    = network%sts%countByType(ST_WEIR)
      network%sts%numculverts = network%sts%countByType(ST_CULVERT)
      network%sts%numPumps    = network%sts%countByType(ST_PUMP)
      network%sts%numOrifices = network%sts%countByType(ST_ORIFICE)
      network%sts%numGates    = network%sts%countByType(ST_GATE)
      network%sts%numGeneralStructures = network%sts%countByType(ST_GENERAL_ST)
      allocate(network%sts%weirIndices(network%sts%numweirs))
      allocate(network%sts%culvertIndices(network%sts%numCulverts))
      allocate(network%sts%pumpIndices(network%sts%numPumps))
      allocate(network%sts%orificeIndices(network%sts%numOrifices))
      allocate(network%sts%gateIndices(network%sts%numGates))
      allocate(network%sts%bridgeIndices(network%sts%numBridges))
      allocate(network%sts%generalStructureIndices(network%sts%numGeneralStructures))
      
      !set structure indices for different structure types
      nweir = 0
      nculvert = 0
      norifice = 0
      ngenstru = 0
      nbridge = 0
      ngate = 0
      do istru = 1, network%sts%Count
         select case (network%sts%struct(istru)%type)
         case (ST_WEIR)
            nweir = nweir+1
            network%sts%weirIndices(nweir) = istru
            ! From now on this is a general structure
            network%sts%struct(istru)%type = ST_GENERAL_ST
         case (ST_CULVERT)
            nculvert = nculvert + 1
            network%sts%culvertIndices(nculvert) = istru
         case (ST_ORIFICE)
            norifice = norifice + 1
            network%sts%orificeIndices(norifice) = istru
            ! From now on this is a general structure
            network%sts%struct(istru)%type = ST_GENERAL_ST
         case (ST_GATE)
            ngate = ngate + 1
            network%sts%gateIndices(ngate) = istru
            ! From now on this is a general structure
            network%sts%struct(istru)%type = ST_GENERAL_ST
         case (ST_BRIDGE)
            nbridge = nbridge + 1
            network%sts%bridgeIndices(nbridge) = istru
         case (ST_GENERAL_ST)
            ngenstru = ngenstru + 1
            network%sts%generalStructureIndices(ngenstru) = istru
         end select
         
      enddo
      
      ! Handle the Structure and Compound Names
      if (allocated(compoundNames)) then
      
         do i = 1, network%sts%Count
            if (network%sts%struct(i)%compound > 0) then
               if (len_trim(compoundNames(network%sts%struct(i)%compound)) > 0) then
                  network%sts%struct(i)%compoundName = compoundNames(network%sts%struct(i)%compound)
               endif
            endif
         enddo
         
         deallocate(compoundNames)

      else
      
         ! Copy ID'S to Names
         do i = 1, network%sts%Count
            network%sts%struct(i)%name = network%sts%struct(i)%id
         enddo
      
      endif
         
      
      call fill_hashtable(network%sts)
      
      if (.not. allocated(network%sts%restartData) .and. (network%sts%count > 0)) then
         allocate(network%sts%restartData(network%sts%count, CFiHighestParameter))
         network%sts%restartData = missingValue
      endif

999   continue
      call tree_destroy(md_ptr)

   end subroutine readStructures

   subroutine read_structure_cache(ibin, network)
   
      type(t_network), intent(inout)         :: network
      integer, intent(in)                    :: ibin
      
      type(t_structure), pointer             :: pstr
      integer                                :: i
      integer                                :: j

      read(ibin) network%sts%count
      network%sts%growsby = network%sts%count + 2
      call realloc(network%sts)

      read(ibin) (network%sts%countByType(i), i = 1, ST_MAX_TYPE)
      
      read(ibin) network%sts%compoundCount
      read(ibin) network%sts%hasExtraResistance

      do i = 1, network%sts%Count
      
         pstr => network%sts%struct(i)
         
         read(ibin) pstr%id
         read(ibin) pstr%name
         read(ibin) pstr%type
         read(ibin) pstr%ibran
         read(ibin) pstr%linknumbers(1)
         read(ibin) pstr%xCoordinates(1)
         read(ibin) pstr%yCoordinates(1)
         read(ibin) pstr%chainage
         read(ibin) pstr%compound
         read(ibin) pstr%compoundName
         
         select case(pstr%type)
            case(ST_WEIR)
               allocate(pstr%weir)
               read(ibin) pstr%weir%allowedflowdir
               read(ibin) pstr%weir%crestlevel
               read(ibin) pstr%weir%crestwidth
               read(ibin) pstr%weir%dischargecoeff
               read(ibin) pstr%weir%latdiscoeff
               read(ibin) pstr%weir%crestlevel
               read(ibin) pstr%weir%cmu
               
            case(ST_ORIFICE)
               allocate(pstr%orifice)
               read(ibin) pstr%orifice%allowedflowdir
               read(ibin) pstr%orifice%crestlevel
               read(ibin) pstr%orifice%crestwidth
               read(ibin) pstr%orifice%contrcoeff
               read(ibin) pstr%orifice%latcontrcoeff
               read(ibin) pstr%orifice%openlevel
               read(ibin) pstr%orifice%uselimitflowpos
               read(ibin) pstr%orifice%limitflowpos
               read(ibin) pstr%orifice%uselimitflowneg
               read(ibin) pstr%orifice%limitflowneg
               
            case(ST_PUMP)
               allocate(pstr%pump)
               read(ibin) pstr%pump%direction
               read(ibin) pstr%pump%nrstages
            
               allocate(pstr%pump%capacity(pstr%pump%nrstages))
               allocate(pstr%pump%ss_onlevel(pstr%pump%nrstages))
               allocate(pstr%pump%ss_offlevel(pstr%pump%nrstages))
               allocate(pstr%pump%ds_onlevel(pstr%pump%nrstages))
               allocate(pstr%pump%ds_offlevel(pstr%pump%nrstages))
               allocate(pstr%pump%ss_trigger(pstr%pump%nrstages))
               allocate(pstr%pump%ds_trigger(pstr%pump%nrstages))
               
               read(ibin) (pstr%pump%capacity(j), j = 1, pstr%pump%nrstages)
               read(ibin) (pstr%pump%ss_onlevel(j), j = 1, pstr%pump%nrstages)
               read(ibin) (pstr%pump%ss_offlevel(j), j = 1, pstr%pump%nrstages)
               read(ibin) (pstr%pump%ds_onlevel(j), j = 1, pstr%pump%nrstages)
               read(ibin) (pstr%pump%ds_offlevel(j), j = 1, pstr%pump%nrstages)
               read(ibin) (pstr%pump%ss_trigger(j), j = 1, pstr%pump%nrstages)
               read(ibin) (pstr%pump%ds_trigger(j), j = 1, pstr%pump%nrstages)
            
               call read_table_cache(ibin, pstr%pump%reducfact)
               
               ! Initialize Parameters for Pump
               pstr%pump%actual_stage      = 0
               pstr%pump%capacitySetpoint  = 0.0d0
               pstr%pump%computed_capacity = 0.0d0
               pstr%pump%discharge         = 0.0d0
               pstr%pump%is_active         = .true.
               pstr%pump%isControlled      = .false.
               pstr%pump%pump_head         = 0.0d0
               pstr%pump%reduction_factor  = 1.0d0
               pstr%pump%ss_level          = 0.0d0
               pstr%pump%ds_level          = 0.0d0
               pstr%pump%stage_capacity    = 0.0d0
            
            case(ST_CULVERT, ST_SIPHON, ST_INV_SIPHON)
               allocate(pstr%culvert)
               read(ibin) pstr%culvert%culvertType
               read(ibin) pstr%culvert%leftlevel
               read(ibin) pstr%culvert%rightlevel
               read(ibin) pstr%culvert%crosssectionnr
               pstr%culvert%pcross => network%crs%cross(pstr%culvert%crosssectionnr)
               read(ibin) pstr%culvert%allowedflowdir
               read(ibin) pstr%culvert%length
               read(ibin) pstr%culvert%inletlosscoeff
               read(ibin) pstr%culvert%outletlosscoeff
               read(ibin) pstr%culvert%has_valve
               if (pstr%culvert%has_valve) then
                   read(ibin) pstr%culvert%inivalveopen
                   call read_table_cache(ibin, pstr%culvert%losscoeff)
               endif
            
               ! Bend Loss for Siphons
               read(ibin) pstr%culvert%bendlosscoeff
            
               ! Levels for Normal Siphon
               read(ibin) pstr%culvert%turnonlevel
               read(ibin) pstr%culvert%turnofflevel
               
            case(ST_UNI_WEIR)
               allocate(pstr%uniweir)
               read(ibin) pstr%uniweir%allowedflowdir
               read(ibin) pstr%uniweir%crestlevel
               read(ibin) pstr%uniweir%dischargecoeff
               read(ibin) pstr%uniweir%freesubmergedfactor
            
               read(ibin) pstr%uniweir%yzcount
               
               allocate(pstr%uniweir%y(pstr%uniweir%yzcount))
               allocate(pstr%uniweir%z(pstr%uniweir%yzcount))

               read(ibin) (pstr%uniweir%y(j), j = 1, pstr%uniweir%yzcount)
               read(ibin) (pstr%uniweir%z(j), j = 1, pstr%uniweir%yzcount)
               
            case(ST_BRIDGE)
               allocate(pstr%bridge)
               read(ibin) pstr%bridge%allowedflowdir
               read(ibin) pstr%bridge%bedLevel
               read(ibin) pstr%bridge%pillarwidth
               read(ibin) pstr%bridge%formfactor
               read(ibin) pstr%bridge%useOwnCrossSection
               read(ibin) pstr%bridge%crosssectionnr
               if (pstr%bridge%useOwnCrossSection) then
                  pstr%bridge%pcross => network%crs%cross(pstr%bridge%crosssectionnr)
               endif
               read(ibin) pstr%bridge%bedFrictionType
               read(ibin) pstr%bridge%groundFrictionType
               read(ibin) pstr%bridge%bedFriction
               read(ibin) pstr%bridge%groundFriction
               read(ibin) pstr%bridge%length
               read(ibin) pstr%bridge%inletlosscoeff
               read(ibin) pstr%bridge%outletlosscoeff
            
            case(ST_GENERAL_ST)
               allocate(pstr%generalst)
               read(ibin) pstr%generalst%wu1
               read(ibin) pstr%generalst%zu1
               read(ibin) pstr%generalst%wu2
               read(ibin) pstr%generalst%zu2
               read(ibin) pstr%generalst%ws
               read(ibin) pstr%generalst%zs
               read(ibin) pstr%generalst%wd1
               read(ibin) pstr%generalst%zd1
               read(ibin) pstr%generalst%wd2
               read(ibin) pstr%generalst%zd2
               read(ibin) pstr%generalst%gateloweredgelevel
               read(ibin) pstr%generalst%cgf_pos
               read(ibin) pstr%generalst%cgd_pos
               read(ibin) pstr%generalst%cwf_pos
               read(ibin) pstr%generalst%cwd_pos
               read(ibin) pstr%generalst%mugf_pos
               read(ibin) pstr%generalst%cgf_neg
               read(ibin) pstr%generalst%cgd_neg
               read(ibin) pstr%generalst%cwf_pos
               read(ibin) pstr%generalst%cwd_neg
               read(ibin) pstr%generalst%mugf_neg
               read(ibin) pstr%generalst%extraresistance
            
            case(ST_EXTRA_RES)
               allocate(pstr%extrares)
               read(ibin) pstr%extrares%erType
               call read_table_cache(ibin, pstr%extrares%values)
            
         end select
      
      enddo
 
      call read_hash_list_cache(ibin, network%sts%hashlist_weir)
      call read_hash_list_cache(ibin, network%sts%hashlist_culvert)
      call read_hash_list_cache(ibin, network%sts%hashlist_bridge)
      call read_hash_list_cache(ibin, network%sts%hashlist_pump)

      ! Allocate and Initialize Restart Data, will be filled when Reading Restart File
      if (network%sts%count > 0) then
         allocate(network%sts%restartData(network%sts%count, CFiHighestParameter))
         network%sts%restartData = missingValue
      endif
      
   end subroutine read_structure_cache

   subroutine write_structure_cache(ibin, sts)
   
      type(t_structureSet), intent(in)    :: sts
      integer, intent(in)                 :: ibin
      
      integer                    :: i
      integer                    :: j
      type(t_structure), pointer :: pstr

      write(ibin) sts%Count

      write(ibin) (sts%countByType(i), i = 1, ST_MAX_TYPE)
      
      write(ibin) sts%compoundCount
      write(ibin) sts%hasExtraResistance

      do i = 1, sts%Count
      
         pstr => sts%struct(i)
         
         write(ibin) pstr%id
         write(ibin) pstr%name
         write(ibin) pstr%type
         write(ibin) pstr%ibran
         write(ibin) pstr%linknumbers(1)
         write(ibin) pstr%xCoordinates(1)
         write(ibin) pstr%yCoordinates(1)
         write(ibin) pstr%chainage
         write(ibin) pstr%compound
         write(ibin) pstr%compoundName
         
         select case(pstr%type)
            case(ST_WEIR)
               write(ibin) pstr%weir%allowedflowdir
               write(ibin) pstr%weir%crestlevel
               write(ibin) pstr%weir%crestwidth
               write(ibin) pstr%weir%dischargecoeff
               write(ibin) pstr%weir%latdiscoeff
               write(ibin) pstr%weir%crestlevel
               write(ibin) pstr%weir%cmu
               
            case(ST_ORIFICE)
               write(ibin) pstr%orifice%allowedflowdir
               write(ibin) pstr%orifice%crestlevel
               write(ibin) pstr%orifice%crestwidth
               write(ibin) pstr%orifice%contrcoeff
               write(ibin) pstr%orifice%latcontrcoeff
               write(ibin) pstr%orifice%openlevel
               write(ibin) pstr%orifice%uselimitflowpos
               write(ibin) pstr%orifice%limitflowpos
               write(ibin) pstr%orifice%uselimitflowneg
               write(ibin) pstr%orifice%limitflowneg
               
            case(ST_PUMP)
               write(ibin) pstr%pump%direction
               write(ibin) pstr%pump%nrstages
            
               write(ibin) (pstr%pump%capacity(j), j = 1, pstr%pump%nrstages)
               write(ibin) (pstr%pump%ss_onlevel(j), j = 1, pstr%pump%nrstages)
               write(ibin) (pstr%pump%ss_offlevel(j), j = 1, pstr%pump%nrstages)
               write(ibin) (pstr%pump%ds_onlevel(j), j = 1, pstr%pump%nrstages)
               write(ibin) (pstr%pump%ds_offlevel(j), j = 1, pstr%pump%nrstages)
               write(ibin) (pstr%pump%ss_trigger(j), j = 1, pstr%pump%nrstages)
               write(ibin) (pstr%pump%ds_trigger(j), j = 1, pstr%pump%nrstages)
            
               call write_table_cache(ibin, pstr%pump%reducfact)
            
            case(ST_CULVERT, ST_SIPHON, ST_INV_SIPHON)
               write(ibin) pstr%culvert%culvertType
               write(ibin) pstr%culvert%leftlevel
               write(ibin) pstr%culvert%rightlevel
               write(ibin) pstr%culvert%crosssectionnr      
               write(ibin) pstr%culvert%allowedflowdir
               write(ibin) pstr%culvert%length
               write(ibin) pstr%culvert%inletlosscoeff
               write(ibin) pstr%culvert%outletlosscoeff
               write(ibin) pstr%culvert%has_valve
               if (pstr%culvert%has_valve) then
                  write(ibin) pstr%culvert%inivalveopen
                  call write_table_cache(ibin, pstr%culvert%losscoeff)
               endif
      
               ! Bend Loss for Siphons
               write(ibin) pstr%culvert%bendlosscoeff

               ! Levels for Normal Siphon
               write(ibin) pstr%culvert%turnonlevel
               write(ibin) pstr%culvert%turnofflevel
               
            case(ST_UNI_WEIR)
               write(ibin) pstr%uniweir%allowedflowdir
               write(ibin) pstr%uniweir%crestlevel
               write(ibin) pstr%uniweir%dischargecoeff
               write(ibin) pstr%uniweir%freesubmergedfactor
            
               write(ibin) pstr%uniweir%yzcount
               write(ibin) (pstr%uniweir%y(j), j = 1, pstr%uniweir%yzcount)
               write(ibin) (pstr%uniweir%z(j), j = 1, pstr%uniweir%yzcount)
               
            case(ST_BRIDGE)
               write(ibin) pstr%bridge%allowedflowdir
               write(ibin) pstr%bridge%bedLevel
               write(ibin) pstr%bridge%pillarwidth
               write(ibin) pstr%bridge%formfactor
               write(ibin) pstr%bridge%useOwnCrossSection
               write(ibin) pstr%bridge%crosssectionnr
               write(ibin) pstr%bridge%bedFrictionType
               write(ibin) pstr%bridge%groundFrictionType
               write(ibin) pstr%bridge%bedFriction
               write(ibin) pstr%bridge%groundFriction
               write(ibin) pstr%bridge%length
               write(ibin) pstr%bridge%inletlosscoeff
               write(ibin) pstr%bridge%outletlosscoeff
            
            case(ST_GENERAL_ST)
               write(ibin) pstr%generalst%wu1
               write(ibin) pstr%generalst%zu1
               write(ibin) pstr%generalst%wu2
               write(ibin) pstr%generalst%zu2
               write(ibin) pstr%generalst%ws
               write(ibin) pstr%generalst%zs
               write(ibin) pstr%generalst%wd1
               write(ibin) pstr%generalst%zd1
               write(ibin) pstr%generalst%wd2
               write(ibin) pstr%generalst%zd2
               write(ibin) pstr%generalst%gateLowerEdgeLevel
               write(ibin) pstr%generalst%cgf_pos
               write(ibin) pstr%generalst%cgd_pos
               write(ibin) pstr%generalst%cwf_pos
               write(ibin) pstr%generalst%cwd_pos
               write(ibin) pstr%generalst%mugf_pos
               write(ibin) pstr%generalst%cgf_neg
               write(ibin) pstr%generalst%cgd_neg
               write(ibin) pstr%generalst%cwf_neg
               write(ibin) pstr%generalst%cwd_neg
               write(ibin) pstr%generalst%mugf_neg
               write(ibin) pstr%generalst%extraresistance

            case(ST_EXTRA_RES)
               write(ibin) pstr%extrares%erType
               call write_table_cache(ibin, pstr%extrares%values)
            
         end select
      
      enddo
 
      call write_hash_list_cache(ibin, sts%hashlist_weir)
      call write_hash_list_cache(ibin, sts%hashlist_culvert)
      call write_hash_list_cache(ibin, sts%hashlist_bridge)
      call write_hash_list_cache(ibin, sts%hashlist_pump)
      
   end subroutine write_structure_cache

   
   subroutine readWeir(weir, md_ptr, success)
   
      type(t_weir), pointer, intent(inout)     :: weir
      type(tree_data), pointer, intent(in)     :: md_ptr
      logical, intent(inout)                   :: success 

      allocate(weir)

      call prop_get_double(md_ptr, 'structure', 'crestlevel', weir%crestlevel, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'crestwidth', weir%crestwidth, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'dischargecoeff', weir%dischargecoeff, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'latdiscoeff', weir%latdiscoeff, success)

      weir%cmu            = weir%dischargecoeff * weir%latdiscoeff

      if (success) call prop_get_integer(md_ptr, 'structure', 'allowedflowdir', weir%allowedflowdir, success)

   end subroutine readWeir
   
   subroutine readUniversalWeir(uniweir, md_ptr, success)
   
      type(t_uni_weir), pointer, intent(inout)     :: uniweir
      type(tree_data), pointer, intent(in)         :: md_ptr
      logical, intent(inout)                       :: success
      
      integer                                      :: istat
      integer                                      :: i

      allocate(uniweir)
      
      call prop_get_double(md_ptr, 'structure', 'crestlevel', uniweir%crestlevel, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'dischargecoeff', uniweir%dischargecoeff, success)
      if (success) call prop_get_integer(md_ptr, 'structure', 'allowedflowdir', uniweir%allowedflowdir, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'freesubmergedfactor', uniweir%freesubmergedfactor, success)
      if (.not. success) uniweir%freesubmergedfactor = 0.667d0
      
      call prop_get_integer(md_ptr, 'structure', 'numLevels', uniweir%yzcount, success) ! UNST-2714: new consistent keyword
      if (.not. success) return
      
      call realloc(uniweir%y, uniweir%yzcount, stat=istat)
      if (istat == 0) call realloc(uniweir%z, uniweir%yzcount, stat=istat)
      if (istat .ne. 0) then
         call SetMessage(LEVEL_FATAL, 'Reading Universal Weir: Error Allocating Y/Z Arrays')
      endif

      call prop_get_doubles(md_ptr, 'structure', 'yValues', uniweir%y, uniweir%yzcount, success)
      if (success) call prop_get_doubles(md_ptr, 'structure', 'zValues', uniweir%z, uniweir%yzcount, success)
      if (.not. success) return
      
      do i = 1, uniweir%yzcount
         uniweir%z(i) = uniweir%z(i) - uniweir%crestlevel
      enddo

   end subroutine readUniversalWeir
   
   subroutine readCulvert(network, istru, md_ptr, success)
   
      type(t_network), intent(inout)              :: network
      integer                                     :: istru
      type(tree_data), pointer, intent(in)        :: md_ptr
      logical, intent(inout)                      :: success 
      
      type(t_culvert), pointer                    :: culvert
      character(len=IdLen)                        :: CrsDefID
      integer                                     :: CrsDefIndx
      integer                                     :: icross
      
      integer                                     :: bedFrictionType
      double precision                            :: bedFriction
      integer                                     :: groundFrictionType
      double precision                            :: groundFriction
      integer                                     :: valveonoff
      
      integer                                     :: lossCoeffCount
      integer                                     :: istat
      double precision, allocatable, dimension(:) :: relOpen
      double precision, allocatable, dimension(:) :: lossCoeff

      allocate(network%sts%struct(istru)%culvert)
      
      culvert => network%sts%struct(istru)%culvert

      call prop_get_string(md_ptr, 'structure', 'csDefId', CrsDefID, success)
      if (.not. success) return
         
      CrsDefIndx = hashsearch(network%CSDefinitions%hashlist, CrsDefID)
      if (CrsDefIndx <= 0) then
         call setMessage(LEVEL_FATAL, 'Error Reading Culvert: Cross-Section Definition not Found')
      endif

      call prop_get_integer(md_ptr, 'structure', 'bedFrictionType', bedFrictionType, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'bedFriction', bedFriction, success)
      if (success) call prop_get_integer(md_ptr, 'structure', 'groundFrictionType', groundFrictionType, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'groundFriction', groundFriction, success)
      if (.not. success) return
         
      icross = AddCrossSection(network%crs, network%CSDefinitions, 0, 0.0d0, CrsDefIndx, 0.0d0, &
                               bedFrictionType, bedFriction, groundFrictionType, groundFriction)
      network%crs%cross(icross)%branchid = -1
         
      culvert%pcross         => network%crs%cross(icross)
      culvert%crosssectionnr = icross
      
      culvert%culvertType    = ST_CULVERT
      
      call prop_get_integer(md_ptr, 'structure', 'allowedflowdir', culvert%allowedflowdir, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'length', culvert%length, success) 
      if (success) call prop_get_double(md_ptr, 'structure', 'leftlevel', culvert%leftlevel, success) 
      if (success) call prop_get_double(md_ptr, 'structure', 'rightlevel', culvert%rightlevel, success) 
      if (success) call prop_get_double(md_ptr, 'structure', 'inletlosscoeff', culvert%inletlosscoeff, success) 
      if (success) call prop_get_double(md_ptr, 'structure', 'outletlosscoeff', culvert%outletlosscoeff, success) 

      call prop_get_integer(md_ptr, 'structure', 'valveonoff', valveonoff, success)
      if (.not. success) return
      
      if (valveonoff == 1) then
         
         culvert%has_valve = .true.
         
         call prop_get_double(md_ptr, 'structure', 'inivalveopen', culvert%inivalveopen, success)

         if (success) call prop_get_integer(md_ptr, 'structure', 'numLossCoeff',   lossCoeffCount, success) ! UNST-2710: new consistent keyword
         if (.not. success) return
            
         call realloc(relOpen, lossCoeffCount, stat=istat)
         if (istat == 0) call realloc(lossCoeff, lossCoeffCount, stat=istat)
         if (istat .ne. 0) then
            call SetMessage(LEVEL_FATAL, 'Reading Culvert: Error Allocating Valve Loss Arrays')
         endif

         call prop_get_doubles(md_ptr, 'structure', 'relativeOpening', relOpen, lossCoeffCount, success)
         if (success) call prop_get_doubles(md_ptr, 'structure', 'lossCoefficient', lossCoeff, lossCoeffCount, success)
         if (.not. success) return
      
         call setTable(culvert%lossCoeff, 0, relOpen, lossCoeff, lossCoeffCount)

         ! Clear Valve Loss Arrays
         istat = 0
         if (allocated(relOpen)) deallocate(relOpen, stat=istat)
         if (istat == 0 .and. allocated(lossCoeff)) deallocate(lossCoeff, stat=istat)
         if (istat .ne. 0) then
            call SetMessage(LEVEL_FATAL, 'Reading Culvert: Error Deallocating Valve Loss Arrays')
         endif
         
      else
         
         culvert%has_valve = .false.
         culvert%inivalveopen = 0.0d0
         
      endif
   
   end subroutine readCulvert
   
   subroutine readSiphon(network, istru, md_ptr, isInvertedSiphon, success)
   
      type(t_network), intent(inout)              :: network
      integer                                     :: istru
      type(tree_data), pointer, intent(in)        :: md_ptr
      logical                                     :: isInvertedSiphon
      logical, intent(inout)                      :: success 

      type(t_culvert), pointer                    :: culvert

      character(len=IdLen)                        :: CrsDefID
      integer                                     :: CrsDefIndx
      integer                                     :: icross

      integer                                     :: bedFrictionType
      double precision                            :: bedFriction
      integer                                     :: groundFrictionType
      double precision                            :: groundFriction
      integer                                     :: valveonoff
      
      integer                                     :: allowedflowdir
      double precision                            :: length
      double precision                            :: leftlevel
      double precision                            :: rightlevel
      double precision                            :: inletlosscoeff
      double precision                            :: outletlosscoeff
      double precision                            :: bendLossCoeff

      integer                                     :: lossCoeffCount
      integer                                     :: istat
      logical                                     :: has_valve
      double precision                            :: inivalveopen
      double precision, allocatable, dimension(:) :: relOpen
      double precision, allocatable, dimension(:) :: lossCoeff

      call prop_get_string(md_ptr, 'structure', 'csDefId', CrsDefID, success)
      if (.not. success) return
         
      CrsDefIndx = hashsearch(network%CSDefinitions%hashlist, CrsDefID)
      if (CrsDefIndx <= 0) then
         call setMessage(LEVEL_FATAL, 'Error Reading Culvert: Cross-Section Definition not Found')
      endif

      call prop_get_integer(md_ptr, 'structure', 'bedFrictionType', bedFrictionType, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'bedFriction', bedFriction, success)
      if (success) call prop_get_integer(md_ptr, 'structure', 'groundFrictionType', groundFrictionType, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'groundFriction', groundFriction, success)
      if (.not. success) return
         
      icross = AddCrossSection(network%crs, network%CSDefinitions, 0, 0.0d0, CrsDefIndx, 0.0d0, &
                               bedFrictionType, bedFriction, groundFrictionType, groundFriction)
      network%crs%cross(icross)%branchid = -1
      
      call prop_get_integer(md_ptr, 'structure', 'allowedflowdir', allowedflowdir, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'length', length, success) 
      if (success) call prop_get_double(md_ptr, 'structure', 'leftlevel', leftlevel, success) 
      if (success) call prop_get_double(md_ptr, 'structure', 'rightlevel', rightlevel, success) 
      if (success) call prop_get_double(md_ptr, 'structure', 'inletlosscoeff', inletlosscoeff, success) 
      if (success) call prop_get_double(md_ptr, 'structure', 'outletlosscoeff', outletlosscoeff, success) 
      if (success) call prop_get_double(md_ptr, 'structure', 'bendLosscoeff', bendLossCoeff, success) 

      if (success) call prop_get_integer(md_ptr, 'structure', 'valveonoff', valveonoff, success)
      if (.not. success) return
      
      if (valveonoff == 1) then
         
         has_valve = .true.
         
         call prop_get_double(md_ptr, 'structure', 'inivalveopen', inivalveopen, success)

         if (success) call prop_get_integer(md_ptr, 'structure', 'lossCoeffCount', lossCoeffCount, success)
         if (.not. success) return
            
         call realloc(relOpen, lossCoeffCount, stat=istat)
         if (istat == 0) call realloc(lossCoeff, lossCoeffCount, stat=istat)
         if (istat .ne. 0) then
            call SetMessage(LEVEL_FATAL, 'Reading Siphon: Error Allocating Valve Loss Arrays')
         endif

         call prop_get_doubles(md_ptr, 'structure', 'relativeOpening', relOpen, lossCoeffCount, success)
         if (success) call prop_get_doubles(md_ptr, 'structure', 'lossCoefficient', lossCoeff, lossCoeffCount, success)
         if (.not. success) return
      
      else
         
         has_valve = .false.
         inivalveopen = 0.0d0
         
      endif

      allocate(network%sts%struct(istru)%culvert)
      culvert => network%sts%struct(istru)%culvert

      culvert%pcross          => network%crs%cross(icross)
      culvert%crosssectionnr  =  icross

      culvert%leftlevel       =  leftlevel
      culvert%rightlevel      =  rightlevel
      culvert%allowedflowdir  =  allowedflowdir
      culvert%length          =  length
      culvert%inletlosscoeff  =  inletlosscoeff
      culvert%outletlosscoeff =  outletlosscoeff
      culvert%bendlosscoeff   =  bendlosscoeff
      culvert%has_valve       =  has_valve
      culvert%inivalveopen    =  inivalveopen
      
      if (has_valve) then
         call setTable(culvert%lossCoeff, 0, relOpen, lossCoeff, lossCoeffCount)
      endif

      if (.not. isInvertedSiphon) then
         
         culvert%culvertType =  ST_SIPHON
         
         call prop_get_double(md_ptr, 'structure', 'turnonlevel', culvert%turnonlevel, success) 
         if (success) call prop_get_double(md_ptr, 'structure', 'turnofflevel', culvert%turnofflevel, success)
         if (.not. success) return
         
         culvert%is_siphon_on = .false.

      else
         culvert%culvertType =  ST_INV_SIPHON
      endif
      
      ! Clear Valve Loss Arrays
      istat = 0
      if (allocated(relOpen)) deallocate(relOpen, stat=istat)
      if (istat == 0 .and. allocated(lossCoeff)) deallocate(lossCoeff, stat=istat)
      if (istat .ne. 0) then
         call SetMessage(LEVEL_FATAL, 'Reading Siphon: Error Deallocating Valve Loss Arrays')
      endif
         
   end subroutine readSiphon
   
   subroutine readBridge(network, istru, md_ptr, isPillarBridge, success)
   
      type(t_network), intent(inout)             :: network
      integer                                    :: istru
      type(tree_data), pointer, intent(in)       :: md_ptr
      logical, intent(in)                        :: isPillarBridge 
      logical, intent(inout)                     :: success 

      type(t_bridge), pointer                    :: bridge
      character(len=IdLen)                       :: CrsDefID
      integer                                    :: CrsDefIndx
      integer                                    :: icross
      
      allocate(network%sts%struct(istru)%bridge)
      
      bridge => network%sts%struct(istru)%bridge
      
      call prop_get_integer(md_ptr, 'structure', 'allowedflowdir', bridge%allowedflowdir, success)
      if (.not. success) return

      if (isPillarBridge) then
         
         call prop_get_double(md_ptr, 'structure', 'pillarwidth', bridge%pillarwidth, success)
         if (success) call prop_get_double(md_ptr, 'structure', 'formfactor', bridge%formfactor, success)
         if (.not. success) return
      
         bridge%bedLevel           = 0.0d0
         bridge%useOwnCrossSection = .false.
         bridge%pcross             => null()
         bridge%crosssectionnr     = 0
         bridge%bedFrictionType    = 0
         bridge%groundFrictionType = 0
         bridge%bedFriction        = 0.0d0
         bridge%groundFriction     = 0.0d0
         bridge%length             = 0.0d0
         bridge%inletlosscoeff     = 0.0d0
         bridge%outletlosscoeff    = 0.0d0
         
      else
         
         call prop_get_string(md_ptr, 'structure', 'csDefId', CrsDefID, success)
         if (.not. success) return
         
         CrsDefIndx = hashsearch(network%CSDefinitions%hashlist, CrsDefID)
         if (CrsDefIndx <= 0) then
            call setMessage(LEVEL_FATAL, 'Error Reading Bridge: Cross-Section Definition not Found')
         endif

         call prop_get_integer(md_ptr, 'structure', 'bedFrictionType', bridge%bedFrictionType, success)
         if (success) call prop_get_double(md_ptr, 'structure', 'bedFriction', bridge%bedFriction, success)
         if (success) call prop_get_integer(md_ptr, 'structure', 'groundFrictionType', bridge%groundFrictionType, success)
         if (success) call prop_get_double(md_ptr, 'structure', 'groundFriction', bridge%groundFriction, success)
         if (.not. success) return
         
         icross = AddCrossSection(network%crs, network%CSDefinitions, 0, 0.0d0, CrsDefIndx, 0.0d0, &
                                  bridge%bedFrictionType, bridge%bedFriction, bridge%groundFrictionType, bridge%groundFriction)
         network%crs%cross(icross)%branchid = -1
         
         bridge%useOwnCrossSection = .true.
         bridge%pcross             => network%crs%cross(icross)
         bridge%crosssectionnr     = icross

         call prop_get_double(md_ptr, 'structure', 'bedlevel', bridge%bedLevel, success)
         if (success) call prop_get_double(md_ptr, 'structure', 'length', bridge%length, success)
         if (success) call prop_get_double(md_ptr, 'structure', 'inletlosscoeff', bridge%inletlosscoeff, success)
         if (success) call prop_get_double(md_ptr, 'structure', 'outletlosscoeff', bridge%outletlosscoeff, success)
         if (.not. success) return
         
         bridge%pillarwidth = 0.0d0
         bridge%formfactor  = 0.0d0

      endif
      
   end subroutine readBridge
   
   subroutine readDambreak(dambr, md_ptr, success)

      type(t_dambreak), pointer, intent(inout) :: dambr      
      type(tree_data), pointer, intent(in)     :: md_ptr
      logical, intent(inout)                   :: success
      
      logical :: localsuccess

      allocate(dambr)

      call prop_get_double(md_ptr, 'Structure', 'StartLocationX',  dambr%startLocationX, success)
      if (.not. success) return

      call prop_get_double(md_ptr, 'Structure', 'StartLocationY',  dambr%startLocationY, success)
      if (.not. success) return

      call prop_get_integer(md_ptr, 'Structure', 'Algorithm', dambr%algorithm, success)
      if (.not. success) return

      call prop_get_double(md_ptr, 'Structure', 'CrestLevelIni', dambr%crestLevelIni, success)
      if (.not. success) return
         
      if (dambr%algorithm == 2) then
         
         call prop_get_double(md_ptr, 'Structure', 'BreachWidthIni', dambr%breachWidthIni, success)
         if (.not. success) return

         call prop_get_double(md_ptr, 'Structure', 'CrestLevelMin', dambr%crestLevelMin, success)
         if (.not. success) return

         call prop_get_double(md_ptr, 'Structure', 'TimeToBreachToMaximumDepth', dambr%timeToBreachToMaximumDepth, success)
         if (.not. success) return

         call prop_get_double(md_ptr, 'Structure', 'F1', dambr%f1, success)
         if (.not. success) return

         call prop_get_double(md_ptr, 'Structure', 'F2', dambr%f2, success)
         if (.not. success) return

         call prop_get_double(md_ptr, 'Structure', 'Ucrit', dambr%ucrit, success)
         if (.not. success) return
         
         ! optional extra fields
         call prop_get_double(md_ptr, 'Structure', 'WaterLevelUpstreamLocationX', dambr%waterLevelUpstreamLocationX, localsuccess)
         
         call prop_get_double(md_ptr, 'Structure', 'WaterLevelUpstreamLocationY', dambr%waterLevelUpstreamLocationY, localsuccess)

         call prop_get_double(md_ptr, 'Structure', 'WaterLevelDownstreamLocationX', dambr%waterLevelDownstreamLocationX, localsuccess)
         
         call prop_get_double(md_ptr, 'Structure', 'WaterLevelDownstreamLocationY', dambr%waterLevelDownstreamLocationY, localsuccess)
         
      endif
      
      ! get the name of the tim file 
      if (dambr%algorithm == 3) then
         call prop_get_string(md_ptr, 'Structure', 'DambreakLevelsAndWidths', dambr%levelsAndWidths, success)
         if (.not. success) return         
      endif

      call prop_get_double(md_ptr, 'Structure', 'T0', dambr%t0, success)
      if (.not. success) return
      
      call setCoefficents(dambr)
      
   end subroutine

   subroutine readPump(pump, md_ptr, st_id, forcinglist, success)
   
      type(t_pump), pointer,        intent(inout) :: pump        !< The pump structure to be read into.
      type(tree_data), pointer,     intent(in   ) :: md_ptr      !< ini tree pointer with user input.
      character(IdLen),             intent(in   ) :: st_id       !< Structure character Id.
      type(t_forcinglist),          intent(inout) :: forcinglist !< List of all (structure) forcing parameters, to which pump forcing will be added if needed.
      logical,                      intent(inout) :: success
      
      integer                                      :: tabsize
      integer                                      :: istat
      double precision, allocatable, dimension(:)  :: head   
      double precision, allocatable, dimension(:)  :: redfac   
      character(CharLn) :: tmpstr
      
      
      allocate(pump)

      call prop_get_integer(md_ptr, 'structure', 'direction', pump%direction, success)
      if (success) call prop_get_integer(md_ptr, 'structure', 'numStages', pump%nrstages, success) ! UNST-2709: new consistent keyword
      if (.not. success) then
         pump%nrstages = 1
         success = .true.
      endif
      
      if (pump%nrstages < 1) then
         call setMessage(LEVEL_FATAL, "Error Reading Pump: No Stages Defined")
      endif

      allocate(pump%capacity(pump%nrstages), stat=istat)
      if (istat == 0) allocate(pump%ss_onlevel(pump%nrstages), stat=istat)
      if (istat == 0) allocate(pump%ss_offlevel(pump%nrstages), stat=istat)
      if (istat == 0) allocate(pump%ds_onlevel(pump%nrstages), stat=istat)
      if (istat == 0) allocate(pump%ds_offlevel(pump%nrstages), stat=istat)
      if (istat == 0) allocate(pump%ss_trigger(pump%nrstages), stat=istat)
      if (istat == 0) allocate(pump%ds_trigger(pump%nrstages), stat=istat)

      
      if (pump%nrstages == 1) then
         ! In case of only 1 stage, capacity is either scalar double, or filename, or 'realtime'.
         call get_value_or_addto_forcinglist(md_ptr, 'capacity', pump%capacity(1), st_id, ST_PUMP, forcinglist, success)
         ! addtimespace gaat qid='pump' gebruiken, de bc provider moet via FM juiste name doorkrijgen (==structureid)
         
      else
         ! Multiple stages: only support table with double precision values.
         call prop_get_doubles(md_ptr, 'structure', 'capacity', pump%capacity, pump%nrstages, success)     
      end if
      if (iabs(pump%direction) == 1 .or. iabs(pump%direction) == 3) then
         if (success) call prop_get_doubles(md_ptr, 'structure', 'startlevelsuctionside', pump%ss_onlevel, pump%nrstages, success)      
         if (success) call prop_get_doubles(md_ptr, 'structure', 'stoplevelsuctionside', pump%ss_offlevel, pump%nrstages, success)      
      endif
      
      if (iabs(pump%direction) == 2 .or. iabs(pump%direction) == 3) then
         if (success) call prop_get_doubles(md_ptr, 'structure', 'startleveldeliveryside', pump%ds_onlevel, pump%nrstages, success)      
         if (success) call prop_get_doubles(md_ptr, 'structure', 'stopleveldeliveryside', pump%ds_offlevel, pump%nrstages, success)
      endif
      
      if (.not. success) return

      pump%ss_trigger = .true.
      pump%ds_trigger = .true.

      ! Reduction Table
      call prop_get_integer(md_ptr, 'structure', 'numReductionLevels', tabsize, success) ! UNST-2709: new consistent keyword
      if (.not. success .or. tabsize < 1) then
         tabsize = 1
      endif
      
      call realloc(head, tabsize, stat=istat)
      if (istat == 0) call realloc(redfac, tabsize, stat=istat)
      if (istat .ne. 0) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Pump: Error Allocating Reduction Factor Arrays')
      endif

      call prop_get_doubles(md_ptr, 'structure', 'head', head, tabsize, success)
      if (success)call prop_get_doubles(md_ptr, 'structure', 'reductionfactor', redfac, tabsize, success)
      if (.not. success .and. tabsize == 1) then
         head(1)   = 0.0d0
         redfac(1) = 1.0d0
         success = .true.
      elseif (.not. success) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Pump: No Proper Reduction Table')
      endif
      
      call setTable(pump%reducfact, 0, head, redfac, tabsize)

      ! Clear Arrays
      istat = 0
      if (allocated(head)) deallocate(head, stat=istat)
      if (istat == 0 .and. allocated(redfac)) deallocate(redfac, stat=istat)
      if (istat .ne. 0) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Pump: Error Dellocating Reduction Factor Arrays')
      endif
      
      ! Initialize Parameters for Pump
      pump%actual_stage      = 0
      pump%capacitySetpoint  = 0.0d0
      pump%computed_capacity = 0.0d0
      pump%discharge         = 0.0d0
      pump%is_active         = .true.
      pump%isControlled      = .false.
      pump%pump_head         = 0.0d0
      pump%reduction_factor  = 1.0d0
      pump%ss_level          = 0.0d0
      pump%ds_level          = 0.0d0
      pump%stage_capacity    = 0.0d0
      
   end subroutine readPump
   
   !> Either retrieve a constant value for parameter KEY, or get the filename for the time series
   subroutine get_value_or_addto_forcinglist(md_ptr, key, value, st_id, st_type, forcinglist, success)
      type(tree_data), pointer,     intent(in   ) :: md_ptr      !< ini tree pointer with user input.
      character(IdLen),             intent(in   ) :: st_id       !< Structure character Id.
      type(t_forcinglist),          intent(inout) :: forcinglist !< List of all (structure) forcing parameters, to which pump forcing will be added if needed.
      logical,                      intent(inout) :: success     
      double precision, target,     intent(  out) :: value       !< constant value from input
      integer,                      intent(in   ) :: st_type     !< structure type
      character(len=*),             intent(in   ) :: key         !< name of the item in the input file
      
      integer                                     :: tabsize
      integer                                     :: istat
      double precision, allocatable, dimension(:) :: head   
      double precision, allocatable, dimension(:) :: redfac   
      character(CharLn) :: tmpstr
      
      
      call prop_get_double(md_ptr, 'structure', key, value, success)
      if (.not. success) then
         call prop_get_string(md_ptr, 'structure', key, tmpstr, success)
         if (success) then
            forcinglist%Count = forcinglist%Count+1
            if (forcinglist%Count > forcinglist%Size) then
               call realloc(forcinglist)
            end if
            forcinglist%forcing(forcinglist%Count)%st_id      = st_id
            forcinglist%forcing(forcinglist%Count)%st_type    = ST_PUMP
            forcinglist%forcing(forcinglist%Count)%param_name = key
            forcinglist%forcing(forcinglist%Count)%targetptr  => value
            forcinglist%forcing(forcinglist%Count)%filename   = tmpstr
         end if
      endif
      

   end subroutine get_value_or_addto_forcinglist
   
   subroutine readOrifice(orifice, md_ptr, success)
   
      type(t_orifice), pointer, intent(inout)     :: orifice
      type(tree_data), pointer, intent(in)        :: md_ptr
      logical, intent(inout)                      :: success 
      
      double precision :: area, height
   
      allocate(orifice)
      
      call prop_get_double(md_ptr, 'structure', 'crestlevel', orifice%crestlevel, success)
      if (success) then
         call prop_get_double(md_ptr, 'structure', 'crestwidth', orifice%crestwidth, success)
         if (success) call prop_get_double(md_ptr, 'structure', 'openlevel', orifice%openlevel, success)
      else
         success = .true.
         orifice%crestlevel = 0d0
         area = 0d0
         call prop_get_double(md_ptr, 'structure', 'bottomlevel', orifice%crestlevel, success)
         call prop_get_double(md_ptr, 'structure', 'area',         area, success)
         height = sqrt(area)
         orifice%crestwidth = height
         orifice%openlevel = orifice%crestlevel+height
      endif
      
      if (success) call prop_get_integer(md_ptr, 'structure', 'allowedflowdir', orifice%allowedflowdir, success)

      if (success) call prop_get_double(md_ptr, 'structure', 'contractioncoeff', orifice%contrcoeff, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'latcontrcoeff', orifice%latcontrcoeff, success)

      if (success) then
         call prop_get_logical(md_ptr, 'structure', 'uselimitflowpos', orifice%uselimitflowpos, success)
         if (success) then
            call prop_get_double(md_ptr, 'structure', 'limitflowpos', orifice%limitflowpos, success)
         else
            orifice%uselimitflowpos = .false.
            success = .true.
         endif
      endif
      
      if (success) then
         call prop_get_logical(md_ptr, 'structure', 'uselimitflowneg', orifice%uselimitflowneg, success)
         if (success) then
            call prop_get_double(md_ptr, 'structure', 'limitflowneg', orifice%limitflowneg, success)
         else
            orifice%uselimitflowneg = .false.
            success = .true.
         endif
      endif
   
   end subroutine readOrifice

   !> Read the weir parameters and define a general structure
   subroutine readWeirAsGenStru(generalst, md_ptr, st_id, forcinglist, success)
   
      use messageHandling
      
      type(t_GeneralStructure), pointer,  intent(inout) :: generalst   !< general structure to be read into 
      type(tree_data), pointer,           intent(in   ) :: md_ptr      !< ini tree pointer with user input.
      logical,                            intent(inout) :: success     !< logical indicating, the reading of the structure was successfull
      character(IdLen),                   intent(in   ) :: st_id       !< Structure character Id.
      type(t_forcinglist),                intent(inout) :: forcinglist !< List of all (structure) forcing parameters, to which pump forcing will be added if needed.
      
      character(len=Idlen) :: dirstring

      allocate(generalst)

      generalst%velheight = .true.
      if (success) call get_value_or_addto_forcinglist(md_ptr, 'crestWidth', generalst%ws, st_id, ST_GENERAL_ST, forcinglist, success)
      if (success) call get_value_or_addto_forcinglist(md_ptr, 'crestLevel', generalst%zs, st_id, ST_GENERAL_ST, forcinglist, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'corrCoeff',  generalst%mugf_pos, success)

      generalst%wu1                = generalst%ws
      generalst%zu1                = generalst%zs
      generalst%wu2                = generalst%ws
      generalst%zu2                = generalst%zs
      generalst%zs                 = generalst%zs
      generalst%wd1                = generalst%ws
      generalst%zd1                = generalst%zs
      generalst%wd2                = generalst%ws
      generalst%zd2                = generalst%zs
      generalst%gateLowerEdgeLevel = huge(1d0)
      generalst%cgf_pos            = 1d0
      generalst%cgd_pos            = 1d0
      generalst%cwf_pos            = 1d0
      generalst%cwd_pos            = 1d0
      generalst%cgf_neg            = 1d0
      generalst%cgd_neg            = 1d0
      generalst%cwf_neg            = 1d0
      generalst%cwd_neg            = 1d0
      generalst%mugf_neg           = generalst%mugf_pos
      generalst%extraresistance    = 0d0
      generalst%gatedoorheight     = huge(1d0)
      generalst%gateopeningwidth   = generalst%ws
      generalst%crestlength        = 0d0
      generalst%velheight          = .true.
      generalst%openingDirection   = GEN_SYMMETRIC

   end subroutine readWeirAsGenStru
 
   !> Read the orifice or gate parameters and define a general structure
   subroutine readOrificeAsGenStru(generalst, md_ptr, st_id, forcinglist, success)
   
      use messageHandling
      
      type(t_GeneralStructure), pointer,  intent(inout) :: generalst   !< general structure to be read into 
      type(tree_data), pointer,           intent(in   ) :: md_ptr      !< ini tree pointer with user input.
      logical,                            intent(inout) :: success     !< logical indicating, the reading of the structure was successfull
      character(IdLen),                   intent(in   ) :: st_id       !< Structure character Id.
      type(t_forcinglist),                intent(inout) :: forcinglist !< List of all (structure) forcing parameters, to which pump forcing will be added if needed.
      
      character(len=Idlen) :: dirstring
      double precision :: area
      allocate(generalst)

      generalst%velheight = .true.
      if (success) call get_value_or_addto_forcinglist(md_ptr, 'crestLevel', generalst%zs, st_id, ST_GENERAL_ST, forcinglist, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'corrCoeff',  generalst%mugf_pos, success)
      if (success) call get_value_or_addto_forcinglist(md_ptr, 'crestWidth', generalst%ws, st_id, ST_GENERAL_ST, forcinglist, success)
      if (.not. success) then
         call  prop_get_double(md_ptr, 'structure', 'area',  area, success)
         if (success) then
            generalst%ws = sqrt(area)
            generalst%gateLowerEdgeLevel = generalst%ws + generalst%zs
         endif
      else
         call get_value_or_addto_forcinglist(md_ptr, 'gateLowerEdgeLevel', generalst%gateLowerEdgeLevel, st_id, ST_GENERAL_ST, &
                                                       forcinglist, success)
      endif
      
      generalst%wu1                = generalst%ws
      generalst%zu1                = generalst%zs
      generalst%wu2                = generalst%ws
      generalst%zu2                = generalst%zs
      generalst%zs                 = generalst%zs
      generalst%wd1                = generalst%ws
      generalst%zd1                = generalst%zs
      generalst%wd2                = generalst%ws
      generalst%zd2                = generalst%zs
      generalst%cgf_pos            = 1d0
      generalst%cgd_pos            = 1d0
      generalst%cwf_pos            = 1d0
      generalst%cwd_pos            = 1d0
      generalst%cgf_neg            = 1d0
      generalst%cgd_neg            = 1d0
      generalst%cwf_neg            = 1d0
      generalst%cwd_neg            = 1d0
      generalst%mugf_neg           = generalst%mugf_pos
      generalst%extraresistance    = 0d0
      generalst%gatedoorheight     = huge(1d0)
      generalst%gateopeningwidth   = 0d0
      generalst%crestlength        = 0d0
      generalst%velheight          = .true.
      generalst%openingDirection   = GEN_SYMMETRIC

   end subroutine readOrificeAsGenStru
 
   !> Read the general structure parameters
   subroutine readGeneralStructure(generalst, md_ptr, st_id, forcinglist, success)
   
      use messageHandling
      
      type(t_GeneralStructure), pointer,  intent(inout) :: generalst   !< general structure to be read into 
      type(tree_data), pointer,           intent(in   ) :: md_ptr      !< ini tree pointer with user input.
      logical,                            intent(inout) :: success     !< logical indicating, the reading of the structure was successfull
      character(IdLen),                   intent(in   ) :: st_id       !< Structure character Id.
      type(t_forcinglist),                intent(inout) :: forcinglist !< List of all (structure) forcing parameters, to which pump forcing will be added if needed.
      
      character(len=Idlen) :: dirstring

      allocate(generalst)

      generalst%velheight = .true.
      call prop_get_double(md_ptr, 'structure', 'upstream1Width', generalst%wu1, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'upstream2Width',  generalst%wu2, success)
      if (success) call get_value_or_addto_forcinglist(md_ptr, 'crestWidth', generalst%ws, st_id, ST_GENERAL_ST, forcinglist, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'downstream1Width', generalst%wd1, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'downstream2Width',   generalst%wd2, success)
                                                                               
      if (success) call prop_get_double(md_ptr, 'structure', 'upstream1Level',   generalst%zu1, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'upstream2Level',  generalst%zu2, success)
      if (success) call get_value_or_addto_forcinglist(md_ptr, 'crestLevel',    generalst%zs, st_id, ST_GENERAL_ST, forcinglist, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'downstream1Level', generalst%zd1, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'downstream2Level',  generalst%zd2, success)

      if (success) call get_value_or_addto_forcinglist(md_ptr, 'gateLowerEdgeLevel', generalst%gateLowerEdgeLevel, st_id, ST_GENERAL_ST, forcinglist, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'crestLength',   generalst%crestlength, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'gateHeight',   generalst%gatedoorheight, success)
      if (success) call get_value_or_addto_forcinglist(md_ptr, 'gateOpeningWidth', generalst%gateopeningwidth, st_id, ST_GENERAL_ST, forcinglist, success)

      dirString = 'symmetric'
      if (success) call prop_get_string(md_ptr, 'structure', 'gateOpeningHorizontalDirection',   dirString)
      generalst%openingDirection = openingDirectionToInt(dirString)
! TODO add extra parameter in t_GeneralStructure
      
      if (success) call prop_get_double(md_ptr, 'structure', 'posfreegateflowcoeff',  generalst%cgf_pos, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'posdrowngateflowcoeff', generalst%cgd_pos, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'posfreeweirflowcoeff',  generalst%cwf_pos, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'posdrownweirflowcoeff', generalst%cwd_pos, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'poscontrcoeffreegate',  generalst%mugf_pos, success)
      
      if (success) call prop_get_double(md_ptr, 'structure', 'negfreegateflowcoeff',  generalst%cgf_neg, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'negdrowngateflowcoeff', generalst%cgd_neg, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'negfreeweirflowcoeff',  generalst%cwf_neg, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'negdrownweirflowcoeff', generalst%cwd_neg, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'negcontrcoeffreegate',  generalst%mugf_neg, success)
      
      if (success) call prop_get_double(md_ptr, 'structure', 'extraresistance', generalst%extraresistance, success)
      
   end subroutine readGeneralStructure
   
   integer function  openingDirectionToInt(dirString)
      character(len=*), intent(inout) :: dirString
      call str_lower(dirString)
      select case(dirString)
      case('symmetric')
         openingDirectionToInt = GEN_SYMMETRIC
      case('fromleft')
         openingDirectionToInt = GEN_FROMLEFT
      case('fromright')
         openingDirectionToInt = GEN_FROMRIGHT
      case default
         openingDirectionToInt = GEN_SYMMETRIC
      end select
      
   end function  openingDirectionToInt
  
   !> Read the general structure parameters for version 1.00 files
   subroutine readGeneralStructure_v100(generalst, md_ptr, success)
   
      type(t_GeneralStructure), pointer, intent(inout)     :: generalst     !< general structure to be read into 
      type(tree_data), pointer, intent(in)                 :: md_ptr        !< ini tree pointer with user input.
      logical, intent(inout)                               :: success 
      
      logical                               :: success1, success2 

      allocate(generalst)

      allocate(generalst%fu(3,1), generalst%ru(3,1), generalst%au(3,1))
      allocate(generalst%widthcenteronlink(1), generalst%gateclosedfractiononlink(1))
      generalst%numlinks = 1
      generalst%gateclosedfractiononlink(1) = 1d0
      generalst%gatedoorheight = huge(1d0)
      generalst%velheight = .true.
      call prop_get_double(md_ptr, 'structure', 'widthLeftW1', generalst%wu1, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'widthleftWsdl',  generalst%wu2, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'widthcenter',    generalst%ws, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'widthrightWsdr', generalst%wd1, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'widthrightW2',   generalst%wd2, success)
                                                                               
      if (success) call prop_get_double(md_ptr, 'structure', 'levelleftZb1',   generalst%zu1, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'levelleftZbsl',  generalst%zu2, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'levelcenter',    generalst%zs, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'levelrightZbsr', generalst%zd1, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'levelrightZb2',  generalst%zd2, success)

      if (success) call prop_get_double(md_ptr, 'structure', 'gateLowerEdgeLevel', generalst%gateLowerEdgeLevel, success)

      if (success) call prop_get_double(md_ptr, 'structure', 'posfreegateflowcoeff',  generalst%cgf_pos, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'posdrowngateflowcoeff', generalst%cgd_pos, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'posfreeweirflowcoeff',  generalst%cwf_pos, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'posdrownweirflowcoeff', generalst%cwd_pos, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'poscontrcoeffreegate',  generalst%mugf_pos, success)
      
      if (success) call prop_get_double(md_ptr, 'structure', 'negfreegateflowcoeff',  generalst%cgf_neg, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'negdrowngateflowcoeff', generalst%cgd_neg, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'negfreeweirflowcoeff',  generalst%cwf_pos, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'negdrownweirflowcoeff', generalst%cwd_neg, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'negcontrcoeffreegate',  generalst%mugf_neg, success)
      
      if (success) call prop_get_double(md_ptr, 'structure', 'extraresistance', generalst%extraresistance, success)
     
   end subroutine readGeneralStructure_v100
  
end module m_readstructures
