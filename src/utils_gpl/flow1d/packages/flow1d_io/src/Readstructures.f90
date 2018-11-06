module m_readstructures
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2018.                                
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
   use m_structure
   use m_Weir
   use m_Universal_Weir
   use m_River_Weir
   use m_Advanced_Weir
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

   contains

   subroutine readStructures(network, structureFile)
      use m_GlobalParameters
      implicit none
      
      type(t_network), intent(inout) :: network
      character*(*), intent(in)      :: structureFile

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
      type(t_branch), pointer                                :: pbr
      
      integer                       :: pos
      integer                       :: ibin = 0
      character(len=Charln)         :: binfile
      logical                       :: file_exist
      
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
      
      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if

      do i = 1, numstr
         
         if (tree_get_name(md_ptr%child_nodes(i)%node_ptr) .eq. 'structure') then
            
            ! Read Common Structure Data
            
            call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'structure', 'id', structureId, success)
            if (.not. success) then
               write (msgbuf, '(a,i0,a)') 'Error Reading Structure #', i, ' from '''//trim(structureFile)//''', Id is missing.'
               call err_flush()
               cycle
            endif
            
            if (success) call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'structure', 'branchid', branchID, success)
            if (success) call prop_get_double(md_ptr%child_nodes(i)%node_ptr, 'structure', 'chainage', Chainage, success)

            if (.not. success) then
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
            
            branchIdx = hashsearch(network%brs%hashlist, branchID)
            
            pbr => network%brs%branch(branchIdx)
            
            call lowercase(typestr, 999)
            select case (typestr)
               
               case ('weir')
                  iStrucType = ST_WEIR
               
               case ('universalweir')
                  iStrucType = ST_UNI_WEIR
               
               case ('riverweir')
                  iStrucType = ST_RIVER_WEIR
               
               case ('advancedweir')
                  iStrucType = ST_ADV_WEIR
               
               case ('culvert')
                  iStrucType = ST_CULVERT
               
               case ('siphon')
                  iStrucType = ST_SIPHON
                  isInvertedSiphon = .false.
               
               case ('invertedsiphon')
                  iStrucType = ST_INV_SIPHON
                  isInvertedSiphon = .true.
               
               case ('bridge')
                  iStrucType = ST_BRIDGE
                  isPillarBridge = .false.
               
               case ('bridgepillar')
                  iStrucType = ST_BRIDGE
                  isPillarBridge = .true.
               
               case ('pump')
                  iStrucType = ST_PUMP
               
               case ('orifice', 'gate')
                  iStrucType = ST_ORIFICE
               
               case ('generalstructure')
                  iStrucType = ST_GENERAL_ST
               
               case ('extraresistance')
                  iStrucType = ST_EXTRA_RES
               
               case default
                  call SetMessage(LEVEL_FATAL, 'Unknown Structure Type')

            end select
            
            istru = Addstructure(network%sts, network%brs, branchIdx, Chainage, iCompound, compoundName, structureId, iStrucType)
               
            select case (iStrucType)
               
               case (ST_WEIR)
                  call readWeir(network%sts%struct(istru)%weir, md_ptr%child_nodes(i)%node_ptr, success)
               
               case (ST_UNI_WEIR)
                  call readUniversalWeir(network%sts%struct(istru)%uniweir, md_ptr%child_nodes(i)%node_ptr, success)
               
               case (ST_RIVER_WEIR)
                  call readRiverWeir(network%sts%struct(istru)%riverweir, md_ptr%child_nodes(i)%node_ptr, success)
               
               case (ST_ADV_WEIR)
                  call readAdvancedWeir(network%sts%struct(istru)%advweir, md_ptr%child_nodes(i)%node_ptr, success)
               
               case (ST_CULVERT)
                  call readCulvert(network, istru, md_ptr%child_nodes(i)%node_ptr, success)
               
               case (ST_SIPHON, ST_INV_SIPHON)
                  call readSiphon(network, istru, md_ptr%child_nodes(i)%node_ptr, isInvertedSiphon, success)
               
               case (ST_BRIDGE)
                  call readBridge(network, istru, md_ptr%child_nodes(i)%node_ptr, isPillarBridge, success)
               
               case (ST_PUMP)
                  call readPump(network%sts%struct(istru)%pump, md_ptr%child_nodes(i)%node_ptr, success)
               
               case (ST_ORIFICE)
                  call readOrifice(network%sts%struct(istru)%orifice, md_ptr%child_nodes(i)%node_ptr, success)
               
               case (ST_GENERAL_ST)
                  call readGeneralStructure(network%sts%struct(istru)%generalst, md_ptr%child_nodes(i)%node_ptr, success)
               
               case (ST_EXTRA_RES)
                  network%sts%hasExtraResistance = .true.
                  call readExtraResistance(network%sts%struct(istru)%extrares, md_ptr%child_nodes(i)%node_ptr, success)
               
            end select

            if (.not. success) then
               call SetMessage(LEVEL_FATAL, 'Error Reading Structure '''//trim(network%sts%struct(istru)%id)//'''')
            endif
         
         endif

      end do
      
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
            network%sts%struct(i)%st_name = network%sts%struct(i)%id
         enddo
      
      endif
         
      
      call fill_hashtable(network%sts)
      
      if (.not. allocated(network%sts%restartData) .and. (network%sts%count > 0)) then
         allocate(network%sts%restartData(network%sts%count, CFiHighestParameter))
         network%sts%restartData = missingValue
      endif

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
         read(ibin) pstr%st_name
         read(ibin) pstr%st_type
         read(ibin) pstr%ibran
         read(ibin) pstr%left_calc_point
         read(ibin) pstr%right_calc_point
         read(ibin) pstr%link_number
         read(ibin) pstr%x
         read(ibin) pstr%y
         read(ibin) pstr%distance
         read(ibin) pstr%compound
         read(ibin) pstr%compoundName
         
         select case(pstr%st_type)
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
            
            case(ST_RIVER_WEIR)
               allocate(pstr%riverweir)
               read(ibin) pstr%riverweir%crestlevel
               read(ibin) pstr%riverweir%crestwidth
               read(ibin) pstr%riverweir%pos_cwcoeff
               read(ibin) pstr%riverweir%pos_slimlimit
               read(ibin) pstr%riverweir%neg_cwcoeff
               read(ibin) pstr%riverweir%neg_slimlimit
               read(ibin) pstr%riverweir%dynstrucfact
               
               call read_table_cache(ibin, pstr%riverweir%pos_reducfact)
               call read_table_cache(ibin, pstr%riverweir%neg_reducfact)
            
            case(ST_ADV_WEIR)
               allocate(pstr%advweir)
               read(ibin) pstr%advweir%crestlevel
               read(ibin) pstr%advweir%totwidth
               read(ibin) pstr%advweir%npiers
               read(ibin) pstr%advweir%pos_height
               read(ibin) pstr%advweir%pos_designhead
               read(ibin) pstr%advweir%pos_piercontractcoeff
               read(ibin) pstr%advweir%pos_abutcontractcoeff
               read(ibin) pstr%advweir%neg_height
               read(ibin) pstr%advweir%neg_designhead
               read(ibin) pstr%advweir%neg_piercontractcoeff
               read(ibin) pstr%advweir%neg_abutcontractcoeff
               read(ibin) pstr%advweir%dynstrucfact
            
            case(ST_GENERAL_ST)
               allocate(pstr%generalst)
               read(ibin) pstr%generalst%widthleftW1
               read(ibin) pstr%generalst%levelleftZb1
               read(ibin) pstr%generalst%widthleftWsdl
               read(ibin) pstr%generalst%levelleftZbsl
               read(ibin) pstr%generalst%widthcenter
               read(ibin) pstr%generalst%levelcenter
               read(ibin) pstr%generalst%widthrightWsdr
               read(ibin) pstr%generalst%levelrightZbsr
               read(ibin) pstr%generalst%widthrightW2
               read(ibin) pstr%generalst%levelrightZb2
               read(ibin) pstr%generalst%gateheight
               read(ibin) pstr%generalst%gateheightintervalcntrl
               read(ibin) pstr%generalst%pos_freegateflowcoeff
               read(ibin) pstr%generalst%pos_drowngateflowcoeff
               read(ibin) pstr%generalst%pos_freeweirflowcoeff
               read(ibin) pstr%generalst%pos_drownweirflowcoeff
               read(ibin) pstr%generalst%pos_contrcoeffreegate
               read(ibin) pstr%generalst%neg_freegateflowcoeff
               read(ibin) pstr%generalst%neg_drowngateflowcoeff
               read(ibin) pstr%generalst%neg_freeweirflowcoeff
               read(ibin) pstr%generalst%neg_drownweirflowcoeff
               read(ibin) pstr%generalst%neg_contrcoeffreegate
               read(ibin) pstr%generalst%extraresistance
               read(ibin) pstr%generalst%stabilitycounter
               read(ibin) pstr%generalst%dynstrucfact
            
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
         write(ibin) pstr%st_name
         write(ibin) pstr%st_type
         write(ibin) pstr%ibran
         write(ibin) pstr%left_calc_point
         write(ibin) pstr%right_calc_point
         write(ibin) pstr%link_number
         write(ibin) pstr%x
         write(ibin) pstr%y
         write(ibin) pstr%distance
         write(ibin) pstr%compound
         write(ibin) pstr%compoundName
         
         select case(pstr%st_type)
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
            
            case(ST_RIVER_WEIR)
               write(ibin) pstr%riverweir%crestlevel
               write(ibin) pstr%riverweir%crestwidth
               write(ibin) pstr%riverweir%pos_cwcoeff
               write(ibin) pstr%riverweir%pos_slimlimit
               write(ibin) pstr%riverweir%neg_cwcoeff
               write(ibin) pstr%riverweir%neg_slimlimit
               write(ibin) pstr%riverweir%dynstrucfact
               
               call write_table_cache(ibin, pstr%riverweir%pos_reducfact)
               call write_table_cache(ibin, pstr%riverweir%neg_reducfact)
            
            case(ST_ADV_WEIR)
               write(ibin) pstr%advweir%crestlevel
               write(ibin) pstr%advweir%totwidth
               write(ibin) pstr%advweir%npiers
               write(ibin) pstr%advweir%pos_height
               write(ibin) pstr%advweir%pos_designhead
               write(ibin) pstr%advweir%pos_piercontractcoeff
               write(ibin) pstr%advweir%pos_abutcontractcoeff
               write(ibin) pstr%advweir%neg_height
               write(ibin) pstr%advweir%neg_designhead
               write(ibin) pstr%advweir%neg_piercontractcoeff
               write(ibin) pstr%advweir%neg_abutcontractcoeff
               write(ibin) pstr%advweir%dynstrucfact
            
            case(ST_GENERAL_ST)
               write(ibin) pstr%generalst%widthleftW1
               write(ibin) pstr%generalst%levelleftZb1
               write(ibin) pstr%generalst%widthleftWsdl
               write(ibin) pstr%generalst%levelleftZbsl
               write(ibin) pstr%generalst%widthcenter
               write(ibin) pstr%generalst%levelcenter
               write(ibin) pstr%generalst%widthrightWsdr
               write(ibin) pstr%generalst%levelrightZbsr
               write(ibin) pstr%generalst%widthrightW2
               write(ibin) pstr%generalst%levelrightZb2
               write(ibin) pstr%generalst%gateheight
               write(ibin) pstr%generalst%gateheightintervalcntrl
               write(ibin) pstr%generalst%pos_freegateflowcoeff
               write(ibin) pstr%generalst%pos_drowngateflowcoeff
               write(ibin) pstr%generalst%pos_freeweirflowcoeff
               write(ibin) pstr%generalst%pos_drownweirflowcoeff
               write(ibin) pstr%generalst%pos_contrcoeffreegate
               write(ibin) pstr%generalst%neg_freegateflowcoeff
               write(ibin) pstr%generalst%neg_drowngateflowcoeff
               write(ibin) pstr%generalst%neg_freeweirflowcoeff
               write(ibin) pstr%generalst%neg_drownweirflowcoeff
               write(ibin) pstr%generalst%neg_contrcoeffreegate
               write(ibin) pstr%generalst%extraresistance
               write(ibin) pstr%generalst%stabilitycounter
               write(ibin) pstr%generalst%dynstrucfact

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
      
      call prop_get_integer(md_ptr, 'structure', 'levelsCount', uniweir%yzcount, success)
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
   
   subroutine readRiverWeir(riverweir, md_ptr, success)
   
      type(t_riverweir), pointer, intent(inout)     :: riverweir
      type(tree_data), pointer, intent(in)          :: md_ptr
      logical, intent(inout)                        :: success 

      integer                                       :: istat
      integer                                       :: sf_count
      double precision, allocatable, dimension(:)   :: sf
      double precision, allocatable, dimension(:)   :: red
      
      allocate(riverweir)

      call prop_get_double(md_ptr, 'structure', 'crestlevel', riverweir%crestlevel, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'crestwidth', riverweir%crestwidth, success)

      if (success) call prop_get_double(md_ptr, 'structure', 'poscwcoef', riverweir%pos_cwcoeff, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'posslimlimit', riverweir%pos_slimlimit, success)

      if (success) call prop_get_double(md_ptr, 'structure', 'negcwcoef', riverweir%neg_cwcoeff, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'negslimlimit', riverweir%neg_slimlimit, success)

      ! Reduction Factors, First in positive Direction, Then in Negative Direction.
      if (success) call prop_get_integer(md_ptr, 'structure', 'possfcount', sf_count, success)
      if (.not. success) return
            
      call realloc(sf, sf_count, stat=istat)
      if (istat == 0) call realloc(red, sf_count, stat=istat)
      if (istat .ne. 0) then
         call SetMessage(LEVEL_FATAL, 'Reading River Weir: Error Allocating Reduction Table Arrays')
      endif

      call prop_get_doubles(md_ptr, 'structure', 'possf', sf, sf_count, success)
      if (success)call prop_get_doubles(md_ptr, 'structure', 'posred', red, sf_count, success)
      if (.not. success) return
      
      call setTable(riverweir%pos_reducfact, 0, sf, red, sf_count)
      
      if (success) call prop_get_integer(md_ptr, 'structure', 'negsfcount', sf_count, success)
      if (.not. success) return
            
      call realloc(sf, sf_count, stat=istat)
      if (istat == 0) call realloc(red, sf_count, stat=istat)
      if (istat .ne. 0) then
         call SetMessage(LEVEL_FATAL, 'Reading River Weir: Error Allocating Reduction Table Arrays')
      endif

      call prop_get_doubles(md_ptr, 'structure', 'negsf', sf, sf_count, success)
      if (success) call prop_get_doubles(md_ptr, 'structure', 'negred', red, sf_count, success)
      if (.not. success) return
      
      call setTable(riverweir%neg_reducfact, 0, sf, red, sf_count)

      riverweir%dynstrucfact   = -1.0d0

      ! Clear Reduction Table Arrays
      istat = 0
      if (allocated(sf)) deallocate(sf, stat=istat)
      if (istat == 0 .and. allocated(red)) deallocate(red, stat=istat)
      if (istat .ne. 0) then
         call SetMessage(LEVEL_FATAL, 'Reading River Weir: Error Deallocating Gridpoint Arrays')
      endif

   end subroutine readRiverWeir
   
   subroutine readAdvancedWeir(advweir, md_ptr, success)
   
      type(t_advweir), pointer, intent(inout)     :: advweir
      type(tree_data), pointer, intent(in)        :: md_ptr
      logical, intent(inout)                      :: success 

      allocate(advweir)

      call prop_get_double(md_ptr, 'structure', 'crestlevel', advweir%crestlevel, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'crestwidth', advweir%totwidth, success)
      if (success) call prop_get_integer(md_ptr, 'structure', 'npiers', advweir%npiers, success)

      if (success) call prop_get_double(md_ptr, 'structure', 'posheight', advweir%pos_height, success)

      if (success) call prop_get_double(md_ptr, 'structure', 'posdesignhead', advweir%pos_designhead, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'pospiercontractcoef', advweir%pos_piercontractcoeff, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'posabutcontractcoef', advweir%pos_abutcontractcoeff, success)

      if (success) call prop_get_double(md_ptr, 'structure', 'negheight', advweir%neg_height, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'negdesignhead', advweir%neg_designhead, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'negpiercontractcoef', advweir%neg_piercontractcoeff, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'negabutcontractcoef', advweir%neg_abutcontractcoeff, success)
      
      advweir%dynstrucfact          = -1.0d0
            
   end subroutine readAdvancedWeir
   
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

         if (success) call prop_get_integer(md_ptr, 'structure', 'lossCoeffCount', lossCoeffCount, success)
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

   subroutine readPump(pump, md_ptr, success)
   
      type(t_pump), pointer, intent(inout)     :: pump
      type(tree_data), pointer, intent(in)     :: md_ptr
      logical, intent(inout)                   :: success
      
      integer                                      :: tabsize
      integer                                      :: istat
      double precision, allocatable, dimension(:)  :: head   
      double precision, allocatable, dimension(:)  :: redfac   
      
      
      allocate(pump)

      call prop_get_integer(md_ptr, 'structure', 'direction', pump%direction, success)
      if (success) call prop_get_integer(md_ptr, 'structure', 'nrstages', pump%nrstages, success)
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

      
      call prop_get_doubles(md_ptr, 'structure', 'capacity', pump%capacity, pump%nrstages, success)     
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
      call prop_get_integer(md_ptr, 'structure', 'reductionfactorlevels', tabsize, success)
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
   
   subroutine readGeneralStructure(generalst, md_ptr, success)
   
      type(t_GeneralStructure), pointer, intent(inout)     :: generalst
      type(tree_data), pointer, intent(in)                 :: md_ptr
      logical, intent(inout)                               :: success 
      
      logical                               :: success1, success2 

      allocate(generalst)

      call prop_get_double(md_ptr, 'structure', 'widthleftW1', generalst%widthleftW1, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'widthleftWsdl', generalst%widthleftWsdl, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'widthcenter', generalst%widthcenter, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'widthrightWsdr', generalst%widthrightWsdr, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'widthrightW2', generalst%widthrightW2, success)

      if (success) call prop_get_double(md_ptr, 'structure', 'levelleftZb1', generalst%levelleftZb1, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'levelleftZbsl', generalst%levelleftZbsl, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'levelcenter', generalst%levelcenter, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'levelrightZbsr', generalst%levelrightZbsr, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'levelrightZb2', generalst%levelrightZb2, success)
      
      if (success) call prop_get_double(md_ptr, 'structure', 'gateheight', generalst%gateheight, success)
      if (success) generalst%gateheightintervalcntrl = generalst%gateheight
      
      call prop_get_double(md_ptr, 'structure', 'pos_freegateflowcoeff',  generalst%pos_freegateflowcoeff, success1)
      call prop_get_double(md_ptr, 'structure', 'posfreegateflowcoeff',   generalst%pos_freegateflowcoeff, success2)  ! Backwards compatible reading of old keyword
      success = success .and. (success1 .or. success2)
      call prop_get_double(md_ptr, 'structure', 'pos_drowngateflowcoeff', generalst%pos_drowngateflowcoeff, success1)
      call prop_get_double(md_ptr, 'structure', 'posdrowngateflowcoeff',  generalst%pos_drowngateflowcoeff, success2) ! Backwards compatible reading of old keyword
      success = success .and. (success1 .or. success2)
      call prop_get_double(md_ptr, 'structure', 'pos_freeweirflowcoeff',  generalst%pos_freeweirflowcoeff, success1)
      call prop_get_double(md_ptr, 'structure', 'posfreeweirflowcoeff',   generalst%pos_freeweirflowcoeff, success2)  ! Backwards compatible reading of old keyword
      success = success .and. (success1 .or. success2)
      call prop_get_double(md_ptr, 'structure', 'pos_drownweirflowcoeff', generalst%pos_drownweirflowcoeff, success1)
      call prop_get_double(md_ptr, 'structure', 'posdrownweirflowcoeff',  generalst%pos_drownweirflowcoeff, success2) ! Backwards compatible reading of old keyword
      success = success .and. (success1 .or. success2)
      call prop_get_double(md_ptr, 'structure', 'pos_contrcoeffreegate',  generalst%pos_contrcoeffreegate, success1)
      call prop_get_double(md_ptr, 'structure', 'poscontrcoeffreegate',   generalst%pos_contrcoeffreegate, success2)  ! Backwards compatible reading of old keyword
      success = success .and. (success1 .or. success2)
      
      call prop_get_double(md_ptr, 'structure', 'neg_freegateflowcoeff',  generalst%neg_freegateflowcoeff, success1)
      call prop_get_double(md_ptr, 'structure', 'negfreegateflowcoeff',   generalst%neg_freegateflowcoeff, success2)  ! Backwards compatible reading of old keyword
      success = success .and. (success1 .or. success2)
      call prop_get_double(md_ptr, 'structure', 'neg_drowngateflowcoeff', generalst%neg_drowngateflowcoeff, success1)
      call prop_get_double(md_ptr, 'structure', 'negdrowngateflowcoeff',  generalst%neg_drowngateflowcoeff, success2) ! Backwards compatible reading of old keyword
      success = success .and. (success1 .or. success2)
      call prop_get_double(md_ptr, 'structure', 'neg_freeweirflowcoeff',  generalst%neg_freeweirflowcoeff, success1)
      call prop_get_double(md_ptr, 'structure', 'negfreeweirflowcoeff',   generalst%neg_freeweirflowcoeff, success2)  ! Backwards compatible reading of old keyword
      success = success .and. (success1 .or. success2)
      call prop_get_double(md_ptr, 'structure', 'neg_drownweirflowcoeff', generalst%neg_drownweirflowcoeff, success1)
      call prop_get_double(md_ptr, 'structure', 'negdrownweirflowcoeff',  generalst%neg_drownweirflowcoeff, success2) ! Backwards compatible reading of old keyword
      success = success .and. (success1 .or. success2)
      call prop_get_double(md_ptr, 'structure', 'neg_contrcoeffreegate',  generalst%neg_contrcoeffreegate, success1)
      call prop_get_double(md_ptr, 'structure', 'negcontrcoeffreegate',   generalst%neg_contrcoeffreegate, success2)  ! Backwards compatible reading of old keyword
      success = success .and. (success1 .or. success2)
      
      if (success) call prop_get_double(md_ptr, 'structure', 'extraresistance', generalst%extraresistance, success)
      
      generalst%stabilitycounter = 0.0d0
      generalst%dynstrucfact     = -1.0d0

   end subroutine readGeneralStructure
   
   subroutine readExtraResistance(extrares, md_ptr, success)
   
      type(t_ExtraResistance), pointer, intent(inout)     :: extrares
      type(tree_data), pointer, intent(in)                :: md_ptr
      logical, intent(inout)                              :: success 
      
      integer                                             :: istat
      integer                                             :: numValues
      double precision, allocatable, dimension(:)         :: levels
      double precision, allocatable, dimension(:)         :: ksi

      allocate(extrares)
      
      call prop_get_integer(md_ptr, 'structure', 'numvalues', numValues, success)
      if (.not. success) return
      
      call realloc(levels, numValues, stat=istat)
      if (istat == 0) call realloc(ksi, numValues, stat=istat)
      if (istat .ne. 0) then
         call SetMessage(LEVEL_FATAL, 'Reading Extra Resistance: Error Allocating Arrays')
      endif

      call prop_get_doubles(md_ptr, 'structure', 'levels', levels, numValues, success)
      if (success)call prop_get_doubles(md_ptr, 'structure', 'ksi', ksi, numValues, success)
      if (.not. success) return
      
      call setTable(extraRes%values, 0, levels, ksi, numValues)

      ! Clear Arrays
      istat = 0
      if (allocated(levels)) deallocate(levels, stat=istat)
      if (istat == 0 .and. allocated(ksi)) deallocate(ksi, stat=istat)
      if (istat .ne. 0) then
         call SetMessage(LEVEL_FATAL, 'Reading Extra Resistance: Error Deallocating Arrays')
      endif

   end subroutine readExtraResistance
   
end module m_readstructures
