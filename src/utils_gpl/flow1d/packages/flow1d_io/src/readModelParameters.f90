module m_readModelParameters
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
   use m_globalParameters
   use ModelParameters
   use properties
   use string_module
   use m_boundaryConditions

   implicit none

   private

   public readModelParameters

   contains

   subroutine readModelParameters(md_ptr, sobekSimIniFile)
      use m_temperature
      
      implicit none
      
      
      type(tree_data), pointer, intent(in)      :: md_ptr
      character(len=Charln)                     :: sobekSimIniFile

      integer                                   :: iValue
      double precision                          :: Value
      logical                                   :: success
      character(len=Idlen)                      :: scheme, heat_model, dens_comp

      call setModelParameterDefaults()
      
      call readFromModelDefinitionFile(md_ptr)
      
      call ReadSobekSimIni(sobekSimIniFile)
      
      ! Read Interpolation Type for Obsevation Points
      call prop_get_string(md_ptr, 'Observations', 'interpolationType', obsIntPolType, success)
      if (.not. success) then
         obsIntPolType = 'OBS_NEAREST'
      endif
      
      ! Read Salt Switch
      call prop_get_integer(md_ptr, 'salinity', 'SaltComputation', iValue, success)
      if (success .and. (iValue .ne. 0)) then
         transportPars%do_salt = .true.
         transportPars%salt_index = 1
         transportPars%constituents_count = 1
         call AddOrReplaceParameter('Salinity', 'SaltComputation', 'true', .true.)
         transportPars%co_h(transportPars%salt_index)%boundary_index = S_BOUN
      else
         transportPars%do_salt = .false.
         transportPars%salt_index = -1
         transportPars%constituents_count = 0
         call AddOrReplaceParameter('Salinity', 'SaltComputation', 'false', .true.)
      endif
      
      ! Read Temperature Switch
      call prop_get_integer(md_ptr, 'TransportComputation', 'Temperature', iValue, success)
      if (success .and. (iValue .ne. 0)) then
         transportPars%do_temp = .true.
         call AddOrReplaceParameter('TransportComputation', 'Temperature', 'true', .true.)
         transportPars%constituents_count = transportPars%constituents_count + 1
         transportPars%temp_index = transportPars%constituents_count 
         transportPars%co_h(transportPars%temp_index)%boundary_index = T_BOUN
         call default_heatfluxes()
         value = 15d0
         call prop_get_double(md_ptr, 'Temperature', 'BackgroundTemperature', value, success)
         call set_par_temperature('air_temperature', value)
         value = 1d6
         call prop_get_double(md_ptr, 'Temperature', 'surfaceArea', value, success)
         call set_par_temperature('s_area', value)
         value = 1d5
         call prop_get_double(md_ptr, 'Temperature', 'atmosphericPressure', value, success)
         call set_par_temperature('p_atm', value)
         value = 0.0013
         call prop_get_double(md_ptr, 'Temperature', 'daltonNumber', value, success)
         call set_par_temperature('c_e_dalton', value)
         value = 3930d0
         call prop_get_double(md_ptr, 'Temperature', 'heatCapacityWater', value, success)
         call set_par_temperature('c_p', value)
         value = 0.0013
         call prop_get_double(md_ptr, 'Temperature', 'stantonNumber', value, success)
         call set_par_temperature('c_h_stanton', value)
      else
         transportPars%do_temp = .false.
         transportPars%temp_index = -1
         call AddOrReplaceParameter('TransportComputation', 'Temperature', 'false', .true.)
      endif
      
      if (.not. transportPars%do_salt) then
         transportPars%salt_index = transportPars%constituents_count + 1
      endif
      if (.not. transportPars%do_temp) then
         transportPars%temp_index = max(transportPars%constituents_count + 1, transportPars%salt_index + 1)
      endif
      
      dens_comp = 'eckart_modified'
      call prop_get_string(md_ptr, 'TransportComputation', 'Density', dens_comp, success)
      call lowercase(dens_comp, 999)
      select case (trim(dens_comp))
      case ('eckart_modified', 'eckhart_modified')
         transportPars%density = DENS_ECKART_MODIFIED
         msgbuf = 'Eckart(modified)'
         call AddOrReplaceParameter('TransportComputation', 'Density', 'true', .true.)
      case ('eckart', 'eckhart')
         transportPars%density = DENS_ECKART
         msgbuf = 'Eckart'
         call AddOrReplaceParameter('TransportComputation', 'Density', 'true', .true.)
      case ('unesco')
         transportPars%density = DENS_UNESCO
         msgbuf = 'Unesco'
         call AddOrReplaceParameter('TransportComputation', 'Density', 'true', .true.)
      case default
         msgbuf = 'unknown density type'
         call err_flush()
      end select
      msgbuf = 'Density computation set to '//trim(msgbuf)
      call msg_flush()
         
      heat_model = 'transport'
      call prop_get_string(md_ptr, 'TransportComputation', 'HeatTransferModel', heat_model, success)
      call lowercase(heat_model, 999)
      select case (trim(heat_model))
      case ('transport')
         tempPars%heat_model = HEAT_TRANSPORT
         call AddOrReplaceParameter('TransportComputation', 'HeatTransferModel', 'transport', .true.)
      case ('excess')
         tempPars%heat_model = HEAT_EXCESS
         call AddOrReplaceParameter('TransportComputation', 'HeatTransferModel', 'excess', .true.)
      case ('composite')
         tempPars%heat_model = HEAT_COMPOSITE
         call AddOrReplaceParameter('TransportComputation', 'HeatTransferModel', 'composite', .true.)
      case default
         tempPars%heat_model = HEAT_TRANSPORT
         call AddOrReplaceParameter('TransportComputation', 'HeatTransferModel', 'transport', .true.)
      end select
      
      call prop_get_double(md_ptr, 'Salinity', 'Teta', transportPars%teta, success)
      
      scheme = 'vanLeer-2'
      call prop_get_string(md_ptr, 'salinity', 'advectionScheme', scheme, success)
      if (scheme == 'vanLeer-2') then
         transportPars%advection_scheme = ADV_VANLEER
      else
         transportPars%advection_scheme = ADV_UPWIND
      endif
      
      ! Read Read-From-UGrid Switch
      call prop_get_integer(md_ptr, 'SimulationOptions', 'ReadNetworkFromUgrid', iValue, success)
      if (success .and. (iValue .ne. 0)) then
         readNetworkFromUgrid = .true.
         call AddOrReplaceParameter('SimulationOptions', 'ReadNetworkFromUgrid', 'true', .true.)
      else
         readNetworkFromUgrid = .false.
         call AddOrReplaceParameter('SimulationOptions', 'ReadNetworkFromUgrid', 'false', .true.)
      endif

      ! Read Write-NetCDF Switch
      call prop_get_integer(md_ptr, 'SimulationOptions', 'WriteNetCDF', iValue, success)
      if (success .and. (iValue .ne. 0)) then
         writeNetCDF = .true.
         call AddOrReplaceParameter('SimulationOptions', 'WriteNetCDF', 'true', .true.)
      else
         writeNetCDF = .false.
         call AddOrReplaceParameter('SimulationOptions', 'WriteNetCDF', 'false', .true.)
      endif

      latitude = 0d0
      longitude = 0d0
      time_zone = 0d0
      call prop_get_double(md_ptr, 'AdvancedOptions', 'Latitude' , latitude,  success)
      call prop_get_double(md_ptr, 'AdvancedOptions', 'Longitude', longitude, success)
      call prop_get_double(md_ptr, 'AdvancedOptions', 'timeZone', time_zone, success)
      
      write_tables = .false.
      call prop_get_logical(md_ptr, 'StorageTable', 'WriteStorageTables', write_tables, success)
      if (write_tables) then
         call prop_get_string(md_ptr, 'StorageTable', 'StorageOutputFile', st_filename, success)
         if (.not. success) then
            call setmessage(LEVEL_ERROR, 'StorageOutputFile not found in md1d file')
         endif
         tb_inc = 0.1
         call prop_get_double(md_ptr, 'StorageTable', 'StorageTableIncrement', tb_inc, success)
      endif
      
      call prop_get_integer(md_ptr, 'Morphology', 'CalculateMorphology', iValue, success)
      if (success .and. iValue==1) then
         call AddOrReplaceParameter('Morphology', 'CalculateMorphology', 'true', .true.)
         
         call prop_get_string(md_ptr, 'Morphology', 'SedimentInputFile', scheme, success)
         if (success .and. iValue==1) then
            call AddOrReplaceParameter('Morphology', 'SedimentInputFile', scheme, .true.)
         endif  
         
         call prop_get_string(md_ptr, 'Morphology', 'MorphologyInputFile', scheme, success)
         if (success .and. iValue==1) then
            call AddOrReplaceParameter('Morphology', 'MorphologyInputFile', scheme, .true.)
         endif    
 
    endif
     
   end subroutine readModelParameters
   
   subroutine readFromModelDefinitionFile(md_ptr)

      type(tree_data), pointer, intent(in)      :: md_ptr
   
      integer                                   :: numstr
      character(len=40)                         :: category
      character(len=40)                         :: keyWord
      character(len=40)                         :: keyValue
      type(tree_data), pointer                  :: cat_ptr
      integer                                   :: numkeys
      integer                                   :: icat
      integer                                   :: ikey
   
      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if
   
      do icat = 1, numstr  ! Skipping 'General', 'Files' and 'GlobalValues'
         
         category = tree_get_name(md_ptr%child_nodes(icat)%node_ptr)
         call str_lower(category)
         
         if (category == 'general' .or. category == 'files' .or. category == 'globalvalues') cycle
         
         if (category == 'time') then
            call readDateTimeStepData(md_ptr)
            cycle
         endif
         
         cat_ptr => md_ptr%child_nodes(icat)%node_ptr
         numkeys = 0
         if (associated(cat_ptr%child_nodes)) then
            numkeys = size(cat_ptr%child_nodes)
         endif
         
         do ikey = 1, numkeys
            
            keyWord = tree_get_name(cat_ptr%child_nodes(ikey)%node_ptr)
            keyValue = tree_get_data(cat_ptr%child_nodes(ikey)%node_ptr)

            call str_lower(keyWord)
            call str_lower(keyValue)
            
            call AddOrReplaceParameter(category, keyWord, keyValue, .true.)
            
            if (category == 'numericalparameters' .and. keyWord == 'thresholdvalueflooding') then
               ! FloodingDividedByDrying and ThresholdValueDrying are not implemented yet into the File Writers
               ! So here calculate and store ThresholdValueDrying
               read (keyValue, *) thresholdDry 
               thresholdDry  = thresholdDry  / 10.0d0
               write(keyValue, '(f8.6)') thresholdDry 
               call AddOrReplaceParameter('numericalparameters', 'thresholdvaluedrying', keyValue, .true.)
            endif
            
            ! Set CacheMode Switches
            if (category == 'advancedoptions' .and. keyWord == 'cachemode') then
            
               select case (keyValue)
                  case ('none')
                     doReadCache  = .false.
                     doWriteCache = .false.
                  case ('read')
                     doReadCache  = .true.
                     doWriteCache = .false.
                  case ('write')
                     doReadCache  = .false.
                     doWriteCache = .true.
                  case default
                     doReadCache  = .false.
                     doWriteCache = .false.
               end select
            endif
            
         enddo
         
      enddo

   
   end subroutine readFromModelDefinitionFile
   
   subroutine readDateTimeStepData(md_ptr)

      type(tree_data), pointer, intent(in)      :: md_ptr
      
      character(Len=40)                         :: startTime
      character(Len=40)                         :: stopTime
      double precision                          :: timeStep
      double precision                          :: outputGrid
      logical                                   :: success
      
      integer                                   :: iYear
      integer                                   :: iMonth
      integer                                   :: iDay
      integer                                   :: iHour
      integer                                   :: iMinute
      integer                                   :: iSecond

      double precision                          :: julDate
      double precision                          :: jul1jan
      integer                                   :: restartTime
     
      call prop_get_string(md_ptr, 'time', 'starttime', startTime, success)   
      if (success) call prop_get_string(md_ptr, 'time', 'stoptime', stopTime, success)   
      if (success) call prop_get_double(md_ptr, 'time', 'timestep', timeStep, success)   
   
      if (.not. success) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Date Time Data')
      endif
      
      if (timeStep <= 0.0d0) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Date Time Data: Time Step must be > 0.0')
      endif

      read (startTime, '(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)') iYear, iMonth, iDay, iHour, iMinute, iSecond
      
      julDate = julian(iYear, iMonth, iDay, iHour, iMinute, iSecond)
      if (julDate <= 0.0d0) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Date Time Data: Invalid Start Date/Time')
      endif
      
      modelTimeStepData%julianStart = juldate
      
      modelTimeStepData%startDate = iYear * 10000 + iMonth * 100 + iDay
      modelTimeStepData%startTime = iHour * 10000 + iMinute * 100 + iSecond     
      
      jul1jan = julian(iYear, 1, 1, 0, 0, 0)
      modelTimeStepData%hoursToStartFromFirstOfJanuari = (juldate - jul1jan)*24d0
      
      read (stopTime, '(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)') iYear, iMonth, iDay, iHour, iMinute, iSecond

      julDate = julian(iYear, iMonth, iDay, iHour, iMinute, iSecond)
      if (julDate <= 0.0d0) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Date Time Data: Invalid Stop Date/Time')
      endif
      
      modelTimeStepData%julianEnd = juldate

      modelTimeStepData%endDate = iYear * 10000 + iMonth * 100 + iDay
      modelTimeStepData%endTime = iHour * 10000 + iMinute * 100 + iSecond
      
      if (modelTimeStepData%julianEnd < modelTimeStepData%julianStart) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Date Time Data: Stop Time must be after Start Time')
      endif
      
      ! Output Time Step
      outputGrid = timeStep
      call prop_get_double(md_ptr, 'time', 'outtimestepgridpoints', outputGrid, success)
      if (success) then 
         if (mod(outputGrid, timeStep) > 0.0d0 .or. outputGrid < timeStep) then
            call SetMessage(LEVEL_FATAL, 'Error Reading Date Time Data: Output Time Step must be multiple of Time Step')
         endif
      else
         outputGrid = timeStep
      endif
      
      modelTimeStepData%timeStep       = timeStep
      modelTimeStepData%outputTimeStep = outputGrid
         
      ! restart data
      call prop_get_string(md_ptr, 'restart', 'starttime', startTime, success)   
      if (success) call prop_get_string(md_ptr, 'restart', 'stoptime', stopTime, success)   
      if (success) call prop_get_double(md_ptr, 'restart', 'restarttimestep', timeStep, success)   
   
      if (success) then
      
         if (timeStep <= 0.0d0) then
            call SetMessage(LEVEL_FATAL, 'Error Reading Date Time Data: Time Step must be > 0.0')
         endif

         read (startTime, '(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)') iYear, iMonth, iDay, iHour, iMinute, iSecond
         julDate = julian(iYear, iMonth, iDay, iHour, iMinute, iSecond)
         RestartTime = nint((juldate - modelTimeStepData%julianStart)*86400)
         modelTimeStepData%nextRestarttimestep =restartTime/modelTimeStepData%timeStep
         
         read (stopTime, '(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)') iYear, iMonth, iDay, iHour, iMinute, iSecond
         julDate = julian(iYear, iMonth, iDay, iHour, iMinute, iSecond)
         RestartTime = nint((juldate - modelTimeStepData%julianStart)*86400)
         modelTimeStepData%restartendTimestep = restartTime/modelTimeStepData%timeStep
         
         if (mod(timeStep, modelTimeStepData%timeStep) > 0.0d0 .or. timeStep < modelTimeStepData%timeStep) then
            call SetMessage(LEVEL_FATAL, 'Error Reading Date Time Data: Output Time Step must be multiple of Time Step')
         endif
         modelTimeStepData%restartInterval = nint(timeStep/modelTimeStepData%timeStep)
      else 
         modelTimeStepData%nextRestarttimestep = nint((modelTimeStepData%julianEnd - modelTimeStepData%julianStart) * 86400)
      endif
      
   end subroutine readDateTimeStepData
   
   subroutine setModelParameterDefaults()
   
      character(len=40)                    :: category
   
      category = 'InitialConditions'
      call AddOrReplaceParameter(category, "InitialEmptyWells", '0', .true.)

      category = 'ResultsNodes'
      call AddOrReplaceParameter(category, 'Density', 'none', .true.)
      call AddOrReplaceParameter(category, 'Lateral1d2d', 'none', .true.)
      call AddOrReplaceParameter(category, 'LateralOnNodes', 'none', .true.)
      call AddOrReplaceParameter(category, 'LevelFromStreetLevel', 'none', .true.)
      call AddOrReplaceParameter(category, 'RunOff', 'none', .true.)
      call AddOrReplaceParameter(category, 'Salinity', 'none', .true.)
      call AddOrReplaceParameter(category, 'TimeWaterOnStreet', 'none', .true.)
      call AddOrReplaceParameter(category, 'TotalArea', 'none', .true.)
      call AddOrReplaceParameter(category, 'TotalWidth', 'none', .true.)
      call AddOrReplaceParameter(category, 'Volume', 'none', .true.)
      call AddOrReplaceParameter(category, 'VolumeError', 'none', .true.)
      call AddOrReplaceParameter(category, 'VolumesOnStreet', 'none', .true.)
      call AddOrReplaceParameter(category, 'WaterDepth', 'none', .true.)
      call AddOrReplaceParameter(category, 'WaterLevel', 'current', .true.)
      call AddOrReplaceParameter(category, 'WaterOnStreet', 'none', .true.)
      
      ! Parameters for Temperature Model
      call AddOrReplaceParameter(category, 'TotalHeatFlux', 'none', .true.)
      call AddOrReplaceParameter(category, 'RadFluxClearSky', 'none', .true.)
      call AddOrReplaceParameter(category, 'HeatLossConv', 'none', .true.)
      call AddOrReplaceParameter(category, 'NetSolarRad', 'none', .true.)
      call AddOrReplaceParameter(category, 'EffectiveBackRad', 'none', .true.)
      call AddOrReplaceParameter(category, 'HeatLossEvap', 'none', .true.)
      call AddOrReplaceParameter(category, 'HeatLossForcedEvap', 'none', .true.)
      call AddOrReplaceParameter(category, 'HeatLossFreeEvap', 'none', .true.)
      call AddOrReplaceParameter(category, 'HeatLossForcedConv', 'none', .true.)
      call AddOrReplaceParameter(category, 'HeatLossFreeConv', 'none', .true.)

      ! Parameters for Morphology
      call AddOrReplaceParameter(category, 'BedLevel', 'none', .true.)
      call AddOrReplaceParameter(category, 'IncreaseCrossSec', 'none', .true.)
      call AddOrReplaceParameter(category, 'MeanBedLevelMain', 'none', .true.)
      call AddOrReplaceParameter(category, 'AdaptedCrossSec', 'none', .true.)
      call AddOrReplaceParameter(category, 'CumIncreaseCrossSec', 'none', .true.)
      call AddOrReplaceParameter(category, 'IntegrSedTransp', 'none', .true.)
      call AddOrReplaceParameter(category, 'GrainSizeD50', 'none', .true.)
      call AddOrReplaceParameter(category, 'GrainSizeD90', 'none', .true.)
      call AddOrReplaceParameter(category, 'SedimentTransport', 'none', .true.)
      call AddOrReplaceParameter(category, 'SedimentTransportLeft', 'none', .true.)
      call AddOrReplaceParameter(category, 'SedimentTransportRight', 'none', .true.)
      call AddOrReplaceParameter(category, 'ShieldsParameter', 'none', .true.)
      call AddOrReplaceParameter(category, 'MorWaterLevel', 'none', .true.)
      call AddOrReplaceParameter(category, 'MorVelocity', 'none', .true.)
      call AddOrReplaceParameter(category, 'MorWidth', 'none', .true.)
      call AddOrReplaceParameter(category, 'MorDepth', 'none', .true.)

      category = 'ResultsBranches'
      call AddOrReplaceParameter(category, 'Chezy', 'none', .true.)
      call AddOrReplaceParameter(category, 'Discharge', 'current', .true.)
      call AddOrReplaceParameter(category, 'Dispersion', 'none', .true.)
      call AddOrReplaceParameter(category, 'EnergyHeadMethod', '11', .true.)
      call AddOrReplaceParameter(category, 'Froude', 'none', .true.)
      call AddOrReplaceParameter(category, 'Fwind', 'none', .true.)
      call AddOrReplaceParameter(category, 'InfiltrationPipes', 'none', .true.)
      call AddOrReplaceParameter(category, 'Levelsoutputonpipes', '0', .true.)
      call AddOrReplaceParameter(category, 'RiverSubsectionParameters', 'current', .true.)
      call AddOrReplaceParameter(category, 'SedimentFrijlink', 'none', .true.)
      call AddOrReplaceParameter(category, 'SedimentVanRijn', 'none', .true.)
      call AddOrReplaceParameter(category, 'Twind', 'none', .true.)
      call AddOrReplaceParameter(category, 'Velocity', 'none', .true.)
      call AddOrReplaceParameter(category, 'Wind', 'none', .true.)
      call AddOrReplaceParameter(category, 'WaterLevelSlope', 'none', .true.)

      category = 'ResultsStructures'
      call AddOrReplaceParameter(category, 'CrestLevel', 'none', .true.)
      call AddOrReplaceParameter(category, 'CrestWidth', 'none', .true.)
      call AddOrReplaceParameter(category, 'Discharge', 'current', .true.)
      call AddOrReplaceParameter(category, 'GateLowerEdgeLevel', 'none', .true.)
      call AddOrReplaceParameter(category, 'Head', 'none', .true.)
      call AddOrReplaceParameter(category, 'OpeningsArea', 'none', .true.)
      call AddOrReplaceParameter(category, 'PressureDifference', 'none', .true.)
      call AddOrReplaceParameter(category, 'Velocity', 'none', .true.)
      call AddOrReplaceParameter(category, 'WaterLevel', 'none', .true.)
      call AddOrReplaceParameter(category, 'Waterleveloncrest', 'none', .true.)

      category = 'ResultsPumps'
      call AddOrReplaceParameter(category, 'PumpResults', 'none', .true.)

      category = 'ResultsWaterBalance'
      call AddOrReplaceParameter(category, '1d2dflows', 'none', .true.)

      category = 'ResultsGeneral'
      call AddOrReplaceParameter(category, 'ActualValue', 'current', .true.)
      call AddOrReplaceParameter(category, 'DelwaqNoStaggeredGrid', '0', .true.)
      call AddOrReplaceParameter(category, 'FlowAnalysisTimeSeries', '0', .true.)
      call AddOrReplaceParameter(category, 'MeanValue', '0', .true.)
      call AddOrReplaceParameter(category, 'MaximumValue', '0', .true.)
      call AddOrReplaceParameter(category, 'SobeksimStamp', '1', .true.)

      category = 'Sediment'
      call AddOrReplaceParameter(category, 'D50', '0.0005', .true.)
      call AddOrReplaceParameter(category, 'D90', '0.001', .true.)
      call AddOrReplaceParameter(category, 'DepthUsedForSediment', '0.3', .true.)

      category = 'Specials'
      call AddOrReplaceParameter(category, 'DesignFactorDLG', '1.0', .true.)

      category = 'Indication'
      call AddOrReplaceParameter(category, 'VelocityReachSegments', '0.5', .true.)
      call AddOrReplaceParameter(category, 'VelocityStructures', '0.75', .true.)

      category = 'NumericalParameters'
      call AddOrReplaceParameter(category, 'AccelerationTermFactor', '1.0', .true.)
      call AddOrReplaceParameter(category, 'AccurateVersusSpeed', '3', .true.)
      call AddOrReplaceParameter(category, 'CourantNumber', '1.0', .true.)
      call AddOrReplaceParameter(category, 'DtMinimum', '0.001', .true.)
      call AddOrReplaceParameter(category, 'EpsilonValueVolume', '0.0001', .true.)
      call AddOrReplaceParameter(category, 'EpsilonValueWaterDepth', '0.0001', .true.)
      call AddOrReplaceParameter(category, 'FloodingDividedByDrying', '10.0', .true.)
      call AddOrReplaceParameter(category, 'Gravity', '9.81', .true.)
      call AddOrReplaceParameter(category, 'MaxDegree', '6', .true.)
      call AddOrReplaceParameter(category, 'MaxIterations', '8', .true.)
      call AddOrReplaceParameter(category, 'MaxTimeStep', '0', .true.)  ! TODO: Default must be corrected when Time Chapter is available
      call AddOrReplaceParameter(category, 'MinimumSurfaceatStreet', '0.1', .true.)
      call AddOrReplaceParameter(category, 'MinimumSurfaceinNode', '0.1', .true.)
      call AddOrReplaceParameter(category, 'MinumumLength', '1.0', .true.)
      call AddOrReplaceParameter(category, 'RelaxationFactor', '1.0', .true.)
      call AddOrReplaceParameter(category, 'Rho', '1000', .true.)
      call AddOrReplaceParameter(category, 'StructureInertiaDampingFactor', '1.0', .true.)
      call AddOrReplaceParameter(category, 'Theta', '1.0', .true.)
      call AddOrReplaceParameter(category, 'ThresholdValueDrying', '0.001', .true.)
      call AddOrReplaceParameter(category, 'ThresholdValueFlooding', '0.01', .true.)
      call AddOrReplaceParameter(category, 'UseOmp', '0', .true.)
      call AddOrReplaceParameter(category, 'UseTimeStepReducerStructures', '0', .true.)

      category = 'SimulationOptions'
      call AddOrReplaceParameter(category, 'allowablelargertimestep', '0', .true.)
      call AddOrReplaceParameter(category, 'allowabletimesteplimiter', '30', .true.)
      call AddOrReplaceParameter(category, 'AllowableVolumeError', '10.0', .true.)
      call AddOrReplaceParameter(category, 'AllowCrestLevelBelowBottom', '0', .true.)
      call AddOrReplaceParameter(category, 'Cflcheckalllinks', '0', .true.)
      call AddOrReplaceParameter(category, 'Channel', '1', .true.)
      call AddOrReplaceParameter(category, 'Debug', '0', .true.)
      call AddOrReplaceParameter(category, 'DebugTime', '0', .true.)
      call AddOrReplaceParameter(category, 'DepthsBelowBobs', '0', .true.)
      call AddOrReplaceParameter(category, 'DumpInput', '0', .true.)
      call AddOrReplaceParameter(category, 'Iadvec1D', '2', .true.)
      call AddOrReplaceParameter(category, 'Jchecknans', '0', .true.)
      call AddOrReplaceParameter(category, 'LaboratoryTest', '0', .true.)
      call AddOrReplaceParameter(category, 'LaboratoryTimeStep', '1', .true.)
      call AddOrReplaceParameter(category, 'LaboratoryTotalStep', '1', .true.)
      call AddOrReplaceParameter(category, 'Limtyphu1D', '1', .true.)
      call AddOrReplaceParameter(category, 'LoggingLevel', 'info', .true.)
      call AddOrReplaceParameter(category, 'Manhloss', '0', .true.)
      call AddOrReplaceParameter(category, 'ManholeLosses', '0', .true.)
      call AddOrReplaceParameter(category, 'MissingValue', '-999.999', .true.)
      call AddOrReplaceParameter(category, 'Morphology', '0', .true.)
      call AddOrReplaceParameter(category, 'NoSuperCriticalInflow', '0', .true.)
      call AddOrReplaceParameter(category, 'PercentAllowableolumeError', '1.0', .true.)
      call AddOrReplaceParameter(category, 'PreissmannMinClosedManholes', '0.001', .true.)
      call AddOrReplaceParameter(category, 'ReadNetworkFromUgrid', '0', .true.)
      call AddOrReplaceParameter(category, 'River', '1', .true.)
      call AddOrReplaceParameter(category, 'Sewer', '0', .true.)
      call AddOrReplaceParameter(category, 'SiphonUpstreamThresholdSwitchOff', '0.1', .true.)
      call AddOrReplaceParameter(category, 'StrucAlfa', '0.9', .true.)
      call AddOrReplaceParameter(category, 'StrucFlowDirectionAccuracyFactor', '1.0', .true.)
      call AddOrReplaceParameter(category, 'StructureDynamicsFactor', '1.0', .true.)
      call AddOrReplaceParameter(category, 'StructureStabilityFactor', '0', .true.)
      call AddOrReplaceParameter(category, 'ThresholdForSummerDike', '0.4', .true.)
      call AddOrReplaceParameter(category, 'TimersOutputFrequency', '0', .true.)
      call AddOrReplaceParameter(category, 'use1d2dcoupling', '0', .true.)
      call AddOrReplaceParameter(category, 'UseEnergyHeadStructures', '0', .true.)
      call AddOrReplaceParameter(category, 'UseRestart', '0', .true.)
      call AddOrReplaceParameter(category, 'UseTimers', '0', .true.)
      call AddOrReplaceParameter(category, 'Usevariableteta', '0', .true.)
      call AddOrReplaceParameter(category, 'UseWlevStateFile', '0', .true.)
      call AddOrReplaceParameter(category, 'VolumeCheck', '0', .true.)
      call AddOrReplaceParameter(category, 'VolumeCorrection', '0', .true.)
      call AddOrReplaceParameter(category, 'WaterQualityInUse', '0', .true.)
      call AddOrReplaceParameter(category, 'WriteNetCDF', '0', .true.)
      call AddOrReplaceParameter(category, 'WriteRestart', '0', .true.)

      category = 'AdvancedOptions'
      call AddOrReplaceParameter(category, 'CacheMode', 'none', .true.)
      call AddOrReplaceParameter(category, 'CalculateDelwaqOutput', '0', .true.)
      call AddOrReplaceParameter(category, 'ExtraResistanceGeneralStructure', '0.0', .true.)
      call AddOrReplaceParameter(category, 'LateralLocation', '0', .true.)
      call AddOrReplaceParameter(category, 'Latitude', '52.00667', .true.)
      call AddOrReplaceParameter(category, 'Longitude', '4.35556', .true.)
      call AddOrReplaceParameter(category, 'FillCulvertsWithGL', '0', .true.)
      call AddOrReplaceParameter(category, 'MaxLoweringCrossAtCulvert', '0.0', .true.)
      call AddOrReplaceParameter(category, 'MaxVolFact', '0.9', .true.)
      call AddOrReplaceParameter(category, 'NoNegativeQlatWhenThereIsNoWater', '1', .true.)
      call AddOrReplaceParameter(category, 'TransitionHeightSD', '1.0', .true.)

      category = 'Salinity'
      call AddOrReplaceParameter(category, 'SaltComputation', '0', .true.)
      call AddOrReplaceParameter(category, 'DiffusionAtBoundaries', '0', .true.)

      category = 'NumericalOptions'
      call AddOrReplaceParameter(category, 'teta', '1.0', .true.)
      call AddOrReplaceParameter(category, 'tidalPeriod', '12.417', .true.)
      call AddOrReplaceParameter(category, 'maxMouthsPerBranch', '1', .true.)
      call AddOrReplaceParameter(category, 'advectionScheme', 'vanLeer-2', .true.)

      category = 'TransportComputation'
      call AddOrReplaceParameter(category, 'Temperature', 'false', .true.)
      call AddOrReplaceParameter(category, 'Density', 'eckart_modified', .true.)
      call AddOrReplaceParameter(category, 'HeatTransferModel', 'transport', .true.)

      category = 'Temperature'
      call AddOrReplaceParameter(category, 'SurfaceArea', '1.0d6', .true.)
      call AddOrReplaceParameter(category, 'AtmosphericPressure', '1.0d5', .true.)
      call AddOrReplaceParameter(category, 'DaltonNumber', '0.0013', .true.)
      call AddOrReplaceParameter(category, 'HeatCapacityWater', '3930', .true.)
      call AddOrReplaceParameter(category, 'StantonNumber', '0.0013', .true.)

      category = 'Observations'
      call AddOrReplaceParameter(category, 'interpolationType', 'nearest', .true.)
   
   end subroutine setModelParameterDefaults

   
   double precision function julian(iyear, imonth, iday, ihour, imin, isec)

      !***********************************************************************
      !
      !     Description of module :
      !
      !        This functions returns the so called Julian day of a date, or
      !        the value -1.0 if an error occurred.
      !
      !        The Julian day of a date is the number of days that has passed
      !        since January 1, 4712 BC at 12h00 ( Gregorian). It is usefull
      !        to compute differces between dates. ( See SUBROUTINE GREGOR
      !        for the reverse proces ).
      !
      !***********************************************************************
      !
      !     Name   Type            Description
      !     ------ -----           ---------------------------
      !     IYEAR  integer         Year   ( -4713-.. )
      !     IMONTH integer         Month  ( 1-12 )
      !     IDAY   integer         Day    ( 1-28,29,30 or 31 )
      !     IHOUR  integer         Hour   ( 0-23 )
      !     IMIN   integer         Minute ( 0-59 )
      !     ISEC   integer         Second ( 0-59 )

      !     Name   Type     Size   Description
      !     ------ -----    ------ ------------------------
      !     TEMP1  real*8   -      Temporary variable
      !     TEMP2  real*8   -      Temporary variable
      !     MONLEN integer  12     Length of month in days
      !
      !     Calls to : none
      !
      !***********************************************************************
      !
      !     Variables :
      !
      implicit none

      integer, intent(in)                 :: iyear
      integer, intent(in)                 :: imonth
      integer, intent(in)                 :: iday
      integer, intent(in)                 :: ihour
      integer, intent(in)                 :: imin
      integer, intent(in)                 :: isec
      integer, dimension(12)              :: monlen
      double precision                    :: temp1
      double precision                    :: temp2

      ! Initialize lenghts of months :
      data monlen / 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /

      if (( iyear  .lt. -4713 ) .or. ( imonth .lt.  1 ) .or.       &
            ( imonth .gt.    12 ) .or. ( iday   .lt.  1 ) .or.     &
            ( iday   .gt. monlen(imonth) ) .or.                    &
            ( ihour  .lt.     0 ) .or. ( ihour  .gt. 23 ) .or.     &
            ( imin   .lt.     0 ) .or. ( imin   .gt. 59 ) .or.     &
            ( isec   .lt.     0 ) .or. ( isec   .gt. 60 )) then
         julian = -1.0d0
      else
         temp1  = int ((imonth - 14.0) / 12.0)
         temp2  = iday - 32075.0 + &
                  int ( 1461.0 * ( iyear + 4800.0 + temp1 ) / 4.0 ) + &
                  int ( 367.0 * ( imonth - 2.0 - temp1 * 12.0 ) / 12.0 ) - &
                  int ( 3.0 * int ( ( iyear + 4900.0 + temp1 ) / 100.0 ) / &
                  4.0 )
         temp1  = float ( ihour ) * 3600.0 + &
                  float ( imin  ) *   60.0 + &
                  float ( isec  ) - 43200.0
         julian = temp2 + ( temp1 / 86400.0 )
      endif

   end function julian
   
   
end module m_readModelParameters
