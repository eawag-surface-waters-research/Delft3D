!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

!> @file
!!    Interface routines for Delta-Shell
!!
!!    Note:
!!    Not all routines can be in a module, they have to
!!    be callable from outside Fortran.
!<

!> Utilities for the routines here (effectively a private module)
module waq_omi_utils
    use m_dhopnf

    integer, parameter :: LEVEL_FATAL   = 1
    integer, parameter :: LEVEL_ERROR   = 2
    integer, parameter :: LEVEL_WARNING = 3
    integer, parameter :: LEVEL_INFO    = 4
    logical, save      :: reporting   = .false.
    integer, save      :: lunlst

    integer, save            :: msg_level = LEVEL_INFO
    character(len=200), save :: msg_text  = 'No message'

contains

!> Find a name in a list of names
subroutine find_index( name, list_names, idx )
    character(len=*)               :: name           !< Name to be found
    character(len=*), dimension(:) :: list_names     !< List of names to be searched
    integer                        :: idx            !< Index (-1 if name unknown)

    integer                        :: i

    idx = -1
    do i = 1,size(list_names)
        if ( name == list_names(i) ) then
            idx = i
            exit
        endif
    enddo

end subroutine find_index

!> Set a subparameter for the integration option (via a DELWAQ core routine)
!! Used here only
subroutine set_intopt( option, keyword_true, keyword_false )

    use m_sysi          ! Timer characteristics

    implicit none

    logical :: option                                  !< Selected value of the option
    character(len=*) :: keyword_true                   !< Keyword describing "true" value for the option
    character(len=*) :: keyword_false                  !< Keyword describing "false" value for the option

    integer :: lunut, ierr2

    lunut = 10
    if ( option ) then
        call dlwq0i( keyword_true, intopt, lunut, ierr2 )
    else
        call dlwq0i( keyword_false, intopt, lunut, ierr2 )
    endif
end subroutine set_intopt

!> Store an error message
!! Used here only
subroutine SetMessage( level, text )
    integer      :: level
    character(*) :: text

    msg_level = level
    msg_text  = text

end subroutine SetMessage

!> Write a one-dimensional, constant array to a work file
subroutine write_array_const( name, suffix, value, size )
    character(len=*)   :: name                         !< Name of the work files
    character(len=*)   :: suffix                       !< Suffix for this particular file
    real               :: value                        !< Constant value to be written
    integer            :: size                         !< Number of times the value must be repeated

    integer            :: i, lunwrk
    integer            :: time_dummy

    time_dummy = 0

    open( newunit = lunwrk, file = trim(name) // '-' // trim(suffix) // '.wrk', form = 'unformatted',access='stream' )
    write( lunwrk ) time_dummy, (value, i = 1,size )
    close( lunwrk )

end subroutine write_array_const

end module waq_omi_utils

!
! interface routines
!
module waq_omi_interface

contains
!> Return the last known message
logical function GetLastMessage( level, text )

    !DEC$ ATTRIBUTES DLLEXPORT::GetLastMessage
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETLASTMESSAGE' :: GetLastMessage

    use waq_omi_utils

    integer, intent(out)      :: level
    character(*), intent(out) :: text

    GetLastMessage = .true.
    level = msg_level
    text  = msg_text

end function GetLastMessage

logical function GetWQDimensions(notot, noseg)

    !DEC$ ATTRIBUTES DLLEXPORT::GetWQDimensions
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETWQDIMENSIONS' :: GetWQDimensions

    use delwaq2_global_data
    use m_sysi          ! Timer characteristics

    implicit none

    integer, intent(out)              :: notot         !< Number of substances
    integer, intent(out)              :: noseg         !< Number of segments

    notot = size_dlwq_state%notot
    noseg = size_dlwq_state%noseg

    GetWQDimensions = .true.

end function GetWQDimensions

!> Set the times for the simulation
logical function SetSimulationTimes(startTime, endTime, timeStep)

    !DEC$ ATTRIBUTES DLLEXPORT::SetSimulationTimes
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETSIMULATIONTIMES' :: SetSimulationTimes

    use delwaq2_global_data
    use m_sysi          ! Timer characteristics


    implicit none

    integer, intent(in)              :: startTime       !< Start time in seconds since the reference date/time
    integer, intent(in)              :: endTime         !< Stop time in seconds since the reference date/time
    integer, intent(in)              :: timeStep        !< Time step in seconds

    itstrt = startTime
    itstop = endTime
    idt    = timeStep

    isflag = 1

    SetSimulationTimes = .true.

end function SetSimulationTimes

!> Retrieve the times for the simulation
logical function GetSimulationTimes(startTime, endTime, timeStep)

    !DEC$ ATTRIBUTES DLLEXPORT::GetSimulationTimes
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETSIMULATIONTIMES' :: GetSimulationTimes

    use delwaq2_global_data
    use m_sysi          ! Timer characteristics


    implicit none

    integer, intent(out)              :: startTime       !< Start time in seconds since the reference date/time
    integer, intent(out)              :: endTime         !< Stop time in seconds since the reference date/time
    integer, intent(out)              :: timeStep        !< Time step in seconds

    startTime = itstrt
    endTime   = itstop
    timeStep  = idt

    GetSimulationTimes = .true.

end function GetSimulationTimes

!> Set time format in monitoring file
logical function SetTimeFormat(timeFormat)

    use m_sysi          ! Timer characteristics


    !DEC$ ATTRIBUTES DLLEXPORT::SetTimeFormat
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETTIMEFORMAT' :: SetTimeFormat

    implicit none

    integer, intent(in)              :: timeFormat      !< Time format (0 = integer, 1 = dd:hh:mm:ss, 2 = yy:ddd:hh:mm:ss)

    isflag = timeFormat

    SetTimeFormat = .true.

end function SetTimeFormat

!> Set the reference date (the so-called T0-string)
logical function SetReferenceDate( year_in, month_in, day_in, hour_in, minute_in, second_in )

    !DEC$ ATTRIBUTES DLLEXPORT::SetReferenceDate
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETREFERENCEDATE' :: SetReferenceDate

    use delwaq2_global_data

    integer, intent(in)             :: year_in
    integer, intent(in)             :: month_in
    integer, intent(in)             :: day_in
    integer, intent(in)             :: hour_in
    integer, intent(in)             :: minute_in
    integer, intent(in)             :: second_in

    ref_year    = year_in
    ref_month   = month_in
    ref_day     = day_in
    ref_hour    = hour_in
    ref_minute  = minute_in
    ref_second  = second_in

    SetReferenceDate = .true.

end function SetReferenceDate

! SetOutputTimers --
!     Set the timers for the output:
!     type: defines what type of output (1 = monitor, 2 = history, 3 = map)
!
logical function SetOutputTimers(type, startTime, endTime, timeStep)

    !DEC$ ATTRIBUTES DLLEXPORT::SetOutputTimers
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETOUTPUTTIMERS' :: SetOutputTimers

    use delwaq2_global_data
    use m_sysn
    use m_sysi

    implicit none

    integer, intent(in)              :: type
    integer, intent(in)              :: startTime
    integer, intent(in)              :: endTime
    integer, intent(in)              :: timeStep

    select case ( type )
        case( 1 )
            imstrt = startTime
            imstop = endTime
            imstep = timeStep
        case( 2 )
            ihstrt = startTime
            ihstop = endTime
            ihstep = timeStep
        case( 3 )
            idstrt = startTime
            idstop = endTime
            idstep = timeStep
        case default
    end select

    noutp = 9
    SetOutputTimers = .true.

end function SetOutputTimers

! SetAttributeInit --
!     Set an attribute
!
!     Note: use before ModelInitialize!
!
logical function SetAttributeInit(idx, ivalue)

    !DEC$ ATTRIBUTES DLLEXPORT::SetAttributeInit
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETATTRIBUTEINIT' :: SetAttributeInit

    use waq_omi_utils
    use delwaq2_global_data
    use m_sysn

    implicit none

    integer, intent(in)              :: idx
    integer, dimension(*), intent(in):: ivalue

    integer                          :: iseg !< segment number
    integer                          :: ilow !< divisor of this attribute
    integer                          :: iup  !< divisor of attributes with higher index
    integer                          :: i1   !< value of attributes with higher index
    integer                          :: i2   !< previous value of this attribute
    integer                          :: i3   !< value of attributes wih lower index

    SetAttributeInit = .false.

    if (idx<=0 .or. idx>10) then
       return
    endif

    ilow = 10**(idx-1)
    iup  = 10**idx

    do iseg = 1,noseg ! + nseg2 (segments in bed)
       i1 = (iknmrk(iseg)/iup)*iup
       i2 = ((iknmrk(iseg)-i1)/ilow)*ilow
       i3 = iknmrk(iseg)-i1-i2
       iknmrk(iseg) = i1 + ivalue(iseg)*ilow + i3
    enddo

    if ( reporting ) then
        write( lunlst, '(a25,i5)' ) 'Values set for attribute ',idx
    endif

    SetAttributeInit = .true.

end function SetAttributeInit

! SetCurrentValueScalarInit --
!     Set the current value of a substance or process parameter
!
!     Note: use before ModelInitialize!
!
logical function SetCurrentValueScalarInit(name, value)

    !DEC$ ATTRIBUTES DLLEXPORT::SetCurrentValueScalarInit
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETCURRENTVALUESCALARINIT' :: SetCurrentValueScalarInit

    use waq_omi_utils
    use delwaq2_global_data

    implicit none

    character(len=*), intent(in)     :: name
    real, intent(in)                 :: value

    integer                          :: idx

    SetCurrentValueScalarInit = .false.

    call find_index( name, substance_name, idx )
    if ( idx > 0 ) then
        substance_conc(idx,:) = value
    else
        call find_index( name, procparam_const, idx )
        if ( idx > 0 ) then
            procparam_const_value(idx) = value
        else
            call find_index( name, procparam_param, idx )
            if ( idx > 0 ) then
                procparam_param_value(idx,:) = value
            else
                call SetMessage(LEVEL_ERROR, &
                    'Name not found (not a substance or process parameter): ' // name)
                return
            endif
        endif
    endif

    if ( reporting ) then
        write( lunlst, '(3a,g14.5,a)' ) 'Initial value for substance ', trim(name), ' set to: ',value
    endif

    SetCurrentValueScalarInit = .true.

end function SetCurrentValueScalarInit

! SetCurrentValueFieldInit --
!     Set the current value of a process parameter (which varies per segment)
!
!     Note: use before ModelInitialize!
!
logical function SetCurrentValueFieldInit(name, value)

    !DEC$ ATTRIBUTES DLLEXPORT::SetCurrentValueFieldInit
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETCURRENTVALUEFIELDINIT' :: SetCurrentValueFieldInit

    use waq_omi_utils
    use delwaq2_global_data
    use m_sysn          ! System characteristics
    use m_sysa          ! Pointers in real array workspace

    implicit none

    character(len=*), intent(in)     :: name
    real, dimension(*),intent(in)    :: value

    integer                          :: idx

    SetCurrentValueFieldInit = .false.

    call find_index( name, substance_name, idx )
    if ( idx > 0 ) then
        substance_conc(idx,1:noseg) = value(1:noseg)
    else
        call find_index( name, procparam_param, idx )
        if ( idx > 0 ) then
            procparam_param_value(idx,1:noseg) = value(1:noseg)
        else
            call SetMessage(LEVEL_ERROR, &
                'Name not found (not a substance or process parameter): ' // name)
            return
        endif
    endif

    if ( reporting ) then
        write( lunlst, '(3a,g14.5,a)' ) 'Initial value for parameter ', trim(name), ' set (varying values)'
    endif

    SetCurrentValueFieldInit = .true.

end function SetCurrentValueFieldInit

! SetCurrentValueScalarRun --
!     Set the current value of a process parameter
!
!     Note: use after ModelInitialize!
!
logical function SetCurrentValueScalarRun(name, value)

    !DEC$ ATTRIBUTES DLLEXPORT::SetCurrentValueScalarRun
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETCURRENTVALUESCALARRUN' :: SetCurrentValueScalarRun

    use waq_omi_utils
    use delwaq2_global_data
    use m_sysn          ! System characteristics
    use m_sysa          ! Pointers in real array workspace

    implicit none

    character(len=*), intent(in)     :: name
    real, intent(in)                 :: value

    integer                          :: idx

    SetCurrentValueScalarRun = .false.

    call find_index( name, procparam_const, idx )
    if ( idx > 0 ) then
        dlwqd%rbuf(icons+idx-1) = value
    else
        call find_index( name, procparam_param, idx )
        if ( idx > 0 ) then
            dlwqd%rbuf(iparm+idx-1:iparm+idx-1+nopa*noseg-1:nopa) = value
        else
            call SetMessage(LEVEL_ERROR, &
                'Name not found (not a process parameter): ' // name)
            return
        endif
    endif

    if ( reporting ) then
        write( lunlst, '(3a,g14.5,a,i0)' ) 'Parameter ', trim(name), ' set to: ',value , ' at time = ', dlwqd%itime
    endif

    SetCurrentValueScalarRun = .true.

end function SetCurrentValueScalarRun

! SetCurrentValueFieldRun --
!     Set the current value of a process parameter (which varies per segment)
!
!     Note: use after ModelInitialize!
!
logical function SetCurrentValueFieldRun(name, value)

    !DEC$ ATTRIBUTES DLLEXPORT::SetCurrentValueFieldRun
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETCURRENTVALUEFIELDRUN' :: SetCurrentValueFieldRun

    use waq_omi_utils
    use delwaq2_global_data
    use m_sysn          ! System characteristics
    use m_sysa          ! Pointers in real array workspace


    implicit none

    character(len=*), intent(in)     :: name
    real, dimension(*),intent(in)    :: value


    integer                          :: idx

    SetCurrentValueFieldRun = .false.

    call find_index( name, procparam_param, idx )
    if ( idx > 0 ) then
        dlwqd%rbuf(iparm+idx-1:iparm+idx-1+nopa*noseg-1:nopa) = value(1:noseg)
    else
        call SetMessage(LEVEL_ERROR, &
            'Name not found (not a process parameter): ' // name)
        return
    endif

    if ( reporting ) then
        write( lunlst, '(3a,g14.5,a,i0)' ) 'Parameter ', trim(name), &
            '(values vary over the model area) set at time = ', dlwqd%itime
    endif

    SetCurrentValueFieldRun = .true.

end function SetCurrentValueFieldRun

! GetCurrentValue --
!     Get the current value of a substance or process parameter
!     for ALL segments. The array value is assumed to be large enough
!
logical function GetCurrentValue(name, value)

    !DEC$ ATTRIBUTES DLLEXPORT::GetCurrentValue
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETCURRENTVALUE' :: GetCurrentValue

    use waq_omi_utils
    use delwaq2_global_data
    use m_sysn          ! System characteristics
    use m_sysi          ! Timer characteristics
    use m_sysa          ! Pointers in real array workspace

    implicit none

    character(len=*), intent(in)     :: name
    real, dimension(*), intent(out)  :: value

    integer                          :: idx
    integer                          :: i

    GetCurrentValue = .false.

    call find_index( name, substance_name, idx )
    if ( idx > 0 ) then
        do i = 1,noseg
            value(i) = dlwqd%rbuf(iconc+idx-1+(i-1)*notot)
        enddo
    else
        call find_index( name, procparam_const, idx )
        if ( idx > 0 ) then
            do i = 1,noseg
                value(i) = dlwqd%rbuf(iconc+idx-1+(i-1)*notot)
            enddo
        else
            call SetMessage(LEVEL_ERROR, &
                'Name not found (not a substance of process parameter): ' // name)
            return
        endif
    endif

    GetCurrentValue = .true.

end function GetCurrentValue

! SetIntegrationOptions --
!     Set the integration option (and all its subparameters)
!
logical function SetIntegrationOptions(method, disp_flow_zero, disp_bound, first_order, forester, anticreep)
    !DEC$ ATTRIBUTES DLLEXPORT::SetIntegrationOptions
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETINTEGRATIONOPTIONS' :: SetIntegrationOptions

    use waq_omi_utils
    use delwaq2_global_data
    use m_sysi

    implicit none

    integer, intent(in)              :: method
    logical, intent(in)              :: disp_flow_zero
    logical, intent(in)              :: disp_bound
    logical, intent(in)              :: first_order
    logical, intent(in)              :: forester
    logical, intent(in)              :: anticreep

    intsrt = method
    intopt = 0

    call set_intopt( disp_flow_zero, 'DISP-AT-NOFLOW', 'NODISP-AT-NOFLOW' )
    call set_intopt( disp_bound,     'DISP-AT-BOUND' , 'NODISP-AT-BOUND'  )
    call set_intopt( first_order,    'LOWER-ORDER-AT-BOUND' , 'HIGHER-ORDER-AT-BOUND'  )
    call set_intopt( forester,       'FORESTER'      , 'NO-FORESTER'      )
    call set_intopt( anticreep,      'ANTICREEP'     , 'NO-ANTICREEP'     )

    SetIntegrationOptions = .true.

end function SetIntegrationOptions

! TODO: local theta!
! TODO: anti-diffusion, scheme 15 unstructured


! SetBalanceOutputOptions --
!     Set the output options for balances
!
logical function SetBalanceOutputOptions(type, lump_processes, lump_loads, lump_transport, suppress_space, suppress_time, unit_type)
    !DEC$ ATTRIBUTES DLLEXPORT::SetBalanceOutputOptions
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETBALANCEOUTPUTOPTIONS' :: SetBalanceOutputOptions

    use waq_omi_utils
    use delwaq2_global_data
    use m_sysi

    implicit none

    integer, intent(in)              :: type
    logical, intent(in)              :: lump_processes
    logical, intent(in)              :: lump_loads
    logical, intent(in)              :: lump_transport
    logical, intent(in)              :: suppress_space
    logical, intent(in)              :: suppress_time
    integer, intent(in)              :: unit_type

    select case ( type )
        case (1,2)
            intopt = intopt + 8 + 16
        case (3)
            intopt = intopt + 8 + 32
        case default
            ! Ignore - defaults to no balance
    end select

    call set_intopt( lump_processes, 'BAL_LUMPPROCESSES', 'BAL_NOLUMPPROCESSES' )
    call set_intopt( lump_loads,     'BAL_LUMPLOADS',     'BAL_NOLUMPLOADS'     )
    call set_intopt( lump_transport, 'BAL_LUMPTRANSPORT', 'BAL_NOLUMPTRANSPORT' )
    call set_intopt( suppress_space, 'BAL_SUPPRESSSPACE', 'BAL_NOSUPPRESSSPACE' )
    call set_intopt( suppress_time,  'BAL_SUPPRESSTIME' , 'BAL_NOSUPPRESSTIME'  )
    if ( unit_type == 0 ) then
        ! This is the default - there is no keyword for it
    endif
    if ( unit_type == 1 ) then
        call set_intopt( .true.,  'BAL_UNITAREA'   , 'xxxxxxxxxxxxxxxxxx'  )
    endif
    if ( unit_type == 2 ) then
        call set_intopt( .true.,  'BAL_UNITVOLUME' , 'xxxxxxxxxxxxxxxxxx'  )
    endif

    SetBalanceOutputOptions = .true.

end function SetBalanceOutputOptions

! DefineWQSchematisation --
!     Define the number of segments and the pointer table
!
logical function DefineWQSchematisation(number_segments, pointer_table, number_exchanges)
    !DEC$ ATTRIBUTES DLLEXPORT::DefineWQSchematisation
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'DEFINEWQSCHEMATISATION' :: DefineWQSchematisation

    use delwaq2_global_data
    use m_sysn

    implicit none

    integer, intent(in)                  :: number_segments
    integer, dimension(4)                :: number_exchanges
    integer, dimension(4,1:sum(number_exchanges)), intent(in)  :: pointer_table

    integer                              :: number_layers
    integer                              :: number_segments_per_layer
    integer                              :: i, j
    integer                              :: lunwrk

    number_layers = 1
    if (number_exchanges(3) > 0) then
       number_segments_per_layer = number_segments - number_exchanges(3)
       if (number_segments_per_layer > 0) then
          number_layers = number_segments/number_segments_per_layer
          if ( number_layers * number_segments_per_layer /= number_segments ) then
              number_layers = 1
          endif
       endif
    endif

    open( newunit = lunwrk, file = trim(runid) // '-to_from.wrk', form = 'unformatted',access='stream' )
    write( lunwrk ) pointer_table(:,1:sum(number_exchanges))
    close( lunwrk )

    noseg  = number_segments
    noq    = sum(number_exchanges)
    noq1   = number_exchanges(1)
    noq2   = number_exchanges(2)
    noq3   = number_exchanges(3)
    noq4   = number_exchanges(4)
    nolay  = number_layers
    if (noq.gt.0) then
        nobnd  = -minval(pointer_table(:,1:noq))
    else
        nobnd  = 0
    end if
    nobtyp = nobnd

    !
    ! determine nomat (actually only needed if intsrt in [15:18, 21, 22] but
    ! the numerical solver may not have been set and at a later time we don't
    ! have access to the pointer_table anymore)
    !
    call dlwq0f(noq1, noq2, noq3, noseg, pointer_table, nomat)

    if ( allocated( ipoint )        ) deallocate( ipoint        )
    if ( allocated( iknmrk )        ) deallocate( iknmrk        )
    if ( allocated( boundary_id )   ) deallocate( boundary_id   )
    if ( allocated( boundary_name ) ) deallocate( boundary_name )
    if ( allocated( boundary_type ) ) deallocate( boundary_type )
    if ( allocated( ibpnt_array )   ) deallocate( ibpnt_array   )

    allocate( ipoint(4,noq) )
    ipoint = pointer_table(1:4,1:noq)

    allocate( iknmrk(1:noseg) ) ! actually noseg+nseg2 (segments in the bed)
    iknmrk = 0

    allocate( boundary_id(1:nobnd) )
    allocate( boundary_name(1:nobnd) )
    allocate( boundary_type(1:nobnd) )
    allocate( ibpnt_array(4,nobnd) )
    boundary_id   = 'Dummy id'
    boundary_name = 'Dummy name'
    boundary_type = 'Dummy type'

    ibpnt_array      = 0
    ibpnt_array(1,:) = 0                     ! Time lags

    do i = 1,noq
        if ( pointer_table(1,i) < 0 .and. pointer_table(2,i) > 0 ) then
            j = -pointer_table(1,i)
            ibpnt_array(2,j) = i
            ibpnt_array(3,j) = pointer_table(2,i)
        endif
        if ( pointer_table(1,i) > 0 .and. pointer_table(2,i) < 0 ) then
            j = -pointer_table(2,i)
            ibpnt_array(2,j) = -i
            ibpnt_array(3,j) = pointer_table(1,i)
        endif
    enddo

    DefineWQSchematisation = .true.

end function DefineWQSchematisation

! DefineWQDispersion --
!     Define the dispersion coefficients and dispersion lengths
!
!     Note:
!     Use after DefineWQSchematisation
!
logical function DefineWQDispersion(dispc, length)
    !DEC$ ATTRIBUTES DLLEXPORT::DefineWQDispersion
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'DEFINEWQDISPERSION' :: DefineWQDispersion

    use delwaq2_global_data
    use m_sysn

    implicit none

    real, dimension(3), intent(in)      :: dispc
    real, dimension(2,noq)              :: length

    integer                             :: time_dummy
    integer                             :: lunwrk

    time_dummy = 0

    open( newunit = lunwrk, file = trim(runid) // '-lengthes.wrk', form = 'unformatted',access='stream' )
    write( lunwrk ) time_dummy, length
    close( lunwrk )

    disp = dispc

    DefineWQDispersion = .true.

end function DefineWQDispersion

! SetWQProcessDefinition --
!     Set the mode and the process definition file
!
logical function SetProcessDefinition(mode, procdef_file)
    !DEC$ ATTRIBUTES DLLEXPORT::SetProcessDefinition
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETPROCESSDEFINITION' :: SetProcessDefinition

    character(len=*)                 :: mode
    character(len=*)                 :: procdef_file

    SetProcessDefinition = SetProcessDefinitionCore(mode, procdef_file, ' ' )

end function SetProcessDefinition

! SetWQProcessDefinitionX --
!     Set the mode, the process definition file and sfrac options file
!
logical function SetProcessDefinitionX(mode, procdef_file, sfrac_file )
    !DEC$ ATTRIBUTES DLLEXPORT::SetProcessDefinitionX
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETPROCESSDEFINITIONX' :: SetProcessDefinitionX

    character(len=*)                 :: mode
    character(len=*)                 :: procdef_file
    character(len=*)                 :: sfrac_file

    SetProcessDefinitionX = SetProcessDefinitionCore(mode, procdef_file, sfrac_file )

end function SetProcessDefinitionX

! SetWQProcessDefinitionCore --
!     Set the mode, the process definition file and sfrac options file
!
logical function SetProcessDefinitionCore(mode, procdef_file, sfrac_file )

    use delwaq2_global_data

    character(len=*)                 :: mode
    character(len=*)                 :: procdef_file
    character(len=*)                 :: sfrac_file

    if ( allocated( argv ) ) then
        deallocate( argv )
    endif

    if (sfrac_file == ' ') then

       allocate( argv(5) )

    else

       allocate( argv(7) )
       argv(6) = '-sfrac'
       argv(7) = sfrac_file

    endif

    argv(1) = 'dlwqlib.dll'
    argv(2) = runid
    argv(3) = mode
    argv(4) = '-p'
    argv(5) = procdef_file

    ! TODO: check the values

    SetProcessDefinitionCore = .true.

end function SetProcessDefinitionCore

! DefineWQProcesses --
!     Define the substances, process parameters etc.

logical function DefineWQProcesses(substance, number_substances, number_transported, &
                                   process_parameter, number_parameters, &
                                   process, number_processes)
    !DEC$ ATTRIBUTES DLLEXPORT::DefineWQProcesses
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'DEFINEWQPROCESSES' :: DefineWQProcesses

    implicit none

    integer, intent(in)                                   :: number_substances
    character(len=*), dimension(number_substances)        :: substance
    integer, intent(in)                                   :: number_transported
    integer, intent(in)                                   :: number_parameters
    character(len=*), dimension(number_parameters)        :: process_parameter
    integer, intent(in)                                   :: number_processes
    character(len=*), dimension(number_processes)         :: process

    integer, dimension(number_substances)                 :: substance_mult

    substance_mult = 1

    DefineWQProcesses = DefineWQProcessesCore(substance, substance_mult, &
                                   number_substances, number_transported, &
                                   process_parameter, number_parameters, &
                                   (/ ' ' /), 0, &
                                   process, number_processes)

end function DefineWQProcesses

! DefineWQProcessesX --
!     Define the substances, multiplicity, process parameters etc.
!
logical function DefineWQProcessesX(substance, substance_mult, &
                                   number_substances, number_transported, &
                                   process_parameter, number_parameters, &
                                   field_parameter, number_fields, &
                                   process, number_processes)
    !DEC$ ATTRIBUTES DLLEXPORT::DefineWQProcessesX
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'DEFINEWQPROCESSESX' :: DefineWQProcessesX

    implicit none

    character(len=*), dimension(*)   :: substance
    integer, dimension(*)            :: substance_mult
    integer, intent(in)              :: number_substances
    integer, intent(in)              :: number_transported
    character(len=*), dimension(*)   :: process_parameter
    integer, intent(in)              :: number_parameters
    character(len=*), dimension(*)   :: field_parameter
    integer, intent(in)              :: number_fields
    character(len=*), dimension(*)   :: process
    integer, intent(in)              :: number_processes

    DefineWQProcessesX = DefineWQProcessesCore(substance, substance_mult, &
                                   number_substances, number_transported, &
                                   process_parameter, number_parameters, &
                                   field_parameter, number_fields, &
                                   process, number_processes)

end function DefineWQProcessesX

! DefineWQProcessesCore --
!     Define the substances, multiplicity, process parameters etc.
!
!     TODO: DELWAQ parameters (now: everything is considered a constant)
!           Output parameters
!
logical function DefineWQProcessesCore(substance, substance_mult, &
                                   number_substances, number_transported, &
                                   process_parameter, number_parameters, &
                                   field_parameter, number_fields, &
                                   process, number_processes)

    use delwaq2_global_data
    use m_sysn

    implicit none

    integer, intent(in)              :: number_substances
    integer, intent(in)              :: number_parameters
    integer, intent(in)              :: number_transported
    integer, intent(in)              :: number_processes
    integer, intent(in)              :: number_fields

    character(len=*), dimension(number_substances)   :: substance
    character(len=*), dimension(number_processes)   :: process
    character(len=*), dimension(number_parameters)   :: process_parameter
    character(len=*), dimension(number_fields)   :: field_parameter

    integer, dimension(number_substances)            :: substance_mult

    integer                          :: numsubstot
    integer                          :: numsubsact
    integer                          :: i
    character(len=3)                 :: numstr
    integer                          :: j

    if ( allocated(substance_name)        ) deallocate( substance_name        )
    if ( allocated(mult)                  ) deallocate( mult                  )
    if ( allocated(substance_conc)        ) deallocate( substance_conc        )
    if ( allocated(procparam_const)       ) deallocate( procparam_const       )
    if ( allocated(procparam_param)       ) deallocate( procparam_param       )
    if ( allocated(procparam_const_value) ) deallocate( procparam_const_value )
    if ( allocated(procparam_param_value) ) deallocate( procparam_param_value )

    numsubstot = 0
    numsubsact = 0
    nomult = 0
    do i = 1,number_substances
        if (substance_mult(i)>1) then
            nomult = nomult+1
        endif
        numsubstot = numsubstot + substance_mult(i)
        if (i<=number_transported) then
            numsubsact = numsubsact + substance_mult(i)
        endif
    enddo

    allocate( substance_name(numsubstot) )
    allocate( mult(2,nomult) )
    allocate( substance_conc(numsubstot,noseg) )
    allocate( procparam_const(number_parameters+number_processes+1) )
    allocate( procparam_const_value(number_parameters+number_processes+1) )
    allocate( procparam_param(number_fields) )
    allocate( procparam_param_value(number_fields,noseg) )

    numsubstot = 0
    nomult = 0
    do i = 1, number_substances
        if (substance_mult(i)==1) then
            numsubstot = numsubstot+1
            substance_name(numsubstot) = substance(i)
        else
            nomult = nomult+1
            mult(1,nomult) = numsubstot+1
            mult(2,nomult) = numsubstot+substance_mult(i)
            do j = 1,substance_mult(i)
                write(numstr,'(I0.2)') j
                substance_name(numsubstot+j) = trim(substance(i))//numstr
            enddo
            numsubstot = numsubstot+substance_mult(i)
        endif
    enddo
    substance_conc = 0.0

    procparam_const(1:number_parameters)        = process_parameter(1:number_parameters)
    procparam_const_value(1:number_parameters)  = -999.0
    procparam_const(number_parameters+1)        = 'ONLY_ACTIVE'
    procparam_const(number_parameters+2:)       = (/ ('ACTIVE_'// process(i) ,i=1,number_processes) /)
    procparam_const_value(number_parameters+1:) = 1.0
    procparam_param                             = field_parameter(1:number_fields)
    procparam_param_value                       = 0.0

    nosys  = numsubsact
    notot  = numsubstot
    nototp = numsubstot        ! Particles not supported yet
    nocons = number_parameters + number_processes + 1
    nopa   = number_fields
    nofun  = 0
    nosfun = 0

!   administrate state sizes for OpenDA use
    size_dlwq_state%notot  = notot
    size_dlwq_state%noseg  = noseg
    size_dlwq_state%conc  = notot*noseg
    size_dlwq_state%other  = 1 ! todo: set this to zero?
    size_dlwq_state%core  = size_dlwq_state%conc + size_dlwq_state%other

    size_dlwq_state%rbuf  = 0   !not known at this time. TODO: Will this cause problems???
    size_dlwq_state%mass   = notot*noseg
    size_dlwq_state%names  = notot
    size_dlwq_state%timeadmin = 3
    size_dlwq_state%pseudo = size_dlwq_state%rbuf + size_dlwq_state%mass + size_dlwq_state%names + size_dlwq_state%timeadmin

    size_dlwq_state%output = 9 + 7*noutp
    size_dlwq_state%total  = size_dlwq_state%core + size_dlwq_state%pseudo + size_dlwq_state%output

    write (*,*) 'state sizes have been set in DefineWQProcessesCore'


    DefineWQProcessesCore = .true.

end function DefineWQProcessesCore

! DefineWQExtraOutputParameters --
!     Define extra output parameters
!
logical function DefineWQExtraOutputParameters(extra_output, number_output)
    !DEC$ ATTRIBUTES DLLEXPORT::DefineWQExtraOutputParameters
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'DEFINEWQEXTRAOUTPUTPARAMETERS' :: DefineWQEXTRAOUTPUTPARAMETERS

    use delwaq2_global_data

    implicit none

    integer                        :: number_output
    character(len=*), dimension(number_output) :: extra_output



    if ( allocated( output_param ) ) deallocate( output_param )

    allocate( output_param(1:number_output) )

    output_param = extra_output(1:number_output)

    DefineWQExtraOutputParameters = .true.

end function DefineWQExtraOutputParameters


! DefineDischargeLocations --
!     Define the location of discharges
!
!     Note:
!     For the moment there is no support for different types of waste loads
!     or for names
!
logical function DefineDischargeLocations(cell, number_loads)
    !DEC$ ATTRIBUTES DLLEXPORT::DefineDischargeLocations
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'DEFINEDISCHARGELOCATIONS' :: DefineDischargeLocations

    use waq_omi_utils
    use delwaq2_global_data
    use m_sysn

    implicit none


    integer, intent(in)              :: number_loads
    integer, dimension(number_loads) :: cell

    integer                          :: i
    character(len=100)               :: message

    DefineDischargeLocations = .false.

    do i = 1,number_loads
        if ( cell(i) < 1 .or. cell(i) > noseg ) then
            write( message, '(a,i0,a)' ) 'Discharge location out of range: ', cell(i), &
                ' - should be between 1 and the number of segments'
            call SetMessage( LEVEL_ERROR, message )
            return
        endif
    enddo

    if ( allocated( load_name )     ) deallocate( load_name     )
    if ( allocated( load_cell )     ) deallocate( load_cell     )
    if ( allocated( load_type )     ) deallocate( load_type     )
    if ( allocated( load_type_def ) ) deallocate( load_type_def )

    allocate( load_name(number_loads) )
    allocate( load_cell(number_loads) )
    allocate( load_type(number_loads) )
    allocate( load_type_def(1) )

    load_type_def = 'Default'

    do i = 1,number_loads
        write( load_name(i), '(a,i0)' ) 'LOAD ', i
        load_cell(i) = cell(i)
        load_type(i)(1:20) = load_name(i)(1:20)
    enddo

    nowst  = number_loads
    nowtyp = 1

    DefineDischargeLocations = .true.

end function DefineDischargeLocations

! DefineMonitoringLocations --
!     Define the location of monitoring points and areas
!
!     Note:
!     For the moment there is no support for monitoring areas or transects
!
logical function DefineMonitoringLocations(cell, name, number_monitoring)
    !DEC$ ATTRIBUTES DLLEXPORT::DefineMonitoringLocations
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'DEFINEMONITORINGLOCATIONS' :: DefineMonitoringLocations

    use waq_omi_utils
    use delwaq2_global_data
    use m_sysn

    implicit none



    integer, intent(in)              :: number_monitoring
    character(len=*), dimension(number_monitoring)   :: name
    integer, dimension(number_monitoring)            :: cell

    integer                          :: i
    character(len=100)               :: message

    DefineMonitoringLocations = .false.

    do i = 1,number_monitoring
        if ( cell(i) < 1 .or. cell(i) > noseg ) then
            write( message, '(a,i0,a)' ) 'Monitoring location out of range: ', cell(i), &
                ' - should be between 1 and the number of segments'
            call SetMessage( LEVEL_ERROR, message )
            return
        endif
    enddo

    if ( allocated(monitor_name)      ) deallocate( monitor_name      )
    if ( allocated(monitor_cell)      ) deallocate( monitor_cell      )
    if ( allocated(cells_per_monitor) ) deallocate( cells_per_monitor )
    if ( allocated(transect_name)     ) deallocate( transect_name     )

    allocate( monitor_name(number_monitoring) )
    allocate( monitor_cell(number_monitoring) )
    allocate( cells_per_monitor(number_monitoring) )
    allocate( transect_name(1) )

    monitor_name      = name(1:number_monitoring)
    monitor_cell      = cell(1:number_monitoring)
    cells_per_monitor = 1

    nodump            = 0
    ndmpar            = number_monitoring

    DefineMonitoringLocations = .true.

end function DefineMonitoringLocations

! SetInitialVolume
!     Set the initial volume for all segments
!
!     Note: use before ModelInitialize
!
logical function SetInitialVolume( volume )

    !DEC$ ATTRIBUTES DLLEXPORT::SetInitialVolume
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETINITIALVOLUME' :: SetInitialVolume

    use waq_omi_utils
    use delwaq2_global_data
    use m_sysn          ! System characteristics
    use m_sysi          ! Timer characteristics
    use m_sysa          ! Pointers in real array workspace

    implicit none

    real, dimension(noseg), intent(in) :: volume

    integer                            :: time_dummy
    integer                            :: lunwrk

    SetInitialVolume = .false.

    time_dummy = 0
    open( newunit = lunwrk, file = trim(runid) // '-volumes.wrk' , form = 'unformatted',access='stream', err=911 )
    write( lunwrk ) time_dummy, volume(1:noseg)
    close( lunwrk )

    SetInitialVolume = .true.
    return

 911 write (*,*) 'setinitialvolume: error!!! '

end function SetInitialVolume

! SetFlowData --
!     Set the current volumes, areas and flows
!
!     Note: use before ModelPerformTimeStep, after ModelInitialize
!
logical function SetFlowData( volume, area, flow )

    !DEC$ ATTRIBUTES DLLEXPORT::SetFlowData
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETFLOWDATA' :: SetFlowData

    use waq_omi_utils
    use delwaq2_global_data
    use m_sysn          ! System characteristics
    use m_sysi          ! Timer characteristics
    use m_sysa          ! Pointers in real array workspace

    implicit none

    real, dimension(noseg), intent(in) :: volume
    real, dimension(noq), intent(in)   :: area
    real, dimension(noq), intent(in)   :: flow


    SetFlowData = .false.

    dlwqd%rbuf(ivol2:ivol2+noseg-1) = volume
    dlwqd%rbuf(iarea:iarea+noq-1) = area
    dlwqd%rbuf(iflow:iflow+noq-1) = flow

    SetFlowData = .true.

end function SetFlowData

! SetFlowData --
!     Set the current volumes only
!
!     Note: use before ModelPerformTimeStep, after ModelInitialize
!
logical function SetFlowDataVolume( volume )

    !DEC$ ATTRIBUTES DLLEXPORT::SetFlowDataVolume
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETFLOWDATAVOLUME' :: SetFlowDataVolume

    use waq_omi_utils
    use delwaq2_global_data
    use m_sysn          ! System characteristics
    use m_sysi          ! Timer characteristics
    use m_sysa          ! Pointers in real array workspace

    implicit none

    real, dimension(noseg), intent(in) :: volume

    SetFlowDataVolume = .false.

    dlwqd%rbuf(ivol2:ivol2+noseg-1) = volume

    SetFlowDataVolume = .true.

end function SetFlowDataVolume

logical function SetFlowDataVelocity( velocity )

    !DEC$ ATTRIBUTES DLLEXPORT::SetFlowDataVelocity
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETFLOWDATAVELOCITY' :: SetFlowDataVelocity

    use waq_omi_utils
    use delwaq2_global_data
    use m_sysn          ! System characteristics
    use m_sysi          ! Timer characteristics
    use m_sysa          ! Pointers in real array workspace

    implicit none

    real, dimension(noq), intent(in)   :: velocity

    SetFlowDataVelocity = .false.

    dlwqd%rbuf(ivelo:ivelo+noq-1) = velocity

    SetFlowDataVelocity = .true.

end function SetFlowDataVelocity

! CorrectVolumeSurface --
!     Correct the mass for all substances via new volumes
!     and horizontal surface areas
!
!     Note: use once after ModelInitialize or ModelInitialize_by_Id
!
!     Note: the initial volume and the horizontal surface area
!           should NOT be zero
!
integer function CorrectVolumeSurface( volume, surf, mass_per_m2 )

    !DEC$ ATTRIBUTES DLLEXPORT::CorrectVolumeSurface
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'CORRECTVOLUMESURFACE' :: CorrectVolumeSurface

    use waq_omi_utils
    use delwaq2_global_data
    use m_sysn          ! System characteristics
    use m_sysi          ! Timer characteristics
    use m_sysa          ! Pointers in real array workspace

    implicit none

    real, dimension(noseg), intent(in) :: volume
    real, dimension(noseg), intent(in) :: surf
    integer                            :: mass_per_m2

    integer                            :: error_count
    integer                            :: iseg, isys, isurf, ioff, ip
    integer                            :: nosubs
    real                               :: ratio

    CorrectVolumeSurface = 1

    !
    ! The option determines if the correction for inactive substances
    ! should be done via the volume or the surface area
    !
    if ( mass_per_m2 /= 0 ) then
        nosubs = nosys
    else
        nosubs = notot
    endif

    error_count = 0

    call find_index( 'SURF      ', procparam_param, isurf )

    do iseg = 1,noseg
        ioff  = imass + (iseg-1)*notot - 1
        ip    = iparm + (iseg-1)*nopa  + isurf - 1

        if ( abs(dlwqd%rbuf(ivol+iseg-1)) > 1.0e-20 ) then
            ratio = volume(iseg) / dlwqd%rbuf(ivol+iseg-1)
            do isys = 1,nosubs
                dlwqd%rbuf(ioff+isys) = dlwqd%rbuf(ioff+isys) * ratio
            enddo
            dlwqd%rbuf(ivol+iseg-1) = volume(iseg)
        else
            error_count = error_count + 1
        endif

        if ( isurf > 0 ) then
            if ( abs(dlwqd%rbuf(ip)) > 1.0e-20 ) then
                ratio = surf(iseg) / dlwqd%rbuf(ip)
                do isys = nosubs+1,notot
                    dlwqd%rbuf(ioff+isys) = dlwqd%rbuf(ioff+isys) * ratio
                enddo
            else
                error_count = error_count + 1
            endif
        endif
    enddo

    if ( error_count /= 0 ) then
        CorrectVolumeSurface = 0
        write(*,*) 'Number of segments with zero volume or surface area:', error_count
    else
        CorrectVolumeSurface = 1
    endif

end function CorrectVolumeSurface

! SetWasteLoadValues --
!     Set the current values for a single waste load
!
!     Note: use before ModelPerformTimeStep, after ModelInitialize
!
logical function SetWasteLoadValues( idx, value )

    !DEC$ ATTRIBUTES DLLEXPORT::SetWasteLoadValues
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETWASTELOADVALUES' :: SetWasteLoadValues

    use waq_omi_utils
    use delwaq2_global_data
    use m_sysn          ! System characteristics
    use m_sysi          ! Timer characteristics
    use m_sysa          ! Pointers in real array workspace

    implicit none

    integer                              :: idx
    real, dimension(notot+1), intent(in) :: value
    character(len=20)                    :: string
    integer                              :: i
    integer                              :: i2
    integer                              :: j

    SetWasteLoadValues = .false.

    if ( idx < 1 .or. idx > nowst ) then
        write( string, '(i0)' ) idx
        call SetMessage(LEVEL_ERROR, &
            'Waste load index out of range - index: ' // string )
        return
    endif

    dlwqd%rbuf(iwste+(idx-1)*(notot+1):iwste+idx*(notot+1)-1) = value

    if ( reporting ) then
        write( lunlst, '(a,i5,a,i10)' ) 'Waste loads for discharge ', idx, ' - at time: ', dlwqd%itime
        string = 'Flow rate'
        do i = 0,nosys,5
            i2 = min( i+4, nosys )
            if ( i == 0 ) then
                write( lunlst, '(5a20)'   ) adjustr(string), (adjustr(substance_name(j)) ,j=i+1,i2)
            else
                write( lunlst, '(5a20)'   ) (adjustr(substance_name(j)) ,j=i,i2)
            endif
            write( lunlst, '(5g20.5)' ) (value(j) ,j=i+1,i2+1)
        enddo
    endif

    SetWasteLoadValues = .true.

end function SetWasteLoadValues

! SetBoundaryConditions --
!     Set the current values for all boundary cells of a given type
!
!     Note: use before ModelPerformTimeStep, after ModelInitialize
!
!     TODO:
!     Make the index work correctly
!
logical function SetBoundaryConditions( idx, value )

    !DEC$ ATTRIBUTES DLLEXPORT::SetBoundaryConditions
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETBOUNDARYCONDITIONS' :: SetBoundaryConditions

    use waq_omi_utils
    use delwaq2_global_data
    use m_sysn          ! System characteristics
    use m_sysi          ! Timer characteristics
    use m_sysa          ! Pointers in real array workspace

    implicit none

    integer                            :: idx
    real, dimension(nosys), intent(in) :: value
    character(len=20)                  :: string
    integer                            :: i
    integer                            :: i2
    integer                            :: j

    SetBoundaryConditions = .false.

    if ( idx < 1 .or. idx > nobnd ) then
        write( string, '(i0)' ) idx
        call SetMessage(LEVEL_ERROR, &
            'Boundary index out of range - index: ' // string )
        return
    endif

    dlwqd%rbuf(ibset+(idx-1)*nosys:ibset+idx*nosys-1) = value

    if ( reporting ) then
        write( lunlst, '(a,i5,a,i10)' ) 'Conditions for boundary cell ', idx, ' - at time: ', dlwqd%itime
        do i = 1,nosys,5
            i2 = min( i+4, nosys )
            write( lunlst, '(5a20)'   ) (adjustr(substance_name(j)) ,j=i,i2)
            write( lunlst, '(5g20.5)' ) (value(j) ,j=i,i2)
        enddo
    endif

    SetBoundaryConditions = .true.

end function SetBoundaryConditions

! ModelPerformTimeStep --
!     Set a single time step
!
integer function ModelPerformTimeStep ()
    !DEC$ ATTRIBUTES DLLEXPORT::ModelPerformTimeStep
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'MODELPERFORMTIMESTEP' :: ModelPerformTimeStep

    use delwaq2_global_data
    use m_actions

    implicit none


    character(len=20), dimension(0) :: argv_dummy

    call dlwqmain( ACTION_SINGLESTEP, 0, argv_dummy, dlwqd )

    ModelPerformTimeStep = 0

end function ModelPerformTimeStep

! WriteRestartFileDefaultName --
!     Write a restart file in .map format using default filename with current status of the model
!
integer function WriteRestartFileDefaultName ()
    !DEC$ ATTRIBUTES DLLEXPORT::WriteRestartFileDefaultName
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'WRITERESTARTFILEDEFAULTNAME' :: WriteRestartFileDefaultName

    use delwaq2_global_data

    implicit none

    character (len=255) lcharmap
    integer    i, k, ierr, found

    lcharmap = lchar(23)
    found = 0
    do i=248,1,-1
      if ( lcharmap(i:i) == '.' .and. found == 0 ) then
        lcharmap(i:i+7) = "_res.map"
        found = 1
      endif
    end do
    if ( found == 0) then
      write ( * , * ) ' Invalid name of restart MAP file !'
      write (lun(19),*) ' Invalid name of restart MAP file !'
      WriteRestartFileDefaultName = 1
    else
      WriteRestartFileDefaultName = WriteRestartFile ( lcharmap )
    end if
end function

! WriteRestartFile --
!     Write a restart file in .map format with current status of the model
!
integer function WriteRestartFile ( lcharmap )
    !DEC$ ATTRIBUTES DLLEXPORT::WriteRestartFile
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'WRITERESTARTFILE' :: WriteRestartFile

    use delwaq2_global_data
    use m_dhopnf
    use m_sysn          ! System characteristics
    use m_sysc          ! Pointers in character array workspace
    use m_sysa          ! Pointers in real array workspace

    implicit none

    character (len=*) lcharmap
    integer    i, k, ierr

    call dhopnf ( lun(23), lcharmap, 23    , 1     , ierr  )
    if ( ierr == 0 ) then
      write ( lun(23) ) (dlwqd%chbuf(imnam+k-1) , k=1,160 )
      write ( lun(23) ) notot, noseg
      write ( lun(23) ) ( substance_name(k) , k=1,notot )
      write ( lun(23) ) dlwqd%itime , ((dlwqd%rbuf(iconc+(k-1)+(i-1)*notot), k=1,notot ), i=1, noseg )
      close ( lun(23) )
      WriteRestartFile = 0
    else
      write ( * , * ) ' Could not open restart MAP file !'
      write (lun(19),*) ' Could not open restart MAP file !'
      WriteRestartFile = 1
    end if
end function

! ModelInitialize --
!     Initialize the model run
!
integer function ModelInitialize ()
    !DEC$ ATTRIBUTES DLLEXPORT::ModelInitialize
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'MODELINITIALIZE' :: ModelInitialize

    use delwaq2_global_data
    use waq_omi_utils
    use dhcommand
    use m_openda_exchange_items, only : openda_buffer_initialize
    use m_actions
    use m_sysn          ! System characteristics
    use m_sysi          ! Timer characteristics
    use m_sysa          ! Pointers in real array workspace
    use m_sysj          ! Pointers in integer array workspace

    implicit none

    type(t_dlwq_item)               :: constants    !< delwaq constants list
    integer                         :: lunrep, lunwrk
    integer                         :: ierr

    !
    ! Arguments have already been initialised
    !
    call delwaq2_global_data_initialize(runid)

    if ( .not. allocated(argv) ) then
        allocate( argv(2) )
        argv(1) = 'dlwqlib.dll'
        argv(2) = runid
    endif

    call dhstore_command( argv )

    !
    ! Some parameters that still need to be set
    !
    lunrep = lun(19)
    ! VORTech: early, otherwise we get those fort.1 files
    call dhopnf ( lunrep , lchar(19) , 19    , 1    , ierr  )

    !
    ! Make sure the harmonic work file exists
    !

    open( newunit = lunwrk, file = lchar(3), form = 'unformatted',access='stream' )
    close( lunwrk )

    nolun  = 45      ! nolun has been declared in sysn_ff.inc
    !
    nothrd = 1       ! Set OpenMP threads to 1 (note: DLWQ07 code to overrule isn't called)
    nogrid = 1       ! No multiple grid option at the moment
    noitem = 11      ! Fixed number of items
    ilflag = 1       ! Always assume varying lengths
    itfact = 86400   ! Auxiliary timescale always 1 day
    newisp = 0
    newrsp = 0
    nlines = noseg*2 + ((noq1+noq2+noq3)*2)*2 ! memory related to volumes, areas, flows?
    npoins = noseg+3 + ((noq1+noq2+noq3)+3)*2 ! memory related to volumes, areas, flows?

    ! fill the constants structure for use in dlwqp1
    ierr = dlwq_init(constants)
    ierr = dlwq_resize(constants,nocons)
    constants%no_item = nocons
    constants%name(1:nocons) = procparam_const(1:nocons)
    constants%constant(1:nocons) = procparam_const_value(1:nocons)

    if (nocons>0) then
       newisp = newisp + nocons+5+2
       newrsp = newrsp + nocons
       nufil  = 0
    endif
    if (nopa>0) then
       newisp = newisp + nopa+noseg+5+3
       newrsp = newrsp + noseg*nopa*3
       nufil  = 1
    endif

    call write_delwaq04( argv(2) )
    call write_array_2d( argv(2), 'initials', substance_conc )
    if ( nopa > 0 ) then
        call write_array_2d( argv(2), 'params'  , procparam_param_value )
        open( newunit=lunwrk, file = lchar(41) )
        write( lunwrk, '(i5,a1,a256)' ) 0,' ',trim(argv(2)) // '-params.wrk'
        close( lunwrk )
    endif

   !
   ! Require SetInitialVolume instead
    call write_array_const( argv(2), 'flows',    0.0, noq )
    call write_array_const( argv(2), 'areas',    1.0, noq )
    call write_array_const( argv(2), 'wastload', 0.0, nowst*(notot+1) )
    call write_array_const( argv(2), 'boundary', 0.0, nobnd*nosys )
    call write_functions( argv(2) )

    call handle_output_requests( argv(2) )
    call handle_processes( argv(2) )

    !
    ! Now write the size information
    !
    call write_delwaq03( argv(2) )

    !
    ! Echo the input (also during the computation)
    !
    reporting = .true.
    lunlst    = 2
    open ( lunlst, file = trim(argv(2)) // '.lst' )
    call report_model_data( lunlst )
    !
    ! Everything has been prepared
    !
    call dlwqmain( ACTION_INITIALISATION, 2, argv, dlwqd )

    ! openDA buffer
    call openda_buffer_initialize

    ModelInitialize = 0

contains
! write_delwaq03 --
!     Write the first DELWAQ system intermediate file
!
subroutine write_delwaq03( name )
    use workspace
    use m_sysn          ! System characteristics
    use m_sysi          ! Timer characteristics

    implicit none

    character(len=*) :: name


    integer                                 :: imaxa, imaxi, imaxc
    real,             dimension(:), pointer :: rbuf  => null()
    integer,          dimension(:), pointer :: ibuf  => null()
    character(len=1), dimension(:), pointer :: chbuf => null()
    integer                                 :: lunwrk


    !noutp = 0 ! TODO: requires additional information in delwaq03

    imaxa = 0
    imaxi = 0
    imaxc = 0

    call space( lunrep, .false., rbuf, ibuf, chbuf, imaxa, imaxi, imaxc )

    open( newunit = lunwrk, file = trim(name) // '-delwaq03.wrk', form = 'unformatted',access='stream' )
    write( lunwrk ) in
    write( lunwrk ) ii
    write( lunwrk ) imaxa, imaxi, imaxc

    write( lunwrk ) lun(1:nolun)
    write( lunwrk ) lchar(1:nolun)
    write( lunwrk ) filtype(1:nolun)
    close( lunwrk )

    if ( associated(rbuf)  ) deallocate( rbuf )
    if ( associated(ibuf)  ) deallocate( ibuf )
    if ( associated(chbuf) ) deallocate( chbuf )

end subroutine write_delwaq03

! write_delwaq04 --
!     Write the second DELWAQ system intermediate file
!
subroutine write_delwaq04( name )
    use Grids
    use m_sysn          ! System characteristics
    use m_sysi          ! Timer characteristics

    implicit none

    character(len=*) :: name


    integer :: i
    integer :: idummy
    integer :: iref
    integer :: load_kind
    integer :: iseg
    integer :: error
    integer :: lunwrk

    type(GridPointer) :: aGrid

    title(1) = 'Wrapper for DELWAQ-DLL'
    title(2) = '                      '
    title(3) = '                      '
    title(4) = '                      '

    write( title(4), '(a4,i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)' ) &
                      'T0: ', ref_year, '.', ref_month,  '.', ref_day,    ' ', &
                              ref_hour, ':', ref_minute, ':', ref_second, '  (scu=       1s)'

    open( newunit = lunwrk, file = trim(name) // '-delwaq04.wrk', form = 'unformatted',access='stream' )
    write( lunwrk ) title

    write( lunwrk ) substance_name

    write( lunwrk ) ( monitor_name(i), i = 1,ndmpar )
    write( lunwrk ) ( monitor_cell(i), i = 1,ndmpar )

    ! Grid definitions - base grid only
    iref = 1
    write( lunwrk ) noseg, iref, ( i, i = 1,noseg )

    !
    ! Copied from grid.f
    aGrid%name            = 'Base grid'
    aGrid%noseg           = noseg
    aGrid%noseg_lay       = noseg / nolay
    aGrid%iref            =    1
    aGrid%name_ref        =  ' '
    aGrid%itype           =  BaseGrid
    aGrid%space_var_nolay =  .FALSE.
    aGrid%nolay           =  nolay
    aGrid%nolay_var       => null()
    allocate ( aGrid%iarray(noseg) )
    allocate ( aGrid%finalpointer(noseg) )
    do iseg = 1 , noseg
        agrid%iarray(iseg) = iseg
        agrid%finalpointer(iseg) = iseg
    enddo

    error = GridWrite( 10, aGrid )

    !
    ! Now the rest ...
    !
    idummy = 1
    write( lunwrk ) (idummy, i = 1,notot) ! SYSGRD in SETPRG
    write( lunwrk ) (idummy, i = 1,notot) ! SYSNDT in SETPRG

    write( lunwrk ) iknmrk

    if ( nodisp > 0 ) write( lunwrk ) diname
    if ( novelo > 0 ) write( lunwrk ) vename

    if ( allocated( idpnt_array ) ) deallocate( idpnt_array )
    allocate( idpnt_array(1:nosys), ivpnt_array(1:nosys) )
    idpnt_array = 0   ! For the moment
    ivpnt_array = 0
    write( lunwrk ) idpnt_array
    write( lunwrk ) ivpnt_array

    if ( nobnd > 0 ) then
        write( lunwrk ) ibpnt_array(2,:)
        write( lunwrk ) ibpnt_array(3,:)
    endif

    ! Monitoring points, not monitoring areas
    ! No transect information yet
    call write_delwaq04_monitoring

    idummy = 0
    write( lunwrk ) idummy, (disp(i)  ,i=1,3)
    write( lunwrk ) idummy, (aleng(i) ,i=1,3)

    if ( nobnd > 0 ) then
        write( lunwrk ) (boundary_id(i), boundary_name(i) ,i=1,nobnd )
        write( lunwrk ) boundary_type
        write( lunwrk ) (/ (i ,i=1,nobnd) /)  ! Type
        write( lunwrk ) ibpnt_array(1,:)      ! Time lag
    endif

    if ( nowst > 0 ) then
        ! TODO:
        ! Allow the waste load kind to be set
        load_kind = 0
        write( lunwrk ) (load_cell(i), load_kind, load_type(i), load_name(i) ,i=1,nowst)
        write( lunwrk ) load_type_def
        write( lunwrk ) (1 ,i=1,nowst )
    endif

    write( lunwrk ) procparam_const
    write( lunwrk ) procparam_param ! No separate function/segment function

    ! Time function information

    if ( allocated( nrftot ) ) deallocate( nrftot )
    if ( allocated( nrharm ) ) deallocate( nrharm )
    allocate( nrftot(1:noitem), nrharm(1:noitem) )
    nrftot = 0
    nrharm = 0
    write( lunwrk ) nrftot
    write( lunwrk ) nrharm

    close( lunwrk )


end subroutine write_delwaq04

subroutine write_delwaq04_monitoring

    integer :: ndmpq
    integer :: ndmps
    integer :: noraai
    integer :: ntraaq
    integer :: ierr
    integer :: noinfo
    integer, dimension(1)      :: nexcraai
    integer, dimension(1)      :: iexcraai
    integer, dimension(1)      :: ioptraai
    integer, dimension(ndmpar) :: nsegdmp
    integer, dimension(ndmpar) :: isegdmp

    ntdmps  = ndmpar ! For now
    nsegdmp = 1
    isegdmp = monitor_cell

    noraai  = 0      ! For now
    ntraaq  = 0      ! For now
    ierr    = 0
    lun(2)  = 10

    call dmpare( lun, ndmpar, ntdmps, noq, noseg, nobnd, ipoint, ntdmpq, ndmpq, ndmps, &
                 noraai, ntraaq, nsegdmp, isegdmp, nexcraai, iexcraai, ioptraai, &
                 ierr, noinfo )

end subroutine write_delwaq04_monitoring

! handle_output_requests --
!     Write a small piece of the DELWAQ input file for handling
!     the output options
!
subroutine handle_output_requests( name )
    use processet
    use output
    use m_sysn          ! System characteristics

    implicit none

    character(len=*) :: name

    integer          :: i
    integer          :: k
    integer          :: luninp

    open( newunit = luninp, file = trim(name) // '.inp' )

    write( luninp, '(a)' ) '1 ; output information in this file'
    do k = 1,4
        write( luninp, '(a)' ) '2 ; all substances and extra output'
        write( luninp, '(i5,a)' ) size(output_param), ' ; number of extra variables'
        do i = 1,size(output_param)
            if ( k.eq.1 .or. k.eq.3 ) then
                write( luninp, '(a,1x,a)' ) output_param(i), ''' '''
            else
                write( luninp, '(a,1x,a)' ) output_param(i)
            endif
        enddo
    enddo

    write( luninp, '(a)' ) ' 1 ; binary history file on'
    write( luninp, '(a)' ) ' 1 ; binary map     file on'
    write( luninp, '(a)' ) ' 1 ; nefis  history file on'
    write( luninp, '(a)' ) ' 1 ; nefis  map     file on'

    write( luninp, '(a)' ) ' #9 ; delimiter for the ninth block'
    write( luninp, '(a)' ) ' #10 ; delimiter for the tenth block'

    close( luninp )

end subroutine handle_output_requests

! handle_processes --
!     Handle all the process information
!
subroutine handle_processes( name )
    use processet
    use output
    use rd_token
    use m_sysn          ! System characteristics

    implicit none

    character(len=*) :: name

!    integer                   :: lun(50)         ! unit numbers
!    character(len=250)        :: lchar(50)       ! filenames
    type(procespropcoll)      :: statprocesdef   ! the statistical proces definition
    type(itempropcoll)        :: allitems        ! all items of the proces system
    integer                   :: ioutps(7,10)    ! (old) output structure
    type(outputcoll)          :: outputs         ! output structure
    integer                   :: noinfo          ! count of informative message
    integer                   :: nowarn          ! count of warnings
    integer                   :: ierr            ! error count
    integer                   :: org_noutp       ! Store the number of output files
                                                 ! Pointers into DELWAQ arrays
    integer, parameter        :: nopred = 6      ! Predefined parameters - fioutv.f
    integer                   :: iocons          ! Constants
    integer                   :: iopa            ! Parameters
    integer                   :: iofun           ! Functions
    integer                   :: iosfun          ! Segment functions
    integer                   :: ioconc          ! Concentrations

    integer, parameter                    :: icmax = 2000
    integer, parameter                    :: iimax = 2000
!    character(len=1)                      :: cchar
    character(len=20), dimension(icmax)   :: car
    integer, dimension(iimax)             :: iar
!    integer                               :: npos
    integer                               :: iwidth
    integer                               :: ibflag
    integer                               :: iwar
    integer                               :: ioutpt ! Dummy
    real                                  :: version = 4.9

    StatProcesDef%maxsize = 0
    StatProcesDef%cursize = 0
    AllItems%maxsize = 0
    AllItems%cursize = 0

    org_noutp = noutp

    iocons = nopred + 1
    iopa   = iocons + nocons
    iofun  = iopa   + nopa
    iosfun = iofun  + nofun
    ioconc = iosfun + nosfun

    !
    ! For the moment: only output the substances, nothing extra
    !
!   nbufmx = noseg * notot

    !nrvart = 4 * (notot + size(output_param) ! Four files
!   nrvart = 4 * notot ! Four files

!   ioutps = 0
!   ioutps(:,1) = (/ imstrt, imstop, imstep, notot, imo3, 0, 0 /)
!   ioutps(:,2) = (/      0,     -1,      1, notot, idmp, 0, 0 /)
!   ioutps(:,3) = (/ idstrt, idstop, idstep, notot, imap, 0, 0 /)
!   ioutps(:,4) = (/ ihstrt, ihstop, ihstep, notot, ihi3, 0, 0 /)
!   allocate( outputs%names(nrvart), outputs%pointers(nrvart) )

!    k = 0
!    do j = 1,4
!        do i = 1,notot
!            k = k + 1
!            outputs%names(k)    = substance_name(i)
!            outputs%pointers(k) = ioconc + i - 1
!        enddo
!        !do i = 1,size(output_param)
!        !    k = k + 1
!        !    outputs%names(k)    = output_param(i)
!        !    outputs%pointers(k) = ioconc + i - 1
!        !enddo
!    enddo

!   outputs%cursize = nrvart

    open( 9,      file = trim(name) // '.inp'      )
    open( lun(29), file = trim(name) // '.lstdummy' )

    npos    = 100
    cchar   = ';'
    iwidth  = 100
    ioutpt  = 0
    ierr    = 0
    iwar    = 0
    ibflag  = 0 ! TODO: correct value
    ilun(1) = 9
    lch (1) = trim(name) // '.inp'

    call dlwq09( lun, lchar, filtype, car, iar, icmax, &
             iimax, iwidth, ibflag, version,           &
             ioutpt, ioutps, outputs, ierr, iwar )

    close(  9 ) ! TODO: status = 'delete'
    close( 11 )

    call dlwqp1( lun, lchar, statprocesdef, allitems, &
             ioutps, outputs, nomult, mult, constants, &
             noinfo, nowarn, ierr )

    noutp = org_noutp

end subroutine handle_processes

subroutine write_array_2d( name, suffix, array )
    implicit none
    character(len=*)     :: name
    character(len=*)     :: suffix
    real, dimension(:,:) :: array

    integer :: time_dummy
    integer :: lunwrk

    time_dummy = 0

    open( newunit = lunwrk, file = trim(name) // '-' // trim(suffix) // '.wrk', form = 'unformatted',access='stream' )
    write( lunwrk ) time_dummy, array
    close( lunwrk )

end subroutine write_array_2d

! write_functions --
!     Write the file with constants, functions etc
!
subroutine write_functions( name )
    character(len=*), intent(in) :: name

    integer, parameter :: FILE_NAME_SIZE = 256
    integer, parameter :: ITEM_NAME_SIZE = 20
    integer            :: k
    integer            :: i
    integer            :: lun = 10
    character(len=FILE_NAME_SIZE) :: filename
    character(len=ITEM_NAME_SIZE) :: loc

    open( lun, file = trim(name) // '-function.wrk', form = 'unformatted',access='stream' )
    write( lun ) ' 5.000PROCES'
    i = 0
    if (nocons>0) then
       i = i+1
    endif
    if (nopa>0) then
       i = i+1
    endif
    write( lun ) i ! proc_pars%cursize
    if (nocons>0) then
       write(lun ) 10       ! subject SUBJECT_CONSTANT
       write(lun ) nocons   ! no_param
       write(lun ) 1        ! no_loc
       write(lun ) 0        ! no_brk
       write(lun ) 0        ! functype FUNCTYPE_CONSTANT
       write(lun ) 1        ! igrid
       write(lun ) .false.  ! extern
       write(lun ) 0        ! filetype FILE_NONE
       filename = ''
       write(lun ) filename
       write(lun ) 2        ! iorder ORDER_LOC_PARAM
       write(lun ) .true.   ! param_named
       write(lun ) procparam_const
       write(lun ) .true.   ! loc_named
       loc = 'constant'
       write(lun ) loc
       write(lun ) .true.   ! param_pointered
       write(lun ) (i, i=1,nocons)
       write(lun ) .false.  ! loc_defaults
       write(lun ) .false.  ! loc_pointered
       write(lun ) .false.  ! scaled
       write(lun ) 1.0_4    ! scale_factor
       write(lun ) .false.  ! param_scaled
       write(lun ) .false.  ! loc_scaled
       write(lun ) procparam_const_value
    endif
    if (nopa>0) then
       write(lun ) 11       ! subject SUBJECT_PARAMETER
       write(lun ) nopa     ! no_param
       write(lun ) noseg    ! no_loc
       write(lun ) 0        ! no_brk
       write(lun ) 0        ! functype FUNCTYPE_CONSTANT
       write(lun ) 1        ! igrid
       write(lun ) .false.  ! extern
       write(lun ) 0        ! filetype FILE_NONE
       filename = ''
       write(lun ) filename
       write(lun ) 2        ! iorder ORDER_LOC_PARAM
       write(lun ) .true.   ! param_named
       write(lun ) procparam_param
       write(lun ) .true.   ! loc_named
       do i = 1, noseg
          write(loc,'(A8,I8)') 'segment ',i
          write(lun ) loc
       enddo
       write(lun ) .true.   ! param_pointered
       write(lun ) (i, i=1,nopa)
       write(lun ) .false.  ! loc_defaults
       write(lun ) .false.  ! loc_pointered
       write(lun ) .false.  ! scaled
       write(lun ) 1.0_4    ! scale_factor
       write(lun ) .false.  ! param_scaled
       write(lun ) .false.  ! loc_scaled
       write(lun ) transpose(procparam_param_value)
    endif

    close( lun )

end subroutine write_functions

! report_model_data --
!     Report the model input in the monitor file
!
subroutine report_model_data( lunrep )
    integer, intent(in)  :: lunrep

    integer              :: i

    write( lunrep, '(a)' ) 'Run:'
    write( lunrep, '(4x,a)' ) title

    write( lunrep, '(/,a)'    ) 'Integration details:'
    write( lunrep, '(4x,a,i5)') 'Integration method: ', intsrt
    write( lunrep, '(4x,a,i5)') 'Secondary options:  ', intopt
    write( lunrep, '(/,a)'    ) 'Schematisation:'
    write( lunrep, '(4x,a,i10)'   ) 'Number of segments: ', noseg
    write( lunrep, '(4x,a,4i10)'  ) 'Number of exchanges:', noq1, noq2, noq3, noq4
    write( lunrep, '(/,a)'    ) 'Substances:'
    write( lunrep, '(i5,1x,a20)' ) (i, substance_name(i) ,i=1,size(substance_name))
    write( lunrep, '(/,a)'    ) 'Constants/functions:'
    if ( size(procparam_const) > 0 ) then
        write( lunrep, '(i5,1x,a20,g14.5)' ) (i, procparam_const(i), procparam_const_value(i) ,i=1,size(procparam_const))
    else
        write( lunrep, '(4xa)' ) 'None'
    endif
    write( lunrep, '(/,a)'    ) 'Parameters/segment functions:'

    if ( size(procparam_param) > 0 ) then
        write( lunrep, '(i5,1x,a20)' ) (i, procparam_param(i) ,i=1,size(procparam_param))
    else
        write( lunrep, '(4xa)' ) 'None'
    endif
    write( lunrep, '(/,a)'    ) 'Monitor points:'
    if ( size(monitor_name) > 0 ) then
        write( lunrep, '(i5,1x,a20,i10)' ) (i, monitor_name(i), monitor_cell(i) ,i=1,size(monitor_name))
    else
        write( lunrep, '(4xa)' ) 'None'
    endif
    write( lunrep, '(/,a)'    ) 'Waste load points:'
    if ( size(load_name) > 0 ) then
        write( lunrep, '(i5,1x,a20,i10,1x,a20)' ) (i, load_name(i), load_cell(i), load_type(i) ,i=1,size(load_name))
    else
        write( lunrep, '(4xa)' ) 'None'
    endif
    write( lunrep, '(/,a)'    ) 'Boundary cells:'
    if ( size(boundary_name) > 0 ) then
        write( lunrep, '(i5,1x,a20,a20,1x,a20)' ) (i, boundary_name(i), boundary_id(i), boundary_type(i) ,i=1,size(boundary_name))
    else
        write( lunrep, '(4xa)' ) 'None'
    endif

    write( lunrep, '(//)' )
end subroutine report_model_data

end function ModelInitialize

! ModelFinalize --
!     Conclude the model run - final output will be written
!
integer function ModelFinalize( )
    !DEC$ ATTRIBUTES DLLEXPORT::ModelFinalize
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'MODELFINALIZE' :: ModelFinalize

    use delwaq2_global_data
!    use m_delwaq_2_openda
    use m_actions

    implicit none

    character(len=20), dimension(0) :: argv_dummy
    integer :: ierr

    call dlwqmain( ACTION_FINALISATION, 0, argv_dummy, dlwqd )

    write(*,*) 'Model has been finalized'

! now deallocate all arrays in this lib!
    call delwaq2_global_data_finalize

    ModelFinalize = 0

end function ModelFinalize

! ModelInitialize_By_Id --
!     Create the model instance from an ID (currently: the name of a
!     preprocessed input file)
!
integer function ModelInitialize_By_Id( runid_given )
    !DEC$ ATTRIBUTES DLLEXPORT::ModelInitialize_By_id
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'MODELINITIALIZE_BY_ID' :: ModelInitialize_By_Id

    use delwaq2_global_data
    use waq_omi_utils
    use dhcommand
    use m_openda_exchange_items, only : openda_buffer_initialize
    use m_actions
    use m_sysn          ! System characteristics
    use m_sysi          ! Timer characteristics
    use m_sysa          ! Pointers in real array workspace
    use m_sysj          ! Pointers in integer array workspace


    implicit none

    character(len=*), intent(in)     :: runid_given

    !
    ! TODO: it is probably not all that simple
    !
    if ( .not. allocated( argv ) ) then
    allocate( argv(3) )
    endif
    argv(1) = 'dlwqlib.dll' ! argument 0 is the executable name on the command line
    argv(2) = runid_given
    argv(3) = '-waq'

    call dhstore_command( argv )

    call delwaq2_global_data_initialize(runid_given)
    !
    ! Leave everything to DELWAQ itself
    !
    call dlwqmain( ACTION_INITIALISATION, 2, argv, dlwqd )

    !
    ! Extract some data (mostly names) from the DLWQD data structure
    ! for later use
    !
    ! TODO



    call delwaq2_global_data_copy( dlwqd )

    ! openDA buffer
    call openda_buffer_initialize

    ModelInitialize_By_Id = 0

end function ModelInitialize_By_Id
end module waq_omi_interface