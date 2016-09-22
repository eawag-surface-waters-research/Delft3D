!!  Copyright (C)  Stichting Deltares, 2012-2016.
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

module bmi
  use iso_c_binding
  implicit none

  ! Define some global constants
  character(*), parameter :: PREFIX = "WAQ"
  !DEC$ ATTRIBUTES DLLEXPORT :: PREFIX
  integer(c_int) :: MAXNAMES = 100
  integer(c_int), BIND(C, name="MAXSTRLEN") :: MAXSTRLEN = 1024
  !DEC$ ATTRIBUTES DLLEXPORT :: MAXSTRLEN
  integer(c_int), BIND(C, name="MAXDIMS") :: MAXDIMS = 6
  !DEC$ ATTRIBUTES DLLEXPORT :: MAXDIMS

contains

  ! Control

  !> The initialize() function accepts a string argument that
  !! gives the name (and path) of its "main input file", called
  !! a configuration file. This function should perform all tasks
  !! that are to take place before entering the model's time loop.
  integer(c_int) function initialize(c_config_file) bind(C, name="initialize")
    !DEC$ ATTRIBUTES DLLEXPORT::INITIALIZE
    use iso_c_binding, only: c_char
    use iso_c_utils
    use delwaq2_global_data
    use dhcommand
    implicit none
    character(kind=c_char),intent(in)    :: c_config_file(MAXSTRLEN)
    character(len=strlen(c_config_file)) :: runid_given
    integer                          :: argc
    integer                          :: errorcode
    include 'actions.inc'

    ! Store the name
    runid_given = char_array_to_string(c_config_file)

    ! We have to find a way to pass other arguments to Delwaq from DIMR
    if ( .not. allocated( argv ) ) then
    argc = 5
    allocate( argv(argc) )
    endif
    argv(1) = 'delwaq.dll' ! argument 0 is the executable name on the command line
    argv(2) = runid_given
    argv(3) = '-waq'
    argv(4) = '-p'
    argv(5) = '..\..\bin\win64\waq\default\proc_def.dat'

!    call dhstore_command( argv )
    call delwaq1(argc, argv, errorcode)
    call delwaq2_global_data_initialize(runid_given)
    call dlwqmain( ACTION_INITIALISATION, 2, argv, dlwqd )

    call delwaq2_global_data_copy( dlwqd )

    initialize = 0
  end function initialize
  
  subroutine get_version_string(c_version_string) bind(C, name="get_version_string")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_version_string
    use iso_c_binding, only: c_char
    use iso_c_utils

    character(kind=c_char), intent(out) :: c_version_string(MAXSTRLEN)
    character(len=MAXSTRLEN) :: name
    character(len=120)       :: idstr
    character(len=10)        :: delwaq_version

    call getidentification(idstr, delwaq_version)
    name = trim(idstr)
    c_version_string = string_to_char_array(trim(name))
  end subroutine get_version_string

  integer function update(dt) bind(C, name="update")
    !DEC$ ATTRIBUTES DLLEXPORT :: update
    use delwaq2_global_data
    use messagehandling
    use iso_c_binding, only: c_double

    implicit none

    real(c_double), value, intent(in) :: dt
    integer :: update_steps, step
    character(len=20), dimension(0) :: argv_dummy
    include 'sysi_ff.inc'
    include 'actions.inc'

    update_steps = nint(dt * dlwqd%tscale) / idt
    do step = 1, update_steps
      call dlwqmain( ACTION_SINGLESTEP, 0, argv_dummy, dlwqd )
    enddo
    update = 0
  end function update

  integer function finalize() bind(C, name="finalize")
    !DEC$ ATTRIBUTES DLLEXPORT :: finalize
    use delwaq2_global_data
    implicit none
    character(len=20), dimension(0) :: argv_dummy
    integer :: ierr
    include 'actions.inc'

    call dlwqmain( ACTION_SINGLESTEP, 0, argv_dummy, dlwqd )
    call dlwqmain( ACTION_FINALISATION, 0, argv_dummy, dlwqd )
    call delwaq2_global_data_finalize

    finalize = 0
  end function finalize

  
  subroutine get_start_time(t) bind(C, name="get_start_time")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_start_time
    use delwaq2_global_data
    use iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(out) :: t
    include 'sysi_ff.inc'

    t = real(dlwqd%otime,8) + real(itstrt,8) / real(dlwqd%tscale,8)
  end subroutine get_start_time

  
  subroutine get_end_time(t) bind(C, name="get_end_time")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_end_time
    use delwaq2_global_data
    use iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(out) :: t
    include 'sysi_ff.inc'

    t = real(dlwqd%otime,8) + real(itstop,8) / real(dlwqd%tscale,8)
  end subroutine get_end_time

  
  subroutine get_time_step(dt) bind(C, name="get_time_step")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_time_step
    use delwaq2_global_data
    use iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(out) :: dt
    include 'sysi_ff.inc'

    dt = real(idt,8) / real(dlwqd%tscale,8)
  end subroutine get_time_step

  
  subroutine get_current_time(t) bind(C, name="get_current_time")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_current_time
    use delwaq2_global_data
    use iso_c_binding, only: c_double
    implicit none
    real(c_double), intent(out) :: t
    integer current
    include 'sysi_ff.inc'

    t = real(dlwqd%otime,8) + real(dlwqd%itime,8) / real(dlwqd%tscale,8)
  end subroutine get_current_time

end module bmi
