module maptonetcdf_version_module

!!  Copyright (C)  Stichting Deltares, 2012-2022.
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

!  $Id$
!  $HeadURL$
    use iso_c_binding

    implicit none

    character(*),  public, parameter :: maptonetcdf_major        = '**'
    character(*),  public, parameter :: maptonetcdf_minor        = '**'
    character(*),  public, parameter :: maptonetcdf_revision     = '**'
    character(*),  public, parameter :: maptonetcdf_build_number = '141484M'

    character(*),  public, parameter :: maptonetcdf_company      = 'Deltares'
    character(*),  public, parameter :: maptonetcdf_company_url  = 'http://www.deltares.nl'
    character(*),  public, parameter :: maptonetcdf_program      = 'maptonetcdf'
    character(*),  public, parameter :: maptonetcdf_programname  = 'maptonetcdf'  ! use in about box and window title

    character(*),  public, parameter :: maptonetcdf_version      = maptonetcdf_major//'.'//maptonetcdf_minor//'.'//maptonetcdf_revision//'.'//maptonetcdf_build_number
    character(*),  public, parameter :: maptonetcdf_version_full = 'Deltares, '//maptonetcdf_program//' Version '//maptonetcdf_version//', '//__DATE__//', '//__TIME__
    character(*),  public, parameter :: maptonetcdf_version_id   = '@(#)'//maptonetcdf_version_full

contains

    subroutine getfullversionstring_maptonetcdf(stringout)
        character(*), intent(out) :: stringout
        integer                   :: length

        length = min(len_trim(maptonetcdf_version_full),len(stringout))
        stringout = maptonetcdf_version_id(5:5+length-1)
    end subroutine getfullversionstring_maptonetcdf

    subroutine getprogramnamestring_maptonetcdf(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(maptonetcdf_programname)
    end subroutine getprogramnamestring_maptonetcdf

    subroutine getshortprogramnamestring_maptonetcdf(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(maptonetcdf_program)
    end subroutine getshortprogramnamestring_maptonetcdf

    subroutine getfeaturenumberstring_maptonetcdf(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(maptonetcdf_version)
    end subroutine getfeaturenumberstring_maptonetcdf

    subroutine getversionnumberstring_maptonetcdf(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(maptonetcdf_version)
    end subroutine getversionnumberstring_maptonetcdf

    subroutine getcompanystring_maptonetcdf(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(maptonetcdf_company)
    end subroutine getcompanystring_maptonetcdf

    subroutine getsvnrevisionstring_maptonetcdf(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(maptonetcdf_build_number)
    end subroutine getsvnrevisionstring_maptonetcdf

    subroutine getarchitecturestring_maptonetcdf(stringout)
        character(*), intent(out) :: stringout

#if defined(WIN32)
        stringout = trim('Win32')
#elif defined(WIN64)
        stringout = trim('Win64')
#else
        if (c_size_t == 4) then
            stringout = trim('Linux32')
        elseif (c_size_t == 8) then
            stringout = trim('Linux64')
        else
            stringout = trim('Unknown')
        end if
#endif

    end subroutine getarchitecturestring_maptonetcdf

end module maptonetcdf_version_module
