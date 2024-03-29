!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2023.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
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

module waq_plugin_wasteload_version_module
    use static_version_info

    implicit none

    character(*),  private, parameter :: component_name                   = 'WAQ_PLUGIN_WASTELOAD'

    character(*),  public, parameter :: waq_plugin_wasteload_version_full = company // ', ' // component_name // major_minor_buildnr
    character(*),  public, parameter :: waq_plugin_wasteload_version_id   = company_id // ', ' // component_name // ' ' // major_minor_buildnr_date_time

contains

    subroutine getfullversionstring_waq_plugin_wasteload(stringout)
        character(*), intent(out) :: stringout

        stringout = waq_plugin_wasteload_version_id(offset:)
    end subroutine getfullversionstring_waq_plugin_wasteload

end module waq_plugin_wasteload_version_module
