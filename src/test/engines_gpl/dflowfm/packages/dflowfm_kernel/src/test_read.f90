!!  Copyright (C)  Stichting Deltares, 2012-2019.
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

module test_read
    use ftnunit

    implicit none

contains

subroutine tests_read
    call test( test_read_obs_points, 'Tests the reading of observation points' )
end subroutine tests_read

subroutine test_read_obs_points
    use m_oobservations
    !
    ! Locals
    integer :: n_obs_pnt
    integer :: n_return
    !
    ! Body
    call loadObservations("ObservationPoints_2.ini", 0)
    n_obs_pnt = 4
    call find_flownode_from_ini_file(n_obs_pnt, n_return)
    call assert_equal( n_obs_pnt, n_return, 'Returned number of observation points correct (read from ini file)' )

end subroutine test_read_obs_points

end module test_read
