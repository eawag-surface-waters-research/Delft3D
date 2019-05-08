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

module test_roughness
    use ftnunit
    use m_roughness


    implicit none

contains

subroutine tests_roughness
    call test( test_roughness_branches,     'Tests circular cross section' )
end subroutine tests_roughness

subroutine test_roughness_branches
   use m_network
   use m_roughness
   use m_read_roughness
   
   type(t_network)      :: network
   character(len=256)   :: roughnessfiles
   character(len=256)   :: mapdir
   call realloc(network%brs)
   network%brs%count = 7
   network%brs%branch(1)%id = 'Channel1'
   network%brs%branch(2)%id = 'Channel2'
   network%brs%branch(3)%id = 'Channel3'
   network%brs%branch(4)%id = 'Channel4'
   network%brs%branch(5)%id = 'Channel5'
   network%brs%branch(6)%id = 'Channel6'
   network%brs%branch(7)%id = 'Channel7'
   call fill_hashtable(network%brs)
   
   roughnessfiles = 'roughness_main.ini'
   mapdir         = 'roughness\'
   call roughness_reader(network, roughnessfiles, mapdir)
   continue
end subroutine test_roughness_branches
   
end module test_roughness
