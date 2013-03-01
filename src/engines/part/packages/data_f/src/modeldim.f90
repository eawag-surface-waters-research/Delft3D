!!  Copyright(C) Stichting Deltares, 2012-2013.
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

!!  Note: The "part" engine is not yet Open Source, but still under
!!  development. This package serves as a temporary dummy interface for
!!  the references in the "waq" engine to the "part" engine.

module modeldim

use precision
implicit none
save

   type :: model_dimensions_

       integer  :: no_timesteps
       integer  :: no_particles

       integer  :: no_substances
       integer  :: no_userdef_substances
       integer  :: no_oil_fractions
       integer  :: no_monitoring_stations
       integer  :: no_particle_tracks
       integer  :: no_constants
       integer  :: no_random_parameters

       integer  :: mmax
       integer  :: nmax
       integer  :: kmax
       integer  :: exchanges

       integer  :: no_hor_segments
       integer  :: no_total_segments
       integer  :: no_layers
       integer  :: nosegl
       integer  :: noseg
       integer  :: noq1
       integer  :: noq2
       integer  :: noq3
       integer  :: noq

       integer  :: mmap
       integer  :: nmap

       integer  :: no_ud_releases
       integer  :: no_dye_releases
       integer  :: no_cont_releases
       integer  :: no_total_releases

       integer  :: max_brkpts_decay
       integer  :: max_brkpts_continuous_releases
       integer  :: max_brkpts_settling_velocities
       integer  :: max_brkpts_plottimes_zoomgrid
       integer  :: max_brkpts_wind
       integer  :: max_brkpts_ini_polygone

       integer  :: no_nefis_elements

       integer  :: buffer_size

   end type model_dimensions_

   type(model_dimensions_), target :: model_dimensions

end module modeldim
