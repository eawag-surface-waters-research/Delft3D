      module waqdata
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
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
!!--description-----------------------------------------------------------------
! This module contains the data and routines for communicating with WAQ
!!--declarations----------------------------------------------------------------
      use precision
      use waqlib
      
      type waqdatatype
          !
          ! reals
          !
          !
          ! integers
          !
          integer  :: aggre     !  -1: no aggregation, 0: active cells only, 1: aggregation according to content of flaggr
          integer  :: itwqff    !  start time for writing binary waq files
          integer  :: itwqfi    !  time step for writing binary waq files
          integer  :: itwqfl    !  end time for writing binary waq files
          integer  :: itim      !  last writen time
          integer  :: lunvol    !  file unit number to an output file
          integer  :: lunare    !  file unit number to an output file
          integer  :: lunflo    !  file unit number to an output file
          integer  :: nobnd     !  number of WAQ boundaries
          integer  :: noseg     !  number of WAQ segments
          integer  :: noq       !  total number of WAQ exchanges
          !
          integer  :: cfoutset  !  'DataCFtoWQ'
          integer  :: wqinset   !  'DataWQtoCF'
          integer  :: wqiinset  !  'DataWQtoWQI'
          integer  :: wqioutset !  'DataWQItoWQ'
          !
          ! pointers
          !
          integer , dimension(:)  , pointer :: isaggn    ! segment aggregation pointer (nodmax)
          integer , dimension(:,:), pointer :: isaggl    ! segment aggregation pointer (2,lintot)
                                                         ! isaggl(1,l) = FROM segment of link
                                                         ! isaggl(2,l) = TO segment of link
          integer , dimension(:)  , pointer :: iqaggr    ! flow aggregation pointer
          integer , dimension(:,:), pointer :: ifrmto    ! from-to pointer table
          integer, dimension(:)   , pointer :: nlinks    ! Number of links per node
          integer , dimension(:,:), pointer :: volcon    ! Connectivity table for subvolumes (volumes in links and nodes)
          real(hp), dimension(:)  , pointer :: area      ! Area of exchange
          real(hp), dimension(:,:), pointer :: lenex     ! From/to distances at exchange
          real(hp), dimension(:)  , pointer :: lenseg    ! Length of a segment
          real(hp), dimension(:,:), pointer :: linkdis   ! Discharges at links (start, centre, end)
          real(hp), dimension(:,:), pointer :: linkvol0  ! Volumes at links (start, end)
          real(hp), dimension(:,:), pointer :: linkvol1  ! Volumes at links (start, end)
          real(hp), dimension(:)  , pointer :: nodevol1  ! Volumes at nodes (start, end)
          real(hp), dimension(:)  , pointer :: vol       ! WAQ volumes
          real(hp), dimension(:)  , pointer :: qag       ! WAQ flux aggregator
          !
          ! logicals
          !
          logical :: first_cf            ! first time of writing to binary waq files online
          logical :: waqlib = .true.     ! Flag if WAQ should be called as shared lib
          !
          ! characters
          !
          character(256) :: flaggr !  Name of input aggregation file
          !
          !
          !
          type(waqlibtype) :: wlp
      end type waqdatatype
      
      type(waqdatatype), pointer :: wdp
      
      end module waqdata
