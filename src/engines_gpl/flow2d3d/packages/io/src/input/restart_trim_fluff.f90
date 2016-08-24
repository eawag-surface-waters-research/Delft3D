subroutine restart_trim_fluff (lundia    ,mfluff    ,rst_fluff ,lsed      ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!-------------------------------------------------------------------------------
!  $Id$
!  $HeadURL$
!!--description-----------------------------------------------------------------
! Reads initial field condition records from a trim-file
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision 
    use globaldata
    use dfparall
    use netcdf, only: nf90_inq_varid, nf90_inquire_variable, nf90_inquire_dimension, NF90_MAX_VAR_DIMS
    use rdarray, only:rdarray_nml
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer       , dimension(:,:)       , pointer :: iarrc
    integer       , dimension(:)         , pointer :: mf
    integer       , dimension(:)         , pointer :: ml
    integer       , dimension(:)         , pointer :: nf
    integer       , dimension(:)         , pointer :: nl
    !
    integer                              , pointer :: i_restart
    integer                              , pointer :: fds
    integer                              , pointer :: filetype
    character(256)                       , pointer :: filename
!
! Global variables
!
    integer                                                                               :: lundia
    logical                                                                 , intent(out) :: rst_fluff
    real(fp), dimension(lsed, gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     , intent(out) :: mfluff
    integer                                                                 , intent(in)  :: lsed
!
! Local variables
!
    integer, external                     :: getelt
    integer, dimension(3,5)               :: cuindex
    integer                               :: idvar
    integer                               :: rst_lsed
    integer                               :: ierror
    integer, dimension(NF90_MAX_VAR_DIMS) :: dimids
    character(16)                         :: grnam
!
!! executable statements -------------------------------------------------------
!
    mf                  => gdp%gdparall%mf
    ml                  => gdp%gdparall%ml
    nf                  => gdp%gdparall%nf
    nl                  => gdp%gdparall%nl
    iarrc               => gdp%gdparall%iarrc
    !
    i_restart           => gdp%gdrestart%i_restart
    fds                 => gdp%gdrestart%fds
    filetype            => gdp%gdrestart%filetype
    filename            => gdp%gdrestart%filename
    !
    rst_fluff = .false.
    !
    if (filetype == -999) return
    !
    if (inode==master) then
       if (filetype==FTYPE_NEFIS) then
          !
          ! initialize group index constant data
          !
          cuindex (3,1) = 1 ! increment in time
          cuindex (1,1) = 1
          cuindex (2,1) = 1
          !
          ierror = getelt(fds, 'map-const', 'LSED', cuindex, 1, 4, rst_lsed)
       else
          ierror = nf90_inq_varid(fds, 'MFLUFF', idvar)
          if (ierror==0) then
             ierror = nf90_inquire_variable(fds, idvar, dimids=dimids)
             if (ierror==0) ierror = nf90_inquire_dimension(fds, dimids(3), len=rst_lsed)
          endif
       endif
    endif
    call dfbroadc_gdp(rst_lsed, 1, dfint, gdp)
    if (rst_lsed /= lsed) return
    !
    ! element 'MFLUFF'
    !
    grnam = 'map-sed-series'
    call rdarray_nml(fds, filename, filetype, grnam, i_restart, &
                 & nf, nl, mf, ml, iarrc, gdp, &
                 & lsed, ierror, lundia, mfluff, 'MFLUFF')
    if (ierror == 0) then
       write(lundia, '(a)') 'Fluff layer content read from restart file.'
       rst_fluff = .true.
    endif
end subroutine restart_trim_fluff
