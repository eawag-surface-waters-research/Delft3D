subroutine initflwpar(gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp), dimension(:,:,:)         , pointer :: fluxu
    real(fp), dimension(:,:,:)         , pointer :: fluxuc
    real(fp), dimension(:,:,:)         , pointer :: fluxv
    real(fp), dimension(:,:,:)         , pointer :: fluxvc
    real(fp), dimension(:,:,:)         , pointer :: fluxw
    real(fp), dimension(:,:,:)         , pointer :: fluxwc
    type (flwoutputtype)               , pointer :: flwoutput
    type (handletype)                  , pointer :: fbcrfile
    type (fbcrbndtype)  , dimension(:) , pointer :: fcrbnd
    logical                            , pointer :: fbccorrection
    character(256)                     , pointer :: fbcrfilnam
    type (gd_flwpar)                   , pointer :: gdflwpar
    !
    integer  :: istat
!
!! executable statements -------------------------------------------------------
!
    fluxu          => gdp%gdflwpar%fluxu
    fluxuc         => gdp%gdflwpar%fluxuc
    fluxv          => gdp%gdflwpar%fluxv
    fluxvc         => gdp%gdflwpar%fluxvc
    fluxw          => gdp%gdflwpar%fluxw
    fluxwc         => gdp%gdflwpar%fluxwc
    flwoutput      => gdp%gdflwpar%flwoutput
    fbcrfile       => gdp%gdflwpar%fbcrfile
    fcrbnd         => gdp%gdflwpar%fcrbnd
    fbccorrection  => gdp%gdflwpar%fbccorrection
    fbcrfilnam     => gdp%gdflwpar%fbcrfilnam
    gdflwpar       => gdp%gdflwpar
    !
    istat = 0
    allocate (gdflwpar%flwoutput, stat = istat)
    allocate (gdflwpar%fbcrfile , stat = istat) 
    !
    fluxu          => gdp%gdflwpar%fluxu
    fluxuc         => gdp%gdflwpar%fluxuc
    fluxv          => gdp%gdflwpar%fluxv
    fluxvc         => gdp%gdflwpar%fluxvc
    fluxw          => gdp%gdflwpar%fluxw
    fluxwc         => gdp%gdflwpar%fluxwc
    flwoutput      => gdp%gdflwpar%flwoutput
    fbcrfile       => gdp%gdflwpar%fbcrfile
    fcrbnd         => gdp%gdflwpar%fcrbnd
    fbccorrection  => gdp%gdflwpar%fbccorrection
    fbcrfilnam     => gdp%gdflwpar%fbcrfilnam
    gdflwpar       => gdp%gdflwpar
    !
    flwoutput%air         = .false.
    flwoutput%addtim      = .false.
    flwoutput%chezy       = .false.
    flwoutput%cumdifuflux = .false.
    flwoutput%difuflux    = .false.
    flwoutput%layering    = .false.
    flwoutput%roughness   = .false.
    flwoutput%temperature = .false.
    flwoutput%veuler      = .true.
    flwoutput%z0cur       = .false.
    flwoutput%z0rou       = .false.
    !
    nullify(gdflwpar%fluxu)
    nullify(gdflwpar%fluxuc)
    nullify(gdflwpar%fluxv)
    nullify(gdflwpar%fluxvc)
    nullify(gdflwpar%fluxw)
    nullify(gdflwpar%fluxwc)
    !
    nullify(gdflwpar%fcrbnd) 
    !
    fbccorrection = .false.
    !
    fbcrfilnam    = ' '
end subroutine initflwpar
