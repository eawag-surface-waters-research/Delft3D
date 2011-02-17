subroutine initsedpar(gdp)
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
    real(fp)                         , pointer :: mdcuni
    integer                          , pointer :: nmudfrac
    real(fp)      , dimension(:)     , pointer :: rhosol
    real(fp)      , dimension(:,:,:) , pointer :: logseddia
    real(fp)      , dimension(:)     , pointer :: logsedsig
    real(fp)      , dimension(:)     , pointer :: sedd10
    real(fp)      , dimension(:)     , pointer :: sedd50
    real(fp)      , dimension(:)     , pointer :: sedd50fld
    real(fp)      , dimension(:)     , pointer :: sedd90
    real(fp)      , dimension(:)     , pointer :: cdryb
    real(fp)      , dimension(:)     , pointer :: dstar
    real(fp)      , dimension(:)     , pointer :: taucr
    real(fp)      , dimension(:)     , pointer :: tetacr
    real(fp)      , dimension(:)     , pointer :: ws0
    real(fp)      , dimension(:)     , pointer :: wsm
    real(fp)      , dimension(:)     , pointer :: salmax
    real(fp)      , dimension(:)     , pointer :: sdbuni
    real(fp)      , dimension(:,:)   , pointer :: tcrdep
    real(fp)      , dimension(:)     , pointer :: tcduni
    real(fp)      , dimension(:,:)   , pointer :: tcrero
    real(fp)      , dimension(:)     , pointer :: tceuni
    real(fp)      , dimension(:,:)   , pointer :: eropar
    real(fp)      , dimension(:)     , pointer :: erouni
    real(fp)      , dimension(:)     , pointer :: mudcnt
    integer       , dimension(:)     , pointer :: nseddia
    integer       , dimension(:)     , pointer :: sedtyp
    character(10) , dimension(:)     , pointer :: inisedunit
    character(20) , dimension(:)     , pointer :: namsed
    character(256), dimension(:)     , pointer :: flsdbd
    character(256), dimension(:)     , pointer :: flstcd
    character(256), dimension(:)     , pointer :: flstce
    character(256), dimension(:)     , pointer :: flsero
    character(256)                   , pointer :: flsdia
    character(256)                   , pointer :: flsmdc
    include 'sedparams.inc'
!
!! executable statements -------------------------------------------------------
!
    mdcuni      => gdp%gdsedpar%mdcuni
    nmudfrac    => gdp%gdsedpar%nmudfrac
    rhosol      => gdp%gdsedpar%rhosol
    logseddia   => gdp%gdsedpar%logseddia
    logsedsig   => gdp%gdsedpar%logsedsig
    sedd10      => gdp%gdsedpar%sedd10
    sedd50      => gdp%gdsedpar%sedd50
    sedd50fld   => gdp%gdsedpar%sedd50fld
    sedd90      => gdp%gdsedpar%sedd90
    cdryb       => gdp%gdsedpar%cdryb
    dstar       => gdp%gdsedpar%dstar
    taucr       => gdp%gdsedpar%taucr
    tetacr      => gdp%gdsedpar%tetacr
    ws0         => gdp%gdsedpar%ws0
    wsm         => gdp%gdsedpar%wsm
    salmax      => gdp%gdsedpar%salmax
    sdbuni      => gdp%gdsedpar%sdbuni
    tcrdep      => gdp%gdsedpar%tcrdep
    tcduni      => gdp%gdsedpar%tcduni
    tcrero      => gdp%gdsedpar%tcrero
    tceuni      => gdp%gdsedpar%tceuni
    eropar      => gdp%gdsedpar%eropar
    erouni      => gdp%gdsedpar%erouni
    mudcnt      => gdp%gdsedpar%mudcnt
    nseddia     => gdp%gdsedpar%nseddia
    sedtyp      => gdp%gdsedpar%sedtyp
    inisedunit  => gdp%gdsedpar%inisedunit
    namsed      => gdp%gdsedpar%namsed
    flsdbd      => gdp%gdsedpar%flsdbd
    flstcd      => gdp%gdsedpar%flstcd
    flstce      => gdp%gdsedpar%flstce
    flsero      => gdp%gdsedpar%flsero
    flsdia      => gdp%gdsedpar%flsdia
    flsmdc      => gdp%gdsedpar%flsmdc
    !
    mdcuni   = 0.0
    !
    nmudfrac = 0
    !
    nullify(gdp%gdsedpar%rhosol)
    !
    nullify(gdp%gdsedpar%logseddia)
    nullify(gdp%gdsedpar%logsedsig)
    nullify(gdp%gdsedpar%sedd10)
    nullify(gdp%gdsedpar%sedd50)
    nullify(gdp%gdsedpar%sedd50fld)
    nullify(gdp%gdsedpar%sedd90)
    !
    nullify(gdp%gdsedpar%cdryb)
    nullify(gdp%gdsedpar%dstar)
    nullify(gdp%gdsedpar%taucr)
    nullify(gdp%gdsedpar%tetacr)
    nullify(gdp%gdsedpar%ws0)
    nullify(gdp%gdsedpar%wsm)
    nullify(gdp%gdsedpar%salmax)
    nullify(gdp%gdsedpar%sdbuni)
    nullify(gdp%gdsedpar%tcrdep)
    nullify(gdp%gdsedpar%tcduni)
    nullify(gdp%gdsedpar%tcrero)
    nullify(gdp%gdsedpar%tceuni)
    nullify(gdp%gdsedpar%eropar)
    nullify(gdp%gdsedpar%erouni)
    nullify(gdp%gdsedpar%mudcnt)
    !
    nullify(gdp%gdsedpar%nseddia)
    nullify(gdp%gdsedpar%sedtyp)
    !
    nullify(gdp%gdsedpar%inisedunit)
    nullify(gdp%gdsedpar%namsed)
    nullify(gdp%gdsedpar%flsdbd)
    nullify(gdp%gdsedpar%flstcd)
    nullify(gdp%gdsedpar%flstce)
    nullify(gdp%gdsedpar%flsero)
    !
    flsdia = ' '
    flsmdc = ' '
end subroutine initsedpar
