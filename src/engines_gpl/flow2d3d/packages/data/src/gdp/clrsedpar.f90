subroutine clrsedpar(istat     ,gdp       )
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
    type (gd_sedpar)                 , pointer :: gdsedpar
!
! Global variables
!
    integer,intent(out) :: istat
!
!! executable statements -------------------------------------------------------
!
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
    inisedunit  => gdp%gdsedpar%inisedunit
    namsed      => gdp%gdsedpar%namsed
    sedtyp      => gdp%gdsedpar%sedtyp
    flsdbd      => gdp%gdsedpar%flsdbd
    flstcd      => gdp%gdsedpar%flstcd
    flstce      => gdp%gdsedpar%flstce
    flsero      => gdp%gdsedpar%flsero
    gdsedpar    => gdp%gdsedpar
    !
    if (associated(gdsedpar%rhosol))     deallocate(gdsedpar%rhosol,     STAT = istat)
    !
    if (associated(gdsedpar%logseddia))  deallocate(gdsedpar%logseddia,  STAT = istat)
    if (associated(gdsedpar%logsedsig))  deallocate(gdsedpar%logsedsig,  STAT = istat)
    if (associated(gdsedpar%sedd10))     deallocate(gdsedpar%sedd10,     STAT = istat)
    if (associated(gdsedpar%sedd50))     deallocate(gdsedpar%sedd50,     STAT = istat)
    if (associated(gdsedpar%sedd50fld))  deallocate(gdsedpar%sedd50fld,  STAT = istat)
    if (associated(gdsedpar%sedd90))     deallocate(gdsedpar%sedd90,     STAT = istat)
    !
    if (associated(gdsedpar%cdryb))      deallocate(gdsedpar%cdryb,      STAT = istat)
    if (associated(gdsedpar%dstar))      deallocate(gdsedpar%dstar,      STAT = istat)
    if (associated(gdsedpar%taucr))      deallocate(gdsedpar%taucr,      STAT = istat)
    if (associated(gdsedpar%tetacr))     deallocate(gdsedpar%tetacr,     STAT = istat)
    if (associated(gdsedpar%ws0))        deallocate(gdsedpar%ws0,        STAT = istat)
    if (associated(gdsedpar%wsm))        deallocate(gdsedpar%wsm,        STAT = istat)
    if (associated(gdsedpar%salmax))     deallocate(gdsedpar%salmax,     STAT = istat)
    if (associated(gdsedpar%sdbuni))     deallocate(gdsedpar%sdbuni,     STAT = istat)
    if (associated(gdsedpar%tcrdep))     deallocate(gdsedpar%tcrdep,     STAT = istat)
    if (associated(gdsedpar%tcduni))     deallocate(gdsedpar%tcduni,     STAT = istat)
    if (associated(gdsedpar%tcrero))     deallocate(gdsedpar%tcrero,     STAT = istat)
    if (associated(gdsedpar%tceuni))     deallocate(gdsedpar%tceuni,     STAT = istat)
    if (associated(gdsedpar%eropar))     deallocate(gdsedpar%eropar,     STAT = istat)
    if (associated(gdsedpar%erouni))     deallocate(gdsedpar%erouni,     STAT = istat)
    if (associated(gdsedpar%mudcnt))     deallocate(gdsedpar%mudcnt,     STAT = istat)
    !
    if (associated(gdsedpar%nseddia))    deallocate(gdsedpar%nseddia,    STAT = istat)
    if (associated(gdsedpar%sedtyp))     deallocate(gdsedpar%sedtyp,     STAT = istat)
    !
    if (associated(gdsedpar%inisedunit)) deallocate(gdsedpar%inisedunit, STAT = istat)
    if (associated(gdsedpar%namsed))     deallocate(gdsedpar%namsed,     STAT = istat)
    if (associated(gdsedpar%flsdbd))     deallocate(gdsedpar%flsdbd,     STAT = istat)
    if (associated(gdsedpar%flstcd))     deallocate(gdsedpar%flstcd,     STAT = istat)
    if (associated(gdsedpar%flstce))     deallocate(gdsedpar%flstce,     STAT = istat)
    if (associated(gdsedpar%flsero))     deallocate(gdsedpar%flsero,     STAT = istat)
end subroutine clrsedpar
