subroutine gdp_alloc_arrays(gdp)
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
!
! NONE
!
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
    integer                              , pointer :: nofou
    integer                              , pointer :: lundia
!
! Global variables
!
!
! Local variables
!
    integer    ::  istat 
!
!! executable statements -------------------------------------------------------
!
    nofou               => gdp%d%nofou
    lundia              => gdp%gdinout%lundia
    !
    ! Allocation of all arrays from GDP structure
    !
    ! Arrays for Fourier analysis (fourier.igs)
    !
    if (nofou > 0) then
                       allocate (gdp%gdfourier%fconno (1:nofou), STAT = istat)
       if (istat == 0) allocate (gdp%gdfourier%flayno (1:nofou), STAT = istat)
       if (istat == 0) allocate (gdp%gdfourier%fnumcy (1:nofou), STAT = istat)
       if (istat == 0) allocate (gdp%gdfourier%ftmsto (1:nofou), STAT = istat)
       if (istat == 0) allocate (gdp%gdfourier%ftmstr (1:nofou), STAT = istat)
       if (istat == 0) allocate (gdp%gdfourier%ifoupt (1:nofou), STAT = istat)
       if (istat == 0) allocate (gdp%gdfourier%iofset (1:nofou), STAT = istat)
       !
       if (istat == 0) allocate (gdp%gdfourier%fknfac (                                          1:nofou), STAT = istat)
       if (istat == 0) allocate (gdp%gdfourier%foucomp(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:nofou), STAT = istat)
       if (istat == 0) allocate (gdp%gdfourier%foufas (                                          1:nofou), STAT = istat)                                         
       if (istat == 0) allocate (gdp%gdfourier%fousma (gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:nofou), STAT = istat)
       if (istat == 0) allocate (gdp%gdfourier%fousmb (gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:nofou), STAT = istat)
       if (istat == 0) allocate (gdp%gdfourier%fouvec (gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:nofou), STAT = istat)
       if (istat == 0) allocate (gdp%gdfourier%fv0pu  (                                          1:nofou), STAT = istat)
       !
       if (istat == 0) allocate (gdp%gdfourier%fouelp (1:nofou), STAT = istat)
       if (istat == 0) allocate (gdp%gdfourier%founam (1:nofou), STAT = istat)
       if (istat == 0) allocate (gdp%gdfourier%foutyp (1:nofou), STAT = istat)
       !
       if (istat /= 0) then
          call prterr(lundia, 'P004', 'memory alloc error in gdp_alloc_arrays for FOURIER parameters ')
          call d3stop(1, gdp)
       endif
    endif
    !
end subroutine gdp_alloc_arrays
