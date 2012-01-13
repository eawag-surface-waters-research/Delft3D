subroutine flwlic(lunscr    ,error     ,username    ,version_full ,version_short   , &
                & soort     ,gdp       )
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
!
!    Function: Obtains add-ons, checks validity of program ver-
!              sion number and gets username from Licence file,
!              using Flexlm.
!              It also detect automatically through checksum
!              whether the file has been tampered with.
!              At the background, it automatically checks
!              authorisation to use Delft3D, verd3dn expiry date,
!              hardware-id (UNIX only).
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use timers
    !
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    character(256)                 , pointer :: licfil
    integer                        , pointer :: nprocs
    integer      , dimension(:, :) , pointer :: nprdim
    character*20 , dimension(:)    , pointer :: procs
!
! Global variables
!
    integer     , intent(in)  :: lunscr        !  Description and declaration in inout.igs
    logical     , intent(out) :: error         !  false if NO errror is encountered
    character(*)              :: soort         !!  Help var. determining the prog. name currently active
    character(*)              :: username      !  Licensee name
    character(*), intent(in)  :: version_short !  Version nr. of the module of the current package
    character(*), intent(in)  :: version_full
!
! Local variables
!
    integer           :: k
    integer           :: n
    character(20)     :: libname
    character(80)     :: txtfil
!
!! executable statements -------------------------------------------------------
!
    nprocs   => gdp%gdusrpar%nprocs
    nprdim   => gdp%gdusrpar%nprdim
    procs    => gdp%gdusrpar%procs
    licfil   => gdp%gdautok%licfil
    !
    error    = .false.
    username = ' '
    n        = 0
    k        = 0
    licfil   = ' '
    !
    nprocs = 0
    do n = 1, mxusrp
       procs(n) = ' '
       do k = 1, 4
          nprdim(k, n) = 0
       enddo
    enddo
    !
    txtfil  = '--------------------------------------------------------------------------------'
    !
    if (.not.parll .or. (parll .and. inode==master)) then
       if (soort == 'tdatom') then
          if (gdp%arch == 'win32') then
             libname = 'flow2d3d.dll'
          else
             libname = 'libflow2d3d.so'
          endif
          write (lunscr, '(a)') txtfil
          write (lunscr, '(2a)') '       ', trim(version_full)
          write (lunscr, '(3a)')  '       ',trim(libname),' entry Flow2D3D::Run'
          write (lunscr, '(a)') txtfil
          write (lunscr, '(a)')
       endif
    endif
    !
    ! set parameters for 'old' style user defined functions
    !
    nprocs = 0
    !
    ! always allow the use of numerical method
    !
    nprocs = nprocs + 1
    procs(nprocs) = 'numerical method    '
    nprdim(1, nprocs) = 1
    !
    ! always allow the use of diagnostic mode
    !
    nprocs = nprocs + 1
    procs(nprocs) = 'diagnostic mode     '
    nprdim(2, nprocs) = 1
    !
    ! always allow the use of rigid sheets
    !
    nprocs = nprocs + 1
    procs(nprocs) = 'rigid sheets        '
    nprdim(1, nprocs) = 1
    !
    ! always allow the use of bc turbulence model
    !
    nprocs = nprocs + 1
    procs(nprocs) = 'bc turbulence model '
    nprdim(1, nprocs) = 1
    !
    ! always allow the use of particle wind factor
    !
    nprocs = nprocs + 1
    procs(nprocs) = 'particle wind factor'
    nprdim(2, nprocs) = 3
    !
    ! always allow the use of z_wave
    !
    nprocs = nprocs + 1
    procs(nprocs) = 'z_wave              '
    nprdim(2, nprocs) = 1
end subroutine flwlic
