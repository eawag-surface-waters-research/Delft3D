subroutine avalan(dps       ,depchg    ,gvu       ,guv       , &
                & icx       ,icy       ,gsqs      ,kcs       ,gdp       )
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
!!--description-----------------------------------------------------------------
!
! Smooth bottom, using avalance effect
! At initialization :
! Checks flag by calling Input() and returns if OFF.
! Get nof points and slope from Input()
! Allocates local memory
! Continues processing :
! When bottom slope in the model becomes steeper then the
! criterium given by the user, avalancing is applied.
! Avalancing means distributing the volume difference
! between a cell and it's neighbour, so they get the same
! depth.
! The whole grid is tested. Once in a direction along
! M-lines using the cell to the right as it's neighbour
! and once in a direction along N-lines, using the cell
! above as it's neighbour.
! Updates depths for avalance effects.
! Depth changes caused by avalance effects are
! accumulatively held in a local array, and afterwards
! added to depchg().
!
! If compiled with option /d_lines the routine will produce
! a file = avalan.rpt, with debug information.
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
    ! They replace the  include igd / include igp lines
    !
    integer                , pointer :: mmax
    integer                , pointer :: nmmax
    integer                , pointer :: kc
    logical                , pointer :: initlz
    real(fp), dimension(:) , pointer :: depchange
    logical                , pointer :: scour
    real(fp)               , pointer :: slope
    integer                , pointer :: lundia
!
! Global variables
!
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: kcs    !  Description and declaration in iidim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              :: depchg !  Description and declaration in rjdim.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)              :: dps    !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: gsqs   !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: guv    !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: gvu    !  Description and declaration in rjdim.f90
!
! Local variables
!
    integer :: icx
    integer :: icy
    integer :: istat
    integer :: nm
    integer :: nmu
    integer :: num
    real(fp):: anm
    real(fp):: anmu
    real(fp):: anum
    real(fp):: ddep
    real(fp):: ddnm
    real(fp):: ddnmu
    real(fp):: ddnum
    real(fp):: depnm
    real(fp):: depnmu
    real(fp):: depnum
    real(fp):: gemvol
    real(fp):: size
!
!! executable statements -------------------------------------------------------
!
    mmax       => gdp%d%mmax
    nmmax      => gdp%d%nmmax
    kc         => gdp%d%kc
    initlz     => gdp%gdavalan%initlz
    depchange  => gdp%gdavalan%depchange
    scour      => gdp%gdscour%scour
    slope      => gdp%gdscour%slope
    lundia     => gdp%gdinout%lundia
    !
    if (initlz) then
       !
       ! processed only once at first call
       !
       initlz = .false.
       if (.not.scour) then
          return
       endif
       !
       ! - nmmax is known through `dimens.igp'
       ! - use gdp structure for allocation
       ! - include .igp again to be sure that the local pointer depchange
       !   points to the allocated memory
       !
       allocate (gdp%gdavalan%depchange(nmmax), stat = istat)
    initlz     => gdp%gdavalan%initlz
    depchange  => gdp%gdavalan%depchange
       if (istat/=0) then
          call prterr(lundia, 'U021', 'Avalan: memory alloc error')
          call d3stop(1, gdp)
       endif
    endif
    !
    ! reset local array
    !
    do nm = 1, nmmax
       depchange(nm) = 0.0
    enddo
    do nm = 1, nmmax-icx
       nmu = nm + icx
       if (kcs(nm)>0 .and. kcs(nmu)>0) then
          depnm  = real(dps(nm),fp)
          depnmu = real(dps(nmu),fp)
          ddep   = depnmu - depnm
          size   = gvu(nm)
          if (abs(ddep/size)>slope) then
             anm            = gsqs(nm)
             anmu           = gsqs(nmu)
             gemvol         = (anm + anmu)/2.0
             ddnm           = 0.5*ddep*gemvol/anm
             dps(nm)        = real(depnm + ddnm,prec)
             depchange(nm)  = depchange(nm) - ddnm
             ddnmu          = -0.5*ddep*gemvol/anmu
             dps(nmu)       = real(depnmu + ddnmu,prec)
             depchange(nmu) = depchange(nmu) - ddnmu
          endif
       endif
    enddo
    do nm = 1, nmmax-icy
       num = nm + icy
       !
       ! Do the same thing as before but now in the other direction
       !
       if (kcs(nm)>0 .and. kcs(num)>0) then
          depnm  = real(dps(nm),fp)
          depnum = real(dps(num),fp)
          ddep   = depnum - depnm
          size   = guv(nm)
          if (abs(ddep/size)>slope) then
             anm            = gsqs(nm)
             anum           = gsqs(num)
             gemvol         = (anm + anum)/2.0
             ddnm           = 0.5*ddep*gemvol/anm
             dps(nm)        = real(depnm + ddnm,prec)
             depchange(nm)  = depchange(nm) - ddnm
             ddnum          = -0.5*ddep*gemvol/anum
             dps(num)       = real(depnum + ddnum,prec)
             depchange(num) = depchange(num) - ddnum
          endif
       endif
    enddo
    do nm = 1, nmmax
       depchg(nm) = depchg(nm) + depchange(nm)
    enddo
end subroutine avalan
