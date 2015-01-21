subroutine wrtmap_nm(fds, grpnam_in, uindex, nf, nl, mf, ml, iarrc, gdp, &
                   & ierr, var, varnam_in)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2015.                                
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
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: glbarr2, dfgather, dfgather_seq
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                            :: ierr
    integer                                                            :: fds
    integer      , dimension(0:nproc-1)                                :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                              :: iarrc         ! array containing collected grid indices 
    integer      , dimension(3,5)                                      :: uindex
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) :: var
    character(*)                                                       :: varnam_in
    character(*)                                                       :: grpnam_in
    !
    ! local
    integer                                       :: namlen
    character(16)                                 :: varnam
    character(16)                                 :: grpnam
    integer                        , external     :: putelt
    !
    ! body
    namlen = min (16,len(varnam_in))
    varnam = varnam_in(1:namlen)
    namlen = min (16,len(grpnam_in))
    grpnam = grpnam_in(1:namlen)
    !
    if (parll) then
       call dfgather(var,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(var, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
    endif       
    if (inode == master) then
       ierr = putelt(fds, grpnam, varnam, uindex, 1, glbarr2)
    endif
end subroutine wrtmap_nm
