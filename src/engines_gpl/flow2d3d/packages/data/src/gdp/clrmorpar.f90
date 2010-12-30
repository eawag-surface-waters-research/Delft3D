subroutine clrmorpar(istat, gdp)
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use flow_tables
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                             , pointer :: morfac
    type (handletype)                    , pointer :: bcmfile
    type (handletype)                    , pointer :: morfacfile
    type (moroutputtype)                 , pointer :: moroutput
    type (mornumericstype)               , pointer :: mornum
    type (bedbndtype)     , dimension(:) , pointer :: morbnd
    type (cmpbndtype)     , dimension(:) , pointer :: cmpbnd
    real(hp)              , dimension(:) , pointer :: mergebuf
    real(fp)              , dimension(:) , pointer :: xx
    type (gd_morpar)                     , pointer :: gdmorpar
!
! Global variables
!
    integer,intent(out) :: istat
!
! Local variables
!
    integer :: i
!
!! executable statements -------------------------------------------------------
!
    morfac              => gdp%gdmorpar%morfac
    bcmfile             => gdp%gdmorpar%bcmfile
    morfacfile          => gdp%gdmorpar%morfacfile
    moroutput           => gdp%gdmorpar%moroutput
    mornum              => gdp%gdmorpar%mornum
    morbnd              => gdp%gdmorpar%morbnd
    cmpbnd              => gdp%gdmorpar%cmpbnd
    mergebuf            => gdp%gdmorpar%mergebuf
    xx                  => gdp%gdmorpar%xx
    gdmorpar            => gdp%gdmorpar
    !
    if (associated(gdmorpar%morbnd)) then
       do i = 1, size(gdmorpar%morbnd)
          if (associated(morbnd(i)%idir))      deallocate(morbnd(i)%idir,      STAT = istat)
          if (associated(morbnd(i)%nm))        deallocate(morbnd(i)%nm,        STAT = istat)
          if (associated(morbnd(i)%nxmx))      deallocate(morbnd(i)%nxmx,      STAT = istat)
          if (associated(morbnd(i)%alfa_dist)) deallocate(morbnd(i)%alfa_dist, STAT = istat)
          if (associated(morbnd(i)%alfa_mag))  deallocate(morbnd(i)%alfa_mag,  STAT = istat)
       enddo
       deallocate(gdmorpar%morbnd, STAT = istat)
    endif
    if (associated(gdmorpar%cmpbnd))    deallocate(gdmorpar%cmpbnd,    STAT = istat)
    if (associated(gdmorpar%xx))        deallocate(gdmorpar%xx,        STAT = istat)
    if (associated(gdmorpar%mergebuf))  deallocate(gdmorpar%mergebuf,  STAT = istat)
    if (associated(gdmorpar%moroutput)) deallocate(gdmorpar%moroutput, STAT = istat)
    if (associated(gdmorpar%mornum))    deallocate(gdmorpar%mornum,    STAT = istat)
    call cleartable(gdmorpar%bcmfile)
    call cleartable(gdmorpar%morfacfile)
    !
end subroutine clrmorpar
