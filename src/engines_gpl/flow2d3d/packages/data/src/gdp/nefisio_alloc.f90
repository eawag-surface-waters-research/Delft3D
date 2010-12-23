subroutine nefisio_alloc(gdp)
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
! NONE
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
!
! Global variables
!
!
! Local variables
!
    integer                          :: i
    integer                          :: m1
    integer                          :: m2
    integer, dimension(nrnefisfiles) :: nelems
!
!! executable statements -------------------------------------------------------
!
    nelems(nefisrdtimc)       = 8
    nelems(nefisrdtimw)       = 3
    nelems(nefisrstcom)       = 8
    nelems(nefisrwbotc)       = 3
    nelems(nefiswrboun)       = 8
    nelems(nefiswrcurt)       = 8
    nelems(nefiswrdwqt)       = 8
    nelems(nefiswrgrid)       = 19
    nelems(nefiswribot)       = 4
    nelems(nefiswridoc)       = 4
    nelems(nefiswridro)       = 9
    if (parll) then
       !
       ! dfwrihis uses addelm and putelt
       !
       nelems(nefiswrihis)    = 0
    else
       !
       ! wrihis uses filldm and putgtr
       !
       nelems(nefiswrihis)    = 28
    endif
    if (parll) then
       !
       ! dfwrimap uses addelm and putelt
       !
       nelems(nefiswrimap)    = 0
    else
       !
       ! wrimap uses filldm and putgtr
       !
       nelems(nefiswrimap)    = 46
    endif
    nelems(nefiswrkenc)       = 3
    nelems(nefiswrkent)       = 4
    nelems(nefiswrparm)       = 9
    nelems(nefiswrplot)       = 4
    nelems(nefiswrrouf)       = 3
    nelems(nefiswrspcp)       = 4
    nelems(nefiswrtdro)       = 2
    nelems(nefiswrthisinf)    = 0
    if (parll) then
       !
       ! dfwrthis uses addelm and putelt
       !
       nelems(nefiswrthis)    = 0
    else
       !
       ! wrthis uses filldm and putgtr
       !
       nelems(nefiswrthis)    = 23
    endif
    nelems(nefiswrtmapinf)    = 0
    nelems(nefiswrtmap)       = 0
    nelems(nefissetwav)       = 9
    nelems(nefiswrsedhinf)    = 0
    nelems(nefiswrsedh)       = 0
    nelems(nefiswrsedminf)    = 0
    nelems(nefiswrsedm)       = 0
    nelems(nefiswrsedmavginf) = 0
    nelems(nefiswrsedmavg)    = 0
    nelems(nefischkcom)       = 4
    nelems(nefiswrwavhinf)    = 0
    nelems(nefiswrwavh)       = 0
    nelems(nefiswrrolm)       = 11
    nelems(nefiswrihisdad)    = 8
    nelems(nefiswrthisdad)    = 5
    nelems(nefiswrihisdis)    = 4
    nelems(nefiswrthisdis)    = 17
    nelems(nefiswrcomwind)    = 0
    nelems(nefiswrsedwaqm)    = 0
    !
    allocate (gdp%nefisio)
    do i = 1, nrnefisfiles
       gdp%nefisio%nefiselem(i)%first  = .true.
       gdp%nefisio%nefiselem(i)%celidt = 1
       gdp%nefisio%nefiselem(i)%nelmx  = nelems(i)
       allocate (gdp%nefisio%nefiselem(i)%elmdms(6, nelems(i)))
       allocate (gdp%nefisio%nefiselem(i)%nbytsg(nelems(i)))
       allocate (gdp%nefisio%nefiselem(i)%elmunt(nelems(i)))
       allocate (gdp%nefisio%nefiselem(i)%elmnms(nelems(i)))
       allocate (gdp%nefisio%nefiselem(i)%elmqty(nelems(i)))
       allocate (gdp%nefisio%nefiselem(i)%elmtps(nelems(i)))
       allocate (gdp%nefisio%nefiselem(i)%elmdes(nelems(i)))
       do m1 = 1, nelems(i)
          do m2 = 1, 2
             gdp%nefisio%nefiselem(i)%elmdms(m2, m1) = 1
          enddo
          do m2 = 3, 6
             gdp%nefisio%nefiselem(i)%elmdms(m2, m1) = 0
          enddo
          gdp%nefisio%nefiselem(i)%nbytsg(m1) = 0
          gdp%nefisio%nefiselem(i)%elmunt(m1) = ' '
          gdp%nefisio%nefiselem(i)%elmnms(m1) = ' '
          gdp%nefisio%nefiselem(i)%elmqty(m1) = ' '
          gdp%nefisio%nefiselem(i)%elmtps(m1) = ' '
          gdp%nefisio%nefiselem(i)%elmdes(m1) = ' '
       enddo
    enddo
end subroutine nefisio_alloc
