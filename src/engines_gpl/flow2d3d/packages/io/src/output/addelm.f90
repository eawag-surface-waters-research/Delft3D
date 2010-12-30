subroutine addelm(nefisgroup ,elmnms_new ,elmqty_new ,elmunt_new ,elmtps_new , &
                & nbytsg_new ,elmdes_new ,dm1        ,dm2        ,dm3        , &
                & dm4        ,dm5        ,dm6        ,lundia     ,gdp)
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
!    Function: Write element dimensions in array elmdms
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer, dimension(:, :)    , pointer :: elmdms
    type (nefiselement)         , pointer :: nefiselem
    integer                     , pointer :: nelmx
    integer      , dimension(:) , pointer :: nbytsg
    character(10), dimension(:) , pointer :: elmunt
    character(16), dimension(:) , pointer :: elmnms
    character(16), dimension(:) , pointer :: elmqty
    character(16), dimension(:) , pointer :: elmtps
    character(64), dimension(:) , pointer :: elmdes
!
! Global variables
!
    integer     , intent(in)  :: dm1
    integer     , intent(in)  :: dm2
    integer     , intent(in)  :: dm3
    integer     , intent(in)  :: dm4
    integer     , intent(in)  :: dm5
    integer     , intent(in)  :: dm6
    integer     , intent(in)  :: lundia
    integer     , intent(in)  :: nbytsg_new
    integer     , intent(in)  :: nefisgroup
    character(*), intent(in)  :: elmdes_new
    character(*), intent(in)  :: elmnms_new
    character(*), intent(in)  :: elmqty_new
    character(*), intent(in)  :: elmtps_new
    character(*), intent(in)  :: elmunt_new
!
! Local variables
!
    integer  :: ie
    integer  :: istat  ! Help var. memory allocation
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefisgroup)
    elmdms  => nefiselem%elmdms
    nelmx   => nefiselem%nelmx
    nbytsg  => nefiselem%nbytsg
    elmunt  => nefiselem%elmunt
    elmnms  => nefiselem%elmnms
    elmqty  => nefiselem%elmqty
    elmtps  => nefiselem%elmtps
    elmdes  => nefiselem%elmdes
    !
    !  Allocate memory: Update space allocated and set
    !                   element properties
    !
    !  Element dimensions
    !
    istat = 0
    ie    = nelmx + 1
    allocate (nefiselem%elmdms(6,ie), stat = istat)
    if (istat==0) then
       nefiselem%elmdms(1:6,1:nelmx) = elmdms(1:6,1:nelmx)
       nefiselem%elmdms(1,ie) = dm1
       nefiselem%elmdms(2,ie) = dm2
       nefiselem%elmdms(3,ie) = dm3
       nefiselem%elmdms(4,ie) = dm4
       nefiselem%elmdms(5,ie) = dm5
       nefiselem%elmdms(6,ie) = dm6
       deallocate (elmdms, stat = istat)
    endif
    !
    !  Element number of bytes
    !
    if (istat==0) allocate (nefiselem%nbytsg(ie), stat = istat)
    if (istat==0) then
       nefiselem%nbytsg(1:nelmx) = nbytsg(1:nelmx)
       nefiselem%nbytsg(ie)      = nbytsg_new
       deallocate (nbytsg, stat = istat)
    endif
    !
    !  Element unit
    !
    if (istat==0) allocate (nefiselem%elmunt(ie), stat = istat)
    if (istat==0) then
       nefiselem%elmunt(1:nelmx) = elmunt(1:nelmx)
       nefiselem%elmunt(ie)      = elmunt_new
       deallocate (elmunt, stat = istat)
    endif
    !
    !  Element name
    !
    if (istat==0) allocate (nefiselem%elmnms(ie), stat = istat)
    if (istat==0) then
       nefiselem%elmnms(1:nelmx) = elmnms(1:nelmx)
       nefiselem%elmnms(ie)      = elmnms_new
       deallocate (elmnms, stat = istat)
    endif
    !
    !  Element quantity
    !
    if (istat==0) allocate (nefiselem%elmqty(ie), stat = istat)
    if (istat==0) then
       nefiselem%elmqty(1:nelmx) = elmqty(1:nelmx)
       nefiselem%elmqty(ie)      = elmqty_new
       deallocate (elmqty, stat = istat)
    endif
    !
    !  Element type
    !
    if (istat==0) allocate (nefiselem%elmtps(ie), stat = istat)
    if (istat==0) then
       nefiselem%elmtps(1:nelmx) = elmtps(1:nelmx)
       nefiselem%elmtps(ie)      = elmtps_new
       deallocate (elmtps, stat = istat)
    endif
    !
    !  Element description
    !
    if (istat==0) allocate (nefiselem%elmdes(ie), stat = istat)
    if (istat==0) then
       nefiselem%elmdes(1:nelmx) = elmdes(1:nelmx)
       nefiselem%elmdes(ie)      = elmdes_new
       deallocate (elmdes, stat = istat)
    endif
    !
    !  Check for memory error
    !
    if (istat/=0) then
       call prterr(lundia, 'U021', 'Addelm: memory alloc error')
       call d3stop(1, gdp)
    endif
    !
    !  Update number of elements in group
    !
    nelmx = ie
end subroutine addelm
