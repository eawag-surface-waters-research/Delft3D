module arrayextend
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
! Reallocate arrays
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !

contains

subroutine extend1d_fp(istat,array,newsize,offset)
!!--description-----------------------------------------------------------------
!
! extend 1D real array.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
!
! Global variables
!
    integer, intent(inout)                         :: istat
    real(fp), dimension(:), pointer                :: array
    integer, intent(in)                            :: newsize
    integer, optional                              :: offset
!
! Local variables
!
    real(fp), dimension(:), pointer                :: oldarray
    integer                                        :: oldsize
    integer                                        :: loffset
!
!! executable statements -------------------------------------------------------
!
    if (istat/=0) return
    !
    oldarray => array
    oldsize = size(oldarray,1)
    !
    allocate(array(newsize), stat=istat)
    if (istat/=0) return
    !
    loffset=0
    if (present(offset)) loffset=offset
    !
    array(1:loffset) = 0.0_fp
    array(loffset+1:loffset+oldsize) = oldarray(1:oldsize)
    array(loffset+oldsize+1:newsize) = 0.0_fp
    !
    deallocate(oldarray, stat=istat)
end subroutine extend1d_fp

subroutine extend2d_fp(istat,array,newsize,offset)
!!--description-----------------------------------------------------------------
!
! extend 2D real array in first direction.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
!
! Global variables
!
    integer, intent(inout)                         :: istat
    real(fp), dimension(:,:), pointer              :: array
    integer, intent(in)                            :: newsize
    integer, optional                              :: offset
!
! Local variables
!
    real(fp), dimension(:,:), pointer              :: oldarray
    integer                                        :: oldsize
    integer                                        :: size2
    integer                                        :: loffset
!
!! executable statements -------------------------------------------------------
!
    if (istat/=0) return
    !
    oldarray => array
    oldsize = size(oldarray,1)
    size2 = size(oldarray,2)
    !
    allocate(array(newsize,size2), stat=istat)
    if (istat/=0) return
    !
    loffset=0
    if (present(offset)) loffset=offset
    !
    array(1:loffset,1:size2) = 0.0_fp
    array(loffset+1:loffset+oldsize,1:size2) = oldarray(1:oldsize,1:size2)
    array(loffset+oldsize+1:newsize,1:size2) = 0.0_fp
    !
    deallocate(oldarray, stat=istat)
end subroutine extend2d_fp

subroutine extend1d_int(istat,array,newsize,offset)
!!--description-----------------------------------------------------------------
!
! extend 1D integer array.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
!
! Global variables
!
    integer, intent(inout)                         :: istat
    integer, dimension(:), pointer                 :: array
    integer, intent(in)                            :: newsize
    integer, optional                              :: offset
!
! Local variables
!
    integer, dimension(:), pointer                 :: oldarray
    integer                                        :: oldsize
    integer                                        :: loffset
!
!! executable statements -------------------------------------------------------
!
    if (istat/=0) return
    !
    oldarray => array
    oldsize = size(oldarray,1)
    !
    allocate(array(newsize), stat=istat)
    if (istat/=0) return
    !
    loffset=0
    if (present(offset)) loffset=offset
    !
    array(1:loffset) = 0
    array(loffset+1:loffset+oldsize) = oldarray(1:oldsize)
    array(loffset+oldsize+1:newsize) = 0
    !
    deallocate(oldarray, stat=istat)
end subroutine extend1d_int

subroutine extend2d_int(istat,array,newsize,offset)
!!--description-----------------------------------------------------------------
!
! extend 2D integer array in first direction.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
!
! Global variables
!
    integer, intent(inout)                         :: istat
    integer, dimension(:,:), pointer               :: array
    integer, intent(in)                            :: newsize
    integer, optional                              :: offset
!
! Local variables
!
    integer, dimension(:,:), pointer               :: oldarray
    integer                                        :: oldsize
    integer                                        :: size2
    integer                                        :: loffset
!
!! executable statements -------------------------------------------------------
!
    if (istat/=0) return
    !
    oldarray => array
    oldsize = size(oldarray,1)
    size2 = size(oldarray,2)
    !
    allocate(array(newsize,size2), stat=istat)
    if (istat/=0) return
    !
    loffset=0
    if (present(offset)) loffset=offset
    !
    array(1:loffset,1:size2) = 0
    array(loffset+1:loffset+oldsize,1:size2) = oldarray(1:oldsize,1:size2)
    array(loffset+oldsize+1:newsize,1:size2) = 0
    !
    deallocate(oldarray, stat=istat)
end subroutine extend2d_int

subroutine extend1d_ch(istat,array,nch,newsize,offset)
!!--description-----------------------------------------------------------------
!
! extend 1D character array.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
!
! Global variables
!
    integer, intent(inout)                         :: istat
    integer, intent(in)                            :: nch
    character(nch), dimension(:), pointer          :: array
    integer, intent(in)                            :: newsize
    integer, optional                              :: offset
!
! Local variables
!
    character(nch), dimension(:), pointer          :: oldarray
    integer                                        :: oldsize
    integer                                        :: loffset
!
!! executable statements -------------------------------------------------------
!
    if (istat/=0) return
    !
    oldarray => array
    oldsize = size(oldarray,1)
    !
    allocate(array(newsize), stat=istat)
    if (istat/=0) return
    !
    loffset=0
    if (present(offset)) loffset=offset
    !
    array(1:loffset) = ' '
    array(loffset+1:loffset+oldsize) = oldarray(1:oldsize)
    array(loffset+oldsize+1:newsize) = ' '
    !
    deallocate(oldarray, stat=istat)
end subroutine extend1d_ch

subroutine extend1d_log(istat,array,newsize,offset)
!!--description-----------------------------------------------------------------
!
! extend 1D logical array.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
!
! Global variables
!
    integer, intent(inout)                         :: istat
    logical, dimension(:), pointer                 :: array
    integer, intent(in)                            :: newsize
    integer, optional                              :: offset
!
! Local variables
!
    logical, dimension(:), pointer                 :: oldarray
    integer                                        :: oldsize
    integer                                        :: loffset
!
!! executable statements -------------------------------------------------------
!
    if (istat/=0) return
    !
    oldarray => array
    oldsize = size(oldarray,1)
    !
    allocate(array(newsize), stat=istat)
    if (istat/=0) return
    !
    loffset=0
    if (present(offset)) loffset=offset
    !
    array(1:loffset) = .false.
    array(loffset+1:loffset+oldsize) = oldarray(1:oldsize)
    array(loffset+oldsize+1:newsize) = .false.
    !
    deallocate(oldarray, stat=istat)
end subroutine extend1d_log

end module arrayextend
