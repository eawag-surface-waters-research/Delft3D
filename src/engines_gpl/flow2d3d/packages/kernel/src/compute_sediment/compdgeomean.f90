subroutine compdgeomean(frac      ,sedd50    ,nmmax     ,lsedtot   , &
                      & sedtyp    ,dg        ,sedd50fld ,nmlb      , &
                      & nmub      )
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
! Function: Determines the geometric mean diameter of the non-mud fractions
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
!
! Global variables
!
    integer                                             , intent(in)  :: lsedtot   ! number of sediment fractions
    integer                                             , intent(in)  :: nmmax     ! last space index to be processed
    integer                                             , intent(in)  :: nmlb      ! start space index
    integer                                             , intent(in)  :: nmub      ! end space index
    real(fp), dimension(nmlb:nmub, lsedtot)             , intent(in)  :: frac      ! fractional composition of sediment
    real(fp), dimension(lsedtot)                        , intent(in)  :: sedd50    ! D50 of sediment fraction
    real(fp), dimension(nmlb:nmub)                      , intent(out) :: dg        ! geometric mean diameter field
    real(fp), dimension(nmlb:nmub)                      , intent(in)  :: sedd50fld ! D50 field (in case of 1 sediment fraction)
    character(4), dimension(lsedtot)                    , intent(in)  :: sedtyp    ! sediment type: sand/mud/bedload
!
! Local variables
!
    integer  :: l
    integer  :: nm
    real(fp) :: fracnonmud
!
!! executable statements -------------------------------------------------------
!
    ! Calculate geometric mean diameter by a weighted average of the
    ! diameters of the non-mud sediments.
    !
    if (lsedtot==1 .and. sedd50(1)<0.0) then
       do nm = 1,nmmax
          dg(nm) = sedd50fld(nm)
       enddo
    else
       do nm = 1,nmmax
          fracnonmud = 0.0
          dg(nm)     = 1.0
          do l = 1, lsedtot
             if (sedtyp(l) /= 'mud') then
                dg(nm)     = dg(nm) * (sedd50(l)**frac(nm,l))
                fracnonmud = fracnonmud + frac(nm,l)
             endif
          enddo
          if (fracnonmud > 0.0) then
             dg(nm) = dg(nm)**(1.0/fracnonmud)
          else
             dg(nm) = 0.0
          endif
       enddo
    endif
end subroutine compdgeomean
