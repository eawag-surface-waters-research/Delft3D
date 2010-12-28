subroutine compdmean(frac      ,seddm     ,nmmax     ,lsedtot   , &
                   & sedtyp    ,dm        ,sedd50fld ,logsedsig , &
                   & nmlb      ,nmub      )
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
! Function: Determines the arithmetic mean diameter of the non-mud fractions
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
    real(fp), dimension(lsedtot)                        , intent(in)  :: seddm     ! mean diameter of sediment fraction
    real(fp), dimension(nmlb:nmub)                      , intent(out) :: dm        ! arithmetic mean diameter field
    real(fp), dimension(lsedtot)                        , intent(in)  :: logsedsig ! std deviation of sediment diameter
    real(fp), dimension(nmlb:nmub)                      , intent(in)  :: sedd50fld ! D50 field (in case of 1 sediment fraction)
    character(4), dimension(lsedtot)                    , intent(in)  :: sedtyp    ! sediment type: sand/mud/bedload
!
! Local variables
!
    integer  :: l
    integer  :: nm
    real(fp) :: fracnonmud
    real(fp) :: mulfac
!
!! executable statements -------------------------------------------------------
!
    ! Calculate mean diameter by a weighted average of the diameters
    ! of the non-mud sediments. Divide by the total percentage of
    ! non-mud sediments to exclude the mud fractions from the computation.
    !
    if (lsedtot==1 .and. seddm(1)<0.0 .and. sedtyp(1) /= 'mud') then
       mulfac = exp(0.5 * logsedsig(1) * logsedsig(1))
       do nm = 1,nmmax
          dm(nm) = sedd50fld(nm)*mulfac
       enddo
    else
       do nm = 1,nmmax
          fracnonmud = 0.0
          dm(nm)     = 0.0
          do l = 1, lsedtot
             if (sedtyp(l) /= 'mud') then
                dm(nm)     = dm(nm) + frac(nm,l) * seddm(l)
                fracnonmud = fracnonmud + frac(nm,l)
             endif
          enddo
          if (fracnonmud > 0.0) then
             dm(nm) = dm(nm) / fracnonmud
          endif
       enddo
    endif
end subroutine compdmean
