subroutine comphidexp(frac      ,dm        ,nmmax     ,lsedtot   , &
                    & sedd50    ,hidexp    ,gdp       )
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
!    Function: - Update underlayer bookkeeping system for erosion/sedimentation
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
    real(fp) , pointer :: asklhe
    real(fp) , pointer :: mwwjhe
    integer  , pointer :: ihidexp
!
! Local parameters
!
    real(hp), parameter :: log10_19 = 1.27875360095283_hp
    real(fp), parameter :: dmmin    = 1.0e-4_fp                                  ! minimum value of dm
!
! Global variables
!
    integer                                             , intent(in)  :: lsedtot
    integer                                             , intent(in)  :: nmmax   !  Description and declaration in dimens.igs
    real(fp), dimension(lsedtot)                        , intent(in)  :: sedd50  !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot)               :: hidexp
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot) , intent(in)  :: frac
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: dm
!
! Local variables
!
    integer  :: l
    integer  :: ll
    integer  :: nm
    real(fp) :: dd
    real(fp) :: phi
    real(fp) :: pei
    real(fp) :: dmloc    ! local copy of dm limited to value larger than dmmin specified here
!
!! executable statements -------------------------------------------------------
!
    asklhe              => gdp%gdmorpar%asklhe
    mwwjhe              => gdp%gdmorpar%mwwjhe
    ihidexp             => gdp%gdmorpar%ihidexp
    !
    select case(ihidexp)
    case(2) ! Egiazaroff
       !
       do nm = 1, nmmax
          dmloc = max(dm(nm), dmmin)
          do l = 1, lsedtot
             !
             dd           = sedd50(l)/dmloc
             hidexp(nm,l) = (log10_19 / (log10_19 + log10(dd)))**2.0
             !
          enddo
       enddo
       !
    case(3) ! Ashida & Michiue
       !
       do nm = 1, nmmax
          dmloc = max(dm(nm), dmmin)
          do l = 1, lsedtot
             !
             dd = sedd50(l)/dmloc
             !
             if (dd<0.38889) then
                hidexp(nm,l) = 0.8429 / dd
             else
                hidexp(nm,l) = (log10_19 / (log10_19 + log10(dd)))**2.0
             endif
             !
          enddo
       enddo
       !
    case(4) ! Parker, Klingeman, McLean
       ! Soehngen, Kellermann, Loy
       !
       do nm = 1, nmmax
          dmloc = max(dm(nm), dmmin)
          do l = 1, lsedtot
             !
             dd           = sedd50(l)/dmloc
             hidexp(nm,l) = dd**(-asklhe)
             !
          enddo
       enddo
       !
    case(5) ! Wu, Wang, Jia
       !
       do nm = 1, nmmax
          do l = 1, lsedtot
             !
             phi = 0.0
             do ll = 1, lsedtot
                phi = phi + frac(nm,ll) * sedd50(ll) / (sedd50(l) + sedd50(ll))
             enddo
             pei          = 1.0 - phi
             hidexp(nm,l) = (pei/phi)**mwwjhe
             !
          enddo
       enddo
       !
    case default ! (1)
       !
       ! no hiding & exposure, hidexp initialized to 1.0 in erosed
       ! keep these values
       !
    endselect
end subroutine comphidexp
