subroutine dredgedump(dbodsd    ,cdryb     ,lsedtotparam   , &
                    & nst       ,timhr     ,morft     , &
                    & gdp       )
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
! Switch from nm to n,m
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
    include 'fsm.i'
    include 'tri-dyn.igd'
    integer , pointer :: c
    integer , pointer :: dp
    integer , pointer :: dps
    integer , pointer :: s1
    integer , pointer :: kfs
    integer , pointer :: kfsed
    integer , pointer :: mmax
    integer , pointer :: nmmax
    integer , pointer :: lsed
    integer , pointer :: lsedtot
!
! Global variables
!
    integer                                            , intent(in) :: nst
    integer                                            , intent(in) :: lsedtotparam ! Copy of lsedtot
    real(hp)                                           , intent(in) :: morft
    real(fp)                                           , intent(in) :: timhr
    real(fp), dimension(lsedtotparam)                  , intent(in) :: cdryb   !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtotparam)        :: dbodsd  !  Description and declaration in rjdim.f90
!
! Local variables
!
    real(fp)                                                        :: morhr
!
!! executable statements -------------------------------------------------------
!
    mmax      => gdp%d%mmax
    nmmax     => gdp%d%nmmax
    lsed      => gdp%d%lsed
    lsedtot   => gdp%d%lsedtot
    c         => gdp%gdr_i_ch%c
    dp        => gdp%gdr_i_ch%dp
    dps       => gdp%gdr_i_ch%dps
    s1        => gdp%gdr_i_ch%s1
    kfs       => gdp%gdr_i_ch%kfs
    kfsed     => gdp%gdr_i_ch%kfsed
    !
    morhr = real(morft*24.0_hp,fp)
    call dredge(nmmax   ,lsedtot ,nst     , &
              & cdryb   ,d(dps)  ,dbodsd  ,i(kfsed), &
              & r(s1)   ,timhr   ,morhr   ,gdp     )
end subroutine dredgedump
