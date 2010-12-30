subroutine initeqtran(gdp       )
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
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                        , pointer :: npar
    character(256), dimension(:)   , pointer :: dll_function_settle
    character(256), dimension(:)   , pointer :: dll_name_settle
    integer,        dimension(:)   , pointer :: dll_handle_settle
    integer       , dimension(:)   , pointer :: dll_integers_settle
    real(hp)      , dimension(:)   , pointer :: dll_reals_settle
    character(256), dimension(:)   , pointer :: dll_strings_settle
    character(256), dimension(:)   , pointer :: dll_function
    integer,        dimension(:)   , pointer :: dll_handle
    integer       , dimension(:)   , pointer :: dll_integers
    real(hp)      , dimension(:)   , pointer :: dll_reals
    character(256), dimension(:)   , pointer :: dll_strings
    character(256), dimension(:)   , pointer :: flstrn
    integer,        dimension(:)   , pointer :: iform
    character(256), dimension(:)   , pointer :: name
    real(fp),       dimension(:,:) , pointer :: par
!
!! executable statements -------------------------------------------------------
!
    npar          => gdp%gdeqtran%npar
    dll_function_settle  => gdp%gdeqtran%dll_function_settle
    dll_name_settle      => gdp%gdeqtran%dll_name_settle
    dll_handle_settle    => gdp%gdeqtran%dll_handle_settle
    dll_integers_settle  => gdp%gdeqtran%dll_integers_settle
    dll_reals_settle     => gdp%gdeqtran%dll_reals_settle
    dll_strings_settle   => gdp%gdeqtran%dll_strings_settle
    dll_function  => gdp%gdeqtran%dll_function
    dll_handle    => gdp%gdeqtran%dll_handle
    dll_integers  => gdp%gdeqtran%dll_integers
    dll_reals     => gdp%gdeqtran%dll_reals
    dll_strings   => gdp%gdeqtran%dll_strings
    flstrn        => gdp%gdeqtran%flstrn
    iform         => gdp%gdeqtran%iform
    name          => gdp%gdeqtran%name
    par           => gdp%gdeqtran%par
    !
    ! Note: 30 is hardcoded in sediment transport formulae
    !
    npar   = 30
    !
    nullify(gdp%gdeqtran%dll_function_settle)
    nullify(gdp%gdeqtran%dll_name_settle)
    nullify(gdp%gdeqtran%dll_handle_settle)
    nullify(gdp%gdeqtran%dll_integers_settle)
    nullify(gdp%gdeqtran%dll_reals_settle)
    nullify(gdp%gdeqtran%dll_strings_settle)
    nullify(gdp%gdeqtran%dll_function)
    nullify(gdp%gdeqtran%dll_handle)
    nullify(gdp%gdeqtran%dll_integers)
    nullify(gdp%gdeqtran%dll_reals)
    nullify(gdp%gdeqtran%dll_strings)
    nullify(gdp%gdeqtran%flstrn)
    nullify(gdp%gdeqtran%iform)
    nullify(gdp%gdeqtran%name)
    nullify(gdp%gdeqtran%par)
end subroutine initeqtran
