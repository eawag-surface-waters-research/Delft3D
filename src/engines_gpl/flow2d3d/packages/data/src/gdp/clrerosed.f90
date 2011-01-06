subroutine clrerosed(istat, gdp)
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
! NONE
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
    !
    real(fp), dimension(:)   , pointer :: bc_mor_array
    real(fp), dimension(:,:) , pointer :: dbodsd
    real(fp), dimension(:)   , pointer :: dcwwlc
    real(fp), dimension(:)   , pointer :: dm
    real(fp), dimension(:)   , pointer :: dg
    real(fp), dimension(:,:) , pointer :: dxx
    real(fp), dimension(:)   , pointer :: dzduu
    real(fp), dimension(:)   , pointer :: dzdvv
    real(fp), dimension(:)   , pointer :: epsclc
    real(fp), dimension(:)   , pointer :: epswlc
    real(fp), dimension(:,:) , pointer :: fixfac
    real(fp), dimension(:,:) , pointer :: frac
    real(fp), dimension(:)   , pointer :: mudfrac
    real(fp), dimension(:,:) , pointer :: hidexp
    real(fp), dimension(:)   , pointer :: rsdqlc
    real(fp), dimension(:,:) , pointer :: sbcu
    real(fp), dimension(:,:) , pointer :: sbcv
    real(fp), dimension(:,:) , pointer :: sbcuu
    real(fp), dimension(:,:) , pointer :: sbcvv
    real(fp), dimension(:,:) , pointer :: sbuuc
    real(fp), dimension(:,:) , pointer :: sbvvc
    real(fp), dimension(:,:) , pointer :: sbwu
    real(fp), dimension(:,:) , pointer :: sbwv
    real(fp), dimension(:,:) , pointer :: sbwuu
    real(fp), dimension(:,:) , pointer :: sbwvv
    real(fp), dimension(:)   , pointer :: sddflc
    real(fp), dimension(:,:) , pointer :: ssuuc
    real(fp), dimension(:,:) , pointer :: ssvvc
    real(fp), dimension(:,:) , pointer :: sswu
    real(fp), dimension(:,:) , pointer :: sswv
    real(fp), dimension(:,:) , pointer :: sswuu
    real(fp), dimension(:,:) , pointer :: sswvv
    real(fp), dimension(:,:) , pointer :: sucor
    real(fp), dimension(:,:) , pointer :: svcor
    real(fp), dimension(:,:) , pointer :: sutot
    real(fp), dimension(:,:) , pointer :: svtot
    real(fp), dimension(:,:) , pointer :: taurat
    real(fp), dimension(:)   , pointer :: ust2
    real(fp), dimension(:)   , pointer :: umod
    real(fp), dimension(:)   , pointer :: uuu
    real(fp), dimension(:)   , pointer :: vvv
    real(fp), dimension(:)   , pointer :: wslc
    real(fp), dimension(:)   , pointer :: zumod
    type (sv_erosed)         , pointer :: sverosed
!
! Global variables
!
    integer,intent(out) :: istat
!
!! executable statements -------------------------------------------------------
!
    bc_mor_array  => gdp%gderosed%bc_mor_array
    dbodsd        => gdp%gderosed%dbodsd
    dcwwlc        => gdp%gderosed%dcwwlc
    dm            => gdp%gderosed%dm
    dg            => gdp%gderosed%dg
    dxx           => gdp%gderosed%dxx
    dzduu         => gdp%gderosed%dzduu
    dzdvv         => gdp%gderosed%dzdvv
    epsclc        => gdp%gderosed%epsclc
    epswlc        => gdp%gderosed%epswlc
    fixfac        => gdp%gderosed%fixfac
    frac          => gdp%gderosed%frac
    mudfrac       => gdp%gderosed%mudfrac
    hidexp        => gdp%gderosed%hidexp
    rsdqlc        => gdp%gderosed%rsdqlc
    sbcu          => gdp%gderosed%sbcu
    sbcv          => gdp%gderosed%sbcv
    sbcuu         => gdp%gderosed%sbcuu
    sbcvv         => gdp%gderosed%sbcvv
    sbuuc         => gdp%gderosed%sbuuc
    sbvvc         => gdp%gderosed%sbvvc
    sbwu          => gdp%gderosed%sbwu
    sbwv          => gdp%gderosed%sbwv
    sbwuu         => gdp%gderosed%sbwuu
    sbwvv         => gdp%gderosed%sbwvv
    sddflc        => gdp%gderosed%sddflc
    ssuuc         => gdp%gderosed%ssuuc
    ssvvc         => gdp%gderosed%ssvvc
    sswu          => gdp%gderosed%sswu
    sswv          => gdp%gderosed%sswv
    sswuu         => gdp%gderosed%sswuu
    sswvv         => gdp%gderosed%sswvv
    sucor         => gdp%gderosed%sucor
    svcor         => gdp%gderosed%svcor
    sutot         => gdp%gderosed%sutot
    svtot         => gdp%gderosed%svtot
    taurat        => gdp%gderosed%taurat
    ust2          => gdp%gderosed%ust2
    umod          => gdp%gderosed%umod
    uuu           => gdp%gderosed%uuu
    vvv           => gdp%gderosed%vvv
    wslc          => gdp%gderosed%wslc
    zumod         => gdp%gderosed%zumod
    sverosed      => gdp%gderosed
    !
    if (associated(sverosed%bc_mor_array)) deallocate(sverosed%bc_mor_array, STAT = istat)
    if (associated(sverosed%dcwwlc))       deallocate(sverosed%dcwwlc      , STAT = istat)
    if (associated(sverosed%epsclc))       deallocate(sverosed%epsclc      , STAT = istat)
    if (associated(sverosed%epswlc))       deallocate(sverosed%epswlc      , STAT = istat)
    if (associated(sverosed%rsdqlc))       deallocate(sverosed%rsdqlc      , STAT = istat)
    if (associated(sverosed%sddflc))       deallocate(sverosed%sddflc      , STAT = istat)
    if (associated(sverosed%umod))         deallocate(sverosed%umod        , STAT = istat)
    if (associated(sverosed%ust2))         deallocate(sverosed%ust2        , STAT = istat)
    if (associated(sverosed%uuu))          deallocate(sverosed%uuu         , STAT = istat)
    if (associated(sverosed%vvv))          deallocate(sverosed%vvv         , STAT = istat)
    if (associated(sverosed%wslc))         deallocate(sverosed%wslc        , STAT = istat)
    if (associated(sverosed%zumod))        deallocate(sverosed%zumod       , STAT = istat)
    if (associated(sverosed%dbodsd))       deallocate(sverosed%dbodsd      , STAT = istat)
    if (associated(sverosed%fixfac))       deallocate(sverosed%fixfac      , STAT = istat)
    if (associated(sverosed%frac))         deallocate(sverosed%frac        , STAT = istat)
    if (associated(sverosed%mudfrac))      deallocate(sverosed%mudfrac     , STAT = istat)
    if (associated(sverosed%dm))           deallocate(sverosed%dm          , STAT = istat)
    if (associated(sverosed%dg))           deallocate(sverosed%dg          , STAT = istat)
    if (associated(sverosed%dxx))          deallocate(sverosed%dxx         , STAT = istat)
    if (associated(sverosed%hidexp))       deallocate(sverosed%hidexp      , STAT = istat)
    if (associated(sverosed%sbcu))         deallocate(sverosed%sbcu        , STAT = istat)
    if (associated(sverosed%sbcv))         deallocate(sverosed%sbcv        , STAT = istat)
    if (associated(sverosed%sbcuu))        deallocate(sverosed%sbcuu       , STAT = istat)
    if (associated(sverosed%sbcvv))        deallocate(sverosed%sbcvv       , STAT = istat)
    if (associated(sverosed%sbuuc))        deallocate(sverosed%sbuuc       , STAT = istat)
    if (associated(sverosed%sbvvc))        deallocate(sverosed%sbvvc       , STAT = istat)
    if (associated(sverosed%sbwu))         deallocate(sverosed%sbwu        , STAT = istat)
    if (associated(sverosed%sbwv))         deallocate(sverosed%sbwv        , STAT = istat)
    if (associated(sverosed%sbwuu))        deallocate(sverosed%sbwuu       , STAT = istat)
    if (associated(sverosed%sbwvv))        deallocate(sverosed%sbwvv       , STAT = istat)
    if (associated(sverosed%ssuuc))        deallocate(sverosed%ssuuc       , STAT = istat)
    if (associated(sverosed%ssvvc))        deallocate(sverosed%ssvvc       , STAT = istat)
    if (associated(sverosed%sswu))         deallocate(sverosed%sswu        , STAT = istat)
    if (associated(sverosed%sswv))         deallocate(sverosed%sswv        , STAT = istat)
    if (associated(sverosed%sswuu))        deallocate(sverosed%sswuu       , STAT = istat)
    if (associated(sverosed%sswvv))        deallocate(sverosed%sswvv       , STAT = istat)
    if (associated(sverosed%sucor))        deallocate(sverosed%sucor       , STAT = istat)
    if (associated(sverosed%svcor))        deallocate(sverosed%svcor       , STAT = istat)
    if (associated(sverosed%sutot))        deallocate(sverosed%sutot       , STAT = istat)
    if (associated(sverosed%svtot))        deallocate(sverosed%svtot       , STAT = istat)
    if (associated(sverosed%dzduu))        deallocate(sverosed%dzduu       , STAT = istat)
    if (associated(sverosed%dzdvv))        deallocate(sverosed%dzdvv       , STAT = istat)
    if (associated(sverosed%taurat))       deallocate(sverosed%taurat      , STAT = istat)
end subroutine clrerosed
