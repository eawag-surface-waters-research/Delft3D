subroutine incmeteo(timhr  ,grdang ,windu  ,windv ,patm   , &
                    kcs    ,alfas ,         &
                    windsu ,windsv ,w10mag ,gdp   )
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
!    Function: - Update meteo related items
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use meteo
    use ec_module
    use precision
    use dfparall
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer  , pointer :: nmax
    integer  , pointer :: mmax
    integer  , pointer :: nlb
    integer  , pointer :: nub
    integer  , pointer :: mlb
    integer  , pointer :: mub
    integer  , pointer :: nmaxus
    integer  , pointer :: kc
    integer  , pointer :: itdate
    real(fp) , pointer :: tzone
    type(tECHandle), pointer :: ECHandle
    integer        , pointer :: patmECItemId
    integer        , pointer :: uwindECItemId
    integer        , pointer :: vwindECItemId
    real(hp)       , pointer :: dtimmin       ! Current timestep (in min) in high precision
!
! Global variables
!
    integer    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcs
    real(fp)                                                        , intent(in)  :: timhr
    real(fp)                                                        , intent(in)  :: grdang
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: alfas
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: windu
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: windv
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: windsu
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: windsv
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: patm
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: w10mag
!
! Local variables
!
    real(fp)                 :: time
    logical                  :: success
!
!! executable statements -------------------------------------------------------
!
    nmax          => gdp%d%nmax
    mmax          => gdp%d%mmax
    nlb           => gdp%d%nlb
    nub           => gdp%d%nub
    mlb           => gdp%d%mlb
    mub           => gdp%d%mub
    nmaxus        => gdp%d%nmaxus
    kc            => gdp%d%kc
    itdate        => gdp%gdexttim%itdate
    tzone         => gdp%gdexttim%tzone
    ECHandle      => gdp%gd_ECHandle
    patmECItemId  => gdp%patmECItemId
    uwindECItemId => gdp%uwindECItemId
    vwindECItemId => gdp%vwindECItemId
    dtimmin       => gdp%gdinttim%dtimmin
    !
    time = timhr*60.0  ! time in minutes
    !
    ! update all meteo items (if necessary)
    !
    if (patmECItemId == -1) then
       success = meteoupdate(gdp%runid, itdate, tzone, time)
       call checkmeteoresult(success, gdp)
       !
       ! update wind arrays
       !
       success = getmeteoval(gdp%runid, 'windu', time, gdp%gdparall%mfg, gdp%gdparall%nfg, nlb, nub, mlb, mub, windu)
       call checkmeteoresult(success, gdp)
       success = getmeteoval(gdp%runid, 'windv', time, gdp%gdparall%mfg, gdp%gdparall%nfg, nlb, nub, mlb, mub, windv)
       call checkmeteoresult(success, gdp)
       success = getmeteoval(gdp%runid,  'patm', time, gdp%gdparall%mfg, gdp%gdparall%nfg, nlb, nub, mlb, mub,  patm)
       call checkmeteoresult(success, gdp)
    else
       success = getVal(ECHandle, patmECItemId , dtimmin,  patm, nlb, nub, mlb, mub)
       call checkResult(ECHandle, success)
       success = getVal(ECHandle, uwindECItemId, dtimmin, windu, nlb, nub, mlb, mub)
       call checkResult(ECHandle, success)
       success = getVal(ECHandle, vwindECItemId, dtimmin, windv, nlb, nub, mlb, mub)
       call checkResult(ECHandle, success)
    endif
    !
    ! Exchange data between partitions
    ! NB: wind velocities might not be required
    !     Patm required as gradient calculated in cucnp
    !
    call dfexchg(patm, 1, 1, dfloat, gdp)    
    call dfexchg(windu, 1, 1, dfloat, gdp)    
    call dfexchg(windv, 1, 1, dfloat, gdp)    
    call windtostress(mmax ,nmax ,nmaxus, grdang, kcs, w10mag, windu, windv, windsu, windsv, gdp)
    !
    ! Exchange data between partitions
    !
    call dfexchg(windsu, 1, 1, dfloat, gdp)
    call dfexchg(windsv, 1, 1, dfloat, gdp)
    call windtogridc (mmax  ,nmax  ,nmaxus,kcs   ,alfas ,windsu,windsv       ,gdp)
    !
    ! Exchange data between partitions
    !
    call dfexchg(windsu, 1, 1, dfloat, gdp)
    call dfexchg(windsv, 1, 1, dfloat, gdp)
end subroutine incmeteo
