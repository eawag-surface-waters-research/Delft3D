subroutine tfzeta(timnow    ,nmax      ,mmax      ,tgfsep    ,xz        , &
                & yz        ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!    Function: This routine calculates the equilibrium tide
!              generated by the tide generating force.
!              To maintain the mass balance, the result will be
!              stored in array WRKB17 to be passed to routines
!              ADI and AOI and used in the acceleration term in
!              UZD, CUCNP & CUCNP2
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp) , dimension(:) , pointer :: tgfcoe
    real(hp),  dimension(:) , pointer :: tgffr
    real(hp) , dimension(:) , pointer :: tgfv0u
    real(hp) , dimension(:) , pointer :: tgfw
    integer  , dimension(:) , pointer :: tgftyp
    integer                 , pointer :: nrcmp
    real(fp)                , pointer :: dt
    real(fp)                , pointer :: tunit
    real(fp)                , pointer :: tzone
!
! Global variables
!
    integer  , intent(in)                                                       :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer  , intent(in)                                                       :: nmax   !  Description and declaration in esm_alloc_int.f90
    real(fp) , intent(in)                                                       :: timnow !!  Current timestep (multiples of dt)
    real(fp) , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: tgfsep !!  Water elev. induced by tide gen.force
    real(fp) , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in) :: xz     !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in) :: yz     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer             :: icmp
    integer             :: m
    integer             :: n
    real(fp), pointer   :: time_nodal_update_tgf
    real(fp)            :: ampl
    real(fp)            :: angle       ! The actual phase in degrees of the 'Harmonics' at this time step 
    real(fp)            :: angloc
    real(fp)            :: ccphi
    real(fp)            :: cosa
    real(fp)            :: cosphi
    real(fp)            :: lovenr
    real(fp)            :: si2phi
    real(fp)            :: tcur        ! Current time (in hours)
    real(fp)            :: tgf_amp 
    !
    !
    !
    data lovenr/0.69_fp/
!
!! executable statements -------------------------------------------------------
!
    !
    !     GLOBAL DATA INITIALISATION
    !
    dt      => gdp%gdexttim%dt
    tunit   => gdp%gdexttim%tunit
    tzone   => gdp%gdexttim%tzone
    tgfcoe  => gdp%gdtfzeta%tgfcoe
    tgffr   => gdp%gdtfzeta%tgffr
    tgfv0u  => gdp%gdtfzeta%tgfv0u
    tgfw    => gdp%gdtfzeta%tgfw
    tgftyp  => gdp%gdtfzeta%tgftyp
    nrcmp   => gdp%gdtfzeta%nrcmp
    time_nodal_update_tgf => gdp%gdinttim%time_nodal_update_tgf
    !
    !-----initialize local parameters
    !
    tcur = ((timnow - time_nodal_update_tgf)*dt*tunit/3600._fp - tzone)
    !
    !-----initialize array tgfsep for all (n, m)
    !
    tgfsep = 0._fp
    !
    !-----Define water level induced by tide generated forces
    !     if not defined then NRCMP=0
    !
    do icmp = 1, nrcmp
       angle = tgfw(icmp)*tcur + tgfv0u(icmp)
       tgf_amp = tgfcoe(icmp)*tgffr(icmp)
       do n = 1, nmax
          do m = 1, mmax
             cosphi = cos(yz(n, m)*degrad)
             ccphi = cosphi*cosphi
             si2phi = sin(2.0_fp*yz(n, m)*degrad)
             if (tgftyp(icmp)==0) then
                cosa = cos(angle*degrad)
                ampl = tgf_amp*(1.5_fp*ccphi - 1._fp)*cosa
             elseif (tgftyp(icmp)==1) then
                angloc = angle + xz(n, m)
                cosa = cos(angloc*degrad)
                ampl = tgf_amp*si2phi*cosa
             elseif (tgftyp(icmp)==2) then
                angloc = angle + 2.0_fp*xz(n, m)
                cosa = cos(angloc*degrad)
                ampl = tgf_amp*ccphi*cosa
             else
             endif
             tgfsep(n, m) = tgfsep(n, m) + ampl
          enddo
       enddo
    enddo
    !
    !-----correction for LOVENR
    do n = 1, nmax
       do m = 1, mmax
          tgfsep(n, m) = tgfsep(n, m)*lovenr
       !
       !              if (n .eq. 2) then
       !                write (lundia,'(f8.2,5f7.4)') tcur*60.,tgfsep(2,2),
       !    *           tgfsep(2,19),tgfsep(2,37),tgfsep(2,55),tgfsep(2,72)
       !              endif
       !
       enddo
    enddo
end subroutine tfzeta
