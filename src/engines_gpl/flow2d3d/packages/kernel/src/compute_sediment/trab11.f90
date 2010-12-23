subroutine trab11(kode      ,ntrsi     ,u         ,v         ,hrms      , &
                & h         ,tp        ,d50       ,par       ,sbotx     , &
                & sboty     ,ssusx     ,ssusy     ,ubot      ,gdp       )
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
! computes sediment transport according to
! the transport formula of Soulsby / Van Rijn
! -
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
    ! They replace the  include igd / include igp lines
    !
    real(fp)                 , pointer :: gammax
    logical                  , pointer :: ubot_from_com
!
! Global variables
!
    integer                , intent(in)  :: kode
    integer                , intent(out) :: ntrsi
    real(fp)               , intent(in)  :: d50
    real(fp)                             :: h
    real(fp)                             :: hrms   !  Description and declaration in rjdim.f90
    real(fp)               , intent(out) :: sbotx
    real(fp)               , intent(out) :: sboty
    real(fp)               , intent(out) :: ssusx
    real(fp)               , intent(out) :: ssusy
    real(fp)                             :: tp     !  Description and declaration in rjdim.f90
    real(fp)               , intent(in)  :: ubot   !  Description and declaration in rjdim.f90
    real(fp)               , intent(in)  :: u
    real(fp)               , intent(in)  :: v
    real(fp), dimension(30), intent(in)  :: par
!
! Local variables
!
    real(fp)                       :: acal
    real(fp)                       :: asb
    real(fp)                       :: ass
    real(fp)                       :: cd
    real(fp)                       :: d90
    real(fp)                       :: delta
    real(fp)                       :: dster
    real(fp)                       :: g          !  gravity acceleration
    real(fp)                       :: k          ! wave number
    real(fp)                       :: pi
    real(fp)                       :: rnu
    real(fp)                       :: term1
    real(fp)                       :: term2
    real(fp)                       :: ucr
    real(fp)                       :: uorb       !  orbital velocity at the bottom layer
    real(fp)                       :: urms
    real(fp)                       :: utot       ! flow velocity
    real(fp)                       :: vonkar
    real(fp)                       :: z0
!
!
!! executable statements -------------------------------------------------------
!
    gammax          => gdp%gdnumeco%gammax
    ubot_from_com   => gdp%gdprocs%ubot_from_com
    !
    if (kode== - 1) then
       return
    endif
    !
    !     Initialisations
    !
    ntrsi = 2
    sbotx = 0.0
    sboty = 0.0
    ssusx = 0.0
    ssusy = 0.0
    vonkar = 0.4
    pi = 4.*atan(1.)
    g = par(1)
    delta = par(4)
    rnu = par(5)
    acal = par(11)
    d90 = par(12)*d50
    z0 = par(13)
    !
    !     Initiliaze Transports to zero
    !     in case of small u, small h, very large h, u<ucr
    !
    ssusx = 0.
    ssusy = 0.
    sbotx = 0.
    sboty = 0.
    !
    !     Prevent small tp
    !
    if (tp<=1. .and. tp>1.E-6) tp = 1.
    if (tp<=1.E-6) then
       tp = 5.
       hrms = 0.01
    endif
    !
    !     limit Hrms to gammax*h, similar to Bijker implementation
    !
    hrms = min(hrms, gammax*h)
    !
    !     Velocity magnitude
    !
    utot = u**2 + v**2
    if (utot>0.) utot = sqrt(utot)
    if (utot<0.000001 .or. h>200. .or. h<0.01) goto 999
    !
    !     Wave number k, urms orbital velocity
    !
    if (tp>1.E-6) then
       call wavenr(h         ,tp        ,k         ,gdp       )
       if (ubot_from_com) then
          uorb = ubot
       else
          uorb = pi*hrms/tp/sinh(k*h)
       endif
       urms = uorb*0.7071
    else
       goto 999
    endif
    !
    !     Soulsby p. 184, (g(s-1)/nu^2)^(1/3) = 25926
    !                    for g=9.81, s=2.65, nu=1e-6
    !
    dster = (g*delta/rnu**2)**(1./3.)*d50
    !
    !     Soulsby p. 176, eq. 133d,e
    !     For d50 > 2 mm in SEDINP an warning is generated
    !     and we will define Ucr as if 0.0005 < d50 < 0.002
    !
    if (d50<=0.0005) then
       ucr = 0.19*d50**0.1*log10(4.*h/d90)
    elseif (d50<0.002) then
       ucr = 8.5*d50**0.6*log10(4.*h/d90)
    else
       ucr = 8.5*d50**0.6*log10(4.*h/d90)
    endif
    !
    !     Soulsby p. 183
    !
    cd = (vonkar/(log(h/z0) - 1.))**2
    asb = 0.005*h*(d50/h/(delta*g*d50))**1.2
    ass = 0.012*d50*dster**( - 0.6)/(delta*g*d50)**1.2
    term1 = (utot*utot + 0.018/cd*urms*urms)**0.5
    if (term1>ucr) then
       term2 = (term1 - ucr)**2.4
    else
       goto 999
    endif
    !
    !     Soulsby p. 183, eq. 136a; bed slope effects are left out,
    !     since they are taken into account elsewhere
    !
    sbotx = acal*asb*u*term2
    sboty = acal*asb*v*term2
    ssusx = acal*ass*u*term2
    ssusy = acal*ass*v*term2
    !
  999 continue
end subroutine trab11
