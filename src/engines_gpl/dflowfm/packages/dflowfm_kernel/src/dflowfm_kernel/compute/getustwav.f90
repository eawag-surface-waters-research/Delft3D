!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2021.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! $Id$
! $HeadURL$

subroutine getustwav(LL, z00, fw, ustw2, csw, snw, Dfu, Dfuc, deltau, costu) ! at u-point, get ustarwave and get ustokes
use m_flow
use m_flowgeom
use m_waves
use m_sferic
implicit none
integer,          intent(in)  :: LL
double precision, intent(in)  :: z00
double precision, intent(out) :: fw, ustw2, csw, snw
double precision, intent(out) :: Dfu                    ! wave dissipation due to bedfriction
double precision, intent(out) :: Dfuc                   ! Dfu/c
double precision, intent(out) :: deltau                 ! wave bed boundary layer thickness


double precision, external    :: sinhsafei
integer                       :: k1, k2 , Lb, Lt, L
double precision              :: Tsig, Hrms, asg, rk, shs, uorbu, astar, phiw, phi1, phi2, dks, aks, omeg, f1u, f2u, f3u, zu, costu, sintu
double precision              :: qsto, usto3Dav , usto2D, p1, p2, h, z, ustoktb, uusto, uwi

Lb   = Lbot(LL) ; Lt = Ltop(LL)
k1   = ln(1,LL) ; k2 = ln(2,LL)
Tsig = 0.5d0*( Twav(k1)   + Twav(k2) )
if (Tsig > 0.05d0) then
   omeg  = twopi/Tsig
else
   ustw2 = 0d0
   if (jawaveStokes > 0) then
       ustokes(Lb:Lt) = 0d0 ; vstokes(Lb:Lt) = 0d0
   endif
   return
endif
if (jawave <= 2) then
   uwi = sqrt(wx(LL)*wx(LL) + wy(LL)*wy(LL) )
   if (uwi > 0d0) then
      csw = wx(LL)/uwi
      snw = wy(LL)/uwi
   else
      csw = 1d0 ; snw = 0d0
   endif
else
   phi1   = Phiwav(k1) ; phi2  = Phiwav(k2)
   if ( phi2 - phi1 > 180d0 ) phi1 = phi1 + 360d0
   phiw   = 0.5d0*( Phi1 + Phi2 )
   csw    = cos(phiw*dg2rd)
   snw    = sin(phiw*dg2rd)
endif

call getwavenr(hu(LL), Tsig ,rk)
Hrms   = 0.5d0*( Hwav(k1)   + Hwav(k2) )
Hrms   = min(hrms,gammax*hu(LL))
asg    = 0.5d0*Hrms                              ! Wave amplitude = 0.5*Hrms
shs    = sinhsafei(rk*hu(LL))
costu  =  csw*csu(LL) + snw*snu(LL)              ! and compute stokes drift
sintu  = -csw*snu(LL) + snw*csu(LL)

if (shs > 0d0) then
   uorbu  = omeg*asg*shs*sqrt(pi)/2.0                        ! Orbital velocity, without sqrt(pi) factor
   !call  Swart(Tsig, uorbu, z00, fw, ustw2)
   call soulsby(Tsig, uorbu, z00, fw, ustw2)
   ustw2  = ftauw*ustw2                          ! ustar wave squared times calibrationcoeff

   dks    = 30d0*z00                             ! should be 30 for consistency with getustb
   aks    = asg*shs/dks                          ! uorbu/(omega*ks), uorbu/omega = particle excursion length
   deltau = 0.09d0 * dks * aks**0.82d0           ! thickness of wave boundary layer also see: Sana, Tanaka 2007
   deltau = alfdeltau*max(deltau, ee*z00 )       ! alfaw = 20d0
   deltau = min(0.5d0*hu(LL), deltau)            !

   Dfu    = ustw2*uorbu/sqrt(pi)                 ! dissipation by waves (m3/s3)
   !Dfu    = 0.28d0*ftauw*fw*abs(uorbu**3d0)     ! dissipation by waves (m3/s3), note Dano
   Dfu =    ftauw *  fw * uorbu**3 / (sqrt(pi))  ! THIS IS TO CHECK
   Dfu    = Dfu/deltau                           ! divided by deltau    (m2/s3)
   Dfuc   = Dfu*rk*Tsig/twopi                    ! Dfuc = dfu/c,        (m /s2) is contribution to adve

else
   ustw2  = 0d0
   Dfu    = 0d0
   Dfuc   = 0d0
   deltau = 0d0
endif

if (jawaveStokes == 1) then
   uusto          =  0.5d0*omeg*asg*asg/hu(LL)
   ustokes(Lb:Lt) =  costu*uusto
   vstokes(Lb:Lt) =  sintu*uusto
else if (jawaveStokes >= 2) then
   f1u    = omeg*rk*asg**2
   h      = hu(LL)
   f3u    = (1d0 - exp(-2d0*rk*h ) )**2
   qsto   = 0d0
   do L   = Lb, Lt
      z   = 0.5d0*( hu(L) + hu(L-1) )         ! here, z is vertical coordinate upward, bed = 0,    (not z = 0 at average wl)
      p1  = max(-25d0,  2d0*rk*(z - h))
      p2  = max(-25d0, -4d0*rk*(z    ))       ! maximisation not necessary
      f2u = exp(p1) * ( 1d0 + exp(p2) )
      uusto      = f1u   * f2u / f3u
      ustokes(L) = costu * uusto
      vstokes(L) = sintu * uusto
      qsto       = qsto + ustokes(L)*(hu(L)-hu(L-1))
   enddo

   usto3Dav = qsto/hu(LL)
   usto2D   = costu*0.5d0*omeg*asg*asg/hu(LL)
   ustoktb  = f1u*(2d0*p1/ (1d0-p1)) **2

endif

end subroutine getustwav
