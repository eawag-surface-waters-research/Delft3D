!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2022.                                
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

 subroutine getustbcfuhi( LL,Lb,ustbLL,cfuhiLL,hdzb, z00,cfuhi3D)                ! see Uittenbogaard's subroutine USTAR
 use m_flow
 use m_flowgeom  , only : ln, dxi, csu, snu, acL
 use m_flowtimes , only : dti
 use m_waves     , only : ustokes, vstokes, taubxu, wblt
 use m_sediment  , only : stm_included
 use m_turbulence, only : tkepro
 use m_flowtimes, only: dts

 implicit none
 integer,          intent (in)  :: LL, Lb
 double precision, intent (out) :: ustbLL, cfuhiLL, hdzb, z00
 double precision, intent(out)  :: cfuhi3D                   ! 3D bedfriction coeffient, advi(Lb) = adbi(Lb) + cfuhi3D

 integer          :: ifrctyp, L
 double precision :: frcn, sqcf, cz, umod, u1Lb, gsx, ustw2, ustc2, fw, Cdrag, abscos, dfuc, costu
 double precision :: taubpuLL                                ! taubpu = umod*ag/C2 or ypar*(taucur+tauwav)
 double precision :: taubxuLL                                ! taubxu = taubpu*umod*rhomean

 double precision :: csw, snw                                ! wave direction cosines
 double precision :: Dfu, Dfu0, Dfu1, tkpr, tkp0, tkp1, aa   ! wave dissipation by bed friction, / (rhomean*c*deltau)
 double precision :: deltau                                  ! wave dissipation layer thickness
 double precision :: huL0, huL1, huL, u2dh

 integer          :: nit, nitm = 100
 double precision :: r, rv = 123.8d0, e = 8.84d0 , eps = 1d-2, s, sd, er, ers, dzb, uux, uuy, htop, dzw, dzu, alin

 cfuhi3D = 0d0 ; nit = 0

 frcn = frcu(LL)
 if (frcn == 0d0 ) return
 ifrctyp = ifrcutp(LL)

 if ( hu(LL) < trsh_u1Lb) then
     gsx = ag*( s1(ln(2,LL)) - s1(ln(1,LL)) ) * dxi(LL)
 endif

 if (ifrctyp < 10) then
    if (frcn > 0d0 ) then
        call getczz0(hu(LL), frcn, ifrctyp, cz, z00)

        hdzb  = 0.5d0*hu(Lb)     + c9of1*z00                ! half bottom layer plus 9z0

        if (z00 > 0d0) then

           if (jaustarint == 0) then
              ! sqcf = vonkar/log(c9of1 + hdzb/z00)            ! till 012015
              sqcf = vonkar/log(hdzb/z00)
           else if (jaustarint == 1) then                      ! Yoeri 2014 long time default for jaustarint == 1
              dzb  = hu(Lb) + c9of1*z00
              sqcf = vonkar / ( log(dzb/z00)-1d0 )
           else if (jaustarint == 2) then                      ! remobilised through jaustarint == 2, good convergence
              dzb  = hu(Lb)/ee + c9of1*z00
              sqcf = vonkar / ( log(dzb/z00) )
           else if (jaustarint == 3) then                     ! Delft3D
              hdzb  = 0.5d0*hu(Lb)     + z00
              sqcf = vonkar / ( log(1d0+0.5d0*hu(Lb)/z00) )    ! D3D:
           else if(jaustarint == 4) then
               !hdzb  = 0.5d0*hu(Lb)     + c9of1*z00/0.65d0
               dzb  = hu(Lb)/ee + c9of1*z00 *0.66d0
              sqcf = vonkar / ( log(dzb/z00) )
           else if (jaustarint == 5) then
              dzb  = hu(Lb)
              sqcf = vonkar / ( ( 1d0 + c9of1 * z00 / dzb ) * log(dzb/z00+c9of1) - c9of1 * z00/dzb * log(c9of1) - 1d0 )
           endif

        else
           sqcf = 0d0
        endif

    else
        hdzb = 0.5d0*hu(Lb)
        sqcf = 0d0
    endif

    u1Lb = u1(Lb)

    10  continue

    if (jawave>0 .and. jawaveStokes >= 1) then                                      ! ustokes correction at bed
       umod = sqrt( (u1Lb-ustokes(Lb))*(u1Lb-ustokes(Lb)) + (v(Lb)-vstokes(Lb))*(v(Lb)-vstokes(Lb)) )
    else
       umod = sqrt( u1Lb*u1Lb + v(Lb)*v(Lb) )
    endif

    if (umod == 0d0) then         ! from dry to wet
       umod = max(1d-4, dts*ag*dxi(LL)*min( abs( s1(ln(1,LL)) - s1(ln(2,LL)) ), 0.333333d0*hu(LL) ) )
    else
       umod = max(umod, 1d-4)     ! 1d-5 for some wave cases     ! until 3D handled like 2D iterative loop , solves Roses problem: ust=1.1e-104 to the power 3 is underflow
    endif

    ustbLL = sqcf*umod                                           ! ustar based upon bottom layer velocity

    if (jawave > 0 .and. .not. flowWithoutWaves) then
       call getustwav(LL, z00, fw, ustw2, csw, snw, Dfu, Dfuc, deltau, costu) ! get ustar wave squared, fw and wavedirection cosines  based upon Swart, ustokes
       if (ustw2 > 1d-8) then
          !ustc2 = ustbLL*ustbLL
          if (modind < 9) then                                   ! wave-current interaction Soulsby (1997)
             cdrag  = ag/(cz*cz)
             uux    = acL(LL)*ucx(ln(1,Lb)) + (1d0-acL(LL))*ucx(ln(2,Lb))
             uuy    = acL(LL)*ucy(ln(1,Lb)) + (1d0-acL(LL))*ucy(ln(2,Lb))
             ! Virtual 2dh velocity
             u2dh = (umod/hu(LL)                                             &
                     & *((hu(LL) + z0urou(LL))*log(1.0 + hu(LL)/z0urou(LL))     &
                     & - hu(LL)))/log(1.0 + 0.5d0*hu(Lb)/z0urou(LL))
             !ustc2  = cdrag*(uux**2 + uuy**2)
             ustc2  = cdrag*u2dh**2
             abscos = abs( csw*uux     + snw*uuy ) / max(1d-4, sqrt(uux*uux + uuy*uuy) )         ! abs(prodin(uw,uc))
             call getsoulsbywci(modind, z00, ustc2, ustw2, fw, cdrag, umod, abscos, taubpuLL, taubxuLL)
             ustbLL = sqrt(umod*taubpuLL)
          else if (modind == 9) then                          ! wave-current interaction van Rijn (2004)
             ! call getvanrijnwci    (LL, z00, ustc2, ustw2, ustcw2, z0urou(LL))
          endif
          sqcf   = max(sqcf,ustbLL / umod )                      ! waveps not needed, see umod = max(umod, 1d-4) line above
          if (stm_included .or. jawaveSwartDelwaq > 1)  then     ! For usage in coupled models
             taubxu(LL) = taubxuLL
             z0ucur(LL) = z00                                    ! And, in case anybody needs these arrays :
             z0urou(LL) = dzb*exp(-vonkar/sqcf - 1d0)            ! inverse of jaustarint == 1 above
             wblt(LL) = deltau
             ! N.b., in Delft3D zourou is established upon velocities just outside the wave boundary layer, at L = Lbw1:
          endif
          !
          if (jawavestreaming >= 1 .and. deltau > 0d0)  then     ! Streaming below deltau with linear distribution
             !tkpr  = 2d0*Dfu                                     ! (m2/s3)
             !tkepro(0) = tkpr*0.5d0*hu(Lb)/deltau                ! fraction in bed layer
             Dfuc  = Dfuc*costu                                  ! (m/s2)
             huL0 = 0d0
             huL1 = huL0
             do L  = Lb, Ltop(LL)
                huL0 = huL1
                huL1 = hu(L)
                huL = ( huL0 + huL1 ) * 0.5d0
                if (huL1 <= deltau) then
                   alin   = 1d0 -  huL / deltau               ! linear from 1 at bed to 0 at deltau
                   Dfu1   = Dfuc*alin
                   adve(L) = adve(L) - Dfu1
                elseif( huL0 <= deltau ) then
                   alin = (min( huL1, deltau )- huL0 ) / (2d0 * (huL1 - huL0) )
                   Dfu1   = Dfuc*alin
                   adve(L) = adve(L) - Dfu1
                else
                   exit
                endif
             enddo
             !Dfu0  = Dfuc
             !Dfu1  = 0.0
             !do L  = Lb, Ltop(LL)
             !   if (hu(L) <= deltau) then
             !      htop   = min( hu(L), deltau )                 ! max height within waveboundarylayer
             !      alin   = 1d0 -  htop / deltau                 ! linear from 1 at bed to 0 at deltau
             !      Dfu1   = Dfuc*alin
             !      dzu    = htop-hu(L-1)
             !      adve(L) = adve(L) - 0.5d0*(Dfu0 + Dfu1)*dzu / deltau
             !      !if (jawavestreaming >= 2 .and. L < Ltop(LL) ) then
             !      !    tkp1 = tkpr*alin
             !      !    dzw  = min( deltau, 0.5d0*(hu(L+1) + hu(L) ) )  - 0.5d0*( hu(L) + hu(L-1) )
             !      !    tkepro(L-Lb+1) = tkp1*dzw / deltau
             !      !endif
             !   endif
             !   Dfu0    = dfu1
             !   if (hu(L) > deltau) then
             !      exit
             !   endif
             !enddo
          endif
       endif
    endif

    cfuhiLL   = sqcf*sqcf/hu(Lb)                              ! cfuhiLL   = g / (H.C.C) = (g.K.K) / (A.A)
    cfuhi3D   = cfuhiLL*umod                                  ! cfuhi3D = frc. contr. to diagonal
    !advi(Lb) = advi(Lb) +  cfuhiLL*umod

    if (jawave==0 .or. flowWithoutWaves) then
       z0ucur(LL) = z00
       z0urou(LL) = z00
    endif

    if (jawave>0 .and. jawaveStokes >= 1 .and. .not. flowWithoutWaves) then                               ! Ustokes correction at bed
       adve(Lb)  = adve(Lb) - cfuhi3D*ustokes(Lb)
    endif

 else if (ifrctyp == 10) then                                 ! Hydraulically smooth, glass etc
     nit = 0
     u1Lb = u1(Lb)
      if (jawaveStokes >= 1) then
         umod  = sqrt( (u1Lb-ustokes(Lb))*(u1Lb-ustokes(Lb)) + (v(Lb)-vstokes(Lb))*(v(Lb)-vstokes(Lb)) )   ! was ustokes(LL)
      else
         umod  = sqrt( u1Lb*u1Lb + v(Lb)*v(Lb) )
     endif

     r   = umod*hu(Lb)/viskin                                  ! Local re-number:
     r   = max(r,0.001d0)
     er  = e*r
     if (r.lt.rv) then                                         ! Viscous sublayer:
        s   = sqrt(r)
     else

        s   = 12d0                                             ! In log-layer; initial trial for s:
100     continue
        nit = nit+1
        sd  = s
        ers = max(er/sd, 1.0001d0)
        s   = log(ers)/vonkar

        if (nit.ge.nitm) then
           call error ('***ERROR in USTAR: no convergence.', ' ', ' ' )
        endif
        if (s.gt.r) then
           call error ('***ERROR in USTAR: S too large.', ' ', ' ' )
        endif


        if (abs(sd-s).gt.(eps*s)) then
            go to 100                                          ! Convergence criterium:
        endif
     endif

     if (s > 0d0) then
        sqcf = 1d0/s
     else
        sqcf = 0d0
     endif
     ustbLL = sqcf*umod                                        ! ustar based upon bottom layer velocity
     cfuhiLL  = sqcf*sqcf/hu(Lb)
     hdzb   = 0.5d0*hu(Lb)

     if (cfuhiLL > 100d0) then
        nit = nit + 1
     endif

!     advi(Lb) = advi(Lb) +  cfuhiLL*umod                        ! g / (H.C.C) = (g.K.K) / (A.A) travels in cfuhi
     cfuhi3D = cfuhiLL*umod

 else if (ifrctyp == 11) then                                    ! Noslip

!    advi(Lb) = advi(Lb) +  2d0*(vicwwu(Lb)+vicouv)/hu(Lb)**2
     cfuhi3D = 2d0*(vicwwu(Lb)+vicoww)/hu(Lb)**2

 endif

 if ( hu(LL) < trsh_u1Lb .and. abs(gsx) > 1d-3 .and. nit <= 3) then
    ! u1Lb = ( u1(Lb)*dti - adve(Lb) - gsx ) / (cfuhi3D + dti)
    u1Lb = ( u1(Lb)*dti            - gsx ) / (cfuhi3D + dti)
    nit  = nit + 1
    goto 10
 endif

 if (jafrculin > 0) then
    cfuhi3D = cfuhi3D + frculin(LL)/hu(Lb)
 endif

 end subroutine getustbcfuhi
