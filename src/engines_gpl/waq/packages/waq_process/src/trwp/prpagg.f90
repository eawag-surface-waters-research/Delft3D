!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.


subroutine PRPAGG   (  pmsa  , fl    , ipoint, increm, noseg , &
                       noflux, iexpnt, iknmrk, noq1  , noq2  , &
                       noq3  , noq4  )
use m_dhkmrk


!>\file
!>       Properties of aggregated particles (one TRW particle and one suspended solids particle)

!
!     Description of the module :
!
!     Calculate the properties of the combined particles and then
!     calculate the sedimentation velocity and the critical shear stress
!     based on the combined particle's physical properties
!
! Name            T   Index   Description                                   Units
! ----            --- -       -------------------                            ----
! DIAMETER1       R*4 1   diameter of the tyre particles                       [um]
! DENSITY1        R*4 2   density of the tyre particles                     [kg/m3]
! DIAMETER2       R*4 3   diameter of the sediment particles                   [um]
! DENSITY2        R*4 4   density of the sediment particles                 [kg/m3]
! BIOFILM_THK     R*4 5   thickness of the biofilm                             [um]
! BIOFILM_DENSITY R*4 6   density of the biofilm                            [kg/m3]
!
! SETTLE_VEL      R*4 1   settling velocity                                   [m/d]
! TCR_SEDIM       R*4 2   critical shear stress for sedimentation              [Pa]
!
! nov 2021 Jos van Gils added a loop over the fractions, to avoid long lists of processes and to speed up ...

    implicit none

    real     pmsa  ( * ) , fl    (*)
    integer  ipoint( * ) , increm(*) , noseg , noflux, &
             iexpnt(4,*) , iknmrk(*) , noq1, noq2, noq3, noq4
!
!   local declarations
!
    integer  iseg, ikmrk1,ikmrk2, itel, noq, iq, ifrom, ipp
    real     diameter1, density1, diameter2, density2, biofilm_thk, biofilm_density
    real     combined_diameter, combined_density, new_shape_factor
    real     settle_vel, tcr_sedim

    integer           :: ipnt(500)  
    integer,parameter :: ip_nTRWP = 1
    integer,parameter :: ip_nIM = 2
    integer,parameter :: ip_BioFilmThk = 3
    integer,parameter :: ip_BioFilmDen = 4
    integer,parameter :: ip_lastsingle = 4

    integer :: ntrwp, itrwp, nsusp, isusp, nitem, offset

    ntrwp = pmsa(ipoint(ip_ntrwp))
    nsusp = pmsa(ipoint(ip_nim  ))
    nitem = 4+2*ntrwp+2*nsusp+3*ntrwp*nsusp ! 

!
!  Note: we only need to do this once, no looping over the segments
!  as all particles of the same size class have the same properties
 
    ipnt(1:nitem) = ipoint(1:nitem)
 
    do iseg = 1 , noseg
        call dhkmrk(1,iknmrk(iseg),ikmrk1)
        if (ikmrk1.eq.1) then
            call dhkmrk(2,iknmrk(iseg),ikmrk2)
            if (ikmrk2.le.4) then   ! surface water

                ! input independentt of fractions
                biofilm_thk     = pmsa(ipnt(ip_BioFilmThk))
                biofilm_density = pmsa(ipnt(ip_BioFilmDen))
            
                ! loop over active fractions, IM are inner loop
                itel = 0
                do itrwp = 1,ntrwp
                do isusp = 1,nsusp

                    itel = itel + 1
                    diameter1       = pmsa(ipnt(ip_lastsingle+itrwp))
                    density1        = pmsa(ipnt(ip_lastsingle+ntrwp+itrwp))
                    diameter2       = pmsa(ipnt(ip_lastsingle+2*ntrwp+isusp))
                    density2        = pmsa(ipnt(ip_lastsingle+2*ntrwp+nsusp+isusp))

                    call add_biofilm( diameter1, density1, biofilm_thk, biofilm_density )
                    call combine_particles( diameter1, diameter2, density1, density2, &
                                            combined_diameter, combined_density, new_shape_factor )
                    call calculate_sedim( combined_diameter, combined_density, new_shape_factor, settle_vel, tcr_sedim )

                    pmsa(ipnt(ip_lastsingle+2*ntrwp+2*nsusp+itel)) = settle_vel
                    pmsa(ipnt(ip_lastsingle+2*ntrwp+2*nsusp+ntrwp*nsusp+itel)) = tcr_sedim
                enddo
                enddo

            endif
        endif
        ipnt(1:nitem) = ipnt(1:nitem) + increm(1:nitem)

    enddo
    
    ! addition for use in 3D
    
    NOQ = NOQ1 + NOQ2 + NOQ3
    offset = ip_lastsingle+2*ntrwp+2*nsusp+2*ntrwp*nsusp
    ipnt(1:nitem) = ipoint(1:nitem)
    do IQ=1,NOQ1+NOQ2
        itel = 0
        do itrwp = 1,ntrwp
        do isusp = 1,nsusp
            itel = itel + 1
            pmsa(ipnt(offset+itel)) = 0.0
        enddo
        enddo
        ipnt(1:nitem) = ipnt(1:nitem) + increm(1:nitem)
    enddo
    do  IQ=NOQ1+NOQ2+1,NOQ
        ifrom = IEXPNT(1,IQ)
!
!       Sedimentation velocity from segment to exchange-area
!
        IF ( ifrom .GT. 0 ) THEN
            itel = 0
            do itrwp = 1,ntrwp
            do isusp = 1,nsusp
                itel = itel + 1
                ipp = ip_lastsingle+2*ntrwp+2*nsusp+itel
                settle_vel = PMSA( ipoint(ipp) + (ifrom-1) * increm(ipp) )
                pmsa(ipnt(offset+itel)) = settle_vel 
            enddo
            enddo
        ENDIF
        ipnt(1:nitem) = ipnt(1:nitem) + increm(1:nitem)
    enddo
    
end

