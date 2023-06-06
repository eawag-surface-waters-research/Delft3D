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

      subroutine s12tim ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
      use m_srstop
      use m_monsys
      use m_dhkmrk


!>\file
!>       Generic module to process resuspension, burial, digging S1 & S2

      implicit none

      real     pmsa  ( * ) , fl    (*)
      integer  ipoint(34) , increm(34) , noseg , noflux,
     +         iexpnt(4,*) , iknmrk(*) , noq1, noq2, noq3, noq4
     
     
      integer  ip(34), iflux, iseg, ikmrk2
      real     fracs1, scals1, fracs2, scals2, fress1, fress2,
     j         fburs1, fburs2, fdigs1, fdigs2, swds1 , swds2 , 
     j         depth , switch, fracs3, scals3, b1, b2, d1, d2, r1, r2
      integer :: iswres
      integer :: isw_zf
      real    :: dms1
      real    :: dms2
      real    :: zres
      real    :: vres
      real    :: tau
      real    :: tcrrs1
      real    :: tcrrs2
      real    :: delt
      real    :: mindep
      real    :: press1
      real    :: press2
      real    :: flres1
      real    :: flres2
      real    :: rfdms1
      real    :: mrdms1
      real    :: delts2
      real    :: rfdms2
      real    :: mrdms2
      real    :: fracs1_res
      real    :: scals1_res
      real    :: fracs2_res
      real    :: scals2_res
      integer :: lunrep

      ip  = ipoint
!
      iflux = 0
      do 9000 iseg = 1 , noseg
      if (btest(iknmrk(iseg),0)) then
      call dhkmrk(2,iknmrk(iseg),ikmrk2)
      if ((ikmrk2.eq.0).or.(ikmrk2.eq.3)) then
!
      fracs1 = pmsa(ip( 1))
      scals1 = pmsa(ip( 2))
      fracs2 = pmsa(ip( 3))
      scals2 = pmsa(ip( 4))
      fracs3 = pmsa(ip( 5))
      scals3 = pmsa(ip( 6))
      fress1 = pmsa(ip( 7))
      fress2 = pmsa(ip( 8))
      fburs1 = pmsa(ip( 9))
      fburs2 = pmsa(ip(10))
      fdigs1 = pmsa(ip(11))
      fdigs2 = pmsa(ip(12))
      swds1  = pmsa(ip(13))
      swds2  = pmsa(ip(14))
      depth  = pmsa(ip(15))
      switch = pmsa(ip(16))
      iswres = nint(pmsa(ip(17)))
!     if iswres = 1 then the resuspension flux is independent of the other fractions and calculated here
      if ( iswres .eq. 1 ) then
         call getmlu(lunrep)
         write(lunrep,*) "Please remove processes S12TraIMx from your 
     +   sub-file, and use the Res_Pickup process instead."
         write(*,*) "Please remove processes S12TraIMx from your 
     +   sub-file, and use the Res_Pickup process instead."
         call srstop(1)
      else
         fracs1_res = fracs1
         fracs2_res = fracs2
         scals1_res = scals1
         scals2_res = scals2
      endif

!***********************************************************************
!**** Processes connected to the BURIAL and DIGGING
!***********************************************************************

!     RESUSPENSION
      R1 = 0.0
      R2 = 0.0
      IF ( FRACS1_RES*SCALS1_RES .GE. 0.0 ) R1 = FRESS1 * FRACS1_RES*SCALS1_RES
      IF ( FRACS2_RES*SCALS2_RES .GE. 0.0 ) R2 = FRESS2 * FRACS2_RES*SCALS2_RES
	
!     BURIAL
      B1 = 0.0
      B2 = 0.0
      IF ( FRACS1*SCALS1 .GE. 0.0 ) B1 = FBURS1 * FRACS1*SCALS1
      IF ( FRACS2*SCALS2 .GE. 0.0 ) B2 = FBURS2 * FRACS2*SCALS2

!     DIGGING
      D1 = 0.0
      D2 = 0.0
      IF ( (SWDS1 .LT. 0.5) .AND. (FRACS1*SCALS1 .GE. 0.0) ) THEN
           D1 = FDIGS1 * FRACS1*SCALS1
      ELSEIF (FRACS2*SCALS2 .GE. 0.0) THEN
           D1 = FDIGS1 * FRACS2*SCALS2
      ENDIF      
      IF ( (SWDS2 .LT. 0.5) .AND. (FRACS2*SCALS2 .GE. 0.0) ) THEN
           D2 = FDIGS2 * FRACS2*SCALS2
      ELSEIF (FRACS3*SCALS3 .GE. 0.0) THEN
           D2 = FDIGS2 * FRACS3*SCALS3
      ENDIF      

!     Store results

      PMSA(IP(28)) = R1
      PMSA(IP(29)) = R2
      IF (ABS(SWITCH).LT.0.5) THEN
!       NO SWITCH
        PMSA(IP(30)) = B1
        PMSA(IP(31)) = 0.0
      ELSE
!       SWITCH
        PMSA(IP(30)) = 0.0
        PMSA(IP(31)) = B1
      ENDIF
      PMSA(IP(32)) = B2
      PMSA(IP(33)) = D1
      PMSA(IP(34)) = D2

      FL( 1 + IFLUX ) = R1/DEPTH 
      FL( 2 + IFLUX ) = R2/DEPTH 
      IF (ABS(SWITCH).LT.0.5) THEN
!       NO SWITCH
        FL( 3 + IFLUX ) = B1/DEPTH 
        FL( 4 + IFLUX ) = 0.0
      ELSE
!       SWITCH
        FL( 3 + IFLUX ) = 0.0
        FL( 4 + IFLUX ) = B1/DEPTH 
      ENDIF
      FL( 5 + IFLUX ) = B2/DEPTH 
      FL( 6 + IFLUX ) = D1/DEPTH 
      FL( 7 + IFLUX ) = D2/DEPTH 

      ENDIF
      ENDIF
!
      IFLUX = IFLUX + NOFLUX
      IP    = IP    + INCREM
!
 9000 CONTINUE
!
      RETURN
!
      END
