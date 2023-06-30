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
module m_dlwqh2

implicit none

contains


      subroutine dlwqh2 ( noseg  , nobnd  , noq1   , noq2   , noq    ,         &
     &                    ipoint , nodisp , novelo , idpnt  , ivpnt  ,         &
     &                    area   , flow   , disp   , disper , velo   ,         &
     &                    isys   , nomat  , amat   , imat   , idiag  ,         &
     &                    diag   , diagcc , iscale , fmat   , tmat   ,         &
     &                    mixlen , iknmrk )

!     Deltares - Delft Software Department

!     Created   : Sept.1996 by Leo Postma

!     Function  : fills off-diagonals of the matrix for GMRES fast solver
!                 horizontally according to backward differences in space
!                 vertically   according to backward differences in space

!     Modified  : Feb.     1997, Robert Vos  : Check on zero's in the scaling
!                 July     2008, Leo Postma  : WAQ perfomance timers
!                 July     2009, Leo Postma  : double precission version
!                 November 2009, Leo Postma  : streamlined for parallel computing

      use timers                         ! WAQ performance timers
      implicit none

!     Arguments           :

!     Kind        Function         Name                  Description

      integer(4), intent(IN   ) :: noseg               ! Number of computational volumes
      integer(4), intent(IN   ) :: nobnd               ! Number of open boundaries
      integer(4), intent(IN   ) :: noq1                ! Number of fluxes first direction
      integer(4), intent(IN   ) :: noq2                ! Number of fluxes second direction
      integer(4), intent(IN   ) :: noq                 ! Total number fluxes in the water phase
      integer(4), intent(IN   ) :: ipoint(4,noq)       ! from, to, from-1, to+1 volume numbers per flux
      integer(4), intent(IN   ) :: nodisp              ! number of additional dispersion arrays
      integer(4), intent(IN   ) :: novelo              ! number of additional velocity   arrays
      integer(4), intent(IN   ) :: idpnt ( * )         ! dispersion array to be applied per substance
      integer(4), intent(IN   ) :: ivpnt ( * )         ! velocity   array to be applied per substance
      real   (4), intent(IN   ) :: area  ( noq )       ! crosssectional surface areas of the fluxes
      real   (4), intent(IN   ) :: flow  ( noq )       ! fluxes
      real   (4), intent(IN   ) :: disp  ( 3 )         ! default dispersions in the 3 directions
      real   (4), intent(IN   ) :: disper(nodisp,noq)  ! additional dispersion arrays
      real   (4), intent(IN   ) :: velo  (novelo,noq)  ! additional velocity arrays
      integer(4), intent(IN   ) :: isys                ! substances number to be used for this matrix
      integer(4), intent(IN   ) :: nomat               ! dimension of off-diagonal matrix amat
      real   (8), intent(  OUT) :: amat  (nomat)       ! matrix with off-diagonal entries
      integer(4), intent(IN   ) :: imat  (nomat)       ! pointers of the off-diagonals in amat
      integer(4), intent(IN   ) :: idiag(0:noseg+nobnd) ! position of the diagonals in amat
      real   (8), intent(INOUT) :: diag  (noseg+nobnd) ! diagonal of the matrix
      real   (8), intent(INOUT) :: diagcc(noseg+nobnd) ! copy of (unscaled) diagonal of the matrix
      integer(4), intent(IN   ) :: iscale              ! = 0 no row scaling of diagonal
                                                       ! = 1    row scaling of diagonal
      integer(4), intent(in   ) :: fmat  (  noq)       ! location from(iq) in matrix
      integer(4), intent(in   ) :: tmat  (  noq)       ! location to  (iq) in matrix
      real   (4), intent(in   ) :: mixlen(  noq)       ! area/length for diffusion
      integer(4), intent(in   ) :: iknmrk(noseg)       ! feature array, bit zero indicates wet or not

!     Local declarations

      logical    lscale    !  if true, then APPLY row scaling of the diagonal
      integer(4) ifrom     !  from volume number
      integer(4) ito       !  to   volume number
      real   (4) a         !  help variable for exchange surface area in m2
      real   (4) q         !  help variable for the flux in m3/s
      real   (4) e         !  help variable for diffusive flux in m3/s
      integer(4) idp,ivp   !  help variables for idpnt(isys) and ivpnt(isys)
      real   (4) q1 , q2   !  help variables
      integer(4) iq , jq   !  loop counters

!     WAQ timers

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqh2", ithandl )

!     set the logicals for dispersion and scaling and other fixed items

      lscale = iscale .eq. 1
      idp    = idpnt(isys)
      ivp    = ivpnt(isys)

!        reset the entire matrix

      amat = 0.0D0

      do 10 iq = 1 , noq

!         pointer administration check for transport anyhow

         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)

         if ( ifrom .eq. 0 .or. ito .eq. 0 ) cycle
         if ( ifrom .gt. 0 ) then
            if ( .not. btest(iknmrk(ifrom),0) ) cycle   ! identified dry at start and end of timestep
         endif
         if ( ito   .gt. 0 ) then
            if ( .not. btest(iknmrk(ito  ),0) ) cycle
         endif

!         initialisations

         a    = area(iq)
         q    = flow(iq)
         if ( a .lt. 1.0e-25 )  a = 1.0
         if ( iq .le. noq1 ) then
            e  = disp(1)
         else if ( iq .le. noq1+noq2 ) then
            e  = disp(2)
         else
            e  = disp(3)
         endif
         e  = e*mixlen(iq)

!             add additional dispersions and fluxes

         if ( idp .gt. 0 ) e = e + disper(idp,iq)*mixlen(iq)
         if ( ivp .gt. 0 ) q = q + velo  (ivp,iq)*a

!             the backward differencing in space

         if ( q .gt. 0.0 ) then
           q1 =   q
           q2 = 0.0
         else
           q1 = 0.0
           q2 =   q
         endif

!        fill the matrix

         if ( ifrom .gt. 0  ) then
            diag ( ifrom  ) = diag ( ifrom  ) + q1 + e
            amat (fmat(iq)) = amat (fmat(iq)) + q2 - e
         endif
         if ( ito   .gt. 0  ) then
            diag (  ito   ) = diag (  ito   ) - q2 + e
            amat (tmat(iq)) = amat (tmat(iq)) - q1 - e
         endif

!        end of the loop over exchanges

   10 continue

!     finally scale the matrix to avoid possible round-off errors in GMRES
!     this scaling may need some adaption for future domain decomposition b.c.

      if ( lscale ) then
         do iq = 1, noseg + nobnd
            ifrom = idiag(iq-1) + 1
            ito   = idiag(iq)

!      check on zero's required for methods 17 and 18

            if ( abs(diag(iq)) .lt. 1.0d-100) diag(iq) = 1.0

            do jq = ifrom, ito
               amat(jq) = amat(jq) / diag(iq)
            enddo

!           copy of diag for later scaling purposes in DLWQF4

            diagcc(iq) = diag (iq)
            diag  (iq) = 1.0d00
         enddo
      else
         do iq = 1, noseg + nobnd
            diagcc(iq) = 1.0d00
         enddo
      endif

      if ( timon ) call timstop ( ithandl )

      return
      end

end module m_dlwqh2
