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
      module m_dlwqe1

      implicit none

      contains


      subroutine dlwqe1 ( nosys  , notot  , noseg  , noqw   , noq    ,
     &                    nodisp , novelo , disp   , disper , velo   ,
     &                    area   , flow   , aleng  , ipoint , iknmrk ,
     &                    idpnt  , ivpnt  , conc   , bound  , iopt   ,
     &                    ilflag , idt    , deriv  , iaflag , amass2 ,
     &                    lun    , ndmpq  , iqdmp  ,
     &                    dmpq   , rhs    , diag   , acodia , bcodia )

!     Deltares Software Centre

!>\file
!>                Implicit solution of the vertical by double sweep,
!>                upwind differencing of the advection terms.
!>
!>                Because of the bed layers, a tridiagonal matrix is filled first
!>                Then for the water phase all forward substitutions of the lower
!>                codiagonal are performed.\n
!>                This method required strict ordering from the top of the water
!>                column towards the bed. The adminitration is generally per layer
!>                downwards for all cells, but also per column downwards for all
!>                columns will work. The NOQW water exchanges should contain all
!>                single exchanges. This should be inclusive of the exchanges from
!>                the water with the first bed cell.\n
!>                The exchanges in the bed are double. So both are taken together.
!>                The bed columns are probably per column, because each column can
!>                have different amount of layers, but per layer could do as well
!>                as long as layers count from the top of the bed to below.\n
!>                For the sweep back the reverse order is followed.\n
!>                Note the option to have settling substances modelled 'upwind'
!>                whereas the water velocity is taken centrally.

!     Created             : March     1988 by Leo Postma
!     Modified            : Unknown        by Jan van Beek balances
!                         : march     2007 by Leo Postma layers in the bed
!                         : September 2010 by Leo Postma FORTRAN-90 look and feel
!                         : June      2012 by Leo Postma general solver for 13 and 14

!     Files               : none

!     Routines            : none

      use timers
      implicit none

!     Parameters          :

!     kind           function         name                   description

      integer  ( 4), intent(in   ) :: nosys                !< number of transported substances
      integer  ( 4), intent(in   ) :: notot                !< total number of substances
      integer  ( 4), intent(in   ) :: noseg                !< number of computational volumes
      integer  ( 4), intent(in   ) :: noqw                 !< number of interfaces waterphase
      integer  ( 4), intent(in   ) :: noq                  !< total number of interfaces
      integer  ( 4), intent(in   ) :: nodisp               !< number additional dispersions
      integer  ( 4), intent(in   ) :: novelo               !< number additional velocities
      real     ( 4), intent(in   ) :: disp  (3)            !< fixed dispersions in the 3 directions
      real     ( 4), intent(in   ) :: disper(nodisp,noq)   !< array with additional dispersions
      real     ( 4), intent(in   ) :: velo  (novelo,noq)   !< array with additional velocities
      real     ( 4), intent(in   ) :: area  (noq)          !< exchange areas in m2
      real     ( 4), intent(in   ) :: flow  (noq)          !< flows through the exchange areas in m3/s
      real     ( 4), intent(in   ) :: aleng (  2   ,noq)   !< mixing length to and from the exchange area
      integer  ( 4), intent(in   ) :: ipoint(  4   ,noq)   !< from, to, from-1, to+1 volume numbers
      integer  ( 4), intent(in   ) :: iknmrk(noseg)        !< feature array
      integer  ( 4), intent(in   ) :: idpnt (nosys)        !< additional dispersion number per substance
      integer  ( 4), intent(in   ) :: ivpnt (nosys)        !< additional velocity number per substance
      real     ( 4), intent(inout) :: conc  (notot,noseg)  !< m asses after horizontal transport step
      real     ( 4), intent(in   ) :: bound (nosys,  *  )  !< open boundary concentrations
      integer  ( 4), intent(in   ) :: iopt                 !< bit 0: 1 if no dispersion at zero flow
                                                           !< bit 1: 1 if no dispersion across boundaries
                                                           !< bit 2: 1 if lower order across boundaries
                                                           !< bit 3: 1 if mass balance output
      integer  ( 4), intent(in   ) :: ilflag               !< if 0 then only 3 constant lenght values
      integer  ( 4), intent(in   ) :: idt                  !< time step in seconds
      real     ( 4), intent(inout) :: deriv (notot,noseg)  !< workspace containing the diagonal
      integer  ( 4), intent(in   ) :: iaflag               !< if 1 then accumulate mass in report array
      real     ( 4), intent(inout) :: amass2(notot, 5   )  !< report array for monitoring file
      integer  ( 4), intent(in   ) :: lun                  !< unit number of monitoring file
      integer  ( 4), intent(in   ) :: ndmpq                !< number of dumped exchanges
      integer  ( 4), intent(in   ) :: iqdmp ( noq )        !< pointers dumped exchages
      real     ( 4), intent(inout) :: dmpq  (nosys,ndmpq,2)!< dmpq(*,*,1) incoming transport
                                                           !< dmpq(*,*,2) outgoing transport
      real     ( 8), intent(inout) :: rhs   (notot,noseg)  !< local right hand side
      real     ( 8), intent(inout) :: diag  (notot,noseg)  !< local diagonal filled with volumes
      real     ( 8), intent(inout) :: acodia(notot,noq)    !< local workarray under codiagonal
      real     ( 8), intent(inout) :: bcodia(notot,noq)    !< local workarray upper codiagonal

!         local variables

      integer  ( 4) iq                    ! loop counter exchanges
      integer  ( 4) isys                  ! loop counter substance
      integer  ( 4) iseg                  ! loop counter computational volumes
      integer  ( 4) ifrom, ito            ! from and to volume numbers
      integer  ( 4) iq2, iq3              ! help variables to identify first or second pointers
      integer  ( 4) iqd                   ! help variable for dump pointers
      real     ( 8) a                     ! this area
      real     ( 8) q                     ! flow for this exchange
      real     ( 8) e                     ! dispersion for this exchange
      real     ( 8) al                    ! this length
      real     ( 8) dl                    ! area / length
      real     ( 8) d                     ! dispersion for this substance
      real     ( 8) v                     ! flow for this substance
      real     ( 8) v1, v2                ! factors for 'upwind'
      real     ( 8) q3, q4                ! flux help variables
      real     ( 8) dq                    ! total flux from and to
      real     ( 8) pivot                 ! help variable matrix inversion
      logical       disp0q0               ! bit zero no disp if q is zero
      logical       disp0bnd              ! bit one  no disp accross bounds
      logical       abound                ! is it a boundary?


      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqe1", ithandl )

!         Initialisation

      rhs      = conc
      diag     = deriv
      acodia   = 0.0
      bcodia   = 0.0
      disp0q0  = btest( iopt , 0 )
      disp0bnd = btest( iopt , 1 )

!         Loop over exchanges to fill the matrices

      do 10 iq = 1 , noq

!         Initialisations, check for transport anyhow

         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)
         if ( ifrom .eq. 0 .or.  ito .eq. 0 ) cycle
         if ( ifrom .lt. 0 .and. ito .lt. 0 ) cycle
         abound = .false.
         if ( ifrom .lt. 0 .or. ito .lt. 0 ) abound = .true.

         a  = area(iq)
         q  = flow(iq)
         e  = disp (1)                                 ! there is only one direction
         al = aleng(1,1)                               ! the vertical
         if ( ilflag .eq. 1 ) then
            al = aleng(1,iq) + aleng(2,iq)
         endif
         if ( al .gt. 1.0E-25 ) then
            dl = a / al
         else
            dl = 0.0
         endif
         e  = e*dl
         if ( iq .gt. noqw ) e = 0.0      !  no constant water diffusion in the bottom

!         the regular case

         do isys = 1, nosys

!           advection

            v = q
            if ( ivpnt(isys) .gt. 0 ) v = v + velo  ( ivpnt(isys), iq ) * a
            if ( v .gt. 0.0 ) then
               v1 = v*idt
               v2 = 0.0
            else
               v1 = 0.0
               v2 = v*idt
            endif

!           diffusion

            d = e
            if ( idpnt(isys) .gt. 0 ) d = d + disper( idpnt(isys), iq ) * dl
            if ( disp0q0 .and. abs(q+v) .lt. 10.0E-25 ) d = 0.0
            if ( abound  .and. disp0bnd ) d = 0.0

!           fill the tridiag matrix

            q3 = v1 + d * idt
            q4 = v2 - d * idt
            if ( .not. abound ) then   ! the regular case
               diag  (isys,ifrom) = diag  (isys,ifrom) + q3
               bcodia(isys,iq   ) = bcodia(isys,iq   ) + q4
               diag  (isys,ito  ) = diag  (isys,ito  ) - q4
               acodia(isys,iq   ) = acodia(isys,iq   ) - q3
            else
               if ( ito   .gt. 0 ) then
                  q3 = q3 * bound(isys,-ifrom)
                  diag  (isys,ito  ) = diag  (isys,ito  ) - q4
                  rhs   (isys,ito  ) = rhs   (isys,ito  ) + q3
               endif
               if ( ifrom .gt. 0 ) then
                  q4 = q4 * bound(isys,-ito  )
                  diag  (isys,ifrom) = diag  (isys,ifrom) + q3
                  rhs   (isys,ifrom) = rhs   (isys,ifrom) - q4
               endif
            endif
         enddo

!        End of loop

   10 continue

!    Now make the solution:  loop over exchanges in the water

      do iq = 1 , noqw
         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)
         if ( ifrom .le. 0 .or. ito .le. 0 ) cycle
         do isys = 1, nosys
            pivot = acodia(isys,iq     )/diag(isys,ifrom)
            diag(isys,ito) = diag(isys,ito) - pivot*bcodia(isys,iq     )
            rhs (isys,ito) = rhs (isys,ito) - pivot*rhs   (isys,ifrom  )
         enddo
      enddo

!    loop over exchanges in the bed

      do iq = noqw+1 , noq
         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)
         if ( ifrom .le. 0 .or. ito .le. 0 ) cycle
         iq3 = 0                            !  find the second equivalent
         do iq2 = iq+1, noq                 !  pointer
            if ( ipoint(1,iq2) .eq. ifrom .and.
     &           ipoint(2,iq2) .eq. ito         ) then
               iq3 = iq2
               exit
            endif
         enddo                              !  if not found, this was the
         if ( iq3 .eq. 0 ) cycle            !  the second and must be skipped
         do isys = 1, nosys
            pivot = acodia(isys,iq ) + acodia(isys,iq3)
            pivot = pivot / diag(isys,ifrom)
            rhs (isys,ito) = rhs (isys,ito) - pivot * rhs(isys,ifrom)
            diag(isys,ito) = diag(isys,ito) - pivot *
     &            ( bcodia(isys,iq ) + bcodia(isys,iq3) )
         enddo
      enddo

!    inverse loop over exchanges in the bed

      do iq = noq , noqw+1 , -1
         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)
         if ( ito .le. 0 ) cycle
         iq3 = 0                            !  find the second equivalent
         do iq2 = iq-1, noqw+1, -1          !  pointer
            if ( ipoint(1,iq2) .eq. ifrom .and.
     &           ipoint(2,iq2) .eq. ito         ) then
               iq3 = iq2
               exit
            endif
         enddo                              !  if not found, this was the
         if ( iq3 .eq. 0 ) cycle            !  the second and must be skipped
         do isys = 1, nosys
            pivot = diag(isys,ito)
            rhs (isys,ito) = rhs(isys,ito) / pivot
            diag(isys,ito) = 1.0
         enddo
         if ( ifrom .le. 0 ) cycle
         do isys = 1, nosys
            pivot = bcodia(isys,iq ) + bcodia(isys,iq3)
            rhs (isys,ifrom) = rhs (isys,ifrom) - pivot*rhs(isys,ito)
         enddo
      enddo

!     Inverse loop over exchanges in the water phase

      do iq = noqw, 1, -1
         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)
         if ( ito   .le. 0 ) cycle
         do isys = 1, nosys
            pivot = diag(isys,ito)
            rhs (isys,ito) = rhs(isys,ito) / pivot
            diag(isys,ito) = 1.0
         enddo
         if ( ifrom .le. 0 ) cycle
         do isys = 1, nosys
            pivot = bcodia(isys,iq)
            rhs (isys,ifrom) = rhs (isys,ifrom) - pivot*rhs(isys,ito)
         enddo
      enddo

      do iseg = 1, noseg       !  for if some diagonal entries are not 1.0
         do isys = 1, nosys
            rhs (isys,iseg) = rhs(isys,iseg) / diag(isys,iseg)
            diag(isys,iseg) = 1.0
         enddo
      enddo
      conc  = rhs
      deriv = diag

!     Mass balances ?

      if ( iaflag .eq. 0 ) goto 9999

      do iq = 1 , noq

         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)
         if ( ifrom .eq. 0 .or.  ito .eq. 0 ) cycle
         if ( ifrom .lt. 0 .and. ito .lt. 0 ) cycle            ! trivial
         abound = .false.
         iqd    = iqdmp(iq)
         if ( ifrom .ge. 0 .and. ito .ge. 0 ) then             ! internal
            if ( iqd .le. 0 ) cycle                            ! no dump required
         else
            abound = .true.                                    ! is boundary
         endif
         a    = area(iq)
         q    = flow(iq)
         e    = disp (1)
         al   = aleng(1,1)
         if ( ilflag .eq. 1 ) then
            al = aleng(1,iq) + aleng(2,iq)
         endif
         if ( al .gt. 1.0E-25 ) then
            dl = a / al
         else
            dl = 0.0
         endif
         e  = e*dl
         if ( iq .gt. noqw ) e = 0.0      !  no constant water diffusion in the bottom

         do isys = 1, nosys

            v = q
            if ( ivpnt(isys) .gt. 0 ) v = v + velo  ( ivpnt(isys), iq ) * a
            if ( v .gt. 0.0 ) then
               v1 = v * idt
               v2 = 0.0
            else
               v1 = 0.0
               v2 = v * idt
            endif

            d = e
            if ( idpnt(isys) .gt. 0 ) d = d + disper( idpnt(isys), iq ) * dl
            if ( disp0q0 .and. abs(q+v) .lt. 10.0E-25 ) d = 0.0
            if ( abound  .and. disp0bnd ) d = 0.0

            q3 = v1 + d * idt
            q4 = v2 - d * idt
            if ( abound ) then
               if ( ito   .gt. 0 )  then
                  dq = q3 * bound(isys,-ifrom) + q4 * rhs  (isys, ito  )
                  if ( dq .gt. 0.0 ) then
                     amass2( isys, 4) = amass2( isys, 4) + dq
                  else
                     amass2( isys, 5) = amass2( isys, 5) - dq
                  endif
               else
                  dq = q3 * rhs  (isys, ifrom) + q4 * bound(isys,-ito  )
                  if ( dq .gt. 0.0 ) then
                     amass2( isys, 5) = amass2( isys, 5) + dq
                  else
                     amass2( isys, 4) = amass2( isys, 4) - dq
                  endif
               endif
            else
               dq = q3 * rhs  (isys, ifrom) + q4 * rhs  (isys, ito  )
            endif
            if ( iqd .gt. 0 ) then
               if ( dq .gt. 0 ) then
                  dmpq(isys,iqd,1) = dmpq(isys,iqd,1) + dq
               else
                  dmpq(isys,iqd,2) = dmpq(isys,iqd,2) - dq
               endif
            endif
         enddo

      enddo

 9999 if ( timon ) call timstop ( ithandl )

      return
      end

      end module m_dlwqe1
