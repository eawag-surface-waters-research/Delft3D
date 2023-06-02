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

      subroutine psolve ( ntrace , x      , rhs    , nomat   , amat   ,
     &                    imat   , diag   , idiag  , nolay   , ioptpc ,
     &                    nobnd  , triwrk  , iexseg )

!     Deltares - Delft Software Department

!     Created   : November 1996 by Kian Tan

!     Function  : Preconditioner for GMRES solver

!     Subroutines called: LSOLVE - preconditoner first sweep
!                         USOLVE - preconditoner 2nd   sweep

!     Modified  : July     2008, Leo Postma  : WAQ perfomance timers
!                 July     2009, Leo Postma  : double precission version
!                 November 2009, Leo Postma  : streamlined for parallel computing

      use m_srstop
      use timers
      implicit none

!     Arguments           :

!     Kind        Function         Name                  Description

      integer(4), intent(in   ) :: ntrace               ! dimension of matrix (length of diagonal)
      real   (8), intent(  out) :: x     (  ntrace)     ! the solution of Mx = y
      real   (8), intent(inout) :: rhs   (  ntrace)     ! right hand side of P-solve only
                                                        ! this vector may be changed on exit!!
      integer(4), intent(in   ) :: nomat                ! number of off-diagonal entries matrix
      real   (8), intent(in   ) :: amat  (  nomat )     ! off-diagonal entries matrix LP format
      integer(4), intent(in   ) :: imat  (  nomat )     ! collumn nrs of off-diagonal entries matrix
      real   (8), intent(in   ) :: diag  (  ntrace)     ! diagonal entries of matrix
      integer(4), intent(in   ) :: idiag (0:ntrace)     ! start of rows in amat
      integer(4), intent(in   ) :: nolay                ! number of layers in the vertical
      integer(4), intent(in   ) :: ioptpc               ! = 0 no preconditioning
                                                        ! = 1 L-GS preconditioning
                                                        ! = 2 U-GS preconditioning
                                                        ! = 3 SSOR preconditioning
      integer(4), intent(in   ) :: nobnd                ! number of open boundaries
      real   (8), intent(inout) :: triwrk(  nolay )     ! work array for vertical double sweep
      integer(4), intent(in   ) :: iexseg(  ntrace)     ! 0 for explicit volumes

!        local variables

      integer(4)                :: noseg                ! nr of volumes
      integer(4)                :: nsegl                ! nr of volumes per layer
      integer(4)                :: iadd                 ! 0 for 2DH, 2 for 3D
      integer(4)                :: iseg                 ! this volume
      integer(4)                :: jcol                 ! collumn counter for off-diagonals
      integer(4)                :: ilow, ihigh          ! loop boundaries

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "psolve", ithandl )

      if ( nolay .eq. 1) then
         iadd = 0
      else
         iadd = 2
      endif

      noseg = ntrace - nobnd
      nsegl = noseg / nolay
      if ( nsegl*nolay .ne. noseg ) then
         write(*,*) 'ERROR in PSOLVE'
         call srstop(1)
      endif

      if ( ioptpc .eq. 0 ) then

         x = rhs

      else if( ioptpc .eq. 1 ) then

         call lsolve ( ntrace, noseg , nolay  , nsegl  , nomat  ,
     &                 amat  , imat  , diag   , idiag  , x      ,
     &                 rhs   , triwrk, iadd   , iexseg )

      else if( ioptpc .eq. 2 ) then

         call usolve ( ntrace, nolay  , nsegl  , nomat  , amat   ,
     &                 imat  , diag   , idiag  , x      , rhs    ,
     &                 triwrk , iadd   , iexseg )

      else if( ioptpc .eq. 3 ) then

!            SSOR (Symmetric Successive Over Relaxation)

!            M = (D - L) "D^-1" (D - U)

         call lsolve ( ntrace, noseg , nolay  , nsegl  , nomat  ,
     &                 amat  , imat  , diag   , idiag  , x      ,
     &                 rhs   , triwrk, iadd   , iexseg )

!        THe "D^{-1}" part, note that due to the b.c entries this is
!        a rather peculiar piece of code

         if ( nolay .eq. 1) then

!              diagonal element is scalar

            do iseg = 1, ntrace

               rhs(iseg) = diag(iseg)*x(iseg)

!              extra "b.c" entries

               ilow  = idiag(iseg-1) + 1
               ihigh = idiag(iseg)
               do jcol = ilow+iadd, ihigh
                  if ( imat(jcol) .gt. noseg ) then
                     rhs(iseg) = rhs(iseg) + amat(jcol) * x(imat(jcol))
                  endif
               enddo

            enddo

         else

!              diagonal element is tridiagonal K x K matrix
!              but we can simply loop over the NOSEG (=N-NOBND) segments
!              There has been a bug in this section already from the start.
!              the first layer has no layer above and the last layer has
!              no layer below.

            do iseg = 1, noseg
               ilow = idiag(iseg-1)+1
               rhs(iseg) = diag(iseg)*x(iseg)
               if ( imat(ilow  ) .gt. 0 ) rhs(iseg) = rhs(iseg) + amat(ilow  )*x(imat(ilow  ))
               if ( imat(ilow+1) .gt. 0 ) rhs(iseg) = rhs(iseg) + amat(ilow+1)*x(imat(ilow+1))

!              extra "b.c." entries

               if ( iexseg(iseg) .eq. 0 ) cycle
               ihigh = idiag(iseg)
               do jcol = ilow+iadd, ihigh
                  if ( imat(jcol) .gt. noseg ) then
                     rhs(iseg) = rhs(iseg) + amat(jcol) * x(imat(jcol))
                  endif
               enddo
            enddo
            do iseg = noseg+1, ntrace
               rhs(iseg) = diag(iseg)*x(iseg)
            enddo

         endif

         call usolve ( ntrace, nolay  , nsegl  , nomat  , amat   ,
     &                 imat  , diag   , idiag  , x      , rhs    ,
     &                                  triwrk , iadd   , iexseg )

      else
          write(*,*) ' This option for Pre-Conditioning '
          write(*,*) ' is not implemented :   ABORT     '
          call srstop(1)
      endif

      if ( timon ) call timstop ( ithandl )
      return
      end
