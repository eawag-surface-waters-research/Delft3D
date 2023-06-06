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

      subroutine sgmres ( ntrace , rhs    , sol    , restrt , work   ,                 &
     &                    ldw    , hess   , ldh    , maxit  , tol    ,                 &
     &                    nomat  , amat   , imat   , diag   , idiag  ,                 &
     &                    klay   , ioptpc , nobnd  , triwrk , iexseg ,                 &
     &                    lurep  , litrep )

!     Deltares Software Centre

!>/file
!>          This is the GMRES solver
!>
!>          The solver:
!>          - preconditions with the psolve routine either:
!>            - none
!>            - upper triangular matrix
!>            - lower triangular matrix
!>            - both (this is the default preconditioner)
!>          - constructs an orthonormal set of approximation vectors (Krylov space)
!>          - if no convergence at end of Krilov space, solver restarts
!>          - if no convergence at maxiter the solver stops



      use m_srstop
      use timers                         ! WAQ performance timers
      implicit none

!     Arguments           :

!     Kind        Function         Name                         Description

      integer(4), intent(in   ) :: ntrace                      !< Dimension of the matrix
      real   (8), intent(in   ) :: rhs    (ntrace)             !< right-hand side (1 substance)
      real   (8), intent(inout) :: sol    (ntrace)             !< on entry: initial guess / on exit: solution
      integer(4), intent(in   ) :: restrt                      !< size of Krylov space, restrt < ntrace !
      real   (8)                   work   (ntrace  ,restrt+5)  !< workspace
      integer(4), intent(in   ) :: ldw                         !< leading dimension >= max(1,ntrace  ) (probably superfluous lp)
      real   (8), intent(in   ) :: hess   (restrt+1,restrt+2)  !< hessenberg matrix
      integer(4), intent(in   ) :: ldh                         !< leading dimension >= max(1,restrt+1) (probably superfluous lp)
      integer(4), intent(in   ) :: maxit                       !< maximum number of iterations
      real   (8), intent(in   ) :: tol                         !< convergence criterion
      integer(4), intent(in   ) :: nomat                       !< number of off-diagonal entries of matrix a (format from lp)
      real   (8), intent(in   ) :: amat   (nomat)              !< off-diagonal entries of matrix a (format from lp)
      integer(4), intent(in   ) :: imat   (nomat)              !< pointer table off-diagonal entries
      real   (8), intent(in   ) :: diag   (ntrace)             !< diagonal entries of matrix a
      integer(4), intent(in   ) :: idiag  (0:ntrace)           !< position of the diagonals in amat
      integer(4), intent(in   ) :: klay                        !< number of layers
      integer(4), intent(in   ) :: ioptpc                      !< option for preconditioner
      integer(4), intent(in   ) :: nobnd                       !< number of open boundaries
      real   (8)                   triwrk (klay*6)             !< workspace for tridiagonal solution vertical
      integer(4), intent(in   ) :: iexseg (ntrace)             !< 0 for explicit volumes
      integer(4), intent(in   ) :: lurep                       !< Unit number report file
      logical   , intent(in   ) :: litrep                      !< Perform report on iteration process if TRUE

!        Local constants

      REAL(8)     SMALL    , SMALL2

!     SMALL MUST always be larger then SMALL2 !!!!!!!!!!!

      PARAMETER ( SMALL= 1.0E-7 , SMALL2= 1.0E-25)

!        Local Scalars

      INTEGER   I      , J      , K      , iter   , AV     , CS     ,                          &
     &          SN     , R      , S      , V      , W      , Y      ,                          &
     &          I2     , IERR   , imax   , iloop
      REAL(8)   AA     , BB     , BNRM2  , RNORM  , RESID, rmax

      LOGICAL   FIRST
      DATA            FIRST  /.TRUE./

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "sgmres", ithandl )

!        sloppy way of output

      ITER = 0
      IERR = 0
      IF (FIRST) THEN
         FIRST = .FALSE.
         IF ( LITREP ) THEN
            WRITE(LUREP,*) '   ITER       TOL  OPTION:   ', maxit,TOL,IOPTPC
            WRITE(LUREP,*) '   CYCLE    RESID  '
         ENDIF
      ENDIF

!        Test the input parameters.

      IF ( ntrace .LT.0 ) THEN
         IERR = -1
      ELSE IF ( LDW.LT.MAX( 1, ntrace ) ) THEN
         IERR = -2
      ELSE IF ( maxit .LE. 0 ) THEN
         IERR = -3
      ELSE IF ( LDH.LT.RESTRT+1 ) THEN
         IERR = -4
      ENDIF
      IF ( IERR.NE.0 ) goto 9999


!     Alias workspace columns.

      R  = 1
      S  = R + 1
      W  = S + 1
      Y  = W
      AV = Y + 1
      V  = AV + 1


!     Store the Givens parameters in matrix H.

      CS = RESTRT + 1
      SN = CS + 1

!   Adapt initial guess if necessary

      do iloop = 1, ntrace
         if ( isnan(rhs(iloop)) ) then
            write ( lurep, '(''ERROR: NaN in RHS of segment:'', i10)' ) iloop
            call srstop(1)
         endif
      enddo
      bnrm2 = sqrt( sum(rhs*rhs) )
      if ( bnrm2 .lt. small ) sol = 0.0

!     Set initial residual (AV is temporary workspace here).

      work(:,av) = rhs
      IF ( sqrt( sum(sol*sol) ) .NE. 0.0D+00 ) THEN
         CALL MATVEC ( ntrace , NOMAT  , -1.0D+00, amat   , imat     ,                         &
     &                 DIAG   , IDIAG  , sol     , 1.0D+00, WORK(1,AV) )
      ENDIF

      CALL PSOLVE ( ntrace , WORK(1,R), WORK(1,AV),                                            &
     &              NOMAT  , amat     , imat      , DIAG   , IDIAG    ,                        &
     &              KLAY   , IOPTPC   , NOBND     , TRIWRK , iexseg   )

      IF ( BNRM2 .EQ. 0.0D+00 ) BNRM2 = 1.0D+00

      RESID = sqrt( sum(WORK(:,R)*work(:,r)) ) / BNRM2
      IF ( LITREP ) THEN
         WRITE (LUREP,'(''        Cycle   Residue       Norm-B '')')
         WRITE (LUREP,'(''GMRES'',I7,2E13.5)') 0, RESID, BNRM2
      ENDIF
!jvb
      I = 0
!jvb
      IF ( BNRM2 .LT. SMALL2 ) THEN
         IF ( LITREP ) THEN
            WRITE (LUREP,'(''NORM RHS < '',E12.4,'' HAS CONVERGED '')') SMALL2
         ENDIF
         GOTO 70
      ENDIF
      IF ( RESID .LT. TOL ) GOTO 70



!     Main GMRES iteration loop

   10 CONTINUE

         I = 0

!        Construct the first column of V.

         work(:,v) = work(:,r)
         rnorm = sqrt( sum(work(:,v)*work(:,v)) )
         work(:,v) = work(:,v)/rnorm

!        Initialize S to the elementary vector E1 scaled by RNORM.

         WORK(1,S) = RNORM
         DO 20 K = 2, ntrace
            WORK(K,S) = 0.0D+00
   20    CONTINUE

   30    CONTINUE

            I = I + 1
            ITER = ITER + 1

            CALL MATVEC ( ntrace , NOMAT  , 1.0D+00, amat  , imat     ,                         &
     &                    DIAG   , IDIAG  , WORK(1,V+I-1)  , 0.0D+00  ,                         &
     &                    WORK(1,AV) )
            CALL PSOLVE ( ntrace , WORK(1,W), WORK(1,AV),                                       &
     &                    NOMAT  , amat     , imat      , DIAG   , IDIAG  ,                     &
     &                    KLAY     , IOPTPC    , NOBND  , TRIWRK , iexseg )

!           Construct I-th column of H orthnormal to the previous I-1 columns.

            CALL BASIS ( I, ntrace, hess(1,I), WORK(1,V), LDW, WORK(1,W) )

!--         Each SROT is a multiplication with a plane rotation such that  --c
!--         [ c  s ][h(k,i)  ] = [ g ]                                     --c
!--         [-s  c ][h(k+1,i)] = [ 0 ]                                     --c

            DO 40 K = 1, I-1
               CALL SROT ( 1      , hess(K,I) , LDH    , hess(K+1,I) ,                          &
     &                     LDH    , hess(K,CS), hess(K,SN)            )
   40       CONTINUE


!           Construct the I-th rot.matrix, and apply it to H so that H(I+1,I)=0.

            AA = hess(I,I)
            BB = hess(I+1,I)
            CALL SROTG ( AA , BB, hess(I,CS), hess(I,SN) )
            CALL SROT  ( 1      , hess(I,I) , LDH     , hess(I+1,I) ,                           &
     &                   LDH    , hess(I,CS), hess(I,SN)            )


!           Apply the I-th rotation matrix to [ S(I), S(I+1) ]'. This
!           gives an approximation of the residual norm. If less than
!           tolerance, update the approximation vector sol and quit.

            CALL SROT ( 1           , WORK(I,S)   , LDW      ,                                  &
     &                  WORK(I+1,S) , LDW         , hess(I,CS)  ,                               &
     &                  hess( I,SN )                            )

            RESID = ABS( WORK(I+1,S) ) / BNRM2

            IF ( LITREP ) THEN
               WRITE (LUREP,'(''GMRES'',I7,E13.5)') ITER, RESID
            ENDIF
            IF ( RESID.LE.TOL ) THEN
               CALL UPDATS ( I      , ntrace , sol    , hess      ,                             &
     &                       LDH    , WORK(1,Y)       , WORK(1,S) ,                             &
     &                       WORK(1,V)       , LDW                )
               GO TO 70
!KHT           GO TO 999
            ENDIF
            IF ( ITER.EQ.MAXIT ) GO TO 50
            IF ( I.LT.RESTRT )   GO TO 30

   50    CONTINUE

!        Compute current solution vector sol

         CALL UPDATS ( RESTRT , ntrace , sol    , hess   , LDH    ,                             &
     &                 WORK(1,Y)       , WORK(1,S)       ,                                      &
     &                 WORK(1,V)       , LDW                      )


!        Compute residual vector R, find norm, then check for tolerance.
!        (AV is temporary workspace here.)


999      CONTINUE

         work(:,av) = rhs
         CALL MATVEC ( ntrace , NOMAT  , -1.0D+00, amat   , imat     ,                          &
     &                 DIAG   , IDIAG  , sol     , 1.0D+00, WORK(1,AV) )
         CALL PSOLVE ( ntrace , WORK( 1,R ), WORK( 1,AV ) ,                                     &
     &                 NOMAT  , amat     , imat      , DIAG   , IDIAG    ,                      &
     &                 KLAY   , IOPTPC   , NOBND     , TRIWRK , iexseg   )
         work(i+1,s) = sqrt( sum(work(:,r)*work(:,r)) )
         RESID = WORK(I+1,S) / BNRM2
         IF ( RESID.LE.TOL  ) GO TO 70
         IF ( ITER.EQ.MAXIT ) GO TO 60

!        Restart.

         IF ( LITREP ) THEN
            WRITE (LUREP,*) 'GMRES RESTARTING', RESID
         ENDIF
         GO TO 10

   60 CONTINUE


!     Iteration fails.

      IERR = 1
      goto 9999

   70 CONTINUE

!     Iteration successful; return.

!     Filter solution for small negative values

      DO I2 = 1, ntrace
         IF ( sol(I2) .LT. 0.0D+00 .AND. sol(I2) .GT. -TOL*10 ) THEN
            sol(I2) = 0.0D+00
         ENDIF
      ENDDO

      work(:,av) = rhs
      CALL MATVEC ( ntrace , NOMAT  , -1.0D+00, amat   , imat     ,                             &
     &              DIAG   , IDIAG  , sol     , 1.0D+00, WORK(1,AV))
      CALL PSOLVE ( ntrace, WORK( 1,R ), WORK( 1,AV ) ,                                         &
     &          NOMAT  , amat     , imat      , DIAG   , IDIAG    ,                             &
     &          KLAY   , IOPTPC   , NOBND     , TRIWRK , iexseg   )
      work(i+1,s) = sqrt( sum(work(:,r)*work(:,r)) )
      RESID = WORK(I+1,S) / BNRM2

      IF ( LITREP ) THEN
         WRITE (LUREP,'(''Cycles,T.U.P.E.RES,T.S.P.E.RES,BNRM2'',                               &
     &         I8,3E13.5)')                                                                     &
     &         ITER, WORK(I+1,S),RESID,BNRM2
      ENDIF

!    Check for true preconditioned residual and effects of the filter

      IF ( RESID .GT. TOL ) THEN
         IF ( LITREP ) THEN
            WRITE(LUREP,*) ' AFTER FILTERING THE RESIDUAL EXCEEDS TOL ', RESID
         ENDIF
      ENDIF

 9999 if ( timon ) call timstop ( ithandl )

!        if on error, stop

      IF (IERR .GT. 0) THEN
         WRITE (*,*) 'ERROR in GMRES', IERR
         WRITE (LUrep,*) ' ERROR in GMRES 1', IERR
         WRITE (LUrep,*) ' Solver did not reach convergence'
         WRITE (LUrep,*) ' maximum contribution in error:', resid
         if ( .not. litrep ) WRITE (LUrep,*) ' Switch ITERATION REPORT to on to see details'
         WRITE (LUrep,*) ' Reduce the output time step to 1 time step close to point of failure'
         WRITE (lurep,*) ' Possible causes in decreasing frequency of likelyness:'
         WRITE (lurep,*) ' 1. NaNs in the solution of water quality'
         WRITE (lurep,*) '    inspect total mass in monitoring file for substance(s) on NaNs'
         WRITE (lurep,*) ' 2. Inconsistency in drying and flooding of hydrodynamics'
         WRITE (lurep,*) '    exact cause will be difficult to identify, cell nr. may help'
         WRITE (lurep,*) ' 3. Normal lack of convergence, e.g. strongly diffusive problem'
         WRITE (LUrep,*) '    possible solutions: increase TOLERANCE'
         WRITE (LUrep,*) '                        decrease timestep'
         WRITE (LUrep,*) '                        increase MAXITER'
         WRITE (lurep,*) ' 4. other: exact cause will be difficult to identify, cell nr. may help'
         CALL SRSTOP(1)
      ELSEIF (IERR .LT. 0 ) THEN
         WRITE (*,*) 'ERROR in GMRES', IERR
         WRITE (LUrep,*) ' ERROR in GMRES 1', IERR
         WRITE (LUrep,*) ' solver entered with wrong parameters'
         WRITE (LUrep,*) ' consult the WAQ helpdesk'
      ENDIF

      RETURN
      END


      SUBROUTINE UPDATS ( I      , N      , X      , H      , LDH     ,                         &
     &                    Y      , S      , V      , LDV              )
      use timers
      IMPLICIT NONE

      INTEGER   N      , I      , LDH    , LDV
      REAL(8)   X(*)   , Y(i)   , S(i)   , H(LDH,*)        , V(LDV,*)

!        Subroutines called (BLAS)

      EXTERNAL  SGEMV, STRSV


!     This routine updates the GMRES iterated solution approximation.

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "updats", ithandl )


!     Solve H*Y = S for upper triangualar H.

      y = s
      CALL STRSV ( 'UPPER', 'NOTRANS', 'NONUNIT', I, H, LDH, Y, 1 )


!     Compute current solution vector X = X + V*Y.

      CALL SGEMV ('NOTRANS', N, I, 1.0D+00, V, LDV, Y, 1, 1.0D+00 , X, 1 )

      if ( timon ) call timstop ( ithandl )
      RETURN
      END


      subroutine basis ( i, n, h, v, ldv, w )

      use timers
      implicit none

      integer  i, n, ldv
      real(8)  h(i+1), w(n), v(n,i+1)

!     Construct the I-th column of the upper Hessenberg matrix H
!     using the Modified Gram-Schmidt process on V and W.

      real(8)   hscal, aid
      integer   k
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "basis", ithandl )

      do k = 1, i
         h(k) = sum( w * v(:,k) )
         w    = w - h(k) * v(:,k )
      enddo

!        re-orthogonalisation

      do k = 1, i
         aid = sum ( w * v(:,k) )
         h( k ) = h(k) + aid
         w   = w - aid * v(:,k )
      enddo

      h(i+1) = sqrt( sum(w*w) )
      hscal  = 1.0D+00 / max(h(i+1),1.0d-12)
      v(:,i+1) = w*hscal

      if ( timon ) call timstop ( ithandl )
      return
      end
