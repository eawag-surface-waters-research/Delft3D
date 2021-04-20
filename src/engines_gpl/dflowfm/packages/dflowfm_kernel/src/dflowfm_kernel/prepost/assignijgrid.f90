!>  assign indices (i,j) to the curvi-linear grid
subroutine assignijgrid(k, ic, jc)

 use m_netw
 use m_grid
 use m_missing

 implicit none

 integer                  :: k                           !< current cell

 integer, dimension(numk) :: ic, jc                      !< indices (i,j) of the nodes

 integer                  :: kcell, kneighbor, kdir, kdirdum
 integer                  :: icount, iter, lowold(2), uppold(2)
 integer, parameter       :: MAXITER = 1000000
! integer, parameter       :: IMISS   = -999999

 integer                  :: i, numiter_guess
!---------------------------------------------------------
! cellmask
!   -1 : inactive,     in curvi-grid
!    0 : inactive, not in curvi-grid
!    1 :   active, not in front
!   >1 :   active,     in front
!---------------------------------------------------------

! mask current cell as frontcell
 cellmask(k) = 10

 lowold = lbound(ijc)
 uppold = ubound(ijc)

 numiter_guess = sqrt(dble(nump)) * 10

 call readyy('creating curvilinear grid', 0d0 )

 do iter = 1,MAXITER
    call readyy('creating curvilinear grid', min(dble(iter-1)/dble(numiter_guess-1), 1d0) )

    icount = 0

    do kcell = 1,nump
       if ( cellmask(kcell) .eq. iter+9 ) then

! done with cell kcell - unmask cell
          cellmask(kcell) = -1

          do kdir = 1,4

             call assignij(kcell, kdir, kneighbor, ic, jc)

             if ( kneighbor.ne.0 ) then
                if ( cellmask(kneighbor).eq.1 ) then
                   cellmask(kneighbor)    = iter + 10
                   icount                 = icount + 1
                end if
             end if
          end do
       end if
    end do


! only one layer of cells will be added during the next iteration at maximum
    call grow_ijc( lowold, uppold,                                                     &
                         (/ minval(ic, ic.ne.IMISS)-1, minval(jc, jc.ne.IMISS)-1 /),   &
                         (/ maxval(ic, ic.ne.IMISS)+1, maxval(jc, jc.ne.IMISS)+1 /), 0)

    if ( icount .eq.0 ) exit
 end do

 if ( iter.eq.MAXITER ) write(6,*) 'assignijgrid: iter=MAXITER'

 call readyy('creating curvilinear grid', -1d0 )

end subroutine assignijgrid
