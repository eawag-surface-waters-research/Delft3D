subroutine compdxx(frac      ,nseddia   ,logseddia ,logsedsig , &
                 & nmmax     ,lsedtot   ,sedtyp    ,dxx       , &
                 & xx        ,nxx       ,sedd50fld ,nmlb      , &
                 & nmub      )
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
!  $Id$
!  $HeadURL$
!!--description-----------------------------------------------------------------
!
! Function:   Determines the diameter Dxx of the xx-percentile of the
!             non-mud fractions.
! Assumption: The fractions stored in the xx array should be in increasing
!             order and be larger than 0 and smaller than 1.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    include 'lognormal.inc'
!
! Global variables
!
    integer                                             , intent(in)  :: lsedtot   ! number of sediment fractions
    integer                                             , intent(in)  :: nxx       ! number of diameters to be determined
    integer                                             , intent(in)  :: nmmax     ! last space index to be processed
    integer                                             , intent(in)  :: nmlb      ! start space index
    integer                                             , intent(in)  :: nmub      ! end space index
    integer     , dimension(lsedtot)                    , intent(in)  :: nseddia   ! number of sediment diameters per fraction
    real(fp)    , dimension(nxx)                        , intent(in)  :: xx        ! percentile: the xx of Dxx, i.e. 0.5 for D50
    real(fp)    , dimension(nmlb:nmub, lsedtot)         , intent(in)  :: frac      ! fractional composition of sediment
    real(fp)    , dimension(nmlb:nmub, nxx)             , intent(out) :: dxx       ! diameters corresponding to percentiles
    real(fp)    , dimension(2,101,lsedtot)              , intent(in)  :: logseddia ! percentile and log-diameter per fraction
    real(fp)    , dimension(lsedtot)                    , intent(in)  :: logsedsig ! std deviation of sediment diameter
    real(fp)    , dimension(nmlb:nmub)                  , intent(in)  :: sedd50fld ! D50 field (in case of 1 sediment fraction)
    character(4), dimension(lsedtot)                    , intent(in)  :: sedtyp    ! sediment type: sand/mud/bedload
!
! Local variables
!
    integer                     :: i
    integer                     :: l
    integer                     :: ltrigger
    integer                     :: n
    integer                     :: nm
    integer                     :: nn
    integer                     :: s
    integer, dimension(lsedtot) :: stage
    real(fp)                    :: dens
    real(fp)                    :: logdiam
    real(fp)                    :: logdprev
    real(fp)                    :: fracnonmud
    real(fp)                    :: fraccum
    real(fp)                    :: fracreq
    real(fp)                    :: fracinterval
    real(fp)                    :: mf1
    real(fp)                    :: mf2
    real(fp)                    :: mulfac
    real(fp)                    :: p1
    real(fp)                    :: p2
    real(fp)                    :: sedsg
    real(fp)                    :: xxperc
!
!! executable statements -------------------------------------------------------
!
    if (nxx==0) return
    !
    ! Calculate the Dxx diameter by scanning the cdf of all
    ! fractions. The algorithm allows for overlapping fraction
    ! definitions and fractions with only one exact diameter.
    ! The density function of each fraction is assumed to be
    ! uniform in -log2(D), i.e. psi, space. Computations are
    ! carried out in ln(D) space.
    !
    dxx = 0.0
    if (lsedtot==1 .and. nseddia(1)<0) then
       !
       ! Handle case with spatially varying sediment diameter
       ! separately using the same approximation of the lognormal
       ! distribution used in the more general case with multiple
       ! fractions each with a constant sediment diameter.
       !
       sedsg = exp(logsedsig(1))
       nn    = size(ilognormal)
       do i = 1, nxx
          xxperc = xx(i) * 100.0
          if (xxperc <= real(ilognormal(1),fp)) then
             p1  = 0.0
             mf1 = -3.0
             p2  = real(ilognormal(1),fp)
             mf2 = lognormal(ilognormal(1))
          elseif (xxperc >= real(ilognormal(nn),fp)) then
             p1  = real(ilognormal(nn),fp)
             mf1 = lognormal(ilognormal(nn))
             p2  = 100.0
             mf2 = 3.0
          else
             do n = 2, nn
                if (xxperc <= real(ilognormal(n),fp)) then
                   p1  = real(ilognormal(n-1),fp)
                   mf1 = lognormal(ilognormal(n-1))
                   p2  = real(ilognormal(n),fp)
                   mf2 = lognormal(ilognormal(n))
                   exit
                endif
             enddo
          endif
          mulfac = sedsg**(((p2-xxperc)*mf1 + (xxperc-p1)*mf2)/(p2-p1))
          do nm = 1,nmmax
             dxx(nm,i) = sedd50fld(nm)*mulfac
          enddo
       enddo
    else
       do nm = 1,nmmax
          !
          ! Initialisation: set stage and compute non-mud fraction.
          ! stage = 0 means logdiam<logseddia(2,1,l)
          !         i means logseddia(2,i,l)<logdiam<logseddia(2,i+1,l)
          !         n means logseddia(2,i,l)<logdiam where n=nseddia(l)
          ! where logdiam is used to scan through the full range of
          ! diameters. nseddia(l)=0 for mud fractions, so they are
          ! excluded from the analysis.
          !
          fracnonmud = 0.0
          do l = 1, lsedtot
             stage(l) = 0
             if (sedtyp(l) /= 'mud') then
                fracnonmud = fracnonmud + frac(nm,l)
             endif
          enddo
          !
          ! Fraccum indicates the cumulative fraction starting from
          ! the finest sediment up to the last diameter (logdprev).
          !
          logdprev = 0.0
          fraccum  = 0.0
          i        = 1
          fracreq  = xx(1) * fracnonmud
          !
          outerfracloop: do while (fracreq > fraccum)
             !
             ! Find the smallest diameter not yet considered and calculate
             ! the density (cdf) in the considered range.
             !
             logdiam  = 999.999
             ltrigger = -1
             dens     = 0.0
             do l = 1, lsedtot
                s = stage(l)
                if (s<nseddia(l)) then
                   if (s>0) then
                      dens = dens + frac(nm,l) * (logseddia(1,s+1,l) - logseddia(1,s,l)) &
                                             & / (logseddia(2,s+1,l) - logseddia(2,s,l))
                   endif
                   if (logseddia(2,s+1,l)<logdiam) then
                      logdiam  = logseddia(2,s+1,l)
                      ltrigger = l
                   endif
                endif
             enddo
             !
             ! Check if we have not reached the end of the composition, due to
             ! some numerical roundoff errors we might not have determined the
             ! diameter of fractions equal (or close) to 100%. Finish them of
             ! now! (This may also happen if we have only mud fractions in the
             ! simulation.)
             !
             if (ltrigger < 0) then
                do while (i <= nxx)
                   dxx(nm,i) = logdprev
                   i = i+1
                enddo
                exit outerfracloop
             endif
             !
             ! Compute total fraction in the current interval.
             !
             fracinterval = dens * (logdiam-logdprev)
             do while (fracinterval >= fracreq-fraccum)
                !
                ! If the total fraction is more than what is needed,
                ! find the diameter at which we have exactly enough.
                ! We have found the requested Dxx.
                !
                logdprev     = logdprev + (logdiam-logdprev)*(fracreq-fraccum)/fracinterval
                fraccum      = fracreq
                dxx(nm,i)    = exp(logdprev)
                fracinterval = dens * (logdiam-logdprev)
                if (i < nxx) then
                   i = i+1
                   fracreq = xx(i) * fracnonmud
                else
                   !
                   ! Diameter of last fraction determined, jump out
                   ! of loop.
                   !
                   exit outerfracloop
                endif
             enddo
             !
             ! The total fraction is less than what is needed for the
             ! currently required fraction, move to the next interval.
             !
             fraccum  = fraccum + fracinterval
             logdprev = logdiam
             !
             ! Update the stage index of the sediment fraction that
             ! is associated with the threshold currently considered.
             !
             if (stage(ltrigger) == 0) then
                !
                ! Start of the range of a new fraction. In the next
                ! range the density of this fraction should be added
                ! so, we set stage(l) to 1. However, if the range of
                ! the fraction is zero, the density function is given
                ! by a dirac function. In that case step over it and
                ! take the total fraction into account at once.
                !
                if (logseddia(2,nseddia(ltrigger),ltrigger) &
                    & <= logseddia(2,1,ltrigger)) then
                   !
                   ! The criterion may have to be extended to include
                   ! narrow ranges as well ...
                   !
                   stage(ltrigger) = 5
                   do while (frac(nm,ltrigger) > fracreq-fraccum)
                      !
                      ! If the total fraction is more than what is needed,
                      ! we have found the requested Dxx.
                      !
                      dxx(nm,i) = exp(logdiam)
                      if (i < nxx) then
                         i = i+1
                         fracreq = xx(i) * fracnonmud
                      else
                         !
                         ! Diameter of last fraction determined, jump out
                         ! of loop.
                         !
                         exit outerfracloop
                      endif
                   enddo
                   !
                   ! If the total fraction is less than what is needed,
                   ! take the fraction into account and continue
                   ! searching for Dxx.
                   !
                   fraccum = fraccum + frac(nm,ltrigger)
                else
                   !
                   ! Range of ltrigger not too narrow, enter it.
                   !
                   stage(ltrigger) = 1
                endif
             else ! (stage(ltrigger)==1)
                !
                ! Exit range of fraction ltrigger
                !
                stage(ltrigger) = stage(ltrigger) + 1
             endif
             !
             ! Continue with next range
             !
          enddo outerfracloop
          !
          ! continue with next nm point
          !
       enddo
    endif
    !
end subroutine compdxx
