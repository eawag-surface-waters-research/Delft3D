     SUBROUTINE nPLOTPLUSMIN(IPM)
     USE M_FLOW
     use M_flowgeom
     implicit none
     integer :: IPM, NRLAY


      IF (IPM == 1) THEN
!         nPLOT = MIN(nPLOT+1,ndx)
         nplot = nplot+1
         if ( nplot.gt.Ndx ) nplot = nplot - Ndx
      ELSE if (ipm == -1) then
!         nPLOT = MAX(nPLOT-1,1)
         nplot = nplot-1
         if ( nplot.lt.1 ) nplot = nplot + Ndx
      else
         nplot = ipm
      ENDIF
      if (kmx > 0) then
         NRLAY = KTOP(NPLOT) - KBOT(NPLOT) + 1
         KPLOT = MAX(1, MIN(KPLOT, NRLAY) )
      endif
      CALL TEXTFLOW()
      END SUBROUTINE nPLOTPLUSMIN
