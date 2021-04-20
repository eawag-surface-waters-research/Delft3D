   SUBROUTINE in_flowcell(xp,yp,kk)

   use m_flowgeom
   use unstruc_display
   use m_missing, only: jins, dmiss
   use geometry_module, only: pinpok, dbdistance

   implicit none
   double precision :: xp, yp, dis
   integer          :: inn, k, kk, nn

   kk = 0

   DO K = 1,ndx2D
      if (.not. allocated(nd(K)%x)) cycle
      NN = size(nd(K)%x)
      CALL PINPOK(xp, yp , NN, nd(K)%x, nd(K)%y, inn, jins, dmiss)
      IF (inn == 1) THEN
         KK = K ; RETURN
      ENDIF
   ENDDO

   END SUBROUTINE in_flowcell
