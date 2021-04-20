   SUBROUTINE isflownode1D2D(xp,yp,kk)
   use m_flowgeom
   use unstruc_display
   use m_missing, only: dmiss, jins
   use geometry_module, only: pinpok, dbdistance
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   double precision :: xp, yp, dis
   integer          :: inn, k, kk, nn

   kk = 0
   DO K = ndx2D+1, ndx
      dis = dbdistance(xz(k), yz(k), xp, yp, jsferic, jasfer3D, dmiss)
      if (dis < rcir) then
         kk = k ; return
      endif
   enddo

   if ( .not.allocated(nd) ) then
      return
   end if

   DO K = 1,ndx2D
      if (.not. allocated(nd(K)%x)) cycle
         NN = size(nd(K)%x)
      CALL PINPOK(xp, yp , NN, nd(K)%x, nd(K)%y, inn, jins, dmiss)
      IF (inn == 1) THEN
         KK = K ; RETURN
      ENDIF
   ENDDO

   END SUBROUTINE isflownode1D2D
