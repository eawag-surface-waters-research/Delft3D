   SUBROUTINE MIRRORLINE (X0,Y0,X1,Y1,X2,Y2,JA,DIS,XN,YN)
   use geometry_module, only: dlinedis
   use m_sferic
   use m_missing

   implicit none
   double precision :: X0,Y0,X1,Y1,X2,Y2,DIS,XN,YN
   integer :: JA

   CALL dLINEDIS(X0,Y0,X1,Y1,X2,Y2,JA,DIS,XN,YN,jsferic, jasfer3D, dmiss)

   XN  = 2*XN - X0
   YN  = 2*YN - Y0

   RETURN
   END SUBROUTINE MIRRORLINE
