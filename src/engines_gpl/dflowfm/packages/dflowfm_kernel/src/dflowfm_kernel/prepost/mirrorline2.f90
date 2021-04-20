   SUBROUTINE MIRRORLINE2(X0,Y0,X1,Y1,X2,Y2,JA,DIS,XN,YN)  ! 2*ZO VER

   use geometry_module, only: dlinedis
   use m_missing,       only: dmiss
   use m_sferic,        only: jsferic, jasfer3D

   implicit none
   double precision :: X0,Y0,X1,Y1,X2,Y2,DIS,XN,YN
   integer :: JA

   CALL dLINEDIS(X0,Y0,X1,Y1,X2,Y2,JA,DIS,XN,YN,jsferic, jasfer3D, dmiss)

   XN  = 3*XN - 2*X0
   YN  = 3*YN - 2*Y0

   RETURN
   END SUBROUTINE MIRRORLINE2
