   SUBROUTINE MIRRORLINEPOINT (X0,Y0,X3,Y3,X1,Y1,X2,Y2,JA,DIS,XN,YN)
   use geometry_module, only: getdxdy, dlinedis
   use m_sferic
   use m_missing
   implicit none
   double precision :: X0,Y0,X3,Y3,X1,Y1,X2,Y2,DIS,XN,YN, dx0, dy0
   double precision :: getdx, getdy
   integer :: JA

   CALL DLINEDIS(X0,Y0,X1,Y1,X2,Y2,JA,DIS,XN,YN,jsferic, jasfer3D, dmiss)
   !DX0 = GETDX(X0,Y0,XN,YN)
   !DY0 = GETDY(X0,Y0,XN,YN)
   call getdxdy(X0,Y0,XN,YN,dx0,dy0, jsferic)
   CALL dlinedis(X3,Y3,X1,Y1,X2,Y2,JA,DIS,XN,YN,jsferic, jasfer3D, dmiss)

   XN = 2*XN-X3 + DX0
   YN = 2*YN-Y3 + DY0

   RETURN
   END SUBROUTINE MIRRORLINEPOINT
