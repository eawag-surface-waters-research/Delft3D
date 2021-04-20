! compute coordinates (xu, yu) from (x1,y1) and (x2,y2) with
!    weights alpha1 and alpha2
subroutine a1x1a2x2(x1,y1,x2,y2,alpha1,alpha2,xu,yu)
   use m_sferic
   use geometry_module, only: sphertocart3D, Cart3Dtospher
   implicit none

   double precision, intent(in)  :: x1, y1
   double precision, intent(in)  :: x2, y2
   double precision, intent(in)  :: alpha1
   double precision, intent(in)  :: alpha2
   double precision, intent(out) :: xu, yu

   double precision              :: xx1, yy1, zz1, xx2, yy2, zz2
   double precision              :: xxu, yyu, zzu

   if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
      call sphertoCart3D(x1,y1,xx1,yy1,zz1)
      call sphertoCart3D(x2,y2,xx2,yy2,zz2)
      xxu = alpha1*xx1 + alpha2*xx2
      yyu = alpha1*yy1 + alpha2*yy2
      zzu = alpha1*zz1 + alpha2*zz2
      call Cart3Dtospher(xxu,yyu,zzu,xu,yu,max(x1,x2))
   else
      xu = alpha1*x1 + alpha2*x2
      yu = alpha1*y1 + alpha2*y2
   end if

   return
end subroutine a1x1a2x2
