!>    return x-component in link coordinate frame of vector in wall coordinate frame
      double precision function wall2linx(nw,i12,ux,uy)
         use m_flowgeom, only: csbw, snbw
         use m_sferic
         implicit none

         integer,          intent(in) :: nw  !< wall element number
         integer,          intent(in) :: i12 !< left (1) or right (2) attached flowlink
         double precision, intent(in) :: ux, uy !< vector components in wall coordinate frame

         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            wall2linx = ux
         else
            wall2linx =  csbw(i12,nw) * ux - snbw(i12,nw) * uy
         end if

         return
      end function wall2linx
