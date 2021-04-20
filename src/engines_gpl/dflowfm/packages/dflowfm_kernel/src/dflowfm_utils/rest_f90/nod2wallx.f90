!>    return x-component in link coordinate frame of vector in wall coordinate frame
      double precision function nod2wallx(nw,ux,uy)
         use m_flowgeom, only: csbwn, snbwn
         use m_sferic
         implicit none

         integer,          intent(in) :: nw  !< wall element number
         double precision, intent(in) :: ux, uy !< vector components in wall coordinate frame

         integer                      :: L

         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            nod2wallx = ux
         else
            nod2wallx =  csbwn(nw) * ux + snbwn(nw) * uy
         end if

         return
      end function nod2wallx
