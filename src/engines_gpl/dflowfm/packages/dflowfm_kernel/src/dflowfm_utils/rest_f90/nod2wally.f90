!>    return x-component in link coordinate frame of vector in wall coordinate frame
      double precision function nod2wally(nw,ux,uy)
         use m_flowgeom, only: csbwn, snbwn
         use m_sferic
         implicit none

         integer,          intent(in) :: nw  !< wall element number
         double precision, intent(in) :: ux, uy !< vector components in wall coordinate frame

         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            nod2wally = uy
         else
            nod2wally = -snbwn(nw) * ux + csbwn(nw) * uy
         end if

         return
      end function nod2wally
