!>    return y-component in link coordinate frame of a vector in corner (netnode) coordinate frame
      double precision function cor2liny(L,i12,ux,uy)
         use m_flowgeom, only: csbn, snbn
         use m_sferic
         implicit none

         integer,          intent(in) :: L   !< flowlink number
         integer,          intent(in) :: i12 !< left (1) or right (2) neighboring corner (netnode)
         double precision, intent(in) :: ux, uy !< vector components in corner coordinate frame

         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            cor2liny = uy
         else
            cor2liny = -snbn(i12,L) * ux + csbn(i12,L) * uy
         end if

         return
      end function cor2liny
