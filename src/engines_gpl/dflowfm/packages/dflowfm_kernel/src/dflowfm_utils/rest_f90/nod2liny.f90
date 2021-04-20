!>    return y-component in link coordinate frame of a vector in node coordinate frame
      double precision function nod2liny(L,i12,ux,uy)
         use m_flowgeom, only: csb, snb
         use m_sferic
         implicit none

         integer,          intent(in) :: L   !< flowlink number
         integer,          intent(in) :: i12 !< left (1) or right (2) neighboring cell
         double precision, intent(in) :: ux, uy !< vector components in flownode coordinate frame


         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            nod2liny = uy
         else
            nod2liny =  -snb(i12,L) * ux + csb(i12,L) * uy
         end if

         return
      end function nod2liny
