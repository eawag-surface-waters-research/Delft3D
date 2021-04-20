!>    return x-component in link coordinate frame of vector in node coordinate frame
      double precision function nod2linx(L,i12,ux,uy)
         use m_flowgeom, only: csb, snb
         use m_sferic
         implicit none

         integer,          intent(in) :: L   !< flowlink number
         integer,          intent(in) :: i12 !< left (1) or right (2) neighboring cell
         double precision, intent(in) :: ux, uy !< vector components in flowlnode coordinate frame

         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            nod2linx = ux
         else
            nod2linx =  csb(i12,L) * ux + snb(i12,L) * uy
         end if

         return
      end function nod2linx
