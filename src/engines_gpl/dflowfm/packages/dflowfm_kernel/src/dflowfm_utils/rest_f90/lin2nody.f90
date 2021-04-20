!>    return y-component in node coordinate frame of a vector in link coordinate frame
      double precision function lin2nody(L,i12,ux,uy)
         use m_flowgeom, only: csb, snb
         use m_sferic
         implicit none

         integer,          intent(in) :: L   !< flowlink number
         integer,          intent(in) :: i12 !< left (1) or right (2) neighboring cell
         double precision, intent(in) :: ux, uy !< vector components in flowlink coordinate frame

         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            lin2nody = uy
         else
            lin2nody =  snb(i12,L) * ux + csb(i12,L) * uy
         end if

         return
      end function lin2nody
