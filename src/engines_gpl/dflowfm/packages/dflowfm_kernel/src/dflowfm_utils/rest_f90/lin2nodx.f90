!>    return x-component in node coordinate frame of a vector in link coordinate frame
      double precision function lin2nodx(L,i12,ux,uy)
         use m_flowgeom, only: csb, snb
         use m_sferic
         implicit none

         integer,          intent(in) :: L   !< flowlink number
         integer,          intent(in) :: i12 !< left (1) or right (2) neighboring cell
         double precision, intent(in) :: ux, uy !< vector components in flowlink coordinate frame

         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            lin2nodx = ux
         else
            lin2nodx =  csb(i12,L) * ux - snb(i12,L) * uy
         end if

         return
      end function lin2nodx
