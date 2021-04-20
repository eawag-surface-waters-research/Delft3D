!>    return x-component in corner (netnode) coordinate frame of a vector in link coordinate frame
      double precision function lin2cory(L,i12,ux,uy)
         use m_flowgeom, only: csbn, snbn
         use m_sferic
         implicit none

         integer,          intent(in) :: L   !< flowlink number
         integer,          intent(in) :: i12 !< left (1) or right (2) corner (netnode)
         double precision, intent(in) :: ux, uy !< vector components in flowlink coordinate frame

         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            lin2cory = uy
         else
            lin2cory =  snbn(i12,L) * ux + csbn(i12,L) * uy
         end if

         return
      end function lin2cory
