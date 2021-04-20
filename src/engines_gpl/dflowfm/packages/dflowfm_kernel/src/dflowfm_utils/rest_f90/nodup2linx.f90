!>    return x-component in link coordinate frame of vector in "klnup"-node coordinate frame
      double precision function nodup2linx(L,ib,ux,uy)
         use m_flowgeom, only: csbup, snbup
         use m_sferic
         implicit none

         integer,          intent(in) :: L   !< flowlink number
         integer,          intent(in) :: ib !< stencil index  (1 (iup=1), 2 (iup=2), 3 (iup=4), or 4 (iup=5))
         double precision, intent(in) :: ux, uy !< vector components in flownode coordinate frame


         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            nodup2linx = ux
         else
            nodup2linx =  csbup(ib,L) * ux + snbup(ib,L) * uy
         end if

         return
      end function nodup2linx
