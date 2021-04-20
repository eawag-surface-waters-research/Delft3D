!> debugging subroutine
subroutine inipole(japole)
   use unstruc_model
   use m_flow
   use m_flowgeom
   use m_sferic
   implicit none

   integer, intent(in) :: japole !< pole (1) or equator (0)

   double precision :: lambda, phi, u

   integer :: L

   integer :: ierror

   integer, external :: flow_modelinit

!  set velocity field
   do L=1,Lnx
      lambda = xu(L)*dg2rd
      phi = yu(L)*dg2rd

      if ( japole.eq.1 ) then
         u1(L) = (  sin(phi) * cos(lambda) ) * csu(L) +   &
                 ( -sin(lambda)            ) * snu(L)

         u1(L) = u1(L) / sqrt( sin(phi)**2 * cos(lambda)**2 + sin(lambda)**2 )
      else
         u1(L) = csu(L)
      end if
   end do

   return
end subroutine inipole
