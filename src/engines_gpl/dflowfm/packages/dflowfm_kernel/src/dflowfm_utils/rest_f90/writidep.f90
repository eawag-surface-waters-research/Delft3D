!> output tide potential as samples
subroutine writidep(time)
   use m_flowgeom
   use m_flow
   use m_partitioninfo
   use m_samples
   implicit none

   double precision, intent(in) :: time

   character(len=256)           :: dateandtime

   integer                      :: k
   integer                      :: jaRestoreSam
   integer                      :: itid

   dateandtime = '_'
   call maketime(dateandtime(2:), time)

   if ( jampi.eq.0 ) then
      call newfil(itid, 'tide_potential' // trim(dateandtime) // '.xyz')
   else
      call newfil(itid, 'tide_potential' // trim(dateandtime) // '_' // trim(sdmn) // '.xyz')
   end if

   jaRestoreSam = 0
   if ( Ns.gt.0 ) then
      call savesam()
      jaRestoreSam = 1
   end if

   call increasesam(Ndx)
   NS = Ndx

   do k=1,Ndx
      xs(k) = xz(k)
      ys(k) = yz(k)
      zs(k) = tidep(1,k)
   end do

   call wrisam(itid)

   call doclose(itid)

   if ( jaRestoreSam.eq.1 ) then
      call restoresam()
   else
      call delsam(0)
   end if

   return
end subroutine writidep
