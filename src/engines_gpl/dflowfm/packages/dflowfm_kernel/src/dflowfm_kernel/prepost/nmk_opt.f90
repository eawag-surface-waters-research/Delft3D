!>  determine optimum nmk in fliplinks, depending on link L
integer function nmk_opt(k)
   use m_netw

   implicit none

   integer, intent(in) :: k            !< node number

!  default value
   nmk_opt = 6

   if( nb(k) .eq. 2 ) nmk_opt = 4
   if( nb(k) .eq. 3 ) nmk_opt = 3

   return
end function nmk_opt
