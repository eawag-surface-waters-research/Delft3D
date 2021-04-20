!> Reorders a 1D array obsArray in valobs, so that values on inactive layers get fillValue. For sigma-layer model, obsArray are actually not changed.
subroutine reorder_valobs_array(n,obsArray, kb, kt, nlayb, fillValue)
   use m_alloc
   implicit none
   integer,                        intent(in   ) :: n        !< Size of this 1D array
   double precision, dimension(n), intent(inout) :: obsArray !< The 1D array from valobs
   integer,                        intent(in   ) :: kb       !< Index on bottom active layer
   integer,                        intent(in   ) :: kt       !< Index on top active layer
   integer,                        intent(in   ) :: nlayb    !< Layer number for the bottom layer (in 1:kmx)
   double precision,               intent(in   ) :: fillValue!< Fill value for obsArray

   double precision, allocatable :: tmpArray(:)
   integer                       :: k, kk, klay

   if (kt < kb-1) then
      return
   end if

   call realloc(tmpArray, n)

   ! copy obsArray to tmpArray
   do k = 1, n
      tmpArray(k) = obsArray(k)
   end do

   ! reset obsArray to fillValue
   obsArray = fillValue

   ! set values in correct order in obsArray
   k = 1
   do kk = kb-1, kt
      klay = kk - kb + nlayb+1
      obsArray(klay) = tmpArray(k)
      k = k + 1
   enddo

end subroutine reorder_valobs_array
