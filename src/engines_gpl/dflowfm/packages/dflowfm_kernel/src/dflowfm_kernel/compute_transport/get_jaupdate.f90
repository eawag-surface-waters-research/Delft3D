!> determine if the cells have to be updated (1) or not (0)
subroutine get_jaupdate(istep,nsubsteps,Ndxi,Ndx,ndeltasteps,jaupdate)
   use timers

   implicit none

   integer,                  intent(in)  :: istep        !< substep number
   integer,                  intent(in)  :: nsubsteps    !< number of substeps
   integer,                  intent(in)  :: Ndxi         !< number of cells, excluding virtual boundary cells
   integer,                  intent(in)  :: Ndx          !< number of cells, including virtual boundary cells
   integer, dimension(Ndx),  intent(in)  :: ndeltasteps  !< number of substeps between updates
   integer, dimension(Ndx),  intent(out) :: jaupdate     !< update cell (1) or not (0)

   integer                               :: kk
   integer                               :: num

   integer(4) ithndl /0/
   if (timon) call timstrt ( "get_jaupdate", ithndl )

   jaupdate = 0

   num = 0
   do kk=1,Ndxi
      if ( mod(istep+1, ndeltasteps(kk)).eq.0 ) then
!      if ( int((istep+1)/ndeltasteps(kk))*ndeltasteps(kk).eq.istep+1 ) then
          jaupdate(kk) = 1
          num = num+1
      end if
   end do

!  BEGIN DEBUG
!   jaupdate = 1
!  END DEBUG

!   if ( istep.lt.nsubsteps ) then
!      write(6,"(I0,':',I0, ' ', $)") istep+1, num
!   end if

   if (timon) call timstop( ithndl )
   return
end subroutine get_jaupdate
