!> draw particles in GUI
subroutine tekpart()
   use m_particles
   use m_wearelt
   use unstruc_display
   use m_sferic, only: jsferic
   use m_missing, only: dmiss
   use geometry_module, only : cart3Dtospher
   implicit none

   double precision :: x, y

   integer :: i

   if ( Npart.lt.1 .or. ndrawpart.eq.1 ) return

!  safety check
   if ( jsferic.eq.1 .and. .not.(allocated(zpart)) ) then
      call dealloc_partmesh()
      call dealloc_partfluxes()
      call dealloc_partrecons()
      call dealloc_particles()
      call dealloc_auxfluxes()
      call dealloc_partparallel()
      japart = 0
      return
   end if

   call setcol(31)

   if ( jsferic.eq.0 ) then
      do i=1,Npart
         if ( kpart(i).eq.0 ) cycle
         call movabs(xpart(i),ypart(i))
         call cir(rcir)
      end do
   else
       do i=1,Npart
         if ( kpart(i).eq.0 ) cycle
         call Cart3Dtospher(xpart(i),ypart(i),zpart(i),x,y,0d0)
         call movabs(x,y)
         call cir(rcir)
      end do
   end if

   return
end subroutine tekpart
