   subroutine dumpstations(name)
   use m_observations
   use m_flow
   use m_flowgeom

   implicit none
   integer :: mhis2, n, k, L1

   character (len=*) :: name

   L1 = index('.',name)
   call newfil(mhis2, trim(name(1:L1))//'stat' )

   do n = 1,numobs
      k = kobs(n)
      write(mhis2,'(6f16.6,2x,A)' ) xobs(n), yobs(n) , smxobs(n) , cmxobs(n), bl(k), ba(k), trim(namobs(n))
   enddo
   write(mhis2,*) ' '

   do n = 1,numobs
      k = kobs(n)
      write(mhis2,*) s1(k)
   enddo
   write(mhis2,*) ' '

   do n = 1,numobs
      k = kobs(n)
      write(mhis2,*) sqrt( ucx(k)*ucx(k) + ucy(k)*ucy(k) )
   enddo


   call doclose(mhis2)
   end subroutine dumpstations
