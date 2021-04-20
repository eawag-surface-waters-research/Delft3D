 !subroutine wricir()
 !use m_sferic
 !use m_flow
 !use m_flowgeom
 !implicit none
 !integer :: mout, k
 !double precision :: phi, r0
 !
 !return
 !call inisferic()
 !call newfil(mout,'circ250.ldb')
 !write(mout,'(a)') 'L001'
 !write(mout,'(a)') '360 2'
 !r0 = 125000d0
 !do k = 0,360
 !   phi = dg2rd*k
 !   write(mout,*) r0*cos(phi), r0*sin(phi), r0, r0
 !enddo
 !call doclose(mout)
 !
 !end subroutine wricir
 subroutine zerowaterdepth()                         ! restart without water
 use m_flow
 use m_flowgeom
 implicit none
 s0 = bl
 s1 = bl
 u0 = 0d0
 u1 = 0d0
 end subroutine zerowaterdepth
