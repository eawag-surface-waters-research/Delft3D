      SUBROUTINE WRIswan(MNET,filnam)

      use m_netw
      use m_polygon
      use m_missing
      use geometry_module, only: dbdistance, cross, normaloutchk
      use m_sferic, only: jsferic, jasfer3D
      use gridoperations

      implicit none

      integer          :: MNET
      character(len=*) :: filnam

      double precision            :: xz2, yz2, dl, xn, yn, sl, sm, crp, xcr, ycr
      integer                     :: k, L, n, kk, ja, k1, k2, k3, k4, jacros, lin

      call savepol()
      npl = 0

      ja = 0
      call triangulate_quadsandmore(ja)

      if (ja == 1) then
         call findcells(3) ! search triangles again
      endif

      call restorepol()

      kc   = 0                 ! binnenpunten
      do L = 1, numl

         if (lnn(L) == 1) then ! dichte randen
            k3  = kn (1,L)
            k4  = kn (2,L)
            kc(k3) = 1 ; kc(k4) = 1
         endif
      enddo

      do L = 1, numl

         if (lnn(L) == 1) then
            k1  = lne(1,L)
            k3  = kn (1,L)
            k4  = kn (2,L)
            dl  = dbdistance( xk(k3), yk(k3), xk(k4), yk(k4), jsferic, jasfer3D, dmiss)
            call normaloutchk(xk(k3), yk(k3), xk(k4), yk(k4), xzw(n), yzw(n), xn, yn, ja, jsferic, jasfer3D, dmiss, dxymis)

            xz2 = xzw(n) + dl*xn ; yz2 = yzw(n) + dl*yn

            lin = 2
            do n   = 1,npl-1
               if (xpl(n) .ne. dmiss) then
                  if (xpl(n+1) .ne. dmiss) then
                     call cross(xpl(n), ypl(n), xpl(n+1), ypl(n+1), xzw(n), yzw(n), xz2, yz2, JACROS,SL,SM,XCR,YCR,CRP,jsferic, dmiss)
                     if (jacros == 1) then
                         kc(k3) = lin ; kc(k4) = lin   ! open door polygon
                     endif
                  else
                     lin = lin + 1
                  endif
               endif
            enddo

         endif

      enddo

      write(mNET,'(I12,A)') numk, '  2  0  1'
      DO K = 1, NUMK
        WRITE(MNET,'(i12, 2F16.5, i3)') k, XK(K), YK(K), kc(k)
      ENDDO
      call doclose(mnet)

      L = index(filnam,'.')
      call newfil(mnet, filnam(1:L)//'nodz')
      write(mNET,'(I12,A)') numk, '  1  0  1'
      DO K = 1, NUMK
        WRITE(MNET,'(F16.5)') ZK(K)
      ENDDO
      call doclose(mnet)



      call newfil(mnet, filnam(1:L)//'ele')
      write(mNET,'(I12,A)') nump, '  3  0'
      do k = 1,nump
         WRITE(MNET,'(4i12)') k, (netcell(k)%nod(kk),kk=1,3)
      enddo
      CALL DOCLOSE(MNET)




      RETURN
      END SUBROUTINE WRIswan
