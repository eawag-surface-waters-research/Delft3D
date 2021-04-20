! =================================================================================================
! =================================================================================================
   subroutine setstruclink()

      use m_flow
      implicit none
      integer          :: i, n, L, Lf, La
      !
      ! === Gates (old)
      !
      do n = 1,ngatesg
         do L = L1gatesg(n), L2gatesg(n)
            Lf = kgate(3,L)
            La = abs( Lf )
            struclink(La) = 1
         enddo
      enddo
      !
      ! === Gates (new)
      !
      do n = 1,ngategen
         i = gate2cgen(n)
         do L = L1cgensg(i), L2cgensg(i)
            Lf = kcgen(3,L)
            La = abs( Lf )
            struclink(La) = 1
         enddo
      enddo
      !
      ! === General structures (old)
      !
      do n = 1,ncgensg
         i = n
         do L = L1cgensg(i),L2cgensg(i)
            Lf = kcgen(3,L)
            La = abs( Lf )
            struclink(La) = 1
         enddo
      enddo
      !
      ! === General structures (new)
      !
      do n = 1,ngenstru
         i = genstru2cgen(n)
         do L = L1cgensg(i),L2cgensg(i)
            Lf = kcgen(3,L)
            La = abs( Lf )
            struclink(La) = 1
         enddo
      enddo
      !
      ! === Weirs
      !
      do n = 1,nweirgen
         i = weir2cgen(n)
         do L = L1cgensg(i),L2cgensg(i)
            Lf = kcgen(3,L)
            La = abs( Lf )
            struclink(La) = 1
         enddo
      enddo

   end subroutine setstruclink
