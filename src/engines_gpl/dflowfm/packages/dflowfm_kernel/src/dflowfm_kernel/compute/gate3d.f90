 subroutine gate3D(jazerou1)
 use m_flow
 use m_flowgeom
 use m_flowexternalforcings
 implicit none
 integer          :: jazerou1 ! 1 set u1 zero above gate
 integer          :: L, LL, Lb, Lt, ng, n, k, kb, kt
 double precision :: zga, bup, zLu, fac

 do ng = 1, ngatesg
    zga   = zgate(ng)
    do n  = L1gatesg(ng), L2gatesg(ng)
       LL = kgate(3,n)
       if (hu(LL) > 0d0) then
         bup = 0.5d0*( bob(1,LL) + bob(2,LL) )

         call getLbotLtop(LL,Lb,Lt)
         do L   = Lb, Lt
            ZLu = bup + hu(L-1)
            fac =  (zga-zLu) / ( hu(L) - hu(L-1) )
            if (fac < 0.1d0) then
               Ltop(LL) = L - 1 ; exit
            else
               fac   = max(0d0, min(1d0, fac ) )
            endif
            hu(L) = hu(L-1) + fac*( hu(L) - hu(l-1) )
            au(L) = au(L)*fac
         enddo
         au( Ltop(LL)+1 : Lbot(LL)+kmxL(LL)-1 ) = 0d0  ! -12346d0 ! 6 not 5

      endif
   enddo
 enddo
 end subroutine gate3D
