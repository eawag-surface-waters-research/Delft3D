 subroutine linkstocentercartcomp(knod,vlin,vnod)
    use m_flow
    use m_netw
    use m_flowgeom

    implicit none

    integer, intent(in)                :: knod
    double precision, intent(in)       :: vlin(lnkx)
    double precision, intent(out)      :: vnod(2,max(kmx,1))

    integer                            :: L, k1, k2, k3, LL, LLL, Lb, Lt, kb, kt, k

    vnod = 0d0
    if (kmx == 0) then
       do L = 1, nd(knod)%lnx
          LL = iabs(L)
          k1 = ln(1,LL); k2 = ln(2,LL)
          if (k1==knod) then
             vnod(1,1) = vnod(1,1) + vlin(LL)*wcx1(LL)
             vnod(2,1) = vnod(2,1) + vlin(LL)*wcy1(LL)
          endif
          if (k2==knod) then
             vnod(1,1) = vnod(1,1) + vlin(LL)*wcx2(LL)
             vnod(2,1) = vnod(2,1) + vlin(LL)*wcy2(LL)
          endif
       enddo

    else
       do L = 1, nd(knod)%lnx
          LL = iabs(nd(knod)%ln(L))
          k1  = ln(1,LL) ; k2 = ln(2,LL)
          if (k1==knod) then
             call getLbotLtop(LL,Lb,Lt)
             if (Lt<Lb) cycle
             do LLL = Lb, Lt
                k3=LLL-Lb+1
                vnod(1,k3) = vnod(1,k3) + vlin(LLL)*wcx1(LL)
                vnod(2,k3) = vnod(2,k3) + vlin(LLL)*wcy1(LL)
             enddo
          endif
          !
          if (k2==knod) then
             call getLbotLtop(LL,Lb,Lt)
             if (Lt<Lb) cycle
             do LLL = Lb, Lt
                k3=LLL-Lb+1
                vnod(1,k3) = vnod(1,k3) + vlin(LLL)*wcx2(LL)
                vnod(2,k3) = vnod(2,k3) + vlin(LLL)*wcy2(LL)
             enddo
          endif
       enddo
    endif

 end subroutine linkstocentercartcomp
