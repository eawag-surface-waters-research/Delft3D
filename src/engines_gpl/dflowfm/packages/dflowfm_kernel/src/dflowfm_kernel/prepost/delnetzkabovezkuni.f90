  subroutine delnetzkabovezkuni()

  use m_netw
  USE M_MISSING
  use gridoperations

  implicit none
  integer :: k, kk, L, k2, jaweg

  do k = 1,numk
     if (zk(k) .ne. dmiss) then
        if (zk(k) > zkuni) then
            jaweg = 0
            do kk = 1,nmk(k)
               L  = nod(k)%lin(kk)
               k2 = kn(1,L) + kn(2,L) - k
               if (zk(k2) > zkuni .or. zk(k2) == dmiss) then
                  jaweg = jaweg + 1
               endif
            enddo
            if (jaweg == nmk(k) ) then
               xk(k) = dmiss ; yk(k) = dmiss; zk(k) = dmiss
            endif

        endif
     else if (zk(k) == dmiss) then
        xk(k) = dmiss ; yk(k) = dmiss; zk(k) = dmiss
     endif
  enddo

  call setnodadm(0)

  end subroutine delnetzkabovezkuni
