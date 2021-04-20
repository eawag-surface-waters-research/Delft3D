!> get neighboring cell center coordinates
  subroutine get_link_neighboringcellcoords(L, isactive, xza, yza, xzb, yzb)
     use network_data
     use m_flowgeom, only: xz, yz ! Note that xz,yz are already filled after findcells.
     implicit none

     integer,          intent(in)  :: L                  !< link number
     integer,          intent(out) :: isactive           !< active link (1) or not (0)
     double precision, intent(out) :: xza, yza, xzb, yzb !< left- and right-neighboring cell centers

     integer                       :: n1, n2

     isactive = 1

        if (kn(3,L) == 1 .or. kn(3,L) == 3 .or. kn(3,L) == 4) then
            n1 = kn(1,L)
            n2 = kn(2,L)
            xza = xk(n1) ; yza = yk(n1)
            xzb = xk(n2) ; yzb = yk(n2)
        else
            n1 = lne(1,L); n2 = lne(2,L)
            if (lnn(L) < 2 .or. n1 <= 0 .or. n2 <= 0 .or. n1 > nump .or. n2 > nump) then
               isactive = 0
               return
            end if
            xza = xz(n1) ; yza = yz(n1)
            xzb = xz(n2) ; yzb = yz(n2)
        end if

     return
  end subroutine get_link_neighboringcellcoords
