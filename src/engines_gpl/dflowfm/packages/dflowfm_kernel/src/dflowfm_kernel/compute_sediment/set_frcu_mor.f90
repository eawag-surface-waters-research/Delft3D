subroutine set_frcu_mor(dim)
    use m_flow, only: frcu, frcu_mor
    use m_flowgeom, only: lnx, lnxi, lnx1d, lnx1Db
    integer, intent(in) :: dim

    integer :: L

    if (dim == 1) then
       do L = 1, lnx1d  ! 1D int
           frcu_mor(L) = frcu(L)
       enddo
       do L = lnxi+1, lnx1Db ! 1D bnd
           frcu_mor(L) = frcu(L)
       enddo
    endif
    if (dim == 2) then
       do L = lnx1d+1, lnxi ! 2D int
           frcu_mor(L) = frcu(L)
       enddo
       do L = lnx1Db+1, lnx ! 2D bnd
           frcu_mor(L) = frcu(L)
       enddo
    endif
end subroutine set_frcu_mor
