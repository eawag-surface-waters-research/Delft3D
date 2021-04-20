!> Constructs the set of crossed flow links for a single path on the
!! current *network* geometry. (Used for thin dams.)
!!
!! Input is a path with path coordinates in xp,yp.
!! Output path contains additional link numbers in kn and edge
!! coordinates in xk,yk.
!!
!! \see crspath_on_flowgeom, thindams_on_netgeom
subroutine crspath_on_netgeom(path)
    use m_crspath
    use network_data
    implicit none
    type(tcrspath), intent(inout) :: path !< Cross section path that must be imposed on network geometry.

       integer                       :: L, isactive
    double precision :: xza, yza, xzb, yzb

    path%lnx = 0 ! Reset link administration for this path.

!   Loop across all net links
    do L = 1,numl
           call get_link_neighboringcellcoords(L, isactive, xza, yza, xzb, yzb)
           if ( isactive.ne.1 ) cycle

           call crspath_on_singlelink(path, L, xk(kn(1,L)), yk(kn(1,L)), xk(kn(2,L)), yk(kn(2,L)), xza, yza, xzb, yzb, 1)
       enddo
   end subroutine crspath_on_netgeom
