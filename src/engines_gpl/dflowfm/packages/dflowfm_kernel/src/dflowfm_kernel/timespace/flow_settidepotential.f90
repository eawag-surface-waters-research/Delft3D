 subroutine flow_settidepotential(timmin)
 use m_flow
 use m_flowgeom
 use m_flowtimes
 use timespace_data
 use m_sferic
 use unstruc_model
 use m_equatorial

 implicit none

 double precision :: timmin

 integer, save    :: ini = 0
 integer          :: ierr, kk

 double precision :: Omeg, tt

 call meteo_tidepotential( julrefdat, TIMmin , xz , yz , size(tidep,1), tidep, ndx, doodsonstart, doodsonstop , doodsoneps)

 if (md_ident == 'equator1d' ) then
    tt   = 60d0*(timmin-tstart_user)
    do kk = 1,ndx
       tidep(1,kk) = ZP*sin(om*tt - nmode*dg2rd*xz(kk) )
    enddo
 endif

 end subroutine flow_settidepotential
