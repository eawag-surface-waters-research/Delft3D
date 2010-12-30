subroutine z_difuflux(stage   ,lundia    ,kmax      ,nmmax     ,nmmaxj    , &
                  & lstsci    ,r0        ,qxk       ,qyk       , &
                  & u         ,v         ,&
                  & dicuv     ,guv       ,gvu       ,areau     ,areav     , &
                  & kfuz1     ,kfvz1     ,kfsz1     ,kcs       ,kfs       , &
                  & kfumin    ,kfumx0    ,kfvmin    ,kfvmx0    ,sigdif    , &
                  & timest    ,icx       ,icy       ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!!--description-----------------------------------------------------------------
!
!    Function: Compute flux corresponding to Z_DIFU
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                             , pointer :: lsed
    real(fp) , dimension(:,:,:)         , pointer :: fluxu
    real(fp) , dimension(:,:,:)         , pointer :: fluxuc
    real(fp) , dimension(:,:,:)         , pointer :: fluxv
    real(fp) , dimension(:,:,:)         , pointer :: fluxvc
    type (flwoutputtype)                , pointer :: flwoutput
    type (gd_flwpar)                    , pointer :: gdflwpar
!
! Global variables
!
    integer                                                       , intent(in) :: icx    !  Description and declaration in inout.igs
    integer                                                       , intent(in) :: icy    !  Description and declaration in inout.igs
    integer                                                       , intent(in) :: kmax   !  Description and declaration in inout.igs
    integer                                                       , intent(in) :: nmmax  !  Description and declaration in inout.igs
    integer                                                       , intent(in) :: nmmaxj !  Description and declaration in inout.igs
    integer                                                       , intent(in) :: lstsci !  Description and declaration in inout.igs
    integer                                                       , intent(in) :: lundia !  Description and declaration in inout.igs
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: kcs    !  Description and declaration in iidim.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: kfumin !  Description and declaration in iidim.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: kfumx0 !  Description and declaration in iidim.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: kfvmin !  Description and declaration in iidim.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: kfvmx0 !  Description and declaration in iidim.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: kfs    !  Description and declaration in iidim.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: kfsz1  !  Description and declaration in iidim.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: kfuz1  !  Description and declaration in iidim.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: kfvz1  !  Description and declaration in iidim.f90
    real(fp)                                                      , intent(in) :: timest !!  Half Integration time step [sec.]
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: areau
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: areav
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: dicuv  !  Description and declaration in rjdim.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: guv    !  Description and declaration in rjdim.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: gvu    !  Description and declaration in rjdim.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: qxk    !  Description and declaration in rjdim.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: qyk    !  Description and declaration in rjdim.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci) , intent(in) :: r0     !  Description and declaration in rjdim.f90
    real(fp)     , dimension(lstsci)                              , intent(in) :: sigdif !  Description and declaration in rjdim.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: u
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: v
    character(8)                                                  , intent(in) :: stage  !!  First or second half time step
!
! Local variables
!
    integer  :: istat
    integer  :: k
    integer  :: l
    integer  :: kenu, kenv
    integer  :: kmin
    integer  :: ndm
    integer  :: nm
    integer  :: nmd
    integer  :: nmu
    integer  :: nmuu
    integer  :: num
    integer  :: nuum
    real(fp) :: cl, cr, difl, difr
    real(fp) :: cfl
    real(fp) :: flux
    real(fp) :: r00
    real(fp) :: rmax
    real(fp) :: rmin

!
!! executable statements -------------------------------------------------------
!
    lsed           => gdp%d%lsed
    fluxu          => gdp%gdflwpar%fluxu
    fluxuc         => gdp%gdflwpar%fluxuc
    fluxv          => gdp%gdflwpar%fluxv
    fluxvc         => gdp%gdflwpar%fluxvc
    flwoutput      => gdp%gdflwpar%flwoutput
    gdflwpar       => gdp%gdflwpar
    !
    if (.not. flwoutput%difuflux .and. lsed == 0) return
    !
    istat = 0
    if (.not. associated(gdflwpar%fluxu)) then
       if (istat==0) allocate (gdflwpar%fluxu(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), stat = istat)
       if (istat==0) allocate (gdflwpar%fluxv(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), stat = istat)
       if (flwoutput%cumdifuflux) then
          if (istat==0) allocate (gdflwpar%fluxuc(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), stat = istat)
          if (istat==0) allocate (gdflwpar%fluxvc(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), stat = istat)
       endif
       !
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'Z_DIFUFLUX: memory alloc error')
          call d3stop(1, gdp)
       endif
       !
       ! Define GDP pointers after the GDP allocations
       !
       fluxu          => gdp%gdflwpar%fluxu
       fluxuc         => gdp%gdflwpar%fluxuc
       fluxv          => gdp%gdflwpar%fluxv
       fluxvc         => gdp%gdflwpar%fluxvc
       flwoutput      => gdp%gdflwpar%flwoutput
       gdflwpar       => gdp%gdflwpar
       !
       if (flwoutput%cumdifuflux) then
          fluxuc = 0.0_fp
          fluxvc = 0.0_fp
       endif
    endif
    !
    ! Initialization
    !
    fluxu = 0.0_fp
    fluxv = 0.0_fp
    !
    do nm = 1, nmmax
       !
       ! Advective transport in X-direction
       !
       nmd  = nm  - icx
       nmu  = nm  + icx
       nmuu = nmu + icx
       ndm  = nm  - icy
       num  = nm  + icy
       nuum = num + icy
       !
       if (kfs(nm)*kfs(nmu) /= 0) then
          kmin = max(kfumin(nm), 1)
          do k = kmin, kmax
             cfl = u(nm, k)*timest/gvu(nm)
             if (qxk(nm, k) > 0.0_fp) then
                do l = 1, lstsci
                   rmax = max(r0(nmd, k, l), r0(nmu, k, l))
                   rmin = min(r0(nmd, k, l), r0(nmu, k, l))
                   if ( kfsz1(nmd, k)*kfsz1(nmu, k) == 0 &
                     & .or. r0(nm, k, l) >= rmax         &
                     & .or. r0(nm, k, l) <  rmin         ) then
                      r00 = r0(nm, k, l)
                   else
                      r00 = r0(nm , k, l) + (1.0_fp - cfl) &
                        & *(r0(nm , k, l) - r0(nmd, k, l)) &
                        & *(r0(nmu, k, l) - r0(nm , k, l)) &
                        & /(r0(nmu, k, l) - r0(nmd, k, l))
                   endif
                   flux = qxk(nm, k) * r00
                   if (kcs(nm) >= 1) then
                      fluxu(nm, k, l) = fluxu(nm, k, l) + flux
                   endif
                enddo
             else
                do l = 1, lstsci
                   rmax = max(r0(nm, k, l), r0(nmuu, k, l))
                   rmin = min(r0(nm, k, l), r0(nmuu, k, l))
                   if (kfsz1(nm, k)*kfsz1(nmuu, k) == 0 &
                     & .or. r0(nmu, k, l) >= rmax       &
                     & .or. r0(nmu, k, l) <  rmin       ) then
                      r00 = r0(nmu, k, l)
                   else
                      r00 = r0(nmu, k, l) + (1.0_fp + cfl)  &
                        & *(r0(nm , k, l) - r0(nmu , k, l)) &
                        & *(r0(nmu, k, l) - r0(nmuu, k, l)) &
                        & /(r0(nm , k, l) - r0(nmuu, k, l))
                   endif
                   flux = qxk(nm, k) * r00
                   if (kcs(nm) >= 1) then
                      fluxu(nm, k, l) = fluxu(nm, k, l) + flux
                   endif
                enddo
             endif
          enddo
       endif
       !
       do l = 1, lstsci
          do k = kfumin(nm), kfumx0(nm)
             if (kfuz1(nm, k) == 1 .and. kfsz1(nm, k)*kfsz1(nmu, k) /= 0) then
                cl   = r0(nm, k, l)
                difl = dicuv(nm, k)
                cr   = r0(nmu, k, l)
                difr = dicuv(nmu, k)
                flux = 0.5_fp*(cr - cl)*(difl + difr)/(sigdif(l)*gvu(nm))
                kenu = max(0, 2 - kcs(nmu))
                fluxu(nm, k, l) = fluxu(nm, k, l) - areau(nm, k)*flux*kenu
             endif
          enddo
       enddo
       !
       if (kfs(nm)*kfs(num) /= 0) then
          kmin = max(kfvmin(nm), 1)
          do k = kmin, kmax
             cfl = v(nm, k)*timest/guv(nm)
             if (qyk(nm, k) > 0.0_fp) then
                do l = 1, lstsci
                   rmax = max(r0(ndm, k, l), r0(num, k, l))
                   rmin = min(r0(ndm, k, l), r0(num, k, l))
                   if (kfsz1(ndm, k)*kfsz1(num, k) == 0 &
                     & .or. r0(nm, k, l) >= rmax        &
                     & .or. r0(nm, k, l) <  rmin        ) then
                      r00 = r0(nm, k, l)
                   else
                      r00 = r0(nm , k, l) + (1.0_fp - cfl) &
                        & *(r0(nm , k, l) - r0(ndm, k, l)) &
                        & *(r0(num, k, l) - r0(nm , k, l)) &
                        & /(r0(num, k, l) - r0(ndm, k, l))
                   endif
                   flux = qyk(nm, k) * r00
                   if (kcs(nm) >= 1) then
                      fluxv(nm, k, l) = fluxv(nm, k, l) + flux
                   endif
                enddo
             else
                do l = 1, lstsci
                   rmax = max(r0(nm, k, l), r0(nuum, k, l))
                   rmin = min(r0(nm, k, l), r0(nuum, k, l))
                   if (kfsz1(nm, k)*kfsz1(nuum, k) == 0 &
                     & .or. r0(num, k, l) >= rmax       & 
                     & .or. r0(num, k, l) <  rmin       ) then
                      r00 = r0(num, k, l)
                   else
                      r00 = r0(num, k, l) + (1.0_fp + cfl)  &
                        & *(r0(nm , k, l) - r0(num , k, l)) &
                        & *(r0(num, k, l) - r0(nuum, k, l)) &
                        & /(r0(nm , k, l) - r0(nuum, k, l))
                   endif
                   flux = qyk(nm, k) * r00
                   if (kcs(nm) >= 1) then
                      fluxv(nm, k, l) = fluxv(nm, k, l) + flux
                   endif
                enddo
             endif
          enddo
       endif
       !
       do l = 1, lstsci
          do k = kfvmin(nm), kfvmx0(nm)
             if (kfvz1(nm, k) == 1 .and. kfsz1(nm, k)*kfsz1(num, k) /= 0) then
                cl   = r0(nm, k, l)
                difl = dicuv(nm, k)
                cr   = r0(num, k, l)
                difr = dicuv(num, k)
                flux = 0.5_fp*(cr - cl)*(difl + difr)/(sigdif(l)*guv(nm))
                kenv = max(0, 2 - kcs(num))
                fluxv(nm, k, l) = fluxv(nm, k, l) - areav(nm, k)*flux*kenv
             endif
          enddo
       enddo
    enddo
    !
    ! Cumulative flux
    !
    if (flwoutput%cumdifuflux) then
       if (icx > 1) then
         fluxuc = fluxuc + fluxu * timest
         fluxvc = fluxvc + fluxv * timest
       else
         fluxuc = fluxuc + fluxv * timest
         fluxvc = fluxvc + fluxu * timest
       endif
    endif
end subroutine z_difuflux
