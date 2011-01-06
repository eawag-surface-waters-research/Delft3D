subroutine z_checku(j         ,nmmaxj    ,nmmax     ,icx       ,kmax      , &
                  & flood     , &
                  & kfu       ,kcs       ,kcu       ,kspu      , &
                  & kfuz1     ,kfumin    ,kfumax    ,kfumx0    ,hu        , &
                  & s1        ,dpu       ,dps       ,umean     ,u0        , &
                  & u1        ,dzu1      ,zk        ,gdp       )
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
!  $Id$
!  $HeadURL$
!!--description-----------------------------------------------------------------
!
!    Function: This routine checks the drying and flooding at ve-
!              locity points and sets the value of the mask
!              arrays to zero.
!              Always upwind-approach for wet cross section.
!
! Method used: Upwind-approach for wet cross section.
!              Fixed layer approach
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
    real(fp)     , pointer :: dryflc
    real(fp)     , pointer :: dzmin
    real(fp)     , pointer :: zbot
    logical      , pointer :: zmodel
    logical      , pointer :: nonhyd
    integer      , pointer :: nh_level
!
! Global variables
!
    integer                                                         :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                              !!  then computation proceeds in the X-
                                                                              !!  dir. If icx=1 then computation pro-
                                                                              !!  ceeds in the Y-dir.
    integer                                                         :: j      !!  Begin pointer for arrays which have
                                                                              !!  been transformed into 1D arrays.
                                                                              !!  Due to the shift in the 2nd (M-)
                                                                              !!  index, J = -2*NMAX + 1
    integer                                                         :: kmax   !  Description and declaration in iidim.f90
    integer                                                         :: nmmax  !  Description and declaration in dimens.igs
    integer                                                         :: nmmaxj !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcs    !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcu    !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfu    !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfumax !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfumin !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: kfumx0 !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)            :: kspu   !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: kfuz1  !  Description and declaration in iidim.f90
    logical                                           , intent(in)  :: flood  !!  Flag for activating flooding part of
                                                                              !!  checku subroutine
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: dps    !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: dpu    !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: hu     !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: s1     !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: umean  !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: dzu1   !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: u0     !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: u1     !  Description and declaration in rjdim.f90
    real(fp)  , dimension(0:kmax)                     , intent(in)  :: zk
!
! Local variables
!
    integer :: k
    integer :: nm
    integer :: nmd
    integer :: nmu
    real(fp):: dzutot
    real(fp):: htrsh
    real(fp):: s1u
    real(fp):: trsh
    real(fp):: zkk
    logical :: found
!
!! executable statements -------------------------------------------------------
!
    dzmin    => gdp%gdzmodel%dzmin
    zbot     => gdp%gdzmodel%zbot
    dryflc   => gdp%gdnumeco%dryflc
    zmodel   => gdp%gdprocs%zmodel
    nonhyd   => gdp%gdprocs%nonhyd
    nh_level => gdp%gdnonhyd%nh_level
    !
    htrsh = 0.5 * dryflc
    trsh  = dryflc
    !
    !   For NH-model without ADI: UMEAN = u1(nm,kfumx0(nm)), Z_MOMCOR
    !
    call upwhu(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
             & zmodel    ,kcs       ,kfu       ,kspu      ,dps       , &
             & s1        ,dpu       ,umean     ,hu        ,gdp       )
    do nm = 1, nmmax
       nmd = nm - icx
       nmu = nm + icx
       if (kcu(nm) /= 0) then
          !
          ! check for drying
          !
          if (kfu(nm)*hu(nm) < htrsh) then
             kfu(nm) = 0
             do k = kfumin(nm), kmax
                kfuz1(nm, k) = 0
             enddo
             kfumax(nm) = -1
          endif
          !
          ! check for flooding
          !
          if (flood) then
             !
             if (  hu(nm) > trsh &
                 & .and. max(s1(nm),s1(nmu)) - max(-real(dps(nm),fp),-real(dps(nmu),fp)) >= trsh) then
                if (kfu(nm) == 0) then
                  !
                  ! s1u is used for setting kfumax; s1 is old waterlevel at dry points.
                  !
                  s1u = max(s1(nm), s1(nmu))
                  if (umean(nm) >= 0.001) then
                     s1u = s1(nm)
                  elseif (umean(nm) <= - 0.001) then
                     s1u = s1(nmu)
                  else
                  endif
                  found = .false.
                  do k = kfumin(nm), kmax
                     if (.not. found .and. zk(k) + dzmin>=s1u .or. &
                         & (s1u>zk(kmax) .and. k==kmax  )) then
                        kfumx0(nm) = k
                        found = .true.
                     endif
                  enddo
                  kfu(nm) = 1
                  kfumax(nm) = kfumx0(nm)
                endif
             endif
          endif
          !
          ! Set some parameters in the wet points
          !
          if (kfu(nm) == 1) then
             !
             ! s1u is used for setting kfumax
             !
             s1u = max(s1(nm), s1(nmu))
             if (umean(nm) >= 0.001) then
                s1u = s1(nm)
             elseif (umean(nm) <= - 0.001) then
                s1u = s1(nmu)
             else
             endif
             !
             ! for couple points
             !
             if ((kcu(nmd)>=1) .and. (kcu(nm)==3)) then
                s1u = s1(nm)
             elseif ((kcu(nmu)>=1) .and. (kcu(nm)==3)) then
                s1u = s1(nmu)
             else
             endif
             !
             ! Set kfumax, using s1u
             !
             do k = kmax, kfumin(nm), -1
                kfuz1(nm, k) = 0
                !if (k == 1) then
                !   zkk = zbot
                !else
                !   zkk = zk(k - 1)
                !endif
                !
                ! 15-3-2007 change to allow S1 > ZK(KMAX), needed for NH-models
                !
                if (zk(k - 1) + dzmin <= s1u.or.(s1u>zk(kmax) .and. k==kmax  )) then
                   kfumax(nm) = k
                   exit
                endif
             enddo
             !
             ! Set kfuz1 but overwrite kfuz1 at points with gates
             !
             do k = kfumin(nm), kfumax(nm)
                if (kspu(nm, 0)*kspu(nm, k)==4 .or. kspu(nm, 0)*kspu(nm, k)==10) then
                   kfuz1(nm, k) = 0
                else
                   kfuz1(nm, k) = 1
                endif
             enddo
             !
             ! Compute dzu1, only for active wet points
             !
             dzutot = 0.0_fp
             do k = kfumin(nm), kfumax(nm)
                if (kfumin(nm) == kfumax(nm)) then
                   dzu1(nm, k) = hu(nm)
                elseif (k == kfumin(nm)) then
                   dzu1(nm, k) = zk(k) + dpu(nm)
                elseif (k == kfumax(nm)) then
                   if (nonhyd .and. nh_level==nh_full) then
                      dzu1(nm, k) = min(zk(k)-zk(k-1) , hu(nm)-dzutot)
                   else
                      dzu1(nm, k) = hu(nm) - dzutot
                   endif
                else
                   dzu1(nm, k) = zk(k) - zk(k - 1)
                endif
                dzutot = dzutot + dzu1(nm, k)
             enddo
             !
             ! Reset dzu1 to zero otherwise
             !
             do k = kfumax(nm) + 1, kmax
                dzu1(nm, k) = 0.0_fp
             enddo
             !
             ! A "trick" to ensure that "wet" cells that were dry
             ! obtains a velocity
             !
             if (kfumax(nm) > kfumx0(nm)) then
                do k = max(kfumx0(nm),kfumin(nm)), kfumax(nm)
                   u0(nm, k) = u1(nm, max(kfumx0(nm),kfumin(nm)))
                   u1(nm, k) = u1(nm, max(kfumx0(nm),kfumin(nm)))
                enddo
             endif
          endif
       endif
    enddo
end subroutine z_checku
