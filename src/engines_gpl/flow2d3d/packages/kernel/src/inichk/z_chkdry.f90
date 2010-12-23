subroutine z_chkdry(j         ,nmmaxj    ,nmmax     ,kmax      ,lstsci    , &
                  & ltur      ,icx       ,icy       ,initia    ,kcu       , &
                  & kcv       ,kcs       ,kfu       ,kfv       ,kfs       , &
                  & kspu      ,kspv      ,kfuz1     ,kfvz1     ,kfsz1     , &
                  & kfumin    ,kfumax    ,kfvmin    ,kfvmax    ,kfsmin    , &
                  & kfsmax    ,dpu       ,dpv       ,hu        ,hv        , &
                  & hkru      ,hkrv      ,s1        ,dps       ,u1        , &
                  & v1        ,umean     ,vmean     ,r1        ,rtur1     , &
                  & guu       ,gvv       ,qxk       ,qyk       ,dzu1      , &
                  & dzv1      ,gdp       )
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
!    Function: ONLY for ZMODEL:
!              - Initiates the depth values at velocity points
!                and check drying on velocity points
!              - Calculate umean and vmean (depth averaged
!                velocities)
!              - Redefine s1, u1, v1 and r1 according to new
!                values of mask arrays
!              - Calculate qxk and qyk
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
    ! They replace the  include igd / include igp lines
    !
    real(fp)       , pointer :: dryflc
    logical        , pointer :: zmodel
    logical        , pointer :: kfuv_from_restart
!
! Global variables
!
    integer                                                                 :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir.
                                                                                      !!  If icx=1 then computation proceeds in the Y-dir.
    integer                                                                 :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                   , intent(in)  :: initia !!  if < 0: iteration process of morsys
                                                                                      !!  else  : equal to initi
    integer                                                                 :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays.
                                                                                      !!  Due to the shift in the 2nd (M-)index, J = -2*NMAX + 1
    integer                                                                 :: kmax   !  Description and declaration in iidim.f90
    integer                                                   , intent(in)  :: lstsci !  Description and declaration in iidim.f90
    integer                                                   , intent(in)  :: ltur   !  Description and declaration in iidim.f90
    integer                                                                 :: nmmax  !  Description and declaration in dimens.igs
    integer                                                                 :: nmmaxj !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs    !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcu    !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcv    !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfs    !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfsmax !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmin !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfu    !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfumax !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfumin !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfv    !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfvmax !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfvmin !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: kspu   !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: kspv   !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: kfsz1  !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: kfuz1  !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: kfvz1  !  Description and declaration in iidim.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps    !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: dpu    !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: dpv    !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guu    !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvv    !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hkru   !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hkrv   !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: hu     !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: hv     !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: s1     !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: umean  !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: vmean  !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, ltur)              :: rtur1  !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzu1   !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzv1   !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: qxk    !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: qyk    !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: u1     !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: v1     !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: r1     !  Description and declaration in rjdim.f90
!             
! Local variables
!
    integer :: k      ! Help var. 
    integer :: kd
    integer :: ku
    integer :: l      ! Help var. 
    integer :: mask   ! Help var. 
    integer :: ndm    ! Help var. NM-ICY 
    integer :: nm     ! Help var. loops 1,nmmax and j,nmmaxj 
    integer :: nmd
    real(fp):: htrsh
    real(fp):: hucres
    real(fp):: hvcres
!
!! executable statements -------------------------------------------------------
!
    zmodel             => gdp%gdprocs%zmodel
    dryflc             => gdp%gdnumeco%dryflc
    kfuv_from_restart  => gdp%gdrestart%kfuv_from_restart
    !
    htrsh = .5*dryflc
    !
    !NOTE: that the contents of KFS are here identical to KCS
    !      (except for boundary points)
    !
    if (initia>0) then
       !
       !-----initialize global arrays
       !     (HU, HV, QXK and QYK are initialized in RJDIM)
       !     (KFS/U/V are already initialised in Z_INIZM)
       !
       do nm = j, nmmaxj
          umean(nm) = 0.0
          vmean(nm) = 0.0
          !
          !---------check based on KFU/V but also on HU because HU can be zero
          !         despite KFU/V = 1. Altering of KFU/V value (to 0, when
          !         velocity point is dry)is done after UPWHU
          !
          if (kfu(nm)==1 .and. hu(nm)>=htrsh) then
             do k = kfumin(nm), kfumax(nm)
                umean(nm) = umean(nm) + (dzu1(nm, k)/hu(nm)) * u1(nm, k)
             enddo
          endif
          if (kfv(nm)==1 .and. hv(nm)>=htrsh) then
             do k = kfvmin(nm), kfvmax(nm)
                vmean(nm) = vmean(nm) + (dzv1(nm, k)/hv(nm)) * v1(nm, k)
             enddo
          endif
       enddo
       !
       !-----redefine S1 in case they are smaller then DPS and reset the mask
       !     arrays KFU,KFV and KFS
       !     -icx := -1 in m-direction, -icy := -1 in n-direction
       !     In Z_INIZM all relevant depths and waterlevels are already checked.
       !     This may be redundant
       !
       do nm = 1, nmmax
          nmd = nm - icx
          ndm = nm - icy
          if (kcs(nm)>0) then
             if (s1(nm)<= - real(dps(nm),fp)) then
                s1(nm) = -real(dps(nm),fp)
                kfu(nm) = 0
                kfu(nmd) = 0
                kfv(nm) = 0
                kfv(ndm) = 0
                kfs(nm) = 0
                do k = 1, kmax
                   kfuz1(nm, k) = 0
                   kfuz1(nmd, k) = 0
                   kfvz1(nm, k) = 0
                   kfvz1(ndm, k) = 0
                   kfsz1(nm, k) = 0
                enddo
             endif
          endif
       enddo
    endif
    !
    !-----calculate HU and HV
    !
    call upwhu(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
             & zmodel    ,kcs       ,kfu       ,kspu      ,dps       , &
             & s1        ,dpu       ,umean     ,hu        ,gdp       )
    call upwhu(j         ,nmmaxj    ,nmmax     ,kmax      ,icy       , &
             & zmodel    ,kcs       ,kfv       ,kspv      ,dps       , &
             & s1        ,dpv       ,vmean     ,hv        ,gdp       )
    !
    !-----check for dry velocity points
    !     Approach for 2D weirs (following WAQUA)
    !     HUCRES is initially set to extreme large value to guarantee
    !     the MIN operator works as planned
    !
    if (initia>0) then
       do nm = 1, nmmax
          hucres = 1E9
          !
          !-----------set hucres / hvcres
          !
          if (abs(kspu(nm, 0))==9) then
             if (umean(nm)>=0.001) then
                hucres = s1(nm) + hkru(nm)
             elseif (umean(nm)<= - 0.001) then
                hucres = s1(nm + icx) + hkru(nm)
             else
                hucres = max(s1(nm + icx), s1(nm)) + hkru(nm)
             endif
          endif
          !
          hvcres = 1E9
          if (abs(kspv(nm, 0))==9) then
             if (vmean(nm)>=0.001) then
                hvcres = s1(nm) + hkrv(nm)
             elseif (vmean(nm)<= - 0.001) then
                hvcres = s1(nm + icy) + hkrv(nm)
             else
                hvcres = max(s1(nm + icy), s1(nm)) + hkrv(nm)
             endif
          endif
          !
          !-----------check for dry velocity points
          !
          if (kfu(nm)*min(hu(nm), hucres)<dryflc .and. kcu(nm)*kfu(nm)==1) then
             !            if (hu (nm) .lt. htrsh) then
             if (.not.kfuv_from_restart) then
                kfu(nm) = 0
             endif
             do k = 1, kmax
                kfuz1(nm, k) = 0
             enddo
          endif
          if (kfv(nm)*min(hv(nm), hvcres)<dryflc .and. kcv(nm)*kfv(nm)==1) then
             !            if (hv (nm).lt. htrsh) then
             if (.not.kfuv_from_restart) then
                kfv(nm) = 0
             endif
             do k = 1, kmax
                kfvz1(nm, k) = 0
             enddo
          endif
       enddo
    endif
    !
    ! set KFS to 0 if the surrounding velocity points are dry
    !
    do nm = 1, nmmax
       if (kcs(nm)>0) then
          nmd = nm - icx
          ndm = nm - icy
          kfs(nm) = max(kfu(nm), kfu(nmd), kfv(nm), kfv(ndm))
          if (kfs(nm)==1) then
             do k = kfsmin(nm), kmax
                if (k<=kfsmax(nm)) then
                   kfsz1(nm, k) = 1
                else
                   kfsz1(nm, k) = 0
                endif
             enddo
          else
             kfsmax(nm) = -1
             do k = kfsmin(nm), kmax
                kfsz1(nm, k) = 0
             enddo
          endif
       endif
    enddo
    !
    !-----mask initial arrays
    !
    do nm = 1, nmmax
       mask = min(1, kcs(nm))
       s1(nm) = s1(nm)*mask
       if (kfs(nm) == 1) then
          !
          ! Currently wet computational point
          ! r1 must be set for the active layers
          !
          do k = 1, kmax
             do l = 1, lstsci
                r1(nm, k, l) = r1(nm, k, l)*kfsz1(nm, k)
             enddo
          enddo
       elseif (kcs(nm) == 0) then
          !
          ! Not a computational point
          ! r1 must be set to zero
          !
          do k = 1, kmax
             do l = 1, lstsci
                r1(nm, k, l) = 0.0
             enddo
          enddo
       endif
       do k = 0, kmax
          ku = min(k+1, kmax)
          kd = max(k  , 1   )
          do l = 1, ltur
             mask            = min(1, kfsz1(nm,kd) + kfsz1(nm,ku))
             rtur1(nm, k, l) = rtur1(nm, k, l) * mask
          enddo
       enddo
    enddo
    !
    !-----calculate flows in x- and y- direction
    !
    do nm = 1, nmmax
       do k = 1, kmax
          u1(nm, k)  = u1(nm, k)* kfuz1(nm, k)
          qxk(nm, k) = guu(nm)  * dzu1(nm, k) * u1(nm, k)
          v1(nm, k)  = v1(nm, k)* kfvz1(nm, k)
          qyk(nm, k) = gvv(nm)  * dzv1(nm, k) * v1(nm, k)
       enddo
    enddo
end subroutine z_chkdry
