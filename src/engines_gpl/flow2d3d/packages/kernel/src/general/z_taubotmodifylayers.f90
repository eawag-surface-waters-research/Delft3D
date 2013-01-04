subroutine z_taubotmodifylayers(nmmax  , kmax   , lstsci , icx          , icy    , & 
                              & kfs    , kfsmin , kfsmax , dps          , dzs1   , &
                              & kfu    , kfumin , kfumax , dpu          , dzu1   , &
                              & kfv    , kfvmin , kfvmax , dpv          , dzv1   , &
                              & r1     , s1     , zk     , modify_dzsuv , gdp    )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
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
!  $Id: z_taubotmodifylayers.f90 1246 2012-02-09 11:25:49Z platzek $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20111115_13532_z-model_improvements_oss-merge/engines_gpl/flow2d3d/packages/kernel/src/general/z_taubotmodifylayers.f90 $
!!--description-----------------------------------------------------------------
!
! Computes inproduct between two vectors.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                                         , intent(in) :: icx      !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in) :: icy      !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in) :: kmax     !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in) :: nmmax
    integer                                         , intent(in) :: lstsci
    integer , dimension(3)                                       :: modify_dzsuv
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfs      !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfu      !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfv      !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfsmin   !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfumin   !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfvmin   !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfsmax   !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfumax   !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfvmax   !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                   :: dps
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                   :: dpu
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                   :: dpv
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                   :: s1
    real(fp), dimension(0:kmax)                                  :: zk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: dzs1
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: dzu1
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: dzv1
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)     :: r1
!
! Local variables
!
    integer  :: k
    integer  :: l
    integer  :: nm
    integer  :: nmu
    integer  :: num
    real(fp) :: s1u
    real(fp) :: s1v
!
!! executable statements -------------------------------------------------------
!
    !
    ! ISSUE: DELFT3D-14744: Modification of near bed layer thicknesses to obtain 
    ! a smoother approximation of the bed shear stress,
	! including mass conserving approach for constituents
    !
    if (modify_dzsuv(1)==1) then
       do nm = 1, nmmax
          if (kfs(nm)==1 .and. kfsmax(nm)>kfsmin(nm)) then
              k = kfsmin(nm)
              if (dzs1(nm,k) < dzs1(nm,k+1)) then
                 !
                 ! Ensure conservation of constituents
                 !
                 do l = 1, lstsci
                    r1(nm,k,l) = (        r1(nm,k+1,l)*(dzs1(nm,k+1)-dzs1(nm,k)) +     &
                               &   2.0_fp*r1(nm,k  ,l)* dzs1(nm,k)                 ) / &
                               & (dzs1(nm,k+1) + dzs1(nm,k))
                 enddo
              elseif (dzs1(nm,k) > dzs1(nm,k+1)) then
                 !
                 ! Ensure conservation of constituents
                 !
                 do l = 1, lstsci
                    r1(nm,k+1,l) = (        r1(nm,k  ,l)*(dzs1(nm,k)-dzs1(nm,k+1)) +     &
                                 &   2.0_fp*r1(nm,k+1,l)* dzs1(nm,k+1)                 ) / &
                                 & (dzs1(nm,k+1) + dzs1(nm,k))
                 enddo
              endif
              dzs1(nm, k  ) = 0.5_fp*(real(dps(nm),fp)+min(zk(k+1),s1(nm)))
              dzs1(nm, k+1) = dzs1(nm,k)
          endif
       enddo
    endif
    !
    ! Layer thicknesses in U-velocity points
    !
    if (modify_dzsuv(2)==1) then
       do nm = 1, nmmax
          if (kfu(nm)==1 .and. kfumax(nm)>kfumin(nm)) then
             k       = kfumin(nm)
             nmu     = nm + icx
             s1u     = max(s1(nm), s1(nmu))
             dzu1(nm, k  ) = 0.5_fp*(dpu(nm)+min(zk(k+1),s1u))
             dzu1(nm, k+1) = dzu1(nm,k)
          endif
       enddo
    endif
    !
    ! Layer thicknesses in V-velocity points
    !
    if (modify_dzsuv(3)==1) then
       do nm = 1, nmmax
          if (kfv(nm)==1 .and. kfvmax(nm)>kfvmin(nm)) then
             k       = kfvmin(nm)
             num     = nm + icy
             s1v     = max(s1(nm), s1(num))
             dzv1(nm, k  ) = 0.5_fp*(dpv(nm)+min(zk(k+1),s1v))
             dzv1(nm, k+1) = dzv1(nm,k)
          endif
       enddo
    endif
end subroutine z_taubotmodifylayers
