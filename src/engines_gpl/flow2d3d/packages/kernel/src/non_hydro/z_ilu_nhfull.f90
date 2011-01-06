subroutine z_ilu_nhfull(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                      & bbka      ,bbkc      ,kmax      ,icx       , &
                      & icy       ,nmmax     ,kfsz1     , &
                      & dinv      ,gdp)

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
! Computes ILU decomposition and stores it in dinv
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
    integer , pointer :: m1_nhy
    integer , pointer :: m2_nhy
    integer , pointer :: n1_nhy
    integer , pointer :: n2_nhy
    real(fp), pointer :: milu
!
! Global variables
!
    integer                                         , intent(in)  :: icx
    integer                                         , intent(in)  :: icy
    integer                                         , intent(in)  :: kmax  !  Description and declaration in iidim.f90
    integer                                                       :: nmmax !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: kfsz1 !  Description and declaration in iidim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: bbk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: bbka
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: bbkc
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: aak
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: aak2
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: cck
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: cck2
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: dinv
    
!
! Local variables
!
    integer  :: ddb
    integer  :: icxy
    integer  :: k
    integer  :: m
    integer  :: ndelta
    integer  :: nm
    integer  :: nmst
    integer  :: nmstart 
    integer  :: nmu
    integer  :: num
    integer  :: nmd
    integer  :: ndm
    real(fp) :: bi
!
!! executable statements -------------------------------------------------------
!
    m1_nhy  => gdp%gdnonhyd%m1_nhy
    m2_nhy  => gdp%gdnonhyd%m2_nhy
    n1_nhy  => gdp%gdnonhyd%n1_nhy
    n2_nhy  => gdp%gdnonhyd%n2_nhy
    milu    => gdp%gdnonhyd%milu
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    ndelta  = n2_nhy - n1_nhy
    nmstart = (n1_nhy+ddb) + (m1_nhy-1+ddb)*icxy
    !
    if ((milu > 0.00001_fp) .and. (milu < 1.1_fp)) then
       !
       ! Modified ILU, should usually only be done for 0<=milu<=1.
       !
       do m = m1_nhy, m2_nhy
          nmst = nmstart + (m-m1_nhy)*icxy
          do nm = nmst, nmst + ndelta
             nmd = nm - icx
             ndm = nm - icy
             if (kfsz1(nm,1) /= 0) then
                dinv(nm, 1) = 1.0_fp / (bbk(nm, 1)                                                                 &
                            &           - aak2(nm,1) * dinv(ndm,1) * (cck2(ndm,1)+milu*(bbkc(ndm,1)+cck (ndm,1)))  &
                            &           - aak (nm,1) * dinv(nmd,1) * (cck (nmd,1)+milu*(bbkc(nmd,1)+cck2(nmd,1))) )
             endif
             do k = 2, kmax
                if (kfsz1(nm,k) /= 0) then
                   dinv(nm,k) = 1.0_fp / (bbk(nm,k)                                                                     &
                              &           - bbka(nm,k) * dinv(nm,k-1) * (bbkc(nm,k-1)+milu*(cck2(nm,k-1)+cck (nm,k-1))) &
                              &           - aak2(nm,k) * dinv(ndm,k)  * (cck2(ndm,k) +milu*(bbkc(ndm,k) +cck (ndm,k)))  &
                              &           - aak (nm,k) * dinv(nmd,k)  * (cck (nmd,k) +milu*(bbkc(nmd,k) +cck2(nmd,k))) )
                endif
             enddo
          enddo
       enddo
    else
       do m = m1_nhy, m2_nhy
          nmst = nmstart + (m-m1_nhy)*icxy
          do nm = nmst, nmst+ndelta
             nmd = nm - icx
             ndm = nm - icy
             if (kfsz1(nm,1) /= 0) then
                dinv(nm,1) = 1.0_fp / (bbk(nm,1) &
                           &           - aak2(nm,1) * dinv(ndm,1) * cck2(ndm,1) &
                           &           - aak (nm,1) * dinv(nmd,1) * cck (nmd,1) )
             endif
             do k = 2, kmax
                if (kfsz1(nm,k) /= 0) then
                   dinv(nm,k) = 1.0_fp / (bbk(nm,k)                                   &
                              &           - bbka(nm,k) * dinv(nm,k-1) * bbkc(nm, k-1) &
                              &           - aak2(nm,k) * dinv(ndm,k)  * cck2(ndm,k)   &
                              &           - aak (nm,k) * dinv(nmd,k)  * cck (nmd,k) )
               endif
            enddo
          enddo
       enddo
    endif
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m-m1_nhy)*icxy
       do nm = nmst, nmst+ndelta
          do k = 1, kmax
             if (kfsz1(nm,k) /= 0) then
                dinv(nm,k) = sqrt(dinv(nm,k))
             endif
          enddo
       enddo
    enddo
end subroutine z_ilu_nhfull
