subroutine restart_lyrs (error     ,restid    ,i_restart ,msed      , &
                       & thlyr     ,lsedtot   ,nmaxus    ,cdryb     , &
                       & mmax      ,nlyr      ,success   ,svfrac    , &
                       & gdp       )
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
! Reads initial field condition records from an
! NEFIS flow output map file
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision 
    use globaldata
    use bedcomposition_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)        , dimension(:)    , pointer :: rhosol
!
! Global variables
!
    integer                                                                                   :: i_restart
    integer                                                                                   :: lsedtot
    integer                                                                                   :: nlyr
    integer                                                                                   :: nmaxus
    integer                                                                                   :: mmax
    logical                                                                                   :: error
    logical                                                                     , intent(out) :: success
    real(fp), dimension(                                                lsedtot), intent(in)  :: cdryb
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, nlyr, lsedtot), intent(out) :: msed
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, nlyr)                       :: svfrac
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, nlyr)         , intent(out) :: thlyr
    character(*)                                                                              :: restid
!
! Local variables
!
    integer                                 :: lrid        ! character variables for files Help var., length of restid
    integer                      , external :: crenef
    integer                      , external :: getelt
    integer                      , external :: clsnef
    integer                      , external :: inqelm
    integer                      , pointer  :: iporosity
    integer                                 :: istat
    integer                                 :: rst_lsed
    integer                                 :: rst_lsedbl
    integer                                 :: rst_lsedtot
    integer                                 :: rst_nlyr
    integer                                 :: ierror
    integer                                 :: fds
    integer                                 :: k
    integer                                 :: l
    integer                                 :: m
    integer                                 :: n
    integer                                 :: nm
    integer  , dimension(3,5)               :: cuindex
    integer  , dimension(3,5)               :: uindex
    integer                                 :: nbytsg
    integer                                 :: elmndm
    integer  , dimension(5)                 :: elmdms
    real(sp), dimension(:,:,:,:), pointer   :: rst_msed
    real(sp), dimension(:,:,:)  , pointer   :: rst_lyr
    real(fp), dimension(lsedtot)            :: mfrac
    real(fp)                                :: mfracsum
    real(fp)                                :: poros
    real(fp)                                :: sedthick
    character(len=256)                      :: dat_file
    character(len=8)                        :: elmtyp
    character(len=16)                       :: elmqty
    character(len=16)                       :: elmunt
    character(len=64)                       :: elmdes
    character(len=256)                      :: def_file
    logical                                 :: layerfrac
!
!! executable statements -------------------------------------------------------
!
    rhosol               => gdp%gdsedpar%rhosol
    !
    nullify(rst_msed)
    nullify(rst_lyr)
    error        = .false.
    success      = .false.
    layerfrac    = .false.
    call noextspaces(restid    ,lrid      )
    !
    ! open NEFIS trim-<restid> file
    !
    dat_file = restid(1:lrid)//'.dat'
    def_file = restid(1:lrid)//'.def'
    ierror   = crenef(fds, dat_file, def_file, ' ', 'r')
    if (ierror/= 0) then
       error = .true.
       goto 9999
    endif
    !
    ! initialize group index constant data
    !
    cuindex (3,1) = 1 ! increment in time
    cuindex (1,1) = 1
    cuindex (2,1) = 1
    !
    ! initialize group index time dependent data
    !
    uindex (3,1) = 1 ! increment in time
    uindex (1,1) = i_restart
    uindex (2,1) = i_restart
    !
    ierror = getelt(fds, 'map-const', 'LSED'  , cuindex, 1, 4, rst_lsed)
    if (ierror/= 0) goto 9999
    ierror = getelt(fds, 'map-const', 'LSEDBL', cuindex, 1, 4, rst_lsedbl)
    if (ierror/= 0) goto 9999
    rst_lsedtot = rst_lsed + rst_lsedbl
    if (rst_lsedtot /= lsedtot) goto 9999
    !
    elmndm = 5
    ierror  = inqelm(fds , 'MSED', elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
    if (ierror /= 0) then
        ierror  = inqelm(fds , 'LYRFRAC', elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
        layerfrac = .true.
        if (ierror /= 0) goto 9999
    endif
    rst_nlyr = elmdms(3)
    allocate(rst_msed(nmaxus, mmax, rst_nlyr, rst_lsedtot))
    allocate(rst_lyr(nmaxus, mmax, rst_nlyr))
    !
    if (layerfrac) then
        ierror = getelt(fds , 'map-sed-series', 'LYRFRAC', uindex, 1, &
                 & mmax*nmaxus*rst_nlyr*rst_lsedtot*4, rst_msed )       
        if (ierror /= 0) goto 9999
    else
        ierror = getelt(fds , 'map-sed-series', 'MSED', uindex, 1, &
                 & mmax*nmaxus*rst_nlyr*rst_lsedtot*4, rst_msed )
        if (ierror /= 0) goto 9999
    endif
    !
    ierror = getelt(fds , 'map-sed-series', 'THLYR', uindex, 1, &
                 & mmax*nmaxus*rst_nlyr*4, rst_lyr )
    if (ierror/= 0) goto 9999
    !
    if (nlyr>=rst_nlyr) then
       !
       ! more layers in simulation than in restart file (or same number)
       !
       ! copy first layer
       !
       thlyr(1:nmaxus,1:mmax,1)            = rst_lyr(1:nmaxus,1:mmax,1)
       msed(1:nmaxus,1:mmax,1,1:lsedtot)   = rst_msed(1:nmaxus,1:mmax,1,1:lsedtot)
       !
       ! insert empty layers (if necessary)
       !
       do k = 2,1+nlyr-rst_nlyr
          thlyr(1:nmaxus,1:mmax,k)              = 0.0_fp
          msed(1:nmaxus,1:mmax,k,1:lsedtot)     = 0.0_fp
       enddo
       !
       ! copy remaining layers
       !
       thlyr(1:nmaxus,1:mmax,nlyr-rst_nlyr+2:nlyr)            = rst_lyr(1:nmaxus,1:mmax,2:rst_nlyr)
       msed(1:nmaxus,1:mmax,nlyr-rst_nlyr+2:nlyr,1:lsedtot)   = rst_msed(1:nmaxus,1:mmax,2:rst_nlyr,1:lsedtot)
    else ! nlyr<rst_nlyr
       !
       ! more layers in restart file than in simulation
       !
       ! copy the first nlyr layers
       !
       thlyr(1:nmaxus,1:mmax,1:nlyr)           = rst_lyr(1:nmaxus,1:mmax,1:nlyr)
       msed(1:nmaxus,1:mmax,1:nlyr,1:lsedtot)  = rst_msed(1:nmaxus,1:mmax,1:nlyr,1:lsedtot)
       !
       ! add contents of other layers to last layer
       !
       do k = nlyr+1, rst_nlyr
          thlyr(1:nmaxus,1:mmax,nlyr)        = thlyr(1:nmaxus,1:mmax,nlyr) &
                                             & + rst_lyr(1:nmaxus,1:mmax,k)
          do l = 1, lsedtot
             msed(1:nmaxus,1:mmax,nlyr,l)    = msed(1:nmaxus,1:mmax,nlyr,l) &
                                             & + rst_msed(1:nmaxus,1:mmax,k,l) 
          enddo
       enddo
    endif
    !
    if (layerfrac) then
       istat = bedcomp_getpointer_integer(gdp%gdmorlyr, 'IPorosity', iporosity)
       !
       ! msed contains volume fractions
       !
       if (iporosity==0) then
          do l = 1,lsedtot
             do k = 1, nlyr
                do m = 1, mmax
                   do n = 1, nmaxus
                      msed(n,m,k,l) = msed(n,m,k,l)*thlyr(n,m,k)*cdryb(l)
                   enddo
                enddo
             enddo
          enddo
       else
          do k = 1, nlyr
             do m = 1, mmax
                do n = 1, nmaxus
                   !
                   ! determine mass fractions
                   !
                   mfracsum = 0.0_fp
                   do l = 1, lsedtot
                      mfrac(l) = msed(n,m,k,l)*rhosol(l)
                      mfracsum = mfracsum + mfrac(l)
                   enddo
                   if (mfracsum>0.0_fp) then
                      do l = 1, lsedtot
                         mfrac(l) = mfrac(l)/mfracsum
                      enddo
                      !
                      ! obtain porosity and sediment thickness without pores
                      !
                      call getporosity(gdp%gdmorlyr, mfrac, poros)
                      sedthick = thlyr(n,m,k)*(1.0_fp-poros)
                   else
                      sedthick = 0.0_fp
                      poros = 0.0_fp
                   endif
                   !
                   ! convert volume fractions to sediment mass
                   !
                   do l = 1, lsedtot
                      msed(n,m,k,l) = msed(n,m,k,l)*sedthick*rhosol(l)
                   enddo
                   svfrac(n,m,k) = 1.0_fp-poros
                enddo
             enddo
          enddo
       endif
    else
       if (iporosity>0) then
          do m = 1, mmax
             do n = 1, nmaxus
                sedthick = 0.0_fp
                do l = 1, lsedtot
                   sedthick = sedthick + msed(n,m,k,l)/rhosol(l)
                enddo
                svfrac(n,m,k) = sedthick/thlyr(n,m,k)
             enddo
          enddo
       endif
    endif
    !
    success = .true.
9999 continue
    if (associated(rst_msed)) deallocate (rst_msed)
    if (associated(rst_lyr))  deallocate (rst_lyr)
    ierror = clsnef(fds) 
end subroutine restart_lyrs
