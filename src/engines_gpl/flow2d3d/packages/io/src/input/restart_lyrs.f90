subroutine restart_lyrs (error     ,restid    ,i_restart ,lyrfrac   , &
                       & thlyr     ,lsedtot   ,nmaxus    ,mmax      ,nlyr      , &
                       & success   ,gdp       )
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
! Reads initial field condition records from an
! NEFIS flow output map file
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
    ! They replace the  include igd / include igp lines
    !
!
! Global variables
!
    integer                                                                                       :: i_restart
    integer                                                                                       :: lsedtot
    integer                                                                                       :: nlyr
    integer                                                                                       :: nmaxus
    integer                                                                                       :: mmax
    logical                                                                                       :: error
    logical                                                                         , intent(out) :: success
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, nlyr, lsedtot), intent(out) :: lyrfrac
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, nlyr)         , intent(out) :: thlyr
    character(*)                                                                                  :: restid
!
! Local variables
!
    integer                                 :: lrid        ! character variables for files Help var., length of restid
    integer                      , external :: crenef
    integer                      , external :: getelt
    integer                      , external :: clsnef
    integer                      , external :: inqelm
    integer                                 :: rst_lsed
    integer                                 :: rst_lsedbl
    integer                                 :: rst_lsedtot
    integer                                 :: rst_nlyr
    integer                                 :: ierror
    integer                                 :: fds
    integer                                 :: k
    integer                                 :: l
    integer  , dimension(3,5)               :: cuindex
    integer  , dimension(3,5)               :: uindex
    integer                                 :: nbytsg
    integer                                 :: elmndm
    integer  , dimension(5)                 :: elmdms
    real(sp) , dimension(:,:,:,:), pointer  :: rst_lyrfrac
    real(sp) , dimension(:,:,:)  , pointer  :: rst_thlyr
    character(len=256)                      :: dat_file
    character(len=8)                        :: elmtyp
    character(len=16)                       :: elmqty
    character(len=16)                       :: elmunt
    character(len=64)                       :: elmdes
    character(len=256)                      :: def_file
!
!! executable statements -------------------------------------------------------
!
    nullify(rst_lyrfrac)
    nullify(rst_thlyr)
    error        = .false.
    success      = .false.
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
    ierror = inqelm(fds , 'LYRFRAC', elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
    if (ierror/= 0) goto 9999
    rst_nlyr = elmdms(3)
    allocate(rst_lyrfrac(nmaxus, mmax, rst_nlyr, rst_lsedtot))
    allocate(rst_thlyr(nmaxus, mmax, rst_nlyr))
    !
    ierror = getelt(fds , 'map-sed-series', 'LYRFRAC', uindex, 1, &
                 & mmax*nmaxus*rst_nlyr*rst_lsedtot*4, rst_lyrfrac )
    if (ierror/= 0) goto 9999
    !
    ierror = getelt(fds , 'map-sed-series', 'THLYR', uindex, 1, &
                 & mmax*nmaxus*rst_nlyr*4, rst_thlyr )
    if (ierror/= 0) goto 9999
    !
    if (nlyr>=rst_nlyr) then
       !
       ! more layers in simulation than in restart file (or same number)
       !
       ! copy first layer
       !
       thlyr(1:nmaxus,1:mmax,1)             = rst_thlyr(1:nmaxus,1:mmax,1)
       lyrfrac(1:nmaxus,1:mmax,1,1:lsedtot) = rst_lyrfrac(1:nmaxus,1:mmax,1,1:lsedtot)
       !
       ! insert empty layers (if necessary)
       !
       do k = 2,1+nlyr-rst_nlyr
          thlyr(1:nmaxus,1:mmax,k)               = 0.0_fp
          lyrfrac(1:nmaxus,1:mmax,k,1:lsedtot-1) = 0.0_fp
          lyrfrac(1:nmaxus,1:mmax,k,lsedtot)     = 1.0_fp
       enddo
       !
       ! copy remaining layers
       !
       thlyr(1:nmaxus,1:mmax,nlyr-rst_nlyr+2:nlyr)             = rst_thlyr(1:nmaxus,1:mmax,2:rst_nlyr)
       lyrfrac(1:nmaxus,1:mmax,nlyr-rst_nlyr+2:nlyr,1:lsedtot) = rst_lyrfrac(1:nmaxus,1:mmax,2:rst_nlyr,1:lsedtot)
    else ! nlyr<rst_nlyr
       !
       ! more layers in restart file than in simulation
       !
       ! copy the first nlyr layers
       !
       thlyr(1:nmaxus,1:mmax,1:nlyr)             = rst_thlyr(1:nmaxus,1:mmax,1:nlyr)
       lyrfrac(1:nmaxus,1:mmax,1:nlyr,1:lsedtot) = rst_lyrfrac(1:nmaxus,1:mmax,1:nlyr,1:lsedtot)
       !
       ! convert last layer fractions into fraction thicknesses
       !
       do l = 1, lsedtot
          lyrfrac(1:nmaxus,1:mmax,nlyr,l) = lyrfrac(1:nmaxus,1:mmax,nlyr,l) &
                                          & * thlyr(1:nmaxus,1:mmax,nlyr)
       enddo
       !
       ! add contents of other layers to last layer
       !
       do k = nlyr+1, rst_nlyr
          thlyr(1:nmaxus,1:mmax,nlyr)        = thlyr(1:nmaxus,1:mmax,nlyr) &
                                             & + rst_thlyr(1:nmaxus,1:mmax,k)
          do l = 1, lsedtot
             lyrfrac(1:nmaxus,1:mmax,nlyr,l) = lyrfrac(1:nmaxus,1:mmax,nlyr,l) &
                    & + rst_lyrfrac(1:nmaxus,1:mmax,k,l) * rst_thlyr(1:nmaxus,1:mmax,k)
          enddo
       enddo
       !
       ! convert last layer fraction thicknesses into fractions
       !
       do l = 1, lsedtot
          lyrfrac(1:nmaxus,1:mmax,nlyr,l) = lyrfrac(1:nmaxus,1:mmax,nlyr,l) &
                                          & / thlyr(1:nmaxus,1:mmax,nlyr)
       enddo
    endif
    success = .true.
9999 continue
    if (associated(rst_lyrfrac)) deallocate (rst_lyrfrac)
    if (associated(rst_thlyr))   deallocate (rst_thlyr)
    ierror = clsnef(fds) 
end subroutine restart_lyrs
