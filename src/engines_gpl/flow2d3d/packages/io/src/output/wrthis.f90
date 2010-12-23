subroutine wrthis(lundia    ,error     ,trifil    ,selhis    ,ithisc    , &
                & itstrt    ,ithisi    ,zmodel    ,nostat    ,ntruv     , &
                & kmax      ,lmax      ,lstsci    ,lsal      ,ltem      , &
                & ltur      ,zkfs      ,zwl       ,zcuru     ,zcurv     , &
                & zcurw     ,zqxk      ,zqyk      ,ztauks    ,ztauet    , &
                & zvicww    ,zdicww    ,zrich     ,zrho      ,gro       , &
                & ztur      ,zvort     ,zenst     ,hydprs    ,fltr      , &
                & ctr       ,atr       ,dtr       ,velt      ,gdp       )
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
!    Function: Writes the time varying groups (1 & 3) to the
!              NEFIS HIS-DAT file
!              Selection is done using SELHIS. For elements like
!              ZCURW where KMAX must be > 1 this coupling between
!              KMAX and SELHIS is done in subroutine RDPRFL
! Method used:
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
    integer , dimension(:)    , pointer :: line_orig
    logical                   , pointer :: first
    integer                   , pointer :: celidt
    integer , dimension(:, :) , pointer :: elmdms
    type (nefiselement)       , pointer :: nefiselem
    integer       , dimension(:, :) , pointer :: mnstat
    real(fp)      , dimension(:, :) , pointer :: xystat
!
! Local parameters
!
    integer, parameter :: nelmx = 23
!
! Global variables
!
    integer                                        , intent(in)  :: ithisc !!  Current time counter for the history data file
    integer                                                      :: ithisi !  Description and declaration in inttim.igs
    integer                                                      :: itstrt !  Description and declaration in inttim.igs
    integer                                                      :: kmax   !  Description and declaration in iidim.f90
    integer                                                      :: lmax   !  Description and declaration in dimens.igs
    integer                                                      :: lsal   !  Description and declaration in dimens.igs
    integer                                                      :: lstsci !  Description and declaration in iidim.f90
    integer                                                      :: ltem   !  Description and declaration in dimens.igs
    integer                                                      :: ltur   !  Description and declaration in iidim.f90
    integer                                                      :: lundia !  Description and declaration in inout.igs
    integer                                                      :: nostat !  Description and declaration in dimens.igs
    integer                                                      :: ntruv  !  Description and declaration in dimens.igs
    integer      , dimension(nostat)                             :: zkfs   !  KFS in monitoring station
    logical                                        , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    logical                                        , intent(in)  :: zmodel !  Description and declaration in procs.igs
    real(fp)     , dimension(nostat)                             :: ztauet !  Description and declaration in rjdim.f90
    real(fp)     , dimension(nostat)                             :: ztauks !  Description and declaration in rjdim.f90
    real(fp)     , dimension(nostat)                             :: zwl    !  Description and declaration in rjdim.f90
    real(fp)     , dimension(nostat, 0:kmax)                     :: zdicww !  Description and declaration in rjdim.f90
    real(fp)     , dimension(nostat, 0:kmax)                     :: zrich  !  Description and declaration in rjdim.f90
    real(fp)     , dimension(nostat, 0:kmax)                     :: zvicww !  Description and declaration in rjdim.f90
    real(fp)     , dimension(nostat, 0:kmax, ltur)               :: ztur   !  Description and declaration in rjdim.f90
    real(fp)     , dimension(nostat, kmax)                       :: hydprs !  Description and declaration in rjdim.f90
    real(fp)     , dimension(nostat, kmax)                       :: zcuru  !  Description and declaration in rjdim.f90
    real(fp)     , dimension(nostat, kmax)                       :: zcurv  !  Description and declaration in rjdim.f90
    real(fp)     , dimension(nostat, kmax)                       :: zcurw  !  Description and declaration in rjdim.f90
    real(fp)     , dimension(nostat, kmax)                       :: zenst  !  Description and declaration in rjdim.f90
    real(fp)     , dimension(nostat, kmax)                       :: zqxk   !  Description and declaration in rjdim.f90
    real(fp)     , dimension(nostat, kmax)                       :: zqyk   !  Description and declaration in rjdim.f90
    real(fp)     , dimension(nostat, kmax)                       :: zrho   !  Description and declaration in rjdim.f90
    real(fp)     , dimension(nostat, kmax)                       :: zvort  !  Description and declaration in rjdim.f90
    real(fp)     , dimension(nostat, kmax, lstsci)               :: gro    !  Description and declaration in rjdim.f90
    real(fp)     , dimension(ntruv)                              :: ctr    !  Description and declaration in rjdim.f90
    real(fp)     , dimension(ntruv)                              :: fltr   !  Description and declaration in rjdim.f90
    real(fp)     , dimension(ntruv, lstsci)                      :: atr    !  Description and declaration in rjdim.f90
    real(fp)     , dimension(ntruv, lstsci)                      :: dtr    !  Description and declaration in rjdim.f90
    character(*)                                   , intent(in)  :: trifil !!  File name for FLOW NEFIS output
                                                                           !!  files (tri"h/m"-"casl""labl".dat/def)
    character(23)                                  , intent(in)  :: selhis !  Description and declaration in tricom.igs    
    character(10)                                  , intent(in)  :: velt   !! Velocity type 'eulerian' or 'GLM'
!
! Local variables
!
    integer                                      :: i         ! Help var. 
    integer                                      :: ierror    ! Local errorflag for NEFIS files 
    integer                                      :: lastcl
    integer                                      :: nelmx1
    integer                                      :: nelmx3
    integer        , dimension(1)                :: idummy    ! Help array to read/write Nefis files 
    integer        , dimension(nelmx)            :: nbytsg    ! Array containing the number of bytes of each single ELMTPS 
    integer                           , external :: neferr
    logical                                      :: wrswch    ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
    real(fp)       , dimension(ntruv)            :: ctr_buff  ! Help array for rearranging the order of cross-sections for writing to NEFIS file
    real(fp)       , dimension(ntruv)            :: fltr_buff ! Help array for rearranging the order of cross-sections for writing to NEFIS file
    real(fp)       , dimension(ntruv, lstsci)    :: atr_buff  ! Help array for rearranging the order of cross-sections for writing to NEFIS file
    real(fp)       , dimension(ntruv, lstsci)    :: dtr_buff  ! Help array for rearranging the order of cross-sections for writing to NEFIS file
    character(10)  , dimension(nelmx)            :: elmunt    ! Array with element physical unit 
    character(16)                                :: grnam1    ! Data-group name defined for the NEFIS-files group 1 
    character(16)                                :: grnam3    ! Data-group name defined for the NEFIS-files group 3 
    character(16)  , dimension(nelmx)            :: elmnms    ! Element name defined for the NEFIS-files 
    character(16)  , dimension(nelmx)            :: elmqty    ! Array with element quantity 
    character(16)  , dimension(nelmx)            :: elmtps    ! Array containing the types of the elements (real, ch. , etc. etc.) 
    character(256)                               :: filnam    ! Help var. for FLOW file name 
    character(256)                               :: errmsg    ! Character var. containing the error message to be written to file. The message depends on the error. 
    character(64)  , dimension(nelmx)            :: elmdes    ! Array with element description 
!
! Data statements
!
    data grnam1/'his-info-series'/
    data grnam3/'his-series'/
    data elmnms/'ITHISC', 'ZKFS', 'ZWL', 'ZCURU', 'ZCURV', 'ZCURW', 'ZQXK', 'ZQYK',    &
        & 'GRO', 'ZTUR', 'ZTAUKS', 'ZTAUET', 'ZVICWW', 'ZDICWW', 'ZRICH', 'ZRHO',&
        & 'HYDPRES', 'XYSTAT', 'MNSTAT', 'FLTR', 'CTR', 'ATR', 'DTR'/
    data elmqty/23*' '/
    data elmunt/2*'[   -   ]', '[   M   ]', 3*'[  M/S  ]', 2*'[  M3/S ]',  &
        & 2*'[   -   ]', 2*'[  N/M2 ]', 2*'[  M2/S ]', '[   -   ]', '[ KG/M3 ]', &
        & '[  N/M2 ]', '[   M   ]', '[   -   ]', &
        & '[   M3  ]', '[  M3/S ]', 2*'[   -   ]'/
    data elmtps/2*'INTEGER', 16*'REAL','INTEGER',4*'REAL'/
    data nbytsg/23*4/
    data (elmdes(i), i = 1, 11)                                                 &
         & /'timestep number (ITHISC*DT*TUNIT := time in sec from ITDATE)  ',    &
         & 'Non-active (0) or active (1) zeta point (time-dependent)      ',     &
         & 'Water-level in station (zeta point)                           ',     &
         & 'U-velocity per layer in station (zeta point)                  ',     &
         & 'V-velocity per layer in station (zeta point)                  ',     &
         & 'W-velocity per layer in station (zeta point)                  ',     &
         & 'U-discharge per layer in station (zeta point)                 ',     &
         & 'V-discharge per layer in station (zeta point)                 ',     &
         & 'Concentrations per layer in station (zeta point)              ',     &
         & 'Turbulent quantity per layer in station (zeta point)          ',     &
         & 'Bottom stress U in station (zeta point)                       '/
    data (elmdes(i), i = 12, 23)                                                &
         & /'Bottom stress V in station (zeta point)                       ',    &
         & 'Vertical eddy viscosity-3D in station (zeta point)            ',     &
         & 'Vertical eddy diffusivity-3D in station (zeta point)          ',     &
         & 'Richardson number in station (zeta point)                     ',     &
         & 'Density per layer in station (zeta point)                     ',     &
         & 'Non-hydrostatic pressure at station (zeta point)              ',     &
         & '(X,Y) coordinates of monitoring stations                      ',     &
         & '(M,N) indices of monitoring stations                          ',     &
         & 'Total discharge through cross section (velocity points)       ',     &
         & 'Monumentary discharge through cross section (velocity points) ',     &
         & 'Advective transport through cross section (velocity points)   ',     &
         & 'Dispersive transport through cross section (velocity points)  '/
!
!! executable statements -------------------------------------------------------
!
    line_orig  => gdp%gdstations%line_orig
    nefiselem  => gdp%nefisio%nefiselem(nefiswrthis)
    first      => nefiselem%first
    celidt     => nefiselem%celidt
    elmdms     => nefiselem%elmdms
    mnstat     => gdp%gdstations%mnstat
    xystat     => gdp%gdstations%xystat
    !
    !
    !-----Initialize local variables
    !
    elmdes(4) = 'U-velocity per layer in station (zeta point, '//velt//')'
    elmdes(5) = 'V-velocity per layer in station (zeta point, '//velt//')'
    nelmx1 = 1
    nelmx3 = nelmx - 1
    ierror = 0
    !
    ! Modifications to append his-files
    ! celidt = INT ( (ithisc - itstrt + 1.01*ithisi) / ithisi )
    !
    filnam = trifil(1:3) // 'h' // trifil(5:)
    !
    wrswch = .false.
    if (first) then
       call getcel(filnam    ,grnam1    ,nelmx1    ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(1) ,celidt    ,wrswch    ,ierror   )
    endif
    celidt = celidt + 1
    !
    errmsg = ' '
    wrswch = .true.
    !
    !-----Set up the element dimensions
    !
    if (first) then
       first = .false.
       call filldm(elmdms    ,1         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       if (nostat>0) then
          if (selhis(1:1)=='Y') then
             call filldm(elmdms    ,2         ,1         ,nostat    ,0         , &
                       & 0         ,0         ,0         )
          endif
          if (selhis(1:1)=='Y') then
             call filldm(elmdms    ,3         ,1         ,nostat    ,0         , &
                       & 0         ,0         ,0         )
          endif
          if (index(selhis(2:3), 'Y')>0) then
             call filldm(elmdms    ,4         ,2         ,nostat    ,kmax      , &
                       & 0         ,0         ,0         )
             call filldm(elmdms    ,5         ,2         ,nostat    ,kmax      , &
                       & 0         ,0         ,0         )
          endif
          if (selhis(4:4)=='Y') then
             call filldm(elmdms    ,6         ,2         ,nostat    ,kmax      , &
                       & 0         ,0         ,0         )
          endif
          if (selhis(20:20)=='Y') then
             call filldm(elmdms    ,7         ,2         ,nostat    ,kmax      , &
                       & 0         ,0         ,0         )
             call filldm(elmdms    ,8         ,2         ,nostat    ,kmax      , &
                       & 0         ,0         ,0         )
          endif
          if (index(selhis(5:12), 'Y')/=0) then
             call filldm(elmdms    ,9         ,3         ,nostat    ,kmax      , &
                       & lstsci    ,0         ,0         )
          endif
          if (index(selhis(13:14), 'Y')/=0) then
             call filldm(elmdms    ,10        ,3         ,nostat    ,kmax + 1  , &
                       & ltur      ,0         ,0         )
          endif
          if (index(selhis(15:16), 'Y')>0) then
             call filldm(elmdms    ,11        ,1         ,nostat    ,0         , &
                       & 0         ,0         ,0         )
             call filldm(elmdms    ,12        ,1         ,nostat    ,0         , &
                       & 0         ,0         ,0         )
          endif
          if (selhis(17:17)=='Y') then
             call filldm(elmdms    ,13        ,2         ,nostat    ,kmax + 1  , &
                       & 0         ,0         ,0         )
          endif
          if (selhis(18:18)=='Y') then
             call filldm(elmdms    ,14        ,2         ,nostat    ,kmax + 1  , &
                       & 0         ,0         ,0         )
          endif
          if (index(selhis(17:18), 'Y')>0) then
             call filldm(elmdms    ,15        ,2         ,nostat    ,kmax + 1  , &
                       & 0         ,0         ,0         )
          endif
          if (selhis(19:19)=='Y') then
             call filldm(elmdms    ,16        ,2         ,nostat    ,kmax      , &
                       & 0         ,0         ,0         )
          endif
          if (zmodel) then
             if (selhis(2:2)=='Y') then
                call filldm(elmdms    ,17        ,2         ,nostat    ,kmax      , &
                          & 0         ,0         ,0         )
             endif
          endif
          call filldm(elmdms    ,18        ,2         ,2         ,nostat    , &
                    & 0         ,0         ,0         )
          call filldm(elmdms    ,19        ,2         ,2         ,nostat    , &
                    & 0         ,0         ,0         )
       endif
       if (ntruv>0) then
          if (selhis(20:20)=='Y') then
             call filldm(elmdms    ,20        ,1         ,ntruv     ,0         , &
                       & 0         ,0         ,0         )
          endif
          if (selhis(21:21)=='Y') then
             call filldm(elmdms    ,21        ,1         ,ntruv     ,0         , &
                       & 0         ,0         ,0         )
          endif
          if (selhis(22:22)=='Y') then
             call filldm(elmdms    ,22        ,2         ,ntruv     ,lstsci    , &
                       & 0         ,0         ,0         )
          endif
          if (selhis(23:23)=='Y') then
             call filldm(elmdms    ,23        ,2         ,ntruv     ,lstsci    , &
                       & 0         ,0         ,0         )
          endif
       endif
    endif
    !
    ! Modification for overwriting instead of appending
    ! if time is already on file
    !
    !-----group 1, element 1 'ITHISC'
    !
    wrswch = .false.
    !-->
   10 continue
    if (celidt>1) then
       idummy(1) = -1
       lastcl = celidt - 1
       call putgti(filnam    ,grnam1    ,nelmx1    ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(1) ,lastcl    ,wrswch    ,ierror   ,idummy    )
       if (idummy(1)>=ithisc) then
          celidt = lastcl
          goto 10
       endif
    else
       celidt = 1
    endif
    !
    wrswch = .true.
    idummy(1) = ithisc
    call putgti(filnam    ,grnam1    ,nelmx1    ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(1) ,celidt    ,wrswch    ,ierror   ,idummy    )
    if (ierror/=0) goto 999
    !
    !-----group 3, first 16 depend on NOSTAT > 0
    !
    if (nostat>0) then
       !
       !--------group 3: element 'ZKFS' only if SELHIS( 1: 1) = 'Y'
       !
       if (selhis(1:1)=='Y') then
          call putgti(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)  , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)     , &
                    & elmnms(2) ,celidt    ,wrswch    ,ierror   ,zkfs           )
          if (ierror/=0) goto 999
       endif
       !
       !--------group 3: element 'ZWL' only if SELHIS( 1: 1) = 'Y'
       !
       if (selhis(1:1)=='Y') then
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)  , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)     , &
                    & elmnms(3) ,celidt    ,wrswch    ,ierror   ,zwl           )
          if (ierror/=0) goto 999
       endif
       !
       !--------group 3: element 'ZCURU' & 'ZCURV'
       !                 only if SELHIS( 2: 3) <> 'NN'
       !
       if (index(selhis(2:3), 'Y')>0) then
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)  , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)     , &
                    & elmnms(4) ,celidt    ,wrswch    ,ierror   ,zcuru         )
          if (ierror/=0) goto 999
          !
          !-----------group 3: element 'ZCURV'
          !
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)  , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)     , &
                    & elmnms(5) ,celidt    ,wrswch    ,ierror   ,zcurv         )
          if (ierror/=0) goto 999
       endif
       !
       !--------group 3: element 'ZCURW', only if KMAX > 1
       !                 (:= SELHIS( 4: 4) = 'Y')
       !
       if (selhis(4:4)=='Y') then
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                    & elmnms(6) ,celidt    ,wrswch    ,ierror   ,zcurw     )
          if (ierror/=0) goto 999
       endif
       !
       !--------group 3: element 'ZQXK' only if SELHIS(20:20) = 'Y'
       !
       if (selhis(20:20)=='Y') then
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                    & elmnms(7) ,celidt    ,wrswch    ,ierror   ,zqxk      )
          if (ierror/=0) goto 999
          !
          !-----------group 3: element 'ZQYK' only if SELHIS(20:20) = 'Y'
          !
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                    & elmnms(8) ,celidt    ,wrswch    ,ierror   ,zqyk      )
          if (ierror/=0) goto 999
       endif
       !
       !--------group 3: element 'GRO', only if LSTSCI > 0
       !                 (:= SELHIS( 5:12) <> 'NNNNNNNN')
       !
       if (index(selhis(5:12), 'Y')/=0) then
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                    & elmnms(9) ,celidt    ,wrswch    ,ierror   ,gro       )
          if (ierror/=0) goto 999
       endif
       !
       !--------group 3: element 'ZTUR', only if LTUR > 0
       !                 (:= SELHIS(13:14) <> 'NN')
       !
       if (index(selhis(13:14), 'Y')/=0) then
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                    & elmnms(10),celidt    ,wrswch    ,ierror   ,ztur      )
          if (ierror/=0) goto 999
       endif
       !
       !--------group 3: element 'ZTAUKS' & 'ZTAUET'
       !                 only if SELHIS(15:16) <> 'NN'
       !
       if (selhis(15:15)=='Y') then
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                    & elmnms(11),celidt    ,wrswch    ,ierror   ,ztauks    )
          if (ierror/=0) goto 999
          !
          !-----------group 3: element 'ZTAUET'
          !
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                    & elmnms(12),celidt    ,wrswch    ,ierror   ,ztauet    )
          if (ierror/=0) goto 999
       endif
       !
       !--------group 3: element 'ZVICWW', only if KMAX > 1
       !                 (:= SELHIS(17:17) = 'Y')
       !
       if (selhis(17:17)=='Y') then
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                    & elmnms(13),celidt    ,wrswch    ,ierror   ,zvicww    )
          if (ierror/=0) goto 999
       endif
       !
       !--------group 3: element 'ZDICWW', only if KMAX > 1
       !                 (:= SELHIS(18:18) = 'Y')
       !
       if (selhis(18:18)=='Y') then
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                    & elmnms(14),celidt    ,wrswch    ,ierror   ,zdicww    )
          if (ierror/=0) goto 999
       endif
       !
       !--------group 3: element 'ZRICH', only if KMAX > 1
       !                 (:= SELHIS(17:18) <> 'NN')
       !
       if (index(selhis(17:18), 'Y')>0) then
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                    & elmnms(15),celidt    ,wrswch    ,ierror   ,zrich     )
          if (ierror/=0) goto 999
       endif
       !
       !--------group 3: element 'ZRHO', only if LSAL > 0 or LTEM > 0
       !                 (:= SELHIS(19:19) = 'Y')
       !
       if (selhis(19:19)=='Y') then
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                    & elmnms(16),celidt    ,wrswch    ,ierror   ,zrho      )
          if (ierror/=0) goto 999
       endif
       !
       !--------group 3: element 'HYDPRES'
       !                 only if selhis( 2: 2) <> 'N'
       !
       if (index(selhis(2:2), 'Y')>0 .and. zmodel) then
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                    & elmnms(17),celidt    ,wrswch    ,ierror   ,hydprs    )
          if (ierror/=0) goto 999
       endif
       !
       !--------group 3: element 'XYSTAT'
       !
       call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
                 & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)  , &
                 & elmnms(18),celidt    ,wrswch    ,ierror   ,xystat     )
       if (ierror/=0) goto 999
       !
       !--------group 3: element 'MNSTAT'
       !
       call putgti(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
                 & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)  , &
                 & elmnms(19),celidt    ,wrswch    ,ierror   ,mnstat     )
       if (ierror/=0) goto 999
    endif
    !
    !-----group 3: next 4 depend on NTRUV > 0
    !
    if (ntruv>0) then
       do i = 1, ntruv
          !
          ! Re-arrange the order with line_orig
          !
          fltr_buff( line_orig(i) )   = fltr(i)
          ctr_buff ( line_orig(i) )   = ctr (i)
          atr_buff ( line_orig(i), :) = atr (i, :)
          dtr_buff ( line_orig(i), :) = dtr (i, :)             
       enddo
       !
       !--------group 3: element 20 'FLTR' only if SELHIS(20:20) = 'Y'
       !
       if (selhis(20:20)=='Y') then
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)  , &
                    & elmnms(20),celidt    ,wrswch    ,ierror   ,fltr_buff  )
          if (ierror/=0) goto 999
       endif
       !
       !--------group 3: element 21 'CTR' only if SELHIS(21:21) = 'Y'
       !
       if (selhis(21:21)=='Y') then
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)  , &
                    & elmnms(21),celidt    ,wrswch    ,ierror   ,ctr_buff   )
          if (ierror/=0) goto 999
       endif
       !
       !--------group 3: element 22 'ATR', only if LSTSCI > 0
       !                 (:= SELHIS(22:22) = 'Y')
       !
       if (selhis(22:22)=='Y') then
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)  , &
                    & elmnms(22),celidt    ,wrswch    ,ierror   ,atr_buff   )
          if (ierror/=0) goto 999
       endif
       !
       !--------group 3: element 23 'DTR', only if LSTSCI > 0
       !                 (:= SELHIS(23:23) = 'Y')
       !
       if (selhis(23:23)=='Y') then
          call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
                    & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)  , &
                    & elmnms(23),celidt    ,wrswch    ,ierror   ,dtr_buff   )
          if (ierror/=0) goto 999
       endif
    endif
    !
    !-----write error message if error occurred and set error = .true.
    !     the files will be closed in clsnef (called in triend)
    !
    !
  999 continue
    if (ierror/= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg, gdp)
       error = .true.
    endif
end subroutine wrthis
