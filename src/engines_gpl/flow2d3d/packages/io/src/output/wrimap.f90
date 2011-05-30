subroutine wrimap(lundia    ,error     ,trifil    ,selmap    ,simdat    , &
                & itdate    ,tzone     ,tunit     ,dt        ,mmax      , &
                & kmax      ,lmax      ,lstsci    ,ltur      ,nmaxus    , &
                & noroco    ,norow     ,nostat    ,nsrc      ,ntruv     , &
                & grdang    ,dpsopt    ,sferic    ,lsed      ,lsedtot   , &
                & zmodel    ,zbot      ,namsrc    ,namcon    ,namsed    , &
                & kcu       ,kcv       ,kcs       ,irocol    ,ibuff     , &
                & xcor      ,ycor      ,xz        ,yz        ,alfas     , &
                & dp        ,thick     ,zk        ,rbuff     ,rbuff1    , &
                & dps       ,dpu       ,dpv       ,gsqs      ,gdp       )
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
!    Function: Writes the initial group 2 ('map-const') to
!              MAP-DAT
!              Selection is done using SELMAP. For elements like
!              NAMCON where LMAX must be > 0 this coupling between
!              LMAX and SELMAP is done in subroutine RDPRFL
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
    integer             , dimension(:, :) , pointer :: mnit
    integer             , dimension(:, :) , pointer :: mnstat
    character(20)       , dimension(:)    , pointer :: namst
    character(20)       , dimension(:)    , pointer :: namtra
    logical                               , pointer :: first
    integer                               , pointer :: celidt
    integer             , dimension(:, :) , pointer :: elmdms
    type (nefiselement)                   , pointer :: nefiselem
!
! Local parameters
!
    integer, parameter :: nelmx = 45
!
! Global variables
!
    integer                                                         , intent(in)  :: itdate  !  Description and declaration in exttim.igs
    integer                                                                       :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                         , intent(in)  :: lmax    !  Description and declaration in dimens.igs
    integer                                                         , intent(in)  :: lsed    !  Description and declaration in esm_alloc_int.f90
    integer                                                         , intent(in)  :: lsedtot !  Description and declaration in esm_alloc_int.f90
    integer                                                         , intent(in)  :: lstsci  !  Description and declaration in esm_alloc_int.f90
    integer                                                         , intent(in)  :: ltur    !  Description and declaration in esm_alloc_int.f90
    integer                                                                       :: lundia  !  Description and declaration in inout.igs
    integer                                                                       :: mmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                                       :: nmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                                       :: nmaxus  !  Description and declaration in esm_alloc_int.f90
    integer                                                                       :: noroco  !  Description and declaration in esm_alloc_int.f90
    integer                                                         , intent(in)  :: norow   !  Description and declaration in esm_alloc_int.f90
    integer                                                                       :: nostat  !  Description and declaration in dimens.igs
    integer                                                                       :: nsrc    !  Description and declaration in esm_alloc_int.f90
    integer                                                                       :: ntruv   !  Description and declaration in dimens.igs
    integer , dimension(5, noroco)                                                :: irocol  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: kcs     !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: kcu     !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: kcv     !  Description and declaration in esm_alloc_int.f90
    integer , dimension(nmaxus, mmax)                                             :: ibuff   !  Description and declaration in esm_alloc_int.f90
    logical                                                         , intent(out) :: error   !!  Flag=TRUE if an error is encountered
    logical                                                         , intent(in)  :: sferic  !  Description and declaration in tricom.igs
    logical                                                         , intent(in)  :: zmodel  !  Description and declaration in procs.igs
    real(fp)                                                        , intent(in)  :: dt      !  Description and declaration in esm_alloc_real.f90
    real(fp)                                                        , intent(in)  :: grdang  !  Description and declaration in tricom.igs
    real(fp)                                                        , intent(in)  :: tunit   !  Description and declaration in exttim.igs
    real(fp)                                                        , intent(in)  :: tzone   !  Description and declaration in exttim.igs
    real(fp)                                                        , intent(in)  :: zbot    !  Description and declaration in zmodel.igs
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: alfas   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: dp      !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: dpu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: dpv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: gsqs    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: xcor    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: xz      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: ycor    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: yz      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax + 1)                                   , intent(out) :: rbuff1  
    real(fp), dimension(kmax)                                                     :: thick   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                                     , intent(in)  :: zk      !!  Vertical coordinates of cell interfaces
                                                                                             !!  Flag for activation of Z-MODEL
    real(fp), dimension(nmaxus, mmax)                                             :: rbuff   !  Description and declaration in r-i-ch.igs
    character(*)                                                    , intent(in)  :: trifil  !!  File name for FLOW NEFIS output
                                                                                             !!  files (tri"h/m"-"casl""labl".dat/def)
    character(16)                                                   , intent(in)  :: simdat  !!  Simulation date representing the
                                                                                             !!  flow condition at this date
    character(20), dimension(lmax)                                  , intent(in)  :: namcon  !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(lsedtot)                               , intent(in)  :: namsed  !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nsrc)                                                :: namsrc  !  Description and declaration in esm_alloc_char.f90
    character(21)                                                   , intent(in)  :: selmap  !  Description and declaration in tricom.igs
    character(8)                                                    , intent(in)  :: dpsopt  !  Description and declaration in numeco.igs
!
! Local variables
!
    integer                                        :: i
    integer                                        :: ierror ! Local errorflag for NEFIS files 
    integer                                        :: k
    integer                                        :: l
    integer                                        :: lhlp   ! Help variable for teller constituents and turbulent quantities 
    integer                                        :: lsedbl ! Number of bed load fractions: lsedtot-lsed
    integer                                        :: m      ! Help variable 
    integer                                        :: n      ! Help variable 
    integer        , dimension(1)                  :: idummy ! Help array to read/write Nefis files 
    integer        , dimension(2)                  :: ival   ! Local array for writing ITDATE and time (:= 00:00:00) 
    integer        , dimension(nelmx)              :: nbytsg ! Array containing the number of by- tes of each single ELMTPS 
    integer                           , external   :: neferr
    logical                                        :: wrswch ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
    real(fp)       , dimension(1)                  :: rdummy ! Help array to read/write Nefis files 
    character(10)  , dimension(nelmx)              :: elmunt ! Array with element physical unit 
    character(16)                                  :: grnam2 ! Data-group name defined for the NEFIS-files 
    character(16)  , dimension(1)                  :: cdum16 ! Help array to read/write Nefis files 
    character(16)  , dimension(nelmx)              :: elmnms ! Element name defined for the NEFIS-files 
    character(16)  , dimension(nelmx)              :: elmqty ! Array with element quantity 
    character(16)  , dimension(nelmx)              :: elmtps ! Array containing the types of the elements (real, ch. , etc. etc.) 
    character(20)  , dimension(:)     ,allocatable :: namhlp ! Help array for name constituents and turbulent quantities 
    character(21)  , dimension(1)                  :: cdum21 ! Help array to read/write Nefis files 
    character(256)                                 :: filnam ! Help var. for FLOW file name 
    character(8)   , dimension(1)                  :: cdum8  ! Help array to read/write Nefis files 
    character(256)                                 :: errmsg ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(64)  , dimension(nelmx)              :: elmdes ! Array with element description 
!
! Data statements
!
    data grnam2/'map-const'/
    data elmnms/'ITDATE', 'TZONE'  ,'TUNIT'  , 'DT'    , 'SIMDAT', &
              & 'SELMAP', 'NMAX'  , 'MMAX'   , 'KMAX'  , 'LSTCI' , &
              & 'LTUR'  , 'NOSTAT', 'NSRC'   , 'NTRUV' , 'GRDANG', &
              & 'XCOR'  , 'YCOR'  , 'XZ'     , 'YZ'    , 'ALFAS' , &
              & 'KCU'   , 'KCV'   , 'KCS'    , 'DP0'   , 'DPS0'  , &
              & 'DPU0'  , 'DPV0'  , &
              & 'DRYFLP', 'NOROW' ,'NOROCO'  , 'IROCOL', 'THICK' , &
              & 'NAMCON', 'MNSTAT','NAMST'   , 'NAMSRC', 'MNTRA' , &
              & 'NAMTRA', 'LSED'  ,'LSEDBL'  , 'NAMSED', 'ZK'    , &
              & 'COORDINATES', 'LAYER_MODEL' , 'GSQS'/
    data elmqty/nelmx*' '/
    data elmunt/  '[YYYYMMDD]',   '[ HOUR  ]', '[   S   ]', 11*'[   -   ]', &  
              &   '[  DEG  ]' , 4*'[   M   ]', '[  DEG  ]',  3*'[   -   ]', &
              & 4*'[   M   ]' , 4*'[   -   ]', '[ .01*% ]',  9*'[   -   ]', &
              &   '[   M   ]' , 2*'[   -   ]', '[  M2   ]'/
    data elmtps/'INTEGER', 3*'REAL', 2*'CHARACTER', 8*'INTEGER', 6*'REAL',      &
        & 3*'INTEGER', 4*'REAL', 'CHARACTER', 3*'INTEGER', 'REAL', 'CHARACTER',    &
        & 'INTEGER', 'CHARACTER', 'CHARACTER', 'INTEGER', 'CHARACTER', &
        & 2*'INTEGER', 'CHARACTER', 'REAL', 2*'CHARACTER', 'REAL'/
    data nbytsg/4*4, 16, 21, 21*4, 8, 4*4, 20, 4, 20, 20, 4, 20, 2*4, 20, 4, 2*16, 4/
    data (elmdes(i), i = 1, 10)                                                  &
         & /'Initial date (input) & time (default 00:00:00)               ',     &
         & 'Local time zone                                               ',     &
         & 'Time scale related to seconds                                 ',     &
         & 'Time step (DT*TUNIT sec)                                      ',     &
         & 'Simulation date and time [YYYYMMDD  HHMMSS]                   ',     &
         & 'Selection flag for field values (2dH, 1dV & 2dV)              ',     &
         & 'Number of N-grid points                                       ',     &
         & 'Number of M-grid points                                       ',     &
         & 'Number of layers                                              ',     &
         & 'Number of constituents                                        '/
    data (elmdes(i), i = 11, 20)                                                 &
         & /'Number of turbulence quantities                              ',     &
         & 'Number of monitoring stations                                 ',     &
         & 'Number of discharge                                           ',     &
         & 'Number of monitoring cross-sections                           ',     &
         & 'Edge between y-axis and real north                            ',     &
         & 'X-coord. bottom point in local system                         ',     &
         & 'Y-coord. bottom point in local system                         ',     &
         & 'X-coord. zeta point in local system                           ',     &
         & 'Y-coord. zeta point in local system                           ',     &
         & 'Orientation ksi-axis w.r.t. pos.x-axis at water level point   '/
    data (elmdes(i), i = 21, 30)                                                 &
         & /'Mask array for U-velocity points                             ',     &
         & 'Mask array for V-velocity points                              ',     &
         & 'Non-active/active water-level point                           ',     &
         & 'Initial bottom depth (positive down)                          ',     &
         & 'Initial bottom depth at zeta points (positive down)           ',     &
         & 'Initial bottom depth at u points (positive down)              ',     &
         & 'Initial bottom depth at v points (positive down)              ',     &
         & 'Criterium to calculate depth in zeta points                   ',     &
         & 'Number of rows for IROCOL table                               ',     &
         & 'Number of rows & columns of IROCOL table                      '/
    data (elmdes(i), i = 31, 40)                                                 &
         & /'Administration of zeta points                                ',     &
         & 'Fraction part of layer thickness of total water-height        ',     &
         & 'Name of constituent & turbulent quantity                      ',     &
         & '(M,N) indices of monitoring stations                          ',     &
         & 'Name of monitoring station                                    ',     &
         & 'Name of discharge source                                      ',     &
         & '(M1,N1)-(M2,N2) indices of monitoring cross-sections          ',     &
         & 'Name of monitoring cross-section                              ',     &
         & 'Number of sediment constituents                               ',     &
         & 'Number of bedload sediment fractions                          '/
    data (elmdes(i), i = 41, 45)                                                 &
         & /'Name of sediment fraction                                    ',     &
         & 'Vertical coordinates of cell interfaces                       ',     &
         & 'Cartesian or Spherical coordinates                            ',     &
         & 'Sigma-model or Z-model                                        ',     &
         & 'Horizontal area of computational cell                         '/
!
!! executable statements -------------------------------------------------------
!
    mnit       => gdp%gdstations%mnit
    mnstat     => gdp%gdstations%mnstat
    namst      => gdp%gdstations%namst
    namtra     => gdp%gdstations%namtra
    nefiselem  => gdp%nefisio%nefiselem(nefiswrimap)
    first      => nefiselem%first
    celidt     => nefiselem%celidt
    elmdms     => nefiselem%elmdms
    !
    ! LSTSCI var. name in MAP FILE must remain LSTCI for GPP to work
    ! properly
    !
    !
    ! Initialize local variables
    !
    ierror = 0
    celidt = 1
    lsedbl = lsedtot - lsed
    !
    filnam = trifil(1:3) // 'm' // trifil(5:)
    errmsg = ' '
    wrswch = .true.
    !
    ! Redefine elmunt for sferic coordinates
    !
    if (sferic) then
       elmunt(15) = '[  DEG  ]'
       elmunt(16) = '[  DEG  ]'
       elmunt(17) = '[  DEG  ]'
       elmunt(18) = '[  DEG  ]'
    endif
    !
    ! Set up the element dimensions
    !
    if (first) then
       first = .false.
       i = 1   ! 'ITDATE'
       call filldm(elmdms    ,i         ,1         ,2         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'TZONE'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'TUNIT'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'DT'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'SIMDAT'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'SELMAP'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'NMAX'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'MMAX'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'KMAX'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'LSTCI'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'LTUR'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1  ! 'NOSTAT'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'NSRC'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'NTRUV'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'GRDANG'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'XCOR'
       call filldm(elmdms    ,i         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'YCOR'
       call filldm(elmdms    ,i         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'XZ'
       call filldm(elmdms    ,i         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'YZ'
       call filldm(elmdms    ,i         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'ALFAS'
       call filldm(elmdms    ,i         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'KCU'
       call filldm(elmdms    ,i         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'KCV'
       call filldm(elmdms    ,i         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'KCS'
       call filldm(elmdms    ,i         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'DP0'
       call filldm(elmdms    ,i         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'DPS0'
       call filldm(elmdms    ,i         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'DPU0'
       call filldm(elmdms    ,i         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'DPV0'
       call filldm(elmdms    ,i         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'DRYFLP'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'NOROW'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'NOROCO'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'IROCOL'
       call filldm(elmdms    ,i         ,2         ,5         ,noroco    , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'THICK'
       call filldm(elmdms    ,i         ,1         ,kmax      ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'NAMCON'
       lhlp = 0
       if (index(selmap(6:13), 'Y')/=0) lhlp = lhlp + lstsci
       if (index(selmap(14:15), 'Y')/=0) lhlp = lhlp + ltur
       lhlp = max(1, lhlp)
       call filldm(elmdms    ,i         ,1         ,lhlp      ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'MNSTAT'
       if (nostat>0) then
          call filldm(elmdms    ,i         ,2         ,2         ,nostat    , &
                    & 0         ,0         ,0         )
               ! 'NAMST'
          call filldm(elmdms    ,i+1       ,1         ,nostat    ,0         , &
                    & 0         ,0         ,0         )
       endif
       i = i+2 ! 'NAMSRC'
       if (nsrc>0) then
          call filldm(elmdms    ,i         ,1         ,nsrc      ,0         , &
                    & 0         ,0         ,0         )
       endif
       i = i+1 ! 'MNTRA'
       if (ntruv>0) then
          call filldm(elmdms    ,i         ,2         ,4         ,ntruv     , &
                    & 0         ,0         ,0         )
               ! 'NAMTRA'
          call filldm(elmdms    ,i+1       ,1         ,ntruv     ,0         , &
                    & 0         ,0         ,0         )
       endif
       i = i+2 ! 'LSED'
       if (lsed>0) then
          call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                    & 0         ,0         ,0         )
       endif
       i = i+1 ! 'LSEDBL'
       if (lsedbl>0) then
          call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                    & 0         ,0         ,0         )
       endif
       i = i+1 ! 'NAMSED'
       if (lsedtot>0) then
          call filldm(elmdms    ,i         ,1        ,lsedtot    ,0         , &
                    & 0         ,0         ,0         )
       endif
       i = i+1 ! 'ZK'
       if (zmodel) then
          call filldm(elmdms    ,i         ,1         ,kmax + 1  ,0         , &
                    & 0         ,0         ,0         )
       endif
       i = i+1 ! 'COORDINATES'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'LAYER_MODEL'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'GSQS'
       call filldm(elmdms    ,i         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
    endif
    !
    ! group 2, element 'ITDATE'
    !
    ival(1) = itdate
    ival(2) = 000000
    !
    i = 1
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,ival      )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'TZONE'
    !
    i = i+1
    rdummy(1) = tzone
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,rdummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'TUNIT'
    !
    i = i+1
    rdummy(1) = tunit
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,rdummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'DT'
    !
    i = i+1
    rdummy(1) = dt
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,rdummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'SIMDAT'
    !
    i = i+1
    cdum16(1) = simdat
    call putgtc(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,cdum16    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'SELMAP'
    !
    i = i+1
    cdum21(1) = selmap
    call putgtc(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,cdum21    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'NMAX'
    !
    i = i+1
    idummy(1) = nmaxus
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,idummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'MMAX'
    !
    i = i+1
    idummy(1) = mmax
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,idummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'KMAX'
    !
    i = i+1
    idummy(1) = kmax
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,idummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'LSTCI' Variable is now LSTSCI
    !
    i = i+1
    idummy(1) = 0
    if (index(selmap(6:13), 'Y')/=0 .and. lstsci>0) idummy(1) = lstsci
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,idummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'LTUR'
    !
    i = i+1
    idummy(1) = 0
    if (index(selmap(14:15), 'Y')/=0 .and. ltur>0) idummy(1) = ltur
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,idummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'NOSTAT'
    !
    i = i+1
    idummy(1) = nostat
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,idummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'NSRC'
    !
    i = i+1
    idummy(1) = nsrc
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,idummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'NTRUV'
    !
    i = i+1
    idummy(1) = ntruv
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,idummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'GRDANG'
    !
    i = i+1
    rdummy(1) = grdang
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,rdummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'XCOR'
    !
    i = i+1
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = xcor(n, m)
       enddo
    enddo
    !
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,rbuff     )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'YCOR'
    !
    i = i+1
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = ycor(n, m)
       enddo
    enddo
    !
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,rbuff     )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'XZ'
    !
    i = i+1
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = xz(n, m)
       enddo
    enddo
    !
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,rbuff     )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'YZ'
    !
    i = i+1
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = yz(n, m)
       enddo
    enddo
    !
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,rbuff     )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'ALFAS'
    !
    i = i+1
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = alfas(n, m)
       enddo
    enddo
    !
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,rbuff     )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'KCU'
    !
    i = i+1
    do m = 1, mmax
       do n = 1, nmaxus
          ibuff(n, m) = kcu(n, m)
       enddo
    enddo
    !
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,ibuff     )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'KCV'
    !
    i = i+1
    do m = 1, mmax
       do n = 1, nmaxus
          ibuff(n, m) = kcv(n, m)
       enddo
    enddo
    !
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,ibuff     )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'KCS'
    !
    i = i+1
    do m = 1, mmax
       do n = 1, nmaxus
          ibuff(n, m) = kcs(n, m)
       enddo
    enddo
    !
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,ibuff     )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'DP0'
    !
    i = i+1
    if (dpsopt == 'DP') then
       do m = 1, mmax
          do n = 1, nmaxus
             rbuff(n, m) = real(dps(n, m),fp)
          enddo
       enddo
    else
       do m = 1, mmax
          do n = 1, nmaxus
             rbuff(n, m) = dp(n, m)
          enddo
       enddo
    endif
    !
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,rbuff     )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'DPS0'
    !
    i = i+1
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = real(dps(n, m),fp)
       enddo
    enddo
    !
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,rbuff     )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'DPU0'
    !
    i = i+1
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = real(dpu(n, m),fp)
       enddo
    enddo
    !
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,rbuff     )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'DPV0'
    !
    i = i+1
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = real(dpv(n, m),fp)
       enddo
    enddo
    !
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,rbuff     )
    if (ierror/=0) goto 999
    !
    ! The necessary information is currently held by DPSOPT but
    ! for backward compatibility the quantity is still called
    ! DRYFLP on the TRIM file.
    !
    ! group 2, element 'DRYFLP'
    !
    i = i+1
    cdum8(1) = dpsopt
    call putgtc(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,cdum8     )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'NOROW'
    !
    i = i+1
    idummy(1) = norow
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,idummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'NOROCO'
    !
    i = i+1
    idummy(1) = noroco
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,idummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'IROCOL'
    !
    i = i+1
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,irocol    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'THICK'
    !
    i = i+1
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,thick     )
    if (ierror/=0) goto 999
    !
    ! group 2, only if lmax   > 0  (:= SELMAP( 6:15) <> 'NNNNNNNNNN')
    !
    i = i+1
    if (index(selmap(6:15), 'Y')/=0) then
       allocate(namhlp(lstsci+ltur))
       lhlp = 0
       if (index(selmap(6:13), 'Y')>0) then
          do l = 1, lstsci
             namhlp(l) = namcon(l)
          enddo
          lhlp = lhlp + lstsci
       endif
       if (index(selmap(14:15), 'Y')>0) then
          do l = 1, ltur
             namhlp(lhlp + l) = namcon(lstsci + l)
          enddo
       endif
       !
       ! group 2, element 'NAMCON'
       !
       call putgtc(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i) ,celidt    ,wrswch    ,ierror   ,namhlp    )
       if (ierror/=0) goto 999
       deallocate(namhlp)
    endif
    !
    ! group 2, only if nostat > 0
    !
    i = i+1
    if (nostat>0) then
       !
       ! group 2, element 'MNSTAT'
       !
       call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i) ,celidt    ,wrswch    ,ierror   ,mnstat    )
       if (ierror/=0) goto 999
       !
       ! group 2, element 'NAMST'
       !
       call putgtc(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i+1) ,celidt    ,wrswch    ,ierror   ,namst     )
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, only if nsrc   > 0
    !
    i = i+2
    if (nsrc>0) then
       !
       ! group 2, element 'NAMSRC'
       !
       call putgtc(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i) ,celidt    ,wrswch    ,ierror   ,namsrc    )
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, only if ntruv  > 0
    !
    i = i+1
    if (ntruv>0) then
       !
       ! group 2, element 'MNTRA'
       !
       call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i) ,celidt    ,wrswch    ,ierror   ,mnit      )
       if (ierror/=0) goto 999
       !
       ! group 2, element 'NAMTRA'
       !
       call putgtc(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i+1) ,celidt    ,wrswch    ,ierror   ,namtra    )
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, element 'LSED'
    !
    i = i+2
    if (lsed>0) then
       idummy(1) = lsed
       call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i) ,celidt    ,wrswch    ,ierror   ,idummy    )
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, element 'LSEDBL'
    !
    i = i+1
    if (lsedbl>0) then
       idummy(1) = lsedbl
       call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i) ,celidt    ,wrswch    ,ierror   ,idummy    )
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, element 'NAMCON'
    !
    i = i+1
    if (lsedtot>0) then
       call putgtc(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i) ,celidt    ,wrswch    ,ierror   ,namsed    )
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, element 'ZK'
    !
    i = i+1
    if (zmodel) then
       do k = 1, kmax
          rbuff1(k + 1) = zk(k)
       enddo
       rbuff1(1) = zbot
       call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i) ,celidt    ,wrswch    ,ierror   ,rbuff1    )
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, element 'COORDINATES'
    !
    i = i+1
    if (sferic) then
       cdum16(1) = 'SPHERICAL'
    else
       cdum16(1) = 'CARTESIAN'
    endif
    call putgtc(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,cdum16    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'LAYER_MODEL'
    !
    i = i+1
    if (zmodel) then
       cdum16(1) = 'Z-MODEL'
    else
       cdum16(1) = 'SIGMA-MODEL'
    endif
    call putgtc(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,cdum16    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'GSQS'
    !
    i = i+1
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = real(gsqs(n, m),fp)
       enddo
    enddo
    !
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror   ,rbuff     )
    if (ierror/=0) goto 999
    !
    ! write errormessage if error occurred and set error = .true.
    ! the files will be closed in clsnef (called in triend)
    !
    !
  999 continue
    if (ierror/= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrimap
