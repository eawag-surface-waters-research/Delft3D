subroutine wrihis(lundia    ,error     ,trifil    ,selhis    ,simdat    , &
                & itdate    ,tzone     ,tunit     ,dt        ,nostat    , &
                & ntruv     ,nmax      ,mmax      ,kmax      ,lmax      , &
                & lstsci    ,ltur      ,grdang    ,sferic    ,lsed      , &
                & lsedtot   ,zbot      ,zmodel    ,namcon    ,namsed    , &
                & xz        ,yz        ,alfas     ,dps       ,thick     , &
                & zk        ,rbuff     ,rbuffc    ,rbuffz    ,gdp       )
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
!    Function: Writes the initial group 2 ('his-const') to
!              HIS-DAT
!              Selection is done using SELHIS. For elements like
!              NAMCON where LMAX must be > 0 this coupling between
!              LMAX and SELHIS is done in subroutine RDPRFL
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
    integer       , dimension(:)    , pointer :: line_orig    
    integer       , dimension(:, :) , pointer :: mnit
    integer       , dimension(:, :) , pointer :: mnstat
    character(20) , dimension(:)    , pointer :: namst
    character(20) , dimension(:)    , pointer :: namtra
    logical                         , pointer :: first
    integer                         , pointer :: celidt
    integer       , dimension(:, :) , pointer :: elmdms
    type (nefiselement)             , pointer :: nefiselem
    real(fp)      , dimension(:, :) , pointer :: xystat
!
! Local parameters
!
    integer, parameter :: nelmx = 28
!
! Global variables
!
    integer                                                             , intent(in)  :: itdate !  Description and declaration in exttim.igs
    integer                                                                           :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)  :: lmax   !  Description and declaration in dimens.igs
    integer                                                             , intent(in)  :: lsed   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)  :: lsedtot!  Description and declaration in iidim.f90
    integer                                                             , intent(in)  :: lstsci !  Description and declaration in iidim.f90
    integer                                                             , intent(in)  :: ltur   !  Description and declaration in iidim.f90
    integer                                                                           :: lundia !  Description and declaration in inout.igs
    integer                                                                           :: mmax   !  Description and declaration in iidim.f90
    integer                                                                           :: nmax   !  Description and declaration in iidim.f90
    integer                                                                           :: nostat !  Description and declaration in dimens.igs
    integer                                                                           :: ntruv  !  Description and declaration in dimens.igs
    logical                                                             , intent(out) :: error  !  Flag=TRUE if an error is encountered
    logical                                                             , intent(in)  :: sferic !  Description and declaration in tricom.igs
    logical                                                             , intent(in)  :: zmodel !  Description and declaration in procs.igs
    real(fp)                                                            , intent(in)  :: dt     !  Description and declaration in rjdim.f90
    real(fp)                                                            , intent(in)  :: grdang !  Description and declaration in tricom.igs
    real(fp)                                                            , intent(in)  :: tunit  !  Description and declaration in exttim.igs
    real(fp)                                                            , intent(in)  :: tzone  !  Description and declaration in exttim.igs
    real(fp)                                                            , intent(in)  :: zbot   !  Description and declaration in zmodel.igs
    real(fp)      , dimension(4, ntruv)                                               :: rbuffc !!  Help arrays for writing NEFIS files
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: alfas  !  Description and declaration in rjdim.f90
    real(prec)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: dps    !  Description and declaration in rjdim.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: xz     !  Description and declaration in rjdim.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: yz     !  Description and declaration in rjdim.f90
    real(fp)      , dimension(kmax + 1)                                               :: rbuffz
    real(fp)      , dimension(kmax)                                                   :: thick  !  Description and declaration in rjdim.f90
    real(fp)      , dimension(0:kmax)                                   , intent(in)  :: zk     !  Vertical coordinates of cell interfaces
                                                                                                !  Flag for activation of Z-MODEL
    real(fp)      , dimension(nostat)                                                 :: rbuff  !  Description and declaration in r-i-ch.igs
    character(*)                                                        , intent(in)  :: trifil !  File name for FLOW NEFIS output
                                                                                                !  files (tri"h/m"-"casl""labl".dat/def)
    character(16)                                                       , intent(in)  :: simdat !  Simulation date representing the flow condition at this date
    character(20) , dimension(lmax)                                     , intent(in)  :: namcon !  Description and declaration in ckdim.f90
    character(20) , dimension(lsedtot)                                  , intent(in)  :: namsed !  Description and declaration in ckdim.f90
    character(23)                                                       , intent(in)  :: selhis !  Description and declaration in tricom.igs
!
! Local variables
!
    integer                                          :: i           ! Help var. 
    integer                                          :: ierror      ! Local errorflag for NEFIS files 
    integer                                          :: k      
    integer                                          :: l      
    integer                                          :: lhlp        ! Help var. for teller constituents and turbulent quantities 
    integer                                          :: lsedbl      ! Number of bed load fractions: lsedtot-lsed
    integer                                          :: m           ! Help var. 
    integer                                          :: m1     
    integer                                          :: m2     
    integer                                          :: n           ! Help var. 
    integer                                          :: n1     
    integer                                          :: n2     
    integer        , dimension(:,:)     ,allocatable :: ibuff       ! Help array for (n,m)-coordinates of cross section locations
    integer        , dimension(1)                    :: idummy      ! Help array to read/write Nefis files 
    integer        , dimension(2)                    :: ival        ! Local array for writing ITDATE and time (:= 00:00:00) 
    integer        , dimension(nelmx)                :: nbytsg      ! Array containing the number of by- tes of each single ELMTPS 
    integer                             , external   :: neferr
    logical                                          :: wrswch      ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
    real(fp)       , dimension(1)                    :: rdummy      ! Help array to read/write Nefis files 
    character(20)  , dimension(ntruv)                :: cbuff       ! Help array for names of cross section locations
    character(10)  , dimension(nelmx)                :: elmunt      ! Array with element physical unit 
    character(16)                                    :: grnam2      ! Data-group name defined for the NEFIS-files 
    character(16)  , dimension(1)                    :: cdum16      ! Help array to read/write Nefis files 
    character(16)  , dimension(nelmx)                :: elmnms      ! Element name defined for the NEFIS-files 
    character(16)  , dimension(nelmx)                :: elmqty      ! Array with element quantity 
    character(16)  , dimension(nelmx)                :: elmtps      ! Array containing the types of the elements (real, ch. , etc. etc.) 
    character(20)  , dimension(:)       ,allocatable :: namhlp      ! Help array for name constituents and turbulent quantities 
    character(23)  , dimension(1)                    :: cdum23      ! Help array to read/write Nefis files 
    character(256)                                   :: filnam      ! Help var. for FLOW file name 
    character(256)                                   :: errmsg      ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(64)  , dimension(nelmx)                :: elmdes      ! Array with element description 
    character(20)  , dimension(ntruv)                :: namtra_help
!
! Data statements
!
    data grnam2/'his-const'/
    data elmnms/'ITDATE', 'TZONE'  ,'TUNIT' , 'DT'    , 'SIMDAT' , &
              & 'SELHIS', 'NOSTAT', 'NTRUV' , 'LSTCI' , 'LTUR'   , &
              & 'KMAX'  , 'MNSTAT', 'XYSTAT', 'NAMST' , 'GRDANG' , &
              & 'ALFAS' , 'DPS'   , 'THICK' , 'MNTRA' , 'XYTRA'  , & 
              & 'NAMTRA', 'NAMCON', 'LSED'  , 'LSEDBL', 'NAMSED' , &
              & 'ZK'    , 'COORDINATES', 'LAYER_MODEL'/
    data elmqty/nelmx*' '/
    data elmunt/  '[YYYYMMDD]',   '[ HOUR  ]',    '[   S   ]', 9*'[   -   ]', &
              &   '[   M   ]',    '[   -   ]' , 2*'[  DEG  ]',   '[   M   ]', &
              &   '[ .01*% ]',    '[   -   ]' ,   '[   M   ]', 5*'[   -   ]', &
              &   '[   M   ]',  2*'[   -   ]'/
    data elmtps/'INTEGER', 3*'REAL', 2*'CHARACTER', 6*'INTEGER', 'REAL',        &
        & 'CHARACTER', 4*'REAL', 'INTEGER', 'REAL', 2*'CHARACTER', 2*'INTEGER', &
        & 'CHARACTER', 'REAL', 2*'CHARACTER'/
    data nbytsg/4*4, 16, 23, 7*4, 20, 6*4, 2*20, 2*4, 20, 4, 2*16/
    data (elmdes(i), i = 1, 10)                                                  &
         & /'Initial date (input) & time (default 00:00:00)               ',     &
         & 'Local time zone                                               ',     &
         & 'Time scale related to seconds                                 ',     &
         & 'Time step (DT*TUNIT sec)                                      ',     &
         & 'Simulation date and time [YYYYMMDD  HHMMSS]                   ',     &
         & 'Selection flag for time histories                             ',     &
         & 'Number of monitoring stations                                 ',     &
         & 'Number of monitoring cross-sections                           ',     &
         & 'Number of constituents                                        ',     &
         & 'Number of turbulence quantities                               '/
    data (elmdes(i), i = 11, 20)                                                 &
         & /'Number of layers                                             ',     &
         & '(M,N) indices of monitoring stations                          ',     &
         & '(X,Y) coordinates of monitoring stations                      ',     &
         & 'Name of monitoring station                                    ',     &
         & 'Edge between y-axis and real north                            ',     &
         & 'Orientation ksi-axis w.r.t. pos.x-axis at water level point   ',     &
         & 'Depth in station                                              ',     &
         & 'Fraction part of layer thickness of total water-height        ',     &
         & '(M1,N1)-(M2,N2) indices of monitoring cross-sections          ',     &
         & '(X1,Y1)-(X2,Y2) coordinates of monitoring cross-sections      '/
    data (elmdes(i), i = 21, 28)                                                 &
         & /'Name of monitoring cross-section                             ',     &
         & 'Name of constituents / turbulent quantities                   ',     &
         & 'Number of sediment constituents                               ',     &
         & 'Number of bedload sediment fractions                          ',     &
         & 'Name of sediment fraction                                     ',     &
         & 'Vertical coordinates of cell interfaces                       ',     &
         & 'Cartesian or Spherical coordinates                            ',     &
         & 'Sigma-model or Z-model                                        '/
!
!! executable statements -------------------------------------------------------
!
    line_orig  => gdp%gdstations%line_orig
    mnit       => gdp%gdstations%mnit
    mnstat     => gdp%gdstations%mnstat
    namst      => gdp%gdstations%namst
    namtra     => gdp%gdstations%namtra
    nefiselem  => gdp%nefisio%nefiselem(nefiswrihis)
    first      => nefiselem%first
    celidt     => nefiselem%celidt
    elmdms     => nefiselem%elmdms
    xystat     => gdp%gdstations%xystat
    !
    ! LSTSCI var. name in HIS FILE must remain LSTCI for GPP to work
    ! properly
    !
    !
    ! Initialize local variables
    !
    ierror = 0
    celidt = 1
    lsedbl = lsedtot - lsed
    !
    filnam = trifil(1:3) // 'h' // trifil(5:)
    errmsg = ' '
    wrswch = .true.
    !
    ! Redefine elmunt for sferic coordinates
    !
    if (sferic) then
       elmunt(12) = '[  DEG  ]'
       elmunt(19) = '[  DEG  ]'
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
       i = i+1 ! 'SELHIS'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'NOSTAT'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'NTRUV'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'LSTCI'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'LTUR'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'KMAX'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'MNSTAT'
       if (nostat>0) then
          call filldm(elmdms    ,i         ,2         ,2         ,nostat    , &
                    & 0         ,0         ,0         )
               ! 'XYSTAT'
          call filldm(elmdms    ,i+1       ,2         ,2         ,nostat    , &
                    & 0         ,0         ,0         )
               ! 'NAMST'
          call filldm(elmdms    ,i+2       ,1         ,nostat    ,0         , &
                    & 0         ,0         ,0         )
       endif
       i = i+3 ! 'GRDANG'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'ALFAS'
       if (nostat>0) then
          call filldm(elmdms    ,i         ,1         ,nostat    ,0         , &
                    & 0         ,0         ,0         )
               ! 'DPS'
          call filldm(elmdms    ,i+1       ,1         ,nostat    ,0         , &
                    & 0         ,0         ,0         )
       endif
       i = i+2 ! 'THICK'
       call filldm(elmdms    ,i         ,1         ,kmax      ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'MNTRA'
       if (ntruv>0) then
          call filldm(elmdms    ,i         ,2         ,4         ,ntruv     , &
                    & 0         ,0         ,0         )
               ! 'XYTRA'
          call filldm(elmdms    ,i+1       ,2         ,4         ,ntruv     , &
                    & 0         ,0         ,0         )
               ! 'NAMTRA'
          call filldm(elmdms    ,i+2       ,1         ,ntruv     ,0         , &
                    & 0         ,0         ,0         )
       endif
       i = i+3 ! 'NAMCON'
       lhlp = 0
       if (index(selhis(5:12), 'Y')/=0 .or. index(selhis(22:23), 'Y')/=0) &
        & lhlp = lhlp + lstsci
       if (index(selhis(13:14), 'Y')/=0) lhlp = lhlp + ltur
       lhlp = max(1, lhlp)
       call filldm(elmdms    ,i         ,1         ,lhlp      ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'LSED'
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
          call filldm(elmdms    ,i         ,1        ,lsedtot   ,0         , &
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
    endif
    !
    ! group 2, element 'ITDATE'
    !
    i = 1
    ival(1) = itdate
    ival(2) = 000000
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror    ,ival      )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'TZONE'
    !
    i = i+1
    rdummy(1) = tzone
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror    ,rdummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'TUNIT'
    !
    i = i+1
    rdummy(1) = tunit
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror    ,rdummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'DT'
    !
    i = i+1
    rdummy(1) = dt
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror    ,rdummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'SIMDAT'
    !
    i = i+1
    cdum16(1) = simdat
    call putgtc(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror    ,cdum16    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'SELHIS'
    !
    i = i+1
    cdum23(1) = selhis
    call putgtc(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror    ,cdum23    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'NOSTAT'
    !
    i = i+1
    idummy(1) = nostat
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror    ,idummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'NTRUV'
    !
    i = i+1
    idummy(1) = ntruv
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror    ,idummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'LSTCI' Variable is now LSTSCI
    !
    i = i+1
    idummy(1) = 0
    if ((index(selhis(5:12), 'Y')/=0 .or. index(selhis(22:23), 'Y')/=0) .and.   &
      & lstsci>0) idummy(1) = lstsci
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror    ,idummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'LTUR'
    !
    i = i+1
    idummy(1) = 0
    if (index(selhis(13:14), 'Y')/=0 .and. ltur>0) idummy(1) = ltur
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror    ,idummy    )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'KMAX'
    !
    i = i+1
    idummy(1) = kmax
    call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror    ,idummy    )
    if (ierror/=0) goto 999
    !
    ! only if nostat > 0 (next 3 elements)
    !
    i = i+1
    if (nostat>0) then
       !
       ! group 2, element 'MNSTAT'
       !
       call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i) ,celidt    ,wrswch    ,ierror    ,mnstat    )
       if (ierror/=0) goto 999
       !
       ! group 2, element 'XYSTAT'
       !
       do k = 1, nostat
          m = mnstat(1, k)
          n = mnstat(2, k)
          xystat(1, k) = xz(n, m)
          xystat(2, k) = yz(n, m)
       enddo
       !
       call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i+1),celidt   ,wrswch    ,ierror    ,xystat    )
       if (ierror/=0) goto 999
       !
       ! group 2, element  'NAMST'
       !
       call putgtc(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i+2) ,celidt    ,wrswch    ,ierror    ,namst     )
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, element 'GRDANG'
    !
    i = i+3
    rdummy(1) = grdang
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror    ,rdummy    )
    if (ierror/=0) goto 999
    !
    ! only if nostat > 0 (next 2 elements)
    !
    i = i+1
    if (nostat>0) then
       !
       ! group 2, element 'ALFAS'
       !
       do k = 1, nostat
          m = mnstat(1, k)
          n = mnstat(2, k)
          rbuff(k) = alfas(n, m)
       enddo
       !
       call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i) ,celidt    ,wrswch    ,ierror    ,rbuff     )
       if (ierror/=0) goto 999
       !
       ! group 2, element 'DPS'
       !
       do k = 1, nostat
          m = mnstat(1, k)
          n = mnstat(2, k)
          rbuff(k) = real(dps(n, m),fp)
       enddo
       !
       call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i+1) ,celidt    ,wrswch    ,ierror    ,rbuff     )
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, element 'THICK'
    !
    i = i+2
    call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierror    ,thick     )
    if (ierror/=0) goto 999
    !
    ! only if ntruv  > 0
    ! the next element of group 3 will be written
    !
    i = i+1
    if (ntruv>0) then
       !
       ! group 2, element 'MNTRA'
       !
       allocate(ibuff(4,ntruv))
       ibuff = 0
       do k = 1, ntruv
          ibuff( 1, line_orig(k) ) = mnit(1, k)
          ibuff( 2, line_orig(k) ) = mnit(2, k)
          ibuff( 3, line_orig(k) ) = mnit(3, k)
          ibuff( 4, line_orig(k) ) = mnit(4, k)
       enddo
       !
       call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i) ,celidt    ,wrswch    ,ierror    ,ibuff      )
       deallocate(ibuff)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'XYTRA'
       !
       do k = 1, ntruv
          m1 = mnit(1,k)
          n1 = mnit(2,k)
          m2 = mnit(3,k)
          n2 = mnit(4,k)
          rbuffc( 1, line_orig(k) ) = xz(n1, m1)
          rbuffc( 2, line_orig(k) ) = yz(n1, m1)
          rbuffc( 3, line_orig(k) ) = xz(n2, m2)
          rbuffc( 4, line_orig(k) ) = yz(n2, m2)
          cbuff (    line_orig(k) ) = namtra( k)   
       enddo
       !
       call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i+1) ,celidt    ,wrswch    ,ierror    ,rbuffc    )
       if (ierror/=0) goto 999
       !
       ! group 2, element 'NAMTRA'
       !
       call putgtc(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i+2) ,celidt    ,wrswch    ,ierror    ,cbuff    )
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, only if lmax   > 0 (:= selhis( 5:14) <> 'NNNNNNNNNN')
    !
    i = i+3
    if (index(selhis(5:14), 'Y')>0 .or. index(selhis(22:23), 'Y')>0) then
       allocate(namhlp(lstsci+ltur))
       lhlp = 0
       if (index(selhis(5:12), 'Y')>0 .or. index(selhis(22:23), 'Y')/=0) then
          do l = 1, lstsci
             namhlp(l) = namcon(l)
          enddo
          lhlp = lhlp + lstsci
       endif
       if (index(selhis(13:14), 'Y')>0) then
          do l = 1, ltur
             namhlp(lhlp + l) = namcon(lstsci + l)
          enddo
       endif
       !
       ! group 2, element 'NAMCON'
       !
       call putgtc(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i) ,celidt    ,wrswch    ,ierror    ,namhlp    )
       if (ierror/=0) goto 999
       deallocate(namhlp)
    endif
    !
    ! group 2, element 'LSED'
    !
    i = i+1
    if (lsed>0) then
       idummy(1) = lsed
       call putgti(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i) ,celidt    ,wrswch    ,ierror    ,idummy    )
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
                 & elmnms(i) ,celidt    ,wrswch    ,ierror    ,idummy    )
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, element 'NAMCON'
    !
    i = i+1
    if (lsedtot>0) then
       call putgtc(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i) ,celidt    ,wrswch    ,ierror    ,namsed    )
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, element 'ZK'
    !
    i = i+1
    if (zmodel) then
       do k = 1, kmax
          rbuffz(k + 1) = zk(k)
       enddo
       rbuffz(1) = zbot
       call putgtr(filnam    ,grnam2    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(i) ,celidt    ,wrswch    ,ierror    ,rbuffz    )
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
              & elmnms(i) ,celidt    ,wrswch    ,ierror    ,cdum16    )
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
              & elmnms(i) ,celidt    ,wrswch    ,ierror    ,cdum16    )
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
end subroutine wrihis
