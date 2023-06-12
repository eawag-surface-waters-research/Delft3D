module m_rdmorlyr
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2023.                                
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
!  
!  
!-------------------------------------------------------------------------------
use m_depfil_stm
contains

subroutine rdmorlyr(lundia    ,error     ,filmor    , &
                  & nmaxus    ,nto       ,lfbedfrm  ,nambnd    ,version   , &
                  & lsedtot   ,namsed    ,morpar    ,morlyr    ,sedpar    , &
                  & mor_ptr   ,griddim   )
!!--description-----------------------------------------------------------------
!
! Reads attribute file for 3D morphology computation
!
!!--declarations----------------------------------------------------------------
    use precision
    use bedcomposition_module
    use properties
    use table_handles
    use morphology_data_module
    use grid_dimens_module, only: griddimtype
    use message_module, only: write_error
    !
    implicit none
!
! Arguments
!
    integer                                         , intent(in)  :: lsedtot  !< Description and declaration in esm_alloc_int.f90
    integer                                                       :: lundia   !< Description and declaration in inout.igs
    integer                                         , intent(in)  :: nmaxus
    integer                                         , intent(in)  :: nto
    integer                                         , intent(in)  :: version
    logical                                         , intent(in)  :: lfbedfrm    
    logical                                         , intent(out) :: error
    character(*)                                                  :: filmor
    character(20)             , dimension(nto)                    :: nambnd   !< Description and declaration in esm_alloc_char.f90
    character(20)             , dimension(lsedtot)                :: namsed   !< Names of all sediment fractions 
    type(morpar_type)                               , pointer     :: morpar
    type(sedpar_type)                               , pointer     :: sedpar
    type(bedcomp_data)                              , pointer     :: morlyr
    type(griddimtype)                    , target   , intent(in)  :: griddim
    type(tree_data)                                 , pointer     :: mor_ptr
!
! Local variables
!
    real(fp)                 :: rmissval
    real(fp)                 :: temp
    real(fp)                 :: thunlyr
    integer                  :: i
    integer                  :: it
    integer                  :: istat
    integer                  :: j
    integer                  :: l
    integer                  :: mxnulyr
    integer                  :: nm
    integer                  :: nval
    character(11)            :: fmttmp       !< Format file ('formatted  ') 
    character(20)            :: parname
    character(20)            :: txtput2
    character(40)            :: txtput1
    character(80)            :: bndname
    character(256)           :: errmsg
    character(256)           :: fildiff
    logical                  :: log_temp
    logical                  :: ex
    logical                  :: found
    type(tree_data), pointer :: morbound_ptr
    character(MAXTABLECLENGTH), dimension(:), allocatable         :: parnames
    !
    logical                             , pointer :: exchlyr
    real(fp)                            , pointer :: bed
    real(fp)                            , pointer :: minmass
    real(fp)                            , pointer :: theulyr
    real(fp)                            , pointer :: thlalyr
    real(fp)         , dimension(:)     , pointer :: thexlyr
    real(fp)         , dimension(:)     , pointer :: thtrlyr
    real(fp)                            , pointer :: ttlalpha
    real(fp)                            , pointer :: ttlmin
    real(fp)         , dimension(:,:)   , pointer :: kdiff
    real(fp)         , dimension(:)     , pointer :: zdiff
    integer                             , pointer :: idiffusion
    integer                             , pointer :: iporosity
    integer                             , pointer :: iunderlyr
    integer                             , pointer :: maxwarn
    integer                             , pointer :: ndiff
    integer                             , pointer :: neulyr
    integer                             , pointer :: nmlb
    integer                             , pointer :: nmub
    integer                             , pointer :: nfrac
    integer                             , pointer :: nlalyr
    integer                             , pointer :: ttlform
    integer                             , pointer :: telform
    integer                             , pointer :: updbaselyr
    type(handletype)                    , pointer :: bcmfile
    type(cmpbndtype) , dimension(:)     , pointer :: cmpbnd
    character(256)                      , pointer :: bcmfilnam
    character(256)                      , pointer :: flcomp
    character(256)                      , pointer :: ttlfil
    character(256)                      , pointer :: telfil
!
!! executable statements -------------------------------------------------------
!
    !lfbedfrm            => gdp%gdbedformpar%lfbedfrm
    bed                 => morpar%bed
    ttlalpha            => morpar%ttlalpha
    ttlmin              => morpar%ttlmin
    ttlform             => morpar%ttlform
    telform             => morpar%telform
    bcmfile             => morpar%bcmfile
    bcmfilnam           => morpar%bcmfilnam
    flcomp              => morpar%flcomp
    ttlfil              => morpar%ttlfil
    telfil              => morpar%telfil
    !
    istat = bedcomp_getpointer_integer(morlyr, 'IUnderLyr', iunderlyr)
    if (istat == 0) istat = bedcomp_getpointer_logical(morlyr, 'ExchLyr'             , exchlyr)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'NLaLyr'              , nlalyr)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'NEuLyr'              , neulyr)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'NFrac'               , nfrac)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'nmLb'                , nmlb)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'nmUb'                , nmub)
    if (istat == 0) istat = bedcomp_getpointer_realfp (morlyr, 'ThEuLyr'             , theulyr)
    if (istat == 0) istat = bedcomp_getpointer_realfp (morlyr, 'ThLaLyr'             , thlalyr)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'UpdBaseLyr'          , updbaselyr)
    if (istat == 0) istat = bedcomp_getpointer_realfp (morlyr, 'MinMassShortWarning' , minmass)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'MaxNumShortWarning'  , maxwarn)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'IPorosity'           , iporosity)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'Ndiff'               , ndiff)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'IDiffusion'          , idiffusion)
    if (istat /= 0) then
       errmsg = 'Memory problem in RDMORLYR'
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
    endif
    !
    nmlb  = griddim%nmlb
    nmub  = griddim%nmub
    nfrac = lsedtot
    !
    error      = .false.
    rmissval   = -999.0_fp
    fmttmp     = 'formatted'
    
    !
    ! allocate memory for boundary conditions. This needs to be done always. 
    !
    istat = 0
    allocate (morpar%cmpbnd(nto), stat = istat)
    if (istat==0) allocate(parnames(2*lsedtot), stat = istat)
    !
    if (istat /= 0) then
       errmsg = 'RDMORLYR: memory alloc error'
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
    endif  
    
    cmpbnd     => morpar%cmpbnd
    do j = 1, nto
       cmpbnd(j)%icond = 1
       cmpbnd(j)%ibcmt = 0
    enddo

    !
    ! return if input file is too old, otherwise get
    ! the data tree read from the input file
    !
    if (version < 2) then
       if (allocmorlyr(morlyr) /= 0) then
          errmsg = 'RDMORLYR: memory alloc error'
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       call set_sediment_properties_for_the_morphological_layers(iporosity, morlyr, sedpar)
       return
    endif

    write (lundia, '(a)') '*** Start  of underlayer input'
    !
    ! underlayer bookkeeping mechanism
    !
    call prop_get_integer(mor_ptr, 'Underlayer', 'IUnderLyr', iunderlyr)
    if (iunderlyr < 1 .or. iunderlyr > 2) then
       errmsg = 'IUnderLyr should be 1 or 2 in ' // trim(filmor)
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
    endif
    txtput1 = 'Underlayer mechanism'
    write (lundia, '(2a,i20)') txtput1, ':', iunderlyr
    !
    ! underlayer mechanism parameters
    !
    select case (iunderlyr)
    case(2)
       !
       ! flag for exchange layer
       !
       call prop_get_logical(mor_ptr, 'Underlayer', 'ExchLyr', exchlyr)
       txtput1 = 'Exchange layer'
       if (exchlyr) then
          txtput2 = '                 YES'
       else
          txtput2 = '                  NO'
       endif
       write (lundia, '(3a)') txtput1, ':', txtput2
       !
       call prop_get_integer(mor_ptr, 'Underlayer', 'IPorosity', iporosity)
       txtput1 = 'Porosity'
       select case (iporosity)
       case (0)
          txtput2 = '      Based on CDRYB'
       case (1)
          txtput2 = '              Linear'
       case (2)
          txtput2 = '          Non-linear'
       end select
       write (lundia, '(3a)') txtput1, ':', txtput2
       !
       nlalyr = 0
       neulyr = 0
       call prop_get_integer(mor_ptr, 'Underlayer', 'NLaLyr', nlalyr)
       if (nlalyr < 0) then
          errmsg = 'Number of Lagrangian under layers should be 0 or more in ' // trim(filmor)
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       call prop_get_integer(mor_ptr, 'Underlayer', 'NEuLyr', neulyr)
       if (neulyr < 0) then
          errmsg = 'Number of Eulerian under layers should be 0 or more in ' // trim(filmor)
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       !
       mxnulyr = nlalyr+neulyr
       call prop_get_integer(mor_ptr, 'Underlayer', 'MxNULyr', mxnulyr)
       if (mxnulyr < 0) then
          errmsg = 'Maximum number of under layers should be 0 or more in ' // trim(filmor)
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       if (mxnulyr /= nlalyr+neulyr) then
          nlalyr = -999
          neulyr = -999
          call prop_get_integer(mor_ptr, 'Underlayer', 'NLaLyr', nlalyr)
          call prop_get_integer(mor_ptr, 'Underlayer', 'NEuLyr', neulyr)
          if (nlalyr<0 .and. neulyr<0) then
             !
             ! neither NLaLyr nor NEuLyr specified
             !
             nlalyr = 0
             neulyr = mxnulyr
          elseif (nlalyr>=0 .and. neulyr>=0) then
             !
             ! mismatch: error
             !
             errmsg = 'Remove MxNULyr or set MxNULyr = NLaLyr+NEuLyr in ' // trim(filmor)
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          elseif (nlalyr>=0) then
             !
             ! NLaLyr specified and MxNULyr
             !
             neulyr = mxnulyr - nlalyr
             if (neulyr<0) then
                errmsg = 'NLaLyr must be less than MxNULyr in ' // trim(filmor)
                call write_error(errmsg, unit=lundia)
             endif
          else
             !
             ! NEuLyr specified and MxNULyr
             !
             nlalyr = mxnulyr - neulyr
             if (nlalyr<0) then
                errmsg = 'NEuLyr must be less than MxNULyr in ' // trim(filmor)
                call write_error(errmsg, unit=lundia)
             endif
          endif
       endif
       mxnulyr = nlalyr + neulyr
       txtput1 = 'Number of Lagrangian layers'
       write (lundia, '(2a,i20)') txtput1, ':', nlalyr
       txtput1 = 'Number of Eulerian layers'
       write (lundia, '(2a,i20)') txtput1, ':', neulyr
       !
       if (mxnulyr > 0) then
          thunlyr = rmissval
          call prop_get(mor_ptr, 'Underlayer', 'ThUnLyr', thunlyr)
          theulyr = thunlyr
          thlalyr = thunlyr
          call prop_get(mor_ptr, 'Underlayer', 'ThEuLyr', theulyr)
          call prop_get(mor_ptr, 'Underlayer', 'ThLaLyr', thlalyr)
          !
          if (nlalyr>0) then
             txtput1 = 'Thickness of Lagrangian underlayers'
             write(lundia,'(2a,e20.4)') txtput1, ':', thlalyr
             if (thlalyr <= 0.0_fp) then
                errmsg = 'ThLaLyr should be positive in ' // trim(filmor)
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             endif
          endif
          if (neulyr>0) then
             txtput1 = 'Thickness of Eulerian underlayers'
             write(lundia,'(2a,e20.4)') txtput1, ':', thlalyr
             if (theulyr <= 0.0_fp) then
                errmsg = 'ThEuLyr should be positive in ' // trim(filmor)
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             endif
          endif
       endif
       !
       call prop_get_integer(mor_ptr, 'Underlayer', 'UpdBaseLyr', updbaselyr)
       if (updbaselyr < 1 .or. updbaselyr > 4) then
          errmsg = 'UpdBaseLyr should be 1-4 in ' // trim(filmor)
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       !
       txtput1 = 'Base layer composition'
       select case (updbaselyr)
       case (1)
          txtput2 = ' computed separately'
       case (2)
          txtput2 = ' constant'
       case (3)
          txtput2 = ' same as layer above'
       case (4)
          txtput1 = 'Base layer composition and thickness'
          txtput2 = '            constant'
       case default
          txtput2 = ' <unknown>'
       end select
       write(lundia,'(3a)') txtput1, ':', txtput2
       !
       ! Numerical settings
       !
       call prop_get(mor_ptr, 'Numerics', 'MinMassShortWarning', minmass)
       call prop_get(mor_ptr, 'Numerics', 'MaxNumShortWarning' , maxwarn)
       !
       ! Mixing between layers
       !
       call prop_get_integer(mor_ptr, 'Underlayer', 'IDiffusion', idiffusion)       
       txtput1 = 'Mixing between layers'
       if (idiffusion>0) then
          txtput2 = '                 YES'
       else
          txtput2 = '                  NO'
       endif
       write (lundia, '(3a)') txtput1, ':', txtput2
       if (idiffusion>0) then
           call prop_get_integer(mor_ptr, 'Underlayer', 'NDiff', ndiff)
           txtput1 = '# diffusion coefficients in z-direction'
           write (lundia, '(2a,i20)') txtput1, ':', ndiff
           if (ndiff < 0) then
              errmsg = 'Number of diffusion coefficients should be 0 or more in ' // trim(filmor)
              call write_error(errmsg, unit=lundia)
              error = .true.
              return
           endif
       endif
       !
    case default
    endselect
    !
    if (allocmorlyr(morlyr) /= 0) then
       errmsg = 'RDMORLYR: memory alloc error'
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
    endif
    !
    ! underlayer mechanism parameters
    !
    select case (iunderlyr)
    case(2)
       if (idiffusion>0) then
           !
           ! Diffusion coefficient
           !
           istat = bedcomp_getpointer_realfp(morlyr, 'Kdiff', kdiff)
           if (istat == 0) istat = bedcomp_getpointer_realfp(morlyr, 'Zdiff', zdiff)
           if (istat /= 0) then
               errmsg = 'Memory problem in RDMORLYR'
               call write_error(errmsg, unit=lundia)
               error = .true.
               return
           endif
           !
           fildiff = ''
           call prop_get(mor_ptr, 'Underlayer', 'Diffusion', fildiff)
           !
           ! Intel 7.0 crashes on an inquire statement when file = ' '
           !
           if (fildiff == ' ') fildiff = 'dummyname'
           inquire (file = fildiff, exist = ex)
           if (.not. ex) then
               txtput1 = 'Constant diffusion coefficient'
               temp = 0.0_fp
               call prop_get(mor_ptr, 'Underlayer', 'Diffusion', temp)
               kdiff = temp
               zdiff = 0.0_fp
               write (lundia, '(2a,e20.4)') txtput1,':', temp
           else
               txtput1 = 'Diffusion coefficient from file'
               write (lundia, '(3a)') txtput1,':', trim(fildiff)
               !
               call rdinidiff(lundia    ,fildiff   ,ndiff     ,kdiff    , &
                            & zdiff     ,griddim   ,error     )
               if (error) return
           endif
       endif
       !
       ! Get the following pointers after allocating the memory for the arrays
       !
       istat = bedcomp_getpointer_realfp(morlyr, 'ThTrLyr', thtrlyr)
       if (istat /= 0) then
          errmsg = 'Memory problem in RDMORLYR'
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       !
       txtput1 = 'Thickness transport layer'
       call prop_get_integer(mor_ptr, 'Underlayer', 'TTLForm', ttlform)
       select case (ttlform)
       case (1)
          !
          ! Transport layer thickness constant in time:
          ! uniform or spatially varying thickness
          !
          ttlfil = ''
          call prop_get_string(mor_ptr, 'Underlayer', 'ThTrLyr', ttlfil)
          !
          ! Intel 7.0 crashes on an inquire statement when file = ' '
          !
          if (ttlfil == ' ') ttlfil = 'dummyname'
          inquire (file = ttlfil, exist = ex)
          !
          if (ex) then
             !
             ! read data from file
             !
             write(lundia,'(3a)') txtput1, ':', ttlfil
             !
             call depfil_stm(lundia    ,error     ,ttlfil    ,fmttmp    , &
                           & thtrlyr   ,1         ,1         ,griddim   ,errmsg)
             if (error) then
                call write_error(errmsg, unit=lundia)
                errmsg = 'Unable to read transport layer thickness from ' // trim(ttlfil)
                call write_error(errmsg, unit=lundia)
                return
             endif
          else
             ttlfil = ' '
             call prop_get(mor_ptr, 'Underlayer', 'ThTrLyr', thtrlyr(1))
             if (thtrlyr(1) <= 0) then
                errmsg = 'ThTrLyr should be positive in ' // trim(filmor)
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             endif
             do it = nmlb, nmub
                thtrlyr(it) = thtrlyr(1)
             enddo
             !
             write(lundia,'(2a,e20.4)') txtput1, ':', thtrlyr(1)
          endif
       case (2, 3)
          !
          ! Transport layer thickness proportional to
          ! the water depth (2) or dune height (3)
          !
          call prop_get(mor_ptr, 'Underlayer', 'TTLAlpha', ttlalpha)
          call prop_get(mor_ptr, 'Underlayer', 'TTLMin'  , ttlmin)
          !
          txtput2 = ' max(a*H,b)'
          if (ttlform == 3) then
             txtput2 = ' max(a*Hdune,b)'
             if (.not.lfbedfrm) then
                errmsg = 'TTLForm=3 can only be used when dunes are computed'
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             endif
          endif
          write(lundia,'(3a)') txtput1, ':', txtput2
          txtput1 = '  a'
          write(lundia,'(2a,e20.4)') txtput1, ':', ttlalpha
          txtput1 = '  b'
          write(lundia,'(2a,e20.4)') txtput1, ':', ttlmin
       case default
          errmsg = 'Invalid transport layer thickness option specified in ' // trim(filmor)
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       end select
       !
       if (exchlyr) then
          istat = bedcomp_getpointer_realfp(morlyr, 'ThExLyr', thexlyr)
          if (istat /= 0) then
             errmsg = 'Memory problem in RDMORLYR'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
          !
          txtput1 = 'Thickness exchange layer'
          call prop_get_integer(mor_ptr, 'Underlayer', 'TELForm', telform)
          select case (telform)
          case (1)
             !
             ! Exchange layer thickness constant in time:
             ! uniform or spatially varying thickness
             !
             telfil = ''
             call prop_get_string(mor_ptr, 'Underlayer', 'ThExLyr', telfil)
             !
             ! Intel 7.0 crashes on an inquire statement when file = ' '
             !
             if (telfil == ' ') telfil = 'dummyname'
             inquire (file = telfil, exist = ex)
             !
             if (ex) then
                write(lundia,'(3a)') txtput1, ':', telfil
                !
                ! read data from file
                !
                call depfil_stm(lundia    ,error     ,telfil    ,fmttmp    , &
                              & thexlyr   ,1         ,1         ,griddim   , errmsg)
                if (error) then
                   call write_error(errmsg, unit=lundia)
                   errmsg = 'Unable to read exchange layer thickness from ' // trim(telfil)
                   call write_error(errmsg, unit=lundia)
                   return
                endif
             else
                telfil = ' '
                call prop_get(mor_ptr, 'Underlayer', 'ThExLyr', thexlyr(1))
                if (thexlyr(1) <= 0) then
                   errmsg = 'ThExLyr should be positive in ' // trim(filmor)
                   call write_error(errmsg, unit=lundia)
                   error = .true.
                   return
                endif
                do it = nmlb, nmub
                   thexlyr(it) = thexlyr(1)
                enddo   
                !
                write(lundia,'(2a,e20.4)') txtput1, ':', thexlyr(1)
             endif
          case default
             errmsg = 'Invalid exchange layer thickness option specified in ' // trim(filmor)
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          end select
       endif
       !
    case default
    endselect
    !
    ! Boundary conditions
    !
    do i = 1, size(mor_ptr%child_nodes)
       !
       ! Does mor_ptr contain a child with name 'Boundary' (converted to lower case)?
       !
       morbound_ptr => mor_ptr%child_nodes(i)%node_ptr
       bndname = tree_get_name( morbound_ptr )
       if ( trim(bndname) /= 'boundary') cycle
       bndname = ''
       call prop_get_string(morbound_ptr, '*', 'Name', bndname)
       found = .false.
       do j = 1, nto
          !
          ! Search known boundaries for match
          !
          if (bndname == nambnd(j)) then
             found = .true.
             exit
          endif
       enddo
       if (.not.found) then
          errmsg = 'Unknown boundary "'//trim(bndname)//'" in '//trim(filmor)
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       !
       call prop_get_integer(morbound_ptr, '*', 'ICmpCond', cmpbnd(j)%icond)
       if (cmpbnd(j)%icond < 0 .or. cmpbnd(j)%icond > 3) then
          errmsg = 'Invalid composition boundary condition at "'//trim(bndname)//'" in '//trim(filmor)
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       !
    enddo
    do j = 1, nto
       txtput1 = 'Boundary name'
       write (lundia, '(2a,a20)') txtput1, ':', trim(nambnd(j))
       !
       txtput1 = '  Composition condition prescribed'
       select case(cmpbnd(j)%icond)
       case (0)
          txtput2 = '                free'
          parname = ' '
       case (1)
          txtput2 = '               fixed'
          parname = ' '
       case (2)
          txtput1 = '  Mass fraction condition prescribed'
          txtput2 = '         time series'
          parname = 'mass fraction'
          nval = lsedtot
       case (3)
          txtput1 = '  Volume fraction condition prescribed'
          txtput2 = '         time series'
          parname = 'volume fraction'
          nval = lsedtot
       end select
       write (lundia, '(3a)') txtput1, ':', txtput2
       !
       ! Check boundary conditions
       !
       if (parname /= ' ') then
          if (bcmfilnam /= ' ') then
             !
             ! Find entries in table
             !
             call gettable(bcmfile            , nambnd(j)          , trim(parname) , &
                             & cmpbnd(j)%ibcmt(1) , cmpbnd(j)%ibcmt(2) , &
                         & cmpbnd(j)%ibcmt(3) , 1                  , errmsg        )
             if (errmsg /= ' ') then
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             endif
             cmpbnd(j)%ibcmt(4) = 1
             txtput1 = '  Variation along boundary'
             !
             ! Check entries in table
             !
             if (cmpbnd(j)%ibcmt(3) == nval) then
                !
                ! Uniform values
                !
                txtput2 = '             uniform'
                write (lundia, '(3a)') txtput1, ':', txtput2
                i = 0
                do l = 1, lsedtot
                   i = i + 1
                   parnames(i) = trim(parname) // ' ' // trim(namsed(l))
                enddo
                !
                call checktableparnames(bcmfile            , parnames           , &
                                          & cmpbnd(j)%ibcmt(1) , cmpbnd(j)%ibcmt(2) , &
                                      & cmpbnd(j)%ibcmt(3) , errmsg             )
                if (errmsg /= ' ') then
                   call write_error(errmsg, unit=lundia)
                   error = .true.
                   return
                endif
                call checktable(bcmfile            , &
                                  & cmpbnd(j)%ibcmt(1) , cmpbnd(j)%ibcmt(2) , &
                              & cmpbnd(j)%ibcmt(3) , CHKTAB_POSITIVE    , errmsg )
                if (errmsg /= ' ') then
                   call write_error(errmsg, unit=lundia)
                   error = .true.
                   return
                endif
             elseif (cmpbnd(j)%ibcmt(3) == nval*2) then
                !
                ! Values at "end A" and "end B"
                !
                txtput2 = '              linear'
                write (lundia, '(3a)') txtput1, ':', txtput2
                i = 0
                do l = 1, lsedtot
                   i = i + 1
                   parnames(i)      = trim(parname) // ' ' // trim(namsed(l)) // ' end A'
                   parnames(nval+i) = trim(parname) // ' ' // trim(namsed(l)) // ' end B'
                enddo
                !
                call checktableparnames(bcmfile            , parnames           , &
                                          & cmpbnd(j)%ibcmt(1) , cmpbnd(j)%ibcmt(2) , &
                                      & cmpbnd(j)%ibcmt(3) , errmsg             )
                if (errmsg /= ' ') then
                   call write_error(errmsg, unit=lundia)
                   error = .true.
                   return
                endif
                call checktable(bcmfile            , &
                                  & cmpbnd(j)%ibcmt(1) , cmpbnd(j)%ibcmt(2) , &
                              & cmpbnd(j)%ibcmt(3) , CHKTAB_POSITIVE    , errmsg )
                if (errmsg /= ' ') then
                   call write_error(errmsg, unit=lundia)
                   error = .true.
                   return
                endif
             else
                !
                ! Invalid number of values specified
                !
                errmsg = 'Invalid number of parameters specified for ''' // &
                       & trim(parname) // ''' at ''' // nambnd(j) // ''' in ' // &
                       & trim(bcmfilnam)
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             endif
          else
             errmsg = 'Missing input file for morphological boundary conditions'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
       endif
    enddo
    !
    ! Initial Bed Composition (Overrules)
    !
    flcomp = ''
    call prop_get(mor_ptr, 'Underlayer', 'IniComp', flcomp)
    !
    if (iunderlyr /= 2 .and. flcomp /= ' ') then  
       write(lundia,'(a)') 'WARNING: IniComp keyword only supported for IUnderLyr=2'
       flcomp = ' '
    endif
    txtput1 = 'Initial bed composition'
    if (flcomp == ' ') then
       txtput2 = 'from sediment file'
       write(lundia,'(2a,a20)') txtput1, ':', trim(txtput2)
    else
       txtput2 = 'from IniComp file -'
       write(lundia,'(3a)') txtput1, ':', trim(flcomp)
    endif
    !
    write (lundia, '(a)') '*** End    of underlayer input'
    write (lundia, *)
    !
    call set_sediment_properties_for_the_morphological_layers(iporosity, morlyr, sedpar)
    !
    deallocate(parnames, stat = istat)
    !
end subroutine rdmorlyr


subroutine set_sediment_properties_for_the_morphological_layers(iporosity, morlyr, sedpar)
    use bedcomposition_module, only : bedcomp_data, setbedfracprop
    use morphology_data_module, only : sedpar_type
    implicit none

    integer                                 , pointer, intent(in)      :: iporosity
    type(bedcomp_data)                      , pointer, intent(inout)   :: morlyr
    type(sedpar_type)                       , pointer, intent(inout)   :: sedpar
                  
    if (iporosity==0) then
       !
       ! porosity is fraction dependent and included in cdryb densities
       !
       call setbedfracprop(morlyr, sedpar%sedtyp, sedpar%sedd50, &
             & sedpar%logsedsig, sedpar%cdryb)
    else
       !
       ! porosity is simulated, the cdryb values are ignored
       !
       call setbedfracprop(morlyr, sedpar%sedtyp, sedpar%sedd50, &
             & sedpar%logsedsig, sedpar%rhosol)
       ! sedpar%cdryb = sedpar%rhosol
    endif
    
end subroutine set_sediment_properties_for_the_morphological_layers

subroutine rdinidiff(lundia    ,fildiff   ,ndiff     ,kdiff    , &
                   & zdiff     ,griddim   ,error     )
!!--description-----------------------------------------------------------------
!
! Reads attribute file for diffusion coefficient in bed
!
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use message_module
    use grid_dimens_module, only: griddimtype
    !
    implicit none
!
! Global variables
!
    type(griddimtype)                        , target   , intent(in)  :: griddim
    integer                                             , intent(in)  :: lundia  !< Description and declaration in inout.igs
    integer                                             , intent(in)  :: ndiff   !< Description and declaration in bedcomposition module
    real(fp), dimension(ndiff)                          , intent(out) :: zdiff   !< Description and declaration in bedcomposition module
    real(fp), dimension(ndiff,griddim%nmlb:griddim%nmub), intent(out) :: kdiff   !< Description and declaration in bedcomposition module
    character(*)                                        , intent(out) :: fildiff
    logical                                             , intent(out) :: error
!
! Local variables
!
    integer                               :: i
    integer                               :: ilyr
    integer                               :: istat
    integer                               :: nm
    logical                               :: ex
    real(fp)                              :: rmissval
    real(fp)                              :: temp
    character(10)                         :: versionstring
    character(80)                         :: parname
    character(11)                         :: fmttmp   ! Format file ('formatted  ') 
    character(256)                        :: filename
    character(300)                        :: message
    character(40)                         :: txtput1
    type(tree_data), pointer              :: mor_ptr
    type(tree_data), pointer              :: layer_ptr
!
!! executable statements -------------------------------------------------------
!
    !
    rmissval      = -999.0_fp
    versionstring = 'n.a.'
    fmttmp        = 'formatted'
    error         = .false.
    !
    ! Create Initial Morphology branch in input tree
    !
    call tree_create  ( "Diffusion input", mor_ptr )
    !
    ! Read diffusion-file into tree data structure
    !
    call prop_file('ini', trim(fildiff), mor_ptr, istat)
    if (istat /= 0) then
       select case (istat)
       case(1)
          call write_error(FILE_NOT_FOUND//trim(fildiff), unit=lundia)
       case(3)
          call write_error(PREMATURE_EOF//trim(fildiff), unit=lundia)
       case default
          call write_error(FILE_READ_ERROR//trim(fildiff), unit=lundia)
       endselect
       error = .true.
       return
    endif
    !
    ! Check version number of mor input file
    !
    call prop_get_string(mor_ptr,'DiffusionFileInformation','FileVersion',versionstring)
    if (trim(versionstring) == '01.00') then
        !
        ilyr = 0
        !
        do i = 1,size(mor_ptr%child_nodes) ! loop over child_nodes
            !
            ! Does sed_ptr contain a child with name 'Level' (converted to lower case)?
            !
            layer_ptr => mor_ptr%child_nodes(i)%node_ptr
            parname = tree_get_name( layer_ptr )
            call small(parname,len(parname))
            if ( trim(parname) /= 'level') cycle
            !
            ! Increment ilyr, but do not exceed ndiff
            !
            ilyr = ilyr+1
            if (ilyr>ndiff) then
                call write_error('Diffusion file contains more levels than specified by NDIFF parameter.', unit=lundia)
                error = .true.
                return
            endif
            filename = ' '
            call prop_get_string(layer_ptr, '*', 'Kdiff', filename)
            !
            ! Intel 7.0 crashes on an inquire statement when file = ' '
            !
            if (filename == ' ') filename = 'dummyname'
            inquire (file = filename, exist = ex)
            if (.not. ex) then
                !
                ! Constant diffusion
                !
                temp = rmissval
                call prop_get(layer_ptr, '*', 'Kdiff', temp)
                if (comparereal(temp,rmissval) == 0) then
                    write (message,'(a,i2,a,a)')  &
                        & 'Missing KDIFF keyword for level ',ilyr,' in file ',trim(fildiff)
                    call write_error(message, unit=lundia)
                    error = .true.
                    return
                endif
                kdiff(ilyr,:) = temp
            else
                !
                ! Spatially varying diffusion coefficient
                !
                call depfil_stm(lundia    ,error     ,filename  ,fmttmp    , &
                              & kdiff(ilyr,griddim%nmlb),1         ,1         ,griddim   , message)
                if (error) then
                   call write_error(message, unit=lundia)
                   message = 'Unable to read diffusion coefficients from ' // trim(filename)
                   call write_error(message, unit=lundia)
                   return
                endif
            endif
            temp = rmissval
            call prop_get(layer_ptr, '*', 'Zdiff', temp)
            if (comparereal(temp,rmissval) == 0) then
                write (message,'(a,i2,a,a)')  &
                    & 'Missing ZDIFF keyword for level ',ilyr,' in file ',trim(fildiff)
                call write_error(message, unit=lundia)
                error = .true.
                return
            endif
            zdiff(ilyr) = temp
            if (ilyr>1 .and. zdiff(ilyr) <= zdiff(ilyr-1)) then
                write (message,'(a,i2,a,i2,a,a)')  &
                    & '*** ERROR Depth of level ',i, &
                    & ' is smaller than that of level ',i-1,' in file ',trim(fildiff)
                call write_error(message, unit=lundia)
                error = .true.
                return
            endif
        enddo ! child nodes
        !
        ! Setting values for remaining levels equal to that of level ilyr
        !
        if (ilyr < ndiff) then
            write (message, '(a,i2,a,i2,a, a)')  &
                & 'Number of levels [',ilyr,'] smaller than NDIFF [', ndiff, '] in file ',trim(fildiff)
            call write_warning(message, unit=lundia)
            write (message, '(a,i2)') &
                & 'Setting values for remaining levels equal to that of level ',ilyr
            call write_warning(message, unit=lundia)
            do i= ilyr+1, ndiff
                kdiff(i,:) = kdiff(ilyr,:)
                zdiff(i)   = zdiff(ilyr)
            enddo
        endif
    else
        call write_error('Invalid file version of '//trim(fildiff), unit=lundia)
        error = .true.
    endif 
    !
end subroutine rdinidiff

subroutine rdinimorlyr(lsedtot   ,lsed      ,lundia    ,error     , &
                     & dims      ,morlyr    ,morpar    ,sedpar    , &
                     & rst_fluff ,rst_bedcmp)
!!--description-----------------------------------------------------------------
!
! Reads attribute file for 3D morphology computation
!
!!--declarations----------------------------------------------------------------
    use precision
    use bedcomposition_module
    use properties
    use string_module, only: remove_leading_spaces
    use grid_dimens_module, only: griddimtype
    use message_module, only: FILE_NOT_FOUND, FILE_READ_ERROR, PREMATURE_EOF
    use MessageHandling, only: mess, LEVEL_ERROR
    use morphology_data_module, only: sedpar_type, morpar_type
    use m_depfil_stm
    !
    implicit none
!
! Global variables
!
    type(griddimtype), target                              , intent(in)  :: dims    !  grid dimensions
    integer                                                , intent(in)  :: lsedtot
    integer                                                , intent(in)  :: lsed
    integer                                                              :: lundia
    logical                                                , intent(out) :: error
    type (morpar_type)                          , target   , intent(in)  :: morpar
    type (sedpar_type)                          , target   , intent(in)  :: sedpar
    type(bedcomp_data)                                                   :: morlyr
    logical                                                , intent(in)  :: rst_fluff
    logical                                                , intent(in)  :: rst_bedcmp
!
! Local variables
!
    integer                               :: i
    integer                               :: ibnd
    integer                               :: ilyr
    integer                               :: ised
    integer                               :: istat
    integer                               :: length
    integer                               :: nm
    integer                               :: nm2
    integer                               :: nmlb
    integer                               :: nmmax
    integer                               :: nmub
    real(fp)                              :: cdrybavg
    real(fp)                              :: fraction
    real(fp), dimension(lsedtot)          :: mfrac
    real(fp)                              :: mfracsum
    real(fp)                              :: poros
    real(fp)                              :: rmissval
    real(fp)                              :: sedbed
    real(fp)                              :: sedmass
    real(fp)                              :: svf
    real(fp)                              :: thick
    real(fp)                              :: totfrac
    real(fp), dimension(:,:), allocatable :: rtemp
    real(fp), dimension(:), allocatable   :: thtemp
    real(fp)                              :: vfracsum
    logical                               :: anyfrac
    logical                               :: anysedbed
    logical                               :: err2
    logical                               :: ex
    logical                               :: success
    character(10)                         :: lstr
    character(10)                         :: versionstring
    character(11)                         :: fmttmp   ! Format file ('formatted  ') 
    character(80)                         :: parname
    character(80)                         :: layertype
    character(256)                        :: filename
    character(300)                        :: message
    type(tree_data)                     , pointer :: mor_ptr
    type(tree_data)                     , pointer :: layer_ptr
    !
    integer                             , pointer :: iporosity
    integer                             , pointer :: iunderlyr
    integer                             , pointer :: nlyr
    real(fp)         , dimension(:)     , pointer :: cdryb
    real(fp)         , dimension(:)     , pointer :: rhosol
    real(fp)         , dimension(:)     , pointer :: sdbuni
    character(10)    , dimension(:)     , pointer :: inisedunit
    character(256)   , dimension(:)     , pointer :: flsdbd
    character(256)                      , pointer :: flcomp
    real(prec)       , dimension(:,:)   , pointer :: bodsed
    real(fp)         , dimension(:,:)   , pointer :: mfluff
    real(fp)         , dimension(:,:,:) , pointer :: msed
    real(fp)         , dimension(:,:)   , pointer :: thlyr
    real(fp)         , dimension(:,:)   , pointer :: svfrac
    real(fp)         , dimension(:)     , pointer :: mfluni
    character(20)    , dimension(:)     , pointer :: namsed
    character(256)   , dimension(:)     , pointer :: mflfil
!
!! executable statements -------------------------------------------------------
!
    cdryb              => sedpar%cdryb
    rhosol             => sedpar%rhosol
    sdbuni             => sedpar%sdbuni
    inisedunit         => sedpar%inisedunit
    flsdbd             => sedpar%flsdbd
    flcomp             => morpar%flcomp
    namsed             => sedpar%namsed
    !
    rmissval      = -999.0_fp
    fmttmp        = 'formatted'
    nmmax         = dims%nmmax
    nmlb          = dims%nmlb
    nmub          = dims%nmub
    error         = .false.
    !
    ! Fluff layer
    !
    if (morpar%flufflyr%iflufflyr>0 .and. .not.rst_fluff) then
       !
       ! If not restart then initialize using values specified in input file
       !
       mfluff => morpar%flufflyr%mfluff
       mfluni => morpar%flufflyr%mfluni
       mflfil => morpar%flufflyr%mflfil
       !
       mfluff = 0.0_fp
       !
       do ised = 1, lsed
           if (sedpar%sedtyp(ised) > sedpar%max_mud_sedtyp) cycle ! check for IFORM == -3 instead?
           inquire (file = mflfil(ised), exist = ex)
           if (ex) then
               call depfil_stm(lundia    ,error     ,mflfil(ised)         , &
                             & fmttmp    ,mfluff    ,lsed      ,ised      , &
                             & dims      ,message)
               if (error) then 
                   call mess(LEVEL_ERROR, message)  
                   return
               endif
           else
               mfluff(ised,:) = mfluni(ised)
           endif
       enddo
    endif 
    !
    ! Bed layers
    !
    if (.not.rst_bedcmp) then
       istat = bedcomp_getpointer_integer(morlyr, 'iunderlyr', iunderlyr)
       if (istat==0) istat = bedcomp_getpointer_integer(morlyr, 'nlyr'   , nlyr)
       if (istat==0) istat = bedcomp_getpointer_realprec(morlyr, 'bodsed', bodsed)
       if (istat==0) istat = bedcomp_getpointer_integer(morlyr, 'iporosity', iporosity)
       if (iunderlyr==2) then
          if (istat==0) istat = bedcomp_getpointer_realfp (morlyr, 'msed'     , msed)
          if (istat==0) istat = bedcomp_getpointer_realfp (morlyr, 'thlyr'    , thlyr)
          if (istat==0) istat = bedcomp_getpointer_realfp (morlyr, 'svfrac'   , svfrac)
       endif
       if (istat/=0) then
          call mess(LEVEL_ERROR, 'Memory problem in RDINIMORLYR')           
          error = .true.
          return          
       endif
       !
       if (iunderlyr==1 .or. flcomp == ' ') then
          !
          ! If not restart and no layer administration, then use the bed composition specified in the sed file.
          !
          error = .false.
          do ised = 1, lsedtot
             if (flsdbd(ised) == ' ') then
                !
                ! Uniform data has been specified
                !
                do nm = 1, nmmax
                   bodsed(ised, nm) = real(sdbuni(ised),prec)
                enddo
             else
                !
                ! Space varying data has been specified
                ! Use routine that also read the depth file to read the data
                !
                call depfil_stm_double(lundia    ,error     ,flsdbd(ised) , &
                                     & fmttmp    ,bodsed    ,lsedtot      , &
                                     & ised      ,dims      ,message      )
                if (error) then
                   message = 'RDMORLYR '//message
                   call mess(LEVEL_ERROR, message)           
                   return
                endif 
              endif
          enddo
          if (iporosity == 0) then
             do ised = 1, lsedtot
                if (inisedunit(ised) == 'm') then
                   do nm = 1, nmmax
                      bodsed(ised, nm) = bodsed(ised, nm) * cdryb(ised)
                   enddo
                else
                   !
                   ! inisedunit(ised) = kg/m2
                   ! no conversion needed
                   !
                endif
             enddo
          else
             do ised = 2, lsedtot
                if (inisedunit(ised) /= inisedunit(1)) then
                   call mess(LEVEL_ERROR, 'All sediment fields in the same layer should have unit.')           
                   error = .true.
                   return
                endif
             enddo
             if (inisedunit(1) == 'm') then
                !
                ! all input specified as thickness
                !
                do nm = 1, nmmax
                   mfracsum = 0.0_fp
                   do ised = 1, lsedtot
                      mfrac(ised) = bodsed(ised, nm) * rhosol(ised)
                      mfracsum = mfracsum + mfrac(ised)
                   enddo
                   if (mfracsum > 0.0_fp) then
                      do ised = 1, lsedtot
                         mfrac(ised) = mfrac(ised) / mfracsum
                      enddo
                      !
                      call getporosity(morlyr, mfrac, poros)
                      svf = 1.0_fp - poros
                   else
                      svf = 1.0_fp
                   endif
                   !
                   do ised = 1, lsedtot
                      bodsed(ised, nm) = bodsed(ised, nm) * svf * rhosol(ised)
                   enddo
                enddo
             else
                !
                ! inisedunit(1) = kg/m2
                ! no conversion needed
                !
             endif
          endif
          !
          ! Check validity of input data
          !
          do nm = 1, nmmax
             if (dims%celltype(nm) == 1) then
                !
                ! At an internal point the composition is important.
                ! Check the values carefully before continuing.
                !
                do ised = 1, lsedtot
                   if (bodsed(ised, nm) < 0.0) then
                      write (message,'(a,i2,a,f15.2,a,a,a,i0,a,f15.2,f15.2,a)')  &
                          & 'Negative sediment thickness for fraction ',ised, ': ',bodsed(ised, nm),' in file ', &
                          & trim(flsdbd(ised)),' at nm=',nm, ' (x, y = ', dims%xz(nm), dims%yz(nm), ')'
                      call mess(LEVEL_ERROR, trim(message))           
                      error = .true.
                      return
                   endif
                enddo
             elseif (dims%celltype(nm) == -1) then
                !
                ! At a ghost point the composition is also important
                ! since it determines the fractions and hence the
                ! the grain sizes and transport rates. We don't need
                ! to raise an error since the owning partition will do
                ! so and because of complications with open boundaries
                ! located in ghost areas.
                !
                do ised = 1, lsedtot
                   bodsed(ised, nm) = max(0.0_fp, bodsed(ised, nm))
                enddo
             elseif (dims%celltype(nm) == 2) then
                !
                ! At an open boundary the composition is also important
                ! but if the input is not valid, mark the data as dummy data:
                ! the data will be overwritten with data coming from the
                ! neighbouring internal point.
                !
                err2 = .false.
                do ised = 1, lsedtot
                   if (bodsed(ised, nm)<0.0) err2=.true.
                enddo
                if (err2) then
                   !
                   ! set dummy flag
                   !
                   bodsed(1, nm) = -1.0
                endif
             else
                !
                ! Point that will never be used: don't care about the values.
                ! Just replace whatever was read by something valid.
                !
                do ised = 1, lsedtot
                   bodsed(ised, nm) = 0.0
                enddo
             endif
          enddo
          !
          ! Copy BODSED data to open boundary points that have not
          ! yet been assigned valid data.
          !
          do ibnd = 1, size(dims%nmbnd,1)
             nm  = dims%nmbnd(ibnd,1)
             if (bodsed(1, nm)<0) then
                nm2 = dims%nmbnd(ibnd,2)
                do ised = 1,lsedtot
                   bodsed(ised, nm) = bodsed(ised, nm2)
                enddo
             endif
          enddo
          !
          ! Use BODSED: compute DPSED and as needed transfer information from BODSED to other arrays
          !
          call bedcomp_use_bodsed(morlyr)
       else
          !
          ! Create Initial Morphology branch in input tree
          !
          call tree_create  ( "Initial Morphology", mor_ptr )
          !
          ! Put mor-file in input tree
          !
          call prop_file('ini', trim(flcomp), mor_ptr, istat)
          if (istat /= 0) then
             select case (istat)
             case(1)
                call mess(LEVEL_ERROR, FILE_NOT_FOUND//trim(flcomp))  
             case(3)
                call mess(LEVEL_ERROR, PREMATURE_EOF//trim(flcomp))  
             case default
                call mess(LEVEL_ERROR, FILE_READ_ERROR//trim(flcomp))  
             endselect
             error = .true.
             return
          endif    
          !
          ! Check version number of mor input file
          !
          versionstring = 'n.a.'
          call prop_get_string(mor_ptr, 'BedCompositionFileInformation', 'FileVersion', versionstring)
          if (trim(versionstring) == '01.00' .or. trim(versionstring) == '02.00') then
             !
             ! reset mass of sediment per fraction to zero
             !
             msed = 0.0_fp
             thlyr = 0.0_fp
             !
             ! allocate temporary array
             !
             ilyr = 0
             allocate(rtemp(nmlb:nmub,lsedtot), stat = istat)
             if (istat == 0) allocate(thtemp(nmlb:nmub), stat = istat)
             if (istat /= 0) then
                call mess(LEVEL_ERROR, 'RdIniMorLyr: memory alloc error')  
                error = .true.
                return
             endif
             !
             do i = 1, size(mor_ptr%child_nodes)
                !
                ! Does mor_ptr contain a child with name 'Layer' (converted to lower case)?
                !
                layer_ptr => mor_ptr%child_nodes(i)%node_ptr
                parname = tree_get_name( layer_ptr )
                call small(parname, len(parname))
                if ( trim(parname) /= 'layer') cycle
                !
                ! Increment ilyr, but do not exceed nlyr
                !
                ilyr = min(nlyr, ilyr+1)
                !
                ! Initialize/reset the temporary array
                !
                rtemp  = 0.0_fp
                thtemp = 0.0_fp
                !
                ! Layer group found, scan it for the layer composition
                !
                layertype = ' '
                call prop_get_string(layer_ptr, '*', 'Type', layertype)
                call small(layertype, len(layertype))
                if (layertype == ' ') then
                   !
                   ! no Type field found
                   !
                   write (message,'(a,i2,2a)') 'No type specified for layer ', ilyr, &
                                             & ' in file ', trim(flcomp)
                   call mess(LEVEL_ERROR, message)  
                   error = .true.
                   return
                elseif (layertype == 'mass fraction' .or. &
                      & layertype == 'volume fraction') then
                   !
                   ! mass or volume fraction and layer thickness specified
                   !
                   parname  = 'Thick'
                   filename = ' '
                   call prop_get_string(layer_ptr, '*', parname, filename)
                   !
                   ! Intel 7.0 crashes on an inquire statement when file = ' '
                   !
                   if (filename == ' ') filename = 'dummyname'
                   inquire (file = filename, exist = ex)
                   if (.not. ex) then
                      !
                      ! Constant thickness
                      !
                      sedbed = rmissval
                      success = .true.
                      call prop_get(layer_ptr, '*', parname, sedbed, success, valuesfirst=.true.)
                      if (.not.success) then
                         if (filename == 'dummyname') then ! string was empty or key not found
                            write (message,'(a,i2,2a)')  &
                               & 'No value assigned to Thick for layer ', ilyr,' in file ', trim(flcomp)
                         else
                            write (message,'(3a,i2,2a)')  &
                               & 'Invalid file or value "',trim(filename),'" assigned to Thick for layer ', ilyr,' in file ', trim(flcomp)
                         endif
                         call mess(LEVEL_ERROR, message)
                         error = .true.
                         return
                      endif
                      do nm = 1, nmmax
                         thtemp(nm) = sedbed
                      enddo
                   else
                      !
                      ! Spatially varying thickness
                      !
                      call depfil_stm(lundia    ,error     ,filename  ,fmttmp    , &
                                    & thtemp    ,1         ,1         ,dims,   message)
                      if (error) then
                         call mess(LEVEL_ERROR, message)  
                         write (message,'(3a,i2,2a)')  &
                             & 'Error reading thickness from ', trim(filename), &
                             & ' for layer ', ilyr, ' in file ', trim(flcomp)
                         call mess(LEVEL_ERROR, message)  
                         return          
                      endif
                   endif
                   !
                   anyfrac = .false.
                   totfrac = 0.0_fp
                   do ised = 1, lsedtot
                      !
                      ! Scan file for fractions
                      !
                      if (trim(versionstring) == '01.00') then
                         write(lstr,'(i10)') ised
                         length = 10
                         call remove_leading_spaces(lstr, length)
                         !
                         ! Keyword SedBed<i> may not be used when layertype is fraction
                         !
                         parname  = 'SedBed' // trim(lstr)
                         filename = ' '
                         call prop_get_string(layer_ptr, '*', parname, filename)
                         if (filename /= ' ') then
                            write (message,'(7a,i2,2a)')  &
                                & 'Use Fraction' ,trim(lstr), ' instead of SedBed', &
                                & trim(lstr), ' for ', trim(layertype), ' layer ', &
                                & ilyr, ' in file ', trim(flcomp)
                            call mess(LEVEL_ERROR, message)  
                            error = .true.
                            return
                         endif
                         !
                         parname  = 'Fraction' // trim(lstr)
                      else
                         parname = namsed(ised)
                      endif
                      filename = ' '
                      call prop_get_string(layer_ptr, '*', parname, filename)
                      !
                      ! Intel 7.0 crashes on an inquire statement when file = ' '
                      !
                      if (filename == ' ') filename = 'dummyname'
                      inquire (file = filename, exist = ex)
                      if (.not. ex) then
                         !
                         ! Constant fraction
                         !
                         fraction = rmissval
                         success = .true.
                         call prop_get(layer_ptr, '*', parname, fraction, success, valuesfirst=.true.)
                         if (.not.success) then
                            if (filename == 'dummyname') then ! string was empty or key not found
                               fraction = 0.0_fp
                            else
                               write (message,'(7a,i2,2a)')  &
                                  & 'Invalid file or value "',trim(filename),'" assigned to ',trim(parname),' of ', trim(layertype), ' layer ', ilyr,' in file ', trim(flcomp)
                               call mess(LEVEL_ERROR, message)
                               error = .true.
                               return
                            endif
                         else
                            anyfrac = .true.
                         endif
                         do nm = 1, nmmax
                            rtemp(nm, ised) = fraction
                         enddo
                      else
                         !
                         ! Spatially varying fraction
                         !
                         anyfrac = .true.
                         call depfil_stm(lundia    ,error     ,filename  , fmttmp    , &
                                       & rtemp(nmlb,ised)        ,1   ,1    ,dims, message)
                         if (error) then
                            call mess(LEVEL_ERROR, message)  
                            write (message,'(a,i2,3a,i2,2a)')  &
                                & 'Error reading fraction ', ised, 'from ', &
                                & trim(filename), ' for layer ', ilyr, ' in file ', &
                                & trim(flcomp)
                            call mess(LEVEL_ERROR, message)  
                            return
                         endif
                      endif
                   enddo
                   !
                   ! Check if we have found any information that makes sense.
                   !
                   if (.not. anyfrac) then
                      write (message,'(a,i2,2a)')  &
                          & 'No data found for any sediment fraction in the data block of layer ' ,ilyr, &
                          & ' in file ', trim(flcomp)
                      call mess(LEVEL_ERROR, message)  
                      error = .true.
                      return
                   endif
                   !
                   ! Check validity of input data.
                   !
                   do nm = 1, nmmax
                      if (dims%celltype(nm) == 1) then
                         !
                         ! At an internal point the composition of all layers is important.
                         ! Check the values carefully before continuing.
                         !
                         if (thtemp(nm) < 0.0_fp) then
                            write (message,'(a,i2,3a,i0)')  &
                                & 'Negative sediment thickness specified for layer ', &
                                & ilyr, ' in file ', trim(flcomp), ' at nm=', nm
                            call mess(LEVEL_ERROR, message)  
                            error = .true.
                            return
                         endif
                         totfrac = 0.0_fp
                         do ised = 1, lsedtot
                            if (rtemp(nm, ised) < 0.0_fp) then
                               write (message,'(2a,i2,a,i2,3a,i0)')  &
                                   & 'Negative ', trim(layertype), ised, ' in layer ', &
                                   & ilyr, ' in file ', trim(flcomp), ' at nm=', nm
                               call mess(LEVEL_ERROR, message)  
                               error = .true.
                               return
                            elseif (rtemp(nm, ised) > 1.0_fp) then
                               write (message,'(a,i2,a,i2,3a,i0)')  &
                                   & trim(layertype), ised, ' bigger than 1 in layer ', &
                                   & ilyr, ' in file ', trim(flcomp), ' at nm=', nm
                               call mess(LEVEL_ERROR, message)  
                               error = .true.
                               return
                            endif
                            totfrac = totfrac + rtemp(nm,ised)
                         enddo
                         if (abs(totfrac-1.0_fp) > 1e-4_fp) then
                            write (message,'(3a,i2,3a,i0)')  &
                                & 'Sum of ', trim(layertype), ' not equal to 1 in layer ', &
                                & ilyr, ' in file ', trim(flcomp), ' at nm=', nm
                            call mess(LEVEL_ERROR, message)  
                            error = .true.
                            return
                         else
                            totfrac = 0.0_fp
                            do ised = 1, lsedtot-1
                               totfrac = totfrac + rtemp(nm, ised)
                            enddo
                            rtemp(nm, lsedtot) = 1.0_fp - totfrac
                         endif
                      elseif (dims%celltype(nm) == -1) then
                         !
                         ! At a ghost point the composition is also important
                         ! since it determines the fractions and hence the
                         ! the grain sizes and transport rates. We don't need
                         ! to raise an error since the owning partition will do
                         ! so and because of complications with open boundaries
                         ! located in ghost areas. However, we still need to
                         ! process it just like an internal point.
                         !
                         ! Arguments: The bed stratigraphy provides critical fraction information
                         !            which we don't want to exchange.
                         !            Open boundaries in ghost cell area cannot be distinguished
                         !            and may not have valid data.
                         !            The other partition will raise errors if necessary.
                         !
                         thtemp(nm) = max(0.0_fp, thtemp(nm))
                         totfrac = 0.0_fp
                         do ised = 1, lsedtot-1
                            rtemp(nm, ised) = max(0.0_fp, min(rtemp(nm, ised), 1.0_fp - totfrac))
                            totfrac = totfrac + rtemp(nm, ised)
                         enddo
                         rtemp(nm, lsedtot) = 1.0_fp - totfrac
                      elseif (dims%celltype(nm) == 2 .and. ilyr == 1) then
                         !
                         ! At an open boundary only the composition of the transport layer
                         ! is important. If it is not valid, mark the data as dummy data:
                         ! the data will be overwritten with data coming from the neighbouring
                         ! internal point.
                         !
                         totfrac = 0.0_fp
                         err2     = .false.
                         do ised = 1, lsedtot
                            if (rtemp(nm, ised) < 0.0_fp .or. rtemp(nm, ised) > 1.0_fp) err2=.true.
                            totfrac = totfrac + rtemp(nm,ised)
                         enddo
                         if (comparereal(totfrac,1.0_fp) /= 0) err2=.true.
                         if (thtemp(nm)<0.0_fp) err2=.true.
                         if (err2) then
                            !
                            ! dummy
                            !
                            rtemp(nm, 1) = -1.0_fp
                         endif
                      else
                         !
                         ! Point/layer that will never be used: don't care about the
                         ! values. Just replace whatever was read by something valid.
                         !
                         do ised = 1, lsedtot
                            rtemp(nm, ised) = 0.0_fp
                         enddo
                         thtemp(nm) = 0.0_fp
                      endif
                   enddo
                   !
                   ! Copy RTEMP data to open boundary points that have not
                   ! yet been assigned valid data.
                   !
                   do ibnd = 1, size(dims%nmbnd,1)
                      nm  = dims%nmbnd(ibnd,1)
                      nm2 = dims%nmbnd(ibnd,2)
                      do ised = 1, lsedtot
                         rtemp(nm, ised) = rtemp(nm2, ised)
                      enddo
                      thtemp(nm) = thtemp(nm2)
                   enddo
                   !
                   ! convert mass fractions into volume fractions
                   !
                   if (iporosity == 0) then
                      if (layertype == 'mass fraction') then
                         do nm = 1, nmmax
                            cdrybavg = 0.0_fp
                            do ised = 1, lsedtot
                               cdrybavg = cdrybavg + rtemp(nm, ised)/cdryb(ised)
                            enddo
                            if (cdrybavg > 0.0_fp) then
                               cdrybavg = max(cdrybavg, 1.0e-8_fp)
                               cdrybavg = 1.0_fp / cdrybavg
                               do ised = 1, lsedtot
                                  rtemp(nm, ised) = rtemp(nm, ised) * cdrybavg / cdryb(ised)
                               enddo
                            endif
                         enddo
                      endif
                      !
                      ! add thicknesses in lyrfrac
                      !
                      do nm = 1, nmmax
                         do ised = 1, lsedtot
                            msed(ised, ilyr, nm) = msed(ised, ilyr, nm) + rtemp(nm, ised)*thtemp(nm)*cdryb(ised)
                         enddo
                         thlyr(ilyr, nm) = thlyr(ilyr, nm) + thtemp(nm)
                      enddo
                   else
                      if (layertype == 'volume fraction') then
                         do nm = 1, nmmax
                            mfracsum = 0.0_fp
                            do ised = 1, lsedtot
                               mfrac(ised) = rtemp(nm, ised) * rhosol(ised)
                               mfracsum = mfracsum + mfrac(ised)
                            enddo
                            do ised = 1, lsedtot
                               mfrac(ised) = mfrac(ised) / mfracsum
                            enddo
                            !
                            call getporosity(morlyr, mfrac, poros)
                            svf = 1.0_fp - poros
                            !
                            do ised = 1, lsedtot
                               msed(ised, ilyr, nm) = msed(ised, ilyr, nm) + rtemp(nm, ised)*thtemp(nm)*rhosol(ised)*svf
                            enddo
                            thick = thlyr(ilyr, nm) + thtemp(nm)
                            svfrac(ilyr, nm) = (thlyr(ilyr, nm) * svfrac(ilyr, nm) + thtemp(nm) * svf) / thick
                            thlyr(ilyr, nm)  = thick
                         enddo
                      else ! layertype == 'mass fraction'
                         do nm = 1, nmmax
                            vfracsum = 0.0_fp
                            do ised = 1, lsedtot
                               mfrac(ised) = rtemp(nm, ised)
                               rtemp(nm, ised) = rtemp(nm, ised) / rhosol(ised)
                               vfracsum = vfracsum + rtemp(nm, ised)
                            enddo
                            do ised = 1, lsedtot
                               rtemp(nm, ised) = rtemp(nm, ised) / vfracsum
                            enddo
                            !
                            call getporosity(morlyr, mfrac, poros)
                            svf = 1.0_fp - poros
                            !
                            do ised = 1, lsedtot
                               msed(ised, ilyr, nm) = msed(ised, ilyr, nm) + rtemp(nm, ised)*thtemp(nm)*rhosol(ised)*svf
                            enddo
                            thick = thlyr(ilyr, nm) + thtemp(nm)
                            svfrac(ilyr, nm) = (thlyr(ilyr, nm) * svfrac(ilyr, nm) + thtemp(nm) * svf) / thick
                            thlyr(ilyr, nm)  = thick
                         enddo
                      endif
                   endif
                   !
                elseif (layertype == 'sediment mass' .or. &
                      & layertype == 'sediment thickness') then
                   !
                   ! sediment mass as specified in sediment input file
                   !
                   anysedbed = .false.
                   do ised = 1, lsedtot
                      !
                      ! Scan file for sediment masses
                      !
                      if (trim(versionstring) == '01.00') then
                         write(lstr,'(i10)') ised
                         length = 10
                         call remove_leading_spaces(lstr, length)
                         !
                         ! Keyword Fraction<i> may not be used when layertype is sediment
                         !
                         parname  = 'Fraction' // trim(lstr)
                         filename = ' '
                         call prop_get_string(layer_ptr, '*', parname, filename)
                         if (filename /= ' ') then
                            write (message,'(7a,i2,2a)')  &
                                & 'Use SedBed', trim(lstr), ' instead of Fraction', &
                                & trim(lstr), ' for ', trim(layertype), ' layer ', &
                                & ilyr, ' in file ', trim(flcomp)
                            call mess(LEVEL_ERROR, message)  
                            error = .true.
                            return
                         endif
                         !
                         parname  = 'SedBed' // trim(lstr)
                      else
                         parname = namsed(ised)
                      endif
                      filename = ' '
                      call prop_get_string(layer_ptr, '*', parname, filename)
                      !
                      ! Intel 7.0 crashes on an inquire statement when file = ' '
                      !
                      if (filename == ' ') filename = 'dummyname'
                      inquire (file = filename, exist = ex)
                      if (.not. ex) then
                         !
                         ! Constant thickness or mass
                         !
                         sedbed = rmissval
                         call prop_get(layer_ptr, '*', parname, sedbed)
                         if (comparereal(sedbed,rmissval) == 0) then
                            sedbed = 0.0_fp
                         elseif (sedbed<0.0_fp) then
                            write (message,'(a,e12.4,5a,i2,3a)')  &
                                & 'Invalid value ',sedbed,' for ', trim(parname), &
                                & ' of ', trim(layertype), ' layer ', &
                                & ilyr, ' in file ', trim(flcomp), &
                                & ' Positive value required.'
                            call mess(LEVEL_ERROR, message)  
                            error = .true.
                            return
                         else
                            anysedbed = .true.
                         endif
                         do nm = 1, nmmax
                            rtemp(nm, ised) = sedbed
                         enddo
                      else
                         !
                         ! Spatially varying thickness or mass
                         !
                         anysedbed = .true.
                         call depfil_stm(lundia    ,error     ,filename  , fmttmp    , &
                                       & rtemp(nmlb,ised)     ,1   ,1    ,dims, message)
                         if (error) then
                            call mess(LEVEL_ERROR, message)  
                            write (message,'(5a,i2,2a)')  &
                                & 'Error reading ', layertype, '  from ', trim(filename), &
                                & ' for layer ', ilyr, ' in file ', trim(flcomp)
                            call mess(LEVEL_ERROR, message)  
                            return
                         endif
                      endif
                   enddo
                   !
                   ! Check if we have found any information that makes sense.
                   !
                   if (.not. anysedbed) then
                      write (message,'(a,i2,2a)')  &
                          & 'No data found for any sediment fraction in the data block of layer ' ,ilyr, &
                          & ' in file ' ,trim(flcomp)
                      call mess(LEVEL_ERROR, message)  
                      error = .true.
                      return
                   endif
                   !
                   ! Check validity of input data.
                   !
                   do nm = 1, nmmax
                      if (dims%celltype(nm) == 1) then
                         !
                         ! At an internal point the composition of all layers is important.
                         ! Check the values carefully before continuing.
                         !
                         do ised = 1, lsedtot
                            if (rtemp(nm, ised) < 0.0_fp) then
                               write (message,'(2a,i2,a,i2,3a,i0)')  &
                                   & 'Negative ', trim(layertype), ised, ' in layer ', &
                                   & ilyr, ' in file ', trim(flcomp), ' at nm=', nm
                               call mess(LEVEL_ERROR, message)  
                               error = .true.
                               return
                            endif
                         enddo
                      elseif (dims%celltype(nm) == -1) then
                         !
                         ! At a ghost point the composition is also important
                         ! since it determines the fractions and hence the
                         ! the grain sizes and transport rates. We don't need
                         ! to raise an error since the owning partition will do
                         ! so and because of complications with open boundaries
                         ! located in ghost areas.
                         !
                         do ised = 1, lsedtot
                            rtemp(nm, ised) = max(0.0_fp, rtemp(nm, ised))
                         enddo
                      elseif (dims%celltype(nm) == 2 .and. ilyr == 1) then
                         !
                         ! At an open boundary only the composition of the transport layer
                         ! is important. If it is not valid, mark the data as dummy data:
                         ! the data will be overwritten with data coming from the neighbouring
                         ! internal point.
                         !
                         err2 = .false.
                         do ised = 1, lsedtot
                            if (rtemp(nm, ised) < 0.0_fp) err2=.true.
                         enddo
                         if (err2) then
                            !
                            ! dummy
                            !
                            rtemp(nm, 1) = -1.0_fp
                         endif
                      else
                         !
                         ! Point/layer that will never be used: don't care about the
                         ! values. Just replace whatever was read by something valid.
                         !
                         do ised = 1, lsedtot
                            rtemp(nm, ised) = 0.0_fp
                         enddo
                      endif
                   enddo
                   !
                   ! Copy RTEMP data to open boundary points that have not
                   ! yet been assigned valid data.
                   !
                   do ibnd = 1, size(dims%nmbnd,1)
                      nm  = dims%nmbnd(ibnd,1)
                      nm2 = dims%nmbnd(ibnd,2)
                      do ised = 1, lsedtot
                         rtemp(nm, ised) = rtemp(nm2, ised)
                      enddo
                   enddo
                   !
                   ! convert sediment mass to sediment thickness
                   !
                   if (iporosity == 0) then
                      if (layertype == 'sediment thickness') then
                         do ised = 1, lsedtot
                            do nm = 1, nmmax
                               rtemp(nm, ised) = rtemp(nm, ised) * cdryb(ised)
                            enddo
                         enddo
                      endif
                      !
                      ! add masses in msed and thicknesses in thlyr
                      !
                      do ised = 1, lsedtot
                         do nm = 1, nmmax
                            msed(ised, ilyr, nm) = msed(ised, ilyr, nm) + rtemp(nm, ised)
                            thlyr(ilyr, nm)   = thlyr(ilyr, nm)    + rtemp(nm, ised)/cdryb(ised)
                         enddo
                      enddo
                   else
                      if (layertype == 'sediment thickness') then
                         do nm = 1, nmmax
                            mfracsum = 0.0_fp
                            do ised = 1, lsedtot
                               mfrac(ised) = rtemp(nm, ised) * rhosol(ised)
                               mfracsum = mfracsum + mfrac(ised)
                            enddo
                            if (mfracsum>0.0_fp) then
                               do ised = 1, lsedtot
                                  mfrac(ised) = mfrac(ised) / mfracsum
                               enddo
                               !
                               call getporosity(morlyr, mfrac, poros)
                               svf = 1.0_fp - poros
                            else
                               svf = 1.0_fp
                            endif
                            !
                            thtemp(nm) = 0.0_fp
                            do ised = 1, lsedtot
                               msed(ised, ilyr, nm) = msed(ised, ilyr, nm) + rtemp(nm, ised) * rhosol(ised) * svf
                               thtemp(nm)        = thtemp(nm)        + rtemp(nm, ised)
                            enddo
                            thick = thlyr(ilyr, nm) + thtemp(nm)
                            svfrac(ilyr, nm) = (thlyr(ilyr, nm) * svfrac(ilyr, nm) + thtemp(nm) * svf) / thick
                            thlyr(ilyr, nm)  = thick
                         enddo
                      else ! layertype == 'sediment mass'
                         !
                         ! add masses in msed and thicknesses in thlyr
                         !
                         do nm = 1, nmmax
                            vfracsum = 0.0_fp
                            do ised = 1, lsedtot
                               mfrac(ised) = rtemp(nm, ised)
                               rtemp(nm, ised) = rtemp(nm, ised) / rhosol(ised)
                               vfracsum = vfracsum + rtemp(nm, ised)
                            enddo
                            do ised = 1, lsedtot
                               rtemp(nm, ised) = rtemp(nm, ised) / vfracsum
                            enddo
                            !
                            thtemp(nm) = 0.0_fp
                            do ised = 1, lsedtot
                               msed(ised, ilyr, nm) = msed(ised, ilyr, nm) + rtemp(nm, ised) * rhosol(ised) * svf
                               thtemp(nm)        = thtemp(nm)        + rtemp(nm, ised)
                            enddo
                            thick = thlyr(ilyr, nm) + thtemp(nm)
                            svfrac(ilyr, nm) = (thlyr(ilyr, nm) * svfrac(ilyr, nm) + thtemp(nm) * svf) / thick
                            thlyr(ilyr, nm)  = thick
                         enddo
                      endif
                   endif
                else
                   write (message,'(3a,i2,2a)') 'Unknown layer type ''', &
                    & trim(layertype), ''' specified for layer ', ilyr, &
                    & ' in file ', trim(flcomp)
                   call mess(LEVEL_ERROR, message)  
                   error = .true.
                   return
                endif
                !
                ! Copy data to open boundary points
                ! 
                do ibnd = 1, size(dims%nmbnd,1)
                   nm  = dims%nmbnd(ibnd,1)
                   nm2 = dims%nmbnd(ibnd,2)
                   svfrac(ilyr, nm) = svfrac(ilyr, nm2)
                   thlyr(ilyr, nm) = thlyr(ilyr, nm2)
                   do ised = 1, lsedtot
                      msed(ised, ilyr, nm) = msed(ised, ilyr, nm2)
                   enddo
                enddo             
             enddo
             !
             deallocate(rtemp , stat = istat)
             deallocate(thtemp, stat = istat)
             !
          else
             write (message,'(3a)') 'Invalid file version of ', trim(flcomp), '. Expecting [BedCompositionFileInformation] FileVersion = 01.00 or 02.00'
             call mess(LEVEL_ERROR, message)  
             error = .true.
             return          
          endif
       endif
    endif
end subroutine rdinimorlyr
                     
end module m_rdmorlyr
