!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2019.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! $Id$
! $HeadURL$
!-------------------------------------------------------------------------------------------------------
!  Origin: 
!     URL: https://svn.oss.deltares.nl/repos/delft3d/trunk/src/engines_gpl/flow2d3d/packages/data/include/fourier.igs
!     Revision: 5108

module m_fourier_analysis

! TODO:
!     * finalizing 
!     * ucx en ucy on (1:#nods) is altijd de diepte gemiddelde, ook in 2d (dan is het de enige snelheid) 

    use precision 
    use string_module, only: str_lower
    use unstruc_netcdf
    use m_flow, only : kmx
    implicit none

    type gd_fourier
       !
       ! integers
       !
       integer :: fouwrt       ! Time to write fourier file (TEKAL/map-NetCDF)
       integer :: nofouvar = 0 ! Number of parameters to write to NetCDF file
       integer :: ibluc
       integer :: ibluv
       integer :: ibluva
       integer :: iblqf
       integer :: iblbs
       integer :: iblep
       integer :: iblwl
       integer :: iblws
       integer :: ibleh
       integer :: iblcn
       !
       ! pointers
       !
       integer          , dimension(:)    , pointer :: fconno        ! Constituent number for Fourier analysis
       integer          , dimension(:)    , pointer :: flayno        ! Layer number for fourier analysis
       integer          , dimension(:)    , pointer :: fnumcy        ! Number of cycles for fourier analysis
       integer          , dimension(:)    , pointer :: ftmsto        ! Integer time step counter stop time for fourier analysis
       integer          , dimension(:)    , pointer :: ftmstr        ! Integer time step counter start time for fourier analysis
       integer          , dimension(:)    , pointer :: foumask       ! 0: no additional mask, 1: initial dry points only
       integer          , dimension(:,:)  , pointer :: idvar         ! Ids of the variables in UGRID format
       integer          , dimension(:,:)  , pointer :: fouref        ! Reference table: (ifou,1): fouid (line in input file)
                                                                     !                  (ifou,2): fouvarstart (first index in fouvarnam/idvar to be used by this ifou
       !
       real(kind=fp)    , dimension(:)    , pointer :: fknfac        ! Fourier amplitude correction factor
       real(kind=fp)    , dimension(:,:,:), pointer :: foucomp       ! Component in Fourier Analysis
       real(kind=fp)    , dimension(:)    , pointer :: foufas        ! Frequency for fourier analysis
       real(kind=fp)    , dimension(:,:,:), pointer :: fousma        ! Suma of fourier analysis
       real(kind=fp)    , dimension(:,:,:), pointer :: fousmb        ! Sumb of fourier analysis
       real(kind=fp)    , dimension(:,:,:), pointer :: fouvec        !  Maximum of vector magnitude for fourier analysis
                                                                     !  For velocity (u,v), discharge (qxk, qyk) and bed shear stress (taubpu,taubpv)
                                                                     !  NB: For discharges the analysis is actually performed on the unit discharges qxk/guu and qyk/gvv
       real(kind=fp)    , dimension(:)    , pointer :: fv0pu         ! Fourier phase correction
       !    
       character(len=1) , dimension(:)    , pointer :: fouelp        !  Y/N: Yes/No requesting elliptic parameters
                                                                     !  X/I: Max/Min values  requested instead of fourier analysis
                                                                     !  E  : Max Energy head requested instead of fourier analysis
       character(len=16), dimension(:)    , pointer :: founam        ! Names of variables for fourier analysis 
       character(len=50), dimension(:)    , pointer :: fouvarnam     ! Names of variables for fourier analysis as written to NetCDF file 
       character(len=50), dimension(:)    , pointer :: fouvarnamlong ! Part of the long names of variables for fourier analysis as written to NetCDF file 
       character(len=50), dimension(:)    , pointer :: fouvarunit    ! Unit of variables for fourier analysis as written to NetCDF file 
       character(len=1) , dimension(:)    , pointer :: foutyp        ! Character indicating whether parameter is a scalar (s) or vector (v) quantity
    end type gd_fourier
!-------------------------------------------------------------------------------------------------------
    
    type(gd_fourier), target :: gdfourier
    character(len=:), allocatable   :: FouOutputFile

    private

    integer                   :: nofou  !< Number of fourier components to be analyzed
    type(t_unc_mapids)        :: fileids!< Set of file and variable ids for this file.

    real(kind=fp)             :: ag_fouana = 9.81d0  
    real(kind=fp)             :: time_unit_factor

    public fouini
    public alloc_fourier_analysis_arrays
    public count_fourier_variables
    public reafou
    public postpr_fourier

    public fourierIsActive, fourierWithUc, fourierWithWindspeed
    public nofou
    public FouOutputFile

    contains

!> fourier is (still) active
   logical function fourierIsActive()
       fourierIsActive = (nofou > 0)
    end function fourierIsActive

!> do fourier with flow magnitue or not
    logical function fourierWithUc()
       fourierWithUc = (gdfourier%ibluc>0)
    end function fourierWithUc

!> do fourier with wind speed or not
    logical function fourierWithWindspeed()
       integer :: ifou
       fourierWithWindspeed = .false.
       do ifou = 1, nofou                         ! scan for windspeed in the list of fourier requests to see whether or not to allocate wmag
          if (gdfourier%founam(ifou)=='ws') then
             fourierWithWindspeed = .true.
          endif
       enddo
    end function fourierWithWindspeed

!> count the number of fourier/min/max quantities
    subroutine count_fourier_variables
       implicit none
       integer :: ifou, ivar
       ifou  = 1
       gdfourier%iblwl = 0
       gdfourier%iblws = 0
       gdfourier%ibleh = 0
       gdfourier%iblcn = 0
       gdfourier%ibluv = 0
       gdfourier%ibluva= 0
       gdfourier%ibluc = 0
       gdfourier%iblqf = 0
       gdfourier%iblbs = 0
       gdfourier%iblep = 0
       do ivar=1, nofou
          !
          select case (gdfourier%founam(ivar)(:2))
          case ('s1')
             if (gdfourier%fouelp(ifou)=='e') then
                gdfourier%ibleh = gdfourier%ibleh + 1
             else
                gdfourier%iblwl = gdfourier%iblwl + 1
             endif
          case ('ws')
             gdfourier%iblws = gdfourier%iblws + 1
          case ('ux')
             gdfourier%ibluv = gdfourier%ibluv + 1
          case ('uy')
             gdfourier%ibluv = gdfourier%ibluv + 1
          case ('uxa') ! todo: unreachable code
             gdfourier%ibluva = gdfourier%ibluva + 1
          case ('uya') ! todo: unreachable code
             gdfourier%ibluva = gdfourier%ibluva + 1
          case ('uc')
             gdfourier%ibluc = gdfourier%ibluc + 1
          case ('r1')
             gdfourier%iblcn = gdfourier%iblcn + 1
          case ('u1')
             gdfourier%ibluv = gdfourier%ibluv + 1
          case ('qx')
             gdfourier%iblqf = gdfourier%iblqf + 1
          case ('ta')
             gdfourier%iblbs = gdfourier%iblbs + 1
          end select
       enddo
    end subroutine count_fourier_variables

!> allocate the arrays for fourier and min/max
   subroutine alloc_fourier_analysis_arrays(gddimens)
   !!--declarations----------------------------------------------------------------
       use precision
       use m_d3ddimens

       !
       implicit none
       !
       type(gd_dimens), intent(inout)     :: gddimens

       integer                      ::    istat
       integer, parameter           ::    imissval = -1
       double precision, parameter  ::    rmissval = -999.999_fp

       istat = 0
       !
       ! Arrays for Fourier analysis (fourier.igs)
       !
       if (istat == 0) allocate (gdfourier%fconno  (1:nofou), STAT = istat)
       if (istat == 0) allocate (gdfourier%flayno  (1:nofou), STAT = istat)
       if (istat == 0) allocate (gdfourier%fnumcy  (1:nofou), STAT = istat)
       if (istat == 0) allocate (gdfourier%ftmsto  (1:nofou), STAT = istat)
       if (istat == 0) allocate (gdfourier%ftmstr  (1:nofou), STAT = istat)
       if (istat == 0) allocate (gdfourier%foumask (1:nofou), STAT = istat)
       if (istat == 0) allocate (gdfourier%idvar   (3,1:gdfourier%nofouvar), STAT = istat)
       if (istat == 0) allocate (gdfourier%fouref  (1:nofou,2), STAT = istat)
       !
       if (istat == 0) allocate (gdfourier%fknfac (                          1:nofou), STAT = istat)
       if (istat == 0) allocate (gdfourier%foucomp(gddimens%nlb:gddimens%nub, gddimens%mlb:gddimens%mub, 1:nofou), STAT = istat)
       if (istat == 0) allocate (gdfourier%foufas (                          1:nofou), STAT = istat)                                         
       if (istat == 0) allocate (gdfourier%fousma (gddimens%nlb:gddimens%nub, gddimens%mlb:gddimens%mub, 1:nofou), STAT = istat)
       if (istat == 0) allocate (gdfourier%fousmb (gddimens%nlb:gddimens%nub, gddimens%mlb:gddimens%mub, 1:nofou), STAT = istat)
       if (istat == 0) allocate (gdfourier%fouvec (gddimens%nlb:gddimens%nub, gddimens%mlb:gddimens%mub, 1:nofou), STAT = istat)
       if (istat == 0) allocate (gdfourier%fv0pu  (                          1:nofou), STAT = istat)
       !
       if (istat == 0) allocate (gdfourier%fouelp (1:nofou), STAT = istat)
       if (istat == 0) allocate (gdfourier%founam (1:nofou), STAT = istat)
       if (istat == 0) allocate (gdfourier%fouvarnam     (1:gdfourier%nofouvar), STAT = istat)
       if (istat == 0) allocate (gdfourier%fouvarnamlong (1:gdfourier%nofouvar), STAT = istat)
       if (istat == 0) allocate (gdfourier%fouvarunit    (1:gdfourier%nofouvar), STAT = istat)
       if (istat == 0) allocate (gdfourier%foutyp (1:nofou), STAT = istat)

       if (istat /= 0) then
          ! Exception handling for allocation of fourier arrays 
          msgbuf = 'Allocation error in alloc_fourier_analysis_arrays; Fourier disabled.'
          call warn_flush()
          nofou = 0
       else
          ! Initialise arrays for Fourier analysis
          gdfourier%fconno   = imissval
          gdfourier%flayno   = imissval
          gdfourier%fnumcy   = imissval
          gdfourier%ftmsto   = imissval
          gdfourier%ftmstr   = imissval
          gdfourier%foumask  = imissval
          gdfourier%idvar    = imissval
          gdfourier%fouref   = imissval
          !
          gdfourier%fknfac   = rmissval
          gdfourier%foucomp  = rmissval
          gdfourier%foufas   = rmissval
          gdfourier%fousma   = rmissval
          gdfourier%fousmb   = rmissval
          gdfourier%fouvec   = rmissval
          gdfourier%fv0pu    = rmissval
          !
          gdfourier%fouelp        = ' '
          gdfourier%founam        = ' '
          gdfourier%fouvarnam     = ' '
          gdfourier%fouvarnamlong = ' '
          gdfourier%fouvarunit    = ' '
          gdfourier%foutyp        = ' '
          !
       endif
   end subroutine alloc_fourier_analysis_arrays

!> - Read fourier input file and stores the
!! variables necessary for the analysis in arrays.
   subroutine reafou(lunfou, filfou, kmax, lstsc, lsal, ltem, tstart, tstop, dt, success)
   !!--declarations----------------------------------------------------------------
       use precision
       use mathconsts
       use string_module
       use unstruc_messages
       !
       implicit none
   !
   ! Global variables
   !
       integer                         , intent(in) :: lsal    !< Description and declaration in dimens.igs
       integer                         , intent(in) :: lstsc   !< Description and declaration in dimens.igs
       integer                         , intent(in) :: ltem    !< Description and declaration in dimens.igs
       integer                         , intent(in) :: lunfou  !< Unit number fourier input file
       character(*)                    , intent(in) :: filfou  !< File name for fourier analysis input
       integer                         , intent(in) :: kmax    !< number of vertical layers 
       real(fp)                        , intent(in) :: tstart  !< simulation start time 
       real(fp)                        , intent(in) :: tstop   !< simulation stop time 
       real(fp)                        , intent(in) :: dt      !< timestep 
       logical                         , intent(out):: success !< function result
   !
   ! Local variables
   !
       integer        , dimension(:)   , pointer :: fconno
       integer        , dimension(:)   , pointer :: flayno
       integer        , dimension(:)   , pointer :: fnumcy
       integer        , dimension(:)   , pointer :: foumask
       integer        , dimension(:,:) , pointer :: idvar
       integer        , dimension(:,:) , pointer :: fouref
       integer                         , pointer :: fouwrt
       integer                         , pointer :: nofouvar
       integer        , dimension(:)   , pointer :: ftmsto
       integer        , dimension(:)   , pointer :: ftmstr
       real(fp)       , dimension(:)   , pointer :: fknfac
       real(fp)       , dimension(:)   , pointer :: foufas
       real(fp)       , dimension(:,:,:) , pointer :: fousma
       real(fp)       , dimension(:,:,:) , pointer :: fousmb
       real(fp)       , dimension(:)   , pointer :: fv0pu
       character(1)   , dimension(:)   , pointer :: fouelp
       character(16)  , dimension(:)   , pointer :: founam
       character(50)  , dimension(:)   , pointer :: fouvarnam
       character(50)  , dimension(:)   , pointer :: fouvarnamlong
       character(50)  , dimension(:)   , pointer :: fouvarunit
       character(1)   , dimension(:)   , pointer :: foutyp
       integer, parameter :: maxvld = 40     ! Maximum number of fields/columns that can be read from a record 

       integer                             :: fouid      ! Counter linenumber-commentlines
       integer                             :: i          ! Counter 
       integer                             :: ifou       ! Counter 
       integer                             :: ivar       ! Counter 
       integer                             :: irelp
       integer                             :: lfile      ! Length of file name 
       integer                             :: linenumber ! Line number in Fourier input file
       integer                             :: nopos      ! Used for format free reading 
       integer                             :: nveld      ! actual number of fields encountered in a record  
       integer                             :: iveld      ! loop counter over fields in a record 
       real(fp)                            :: rstart     ! Start time for fourier analysis 
       real(fp)                            :: rstop      ! Stop  time for fourier analysis 
       character(4)                        :: cdummy     ! Help string to read FOUELP 
                                                         ! The message depends on the error. 
       character(7)                        :: fmt        ! Used for format free reading 
       character(300)                      :: message
       character(132)                      :: record     ! Used for format free reading 
       character(30)  , dimension(maxvld)  :: columns    ! each record is split into separate fields (columns)
       integer                             :: iostat
   !
   !! executable statements -------------------------------------------------------
   !
       fknfac        => gdfourier%fknfac
       foufas        => gdfourier%foufas
       fv0pu         => gdfourier%fv0pu
       fconno        => gdfourier%fconno
       flayno        => gdfourier%flayno
       fnumcy        => gdfourier%fnumcy
       foumask       => gdfourier%foumask
       idvar         => gdfourier%idvar
       fouref        => gdfourier%fouref
       fouwrt        => gdfourier%fouwrt
       nofouvar      => gdfourier%nofouvar
       ftmsto        => gdfourier%ftmsto
       ftmstr        => gdfourier%ftmstr
       fouelp        => gdfourier%fouelp
       founam        => gdfourier%founam
       fouvarnam     => gdfourier%fouvarnam
       fouvarnamlong => gdfourier%fouvarnamlong
       fouvarunit    => gdfourier%fouvarunit
       foutyp        => gdfourier%foutyp
       fousma        => gdfourier%fousma
       fousmb        => gdfourier%fousmb

       !
       success = .false.
       ifou = 1
       do i = 1, nofou
          flayno(i)   = 1
          fconno(i)   = 1
          foumask(i)  = 0
          foutyp(i)   = 'n'
          fouref(i,:) = -1
          fouelp(i)   = 'n'
       enddo
       do i = 1, nofouvar
          idvar(:,i)       = 0
          fouvarnam(i)     = ' '
          fouvarnamlong(i) = ' '
          fouvarunit(i)    = ' '
       enddo
       !
       ! define length of file name
       !
       call remove_leading_spaces(filfou, lfile)
       !
       cdummy = ' '
       !
       linenumber = 0
       fouid      = 0
       !
       ! reading file
       !
       ! -->
      20 continue
       read (lunfou, '(a)',iostat=iostat) record
       if (iostat/=0) then 
          write (msgbuf, '(a)') 'Error reading record from .fou file.'
          call warn_flush()
          goto 6666
       endif 
       !
       linenumber = linenumber + 1
       fouid      = fouid      + 1
       !
       call str_lower(record, 132)
       columns = ''
       read(record,*,iostat=iostat) (columns(iveld),iveld=1,maxvld)          ! TODO: cover for read errors 
       do nveld = maxvld,1,-1
          if (len_trim(columns(nveld))>0) then 
             exit
          endif 
       enddo 
       !
       if (columns(1)(1:1)=='*' .or. nveld==0) then
          fouid = fouid - 1
          goto 20
       endif
       ! <--
       !
       ! determine array names and type (scalar or vectorial) for
       !       fourier analysis
       !
       founam(ifou) = trim(columns(1))
       !
       if (founam(ifou)=='wl') then
          founam(ifou)   = 's1              '
          foutyp(ifou)   = 's'
          fouref(ifou,1) = fouid
       elseif (founam(ifou)=='ws') then             ! absolute wind-speed 
          founam(ifou)   = 'ws              '
          foutyp(ifou)   = 's'
          fouref(ifou,1) = fouid
       elseif (founam(ifou)=='ux' .or. founam(ifou)=='uxa') then
          foutyp(ifou)   = 's'
          fouref(ifou,1) = fouid
       elseif (founam(ifou)=='uy' .or. founam(ifou)=='uya') then
          foutyp(ifou)   = 's'
          fouref(ifou,1) = fouid
       elseif (founam(ifou)=='eh') then
          !
          ! founam must be s1 to pass through s1 to fouana
          ! use fouelp to flag energy head
          !
          founam(ifou)   = 's1              '               ! waterlevel 
          foutyp(ifou)   = 's'
          fouref(ifou,1) = fouid
          fouelp(ifou)   = 'e'
       elseif (founam(ifou)=='uc') then                     ! absolute cell-centre velocity magnitude
          founam(ifou)   = 'uc              '
          foutyp(ifou)   = 's'
          fouref(ifou,1) = fouid
       elseif (founam(ifou)=='qf') then                     ! interpolated cell-centre velocities (vector)
          founam(ifou)     = 'qxk             '             ! ucx 
          foutyp(ifou)     = 's'
          fouref(ifou,1)   = fouid
       elseif (founam(ifou)=='bs') then
          founam(ifou)     = 'ta              '
          foutyp(ifou)     = 's'
          fouref(ifou,1)   = fouid
       elseif (founam(ifou)=='ct') then                     ! constituent: temperature (scalar)
          if (ltem/=0) then
             founam(ifou)   = 'r1              '
             foutyp(ifou)   = 's'
             fconno(ifou)   = ltem
             fouref(ifou,1) = fouid
          else
             ! Exception: no temperature 
             write (msgbuf, '(a)') 'Temperature specified in .fou file, but no temperature available. '
             call warn_flush()
             goto 6666
          endif
       elseif (founam(ifou)=='cs') then                     ! constituent: salinity (scalar)
          if (lsal/=0) then
             founam(ifou)   = 'r1              '
             foutyp(ifou)   = 's'
             fconno(ifou)   = lsal
             fouref(ifou,1) = fouid
          else
             ! Exception: no salt 
             write (msgbuf, '(a)') 'Salinity specified in .fou file, but no salinity available.'
             call warn_flush()
             goto 6666
          endif
       else                                                 ! constituent, anything else 
          read (founam(ifou)(2:2), '(i1)', iostat=iostat) fconno(ifou)
          if (iostat /= 0) then
             write (msgbuf, '(a)') 'Unable to initialize fourier analysis for '''//trim(founam(ifou))//'''.'
             call warn_flush()
             goto 6666
          endif
          fconno(ifou) = fconno(ifou) + max(lsal, ltem)
          if (fconno(ifou)>lstsc) then
             write (msgbuf, '(a)') 'Unable to initialize fourier analysis for constituent '''//trim(founam(ifou))//'''.'
             call warn_flush()
             goto 6666
          endif
          founam(ifou)   = 'r1             '
          foutyp(ifou)   = 's'
          fouref(ifou,1) = fouid
       endif
       !
       ! read start time, stop time, number of cycles
       !       determine corresponding integer time step numbers and frequency
       !
       fmt = '(f    )'
       !
       nopos = len_trim(columns(2))
       !
       if (nopos<10) then
          write (fmt(3:3), '(i1)') nopos
          write (fmt(4:5), '(a2)') '.0'
       else
          write (fmt(3:4), '(i2)') nopos
          write (fmt(5:6), '(a2)') '.0'
       endif
       !
       read (columns(2), fmt) rstart
       rstart = rstart * time_unit_factor           ! convert to kernel time unit
       !
       ftmstr(ifou) = nint(rstart/dt)
       !
       ! original code translates as : 'if abs(real(nint(rstart/dt)*dt-rstart) > (0.1_fp*dt)'         ! RL666
       if ( mod(real(rstart,fp),real(dt,fp)) > 0.1_fp*dt) then 
          ! call prterr(lundia, 'U044', errmsg)
          write (msgbuf,*) 'Fourier sample interval start not at an integer number of user timesteps DtUser.'
          call warn_flush()
          goto 6666
       endif
       !
       !
       if (rstart<tstart) then
          ! call prterr(lundia, 'F005', ' ')
          write (msgbuf,*) 'Fourier sample interval start preceeds simulation start TStart.'
          call warn_flush()
          goto 6666
       endif
       !
       nopos = len_trim(columns(3))
       !
       if (nopos<10) then
          write (fmt(3:3), '(i1)') nopos
          write (fmt(4:5), '(a2)') '.0'
       else
          write (fmt(3:4), '(i2)') nopos
          write (fmt(5:6), '(a2)') '.0'
       endif
       !
       read (columns(3), fmt) rstop
       rstop = rstop * time_unit_factor           ! convert to kernel time unit
       !
       ftmsto(ifou) = nint(rstop/dt)
       ! original code translates as : 'if abs(real(nint(rstop/dt)*dt-rstart) > (0.1_fp*dt)'         ! RL666
       if ( mod(real(rstop,fp),real(dt,fp)) > 0.1_fp*dt) then 
          ! call prterr(lundia, 'U044', errmsg)
          write (msgbuf,*) 'Fourier sample interval stop not at an integer number of user timesteps DtUser.'
          call warn_flush()
          goto 6666
       endif
       !
       !
       if (rstop>tstop) then
          ! call prterr(lundia, 'F006', ' ')
          write (msgbuf,*) 'Fourier sample interval stop exceeds simulation end TStop.'
          call warn_flush()
          goto 6666
       endif
       !
       ! Fouwrt catches the end of all fourier analyses
       !
       fouwrt = max(fouwrt,(ftmsto(ifou)-1))
       !
       fmt = '(i    )'
       nopos = len_trim(columns(4))
       !
       if (nopos<10) then
          write (fmt(3:3), '(i1)') nopos
       else
          write (fmt(3:4), '(i2)') nopos
       endif
       !
       read (columns(4), fmt) fnumcy(ifou)
       !
       if (fnumcy(ifou)==0) then
          foufas(ifou) = 0.
       else
          foufas(ifou) = 2.*pi*real(fnumcy(ifou),fp)/real(ftmsto(ifou) - ftmstr(ifou),fp)
       endif
       !
       ! read nodal amplifications and phase shifts for comparison
       !       with cotidal maps
       !
       fmt = '(f    )'
       nopos = len_trim(columns(5))

       !
       if (nopos<10) then
          write (fmt(3:3), '(i1)') nopos
          write (fmt(4:5), '(a2)') '.0'
       else
          write (fmt(3:4), '(i2)') nopos
          write (fmt(4:5), '(a2)') '.0'
       endif
       !
       read (columns(5), fmt) fknfac(ifou)
       !
       fmt = '(f    )'
       nopos = len_trim(columns(6))
       !
       if (nopos<10) then
          write (fmt(3:3), '(i1)') nopos
          write (fmt(4:5), '(a2)') '.0'
       else
          write (fmt(3:4), '(i2)') nopos
          write (fmt(4:5), '(a2)') '.0'
       endif
       !
       read (columns(6), fmt) fv0pu(ifou)
       !
       if (fv0pu(ifou)<0.) fv0pu(ifou) = fv0pu(ifou) + 360.
       fv0pu(ifou) = mod(fv0pu(ifou), 360.0_fp)
       !
       irelp = 7
       !
       if ( founam(ifou)(1:2)/='s1'  .and. &
            founam(ifou)(1:2)/='ta' .and. &
            founam(ifou)(1:3)/='uxa' .and. &
            founam(ifou)(1:3)/='uya' .and. &
            founam(ifou)(1:2)/='ws') then
          fmt = '(i    )'
          nopos = len_trim(columns(7))
          !
          if (nopos<10) then
             write (fmt(3:3), '(i1)') nopos
          else
             write (fmt(3:4), '(i2)') nopos
          endif
          !
          read (columns(7), fmt) flayno(ifou)
          if (flayno(ifou)>kmax) then
             ! call prterr(lundia, 'F007', ' ')
             write (msgbuf, '(a,i0,a,i0,a)') 'Requested layer exceeds,',flayno(ifou),' exceeds max layer, ',kmax,'.'
             call warn_flush()
             goto 6666
          endif
          irelp = irelp + 1
       endif
       !
       ! Elliptic parameters requested / MAX - MIN added
       !
       if (nveld>=irelp) then
          cdummy = trim(columns(irelp))
          !
          ! check for MAX and or MIN before Y/N
          !
          if (cdummy=='max' .or. fouelp(ifou)=='e' .or. cdummy=='min' .or. cdummy=='avg') then
             if (fnumcy(ifou)>0) then
                fnumcy(ifou) = 0
                foufas(ifou) = 0.
                ! call prterr(lundia, 'F008', 'min')
             endif
             if (fouelp(ifou)=='e') then
                select case (cdummy)
                   case ('min')
                      write (msgbuf, '(3a,i0,a)') 'in file ', trim(filfou), ' line ', linenumber, &
                            & ': energy head in combination with "min" is not supported'
                      ! call prterr(lundia, 'P004', trim(message))
                      call warn_flush()
                      goto 6666
                end select
             else
                select case (cdummy)
                   case ('max')
                      fouelp(ifou) = 'x'
                   case ('min')
                      fouelp(ifou) = 'i'
                   case ('avg')
                      fouelp(ifou) = 'a'
                end select
             endif
          elseif (foutyp(ifou)=='v') then
             if (cdummy(1:1)=='n') then
                fouelp(ifou) = 'n'
             elseif (cdummy(1:1)=='y') then
                fouelp(ifou) = 'y'
             elseif (cdummy /= 'mean') then
                write (msgbuf, '(3a,i0,2a)') 'in file ', trim(filfou), ' line ', linenumber, &
                      & ': expecting min, max, mean, yes or no, instead of ', trim(cdummy)
                call warn_flush()
                goto 6666
             endif
          else
             if (cdummy /= 'mean') then
                write (msgbuf, '(3a,i0,2a)') 'in file ', trim(filfou), ' line ', linenumber, &
                      & ': expecting avg, min, max or mean, instead of ', trim(cdummy)
                call warn_flush()
                goto 6666
             endif
          endif
       endif
       !
       if ((fouelp(ifou)=='x'.or.fouelp(ifou)=='e') .and. founam(ifou)=='s1') then
          !
          ! Currently, inidryonly is only implemented for fouelp=x, founam=s1 or eh
          !
          if (index(record,'inidryonly') > 0) then
             foumask(ifou) = 1
             write (message,'(3a,i0,a)') 'in file ', trim(filfou), ' line ', linenumber, &
                   & ': Fourier analysis only for initially dry points'
             ! call prterr(lundia, 'G051', trim(message))
          endif
       endif
       if (foutyp(ifou)=='v') then                 ! for the second vector component
          ifou          = ifou + 1
          foutyp(ifou)  = 'v'
          fouref(ifou,1)  = fouref(ifou - 1,1)
          ftmstr(ifou)  = ftmstr(ifou - 1)
          ftmsto(ifou)  = ftmsto(ifou - 1)
          fnumcy(ifou)  = fnumcy(ifou - 1)
          flayno(ifou)  = flayno(ifou - 1)
          fconno(ifou)  = fconno(ifou - 1)
          foufas(ifou)  = foufas(ifou - 1)
          fknfac(ifou)  = fknfac(ifou - 1)
          fv0pu (ifou)  = fv0pu(ifou - 1)
          fouelp(ifou)  = fouelp(ifou - 1)
          foumask(ifou) = foumask(ifou - 1)
       endif
       !
       ifou = ifou + 1
       !
       if (ifou<=nofou) goto 20
       ! <--
       !
       success = .True. 
       if (iostat>0) then 
          ! ToDo: some exception handling....apparently something wring with the file
       endif
       !
       ! Define all variable names to be written
       ! Add the (start-)index ivar to fouref(..,2)
       !
       ivar = 0
       do ifou = 1, nofou
          fouref(ifou,2)   = ivar + 1
          if (foutyp(ifou) == 's') then
              if (fouelp(ifou)=='x') then
                 ivar = ivar + 1
                 write(fouvarnam(ivar),'(a,i3.3,a)') "fourier", fouref(ifou,1), "_max"
                 fouvarnamlong(ivar) = "maximum value"
                 call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
                 if (founam(ifou) == 's1') then
                    ivar = ivar + 1
                    write(fouvarnam(ivar),'(a,i3.3,a)') "fourier", fouref(ifou,1), "_max_depth"
                    fouvarnamlong(ivar) = "maximum depth value"
                    call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
                 endif
                 if (foumask(ifou) == 1) then
                    write(fouvarnam    (ivar  ),'(2a)') trim(fouvarnam    (ivar  )), "_inidryonly"
                    write(fouvarnamlong(ivar  ),'(2a)') trim(fouvarnamlong(ivar  )), ", initially dry points only"
                    !if (founam(ifou) == 's1') then
                    !   write(fouvarnam    (ivar-1),'(2a)') trim(fouvarnam    (ivar-1)), "_inidryonly"
                    !   write(fouvarnamlong(ivar-1),'(2a)') trim(fouvarnamlong(ivar-1)), ", initially dry points only"
                    !endif
                 endif
              elseif (fouelp(ifou)=='e') then
                 ivar = ivar + 1
                 write(fouvarnam(ivar),'(a,i3.3,a)') "fourier", fouref(ifou,1), "_max"
                 fouvarnamlong(ivar) = "maximum value"
                 fouvarunit(ivar) = 'm'
                 if (foumask(ifou) == 1) then
                    write(fouvarnam    (ivar  ),'(2a)') trim(fouvarnam    (ivar  )), "_inidryonly"
                    write(fouvarnamlong(ivar  ),'(2a)') trim(fouvarnamlong(ivar  )), ", initially dry points only"
                 endif
              elseif (fouelp(ifou)=='i') then
                 ivar = ivar + 1
                 write(fouvarnam(ivar),'(a,i3.3,a)') "fourier", fouref(ifou,1), "_min"
                 fouvarnamlong(ivar) = "minimum value"
                 call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
                 if (founam(ifou) == 's1') then
                    ivar = ivar + 1
                    write(fouvarnam(ivar),'(a,i3.3,a)') "fourier", fouref(ifou,1), "_min_depth"
                    fouvarnamlong(ivar) = "minimum depth value"
                    call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
                 endif
              elseif (fouelp(ifou)=='a') then
                 ivar = ivar + 1
                 write(fouvarnam(ivar),'(a,i3.3,a)') "fourier", fouref(ifou,1), "_avg"
                 fouvarnamlong(ivar) = "average value"
                 call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
              else
                 if (fnumcy(ifou)==0) then          ! zero fourier mode without further notice means 'MEAN'
                    ivar = ivar + 1
                    write(fouvarnam(ivar),'(a,i3.3,a)') "fourier", fouref(ifou,1), "_mean"
                    fouvarnamlong(ivar) = "average value"
                    call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
                 else                               ! non-zero fourier mode
                    ivar = ivar + 1
                    write(fouvarnam(ivar),'(a,i3.3,a)') "fourier", fouref(ifou,1), "_amp"
                    fouvarnamlong(ivar) = "Fourier amplitude"
                    call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
                    ivar = ivar + 1
                    write(fouvarnam(ivar),'(a,i3.3,a)') "fourier", fouref(ifou,1), "_phs"
                    fouvarnamlong(ivar) = "Fourier phase"
                    fouvarunit(ivar)    = "degree"
                 endif
              endif
          else
              !
              ! foutyp=v
              !
              if (fouelp(ifou)=='x') then
                 ivar = ivar + 1
                 write(fouvarnam(ivar),'(a,i3.3,2a)') "fourier", fouref(ifou,1), "_max_", trim(founam(ifou))
                 write(fouvarnamlong(ivar),'(2a)') "maximum value component ", trim(founam(ifou))
                 call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
                 if (index(founam(ifou),'v') > 0) then
                    ivar = ivar + 1
                    write(fouvarnam(ivar),'(a,i3.3,a)') "fourier", fouref(ifou,1), "_max_mag"
                    fouvarnamlong(ivar) = "maximum value magnitude"
                    call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
                 endif
              elseif (fouelp(ifou)=='i') then
                 ivar = ivar + 1
                 write(fouvarnam(ivar),'(a,i3.3,2a)') "fourier", fouref(ifou,1), "_min_", trim(founam(ifou))
                 write(fouvarnamlong(ivar),'(2a)') "minimum value component ", trim(founam(ifou))
                 call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
                 if (index(founam(ifou),'v') > 0) then
                    ivar = ivar + 1
                    write(fouvarnam(ivar),'(a,i3.3,a)') "fourier", fouref(ifou,1), "_min_mag"
                    fouvarnamlong(ivar) = "minimum value magnitude"
                    call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
                 endif
              elseif (fouelp(ifou)=='a') then
                 ivar = ivar + 1
                 write(fouvarnam(ivar),'(a,i3.3,2a)') "fourier", fouref(ifou,1), "_avg_", trim(founam(ifou))
                 write(fouvarnamlong(ivar),'(2a)') "average value component ", trim(founam(ifou))
                 call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
                 if (index(founam(ifou),'v') > 0) then
                    ivar = ivar + 1
                    write(fouvarnam(ivar),'(a,i3.3,a)') "fourier", fouref(ifou,1), "_avg_mag"
                    fouvarnamlong(ivar) = "average value magnitude"
                    call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
                 endif
              else
                 ivar = ivar + 1
                 write(fouvarnam(ivar),'(a,i3.3,2a)') "fourier", fouref(ifou,1), "_amp_", trim(founam(ifou))
                 write(fouvarnamlong(ivar),'(2a)') "Fourier amplitude component ", trim(founam(ifou))                ! RL666 Wat is fouref 
                 call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
                 ivar = ivar + 1
                 write(fouvarnam(ivar),'(a,i3.3,2a)') "fourier", fouref(ifou,1), "_phs_", trim(founam(ifou))
                 write(fouvarnamlong(ivar),'(2a)') "Fourier phase component ", trim(founam(ifou))
                 fouvarunit(ivar)    = "degree"
              endif
              if (index(founam(ifou),'v')>0 .and. fouelp(ifou)=='y') then
                 ivar = ivar + 1
                 write(fouvarnam(ivar),'(a,i3.3,a)') "fourier", fouref(ifou,1), "_ellip_amp"
                 fouvarnamlong(ivar) = "elliptic amplitude"
                 call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
                 ivar = ivar + 1
                 write(fouvarnam(ivar),'(a,i3.3,a)') "fourier", fouref(ifou,1), "_ellip_ecc"
                 fouvarnamlong(ivar) = "elliptic eccentricity"
                 fouvarunit(ivar)    = ""
                 ivar = ivar + 1
                 write(fouvarnam(ivar),'(a,i3.3,a)') "fourier", fouref(ifou,1), "_ellip_phs"
                 fouvarnamlong(ivar) = "elliptic phase"
                 fouvarunit(ivar)    = "degree"
                 ivar = ivar + 1
                 write(fouvarnam(ivar),'(a,i3.3,a)') "fourier", fouref(ifou,1), "_ellip_inc"
                 fouvarnamlong(ivar) = "elliptic inclination"
                 fouvarunit(ivar)    = ""
              endif
          endif
       enddo

       ! init fourier arrays
       do ifou = 1,nofou
          if (fouelp(ifou)=='e') then
             fousma(1:10,1:10, ifou) = 0.d0
             fousma(:, :, ifou) = -1.0e+30_fp
             fousmb(:, :, ifou) = -1.0e+30_fp
          elseif (fouelp(ifou)=='x') then
             fousma(:, :, ifou) = -1.0e+30_fp
             fousmb(:, :, ifou) = -1.0e+30_fp
          elseif (fouelp(ifou)=='i') then
             fousma(:, :, ifou) =  1.0e+30_fp
             fousmb(:, :, ifou) =  1.0e+30_fp
          else
             fousma(:, :, ifou) =  0.0_fp
             fousmb(:, :, ifou) =  0.0_fp
          endif
       enddo
       return
6666   continue   ! abort fourier analysis, something went wrong
       write (msgbuf, '(a)') 'Invalid input for Fourier analysis, record='''//trim(record)//''''
       call warn_flush()
       write (msgbuf, '(a)') 'Switching off fourier analysis......'
       call warn_flush()
       nofou = 0
   end subroutine reafou


subroutine setfouunit(founam, lsal, ltem, fconno, fouvarunit)
    !
    ! parameters
    character(*), intent(in)  :: founam
    integer     , intent(in)  :: lsal
    integer     , intent(in)  :: ltem
    integer     , intent(in)  :: fconno
    character(*), intent(out) :: fouvarunit
    !
    ! body
    select case (founam(:2))
    case ('s1')
       fouvarunit = 'm'
    case ('ws')
       fouvarunit = 'm/s'
    case ('u1', 'v1', 'ux', 'uy','uc')
       fouvarunit = 'm/s'
    case ('ta')
       fouvarunit = 'N m-2'
    case ('r1')
       if (fconno == ltem) then
          fouvarunit = 'degrees_Celsius'
       elseif (fconno == lsal) then
          fouvarunit = 'ppt'
       else
          fouvarunit = 'kg/m3'
       endif
    case default
       fouvarunit = ''
    end select 
end subroutine setfouunit

!> performs fourier analysis i.e. computes suma and sumb
!! - calculates MAX or MIN value
   subroutine fouana( ifou, nst, rarray, bl, gddimens, umean, vmean)
   !!--declarations----------------------------------------------------------------
       use precision
       use m_d3ddimens
       !
       implicit none
       !
   !
   ! Global variables
   !
       integer                                                 , intent(in)  :: ifou   !!  Counter
       integer                                                , intent(in)  :: nst    !!  Time step number
       type(gd_dimens)  , pointer :: gddimens
       real(fp)  , dimension(gddimens%nlb:gddimens%nub, gddimens%mlb:gddimens%mub)        , intent(in)  :: rarray !  Array for fourier analysis
       real(fp)  , dimension(gddimens%nlb:gddimens%nub, gddimens%mlb:gddimens%mub)        , intent(in), optional  :: umean  !  Description and declaration in esm_alloc_real.f90
       real(fp)  , dimension(gddimens%nlb:gddimens%nub, gddimens%mlb:gddimens%mub)        , intent(in), optional  :: vmean  !  Description and declaration in esm_alloc_real.f90
       real(prec), dimension(gddimens%nlb:gddimens%nub, gddimens%mlb:gddimens%mub)        , intent(in)  :: bl
       !
       ! The following list of pointer parameters is used to point inside the gdp structure
       !
       integer        , dimension(:)        , pointer :: ftmsto
       integer        , dimension(:)        , pointer :: ftmstr
       integer        , dimension(:)        , pointer :: foumask
       real(fp)       , dimension(:)        , pointer :: foufas
       real(fp)       , dimension(:,:,:)    , pointer :: fousma
       real(fp)       , dimension(:,:,:)    , pointer :: fousmb
       character(1)   , dimension(:)        , pointer :: fouelp
       character(16)  , dimension(:)        , pointer :: founam
       integer                              , pointer :: mmax
       integer                              , pointer :: nmaxus
       integer                              , pointer :: ndx
       integer                              , pointer :: lnx
       integer                              , pointer :: ndkx
       integer                              , pointer :: lnkx
   !
   ! Local variables
   !
       integer         :: m       ! Loop counter over MMAX
       integer         :: n       ! Loop counter over NMAXUS 
       real(fp)        :: angl
       real(fp)        :: uuu     ! umean in zeta point
       real(fp)        :: vvv     ! vmean in zeta point
       real(fp)        :: utot2   ! |U|**2 = uuu**2 + vvv**2
   !
   !! executable statements -------------------------------------------------------
   !
       ftmsto    => gdfourier%ftmsto
       ftmstr    => gdfourier%ftmstr
       foumask   => gdfourier%foumask
       foufas    => gdfourier%foufas
       fousma    => gdfourier%fousma
       fousmb    => gdfourier%fousmb
       fouelp    => gdfourier%fouelp
       founam    => gdfourier%founam
       mmax      => gddimens%mmax
       nmaxus    => gddimens%nmaxus
       ndx       => gddimens%ndx
       lnx       => gddimens%lnx
       ndkx      => gddimens%ndkx
       lnkx      => gddimens%lnkx

       ! Perform fourier analysis, every timestep as long as NST value
       ! lies in requested time interval FTMSTR and FTMSTO
       !
       ! The name of the variable for Fourier analysis fully specified the number of elements for the fourier loop
       select case (founam(ifou))
       case('s1')
            nmaxus = ndx
       case('ws')
            nmaxus = lnx
       case('u1', 'v1')
            nmaxus = lnkx
       case('qxk')
            nmaxus = lnx
       case('ta')
            nmaxus = ndx
       case('ux','uy','uc')
            nmaxus = ndkx
       case('uxa','uya')
            nmaxus = ndx
       case('r1')
            nmaxus = ndkx
       end select
       
       if (nst>=ftmstr(ifou) .and. nst<ftmsto(ifou)) then
          if (fouelp(ifou) == 'x') then
             !
             ! Calculate MAX value
             !
             if (founam(ifou) == 's1') then
                
                do n = 1, nmaxus
                   do m = 1, mmax
                      !
                      ! Waterlevel (fousma) and waterdepth (fousmb),
                      !
                      fousma(n,m,ifou) = max(fousma(n,m,ifou), rarray(n,m))
                      fousmb(n,m,ifou) = max(fousmb(n,m,ifou), rarray(n,m) - real(bl(n,m),fp))               ! NOTE: bl is a HEIGHT (as bl in fm) and NOT a DEPTH (delft3d)
                   enddo
                enddo
             else
                do n = 1, nmaxus
                   do m = 1, mmax
                        fousma(n, m, ifou) = max(fousma(n,m,ifou), rarray(n,m))
                   enddo
                enddo
             endif
          elseif (fouelp(ifou) == 'e') then
             !
             ! Calculate MAX Energy head value
             !
             if (present(umean) .and. present(vmean)) then 
                do n = 1, nmaxus
                   do m = 1, mmax
                      !
                      ! Energy head, based on cell-centre velocities
                      !
                      uuu   = umean(n,m)                 ! cell-centre u
                      vvv   = vmean(n,m)                 ! cell-centre v
                      utot2 = uuu*uuu + vvv*vvv
                      fousma(n,m,ifou) = max(fousma(n,m,ifou), 0.5_hp*utot2/ag_fouana + rarray(n,m))
                   enddo
                enddo
             endif
          elseif (fouelp(ifou) == 'i') then
             !
             ! Calculate MIN value
             !
             do n = 1, nmaxus
                do m = 1, mmax
                     fousma(n,m,ifou) = min(fousma(n,m,ifou), rarray(n,m))
                enddo
             enddo
             if (founam(ifou) == 's1') then
                do n = 1, nmaxus
                   do m = 1, mmax
                        fousmb(n,m,ifou) = min(fousmb(n,m,ifou), rarray(n,m) - real(bl(n,m),fp))
                   enddo
                enddo
             endif
          elseif (fouelp(ifou) == 'a') then
             !
             ! Calculate AVG value
             !
             do n = 1, nmaxus
                do m = 1, mmax
                   fousma(n,m,ifou) = fousma(n,m,ifou) + rarray(n,m)
                   fousmb(n,m,ifou) = fousmb(n,m,ifou) + 1.
                enddo
             enddo
          !
          ! Calculate total for fourier analyse
          !
          else
             angl = real(nst - ftmstr(ifou),fp)*foufas(ifou)
             do n = 1, nmaxus
                do m = 1, mmax
                   fousma(n,m,ifou) = fousma(n,m,ifou) + rarray(n,m)*cos(angl)
                   fousmb(n,m,ifou) = fousmb(n,m,ifou) + rarray(n,m)*sin(angl)
                enddo
             enddo
          endif
       endif
   end subroutine fouana

!> - Checks if fourier analysis are requested
!!  and determines the number of variables for which a fourier analysis is requested
   subroutine fouini(lunfou, success, ag, time_unit_user, time_unit_kernel)
   !!--declarations----------------------------------------------------------------
       use precision
       use unstruc_messages
       !
       implicit none
   !
   ! Global variables
   !
       integer , intent(in)            :: lunfou   !!  Unit number for fourier input file
       real(fp), intent(in)            :: ag       !!  override gravitational constant 
       character(len=*), intent(in)    :: time_unit_user, time_unit_kernel
       logical                         :: success  !!  Flag=True if no error is encountered

   !
   ! Local variables
   !
       integer, parameter                 :: maxvld = 40
       integer                            :: nveld  ! Used for format free reading 
       character(300)                     :: record ! Used for format free reading 300 = 256 + a bit (field, =, ##, etc.) 
       character(30)  ,dimension(maxvld)  :: columns! each record is split into separate fields (columns)
       integer                            :: iostat
       integer                            :: iveld
       integer, pointer                   :: nofouvar
   !
   !! executable statements -------------------------------------------------------
   !
       time_unit_factor=1.0
       select case (time_unit_user)
       case('D')
          time_unit_factor=3600.*24. 
       case('H')
          time_unit_factor=3600. 
       case('M')
          time_unit_factor=60. 
       case('S')
          time_unit_factor=1. 
       end select

       select case (time_unit_kernel)
       case('D')
          time_unit_factor=time_unit_factor/(3600.*24.)
       case('H')
          time_unit_factor=time_unit_factor/(3600.)
       case('M')
          time_unit_factor=time_unit_factor/(60.)
       end select

       ! The user specified times in the .fou files need to be multiplied by the time_unit_factor, to correspond with the kernel times

       ag_fouana = ag 
       nofouvar => gdfourier%nofouvar
       ! initialisation
       !
       nofou    = 0
       nofouvar = 0
       !
       ! cycle through file, while reading records
       !
       ! -->
       !
      10 continue
       read (lunfou, '(a)', end = 999) record
       !
       ! reset record in smaller case characters and define contents
       !
       call str_lower(record, 300)
       columns = ''
       read(record,*,iostat=iostat) (columns(iveld),iveld=1,maxvld)          ! TODO: cover for read errors 
       !
       do nveld = maxvld,1,-1
          if (len_trim(columns(nveld))>0) exit
       enddo 
       if (columns(1)(1:1)=='*' .or. nveld==0) goto 10
       !
       ! test for continuation record
       !
       ! requested fourier analysis water-level
       !
       if (columns(1)(1:2)=='wl') then
          nofou = nofou + 1
          if (index(record,'avg')>0) then
             nofouvar = nofouvar + 1
          else 
             !
             ! max and min: also write max depth
             !
             nofouvar = nofouvar + 2
          endif
       !
       ! requested fourier analysis cell-centre vertically average velocities
       !
       elseif (columns(1)(1:3)=='uva') then
          nofou = nofou + 2
          if (index(record,'max')>0 .or. index(record,'min')>0 .or. index(record,'avg')>0) then
             nofouvar = nofouvar + 2
          else
             nofouvar = nofouvar + 4
          endif
       !
       ! requested fourier analysis wind-speed
       !
       elseif (columns(1)(1:2)=='ws') then
          nofou = nofou + 1
          if (index(record,'max')>0 .or. index(record,'min')>0 .or. index(record,'avg')>0) then
             nofouvar = nofouvar + 1
          else
             nofouvar = nofouvar + 2
          endif
       !
       ! requested fourier analysis temperature
       !
       elseif (columns(1)(1:2)=='ct') then
          nofou = nofou + 1
          if (index(record,'max')>0 .or. index(record,'min')>0 .or. index(record,'avg')>0) then
             nofouvar = nofouvar + 1
          else
             nofouvar = nofouvar + 2
          endif
       !
       ! requested fourier analysis cell-centred eastward and northward velocity
       !
       elseif (columns(1)(1:2)=='ux' .or. columns(1)(1:2)=='uy') then
          nofou = nofou + 1
          if (index(record,'max')>0 .or. index(record,'min')>0 .or. index(record,'avg')>0) then
             nofouvar = nofouvar + 1
          else
             nofouvar = nofouvar + 2
          endif
       !
       ! requested fourier analysis cell centred velocity magnitude   ucmag
       !
       elseif (columns(1)(1:2)=='uc') then
          nofou = nofou + 1
          if (index(record,'max')>0 .or. index(record,'min')>0 .or. index(record,'avg')>0) then
             nofouvar = nofouvar + 1
          else
             nofouvar = nofouvar + 2
          endif
       !
       ! requested fourier analysis salinity
       !
       elseif (columns(1)(1:2)=='cs') then
          nofou = nofou + 1
          if (index(record,'max')>0 .or. index(record,'min')>0 .or. index(record,'avg')>0) then
             nofouvar = nofouvar + 1
          else
             nofouvar = nofouvar + 2
          endif
       !
       ! requested fourier analysis bed shear stress
       !
       elseif (columns(1)(1:2)=='bs') then
          nofou = nofou + 1
          if (index(record,'max')>0 .or. index(record,'min')>0 .or. index(record,'avg')>0) then
             nofouvar = nofouvar + 1
          else
             nofouvar = nofouvar + 2
          endif
       !
       ! requested fourier analysis constituent
       !
       elseif ((columns(1)(1:1)=='c') .and. (index('12345',columns(1)(2:2))>0)) then 
          nofou = nofou + 1
          if (index(record,'max')>0 .or. index(record,'min')>0 .or. index(record,'avg')>0) then
             nofouvar = nofouvar + 1
          else
             nofouvar = nofouvar + 2
          endif
       else
          !
          ! requested fourier analysis undefined
          !
          write(msgbuf,'(a)') 'Fourier analysis: variable keyword '''//trim(columns(1))//''' not recognized, ignored' 
          call msg_flush()
          continue
          ! TODO: Issue a warning that we ignored a line in the fou file
       endif
       !
       goto 10
       !
       ! <-- next record
       !
       !
   999 continue
       fileids%ncid = 0
       success = .True. 
     rewind(lunfou)
   end subroutine fouini

!> do the actual fourier and min/max update
!! write to file after last update
    subroutine postpr_fourier(nst, trifil, dtsec, refdat, hdt, tzone)
    use m_d3ddimens
    use m_transport
    use m_flowgeom
    use m_wind
    use m_flow
    implicit none

       real(fp)          , intent(in) :: dtsec   !<  Integration time step [in seconds]
       real(fp)          , intent(in) :: hdt     !< Half Integration time step [seconds] => gdp%gdnumeco%hdt
       real(fp)          , intent(in) :: tzone   !< Local (FLOW) time - GMT (in hours)  => gdp%gdexttim%tzone
       integer           , intent(in) :: nst     !< timestep number 
       character(len=*)  , intent(in) :: trifil  !< output filename 
       character(len=*)  , intent(in) :: refdat  !< reference date

    ! NOTE: In DELFT3D depth is used, but bl is passed (positive bottomlevel). Defined direction is different  
       
    !
    ! Perform analysis and write to Fourier file
    !
    integer                                        :: ifou 
    integer          , dimension(:)      , pointer :: fconno
    integer          , dimension(:)      , pointer :: flayno
    integer          , dimension(:)      , pointer :: fnumcy
    integer                              , pointer :: fouwrt
    integer          , dimension(:)      , pointer :: ftmsto
    integer          , dimension(:)      , pointer :: ftmstr
    real(fp)         , dimension(:)      , pointer :: fknfac
    real(fp)         , dimension(:,:,:)  , pointer :: foucomp
    real(fp)         , dimension(:)      , pointer :: foufas
    real(fp)         , dimension(:,:,:)  , pointer :: fousma
    real(fp)         , dimension(:,:,:)  , pointer :: fousmb
    real(fp)         , dimension(:,:,:)  , pointer :: fouvec
    real(fp)         , dimension(:)      , pointer :: fv0pu
    character(1)     , dimension(:)      , pointer :: fouelp
    character(16)    , dimension(:)      , pointer :: founam
    character(1)     , dimension(:)      , pointer :: foutyp
    integer                              , pointer :: nmax
    integer                              , pointer :: mmax
    integer                              , pointer :: nlb
    integer                              , pointer :: nub
    integer                              , pointer :: mlb
    integer                              , pointer :: mub
    integer                              , pointer :: nmaxus
    integer                              , pointer :: kmax

    double precision, pointer        :: fieldptr1(:,:)
    double precision, pointer        :: bl_ptr(:,:)
    integer         , pointer        :: kfs_ptr(:,:), kfst0_ptr(:,:)

    integer    :: itdate   !<  Reference time in yyyymmdd as an integer 
    double precision, allocatable, target :: constit(:)
    
    read(refdat,*) itdate 
    
    fconno              => gdfourier%fconno
    flayno              => gdfourier%flayno
    fnumcy              => gdfourier%fnumcy
    fouwrt              => gdfourier%fouwrt
    ftmsto              => gdfourier%ftmsto
    ftmstr              => gdfourier%ftmstr
    fknfac              => gdfourier%fknfac
    foucomp             => gdfourier%foucomp
    foufas              => gdfourier%foufas
    fousma              => gdfourier%fousma
    fousmb              => gdfourier%fousmb
    fouvec              => gdfourier%fouvec
    fv0pu               => gdfourier%fv0pu
    fouelp              => gdfourier%fouelp
    founam              => gdfourier%founam
    foutyp              => gdfourier%foutyp
    
    nmax                => gddimens%nmax
    mmax                => gddimens%mmax
    nlb                 => gddimens%nlb
    nub                 => gddimens%nub
    mlb                 => gddimens%mlb
    mub                 => gddimens%mub
    nmaxus              => gddimens%nmaxus
    kmax                => gddimens%kmax
    
    bl_ptr(1:gddimens%ndx,1:1) => bl
    kfs_ptr(1:gddimens%ndx,1:1) => kfs
    kfst0_ptr(1:gddimens%ndx,1:1) => kfst0
    
    if (.not. allocated(constit)) then
      allocate (constit(1:gddimens%ndkx))
    endif

    if (allocated(wmag) .and. allocated(wx) .and. allocated(wy)) then
       wmag = sqrt(wx*wx + wy*wy)
    endif
    
    if (nofou > 0) then
       ifou = 1
       do 
          if (ifou > nofou) exit
          !
          ! Perform fourier analysis, every timestep as long as NST value
          ! lies in requested time interval FTMSTR and FTMSTO
          !
          if (foutyp(ifou) == 's') then                  ! scalar 
             !
             ! Scalar type Fourier component
             !
             ! Get Fourier component pointer
             !
             select case (founam(ifou))
             case ('s1')                        ! waterlevels 
                fieldptr1(1:gddimens%ndx,1:1) => s1
             case ('ws')                        ! absolute wind magnitude
                fieldptr1(1:gddimens%ndx,1:1) => wmag
             case ('ux')
                fieldptr1(1:gddimens%ndkx,1:1) => ucx
             case ('uy')
                fieldptr1(1:gddimens%ndkx,1:1) => ucy
             case ('uxa')
                fieldptr1(1:gddimens%ndkx,1:1) => ucxq
             case ('uya')
                fieldptr1(1:gddimens%ndkx,1:1) => ucyq
             case ('uc')                        ! ucmag, velocity magnitude
                fieldptr1(1:gddimens%ndkx,1:1) => ucmag
             case ('r1')
                constit(1:gddimens%ndkx) = constituents(fconno(ifou),:)
                fieldptr1(1:gddimens%ndkx,1:1) => constit
             case ('ta')
                call gettaus(1)
                fieldptr1(1:gddimens%ndx,1:1) => taus
             case default 
                continue         ! Unknown FourierVariable exception 
             end select
             call fouana(ifou, nst, fieldptr1, bl_ptr, gddimens_ptr)
             ifou = ifou + 1
          else 
             !
             ! Incorrect Fourier type found, issue a warning 
             !
          endif
       enddo
       !
       ! Write results of fourier analysis to data file
       ! only once when all fourier periods are complete
       !
       if (nst==fouwrt) then
          if (fileids%ncid == 0) then
             call wrfou(trifil, dtsec, const_names, itdate, hdt ,tzone, gddimens_ptr)
             nofou = 0  ! this stops calling postpr_fourier; todo: cleanup
          endif
       endif
    endif
    end subroutine postpr_fourier

!> - open fourier analysis output file
!! - writes results of fourier analysis to output file
!! - closes fourier analysis output file
    subroutine wrfou(trifil, dtsec, namcon, itdate, hdt, tzone, gddimens)
    !!--declarations----------------------------------------------------------------
        use precision
        use mathconsts
        use netcdf
        use unstruc_netcdf
        use m_sferic, only: jsferic
        use m_d3ddimens
        !
        implicit none
        ! 
    !
    ! Global variables
    !
        integer                        , intent(in)  :: itdate  !< Reference time in yyyymmdd as an integer
        real(fp)                       , intent(in)  :: dtsec   !< Integration time step [in seconds]
        character(len=*) , dimension(:), intent(in)  :: namcon  !< Description and declaration in esm_alloc_char.f90
        character(*)                   , intent(in)  :: trifil  !< File name for FLOW NEFIS output files (tri"h/m"-"casl""labl".dat/def)
        real(fp)                       , intent(in)  :: tzone   !< Local (FLOW) time - GMT (in hours)  => gdp%gdexttim%tzone
        real(fp)                       , intent(in)  :: hdt     !< Half Integration time step [seconds] => gdp%gdnumeco%hdt         
        type(gd_dimens)  , pointer :: gddimens            !< Model geometry/grid structure 

        !
        integer                        , pointer :: nofouvar
        integer        , dimension(:)  , pointer :: fconno
        character(1)   , dimension(:)  , pointer :: foutyp
        character(1)   , dimension(:)  , pointer :: fouelp
        character(16)  , dimension(:)  , pointer :: founam
        character(50)  , dimension(:)  , pointer :: fouvarnam
        character(50)  , dimension(:)  , pointer :: fouvarnamlong
        character(50)  , dimension(:)  , pointer :: fouvarunit
        integer        , dimension(:,:), pointer :: fouref
        integer        , dimension(:)  , pointer :: ftmsto
        integer        , dimension(:)  , pointer :: ftmstr
        real(fp)       , dimension(:)  , pointer :: foufas
        integer        , dimension(:)  , pointer :: flayno
        integer        , dimension(:)  , pointer :: fnumcy
        integer                        , pointer :: iblwl
        integer                        , pointer :: iblws
        integer                        , pointer :: ibleh
        integer                        , pointer :: iblcn
        integer                        , pointer :: ibluv
        integer                        , pointer :: ibluva
        integer                        , pointer :: ibluc
        integer                        , pointer :: iblqf
        integer                        , pointer :: iblbs
        integer                        , pointer :: iblep
        integer        , dimension(:,:), pointer :: idvar
        integer                        , pointer :: nmaxgl
        integer                        , pointer :: mmaxgl
        integer                        , pointer :: mmax 
        integer                        , pointer :: nmax 
        integer                        , pointer :: ndx
        integer                        , pointer :: lnx
        integer                        , pointer :: ndkx
        integer                        , pointer :: lnkx
        integer                        , pointer :: nmaxus
    !
    ! Local variables
    !
        integer                       :: ierr 
        integer                       :: ifou         ! Local teller for fourier functions 
        integer                       :: ivar         ! Local teller for fourier functions 
        real(fp)                      :: freqnt       ! Frequency in degrees per hour 
        real(fp)                      :: tfasto       ! Stop time in minutes 
        real(fp)                      :: tfastr       ! Start time in minutes 
        character(len=4)              :: blnm
        character(len=:), allocatable :: namfun       ! Local name for fourier function 
        character(len=:), allocatable :: namfunlong   ! Local name for fourier function, including reference to the line in the fourier input file 
        character(len=3)              :: cnumber      ! temp string for int2str conversion
        integer, parameter            :: imissval = -1
        integer                       :: unc_loc
        integer, allocatable          :: all_unc_loc(:)

        integer, parameter            :: REQUESTTYPE_DEFINE =  1
        integer, parameter            :: REQUESTTYPE_WRITE  =  2

    !
    !! executable statements -------------------------------------------------------
    !
        nofouvar      => gdfourier%nofouvar
        fconno        => gdfourier%fconno
        foutyp        => gdfourier%foutyp
        fouelp        => gdfourier%fouelp
        founam        => gdfourier%founam
        fouvarnam     => gdfourier%fouvarnam
        fouvarnamlong => gdfourier%fouvarnamlong
        fouvarunit    => gdfourier%fouvarunit
        fouref        => gdfourier%fouref
        ftmsto        => gdfourier%ftmsto
        ftmstr        => gdfourier%ftmstr
        foufas        => gdfourier%foufas
        flayno        => gdfourier%flayno
        fnumcy        => gdfourier%fnumcy
        iblwl         => gdfourier%iblwl
        iblws         => gdfourier%iblws
        ibleh         => gdfourier%ibleh
        iblcn         => gdfourier%iblcn
        ibluv         => gdfourier%ibluv
        ibluva        => gdfourier%ibluva
        ibluc         => gdfourier%ibluc
        iblqf         => gdfourier%iblqf
        iblbs         => gdfourier%iblbs
        iblep         => gdfourier%iblep
        idvar         => gdfourier%idvar
        mmax          => gddimens%mmax
        nmax          => gddimens%nmax
        lnx           => gddimens%lnx
        ndx           => gddimens%ndx 
        lnkx          => gddimens%lnkx
        ndkx          => gddimens%ndkx 
        nmaxus        => gddimens%nmaxus

        mmaxgl        => mmax                                ! => gdp%gdparall%mmaxgl         ! RL TODO: Hier een  aparte parameter voor nodig ? 
        nmaxgl        => nmax                                ! => gdp%gdparall%nmaxgl

        !
        ierr = unc_create(trim(trifil), 0, fileids%ncid)
        ierr = ug_addglobalatts(fileids%ncid, ug_meta_fm) 
        call unc_write_flowgeom_filepointer_ugrid(fileids%ncid, fileids%id_tsp, 1)

        allocate(all_unc_loc(nofou))
        !
        ifou  = 1
        iblwl = 0
        iblws = 0
        ibleh = 0
        iblcn = 0
        ibluv = 0
        ibluva= 0
        ibluc = 0
        iblqf = 0
        iblbs = 0
        iblep = 0
        do ivar=1, nofouvar
           if (ifou < nofou) then
              if (fouref(ifou+1,2) <= ivar) then
                 ifou = ifou + 1
              endif
           endif
           freqnt = foufas(ifou)*raddeg*3600.0_fp/dtsec
           tfastr = real(ftmstr(ifou),fp)*dtsec/60.0_fp
           tfasto = real(ftmsto(ifou),fp)*dtsec/60.0_fp
           !
           if (founam(ifou)(:2)=='s1') then
              unc_loc = UNC_LOC_S
              if (fouelp(ifou)=='e') then
                 ibleh = ibleh + 1
                 blnm = 'EH??'
                 write (blnm(3:4), '(i2.2)') ibleh
                 namfun = 'energy head'
              else
                 iblwl = iblwl + 1
                 blnm = 'WL??'
                 write (blnm(3:4), '(i2.2)') iblwl
                 namfun = 'water level'
              endif
           endif
           if (founam(ifou)(:2)=='ws') then
              unc_loc = UNC_LOC_U
              iblws = iblws + 1
              blnm = 'WS??'
              write (blnm(3:4), '(i2.2)') iblws
              namfun = 'wind speed'
           endif
           if (founam(ifou)(:2)=='ux') then
              unc_loc = merge(UNC_LOC_S3D, UNC_LOC_S, kmx > 0)
              ibluv = ibluv + 1
              blnm = 'UX??'
              write (blnm(3:4), '(i2.2)') ibluv
              namfun = 'U-component of cell-centre velocity'
           endif
           if (founam(ifou)(:2)=='uy') then
              unc_loc = merge(UNC_LOC_S3D, UNC_LOC_S, kmx > 0)
              ibluv = ibluv + 1
              blnm = 'UY??'
              write (blnm(3:4), '(i2.2)') ibluv
              namfun = 'V-component of cell-centre velocity'
           endif
           if (founam(ifou)(:3)=='uxa') then
              unc_loc = UNC_LOC_S
              ibluva = ibluva + 1
              blnm = 'UX??'
              write (blnm(3:4), '(i2.2)') ibluva
              namfun = 'U-component velocity, column average'
           endif
           if (founam(ifou)(:3)=='uya') then
              unc_loc = UNC_LOC_S
              ibluva = ibluva + 1
              blnm = 'UY??'
              write (blnm(3:4), '(i2.2)') ibluva
              namfun = 'V-component velocity, column average'
           endif
           if (founam(ifou)(:2)=='uc') then
              unc_loc = merge(UNC_LOC_S3D, UNC_LOC_S, kmx > 0)
              ibluc = ibluc + 1
              blnm = 'UC??'
              write (blnm(3:4), '(i2.2)') ibluc
              namfun = 'velocity magnitude'
           endif
           if (founam(ifou)(:2)=='r1') then
              unc_loc = merge(UNC_LOC_S3D, UNC_LOC_S, kmx > 0)
              iblcn = iblcn + 1
              blnm = 'CO??'
              write (blnm(3:4), '(i2.2)') iblcn
              namfun = namcon(fconno(ifou))
           endif
           if (founam(ifou)(:2)=='u1') then
              unc_loc = UNC_LOC_U
              ibluv = ibluv + 1
              blnm = 'UV??'
              write (blnm(3:4), '(i2.2)') ibluv
              namfun = 'velocity'
           endif
           if (founam(ifou)(:2)=='qx') then
              unc_loc = UNC_LOC_U
              iblqf = iblqf + 1
              blnm = 'QF??'
              write (blnm(3:4), '(i2.2)') iblqf
              namfun = 'unit discharge'
           endif
           if (founam(ifou)(:2)=='ta') then
              unc_loc = UNC_LOC_S
              iblbs = iblbs + 1
              blnm = 'BS??'
              write (blnm(3:4), '(i2.2)') iblbs
              namfun = 'bed stress'
           endif
           all_unc_loc(ifou) = unc_loc
           write(cnumber,'(i3.3)') fouref(ifou,1)
           namfunlong = cnumber // ": " // namfun
           !
           idvar(:,ivar) = imissval
           ierr = unc_def_var_map(fileids%ncid,fileids%id_tsp, idvar(:,ivar), NF90_DOUBLE, unc_loc, trim(fouvarnam(ivar)), trim(fouvarnam(ivar)), &
                          'Fourier analysis ' // namfunlong // ', ' // trim(fouvarnamlong(ivar)), fouvarunit(ivar), 0)
           ierr = unc_put_att(fileids%ncid,idvar(:,ivar), 'long_name','Fourier analysis '// namfunlong // ', ' // trim(fouvarnamlong(ivar)))
           ierr = unc_put_att(fileids%ncid,idvar(:,ivar), 'units',fouvarunit(ivar))
           ierr = unc_put_att(fileids%ncid,idvar(:,ivar), 'Reference_date_in_yyyymmdd', itdate)
           ierr = unc_put_att(fileids%ncid,idvar(:,ivar), 'Starttime_fourier_analysis_in_minutes_since_reference_date', tfastr)
           ierr = unc_put_att(fileids%ncid,idvar(:,ivar), 'Stoptime_fourier_analysis_in_minutes_since_reference_date', tfasto)
           
           ierr = unc_add_gridmapping_att(fileids%ncid, idvar(:,ivar), jsferic)
           select case (founam(ifou))
           case('s1','r1','u1','ux','uy','uxa','uya','uc','ta')
              ierr = unc_put_att(fileids%ncid, idvar(:,ivar),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           case('qxk','ws')
              ierr = unc_put_att(fileids%ncid, idvar(:,ivar),  'coordinates'  , 'FlowLink_xu FlowLink_yu')
           end select
           ierr = unc_put_att(fileids%ncid,idvar(:,ivar), 'Number_of_cycles', fnumcy(ifou))
           ierr = unc_put_att(fileids%ncid,idvar(:,ivar), 'Frequency_degrees_per_hour', freqnt)
           !
        enddo
        !
        ! Reset the global indices
        !
        iblwl = 0
        iblws = 0
        ibleh = 0
        iblcn = 0
        ibluv = 0
        ibluva= 0
        ibluc = 0
        iblqf = 0
        iblbs = 0
        iblep = 0

        ! END DEFINITION MODE 
        ierr =  nf90_enddef(fileids%ncid)

        ! START DATA MODE  
        !
        ifou = 1
        !
        ! Write requested fourier function output for scalar quantity
        !
        do 
           if (ifou > nofou) exit
           !
           unc_loc = all_unc_loc(ifou)
           !
           if (foutyp(ifou)=='s') then
              call wrfous(ifou, dtsec, hdt, tzone, gddimens, fileids, unc_loc)
              ifou = ifou + 1
           else
!             call wrfous(ifou   ,dtsec   ,namcon  ,hdt  ,tzone  ,gdfourier  ,gddimens   )
!             call wrfous(ifou+1 ,dtsec   ,namcon  ,hdt  ,tzone  ,gdfourier  ,gddimens   )
              ! call wrfouv(ifou   ,dtsec   ,hdt  ,tzone  ,gdfourier  ,gddimens   ,fileids   ,UNC_LOC_S)
              
              ! Vectors are not supported at the moment.
              ! The intention is to write vector components as separate scalar variables, like it is done in the map-file for ucx, ucy
              ! Todo: implement writing vectors by adding a variable
              ! Todo: Add the additional vector stuff, such as elliptic results etc. 
              !
              ifou = ifou + 2
           endif
        enddo
        !
        ! Close fourier output file
        !
        ierr = nf90_close(fileids%ncid)         ! ncid NOT set to zero, to avoid writing the fourier output file again.
    end subroutine wrfou

!> - writes results of fourier analysis to output
!! file lunfou for scalair quantities
   subroutine wrfous(ifou, dtsec, hdt, tzone, gddimens, fileids, iloc)
   !!--declarations----------------------------------------------------------------
       use precision
       use mathconsts
       use m_d3ddimens
       use netcdf
       use unstruc_netcdf
       !
       implicit none
   !
   ! Global variables
   !
       integer                     , intent(in) :: ifou   !< Fourier counter
       real(fp)                    , intent(in) :: dtsec  !< Integration time step [in seconds]
       real(fp)                    , intent(in) :: tzone  !< Local (FLOW) time - GMT (in hours)  => gdp%gdexttim%tzone
       real(fp)                    , intent(in) :: hdt    !< Half Integration time step [seconds] => gdp%gdnumeco%hdt
       type(t_unc_mapids)          , intent(in) :: fileids!< Set of file and variable ids for this file.
       integer                     , intent(in) :: iloc
       type(gd_dimens)             , pointer    :: gddimens
   !
   ! Local variables
   !
       integer                              , pointer :: nmaxgl
       integer                              , pointer :: mmaxgl
       integer        , dimension(:)        , pointer :: fconno
       integer        , dimension(:)        , pointer :: flayno
       integer        , dimension(:)        , pointer :: fnumcy
       integer        , dimension(:)        , pointer :: ftmsto
       integer        , dimension(:)        , pointer :: ftmstr
       integer        , dimension(:,:)      , pointer :: idvar
       integer                              , pointer :: iblwl
       integer                              , pointer :: iblws
       integer                              , pointer :: ibluv
       integer                              , pointer :: ibluc
       integer                              , pointer :: ibluva
       integer                              , pointer :: ibleh
       integer                              , pointer :: iblcn
       integer                              , pointer :: iblbs
       real(fp)       , dimension(:)        , pointer :: fknfac
       real(fp)       , dimension(:)        , pointer :: foufas
       real(fp)       , dimension(:,:,:)    , pointer :: fousma
       real(fp)       , dimension(:,:,:)    , pointer :: fousmb
       real(fp)       , dimension(:)        , pointer :: fv0pu
       character(1)   , dimension(:)        , pointer :: fouelp
       character(16)  , dimension(:)        , pointer :: founam
       character(50)  , dimension(:)        , pointer :: fouvarnam
       integer                              , pointer :: nofouvar
       integer        , dimension(:,:)      , pointer :: fouref
       integer                              , pointer :: mmax
       integer                              , pointer :: nmax
       integer                              , pointer :: nmaxus
       integer                              , pointer :: ndx
       integer                              , pointer :: lnx
       integer                              , pointer :: ndkx
       integer                              , pointer :: lnkx

       integer                   :: ierror
       integer                   :: fouvar
       integer                   :: m                  ! Loop counter over MMAX 
       integer                   :: n                  ! Loop counter over NMAXUS 
       logical                   :: ltest              ! Help variable for atan2 function test 
       real(fp)                  :: amp                ! Fourier amplitude 
       real(fp)                  :: fas                ! Fourier phase 
       real(fp)                  :: freqnt             ! Frequency in degrees per hour 
       real(fp)                  :: shift              ! Phase shift 
       real(fp)                  :: tfasto             ! Stop time in minutes 
       real(fp)                  :: tfastr             ! Start time in minutes 
       real(sp), parameter       :: defaul = -999.0_sp ! Default value 
       character(4)              :: blnm

   real(sp), dimension(:,:,:),   allocatable, save :: glbarr3
   !
   !! executable statements -------------------------------------------------------
   !
       fconno        => gdfourier%fconno
       flayno        => gdfourier%flayno
       fnumcy        => gdfourier%fnumcy
       iblwl         => gdfourier%iblwl
       iblws         => gdfourier%iblws
       ibluv         => gdfourier%ibluv
       ibluva        => gdfourier%ibluva
       ibluc         => gdfourier%ibluc
       ibleh         => gdfourier%ibleh
       iblcn         => gdfourier%iblcn
       iblbs         => gdfourier%iblbs
       ftmsto        => gdfourier%ftmsto
       ftmstr        => gdfourier%ftmstr
       idvar         => gdfourier%idvar
       fknfac        => gdfourier%fknfac
       foufas        => gdfourier%foufas
       fousma        => gdfourier%fousma
       fousmb        => gdfourier%fousmb
       fv0pu         => gdfourier%fv0pu
       fouelp        => gdfourier%fouelp
       founam        => gdfourier%founam
       fouvarnam     => gdfourier%fouvarnam
       nofouvar      => gdfourier%nofouvar
       fouref        => gdfourier%fouref

       mmax          => gddimens%mmax
       nmax          => gddimens%nmax
       nmaxus        => gddimens%nmaxus
       ndx           => gddimens%ndx
       lnx           => gddimens%lnx
       ndkx          => gddimens%ndkx
       lnkx          => gddimens%lnkx

       mmaxgl        => mmax                                ! => gdp%gdparall%mmaxgl         ! RL TODO: Hier een  aparte parameter voor nodig ? 
       nmaxgl        => nmax                                ! => gdp%gdparall%nmaxgl

       !
       ! Initialize local variables
       !
       ! Frequention := 360 degree / period
       ! where period = [ (FTMSTO - FMTSTR) * DTSEC ] / [ FNUMCY * 3600 ]
       ! FOUFAS is defined in RDFOUR as
       ! FOUFAS =  2 * PI * FNUMCY / [(FTMSTO - FMTSTR) ]
       ! so FREQNT = FOUFAS * RADDEG * 3600 / DTSEC is OK
       !
       shift = ftmstr(ifou)*foufas(ifou)
       freqnt = foufas(ifou)*raddeg*3600.0_fp/dtsec
       tfastr = real(ftmstr(ifou),fp)*dtsec/60.0_fp
       tfasto = real(ftmsto(ifou),fp)*dtsec/60.0_fp
       !
       select case (founam(ifou))
       case('s1')
            nmaxus = ndx
       case('ws')
            nmaxus = lnx
       case('u1','v1')
            nmaxus = lnkx
       case('qxk')
            nmaxus = lnx
       case('ta')
            nmaxus = ndx
       case('ux','uy','uc')
            nmaxus = ndkx
       case('uxa','uya')
            nmaxus = ndx
       case('r1')
            nmaxus = ndkx
       end select

       if (founam(ifou)(:2)=='s1') then
          if (fouelp(ifou)=='e') then
             ibleh = ibleh + 1
             blnm = 'EH??'
             write (blnm(3:4), '(i2.2)') ibleh
          else
             iblwl = iblwl + 1
             blnm = 'WL??'
             write (blnm(3:4), '(i2.2)') iblwl
          endif
       endif
       if (founam(ifou)(:2)=='ws') then
          iblws = iblws + 1
          blnm = 'WS??'
          write (blnm(3:4), '(i2.2)') iblws
       endif
       if (founam(ifou)(:2)=='r1') then
          iblcn = iblcn + 1
          blnm = 'CO??'
          write (blnm(3:4), '(i2.2)') iblcn
       endif
       if (founam(ifou)(:2)=='uy' .or. founam(ifou)(:2)=='ux') then
          ibluv = ibluv + 1
          write (blnm(3:4), '(i2.2)') ibluv
          if (founam(ifou)(:3)=='ux') then
             blnm = 'UX??'
          else
             blnm = 'UY??'
          endif
       endif
       if (founam(ifou)(:3)=='uya' .or. founam(ifou)(:3)=='uxa') then
          ibluva = ibluva + 1
          write (blnm(3:4), '(i2.2)') ibluva
          if (founam(ifou)(:3)=='uxa') then
             blnm = 'UX??'
          else
             blnm = 'UY??'
          endif
       endif
       if (founam(ifou)(:2)=='uc') then
          ibluc = ibluc + 1
          blnm = 'UC??'
          write (blnm(3:4), '(i2.2)') ibluc
       endif
       if (founam(ifou)(:2)=='ta') then
          iblbs = iblbs + 1
          blnm = 'BS??'
          write (blnm(3:4), '(i2.2)') iblbs
       endif

       !
       ! Write data for user defined dimensions, hence NMAXUS and MMAX
       ! First for Maximum or Minimum
       !
       if (allocated(glbarr3)) deallocate(glbarr3, stat = ierror)
       allocate(glbarr3(nmaxgl,mmaxgl,2), stat = ierror)
       glbarr3 = defaul

       if (fouelp(ifou)=='x' .or. fouelp(ifou)=='i' .or. fouelp(ifou)=='a' .or. fouelp(ifou)=='e') then
          do n = 1, nmaxus
             do m = 1, mmax
                !
                ! Only write values unequal to initial min/max values (-/+1.0e+30)
                !
                if (comparereal(abs(fousma(n,m,ifou)),1.0e29_fp)==-1) then
                   select case (fouelp(ifou))
                   case ('x','i','e')
                       glbarr3(n,m,1) = real(fousma(n,m,ifou),sp)
                   case ('a')
                      if( fousmb(n,m,ifou) > 0d0 ) then
                         glbarr3(n,m,1) = real(fousma(n,m,ifou),sp)/ fousmb(n,m,ifou)
                      endif
                   end select
                endif
             enddo
          enddo
          fouvar = fouref(ifou,2)
          ierror = unc_put_var_map(fileids%ncid,fileids%id_tsp, idvar(:,fouvar),   iloc, glbarr3(1:nmaxus,1,1))          ! write amplitudes
          if ((fouelp(ifou)=='i' .or. fouelp(ifou)=='x') .and. founam(ifou)=='s1') then
             !
             ! Write min or max waterdepth too
             !
             do n = 1, nmaxus
                do m = 1, mmax
                   !
                   ! Only write values unequal to initial min/max values (-/+1.0e+30)
                   !
                   if (comparereal(abs(fousmb(n,m,ifou)),1.0e29_fp)==-1) then
                      glbarr3(n,m,2) = real(fousmb(n,m,ifou),sp)
                   endif
                enddo
             enddo
             ierror = unc_put_var_map(fileids%ncid,fileids%id_tsp, idvar(:,fouvar+1), iloc, glbarr3(1:nmaxus,1,2))          ! write phase
          endif
       else
          if (allocated(glbarr3)) deallocate(glbarr3, stat = ierror)
          allocate(glbarr3(nmaxgl,mmaxgl,2), stat = ierror)
          glbarr3 = defaul
          !
          ! Write data for user defined dimensions, hence NMAXUS and MMAX
          !
          fouvar = fouref(ifou,2)
          do n = 1, nmaxus
             do m = 1, mmax
                ltest = (fousma(n, m, ifou)==0.0_fp .and. fousmb(n, m, ifou)==0.0_fp)
                !
                ! Test for active point and non-zero values
                ! when KCS (N,M) = 1 N > 1 and M > 1 per definition
                !
                if (.not.ltest) then
                   fousma(n, m, ifou) = fousma(n, m, ifou)                         &
                                      & *2.0_fp/(real(ftmsto(ifou) - ftmstr(ifou),fp))
                   fousmb(n, m, ifou) = fousmb(n, m, ifou)                         &
                                      & *2.0_fp/(real(ftmsto(ifou) - ftmstr(ifou),fp))
                   amp = sqrt(fousma(n, m, ifou)*fousma(n, m, ifou)                &
                       & + fousmb(n, m, ifou)*fousmb(n, m, ifou))
                   fas = atan2(fousmb(n, m, ifou), fousma(n, m, ifou)) + shift
                   if (fnumcy(ifou)==0) then
                      amp = 0.5_fp*amp*cos(fas)
                      fas = 0.0_fp
                   endif
                   !
                   ! Timezone correction added timezone*phase [degrees/hr].
                   ! foufas       is in [rad/timestep]
                   ! halftimestep is in [sec/timestep]
                   ! => timezonecorr = tzone [-] * foufas [rad/timestep] * raddeg [deg/rad] * [sec/hr] / (2 * halftimestep [sec/timestep])
                   !
                   fas = fas*raddeg + fv0pu(ifou) - tzone*foufas(ifou)*raddeg*1800.0_fp/hdt
                   !
                   ! To define FAS between 0 and 360. add 720. to the MOD of
                   ! FAS and re-use the MOD function
                   !
                   fas = mod(mod(fas, 360.0_fp) + 720.0_fp, 360.0_fp)
                   amp = amp/fknfac(ifou)
                   glbarr3(n,m,1) = real(amp,sp)
                   glbarr3(n,m,2) = real(fas,sp)
                else
                   !
                   ! Inactive point (not inside grid, can be open boundary)
                   ! defaul instead of xz/yz needed for GPP
                   ! '0' instead of kcs, because TEKAL does not accept '2'
                   !
                   glbarr3(n,m,1) = defaul               ! amplitudes 
                   glbarr3(n,m,2) = defaul               ! phases 
                endif
             enddo
          enddo
          ierror = unc_put_var_map(fileids%ncid,fileids%id_tsp, idvar(:,fouvar),   iloc, glbarr3(1:nmaxus,1,1))          ! write amplitudes
          ierror = unc_put_var_map(fileids%ncid,fileids%id_tsp, idvar(:,fouvar+1), iloc, glbarr3(1:nmaxus,1,2))          ! write phase
       endif
   end subroutine wrfous

end module m_fourier_analysis
