!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

! 
! 

module unstruc_startup
!! Separates some startup/initialization procedures from the main program in net.f90

use unstruc_ini
use unstruc_files
use properties
use unstruc_messages

implicit none

contains

!> Initialized global program settings
!! Used to be SUBROUTINE OPENING()
subroutine initProgram()
    use m_flowparameters
    use unstruc_colors
    use unstruc_model

    character(len=76)  :: filnam

    logical   :: jawel
    integer   :: istat

    call initSysEnv()                   ! Init paths



    call initGUI(1) ! READINI + INTINI

    ! Read hlp file
    FILNAM = trim(base_name)//'.hlp'
    INQUIRE(FILE = FILNAM,EXIST = JAWEL)
    IF (JAWEL) THEN
        call oldfil(MHLP,FILNAM)
    Endif
    if (mhlp < 1) then
        CALL SYSFIL(MHLP,FILNAM)
    ENDIF

    CALL HELPIN() ! TODO: help module? [AvD]

    !CALL IUPPERCASE(product_name)

    CALL SETCOLTABFILE(coltabfile,0)
    CALL SETCOLTABFILE(coltabfile2,1)

    RETURN
END subroutine initProgram


!> Initializes some info on the system environment.
!! Used to be biggest part of SUBROUTINE OPENING()
subroutine initSysEnv()
    use unstruc_files
    use dflowfm_version_module, only : product_name, base_name
    use string_module, only: get_dirsep
    implicit none

    integer :: larch
    integer :: lendum
    integer :: lenp
    integer :: lertxt
    integer :: lslash
    integer :: nval
    CHARACTER  FILNAM*76
    character  errtxt*8,arch*10,hlpstr*999,slash*1
    LOGICAL JAWEL ,d3dhom
!-----------------------------------------------------------------------
!-----Environment variable defined as D3D_HOME-ARCH-PROGNM-
!     or RGForQN_PATH-
!-----------------------------------------------------------------------
      hlpstr = ' '
      pathdi = ' '
      nval   = 0
      d3dhom = .false.
!

!-----------------------------------------------------------------------
!-----Environment variable D3D_HOME not defined or directory not found
!     Initialize PROGRAM PATH
!-----------------------------------------------------------------------
      if (nval   .ne. 0) then
         d3dhom = .false.
         lendum = LEN(pathdi) - 1
         nval   = 0
         if (product_name .eq. 'QUICKIN' ) then
            errtxt = 'QN_PATH'
         else if (product_name .eq. 'rgfgrid')then
            errtxt = 'RGF_PATH'
         else if (product_name .eq. 'KERNfl' ) then
            errtxt = 'FLS_PATH'
         else if (product_name .eq. 'NETWORK' ) then
            errtxt = 'NET_PATH'
         endif
         LERTXT = len_trim(ERRTXT)
         hlpstr = errtxt(:lertxt)//CHAR (0)
         call  HCACCESS(nval     ,lendum    ,hlpstr    )
      endif
!-----------------------------------------------------------------------
!-----If not found just give error messages and go ahead
!-----------------------------------------------------------------------
      if (nval .ne. 0) then
!        if (nval .eq. -111) then
!           write(*,*) '*** WARNING Environment variable '//errtxt//
!    *                 ' not found. Check Installation procedure'
!        elseif (nval .eq. -11) then
!           write(*,*) '*** WARNING Environment variable '//errtxt//
!    *                 ' to long. Check Installation procedure'
!        else
!           write(*,*) '*** WARNING Directory for '//errtxt//
!    *                 ' not found. Check Installation procedure'
!        endif
      else
!-----------------------------------------------------------------------
!--------Find out if system is PC (directory seperator character \ (92)
!        or UNIX (directory seperator character / (47))
!-----------------------------------------------------------------------
         slash = get_dirsep()
!-----------------------------------------------------------------------
!--------Define directory when environment variable is D3D_HOME etc.
!-----------------------------------------------------------------------
         LENDUM = len_trim(HLPSTR)
         LARCH  = len_trim(ARCH)
         if (d3dhom) then
            if (larch .eq. 0) then
               pathdi = hlpstr(:lendum)//slash//base_name//slash
            else
               pathdi = hlpstr(:lendum)//slash//arch  (:larch)//slash//base_name//slash
            endif
         else
!-----------------------------------------------------------------------
!-----------Define directory when environment variable is QN/RGF_PATH
!-----------------------------------------------------------------------
            pathdi = hlpstr(:lendum)//slash
         endif
      endif
end subroutine

SUBROUTINE HCACCESS(nval     ,larch     ,arch      )
      implicit none
      integer :: infoopsystem
      integer :: l
      integer :: larch
      integer :: lendum
      integer :: nopsys
      integer :: nval
      CHARACTER ARCH*(*), HULPSTR*64
      NOPSYS = INFOOPSYSTEM(1)
      HULPSTR =  &
      '                                                                '
      CALL get_environment_variable(trim(arch), HULPSTR)
      LENDUM = len_trim(HULPSTR)
      IF (LENDUM .GT. 0) THEN
         IF (LENDUM .LE. LARCH) THEN
            NVAL = 0
            WRITE(ARCH,'(A)') HULPSTR(1:LENDUM)
         ELSE
            NVAL = -11
         ENDIF
      ELSE
         NVAL = -111
      ENDIF
      RETURN
      END subroutine hcaccess

    !> Initializes interface/screen settings
    !! Used to be SUBROUTINE REACOL
subroutine initGUI(INTINIT)
    USE M_MISSING
    use unstruc_display
    use dflowfm_version_module, only : base_name
    use m_arcinfo

    implicit none
    double precision :: croshrsz
    double precision :: dv
    double precision :: dx
    double precision :: dxshow
    double precision :: dy
    integer :: i, INTINIT, ISTAT, maxarctiler, maxsamarcr
    integer :: iblue
    integer :: icl
    integer :: ifltyp
    integer :: igreen
    integer :: ihcopts
    integer :: ihmous
    integer :: ired
    integer :: ivmous
    integer :: ja
    integer :: jaauto
    integer :: jvga
    integer :: k
    integer :: keepstartdir
    integer :: limslo
    integer :: limtel
    integer :: limwat
    integer :: mout
    integer :: ncols
    integer :: ndec
    integer :: ndraw
    integer :: nhcdev
    integer :: nie
    integer :: nis
    integer :: ntxcols
    integer :: ntxrows
    integer :: numhcopts
    integer :: nv
    integer :: nvec
    integer :: nxpix
    integer :: nypix, jaopengl_loc
    double precision :: rmiss
    double precision :: scalesize
    double precision :: signz
    double precision :: val
    double precision :: vfac
    double precision :: vfacforce
    double precision :: vmax
    double precision :: vmin
    double precision :: xd
    double precision :: xsc
    double precision :: ysc
    double precision :: tsize
    integer, dimension(4,50) :: rgbvalues
    logical :: jawel

    character(len=76)  :: filnam
    character(len=180) :: inifilename

    CHARACTER REC*132
    COMMON /CSPEED/  LIMTEL, LIMSLO, LIMWAT, IHMOUS, IVMOUS
    COMMON /INITSCREEN/  CROSHRSZ,JVGA,NXPIX,NYPIX,NTXCOLS,NTXROWS
    COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
    COMMON /TEXTSIZE/ TSIZE

    COMMON /HARDCOPY/ NHCDEV,NUMHCOPTS,IHCOPTS(2,20)
    COMMON /OLDORNEWNAMES/ IFLTYP
    COMMON /STARTDIR/  KEEPSTARTDIR
    COMMON /SCALEPOS/ XSC,YSC,SCALESIZE,NDEC
    COMMON /VFAC/ VFAC, VFACFORCE, NVEC
    COMMON /DRAWTHIS/ ndraw(50)

    type(tree_data), pointer :: ini_ptr !< Unstruc.ini settings in tree_data

    ! Read ini file
    FILNAM = trim(base_name)//'.ini'
    INQUIRE(FILE = FILNAM,EXIST = JAWEL)
    IF (JAWEL) THEN
        inifilename = filnam
    else
        call sysfilepath(filnam, inifilename)
        if (inifilename(:1)=='\') then
           inifilename=inifilename(2:)
        endif
    end if

    call readIniFile(inifilename, ini_ptr, errmsg=msgbuf, istat=istat)
    if (istat /= 0) then
       ! make default unstruc.ini, try again
       call makeunstrucini(filnam,istat)
       if (istat==0) then
          call readIniFile(inifilename, ini_ptr, errmsg=msgbuf, istat=istat)
          if (istat /= 0) then 
             call err_flush()
          endif
       else
          call err_flush()
       endif
    end if

    call get_req_integer(ini_ptr, 'screen', 'JVGA',    JVGA)
    call get_req_integer(ini_ptr, 'screen', 'NXPIX',   NXPIX)
    call get_req_integer(ini_ptr, 'screen', 'NYPIX',   NYPIX)
    call get_req_integer(ini_ptr, 'screen', 'NTXCOLS', NTXCOLS)
    call get_req_integer(ini_ptr, 'screen', 'NTXROWS', NTXROWS)


    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLDG',    NCOLDG)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLRG',    NCOLRG)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLDN',    NCOLDN)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLRN',    NCOLRN)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLNN',    NCOLNN)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLSP',    NCOLSP)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLLN',    NCOLLN)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLTX',    NCOLTX)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLPL',    NCOLPL)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLCRS',   NCOLCRS)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLTHD',   NCOLTHD)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLFXW',   NCOLFXW)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLMH',    NCOLMH)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLWARN1', NCOLWARN1)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLWARN2', NCOLWARN2)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLHL',    NCOLHL)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLANA',   NCOLANA)

    call prop_get_integer(ini_ptr, 'grafcol', 'KLVEC',  KLVEC)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLAXS',  KLAXS)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLSCL',  KLSCL)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLTEX',  KLTEX)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLFRA',  KLFRA)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLOBS',  KLOBS)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLSAM',  KLSAM)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLZM',   KLZM)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLANK',  KLANK)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLPROF', KLPROF)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLSRC',  KLSRC)
    
    ! Cursor speed (in graphic mode)
    LIMTEL=200
    LIMSLO=20
    LIMWAT=400
    IHMOUS=40
    IVMOUS=40 !(orgiginal values 200, 20, 400)
    !Decrease LIMTEL if cursor movement responds too slow to the arrow keys.
    !LIMSLO gives the maximum increase of the cursor-postion
    !per time step in pixels. Increase it for higher maximum speeds.
    !LIMWAT gives the number of cycles to wait after the
    !'Ins'- or 'Enter'-key or left/right mouse buttons have been pressed.
    !Decrease if response is too slow, increase if response is too fast.
    !IHMOUS, IVMOUS mouse sensitivity, larger numbers, more hand movement

    ! size + position of HELP text screen
    NPOS(1)=2
    NPOS(2)=2
!    NPOS(3)=78
    NPOS(3)=120
    NPOS(4) = 16

    CR = .004             ! size of circle relative to screen size
    CROSHRSZ = .01        ! size of crosshair cursor relative to screen size

    ! Color scheme isolines
    call prop_get_string(ini_ptr, 'isocol', 'COLTABFILE', coltabfile)
    inquire(file = trim(coltabfile), exist = jawel)
    if (.not. jawel) then
        coltabfile = 'ISOCOLOUR.hls'
    end if

    coltabfile2 = coltabfile
    
    call get_req_integer(ini_ptr, 'isocol', 'AUTO', JAAUTO)
    call get_req_integer(ini_ptr, 'isocol', 'NV',   NV)
    call get_req_double (ini_ptr, 'isocol', 'VMIN', VMIN)
    call get_req_double (ini_ptr, 'isocol', 'VMAX', VMAX)
    NIS = 46   !INDEX FIRST ISOLINE COLOUR <1, 250>
    NIE = 224  !INDEX LAST  ISOLINE COLOUR <NIS+NV, 254>
    call prop_get_integer(ini_ptr, 'isocol', 'NIS', NIS)
    call prop_get_integer(ini_ptr, 'isocol', 'NIE', NIE)

    DV   = VMAX - VMIN
    DO I = 1,NV
        VAL(I) = VMIN + (I-1)*DV/(NV-1)
    ENDDO
    call inidepmax2()

    ! Text size
    call get_req_double(ini_ptr, 'text', 'TSIZE', TSIZE)

    ! Harcopy output
    ! (format of hardcopy output file)
    NHCDEV=6   ! (1:hpgl, 2:ps , 3:acorn, 4:raster,
               ! 5:tek , 6:pcx, 7:pic  , 8:dxf   ,
               ! 9:cgm ,12: hpgl2)
               ! (and windows only: 10 print manager, 11 windows metafile)

    call prop_get_integers(ini_ptr, 'hardcopyoptions', 'IHCOPTS', IHCOPTS, size(ihcopts))
    NUMHCOPTS = 0
    ! Determine actual number of HC-options read.
    do
        if (numhcopts >= size(ihcopts,2)) then
            exit
        endif
        if (ihcopts(1,numhcopts+1) == 0) then
            exit
        endif
        numhcopts = numhcopts + 1
    enddo

    call prop_get_integer(ini_ptr, 'display', 'NTEK', NTEK)
    call prop_get_integer(ini_ptr, 'display', 'PLOTTOFILE', plottofile)
    call prop_get_integer(ini_ptr, 'display', 'JADATETIME', jadatetime)
    jaopengl_loc = -1   ! unset
    call prop_get_integer(ini_ptr, 'display', 'JAOPENGL'  , jaopengl_loc)
    if ( jaopengl_loc.ne.-1 ) then
       call iset_jaopengl(jaopengl_loc)
    end if


    if (plottofile==1) then
        ndraw(10) = 1
    end if
    VFAC   = 1
    NVEC   = 1
    call prop_get_double(ini_ptr, 'display', 'VFAC', vfac)

    ! Old or new file names
    IFLTYP = 1       ! 0, OLD FILENAMES TELMCRGF.*, RGFLANDB.*
                     ! 1, NEW FILENAMES *.GRD, *.LDB, *.DEP, *.XYZ, *.A*,

    KEEPSTARTDIR = 0 ! 1 : always go back to startup directory
                     ! 0 : keep directory of latest directory change

! TODO: rgfspul Wordt elders gezet, maar niet alles (bijv fsma)
!    ! RGF SETTINGS
!    MFAC=5
!    NFAC=5
!    ITATP=3
!    ITBND=15
!    ITIN=25
!    ATPF=1.0
!    BFAC=1.0
!
!    CSMO=0.2
!    RFAC=.10
!    BAAS2=0.5
!    SRM=1
!    SRN=0.2          ! (SIZERATIO DEPTH/SLOPE DESIGN)
!    DEPSLO=1.00      ! (DEPTH/SLOPE DESIGN WEIGHT)
!    ITSMA=10
!    FSMA=.10         ! (DEPTH/SLOPE WEIGHT SMOOTHING)
!    ALINEN=0.0
!    ALINEM=0.0       ! LINE/FIELD WEIGHT, FIELD = 0, LINE = 1



!     Interactor klaarzetten
    IF (INTINIT == 1) THEN
       CALL INTINI()
    ENDIF

    NREDS    = 0
    NGREENS  = 0
    NBLUES   = 0
    NREDP    = 255
    NGREENP  = 255
    NBLUEP   = 200
    call prop_get_integer(ini_ptr, 'grafcol', 'NREDS'  , NREDS  )
    call prop_get_integer(ini_ptr, 'grafcol', 'NGREENS', NGREENS)
    call prop_get_integer(ini_ptr, 'grafcol', 'NBLUES' , NBLUES )
    call prop_get_integer(ini_ptr, 'grafcol', 'NREDP'  , NREDP  )
    call prop_get_integer(ini_ptr, 'grafcol', 'NGREENP', NGREENP)
    call prop_get_integer(ini_ptr, 'grafcol', 'NBLUEP' , NBLUEP )
    CALL IGRPALETTERGB(0,NREDS,NGREENS,NBLUES)

    call prop_get_integer(ini_ptr, 'display', 'JAFULLBOTTOMLINE' , jafullbottomline )

    rgbvalues(:,:) = 0
    rgbvalues(1:4,1)  = (/ 210,    3,    3,    3 /)
    rgbvalues(1:4,2)  = (/ 211,    1,  128,  255 /)  ! NCOLRN = SHOW ALL LINKS/prev net
    rgbvalues(1:4,3)  = (/ 212,  255,  160,  192 /)  ! NCOLRG = prev grid
    rgbvalues(1:4,4)  = (/ 210,  200,  200,  200 /)  ! NCOLTX = SOME TEXTST 
    rgbvalues(1:4,5)  = (/ 230,   32,  176,    0 /)  ! NCOLCRS = CROSS SECTIONS
    rgbvalues(1:4,6)  = (/ 231,  255,    0,    0 /)  ! NCOLTHD = THIN DAMS
    rgbvalues(1:4,7)  = (/ 232,  255,  106,    0 /)  ! NCOLFXW = FIXED WEIRS
    rgbvalues(1:4,8)  = (/ 227,    0,  200,  200 /)  ! KLOBS = OBS.STATIONS
    rgbvalues(1:4,9)  = (/ 203,    0,  255,  255 /)  ! NCOLLN = LAND BOUNDARY
    rgbvalues(1:4,10) = (/ 204,  255,  255,  150 /)  ! NCOLSP = SPLINES
    rgbvalues(1:4,11) = (/ 205,  255,  255,  150 /)  ! NCOLNN = NET NODES (in case they differ from splines)

    ! Initialise more standard colors.
    ! Used most colors from the HTML 4.01 specification, see http://www.w3.org/TR/REC-html40/types.html#h-6.5
    ! and added some basic colors.
    !call IGRPALETTERGB(ncolgray, 128, 128, 128) ! gray is already set by default (background color).
    i = 11
    i=i+1; rgbvalues(1:4, i) = (/ ncolblack, 0, 0, 0 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncolwhite, 255, 255, 255 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncolred, 255, 0, 0 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncolyellow, 255, 255, 0 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncolgreen, 0, 255, 0 /) !< lime
    i=i+1; rgbvalues(1:4, i) = (/ ncolcyan, 0, 255, 255 /) !< aqua
    i=i+1; rgbvalues(1:4, i) = (/ ncolblue, 0, 0, 255 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncolmagenta, 255, 0, 255 /) !< fuchsia
    i=i+1; rgbvalues(1:4, i) = (/ ncolmaroon, 128, 0, 0 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncoldarkgreen, 0, 128, 0 /) !< green
    i=i+1; rgbvalues(1:4, i) = (/ ncolteal, 0, 128, 128 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncolpink, 255, 0, 128 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncolorange, 255, 128, 0 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncollavender, 128, 128, 255 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncolbrown, 128, 64, 0 /)
    K=1
    ! First load default colours into Interacter colors:
    do
        if (rgbvalues(1,k) == 0) then
            exit
        end if
        ICL   = MAX(1,MIN(rgbvalues(1,k),255))
        IRED  = MAX(0,MIN(rgbvalues(2,k),255))
        IGREEN= MAX(0,MIN(rgbvalues(3,k),255))
        IBLUE = MAX(0,MIN(rgbvalues(4,k),255))
        K=K+1
        CALL IGRPALETTERGB(ICL,IRED,IGREEN,IBLUE)
    end do


    ! Reset again
    rgbvalues(:,:) = 0
    ! And override with colors from inifile.
    call prop_get_integers(ini_ptr, 'grafcol','rgbvalues', rgbvalues, size(rgbvalues))
    k = 1
    do
        if (rgbvalues(1,k) == 0) then
            exit
        end if
        ICL   = MAX(1,MIN(rgbvalues(1,k),255))
        IRED  = MAX(0,MIN(rgbvalues(2,k),255))
        IGREEN= MAX(0,MIN(rgbvalues(3,k),255))
        IBLUE = MAX(0,MIN(rgbvalues(4,k),255))
        k = k+1
        CALL IGRPALETTERGB(ICL,IRED,IGREEN,IBLUE)
    end do
!     CALL READXYMIS(MINI)
!     CALL READAMISS(MINI)

      TXLIN  =  ' ' ! alle drie leeg

      TXSIZE = 0.75d0
      TXXpos = 0.5d0
      TXYpos = 0.015d0

      XSC    = 0.01d0
      YSC    = 0.07d0
      NDEC   = 3
      SCALESIZE = 0.5d0

      maxarctiler = 0 ; maxsamarcr = 0
      call prop_get_integer(ini_ptr, 'ARCINFOSAMPLES', 'MAXARCTILE', maxarctiler)
      call prop_get_integer(ini_ptr, 'ARCINFOSAMPLES', 'MAXSAMARC' , maxsamarcr)

      if (maxarctiler > 0) then 
          maxarctile = maxarctiler*maxarctiler
      endif
      if (maxsamarcr  > 0) then 
          maxsamarc = maxsamarcr*maxsamarcr
      endif
 

      RETURN
END subroutine initGUI


subroutine makeunstrucini(filnam,istat)
   
    implicit none

    character(len=76), intent(in)  :: filnam
    integer          , intent(out) :: istat

    integer  :: mout

    istat=-1

    call newfil(mout, filnam)
    if (mout<0) then
       istat=0
       write(mout,'(a)') '[program]                                                                   '
       write(mout,'(a)') 'Ident = #Unstruc 1.00.11#                                                   '
       write(mout,'(a)') '                                                                            '
       write(mout,'(a)') '[screen]           (screen type on pc, screen dimension on unix)            '
       write(mout,'(a)') 'JVGA=2             JVGA = 1, vga: JVGA  = 2, supervga:  (only on DOS)       '
       write(mout,'(a)') 'NXPIX=1600         preferred hor. and ver. resolution of initial screen     '
       write(mout,'(a)') 'NYPIX=1000                                                                  '
       write(mout,'(a)') 'NTXCOLS=100         number of columns and rows TEXT screen                  '
       write(mout,'(a)') 'NTXROWS=50                                                                  '
       write(mout,'(a)') '                                                                            '
       write(mout,'(a)') '[GRAFCOL]          colors in graphic screens                                '
       write(mout,'(a)') 'NCOLDG=31          DESIGN GRID                                              '
       write(mout,'(a)') 'NCOLRG=212         PREVIOUS STATE GRID                                      '
       write(mout,'(a)') 'NCOLDN=3           DESIGN NET                                               '
       write(mout,'(a)') 'NCOLRN=211         PREVIOUS STATE NET                                       '
       write(mout,'(a)') 'NCOLNN=205         NETNODES                                                 '
       write(mout,'(a)') 'NCOLSP=204         SPLINES                                                  '
       write(mout,'(a)') 'NCOLLN=120         LAND BOUNDARY  OR 203 IF YOU LIKE PINK                   '
       write(mout,'(a)') 'NCOLTX=210         POLYGON                                                  '
       write(mout,'(a)') 'NCOLCRS=230        CROSS SECTIONS                                           '
       write(mout,'(a)') 'NCOLTHD=231        THIN DAMS                                                '
       write(mout,'(a)') 'NCOLTDK=232        THIN DYKES                                               '
       write(mout,'(a)') 'NCOLWARN1=191      WARNING 1                                                '
       write(mout,'(a)') 'NCOLWARN2=31       WARNING 2                                                '
       write(mout,'(a)') 'NCOLHL=31          HIGHLIGHT NODES/LINKS                                    '
       write(mout,'(a)') '                                                                            '
       write(mout,'(a)') 'KLVEC=4            VECTORS 110                                              '
       write(mout,'(a)') 'KLAXS=31           AXIS                                                     '
       write(mout,'(a)') 'KLSCL=221          ISOSCALE LEGEND                                          '
       write(mout,'(a)') 'KLTEX=3            NUMBERS                                                  '
       write(mout,'(a)') 'KLFRA=31           FRAME                                                    '
       write(mout,'(a)') 'KLSAM=31           SAMPLE MONOCOLOR                                         '
       write(mout,'(a)') 'KLOBS=227          OBSERVATION POINTS                                       '
       write(mout,'(a)') 'KLZM=31            ZOOMWINDOW                                               '
       write(mout,'(a)') 'KLANK=31           ANCHOR                                                   '
       write(mout,'(a)') '                                                                            '
       write(mout,'(a)') 'RGBVALUES=\           # COLORNUMBER  RED  GREEN  BLUE                       '
       write(mout,'(a)') '210    3    3    3 \                                                        '
       write(mout,'(a)') '211    1  128  255 \  # NCOLRN = SHOW ALL LINKS/prev net                    '
       write(mout,'(a)') '212  255  160  192 \  # NCOLRG = prev grid                                  '
       write(mout,'(a)') '210  200  200  200 \  # NCOLTX = POLYGON                                    '
       write(mout,'(a)') '230   32  176    0 \  # NCOLCRS = CROSS SECTIONS                            '
       write(mout,'(a)') '231  255    0    0 \  # NCOLTHD = THIN DAMS                                 '
       write(mout,'(a)') '232  255  106    0 \  # NCOLTDK = THIN DYKES                                '
       write(mout,'(a)') '227    0  200  200 \  # KLOBS = OBS.STATIONS                                '
       write(mout,'(a)') '203    0  255  255 \  # NCOLLN = LAND BOUNDARY                              '
       write(mout,'(a)') '204  255  255  150    # NCOLSP = SPLINES                                    '
       write(mout,'(a)') '205  204  255  102    # NCOLSP = SPLINES                                    '
       write(mout,'(a)') '                      # KLVEC  = VECTORS                                    '
       write(mout,'(a)') '                                                                            '
       write(mout,'(a)') '# RED GREEN, BLUE VALUES FOR SCREEN AND FOR PLOTTING                        '
       write(mout,'(a)') 'NREDS  =100                                                                 '
       write(mout,'(a)') 'NGREENS=100                                                                 '
       write(mout,'(a)') 'NBLUES =100                                                                 '
       write(mout,'(a)') 'NREDP  =140                                                                 '
       write(mout,'(a)') 'NGREENP=140                                                                 '
       write(mout,'(a)') 'NBLUEP =140                                                                 '
       write(mout,'(a)') '                                                                            '
       write(mout,'(a)') '                                                                            '
       write(mout,'(a)') '[ISOCOL]           (for colorscheme isolines)                               '
       write(mout,'(a)') 'AUTO   = 1         (0,1)      autoscale off or on                           '
       write(mout,'(a)') 'NV     = 19        number of isolines                                       '
       write(mout,'(a)') 'VMIN   = 1         minimum isoline value (only to be used if JAAUTO = 0)    '
       write(mout,'(a)') 'VMAX   = 0         maximum isoline value (only to be used if JAAUTO = 0)    '
       write(mout,'(a)') 'NIS    = 46        INDEX FIRST ISOLINE COLOUR <1, 250>                      '
       write(mout,'(a)') 'NIE    = 224       INDEX LAST  ISOLINE COLOUR <NIS+NV, 254>                 '
       write(mout,'(a)') '                                                                            '
       write(mout,'(a)') '[TEXT]             (for display of ''numbers'')                               '
       write(mout,'(a)') 'TSIZE=0.50                                                                  '
       write(mout,'(a)') '                                                                            '
       write(mout,'(a)') '[HARDCOPYOPTIONS]                                                           '
       write(mout,'(a)') 'IHCOPTS = \                                                                 '
       write(mout,'(a)') '1     1200 \       # bitmap x resolution                                    '
       write(mout,'(a)') '2     900  \       # bitmap y resolution                                    '
       write(mout,'(a)') '5     1    \       # IHCOPTS(1,I), IHCOPTS(2,I) 0:portrait 1:landscape      '
       write(mout,'(a)') '7     0    \       # postscript: 0 = coloured lines, 1 =black lines         '
       write(mout,'(a)') '9     1    \       # thinnest lines                                         '
       write(mout,'(a)') '25    1    \       # device fill 0:no, 1:yes                                '
       write(mout,'(a)') '18    0    \       # no hpgl replay info                                    '
       write(mout,'(a)') '19    3    \       # hp-gl,pcl,epson escape seqences (0=no,3=yes)           '
       write(mout,'(a)') '22    1    \       # encapsulated postscript 0:no, 1:yes                    '
       write(mout,'(a)') '23    8    \       # number of bitplanes                                    '
       write(mout,'(a)') '26    2    \       # pcx:0, bmp:1,uncompressed, bmp:2,compressed            '
       write(mout,'(a)') '6     0            # 0:keep colours, 1:invert colours                       '
       write(mout,'(a)') '                                                                            '
       write(mout,'(a)') '[display]                                                                   '
       write(mout,'(a)') 'NTEK             = 10    # Nr of user timesteps between two redraws         '
       write(mout,'(a)') 'PLOTTOFILE       = 0     # Produce harcopy (1) or not (0)                   '
       write(mout,'(a)') 'JAOPENGL         = 1     # 1 : use OpenGL, 0 : use Interacter               '
       write(mout,'(a)') 'JAFULLBOTTOMLINE = 0     # Full explanation yes/no                          '

    endif
       
    call doclose(mout)

end subroutine makeunstrucini

end module unstruc_startup
