!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017.                                     
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

! $Id: xbeachwaves.f90 52266 2017-09-02 11:24:11Z klecz_ml $
! $HeadURL: https://repos.deltares.nl/repos/ds/branches/dflowfm/20161017_dflowfm_codecleanup/engines_gpl/dflowfm/packages/dflowfm_kernel/src/xbeachwaves.f90 $
module m_xerf
implicit none

contains

function xerf(x) result (y)

    implicit none

    integer                                         :: i
    double precision, dimension(:)                            :: x
    double precision, dimension(size(x))                      :: w,y
    integer                                         :: k
    double precision                                          :: t
    double precision, dimension(0:64)                         :: a,b

    ! based on derf.f from http://www.kurims.kyoto-u.ac.jp/~ooura/

    data (a(i), i = 0, 12) /                                    &
         0.00000000005958930743d0, -0.00000000113739022964d0,    &
         0.00000001466005199839d0, -0.00000016350354461960d0,    &
         0.00000164610044809620d0, -0.00001492559551950604d0,    &
         0.00012055331122299265d0, -0.00085483269811296660d0,    &
         0.00522397762482322257d0, -0.02686617064507733420d0,    &
         0.11283791670954881569d0, -0.37612638903183748117d0,    &
         1.12837916709551257377d0 /
    data (a(i), i = 13, 25) /                                   &
         0.00000000002372510631d0, -0.00000000045493253732d0,    &
         0.00000000590362766598d0, -0.00000006642090827576d0,    &
         0.00000067595634268133d0, -0.00000621188515924000d0,    &
         0.00005103883009709690d0, -0.00037015410692956173d0,    &
         0.00233307631218880978d0, -0.01254988477182192210d0,    &
         0.05657061146827041994d0, -0.21379664776456006580d0,    &
         0.84270079294971486929d0 / 
    data (a(i), i = 26, 38) /                                   &
         0.00000000000949905026d0, -0.00000000018310229805d0,    &
         0.00000000239463074000d0, -0.00000002721444369609d0,    &
         0.00000028045522331686d0, -0.00000261830022482897d0,    &
         0.00002195455056768781d0, -0.00016358986921372656d0,    &
         0.00107052153564110318d0, -0.00608284718113590151d0,    &
         0.02986978465246258244d0, -0.13055593046562267625d0,    &
         0.67493323603965504676d0 / 
    data (a(i), i = 39, 51) /                                   &
         0.00000000000382722073d0, -0.00000000007421598602d0,    &
         0.00000000097930574080d0, -0.00000001126008898854d0,    &
         0.00000011775134830784d0, -0.00000111992758382650d0,    &
         0.00000962023443095201d0, -0.00007404402135070773d0,    &
         0.00050689993654144881d0, -0.00307553051439272889d0,    &
         0.01668977892553165586d0, -0.08548534594781312114d0,    &
         0.56909076642393639985d0 / 
    data (a(i), i = 52, 64) /                                   &
         0.00000000000155296588d0, -0.00000000003032205868d0,    &
         0.00000000040424830707d0, -0.00000000471135111493d0,    &
         0.00000005011915876293d0, -0.00000048722516178974d0,    &
         0.00000430683284629395d0, -0.00003445026145385764d0,    &
         0.00024879276133931664d0, -0.00162940941748079288d0,    &
         0.00988786373932350462d0, -0.05962426839442303805d0,    &
         0.49766113250947636708d0 / 
    data (b(i), i = 0, 12) /                                    &
         -0.00000000029734388465d0, 0.00000000269776334046d0,    &
         -0.00000000640788827665d0, -0.00000001667820132100d0,   &
         -0.00000021854388148686d0, 0.00000266246030457984d0,    &
         0.00001612722157047886d0, -0.00025616361025506629d0,    &
         0.00015380842432375365d0, 0.00815533022524927908d0,     &
         -0.01402283663896319337d0, -0.19746892495383021487d0,   &
         0.71511720328842845913d0 / 
    data (b(i), i = 13, 25) /                                   &
         -0.00000000001951073787d0, -0.00000000032302692214d0,   &
         0.00000000522461866919d0, 0.00000000342940918551d0,     &
         -0.00000035772874310272d0, 0.00000019999935792654d0,    &
         0.00002687044575042908d0, -0.00011843240273775776d0,    &
         -0.00080991728956032271d0, 0.00661062970502241174d0,    &
         0.00909530922354827295d0, -0.20160072778491013140d0,    &
         0.51169696718727644908d0 / 
    data (b(i), i = 26, 38) /                                   &
         0.00000000003147682272d0, -0.00000000048465972408d0,    &
         0.00000000063675740242d0, 0.00000003377623323271d0,     &
         -0.00000015451139637086d0, -0.00000203340624738438d0,   &
         0.00001947204525295057d0, 0.00002854147231653228d0,     &
         -0.00101565063152200272d0, 0.00271187003520095655d0,    &
         0.02328095035422810727d0, -0.16725021123116877197d0,    &
         0.32490054966649436974d0 / 
    data (b(i), i = 39, 51) /                                   &
         0.00000000002319363370d0, -0.00000000006303206648d0,    &
         -0.00000000264888267434d0, 0.00000002050708040581d0,    &
         0.00000011371857327578d0, -0.00000211211337219663d0,    &
         0.00000368797328322935d0, 0.00009823686253424796d0,     &
         -0.00065860243990455368d0, -0.00075285814895230877d0,   &
         0.02585434424202960464d0, -0.11637092784486193258d0,    &
         0.18267336775296612024d0 / 
    data (b(i), i = 52, 64) /                                   &
         -0.00000000000367789363d0, 0.00000000020876046746d0,    &
         -0.00000000193319027226d0, -0.00000000435953392472d0,   &
         0.00000018006992266137d0, -0.00000078441223763969d0,    &
         -0.00000675407647949153d0, 0.00008428418334440096d0,    &
         -0.00017604388937031815d0, -0.00239729611435071610d0,   &
         0.02064129023876022970d0, -0.06905562880005864105d0,    &
         0.09084526782065478489d0 /

    w = abs(x)

    do i = 1,size(x)
       if (w(i) .lt. 2.2d0) then
          t       = w(i) * w(i)
          k       = int(t)
          t       = t - k
          k       = k * 13
          y(i)    = ((((((((((((a(k) * t + a(k + 1)) * t +        &
               a(k + 2)) * t + a(k + 3)) * t + a(k + 4)) * t +     &
               a(k + 5)) * t + a(k + 6)) * t + a(k + 7)) * t +     &
               a(k + 8)) * t + a(k + 9)) * t + a(k + 10)) * t +    &
               a(k + 11)) * t + a(k + 12)) * w(i)
       else if (w(i) .lt. 6.9d0) then
          k       = int(w(i))
          t       = w(i) - k
          k       = 13 * (k - 2)
          y(i)    = (((((((((((b(k) * t + b(k + 1)) * t +         &
               b(k + 2)) * t + b(k + 3)) * t + b(k + 4)) * t +     &
               b(k + 5)) * t + b(k + 6)) * t + b(k + 7)) * t +     &
               b(k + 8)) * t + b(k + 9)) * t + b(k + 10)) * t +    &
               b(k + 11)) * t + b(k + 12)
          y(i)    = 1 - y(i)**16
       else
          y(i)    = 1
       end if

       if (x(i) .lt. 0) y(i) = -y(i)
    enddo

  end function xerf

end module m_xerf

subroutine xbeach_wave_input
!! Start logging
!! Read input from params.txt
   use m_flowgeom
   use m_xbeach_data
   use m_xbeach_readkey
   use m_xbeach_filefunctions

   implicit none

   logical, save                     :: init = .false.   

   if (.not. init) then
      !! Start logging
      call start_logfiles(0)
      call writelog_startup()
      call xbeach_all_input()
      call writelog('ls','','Initializing .....')
   else
      call writelog_startup()
      call xbeach_all_input()
      call writelog('ls','','Reinitialized model .....')
   end if
   init = .true.
end subroutine xbeach_wave_input 


subroutine xbeach_all_input
   use m_flowgeom
   use m_xbeach_data
   use m_xbeach_readkey
   use m_xbeach_filefunctions
   use m_xbeach_errorhandling
   use m_flowtimes

   implicit none

    character(slen)                                     :: testc,line
    character(slen)                                     :: dummystring
    character(slen), dimension(:), allocatable          :: allowednames,oldnames

    integer                                             :: filetype,mmax,ier,ic
    logical                                             :: comment
    logical                                             :: fe1,fe2

    call writelog('sl','','Reading input parameters: ')
    !
    ! Check params.txt exists
    !
    call check_file_exist('params.txt')
    !
    !
    ! Physical processes
    call writelog('l','','--------------------------------')
    call writelog('l','','Physical processes: ')
    swave       = readkey_int ('params.txt','swave',         1,        0,     1)
    lwave       = readkey_int ('params.txt','lwave',         1,        0,     1)
    !
    ! Grid parameters
	call writelog('l','','--------------------------------')
    call writelog('l','','Grid parameters: ')
    thetamin = readkey_dbl ('params.txt','thetamin', -90.d0,    -180.d0,  180.d0,required=(swave==1))
    thetamax = readkey_dbl ('params.txt','thetamax',  90.d0,    -180.d0,  180.d0,required=(swave==1))
    dtheta   = readkey_dbl ('params.txt','dtheta',    10.d0,      0.1d0,   20.d0,required=(swave==1))
    thetanaut= readkey_int ('params.txt','thetanaut',    0,        0,     1)
    !
    !
    ! Wave boundary condition parameters
    call writelog('l','','--------------------------------')
    call writelog('l','','Wave boundary condition parameters: ')
    allocate(allowednames(12),oldnames(12))
    allowednames=(/'stat        ','bichrom     ','ts_1        ','ts_2        ','jons        ','swan        ', &
         'vardens     ','reuse       ','ts_nonh     ','off         ','stat_table  ','jons_table  '/)
    oldnames=(/'0 ','1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 ','40','41'/)
    !             function =   file         key      default  n allowed  n old allowed  allowed names  old allowed names
    instat  = readkey_str('params.txt', 'instat', 'bichrom', 12, 12, allowednames, oldnames, required=(swave==1))
    deallocate(allowednames,oldnames)
    if (  trim(instat)=='jons' .or. &
         trim(instat)=='swan' .or. &
         trim(instat)=='vardens'.or. &
         trim(instat)=='stat_table' .or. &
         trim(instat)=='jons_table' &
         )then
       bcfile = readkey_name('params.txt','bcfile')
       call check_file_exist(bcfile)
       call checkbcfilelength(tstop_user,instat,bcfile,filetype)
       !filetype = 0 
    elseif (trim(instat)=='reuse') then
       ! TO DO: check file length is done after recomputation of tstop due to morfacopt
       ! at the end of this subroutine.

          inquire(file='ebcflist.bcf',exist=fe1)
          inquire(file='qbcflist.bcf',exist=fe2)


       if (.not. (fe1 .and. fe2)) then
             call writelog('lswe','', &
                    'If ''instat=reuse'' the model directory may not contain sufficient boundary definition files.')
             if (.not. fe1) then
                call writelog('lswe','','Model currently missing ebcflist.bcf')
             elseif (.not. fe2) then
                call writelog('lswe','','Model currently missing qbcflist.bcf')
             endif
             call xbeach_errorhandler()
       else
             call writelog('lswe','','If ''instat=reuse'' the model directory must contain boundary definition files.')
             call writelog('lswe','','Use ebcflist.bcf and qbcflist.bcf')
             call xbeach_errorhandler()
       endif
    else
       filetype=-1
    endif
    taper    = readkey_dbl ('params.txt','taper',   100.d0,      0.0d0, 1000.d0)
    nwavmax     = readkey_dbl ('params.txt','nmax',    0.8d0,       0.5d0, 1.d0)
    if (trim(instat) == 'stat') then
       Hrms  = readkey_dbl ('params.txt','Hrms',      1.d0,      0.d0,    10.d0)
       Tm01  = readkey_dbl ('params.txt','Tm01',     10.d0,      1.d0,    20.d0)
       Trep  = readkey_dbl ('params.txt','Trep',     Tm01,   1.d0,    20.d0)
       dir0  = readkey_dbl ('params.txt','dir0',    270.d0,    180.d0,   360.d0)
       m     = readkey_int ('params.txt','m',        10,         2,      128)
    elseif (trim(instat) == 'bichrom') then
       Hrms  = readkey_dbl ('params.txt','Hrms',      1.d0,      0.d0,    10.d0)
       Tm01  = readkey_dbl ('params.txt','Tm01',     10.d0,      1.d0,    20.d0)
       Trep  = readkey_dbl ('params.txt','Trep',     Tm01,   1.d0,    20.d0)
       Tlong = readkey_dbl ('params.txt','Tlong',    80.d0,     20.d0,   300.d0)
       dir0  = readkey_dbl ('params.txt','dir0',    270.d0,    180.d0,   360.d0)
       m     = readkey_int ('params.txt','m',        10,         2,      128)
    elseif (trim(instat) == 'ts_1' .or. trim(instat) == 'ts_2') then
       Hrms  = readkey_dbl ('params.txt','Hrms',      1.d0,      0.d0,    10.d0)
       Tm01  = readkey_dbl ('params.txt','Tm01',     10.d0,      1.d0,    20.d0)
       Trep  = readkey_dbl ('params.txt','Trep',     Tm01,   1.d0,    20.d0)
       dir0  = readkey_dbl ('params.txt','dir0',    270.d0,    180.d0,   360.d0)
       m     = readkey_int ('params.txt','m',        10,         2,      128)
       call check_file_exist('bc/gen.ezs')
    endif
    !
    !
    ! Wave-spectrum boundary condition parameters
    if (    trim(instat) == 'jons'          .or.    &
         trim(instat) == 'swan'          .or.    &
         trim(instat) == 'vardens'       .or.    &
         trim(instat) == 'jons_table'                ) then

       call writelog('l','','--------------------------------')
       call writelog('l','','Wave-spectrum boundary condition parameters: ')

       random          = readkey_int ('params.txt','random',       1,          0,          1       )
       fcutoff         = readkey_dbl ('params.txt','fcutoff',      0.d0,       0.d0,       40.d0   )
       nspr            = readkey_int ('params.txt','nspr',         0,          0,          1       )
       trepfac         = readkey_dbl ('params.txt','trepfac',      0.01d0,     0.d0,       1.d0    )
       sprdthr         = readkey_dbl ('params.txt','sprdthr',      0.08d0,     0.d0,       1.d0    )
       correctHm0      = readkey_int ('params.txt','correctHm0',   1,          0,          1       )
       Tm01switch      = readkey_int ('params.txt','Tm01switch',   0,          0,          1       )

       if (filetype==0) then
          rt          = readkey_dbl('params.txt','rt',   min(3600.d0,tstop_user),    1200.d0,    7200.d0 ) !! to do
          dtbc        = readkey_dbl('params.txt','dtbc',          1.0d0,      0.1d0,      2.0d0   )
       endif

       if (trim(instat)=='swan') then
          dthetaS_XB  = readkey_dbl ('params.txt','dthetaS_XB',   0.0d0,      -360.d0,    360.0d0 )
       endif

       nspectrumloc    = readkey_int ('params.txt','nspectrumloc',   1,          1,       10000 )

    endif
    !
    !
    ! Flow boundary condition parameters
    ! front
    call writelog('l','','--------------------------------')
    call writelog('l','','Flow boundary condition parameters: ')
    ARC         = readkey_int ('params.txt','ARC',      1,              0,       1       )
    order       = readkey_dbl ('params.txt','order',    2.d0,           1.d0,    2.d0    )
    freewave    = readkey_int ('params.txt','freewave', 0,    0,       1       )
    epsi        = readkey_dbl ('params.txt','epsi',     -1.d0,          -1.d0,   0.2d0   )
    hminlw      =  readkey_dbl ('params.txt','hmin',    0.2d0,     0.001d0,      1.d0)
    allocate(allowednames(2),oldnames(0))
    allowednames=(/'instant ','velocity'/)
    tidetype= readkey_str('params.txt','tidetype','velocity',2,0,allowednames,oldnames)
    deallocate(allowednames,oldnames)
    !
    !
    ! Wave breaking parameters
    
    if (swave==1) then
       call writelog('l','','--------------------------------')
       call writelog('l','','Wave breaking parameters: ')
       allocate(allowednames(5),oldnames(5))
       allowednames  =(/'roelvink1    ','baldock      ','roelvink2    ','roelvink_daly','janssen      '/)
       oldnames      =(/'1','2','3','4','5'/)
       if (trim(instat) == 'stat' .or. trim(instat) == 'stat_table') then
          break      = readkey_str('params.txt','break','baldock',5,5,allowednames,oldnames)
      else
          break      = readkey_str('params.txt','break','roelvink2',5,5,allowednames,oldnames)
       endif
       deallocate(allowednames,oldnames)
       gamma         = readkey_dbl ('params.txt','gamma',   0.55d0,     0.4d0,     0.9d0)   
       if (trim(break)=='roelvink_daly') then
          gamma2     = readkey_dbl ('params.txt','gamma2',   0.3d0,     0.0d0,     0.5d0)
       endif
       alpha         = readkey_dbl ('params.txt','alpha',   1.0d0,     0.5d0,     2.0d0)
       nroelvink     = readkey_dbl ('params.txt','n',       10.0d0,     5.0d0,    20.0d0)   
       gammax        = readkey_dbl ('params.txt','gammax',   2.d0,      .4d0,      5.d0)    
       deltaH        = readkey_dbl ('params.txt','delta',   0.0d0,     0.0d0,     1.0d0)
       fw            = readkey_dbl ('params.txt','fw',       0.d0,   0d0,      1.0d0)
       fwcutoff      = readkey_dbl ('params.txt','fwcutoff',  1000.d0,   0d0,      1000.d0)
       breakerdelay  = readkey_int ('params.txt','breakerdelay',    1,   0,      1)
       !
       !
       ! Roller parameters
       call writelog('l','','--------------------------------')
       call writelog('l','','Roller parameters: ')
       roller           = readkey_int ('params.txt','roller',     1,        0,     1)
       beta             = readkey_dbl ('params.txt','beta',    0.10d0,     0.05d0,   0.3d0)
       !
       !
       ! Wave-current interaction parameters
       call writelog('l','','--------------------------------')
       call writelog('l','','Wave-current interaction parameters: ')
       wci      = readkey_int ('params.txt','wci',        0,        0,     1)
       hwci     = readkey_dbl ('params.txt','hwci',   0.1d0,   0.001d0,      1.d0)
       cats     = readkey_dbl ('params.txt','cats',   4.d0,     1.d0,      50.d0)
    endif
    !
    !
    ! Wave numerics parameters
    call writelog('l','','--------------------------------')
    call writelog('l','','Wave numerics parameters: ')
    if (trim(instat) == 'stat' .or. trim(instat) == 'stat_table') then
       wavint     = readkey_dbl ('params.txt','wavint',    60.d0,      1.d0,  3600.d0)
       maxerror   = readkey_dbl ('params.txt','maxerror', 0.00005d0, 0.00001d0, 0.001d0)
       maxiter    = readkey_int ('params.txt','maxiter',    500,         2,      1000)
    endif
    !
    !
    ! Finish
    call writelog('l','','--------------------------------')
    call writelog('sl','','Finished reading input parameters')
    call writelog('l','','--------------------------------')
    !
    !
    ! -------------------   Post-input processing -------------------------
    !
    !
    ! Set taper to non-zero
    taper    = max(taper,1.d-6)
    !
    !
    ! Only allow Baldock in stationary mode and Roelvink in non-stationary
    if (trim(instat) == 'stat' .or. trim(instat) == 'stat_table') then
       if (trim(break) .ne. 'baldock') then
          if(trim(break)=='roelvink_daly') then
             call writelog('lws','','Warning: Roelvink-Daly formulations not implemented in stationary wave mode, use Baldock')
             call writelog('lws','','         formulation.')
             call xbeach_errorhandler()
          else
             call writelog('lws','','Warning: Roelvink formulations not allowed in stationary, use Baldock')
             call writelog('lws','','         formulation.')
          endif
       endif
    else
       if (trim(break)=='baldock') then
          call writelog('lws','','Warning: Baldock formulation not allowed in non-stationary, use a Roelvink')
          call writelog('lws','','         formulation.')
       endif
    endif
    !
    !
    ! Wave-current interaction with non-stationary waves still experimental
    if (trim(instat)/='stat' .and. trim(instat)/='stat_table' .and. wci==1) then
       call writelog('lws','','Warning: Wave-current interaction with non-stationary waves is still')
       call writelog('lws','','         experimental, continue with computation nevertheless')
    endif
    !    !
    !
    ! Check for unknown parameters
    call readkey('params.txt','checkparams',dummystring)

!   check swave and Lwave
    if ( swave.eq.0 ) Lwave = 0
   
end subroutine xbeach_all_input

subroutine xbeach_wave_init
   use m_flowgeom
   use m_flowexternalforcings
   use m_xbeach_data
   use m_sferic, only: pi, twopi
   use m_physcoef

   implicit none

   integer, allocatable, dimension(:) :: idum

   integer                            :: itheta, i, k, L

    if ( trim(instat)=='jons' .or. &
         trim(instat)=='jons_table' .or. &
         trim(instat)=='swan' .or. &
         trim(instat)=='vardens' .or. &
         trim(instat)=='reuse' &
         ) Trep=10.d0

    if ( trim(instat)=='jons' .or. &
         trim(instat)=='jons_table' .or. &
         trim(instat)=='swan' .or. &
         trim(instat)=='vardens') then 
       call xbeach_spectral_wave_init()
    endif

    if ( ntheta.gt.0 ) then
    
       ! dispersion
       do itheta=1,ntheta
          sigt(itheta,:) = twopi/Trep
       end do
       sigmwav = sum(sigt, dim=1)/ntheta
       L0 = 2*pi*ag/(sigmwav**2)
       L1 = L0
       Ltemp = L0
       call xbeach_dispersion()
    
    end if


   if ( allocated(kbndu2kbndw) ) deallocate(kbndu2kbndw)
   allocate(kbndu2kbndw(nbndu))

   if ( allocated(kbndw2kbndu) ) deallocate(kbndw2kbndu)
   allocate(kbndw2kbndu(nbndw))

   if ( allocated(kbndz2kbndw) ) deallocate(kbndz2kbndw)
   allocate(kbndz2kbndw(nbndz))

   allocate(idum(Lnx))

   idum = 0

!< Map velocity to wave
   do i=1,nbndw
      L = kbndw(3,i)
      idum(L) = i
   end do

   do i=1,nbndu
      L = kbndu(3,i)
      kbndu2kbndw(i) = idum(L)
   end do

!< map wl to wave
   do i=1,nbndz
      L = kbndz(3,i)
      kbndz2kbndw(i) = idum(L)
   end do

   idum = 0

!< Map wave to velocity
   do i=1,nbndu
      L = kbndu(3,i)
      idum(L) = i
   end do

   do i=1,nbndw
      L = kbndw(3,i)
      kbndw2kbndu(i) = idum(L)
   end do

   if ( allocated(idum) ) deallocate(idum)
   
   return
end subroutine xbeach_wave_init



!> make the thetagrid, in init_flowgeom
subroutine xbeach_makethetagrid()
   use m_flowgeom !, only:  ntheta, thetamax, thetamin, thetanaut, dtheta, Ndx, theta, theta0
   use m_xbeach_data
   use m_sferic
   use m_alloc
   implicit none

   integer                                     :: itheta, ierr, k

   ntheta = nint((thetamax-thetamin)/dtheta)

   call realloc(csx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('csx  (ndx)', ierr, ndx)
   call realloc(snx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('snx  (ndx)', ierr, ndx)
   call realloc(thet, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('thet  (ntheta,ndx)', ierr, ntheta*ndx)
   call realloc(costh, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('costh  (ntheta,ndx)', ierr, ntheta*ndx)
   call realloc(sinth, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('sinth  (ntheta,ndx)', ierr, ntheta*ndx)
   call realloc(thetabin, ntheta, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('thetabin  (ntheta)', ierr, ntheta)

   

   theta0=(1.5d0*pi)-dir0*atan(1.d0)/45d0
   if (theta0< -pi) theta0=theta0+2.d0*pi
   if (theta0> pi) theta0=theta0-2.d0*pi

   if (thetanaut==1) then  
      thetamin=(270.d0-thetamax)*dg2rd
      thetamax=(270.d0-thetamin)*dg2rd
   else
      thetamin=thetamin*dg2rd
      thetamax=thetamax*dg2rd
   endif

   if (thetamax>pi) then
      thetamax=thetamax-2*pi
      thetamin=thetamin-2*pi
   endif

   if (thetamin<-pi) then
      thetamax=thetamax+2*pi
      thetamin=thetamin+2*pi
   endif

   dtheta = dtheta*dg2rd

   do itheta=1,ntheta
      thetabin(itheta)=thetamin+dtheta/2d0+dtheta*(itheta-1)
   end do

   do itheta=1,ntheta
      csx(itheta)     = cos(thetabin(itheta))          
      snx(itheta)     = sin(thetabin(itheta))
      do k = 1, ndx
         thet(itheta,k)  = thetabin(itheta)          
         costh(itheta,k) = cos(thetabin(itheta))          
         sinth(itheta,k) = sin(thetabin(itheta))
      enddo
   enddo

end subroutine xbeach_makethetagrid

subroutine xbeach_dispersion()
    use m_xbeach_filefunctions
    use m_flowgeom
    use m_flow, only: hs
    use m_flowparameters, only: epshs
    use m_sferic, only: pi
    use m_xbeach_data
    use m_physcoef, only: ag
    use m_flowtimes, only: time0

    ! Robert: iteration along L=L0tanh(2pih/L)

    IMPLICIT NONE

!    double precision, dimension(:), allocatable      :: L0,kh,hh
        
    integer                                          :: i,j,j1,j2,k
    double precision, external                       :: iteratedispersion

    hdisp = max(hs + deltaH*H,epshs)

    L0 = 2*pi*ag/(sigmwav**2)
    !
    !if (.not. allocated(L1)) then
    !   allocate(L1(ndx))
    !   L1=L0
    !endif
    !if (.not. allocated(Ltemp)) then
    !   allocate(Ltemp(ndx))
    !   Ltemp = L0
    !end if


    do k = 1,ndx
          if (2*pi/L0(k)*hdisp(k) > 5d0) then
             Ltemp = L0
          else
             Ltemp(k) = iteratedispersion(L0(k),Ltemp(k),pi,hdisp(k))
             if (Ltemp(k)<0.d0) then   ! this is an error from iteratedispersion
                Ltemp(k) = -Ltemp(k)
                call writelog('lws','','Warning: no convergence in dispersion relation iteration at t = ', &
                     time0)
             endif
          end if
    end do
    L1 = Ltemp    
 
    kwav  = 2*pi/L1
    cwav  = sigmwav/kwav
    khdisp   = min(kwav*hdisp,10.0d0)
    nwav=0.5d0+khdisp/sinh(2*khdisp)
    cgwav=cwav*nwav

  end subroutine xbeach_dispersion

  function iteratedispersion(L0,Lestimate,px,h) result(L)

    implicit none
    ! input
    double precision,intent(in)    :: L0
    double precision,intent(in)    :: Lestimate
    double precision,intent(in)    :: px
    double precision,intent(in)    :: h
    ! output
    double precision               :: L
    ! internal
    double precision               :: L1,L2
    integer                        :: iter
    double precision               :: err
    double precision,parameter     :: aphi = 1.d0/(((1.0d0 + sqrt(5.0d0))/2)+1)
    double precision,parameter     :: bphi = ((1.0d0 + sqrt(5.0d0))/2)/(((1.0d0 + sqrt(5.0d0))/2)+1)
    integer,parameter              :: itermax = 150
    double precision,parameter     :: errmax = 0.00001d0


    err = huge(0.0d0)
    iter = 0
    L1 = Lestimate
    do while (err > errmax .and. iter < itermax)
       iter  = iter+1
       L2    = L0*tanh(2*px*h/L1)
       L1    = (L1*aphi + L2*bphi)          ! Golden ratio
       err   = abs(L2 - L1)
    end do

    if (iter<=itermax) then
       L = L1
    else
       ! signal this went wrong
       L = -L1
    endif

  end function iteratedispersion


!subroutine xbeach_dispersion()
!    use m_flowparameters, only : epshs
!    use m_flow, only: hs
!    use m_flowgeom, only : ndx
!    use m_physcoef, only : ag
!    use m_sferic, only: pi
!    use m_xbeach_data
!
!    implicit none
!
!    double precision, dimension(ndx)  :: hh, kh, k0, k1, del_k, del
!    integer                           :: kk
!
!    hh = max(hs+deltaH*H, epshs)
!    k0 = 4d0*pi*pi*(sigmwav**2d0)/ag     ! deep water approx
!    del = k0/1000
!
!    where (hh*k0 > 10d0)           ! deep
!       kwav = k0
!    else where (hh*k0 < pi*1d-2)   ! shallow
!       kwav = 2*pi*sigmwav/sqrt(ag*hh)
!    elsewhere                      !intermediate
!       call xbeach_iterate_dispersion()
!    end where
!
!    cwav = sigmwav / kwav
!    nwav = 0.5d0 + kh/dsinh(2*kh)
!    
!    do kk=1,ndx
!       cgwav(kk) = cwav(kk)*nwav(kk)
!    end do
!
!end subroutine xbeach_dispersion



subroutine dhsdxdhsdy(dhsdx, dhsdy)
    use m_flow
    use m_flowgeom
    use m_netw
    use m_xbeach_data
    use m_alloc

    implicit none

    integer                                        :: L, k1, k2, k3, k4, kkk, nwalls, kb, ki, ierr
    double precision                               :: dxx, dyy, wuL, cs, sn
    double precision, intent(out), dimension(ndx)  :: dhsdx, dhsdy
    double precision, allocatable                  :: dbdx(:), dbdy(:), dsdx(:), dsdy(:)
    double precision, external                     :: getdx, getdy
    
    allocate(dbdx(1:ndx), dbdy(1:ndx), dsdx(1:ndx), dsdy(1:ndx), stat = ierr)
    dbdx = 0d0
    dbdy = 0d0
    dsdx = 0d0
    dsdy = 0d0
    
    do L = 1,lnx
      k1 = ln(1,L)
      k2 = ln(2,L)
      k3 = lncn(1,L)
      k4 = lncn(2,L)
      
      dxx = getdx(xk(k3),yk(k3),xk(k4),yk(k4))
      dyy = getdy(xk(k3),yk(k3),xk(k4),yk(k4))

      dbdx(k1) = dbdx(k1)+.5d0*(zk(k3)+zk(k4))*dyy    !dimensie m*m JRE: maybe Perot averaging
      dbdy(k1) = dbdy(k1)-.5d0*(zk(k3)+zk(k4))*dxx
      dbdx(k2) = dbdx(k2)-.5d0*(zk(k3)+zk(k4))*dyy
      dbdy(k2) = dbdy(k2)+.5d0*(zk(k3)+zk(k4))*dxx
    end do

   nwalls = 0
    do nwalls=1,mxwalls
      k1 = walls(1,nwalls)
      k2 = walls(2,nwalls)
      k3 = walls(3,nwalls)
      
      cs = walls(7,nwalls)
      sn = walls(8,nwalls)
      wuL = walls(9,nwalls)
    
      dbdx(k1) = dbdx(k1)+0.5*(zk(k3)+zk(k2))*wuL*sn
      dbdy(k1) = dbdy(k1)-0.5*(zk(k3)+zk(k2))*wuL*cs
    end do

    do kkk=1,ndxi
      dbdx(kkk) =dbdx(kkk)/ba(kkk)
      dbdy(kkk) =dbdy(kkk)/ba(kkk)
    end do 
    
    do L = 1,Lnx
        if (hu(L) > epshu) then                            ! link flows
            k1 = ln(1,L)
            k2 = ln(2,L)
            dsdx(ln(1,L)) = dsdx(ln(1,L)) + wcx1(L)*(s1(k2) - s1(k1)) * dxi(L) ! dimension m4/m
            dsdy(ln(1,L)) = dsdy(ln(1,L)) + wcy1(L)*(s1(k2) - s1(k1)) * dxi(L)  
            dsdx(ln(2,L)) = dsdx(ln(2,L)) + wcx2(L)*(s1(k2) - s1(k1)) * dxi(L)
            dsdy(ln(2,L)) = dsdy(ln(2,L)) + wcy2(L)*(s1(k2) - s1(k1)) * dxi(L)
        endif
    enddo
        
    dhsdx = dsdx - dbdx
    dhsdy = dsdy - dbdy
    
    do kkk  = 1,nbndw
        kb = kbndw(1,kkk)
        ki = kbndw(2,kkk)
        dhsdx(kb) = dhsdx(ki)
        dhsdy(kb) = dhsdy(ki)
    enddo
    
    !call realloc(plotlin, max(Ndx,Lnx))
    !do kb=1,Ndx
    !   plotlin(kb) = sqrt(dhsdx(kb)**2+dhsdy(kb)**2)
    !end do

    deallocate(dbdx, dbdy, dsdx, dsdy, stat = ierr)
end subroutine dhsdxdhsdy

subroutine xbeach_wave_update_energy()
    use m_sferic, only:pi
    use m_physcoef, only: rhog, ag
    use m_flowgeom
    use m_flow, only: hs, epshu, vol1, rhomean, epshs, plotlin
    use m_flowexternalforcings, only: nbndw, zbndw
    use m_xbeach_data
    use m_partitioninfo
    use m_timer
    use m_alloc

    implicit none

    integer                        :: k, itheta, ierr, L, k1, k2, kb, ki, nwalls
    double precision, allocatable  :: hh(:), uorblok(:), Df(:), ddlok(:,:), wete(:,:), drr(:,:)
    double precision, allocatable  :: uwf(:), vwf(:), ustr(:), urf(:), vrf(:), ustw(:)

    double precision               :: dfac, fsqrtt

    allocate(hh(1:ndx), Df(1:ndx), ddlok(1:ntheta, 1:ndx), wete(1:ntheta, 1:ndx), drr(1:ntheta,1:ndx), stat = ierr)
    allocate(uorblok(1:ndx), ustw(1:ndx), uwf(1:ndx), vwf(1:ndx), ustr(1:ndx), stat = ierr)
    allocate(urf(1:ndx), vrf(1:ndx), stat = ierr)

    hh   = 0.d0
    Df   = 0.d0
    ddlok = 0.d0
    wete = 0.d0
    drr = 0.d0
    uorblok = 0d0
    ustw = 0d0
    uwf = 0d0
    vwf = 0d0
    ustr = 0d0
    urf = 0d0
    vrf = 0d0

    BR = beta
    hh = max(hs, epshs)

    thetamean=(sum(ee1*thet,1)/dble(ntheta))/(max(sum(ee1,1),0.00001d0)/dble(ntheta)) ! energy weighted wave direction
    sigmwav = max((sum(sigt,1)/dble(ntheta)),0.01d0)

    ee1 = ee1/sigt    
    call advec_horz(ee1, cgwav, horadvec)
    call advec_dir(ee1, ctheta, thetaadvec)
    
    do k = 1,ndxi
       do itheta = 1,ntheta
          if ( vol1(k) > epshs*ba(k) ) then
             ee1(itheta,k) = ee1(itheta,k) - dtmaxwav*(horadvec(itheta,k)  * bai(k) + thetaadvec(itheta,k)) 
          else
             ee1(itheta,k) = 0d0
          endif
       enddo
    enddo

    !plotlin(1:Ndx) = sum(horadvec(:,1:Ndx),dim=1)
    
    ee1 = ee1*sigt                   ! Back to wave energy

    ee1=max(ee1,0.0d0)

!
!   Energy integrated over wave directions,Hrms
!
    E=sum(ee1,dim=1)*dtheta
    H=sqrt(8.d0*E/rhog)

    do itheta=1,ntheta
       ee1(itheta,:)=ee1(itheta,:)/max(1.d0,(H/(gammax*hh))**2)
    enddo

    H=min(H,gammax*hh)
    E=1.d0/8.d0*rhog*(H**2)   

!   Breaker dissipation
    call xbeach_wave_breaker_dissipation()

!   Dissipation by bed friction
    !fw = 0d0
    dfac = 2.d0*fw*rhomean/(3.d0*pi)
    do k=1,Ndx
       uorblok(k) = pi * H(k) / Trep / sinh(min(max(kwav(k),0.01d0)*max(hh(k),deltaH*H(k)),10.0d0))
       Df(k)=dfac*fw*uorblok(k)**3
    end do
    where (hh>fwcutoff)
       Df = 0.d0
    end where
!
!   Distribution of total dissipation over directions
!
    do itheta=1,ntheta
       ddlok(itheta,:)=ee1(itheta,:)*(D+Df)/max(E,0.00001d0)
    enddo

    do itheta = 1, ntheta
       where (hh+deltaH*H>epshs) 
            wete(itheta,:)=1d0
       elsewhere
            wete(itheta,:)=0d0
       end where
    enddo

!!  Roller energy balance
    call advec_horz(rr,cwav, rrhoradvec)
    call advec_dir(rr,ctheta,rrthetaadvec)

    do k = 1,ndxi
       do itheta = 1,ntheta
          if ( vol1(k) > 0d0 ) then  
             rr(itheta,k) = rr(itheta,k) - dtmaxwav*(rrhoradvec(itheta,k)  * bai(k) + rrthetaadvec(itheta,k)) 
          else
             rr(itheta,k) = 0d0  ! check
          endif
       enddo
    enddo

    rr=max(rr,0.0d0)
!
!  euler step roller energy dissipation (source and sink function)
!
   do k = 1,ndx  ! ndx
      do itheta=1,ntheta
         if(wete(itheta, k)==1) then
            ee1(itheta,k)=ee1(itheta, k)-dtmaxwav*ddlok(itheta, k)
            if(roller==1) then
               drr(itheta, k) = 2*ag*BR(k)*max(rr(itheta, k),0.0d0)/    &
                  sqrt(cwav(k))
               rr(itheta, k)=rr(itheta, k)+dtmaxwav*(ddlok(itheta, k)   &
                  -drr(itheta, k))
            else if (roller==0) then
               rr(itheta, k)  = 0.0d0
               drr(itheta, k) = 0.0d0
            endif
            ee1(itheta, k)    = max(ee1(itheta, k),0.0d0)
            rr(itheta, k)     = max(rr(itheta, k),0.0d0)
         elseif(wete(itheta, k)==0) then
            ee1(itheta, k)    = 0.0d0
            rr(itheta, k)     = 0.0d0
         end if
      end do
end do

if ( jampi.eq.1 ) then
   write(6,*) 'my_rank=', my_rank
   if ( jatimer.eq.1 ) call starttimer(IXBEACH)
   call update_ghosts(ITYPE_Sall, Ntheta, Ndx, ee1, ierr)
   call update_ghosts(ITYPE_Sall, Ntheta, Ndx, rr,  ierr)
   if ( jatimer.eq.1 ) call stoptimer(IXBEACH)
end if

! Orbital velocity
fsqrtt = sqrt(0.5d0) ! 1 / sqrt(2.0)
do L=1,Lnx
   k1 = ln(1,L)
   k2 = ln(2,L)
   urms(L) = (acL(L) * uorblok(k1) + (1d0-acl(L))*uorblok(k2)) * fsqrtt
end do

call xbeach_apply_wave_bc()

!   OUTPUT Bulk quantities
    E  = sum(ee1,dim=1)*dtheta
    R  = sum(rr,dim=1)*dtheta
    DR = sum(drr,dim=1)*dtheta
    H  = sqrt(8.d0*E/rhog)
    thetamean=(sum(ee1*thet,dim=1)/dble(ntheta))/(max(sum(ee1,dim=1),0.00001d0)/dble(ntheta))

    ee1sum = H
!    call realloc(plotlin, ndx, fill = 0d0, keepExisting=.false.)
!    plotlin = 270d0 - thetamean*pi/180

! Stokes drift
    ustw= E/max(cwav,sqrt(epshs*ag))/rhomean/max(hh,epshs) !waves
    ustr=2d0*R/max(cwav,sqrt(epshs*ag))/rhomean/max(hh,epshs) !roller
    uwf = ustw*dcos(thetamean)                    !! Cartesian decomposition
    vwf = ustw*dsin(thetamean)
    urf = ustr*dcos(thetamean)
    vrf = ustr*dsin(thetamean)

    do L=1,lnx                                    !! facenormal decomposition
       k1 = ln(1,L); k2 = ln(2,L)
       ust(L) = acL(L)*(csu(L)*(uwf(k1)+urf(k1))+snu(L)*(vwf(k1)+vrf(k1))) + &
          (1d0-acL(L))*(csu(L)*(uwf(k2)+urf(k2))+snu(L)*(vwf(k2)+vrf(k2)))


       vst(L) = acL(L)*(-snu(L)*(uwf(k1)+urf(k1))+csu(L)*(vwf(k1)+vrf(k1))) + &
          (1d0-acL(L))*(-snu(L)*(uwf(k2)+urf(k2))+csu(L)*(vwf(k2)+vrf(k2)))
    enddo

   deallocate(hh, Df, ddlok, wete, drr, stat = ierr)
   deallocate(uorblok, ustw, ustr, uwf, vwf, urf, vrf, stat = ierr)

end subroutine xbeach_wave_update_energy


subroutine xbeach_wave_compute_flow_forcing()
    use m_flowgeom
    use m_flow
    use m_xbeach_data

    implicit none

    integer                        :: k, itheta, ierr, L, k1, k2, kb, ki
    double precision               :: dumFx, dumFy, ustL
    double precision, save         :: FMAX=1d5, deps
    double precision, allocatable  :: hh(:) 
    double precision, allocatable  :: ustnod(:)
    double precision, allocatable  :: Fxnod(:), Fynod(:)

    allocate(hh(1:ndx), stat = ierr) 
    allocate(ustnod(1:ndx), Fxnod(1:ndx), Fynod(1:ndx), stat = ierr)

!   initialization
    hh = 0d0
    ustnod = 0d0
    Fxnod = 0d0
    Fynod = 0d0

!   Radiation stresses
    !write(6,*)'ee1',ee1
    !write(6,*)'costh',costh
    !write(6,*)'sinth',sinth
    !write(6,*)'nwav',nwav
    !write(6,*)'dtheta',dtheta
    !write(6,*)'Sxx',Sxx
    !write(6,*)'Syy',Syy
    !write(6,*)'Sxy',Sxy

    Sxx=(nwav*sum((1.d0+costh**2)*ee1,dim=1)-.5d0*sum(ee1,dim=1))*dtheta     ! wave energy contribution
    Syy=(nwav*sum((1.d0+sinth**2)*ee1,dim=1)-.5d0*sum(ee1,dim=1))*dtheta
    Sxy=nwav*sum(sinth*costh*ee1,dim=1)*dtheta

    Sxx = Sxx + sum((costh**2)*rr,dim=1)*dtheta                    ! Roller contribution
    Syy = Syy + sum((sinth**2)*rr,dim=1)*dtheta
    Sxy = Sxy + sum(sinth*costh*rr,dim=1)*dtheta

!   Wave forces Fx, Fy, value on links
    Fxnod = 0d0
    Fynod = 0d0
    do L = 1, Lnx
       k1 = ln(1,L);   k2 = ln(2,L)
       dumFx = -(acl(L)*Sxx(k1)+(1-acl(L))*Sxx(k2))*wu(L)*csu(L) - (acl(L)*Sxy(k1)+(1-acl(L))*Sxy(k2))*wu(L)*snu(L)
       dumFy = -(acl(L)*Sxy(k1)+(1-acl(L))*Sxy(k2))*wu(L)*csu(L) - (acl(L)*Syy(k1)+(1-acl(L))*Syy(k2))*wu(L)*snu(L)
       Fxnod(k1) = Fxnod(k1) + dumFx
       Fxnod(k2) = Fxnod(k2) - dumFx
       Fynod(k1) = Fynod(k1) + dumFy
       Fynod(k2) = Fynod(k2) - dumFy
    enddo
    
    do k = 1, ndx
       Fxnod(k) = Fxnod(k)*bai(k)
       Fynod(k) = Fynod(k)*bai(k)
    enddo

    do L = 1,nbndw
       k1 = kbndw(1,L); k2=kbndw(2,L)
       Fxnod(k1) = Fxnod(k2)
       Fynod(k1) = Fynod(k2)
    end do

   do L = 1, nbndz
      k1 = kbndz(1,L); k2=kbndz(2,L)
      Fxnod(k1) = Fxnod(k2)
      Fynod(k1) = Fynod(k2)
   end do

   do L = 1, nbndu
      k1 = kbndu(1,L); k2=kbndu(2,L)
      Fxnod(k1) = Fxnod(k2)
      Fynod(k1) = Fynod(k2)
   end do

   !deps = max(1d1*epshu, 1d-4)  ! safety

    do L = 1,Lnx
       !if ( hu(L).gt.5d-2 ) then ! for robustness sake
          k1 = ln(1,L); k2 = ln(2,L)
          Fx(L) = ( acL(L)*Fxnod(k1) + (1d0-acL(L))*Fxnod(k2) )
          Fy(L) = ( acL(L)*Fynod(k1) + (1d0-acL(L))*Fynod(k2) )
       !else
       !   Fx(L) = 0d0
       !   Fy(L) = 0d0
       !end if

       !plotlin(L) = sqrt(Fx(L)**2 + Fy(L)**2)

       !if ( sqrt(Fx(L)**2 + Fy(L)**2).gt.FMAX ) then
       !   continue
       !end if
    enddo

    deallocate(hh)
    deallocate(Fxnod, Fynod, ustnod, stat = ierr)

end subroutine xbeach_wave_compute_flow_forcing

subroutine xbeach_wave_maxtimestep()
use m_flowtimes
use m_flow
use m_flowgeom
use m_xbeach_data
use m_partitioninfo

implicit none

integer           :: k, k1, k2, kk, L, itheta
double precision  :: dum, cgwavL, cwuL, dt

dtmaxwav = huge(0d0)

!!! Calculate max CFL based timestep for wave calculation
do k = 1, ndx
  do itheta = 1, ntheta
     dum = 0d0
     do kk = 1, nd(k)%lnx
       L = iabs(nd(k)%ln(kk))
       k1 = ln(1,L)
       k2 = ln(2,L)
       !cgwavL = 5d-1*(cgwav(k1) + cgwav(k2))
       cgwavL = 5d-1*(cwav(k1) + cwav(k2)) ! phase velocity instead of group velocity

       cwuL    = cgwavL*( csu(L)*csx(itheta) + snu(L)*snx(itheta) ) 

       ! cwuL = 2d0*cwuL  ! REMOVE
    
       if (ln(2,L) .eq. k) cwuL = -cwuL
    
       if (cwuL .ge. 0.) then        ! outgoing velocities only
          dum = dum + cwuL*wu(L)
       end if
      end do
      dum = dum + ctheta(itheta, k)/dtheta*ba(k)
      if (dum > tiny(0d0)) then
          dt = cflmx*ba(k) / dum
          if ( dt.lt.dtmaxwav ) then 
              dtmaxwav = dt
              !write(6,*) k, dt, ba(k)
          end if
      end if
  end do
end do

if ( jampi.eq.1 ) then
   call reduce_double_min(dtmaxwav)
end if

if (dtmaxwav > dts) dtmaxwav = dts
dtmaxwav = dts/ceiling(dts/dtmaxwav)
! DEBUG
dts = dtmaxwav
dti = 1d0/dts
! DEBUG
!write(6,*) dtmaxwav
end subroutine xbeach_wave_maxtimestep

subroutine xbeach_wave_compute_celerities
use m_flow
use m_flowgeom
use m_flowparameters, only: epshu
use m_sferic, only: pi
use m_xbeach_data

implicit none

integer                       ::  k, itheta, ierr
double precision, allocatable ::  dhsdx(:), dhsdy(:), hh(:), sinh2kh(:)

allocate(dhsdx(1:ndx), dhsdy(1:ndx), hh(1:ndx), sinh2kh(1:ndx), stat = ierr)
dhsdx   = 0d0
dhsdy   = 0d0
hh      = 0d0
sinh2kh = 0d0

hh = max(hs,epshs)

do itheta=1,ntheta
       sigt(itheta,:) = 2*pi/Trep
end do
sigmwav = sum(sigt,dim = 1)/dble(ntheta)

call xbeach_dispersion()                                               ! gives k, sigmwav, kh, cg, c on nodes
call dhsdxdhsdy(dhsdx, dhsdy)

where(2*hh*kwav<=3000.d0)
       sinh2kh=sinh(min(2*kwav*hh,10.0d0))
elsewhere
       sinh2kh = 3000.d0
endwhere
    
if (ntheta > 1) then
!!! Calculate refraction velocity, no wave current interaction yet, to do
   do k = 1, ndx                   
         do itheta=1,ntheta    
            ctheta(itheta, k) =                                           &
               sigmwav(k)/sinh2kh(k)*(dhsdx(k)*snx(itheta)-dhsdy(k)*csx(itheta))
         enddo
   enddo
   ctheta=sign(1.d0,ctheta)*min(abs(ctheta),.5*pi/Trep)    ! Dano, limit refraction velocity to 0.5*pi/wave period
end if

deallocate(dhsdx, dhsdy, hh, sinh2kh, stat=ierr)
end subroutine xbeach_wave_compute_celerities


!> compute wave boundary conditions
subroutine xbeach_wave_bc()
   use m_flowgeom
   use m_xbeach_data
   use m_flowexternalforcings
   use wave_boundary_main_module
   use m_flowtimes, only: time0, time1, tstop_user
   use m_physcoef, only: rhomean, ag
   use m_sferic, only: pi
   use m_flowparameters, only: epshs
   use m_flow, only:hs, u1, v, plotlin
   use m_alloc
   use m_xbeach_filefunctions
   use wave_boundary_datastore
   use interp
   use m_partitioninfo

   implicit none

    integer, save                                         :: nt
    integer, save                                         :: old
    integer, save                                         :: curline
    integer                                               :: i, itheta, j, E_idx, ier, ier2, ierror 
    double precision                                      :: E1,ei,dum,Hm0, dum1, spreadpar, bcdur, dum2, dthetarad, cgwavin
    double precision, save                                :: bcendtime,bcstarttime
    double precision                                      :: em,tshifted,tnew,fac,hboundary(1)
    double precision, save                                :: Emean,Llong
    double precision                                      :: hh
    character(len=1)                                      :: bline
    character(slen)                                       :: ebcfname,qbcfname,fname
    logical                                               :: startbcf

    double precision, allocatable, save                   :: dist(:), factor(:)
                                                          
    double precision                                      :: E0
    double precision, dimension(nbndw)                    :: qxbc,qybc
    double precision, dimension(nbndw, ntheta)            :: eeout
                                                          
    double precision                                      :: Hbc,Tbc,Dbc
                                                          
    logical                                               :: isRecomputed
                                                          
    integer                                               :: k, kb, ki, Lb, LL, Lw, L, nw, k2
    
    logical, save                                         :: bccreated=.false.
   
   
   ierror = 1
   if (.not. allocated(dist)) allocate(dist(1:ntheta),factor(1:ntheta), e01(1:ntheta))

   eeout = 0d0
   uin = 0d0
   vin = 0d0 
   qxbc = 0d0
   qybc = 0d0
   
!  note: also in xbeach_spectral_wave_init   
   call get_hboundary(hboundary)
   
   waveBoundaryParameters%hboundary=hboundary(1)
   dthetarad = dtheta*pi/180d0
   startbcf=.false.   

if(  .not. (trim(instat).eq.'stat' .or. &
            trim(instat).eq.'bichrom' .or. &
            trim(instat).eq.'ts_1' .or. &
            trim(instat).eq.'ts_2' .or. &
            trim(instat).eq.'stat_table' &
            ))then
    
   ! TODO
   ! This subroutine only needs to be called by boundary
   ! domains.
      if ( nbndw.gt.0 ) then
         call create_incident_waves_surfbeat(nbndw,xbndw,ybndw,&
                                        waveBoundaryParameters%ntheta,waveBoundaryParameters%dtheta,waveBoundaryParameters%theta,time0, &
                                        1,bcfile, &
                                        waveBoundaryParameters%x0,waveBoundaryParameters%y0,waveBoundaryParameters%hboundary, &
                                        waveBoundaryParameters%randomseed, &
                                        eeout,qxbc,qybc, & 
                                        Hbc,Tbc,Dbc,isRecomputed,nspr=nspr)
      end if
   
       
      do i=1,nbndu       !! for Riemann bnd's
         nw = kbndu2kbndw(i)
         if ( nw.gt.0 ) then
            hh = max(hs(kbndw(1,nw)),epshs)
            uin(nw) = qxbc(nw)/hh
            vin(nw) = qybc(nw)/hh
         end if
      end do

      do i=1,nbndw
         zbndw(1:ntheta,i) = eeout(i,1:ntheta)
      end do

   
   
else !! instat = stat, stat_table, ts_1, ts_2, bichrom
   if(.not. bccreated ) then
       call writelog('ls','','Setting up boundary conditions')
       bccreated=.true.
       startbcf=.true.                     ! trigger read from bcf for instat 3,4,5,7
       bcendtime=huge(0.0d0)               ! initial assumption for instat 3,4,5,7
       newstatbc=1

       call get_refpoint(xref0, yref0)

       if (trim(instat)=='ts_1') then
          open( unit=7, file='bc/gen.ezs')
5         continue
          read(7,'(a)',iostat=ier) bline
          if (ier .ne. 0) then
             call report_file_read_error('bc/gen.ezs')
          endif
          if(bline.eq.'*') goto 5
          read(7,*,iostat=ier) nt    ! no of timesteps
          if (ier .ne. 0) then
             call report_file_read_error('bc/gen.ezs')
          endif

          allocate(dataE  (nt))
          allocate(tE     (nt))
          do i=1,nt
                read(7,*,iostat=ier) tE(i),dum,dataE(i)
                if (ier .ne. 0) then
                   call report_file_read_error('bc/gen.ezs')
                endif
          end do
          close(7)
          Emean=sum(dataE)/nt
		  
       elseif (trim(instat)=='ts_2') then
          open( unit=7, file='bc/gen.ezs')
6         continue
          read(7,'(a)',iostat=ier)bline
          if (ier .ne. 0) then
             call report_file_read_error('bc/gen.ezs')
          endif
          if(bline.eq.'*') goto 6
          read(7,*,iostat=ier)nt
          if (ier .ne. 0) then
             call report_file_read_error('bc/gen.ezs')
          endif

          allocate(dataE  (nt))
          allocate(databi (nt))
          allocate(tE     (nt))
          do i=1,nt
                read(7,*,iostat=ier) tE(i),databi(i),dataE(i)
                if (ier .ne. 0) then
                   call report_file_read_error('bc/gen.ezs')
                endif
          end do
          close(7)
          Emean=sum(dataE)/nt
       elseif (trim(instat)=='stat_table') then
          open( unit=7, file=bcfile)
          read(7,*,iostat=ier) Hm0, Trep, dir0, dum1, spreadpar, bcendtime, dum2
          if (ier .ne. 0) then
             call report_file_read_error(bcfile)
          endif
          Hrms = Hm0/sqrt(2.d0)
          m = 2.0d0*spreadpar
          theta0=(1.5d0*pi) - dir0*atan(1.d0)/45.d0
          if (theta0>pi) theta0=theta0-2*pi
          if (theta0<-pi) theta0=theta0+2*pi
          newstatbc=1   

          do itheta=1,ntheta
             sigt(itheta,:) = 2.d0*pi/Trep
          end do
          sigmwav = sum(sigt,1)/ntheta
          call xbeach_dispersion()
       endif
       !
       ! Directional distribution
       !
       dist=(cos(thetabin-theta0))**m
       do i=1,ntheta
          if(cos(thetabin(i)-theta0)<0.d0) then
             dist(i)=0.0d0
          end if
       end do
       if (trim(instat)=='ts_1' .or. trim(instat)=='ts_2') then
          Hrms=sqrt(8d0*Emean/(rhomean*ag))
       endif
       E0=0.125d0*ag*rhomean*Hrms**2

       ! energy density distribution

       if (sum(dist)>0.d0) then
          factor = (dist/sum(dist))/dtheta
       else
          factor=0.d0
       endif
       e01    = factor*E0;                            ! 1:ntheta ding
       e01    = max(e01,0.0d0);

       if ( jampi.eq.0 ) then
          if ( nbndw.gt.0 ) then
             Llong=Tlong*maxval(cgwav(kbndw(1,1:nbndw)))                   !! cg at some boundary point, xbeach_dispersion(). This implies that this value is the same everywhere!!
          else
             Llong = -huge(0d0)
          end if
       else
          if ( nbndw.gt.0 ) then    ! may give different results for parallel runs
             Llong=Tlong*maxval(cgwav(kbndw(1,1:nbndw)))
          else
             Llong = -huge(0d0)
          end if
          call reduce_double_max(Llong)
       end if
       
       call writelog('sl','','Boundary conditions complete, starting computation')
    end if
	

	if (time0 .ge. bcendtime) then  ! Recalculate bcf-file
       if (trim(instat)=='stat_table') then
          call writelog('ls','','Reading new wave conditions')
          read(7,*,iostat=ier) Hm0, Trep, dir0, dum1, spreadpar, bcdur, dum2
          if (ier .ne. 0) then
             call report_file_read_error(bcfile)
          endif
          Hrms = Hm0/sqrt(2.d0)
          taper = 1.d0 
          m = 2.0d0*spreadpar
          bcendtime=bcendtime+bcdur
          theta0=(1.5d0*pi)-dir0*atan(1.d0)/45.d0

          if (theta0>pi) theta0=theta0-2d0*pi
          if (theta0<-pi) theta0=theta0+2d0*pi
          newstatbc=1                    ! modulevar

          do itheta=1,ntheta
             sigt(itheta,:) = 2*pi/Trep
          end do
          sigmwav = sum(sigt,1)/ntheta
          call xbeach_dispersion()

          dist=(cos(thetabin-theta0))**m
          do i=1,ntheta
             if(abs(thetabin(i)-theta0)>pi/2.d0) then
                dist(i)=0
             end if
          end do
          E0=1d0/8d0*rhomean*Hrms**2

          ! energy density distribution

          if (sum(dist)>0.d0) then
             factor = (dist/sum(dist))/dtheta
          else
             factor=0.d0
          endif
          e01    = factor*E0;
          e01    = max(e01,0.0d0);
       elseif (trim(instat)=='reuse') then
          close(71)
          close(72)
          startbcf=.true.
          if (time0 <= (tstop_user-time0)) then
             curline = curline + 1
          end if
       end if
!
    end if
!	
!!> Calculate boundary wave energy bc
	if (trim(instat)=='stat' .or. trim(instat)=='stat_table') then
       do L = 1, nbndw
	      kb = kbndw(1,L)
         zbndw(:,L)=e01*min(time0/taper,1.0d0)
         bi(L) = 0.0d0
       end do
	   
       if (nbndu .gt. 0) then
	       do i=1,nbndw
             uin(kbndw2kbndu(i)) = 0d0
             vin(kbndw2kbndu(i)) = 0d0
          end do
       end if
  
! to check: MPI compliancy - okay for xref0, yref0      
    elseif (trim(instat)=='bichrom') then
       do L = 1, nbndw
         kb = kbndw(1,L)
         zbndw(:,L)=e01*0.5d0 * &
              (1.d0+cos(2*pi*(time0/Tlong-( sin(theta0)*(ybndw(L)-yref0) &
              +cos(theta0)*(xbndw(L) - xref0))/Llong))) * & 
              min(time0/taper,1.d0)
         if (nbndu .gt. 0) then
            em = (sum(0.5d0*e01))*dtheta *min(time0/taper,1.d0)
            ei =  sum(zbndw(:,L), dim=1)*dtheta
            bi(L) = -(2d0*cgwav(kb)/cwav(kb)-0.5d0)*(em-ei)/(cgwav(kb)**2-ag*hs(kb))/rhomean
            uin(kbndw2kbndu(L)) = cgwav(kb)*bi(L)/hs(kb)*cos(theta0)
         end if                                  
       end do
	   
    elseif (trim(instat)=='ts_1') then
       do L = 1, nbndw
	       kb = kbndw(1,L)
          call linear_interp(tE,dataE,nt,time0,E1,E_idx)                                   
          zbndw(:,kb)=e01*E1/max(Emean,0.000001d0)*min(time0/taper,1.d0)
          if (nbndu .gt. 0) then                
             em = Emean *min(time0/taper,1.d0)
             ei = sum(zbndw(:,kb), dim=1)*dtheta
             bi(kb) = -(2*cgwav(kb)/cwav(kb)-0.5d0)*(em-ei)/(cgwav(kb)**2-ag*hs(kb))/rhomean
             uin(kbndw2kbndu(L)) = cgwav(kb)*bi(L)/hs(kb)*cos(theta0) 
          end if                                       
       end do

    elseif (trim(instat)=='ts_2') then
       theta0=(1.5d0*pi)-dir0*atan(1.d0)/45.d0
       do L = 1,nbndw
          kb = kbndw(1,L)
		    ki = kbndw(2,L)

          if (abs(theta0)<1e-3) then                             ! perpendicularly incoming
             call linear_interp(tE,dataE,nt,time0,E1,E_idx)
             call linear_interp(tE,databi,nt,time0,bi(L),E_idx)
          else
              
             if (jampi .eq. 0) then
                 cgwavin = maxval(cgwav(kbndw(1,1:nbndw)))
             else
                if ( nbndw.gt.0 ) then    ! to check for different results for parallel runs
                   cgwavin = maxval(cgwav(kbndw(1,1:nbndw)))
                else
                   cgwavin = -huge(0d0)
                end if
                call reduce_double_max(cgwavin)
             end if
             
             tshifted = max(time0-(ybndw(L)-ybndw(1))*sin(theta0)/cgwav(kbndw(1,1)) &
                  -(xbndw(L)-xbndw(1))*cos(theta0)/cgwavin,0.d0)
             call linear_interp(tE,dataE,nt,tshifted,E1,E_idx)
             call linear_interp(tE,databi,nt,tshifted,bi(L),E_idx)
          endif
        
          zbndw(:,L)=e01*E1/max(Emean,0.000001d0)*min(time0/taper,1.d0)
          if (nbndu .gt. 0) then
             if (freewave == 1) then
                uin(kbndw2kbndu(L)) = sqrt(ag/hs(kb))*bi(L)
             else
                uin(kbndw2kbndu(L)) = cgwav(kb)*bi(L)/hs(kb)*cos(theta0)*min(time0/taper,1.d0)
             end if
          end if
       end do
    end if
end if 

ierror = 0

1234 continue
return
end subroutine xbeach_wave_bc

!> apply computed boundary conditions
subroutine xbeach_apply_wave_bc()
   use m_sferic
   use m_flowgeom
   use m_flowexternalforcings
   use m_xbeach_data
   use m_physcoef
   use m_flow, only:hs, u1, v, plotlin

   implicit none

   integer                             :: k, itheta, kb, ki, L
   integer                             :: i

   
   ! initially: all boundaries have Neumann boundary conditions
   do L=Lnxi+1,Lnx
      kb = ln(1,L)
      ki = ln(2,L)
      do itheta=1,ntheta
         ee1(itheta,kb) = ee1(itheta,ki)
         rr(itheta,kb) = rr(itheta,ki)
      end do
   end do

   do k  = 1,nbndw                                         
   ! overwrite with stored boundary conditions
      kb = kbndw(1,k)
      do itheta = 1,ntheta
         ee1(itheta,kb) = zbndw(itheta,k)
      enddo                                                               
   enddo

   sigt      = twopi/Trep

end subroutine xbeach_apply_wave_bc


subroutine xbeach_wave_breaker_dissipation
use m_flow
use m_flowgeom
use m_sferic, only: pi
use m_physcoef, only: rhog, rhomean
use m_xbeach_data
use m_xerf

implicit none

integer                               :: ierr, i, k
double precision                      :: f
double precision, allocatable         :: hh(:), hr(:), kmr(:), arg(:), kh(:), Hb(:), Qb_advec(:)

allocate(hh(1:ndx), hr(1:ndx), kmr(1:ndx), arg(1:ndx), kh(1:ndx), Hb(1:ndx), Qb_advec(1:ndx), stat=ierr)
hh = 0d0
hr = 0d0
kmr = 0d0
arg = 0d0
kh = 0d0
Hb = 0d0
Qb_advec = 0d0

break = trim(break)
hh = max(hs, epshu)

! no wave current interaction yet! to do..

if (break == 'roelvink1') then                  ! Dissipation according to Roelvink (1993)
   hr  = hh + deltaH*H
   kmr = min(max(kwav, 0.01d0), 100.d0)
   arg = -( H / (gamma*hr              ))**nroelvink
   Qb  = min(1.d0 - exp(max(arg,-100.d0)), 1.d0)
   D = Qb * 2.d0 * alpha * E
   D = D / Trep

elseif (break == 'baldock') then                ! Dissipation according to Baldock et al. (1998), only in stationary mode
                                                 ! Not implemented for now

   f = 1.d0 / Trep
   kh  = kwav * hs

   H   = sqrt(8.d0/rhomean/ag*sum(ee1,dim=1)*dtheta)
   Hb  = tanh(gamma*kh/0.88d0)*(0.88d0/kwav)
   R   = Hb/max(H,0.00001d0)

   Qb   = exp(-R**2)
   D   = 0.25d0 * alpha * f * rhomean * ag * (Hb**2+H**2) * Qb

elseif (break == 'roelvink2') then
    hr  = hh + deltaH*H
    kmr = min(max(kwav, 0.01d0), 100.d0)
    arg = -( H / (gamma*hr              ))**nroelvink
    Qb  = min(1.d0 - exp(max(arg,-100.d0)), 1.d0)
    D = Qb * 2.d0 * alpha * E
    D = D /Trep * H / hh

elseif (trim(break) == 'roelvink_daly') then

    call advec_upw_bulk(Qb,cwav,Qb_advec) ! first order upwind, with mean direction
    do k = 1, ndxi
        Qb(k) = Qb(k) - dtmaxwav * Qb_advec(k) * bai(k)
    end do
    hr  = hh + deltaH*H
    kmr = min(max(kwav, 0.01d0), 100.d0)
    where (H > gamma * hr)   Qb = 1.d0
    where (H < gamma2 * hr)  Qb = 0.d0
    Qb = max(Qb, 0.d0) 
    D = Qb * 2.d0 * alpha * E
    D = D /Trep * H / hh

elseif (break == 'janssen') then                 ! Dissipation according to Janssen and Battjes (2007)
    f = 1.d0 / Trep
    kh  = kwav * (hh + deltaH*H)
    Hb  = tanh(gamma*kh/0.88d0)*(0.88d0/kwav)
    R   = Hb/max(H,0.00001d0)

    Qb   = 1 + 4/(3*sqrt(pi)) * (R**3 + 3/2*R) * exp(-R**2) - xerf(R)
    D   = 3*sqrt(pi)/16      * alpha * f * rhomean * ag * H**3/hh * Qb    ! alpha is B from the paper, same as Roelvink 1993
endif

deallocate(hh, hr, kmr, arg, kh, Hb, Qb_advec, stat = ierr)

end subroutine xbeach_wave_breaker_dissipation


subroutine advec_horz(quant, veloc, advec)
use m_sferic
use m_physcoef
use m_flowgeom
use m_flow
use m_xbeach_data !, only: snx, csx, ntheta, dtmaxwav, limtypw

implicit none

integer                                        :: L, k, k1, k2, itheta, ku, kl2s, kl2, kl1, kd, is, ip
double precision                               :: velocL, qds, qst, half, fluxvel1, waku, sl1, sl2, sl3
double precision                               :: cf, ds2, ds1, ds, cwuL
double precision, intent(in), dimension(ndx)   :: veloc
double precision, intent(in), dimension(ntheta,ndx) :: quant
double precision, intent(out), dimension(ntheta, ndx)  :: advec
double precision, external                     :: dslim

advec = 0d0
do L  = 1,lnx                                                              ! upwind (supq) + limited high order (dsq), loop over link
        k1  = ln(1,L) ; k2 = ln(2,L)                                       ! linker en rechtercelnr geassocieerd aan de links
        velocL = acL(L)*veloc(k1) + (1d0-acL(L))*veloc(k2)
        
        do itheta = 1,ntheta
            cwuL    = velocL*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
                                                                           ! inproduct cgx*csu+cgy*snu

            if (cwuL > 0) then                                              !   ->      ds1   ds2
                k = k1 ; kd = k2 ; is =  1 ; half = 1d0 - acl(L) ; ip = 0   !   ->   ku     k     kd
            else                                                            !   <-      ds2   ds1
                k = k2 ; kd = k1 ; is = -1 ; half = acl(L)       ; ip = 3   !   <-   kd     k     ku
            endif                                                           ! acL = linkse dx fractie van afstand tussen flownodes (slide 83)

            fluxvel1  = is*cwuL*wu(L)                                       ! snelheidsbijdrage linkse cel
            qst = fluxvel1*quant(itheta,k)                                  ! cg*E voor link L, sector itheta
            advec(itheta,kd) = advec(itheta,kd) - qst                       ! downwind cel krijgt bijdrage
            advec(itheta,k)  = advec(itheta,k)  + qst                       ! centrale cel verliest bijdrage

            if (limtypw > 0 ) then                                          ! hogere orde, tijdstapafhankelijk!
                ku  = klnup(1+ip,L)                                         ! pointer upwind cel horende bij link L

                if (ku .ne. 0 ) then
                    kl2s = klnup(2+ip,L) ; kl2 = iabs(kl2s)                 ! 

                    if (ku < 0) then
                        waku = quant(itheta,abs(ku))                        ! pointer naar cel negatief?
                    else
                        kl1  = ku
                        sl1  = slnup(1+ip,L) ; sl2  = slnup(2+ip,L)             ! link upwind cell weight
                        waku  = quant(itheta,kl1)*sl1 + quant(itheta,kl2)*sl2   ! gewogen gemiddelde upwind waarden
                    endif  

                    sl3 = slnup(3+ip,L)
                    cf  =  dtmaxwav*abs(cwuL)*dxi(L)                  
                    cf  =  half*max( 0d0,1d0-cf )                     ! cf  =  half* (1d0-cf)
                    ds2  =  quant(itheta,kd) - quant(itheta,k)        ! ds1 = voorlopende slope, ds2 = eigen slope
                    ds1  = (quant(itheta,k)  - waku )*sl3

                    if (abs(ds2)  > eps10 .and. abs(ds1) > eps10) then
                        ds  =  cf*dslim(ds1, ds2, limtypw)                  ! reconstructie van totale slope volgens 1 van de 4 schema's

                        if (abs(ds) > eps10) then                           ! als celgemiddelde niet volstaat
                            qds      =  ds*fluxvel1                               ! slope * linkse celbijdrage
                            advec(itheta,kd) =  advec(itheta,kd) - qds        ! downwind cel krijgt bijdrage
                            advec(itheta,k ) =  advec(itheta,k ) + qds        ! cel verliest bijdrage
                        endif
                    endif
                endif
            endif
        enddo ! directions
    enddo ! links

end subroutine advec_horz





subroutine advec_upw_bulk(quant, veloc, advec)
use m_sferic
use m_physcoef
use m_flowgeom
use m_flow
use m_xbeach_data !, only: snx, csx, ntheta, dtmaxwav, thetamean

implicit none

integer                                        :: L, k, k1, k2, itheta, kd, is, ip
double precision                               :: velocL, qst, half, fluxvel
double precision                               :: cwuL
double precision, intent(in), dimension(ndx)   :: veloc
double precision, intent(in), dimension(ndx) :: quant
double precision, intent(out), dimension(ndx)  :: advec

advec = 0d0
do L  = 1,lnx                                                              ! upwind (supq) + limited high order (dsq), loop over link
   k1  = ln(1,L) ; k2 = ln(2,L)                                            ! linker en rechtercelnr geassocieerd aan de links
   velocL = acL(L)*veloc(k1) + (1d0-acL(L))*veloc(k2)

   cwuL    = velocL*( csu(L)*dcos(thetamean(k1)) + snu(L)*dsin(thetamean(k1)))    ! met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
                                                                         ! inproduct cgx*csu+cgy*snu

   if (cwuL > 0) then                                              !   ->      ds1   ds2
      k = k1 ; kd = k2 ; is =  1 ; half = 1d0 - acl(L) ; ip = 0    !   ->   ku     k     kd
   else                                                            !   <-      ds2   ds1
      k = k2 ; kd = k1 ; is = -1 ; half = acl(L)       ; ip = 3    !   <-   kd     k     ku
   endif                                                           ! acL = linkse dx fractie van afstand tussen flownodes (slide 83)

   fluxvel  = is*cwuL*wu(L)                                       ! snelheidsbijdrage linkse cel
   qst = fluxvel*quant(k)                                  ! cg*E voor link L, sector itheta
   advec(kd) = advec(kd) - qst                       ! downwind cel krijgt bijdrage
   advec(k)  = advec(k)  + qst                       ! centrale cel verliest bijdrage
enddo

end subroutine advec_upw_bulk






subroutine advec_dir(quan, veloc, advec)
use m_sferic
use m_physcoef
use m_flowgeom
use m_flow
use m_xbeach_data !, only: ntheta, dtheta

implicit none

integer                                             :: k, itheta
double precision                                    :: ctheta_between, eeup
double precision, dimension(ntheta)                 :: fluxtheta
double precision, dimension(ntheta,ndx), intent(in) :: veloc, quan
double precision, dimension(ntheta,ndx), intent(out):: advec

advec = 0d0
if (ntheta > 1) then
        do k = 1, ndx
            do itheta = 2, ntheta - 2    
                ctheta_between = 0.5d0 * veloc(itheta+1,k)
                if (ctheta_between>0) then
                    eeup=1.5d0*quan(itheta, k)-.5*quan(itheta-1, k)
                    if (eeup<0.d0) then
                       eeup=quan(itheta, k)
                    endif
                    fluxtheta(itheta)=eeup*ctheta_between
                else
                    eeup=1.5d0*quan(itheta+1, k)-.5*quan(itheta+2, k)
                    if (eeup<0.d0) then
                       eeup=quan(itheta+1, k)
                    endif
                    fluxtheta(itheta)=eeup*ctheta_between
                endif
            enddo
                
            itheta=1                                                    ! only compute for itheta==1
            ctheta_between=.5*(veloc(itheta+1, k)+veloc(itheta, k))
            if (ctheta_between>0) then
                fluxtheta(itheta)=quan(itheta,k)*ctheta_between
            else
                eeup=1.5d0*quan(itheta+1, k)-.5*quan(itheta+2, k)
                if (eeup<0.d0) eeup=quan(itheta+1, k)
                fluxtheta(itheta)=eeup*ctheta_between
            endif
            
            itheta=ntheta-1                                              ! only compute for itheta==ntheta-1
            ctheta_between=.5*(veloc(itheta+1, k)+veloc(itheta, k))
            if (ctheta_between>0) then
                eeup=1.5d0*quan(itheta, k)-.5*quan(itheta-1, k)
                if (eeup<0.d0) eeup=quan(itheta, k)
                fluxtheta(itheta)=eeup*ctheta_between
            else
                eeup=quan(itheta+1, k)
                fluxtheta(itheta)=eeup*ctheta_between
            endif
            
            advec(1, k)=(fluxtheta(1)-0.d0)/dtheta                 ! No flux across lower boundary theta grid
            do itheta=2,ntheta-1
                advec(itheta, k)=(fluxtheta(itheta)-fluxtheta(itheta-1))/dtheta
            enddo
            advec(ntheta, k)=(0.d0-fluxtheta(ntheta-1))/dtheta    ! No flux across upper boundary theta grid
        enddo
    endif

end subroutine advec_dir


!> reset XBeach wave data
subroutine xbeach_reset()
   use m_xbeach_readkey ! for reset_paramfile
   implicit none

   call reset_paramfile()
   
   

end subroutine xbeach_reset


!> compute flow boundary conditions
subroutine xbeach_flow_bc()
   use m_flowexternalforcings, only: nbndu
   implicit none

   integer                                     :: ierror

   ierror = 1

   if ( nbndu.lt.1 ) goto 1234

   call xbeach_riemann_velocity_bc()

   ierror = 0
1234 continue

   return
end subroutine xbeach_flow_bc


!> compute initial water-level normal gradient for Riemann boundary condition
subroutine xbeach_riemann_velocity_bc()
      use m_sferic
      use m_xbeach_data, only: Fx, Fy, cgwav, freewave, ARC, order, uin, vin, kbndu2kbndw, s1initial, ust
      use m_flowgeom
      use m_flow, only: u1, v, s1, hs, hu, plotlin, cfuhi
      use m_flowtimes, only: dts, time0
      use network_data, only: xk, yk
      use m_physcoef, only: ag, rhomean
      use m_flowexternalforcings
      use m_alloc
      use unstruc_messages
      use m_xbeach_errorhandling
      use m_missing
      implicit none
      
      integer :: ierror

      integer, parameter                  :: MAXLNX=100
      double precision, dimension(MAXLNX) :: wgradx, wgrady
      
      double precision, dimension(:), allocatable :: uave, vave, dlength
      integer                                     :: numbnd, maxnumbnds
      
      double precision :: beta, betanp1, dbetadt
      double precision :: dbetadx, dbetady, dxx, dyy
      double precision :: ux, uy, duxdx, duxdy, duydx, duydy
      double precision :: dbetadn, dbetads, dvds, dhdn
      double precision :: c, un, Fn, Ftau
      double precision :: thetai, ur, umean, vmean, vert
      double precision :: alpha2, alphanew
      double precision :: cgbound
      double precision :: dumx, dumy, dum, cg0
      double precision :: uin_loc, vin_loc, hum
      double precision :: factime
      
      integer :: n, Lb, L, kb, ki, k1, k2, k3, k4, i, jj
      integer :: NLNX, nw
      
      double precision, external :: getdx, getdy
      
      ierror = 1
      
!     this subroutine will alter zbndu, store zbndu and restore prior to flow_set external forcings on boundaries
      zbndu_store = zbndu
      
      call realloc(plotlin, Lnx, keepExisting=.false., fill=DMISS)

      ! Dirty trick to be able to use Riemann without jawave = 4
      ! allocate Fx, Fy, uin, vin and init to 0
      !if (.not. allocated(Fx)) then 
      !   call realloc(Fx, Lnx, keepExisting=.false., fill=0d0)
      !   call realloc(Fy, Lnx, keepExisting=.false., fill=0d0)
      !   call realloc(uin, nbndw, keepExisting=.false., fill=0d0)
      !   call realloc(vin, nbndw, keepExisting=.false., fill=0d0)
      !end if     
     
      dbetadt = 0d0
      
!     debug
      factime = 0.0166666666666666666666666666666667d0*dts

      
!     compute boundary-averaged velocities
      maxnumbnds=100
      allocate(uave(maxnumbnds),vave(maxnumbnds),dlength(maxnumbnds))
      uave = 0d0
      vave = 0d0
      dlength = 0d0
      numbnd = 0
      do n=1,nbndu
         if ( kbndu(4,n).eq. 5 ) then
            Lb = kbndu(3,n)
            numbnd = kbndu(5,n)
            if ( numbnd.gt.maxnumbnds ) then
               maxnumbnds = max(int(1.2d0*numbnd),maxnumbnds+1)
               call realloc(uave, maxnumbnds, keepExisting=.true., fill=0d0)
               call realloc(vave, maxnumbnds, keepExisting=.true., fill=0d0)
               call realloc(dlength, maxnumbnds, keepExisting=.true., fill=0d0)
            end if
            Lb = kbndu(3,n)
            uave(numbnd) = uave(numbnd) + wu(Lb)*u1(Lb)
            vave(numbnd) = vave(numbnd) + wu(Lb)*v(Lb)
            dlength(numbnd) = dlength(numbnd) + wu(Lb)
         end if
      end do

      if (numbnd .gt. 0) then
         uave(1:numbnd) = uave(1:numbnd)/max(dlength(1:numbnd),1d-16)
         vave(1:numbnd) = vave(1:numbnd)/max(dlength(1:numbnd),1d-16)
      end if
      
      do n=1,nbndu

         if ( kbndu(4,n).eq. 5 ) then  ! Riemann boundary
            kb = kbndu(1,n)
            ki = kbndu(2,n)
            Lb = kbndu(3,n)
            dbetadx = 0d0
            dbetady = 0d0
            duxdx   = 0d0
            duxdy   = 0d0
            duydx   = 0d0
            duydy   = 0d0

            NLNX = nd(ki)%lnx

            nw = kbndu2kbndw(n)  ! wave-boundary index
            
            if ( nw.gt.0 ) then  ! for now set to zero, still buggy
               uin_loc = uin(nw)*csu(Lb) + vin(nw)*snu(Lb)
               vin_loc = vin(nw)*csu(Lb) - uin(nw)*snu(Lb)
            else
               uin_loc = 0d0
               vin_loc = 0d0
            end if

!           check array size
            if ( NLNX.gt.MAXLNX ) then
                call mess(LEVEL_ERROR, 'xbeach_riemann_velocity_bc: array size error')
                call xbeach_errorhandler
            end if
            
!!           begin debug
!            zbndu(n) = uin(nw)
!            cycle
!!           end debug

            do i=1,nd(ki)%lnx
               L = iabs(nd(ki)%ln(i))
               
               un = (u1(L)*csu(L) - v(L)*snu(L))*csu(Lb) + (u1(L)*snu(L) + v(L)*csu(L))*snu(Lb)
               beta = un - 2d0*sqrt(ag*hu(L))
               
!               plotlin(L) = beta
      
               k1 = ln(1,L)
               k2 = ln(2,L)
               k3 = lncn(1,L)
               k4 = lncn(2,L)
      
               dxx = getdx(xk(k3),yk(k3),xk(k4),yk(k4))
               dyy = getdy(xk(k3),yk(k3),xk(k4),yk(k4))
      
               if ( k1.ne.ki ) then ! 1-2-3-4 notation (inward positive)
                  dxx = -dxx
                  dyy = -dyy
               end if

!              remember weights in gradient operator for later use
               wgradx(i) =  dyy
               wgrady(i) = -dxx
      
               ux = u1(L)*csu(L) - v(L)*snu(L)
               uy = u1(L)*snu(L) + v(L)*csu(L)
      
               dbetadx = dbetadx + beta*dyy
               dbetady = dbetady - beta*dxx
      
               duxdx    = duxdx    +   ux*dyy
               duxdy    = duxdy    -   ux*dxx
      
               duydx    = duydx    +   uy*dyy
               duydy    = duydy    -   uy*dxx
            end do
            wgradx(1:NLNX) = wgradx(1:NLNX) * bai(ki)
            wgrady(1:NLNX) = wgrady(1:NLNX) * bai(ki)

            dbetadx = dbetadx * bai(ki)
            dbetady = dbetady * bai(ki)
      
            duxdx    = duxdx    * bai(ki)
            duxdy    = duxdy    * bai(ki)
      
            duydx    = duydx    * bai(ki)
            duydy    = duydy    * bai(ki)
      
            dbetadn =  csu(Lb) * dbetadx + snu(Lb) * dbetady ! 1-2 direction
            dbetads = -snu(Lb) * dbetadx + csu(Lb) * dbetady ! 3-4 direction
      
            dvds    = -snu(Lb) * (-snu(Lb) * duxdx   + csu(Lb) * duxdy) +  &
                       csu(Lb) * (-snu(Lb) * duydx   + csu(Lb) * duydy)
      
            !dhdn    = ( s1initial(ki)-bl(ki)-(s1initial(kb)-bl(kb)) ) * dxi(Lb)   ! 1-2 direction (inward positive)
            dhdn    = ( zbndu(n)-bl(ki)-(zbndu(n)-bl(kb)) ) * dxi(Lb)   ! 1-2 direction (inward positive)
      
            Fn      = csu(Lb) * Fx(Lb) + snu(Lb) * Fy(Lb)   ! 1-2 direction (inward positive)
!           compute bed friction
            Ftau    =  cfuhi(Lb) * sqrt(u1(Lb)**2+v(Lb)**2) * ( u1(Lb)-ust(Lb) )  ! formally, not exactly correct, since we also need u1L (see furu)
      
            c = sqrt(ag*hu(Lb))
      
            if ( abs(hu(Lb)).lt.1d-6 ) then
               goto 1234
            end if

            dbetadt = - (u1(Lb)-c)*dbetadn - v(Lb)*dbetads + c*dvds + ag*dhdn + Fn/(rhomean*hu(Lb)) - Ftau
            beta = u1(Lb) - 2d0*sqrt(ag*hu(Lb))

            !thetai = atan2(uin_loc*csu(Lb) + vin_loc * snu(Lb),-uin_loc*snu(Lb) + vin_loc * csu(Lb))
            thetai = atan2(vin_loc, uin_loc)   ! atan2(y,x)
            
            umean = 0.d0     ! only if tide specified as water level
            vmean = 0.d0
            
            betanp1   = beta + dbetadt*dts
            alpha2 = -(theta0) 
            alphanew  = 0.d0

            !dum = 0.1d0*cos(2*pi*time0/500d0)
            dum = zbndu(n)

            !cg0 = dsqrt(ag*max(0.5d0*(s1initial(kb)-bl(kb)+s1initial(ki)-bl(ki)),0d0))
            !cg0 = dsqrt(ag*max(0.5d0*(s1initial(kb)-bl(kb)+s1initial(ki)-bl(ki)) + dum,0d0))
            cg0 = dsqrt(ag*max(0.5d0*(zbndu(n)-bl(kb)+zbndu(n)-bl(ki)),0d0))

            umean = sqrt(ag/hu(Lb))*(dum-0.5d0*(s1initial(kb)+s1initial(ki)))
            !numbnd = kbndu(5,n)
            !umean = factime*uave(numbnd) + (1d0-factime)*u1(Lb)
            !vmean = factime*vave(numbnd) + (1d0-factime)*v(Lb)
            !
             do jj=1,50
            
               if (freewave==1) then    ! assuming incoming long wave propagates at sqrt(g*h) (free wave)
                  ur = dcos(alpha2)/(dcos(alpha2)+1.d0)  &
                       *(betanp1-umean+2.d0*cg0 &
                       -uin_loc*(dcos(thetai)-1.d0)/dcos(thetai))
               else                     ! assuming incoming long wave propagates at group velocity (bound wave)
                  cgbound = 0.5d0*(cgwav(kb)+cgwav(ki))
                  dum = uin_loc*(cgbound*dcos(thetai)-cg0)/ (cgbound*dcos(thetai))
                  !dum = 0d0
                  !if ( uin_loc.ne.0d0 ) then
                  !   dum = uin_loc*(cgbound*dcos(thetai)-cg0)/ (cgbound*dcos(thetai))
                  !end if
                  ur = dcos(alpha2)/(dcos(alpha2)+1.d0)  &
                       *(betanp1-umean+2.d0*cg0 - dum)
               endif

               vert = v(Lb) - vmean - vin_loc ! tangential component along cell face
               !vert = 0d0
               alphanew = atan2(vert,(ur+1.d-16))               
               if (alphanew .gt. (pi*0.5d0))  alphanew = alphanew-pi
               if (alphanew .le. (-pi*0.5d0)) alphanew = alphanew+pi
               if (dabs(alphanew - alpha2).lt.0.001d0) exit
               alpha2 = alphanew
            end do !! loopje voor hoek
            
          if( alphanew.ne.0d0 .or. jj.gt.10) then
             continue
          end if
                          
          plotlin(Lb) = thetai
       
          if (ARC==0) then
             u1(Lb)= (order-1d0)*uin_loc    ! face normal velocity
             s1(kb) = s1(ki)
          else
             zbndu(n) = (order-1.d0)*uin_loc + ur + umean
             !u1(Lb) = (order-1.d0)*(uin_loc*csu(Lb) + vin_loc * snu(Lb)) + ur + umean

             s1(kb) = s1(ki)

             if ( .false.) then
!            compute gradient for linear extrapolation
                dumx = 0d0
                dumy = 0d0
                do i=1,NLNX
                   L = iabs(nd(ki)%ln(i))
                   k1 = ln(1,L)
                   k2 = ln(2,L)
                  
                   un = (u1(L)*csu(L) - v(L)*snu(L))*csu(Lb) + (u1(L)*snu(L) + v(L)*csu(L))*snu(Lb)
                   if ( L.eq.Lb ) then
                      beta = betanp1
                   else
                      beta = un - 2d0*sqrt(ag*hu(L))
                   end if
                
                   dumx = dumx + wgradx(i) * (0.25d0*(beta-un)**2 / ag + 0.5d0*(bl(k1)+bl(k2)))
                   dumy = dumy + wgrady(i) * (0.25d0*(beta-un)**2 / ag + 0.5d0*(bl(k1)+bl(k2)))
                end do
                
                
                dxx = getdx(xu(Lb),yu(Lb),xz(ki),yz(ki))
                dyy = getdy(xu(Lb),yu(Lb),xz(ki),yz(ki))
                
                s1(kb) = 0.25d0*(betanp1-u1(Lb))**2 / ag + 0.5d0*(bl(kb)+bl(ki)) + dumx * dxx + dumy * dyy

             end if
          end if 

       end if   ! riemannpuntje
    end do ! loop snelheidslinks

   ierror = 0
1234 continue

   if ( allocated(uave) ) deallocate(uave)
   if ( allocated(vave) ) deallocate(vave)
   if ( allocated(dlength) ) deallocate(dlength)

   return
end subroutine xbeach_riemann_velocity_bc

!> initialize wave spectra
subroutine xbeach_spectral_wave_init()
   use m_xbeach_filefunctions
   use wave_boundary_datastore
   use m_xbeach_data
   use m_flowexternalforcings
   use m_flowgeom
   use m_xbeach_errorhandling
   use m_polygon
   use m_missing
   use m_sferic, only:twopi
   use timespace_triangle
   use m_flowparameters, only: epshs
   use m_flow, only: hs
   use m_flowtimes, only: time0
   use m_partitioninfo
   implicit none
   
   integer                                   :: fid,err
   integer                                   :: i, itheta
   integer,dimension(1)                      :: minlocation
   character(slen)                           :: testline
   integer,         dimension(:),allocatable :: iperm, kpl, kL, kR, kLspec
   double precision,dimension(:),allocatable :: drL,        wL, wR, wLspec
!   double precision, allocatable             :: hh(:)
   double precision                          :: mindistr
   double precision, dimension(1)            :: hboundary
   double precision                          :: fac
   double precision                          :: xa, ya, xb, yb, xx, yy
   double precision                          :: disall, dis, xn, yn, rL, darc
   
   integer                                   :: ibnd, minp, ip, ja
   integer                                   :: k, L, j, k2
   integer                                   :: ierror
   
   logical, save                             :: bccreated = .false.
   
   double precision, external                :: dbdistance
   
   ierror = 1
   
!   allocate(hh(Ndx))

!   ! TODO
!   ! randomseed should be set by internal clock time for true
!   ! random series. Integer randomseed needs to be distributed
!   ! across all domains.
   if (.not. bccreated) then
      randomseed=123
	   !call SYSTEM_CLOCK(randomseed)
    !  call random_seed(randomseed)
   end if
   
   call get_hboundary(hboundary)
  
   call get_refpoint(xref0, yref0)
   
   waveBoundaryParameters%masterFileName = bcfile
   waveBoundaryParameters%np = nbndw
   waveBoundaryParameters%ntheta = ntheta
   waveBoundaryParameters%dtheta = dtheta
   waveBoundaryParameters%x0 = xref0
   waveBoundaryParameters%y0 = yref0
   waveBoundaryParameters%hboundary = hboundary(1)

   if(allocated(waveBoundaryParameters%xb)) deallocate(waveBoundaryParameters%xb)
   if(allocated(waveBoundaryParameters%yb)) deallocate(waveBoundaryParameters%yb)
   if(allocated(waveBoundaryParameters%theta)) deallocate(waveBoundaryParameters%theta)

   ! Now allocate arrays to the correct size and set values
   allocate(waveBoundaryParameters%xb(waveBoundaryParameters%np))
   allocate(waveBoundaryParameters%yb(waveBoundaryParameters%np))
   allocate(waveBoundaryParameters%theta(waveBoundaryParameters%ntheta))
   if ( nbndw.gt.0 ) then
      waveBoundaryParameters%xb = xbndw
      waveBoundaryParameters%yb = ybndw
   end if
   waveBoundaryParameters%theta = thetabin

   ! Ensure all theta directions are between 0 and 2pi, required for some trig. on some compilers
   do itheta=1,ntheta
      waveBoundaryParameters%theta(itheta) = mod(waveBoundaryParameters%theta(itheta)+twopi,8.d0*atan(1.d0))
   enddo

   ! Allocate space for the random seed. This seed is set to 40 integers and
   ! should be identical on all processes
   waveBoundaryParameters%randomseed = randomseed

   if (.not.waveBoundaryAdministration%initialized) then
      
      call writelog('l','','--------------------------------')
      call writelog('l','','Initializing spectral wave boundary conditions ')
      ! Initialize that wave boundary conditions need to be calculated (first time at least)
      ! Stored and defined in spectral_wave_bc_module
      waveSpectrumAdministration%repeatwbc = .false.
      ! Initialize the number of times wave boundary conditions have been generated.
      ! Stored and defined in spectral_wave_bc_module
      waveSpectrumAdministration%bccount  = 0
      ! Initialize bcendtime to zero.
      ! Stored and defined in spectral_wave_bc_module
      waveSpectrumAdministration%spectrumendtime = 0.d0
      ! Initialise lastwaveheight to zero
      ! Stored and defined in wave_boundary_main_module
      allocate(waveSpectrumAdministration%lastwaveelevation(waveBoundaryParameters%np,&
                                                          waveBoundaryParameters%ntheta))
      
      
      if (nspectrumloc<1) then
         call writelog('ewls','','number of boundary spectra (''nspectrumloc'') may not be less than 1')
         call xbeach_errorhandler()
      endif
      
   ! open location list file

      call oldfil(fid,bcfile)
      !fid = create_new_fid()
      !open(fid,file=bcfile,status='old',form='formatted')
      ! check for LOCLIST
      read(fid,*)testline
      if (trim(testline)=='LOCLIST') then
         waveSpectrumAdministration%nspectra = nspectrumloc
         allocate(wavespectrumadministration%bcfiles(nspectrumloc))     ! stored and defined in spectral_wave_bc_module
         allocate(wavespectrumadministration%xspec(nspectrumloc))
         allocate(wavespectrumadministration%yspec(nspectrumloc))
         allocate(wavespectrumadministration%kL(nbndw))
         allocate(wavespectrumadministration%wL(nbndw))
         allocate(wavespectrumadministration%kR(nbndw))
         allocate(wavespectrumadministration%wR(nbndw))
         do i=1,nspectrumloc
            ! read x,y and file name per location
            read(fid,*,IOSTAT=err)wavespectrumadministration%xspec(i),wavespectrumadministration%yspec(i),wavespectrumadministration%bcfiles(i)%fname
            wavespectrumadministration%bcfiles(i)%listline = 0
            if (err /= 0) then
               ! something has gone wrong during the read of this file
               call writelog('lswe','a,i0,a,a)','error reading line ',i+1,' of file ',bcfile)
               call writelog('lswe','','check file for format errors and ensure the number of  ',&
                    'lines is equal to nspectrumloc')
               call xbeach_errorhandler()
            endif
         enddo
         
   !     sort spectra in increasing arclength along the wave-energy boundary
         allocate(drL(nspectrumloc),iperm(nspectrumloc),kpl(nspectrumloc))
         allocate(kL(nbndw),kR(nbndw),wL(nbndw),wR(nbndw))
         allocate(kLspec(nspectrumloc),wLspec(nspectrumloc))
         do ibnd=1,1   ! nwbnd
            call oldfil(minp, fnamwbnd(ibnd))
            call delpol()
            call reapol(minp,0)
            
         
   !        get weights to polyline at flownodes
            do i=1,nbndw
   !           get weight to the polyline nodes
               L = kbndw(3,i)
               call polyindexweight(xu(L),yu(L),xy2bndw(1,i),xy2bndw(2,i),   &
                                   xpl, ypl, (/ (1, k=1,NPL) /), NPL, kL(i), wL(i), kR(i), wR(i))
            end do
            
            
   !        project spectrum locations on polyline
            do i=1,nspectrumloc
   !	        find nearest point on polyline
               disall = 1d99
               darc = 0d0
               do ip=1,NPL-1
                  xa = XPL(ip)
                  ya = YPL(ip)
   			      xb = XPL(ip+1)
                  yb = YPL(ip+1)
                  xx = wavespectrumadministration%xspec(i)
                  yy = wavespectrumadministration%yspec(i)
                  if ( xa.ne.dmiss .and. xb.ne.dmiss ) then
                     call dlinedis3(xx,yy,xa,ya,xb,yb,ja,dis,xn,yn,rL)
                     if ( dis.lt.disall ) then
                        disall = dis
                        drL(i) = darc + dbdistance(xa,ya,xn,yn)
                        kLspec(i) = ip
                        wLspec(i) = rL
                     end if
                  end if
                  
                  darc = darc + dbdistance(xa,ya,xb,yb)
               end do
            end do
         end do
         call indexx(nspectrumloc,drL,iperm)
         
   !     compute weights from mesh to spectrum locations
         do i=1,nbndw
   !        determine arc length along polyline
            darc = 0d0
            do ip=1,kL(i)-1
                xa = XPL(ip)
                ya = YPL(ip)
   			 xb = XPL(ip+1)
                yb = YPL(ip+1)
                darc = darc + dbdistance(xa,ya,xb,yb)
            end do
            ip = kL(i)
            xa = XPL(ip)
            ya = YPL(ip)
   		   xb = XPL(ip+1)
            yb = YPL(ip+1)
            darc = darc + wR(i)*dbdistance(xa,ya,xb,yb)
            
   !        determine weights from spectrum locations to boundary links
            j = 1
            do while ( drL(iperm(j)).lt.darc .and. j.lt.nspectrumloc )
               j=j+1   ! j is right pointer
         end do
            if ( j.gt.1 ) then
                j=j-1  ! j is left pointer
            end if
         
            wavespectrumadministration%kL(i) = iperm(j)
            wavespectrumadministration%kR(i) = iperm(j)
            wavespectrumadministration%wL(i) = 1d0
            wavespectrumadministration%wR(i) = 0d0
            if ( j+1.le.nspectrumloc ) then
                wavespectrumadministration%kR(i) = iperm(j+1)
                wavespectrumadministration%wL(i) = min(max( 1d0-(darc-drL(iperm(j))) / (drL(iperm(j+1))-drL(iperm(j))), 0d0), 1d0)
                wavespectrumadministration%wR(i) = 1d0 - wavespectrumadministration%wL(i)
            end if
            
         end do
   
   !!     BEGIN DEBUG
   !      do i=1,nbndw
   !         kL(i) = wavespectrumadministration%kL(i)
   !         kR(i) = wavespectrumadministration%kR(i)
   !         wL(i) = wavespectrumadministration%wL(i)
   !         wR(i) = wavespectrumadministration%wR(i)
   !         zbndw(1,i) = wL(i)*wavespectrumadministration%yspec(kL(i)) + &
   !                      wR(i)*wavespectrumadministration%yspec(kR(i))
   !         
   !         write(6,"(2F15.5)") zbndw(1,i), ybndw(i)
   !      end do
   !!     END DEBUG
         
         deallocate(drL,iperm,kpl)
         deallocate(kL,kR,wL,wR)
         deallocate(kLspec,wLspec)
   
       else
         if (nspectrumloc==1) then
            allocate(waveSpectrumAdministration%bcfiles(nspectrumloc))
            allocate(waveSpectrumAdministration%xspec(nspectrumloc))
            allocate(waveSpectrumAdministration%yspec(nspectrumloc))
            waveSpectrumAdministration%bcfiles(1)%listline = 0
            waveSpectrumAdministration%xspec = xref0
            waveSpectrumAdministration%yspec = yref0
            waveSpectrumAdministration%nspectra = nspectrumloc
            waveSpectrumAdministration%bcfiles(1)%fname = bcfile
            waveSpectrumAdministration%bcfiles(1)%listline = 0     ! for files that have multiple lines, set listline to 0
            allocate(wavespectrumadministration%kL(nbndw))
            allocate(wavespectrumadministration%wL(nbndw))
            allocate(wavespectrumadministration%kR(nbndw))
            allocate(wavespectrumadministration%wR(nbndw))
            wavespectrumadministration%kL = 1
            wavespectrumadministration%wL = 1d0
            wavespectrumadministration%kR = 1
            wavespectrumadministration%wR = 0d0
         else
            call writelog('ewls','','if nspectrumloc>1 then bcfile should contain spectra locations with LOCLIST header')
            close(fid)
            call xbeach_errorhandler()
      endif
      endif
   
      waveBoundaryAdministration%initialized = .true.

      close(fid)
   
   end if

   ! Set time to recompute new boundary condition time series to 
   ! now so boundary conditions are generated in first time step
   waveBoundaryAdministration%startComputeNewSeries = time0

   call writelog('l','','--------------------------------')
   
   ierror = 0
1234 continue
     
   return
end subroutine xbeach_spectral_wave_init


!> get reference point for wave energy bc
subroutine get_refpoint(xref0, yref0)
   use m_flowexternalforcings
   use m_partitioninfo
   implicit none
   
   double precision, intent(out) :: xref0, yref0
   
   xref0 = huge(0d0)
   yref0 = huge(0d0)
   if ( nbndw.gt.0 ) then
      xref0 = minval(xbndw(1:nbndw))
      yref0 = minval(ybndw(1:nbndw))
   end if
   if ( jampi.eq.1 ) then
      call reduce_double_min(xref0)
      call reduce_double_min(yref0)
   end if
   
   if ( xref0.eq.huge(0d0) ) then   ! nbndw=0 for all subdomains, or in sequential run
      xref0 = 0d0
      yref0 = 0d0
   end if
end subroutine get_refpoint


!> determine average height along wave energy boundary
subroutine get_hboundary(hboundary)
   use m_flow
   use m_flowgeom
   use m_flowparameters
   use m_xbeach_data
   use m_partitioninfo
   implicit none
   
   double precision, dimension(1), intent(out) :: hboundary
   
   double precision, dimension(1)              :: dlength
   
   double precision, dimension(2)              :: dum
   
   integer                                     :: i, k2
   
   hboundary = 0d0
   dlength   = 0d0
   if ( jampi.eq.0 ) then
!     integerate along wave boundary
      do i=1,nbndw
         hboundary = hboundary + max(hs(kbndw(1,i)),epshs) * wu(kbndw(3,i))
         dlength   = dlength   + wu(kbndw(3,i))
      enddo 
      
!     compute average
      if ( dlength(1).gt.0d0 ) then
         hboundary(1) = hboundary(1) / dlength(1)
      else
         hboundary(1) = 0d0
      end if
   else
!     integerate along wave boundary
      do i=1,nbndw
         k2 = kbndw(2,i)
         if ( idomain(k2).eq.my_rank ) then
            hboundary = hboundary + max(hs(kbndw(1,i)),epshs) * wu(kbndw(3,i))
            dlength   = dlength   + wu(kbndw(3,i))
         end if
      enddo
      
!     global reduction
      dum = (/ hboundary(1), dlength(1) /)
      call reduce_sum(2,dum)
      
!     compute average
      if ( dum(2).gt.0d0 ) then
         hboundary(1) = dum(1)/dum(2)
      else
         hboundary(1) = 0d0
      end if
   end if
   
   return
end subroutine
