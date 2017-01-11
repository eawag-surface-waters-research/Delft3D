!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2017.                                
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

      subroutine read_hyd(hyd)

      ! function : read a hydrodynamic description file

      ! global declarations

      use hydmod
      implicit none

      ! declaration of the arguments

      type(t_hyd)         :: hyd                    ! description of the hydrodynamics

      ! local declarations

      integer, parameter  :: nokey   = 77           ! number of keywords in hyd file
      character(len=40)   :: key(nokey)             ! keywords in the hyd file
      integer             :: ikey                   ! index keyword (first level)
      integer             :: ikey2                  ! index keyword (second level)
      integer             :: lunhyd                 ! unit number hyd file
      integer             :: lunrep                 ! unit number report file
      integer             :: ilay                   ! index layers
      integer             :: i_desc                 ! index in description
      integer             :: i_wasteload            ! index in collection
      type(t_wasteload)   :: wasteload              ! one wasteload description
      integer             :: i_domain               ! index in collection
      type(t_domain)      :: domain                 ! one domain description
      integer             :: i_dd_bound             ! index in collection
      type(t_dd_bound)    :: dd_bound               ! one dd_bound description
      character(len=255)  :: line                   ! line buffer input file
      character(len=255)  :: ctoken                 ! line buffer input file
      integer             :: ierr                   ! error indicator
      logical             :: token_used             ! token_used
      integer             :: platform               ! computer platform
      integer             :: ft_dat                 ! type of the data files
      type(inputfilestack):: inpfil                 ! input file strucure with include stack
      integer             :: i_swap                 ! variable used in swapping values
      character(len=256)  :: filpath                ! path to hyd file
      integer             :: pathlen                ! lentgth of path to hyd file
      integer             :: idummy                 ! idummy
      real                :: rdummy                 ! rdummy
      character           :: cdummy                 ! cdummy
      integer             :: ierr2                  ! ierr2
      logical             :: lfound                 ! indication if command line argument was found
      integer             :: iy                     ! year
      integer             :: imo                    ! month
      integer             :: id                     ! day
      integer             :: ih                     ! hour
      integer             :: im                     ! minute
      integer             :: is                     ! second
      integer             :: idate                  ! date
      integer             :: itime                  ! time
      real*8              :: julian                 ! julian function

      key(1)  = 'task'
      key(2)  = 'geometry'
      key(3)  = 'horizontal-aggregation'
      key(4)  = 'minimum-vert-diffusion-used'
      key(5)  = 'vertical-diffusion'
      key(6)  = 'description'
      key(7)  = 'end-description'
      key(8)  = 'reference-time'
      key(9)  = 'hydrodynamic-start-time'
      key(10) = 'hydrodynamic-stop-time'
      key(11) = 'hydrodynamic-timestep'
      key(12) = 'conversion-ref-time'
      key(13) = 'conversion-start-time'
      key(14) = 'conversion-stop-time'
      key(15) = 'conversion-timestep'
      key(16) = 'grid-cells-first-direction'
      key(17) = 'grid-cells-second-direction'
      key(18) = 'number-hydrodynamic-layers'
      key(19) = 'number-water-quality-layers'
      key(20) = 'hydrodynamic-file'
      key(21) = 'aggregation-file'
      key(22) = 'grid-indices-file'
      key(23) = 'grid-coordinates-file'
      key(24) = 'volumes-file'
      key(25) = 'areas-file'
      key(26) = 'flows-file'
      key(27) = 'pointers-file'
      key(28) = 'lengths-file'
      key(29) = 'salinity-file'
      key(30) = 'temperature-file'
      key(31) = 'vert-diffusion-file'
      key(32) = 'surfaces-file'
      key(33) = 'total-grid-file'
      key(34) = 'discharges-file'
      key(35) = 'chezy-coefficients-file'
      key(36) = 'shear-stresses-file'
      key(37) = 'walking-discharges-file'
      key(38) = 'minimum-vert-diffusion'
      key(39) = 'upper-layer'
      key(40) = 'lower-layer'
      key(41) = 'interface-depth'
      key(42) = 'end-minimum-vert-diffusion'
      key(43) = 'constant-dispersion'
      key(44) = 'first-direction'
      key(45) = 'second-direction'
      key(46) = 'third-direction'
      key(47) = 'end-constant-dispersion'
      key(48) = 'hydrodynamic-layers'
      key(49) = 'end-hydrodynamic-layers'
      key(50) = 'water-quality-layers'
      key(51) = 'end-water-quality-layers'
      key(52) = 'discharges'
      key(53) = 'end-discharges'
      key(54) = 'domains'
      key(55) = 'end-domains'
      key(56) = 'dd-boundaries'
      key(57) = 'end-dd-boundaries'
      key(58) = 'normal'
      key(59) = 'inlet'
      key(60) = 'outlet'
      key(61) = 'full-coupling'
      key(62) = 'coupling-per-domain'
      key(63) = 'attributes-file'
      key(64) = 'depths-file'
      key(65) = 'curvilinear-grid'
      key(66) = 'yes'
      key(67) = 'no'
      key(68) = 'calculated'
      key(69) = 'unstructured'
      key(70) = 'number-horizontal-exchanges'
      key(71) = 'number-vertical-exchanges'
      key(72) = 'number-water-quality-segments-per-layer'
      key(73) = 'horizontal-surfaces-file'
      key(74) = 'boundaries-file'
      key(75) = 'waqgeom-file'
      key(76) = 'automatic'
      key(77) = 'walking'

      platform = dlwq_platform()
      if ( platform .eq. fs_dos ) then
         ft_dat = ft_bin
      elseif ( platform .eq. FS_UNX ) then
         ft_dat = ft_unf
      elseif ( platform .eq. FS_ASC ) then
         ft_dat = ft_asc
      else
         ft_dat = 0
      endif

      call getmlu(lunrep)

      inpfil%inputf = 0
      call filestack_add(inpfil,hyd%file_hyd%name,ierr)
      if ( ierr .ne. 0 ) then
         write(lunrep,*) ' error opening hydrodynamic description file'
         write(lunrep,*) ' file :',trim(hyd%file_hyd%name)
      endif
      inpfil%cchar  = '#'

      hyd%description = ' '
      call dhpath ( hyd%file_hyd%name, filpath, pathlen)

      hyd%wasteload_coll%cursize = 0
      hyd%wasteload_coll%maxsize = 0
      hyd%domain_coll%cursize = 0
      hyd%domain_coll%maxsize = 0
      hyd%dd_bound_coll%cursize = 0
      hyd%dd_bound_coll%maxsize = 0
      hyd%file_com=t_dlwqfile(' ',' ',0,FT_NEF,FILE_STAT_UNOPENED)
      hyd%file_dwq=t_dlwqfile(' ',' ',0,FT_ASC,FILE_STAT_UNOPENED)
      hyd%file_vag=t_dlwqfile(' ',' ',0,FT_ASC,FILE_STAT_UNOPENED)
      hyd%file_lga=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_cco=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_bnd=t_dlwqfile(' ',' ',0,FT_ASC,FILE_STAT_UNOPENED)
      hyd%file_geo=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_vol=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_are=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_flo=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_poi=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_len=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_sal=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_tem=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_vdf=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_srf=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_hsrf=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_lgt=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_src=t_dlwqfile(' ',' ',0,FT_ASC,FILE_STAT_UNOPENED)
      hyd%file_chz=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_tau=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_wlk=t_dlwqfile(' ',' ',0,FT_ASC,FILE_STAT_UNOPENED)
      hyd%file_atr=t_dlwqfile(' ',' ',0,FT_ASC,FILE_STAT_UNOPENED)
      hyd%file_dps=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%mmax = 0
      hyd%nmax = 0
      hyd%kmax = 1
      hyd%nosegl = 0
      hyd%noseg = 0
      hyd%nolay = 1
      hyd%noq1 = 0
      hyd%noq2 = 0
      hyd%noq3 = 0
      hyd%noq4 = 0
      hyd%noq  = 0
!

      ! loop over all the tokens in the file

      do

         call dlwq_read_token( inpfil, ctoken, ierr)

         ! if end of file the exit loop

         if ( ierr .ne. 0 ) exit

         call zoek ( ctoken, nokey , key , 30 , ikey )
         if ( ikey .eq. 1 ) then

            ! task

            call dlwq_read_token( inpfil, ctoken, ierr)
            if ( ierr .ne. 0 ) goto 900

            call zoek ( ctoken, nokey , key , 30 , ikey2 )
            if ( ikey2 .eq. 61 ) then
               hyd%task = HYD_TASK_FULL
            elseif ( ikey2 .eq. 62 ) then
               hyd%task = HYD_TASK_DDC
            else
               hyd%task = HYD_TASK_UNKNOWN
               write(lunrep,'(a)') ' warning unknown task in hydrodynamic file'
               write(lunrep,'(2a)') ' task =',trim(ctoken)
            endif

         elseif ( ikey .eq. 2 ) then

            ! geometry

            call dlwq_read_token( inpfil, ctoken, ierr)
            if ( ierr .ne. 0 ) goto 900

            call zoek ( ctoken, nokey , key , 30 , ikey2 )
            if ( ikey2 .eq. 65 ) then
               hyd%geometry = HYD_GEOM_CURVI
            elseif ( ikey2 .eq. 69 ) then
               hyd%geometry = HYD_GEOM_UNSTRUC
            else
               hyd%geometry = HYD_GEOM_UNKNOWN
               write(lunrep,'(a)') ' warning unknown geometry in hydrodynamic file'
               write(lunrep,'(2a)') ' geometry =',trim(ctoken)
            endif

         elseif ( ikey .eq. 6 ) then

            ! description

            i_desc = 0
            do

               ! look for end-description token

               call dlwq_read_token( inpfil, ctoken, ierr)
               if ( ierr .ne. 0 ) goto 900
               call zoek ( ctoken, nokey , key , 30 , ikey2 )
               if ( ikey2 .eq. 7 ) exit

               ! it is a description line, store up to three

               i_desc = i_desc + 1
               if ( i_desc .le. 3 ) hyd%description(i_desc) = ctoken

            enddo

         elseif ( ikey .eq. 8 ) then

            ! reference time

            call dlwq_read_token( inpfil, hyd%hyd_ref, ierr)
            if ( ierr .ne. 0 ) goto 900

            ! convert to julian

            read (hyd%hyd_ref(1:8),'(i8)') idate
            read (hyd%hyd_ref(9:14),'(i6)') itime
            hyd%time_ref = julian ( idate , itime )

         elseif ( ikey .eq. 9 ) then

            ! hydrodynamic start

            call dlwq_read_token( inpfil, hyd%hyd_start, ierr)
            if ( ierr .ne. 0 ) goto 900

         elseif ( ikey .eq. 10) then

            ! hydrodynamic stop

            call dlwq_read_token( inpfil, hyd%hyd_stop, ierr)
            if ( ierr .ne. 0 ) goto 900

         elseif ( ikey .eq. 11) then

            ! hydrodynamic step

            call dlwq_read_token( inpfil, hyd%hyd_step, ierr)
            if ( ierr .ne. 0 ) goto 900

         elseif ( ikey .eq. 12) then

            ! conversion reference time

            call dlwq_read_token( inpfil, hyd%cnv_ref, ierr)
            if ( ierr .ne. 0 ) goto 900

         elseif ( ikey .eq. 13) then

            ! conversion start time

            call dlwq_read_token( inpfil, hyd%cnv_start, ierr)
            if ( ierr .ne. 0 ) goto 900

         elseif ( ikey .eq. 14) then

            ! conversion stop time

            call dlwq_read_token( inpfil, hyd%cnv_stop, ierr)
            if ( ierr .ne. 0 ) goto 900

         elseif ( ikey .eq. 15) then

            ! conversion step time

            call dlwq_read_token( inpfil, hyd%cnv_step, ierr)
            if ( ierr .ne. 0 ) goto 900
            read(hyd%cnv_step,'(i4,i2,i2,i2,i2,i2)') iy,imo,id,ih,im,is
            if (iy .ne. 0 .or. imo .ne. 0 ) then
               write(lunrep,*) ' error conversion step has year or month, this is not supported'
               goto 900
            endif
            hyd%cnv_step_sec = id*86400+ih*3600+im*60+is

         elseif ( ikey .eq. 16) then

            ! grid cells first direction

            call dlwq_read_token( inpfil, hyd%mmax, ierr)
            if ( ierr .ne. 0 ) goto 900

         elseif ( ikey .eq. 17) then

            ! grid cells second direction

            call dlwq_read_token( inpfil, hyd%nmax, ierr)
            if ( ierr .ne. 0 ) goto 900

         elseif ( ikey .eq. 18) then

            ! number of hydrodynamic layers

            call dlwq_read_token( inpfil, hyd%kmax, ierr)
            if ( ierr .ne. 0 ) goto 900

         elseif ( ikey .eq. 19) then

            ! number of waq layers

            call dlwq_read_token( inpfil, hyd%nolay, ierr)
            if ( ierr .ne. 0 ) goto 900

         elseif ( ikey .eq. 70) then

            ! number of horizontal exchanges

            call dlwq_read_token( inpfil, hyd%noq1, ierr)
            if ( ierr .ne. 0 ) goto 900

         elseif ( ikey .eq. 71) then

            ! number of vertical exchanges

            call dlwq_read_token( inpfil, hyd%noq3, ierr)

            if ( ierr .ne. 0 ) goto 900

         elseif ( ikey .eq. 72) then

            ! number of water quality segments per layer

            call dlwq_read_token( inpfil, hyd%nosegl, ierr)

            if ( ierr .ne. 0 ) goto 900

         elseif ( ikey .eq. 20) then

            ! com file

            call dlwq_read_token( inpfil, hyd%file_com%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_com%name = trim(filpath)//hyd%file_com%name

         elseif ( ikey .eq. 21) then

            ! dwq file

            call dlwq_read_token( inpfil, hyd%file_dwq%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_dwq%name = trim(filpath)//hyd%file_dwq%name

         elseif ( ikey .eq. 22) then

            ! lga file

            call dlwq_read_token( inpfil, hyd%file_lga%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_lga%name = trim(filpath)//hyd%file_lga%name

         elseif ( ikey .eq. 23) then

            ! cco file

            call dlwq_read_token( inpfil, hyd%file_cco%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_cco%name = trim(filpath)//hyd%file_cco%name

         elseif ( ikey .eq. 74) then

            ! bnd file (unstructured)

            call dlwq_read_token( inpfil, hyd%file_bnd%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_bnd%name = trim(filpath)//hyd%file_bnd%name

         elseif ( ikey .eq. 75) then

            ! waqgeom file (unstructured)

            call dlwq_read_token( inpfil, hyd%file_geo%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_geo%name = trim(filpath)//hyd%file_geo%name

         elseif ( ikey .eq. 24) then

            ! vol file

            call dlwq_read_token( inpfil, hyd%file_vol%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_vol%name = trim(filpath)//hyd%file_vol%name

         elseif ( ikey .eq. 25) then

            ! are file

            call dlwq_read_token( inpfil, hyd%file_are%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_are%name = trim(filpath)//hyd%file_are%name

         elseif ( ikey .eq. 26) then

            ! flo file

            call dlwq_read_token( inpfil, hyd%file_flo%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_flo%name = trim(filpath)//hyd%file_flo%name

         elseif ( ikey .eq. 27) then

            ! poi file

            call dlwq_read_token( inpfil, hyd%file_poi%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_poi%name = trim(filpath)//hyd%file_poi%name

         elseif ( ikey .eq. 28) then

            ! len file

            call dlwq_read_token( inpfil, hyd%file_len%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_len%name = trim(filpath)//hyd%file_len%name

         elseif ( ikey .eq. 29) then

            ! sal file

            call dlwq_read_token( inpfil, ctoken, ierr)
            if ( ierr .ne. 0 ) goto 900
            if ( ctoken.ne. 'none' ) then
               hyd%file_sal%name = trim(filpath)//ctoken
               hyd%sal_present = .true.
            else
               hyd%file_sal%name = ' '
               hyd%sal_present = .false.
            endif

         elseif ( ikey .eq. 30) then

            ! tmp file

            call dlwq_read_token( inpfil, ctoken, ierr)
            if ( ierr .ne. 0 ) goto 900
            if ( ctoken.ne. 'none' ) then
               hyd%file_tem%name = trim(filpath)//ctoken
               hyd%tem_present = .true.
            else
               hyd%file_tem%name = ' '
               hyd%tem_present = .false.
            endif

         elseif ( ikey .eq. 31) then

            ! vdf file

            call dlwq_read_token( inpfil, ctoken, ierr)
            if ( ierr .ne. 0 ) goto 900
            if ( ctoken.ne. 'none' ) then
               hyd%file_vdf%name = trim(filpath)//ctoken
               hyd%vdf_present = .true.
            else
               hyd%file_vdf%name = ' '
               hyd%vdf_present = .false.
            endif

         elseif ( ikey .eq. 32) then

            ! srf file

            call dlwq_read_token( inpfil, hyd%file_srf%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_srf%name = trim(filpath)//hyd%file_srf%name

         elseif ( ikey .eq. 73) then

            ! hsrf file

            call dlwq_read_token( inpfil, hyd%file_hsrf%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_hsrf%name = trim(filpath)//hyd%file_hsrf%name


         elseif ( ikey .eq. 33) then

            ! lgt file

            call dlwq_read_token( inpfil, hyd%file_lgt%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_lgt%name = trim(filpath)//hyd%file_lgt%name

         elseif ( ikey .eq. 34) then

            ! src file

            call dlwq_read_token( inpfil, hyd%file_src%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_src%name = trim(filpath)//hyd%file_src%name

         elseif ( ikey .eq. 35) then

            ! chz file

            call dlwq_read_token( inpfil, hyd%file_chz%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_chz%name = trim(filpath)//hyd%file_chz%name

         elseif ( ikey .eq. 36) then

            ! tau file

            call dlwq_read_token( inpfil, ctoken, ierr)
            if ( ierr .ne. 0 ) goto 900
            if ( ctoken.ne. 'none' ) then
               hyd%file_tau%name = trim(filpath)//ctoken
               hyd%tau_present = .true.
            else
               hyd%file_tau%name = ' '
               hyd%tau_present = .false.
            endif

         elseif ( ikey .eq. 37) then

            ! wlk file

            call dlwq_read_token( inpfil, hyd%file_wlk%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_wlk%name = trim(filpath)//hyd%file_wlk%name

         elseif ( ikey .eq. 63) then

            ! attrubutes file

            call dlwq_read_token( inpfil, hyd%file_atr%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_atr%name = trim(filpath)//hyd%file_atr%name

         elseif ( ikey .eq. 64) then

            ! depths file

            call dlwq_read_token( inpfil, hyd%file_dps%name, ierr)
            if ( ierr .ne. 0 ) goto 900
            hyd%file_dps%name = trim(filpath)//hyd%file_dps%name

         elseif ( ikey .eq. 48) then

            ! hydrodynamic-layers

            allocate(hyd%hyd_layers(hyd%kmax))
            do ilay = 1 , hyd%kmax
               call dlwq_read_token( inpfil, hyd%hyd_layers(ilay), ierr)
               if ( ierr .ne. 0 ) goto 900
            enddo

            ! end-hydrodynamic-layers

            call dlwq_read_token( inpfil, ctoken, ierr)

         elseif ( ikey .eq. 50) then

            ! water-quality-layers

            allocate(hyd%waq_layers(hyd%nolay))
            do ilay = 1 , hyd%nolay
               call dlwq_read_token( inpfil, hyd%waq_layers(ilay), ierr)
               if ( ierr .ne. 0 ) goto 900
            enddo

            ! end-water-quality-layers

            call dlwq_read_token( inpfil, ctoken, ierr)

         elseif ( ikey .eq. 52) then

            ! discharges

            token_used = .true.
            do
               if ( token_used ) then
                  call dlwq_read_token( inpfil, ctoken, ierr)
                  if ( ierr .ne. 0 ) goto 900
               endif
               call zoek ( ctoken, nokey , key , 30 , ikey2 )
               if ( ikey2 .eq. 53 ) exit

               ! a new wasteload

               if ( inpfil%t_token .eq. TYPE_INT ) then
                  wasteload%n    = inpfil%itoken
               else
                  goto 900
               endif
               call dlwq_read_token( inpfil, wasteload%m, ierr)
               if ( ierr .ne. 0 ) goto 900
               call dlwq_read_token( inpfil, wasteload%k, ierr)
               if ( ierr .ne. 0 ) goto 900
               call dlwq_read_token( inpfil, wasteload%name, ierr)
               if ( ierr .ne. 0 ) goto 900
               call dlwq_read_token( inpfil, ctoken, ierr)
               if ( ierr .ne. 0 ) goto 900
               call zoek ( ctoken, nokey , key , 30 , ikey2 )
               if ( ikey2 .eq. 58 .or. ikey2 .eq. 59 .or. ikey2 .eq. 60 .or. ikey2 .eq. 77 ) then
                  token_used = .true.
                  if ( ikey2 .eq. 58 ) then
                     wasteload%type = DLWQ_WASTE_NORMAL
                  elseif ( ikey2 .eq. 59 ) then
                     wasteload%type = DLWQ_WASTE_INLET
                  elseif ( ikey2 .eq. 60 ) then
                     wasteload%type = DLWQ_WASTE_OUTLET
                  elseif ( ikey2 .eq. 77 ) then
                     wasteload%type = DLWQ_WASTE_WALK
                  endif
               else
                  wasteload%type = DLWQ_WASTE_NORMAL
                  token_used = .false.
               endif
               wasteload%waqtype = ' '

               ! add to wasteload collection

               i_wasteload = wasteload_coll_add(hyd%wasteload_coll, wasteload)

            enddo

         elseif ( ikey .eq. 54) then

            ! domains

            do
               call dlwq_read_token( inpfil, ctoken, ierr)
               if ( ierr .ne. 0 ) goto 900

               ! look for end-domains keyword

               call zoek ( ctoken, nokey , key , 30 , ikey2 )
               if ( ikey2 .eq. 55 ) exit

               ! key is domain name , read mmax nmax and dido file do not store dido file

               domain%name = ctoken
               call dlwq_read_token( inpfil, domain%mmax, ierr)
               if ( ierr .ne. 0 ) goto 900
               call dlwq_read_token( inpfil, domain%nmax, ierr)
               if ( ierr .ne. 0 ) goto 900
               call dlwq_read_token( inpfil, ctoken, ierr)
               if ( ierr .ne. 0 ) goto 900

               ! add to domains collection

               i_domain = domain_coll_add(hyd%domain_coll, domain)

            enddo

         elseif ( ikey .eq. 56) then

            ! dd-boundaries

            do
               call dlwq_read_token( inpfil, ctoken, ierr)
               if ( ierr .ne. 0 ) goto 900

               ! look for end-dd-boundaries keyword

               call zoek ( ctoken, nokey , key , 30 , ikey2 )
               if ( ikey2 .eq. 57 ) exit

               ! ctokenis domain name 1 , read m_begin1, n_begin1, m_end1, n_end1, domain name 2, m_begin2, n_begin2, m_end2, n_end2

               dd_bound%name1 = ctoken
               call dlwq_read_token( inpfil, dd_bound%m_begin1, ierr) ; if ( ierr .ne. 0 ) goto 900
               call dlwq_read_token( inpfil, dd_bound%n_begin1, ierr) ; if ( ierr .ne. 0 ) goto 900
               call dlwq_read_token( inpfil, dd_bound%m_end1, ierr)   ; if ( ierr .ne. 0 ) goto 900
               call dlwq_read_token( inpfil, dd_bound%n_end1, ierr)   ; if ( ierr .ne. 0 ) goto 900

               call dlwq_read_token( inpfil, dd_bound%name2, ierr)    ; if ( ierr .ne. 0 ) goto 900
               call dlwq_read_token( inpfil, dd_bound%m_begin2, ierr) ; if ( ierr .ne. 0 ) goto 900
               call dlwq_read_token( inpfil, dd_bound%n_begin2, ierr) ; if ( ierr .ne. 0 ) goto 900
               call dlwq_read_token( inpfil, dd_bound%m_end2, ierr)   ; if ( ierr .ne. 0 ) goto 900
               call dlwq_read_token( inpfil, dd_bound%n_end2, ierr)   ; if ( ierr .ne. 0 ) goto 900

               ! make sure the numbering is always increasing

               if ( dd_bound%m_begin1 .gt. dd_bound%m_end1 ) then
                  i_swap            = dd_bound%m_begin1
                  dd_bound%m_begin1 = dd_bound%m_end1
                  dd_bound%m_end1   = i_swap
               endif
               if ( dd_bound%n_begin1 .gt. dd_bound%n_end1 ) then
                  i_swap            = dd_bound%n_begin1
                  dd_bound%n_begin1 = dd_bound%n_end1
                  dd_bound%n_end1   = i_swap
               endif
               if ( dd_bound%m_begin2 .gt. dd_bound%m_end2 ) then
                  i_swap            = dd_bound%m_begin2
                  dd_bound%m_begin2 = dd_bound%m_end2
                  dd_bound%m_end2   = i_swap
               endif
               if ( dd_bound%n_begin2 .gt. dd_bound%n_end2 ) then
                  i_swap            = dd_bound%n_begin2
                  dd_bound%n_begin2 = dd_bound%n_end2
                  dd_bound%n_end2   = i_swap
               endif

               ! add to dd_bound collection

               i_dd_bound = dd_bound_coll_add(hyd%dd_bound_coll, dd_bound)

            enddo

         endif

      enddo

      ! 2d then no vdf file

      if ( hyd%nolay .le. 1 ) then
         hyd%file_vdf%name = ' '
         hyd%vdf_present = .false.
      endif

      ! unstructured set nmax to 1

      if ( hyd%geometry .eq. HYD_GEOM_UNSTRUC ) then
         hyd%nmax = 1
      endif

      return
 900  call dherrs('error reading hyd file ('//trim(key(ikey))//'), last line:'//trim(inpfil%linbuf(inpfil%inputf)),1)
      end subroutine read_hyd
