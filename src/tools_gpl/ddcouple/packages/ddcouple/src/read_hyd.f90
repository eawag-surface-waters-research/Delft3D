!!  Copyright (C)  Stichting Deltares, 2021-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine read_hyd(hyd)

      ! function : read a hydrodynamic description file

      ! global declarations

      use m_zoek
      use m_monsys
      use m_julian
      use m_dhpath
      use hydmod
      use m_dherrs
      use rd_token                    ! tokenized reading
      use :: m_hyd_keys, only: key, nokey     ! keywords in hydfile
      implicit none

      ! declaration of the arguments

      type(t_hyd)         :: hyd                    ! description of the hydrodynamics

      ! local declarations

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
      integer             :: i_swap                 ! variable used in swapping values
      character(len=256)  :: filpath                ! path to hyd file
      integer             :: pathlen                ! lentgth of path to hyd file
      integer             :: idummy                 ! idummy
      real                :: rdummy                 ! rdummy
      character           :: cdummy                 ! cdummy
      integer             :: itype                  ! itype
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
      logical, parameter  :: untileol = .true.      ! read until the end of the line


      ft_dat = ft_bin
      call getmlu(lunrep)

      hyd%file_hyd%type = ft_asc
      call dlwqfile_open(hyd%file_hyd)

      ! initialise tokenised reading
      ilun    = 0
      ilun(1) = hyd%file_hyd%unit_nr
      lch (1) = hyd%file_hyd%name
      npos   = 1000
      cchar  = '#'
      ierr = 0

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
      hyd%file_vol=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_are=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_flo=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_poi=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_len=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_sal=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_tem=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_vdf=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
      hyd%file_srf=t_dlwqfile(' ',' ',0,ft_dat,FILE_STAT_UNOPENED)
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
      hyd%zbot = -999.0
      hyd%ztop = -999.0
!

      ! loop over all the tokens in the file

      do

         ! if end of file the exit loop
         if ( gettoken( ctoken, idummy, rdummy, itype, ierr) .ne. 0 ) exit
         if (itype .ne. 1) then
             goto 900
         end if

         call zoek ( ctoken, nokey , key , 30 , ikey )
         if ( ikey .eq. 1 ) then

            ! task
            if ( gettoken( ctoken, ierr) .ne. 0 ) goto 900
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
            if ( gettoken( ctoken, ierr) .ne. 0 ) goto 900
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

            ! layer type
            hyd%layer_type = HYD_LAYERS_SIGMA ! Always assume sigma layers, unless otherwise stated
            if ( gettoken( ctoken, ierr) .eq. 0 ) then
               call zoek ( ctoken, nokey , key , 30 , ikey2 )
               if ( ikey2 .eq. 82 ) then
                  hyd%layer_type = HYD_LAYERS_Z
               else
                  if ( puttoken( ctoken ) .ne. 0 ) goto 900
               end if
            else
               if ( puttoken( ctoken ) .ne. 0 ) goto 900
            end if
         elseif ( ikey .eq. 6 ) then
            ! description
            i_desc = 0
            do
               ! look for end-description token
               if ( gettoken ( ctoken, ierr) .ne. 0 ) goto 900
               call zoek ( ctoken, nokey , key , 30 , ikey2 )
               if ( ikey2 .eq. 7 ) exit
               ! it is a description line, store up to three
               i_desc = i_desc + 1
               if ( i_desc .le. 3 ) hyd%description(i_desc) = ctoken
            enddo

         elseif ( ikey .eq. 8 ) then

            ! reference time
            if ( gettoken (hyd%hyd_ref, ierr) .ne. 0 ) goto 900
            ! convert to julian
            read (hyd%hyd_ref(1:8),'(i8)') idate
            read (hyd%hyd_ref(9:14),'(i6)') itime
            hyd%time_ref = julian ( idate , itime )

         elseif ( ikey .eq. 9 ) then
            ! hydrodynamic start
            if ( gettoken(hyd%hyd_start, ierr) .ne. 0 ) goto 900

         elseif ( ikey .eq. 10) then
            ! hydrodynamic stop
            if ( gettoken(hyd%hyd_stop, ierr) .ne. 0 ) goto 900

         elseif ( ikey .eq. 11) then
            ! hydrodynamic step
            if ( gettoken(hyd%hyd_step, ierr) .ne. 0 ) goto 900

         elseif ( ikey .eq. 12) then
            ! conversion reference time
            if ( gettoken(hyd%cnv_ref, ierr) .ne. 0 ) goto 900

         elseif ( ikey .eq. 13) then
            ! conversion start time
            if ( gettoken(hyd%cnv_start, ierr) .ne. 0 ) goto 900

         elseif ( ikey .eq. 14) then
            ! conversion stop time
            if ( gettoken(hyd%cnv_stop, ierr) .ne. 0 ) goto 900

         elseif ( ikey .eq. 15) then
            ! conversion step time
            if ( gettoken(hyd%cnv_step, ierr) .ne. 0 ) goto 900
            read(hyd%cnv_step,'(i4,i2,i2,i2,i2,i2)') iy,imo,id,ih,im,is
            if (iy .ne. 0 .or. imo .ne. 0 ) then
               write(lunrep,*) ' error conversion step has year or month, this is not supported'
               goto 900
            endif
            hyd%cnv_step_sec = id*86400+ih*3600+im*60+is

         elseif ( ikey .eq. 16) then
            ! grid cells first direction
            if ( gettoken(hyd%mmax, ierr) .ne. 0 ) goto 900

         elseif ( ikey .eq. 17) then
            ! grid cells second direction
            if ( gettoken(hyd%nmax, ierr) .ne. 0 ) goto 900

         elseif ( ikey .eq. 18) then
            ! number of hydrodynamic layers
            if ( gettoken(hyd%kmax, ierr) .ne. 0 ) goto 900

         elseif ( ikey .eq. 19) then
            ! number of waq layers
            if ( gettoken(hyd%nolay, ierr) .ne. 0 ) goto 900

         elseif ( ikey .eq. 70) then
            ! number of horizontal exchanges
            if ( gettoken(hyd%noq1, ierr) .ne. 0 ) goto 900

         elseif ( ikey .eq. 71) then
            ! number of vertical exchanges
            if ( gettoken(hyd%noq3, ierr) .ne. 0 ) goto 900

         elseif ( ikey .eq. 72) then
            ! number of water quality segments per layer
            if ( gettoken(hyd%nosegl, ierr) .ne. 0 ) goto 900

         elseif ( ikey .eq. 20) then
            ! com file
            if ( gettoken(hyd%file_com%name, ierr) .ne. 0 ) goto 900
            hyd%file_com%name = trim(filpath)//hyd%file_com%name

         elseif ( ikey .eq. 21) then
            ! dwq file
            if ( gettoken(hyd%file_dwq%name, ierr) .ne. 0 ) goto 900
            hyd%file_dwq%name = trim(filpath)//hyd%file_dwq%name

         elseif ( ikey .eq. 22) then
            ! lga file
            if ( gettoken(hyd%file_lga%name, ierr) .ne. 0 ) goto 900
            hyd%file_lga%name = trim(filpath)//hyd%file_lga%name

         elseif ( ikey .eq. 23) then
            ! cco file
            if ( gettoken(hyd%file_cco%name, ierr) .ne. 0 ) goto 900
            hyd%file_cco%name = trim(filpath)//hyd%file_cco%name


         elseif ( ikey .eq. 24) then

            ! vol file
            if ( gettoken(hyd%file_vol%name, ierr) .ne. 0 ) goto 900
            hyd%file_vol%name = trim(filpath)//hyd%file_vol%name

         elseif ( ikey .eq. 25) then
            ! are file
            if ( gettoken(hyd%file_are%name, ierr) .ne. 0 ) goto 900
            hyd%file_are%name = trim(filpath)//hyd%file_are%name

         elseif ( ikey .eq. 26) then
            ! flo file
            if ( gettoken(hyd%file_flo%name, ierr) .ne. 0 ) goto 900
            hyd%file_flo%name = trim(filpath)//hyd%file_flo%name

         elseif ( ikey .eq. 27) then
            ! poi file
            if ( gettoken(hyd%file_poi%name, ierr) .ne. 0 ) goto 900
            hyd%file_poi%name = trim(filpath)//hyd%file_poi%name

         elseif ( ikey .eq. 28) then
            ! len file
            if ( gettoken(hyd%file_len%name, ierr) .ne. 0 ) goto 900
            hyd%file_len%name = trim(filpath)//hyd%file_len%name

         elseif ( ikey .eq. 29) then
            ! sal file
            if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900
            if ( ctoken.ne. 'none' ) then
               hyd%file_sal%name = trim(filpath)//ctoken
               hyd%sal_present = .true.
            else
               hyd%file_sal%name = ' '
               hyd%sal_present = .false.
            endif

         elseif ( ikey .eq. 30) then
            ! tmp file
            if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900
            if ( ctoken.ne. 'none' ) then
               hyd%file_tem%name = trim(filpath)//ctoken
               hyd%tem_present = .true.
            else
               hyd%file_tem%name = ' '
               hyd%tem_present = .false.
            endif

         elseif ( ikey .eq. 31) then
            ! vdf file
            if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900
            if ( ctoken.ne. 'none' ) then
               hyd%file_vdf%name = trim(filpath)//ctoken
               hyd%vdf_present = .true.
            else
               hyd%file_vdf%name = ' '
               hyd%vdf_present = .false.
            endif

         elseif ( ikey .eq. 32) then
            ! srf file
            if ( gettoken(hyd%file_srf%name, ierr) .ne. 0 ) goto 900
            hyd%file_srf%name = trim(filpath)//hyd%file_srf%name


         elseif ( ikey .eq. 33) then
            ! lgt file
            if ( gettoken(hyd%file_lgt%name, ierr) .ne. 0 ) goto 900
            hyd%file_lgt%name = trim(filpath)//hyd%file_lgt%name

         elseif ( ikey .eq. 34) then
            ! src file
            if ( gettoken(hyd%file_src%name, ierr) .ne. 0 ) goto 900
            hyd%file_src%name = trim(filpath)//hyd%file_src%name

         elseif ( ikey .eq. 35) then
            ! chz file
            if ( gettoken(hyd%file_chz%name, ierr) .ne. 0 ) goto 900
            hyd%file_chz%name = trim(filpath)//hyd%file_chz%name

         elseif ( ikey .eq. 36) then
            ! tau file
            if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900
            if ( ctoken.ne. 'none' ) then
               hyd%file_tau%name = trim(filpath)//ctoken
               hyd%tau_present = .true.
            else
               hyd%file_tau%name = ' '
               hyd%tau_present = .false.
            endif

         elseif ( ikey .eq. 37) then
            ! wlk file
            if ( gettoken(hyd%file_wlk%name, ierr) .ne. 0 ) goto 900
            hyd%file_wlk%name = trim(filpath)//hyd%file_wlk%name

         elseif ( ikey .eq. 63) then
            ! attrubutes file
            if ( gettoken(hyd%file_atr%name, ierr) .ne. 0 ) goto 900
            hyd%file_atr%name = trim(filpath)//hyd%file_atr%name

         elseif ( ikey .eq. 64) then
            ! depths file
            if ( gettoken(hyd%file_dps%name, ierr) .ne. 0 ) goto 900
            hyd%file_dps%name = trim(filpath)//hyd%file_dps%name

         elseif ( ikey .eq. 83) then
            ! ztop
            if ( gettoken( hyd%ztop, ierr) .ne. 0 ) goto 900

         elseif ( ikey .eq. 84) then
            ! ztop
            if ( gettoken( hyd%zbot, ierr) .ne. 0 ) goto 900

         elseif ( ikey .eq. 48) then
            ! hydrodynamic-layers
            allocate(hyd%hyd_layers(hyd%kmax))
            do ilay = 1 , hyd%kmax
               if ( gettoken(hyd%hyd_layers(ilay), ierr) .ne. 0 ) goto 900
            enddo
            ! end-hydrodynamic-layers
            if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900

         elseif ( ikey .eq. 50) then
            ! water-quality-layers
            allocate(hyd%waq_layers(hyd%nolay))
            do ilay = 1 , hyd%nolay
               if ( gettoken(hyd%waq_layers(ilay), ierr) .ne. 0 ) goto 900
            enddo
            ! end-water-quality-layers
            if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900

         elseif ( ikey .eq. 52) then
            ! discharges
            token_used = .true.
            do
               if ( token_used ) then
                  if ( gettoken(ctoken, idummy, rdummy, itype, ierr) .ne. 0 ) goto 900
               endif
               call zoek ( ctoken, nokey , key , 30 , ikey2 )
               if ( ikey2 .eq. 53 ) exit

               ! a new wasteload
               if ( itype .eq. TYPE_INT ) then
                  wasteload%n    = idummy
               else
                  goto 900
               endif
               if ( gettoken(wasteload%m, ierr) .ne. 0 ) goto 900
               if ( gettoken(wasteload%k, ierr) .ne. 0 ) goto 900
               if ( gettoken(wasteload%name, ierr) .ne. 0 ) goto 900
               if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900
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

               ! add to wasteload collection
               i_wasteload = wasteload_coll_add(hyd%wasteload_coll, wasteload)

            enddo

         elseif ( ikey .eq. 54) then
            ! domains
            do
               if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900
               ! look for end-domains keyword
               call zoek ( ctoken, nokey , key , 30 , ikey2 )
               if ( ikey2 .eq. 55 ) exit

               ! key is domain name , read mmax nmax and dido file do not store dido file
               domain%name = ctoken
               if ( gettoken(domain%mmax, ierr) .ne. 0 ) goto 900
               if ( gettoken(domain%nmax, ierr) .ne. 0 ) goto 900
               if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900

               ! add to domains collection
               i_domain = domain_coll_add(hyd%domain_coll, domain)
            enddo

         elseif ( ikey .eq. 56) then
            ! dd-boundaries
            do
               if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900

               ! look for end-dd-boundaries keyword
               call zoek ( ctoken, nokey , key , 30 , ikey2 )
               if ( ikey2 .eq. 57 ) exit

               ! ctokenis domain name 1 , read m_begin1, n_begin1, m_end1, n_end1, domain name 2, m_begin2, n_begin2, m_end2, n_end2
               dd_bound%name1 = ctoken
               if (gettoken(dd_bound%m_begin1, ierr) .ne. 0 ) goto 900
               if (gettoken(dd_bound%n_begin1, ierr) .ne. 0 ) goto 900
               if (gettoken(dd_bound%m_end1, ierr)   .ne. 0 ) goto 900
               if (gettoken(dd_bound%n_end1, ierr)   .ne. 0 ) goto 900

               if (gettoken(dd_bound%name2, ierr)    .ne. 0 ) goto 900
               if (gettoken(dd_bound%m_begin2, ierr) .ne. 0 ) goto 900
               if (gettoken(dd_bound%n_begin2, ierr) .ne. 0 ) goto 900
               if (gettoken(dd_bound%m_end2, ierr)   .ne. 0 ) goto 900
               if (gettoken(dd_bound%n_end2, ierr)   .ne. 0 ) goto 900

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

         elseif ( ikey .eq. 78) then
            ! file-created-by string.
            if (gettoken(line, untileol, ierr) .ne. 0 ) goto 900
            hyd%created_by = line(1:80)

         elseif ( ikey .eq. 79) then
            ! file-creation-date
            if (gettoken(line, untileol, ierr) .ne. 0 ) goto 900
            hyd%creation_date = line(1:40)

         elseif ( ikey .eq. 80) then
            ! sink-sources
            do
               if ( gettoken(ctoken, idummy, rdummy, itype, ierr) .ne. 0 ) goto 900
                  if(itype==1) then
                     ! look for end-domains keyword
                     call zoek ( ctoken, nokey , key , 30 , ikey2 )
                     if ( ikey2 .eq. 81 ) exit
                  endif
!               ! key is domain name , read mmax nmax and dido file do not store dido file
!               domain%name = ctoken
!               if ( gettoken(domain%mmax, ierr) .ne. 0 ) goto 900
!               if ( gettoken(domain%nmax, ierr) .ne. 0 ) goto 900
!               if ( gettoken(ctoken, ierr) .ne. 0 ) goto 900
!
!               ! add to domains collection
!               i_domain = domain_coll_add(hyd%domain_coll, domain)
            enddo

         else
            ! unknown keyword, ignore until the end of the line
            if (gettoken(line, untileol, ierr) .ne. 0 ) goto 900

         endif

      enddo

      ! 2d then no vdf file

      if ( hyd%nolay .le. 1 ) then
         hyd%file_vdf%name = ' '
         hyd%vdf_present = .false.
      endif

      return
 900  call dherrs('error reading hyd file ('//trim(key(ikey))//')', 0)
      end subroutine read_hyd
