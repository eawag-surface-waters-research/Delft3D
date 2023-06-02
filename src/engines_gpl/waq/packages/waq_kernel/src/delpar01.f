!!  Copyright (C)  Stichting Deltares, 2012-2023.
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
      subroutine delpar01 ( itime   , noseg   , nolay   , noq     , nosys   ,
     &                      notot   , dwqvol  , surface , dwqflo  , syname  ,
     &                      nosfun  , sfname  , segfun  , amass   , conc    ,
     &                      iaflag  , intopt  , ndmps   , isdmp   , dmps    ,
     &                      amass2  )

      use m_zoek
      use partmem      !   for PARTicle tracking
      use timers
      use parths_mod                 ! explicit interface
!      use rdhydr_mod                 ! explicit interface
      use partwq_mod                 ! explicit interface
      use oildsp_mod                 ! explicit interface
      use part03_mod                 ! explicit interface
      use part09_mod                 ! explicit interface
      use part10_mod                 ! explicit interface
      use part12_mod                 ! explicit interface
      use part13_mod                 ! explicit interface
      use part14_mod                 ! explicit interface
      use part18_mod                 ! explicit interface
      use part21_mod                 ! explicit interface
      use partur_mod                 ! explicit interface
      use spec_feat_par
      use partini_mod
      use m_part_regular
      use larvae_mod
      use abm_mod

      implicit none

!     Arguments

!     kind           function         name                      description

      integer  (ip), intent(in   ) :: itime                   !< actual time
      integer  (ip), intent(in   ) :: noseg                   !< delwaq noseg
      integer  (ip), intent(in   ) :: nolay                   !< delwaq layers
      integer  (ip), intent(in   ) :: noq                     !< delwaq noq
      integer  (ip), intent(in   ) :: nosys                   !< delwaq transported subs
      integer  (ip), intent(in   ) :: notot                   !< delwaq total subs, part subs included
      real     (rp), intent(in   ) :: dwqvol (noseg )         !< delwaq volumes
      real     (rp), intent(in   ) :: surface(noseg )         !< horizontal surfaces
      real     (rp), intent(in   ) :: dwqflo (noq   )         !< delwaq flows
      character(20), intent(in   ) :: syname (notot )         !< names of sumstances
      integer  (ip), intent(in   ) :: nosfun                  !< number of segment functions
      character(20), intent(in   ) :: sfname (nosfun)         !< names of segment functions
      real     ( 4), intent(in   ) :: segfun (noseg ,nosfun)  !< segment function values
      real     ( 4), intent(inout) :: amass  (notot ,noseg )  !< delwaq mass array
      real     ( 4), intent(inout) :: conc   (notot ,noseg )  !< delwaq conc array
      integer   (4), intent(in   ) :: iaflag                  !< if 1 then accumulation of balances
      integer   (4), intent(in   ) :: intopt                  !< integration suboptions
      integer   (4), intent(in   ) :: ndmps                   !< number of dumped volumes for balances
      integer   (4), intent(in   ) :: isdmp  (noseg )         !< volume to dump-location pointer
      real      (4), intent(inout) :: dmps   (notot ,ndmps,*) !< dumped segment fluxes if INTOPT > 7
      real      (4), intent(inout) :: amass2 (notot , 5 )     !< mass balance array


!     Locals

      integer(ip) lunut             !  output unit number
      integer     lunpr
      integer( 4) indx              !  index in segment names
      integer( 4) ioff              !  offset in substances array
      integer( 4) isys              !  loop counter substances
      logical     :: first  = .true.
      integer(ip), save :: idtimd , itimd1 , itimd2     ! timings of the vertical diffusion file
      integer(ip), save :: idtimt , itimt1 , itimt2     ! timings of the tau file
      integer(ip), save :: idtims , itims1 , itims2     ! timings of the salinity file
      integer(ip), save :: idtimtm , itimtm1 , itimtm2     ! timings of the temperature file
      integer(ip), save :: ifflag , isflag
      logical    , save :: updatd
      integer(ip) nosubud
      integer(ip) iseg, i, i2, ipart
      real   (rp) depmin
      real(sp) :: densty  ! AddedDana
      logical     update
      integer     iniday
      integer  :: lures
      integer(4)  ithandl /0/

      if ( alone ) return
      if ( timon ) call timstrt ( "delpar01", ithandl )
      lunut = lunitp(2)

!           this replaces the call to rdhydr
      if ( modtyp .eq.model_abm ) then
        if(idp_file .ne. ' ' .and. itime .eq. nint(const(8))*86400 ) then ! release at day iniday - specific for ABM module (modtyp=model_abm) FMK 13-2-2017
            call partini( nopart, nosubs, idp_file, wpart  , xpart ,
     &                    ypart , zpart , npart   , mpart  , kpart ,
     &                    iptime, lunpr )
        endif
      endif

      do i=1, noseg
        volumep(cellpntp(i)) = dwqvol(i)
      enddo

      flow = 0.0
      do i = 1, noqp
         if ( flowpntp(i,1) .gt. 0 ) flow(flowpntp(i,1)) = flow(flowpntp(i,1)) + dwqflo(i)
         if ( flowpntp(i,2) .gt. 0 ) flow(flowpntp(i,2)) = flow(flowpntp(i,2)) + dwqflo(i)
      enddo
      depmin = (0.05*nmaxp*mmaxp)/mnmaxk
      depmin = max(depmin,0.001)
      do iseg = 1, mnmaxk        !       limit volume to depmin
         i2 = mod(iseg-1,nmaxp*mmaxp) + 1
         volumep(iseg) = max(volumep(iseg), area(i2) * depmin)
      enddo
      if ( first ) then
          ifflag = 1
      else
          ifflag = 0
      endif
      if ( lsettl .or. layt .gt. 1 ) then
         call zoek20 ( 'TAU       ', nosfun, sfname, 10, indx )
         if ( indx .gt. 0 ) then
            do i=1, noseg
               tau(cellpntp(i)) = segfun(i,indx)
            enddo
         else if ( lunitp(21) .gt. 0 ) then
            call dlwqbl ( lunitp(21), lunut   , itime     , idtimt  , itimt1  ,
     &                    itimt2    , ihdel   , noseg     , mnmaxk  , tau1    ,
     &                    tau       , cellpntp, fnamep(21), isflag  , ifflag  ,
     &                    updatd    )
         endif
         if ( layt .gt. 1 ) then
            call zoek20 ( 'VERTDISP  ', nosfun, sfname, 10, indx )
            if ( indx .gt. 0 ) then
               do i=1, noseg
                  vdiff(cellpntp(i)) = segfun(i,indx)
               enddo
            else if ( lunitp(20) .gt. 0 ) then
               call dlwqbl ( lunitp(20), lunut   , itime     , idtimd  , itimd1  ,
     &                       itimd2    , ihdel   , noseg     , mnmaxk  , vdiff1  ,
     &                       vdiff     , cellpntp, fnamep(20), isflag  , ifflag  ,
     &                       updatd    )
               if ( layt .gt. 1 ) then                              ! fill the zero last layer with the
                  vdiff(mnmaxk-  nmaxp*mmaxp+1:mnmaxk           ) =   ! values above
     &            vdiff(mnmaxk-2*nmaxp*mmaxp+1:mnmaxk-nmaxp*mmaxp )
               endif
            else
               vdiff = 0.0
            endif
         endif

         ! for salinity and temperature
         !.. salinity

         call zoek20 ( 'SALINITY  ', nosfun, sfname, 10, indx )
         if ( indx .gt. 0 ) then
            do i=1, noseg
              salin(cellpntp(i)) = segfun(i,indx)
            enddo
         else if ( lunitp(22) .gt. 0 ) then
         call dlwqbl ( lunitp(22), lunut   , itime    , idtims , itims1 ,
     &                 itims2   , ihdel   , noseg    , mnmaxk , salin1 ,
     &                 salin    , cellpntp , fnamep(22), isflag , ifflag ,
     &                 updatd   )
         endif


!.. temperature

         call zoek20 ( 'TEMP      ', nosfun, sfname, 10, indx )
         if ( indx .gt. 0 ) then
            do i=1, noseg
              temper(cellpntp(i)) = segfun(i,indx)
            enddo
         else if ( lunitp(23) .gt. 0 ) then
         call dlwqbl ( lunitp(23), lunut   , itime    , idtimtm , itimtm1 ,
     &                 itimtm2   , ihdel   , noseg    , mnmaxk , temper1,
     &                 temper   , cellpntp , fnamep(23), isflag , ifflag ,
     &                 updatd   )
        endif
      endif
      first = .false.
      if ( lunitp(22) .gt. 0 .and. lunitp(23) .gt. 0 ) then
          do i = 1, noseg
            rhowatc(i) = densty(max(0.0e0,salin1(i)), temper1(i))
          enddo
      else
          do i = 1, noseg
            rhowatc(i) = rhow
          enddo
      endif

!     Taking over of aged particles by Delwaq
! first for oil model
      if (modtyp.eq.model_oil) then
        call oil2waq( nopart , nosys    , notot    , nosubs   , noseg    ,
     &              nolay    , dwqvol   , surface  , nmaxp    , mmaxp    ,
     &              lgrid3   , syname   , itime    , iddtim   , npwndw   ,
     &              iptime   , npart    , mpart    , kpart    , wpart    ,
     &              amass    , conc     , iaflag   , intopt   , ndmps    ,
     &              isdmp    , dmps     , amass2   )
      else
        call par2waq( nopart , nosys    , notot    , nosubs   , noseg    ,
     &              nolay    , dwqvol   , surface  , nmaxp    , mmaxp    ,
     &              lgrid3   , syname   , itime    , iddtim   , npwndw   ,
     &              iptime   , npart    , mpart    , kpart    , wpart    ,
     &              amass    , conc     , iaflag   , intopt   , ndmps    ,
     &              isdmp    , dmps     , amass2   )
      endif

!     Echo actual time to screen

      write ( *, 1020) itime  /86400, mod(itime  , 86400)/3600, mod(itime  , 3600)/60, mod(itime  , 60),
     &                 itstopp/86400, mod(itstopp, 86400)/3600, mod(itstopp, 3600)/60, mod(itstopp, 60),
     &                 nopart - npwndw + 1, npmax
      write (lunut, '(/a)')
     &  '----------------------------------------------------------------------------------'
      write (lunut, 1020 ) itime  /86400, mod(itime  , 86400)/3600, mod(itime  , 3600)/60, mod(itime  , 60),
     &                     itstopp/86400, mod(itstopp, 86400)/3600, mod(itstopp, 3600)/60, mod(itstopp, 60),
     &                     nopart - npwndw + 1, npmax

!     Part15 adapts wind and direction for actual time

      call part15 ( lunut    , itime    , spawnd   , mnmax2   , nowind   ,
     &              iwndtm   , wveloa   , wdira    , wvelo    , wdir     )

!     Part12 makes .map files, binary and Nefis versions

      call part12 ( lunitp(8), fnamep(8), lunut    , title    , subst2   ,
     &              lgrid    , lgrid2   , lgrid3   , nmaxp    , mmaxp    ,
     &              concp    , volumep  , npart    , mpart    , wpart    ,
     &              nopart   , itime    , idelt    , icwsta   , icwsto   ,
     &              icwste   , atotal   , npwndw   , kpart    , pblay    ,
     &              iptime   , npwndn   , modtyp   , nosubs   , noslay   ,
     &              iyear    , imonth   , iofset   , pg(1)    , rbuffr   ,
     &              nosta    , mnmax2   , noseglp  , isfile   , mapsub   ,
     &              layt     , area     , nfract   , lsettl   , mstick   ,
     &              elt_names, elt_types, elt_dims , elt_bytes, locdep   ,
     &              nosub_max, bufsize  )
      ioff = notot - nosubs
      do iseg = 1, noseg            !  give Part concentrations to Waq
         do isys = 1, nosubs
            conc ( ioff+isys, iseg ) = concp( isys, iseg )
            amass( ioff+isys, iseg ) = concp( isys, iseg ) * dwqvol( iseg )
         enddo
      enddo

!     Part13 makes 3d detail plot grids corrected for recovery rate

      call part13 ( lunitp(9), fnamep(9), lunut    , title    , subst2   ,
     &              lgrid2   , nmaxp    , volumep  , area     , npart    ,
     &              mpart    , xpart    , ypart    , wpart    , nopart   ,
     &              itime    , idelt    , ipset    , iptset   , xa       ,
     &              ya       , xb       , yb       , pg(1)    , recovr   ,
     &              atotal   , iyear    , imonth   , iofset   , npwndw   ,
     &              lgrid    , pblay    , modtyp   , apeak    , adepth   ,
     &              noslay   , nosubs   , rbuffr   , kpart    , itrack   ,
     &              nplot    , mapsub   , ntrack   , isfile   , mmaxp    ,
     &              nfract   , lsettl   , mstick   , elt_names, elt_types,
     &              elt_dims , elt_bytes, locdep   , zpart    , za       ,
     &              dpsp     , tcktot   , nosub_max, bufsize  )

!     Parths makes 2D averaged time histories every ihstep

      call parths ( lunitp(13),lunut    , title    , subst    , mmaxp    ,
     &              lgrid2   , nmaxp    , volumep  , area     , npart    ,
     &              mpart    , xpart    , ypart    , wpart    , nopart   ,
     &              itime    , idelt    , xa       , npwndw   , lgrid    ,
     &              ya       , xb       , yb       , pg(1)    , pblay    ,
     &              modtyp   , noslay   , nosubs   , concp    , chismp   ,
     &              chispl   , nosta    , nmstat   , xstat    , ystat    ,
     &              nstat    , mstat    , nplsta   , mplsta   , ihstrtp  ,
     &              ihstopp  , ihstepp  , ihplot   , fnamep(13),kpart    ,
     &              mnmax2   , noseglp  , nfract   , lsettl   , mstick   ,
     &              elt_names, elt_types, elt_dims , elt_bytes, rbuffr   ,
     &              zpart    , za       , locdep   , dpsp     , tcktot   ,
     &              lgrid3   )

      if ( itime .ge. itstopp ) then

!       Write the restart files when needed
        if (write_restart_file) then
           call write_part_restart_file()
        endif

        goto 9999 ! <=== here the simulation loop ends
      endif


!     Part03 computes velocities and depth

      call part03 ( lgrid     , volumep   , flow      , dx       , dy       ,
     &              nmaxp     , mmaxp     , mnmaxk    , lgrid2   , velo     ,
     &              layt      , area      , depth     , dpsp     , locdep   ,
     &              zlevel    , zmodel    , laytop    , laytopp  , laybot   ,
     &              pagrid    , aagrid    , tcktot    , ltrack   , flow2m   ,
     &              lgrid3    , vol1      , vol2      , vel1     , vel2     )

!     This section does water quality processes

      select case ( modtyp )

         case ( 1 )     ! = conservative tracer model

         case ( 2, 5 )  ! = temperature model
            call partwq ( lgrid    , nmaxp    , concp    , volumep  , area     ,
     &                    npart    , mpart    , wpart    , radius   , nodye    ,
     &                    npwndw   , nopart   , idelt    , velo     , wvelo    ,
     &                    const    , noconsp  , ptlay    , lunut    , nosubs   ,
     &                    nolayp   , lgrid2   , mmaxp    , xb       , yb       ,
     &                    t0cf     , acf      , nwaste   , mwaste   , kpart    ,
     &                    mapsub   , layt     , mnmaxk   )

         case ( 3 )     ! = obsolete

         case ( 4 )     ! = oil model
            call oildsp ( lgrid    , nmaxp    , concp    , volumep  , area     ,
     &                    npart    , mpart    , wpart    , radius   , nodye    ,
     &                    npwndw   , nopart   , itime    , idelt    , wvelo    ,
     &                    const    , lunut    , nosubs   , noslay   , lgrid2   ,
     &                    lgrid3   ,
     &                    mmaxp    , xb       , yb       , kpart    , mapsub   ,
     &                    isfile   , nfract   , mstick   , nstick   , fstick   ,
     &                    xa       , ya       , pg(1)    , lsettl   , xpart    ,
     &                    ypart    , zpart    , za       , locdep   , dpsp     ,
     &                    tcktot   , substi   ,            npmax    , rhow     ,
     &                    amassd   , ioptrad  , ndisapp  , idisset  , tydisp   ,
     &                    efdisp   , xpoldis  , ypoldis  , nrowsdis , wpartini ,
     &                    iptime)

         case ( 7 )     ! = abm model
              if ( mod(itime,86400) .eq. 0 ) then !jvb for output within abm module this is a temporary hack
                 call part11 ( lgrid , xb       , yb       , nmaxp    , npart,
     &                         mpart  , xpart    , ypart    , xa       , ya   ,
     &                         nopart , npwndw   , lgrid2   , kpart    , zpart,
     &                         za     , locdep   , dpsp     , layt     , mmaxp,
     &                         tcktot   )
              endif
               call abm( lunut   , itime    , idelt    , nmaxp    , mmaxp    ,
     &                   layt     , noseglp  , nolayp   , mnmaxk   , lgrid    ,
     &                   lgrid2   , lgrid3   , nopart   , npwndw   , nosubs   ,
     &                   npart    , mpart    , kpart    , xpart    , ypart    ,
     &                   zpart    , wpart    , iptime   , wsettl   , locdep   ,
     &                   noconsp  , const    , concp    , xa       , ya       ,
     &                   angle    , vol1     , vol2     , flow     , depth    ,
     &                   vdiff1   , salin1   , temper1  , v_swim   , d_swim   ,
     &                   itstrtp  , vel1     , vel2     , abmmt    , abmsd    ,
     &                   chronrev , selstage , zmodel   , laybot   , laytop   )
         end select

!     two-layer system with stratification

      if ( modtyp .eq. model_two_layer_temp )
     &   call part18 ( lgrid    , velo     , concp    , flres    , volumep  ,
     &                 area     , mnmaxk   , npart    , mpart    , wpart    ,
     &                 zpart    , nopart   , idelt    , nolayp   , npwndw   ,
     &                 vdiff    , pblay    , ptlay    , const    , noconsp  ,
     &                 lunut    , nosubs   , layt     , kpart    , mapsub(1),
     &                 wvelo    , alpha    , nosubc   , mapsub(2) )

!      add dye release

      if ( nodye .gt. 0 )
     &   call part09 ( lunut    , itime    , nodye    , nwaste   , mwaste   ,
     &                 xwaste   , ywaste   , iwtime   , amassd   , aconc    ,
     &                 npart    , mpart    , xpart    , ypart    , zpart    ,
     &                 wpart    , iptime   , nopart   , radius   , nrowswaste,
     &                 xpolwaste           , ypolwaste           , lgrid    ,
     &                 lgrid2   , nmaxp    , mmaxp    , xb       , yb       ,
     &                 dx       , dy       , ndprt    , nosubs   , kpart    ,
     &                 layt     , tcktot   , zmodel   , laytop   , laybot   ,
     &                 nplay    , kwaste   , nolayp   ,
     &                 modtyp   , zwaste   , track    , nmdyer   , substi   ,
     &                 rhopart)


!      add continuous release

       if ( nocont .gt. 0 )
     &    call part14 ( itime    , idelt    , nodye    , nocont   , ictime   ,
     &                 ictmax   , nwaste   , mwaste   , xwaste   , ywaste   ,
     &                 zwaste   , aconc    , rem      , npart    , ndprt    ,
     &                 mpart    , xpart    , ypart    , zpart    , wpart    ,
     &                 iptime   , nopart   , pblay    , radius   , nrowswaste,
     &                 xpolwaste           , ypolwaste           , lgrid    ,
     &                 lgrid2   , nmaxp    , mmaxp    , xb       , yb       ,
     &                 dx       , dy       , ftime    , tmassu   , nosubs   ,
     &                 ncheck   , t0buoy   , modtyp   , abuoy    , t0cf     ,
     &                 acf      , lunut    , kpart    , layt     , tcktot   ,
     &                 zmodel   , laytop   , laybot   , nplay    , kwaste   ,
     &                 nolayp   , linear   , track    ,
     &                 nmconr   , spart    , rhopart  , noconsp  , const   )

!     write particle tracks

      if (ltrack.and.itime.eq.(itstrtp+idelt*itrakc-idelt)) then
         ! get the absolute x,y,z's of the particles
         call part11 ( lgrid    , xb       , yb       , nmaxp    , npart    ,
     &                 mpart    , xpart    , ypart    , xa       , ya       ,
     &                 nopart   , npwndw   , lgrid2   , kpart    , zpart    ,
     &                 za       , locdep   , dpsp     , nolayp   , mmaxp    ,
     &                 tcktot   )
!           write actual particle tracks (file #16)
         if (itime.eq.(itstrtp+idelt*itrakc-idelt)) then
            call wrttrk ( lunut   , fout     , fnamep(16), itrakc   , nopart  ,
     &                    npmax    , xa       , ya       , za       , xyztrk  )
            itrakc = itrakc + itraki
         endif
      endif

      if ( noudef .gt. 0 )  then

!       add release in a way defined by the user
!       array isub contains references to substances

         call partur (  itime    , noudef   , iutime   , mpart    , npart    ,
     &                  kpart    , xpart    , ypart    , zpart    , wpart    ,
     &                  iptime   , nopart   , lgrid    , nmaxp    , mmaxp    ,
     &                  tmasud   , ipntp    , substi   , nosubs   , nolayp   ,
     &                  nocont   , ndprt    , nodye    , lunut    , rbuffr   ,
     &                  volumep  , aconud   , uscal    , isub     , finud    ,
     &                  iftime   , ifopt    , nosyss   , isfud    , nosubud  ,
     &                  subsud   )

      endif

!     calculate the settling velocities on a refined grid
!                        (NB: this routine is NOT part of any test in the testbench)
      if ( anfac .ne. 0.0 ) then
         call part21 ( lunut    , lgrid    , lgrid2   , xb       , yb       ,
     &                 area     , volumep  , nmaxp    , mmaxp    , noslay   ,
     &                 nosubs   , nopart   , npart    , mpart    , kpart    ,
     &                 xpart    , ypart    , zpart    , wpart    , npwndw   ,
     &                 pg(1)    , amapsett , xa       , ya       , za       ,
     &                 atotal   , apeak    , adepth   , imap     , nplay    ,
     &                 wsettl   , irfac    , anfac    , lsettl   , locdep   ,
     &                 tcktot   , dpsp     )
      else
! jvb removed this line (commented, so execute if not modtyp=model_abm?
           if (modtyp.ne.model_abm) wsettl = 1.0  ! whole array assignment
         endif
! jvb removed call to partvs (commented, so execute if not modtyp=model_abm?
         if (modtyp.ne.model_abm) call partvs ( lunut   , itime    , nosubs   , nopart   , ivtset   ,
     &              ivtime   , vsfour   , vsfact   , wpart    , wsettl   ,
     &              modtyp   , nmaxp    , mmaxp    , lgrid3   , noslay   ,
     &              npart    , mpart    , kpart    , nosegp   , noseglp  ,
     &              rhopart  , rhowatc  , spart    , iptime)

!      calculate actual decaycoefficient

      if ( idtset .gt. 0 )
     &call part17 ( itime    , nosubs   , idtset   , idtime   , decay    ,
     &              decays   )



 9999 if ( timon ) call timstop ( ithandl )
      return

!     formats

 1010 format( '  Start  time :', i4.4 ,'D-', i2.2 ,'H-', i2.2 , 'M-', i2.2 ,'S.'/
     &        '  Stop   time :', i4.4 ,'D-', i2.2 ,'H-', i2.2 , 'M-', i2.2 ,'S.'// )
 1020 format( '  Time ', i4.4 ,'D-', i2.2 ,'H-', i2.2 ,'M-', i2.2 ,'S.',' Stop time ',
     &          i4.4 ,'D-', i2.2 ,'H-', i2.2 ,'M-', i2.2 ,'S.', i11,' part. (of',i11,')')
      end
