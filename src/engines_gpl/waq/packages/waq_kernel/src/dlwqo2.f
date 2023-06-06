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

      subroutine dlwqo2 ( notot , noseg , nopa  , nosfun, itime ,
     +                    moname, syname, duname, idump , nodump,
     +                    conc  , cons  , param , func  , segfun,
     +                    volume, nocons, nofun , idt   , noutp ,
     +                    lchar , lun   , ioutps, iopoin, riobuf,
     +                    ousnm , ouuni , oudsc , sysnm , syuni , sydsc ,
     +                    ounam , nx    , ny    , lgrid , cgrid ,
     +                    nosys , bound , ip    , amass , amass2,
     +                    asmass, noflux, flxint, isflag, iaflag,
     +                    ibflag, imstrt, imstop, imstep, idstrt,
     +                    idstop, idstep, ihstrt, ihstop, ihstep,
     +                    imflag, idflag, ihflag, noloc , proloc,
     +                    nodef , defaul, itstrt, itstop, ndmpar,
     +                    danam , ndmpq , ndmps , iqdmp , isdmp ,
     +                    ipdmp , dmpq  , dmps  , flxdmp, ntdmpq,
     +                    nambuf, noraai, ntraaq, ioraai, nqraai,
     +                    iqraai, trraai, ranam , stochi, nogrid,
     +                    novar , vararr, varidx, vartda, vardag,
     +                    arrknd, arrpoi, arrdm1, arrdm2, vgrset,
     +                    grdnos, grdseg, a     , nobnd , nobtyp,
     +                    bndtyp, inwtyp, coname, noq   , ipoint,
     +                    intopt, paname, funame, sfname, dmpbal,
     +                    nowst , nowtyp, wsttyp, iwaste, inxtyp,
     +                    wstdmp, iknmrk, isegcol)
!
!     Created             : january 1993 Jan van Beek
!
!
!     Function            : Driver output system
!
!     Subroutines called  : baldmp, fills balance for dump area's
!                           fioutv, fills output buffer, single cel grids
!                           fiosub, fills output buffer, sub-grids
!                           outmon, performs a monitor output step
!                           outdmp, performs a grid dump output step
!                           outhis, performs a history output step
!                           outhnf, performs a history NEFIS step
!                           outmap, performs a map output step
!                           outmnf, performs a map NEFIS step
!                           outbal, performs a balance output step
!                           raatra, fills transport for raaien
!                           stepyn, evaluates timers
!
!     Parameters
!
!     Name    Kind     Length     Funct.  Description
!     ----    -----    ------     ------- -----------
!     notot   integer       1     input   Total number of substances
!     noseg   integer       1     input   Nr. of computational elements
!     nopa    integer       1     input   Number of parameters
!     nosfun  integer       1     input   Number of segment functions
!     itime   integer       1     input   Time in system clock units
!     moname  char*40       4     input   Model and run names
!     syname  char*20    notot    input   names of substances
!     duname  char*20    nodump   input   names of dump locations
!     idump   integer    nodump   input   dump segment numbers
!     nodump  integer       1     input   number of dump locations
!     conc    real   notot,noseg  input   Model concentrations
!     cons    real          *     in/out  Model constants
!     param   real    nopa,noseg  in/out  Model parameters
!     func    real          *     in/out  Model functions at ITIME
!     segfun  real   noseg,nosfun in/out  Segment functions at ITIME
!     volume  real      noseg     input   Segment volumes
!     nocons  integer       1     input   Number of constants used
!     nofun   integer       1     input   Number of functions ( user )
!     idt     integer       1     input   Simulation timestep
!     noutp   integer       1     input   Number of output files
!     lchar   char*(*)      *     input   File names
!     lun     integer       *     input   Uint numbers
!     ioutps  integer 7*noutp    in/out   Output structure
!                                            index 1 = start time
!                                            index 2 = stop time
!                                            index 3 = time step
!                                            index 4 = number of vars
!                                            index 5 = kind of output
!                                            index 6 = grid of output
!                                            index 7 = initialize flag
!     iopoin  integer       *     input   Pointer to DELWAQ array's
!     riobuf  real          *     local   Output buffer
!     ounam   char*20       *     input   name of output variable
!     nx      integer       1     input   Width of output grid
!     ny      integer       1     input   Depth of output grid
!     lgrid   integer     nx*ny   input   grid-layout
!     cgrid   char*20       *     local   Char buffer for dmp output
!     nosys   integer       1     input   Number of active substances
!     bound   real          *     input   Bounary conditions
!     ip      integer       *     in/out  Paging structure
!     amass   real       notot,*  input   Mass array
!     amass2  real       notot,*  in/out  Cummulative balance on whole
!     asmass  real       notot,*  in/out  Cummulative balance per segment
!     noflux  integer       1     input   Number of fluxes
!     flxint  real  noflux*ndmpar in/out  Integrated fluxes at dump segments
!     isflag  integer       1     input   if 1 then dd-hh:mm'ss"
!     iaflag  integer       1     output  if 1 then accumulate mass bal
!     ibflag  integer       1     input   Flag = 1 then balances
!     imstrt  integer       1     input   Monitoring start time ( scu )
!     imstop  integer       1     input   Monitoring stop time ( scu )
!     imstep  integer       1     input   Monitoring time step ( scu )
!     idstrt  integer       1     input   Dump start time ( scu )
!     idstop  integer       1     input   Dump stop time ( scu )
!     idstep  integer       1     input   Dump time step ( scu )
!     ihstrt  integer       1     input   History start time ( scu )
!     ihstop  integer       1     input   History stop time ( scu )
!     ihstep  integer       1     input   History time step ( scu )
!     imflag  logical       1     output  If .T. then monitor step
!     idflag  logical       1     output  If .T. then dump step
!     ihflag  logical       1     output  If .T. then history step
!     noloc   integer       1     input   Number of variables in PROLOC
!     param   real   noloc,noseg  input   Parameters local in PROCES system
!     nodef   integer       1     input   Number of used defaults
!     defaul  real          *     input   Default proces parameters
!     itstrt  integer     1       input   start time
!     itstop  integer     1       input   stop time
!     ndmpar  integer     1       input   Number of dump areas
!     danam   char*20  ndmpar     input   Dump area names
!     ndmpq   integer     1       input   Number of dumped exchanges
!     ndmps   integer     1       input   Number of dumped segments
!     iqdmp   integer       *     input   Exchange to dumped exchange pointer
!     isdmp   integer       *     input   Segment to dumped segment pointer
!     ipdmp   integer       *     input   pointer structure dump area's
!     dmpq    real  notot*ndmps*? input   mass balance dumped segments
!     dmps    real  nosys*ndmpq*? input   mass balance dumped exchange
!     flxdmp  real  noflux*ndmps  input   Integrated fluxes
!     nambuf  char*20       *     input   Buffer for names
!     noraai  integer       1     input   Number of raaien
!     ntraaq  integer       1     input   Total number of exch. in raaien
!     ioraai  integer       *     input   Output option for raai
!     nqraai  integer       *     input   Number of exchanges in raai
!     iqraai  integer       *     input   Exchanges in raai
!     trraai  real notot*ndmpar*6 in/out  Cummulative transport over raai
!     ranam   char*20       *     input   Raaien names
!     stochi  real   notot*noflux input   Proces stochiometry
!     intopt  integer     1       input   Integration and balance suboptions
!     ==================================================================
!
      use m_zero
      use m_srstop
      use m_getcom
      use timers
      use precision
      use output
      use nan_check_module
      use m_outmnc
      implicit none
!
      integer       notot , noseg , nopa  , nosfun, itime ,
     +              nodump, nocons, nofun , idt   , noutp ,
     +              nx    , ny    , nosys , noflux, isflag,
     +              iaflag, ibflag, imstrt, imstop, imstep,
     +              idstrt, idstop, idstep, ihstrt, ihstop,
     +              ihstep, noloc , nodef , itstrt, itstop,
     +              ndmpar, ndmpq , ndmps , ntdmpq, noraai,
     +              ntraaq, nogrid, novar , nobnd , nobtyp,
     +              noq  
      integer       idump(*)      , lun(*)        ,
     +              ioutps(7,*)   , iopoin(*)     ,
     +              lgrid(*)      , ip(*)         ,
     +              iqdmp(*)      , isdmp(*)      ,
     +              ipdmp(*)      , ioraai(*)     ,
     +              nqraai(*)     , iqraai(*)     ,
     +              vararr(novar) , varidx(novar) ,
     +              vartda(novar) , vardag(novar) ,
     +              arrknd(*)     , arrpoi(*)     ,
     +              arrdm1(*)     , arrdm2(*)     ,
     +              vgrset(novar,*),grdnos(nogrid),
     +              grdseg(noseg,nogrid)          ,
     +              inwtyp(*)     , ipoint( 4,noq)
      integer(4), intent(in   ) :: iknmrk(noseg)      ! Feature array. Bit zero set means active.
      real          conc ( notot, noseg ),
     &                              cons(*)       ,
     &              param( nopa , noseg ),
     &                              func(*)       ,
     &              segfun(noseg, nosfun),
     &                              volume(*)     ,
     +              riobuf(*)     , bound(*)      ,
     +              amass( notot, noseg ),
     &                              amass2(notot,5),
     +              asmass(*)     , flxint(*)     ,
     +              proloc(*)     , defaul(*)     ,
     +              dmpq(*)       , dmps(*)       ,
     +              flxdmp(*)     , trraai(nosys,*),
     +              stochi(notot,noflux), a(*)
      character*20  syname(*)     , duname(*)     ,
     +              ounam(*)      , cgrid(*)      ,
     +              danam(*)      , nambuf(*)     ,
     +              ranam(*)      , bndtyp(*)     ,
     +              coname(*)     , paname(*)     ,
     +              funame(*)     , sfname(*)
      character*100 ousnm(*)      , sysnm(*)
      character*40  ouuni(*)      , syuni(*)
      character*60  oudsc(*)      , sydsc(*)

      character*100, allocatable :: hnc_standard(:)
      character*40, allocatable  :: hnc_unit(:)
      character*60, allocatable  :: hnc_description(:)

      character*40  moname(4)
      character*(*) lchar (*)
      logical       imflag, idflag, ihflag
      integer                    :: dmpbal(ndmpar)        ! indicates if dump area is included in the balance
      integer                    :: nowst                 ! number of wasteloads
      integer                    :: nowtyp                ! number of wasteload types
      character(len=20)          :: wsttyp(nowtyp)        ! wasteload types names
      integer                    :: iwaste(nowst)         ! segment numbers of the wasteloads
      integer                    :: inxtyp(nowst)         ! wasteload type number (index in wsttyp)
      real                       :: wstdmp(notot,nowst,2) ! accumulated wasteloads 1/2 in and out
      integer, intent(in   )     :: isegcol(*)            ! pointer from segment to top of column
!
!     Local declarations
!
      integer, parameter  :: igseg = 1
      integer, parameter  :: igmon = 2
      integer, parameter  :: iggrd = 3
      integer, parameter  :: igsub = 4
      integer, parameter  :: luoff = 18
      integer, parameter  :: luoff2= 36
      integer       k1    , iostrt, iostop, iostep, nrvar ,
     +              isrtou, igrdou, iniout, lunout, iout  ,
     +              ierr  , ierr2 , i     , i1    , i2    ,
     +              ifi   , ncout , nrvar2, nrvar3, ip1   ,
     +              iof   , nsegou, intopt

      character*255 lchout
      character*20  name
      logical       loflag, lmfirs, ldfirs, lhfirs, ldummy, lnonans
      logical       lget  , lread
      real, allocatable  :: surf(:)
      integer            :: idummy       ! dummy not used
      real               :: rdummy       ! dummy not used
      character(len=256) :: adummy       ! dummy not used
      logical            :: lfound       ! Keyword found (or not)
      logical, save      :: lnancheck    ! Do check on NAN in conc array

      integer, save ::       mncrec = 0                            ! netCDF map
      integer, save ::       hncrec = 0                            ! netCDF history
      integer, save ::       timeid, bndtimeid                     ! netCDF map
      integer, save ::       timeidh, bndtimeidh                   ! netCDF history
      integer, allocatable, save ::  mncwqid1(:,:), mncwqid2(:,:)  ! netCDF map
      integer, allocatable, save ::  hncwqid1(:,:), hncwqid2(:,:)  ! netCDF history

      logical, save ::       first = .true.

      real(hp)           :: damass2(notot,5)

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqo2", ithandl )

      if (first) then
         allocate(mncwqid1(notot,3) , mncwqid2(novar,3))
         allocate(hncwqid1(notot,2) , hncwqid2(novar,2))
!        allow switching of NAN concentrations check
         call getcom ( '-nonancheck', 0, lfound, idummy, rdummy, adummy, ierr2)
         lnancheck = .not. lfound
         first = .false.
      endif

      if (lnancheck) then
!        Check for NANs in the concentration array
         lunout = lun(19)
         lnonans = nan_check(conc, 'conc(notot, noseg)', lunout, 1, 1)
         if ( .not. lnonans ) then
            write(lunout,'(/A)')  '  ERROR : NAN found the concentration array, ending calculation.'
            write(*     ,'(/A)')  '  ERROR : NAN found the concentration array, ending calculation. See location in mon-file.'
            write(lunout,'(A)')   '          Current concentration fields written to _res.map.'
            write(*     ,'(A)')   '          Current concentration fields written to _res.map.'
            write(lunout,'(/A/)') '  INFO  : If you don''t want NAN checks, use -nonancheck at command line.'
            write(*     ,'(/A/)') '  INFO  : If you don''t want NAN checks, use -nonancheck at command line.'
            call dlwq13 (lun, lchar, conc, itime, moname, syname, notot, noseg)
            call srstop(1)
         endif
      endif
!
!     Evaluate standard DELWAQ output timers
!
      call stepyn (itime , idt   , imstrt, imstop, imstep,
     +             imflag, lmfirs)
      call stepyn (itime , idt   , idstrt, idstop, idstep,
     +             idflag, ldfirs)
      call stepyn (itime , idt   , ihstrt, ihstop, ihstep,
     +             ihflag, lhfirs)
!
!     Fill mass in AMASS2 array by summing AMASS over all segments
!
      if ( imflag ) then
         damass2 = amass2
         iaflag = 1
         do i2 = 1,notot
            amass2(i2,1) = 0.0
            do i1 = 1,noseg
               damass2(i2,1) = damass2(i2,1) + amass(i2,i1)
            enddo
         enddo
         amass2 = damass2
      endif
!
!     Fill mass in ASMASS array using DMPQ and DMPS
!
      if ( imflag .or. ( ihflag .and. noraai .gt. 0) ) then
         if ( ibflag .eq. 1 ) then
            call baldmp (notot , nosys , noflux, ndmpar, ndmpq ,
     +                      ndmps , ntdmpq, iqdmp , isdmp , ipdmp ,
     +                      dmpq  , amass , dmps  , flxdmp, asmass,
     +                      flxint)
         endif

         if ( noraai .gt. 0 ) then
            if ( lhfirs ) then
               call zero   (trraai, noraai*nosys  )
            else
               call raatra (nosys , ndmpq , noraai, ntraaq, ioraai,
     +                         nqraai, iqraai, iqdmp , dmpq  , trraai)
            endif
         endif
!
      endif
!
!     Initialize K1, pointer in IOPOIN and OUNAM
!
      lread = .true.
      k1 = 1
!
!     Loop over the output files
!
      do 200 iout = 1 , noutp
!
!        Map output structure to single variables part 1
!
         iostrt = ioutps(1,iout)
         iostop = ioutps(2,iout)
         iostep = ioutps(3,iout)
         nrvar  = ioutps(4,iout)
!
!        Output required ?
!
         call stepyn (itime , idt   , iostrt, iostop, iostep,
     +                loflag, ldummy)
!
         if ( .not. loflag ) goto 100
!
!        Map output structure to single variables part 2
!
            isrtou = ioutps(5,iout)
            igrdou = ioutps(6,iout)
            iniout = ioutps(7,iout)
            if ( iout .le. 4 ) then
               ifi = iout + luoff
            elseif ( iout .le. 7 ) then
               ifi = iout + luoff2 - 4
            else
               ifi = iout + luoff2 - 2
            endif
            lunout = lun(ifi)
            lchout = lchar(ifi)
!
!        No balance output if they are not active
!
            if ( ( isrtou .eq. ibal .or. isrtou .eq. iba2 .or.
     +             isrtou .eq. iba2) .and. ibflag .ne. 1 ) goto 100
!
!        Set all local variables used active on base grid
!
            call actloc (iopoin, nrvar , nocons, nopa  , nofun ,
     +                   nosfun, notot , noseg , noloc , nogrid,
     +                   novar , vararr, varidx, vartda, vardag,
     +                   arrknd, arrpoi, arrdm1, arrdm2, vgrset,
     +                   grdnos, grdseg, a     )
!
!        Fill output buffer
!
            if ( isrtou .eq. iba2 ) then
!
               call flxbal (notot , noflux, ndmpar, nrvar , stochi,
     +                      flxint, asmass, riobuf)
!
            elseif ( isrtou .eq. iba3 ) then
!     jos doet het zelf
            elseif ( igrdou .eq. igsub ) then
               if (isrtou .eq. imo3 .or.
     +             isrtou .eq. ihi3 .or.
     +             isrtou .eq. ihnc3 .or.
     +             isrtou .eq. ihn3     ) then
                  ncout = notot
               else
                  ncout = 0
               endif
               nrvar2 = nrvar/2
!
!           For the dump area's
!
               call fiosub (riobuf, iopoin(k1), nrvar2, nocons, nopa  ,
     +                      nofun , nosfun    , notot , conc  , segfun,
     +                      func  , param     , cons  , idt   , itime ,
     +                      volume, noseg     , nosys , ndmpar, ipdmp ,
     +                      bound , noloc     , proloc, nodef , defaul,
     +                      ncout , ntdmpq    , paname, sfname, funame,
     +                      danam )
!
!           For the raaien
!
               if ((isrtou .eq. ihi3 .or.
     +              isrtou .eq. ihnc3 .or.
     +              isrtou .eq. ihn3     ) .and.
     +              noraai .gt. 0               ) then
                  nrvar3 = notot + nrvar2
                  ip1 = (ncout+nrvar2)*ndmpar + 1
                  call fioraa (riobuf(ip1), nrvar3, trraai, noraai, nosys)
               endif
!
            else
               nrvar2 = nrvar
               call fioutv ( riobuf, iopoin(k1), nrvar , nocons, nopa  ,
     +                       nofun , nosfun    , notot , conc  , segfun,
     +                       func  , param     , cons  , idt   , itime ,
     +                       volume, noseg     , nosys , nodump, idump ,
     +                       nx    , ny        , lgrid , igrdou, bound ,
     +                       noloc , proloc    , nodef , defaul)
            endif
!
!        Fill character buffer with substance names and output names
!
            if ( isrtou .eq. imnf .or.
     +           isrtou .eq. ihnf .or.
     +           isrtou .eq. ihnf .or.
     +           isrtou .eq. ihnc3 .or.
     +           isrtou .eq. imo3 .or.
     +           isrtou .eq. ihi3 .or.
     +           isrtou .eq. ihn3     ) then

               if ( allocated(hnc_standard) ) then
                  deallocate( hnc_standard )
                  deallocate( hnc_unit )
                  deallocate( hnc_description )
               endif

               allocate( hnc_standard(notot+nrvar2) )
               allocate( hnc_unit(notot+nrvar2) )
               allocate( hnc_description(notot+nrvar2) )

               do 30 i = 1 , notot
                  nambuf(i)          = syname(i)
                  hnc_standard(i)    = sysnm(i)
                  hnc_unit(i)        = syuni(i)
                  hnc_description(i) = sydsc(i)
   30          continue
               do 40 i = 1 , nrvar2
                  nambuf(notot+i)          = ounam(k1+i-1)
                  hnc_standard(notot+i)    = ousnm(k1+i-1)
                  hnc_unit(notot+i)        = ouuni(k1+i-1)
                  hnc_description(notot+i) = oudsc(k1+i-1)
   40          continue
            endif
!
!        Perform output
!
            if ( isrtou .eq. imon ) then
!
               call outmon ( lunout   , idump , conc  , amass2, itime ,
     +                       duname   , syname, moname, nodump, notot ,
     +                       ip       , isflag, asmass, ibflag, nrvar ,
     +                       ounam(k1), riobuf, itstrt, itstop, ndmpar,
     +                       danam    )
!
            elseif ( isrtou .eq. imo2 ) then
!
               call outmon ( lunout   , idump , conc  , amass2, itime ,
     +                       duname   , syname, moname, nodump, 0     ,
     +                       ip       , isflag, asmass, ibflag, nrvar ,
     +                       ounam(k1), riobuf, itstrt, itstop, ndmpar,
     +                       danam    )
!
            elseif ( isrtou .eq. imo3 ) then
!
               call outmo3 ( lunout, amass2   , itime , syname, moname,
     +                       notot , ip       , isflag, asmass, ibflag,
     +                       nrvar2, ounam(k1), riobuf, itstrt, itstop,
     +                       ndmpar, danam    )
!
            elseif ( isrtou .eq. imo4 ) then
!
               call outmo3 ( lunout, amass2   , itime , syname, moname,
     +                       0     , ip       , isflag, asmass, ibflag,
     +                       nrvar2, ounam(k1), riobuf, itstrt, itstop,
     +                       ndmpar, danam    )
!
            elseif ( isrtou .eq. idmp ) then
!
               call outdmp (lunout, lchout, itime , moname, nx       ,
     +                      ny    , lgrid , cgrid , notot , nosys    ,
     +                      syname, conc  , bound , nrvar , ounam(k1),
     +                      riobuf, ip(5) , isflag, iniout)
!
            elseif ( isrtou .eq. idm2 ) then
!
               call outdmp (lunout, lchout, itime , moname, nx       ,
     +                      ny    , lgrid , cgrid , 0     , 0        ,
     +                      syname, conc  , bound , nrvar , ounam(k1),
     +                      riobuf, ip(5) , isflag, iniout)
!
            elseif ( isrtou .eq. ihis ) then
!
               call outhis (lunout, lchout   , itime , moname, nodump,
     +                      idump , duname   , notot , syname, conc  ,
     +                      nrvar , ounam(k1), riobuf, iniout)
!
            elseif ( isrtou .eq. ihnf ) then
!
               iof = nrvar*nodump + 1
               call outhnf (lunout, lchout     , itime , moname, noseg ,
     +                      notot , conc       , nambuf, nrvar , riobuf,
     +                      iostrt, iostop     , iostep, nodump, idump ,
     +                      duname, riobuf(iof), iniout)
!
            elseif ( isrtou .eq. ihnc ) then
!
               hncrec = hncrec + 1
               iof = nrvar*nodump + 1
               call outhnc (lun(47)   , lchar(47), lchar(46), timeidh,
     +                      bndtimeidh, hncrec   , itime    , moname ,
     +                      idump     , duname   , nodump   , notot  ,
     +                      conc      , syname   , sysnm    , syuni  ,
     +                      sydsc     , hncwqid1 , nrvar    , riobuf ,
     +                      ounam(k1) , ousnm(k1), ouuni(k1), oudsc(k1),
     +                      hncwqid2  , lun(19))
!
            elseif ( isrtou .eq. ihi2 ) then
!
               call outhis (lunout, lchout   , itime , moname, nodump,
     +                      idump , duname   , 0     , syname, conc  ,
     +                      nrvar , ounam(k1), riobuf, iniout)
!
            elseif ( isrtou .eq. ihn2 ) then
!
               iof = nrvar*nodump + 1
               call outhnf (lunout, lchout     , itime    , moname, noseg ,
     +                      0     , conc       , ounam(k1), nrvar , riobuf,
     +                      iostrt, iostop     , iostep   , nodump, idump ,
     +                      duname, riobuf(iof), iniout   )
!
            elseif ( isrtou .eq. ihnc2 ) then
!
               hncrec = hncrec + 1
               iof = nrvar*nodump + 1
               call outhnc (lun(47)   , lchar(47), lchar(46), timeidh,
     +                      bndtimeidh, hncrec   , itime    , moname ,
     +                      idump     , duname   , nodump   , 0      ,
     +                      conc      , syname   , sysnm    , syuni  ,
     +                      sydsc     , hncwqid1 , nrvar    , riobuf ,
     +                      ounam(k1) , ousnm(k1), ouuni(k1), oudsc(k1),
     +                      hncwqid2  , lun(19))
!
            elseif ( isrtou .eq. ihi3 ) then
!
!           Let op RANAM achter DANAM
!
               nrvar3 = notot + nrvar2
               nsegou = ndmpar + noraai
               call outhis (lunout, lchout   , itime , moname, nsegou,
     +                      idump , danam    , 0     , syname, conc  ,
     +                      nrvar3, nambuf   , riobuf, iniout)
!
            elseif ( isrtou .eq. ihn3 ) then
!
!           Let op RANAM achter DANAM
!
               nrvar3 = notot + nrvar2
               nsegou = ndmpar + noraai
               iof = nrvar3*nsegou + 1
               call outhnf (lunout, lchout     , itime    , moname, noseg ,
     +                      0     , conc       , nambuf   , nrvar3, riobuf,
     +                      iostrt, iostop     , iostep   , nsegou, idump ,
     +                      danam , riobuf(iof), iniout   )
!
            elseif ( isrtou .eq. ihnc3 ) then
!
!           Let op RANAM achter DANAM
!

               hncrec = hncrec + 1
               nrvar3 = notot + nrvar2
               nsegou = ndmpar + noraai
               iof = nrvar3*nsegou + 1
               call outhnc (lun(47)   , lchar(47), lchar(46), timeidh,
     +                      bndtimeidh, hncrec   , itime    , moname ,
     +                      idump     , danam    , nsegou   , 0      ,
     +                      conc      , nambuf   , sysnm    , syuni  ,
     +                      sydsc     , hncwqid1 , nrvar3   , riobuf ,
     +                      nambuf    , hnc_standard, hnc_unit       ,
     +                      hnc_description,       hncwqid2  , lun(19))
!
            elseif ( isrtou .eq. ihi4 ) then
!
               call outhis (lunout, lchout   , itime , moname, ndmpar,
     +                      idump , danam    , 0     , syname, conc  ,
     +                      nrvar2, ounam(k1), riobuf, iniout)
!
            elseif ( isrtou .eq. ihn4 ) then
!
               iof = nrvar2*ndmpar + 1
               call outhnf (lunout, lchout     , itime    , moname, noseg ,
     +                      0     , conc       , ounam(k1), nrvar2, riobuf,
     +                      iostrt, iostop     , iostep   , ndmpar, idump ,
     +                      danam , riobuf(iof), iniout   )
!
            elseif ( isrtou .eq. ihnc4 ) then
!
               hncrec = hncrec + 1
               iof = nrvar2*ndmpar + 1
               call outhnc (lun(47)   , lchar(47), lchar(46), timeidh,
     +                      bndtimeidh, hncrec   , itime    , moname ,
     +                      idump     , danam    , nsegou   , 0      ,
     +                      conc      , syname   , sysnm    , syuni  ,
     +                      sydsc     , hncwqid1 , nrvar2   , riobuf ,
     +                      ounam(k1) , ousnm(k1), ouuni(k1), oudsc(k1),
     +                      hncwqid2  , lun(19))
!
            elseif ( isrtou .eq. imap ) then
!
               call outmap (lunout   , lchout, itime , moname, noseg ,
     +                      notot    , conc  , syname, nrvar , riobuf,
     +                      ounam(k1), iknmrk, iniout)
!
            elseif ( isrtou .eq. imnf ) then
!
               iof = nrvar*noseg + 1
               call outmnf (lunout   , lchout, itime , moname, noseg      ,
     +                      notot    , conc  , syname, nrvar , riobuf     ,
     +                      ounam(k1), iostrt, iostop, iostep, riobuf(iof),
     +                      iniout   )
!
            elseif ( isrtou .eq. imnc ) then
!
               mncrec = mncrec + 1
               call outmnc (lun(49)  , lchar(49), lchar(46), timeid, bndtimeid, mncrec ,
     +                      itime    , moname   , noseg    , notot  ,
     +                      conc     , syname   , sysnm, syuni, sydsc, mncwqid1 , nrvar  ,
     +                      riobuf   , ounam(k1), ousnm(k1), ouuni(k1), oudsc(k1), mncwqid2 ,
     +                      volume   , iknmrk   , lun(19))
!
            elseif ( isrtou .eq. ima2 ) then
!
               call outmap (lunout   , lchout, itime , moname, noseg ,
     +                      0        , conc  , syname, nrvar , riobuf,
     +                      ounam(k1), iknmrk, iniout)
!
            elseif ( isrtou .eq. imn2 ) then
!
               iof = nrvar*noseg + 1
               call outmnf (lunout   , lchout, itime , moname, noseg      ,
     +                      0        , conc  , syname, nrvar , riobuf     ,
     +                      ounam(k1), iostrt, iostop, iostep, riobuf(iof),
     +                      iniout   )
!
            elseif ( isrtou .eq. imnc2 ) then
!
               mncrec = mncrec + 1
               call outmnc (lun(49)  , lchar(49), lchar(46), timeid, bndtimeid, mncrec ,
     +                      itime    , moname   , noseg    , 0      ,
     +                      conc     , syname   , sysnm, syuni, sydsc, mncwqid1 , nrvar  ,
     +                      riobuf   , ounam(k1), ousnm(k1), ouuni(k1), oudsc(k1), mncwqid2 ,
     +                      volume   , iknmrk   , lun(19))
!
            elseif ( isrtou .eq. ibal ) then
!
               call outbal (lunout, lchout, itime , moname, notot ,
     +                      noflux, syname, ndmpar, danam , asmass,
     +                      flxint, nrvar2, riobuf, iniout)
!
            elseif ( isrtou .eq. iba2 ) then
!
               call outhis (lunout, lchout   , itime , moname, ndmpar,
     +                      idump , danam    , 0     , syname, conc  ,
     +                      nrvar , ounam(k1), riobuf, iniout)
!
            elseif ( isrtou .eq. iba3 ) then
!
               allocate(surf(noseg))
               name = 'SURF'
               lget = .true.
               call values ( name   , noseg  , surf   , nocons , nopa   ,
     +                       nofun  , nosfun , cons   , coname , param  ,
     +                       paname , func   , funame , segfun , sfname ,
     +                       lget   , ierr   )

               call sobbal ( notot , itime , nosys , noflux   , ndmpar,
     j                       ndmpq , ntdmpq, itstop, imstrt   , imstop,
     j                       iqdmp , ipdmp , asmass, flxint   , stochi,
     j                       syname, danam , moname, dmpq     , nobnd ,
     j                       nobtyp, bndtyp, inwtyp, nocons   , coname,
     j                       cons  , noq   , ipoint, ounam(k1), intopt,
     j                       volume, surf  , noseg , lunout   , lchout,
     j                       iniout, dmpbal, nowst , nowtyp   , wsttyp,
     j                       iwaste, inxtyp, wstdmp, isegcol  , imstep)

               lun(ifi) = lunout ! Ad hoc: routine dhopnf sets the LU-number via newunit
               deallocate (surf)
!
            endif
!
            ioutps(7,iout) = iniout
!
!
  100    continue
!
!        Update K1, pointer in IOPOIN and OUNAM
!
         k1 = k1 + nrvar
!
  200 continue

      if ( timon ) call timstop ( ithandl )
      return
      end

