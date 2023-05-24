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

      subroutine dlwqta ( lun    , lch    , lunrep , noseg  , nocons ,
     &                    nopa   , nofun  , nosfun , const  , param  ,
     &                    funcs  , sfuncs , isflag , ifflag , itime  ,
     &                    GridPs , dlwqd  , ierr   )

!       Deltares Software Centre

!>\file
!>           Makes values at time = itime for the process parameters

!     Created             : ??????????      by Jan van Beek

!     Modified            : July   29, 2011 by Leo Postma   : support of unformatted and big-endian files (telemac)

!     logical unitnumbers : lun   - binary process parameters system file
!                           lunrep - monitoring file

!     global declarations

      use m_srstop
      use m_dhopnf
      use m_dhnlun
      use timers
      use delwaq2_data
      use grids
      use dlwq_data
      implicit none

!     declaration of the arguments

!     kind                       function         name                   Descriptipon

      integer                  , intent(inout) :: lun                  !< unit number binary input file
      character(len=*)         , intent(in   ) :: lch                  !< name input file
      integer                  , intent(in   ) :: lunrep               !< unit number report file
      integer                  , intent(in   ) :: noseg                !< number of segments
      integer                  , intent(in   ) :: nocons               !< number of constants
      integer                  , intent(in   ) :: nopa                 !< number of parameters
      integer                  , intent(in   ) :: nofun                !< number of functions
      integer                  , intent(in   ) :: nosfun               !< number of segment functions
      real                     , intent(  out) :: const(nocons)        !< constants array
      real                     , intent(  out) :: param(nopa,noseg)    !< parameters array
      real                     , intent(  out) :: funcs(nofun)         !< functions array
      real                     , intent(  out) :: sfuncs(noseg,nosfun) !< segment functions array
      integer                  , intent(in   ) :: isflag               !< = 1 then 'ddhhmmss' format
      integer                  , intent(in   ) :: ifflag               !< = 1 then first invocation
      integer                  , intent(in   ) :: itime                !< system timer
      type(GridPointerColl)    , intent(in   ) :: GridPs               !< collection off all grid definitions
      type(delwaq_data), target, intent(inout) :: dlwqd                !< derived type for persistent storage
      integer                  , intent(inout) :: ierr                 !< error count

!     Local declarations

      integer                              :: no_proc_pars         ! number of process parameters data blocks
      type(t_dlwqdata), pointer            :: proc_par             ! a pointer to one data block
      integer                              :: ntotal               ! number of process parameters
      character(len=12)                    :: chlp
      integer                                 ftype                ! the equivalent of the ftype array elsewhere
      integer                              :: ierr2                ! io error indicator
      integer                              :: i, i2
      integer                              :: ntt, ndim1, ndim2, nobrk
      integer, allocatable                 :: ipntloc(:)
      integer                              :: idummy
      logical                              :: ldummy, ldumm2
      real                                 :: rdummy
      integer(4) ithandl /0/

      if ( timon ) call timstrt ( "dlwqta", ithandl )

      ! read the data from file

      ntotal = nocons+nopa+nofun+nosfun
      if ( ntotal .gt. 0 ) then
         if ( ifflag .eq. 1 ) then
            call dhopnf ( lun , lch , 16 , 2 , ierr2 )
            if ( ierr2 .ne. 0 ) then
               write(lunrep,*) 'error in dlwqta, opening file'
               write(lunrep,*) 'file    :',lch
               write(lunrep,*) 'unit    :',lun
               call srstop(1)
            endif
            read ( lun , iostat = ierr2 ) chlp
            if ( ierr2.ne.0 .or. chlp(1:6) .ne. ' 5.000' ) then
               write(lunrep,*) 'error in dlwqta, file not new'
               call srstop(1)
            endif

            read(lun,iostat=ierr2) no_proc_pars
            if ( ierr2 .ne. 0 ) then
               write(lunrep,1000) trim(lch)
               call srstop(1)
            endif
            allocate(dlwqd%proc_pars%dlwqdata(no_proc_pars))
            dlwqd%proc_pars%maxsize = no_proc_pars
            dlwqd%proc_pars%cursize = no_proc_pars

            do i = 1 , no_proc_pars
               ierr2 = dlwqdataRead(lunrep,lun,dlwqd%proc_pars%dlwqdata(i))
               if ( ierr2 .ne. 0 ) then
                  write(lunrep,1000) trim(lch)
                  call srstop(1)
               endif
               proc_par => dlwqd%proc_pars%dlwqdata(i)
               if ( proc_par%extern .and. ( mod(proc_par%filetype,10) .eq. FILE_BINARY .or.
     &                                      mod(proc_par%filetype,10) .eq. FILE_UNFORMATTED ) ) then
                  if ( proc_par%iorder .eq. ORDER_PARAM_LOC ) then
                     ndim1 = proc_par%no_param
                     ndim2 = proc_par%no_loc
                  else
                     ndim1 = proc_par%no_loc
                     ndim2 = proc_par%no_param
                  endif
                  nobrk = 1
                  proc_par%no_brk=nobrk
                  allocate(proc_par%values(ndim1,ndim2,nobrk))
                  !
                  ! Some obscure magic going on with LU-numbers - keep it for the moment
                  !
                  call dhnlun(801,proc_par%lun)
                  ftype = 2
                  if ( mod(proc_par%filetype,10) .eq. FILE_UNFORMATTED ) ftype = ftype + 10
                  if ( proc_par%filetype/10 .eq. 1 ) ftype = ftype + 20       ! I am in for a better solution (lp)
                  call dhopnf ( proc_par%lun , proc_par%filename , 40 , ftype , ierr2 )
               endif
            enddo

            close ( lun )

         endif

         ! evaluate data

         do i = 1 , dlwqd%proc_pars%cursize

            ! update external data to values

            proc_par => dlwqd%proc_pars%dlwqdata(i)
            if ( proc_par%extern .and. ( mod(proc_par%filetype,10) .eq. FILE_BINARY .or.
     &                                   mod(proc_par%filetype,10) .eq. FILE_UNFORMATTED ) ) then
               if ( proc_par%iorder .eq. ORDER_PARAM_LOC ) then
                  ndim1 = proc_par%no_param
                  ndim2 = proc_par%no_loc
               else
                  ndim1 = proc_par%no_loc
                  ndim2 = proc_par%no_param
               endif
               ntt = ndim1*ndim2
               allocate(ipntloc(ntt))
               do i2 = 1 , ntt; ipntloc(i2)=-i2; enddo
               call dlwqt4( idummy, proc_par%filename, idummy , lunrep, proc_par%lun,
     +                      itime , proc_par%values  , ipntloc, ndim1 , ntt         ,
     +                      isflag, ifflag           , ldummy , 0     ,.false.      ,
     +                      ldumm2, rdummy           , dlwqd  )
               deallocate(ipntloc)
            endif

            if ( proc_par%subject .eq. SUBJECT_CONSTANT .and. ifflag .eq. 1 ) then
               ierr2 = dlwqdataEvaluate(proc_par,GridPs,itime,nocons,1,const)
               if ( ierr2 .ne. 0 ) then
                  write(lunrep,1010)
                  call srstop(1)
               endif
            elseif ( proc_par%subject .eq. SUBJECT_FUNCTION ) then
               ierr2 = dlwqdataEvaluate(proc_par,GridPs,itime,nofun,1,funcs)
               if ( ierr2 .ne. 0 ) then
                  write(lunrep,1010)
                  call srstop(1)
               endif
            elseif ( proc_par%subject .eq. SUBJECT_PARAMETER .and. ifflag .eq. 1 ) then
               ierr2 = dlwqdataEvaluate(proc_par,GridPs,itime,nopa,noseg,param)
               if ( ierr2 .ne. 0 ) then
                  write(lunrep,1010)
                  call srstop(1)
               endif
            elseif ( proc_par%subject .eq. SUBJECT_SEGFUNC ) then
               ierr2 = dlwqdataEvaluate(proc_par,GridPs,itime,noseg,nosfun,sfuncs)
               if ( ierr2 .ne. 0 ) then
                  write(lunrep,1010)
                  call srstop(1)
               endif
            endif
         enddo

      endif

      if ( timon ) call timstop ( ithandl )
      return
 1000 format (/' ERROR: reading process parameters system file:',A)
 1010 format (/' ERROR: evaluating process parameters')
      end
