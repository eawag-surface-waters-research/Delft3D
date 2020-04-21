!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2018-2020.!
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
   
   subroutine mba_init()
   
   use m_alloc
   use m_mass_balance_areas
   use m_fm_wq_processes
   use m_transport
   use m_partitioninfo
   use m_flowtimes, only: tstart_user
   use m_flowgeom, only: Ndxi, Lnxi, ln, lne2ln
   use unstruc_model, only: md_ident, md_ident_sequential, getoutputdir
   use m_flowexternalforcings
   use unstruc_files

   implicit none
   
   integer :: iconst, imbs, isys, iwqbot, imba, i, j, istart, ibnd, isrc, L, LL, Lf, kk1, kk2, ba1, ba2, to, from, ierr
   logical :: writebalance
   character(len=64)  :: ident !< Identifier of the model, used as suggested basename for some files. (runid)

   jamba = 1
   ibflag = 1

   timembastart = tstart_user ! when DFM doesn't start at t=0.0??
   timembastarttot = timembastart
   itimembastart = nint(tstart_user)
   itimembastarttot = itimembastart
   
   flxdmp = 0.0
   flxdmptot = 0.0

!  Allocate the mass names, balance flux and derivative arrays
   nombs = numconst + numwqbots
   call realloc(mbsname, nombs, keepExisting=.false., fill=' ')
   call realloc(imbs2sys, nombs, keepExisting=.false., fill=0)
   do iconst = 1, numconst
      mbsname(iconst) = const_names(iconst)
      if (nosys.gt.0) then
         imbs2sys(iconst) = iconst2sys(iconst)
      endif
   enddo
   do iwqbot = 1, numwqbots
      mbsname(numconst + iwqbot) = wqbotnames(iwqbot)
      imbs2sys(numconst + iwqbot) = nosys + iwqbot
   enddo
  
   nombabnd = nomba + nopenbndsect

   call realloc(mbaarea, nomba, keepExisting=.false., fill=0d0)

   call realloc(mbavolumebegin   , nomba, keepExisting=.false., fill=0d0)
   call realloc(mbavolumebegintot, nomba, keepExisting=.false., fill=0d0)
   call realloc(mbavolumeend     , nomba, keepExisting=.false., fill=0d0)
      
   call realloc(mbaflowhor, [2, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
   call realloc(mbaflowhortot, [2, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
   call realloc(mbaflowsorsin, [2, numsrc], keepExisting=.false., fill=0d0)
   call realloc(mbaflowsorsintot, [2, numsrc], keepExisting=.false., fill=0d0)
   call realloc(mbaflowraineva, [2, nomba], keepExisting=.false., fill=0d0)
   call realloc(mbaflowrainevatot, [2, nomba], keepExisting=.false., fill=0d0)
   call realloc(mbafloweva, nomba, keepExisting=.false., fill=0d0)
   call realloc(mbaflowevatot, nomba, keepExisting=.false., fill=0d0)

   call realloc(mbamassbegin   , [nombs, nomba], keepExisting=.false., fill=0d0)
   call realloc(mbamassbegintot, [nombs, nomba], keepExisting=.false., fill=0d0)
   call realloc(mbamassend     , [nombs, nomba], keepExisting=.false., fill=0d0)
      
   call realloc(mbafluxhor, [2, numconst, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
   call realloc(mbafluxhortot, [2, numconst, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
   call realloc(mbafluxsorsin, [2, 2, numconst, numsrc], keepExisting=.false., fill=0d0)
   call realloc(mbafluxsorsintot, [2, 2, numconst, numsrc], keepExisting=.false., fill=0d0)

   if ( jampi.eq.1 ) then
      call realloc(mbavolumereduce  , nomba, keepExisting=.false., fill=0d0)
      call realloc(mbaflowhorreduce , [2, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
      call realloc(mbaflowsorsinreduce, [2, numsrc], keepExisting=.false., fill=0d0)
      call realloc(mbaflowrainevareduce , [2, nomba], keepExisting=.false., fill=0d0)
      call realloc(mbaflowevareduce , nomba, keepExisting=.false., fill=0d0)
      call realloc(mbamassreduce    , [nombs, nomba], keepExisting=.false., fill=0d0)
      call realloc(mbafluxhorreduce , [2, numconst, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
      call realloc(mbafluxsorsinreduce, [2, 2, numconst, numsrc], keepExisting=.false., fill=0d0)
   end if

!  Determine 2D pointers fo links (from balance area to balance area)
   nombaln = 0 
   do LL=1,Lnxi
      kk1=ln(1,LL)
      kk2=ln(2,LL)
      ba1=mbadef(kk1)
      ba2=mbadef(kk2)
      ! check on ghosts!
      if ( jampi.eq.1 ) then
!        if neither is in my domain, don't use it
         if ( idomain(kk1).ne.my_rank .and. idomain(kk2).ne.my_rank ) cycle
         if ( idomain(kk1).lt.my_rank .or. idomain(kk2).lt.my_rank ) cycle
      end if
      if (ba1.ne.ba2) then
         nombaln = nombaln + 1
         call realloc(mbalnlist, nombaln, keepExisting=.true., fill=LL)
         call realloc(mbalnfromto, [2, nombaln], keepExisting=.true., fill=0)
         mbalnfromto(1, nombaln) = ba1
         mbalnfromto(2, nombaln) = ba2
      endif
   enddo
      
   if (nopenbndsect.gt.0) then
      istart = 1
      call realloc(mbaname, nombabnd, keepExisting=.true., fill=' ')
      do ibnd=1,nopenbndsect
         mbaname(nomba + ibnd) = 'bnd_'//openbndname(ibnd)
         do LL = istart, nopenbndlin(ibnd)
            L  = openbndlin(LL)
            Lf = lne2ln(L)
            ! check on ghosts!
            if ( jampi.eq.1 ) then
               if ( idomain(ln(2,Lf)).ne.my_rank) cycle
            end if
            nombaln = nombaln + 1
            call realloc(mbalnlist, nombaln, keepExisting=.true., fill=Lf)
            call realloc(mbalnfromto, [2, nombaln], keepExisting=.true., fill=0)
            mbalnfromto(1, nombaln) = nomba + ibnd
            mbalnfromto(2, nombaln) = mbadef(ln(2,Lf))
         enddo
         istart = nopenbndlin(ibnd) + 1
      enddo
   endif
      
   call realloc(mbalnused, [nomba, nombabnd], keepExisting=.true., fill=0)
   do imba = 1, nombaln
      to = mbalnfromto(1, imba)
      from = mbalnfromto(2, imba)
      if ( to <= nomba) then
         mbalnused(to,from) = mbalnused(to,from) + 1
      endif
      if ( from <= nomba) then
         mbalnused(from,to) = mbalnused(from,to) + 1
      endif
   enddo

   if (jampi.eq.1) then
      call reduce_int_array_sum(nomba * nombabnd, mbalnused)
   endif
      
   call realloc(mbasorsin, [2, numsrc], keepExisting=.true., fill=0)
   call realloc(mbasorsinout, [2, numsrc], keepExisting=.true., fill=0)
   do isrc = 1, numsrc
      kk1    = ksrc(1,isrc)                   ! 2D pressure cell nr FROM
      kk2    = ksrc(4,isrc)                   ! 2D pressure cell nr TO
      if(kk1 > 0) then
         mbasorsin(1,isrc) = mbadef(kk1)
         if ( jampi.eq.1 ) then
            if ( idomain(kk1) /= my_rank ) mbasorsin(1,isrc) = 0
         endif
      endif
      if(kk2 > 0) then
         mbasorsin(2,isrc) = mbadef(kk2)
         if ( jampi.eq.1 ) then
            if ( idomain(kk2) /= my_rank ) mbasorsin(2,isrc) = 0
         endif
      endif
      mbasorsinout(1,isrc) = mbasorsin(1,isrc)
      mbasorsinout(2,isrc) = mbasorsin(2,isrc)
   enddo

   if (jampi.eq.1) then
      call reduce_int_array_sum(2 * numsrc, mbasorsinout)
   endif

   call realloc(flxdmp, [2,nflux, nomba], keepExisting=.false., fill=0.0d0 )       !< Fluxes at dump segments
   call realloc(flxdmpreduce, [2,nflux, nomba], keepExisting=.false., fill=0.0d0 )       !< Fluxes at dump segments
   call realloc(flxdmptot, [2,nflux, nomba], keepExisting=.false., fill=0.0d0 )       !< Fluxes at dump segments

   call mba_sum_area(nomba, mbadefdomain, mbaarea)
   call mba_sum(nombs, nomba, mbadefdomain, mbavolumebegin, mbamassbegin)
   if ( jampi.eq.1 ) then
      call reduce_double_sum(nomba, mbaarea, mbavolumereduce)
      do imba =1, nomba
         mbaarea(imba) = mbavolumereduce(imba)
      enddo
      call reduce_double_sum(nomba, mbavolumebegin, mbavolumereduce)
      do imba =1, nomba
         mbavolumebegin(imba) = mbavolumereduce(imba)
      enddo
      call reduce_double_sum((nombs) * nomba, mbamassbegin, mbamassreduce)
      do imba =1, nomba
         do imbs = 1, nombs
            mbamassbegin(imbs, imba) = mbamassreduce(imbs, imba)
         enddo
      enddo
   endif
   
   do imba = 1, nomba
      mbavolumebegintot(imba) = mbavolumebegin(imba)
   end do

   do imba = 1, nomba
      do imbs=1, nombs
         mbamassbegintot(imbs,imba) = mbamassbegin(imbs,imba)
      end do
   end do

!  write mbahis to a his files (for now) and ascii bal-files
   writebalance = .true.
   if ( jampi.eq.1 ) then
!     in MPI mode      
      if (my_rank.ne.0) then
!        this is not the main node that writes the full balance over all domains, switch of writing
         writebalance = .false.
      else
!        use the original sequential ident without domain number
         ident = md_ident_sequential
      endif
   else
!     not in MPI mode, we can use ident
      ident = md_ident
   endif

   if (writebalance) then
!      open(newunit=lunmbahis,file=trim(getoutputdir())//trim(ident)//'_mba.his', &
!           form='unformatted', access='stream', status='replace')
!      call mba_write_his_header(lunmbahis)
      open(newunit=lunmbabal,file=defaultfilename('mba'))
      call mba_write_bal_header(lunmbabal, numconst, const_names, iconst2sys, nosys, notot, isys2wqbot, syname_sub, nomba, mbaname, nflux, &
                                totfluxsys, stochi, fluxname, fluxprocname, nfluxsys, ipfluxsys, fluxsys)

!      open(newunit=lunmbatothis,file=trim(getoutputdir())//trim(ident)//'_mbatot.his', &
!           form='unformatted', access='stream', status='replace')
!      call mba_write_his_header(lunmbatothis)
   endif
   
   end subroutine mba_init

!> Convert qid (from .ext file) to waq input name (split in generic qidname and specific input name).
!! If the input qid is not mba input name, then the same qid is returned (and no mba input name)
   subroutine get_mbainputname(qid, inputname, qidname)
      implicit none

      character(len=*), intent(in)    :: qid       !< Original quantityid, e.g., 'massbalanceareanorth'.
      character(len=*), intent(inout) :: inputname !< The trimmed waq input name, e.g., 'north'.
      character(len=*), intent(inout) :: qidname   !< The base input name for further use in external file analisys, e.g., 'massbalancearea'.

      character(len=256)              :: qidloc    !< Original quantityid, e.g., 'massbalanceareanorth'.

      qidloc = qid
      if (qidloc(1:15).eq.'massbalancearea' ) then
         qidname = qidloc(1:15)
         if ( len_trim(qidloc).gt.15 ) then
            inputname = trim(qidloc(16:))
         end if
      else if (qidloc(1:18).eq.'waqmassbalancearea' ) then ! keep for backwards compatibility
         qidname = 'massbalancearea'
         if ( len_trim(qidloc).gt.18 ) then
            inputname = trim(qidloc(19:))
         end if
      end if
      return
   end subroutine get_mbainputname

   subroutine mba_update(time)
   use m_mass_balance_areas
   use m_fm_wq_processes
   use m_partitioninfo
   use m_flowexternalforcings, only: numsrc, srcname
   use m_wind, only: jarain, jaevap
   use m_flowparameters, only: jatem
   use m_transport, only: numconst, isalt, itemp

   implicit none

   double precision, intent(in) :: time !< time     for waq in seconds

   integer :: iconst, imbs, imba, jmba, iflx, isrc, j
   logical :: writebalance

   itimembaend = int(time)
   timembaend = time

!  New total volumes and masses
   call mba_sum(nombs, nomba, mbadefdomain, mbavolumeend, mbamassend)

!  If in parallel mode, reduce arrays
   writebalance = .true.
   if ( jampi.eq.1 ) then
      if (my_rank.ne.0) writebalance = .false.

      call reduce_double_sum(nomba, mbavolumeend, mbavolumereduce)
      do imba =1, nomba
         mbavolumeend(imba) = mbavolumereduce(imba)
      enddo
      call reduce_double_sum(2 * nombabnd * nombabnd, mbaflowhor, mbaflowhorreduce)
      do imba = 1, nombabnd
         do jmba = 1, nombabnd
            mbaflowhor(1:2, imba, jmba) = mbaflowhorreduce(1:2, imba, jmba)
         enddo
      enddo
      call reduce_double_sum(2 * numsrc, mbaflowsorsin, mbaflowsorsinreduce)
      do isrc = 1, numsrc
         mbaflowsorsin(1:2,isrc) = mbaflowsorsinreduce(1:2,isrc)
      enddo
      call reduce_double_sum(2 * nomba, mbaflowraineva, mbaflowrainevareduce)
      do imba = 1, nomba
         mbaflowraineva(1:2,imba) = mbaflowrainevareduce(1:2,imba)
      enddo
      call reduce_double_sum(nomba, mbafloweva, mbaflowevareduce)
      do imba = 1, nomba
         mbafloweva(imba) = mbaflowevareduce(imba)
      enddo

      call reduce_double_sum(notot * nomba, mbamassend, mbamassreduce)
      do imba =1, nomba
         do imbs = 1, nombs
            mbamassend(imbs, imba) = mbamassreduce(imbs, imba)
         enddo
      enddo
      call reduce_double_sum(2 * numconst * nombabnd * nombabnd, mbafluxhor, mbafluxhorreduce)
      do imba = 1, nombabnd
         do jmba = 1, nombabnd
            do iconst = 1, numconst
               mbafluxhor(1:2, iconst, imba, jmba) = mbafluxhorreduce(1:2, iconst, imba, jmba)
            enddo
         enddo
      enddo
      call reduce_double_sum(2 * 2 * numconst * numsrc, mbafluxsorsin, mbafluxsorsinreduce)
      do isrc = 1, numsrc
         do iconst = 1, numconst
            mbafluxsorsin(1:2,1:2,iconst,isrc) = mbafluxsorsinreduce(1:2,1:2,iconst,isrc)
         enddo
      enddo
      if(nflux.gt.0) then
         call reduce_double_sum(2 * nflux * nomba, flxdmp, flxdmpreduce)
         do imba = 1, nomba
            do iflx = 1, nflux
               flxdmp(1:2, iflx, imba) = flxdmpreduce(1:2, iflx, imba)
            enddo
         enddo
      endif
   endif

   if (writebalance) then
      call mba_write_bal_time_step(lunmbabal, timembastart, timembaend, numconst, notot, nombs, imbs2sys, nomba, nombabnd, &
                                   nflux, totfluxsys, mbsname, mbaname, mbalnused, numsrc, srcname, mbasorsinout, &
                                   mbaarea, mbavolumebegin, mbavolumeend, mbaflowhor, mbaflowsorsin, mbaflowraineva, &
                                   mbafloweva, mbamassbegin, mbamassend, mbafluxhor, mbafluxsorsin, &
                                   flxdmp, stochi, fluxname, nfluxsys, ipfluxsys, fluxsys, jarain, jaevap, jatem, isalt, itemp)
   endif

   ! Store end volumes and masses as begin volumes and masses for the next balance output step
   itimembastart = itimembaend
   timembastart = timembaend
   do imba = 1, nomba
      mbavolumebegin(imba) = mbavolumeend(imba)
   end do
   do imba = 1, nomba
      do imbs=1, nombs
         mbamassbegin(imbs,imba) = mbamassend(imbs,imba)
      end do
   end do

   ! add fluxes to the full calculation period fluxes
   do imba = 1, nombabnd
      do jmba = 1, nombabnd
         mbaflowhortot(1:2, imba, jmba) = mbaflowhortot(1:2, imba, jmba) + mbaflowhor(1:2, imba, jmba)
      enddo
   enddo
   do isrc = 1, numsrc
      mbaflowsorsintot(1:2,isrc) = mbaflowsorsintot(1:2,isrc) + mbaflowsorsin(1:2,isrc)
   enddo
   do imba = 1, nomba
      mbaflowrainevatot(1:2,imba) = mbaflowrainevatot(1:2,imba) + mbaflowraineva(1:2,imba)
   enddo
   do imba = 1, nomba
      mbaflowevatot(imba) = mbaflowevatot(imba) + mbafloweva(imba)
   enddo

   do imba = 1, nombabnd
      do jmba = 1, nombabnd
         do iconst = 1, numconst
            mbafluxhortot(1:2, iconst, imba, jmba) = mbafluxhortot(1:2, iconst, imba, jmba) + mbafluxhor(1:2, iconst, imba, jmba)
         enddo
      enddo
   enddo
   do isrc = 1, numsrc
      do iconst = 1, numconst
         mbafluxsorsintot(1:2,1:2,iconst,isrc) = mbafluxsorsintot(1:2,1:2,iconst,isrc) + mbafluxsorsin(1:2,1:2,iconst,isrc)
      enddo
   enddo
   if (nflux.gt.0) then
      do imba = 1, nomba
         do iflx = 1, nflux
            flxdmptot(1:2, iflx, imba) = flxdmptot(1:2, iflx, imba) + flxdmp(1:2, iflx, imba)
         enddo
      enddo
   endif

   ! reset flux accumulators
   mbaflowhor = 0.0d0
   mbaflowsorsin = 0.0d0
   mbaflowraineva = 0.0d0
   mbafloweva = 0.0d0
   mbafluxhor = 0.0d0
   mbafluxsorsin = 0.0d0
   flxdmp = 0.0


   end subroutine mba_update

   subroutine mba_final(time)
   use m_mass_balance_areas
   use m_fm_wq_processes
   use m_partitioninfo
   use m_flowexternalforcings, only: numsrc, srcname
   use m_transport, only: numconst, isalt, itemp
   use m_wind, only: jarain, jaevap
   use m_flowparameters, only: jatem

   implicit none

   double precision, intent(in) :: time !< time     for waq in seconds

   integer :: isys, imba, jmba, j
   logical :: writebalance

   itimembaend = int(time)

   writebalance = .true.
   if ( jampi.eq.1 ) then
      if (my_rank.ne.0) writebalance = .false.
   endif

   if (writebalance) then
      write(lunmbabal,1000)
      call mba_write_bal_time_step(lunmbabal, timembastarttot, timembaend, numconst, notot, nombs, imbs2sys, nomba, nombabnd, &
                                   nflux, totfluxsys, mbsname, mbaname, mbalnused, numsrc, srcname, mbasorsinout, &
                                   mbaarea, mbavolumebegintot, mbavolumeend, mbaflowhortot, mbaflowsorsintot, mbaflowrainevatot, &
                                   mbaflowevatot, mbamassbegintot, mbamassend, mbafluxhortot, mbafluxsorsintot, &
                                   flxdmptot, stochi, fluxname, nfluxsys, ipfluxsys, fluxsys, jarain, jaevap, jatem, isalt, itemp)
   endif

   1000 format (///'============================================================='&
                  /'Mass balances for whole calculation period'                   &
                  /'=============================================================')

   end subroutine mba_final

   subroutine mba_sum(nombs, nomba, mbadef, mbavolume, mbamass)
   
   use m_fm_wq_processes, only: numwqbots, wqbot
   use m_partitioninfo
   use m_flowgeom
   use m_flow
   use m_transport
   
   implicit none

   integer          :: nombs, nomba
   integer          :: mbadef(ndxi)
   double precision :: mbavolume(nomba)      ! volumes
   double precision :: mbamass(nombs, nomba)  ! masses

   integer :: k, kk, kb, kt, iconst, iwqbot, imba

   mbavolume = 0.0d0
   mbamass = 0.0d0

   do kk = 1,ndxi
      if ( jampi.eq.1 ) then
!        do not include ghost cells
         if ( idomain(kk).ne.my_rank ) cycle
      end if
      imba = mbadef(kk)
      call getkbotktop(kk,kb,kt)
      do k = kb,kt
         mbavolume(imba) = mbavolume(imba) + vol1(k)
         do iconst=1,numconst
            mbamass(iconst,imba) = mbamass(iconst,imba) + constituents(iconst,k)*vol1(k)
         end do
      end do
      do iwqbot=1,numwqbots
         mbamass(numconst+iwqbot,imba) = mbamass(numconst+iwqbot,imba) + wqbot(iwqbot,kk)*ba(kk)
      end do
   end do 
   end subroutine mba_sum

   subroutine mba_sum_area(nomba, mbadef, mbaba)
   
   use m_partitioninfo
   use m_flowgeom
   
   implicit none

   integer          :: nomba
   integer          :: mbadef(ndxi)
   double precision :: mbaba(nomba)      ! areas

   integer :: kk, imba

   mbaba = 0.0d0

   do kk = 1,ndxi
      if ( jampi.eq.1 ) then
!        do not include ghost cells
         if ( idomain(kk).ne.my_rank ) cycle
      end if
      imba = mbadef(kk)
      mbaba(imba) = mbaba(imba) + ba(kk)
   end do 
   end subroutine mba_sum_area

   subroutine mba_write_bal_header(lunbal, numconst, const_names, iconst2sys, nosys, notot, isys2wqbot, syname_sub, nomba, mbaname, nflux, &
                                   totfluxsys, stochi, fluxname, fluxprocname, nfluxsys, ipfluxsys, fluxsys)
   
   use unstruc_version_module, only: unstruc_version_full, get_unstruc_source

   implicit none
   
   integer                     :: lunbal                    ! logical unit

   integer                     :: numconst                  ! Total number of constituents
   character(len=*)            :: const_names(numconst)     ! constituent names
   
   integer                     :: iconst2sys(numconst)      ! WAQ substance to D-Flow FM constituents
   integer                     :: nosys                     ! Number of active systems
   integer                     :: notot                     ! Number of systems
   integer                     :: isys2wqbot(notot)         ! WAQ inactive system to D-FlowFM water quality bottom variable
   character(20)               :: syname_sub(notot)         ! sunstance names

   integer                     :: nomba                     ! Number of balance areas
   character(*)                :: mbaname(nomba)            ! balance names
   
   integer                     :: nflux                     ! number of fluxes
   integer                     :: totfluxsys                ! total number of fluxes for all sustances

   real                        :: stochi(notot,nflux)
   character(10)               :: fluxname(nflux)
   character(10)               :: fluxprocname(nflux)
      
   integer                     :: nfluxsys(notot)
   integer                     :: ipfluxsys(notot)
   integer                     :: fluxsys(totfluxsys)

   character(255)              :: tex
   character(20)               :: rundat
   integer                     :: imba
   integer                     :: iconst
   integer                     :: isys
   integer                     :: iflux
   integer                     :: jflux
   integer                     :: ifluxsys

   write (lunbal, '("=============================================================")')
   write(lunbal,'(A)') trim(unstruc_version_full)
   call get_unstruc_source(tex)
   write(lunbal,'(A)') 'Source: '//trim(tex)
   call datum(rundat)
   write(lunbal,'(A)') 'File creation date: '//rundat
   write (lunbal, '(/"Balances for all mass balance areas")')
   write (lunbal, '("=============================================================")')

   write (lunbal, '(/"Overview of mass balance areas")')
   write (lunbal, '( "-------------------------------------------------------------")')
   write (lunbal, '( "Number of mass balance areas                    :",I8)') nomba
   do imba = 1, nomba
      write (lunbal, '(I8,2X,A40)') imba, mbaname(imba)
   enddo

   write (lunbal, '(/"Overview of constituents and substances")')
   write (lunbal, '( "-------------------------------------------------------------")')
   write (lunbal, '(/"Total number of FM constituents                 :",I8)') numconst
   write (lunbal, '( "Total number of WQ substances                   :",I8)') notot
   write (lunbal, '( "Number of active (transported) substances       :",I8)') nosys
   write (lunbal, '( "Number of inactive (not transported) substances :",I8)') notot - nosys
   write (lunbal, '(/"List of consituents/active WQ substances")')
   write (lunbal, '(/" FM number   WQ number  Name")')
   do iconst = 1, numconst
      isys = iconst2sys(iconst)
      if (isys.gt.0) then
         write (lunbal, '(2x,i8,4x,i8,2x,a)') iconst, isys, const_names(iconst)
      else
         write (lunbal, '(2x,i8,11x,"-",2x,a)') iconst, const_names(iconst)
      endif
   enddo
   if(nosys .lt. notot) then
      write (lunbal, '(/"List of WQ bot variables/inactive WQ substances")')
      write (lunbal, '(/" FM number   WQ number  Name")')
      do isys = nosys + 1, notot
         write (lunbal, '(2x,i8,4x,i8,2x,a)') isys2wqbot(isys), isys, syname_sub(isys)
      enddo
   endif

   write (lunbal, '(/"Overview of fluxes")')
   write (lunbal, '( "-------------------------------------------------------------")')
   write (lunbal, '( "total number of substances fluxes               :",I8)') totfluxsys
   write (lunbal, '(/"Substance      Process        Flux        Stochiometry factor")')
   write (lunbal, '( "-------------------------------------------------------------")')
   ifluxsys = 0
   do isys = 1, notot
      ipfluxsys(isys) = ifluxsys
      if (nfluxsys(isys).gt.0) then
         do iflux = ifluxsys + 1, ifluxsys + nfluxsys(isys)
            jflux = fluxsys(iflux)
            write (lunbal, '(A10,5X,A10,5X,A10,ES20.6)') syname_sub(isys), fluxprocname(jflux), fluxname(jflux), stochi(isys,jflux)
         enddo
         ifluxsys = ifluxsys + nfluxsys(isys)
      endif
   enddo

   return
   end subroutine mba_write_bal_header
   
   subroutine mba_write_bal_time_step(lunbal, timestart, timeend, numconst, notot, nombs, imbs2sys, nomba, nombabnd, &
                                      nflux, totfluxsys, mbsname, mbaname, mbalnused, numsrc, srcname, mbasorsinout, &
                                      mbaarea, mbavolumebegin, mbavolumeend, mbaflowhor, mbaflowsorsin, mbaflowraineva, &
                                      mbafloweva, mbamassbegin, mbamassend, mbafluxhor, mbafluxsorsin, flxdmp, stochi, &
                                      fluxname, nfluxsys, ipfluxsys, fluxsys, jarain, jaevap, jatem, isalt, itemp)

   implicit none
   
   integer                     :: lunbal                    ! logical unit
   
   double precision            :: timestart                 ! start time of balance period
   double precision            :: timeend                   ! end time of balance period
   integer                     :: numconst                  ! Number of constituents
   integer                     :: nombs                     ! Number of mass balances
   integer                     :: nomba                     ! Number of balance areas
   integer                     :: notot                     ! Number of WAQ sustances
   integer                     :: imbs2sys(nombs)           ! mass balance number to WAQ substance (0=not a WAQ substance)
   integer                     :: nombabnd                  ! Number of balance areas and boundaries
   integer                     :: nflux                     ! number of fluxes
   integer                     :: totfluxsys                ! total number of fluxes for all sustances

   character(*)                :: mbsname(nombs)            ! mass balance names
   character(*)                :: mbaname(nombabnd)         ! mass balance area names
   
   integer                     :: mbalnused(nomba,nombabnd) ! number of links between mda and mbabnd that are actually active

   integer                     :: numsrc                    ! nr of point sources/sinks
   character(len=255)          :: srcname(numsrc)           ! sources/sinks name (numsrc)
   integer                     :: mbasorsinout(2,numsrc)    ! (reduced) mba for each side of a source sink
   
   double precision            :: mbaarea(nomba)            ! surface area of mass balance area

   double precision            :: mbavolumebegin(nomba)     ! begin volume in mass balance area
   double precision            :: mbavolumeend(nomba)       ! end volume in mass balance area
   double precision            :: mbaflowhor(2,nombabnd,nombabnd) ! periodical flows between balance areas and between boundaries and balance areas
   double precision            :: mbaflowsorsin(2,numsrc)   ! periodical flow from source sinks
   double precision            :: mbaflowraineva(2,nomba)   ! periodical flow from rain and forced evaportion
   double precision            :: mbafloweva(nomba)       ! periodical flow from calculated evaportion

   double precision            :: mbamassbegin(nombs,nomba) ! begin volume in mass balance area
   double precision            :: mbamassend(nombs,nomba)   ! end volume in mass balance area
   double precision            :: mbafluxhor(2,numconst,nombabnd,nombabnd) ! periodical fluxes between balance areas and between boundaries and balance areas
   double precision            :: mbafluxsorsin(2,2,numconst,numsrc) ! periodical fluxes from source sinks

   double precision            :: flxdmp(2,nflux, nomba)
   real                        :: stochi(notot,nflux)
   character(10)               :: fluxname(nflux)
      
   integer                     :: nfluxsys(notot)
   integer                     :: ipfluxsys(notot)
   integer                     :: fluxsys(totfluxsys)
   
   integer                     :: jarain                    ! use rain yes or no
   integer                     :: jaevap                    ! use evaporation yes or no
   integer                     :: jatem                     ! Temperature model (0=no, 5=heatfluxmodel)
   integer                     :: isalt                     ! constituent that is salt
   integer                     :: itemp                     ! constituent that is temperature

   integer, parameter :: long = SELECTED_INT_KIND(16)
   character(len=20), external :: seconds_to_dhms
   integer :: imbs, imba, jmba, isrc, isys, iflux, jflux, ifluxsys
   double precision            :: totals(2)                 ! totals for both columns
   double precision            :: concbegin
   double precision            :: concend
   double precision            :: summbaarea
   double precision            :: summbavolumebegin
   double precision            :: summbavolumeend
   double precision            :: summbamassbegin
   double precision            :: summbamassend
   double precision            :: flux(2)
   double precision            :: reference                 ! reference for relative error
   double precision            :: relative_error            ! relative error
   double precision, parameter :: zero = 0.0
   double precision, parameter :: tiny = 1.0d-10

    do imba = 1, nomba
      totals = zero
      write (lunbal, 1000) mbaname(imba)
      write (lunbal, 1001) seconds_to_dhms(nint(timestart, long)), seconds_to_dhms(nint(timeend, long)), mbaarea(imba)
      write (lunbal, 2000) mbavolumebegin(imba), mbavolumeend(imba)
      write (lunbal, 1002)
      if (mbaarea(imba).gt.0.0) then
         write (lunbal, 2000) mbavolumebegin(imba)/mbaarea(imba), mbavolumeend(imba)/mbaarea(imba)
      else
         write (lunbal, 2005)
      endif
      write (lunbal, 1003)
      if (mbavolumebegin(imba).gt.mbavolumeend(imba)) then
         totals(1) = mbavolumebegin(imba) - mbavolumeend(imba)
      else
         totals(2) = mbavolumeend(imba) - mbavolumebegin(imba)
      endif
      write (lunbal, 2002) totals
      do jmba = 1, nombabnd
         if (mbalnused(imba,jmba).gt.0) then
            totals = totals + mbaflowhor(1:2, imba, jmba)
            write (lunbal, 2001) mbaname(jmba), mbaflowhor(1:2, imba, jmba)
         endif
      end do
      do isrc = 1, numsrc
         if (mbasorsinout(1,isrc).eq.imba) then
            totals = totals + mbaflowsorsin(1:2, isrc)
            write (lunbal, 2001) 'src_'//srcname(isrc), mbaflowsorsin(1:2, isrc)
         endif
         if (mbasorsinout(2,isrc).eq.imba) then
            totals = totals + mbaflowsorsin(2:1:-1, isrc)
            write (lunbal, 2001) 'src_'//srcname(isrc), mbaflowsorsin(2:1:-1, isrc)
         endif
      end do
      if (jarain > 0) then
         totals = totals + mbaflowraineva(1:2, imba)
         write (lunbal, 2001) 'Rain/prescribed evaporation   ', mbaflowraineva(1:2, imba)
      endif   
      if (jaevap > 0 .and. jatem > 3) then
         totals(2) = totals(2) + mbafloweva(imba)
         write (lunbal, 2001) 'Calculated evaporation        ', 0.0d0, mbafloweva(imba)
      endif   
      write (lunbal, 1004)
      write (lunbal, 2003) totals
      write (lunbal, 2010) totals(2)-totals(1)
      reference = max(abs(mbavolumebegin(imba)),abs(mbavolumeend(imba)),totals(1),totals(2))
      if (reference .gt. tiny) then
         relative_error = 1.0d2*abs(totals(2)-totals(1))/reference
         write (lunbal, 2011) relative_error
      else
         write (lunbal, 2012)
      endif
      do imbs = 1, nombs
         totals = zero
         write (lunbal, 1010) seconds_to_dhms(nint(timestart, long)), seconds_to_dhms(nint(timeend, long)), mbaname(imba), mbsname(imbs)
         write (lunbal, 2000) mbamassbegin(imbs, imba), mbamassend(imbs, imba)
         if (imbs.le.numconst) then
            if (imbs == isalt) then
               write (lunbal, 1012)
            else if (imbs == itemp) then
               write (lunbal, 1013)
            else
               write (lunbal, 1011)
            endif
            if (mbavolumebegin(imba).gt.0.0) then
               concbegin = mbamassbegin(imbs, imba) / mbavolumebegin(imba)
            else
               concbegin = 0.0
            endif
            if (mbavolumeend(imba).gt.0.0) then
               concend = mbamassend(imbs, imba) / mbavolumeend(imba)
            else
               concend = 0.0
            endif
         else
            write (lunbal, 1014)
            if (mbaarea(imba).gt.0.0) then
               concbegin = mbamassbegin(imbs, imba) / mbaarea(imba)
               concend = mbamassend(imbs, imba) / mbaarea(imba)
            else
               concbegin = 0.0
               concend = 0.0
            endif
         endif
         write (lunbal, 2000) concbegin, concend
         write (lunbal, 1015) mbsname(imbs)
         if (mbamassbegin(imbs, imba).gt.mbamassend(imbs, imba)) then
            totals(1) = mbamassbegin(imbs, imba) - mbamassend(imbs, imba)
         else
            totals(2) = mbamassend(imbs, imba) - mbamassbegin(imbs, imba)
         endif
         write (lunbal, 2002) totals
         if (imbs.le.numconst) then
            do jmba = 1, nombabnd
               if (mbalnused(imba,jmba).gt.0) then
                  totals = totals + mbafluxhor(1:2, imbs, imba, jmba)
                  write (lunbal, 2001) mbaname(jmba), mbafluxhor(1:2, imbs, imba, jmba)
               endif
            end do
            do isrc = 1, numsrc
               if (mbasorsinout(1,isrc).eq.imba) then
                  totals = totals + mbafluxsorsin(1:2, 1, imbs, isrc)
                  write (lunbal, 2001) 'src_'//srcname(isrc), mbafluxsorsin(1:2, 1, imbs, isrc)
               endif
               if (mbasorsinout(2,isrc).eq.imba) then
                  totals = totals + mbafluxsorsin(2:1:-1, 2, imbs, isrc)
                  write (lunbal, 2001) 'src_'//srcname(isrc), mbafluxsorsin(2:1:-1, 2, imbs, isrc)
               endif
            end do
         endif
         isys = imbs2sys(imbs)
         if (isys.gt.0) then
            if (nfluxsys(isys).gt.0) then
               do iflux = ipfluxsys(isys) + 1, ipfluxsys(isys) + nfluxsys(isys)
                  jflux = fluxsys(iflux)
                  if(stochi(isys,jflux).ge.0.0) then
                     flux(1) =  dble(stochi(isys,jflux)) * flxdmp(1,jflux, imba)
                     flux(2) =  dble(stochi(isys,jflux)) * flxdmp(2,jflux, imba)
                  else
                     flux(1) =  -dble(stochi(isys,jflux)) * flxdmp(2,jflux, imba)
                     flux(2) =  -dble(stochi(isys,jflux)) * flxdmp(1,jflux, imba)
                  endif
                  write (lunbal, 2004) fluxname(jflux), flux(1:2)
                  totals(1:2) = totals(1:2) + flux(1:2)
               enddo
            endif
         endif
         write (lunbal, 1004)
         write (lunbal, 2003) totals
         write (lunbal, 2020) mbsname(imbs), totals(2)-totals(1)
         reference = max(abs(mbamassbegin(imbs,imba)),abs(mbamassend(imbs,imba)),totals(1),totals(2))
         if (reference .gt. tiny) then
            relative_error = 1.0d2*abs(totals(2)-totals(1))/reference
            write (lunbal, 2021) mbsname(imbs), relative_error
         else
            write (lunbal, 2022) mbsname(imbs)
         end if
      end do
   end do
   totals = zero
   summbaarea = sum(mbaarea)
   summbavolumebegin = sum(mbavolumebegin)
   summbavolumeend = sum(mbavolumeend)
   write (lunbal, 1000) 'Whole model'
   write (lunbal, 1001) seconds_to_dhms(nint(timestart, long)), seconds_to_dhms(nint(timeend, long)), summbaarea
   write (lunbal, 2000) summbavolumebegin, summbavolumeend
   write (lunbal, 1002) 
   if (summbaarea.gt.0.0) then
      write (lunbal, 2000) summbavolumebegin/summbaarea, summbavolumeend/summbaarea
   else
      write (lunbal, 2005)
   endif
   write (lunbal, 1003) 
   if (summbavolumebegin.gt.summbavolumeend) then
      totals(1) = summbavolumebegin - summbavolumeend
   else
      totals(2) = summbavolumeend - summbavolumebegin
   endif
   write (lunbal, 2002) totals
   do jmba = nomba + 1, nombabnd
      totals(1) = totals(1) + sum(mbaflowhor(1, :, jmba))
      totals(2) = totals(2) + sum(mbaflowhor(2, :, jmba))
      write (lunbal, 2001) mbaname(jmba), sum(mbaflowhor(1, :, jmba)), sum(mbaflowhor(2, :, jmba))
   end do
   do isrc = 1, numsrc
      if (mbasorsinout(1,isrc).gt.0) then
         totals = totals + mbaflowsorsin(1:2, isrc)
         write (lunbal, 2001) 'src_'//srcname(isrc), mbaflowsorsin(1:2, isrc)
      endif
      if (mbasorsinout(2,isrc).gt.0) then
         totals = totals + mbaflowsorsin(2:1:-1, isrc)
         write (lunbal, 2001) 'src_'//srcname(isrc), mbaflowsorsin(2:1:-1, isrc)
      endif
   end do
   if (jarain > 0) then
      totals(1) = totals(1) + sum(mbaflowraineva(1, :))
      totals(2) = totals(2) + sum(mbaflowraineva(2, :))
      write (lunbal, 2001) 'Rain/prescribed evaporation   ', sum(mbaflowraineva(1, :)), sum(mbaflowraineva(2, :))
   endif
   if (jaevap > 0 .and. jatem > 3) then
      totals(2) = totals(2) + sum(mbafloweva(:))
      write (lunbal, 2001) 'Calculated evaporation        ', 0.0d0, sum(mbafloweva(:))
   endif   
   write (lunbal, 1004)
   write (lunbal, 2003) totals
   write (lunbal, 2010) totals(2)-totals(1)
   reference = max(abs(summbavolumebegin),abs(summbavolumeend),totals(1),totals(2))
   if (reference .gt. tiny) then
      relative_error = 1.0d2*abs(totals(2)-totals(1))/reference
      write (lunbal, 2011) relative_error
   else
      write (lunbal, 2012)
   endif
   do imbs = 1, nombs
      totals = zero
      write (lunbal, 1010) seconds_to_dhms(nint(timestart, long)), seconds_to_dhms(nint(timeend, long)), 'Whole model', mbsname(imbs)
      summbamassbegin = sum(mbamassbegin(imbs, :))
      summbamassend = sum(mbamassend(imbs, :))
      write (lunbal, 2000) summbamassbegin, summbamassend
      if(imbs.le.numconst) then
         write (lunbal, 1011)
         if (summbavolumebegin.gt.0.0) then
            concbegin = summbamassbegin / summbavolumebegin
         else
            concbegin = 0.0
         endif
         if (summbavolumeend.gt.0.0) then
            concend = summbamassend / summbavolumeend
         else
            concend = 0.0
         endif
      else
         write (lunbal, 1014)
         if (summbaarea.gt.0.0) then
            concbegin = summbamassbegin / summbaarea
            concend = summbamassend / summbaarea
         else
            concbegin = 0.0
            concend = 0.0
         endif
      endif
      write (lunbal, 2000) concbegin, concend
      write (lunbal, 1015) mbsname(imbs)
      if (summbamassbegin.gt.summbamassend) then
         totals(1) = summbamassbegin - summbamassend
      else
         totals(2) = summbamassend - summbamassbegin
      endif
      write (lunbal, 2002) totals
      if (imbs.le.numconst) then
         do jmba = nomba + 1, nombabnd
            totals(1) = totals(1) + sum(mbafluxhor(1, imbs, :, jmba))
            totals(2) = totals(2) + sum(mbafluxhor(2, imbs, :, jmba))
            write (lunbal, 2001) mbaname(jmba), sum(mbafluxhor(1, imbs, :, jmba)), sum(mbafluxhor(2, imbs, :, jmba))
         end do
         do isrc = 1, numsrc
            if (mbasorsinout(1,isrc).gt.0) then
               totals = totals + mbafluxsorsin(1:2, 1, imbs, isrc)
               write (lunbal, 2001) 'src_'//srcname(isrc), mbafluxsorsin(1:2, 1, imbs, isrc)
            endif
            if (mbasorsinout(2,isrc).gt.0) then
               totals = totals + mbafluxsorsin(2:1:-1, 2, isys, isrc)
               write (lunbal, 2001) 'src_'//srcname(isrc), mbafluxsorsin(2:1:-1, 2, imbs, isrc)
            endif
         end do
      endif
      isys = imbs2sys(imbs)
      if (isys.gt.0) then
         if (nfluxsys(isys).gt.0) then
            do iflux = ipfluxsys(isys) + 1, ipfluxsys(isys) + nfluxsys(isys)
               jflux = fluxsys(iflux)
               if(stochi(isys,jflux).ge.0.0) then
                  flux(1) =  dble(stochi(isys,jflux)) * sum(flxdmp(1,jflux, :))
                  flux(2) =  dble(stochi(isys,jflux)) * sum(flxdmp(2,jflux, :))
               else
                  flux(1) =  -dble(stochi(isys,jflux)) * sum(flxdmp(2,jflux, :))
                  flux(2) =  -dble(stochi(isys,jflux)) * sum(flxdmp(1,jflux, :))
               endif
               write (lunbal, 2004) fluxname(jflux), flux(1:2)
               totals(1:2) = totals(1:2) + flux(1:2)
            enddo
         endif
      endif
      write (lunbal, 1004)
      write (lunbal, 2003) totals
      write (lunbal, 2020) mbsname(imbs), totals(2)-totals(1)
      reference = max(abs(summbamassbegin),abs(summbamassend),totals(1),totals(2))
      if (reference .gt. tiny) then
         relative_error = 1.0d2*abs(totals(2)-totals(1))/reference
         write (lunbal, 2021) mbsname(imbs), relative_error
      else
         write (lunbal, 2022) mbsname(imbs)
      end if
   end do

   return
   
   1000 format (///'============================================================='&
                  /'Mass balances for ',a                                         &
                  /'=============================================================')
   1001 format (  /'Mass balance period start time: ',a                           &
                  /'Mass balance period end time  : ',a                           &
                 //'Surface area (m2)             : ',ES15.6E3                    &
                 //'Water (m3)                              Begin            End '&
                  /'-------------------------------------------------------------')
   1002 format (  /'Average depth (m)                       Begin            End '&
                  /'-------------------------------------------------------------')
   1003 format (  /'Water (m3)                    Sources/Inflows Sinks/Outflows '&
                  /'-------------------------------------------------------------')

   1004 format (   '-------------------------------------------------------------')

   1010 format ( //'-------------------------------------------------------------'&
                  /'Mass balance period start time: ',a                           &
                  /'Mass balance period end time  : ',a                           &
                 //'Mass balance area             : ',a                           &
                 //'Mass substance ',A20,'     Begin            End '             &
                  /'-------------------------------------------------------------')
   1011 format (  /'Average concentration (mass/m3)         Begin            End '&
                  /'-------------------------------------------------------------')
   1012 format (  /'Average concentration (1e-3)            Begin            End '&
                  /'-------------------------------------------------------------')
   1013 format (  /'Average concentration (degC)            Begin            End '&
                  /'-------------------------------------------------------------')
   1014 format (  /'Average concentration (mass/m2)         Begin            End '&
                  /'-------------------------------------------------------------')
   1015 format (  /'Substance ',A20,'Sources/Inflows Sinks/Outflows '             &
                  /'-------------------------------------------------------------')

   2000 format (30X,2ES15.6E3)
   2001 format (A30,2ES15.6E3)
   2002 format (   'Taken from/added to storage   ',2ES15.6E3)
   2003 format (   'Sum of all terms              ',2ES15.6E3)
   2004 format (   'Process flux ',A10,7X,2ES15.6E3)
   2005 format (45X,'no surface area')

   2010 format (  /'Water balance error (m3)                     ',ES15.6E3)
   2011 format (   'Water balance error                          ',F15.6,'%')
   2012 format (   'Water balance error                                       - %')
 
   2020 format (  /A20,' Mass balance error      ',ES15.6E3)
   2021 format (   A20,' Mass balance error      ',F15.6,'%')
   2022 format (   A20,' Mass balance error                   - %')

   end subroutine mba_write_bal_time_step
