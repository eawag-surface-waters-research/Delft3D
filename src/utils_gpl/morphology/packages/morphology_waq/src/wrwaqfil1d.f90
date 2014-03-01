      module wrwaqfil1d
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2014.                                
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
!-------------------------------------------------------------------------------

      contains
      
      subroutine wrwaqfil1d (ntab, cross, mxcross, table, mode, istep, OnLineWQ)
!!--description-----------------------------------------------------------------
! Routine is called every time step to allow a direct writing of WAQ files
!!--declarations----------------------------------------------------------------
      use precision
      use waqdata
      use cpluv
      use ident
      use wrwaq
      use c_stat ! for dtcum
!
      implicit none
!
!           Global variables
!
      integer                   , intent(in) :: mxcross
      integer, dimension(4, *)  , intent(in) :: ntab
      real, dimension(mxcross,*), intent(in) :: cross
      real(hp), dimension(*)    , intent(in) :: table
      integer                   , intent(in) :: istep
      integer                   , intent(in) :: mode
      logical                   , intent(in) :: OnLineWQ
!
!           Local variables
!
      integer :: istat
      integer :: l
      integer :: n
      integer :: noqmax
      integer, dimension(:), allocatable :: links ! links per node
      integer, dimension(:), allocatable :: link_offset ! offset of links per node
      logical :: ascii
      character(len=256) :: comfile
      character(len=256) :: filename
!
      integer                 , pointer :: itim
      integer                 , pointer :: nobnd
      integer                 , pointer :: noseg
      integer                 , pointer :: noq
      integer, dimension(:)   , pointer :: nlinks
      integer, dimension(:,:) , pointer :: ifrmto
      integer, dimension(:)   , pointer :: isaggn
      integer, dimension(:,:) , pointer :: isaggl
      integer, dimension(:)   , pointer :: iqaggr
      integer, dimension(:,:) , pointer :: volcon
      real(hp), dimension(:)  , pointer :: area      ! WAQ area of exchange
      real(hp), dimension(:,:), pointer :: lenex     ! WAQ from/to distances at exchange
      real(hp), dimension(:)  , pointer :: lenseg    ! WAQ segment length
      real(hp), dimension(:,:), pointer :: linkdis   ! Discharges at flowlinks
      real(hp), dimension(:,:), pointer :: linkvol0  ! Volumes at flowlinks
      real(hp), dimension(:,:), pointer :: linkvol1  ! Volumes at flowlinks
      real(hp), dimension(:)  , pointer :: nodevol1  ! Volumes at nodes
      real(hp), dimension(:)  , pointer :: vol       ! WAQ volume at start of step
      real(hp), dimension(:)  , pointer :: qag       ! WAQ flux aggregator
!
!! executable statements -------------------------------------------------------
!
      comfile = 'sobek_delwaq'
      ascii = .true.
      !
      if (mode==0) then
         allocate ( wdp, stat=istat)
         if (istat/=0) then
            write(*,*) '*** ERROR: wrwaqfil: memory allocation error'
            return
         endif
         !
         wdp%lunvol = -999
         wdp%lunare = -999
         wdp%lunflo = -999
         wdp%first_cf = .true.
      endif
      !
      ! set scalar pointers
      !
      nobnd => wdp%nobnd
      noseg => wdp%noseg
      noq   => wdp%noq
      itim  => wdp%itim
      !
      if (mode==0) then
         itim = istep*dtcum
         !
         ! allocate memory for counting the number of links per node (nlinks),
         ! for storing segment aggregation assignment per node (iSaggN) and per
         ! half (flow)link (iSaggL).
         !
                       allocate ( wdp%nlinks(nodmax)  , stat=istat)
         if (istat==0) allocate ( wdp%isaggn(nodmax)  , stat=istat)
         if (istat==0) allocate ( wdp%isaggl(2,lintot), stat=istat)
         if (istat/=0) then
            write(*,*) '*** ERROR: wrwaqfil: memory allocation error'
            return
         endif
      endif
      !
      ! set array pointers
      !
      nlinks => wdp%nlinks
      isaggn => wdp%isaggn
      isaggl => wdp%isaggl
      !
      ! if mode==0 then we need to initialize the arrays
      !
      if (mode==0) then
         !
         ! Initialize the segment aggregation tables.
         !
         isaggn = 0
         isaggl = 0
         !
         ! Count number of links per node.
         !
         call count_node_links(nlinks, linev, lintot)
         !
         ! Allocate space for links per node tables.
         !
                       allocate ( links(2*lintot)      , stat=istat)
         if (istat==0) allocate ( link_offset(nodmax)  , stat=istat)
         if (istat/=0) then
            write(*,*) '*** ERROR: wrwaqfil: memory allocation error'
            return
         endif
         !
         ! Collect links per node.
         !
         call collect_node_links(nlinks, links, link_offset, linev, &
                               & lintot, nodmax)
         !
         if (.false.) then
            !
            ! Aggregation from file: read aggregation arrays isaggl and isaggn.
            !
            call rd_aggregation(isaggl, isaggn, lintot, nodmin, nodmax)            
            !
            ! The number of segments is equal to the maximum segment number
            ! used. The number of boundaries is equal to the maximum negative
            ! segment number used.
            !
            noseg = 0
            nobnd = 0
            do l = 1,lintot
               noseg = max(noseg,isaggl(1,l),isaggl(2,l))
               nobnd = max(nobnd,-isaggl(1,l),-isaggl(2,l))
            enddo
            do n = nodmin,nodmax
               noseg = max(noseg,isaggn(n))
               nobnd = max(nobnd,-isaggn(n))
            enddo
         else
            !
            ! Default no aggregation: loop over all SOBEK links and nodes to
            ! generate aggregation tables isaggl and isaggn.
            !
            call mkwaqseg_noagg(linev, kcs, nlinks, links, link_offset, &
                              & lintot, nodmin, nodmax, isaggl, isaggn, &
                              & noseg, nobnd)
         endif
         !
         ! The number of exchanges reaches a maximum if each and every half
         ! link becomes a separate segment. The number of exchanges then
         ! equals the number of pre-existing links plus the number of links
         ! that will be created at a node. That number is less or equal to
         ! the number of links (NLINKS) of the node if the node itself has
         ! a segment ID, or it is less or equal to the sum of j for
         ! j=1:NLINKS-1 if the node doesn't have a segment ID assigned to it
         ! in which case the surrounding links become a segment cluster.
         !
         noqmax = lintot
         do n = nodmin,nodmax
            if (isaggn(n)==0) then
               !
               ! The node does not have a segment ID assigned to it, so all
               ! links will be connected to each other as a dense cluster of
               ! interconnects.
               !
               noqmax = noqmax + (nlinks(n)*(nlinks(n)-1))/2
            else
               !
               ! The node has a segment ID assigned to it, so the
               ! surrounding links will not be connected to each other link
               ! but only to the node: this results in at most NLINKS
               ! exchanges.
               !
               noqmax = noqmax + nlinks(n)
            endif
         enddo
         !
         ! The number of exchanges cannot be larger than the number of
         ! possible connections among the NOSEG segments and NOBND
         ! boundaries: [NOSEG*(NOSEG-1)]/2 + NOSEG*NOBND. However, if no
         ! aggregation is applied then NOSEG increases rapidly and for
         ! NOSEG larger than 46341 the integer product NOSEG*(NOSEG-1) will
         ! no longer be correct. So, don't try to apply this product.
         ! NOQMAX is used both as an upper limit for NOQ and as the precise
         ! number of connections between subvolumes (i.e. volumes in half links
         ! and at nodes separately).
         !
         ! Allocate memory for storing flux aggregation assignment per
         ! link (iQaggr) and volume connectivity at subflow level (volcon).
         !
                       allocate ( wdp%iqaggr(noqmax)  , stat=istat)
         if (istat==0) allocate ( wdp%volcon(4,noqmax), stat=istat)
         if (istat/=0) then
            write(*,*) '*** ERROR: wrwaqfil: memory allocation error'
            return
         endif
      endif
      !
      ! set array pointers
      !
      iqaggr => wdp%iqaggr
      volcon => wdp%volcon
      !
      ! if mode==0 then we need to initialize the arrays
      !
      if (mode==0) then
         !
         ! Initialize the exchange aggregation table and the connectivity
         ! table.
         !
         iqaggr = 0
         volcon = 0
         !
         ! Determine the actual number of exchanges.
         !
         call mkexchanges(lintot, nodmin, nodmax, isaggl, isaggn, nlinks, &
                        & links, link_offset, linev, noqmax, noq, iqaggr, &
                        & volcon)
         !
         ! allocate memory for pointer table and variables
         !
                       allocate ( wdp%ifrmto(4,noq)      , stat=istat)
         if (istat==0) allocate ( wdp%vol(noseg)         , stat=istat)
         if (istat==0) allocate ( wdp%lenseg(noseg)      , stat=istat)
         if (istat==0) allocate ( wdp%area(noq)          , stat=istat)
         if (istat==0) allocate ( wdp%lenex(2,noq)       , stat=istat)
         if (istat==0) allocate ( wdp%linkdis(0:2,lintot), stat=istat)
         if (istat==0) allocate ( wdp%linkvol0(2,lintot) , stat=istat)
         if (istat==0) allocate ( wdp%linkvol1(2,lintot) , stat=istat)
         if (istat==0) allocate ( wdp%nodevol1(nodmax)   , stat=istat)
         if (istat==0) allocate ( wdp%qag(noq)           , stat=istat)
         if (istat/=0) then
            write(*,*) '*** ERROR: wrwaqfil: memory allocation error'
            return
         endif
      endif
      !
      ! set array pointers
      !
      ifrmto => wdp%ifrmto
      area => wdp%area
      lenex => wdp%lenex
      linkdis => wdp%linkdis
      linkvol0 => wdp%linkvol0
      linkvol1 => wdp%linkvol1
      nodevol1 => wdp%nodevol1
      qag => wdp%qag
      vol => wdp%vol
      lenseg => wdp%lenseg
      !
      if (mode==0) then
         !
         ! fill pointer table
         !
         call mkwaqpoi(isaggl, isaggn, iqaggr, volcon, noqmax, ifrmto)
         !
         ! compute segment distances
         !
         call mkwaqlen(ifrmto, noq, isaggn, isaggl, noseg, lenseg, lenex)
         !
         ! deallocate links and link_offset arrays since we don't need them
         ! anymore ...
         !
         deallocate(links, stat=istat)
         deallocate(link_offset, stat=istat)
         !
         ! prepare oxygen fluxes at structures
         !
         !call preprear()
         !
         ! compute first volume record (also salinity)
         !
         linkvol0 = 0.0
         call mkelmvol(ntab, cross, mxcross, table, isaggn, isaggl, &
                     & linkvol1, nodevol1)
         call mkwaqvol(isaggn, isaggl, linkvol1, nodevol1, vol)
         !
         if (wdp%waqlib) then
            call init_waqlib(noq, noseg, 0, 1000000, 1, ifrmto, lenex, vol, wdp%wlp)
         else
            !
            ! print segment table
            !
            filename = trim(comfile)//'.seg'
            call wrwaqseg(isaggl, isaggn, nobnd, noseg, filename)
            filename = 'nrofsegm.dat'
            call wr_nrofseg(noseg, filename)
            filename = 'nrofexch.dat'
            call wr_nrofexch(noq, 0, 0, filename)
            !
            ! write pointer file
            !
            filename = trim(comfile)//'.poi'
            call wrwaqpoi(ifrmto, noq, filename, ascii)
            !
            ! write monitoring segments list (all segments) and monitoring areas
            !
            filename = 'segment.dat'
            call wrmonseg(noseg, filename)
            !call monitarea()
            !
            ! write list of dry waste loads
            !
            !call wdrywaste()
            !
            ! write segment distances
            !
            filename = trim(comfile)//'.len'
            call wrwaqlen(lenex, noq, filename, ascii)
            !
            ! write first volumes
            !
            filename = trim(comfile)//'.vol'
            call wrwaqbin(itim, vol, noseg, filename, ascii, wdp%lunvol)
            !
         endif
         return
      endif
      !
      ! determine exchange related items
      !
      call mkwaqexchareas(ifrmto, vol, lenseg, noq, area)
      call mkelmdis(linkvol0, linkvol1, linkdis)
      call mkwaqexchflows(iqaggr, isaggn, linkdis, nlinks, volcon, lintot, &
                        & nodmin, nodmax, qag)
      !
      ! determine volumes at end of this time step (also salinity)
      !
      linkvol0 = linkvol1
      call mkelmvol(ntab, cross, mxcross, table, isaggn, isaggl, &
                  & linkvol1, nodevol1)
      call mkwaqvol(isaggn, isaggl, linkvol1, nodevol1, vol)
      !
      if (wdp%waqlib) then
         call step_waqlib(vol, area, qag, wdp%wlp)
         !
         !success = GetCurrentValue( 'Salinity', value )
      else
         !
         ! write exchanges
         !
         filename = trim(comfile)//'.are'
         call wrwaqbin(itim, area, noq, filename, ascii, wdp%lunare)
         filename = trim(comfile)//'.flo'
         call wrwaqbin(itim, qag , noq, filename, ascii, wdp%lunflo)
         !
         if ( OnLineWQ ) then
            if ( wdp%first_cf ) then
               wdp%first_cf = .false.
               call putdio ( 'CFtoWQ' , 'DataCFtoWQ'  , .true. , wdp%cfoutset  )
               write(*,*) 'put CFtoWQ, flow initialized 1'
               call getdio ( 'WQtoWQI', 'DataWQtoWQI' , .true. , wdp%wqiinset  )
               write(*,*) 'get WQtoWQI, flow initialized 1'
               call putdio ( 'WQItoWQ', 'DataWQItoWQ' , .true. , wdp%wqioutset )
               write(*,*) 'put WQItoWQ flow initialize 2'
               call getdio ( 'WQtoCF' , 'DataWQtoCF'  , .true. , wdp%wqinset   )
               write(*,*) 'get WQtoCF flow initialize 2'
            else
               call getdio ( 'WQtoCF' , 'DataWQtoCF'  , .false., wdp%wqinset   )
               write(*,*) 'get WQtoCF flow from WAQ, using the stream , waiting for WAQ 3'
            endif
            call putdio ( 'CFtoWQ' , 'DataCFtoWQ'  , .false., wdp%cfoutset  )
            write(*,*) 'put CFtoWQ, flow put, using the stream 3'
            call getdio ( 'WQtoWQI', 'DataWQtoWQI' , .false., wdp%wqiinset   )
            write(*,*) 'get WQtoWQI, waq get 4'
            call putdio ( 'WQItoWQ', 'DataWQItoWQ' , .false., wdp%wqioutset )
            write(*,*) 'put WQItoWQ, flow put 4'
         endif
         !
         ! write new volumes
         !
         itim = itim + dtcum
         filename = trim(comfile)//'.vol'
         call wrwaqbin(itim, vol, noseg, filename, ascii, wdp%lunvol)
      endif
      !
      ! check mass balance...
      !
      ! call wrwaqbal()
      !
      !
      ! last instance: finalize output with a dummy area and flow record
      !                rearrange the wasteload file
      !
      if ( mode .eq. 2 ) then
         !
         ! end of coupline period, close all files that are (still) open
         !
         if (wdp%lunvol>0) close ( wdp%lunvol )
         if (wdp%lunvol>0) close ( wdp%lunare )
         if (wdp%lunvol>0) close ( wdp%lunflo )
      endif
      end subroutine wrwaqfil1d


      subroutine count_node_links(nlinks, linev, lintot)
!!--description-----------------------------------------------------------------
! Count number of links per node
!!--declarations----------------------------------------------------------------
!     use xxx
!
      implicit none
!
!           Global variables
!
      integer                 , intent(in)  :: lintot
      integer, dimension(0:,:), intent(in)  :: linev
      integer, dimension(:)   , intent(out) :: nlinks ! number of links per node
!
!           Local variables
!
      integer :: l
      integer :: n
!
!! executable statements -------------------------------------------------------
!
      nlinks = 0
      do l = 1,lintot
         n = linev(l,5)
         if (n>0) nlinks(n) = nlinks(n)+1
         n = linev(l,6)
         if (n>0) nlinks(n) = nlinks(n)+1
      enddo
      end subroutine count_node_links

      
      subroutine collect_node_links(nlinks, links, link_offset, linev, &
                                  & lintot, nodmax)
!!--description-----------------------------------------------------------------
! Collect links per node in links and link_offset arrays
!!--declarations----------------------------------------------------------------
!     use xxx
!
      implicit none
!
!           Global variables
!
      integer                 , intent(in)    :: lintot
      integer                 , intent(in)    :: nodmax
      integer, dimension(0:,:), intent(in)    :: linev
      integer, dimension(:)   , intent(inout) :: nlinks ! number of links per node
      integer, dimension(:)   , intent(out)   :: links ! links per node
      integer, dimension(:)   , intent(out)   :: link_offset ! offset of links per node
!
!           Local variables
!
      integer :: l
      integer :: n
!
!! executable statements -------------------------------------------------------
!
      link_offset(1) = 0
      do n = 2,nodmax
         link_offset(n) = link_offset(n-1)+nlinks(n-1)
      enddo
      !
      nlinks = 0
      do l = 1,lintot
         n = linev(l,5)
         if (n>0) then
            nlinks(n) = nlinks(n)+1
            links(link_offset(n)+nlinks(n)) = l
         endif
         n = linev(l,6)
         if (n>0) then
            nlinks(n) = nlinks(n)+1
            links(link_offset(n)+nlinks(n)) = l
         endif
      enddo
      end subroutine collect_node_links
      
      
      subroutine rd_aggregation(isaggl, isaggn, lintot, nodmin, nodmax)
!!--description-----------------------------------------------------------------
! Read ISAGGL and ISAGGN segment aggregation tables from file
!!--declarations----------------------------------------------------------------
!     use xxx
!
      implicit none
!
!           Global variables
!
      integer                , intent(in)    :: lintot
      integer                , intent(in)    :: nodmin
      integer                , intent(in)    :: nodmax
      integer, dimension(:,:), intent(out)   :: isaggl
      integer, dimension(:)  , intent(out)   :: isaggn
!
!           Local variables
!
      integer :: l
      integer :: n
!
!! executable statements -------------------------------------------------------
!
      ! First read the link aggregation table.
      !
      do l = 1,lintot
         !
         ! Dummy statements to be replaced by reading aggregation table
         ! from file.
         !
         isaggl(1,l) = l
         isaggl(2,l) = lintot+l
      enddo
      !
      ! Read the node aggregation table.
      !
      do n = nodmin,nodmax
         !
         ! Dummy statements to be replaced by reading aggregation table
         ! from file.
         !
         isaggn(n) = 0
      enddo
      end subroutine rd_aggregation


      subroutine mkwaqseg_noagg(linev, kcs, nlinks, links, link_offset, &
                              & lintot, nodmin, nodmax, isaggl, isaggn, &
                              & noseg, nobnd)
!!--description-----------------------------------------------------------------
! Make segments without aggregation
!!--declarations----------------------------------------------------------------
!     use xxx
!
      implicit none
!
!           Global variables
!
      integer                 , intent(in)  :: lintot
      integer                 , intent(in)  :: nodmin
      integer                 , intent(in)  :: nodmax
      integer, dimension(0:,:), intent(in)  :: linev
      integer, dimension(0:)  , intent(in)  :: kcs
      integer, dimension(:)   , intent(in)  :: nlinks
      integer, dimension(:)   , intent(in)  :: links
      integer, dimension(:)   , intent(in)  :: link_offset
      integer, dimension(:)   , intent(out) :: isaggn
      integer, dimension(:,:) , intent(out) :: isaggl
      integer                 , intent(out) :: noseg
      integer                 , intent(out) :: nobnd
!
!           Local variables
!
     integer :: default_aggregation_mode
     integer :: i
     integer :: l
     integer :: n
!
!! executable statements -------------------------------------------------------
!
      noseg = 0
      nobnd = 0
      !
      default_aggregation_mode = 2
      select case (default_aggregation_mode)
      case (1) ! every half a link becomes a separate segment
         !
         ! for each node
         !
         isaggn = 0
         do n = nodmin,nodmax
            !
            ! if node has volume, give it a segment number
            !
         !   if (xxxx .and. kcs(n)>0) then
         !       noseg = noseg+1
         !       isaggn(n) = noseg
         !   endif
         enddo
         !
         ! for each flowlink
         !
         do l = 1,lintot
            !
            ! for start and end node of flowlink
            !
            do i = 1,2
               n = linev(l,4+i)
               !
               ! if node is an internal calculation point or node
               !
               if (kcs(n)>0) then
                  noseg = noseg+1
                  isaggl(i,l) = noseg
               elseif (kcs(n)<0) then
                  !
                  ! if node n is an open boundary.
                  !
                  nobnd = nobnd+1
                  isaggl(i,l) = -nobnd
                  isaggn(n) = -nobnd
               endif
            enddo
         enddo
      case (2) ! every link becomes a segment; finest "classic" mode
         !
         ! for each flowlink
         !
         do l = 1,lintot
            !
            ! assign both parts of flowlink to same segment
            !
            noseg = noseg+1
            isaggl(1,l) = noseg
            isaggl(2,l) = noseg
         enddo
         !
         ! for each boundary node assign a boundary id
         ! how to treat (internal) nodes with volume?
         !
         isaggn = 0
         do n = nodmin,nodmax
            !
            ! if node is an open boundary
            !
            if (kcs(n)<0) then
               nobnd = nobnd+1
               isaggn(n) = -nobnd
            else
               !
               ! if node has volume, give it a segment number
               !
            !   if (xxxx .and. kcs(n)>0) then
            !       noseg = noseg+1
            !       isaggn(n) = noseg
            !   endif
            endif
         enddo
      case (3) ! every flow cell becomes a segment; match flow also across 
               ! nodes if number of links is equal to 2 (or less).
         !
         ! for each node
         !
         do n = nodmin,nodmax
            !
            ! assign a segment number to each node with 2 (or less) links
            !
            if (kcs(n)>0 .and. nlinks(n)<=2) then
               noseg = noseg+1
               isaggn(n) = noseg
            elseif (kcs(n)<0) then
               nobnd = nobnd+1
               isaggn(n) = -nobnd
            endif
         enddo
         !
         ! for each flowlink
         !
         do l = 1,lintot
            !
            ! for start and end node of flowlink
            !
            do i = 1,2
               n = linev(l,4+i)
               !
               ! Copy node segment number to flowlink if it differs from 0.
               !
               if (isaggn(n) /= 0) then
                  isaggl(i,l) = isaggn(n)
               else
                  noseg = noseg+1
                  isaggl(i,l) = noseg
               endif
            enddo
         enddo
         !
         ! for each node
         !
         do n = nodmin,nodmax
            !
            ! remove segment numbers from nodes
            !
            if (isaggn(n)>0) isaggn(n) = 0
         enddo
      end select
      end subroutine mkwaqseg_noagg


      subroutine mkexchanges(lintot, nodmin, nodmax, isaggl, isaggn, nlinks, &
                           & links, link_offset, linev, noqmax, noq, iqaggr, &
                           & volcon)
!!--description-----------------------------------------------------------------
! Fill aggregation table for exchanges
!!--declarations----------------------------------------------------------------
!     use xxx
!
      implicit none
!
!           Global variables
!
      integer                 , intent(in)    :: lintot
      integer                 , intent(in)    :: nodmin
      integer                 , intent(in)    :: nodmax
      integer                 , intent(in)    :: noqmax
      integer, dimension(:,:) , intent(in)    :: isaggl
      integer, dimension(:)   , intent(in)    :: isaggn
      integer, dimension(:)   , intent(in)    :: nlinks
      integer, dimension(:)   , intent(in)    :: links
      integer, dimension(:)   , intent(in)    :: link_offset
      integer, dimension(0:,:), intent(in)    :: linev
      integer                 , intent(out)   :: noq
      integer, dimension(:)   , intent(out)   :: iqaggr
      integer, dimension(:,:) , intent(out)   :: volcon
!
!           Local variables
!
      integer :: qindex
      integer :: i1
      integer :: i2
      integer :: istat
      integer :: j
      integer :: j1
      integer :: j2
      integer :: l
      integer :: l1
      integer :: l2
      integer :: n
      integer :: seg1
      integer :: seg2
      integer, dimension(:,:), allocatable :: exchanges
!
!! executable statements -------------------------------------------------------
!
      istat = 0
                    allocate ( exchanges(3,noqmax) , stat=istat)
      if (istat/=0) then
         write(*,*) '*** ERROR: mkexchanges: memory allocation error'
         return
      endif
      exchanges = 0
      !
      ! Loop over links.
      !
      noq = 0
      do l = 1,lintot
         !
         ! Store this within flowlink connection in the volcon table.
         !
         volcon(1,l) = 1
         volcon(2,l) = l
         volcon(3,l) = 2
         volcon(4,l) = l
         !
         seg1 = isaggl(1,l)
         seg2 = isaggl(2,l)
         if (seg1 == seg2) then
            !
            ! Both parts of the flowlink belong to the same segment: no
            ! exchange will be created.
            !
            iqaggr(l) = 0
            cycle
         endif
         !
         ! This is an exchange, is it new?
         ! To determine that we need to check whether the pair of
         ! segments is already in the list. Search in list.
         !
         call findexchange(exchanges, noq, seg1, seg2, qindex)
         !
         ! Store this information in the link-exchange aggregation
         ! table.
         !
         iqaggr(l) = qindex
      enddo
      !
      ! Loop over nodes.
      !
      l = lintot
      do n = nodmin,nodmax
         !
         ! Nodes may be associated with a segment or not.
         !
         if (isaggn(n)==0) then
            !
            ! If the node has no segment ID, the exchanges are defined
            ! from flowlink to flowlink. Loop over all flowlinks
            ! connected to this node.
            !
            do j1 = 1,nlinks(n)
               l1 = links(link_offset(n)+j1)
               !
               ! First flowlink connected to node n ...
               !
               seg1 = 0
               if (linev(l1,5) == n) then
                  i1 = 1
                  seg1 = isaggl(1,l1)
               elseif (linev(l1,6) == n) then
                  i1 = 2
                  seg1 = isaggl(2,l1)
               endif
               !
               do j2 = j1+1,nlinks(n)
                  l2 = links(link_offset(n)+j2)
                  !
                  ! ... and second flowlink connected to node n ...
                  !
                  seg2 = 0
                  if (linev(l2,5) == n) then
                     i2 = 1
                     seg2 = isaggl(1,l2)
                  elseif (linev(l2,6) == n) then
                     i2 = 2
                     seg2 = isaggl(2,l2)
                  endif
                  !
                  ! ... are connected ...
                  !
                  l = l+1
                  !
                  ! ... and may together result in a new exchange.
                  ! Store this flowlink-flowlink connection in the
                  ! volcon table.
                  !
                  volcon(1,l) = i1
                  volcon(2,l) = l1
                  volcon(3,l) = i2
                  volcon(4,l) = l2
                  !
                  if (seg1 == seg2) then
                     !
                     ! The two links belong to the same segment: no
                     ! exchange will be created.
                     !
                     iqaggr(l) = 0
                     cycle
                  endif
                  !
                  call findexchange(exchanges, noq, seg1, seg2, qindex)
                  !
                  ! Store this information in the
                  ! node-exchange aggregation table.
                  !
                  iqaggr(l) = qindex
               enddo
            enddo
         else
            !
            ! If the node has a segment ID, the exchanges connect the
            ! flowlinks to this node.
            !
            do j = 1,nlinks(n)
               l2 = links(link_offset(n)+j)
               if (linev(l2,5) == n) then
                  seg1 = isaggn(n)
                  i2 = 1
                  seg2 = isaggl(1,l2)
               elseif (linev(l2,6) == n) then
                  seg1 = isaggn(n)
                  i2 = 2
                  seg2 = isaggl(2,l2)
               else
                  cycle
               endif
               !
               ! Node n and flowlink l1 are connected ...
               !
               l = l+1
               !
               ! ... and may together result in a new exchange.
               ! Store this flowlink-node connection in the volcon
               ! table.
               !
               volcon(1,l) = 0
               volcon(2,l) = n
               volcon(3,l) = i2
               volcon(4,l) = l2
               !
               if (seg1 == seg2) then
                  !
                  ! The node and link belong to the same segment: no
                  ! exchange will be created.
                  !
                  iqaggr(l) = 0
                  cycle
               endif
               !
               call findexchange(exchanges, noq, seg1, seg2, qindex)
               !
               ! Store this information in the node-exchange
               ! aggregation table.
               !
               iqaggr(l) = qindex
            enddo
         endif
      enddo
      !
      deallocate ( exchanges , stat=istat)
      end subroutine mkexchanges
      
      
      subroutine findexchange(exchanges, noq, seg1, seg2, index)
!!--description-----------------------------------------------------------------
! Search exchange list to check whether an exchange exists between seg1 and seg2
!!--declarations----------------------------------------------------------------
!     use xxx
!
      implicit none
!
!           Global variables
!
      integer                  , intent(inout) :: noq
      integer, dimension(:,:)  , intent(inout) :: exchanges ! 3,noqmax
      integer                  , intent(inout) :: seg1
      integer                  , intent(inout) :: seg2
      integer                  , intent(out)   :: index
!
!           Local variables
!
      integer :: iq
      integer :: q1
      integer :: q2
      logical :: flipped
!
!! executable statements -------------------------------------------------------
!
      !
      ! Routine searches for element in array using bubble-search algorithm.
      ! INDEX equals the index of the element that matches the element that the
      ! routine searched for. If the element not yet occurs in the list then it
      ! is added and the associated INDEX is returned.
      !
      ! Segment number 1 must be smaller than segment number 2. If that is not
      ! the case, then flip the segment numbers. The variable INDEX will be
      ! negative if seg1>seg2.
      !
      flipped = .false.
      if (seg1>seg2) then
         flipped = .true.
         iq = seg1
         seg1 = seg2
         seg2 = iq
      elseif (seg1==seg2) then
         index = 0
         return
      endif
      !
      if (noq==0) then
         q2 = 1
      else
         q1 = 0
         q2 = noq+1
         do ! while (.true.)
            iq = (q1+q2)/2
            if (exchanges(1,iq)<seg1) then
               q1 = iq
            elseif (exchanges(1,iq)>seg1) then
               q2 = iq
            elseif (exchanges(2,iq)<seg2) then
               q1 = iq
            elseif (exchanges(2,iq)>seg2) then
               q2 = iq
            else
               !
               ! index found
               !
               index = exchanges(3,iq)
               if (flipped) index = -index
               return
            endif
            if (q2 == q1+1) exit
         enddo
      endif
      !
      ! segment pair not found, add segment pair to exchanges array
      ! and increment noq.
      !
      do q1 = noq,q2,-1
         exchanges(:,q1+1) = exchanges(:,q1)
      enddo
      exchanges(1,q2) = seg1
      exchanges(2,q2) = seg2
      noq = noq+1
      !
      index = noq
      exchanges(3,q2) = index
      if (flipped) index = -index
      end subroutine findexchange


      subroutine mkwaqpoi(isaggl, isaggn, iqaggr, volcon, noqmax, ifrmto)
!!--description-----------------------------------------------------------------
! Create from/to pointer table from aggregation data
!!--declarations----------------------------------------------------------------
      use cpluv
!
      implicit none
!
!           Global variables
!
      integer                , intent(in)  :: noqmax
      integer, dimension(:)  , intent(in)  :: iqaggr
      integer, dimension(:)  , intent(in)  :: isaggn
      integer, dimension(:,:), intent(in)  :: isaggl
      integer, dimension(:,:), intent(in)  :: volcon
      integer, dimension(:,:), intent(out) :: ifrmto
!
!           Local variables
!
      integer :: i1
      integer :: i2
      integer :: iq
      integer :: l
      integer :: l1
      integer :: l2
      integer :: seg1
      integer :: seg2
!
!! executable statements -------------------------------------------------------
!
      !
      ! Loop over all connections.
      !
      ifrmto = 0
      do l = 1,noqmax
         !
         ! If this connection is across segment boundaries then it will have a
         ! non-zero exchange number.
         !
         iq = iqaggr(l)
         if (iq /= 0) then
            i1 = volcon(1,l)
            l1 = volcon(2,l)
            i2 = volcon(3,l)
            l2 = volcon(4,l)
            !
            ! Distinguish between the case of a flowlink-node connection and
            ! the case of a flowlink-flowlink connection. Nodes are indicated
            ! by i1 equal to 0.
            !
            if (i1==0) then
               !
               ! Connection of node to flowlink.
               !
               seg1 = isaggn(l1)
               seg2 = isaggl(i2,l2)
            else
               !
               ! Connection of two (parts of) flowlinks.
               !
               seg1 = isaggl(i1,l1)
               seg2 = isaggl(i2,l2)
            endif
            !
            ! Fill from/to table
            !
            if (iq>0) then
               ifrmto(1,iq) = seg1
               ifrmto(2,iq) = seg2
            else
               ifrmto(1,-iq) = seg2
               ifrmto(2,-iq) = seg1
            endif
         endif
      enddo
      !
      ! We are not yet filling the "from-1" (3) and "to+1" (4) entries. These
      ! are more tricky, may cause asymmetry, and should probably be done
      ! differently for 1D and 2D.
      !
      end subroutine mkwaqpoi

      
      subroutine mkwaqlen(ifrmto, noq, isaggn, isaggl, noseg, lenseg, lenex)
!!--description-----------------------------------------------------------------
! Compute from/to segment lengths at exchanges
! Note that this procedure does not allow for aggregating volumes across
! 3 or more branches since segment lengths can't be added linearly in such
! cases.
!!--declarations----------------------------------------------------------------
      use precision
      use cpluv
      use ident
      use modelGlobalData ! for structures derived type
!
      implicit none
!
!           Global variables
!
      integer, dimension(:,:)  , intent(in)  :: ifrmto ! 4,noq
      integer, dimension(:)    , intent(in)  :: isaggn ! nodmax
      integer, dimension(:,:)  , intent(in)  :: isaggl ! 2,lintot
      integer                  , intent(in)  :: noq
      integer                  , intent(in)  :: noseg
      real(hp), dimension(:)   , intent(out) :: lenseg ! noseg
      real(hp), dimension(:,:) , intent(out) :: lenex  ! 2,noq
!
!           Local variables
!
      integer :: ifrm
      integer :: ito
      integer :: l
      integer :: m
      integer :: n
      integer :: nbr
      integer :: ex
      integer :: s
      real    :: factor1
      real    :: factor2
!
!! executable statements -------------------------------------------------------
!
      !
      ! Initialize segment lengths
      !
      lenseg = 0.0
      !
      ! Loop over all nodes, count surface area of nodes with volume
      !
      do n = nodmin,nodmax
         s = isaggn(n)
         ! from plhidwdio: nodesvol.his/par(2) = nodsur(nod) for nod = 1,nnodpl
         !if (s>0) lenseg(s) = lenseg(s) + sqrt(nodsur(nod))
      enddo
      !
      ! Loop over all links, count distance along branches.
      !
      do nbr = 1,nbranp
         do m = mbr(nbr,1)-1,mbr(nbr,2)
            l = mtol(m)
            !
            if (kcu(m)==1) then
               factor1 = 0.5
               factor2 = 0.5
            elseif ((kcu(m)>1).and. &
                    (struclinkup(pluvstr(m)) /= '').and. &
                    (struclinkdwn(pluvstr(m)) /= '')) then
               factor1 = (structures(pluvstr(m))%distance - xgrid(m))  / &
                       & (xgrid(m+1)-xgrid(m))
               factor2 = (xgrid(m+1) - structures(pluvstr(m))%distance)/ &
                       & (xgrid(m+1)-xgrid(m))
            else
               factor1 = 0.5
               factor2 = 0.5
            endif
            !
            s = isaggl(1,l)
            if (s>0) lenseg(s) = lenseg(s) + factor1*dx(m)
            !
            s = isaggl(2,l)
            if (s>0) lenseg(s) = lenseg(s) + factor2*dx(m)
         enddo
      enddo
      !
      ! Loop over all exchanges.
      !
      do ex = 1,noq
         ito  = ifrmto(1,ex) ! TO segment
         ifrm = ifrmto(2,ex) ! FROM segment
         !
         ! if TO/FROM is not boundary segment, then use half the segment length
         !
         if (ito>0)  lenex(1,ex) = lenseg(ito)/2.0
         if (ifrm>0) lenex(2,ex) = lenseg(ifrm)/2.0
         !
         ! if TO/FROM is boundary segment, then mirror the internal length
         !
         if (ito<=0)  lenex(1,ex) = lenex(2,ex)
         if (ifrm<=0) lenex(2,ex) = lenex(1,ex)
      enddo
      end subroutine mkwaqlen

      
      subroutine mkelmvol(ntab, cross, mxcross, table, isaggn, isaggl, &
                        & linkvol, nodevol)
!!--description-----------------------------------------------------------------
! Compute elementary volumes at links
!!--declarations----------------------------------------------------------------
      use precision
      use cpluv
!
      implicit none
!
!           Global variables
!
      integer                   , intent(in)   :: mxcross
      integer, dimension(4, *)  , intent(in)   :: ntab
      real, dimension(mxcross,*), intent(in)   :: cross
      real(hp), dimension(*)    , intent(in)   :: table
      integer, dimension(:)     , intent(in)   :: isaggn   ! nodmax
      integer, dimension(:,:)   , intent(in)   :: isaggl   ! 2,lintot
      real(hp), dimension(:,:)  , intent(out)  :: linkvol  ! 2,lintot
      real(hp), dimension(:)    , intent(out)  :: nodevol  ! nodmax
!
!           Local variables
!
      real(hp) :: dpt
      real(hp) :: surfarea
      integer :: l
      integer :: m
      integer :: n
      integer :: nbr
!
!! executable statements -------------------------------------------------------
!
      !
      ! Initialize volume array
      !
      linkvol = 0.0
      !
      ! Loop over all links, determine link volumes.
      !
      do nbr = 1,nbranp
         do m = mbr(nbr,1)-1,mbr(nbr,2)
            l = mtol(m)
            !
            ! Calculate the volumes of the two half links.
            !
            linkvol(1,l) = complinkvol(ntab, table, cross, mxcross, m, 1)
            linkvol(2,l) = complinkvol(ntab, table, cross, mxcross, m, 2)
         enddo
      enddo
      !
      ! Determine node volumes.
      !
      nodevol = 0.0
      do n = 1,ngridp
         if (kcs(n)>0) then
            dpt = max(s1(n)+dp(n),0.0)
            call well(n, dpt, nodevol(n), surfarea, ntab, table)
         endif
      enddo
      end subroutine mkelmvol


      function complinkvol(ntab, table, cross, mxcross, m, half) result (vol)
!!--description-----------------------------------------------------------------
! Compute volume of half link
!!--declarations----------------------------------------------------------------
      use precision
      use cpluv
      use modelGlobalData ! for structures derived type
!
      implicit none
!
!           Global variables
!
      integer                   , intent(in) :: mxcross
      integer, dimension(4, *)  , intent(in) :: ntab
      real, dimension(mxcross,*), intent(in) :: cross
      real(hp), dimension(*)    , intent(in) :: table
      integer                   , intent(in) :: m      ! pluvius link number
      integer                   , intent(in) :: half   ! 1 (left half), 2 (right half)
      real(hp)                               :: vol    ! volume of half link
!
!           Local variables
!
      integer :: mnod
      integer, dimension(3, 1) :: dummybfrict
      real(hp) :: asm
      real(hp) :: ddummy
      real(hp) :: dpm
      real(hp) :: factor
      real(hp) :: kdummy
      real(hp) :: wdummy
!
!! executable statements -------------------------------------------------------
!
         !
         ! Determine associated (calculation) node mnod
         !
         if (half==1) then
            mnod = m
         else
            mnod = m+1
         endif
         !
         dpm = max(0.0D0, bob(m, half) + s1(mnod))
         if (dpm<1.0D-5) then
            asm = 0.0
         else
            call wetcrs(ntab, table, cross, dummybfrict, ic(mnod), mnod, 0, &
                      & dpm, asm, 0, ddummy, 0, wdummy, 0, kdummy, .false., &
                      & m, half)
            ! substract ground layer from wetted area
            call adjustforgroundlayer(m, mnod, dpm, .true., asm, .false.,   &
                                    & ddummy, .false., wdummy, .false.)
            ! Add storage above surface level defined at the cross sections at
            ! location on reach
            call adjustforsurfacestorage(mnod, mnod, m, half, .true., asm,  &
                                       & .false., ddummy, ntab, table,      &
                                       & cross)
            ! Add summer dike wetter area (if any)
            call adjustforsummerdike(mnod, asm, ddummy, .false.)
         endif
         if (kcu(m)/=1) then
            if (half==1) then
               factor = (structures(pluvstr(m))%distance - xgrid(mnod))     &
                      & /(xgrid(m+1) - xgrid(m))
            else
               factor = (xgrid(mnod) - structures(pluvstr(m))%distance)     &
                      & /(xgrid(m+1) - xgrid(m))
            endif
         else
            factor = 0.5
         endif
         vol = factor*asm*dx(m)
      end function complinkvol

      
      subroutine mkwaqvol(isaggn, isaggl, linkvol, nodevol, vol)
!!--description-----------------------------------------------------------------
! Compute volumes
!!--declarations----------------------------------------------------------------
      use precision
      use cpluv
!
      implicit none
!
!           Global variables
!
      integer, dimension(:)    , intent(in)  :: isaggn   ! nodmax
      integer, dimension(:,:)  , intent(in)  :: isaggl   ! 2,lintot
      real(hp), dimension(:,:) , intent(in)  :: linkvol  ! 2,lintot
      real(hp), dimension(:)   , intent(in)  :: nodevol  ! nodmax
      real(hp), dimension(:)   , intent(out) :: vol      ! noseg
!
!           Local variables
!
      real(hp) :: nodvol
      integer  :: l
      integer  :: m
      integer  :: n
      integer  :: nbr
      integer  :: s
      logical  :: internal
!
!! executable statements -------------------------------------------------------
!
      !
      ! Initialize volume array
      !
      vol = 0.0
      !
      ! Loop over all nodes, accumulate node volumes.
      !
      do n = nodmin,nodmax
         s = isaggn(n)
         ! from volwaq: volwq(n)? nodsur(n)?
         !if (s>0) vol(s) = vol(s) + ...(n)
      enddo
      !
      ! Loop over all links, accumulate link volumes.
      !
      do nbr = 1,nbranp
         !
         ! First half of first flowlink in branch
         !
         m = mbr(nbr,1)-1
         internal = mbr(nbr,5) /= 0 .and. (kcu(m)==1 .or. strucvol(m)==1)
         l = mtol(m)
         s = isaggl(1,l)
         if (s>0 .and. internal) then
            vol(s) = vol(s) + linkvol(1,l)
         endif
         !
         ! Loop over calculation nodes.
         !
         do m = mbr(nbr,1),mbr(nbr,2)
            nodvol = nodevol(m)
            !
            ! The half flowlink before this calculation node.
            !
            l = mtol(m-1)
            s = isaggl(2,l)
            if (s>0) then
               vol(s) = vol(s) + 0.5*nodvol + linkvol(2,l)
            endif
            !
            ! The half flowlink after this calculation node.
            !
            l = mtol(m)
            s = isaggl(1,l)
            if (s>0) then
               vol(s) = vol(s) + 0.5*nodvol + linkvol(1,l)
            endif
         enddo
         !
         ! Second half of last flowlink in branch
         !
         m = mbr(nbr,2)
         internal = mbr(nbr,6) /= 0 .and. (kcu(m)==1 .or. strucvol(m)==1)
         l = mtol(m)
         s = isaggl(2,l)
         if (s>0 .and. internal) then
            vol(s) = vol(s) + linkvol(2,l)
         endif
      enddo

      end subroutine mkwaqvol

      
      subroutine mkwaqexchareas(ifrmto, vol, lenseg, noq, area)
!!--description-----------------------------------------------------------------
! Compute exchange areas  !wqexar in old wqinr
!!--declarations----------------------------------------------------------------
      use precision
!
      implicit none
!
!           Global variables
!
      integer, dimension(:,:)  , intent(in)  :: ifrmto  ! 4,noq
      integer                  , intent(in)  :: noq
      real(hp), dimension(:)   , intent(in)  :: vol     ! noseg
      real(hp), dimension(:)   , intent(in)  :: lenseg  ! noseg
      real(hp), dimension(:)   , intent(out) :: area    ! noq
!
!           Local variables
!
      integer :: iq
      integer :: s1
      integer :: s2
      real(hp) :: area1
      real(hp) :: area2

!
!! executable statements -------------------------------------------------------
!
      !
      ! Initialize area array
      !
      area = 0.0
      !
      ! loop over all exchanges
      !
      do iq = 1,noq
         s1 = ifrmto(1,iq)
         s2 = ifrmto(2,iq)
         !
         area1 = 1e25
         if (s1>0) area1 = vol(s1) / lenseg(s1)
         area2 = 1e25
         if (s2>0) area2 = vol(s2) / lenseg(s2)
         !
         area(iq) = area(iq) + min(area1,area2)
      enddo
      end subroutine mkwaqexchareas


      subroutine mkelmdis(linkvol0,linkvol1,linkdis)
!!--description-----------------------------------------------------------------
! Determine fluxes at start, centre and end of links
!!--declarations----------------------------------------------------------------
      use precision
      use c_stat
      use cpluv
!
      implicit none
!
!           Global variables
!
      real(hp), dimension(:,:)  , intent(in)  :: linkvol0  ! 2,lintot
      real(hp), dimension(:,:)  , intent(in)  :: linkvol1  ! 2,lintot
      real(hp), dimension(0:,:) , intent(out) :: linkdis   ! 3,lintot
!
!           Local variables
!
      integer :: l
      integer :: m
      integer :: nbr
!
!! executable statements -------------------------------------------------------
!
      !
      ! initialize all discharges to 0.
      !
      linkdis = 0.0
      do nbr = 1,nbranp
         do m = mbr(nbr,1)-1,mbr(nbr,2)
            l = mtol(m)
            !
            ! flux at centre
            !
            linkdis(0,l) = qcum(m)/dtcum
            !
            ! flux at start
            !
            linkdis(1,l) = linkdis(0,l) + (linkvol1(1,l) - linkvol0(1,l))/dtcum
            !
            ! flux at end
            !
            linkdis(2,l) = linkdis(0,l) - (linkvol1(2,l) - linkvol0(2,l))/dtcum
         enddo
      enddo
      end subroutine mkelmdis


      subroutine mkwaqexchflows(iqaggr, isaggn, linkdis, nlinks, volcon, lintot, &
                              & nodmin, nodmax, qag)
!!--description-----------------------------------------------------------------
! Compute exchange fluxes  !wqexfl in old wqinr
!!--declarations----------------------------------------------------------------
      use precision
!
      implicit none
!
!           Global variables
!
      integer, dimension(:)    , intent(in)  :: iqaggr  ! noqmax
      integer, dimension(:)    , intent(in)  :: isaggn  ! nodmax
      integer, dimension(:)    , intent(in)  :: nlinks  ! nodmax
      integer, dimension(:,:)  , intent(in)  :: volcon  ! 4,noqmax
      real(hp), dimension(0:,:), intent(in)  :: linkdis ! 3,lintot
      integer                  , intent(in)  :: lintot
      integer                  , intent(in)  :: nodmin
      integer                  , intent(in)  :: nodmax
      real(hp), dimension(:)   , intent(out) :: qag     ! noq
!
!           Local variables
!
      integer  :: i
      integer  :: i1
      integer  :: i2
      integer  :: l
      integer  :: l1
      integer  :: l2
      integer  :: n
      integer  :: iq
      real(hp) :: q1
      real(hp) :: q2
      real(hp) :: qin
      real(hp) :: qnew
      real(hp) :: qout

!
!! executable statements -------------------------------------------------------
!
      !
      ! Initialize flux array, before accumulating fluxes. Since we are not
      ! allowed to aggregate across branches (see comments in mkwaqlen)
      ! segments in a 1D network are either linear or point segments. As a
      ! result, all exchanges will correspond to fluxes at one particular
      ! location in the network. Thus, we could just assign instead of
      ! accumulate fluxes. However, to be more flexible to future extensions
      ! we accumulate fluxes here.
      !
      qag  = 0.0
      !
      ! loop over all flowlinks
      !
      do l = 1,lintot
         !
         ! if segment exchange corresponds to a flowlink, then add the
         ! discharge of that flowlink to the segment exchange.
         !
         iq = iqaggr(l)
         if (iq/=0) then
            qnew = linkdis(0,l)
            !
            ! if iq<0 then isaggl(2,l)<isaggl(1,l). Positive flux at
            ! exchange is always from lower segment number to higher
            ! segment number, hence flip sign of iq
            !
            if (iq<0) then
               qnew = -qnew
               iq = iabs(iq)
            endif
            qag(iq) = qag(iq) + qnew
         endif
      enddo
      !
      ! loop over all nodes
      !
      l = lintot
      do n = nodmin,nodmax
         if (isaggn(n) /= 0) then
            !
            ! if node has volume and thus has segment associated with it, then
            ! all links will connect to node (includes boundary case).
            !
            do i = 1,nlinks(n) !while (l+1<noqmax .and. volcon(2,l+1)==n)
               l = l+1
               iq = iqaggr(l)
               !
               if (iq/=0) then
                  !
                  ! exchange from node to flowlink
                  !
                  i2 = volcon(3,l)
                  l2 = volcon(4,l)
                  qnew = linkdis(i2,l2)
                  if (i2==1) qnew = -qnew
                  !
                  if (iq<0) then
                     qnew = -qnew
                     iq = iabs(iq)
                  endif
                  qag(iq) = qag(iq) + qnew
               endif
            enddo
         else
            !
            ! node is not associated with a waq volume, hence all links are
            ! connected to each other. Split all link to node fluxes of flow
            ! across the segment exchanges in waq.
            !
            ! for such nodes:
            !  * determine total inflow, total outflow
            !  * for each associated exchange: determine relative contribution
            !    to overall flow and assign the associated flux
            !
            qin = 0.0
            qout = 0.0
            do i = 1,nlinks(n)
               !
               ! the entries in the volcon table read
               !    L1 L2
               !    L1 L3
               !    :  :
               !    L1 LN
               !    L2 L3
               !    :  :
               !    L2 LN
               !    etc.
               ! use the first NLINKS-1 entries to determine the flowlinks
               !
               if (i==1) then
                  i1 = volcon(1,l+1)
                  l1 = volcon(2,l+1)
                  qnew = linkdis(i1,l1)
                  if (i1==1) qnew = -qnew
               else
                  i2 = volcon(3,l+i-1)
                  l2 = volcon(4,l+i-1)
                  qnew = linkdis(i2,l2)
                  if (i2==1) qnew = -qnew
               endif
               !
               if (qnew>0.0) then
                  qin  = qin + qnew
               else
                  qout = qout - qnew
               endif
            enddo
            !
            do i = 1,(nlinks(n)*(nlinks(n)-1))/2
               l = l+1
               iq = iqaggr(l)
               !
               if (iq/=0 .and. qout>0.0) then
                  i1 = volcon(1,l)
                  l1 = volcon(2,l)
                  i2 = volcon(3,l)
                  l2 = volcon(4,l)
                  !
                  q1 = linkdis(i1,l1)
                  if (i1==1) q1 = -q1
                  q2 = linkdis(i2,l2)
                  if (i2==1) q2 = -q2
                  !
                  if (q1*q2<0.0) then
                     qnew = q1*abs(q2)/qout
                     !
                     if (iq<0) then
                        qnew = -qnew
                        iq = iabs(iq)
                     endif
                     qag(iq) = qag(iq) + qnew
                  endif
               endif
            enddo
         endif
      enddo
      end subroutine mkwaqexchflows


      subroutine wrwaqseg(isaggl, isaggn, nobnd, noseg, filename)
!!--description-----------------------------------------------------------------
! Write ASCII or binary segment definition file
!!--declarations----------------------------------------------------------------
      use cpluv
      use ident
!
      implicit none
!
!           Global variables
!
      integer, dimension(:,:), intent(in) :: isaggl
      integer, dimension(:)  , intent(in) :: isaggn
      integer                , intent(in) :: nobnd
      integer                , intent(in) :: noseg
      character(*)           , intent(in) :: filename
!
!           Local variables
!
      integer :: l
      integer :: lunout
      integer :: n
      integer :: s
      character(40)   :: name
      character(1024) :: volumes
      !
      integer, external :: newunit
!
!! executable statements -------------------------------------------------------
!
         lunout = newunit()
         open(lunout, file=filename)
         write(lunout,'(a,i)') 'nsegments = ',noseg
         do s = 1,noseg
            volumes = ''
            do n = nodmin,nodmax
               if (isaggn(n)==s) then
                  name = 'Node('''//trim(gridid(grdp2s(n)))//''')'
                  if (volumes == '') then
                     volumes = name
                  else
                     volumes = trim(volumes)//', '//trim(name)
                  endif
               endif
            enddo
            do l = 1,lintot
               if (isaggl(1,l)==s) then
                  name = 'FromLink('''//trim(rchsegid(grdp2s(iabs(linev(l,1)))))//''')'
                  if (volumes == '') then
                     volumes = name
                  else
                     volumes = trim(volumes)//', '//trim(name)
                  endif
               endif
               if (isaggl(2,l)==s) then
                  name = 'ToLink('''//trim(rchsegid2(grdp2s(iabs(linev(l,1)))+1))//''')'
                  if (volumes == '') then
                     volumes = name
                  else
                     volumes = trim(volumes)//', '//trim(name)
                  endif
               endif
            enddo
            write(lunout,'(a,i5,2a)') '''Segment',s,''' : ',trim(volumes)
         enddo
         write(lunout,'(a,i)') 'nboundaries = ',nobnd
         do s = 1,nobnd
            volumes = ''
            do n = nodmin,nodmax
               if (isaggn(n)==-s) then
                  name = 'Node('''//trim(gridid(grdp2s(n)))//''')'
                  if (volumes == '') then
                     volumes = name
                  else
                     volumes = trim(volumes)//', '//trim(name)
                  endif
               endif
            enddo
            write(lunout,'(a,i5,2a)') '''Boundary',s,''' : ',trim(volumes)
         enddo
         close(lunout)
      end subroutine wrwaqseg
           
      end module wrwaqfil1d