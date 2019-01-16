!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2019.                                
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
      subroutine merge_domains(hyd, domain_hyd_coll)

      ! function : merge the domains, make pointers to final domain

      ! global declarations

      use hydmod
      use m_alloc
      implicit none

      ! declaration of the arguments

      type(t_hyd)                            :: hyd                    ! description of the hydrodynamics
      type(t_hyd_coll)                       :: domain_hyd_coll        ! description of all domain hydrodynamics

      ! local declarations

      integer                                :: n_domain               ! number of domains
      integer                                :: i_domain               ! index in collection
      integer                                :: idmn                   ! flow like domain index (0:n_domain-1)
      type(t_hyd), pointer                   :: domain_hyd             ! description of one domain hydrodynamics
      integer                                :: noseg                  ! total number of segments
      integer                                :: nosegl                 ! total number of segments per layer
      integer                                :: nobnd                  ! total number of boundaries
      integer                                :: nobndl                 ! total number of boundaries per layer
      integer                                :: nolay                  ! number of layers
      integer                                :: noq1                   ! total number of exchanges in first directory
      integer                                :: noq1l_domain           ! number of exchanhes per layer in domain
      integer                                :: iseg                   ! segment index
      integer                                :: isegl                  ! segment index in layer
      integer                                :: iseg_glob              ! global segment index
      integer                                :: ilay                   ! layer index
      integer                                :: iq                     ! exchange index
      integer                                :: iq_global              ! global exchange index layer
      integer                                :: iq_glob                ! global exchange index overall
      integer                                :: iq_domain              ! domain exchange index
      integer                                :: ip1                    ! segment pointer index
      integer                                :: ip2                    ! segment pointer index
      integer                                :: ip3                    ! segment pointer index
      integer                                :: ip4                    ! segment pointer index
      integer                                :: ip1_lay                ! segment pointer index
      integer                                :: ip2_lay                ! segment pointer index
      integer                                :: ip3_lay                ! segment pointer index
      integer                                :: ip4_lay                ! segment pointer index
      integer                                :: numcontpts             ! numcontpts number of contour nodes
      integer                                :: no_sect                ! number of sections
      integer                                :: i_sect                 ! index of section
      integer                                :: isect                  ! index of section
      integer                                :: no_bnd                 ! number of boundaries in section
      integer                                :: i_bnd                  ! index of boundary
      type(t_openbndsect), pointer           :: openbndsect            ! single section
      type(t_openbndlin),pointer             :: openbndlin             ! single open boundary lin
      type(t_openbndsect)                    :: new_sect               ! single section new
      logical                                :: bnd_active             ! if a boundary is active
      integer                                :: iret                   ! return value
      
      integer                                :: ik                     ! node counter
      integer                                :: il                     ! link counter
      integer                                :: nodeoffset             ! node offset
      integer                                :: nodelinkoffset         ! node link offset
      integer                                :: nv                     ! max node for element
      integer                                :: inv                    ! index countour node for element

      integer, allocatable                   :: iglobal_active(:)      ! does a global segment actually exist
      integer, allocatable                   :: iglobal_new(:)         ! what is the new iglobal for all old iglobal
      integer                                :: inew                   ! new number

      ! allocate local arrays

      n_domain = domain_hyd_coll%cursize

      ! copy projection attributes
      
      hyd%crs  = domain_hyd_coll%hyd_pnts(1)%crs

      ! init totals

      hyd%nmax  = 1
      hyd%kmax  = domain_hyd_coll%hyd_pnts(1)%kmax
      hyd%nolay = domain_hyd_coll%hyd_pnts(1)%nolay
      hyd%geometry    = domain_hyd_coll%hyd_pnts(1)%geometry
      hyd%sal_present = domain_hyd_coll%hyd_pnts(1)%sal_present
      hyd%tem_present = domain_hyd_coll%hyd_pnts(1)%tem_present
      hyd%tau_present = domain_hyd_coll%hyd_pnts(1)%tau_present
      hyd%vdf_present = domain_hyd_coll%hyd_pnts(1)%vdf_present
      hyd%description = ' '
      hyd%hyd_ref     = domain_hyd_coll%hyd_pnts(1)%hyd_ref
      hyd%hyd_start   = domain_hyd_coll%hyd_pnts(1)%hyd_start
      hyd%hyd_stop    = domain_hyd_coll%hyd_pnts(1)%hyd_stop
      hyd%hyd_step    = domain_hyd_coll%hyd_pnts(1)%hyd_step
      hyd%cnv_ref     = domain_hyd_coll%hyd_pnts(1)%cnv_ref
      hyd%cnv_start   = domain_hyd_coll%hyd_pnts(1)%cnv_start
      hyd%cnv_stop    = domain_hyd_coll%hyd_pnts(1)%cnv_stop
      hyd%cnv_step    = domain_hyd_coll%hyd_pnts(1)%cnv_step
      hyd%cnv_step_sec= domain_hyd_coll%hyd_pnts(1)%cnv_step_sec
	  
      hyd%openbndsect_coll%maxsize = 0
      hyd%openbndsect_coll%cursize = 0
      hyd%wasteload_coll%cursize = 0
      hyd%wasteload_coll%maxsize = 0
      hyd%dd_bound_coll%cursize = 0
      hyd%dd_bound_coll%maxsize = 0

      ! jvb_check 2D, later 3D

      ! iglobal is only partially filled first look for highest number

      nosegl = 0
      hyd%numk = 0
      hyd%numl = 0
      hyd%nv = 0
      hyd%nump = 0
      do i_domain = 1, n_domain
         idmn = i_domain - 1
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do iseg = 1, domain_hyd%nosegl
            if ( domain_hyd%idomain(iseg) .eq. idmn ) then
               nosegl = max(nosegl,domain_hyd%iglobal(iseg))
               hyd%nump = hyd%nump + 1
            endif
         enddo
         hyd%numk = hyd%numk + domain_hyd%numk
         hyd%numl = hyd%numl + domain_hyd%numl
         hyd%nv   = max(hyd%nv, domain_hyd%nv)
      enddo

      if (hyd%nump.lt.nosegl) then
         ! Apparently the highest iglobal is higher than the sum of the number of active cells of each domain.
         ! We have to skip inactive cells, create a renumber list, and update the global segment numbers in each domain.
         write (msgbuf, '(a)')  'Apparently the highest iglobal is higher than the sum of the number of active cells of each domain.'
         call msg_flush()
         write (msgbuf, '(a,i10,a,i10)') 'Highest iglobal:', nosegl, ', number of active cells:', hyd%nump
         call msg_flush()
         write (msgbuf, '(a)') 'We have to skip inactive cells, create a renumber list,'
         call msg_flush()
         write (msgbuf, '(a)') 'and update the global segment numbers in each domain.'
         call msg_flush()
         
         call realloc (iglobal_active, nosegl, fill=0)
         call realloc (iglobal_new, nosegl, fill=0)
         do i_domain = 1, n_domain
            idmn = i_domain - 1
            domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            do iseg = 1, domain_hyd%nosegl
               iglobal_active(domain_hyd%iglobal(iseg)) = 1
            enddo
         enddo
         inew = 0
         do iseg = 1, nosegl
            if (iglobal_active(iseg).eq.1) then
               inew = inew + 1
               iglobal_new(iseg) = inew
            endif
         end do
         if (inew.ne.hyd%nump) then
            write (msgbuf, '(a)') 'Unfortunatly the renumbering went wrong!'
            call msg_flush()
            write (msgbuf, '(a,i10,a,i10)') 'Highest new global number: ', inew, ', number of active cells:', hyd%nump
            call err_flush()
            write(*,*) 
            stop
         end if
         write (msgbuf, '(a)')  'New numbering is fine!'
         call msg_flush()
         write (msgbuf, '(a,i10,a,i10)')  'Highest new global number: ', inew, ', number of active cells:', hyd%nump
         call msg_flush()
         nosegl = hyd%nump
         do i_domain = 1, n_domain
            idmn = i_domain - 1
            domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            do iseg = 1, domain_hyd%nosegl
               domain_hyd%iglobal(iseg) = iglobal_new(domain_hyd%iglobal(iseg))
            enddo
         enddo
      endif
      
      ! sequentially fill in segment numbers in the third dimension (when hyd nolay > 1)

      if (hyd%nolay.gt.1) then
         do i_domain = 1, n_domain
            idmn = i_domain - 1
            domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            do iseg = 1, domain_hyd%nosegl
               do ilay = 2, domain_hyd%nolay
                  domain_hyd%idomain(iseg + (ilay - 1) * domain_hyd%nosegl) = domain_hyd%idomain(iseg)
                  domain_hyd%iglobal(iseg + (ilay - 1) * domain_hyd%nosegl) = domain_hyd%iglobal(iseg) + (ilay - 1) * nosegl
               enddo
            enddo
         enddo
      endif

      ! set the dimensions of the overall domain

      hyd%nosegl = nosegl
      hyd%noseg  = nosegl * hyd%nolay
      hyd%mmax   = nosegl

      ! global exchanges count, boundary count

      noq1  = 0
      nobnd = 0
      do i_domain = 1, n_domain
         idmn = i_domain - 1
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         domain_hyd%iglobal_link = 0
         do iq = 1, domain_hyd%noq1
            noq1 = noq1 + 1
            domain_hyd%iglobal_link(iq) = noq1
            ip1 = domain_hyd%ipoint(1,iq)
            ip2 = domain_hyd%ipoint(2,iq)
            if ( ip1 .lt. 0 ) then
               if (-ip1 .le. domain_hyd%nobndl .and. domain_hyd%idomain(ip2) .eq. idmn) then
                  nobnd = nobnd + 1
                  domain_hyd%iglobal_bnd(-ip1) = -nobnd
                  call renum_bnd(domain_hyd%openbndsect_coll,ip1,-nobnd)
               else if (domain_hyd%idomain(ip2) .ne. idmn) then
                  ! from cell is in ghost domain, revert addition of exchange 
                  domain_hyd%iglobal_link(iq) = 0
                  noq1 = noq1 - 1
               end if
            else if ( ip2 .lt. 0 ) then
               if (-ip2 .le. domain_hyd%nobndl .and. domain_hyd%idomain(ip1) .eq. idmn) then
                  nobnd = nobnd + 1
                  domain_hyd%iglobal_bnd(-ip2) = -nobnd
                  call renum_bnd(domain_hyd%openbndsect_coll,ip2,-nobnd)
               else if (domain_hyd%idomain(ip1) .ne. idmn) then
                  ! from cell is in ghost domain, revert addition of exchange 
                  domain_hyd%iglobal_link(iq) = 0
                  noq1 = noq1 - 1
               end if
            else if (min(domain_hyd%idomain(ip1),domain_hyd%idomain(ip2)) .ne. idmn) then
               ! one of the cells is in ghost domain with a lower domain number, revert addition of exchange 
               domain_hyd%iglobal_link(iq) = 0
               noq1 = noq1 - 1
            end if
         enddo
      enddo
      hyd%noq1 = noq1
      hyd%noq2 = 0
      hyd%noq3 = hyd%nosegl*(hyd%nolay-1)
      hyd%noq4 = 0
      hyd%noq  = hyd%noq1 + hyd%noq2 + hyd%noq3 + hyd%noq4
      hyd%nobndl = nobnd
      hyd%nobnd  = nobnd*hyd%nolay

      ! sequentially fill in boundary numbers in the third dimension (when hyd nolay > 1)

      if (hyd%nolay.gt.1) then
         do i_domain = 1, n_domain
            idmn = i_domain - 1
            domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            do i_bnd = 1, domain_hyd%nobndl
               if ( domain_hyd%iglobal_bnd(i_bnd).ne.0) then
                  do ilay = 2, domain_hyd%nolay
                     domain_hyd%iglobal_bnd(i_bnd + (ilay - 1) * domain_hyd%nobndl) = domain_hyd%iglobal_bnd(i_bnd) - (ilay - 1) * nobnd
                  enddo
               endif
            enddo
         enddo
      endif
      
      ! make final pointer table

      nobnd  = 0
      nobndl = hyd%nobndl
      allocate(hyd%ipoint(4,hyd%noq))
      do i_domain = 1, n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do iq = 1, domain_hyd%noq1
            iq_global = domain_hyd%iglobal_link(iq)
            if ( iq_global .gt. 0 ) then
               ip1 = domain_hyd%ipoint(1,iq)
               ip2 = domain_hyd%ipoint(2,iq)
               ip3 = domain_hyd%ipoint(3,iq)
               ip4 = domain_hyd%ipoint(4,iq)
               if ( ip1 .gt. 0 ) then
                  ip1 = domain_hyd%iglobal(ip1)
               elseif ( ip1 .lt. 0 ) then
                  ip1 = domain_hyd%iglobal_bnd(-ip1)
               else
                  continue ! ip1 = 0
               endif
               if ( ip2 .gt. 0 ) then
                  ip2 = domain_hyd%iglobal(ip2)
               elseif ( ip2 .lt. 0 ) then
                  ip2 = domain_hyd%iglobal_bnd(-ip2)
               else
                  continue ! ip2 = 0
               endif
               if ( ip3 .gt. 0 ) then
                  ip3 = domain_hyd%iglobal(ip3)
               else
                  ip3 = 0
               endif
               if ( ip4 .gt. 0 ) then
                  ip4 = domain_hyd%iglobal(ip4)
               else
                  ip4 = 0
               endif
               if ( ip1 .eq. 0 .or. ip2.eq.0) then
                  continue
               endif
               hyd%ipoint(1,iq_global) = ip1
               hyd%ipoint(2,iq_global) = ip2
               hyd%ipoint(3,iq_global) = ip3
               hyd%ipoint(4,iq_global) = ip4
            endif
         enddo
      enddo

      ! pointers in third dimension

      do iseg = 1, hyd%nosegl
         do ilay = 1, hyd%nolay - 1
            iq_glob = hyd%noq1 + (ilay-1)*hyd%nosegl + iseg
            ip1 = (ilay-1)*hyd%nosegl + iseg
            ip2 = (ilay  )*hyd%nosegl + iseg
            if ( ilay .ne. 1 ) then
               ip3 = (ilay-2)*hyd%nosegl + iseg
            else
               ip3 = 0
            endif
            if ( ilay .ne. hyd%nolay - 1 ) then
               ip4 = (ilay+1)*hyd%nosegl + iseg
            else
               ip4 = 0
            endif
            hyd%ipoint(1,iq_glob) = ip1
            hyd%ipoint(2,iq_glob) = ip2
            hyd%ipoint(3,iq_glob) = ip3
            hyd%ipoint(4,iq_glob) = ip4
         enddo
      enddo

      ! all nodes, node links
      allocate (hyd%xk(hyd%numk))
      allocate (hyd%yk(hyd%numk))
      allocate (hyd%zk(hyd%numk))
      allocate (hyd%kn(2,hyd%numl))
      nodeoffset = 0
      nodelinkoffset = 0
      do i_domain = 1, n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do ik = 1, domain_hyd%numk
            hyd%xk(ik + nodeoffset) = domain_hyd%xk(ik)
            hyd%yk(ik + nodeoffset) = domain_hyd%yk(ik)
            hyd%zk(ik + nodeoffset) = domain_hyd%zk(ik)
         enddo
         do il = 1, domain_hyd%numl
            hyd%kn(1,il + nodelinkoffset) = domain_hyd%kn(1, il) + nodeoffset
            hyd%kn(2,il + nodelinkoffset) = domain_hyd%kn(2, il) + nodeoffset
         enddo
         nodeoffset = nodeoffset + domain_hyd%numk
         nodelinkoffset = nodelinkoffset + domain_hyd%numl
      enddo

      ! coordinates segments

      hyd%numcontpts = 0
      do i_domain = 1, n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         hyd%numcontpts = max(hyd%numcontpts,domain_hyd%numcontpts)
      enddo
      allocate(hyd%xdepth(1,hyd%nosegl))
      allocate(hyd%ydepth(1,hyd%nosegl))
      allocate(hyd%flowelemcontourx(hyd%numcontpts,hyd%nosegl))
      allocate(hyd%flowelemcontoury(hyd%numcontpts,hyd%nosegl))
      allocate(hyd%netcellnod(hyd%nv,hyd%nosegl))
      nodeoffset = 0
      hyd%xdepth           = 0.0
      hyd%ydepth           = 0.0
      hyd%flowelemcontourx = 0.0
      hyd%flowelemcontoury = 0.0
      hyd%netcellnod       = 0
      do i_domain = 1, n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         numcontpts = domain_hyd%numcontpts
         nv = domain_hyd%nv
         do iseg = 1, domain_hyd%nosegl
            iseg_glob = domain_hyd%iglobal(iseg)
            if ( iseg_glob .gt. 0 ) then
               hyd%xdepth(1,iseg_glob) = domain_hyd%xdepth(1,iseg)
               hyd%ydepth(1,iseg_glob) = domain_hyd%ydepth(1,iseg)
               hyd%flowelemcontourx(1:numcontpts,iseg_glob) = domain_hyd%flowelemcontourx(1:numcontpts,iseg)
               hyd%flowelemcontoury(1:numcontpts,iseg_glob) = domain_hyd%flowelemcontoury(1:numcontpts,iseg)
               do inv=1,nv
                  if ( domain_hyd%netcellnod(inv,iseg) .gt. 0) then
                     hyd%netcellnod(inv,iseg_glob) = domain_hyd%netcellnod(inv,iseg) + nodeoffset
                  else
                     hyd%netcellnod(inv,iseg_glob) = 0
                  endif
               enddo
            endif
         enddo
         nodeoffset = nodeoffset + domain_hyd%numk
      enddo

      ! coordinates exchanges

      allocate(hyd%xu(noq1))
      allocate(hyd%yu(noq1))
      hyd%xu = 0.0
      hyd%yu = 0.0
      do i_domain = 1, n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do iq = 1, domain_hyd%lnx
            iq_global = domain_hyd%iglobal_link(iq)
            if ( iq_global .gt. 0 ) then
               hyd%xu(iq_global) = domain_hyd%xu(iq)
               hyd%yu(iq_global) = domain_hyd%yu(iq)
            endif
         enddo
      enddo

      ! layering

      allocate(hyd%hyd_layers(hyd%kmax))
      allocate(hyd%waq_layers(hyd%nolay))
      hyd%hyd_layers = domain_hyd_coll%hyd_pnts(1)%hyd_layers
      hyd%waq_layers = domain_hyd_coll%hyd_pnts(1)%waq_layers


      ! boundaries, add the active sections and boundaries to the collections

      do i_domain = 1, n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         no_sect = domain_hyd%openbndsect_coll%cursize
         do i_sect = 1 , no_sect
            openbndsect => domain_hyd%openbndsect_coll%openbndsect_pnts(i_sect)
            no_bnd = openbndsect%openbndlin_coll%cursize
            bnd_active = .false.
            do i_bnd = 1 , no_bnd
               if ( openbndsect%openbndlin_coll%openbndlin_pnts(i_bnd)%ibnd_new .ne. 0 ) then
                  bnd_active = .true.
               endif
            enddo
            if ( bnd_active ) then
               isect = openbndsect_coll_find( hyd%openbndsect_coll, openbndsect%name )
               if ( isect .le. 0 ) then
                  new_sect%name = openbndsect%name
                  new_sect%openbndlin_coll%cursize = 0
                  new_sect%openbndlin_coll%maxsize = 0
                  new_sect%openbndlin_coll%openbndlin_pnts => null()
                  isect = coll_add(hyd%openbndsect_coll, new_sect)
               endif
               do i_bnd = 1 , no_bnd
                  if ( openbndsect%openbndlin_coll%openbndlin_pnts(i_bnd)%ibnd_new .ne. 0 ) then
                     iret = coll_add(hyd%openbndsect_coll%openbndsect_pnts(isect)%openbndlin_coll,openbndsect%openbndlin_coll%openbndlin_pnts(i_bnd))
                     hyd%openbndsect_coll%openbndsect_pnts(isect)%openbndlin_coll%openbndlin_pnts(iret)%ibnd = openbndsect%openbndlin_coll%openbndlin_pnts(i_bnd)%ibnd_new
                  endif
               enddo
            endif
         enddo
      enddo

      ! allocate rest of the arrays

      allocate(hyd%volume(hyd%noseg))
      allocate(hyd%area(hyd%noq))
      allocate(hyd%flow(hyd%noq))
      allocate(hyd%displen(2,hyd%noq))
      allocate(hyd%surf(hyd%noseg))
      allocate(hyd%depth(hyd%noseg))
      allocate(hyd%attributes(hyd%noseg))
      if (hyd%sal_present) allocate(hyd%sal(hyd%noseg))
      if (hyd%tem_present) allocate(hyd%tem(hyd%noseg))
      if (hyd%tau_present) allocate(hyd%tau(hyd%noseg))
      if (hyd%vdf_present) allocate(hyd%vdf(hyd%noseg))

      ! time independent items

      hyd%atr_type = ATR_FM
      hyd%no_atr = 2
      nolay     = hyd%nolay
      hyd%displen = 0.0

      do i_domain = 1 , n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do isegl = 1 , domain_hyd%nosegl
            iseg_glob = domain_hyd%iglobal(isegl)
            if ( iseg_glob .gt. 0 ) then
               do ilay = 1,nolay
                  hyd%surf(iseg_glob + (ilay - 1) * hyd%nosegl) = domain_hyd%surf(isegl + (ilay - 1) * domain_hyd%nosegl)
                  hyd%attributes(iseg_glob + (ilay - 1) * hyd%nosegl) = domain_hyd%attributes(isegl + (ilay - 1) * domain_hyd%nosegl)
               enddo
               hyd%depth(iseg_glob) = domain_hyd%depth(isegl)
            endif
         enddo
         do iq = 1, domain_hyd%noq1
            iq_global = domain_hyd%iglobal_link(iq)
            if ( iq_global .gt. 0 ) then
               hyd%displen(1,iq_global) = domain_hyd%displen(1,iq)
               hyd%displen(2,iq_global) = domain_hyd%displen(2,iq)
            endif
         enddo
      enddo


      return
      end
