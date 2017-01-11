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

      subroutine read_hyd_init(hyd)

      ! function : read the time independent data from a hydrodynamics

      ! global declarations

      use hydmod
      implicit none

      ! declaration of the arguments

      type(t_hyd)         :: hyd     ! description of the hydrodynamics

      ! local declarations

      integer             :: i          ! loop counter
      integer             :: j          ! loop counter
      integer             :: iseg       ! loop counter
      integer             :: isegl      ! loop counter
      integer             :: ilay       ! loop counter
      integer             :: ierr       ! error indicator
      integer             :: ierr_alloc ! allocation error indicator
      integer             :: itime      ! time indicator
      integer             :: lunrep     ! unit number report file

      ! some init

      call getmlu(lunrep)

      ! allocate and read or define grid table

      allocate(hyd%lgrid(hyd%nmax,hyd%mmax),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) goto 980
      if(hyd%geometry .eq. HYD_GEOM_CURVI) then
         call read_lga(hyd%file_lga, hyd%mmax, hyd%nmax, hyd%nolay, hyd%nosegl, &
                       hyd%noq1    , hyd%noq2, hyd%noq3, hyd%lgrid)
      else
         do i = 1,hyd%nosegl
            hyd%lgrid(1,i) = i
         enddo
      endif        

      hyd%noseg = hyd%nosegl*hyd%nolay
      hyd%noq   = hyd%noq1 + hyd%noq2 + hyd%noq3

      if(hyd%geometry .eq. HYD_GEOM_CURVI) then
         ! allocate and read cco file
   
         allocate(hyd%xdepth(hyd%nmax,hyd%mmax),stat=ierr_alloc)
         if ( ierr_alloc .ne. 0 ) goto 980
         allocate(hyd%ydepth(hyd%nmax,hyd%mmax),stat=ierr_alloc)
         if ( ierr_alloc .ne. 0 ) goto 980
         call read_cco(hyd%file_cco, hyd%mmax, hyd%nmax, hyd%xdepth, hyd%ydepth)
      elseif (hyd%geometry .eq. HYD_GEOM_UNSTRUC) then
         ! allocate and read waqgeom file!?
      endif

      ! allocate and read pointer table

      allocate(hyd%ipoint(4,hyd%noq),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) goto 990
      call read_poi(hyd%file_poi, hyd%noq, hyd%noq1, hyd%noq2, hyd%noq3, &
                    hyd%ipoint  )
      hyd%nobnd  = -minval(hyd%ipoint)
      hyd%nobndl = hyd%nobnd/hyd%nolay

      allocate(hyd%surf(hyd%noseg),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) goto 970
      if(hyd%geometry .eq. HYD_GEOM_CURVI) then
         call read_srf(hyd%file_srf, hyd%mmax, hyd%nmax, hyd%nosegl, hyd%surf )
         do iseg = 1 , hyd%nosegl
            do ilay = 2 , hyd%nolay
               isegl = (ilay-1)*hyd%nosegl + iseg
               hyd%surf(isegl) = hyd%surf(iseg)
            enddo
         enddo
      else
         call read_hsrf(hyd%file_hsrf, hyd%noseg, hyd%surf )
      endif

      allocate(hyd%depth(hyd%noseg),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) goto 970
      hyd%depth = 0.0
      if ( hyd%file_dps%name .ne. ' ' ) then
         call read_srf(hyd%file_dps, hyd%mmax, hyd%nmax, hyd%nosegl, hyd%depth )
      endif

      ! allocate arrays time dependent arrays

      allocate(hyd%volume(hyd%noseg),stat=ierr_alloc) ; if ( ierr_alloc .ne. 0 ) goto 970
      allocate(hyd%area(hyd%noq),stat=ierr_alloc) ; if ( ierr_alloc .ne. 0 ) goto 990
      allocate(hyd%flow(hyd%noq),stat=ierr_alloc) ; if ( ierr_alloc .ne. 0 ) goto 990
      allocate(hyd%displen(2,hyd%noq),stat=ierr_alloc) ; if ( ierr_alloc .ne. 0 ) goto 990
      allocate(hyd%sal(hyd%noseg),stat=ierr_alloc) ; if ( ierr_alloc .ne. 0 ) goto 970
      allocate(hyd%tem(hyd%noseg),stat=ierr_alloc) ; if ( ierr_alloc .ne. 0 ) goto 970
      allocate(hyd%tau(hyd%noseg),stat=ierr_alloc) ; if ( ierr_alloc .ne. 0 ) goto 970
      allocate(hyd%vdf(hyd%noseg),stat=ierr_alloc) ; if ( ierr_alloc .ne. 0 ) goto 970
      allocate(hyd%attributes(hyd%noseg),stat=ierr_alloc) ; if ( ierr_alloc .ne. 0 ) goto 970
!     allocate(hyd%wasteflow(hyd%wasteload_coll%actual_size))

      ! read dispersion length, assume time independent

      call dlwqfile_open(hyd%file_len)
      read(hyd%file_len%unit_nr,iostat=ierr) itime,((hyd%displen(i,j),i=1,2),j=1,hyd%noq)
      if ( ierr .ne. 0 ) then
         write(*,*) 'ERROR: reading dispersion length file'
         write(lunrep,*) 'ERROR: reading dispersion length file'
         call srstop(1)
      endif

      ! read attributes

      if ( hyd%file_atr%name .ne. ' ' ) then
         call read_atr(hyd%file_atr, hyd%atr_type, hyd%no_atr, hyd%noseg, hyd%attributes)
      else
         hyd%atr_type = ATR_UNKNOWN
      endif

      return
  970 write(lunrep,*) 'error allocating memory:',ierr_alloc
      write(lunrep,*) 'hyd%noseg:',hyd%noseg
      call srstop(1)
  980 write(lunrep,*) 'error allocating memory:',ierr_alloc
      write(lunrep,*) 'hyd%nmax:',hyd%nmax
      write(lunrep,*) 'hyd%mmax:',hyd%mmax
      call srstop(1)
  990 write(lunrep,*) 'error allocating memory:',ierr_alloc
      write(lunrep,*) 'hyd%noq:',hyd%noq
      call srstop(1)
      end
