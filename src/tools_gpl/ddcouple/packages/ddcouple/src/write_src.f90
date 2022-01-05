      subroutine write_src(hyd)

      ! function : write a src file

      ! (c) Deltares

      ! global declarations

      use hydmod                   ! module contains everything for the hydrodynamics
      implicit none

      ! declaration of the arguments

      type(t_hyd)                            :: hyd                   ! description of the hydrodynamics

      ! local declarations

      integer                                :: lunrep                ! unit number report file
      integer                                :: lunsrc                ! unit number sources file
      integer                                :: nowast                ! number of wasteloads
      integer                                :: noflow                ! number of flows
      integer                                :: nolay                 ! number of layers
      integer                                :: nobrk                 ! number of breakpoints
      real, allocatable                      :: waq_layers_frac(:)    ! frcations of the water column
      integer                                :: i                     ! loop counter
      integer                                :: ilay                  ! waq layer index
      integer                                :: k                     ! hyd layer index
      integer                                :: nok                   ! number of hyd layer per waq layer
      integer                                :: koff                  ! hyd layer offset
      real                                   :: flow                  ! flow for one wasteload
      integer                                :: iwaste                ! wasteload index
      integer                                :: ibrk                  ! breakpoint index

      call getmlu(lunrep)

      nowast = hyd%wasteload_coll%cursize
      nolay  = hyd%nolay
      nobrk  = hyd%wasteload_data%no_brk

      if ( nowast .ne. hyd%wasteload_data%no_loc ) then
         write(lunrep,*) 'error, number of wasteloads in hyd file does not equal the data files'
         write(lunrep,*) 'number from hyd file: ',nowast
         write(lunrep,*) 'number from data    :',hyd%wasteload_data%no_loc
         call srstop(1)
      endif

      if ( nolay .gt. 1 ) then
         allocate(waq_layers_frac(nolay))
         waq_layers_frac = 0.0
         koff = 0
         do ilay = 1 , nolay
            nok = nint(hyd%waq_layers(ilay))
            do k = 1 , nok
               waq_layers_frac(ilay) = waq_layers_frac(ilay) + hyd%hyd_layers(koff+k)
            enddo
            koff = koff + nok
         enddo
         noflow = 0
         do i = 1, nowast
            if ( hyd%wasteload_coll%wasteload_pnts(i)%k .eq. 0 ) then
               noflow = noflow + nolay
            else
               noflow = noflow + 1
            endif
         enddo
      else
         noflow = nowast
         allocate(waq_layers_frac(1))
         waq_layers_frac = 1.0
      endif

      call dlwqfile_open(hyd%file_src)
      lunsrc = hyd%file_src%unit_nr

      if ( hyd%time_in_seconds ) then
          write(lunsrc,*) '  SECONDS   ; time given in seconds'
      endif
      write(lunsrc,*) '    3    ; time dependent sources'
      write(lunsrc,*) '    1    ; block function'
      write(lunsrc,*) noflow,' ; no. of sources in this block'
      write(lunsrc,'(8(i6,1x))') (i,i=1,noflow)
      write(lunsrc,*) nobrk,' ; number of breakpoints'
      write(lunsrc,*) '1.0 1.0  ; scale factors'

      ! loop over the breakpoints

      do ibrk = 1 , nobrk

         write(lunsrc,*) hyd%wasteload_data%times(ibrk),' ; breakpoint time'

         ! loop over the wasteloads write flow and dummy concentration

         do ilay = 1 , nolay
            do iwaste = 1 , nowast
               if ( ilay .eq. 1 .or. hyd%wasteload_coll%wasteload_pnts(iwaste)%k .eq. 0 ) then
                  if ( hyd%wasteload_coll%wasteload_pnts(iwaste)%k .eq. 0 ) then
                     flow = hyd%wasteload_data%values(1,iwaste,ibrk)*waq_layers_frac(ilay)
                  else
                     flow = hyd%wasteload_data%values(1,iwaste,ibrk)
                  endif
                  write(lunsrc,'(e14.6,a,i6)') flow,' 1.0 ; source:',iwaste
               endif
            enddo
         enddo

      enddo
      deallocate(waq_layers_frac)

      close(hyd%file_src%unit_nr)
      hyd%file_src%status = FILE_STAT_UNOPENED

      return
      end
