      subroutine agr_hyd_step(input_hyd, ipnt, ipnt_q, ipnt_vdf, ipnt_tau, output_hyd)

      ! function : aggregeate one hydrodynamic step

      ! (c) DELFT HYDRAULICS

      ! global declarations

      use hydmod
      implicit none

      ! declaration of the arguments

      type(t_hyd)         :: input_hyd     ! the input hydrodynamics
      integer             :: ipnt(*)       ! aggregation pointer segments
      integer             :: ipnt_q(*)     ! aggregation pointer exchanges
      integer             :: ipnt_vdf(*)   ! aggregation pointer vertical diffusion
      integer             :: ipnt_tau(*)   ! aggregation pointer tau
      type(t_hyd)         :: output_hyd    ! the output hydrodynamics

      ! local declarations

      real, allocatable   :: rwork(:)      ! general work array aggregation
      real, allocatable   :: vdfwork(:)    ! work array vertical diffusion, minimum over the aggregated layers
      integer             :: noseg2        ! length of vdfwork
      integer             :: ilay          ! loop counter layers
      integer             :: iseg          ! segment index
      integer             :: iq            ! exchange index
      integer             :: i1, i2, i3, i4! indexes in array to point to a certain layer
      integer             :: ierr_alloc    !
      real, parameter     :: rmiss = -999.

      ! in future from a module:

      integer, parameter  :: IAGTYP_ACCUM = 1 ! aggregation using accumulation
      integer, parameter  :: IAGTYP_AVG   = 2 ! aggregation using averaging
      integer, parameter  :: IAGTYP_WAVG  = 3 ! aggregation using averaging with a weight variable
      integer, parameter  :: IAGTYP_MIN   = 4 ! aggregation using minimum value

      ! some init

      allocate(rwork(output_hyd%noseg),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error allocating memory' ; call srstop(1) ; endif

      ! volumes

      call dhaggr( input_hyd%noseg , output_hyd%noseg ,
     +             1               , 1                ,
     +             1               , 1                ,
     +             1               , 0                ,
     +             0               , 1                ,
     +             ipnt            , IAGTYP_ACCUM     ,
     +             input_hyd%volume, rwork            ,
     +             rwork           , output_hyd%volume)

      ! volumes always positive

      do iseg = 1 , output_hyd%noseg
         output_hyd%volume(iseg) = max(0.0,output_hyd%volume(iseg))
      enddo

      ! areas

      call dhaggr( input_hyd%noq   , output_hyd%noq   ,
     +             1               , 1                ,
     +             1               , 1                ,
     +             1               , 0                ,
     +             0               , 1                ,
     +             ipnt_q          , IAGTYP_ACCUM     ,
     +             input_hyd%area  , rwork            ,
     +             rwork           , output_hyd%area  )

      ! areas always positive

      do iq = 1 , output_hyd%noq
         output_hyd%area(iq) = max(0.0,output_hyd%area(iq))
      enddo

      ! flows

      call dhaggr( input_hyd%noq   , output_hyd%noq   ,
     +             1               , 1                ,
     +             1               , 1                ,
     +             1               , 0                ,
     +             0               , 1                ,
     +             ipnt_q          , IAGTYP_ACCUM     ,
     +             input_hyd%flow  , rwork            ,
     +             rwork           , output_hyd%flow  )

      ! salinity, averaged with volume

      if ( input_hyd%sal_present ) then
         call dhaggr( input_hyd%noseg  , output_hyd%noseg  ,
     +                1                , 1                 ,
     +                1                , 1                 ,
     +                1                , 1                 ,
     +                1                , 1                 ,
     +                ipnt             , IAGTYP_WAVG       ,
     +                input_hyd%sal    , input_hyd%volume  ,
     +                rwork            , output_hyd%sal    )
      endif

      ! temperature, averaged with volume

      if ( input_hyd%tem_present ) then
         call dhaggr( input_hyd%noseg  , output_hyd%noseg  ,
     +                1                , 1                 ,
     +                1                , 1                 ,
     +                1                , 1                 ,
     +                1                , 1                 ,
     +                ipnt             , IAGTYP_WAVG       ,
     +                input_hyd%tem    , input_hyd%volume  ,
     +                rwork            , output_hyd%tem    )
      endif

      ! tau, only do last layer, other layers 0.0

!     if ( input_hyd%tau_present ) then
!        output_hyd%tau = 0.0
!        i1 = (input_hyd%nolay-1)*input_hyd%nosegl + 1
!        i2 = (output_hyd%nolay-1)*output_hyd%nosegl + 1
!        call dhaggr( input_hyd%nosegl , output_hyd%nosegl ,
!    +                1                , 1                 ,
!    +                1                , 1                 ,
!    +                1                , 1                 ,
!    +                1                , 1                 ,
!    +                ipnt             , IAGTYP_WAVG       ,
!    +                input_hyd%tau(i1), input_hyd%surf(i1),
!    +                rwork            , output_hyd%tau(i2))
!     endif
      if ( input_hyd%tau_present ) then
         output_hyd%tau = 0.0
         call dhaggr( input_hyd%noseg  , output_hyd%noseg  ,
     +                1                , 1                 ,
     +                1                , 1                 ,
     +                1                , 1                 ,
     +                1                , 1                 ,
     +                ipnt_tau         , IAGTYP_WAVG       ,
     +                input_hyd%tau    , input_hyd%surf    ,
     +                rwork            , output_hyd%tau    )
      endif

      ! vdf

      if ( input_hyd%vdf_present ) then

         if ( input_hyd%nolay .ne. output_hyd%nolay ) then

            ! take minimum of the surrounding segments (actauly exchanges) over the layers only

            noseg2 = input_hyd%nosegl*output_hyd%nolay
            allocate(vdfwork(noseg2),stat=ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error allocating memory' ; call srstop(1) ; endif
            do iseg = 1 , noseg2
               vdfwork(iseg) = rmiss
            enddo
            call dhaggr( input_hyd%noseg , noseg2           ,
     +                   1               , 1                ,
     +                   1               , 1                ,
     +                   1               , 1                ,
     +                   1               , 1                ,
     +                   ipnt_vdf        , IAGTYP_MIN       ,
     +                   input_hyd%vdf   , input_hyd%volume ,
     +                   rwork           , vdfwork          )

            ! then average over the horizonatal aggregation, last layer is not used

            do ilay = 1 , output_hyd%nolay - 1
               i1 = (ilay-1)*input_hyd%nosegl + 1
               i4 = ilay*input_hyd%nosegl
               i2 = (ilay-1)*output_hyd%nosegl + 1
               i3 = ilay*output_hyd%nosegl
               call dhaggr( input_hyd%nosegl, output_hyd%nosegl ,
     +                      1               , 1                 ,
     +                      1               , 1                 ,
     +                      1               , 1                 ,
     +                      1               , 1                 ,
     +                      ipnt            , IAGTYP_WAVG       ,
     +                      vdfwork(i1)     , input_hyd%surf    ,
     +                      rwork           , output_hyd%vdf(i2:i3))
            enddo

            ! last layer set to zero

            i2 = (output_hyd%nolay-1)*output_hyd%nosegl + 1
            output_hyd%vdf(i2:output_hyd%noseg) = 0.0

            deallocate(vdfwork,stat=ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error deallocating memory' ; call srstop(1) ; endif

         else

            output_hyd%vdf = 0.0
            call dhaggr( input_hyd%noseg , output_hyd%noseg  ,
     +                   1               , 1                 ,
     +                   1               , 1                 ,
     +                   1               , 1                 ,
     +                   1               , 1                 ,
     +                   ipnt            , IAGTYP_WAVG       ,
     +                   input_hyd%vdf   , input_hyd%surf    ,
     +                   rwork           , output_hyd%vdf    )

         endif

      endif

      deallocate(rwork,stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error deallocating memory' ; call srstop(1) ; endif
      return
      end
