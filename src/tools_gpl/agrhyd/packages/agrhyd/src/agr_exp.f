      subroutine agr_exp(input_hyd, output_hyd, ipnt   )

      use hydmod
      implicit none

      type(t_hyd)          :: input_hyd                           ! description of the input hydrodynamics
      type(t_hyd)          :: output_hyd                          ! description of the output hydrodynamics
      integer              :: ipnt(input_hyd%nmax,input_hyd%mmax) ! aggregation pointer

      ! local declarations

      integer              :: mmax          ! mmax
      integer              :: nmax          ! nmax
      integer              :: m             ! m index
      integer              :: n             ! n index
      integer              :: iseg          ! segment index
      integer              :: iseg_new      ! segment index in the new grid
      integer              :: lunrep        ! unit number report file

      call getmlu(lunrep)

      mmax = input_hyd%mmax
      nmax = input_hyd%nmax

      do m = 1 , mmax
         do n = 1 , nmax
            iseg     = input_hyd%lgrid(n,m)
            if ( iseg .gt. 0 ) then
               iseg_new  = (m-1)*nmax + n
               ipnt(n,m) = iseg_new
            else
               ipnt(n,m) = iseg
            endif
         enddo
      enddo

      return
      end
