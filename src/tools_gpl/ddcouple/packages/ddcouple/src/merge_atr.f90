      subroutine merge_atr( hyd, domain_hyd_coll,maxbnd,maxseg,n_domain,ipnew)

      ! function : merges the attributes

      ! (c) Deltares

      ! global declarations

      use hydmod                   ! module contains everything for the hydrodynamics
      implicit none

      ! declaration of the arguments

      type(t_hyd)                            :: hyd                   ! description of the hydrodynamics
      type(t_hyd_coll)                       :: domain_hyd_coll       ! description of the domain hydrodynamics
      integer                                :: maxbnd                ! maximum boundary id
      integer                                :: maxseg                ! maximum segment id
      integer                                :: n_domain              ! number of domains
      integer                                :: ipnew(-2*maxbnd:maxseg,n_domain)

      ! local declarations

      type(t_hyd), pointer                   :: domain_hyd            ! description of one domain hydrodynamics
      integer                                :: i_domain              ! domain index
      integer                                :: iseg                  ! segment index
      integer                                :: isegl                 ! segment index
      integer                                :: iseg_domain           ! segment index

      do i_domain = 1 , n_domain

         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do iseg_domain = 1 , domain_hyd%noseg
            iseg = ipnew(iseg_domain,i_domain)
            hyd%attributes(iseg) = domain_hyd%attributes(iseg_domain)
         enddo
      enddo

      return
      end
