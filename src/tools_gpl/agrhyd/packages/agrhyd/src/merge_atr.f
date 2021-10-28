      subroutine merge_atr( hyd, domain_hyd_coll)

      ! function : merges the attributes

      ! (c) DELFT HYDRAULICS

      ! global declarations

      use hydmod                   ! module contains everything for the hydrodynamics
      implicit none

      ! declaration of the arguments

      type(t_hyd)                            :: hyd                   ! description of the hydrodynamics
      type(t_hyd_coll)                       :: domain_hyd_coll       ! description of the domain hydrodynamics

      ! local declarations

      type(t_hyd), pointer                   :: domain_hyd            ! description of one domain hydrodynamics
      integer                                :: n_domain              ! number of domains
      integer                                :: i_domain              ! domain index
      integer                                :: nolay                 ! number of layers
      integer                                :: ilay                  ! layer index
      integer                                :: iseg                  ! segment index
      integer                                :: isegl                 ! segment index
      integer                                :: iseg_domain           ! segment index
      integer                                :: isoff                 ! segment offset

      ! copy to locals for convenience

      n_domain  = hyd%domain_coll%cursize
      nolay     = hyd%nolay

      isoff  = 0
      do i_domain = 1 , n_domain

         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do ilay = 1 , nolay
            do isegl = 1 , domain_hyd%nosegl
               iseg_domain = (ilay-1)*domain_hyd%nosegl + isegl
               iseg        = (ilay-1)*hyd%nosegl + isegl + isoff
               hyd%attributes(iseg) = domain_hyd%attributes(iseg_domain)
            enddo
         enddo

         isoff  = isoff  + domain_hyd%nosegl

      enddo

      return
      end
