      subroutine sing_dis(hyd)

      ! function : rename discharge names singapore models

      ! (c) DELFT HYDRAULICS

      ! global declarations

      use hydmod                   ! module contains everything for the hydrodynamics
      implicit none

      ! declaration of the arguments

      type(t_hyd)                            :: hyd                   ! description of the hydrodynamics

      ! local declarations

      integer                                :: nowast                ! number of wasteloads
      integer                                :: iwaste                ! wasteload index
      character(len=3)                       :: c3                    ! prefix
      character(len=20)                      :: org_name              ! original discharge name which equals type

      ! some init

      nowast = hyd%wasteload_coll%cursize
      if ( nowast .le. 0 ) return

      ! loop over the wasteloads

      do iwaste = 1 , nowast

         ! add the layer to the name as prefix, this is to make them unique, this will be the name in the hyd file

         write(c3,'(i2.2,a1)') hyd%wasteload_coll%wasteload_pnts(iwaste)%k,'_'
         org_name = hyd%wasteload_coll%wasteload_pnts(iwaste)%name
         hyd%wasteload_coll%wasteload_pnts(iwaste)%name = c3//org_name
         hyd%wasteload_coll%wasteload_pnts(iwaste)%waqtype = org_name

      enddo

      return
      end
