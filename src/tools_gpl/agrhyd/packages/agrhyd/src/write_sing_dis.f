      subroutine write_sing_dis(hyd, file_names)

      ! function : write discharge names singapore models

      ! (c) DELFT HYDRAULICS

      ! global declarations

      use hydmod                   ! module contains everything for the hydrodynamics
      implicit none

      ! declaration of the arguments

      type(t_hyd)                            :: hyd                   ! description of the hydrodynamics
      character(len=*)                       :: file_names            ! file with new discharge names and type

      ! local declarations

      integer                                :: lun_names             ! unit number
      integer                                :: nowast                ! number of wasteloads
      integer                                :: nowast_tot            ! number of wasteloads total in waq
      integer                                :: nolay                 ! number of layers
      integer                                :: nolay_waste           ! number of layers for specific load
      integer                                :: iwaste_lay            ! follow number load
      integer                                :: ilay                  ! waq layer index
      integer                                :: iwaste                ! wasteload index
      integer                                :: m                     ! m index
      integer                                :: n                     ! n index
      integer                                :: k                     ! k index
      integer                                :: iseg                  ! segment number wasteload
      character(len=3)                       :: c3                    ! prefix
      character(len=20)                      :: org_name              ! original discharge name which equals type
      character(len=20)                      :: c20_name              ! final name WAQ-GUI
      character(len=100)                     :: c100_name             ! temporary name
      character(len=1), parameter            :: quote = ''''
      character(len=1), parameter            :: space = ' '

      ! some init

      nowast = hyd%wasteload_coll%cursize
      if ( nowast .le. 0 ) return
      nolay  = hyd%nolay

      ! count total number of waq loads

      nowast_tot = 0
      do iwaste = 1 , nowast
         if ( hyd%wasteload_coll%wasteload_pnts(iwaste)%k .eq. 0 ) then
            nolay_waste = nolay
         else
            nolay_waste = 1
         endif
         do ilay = 1 , nolay_waste
            nowast_tot = nowast_tot + 1
         enddo
      enddo

      ! open file with names and types

      call dhnlun(761,lun_names)
      open(lun_names,file=file_names)

      write(lun_names,'(i12,a)') nowast_tot,' ; number of wastelaods'

      ! loop over the wasteloads

      iwaste_lay = 0
      do iwaste = 1 , nowast

         ! now write the file whith the final WAQ-GUI names adding the org_name as type

         m = hyd%wasteload_coll%wasteload_pnts(iwaste)%m
         n = hyd%wasteload_coll%wasteload_pnts(iwaste)%n
         k = hyd%wasteload_coll%wasteload_pnts(iwaste)%k

         if ( k .eq. 0 ) then
            nolay_waste = nolay
            k = 1
         else
            nolay_waste = 1
         endif
         do ilay = 1 , nolay_waste
            iwaste_lay = iwaste_lay + 1

            ! this will be the name as created by WAQ-GUI

            if ( iwaste_lay .lt. 10 ) then
               write(c100_name,'(i1,3a)') iwaste_lay,' (',trim(hyd%wasteload_coll%wasteload_pnts(iwaste)%name),')'
            elseif ( iwaste_lay .lt. 100 ) then
               write(c100_name,'(i2,3a)') iwaste_lay,' (',trim(hyd%wasteload_coll%wasteload_pnts(iwaste)%name),')'
            elseif ( iwaste_lay .lt. 1000 ) then
               write(c100_name,'(i3,3a)') iwaste_lay,' (',trim(hyd%wasteload_coll%wasteload_pnts(iwaste)%name),')'
            elseif ( iwaste_lay .lt. 10000 ) then
               write(c100_name,'(i4,3a)') iwaste_lay,' (',trim(hyd%wasteload_coll%wasteload_pnts(iwaste)%name),')'
            else
               write(c100_name,'(i5,3a)') iwaste_lay,' (',trim(hyd%wasteload_coll%wasteload_pnts(iwaste)%name),')'
            endif

            c20_name = c100_name

            iseg = hyd%lgrid(n,m) + (k-1)*hyd%nosegl

            write(lun_names,'(i8,1x,11a)') iseg,
     +                               quote,trim(c20_name),quote,space,
     +                               quote,trim(c20_name),quote,space,
     +                               quote,trim(hyd%wasteload_coll%wasteload_pnts(iwaste)%waqtype),quote

            k = k + 1

         enddo
      enddo

      close(lun_names)

      return
      end
