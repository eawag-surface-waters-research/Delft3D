   ! update the bed levels due to subsidence/uplift
   subroutine apply_subsupl()
      use m_flowtimes, only: dts, dt_user
      use m_subsidence, only: sdu_blp, subsupl, subsupl_tp
      use m_flowparameters, only: ibedlevtyp
      use m_flowgeom, only: lnx, ndx, bl, blu
      use network_data, only: numk, zk

      implicit none

      integer           :: k !< face/cell or node index
      integer           :: L !< link/edge index


      ! copy bed level at cell centres to detect bed level changes later when updating water levels
      do k = 1, ndx
         sdu_blp(k) = bl(k)
      enddo

      ! subsidence rate is interpolated in space and time in setexternalforcings() at tim=tstart+n*dt_user
      ! Where the subs/uplift is applied depends on ibedlevtyp
      ! According to setblfromextfile:
      !
      ! ibedlevtyp determines from which source data location the bed levels are used to derive bobs and bl.
      ! These types need to be mapped to one of three possible primitive locations (center/edge/corner).
      !select case (ibedlevtyp)
      !case (1)       ! position = waterlevelpoint, cell centre
      !   iprimpos = 2 ; mx = max(numk, ndx)
      !case (2)       ! position = velocitypoint, cellfacemid
      !   iprimpos = 1 ; mx = max(numk, lnx)
      !case (3,4,5,6) ! position = netnode, cell corner
      !   iprimpos = 3 ; mx = numk
      !end select
      select case (ibedlevtyp)
         case (1)
            do k = 1,ndx
               bl(k) = bl(k) + (subsupl(k)-subsupl_tp(k))/dt_user*dts
            enddo
         case (2)
            do L = 1,lnx
               blu(L) = blu(L) + (subsupl(L)-subsupl_tp(L))/dt_user*dts
            enddo
         case (3,4,5,6)
            do k = 1,numk
               zk(k) = zk(k) + (subsupl(k)-subsupl_tp(k))/dt_user*dts
            enddo
      end select
   end subroutine apply_subsupl
