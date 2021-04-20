subroutine update_pumps_with_levels()

   use m_flowgeom
   use m_flow
   use m_missing
   use m_structures
   use unstruc_channel_flow
   use m_pump
   use m_partitioninfo

   integer :: ierr, n, istru

   !Pump with levels, SOBEK style, outside OpenMP region
   ! TODO: merge water level calculations with dambreak
   if (nPumpsWithLevels > 0) then

      ! Initialize
      pumpAveraging        = 0.0d0
      waterLevelsPumpLeft  = 0.0d0
      waterLevelsPumpRight = 0.0d0

      ! Compute sumQuantitiesByWeight and sumWeights for the suction side
      !LC: TODO, do the average only over open links
      ierr = getAverageQuantityFromLinks(L1pumpsg, L2pumpsg, wu, kpump(3,:), s1, kpump(1,:), pumpAveraging, 0)
      if (ierr.ne.0) success=.false.

      do n = 1, npumpsg
         if (pumpAveraging(2,n)>0.0d0) then
            waterLevelsPumpLeft(n)  = pumpAveraging(1,n)/pumpAveraging(2,n)
         endif
      enddo

      ! Compute sumQuantitiesByWeight and sumWeights for the delivery side
      ierr = getAverageQuantityFromLinks(L1pumpsg, L2pumpsg, wu, kpump(3,:), s1, kpump(2,:), pumpAveraging, 0)
      if (ierr.ne.0) success=.false.

      do n = 1, npumpsg
         if (pumpAveraging(2,n)>0.0d0) then
            waterLevelsPumpRight(n)  = pumpAveraging(1,n)/pumpAveraging(2,n)
         endif
      enddo

      !TODO remove this code:
      !Compute pump discharges
      !do n = 1, npumpsg
      !   ! Retrive a valid index in the network%sts%struct
      !   istru = pumpsWithLevels(n)
      !   ! Do not use PrepareComputePump to compute the legacy pumps discharges
      !   if (istru.eq.-1) cycle
      !   if (associated(network%sts%struct(istru)%pump)) then
      !      !call PrepareComputePump(network%sts%struct(istru)%pump, waterLevelsPumpLeft(n), waterLevelsPumpRight(n))
      !      !qpump(n) = network%sts%struct(istru)%pump%discharge
      !   endif
      !enddo
   end if

end subroutine update_pumps_with_levels
