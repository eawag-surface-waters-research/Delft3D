!> update cellmask from samples
!> a cell is dry when it is:
!>   1) inside ANY "1"-polygon (drypnts), OR
!>   2) outside ALL "-1"-polygons (enclosures)
subroutine pol_to_cellmask()
   use network_data
   use m_polygon
   use m_missing, only: dmiss, JINS
   use geometry_module, only: dbpinpol_optinside_perpol

   implicit none

   integer                                     :: i, ic, in, k, KMOD
   integer                                     :: num

   if ( allocated(cellmask) ) deallocate(cellmask)
   allocate(cellmask(nump1d2d))
   cellmask = 0

   if ( NPL.eq.0 ) return  ! no polygon

   CALL READYY('Applying polygon cellmask',0d0)
   KMOD = MAX(1,NUMP/100)

   !  generate cell mask
   in = -1
   do k = 1,nump
!     check if cell is in any "zpl>0" polygons
      call dbpinpol_optinside_perpol(xzw(k), yzw(k), 0, 1, in, num, dmiss, JINS, NPL, xpl, ypl, zpl)

      if ( in.eq.0 ) then
!        check if cell is outside all "zpl<0" polygons (enclosure)
         call dbpinpol_optinside_perpol(xzw(k), yzw(k), 0, -1, in, num, dmiss, JINS, NPL, xpl, ypl, zpl)   ! in=0: outside all "-1"-pol
         if ( num.gt.0) in = 1-in   ! only if at least one "-1"-type polygon was encountered
      end if

      if ( in.gt.0 ) then
!        mask cell
         cellmask(k) = 1
      end if
      IF (MOD(k,KMOD) .EQ. 0) THEN
         CALL READYY(' ',MIN( 1d0,dble(k)/nump ) )
      ENDIF
   end do
   CALL READYY(' ',-1d0)

   return
end subroutine pol_to_cellmask
