      SUBROUTINE REAdarcinfo(Marc, ja)
      USE M_SFERIC
      use m_netw
      USE m_grid
      USE M_ARCINFO
      USE M_MISSING
      implicit none

      INTEGER            :: Marc, JA, in
      CHARACTER NAME2*76, TEX*3, REC*132, REC1*132


      logical jawel

      integer :: i, j, k, k0, l0
      integer :: jadoorladen, merr
      double precision XX(8), YY(8), ZZ(8)
      double precision :: af

      JSFERIC  = 0
      JSFERTEK = 0

      MERR  = 0
      MC    = 0

      CALL READYY('Reading arcinfo',0d0)

      CALL READYY('Reading arcinfo',1d0)


      call reaarc (marc,1)
      CALL DOCLOSE(marc)

      mc = mca ; nc = nca

      CALL INCREASEGRID(MC,NC)

      XC = DMISS
      YC = DMISS

      do i = 1,mc
         do j = 1,nc
            if (d(I,J) .ne. dmiss) then
               xc(i,j) =  x0 + dxa*(i-1)
               yc(i,j) =  y0 + dxa*(j-1)
               zc(i,j) =  d(i,j)
            endif
         enddo
      enddo

      if (allocated(d) ) deallocate(d)

!     disable grid outside selecting polygon
!      if ( NPL.gt.0 ) then
!         in = -1
!         do j=1,nc
!            do i=1,mc
!               call dbpinpol(xc(i,j),yc(i,j),in)
!               if ( in.ne.1 ) then
!                  xc(i,j) = DMISS
!                  yc(i,j) = DMISS
!               end if
!            end do
!         end do
!      end if

!      call gridtonet()
!
!      if (allocated(xc) ) then
!         deallocate(xc,yc,zc) ; mc = 0
!      endif


      CALL READYY(' ',-1d0)

      JA = 1

      END SUBROUTINE REAdarcinfo
