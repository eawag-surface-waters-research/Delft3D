      SUBROUTINE REAARC(MINP,japrompt)
      USE M_ARCINFO
      use m_polygon
      use m_missing
      use m_alloc

      implicit none

      integer :: ierr
      integer :: minp

      integer, intent(in) :: japrompt  !< prompt for step size (1) or not (0)

      integer :: istep, jstep, MCfile, NCfile
      integer :: istart, iend, jstart, jend  !< block to be read in file-index numbering

      double precision :: distep, djstep, dsqrtnumcur

      integer, parameter :: MAXSAMSIZE = 1000000

      CALL READARCINFOHEADER(MINP,MCa,NCa,X0,Y0,DXa,DYa,RMIS)

      IF ( ALLOCATED(D) ) THEN
           DEALLOCATE(D)
      ENDIF

      if ( japrompt == 0 ) then

         ALLOCATE ( D(MCa,NCa),STAT=IERR)
         CALL AERR('D(MCa,NCa)',IERR,MCa*NCa)

         CALL READARCINFOBLOCK (MINP,D,MCa,NCa,RMIS)

      else
         istep = 1
         jstep = 1
         ierr  = 1

         MCfile = MCa
         NCfile = NCa

!         do while ( ierr.ne.0 )


         if ( NPL.le.0 ) then
            istart = 1
            iend   = MCa
            jstart = 1
            jend   = NCa
         else  ! use selecting polygon for dimensions of block to be read
            istart = max(1+int( (minval(xpl(1:NPL), xpl(1:NPL).ne.DMISS)-X0)/DXa ), 1)
            iend   = min(1+int( (maxval(xpl(1:NPL), xpl(1:NPL).ne.DMISS)-X0)/DXa ), MCa)

            jstart = max(1+int( (minval(ypl(1:NPL), ypl(1:NPL).ne.DMISS)-Y0)/DYa ), 1)
            jend   = min(1+int( (maxval(ypl(1:NPL), ypl(1:NPL).ne.DMISS)-Y0)/DYa ), NCa)
         end if

         if ( japrompt.eq.1 ) then
!           automatic istep, jstep
            dsqrtnumcur = sqrt(dble(iend-istart+1))*sqrt(dble(jend-jstart+1))
            distep = dsqrtnumcur/sqrt(dble(MAXSAMSIZE))
            distep = dble(int(distep+0.5d0))
            djstep = distep

            if ( distep.gt.1d0 ) then  ! only if necessary
               call getreal("istep = ", distep)
               call getreal("jstep = ", djstep)
            end if

            istep = max(int(distep),1)
            jstep = max(int(djstep),1)
         end if

            MCa = (iend-istart+1)/istep
            NCa = (jend-jstart+1)/jstep

            ALLOCATE ( D(MCa,NCa),STAT=IERR)
!            CALL AERR('D(MCa,NCa)',IERR,MCa*NCa)

   !        check for allocation error
            if ( IERR.ne.0 ) then
               call qnerror('Sample file too large: increase istep and/or jstep', ' ', ' ')
               MCA = 0
               NCA = 0
   !           we cannot deallocate erroneously allocated arrays directly and need to reallocate it correctly first
               allocate(D(1,1))
               deallocate(D)
               goto 1234
            end if
!         end do   ! do while ( ierr.ne.0 )

         call ReadLargeArcInfoBlock(MINP, MCfile, NCfile, istart, iend, jstart, jend, MCa, NCa, RMIS, istep, jstep, D)

!        modife arcinfo module data
!         X0 = X0 + dble(istep-1)*0.5d0*DXa
!         Y0 = Y0 + dble(jstep-1)*0.5d0*DYa
         X0 = X0 + (istart-1)*Dxa + dble(istep-1)*0.5d0*DXa
         Y0 = Y0 + (jstart-1)*Dya + dble(jstep-1)*0.5d0*DYa
         DXa = dble(istep)*DXa
         DYa = dble(jstep)*DYa

      end if   ! if ( LdirectReadBlock )

!     error handling
 1234 continue

      RETURN
      END
