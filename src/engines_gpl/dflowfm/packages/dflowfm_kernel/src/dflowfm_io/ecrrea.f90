 !> read from rfg grid file
      SUBROUTINE ECRREA(X,MMAX,NMAX,MC,NC,MRGF,HALF)
      use m_missing
      implicit none
      character dummy*10, REC*132
!     LEES RGF
      integer,          intent(in) :: MMAX, NMAX   !< array sizes
      integer,          intent(in) :: mc, nc       !< grid size
      integer,          intent(in) :: mrgf         !< grid-file unit number
      double precision, intent(in) :: half         !< progress bar length, 0:half, 0.5:full
      double precision             :: X(MMAX,NMAX)
      double precision             :: af
      integer                      :: i,j

      DO J=1,NC
         IF (HALF > -1D0) THEN
            AF = HALF + 0.5d0*dble(J)/dble(NC)
            CALL READYY('Reading Grid File',AF)
         ENDIF
         READ(MRGF,*,err=777,end=999) dummy,dummy, (X(I,J),I=1,MC)
      ENDDO

      RETURN

  777 BACKSPACE (MRGF)
      BACKSPACE (MRGF)
      DO J=1,NC
         IF (HALF > -1D0) THEN
            AF = HALF + 0.5d0*dble(J)/dble(NC)
            CALL READYY('Reading Grid File',AF)
         ENDIF
         READ(MRGF,'(10X5F12.0)',err=888,END=999) (X(I,J),I=1,MC)
      ENDDO


      ! where (x == 0d0) x = dxymis

      RETURN

  888 BACKSPACE (MRGF)
      READ(MRGF,'(A)') REC
      CALL QNREADERROR('Reading Grid Coordinates but Getting',REC,MRGF)
      RETURN

  999 BACKSPACE (MRGF)
      READ(MRGF,'(A)') REC
      CALL QNEOFERROR(MRGF)
      RETURN
      END SUBROUTINE ECRREA
