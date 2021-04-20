      SUBROUTINE WRIARCsam(MARC,DP,MMAX,NMAX,MC,NC,X0,Y0,DX,DY,dmiss)
      implicit none
      double precision :: dmiss
      double precision :: dp
      double precision :: dx, dy
      integer :: i
      integer :: j
      integer :: marc
      integer :: mc
      integer :: mmax
      integer :: nc
      integer :: nmax
      double precision :: x0
      double precision :: y0
!      DIMENSION DP  (MMAX,NMAX)
      DIMENSION DP  (NMAX,MMAX)   ! SPvdP: j-index is fastest running in sample arrays

      CALL WRITEARCINFOHEADER(MARC,MC,NC,X0,Y0,DX,DY,dmiss)

!      DO 10 J = NC,1,-1      ! SPvdP: j-index is already reverse-order in sample arrays
      DO 10 J = 1,NC
!         WRITE(MARC,'(5000F10.2)') ( DP(I,J),I = 1,MC)
         WRITE(MARC,'(5000F10.2)') ( DP(J,I),I = 1,MC)   ! SPvdP: j-index is fastest running in sample arrays
   10 CONTINUE

      RETURN
      END
