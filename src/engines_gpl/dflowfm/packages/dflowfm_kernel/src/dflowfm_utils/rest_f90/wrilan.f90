      SUBROUTINE WRILAN(MPOL)
      USE M_LANDBOUNDARY
      implicit none
      integer :: mpol
      integer :: mx
      double precision, ALLOCATABLE :: XL(:), YL(:)
      double precision :: ZL(0)    ! no z-values
      character(len=1) :: names(1) ! no names

      MX = MAXLAN
      ALLOCATE ( XL(MX), YL(MX))
      XL (1:MXLAN)  = XLAN(1:MXLAN)
      YL (1:MXLAN)  = YLAN(1:MXLAN)
      names = ' '

      CALL WRILDB(MPOL, XL, YL, MXLAN, nclan, MXLAN, ZL, 0, names, 1, 1)
      DEALLOCATE (XL, YL)

      END
