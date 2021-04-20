      subroutine TEKgrid(key)
      use m_grid
      use unstruc_colors
      implicit none
      integer :: key
      integer :: ndraw
      COMMON /DRAWTHIS/ ndraw(50)
      call tekgrd(XC,YC,MMAX,NMAX,1,1,mc,nc,NCOLDG,ndraw(38),key,MC)

      end subroutine TEKgrid
