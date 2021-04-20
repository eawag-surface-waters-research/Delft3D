      subroutine cleargrid()
      use m_grid
      use unstruc_colors
      implicit none
      integer :: key
      integer :: ndraw
      COMMON /DRAWTHIS/ ndraw(50)
      call tekgrd(XC,YC,MMAX,NMAX,1,1,mc,nc,0,ndraw(38),key,MC)
      end subroutine cleargrid
