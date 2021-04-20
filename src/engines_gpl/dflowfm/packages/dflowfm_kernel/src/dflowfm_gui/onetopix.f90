      SUBROUTINE ONETOPIX(X,Y,NX,NY)
      use m_devices
      implicit none
      integer :: nx
      integer :: ny
      double precision :: x
      double precision :: y
      NX = X*NPX
      NY = Y*NPY
      RETURN
      END
