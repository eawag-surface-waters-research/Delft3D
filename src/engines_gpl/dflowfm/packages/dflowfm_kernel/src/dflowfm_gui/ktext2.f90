      SUBROUTINE KTEXT2(TEX,NX,NY,NCOL,NCOL2)
      implicit none
      integer :: ncol
      integer :: ncol2
      integer :: nx
      integer :: ny
!     tekst op normale text posities met EIGEN achtergrond
      CHARACTER* (*) TEX
      CALL ITEXTCOLOURN(NCOL,NCOL2)
      CALL IOUTSTRINGXY(NX,NY,trim(TEX))
      RETURN
      END
