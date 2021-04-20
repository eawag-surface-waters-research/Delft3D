      SUBROUTINE KTEXT(TEXNU,NX,NY,NCOL)
      implicit none
      integer :: ncol
      integer :: nx
      integer :: ny
!     tekst op normale text posities met standaard blauwe achtergrond
      CHARACTER* (*) TEXNU
      CALL ITEXTCOLOURN(NCOL,5)
      CALL IOUTSTRINGXY(NX,NY,trim(TEXNU))
      RETURN
      END
