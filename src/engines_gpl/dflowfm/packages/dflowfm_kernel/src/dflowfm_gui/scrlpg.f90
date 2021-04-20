!
      SUBROUTINE SCRLPG(HLPTXT,NUMTXT,NUMTOP,NUMCHC,IH)
      implicit none
      integer :: ih
      integer :: numchc
      integer :: numtop
      integer :: numtxt
!     Display choiceline and one page, take care, numchc <= numtxt
      CHARACTER HLPTXT(NUMTXT)*(*)
!
      IF (NUMCHC .LT. NUMTOP) THEN
         NUMTOP = NUMCHC
      ELSE IF (NUMCHC .GE. NUMTOP+IH) THEN
         NUMTOP = NUMCHC - IH + 1
      ENDIF
!
      CALL PAGE(HLPTXT,NUMTXT,NUMTOP,IH)
!
      RETURN
      END
