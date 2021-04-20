      SUBROUTINE PAGE(HLPTXT,NUMTXT,NUMTOP,IH)
      implicit none
      integer :: i
      integer :: ih
      integer :: line
      integer :: numtop
      integer :: numtxt
!     Display one page, take care, numtop =< numtxt-ih
      CHARACTER HLPTXT(NUMTXT)*(*)
      LINE    = 0
      DO 10 I = NUMTOP,MIN(NUMTOP + IH - 1,NUMTXT)
         LINE = LINE + 1
         CALL IWinOutStringXY(1,LINE,HLPTXT(I))
   10 CONTINUE
      RETURN
      END
