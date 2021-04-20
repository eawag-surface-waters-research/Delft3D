      SUBROUTINE SEARCH(NAHEAD,NLEVEL,HLPTXT,NUMTXT,WRDKEY,NUMCHC,JOFND)
      implicit none
      integer :: jofnd
      integer :: k
      integer :: len
      integer :: nahead
      integer :: nlevel
      integer :: numchc
      integer :: numtxt
!     Search at level NLEVEL
      CHARACTER HLPTXT(NUMTXT)*(*),WRDKEY*40

      LEN   = len_trim(WRDKEY)
      IF (LEN .EQ. 0) RETURN

      JOFND = 0
      K     = NUMCHC - NAHEAD

   10 CONTINUE
      K = K + NAHEAD
      IF (K .GT. NUMTXT .OR. K .LT. 1) THEN
         IF (JOFND .EQ. 0) CALL OKAY(0)
         RETURN
      ELSE
         IF (HLPTXT(K)(NLEVEL:NLEVEL+LEN-1) .NE. WRDKEY) GOTO 10
      ENDIF

      JOFND  = 1
      NUMCHC = K
      RETURN
      END
