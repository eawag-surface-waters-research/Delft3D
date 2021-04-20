      SUBROUTINE SEARC2(NAHEAD,NLEVEL,HLPTXT,NUMTXT,LOOKUP,NUMCHC,JOFND)
      implicit none
      integer :: jofnd
      integer :: k,len
      integer :: nahead
      integer :: nlevel
      integer :: numchc
      integer :: numtxt
!     Search everywhere
      CHARACTER HLPTXT(NUMTXT)*(*),LOOKUP*20

      LEN   = len_trim(LOOKUP)
      IF (LEN .EQ. 0) RETURN

      JOFND = 0
      K     = NUMCHC - NAHEAD

   10 CONTINUE
      K = K + NAHEAD
      IF (K .GT. NUMTXT .OR. K .LT. 1) THEN
         IF (JOFND .EQ. 0) CALL OKAY(0)
         RETURN
      ELSE
         IF (INDEX(HLPTXT(K),LOOKUP(1:LEN)) .EQ. 0) GOTO 10
      ENDIF

      JOFND  = 1
      NUMCHC = MIN(NUMTXT,K + 1)
      RETURN
      END
