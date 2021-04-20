      SUBROUTINE NEXT(NAHEAD,NLEVEL,NUMCHC,HLPTXT,NUMTXT)
      implicit none
      integer :: nahead
      integer :: nlevel
      integer :: numchc
      integer :: numtxt
!     Searches for previous or next keyword at level nlevel
      CHARACTER HLPTXT(NUMTXT)*(*)
   10 CONTINUE

      NUMCHC = NUMCHC + NAHEAD
      IF (NUMCHC .LE. 1) THEN
         NUMCHC = 1
      ELSE IF (NUMCHC .GE. NUMTXT) THEN
         NUMCHC = NUMTXT
      ELSE IF (HLPTXT(NUMCHC)(1:NLEVEL) .EQ. '   ') THEN
         GOTO 10
      ENDIF

      RETURN
      END
