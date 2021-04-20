      SUBROUTINE HELPIN()
      use unstruc_files
      implicit none
      integer :: k
      integer :: maxhlp
      integer :: numtxt
!     reads NUMTXT lines of HELPTEXT
      PARAMETER (MAXHLP = 2000)
      CHARACTER HLPTXT(MAXHLP)*80
      COMMON /HELPC/  HLPTXT,NUMTXT

      NUMTXT = 0
      IF (MHLP == 0) RETURN

      K    = 0

   10 CONTINUE
      K = K + 1
      READ(MHLP,'(A)',END = 9999) HLPTXT(K)
      GOTO 10

 9999 CONTINUE
      call doclose(mhlp)
      NUMTXT = K - 1

      RETURN
      END
