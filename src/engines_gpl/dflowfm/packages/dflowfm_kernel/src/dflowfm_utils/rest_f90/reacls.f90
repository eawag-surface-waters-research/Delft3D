      SUBROUTINE REACLS (MLAN)      ! DV
      implicit none
      double precision :: dv
      integer :: i
      integer :: jaauto
      integer :: mlan
      integer :: ncols
      integer :: nie
      integer :: nis
      integer :: nrow
      integer :: nv
      double precision :: val
      double precision :: vmax
      double precision :: vmin
      double precision :: x
!     ------------------------------------------------------------------
!     LEZEN FILE MET CLASSES
!    ------------------------------------------------------------------
      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

      CHARACTER MATR*4

      CALL READYY ('READING CLS-FILE',0d0)

!     ------------------------------------------------------------------
!     EERST LEZEN ALS DUMMY WAARDEN OM TE TESTEN OF ER FOUTEN OPTREDEN
!     ------------------------------------------------------------------
   10 READ(MLAN,'(A)',END=999) MATR
      IF (MATR(1:1) .EQ. '*') GOTO 10

      READ (MLAN,*,ERR=999) NROW

      NROW = MIN(NROW,30)

      DO 20 I = 1,NROW
         READ (MLAN,*,ERR=999,END=999) X
   20 CONTINUE

!     -------------------------------------
!     NO ERRORS, SO READ THE VALUES
!     -------------------------------------
      REWIND (MLAN)

  110 READ(MLAN,'(A)',END=999) MATR
      IF (MATR(1:1) .EQ. '*') GOTO 110

      READ (MLAN,*,ERR=999) NROW

      NROW = MIN(NROW,30)

      DO 120 I = 1,NROW
         READ (MLAN,*) VAL(I)
         VMIN = MIN(VMIN,VAL(I))
         VMAX = MAX(VMAX,VAL(I))
  120 CONTINUE

      JAAUTO = 2
      NV = NROW

  999 CONTINUE

      CALL READYY(' ', 1d0)
      CALL READYY(' ',-1d0)
      call doclose (MLAN)
      RETURN
      END
