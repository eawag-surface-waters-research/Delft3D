      SUBROUTINE RE0RCINFODIMENSIONS(MINP,MMAX,NMAX,DX,X0,Y0)
      implicit none
      double precision :: dx
      integer :: ja
      integer :: larc
      integer :: marc
      integer :: mc
      integer :: minp
      integer :: mmax
      integer :: nc
      integer :: nmax
      double precision :: rmis
      double precision :: x0
      double precision :: y0
      CHARACTER REC*132, FILENAME*80

      REWIND(MINP)
      CALL ZOEKJA(MINP,REC,'ARC-INFO',JA)
      IF (JA .EQ. 1) THEN
         LARC = INDEX(REC,'ARC')
         READ(REC(LARC+9:),'(A)',ERR = 888) FILENAME
         CALL OLDFIL(MARC,FILENAME)
         CALL READARCINFOHEADER(MARC,MC,NC,X0,Y0,DX,dx,RMIS)
         call doclose(MARC)
         MMAX = MC
         NMAX = NC
         CALL MESSAGE('MAIN DIMENSIONS PLUS GRID PARAMETERS HAVE',       &
                      'BEEN READ FROM ARC-INFO FILE:', FILENAME)
      ELSE
         CALL ERROR('NEITHER MAIN DIMENSIONS NOR ARC-INFO FILE FOUND,',  &
                    'SPECIFY MAIN DIMENSIONS BY KEYWORD:',               &
                    'MAIN DIMENSIONS OR BY ARC-INFO FILE')
      ENDIF
      RETURN

  888 CALL ERROR('LOOKING FOR ARC-INFO FILENAME, BUT GETTING:',REC,' ')
      END
