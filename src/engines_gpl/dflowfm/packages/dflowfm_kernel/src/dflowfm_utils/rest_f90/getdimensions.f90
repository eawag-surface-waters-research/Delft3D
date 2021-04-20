      SUBROUTINE GETDIMENSIONS(MXD,NXD,MXLN,NSX)
      implicit none
      integer :: mout
      integer :: mxd
      integer :: mxln
      integer :: nsx
      integer :: nxd
      CHARACTER GETAL*100
      LOGICAL THISISANUMBER, JAWEL

      MXD  = 500       ! ROOSTERS EN SPLINES M-RICHTING
      NXD  = 500       ! ROOSTERS EN SPLINES N-RICHTING
      MXLN = 100000    ! land boundary
      NSX  = 100000    ! SAMPLES

      GETAL = ' '
      CALL get_command_argument(1,GETAL)
      IF (THISISANUMBER(GETAL)) READ(GETAL,*) MXD

      GETAL = ' '
      CALL get_command_argument(2,GETAL)
      IF (THISISANUMBER(GETAL)) READ(GETAL,*) NXD

      GETAL = ' '
      CALL get_command_argument(3,GETAL)
      IF (THISISANUMBER(GETAL)) READ(GETAL,*) MXLN

      GETAL = ' '
      CALL get_command_argument(4,GETAL)
      IF (THISISANUMBER(GETAL)) READ(GETAL,*) NSX

      INQUIRE (FILE = 'rgfdim', EXIST = JAWEL)
      IF (JAWEL) THEN
         MOUT  = 10
         call oldfil(MOUT,'rgfdim')
         READ  (MOUT,*,ERR=999) MXD,NXD,MXLN,NSX
         call doclose(MOUT)
      ENDIF
  999 CONTINUE
      RETURN
      END
