SUBROUTINE MINMXNETCELLS()

  use m_netw
  use m_flowgeom
  use m_missing

  implicit none

  double precision :: dv
  integer :: i
  integer :: jaauto
  integer :: k
  integer :: ncols
  integer :: nie
  integer :: nis
  integer :: nv
  double precision :: rd
  double precision :: rmax
  double precision :: rmin
  double precision :: val
  double precision :: vmax
  double precision :: vmin

  double precision, external :: znetcell
  logical inview

  ! BEPAAL MINIMUM EN MAXIMUM VAN DIEPTES BINNEN VIEWING AREA
  COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

  IF (JAAUTO > 0) THEN
     RMIN =  1.0D30
     NODMIN = 0
     RMAX = -1.0D30
     NODMAX = 0
     DO K = 1,max(NUMP,nump1d2d)
        IF ( INVIEW(XZ(K),YZ(K)) ) THEN
           RD = RLIN(K)
           IF (rd .ne. dmiss) then
              IF (RD < RMIN ) THEN
                 RMIN = RD
                 netcelMIN = K
              ENDIF
              IF (RD > RMAX) THEN
                 RMAX = RD
                 netcelMAX = K
              ENDIF
           ENDIF
        ENDIF
     ENDDO
     VMAX = RMAX
     VMIN = RMIN
  ENDIF

  DV   = VMAX - VMIN
  DO I = 1,NV
     VAL(I) = VMIN + (I-1)*DV/(NV-1)
  ENDDO

  RETURN
  END SUBROUTINE MINMXNETCELLS
