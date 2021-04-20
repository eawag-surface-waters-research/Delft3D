      SUBROUTINE SETCOLORTABLE()
      implicit none
      double precision :: VMAX,VMIN,DV,VAL
      integer          :: NCOLS,NV,NIS,NIE,JAAUTO

      double precision :: VMAX2,VMIN2,DV2,VAL2
      integer          :: NCOLS2,NV2,NIS2,NIE2,JAAUTO2

      integer          :: i

      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
      COMMON /DEPMAX2/ VMAX2,VMIN2,DV2,VAL2(256),NCOLS2(256),NV2,NIS2,NIE2,JAAUTO2

      NIS  = 72
      DO I = 1,256
         NCOLS(I) = MIN(255,NIS + I-1)
      enddo

      NIS2 = 136
      DO I = 1,256
         NCOLS2(I) = MIN(255,NIS2 + I-1)
      enddo
      end SUBROUTINE SETCOLORTABLE
