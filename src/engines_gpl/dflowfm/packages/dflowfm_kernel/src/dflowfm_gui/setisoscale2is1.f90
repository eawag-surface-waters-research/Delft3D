      SUBROUTINE SETISOSCALE2IS1()
      implicit none
      double precision :: VMAX,VMIN,DV,VAL
      integer          :: NCOLS,NV,NIS,NIE,JAAUTO

      double precision :: VMAX2,VMIN2,DV2,VAL2
      integer          :: NCOLS2,NV2,NIS2,NIE2,JAAUTO2

      integer          :: i

      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
      COMMON /DEPMAX2/ VMAX2,VMIN2,DV2,VAL2(256),NCOLS2(256),NV2,NIS2,NIE2,JAAUTO2

      if (jaauto2 == 2) then
         nv2   = nv
         VMAX2 = VMAX
         VMIN2 = VMIN
         DV2   = DV
         VAL2  = VAL
      endif

      end SUBROUTINE SETISOSCALE2IS1
