   SUBROUTINE INIDEPMAX2
   use unstruc_display
   implicit none
   double precision :: VMAX,VMIN,DV,VAL
   integer          :: NCOLS,NV,NIS,NIE,JAAUTO
   double precision :: VMAX2,VMIN2,DV2,VAL2
   integer          :: NCOLS2,NV2,NIS2,NIE2,JAAUTO2
   COMMON /DEPMAX2/ VMAX2,VMIN2,DV2,VAL2(256),NCOLS2(256),NV2,NIS2,NIE2,JAAUTO2


   COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
   VMAX2   =  VMAX
   VMIN2   =  VMIN
   NV2     =  NV
   ! Actual ncols2 set later in setcoltabfile
   NIS2    =  NIS
   NIE2    =  NIE
   JAAUTO2 =  JAAUTO
   RETURN
   END SUBROUTINE INIDEPMAX2
