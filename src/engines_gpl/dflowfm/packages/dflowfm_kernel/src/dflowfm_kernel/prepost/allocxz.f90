   SUBROUTINE ALLOCXZ()
   use m_netw
   USE M_FLOWGEOM
   implicit none

   integer :: mxp
   INTEGER :: IERR

   IF (ALLOCATED(XZ) ) DEALLOCATE  (XZ, YZ)
   MXP = MAX(NUMP, NDX)
   ALLOCATE ( XZ(MXP), YZ(MXP) , STAT=IERR)
   CALL AERR('XZ(MXP), YZ(MXP)', IERR, 2*MXP)

   END SUBROUTINE ALLOCXZ
