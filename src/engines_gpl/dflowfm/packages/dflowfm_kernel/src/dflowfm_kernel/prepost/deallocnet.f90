   SUBROUTINE DEALLOCNET()
   use m_netw
   USE M_FLOWgeom
   implicit none
   integer :: p, numpx

   NUMK = 0; NUML = 0
   IF (SIZE(XK) > 0) THEN
      DEALLOCATE(NOD,XK ,YK , KC ,NMK)
      DEALLOCATE(KN,LC)
   ENDIF
   IF (SIZE(XK0) > 0) THEN
      DEALLOCATE(NOD0,XK0 ,YK0 ,ZK0 , KC0 ,NMK0)
      DEALLOCATE(KN0,LC0)
   ENDIF
   IF ( SIZE(LNN)  > 0) THEN
      DEALLOCATE (LNN, LNE, ln2lne)
   ENDIF
   IF (NUMP > 0) THEN
      numpx = size(netcell)
      do p=1,numpx
        if (allocated(netcell(p)%nod)) then
            deallocate(netcell(p)%nod, netcell(p)%lin)
        end if
      end do
      deallocate (netcell)
      nump = 0
   ENDIF
   END SUBROUTINE DEALLOCNET
