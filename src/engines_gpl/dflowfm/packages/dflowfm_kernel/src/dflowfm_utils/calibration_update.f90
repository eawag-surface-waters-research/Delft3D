subroutine calibration_update()
 use m_calibration
 use m_flow,        only: frcu, frcu_bkp, cfclval
 use m_flowgeom,    only: lnx, ln2lne

 integer :: LF
 integer :: L

 ! Update calibration definitions for discharge and water level dependent values
 call update_clddata()

 ! Update average calibration areal definitions (background calibration value = 1)
 call update_clldata()

 ! Apply calibration factor
 do LF = 1, lnx
     L        = ln2lne(LF)
     frcu(LF) = frcu_bkp(LF) * cfclval(L)
 end do

end subroutine calibration_update
