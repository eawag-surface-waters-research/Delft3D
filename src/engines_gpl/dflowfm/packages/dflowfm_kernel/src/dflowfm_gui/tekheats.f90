  SUBROUTINE TEKHEATS( TIMNOW)
  use m_heatfluxes
  implicit none
  double precision       :: TIMNOW, tday

  TDAY = modulo (TIMNOW, 1440d0*60d0)
  CALL GTEXT('SUN',TDAY,QSunav  ,221)
  CALL GTEXT('LWR',TDAY,QLongav ,221)
  CALL GTEXT('CON',TDAY,QEVAav  ,221)
  CALL GTEXT('EVA',TDAY,QCONav  ,221)
  CALL GTEXT('fre',TDAY,Qfreeav ,221)

  RETURN
   END
