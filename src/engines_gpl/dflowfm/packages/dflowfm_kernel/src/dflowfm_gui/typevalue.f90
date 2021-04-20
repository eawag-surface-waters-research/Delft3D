   SUBROUTINE TYPEVALUE(RD,KEY)
   USE M_DEVICES
   implicit none
   double precision :: rdin
   double precision :: RD
   integer :: KEY
   integer :: infoinput

   RDIN = RD
   CALL INPOPUP('ON')
   CALL INHIGHLIGHT('WHITE','RED')
   CALL INDOUBLEXYDEF(IWS/2-10,IHS-3,'VALUE : ',1,RD,11,'(F11.4)')
   KEY = InfoInput(55)
   IF (KEY .EQ. 23) THEN
      RD = RDIN
   ENDIF
   CALL INPOPUP('OFF')
   RETURN
   END SUBROUTINE TYPEVALUE
