 integer function julday(mm,id,iyyy)
 implicit none
 integer :: igreg
 integer :: mm, id, iyyy
 integer :: jy, jm, ja
 parameter (igreg=15+31*(10+12*1582))
 !     if (iyyy.eq.0) pause 'there is no year zero.'
 if (iyyy.lt.0) iyyy=iyyy+1
 if (mm.gt.2) then
   jy=iyyy
   jm=mm+1
 else
   jy=iyyy-1
   jm=mm+13
 endif
 julday=int(365.25*jy)+int(30.6001*jm)+id+1720995
 if (id+31*(mm+12*iyyy).ge.igreg) then
   ja=int(0.01*jy)
   julday=julday+2-ja+int(0.25*ja)
 endif
 return
 end function julday
