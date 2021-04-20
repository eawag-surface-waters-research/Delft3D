 subroutine statisticsonemorepoint(dif)
 use m_statistics
 implicit none

 double precision :: dif
 avedif = avedif + dif
 sqadif = sqadif + dif*dif
 dmxdif = max(dmxdif,dif)
 numdif = numdif + 1
 end subroutine statisticsonemorepoint
