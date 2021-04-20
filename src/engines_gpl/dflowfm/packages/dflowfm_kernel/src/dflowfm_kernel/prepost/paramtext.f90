   SUBROUTINE PARAMTEXT(OPTION,NR)
   use M_isoscaleunit
   implicit none
   integer :: l1
   integer :: l2
   CHARACTER*(*) OPTION
   INTEGER NR
   L1 = INDEX(OPTION,'(')
   L2 = INDEX(OPTION,')')
   UNIT(NR) =  ' ' ; PARAMTEX(NR) = ' '
   IF (L1 .NE. 0) WRITE(UNIT(NR)(1:L2-L1+1),'(A)')   OPTION(L1:L2)
   WRITE(PARAMTEX(NR)(1:14) ,'(A)')   OPTION(1:14)
   RETURN
   END SUBROUTINE PARAMTEXT
