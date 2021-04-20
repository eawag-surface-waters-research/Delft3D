      SUBROUTINE LOCALREFINE(num, m1, n1, m2, n2, NOPTION)
      implicit none
      integer :: num, m1, m2, n1, n2, NOPTION

      if (NOPTION == 1) then
          CALL REFINE(M1,N1,M2,N2,NUM)
      else if (NOPTION == 2) then
          CALL DEREFINE(M1,N1,M2,N2,NUM)
      end if
      END subroutine localrefine
