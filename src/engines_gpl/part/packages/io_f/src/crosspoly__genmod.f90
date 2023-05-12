        !COMPILER-GENERATED INTERFACE MODULE: Fri May 12 09:27:05 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CROSSPOLY__genmod
          INTERFACE 
            SUBROUTINE CROSSPOLY(XA,YA,XB,YB,XPL,YPL,NPL,XM,YM,CRPM,JA)
              INTEGER(KIND=4) :: NPL
              REAL(KIND=8) :: XA
              REAL(KIND=8) :: YA
              REAL(KIND=8) :: XB
              REAL(KIND=8) :: YB
              REAL(KIND=8) :: XPL(NPL)
              REAL(KIND=8) :: YPL(NPL)
              REAL(KIND=8) :: XM
              REAL(KIND=8) :: YM
              REAL(KIND=8) :: CRPM
              INTEGER(KIND=4) :: JA
            END SUBROUTINE CROSSPOLY
          END INTERFACE 
        END MODULE CROSSPOLY__genmod
