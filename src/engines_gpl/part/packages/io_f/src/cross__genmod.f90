        !COMPILER-GENERATED INTERFACE MODULE: Fri May 12 11:19:34 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CROSS__genmod
          INTERFACE 
            SUBROUTINE CROSS(X1,Y1,X2,Y2,X3,Y3,X4,Y4,JACROS,SL,SM,XCR,  &
     &YCR,CRP)
              REAL(KIND=8), INTENT(IN) :: X1
              REAL(KIND=8), INTENT(IN) :: Y1
              REAL(KIND=8), INTENT(IN) :: X2
              REAL(KIND=8), INTENT(IN) :: Y2
              REAL(KIND=8), INTENT(IN) :: X3
              REAL(KIND=8), INTENT(IN) :: Y3
              REAL(KIND=8), INTENT(IN) :: X4
              REAL(KIND=8), INTENT(IN) :: Y4
              INTEGER(KIND=4) :: JACROS
              REAL(KIND=8) :: SL
              REAL(KIND=8) :: SM
              REAL(KIND=8) :: XCR
              REAL(KIND=8) :: YCR
              REAL(KIND=8) :: CRP
            END SUBROUTINE CROSS
          END INTERFACE 
        END MODULE CROSS__genmod
