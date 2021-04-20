 subroutine GETSHIPCONTROL()
    use m_ship
    implicit none

    integer :: ndraw
    COMMON /DRAWTHIS/ ndraw(50)

    integer          :: key, n

    CALL InKeyEventIMM(KEY)

    n = 0
                                 !        pijltjesbeweging
    IF (KEY .EQ. 128) THEN
       fstuw(1) = min( 1d0, fstuw(1) + 0.02)
       n = 1
    ELSE IF (KEY .EQ. 129) THEN
       fstuw(1) = max(-1d0, fstuw(1) - 0.02)
       n = 1
    ELSE IF (KEY .EQ. 130) THEN
       fROER(1) = MIN( 1D0, fROER(1) + 0.02)
       n = 1
    ELSE IF (KEY .EQ. 131) THEN
       fROER(1) = MAX(-1D0, fROER(1) - 0.02)
       n = 1
    ELSE IF (KEY .EQ. 53) THEN
       FSTUW(1) = 0D0
       FROER(1) = 0D0
       n = 1
    ENDIF

    IF (KEY .EQ. 87 .OR. KEY .EQ. 87+32) THEN ! W
       fstuw(2) = min( 1d0, fstuw(2) + 0.02)
       n = 2
    ELSE IF (KEY .EQ. 83 .OR. KEY .EQ.  83+32) THEN ! S
       fstuw(2) = max(-1d0, fstuw(2) - 0.02)
       n = 2
    ELSE IF (KEY .EQ. 68 .OR. KEY .EQ. 68+32) THEN
       fROER(2) = MIN( 1D0, fROER(2) + 0.02)
       n = 2
    ELSE IF (KEY .EQ. 65 .OR. KEY .EQ. 65+32) THEN
       fROER(2) = MAX(-1D0, fROER(2) - 0.02)
       n = 2
    else if (KEY .EQ. 81 .OR. KEY .EQ. 81+32) then
       FSTUW(2) = 0D0
       FROER(2) = 0D0
       n = 2
    ENDIF

    if (n > 0) THEN
        ndraw(1) = 0        ! no CLS
        call tekship()
    endif

 end subroutine getshipcontrol
