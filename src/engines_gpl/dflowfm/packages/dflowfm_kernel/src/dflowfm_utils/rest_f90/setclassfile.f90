    SUBROUTINE SETCLASSFILE()
    implicit none
    integer :: minp
    integer :: ierror
    CHARACTER FILNAM*86
    FILNAM = '*.cls'
    MINP   = 0
    CALL FILEMENU(MINP,FILNAM,ierror)
    IF (ierror /= 0) THEN
       CALL REACLS(MINP)
    ENDIF
    RETURN
    END
