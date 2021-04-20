  SUBROUTINE SYSORLOCALFIL(LUNID,FILNAM,MUSTBE)
  use string_module, only: find_first_char
  use unstruc_messages
  use unstruc_files
  implicit none
  CHARACTER(len=*), intent(in   ) :: FILNAM !< Name of file to be opened.
  integer,          intent(  out) :: lunid  !< File unit of the opened file, 0 in case of error.
  integer,          intent(in   ) :: mustbe !< Whether or not (1/0) the file must be checked whether it exists. When 1 and file does not exist, an error is given.

  integer :: istart
  integer :: k1
  integer :: k2
  integer :: istat
  CHARACTER FULNAM*180
  LOGICAL JA

  LUNID = 0
  INQUIRE(FILE = FILNAM,EXIST = JA)
  IF (JA) THEN
     CALL OLDFIL(LUNID,FILNAM)
     WRITE(msgbuf,'(2A)') 'Using Local File', FILNAM; call msg_flush()
  ELSE

     FULNAM = PATHDI
     ISTART = len_trim(PATHDI) + 1
     WRITE(FULNAM(ISTART:),'(A)') FILNAM
     K1 = find_first_char(FULNAM)
     K2 = len_trim(FULNAM)
     INQUIRE(FILE= FULNAM(K1:K2),EXIST=JA)
     IF (JA) THEN
        CALL OLDFIL(LUNID,FULNAM)
        call mess(LEVEL_INFO, 'Using Program File ', FULNAM(K1:K2))
     ELSE IF (MUSTBE .EQ. 1) THEN
        call mess(LEVEL_ERROR, 'Program File ',FULNAM(K1:K2),' Not Found')
     ENDIF

  ENDIF

  RETURN
  END
