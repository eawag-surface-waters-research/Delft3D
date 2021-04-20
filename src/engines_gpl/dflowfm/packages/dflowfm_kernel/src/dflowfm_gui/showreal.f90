      ! Now a double precision (double precision ::)
      SUBROUTINE SHOWREAL(TEXT,VALUE)
use m_devices
implicit none
integer :: infoattribute
integer :: ixp
integer :: iyp
integer :: len
integer :: nbckgr
integer :: nforgr
integer :: nlevel
double precision :: val
double precision :: value
      CHARACTER WRDKEY*40, TEXT*(*)
      COMMON /HELPNOW/   WRDKEY,NLEVEL
      VAL    = VALUE
      IXP    = IWS/2
      IYP    = IHS/2
      NFORGR = InfoAttribute(13)
      NBCKGR = InfoAttribute(14)
      LEN    = len_trim(TEXT)
      CALL INPOPUP('ON')
      CALL ITEXTCOLOUR('BWHITE','BLUE')
!      CALL IWINOPEN(IXP,IYP,LEN+8,1)
      CALL IWINOPEN(IXP,IYP,LEN+11,1)
      CALL ITEXTCOLOUR('BBLUE','BWHITE')
      CALL IWINOUTSTRINGXY(1,1,TEXT)
!      CALL IWINOUTDOUBLEXY(1+LEN,1,VALUE,'(F8.1)')
      CALL IWINOUTDOUBLEXY(1+LEN,1,VALUE,'(F11.1)')
      CALL ITEXTCOLOURN(NFORGR,NBCKGR)
      RETURN
      END
