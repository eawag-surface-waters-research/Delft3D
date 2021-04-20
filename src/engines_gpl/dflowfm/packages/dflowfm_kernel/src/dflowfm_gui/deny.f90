      SUBROUTINE DENY(IXP,IYP)
      implicit none
      integer :: infoattribute
      integer :: ixp
      integer :: iyp
      integer :: nbckgr
      integer :: nforgr
      NFORGR = InfoAttribute(13)
      NBCKGR = InfoAttribute(14)
      CALL IWinAction('FPC')
      CALL ITEXTCOLOUR('BWHITE','RED')
      CALL IWinOpen(IXP+40,IYP+9,24,2)
      CALL IWinOutStringXY(1,1,'THIS FILE DOES NOT EXIST')
      CALL IWinOutStringXY(1,2,'CHOOSE ANOTHER OR EXIT')
      CALL TOEMAAR()
      CALL IWinClose(1)
      CALL ITEXTCOLOURN(NFORGR,NBCKGR)
      RETURN
      END
