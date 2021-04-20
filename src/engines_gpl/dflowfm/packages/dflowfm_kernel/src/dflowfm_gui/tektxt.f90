      subroutine TEKTXT ()
      use m_wearelt
      implicit none
      integer :: ia
      integer, save :: ini = 0
      integer :: k
      integer :: maxtxt
      integer :: ntxt
!      ------------------------------------------------------------------
!     tekenen van de strings die in een file staan en ingelezen zijn met
!     REATXT
!     ------------------------------------------------------------------
      common /XYTEXT/    xtxt,ytxt,coltxt,symtxt,heitxt,ntxt
      common /TEXTSS/    xytexts
      parameter (maxtxt = 2000)
      double precision :: xtxt(maxtxt), ytxt(maxtxt),heitxt(maxtxt)
      integer    symtxt(maxtxt), coltxt(maxtxt)
      character  xytexts(maxtxt)*120

      if (ini .eq. 0) then
         ntxt = 0
         ini  = 1
      endif
      IF (NTXT .LE. 0) RETURN

!     call IGrSymbSet('calctek.smb')
!     call IGrCharSet('symbols.chr')
!     call IGrCharSize(3.0,3.0)
!     call IGrCharJustify ('C')

      do 10 k = 1,ntxt
         call SETCOL (coltxt(k))

!        call IGRMOVETO    ( xtxt(k),ytxt(k) )
!        call IGRCIRCLEREL ( rcir            )

         call IGrCharJustify ('C')
         call IGrCharSize (real(heitxt(k)),real(heitxt(k)))

         if (symtxt(k) .ne. 0) then
            call IGrSymbOut (real(xtxt(k)),real(ytxt(k)),symtxt(k))
         endif

         call IGrMoveTo    ( real(xtxt(k)+1.1*rcir),real(ytxt(k)) )
         ia = len_trim(xytexts(k))
         call IGrCharJustify ('L')
         call DRAWTEXT   ( real(xtxt(k)+1.1*rcir),real(ytxt(k)),xytexts(k)(1:ia))
   10 continue

      call IGrCharSize (0.5,0.5)

      return
      end
