   subroutine plotnu(fnam)
   implicit none
   COMMON /DRAWTHIS/  ndraw(50)
   COMMON /PLOTFIL/   PLOTJE
   integer :: key, ndraw
   CHARACTER PLOTJE*255
   character (len=*) fnam

   plotje = fnam
   key = 3 ; ndraw(10) = 1
   call drawnu(key)
   plotje = ' '

   end subroutine plotnu
