   subroutine setmodind(rouwav, modind)
   implicit none
   integer, intent(out)     :: modind
   character*4, intent(in)  :: rouwav

   modind = 0  ! safety
   if (rouwav=='FR84') then
      modind = 1
   elseif (rouwav=='MS90') then
      modind = 2
   elseif (rouwav=='HT91') then
      modind = 3
   elseif (rouwav=='GM79') then
      modind = 4
   elseif (rouwav=='DS88') then
      modind = 5
   elseif (rouwav=='BK67') then
      modind = 6
   elseif (rouwav=='CJ85') then
      modind = 7
   elseif (rouwav=='OY88') then
      modind = 8
   elseif (rouwav=='VR04') then
      modind = 9
   elseif (rouwav=='RU03') then
      modind = 10
   endif
   end subroutine setmodind
