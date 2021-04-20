subroutine teksorsin()      ! teksrc
use m_flowexternalforcings
use unstruc_display
use m_transport, only: isalt, itemp

implicit none
COMMON /DRAWTHIS/  ndraw(50)
integer           :: ndraw
integer           :: n, k, kb, kt, n2, ncol
character*40      :: tex
double precision  :: znod, temb, temt, xp, yp
logical inview


if (ndraw(41) <= 1 .or. numsrc == 0) return

call IGrCharJustify('L')
call settextsizefac(1.0d0)

do n = 1,numsrc ! teksorsin
   k = ksrc(1,n)
   if (k .ne. 0) then
      n2 = 1 ; xp = xsrc(n,n2) ; yp = ysrc(n,n2)
      if ( inview(xp,yp) ) then
          if (qsrc(n) > 0) then
             ncol = 3
          else
             ncol = 221
          endif
          call cirr(xp, yp, ncol)
          if (ndraw(41) == 3) then
             call gtext(' '//trim(srcname(n)), xp, yp , klsrc)
          else if (ndraw(41) == 4) then
             write(tex,'(f10.3)') -qsrc(n)
             call gtext(trim(tex)//' (m3/s)', xp, yp , klsrc)
          else if (ndraw(41) == 5.and. isalt > 0) then
             if (qsrc(n) < 0d0) then
                write(tex,'(f10.3)') ccsrc(isalt,n)
                call gtext(trim(tex)//' (ppt)', xp, yp , klsrc)
             endif
          else if (ndraw(41) == 6 .and. itemp > 0) then
             if (qsrc(n) < 0d0) then
                write(tex,'(f10.3)') ccsrc(itemp,n)
                call gtext(trim(tex)//' (degC)', xp, yp , klsrc)
             endif
          endif
      endif
   endif
   k = ksrc(4,n)
   if (k .ne. 0) then
      n2 = nxsrc(n) ; xp = xsrc(n,n2) ; yp = ysrc(n,n2)
      if ( inview(xp,yp) ) then
          if (qsrc(n) > 0) then
             ncol = 221
          else
             ncol = 3
          endif
          call cirr(xp, yp, ncol)
          if (ndraw(41) == 3) then
             call gtext(' '//trim(srcname(n)), xp, yp , klsrc)
          else if (ndraw(41) == 4) then
             write(tex,'(f10.3)') qsrc(n)
             call gtext(trim(tex)//' (m3/s)', xp, yp , klsrc)
          else if (ndraw(41) == 5 .and. isalt > 0) then
             if (qsrc(n) > 0d0) then
                write(tex,'(f10.3)') ccsrc(isalt,n)
                call gtext(trim(tex)//' (ppt)', xp, yp , klsrc)
             endif
          else if (ndraw(41) == 6 .and. itemp > 0) then
             if (qsrc(n) > 0d0) then
                write(tex,'(f10.3)') ccsrc(itemp,n)
                call gtext(trim(tex)//' (degC)', xp, yp , klsrc)
             endif
          endif
      endif
   endif
enddo

end subroutine teksorsin
