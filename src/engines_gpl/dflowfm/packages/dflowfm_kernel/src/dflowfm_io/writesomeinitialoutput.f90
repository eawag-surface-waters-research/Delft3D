subroutine writesomeinitialoutput()
 use m_sferic
 use timers
 use m_flow
 use m_netw
 use m_flowgeom
 use m_flowtimes
 use unstruc_messages
 use m_partitioninfo
 use m_samples
 use unstruc_model, only: md_ident
 use geometry_module, only: dbdistance

 implicit none
 integer          :: k, mbalat,mwrong,L,msam, n, nf, jacheckba = 0
 double precision :: batotown(1), batot(1), voltotown(1), volto(1), dist, dismin

 batotown = 0 ; voltotown = 0

 ! Stop for initialisation timer and start timer for actual computation time
 call datum (rundat0)

 write(msgbuf,'(a,a)') 'Modelinit finished   at: '         , rundat0                  ; call msg_flush()

 msgbuf = ' ' ; call msg_flush()
 msgbuf = ' ' ; call msg_flush()

 write(msgbuf,'(a,I25)')    'nr of netnodes         ( )  :' , numk                       ; call msg_flush()
 write(msgbuf,'(a,I25)')    'nr of netlinks         ( )  :' , numl                       ; call msg_flush()
 write(msgbuf,'(a,I25)')    'nr of flownodes        ( )  :' , ndx                        ; call msg_flush()
 write(msgbuf,'(a,I25)')    'nr of openbnd cells    ( )  :' , ndx - ndxi                 ; call msg_flush()
 write(msgbuf,'(a,I25)')    'nr of 1D-flownodes     ( )  :' , ndxi - ndx2d               ; call msg_flush()
 write(msgbuf,'(a,I25)')    'nr of flowlinks        ( )  :' , lnx                        ; call msg_flush()
 write(msgbuf,'(a,I25)')    'nr of internal links   ( )  :' , lnxi                       ; call msg_flush()
 write(msgbuf,'(a,I25)')    'nr of 1D links         ( )  :' , lnx1D                      ; call msg_flush()
 write(msgbuf,'(a,I25)')    'nr of closed walls     ( )  :' , mxwalls                    ; call msg_flush()

 if (kmx > 0) then
 write(msgbuf,'(a,I25)')    'max nr of layers       ( )  :' , kmx                        ; call msg_flush()
 write(msgbuf,'(a,I25)')    'nr of 3D cells         ( )  :' , ndkx-2*ndx                 ; call msg_flush()
 write(msgbuf,'(a,I25)')    'nr of 3D links         ( )  :' , Lnkx-2*lnx                 ; call msg_flush()
 endif

 msgbuf = ' ' ; call msg_flush()
 msgbuf = ' ' ; call msg_flush()

 if (jacheckba == 1)  then
    if (jampi == 1) then
       L = index(md_ident,'_0') - 1
       call oldfil(msam, 'ba_'//trim(md_ident(1:L))//'.xyz')
       call reasam(msam, 0)
       call newfil(mwrong, 'bawrong_'//trim(md_ident)//'.xyz')
   endif
   call newfil(mbalat, 'ba_'//trim(md_ident)//'.xyz')
 endif

 DO K = 1,NDXI
    if (jampi == 1) then
       if (idomain(k) == my_rank) then
          batotown(1)  = batotown(1)  +   ba(k)
          voltotown(1) = voltotown(1) + vol1(k)
          if (jacheckba == 1) then
              write(mbalat,*) xz(k), yz(k), ba(k)
              if (ns > 0) then
                 dismin = 1d9 ; nf = 0
                 do n = 1,ns
                    call dbdistancehk(xz(k), yz(k), xs(n), ys(n), dist)
                    if (dist  < dismin) then
                       dismin = dist
                       nf     = n
                    endif
                 enddo
                 if (nf > 0 .and. dismin < 1d0) then
                    if ( abs( ba(k) - zs(nf) ) > 1d-4 ) then
                        write(mwrong,'(4F20.5)') xz(k), yz(k), ba(k), zs(nf)
                    endif
                 endif
              endif
           endif
       endif
    else
       batotown(1)  = batotown(1)  +   ba(k)
       voltotown(1) = voltotown(1) + vol1(k)
       if (jacheckba == 1) then
          write(mbalat,*) xz(k), yz(k), ba(k)
       endif
    endif
 enddo

 if (jacheckba == 1) then
    call doclose(mbalat)
    if (jampi == 1) then
       call doclose(mwrong)
    endif
 endif

 write(msgbuf,'(a,E25.10)') 'my model area          (m2) :' , batotown                   ; call msg_flush()
 if (jampi == 1) then
 k = 1
 call reduce_double_sum(k, batotown, batot )
 write(msgbuf,'(a,E25.10)') 'total model area       (m2) :' , batot                      ; call msg_flush()
 endif

 write(msgbuf,'(a,E25.10)') 'my model volume        (m3) :' , voltotown                  ; call msg_flush()
 if (jampi == 1) then
 k = 1
 call reduce_double_sum(k, voltotown, volto )
 write(msgbuf,'(a,E25.10)') 'total model volume     (m3) :' , volto                     ; call msg_flush()
 endif

 msgbuf = ' ' ; call msg_flush()
 msgbuf = ' ' ; call msg_flush()


 end subroutine writesomeinitialoutput
