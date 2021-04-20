   subroutine fm_red_soursin()
   !
   !!--description-----------------------------------------------------------------
   !
   !    Function: Reduces sourse and sink terms to avoid large
   !              bed level changes
   !
   !!--declarations----------------------------------------------------------------
   use precision
   use sediment_basics_module
   use unstruc_files, only: mdia
   use m_flowtimes, only: dts, tstart_user, time1, dnt, tfac
   use m_transport, only: ised1
   use m_flow, only: s0, s1, kmx, zws, zws0
   use m_transport, only: constituents
   use message_module, only: write_warning
   use m_flowgeom
   use m_fm_erosed
   !
   implicit none
   !
   real(fp), dimension(kmx)             :: thick
   !
   ! Local parameters
   !
   integer, parameter  :: reducmessmax = 50
   !
   ! Global variables
   !
   !
   ! Local variables
   !
   integer          :: lstart, reducmesscount, l, ll, nm, kmaxsd, kb, kt
   real(fp)         :: h0, h1, thick0, thick1, dz, reducfac
   character(150)   :: message
   !
   !! executable statements -------------------------------------------------------
   !!
   lstart         = ised1-1
   reducmesscount = 0
   !
   do l = 1, lsed
      ll = lstart + l
      !
      ! Reduction is not applied to mud and not to bedload
      !
      if (sedtyp(l) == SEDTYP_NONCOHESIVE_SUSPENDED) then
         if (kmx > 0) then
            do nm = 1, ndx
               call getkbotktop(nm, kb, kt)
               !
               ! Apply reduction factor to source and sink terms if
               ! bottom is closer than user-specified threshold and
               ! erosive conditions are expected
               !
               kmaxsd = kmxsed (nm,l)
               h0     = max(0.01_fp, s0(nm) - bl(nm))
               h1     = max(0.01_fp, s1(nm) - bl(nm))
               thick0 = zws0(kb)-zws0(kb-1) ! should be safe for z and sigma, was: thick(kmaxsd)*h0
               thick1 = zws(kb)-zws(kb-1)   ! thick(kmaxsd)*h1
               dz     = (dts*morfac/cdryb(l)) &
                  & * (sourse(nm, l)*thick0 - (sinkse(nm, l)+sour_im(nm, l))*thick1*constituents(ll, nm))
               if (abs(dz) > h1*dzmax) then
                  reducfac = (h1*dzmax)/abs(dz)
                  if (reducfac < 0.01 .and. (time1 > tstart_user + tmor * tfac) .and. bedupd) then
                     !                   !
                     !                   ! Only write reduction warning when bed updating is true (and started)
                     !                   ! (otherwise no problem)
                     !                   ! Limit the number of messages with reducmessmax
                     !                   ! (otherwise tri-diag will grow very fast)
                     !                   !
                     reducmesscount = reducmesscount + 1
                     if (reducmesscount <= reducmessmax) then
                        !call nm_to_n_and_m(nm, n, m, gdp)
                        write(message,'(a,i0,a,f12.2,a,2(i0,a))') &
                           & 'Source and sink term sediment ',l,' reduced with factor', &
                           & 1/reducfac,' node number=(',nm,'), after ', int(dnt) , ' timesteps.'
                        call write_warning(message, unit=mdia)
                     endif
                  endif
                  sourse(nm, l)  = sourse(nm, l) *reducfac
                  sour_im(nm, l) = sour_im(nm, l)*reducfac
                  sinkse(nm, l)  = sinkse(nm, l) *reducfac
               endif
               !
               ! Apply reduction factor to source and sink
               ! terms if bottom is closer than user-specified
               ! threshold and erosive conditions are expected
               !
               !
               ! If erosion conditions are expected then apply
               ! reduction factor to sour and sink terms.
               ! estimate sink term based on previous cell
               ! concentration
               !
               if ((sinkse(nm,l)+sour_im(nm,l))*constituents(ll, nm) < sourse(nm,l)) then
                  sinkse(nm, l)  = sinkse(nm, l) * fixfac(nm,l)
                  sourse(nm, l)  = sourse(nm, l) * fixfac(nm,l)
                  sour_im(nm, l) = sour_im(nm, l)* fixfac(nm,l)
                  rsedeq(nm, l)  = rsedeq(nm, l) * fixfac(nm,l)
               endif
            enddo               ! nm
         else
            do nm = 1, ndx
               !
               h0     = max(0.01_fp, s0(nm) - bl(nm))
               h1     = max(0.01_fp, s1(nm) - bl(nm))
               dz     = (dts*morfac/cdryb(l)) &
                  & * (sourse(nm, l)*h0 - (sinkse(nm, l)+sour_im(nm, l))*h1*constituents(ll, nm))
               !
               if (abs(dz) > h1*dzmax) then
                  reducfac = (h1*dzmax)/abs(dz)
                  if (reducfac < 0.01 .and. (time1 > tstart_user + tmor * tfac) .and. bedupd) then
                     reducmesscount = reducmesscount + 1
                     !if (reducmesscount <= reducmessmax) then
                     !   write(message,'(a,i0,a,f12.2,a,2(i0,a))') &
                     !      & 'Source and sink term sediment ',l,' reduced with factor', &
                     !      & 1/reducfac,' node number=(',nm,'), after ', int(dnt) , ' timesteps.'
                     !   call write_warning(message, unit=mdia)
                     !endif
                  endif
                  sourse(nm, l)  = sourse(nm, l) *reducfac
                  sour_im(nm, l) = sour_im(nm, l)*reducfac
                  sinkse(nm, l)  = sinkse(nm, l) *reducfac
               endif
               !
               if ((sinkse(nm,l)+sour_im(nm,l))*constituents(ll, nm) < sourse(nm,l)) then
                  sinkse(nm, l)  = sinkse(nm, l) * fixfac(nm,l)
                  sourse(nm, l)  = sourse(nm, l) * fixfac(nm,l)
                  sour_im(nm, l) = sour_im(nm, l)* fixfac(nm,l)
                  rsedeq(nm, l)  = rsedeq(nm, l) * fixfac(nm,l)
               endif
            enddo            ! nm
         endif               ! kmx
      endif                  ! sedtyp
   enddo                     ! lsedtot
   if (reducmesscount > reducmessmax) then
      write (mdia,'(12x,a,i0,a)') 'Reduction messages skipped (more than ',reducmessmax,')'
      write (mdia,'(12x,a,f5.0,a,i0)') 'Total number of reduction messages for timestep ', &
         & dnt,' : ', reducmesscount
   endif
   !
   end subroutine fm_red_soursin
