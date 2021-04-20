   subroutine wrihistek(tim)
   use m_observations
   use m_monitoring_crosssections
   use m_flow
   use m_flowgeom
   use m_ship
   use unstruc_model
   use m_flowtimes
   use unstruc_files, only: defaultFilename

   implicit none
   integer            :: n, i, ntbal, k1, k2
   double precision   :: tim, ue, te
   double precision   :: vv1, vv2, eh1, eh2, ee1, ee2, ft, dinch, wid,h1, h2, rr1, rr2, xl1, xl2, df1, df2, hb, AA1, AA2, QQ, s12, froude2

   character(len=256) :: nam

    if (mxls /= 0 .and. dnt == 1) then  ! volerr, volerrcum
        call doclose(mxls)
        mxls = 0
    end if

   !  return ! His file broken
   if (mxls == 0) then
      nam = defaultFilename('histek')
      call newfil(mxls, nam)
      ntbal = -1 + int(Tstop_user - Tstart_user) / Ti_xls

      if (nshiptxy == 0) then
         !write(mxls,'(a)') '* column 2  : Waterlevel Obs 1     (m  ) '
         !write(mxls,'(a)') '* column 3  : Waterdepth Obs 1     (m  ) '

         write(mxls,'(a)') '* column 1  : Time (min)'
         write(mxls,'(a)') '* column 2  : H1 DFM (m)'
         write(mxls,'(a)') '* column 3  : H2 DFM (m)'
         write(mxls,'(a)') '* column 4  : S12 DFM (m)'
         write(mxls,'(a)') '* column 5  : Hb DFM (m)'
         write(mxls,'(a)') '* column 6  : Kb1 DFM ( )'
         write(mxls,'(a)') '* column 7  : Kb2 DFM ( )'
         write(mxls,'(a)') '* column 8  : F12 DFM (m)'
         write(mxls,'(a)') '* column 9  : EH1 DFM (m)'
         write(mxls,'(a)') '* column 10 : QS DFM (m2/s)'
         write(mxls,'(a)') '* column 11 : Z12 DFM (m2/s)'
         write(mxls,'(a)') '* column 12 : Froude2 DFM ( )'


         write(mxls,'(a)') 'BL01'
         write(mxls,'(i0, a)') ntbal+2, '   12'
      else
         write(mxls, '(a)' )       '*tim,   (fx2(n),    fy2(n),    fm2(n),    fricx(n),    fricy(n),    fricm(n),  &
                                   fx2(n)+fricx(n),   fy2(n)+fricy(n),  fm2(n)+fricm(n),   shx(n),    shu(n), squat(n), squatbow(n), n=1, nshiptxy ), cfav'
         write(mxls, '(a)' ) 'BL01'
         write(mxls,'(i0, a)') ntbal, '   15'
      endif
   endif

   if (nshiptxy == 1) then
       write(mxls, '(100f18.5)' ) tim,    (fx2(n), fy2(n), fm2(n), fricx(n), fricy(n), fricm(n),  &
                                    fx2(n)+fricx(n),   fy2(n)+fricy(n),  fm2(n)+fricm(n), shx(n), shu(n), squat(n), squatbow(n), n=1, nshiptxy ), cfav
   else

      !if (ncrs.gt.0) then
      !   write(mxls,'(13f14.4)')  ( crs(i)%sumvalcur(1), i=1,min(6,ncrs) )
      !endif
      ue = sqrt(2*9.81)
      Te = 2*400/ue

      !write(mxls,'(13f14.6)') tim/Te ,  ucx(kobs(1)) / ue !   , s1(kobs(1)) - bl(kobs(1))
      if (numobs > 0 .and. kobs(1) > 0 ) then

         k1   = kobs(1)                                 ; k2  = kobs(2)
         ft   = 0.3048 ; dinch = 0.0254d0               ; wid = 6d0*dinch
         h1   = hs(k1)                                  ; h2  = hs(k2)
         AA1  = h1*wid                                  ; AA2 = h2*wid
         QQ   = crs(1)%sumvalcur(1)
         vv1  = sqrt( ucx(k1)**2 + ucy(k1)**2 )         ; vv2 = sqrt( ucx(k2)**2 + ucy(k2)**2 ) ! centre value
         vv1  = QQ/AA1                                  ; vv2 = QQ/AA2                          ! average value
         eh1  = vv1*vv1/(2d0*ag)                        ; ee1 = s1(k1) + eh1
         eh2  = vv2*vv2/(2d0*ag)                        ; ee2 = s1(k2) + eh2
         RR1  = AA1 / (wid + 2*h1)                      ; RR2 = AA2 / (wid + 2*h2)
         xl1  = 1.43d0                                  ; xl2 = 1.59d0
         df1  = xL1*0.01*0.01*vv1*vv1/(RR1**1.333333)   ; df2 = xL2*0.01*0.01*vv2*vv2/(RR2**1.333333)
         hb   = ee1 - ee2 - (df1+df2)
         s12  = s1(k1) - s1(k2)
         Froude2 = vv2/sqrt(ag*h2)
         write(mxls,'(15F8.4)') tim/60d0,  h1, h2, s12, hb, hb/max(eh1,0.001d0), hb/max(eh2,0.001d0), (df1+df2), eh1, QQ/wid, bl(k1) - bl(k2), froude2
      endif
   endif


   end subroutine wrihistek
