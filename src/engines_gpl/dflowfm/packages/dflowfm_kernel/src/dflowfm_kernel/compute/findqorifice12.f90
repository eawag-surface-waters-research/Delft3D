subroutine findqorifice12(gateheight,crestheight,h1,h2,q,hg,regime,num,qcrit)     ! bepaal q en hg waterstand links = h1, rechts= h2
implicit none
double precision   :: gateheight                   ! gate height above crest
double precision   :: crestheight                  ! crest height above bed
double precision   :: h1                           ! upstream waterheight above crest
double precision   :: q                            ! flux m3/s                                    (out)
double precision   :: h2                           ! pressure height above crest       after gate (out)
double precision   :: hg                           ! vena contracta height above crest after gate (out)
double precision   :: qcrit                        ! critical discharge m2/s                      (out)
character(len=*)   :: regime                       !                                              (out)
double precision   :: g, dh, qmax, hkmin,ha,hb,qa,qb,ha0,qa0,hb0,qb0,qc,hc,a,d, qda, qdb, qdc, hga, hgb, hgc
integer            :: num, k, kk, nummin
double precision   :: coeffs(5), ccx(4), cc, alfa = 0.02d0, qf,hgf,h2f,qer,qermin
double precision   :: aa, bb
g  = 9.81                                          ! h1 = waterhoogte bovenstrooms
h2 = min(h2,h1-0.0001)                             ! hg = gateheight * contractie = effectieve keeldoorsnee
d  = crestheight
a  = gateheight
h1 = max(h1,   0.00010)
h2 = max(h2,   0.00001)

hg     = gateheight*0.5d0                          ! lower boundary
hg     = max(hg ,   0.0001)

if (gateheight >= h1) then                         ! gate above water
   q      =  11111d0
   regime = 'gate above water'
   return
else if (gateheight < 0.001) then
   q      =  0d0
   regime = 'gate closed, a<0.001 '
   return
endif

qcrit = sqrt( 2d0*g*(h1-hg) / (hg**(-2)-h1**(-2)) )

ha = hg ; hb = h2
call qorifdif12(ha,d,a,h1,h2,qda)
call qorifdif12(hb,d,a,h1,h2,qdb)

num = 0 ; qdc = 1d9
do while ( abs(qdc) > 1d-6 .and. abs(qda-qdb) > 1d-6 .and. num < 50 )

   num = num + 1

   hc  = ha - qda*(ha-hb)/(qda-qdb)      ! regula falsi
   hc  = max(hc,hg)
   hc  = min(hc,h2)
   call qorifdif12(hc,d,a,h1,h2,qdc)
   if (qda*qdc > 0) then
      ha = hc ; qda = qdc
   else if (qdb*qdc > 0) then
      hb = hc ; qdb = qdc
   endif

enddo

hg = hc
call getq3(hg,d,a,h1,h2,q)

return

do k = 1, 10

   a = 0.1d0*dble(k)*h1

   aa =  2d0*(h1-a)
   bb = -2d0*h1**2
   cc =  a*h1*h1

   hgb = (-bb + sqrt(bb*bb -4d0*aa*cc))/ (2d0*aa)
   hgc = (-bb - sqrt(bb*bb -4d0*aa*cc))/ (2d0*aa)

   hgb = hgb / h1
   hgc = hgc / h1

enddo

end subroutine findqorifice12
