 subroutine checknans()
 use m_flowgeom
 use m_flow
 use m_reduce
 implicit none

 call newfil(mdump     , 'dump')

 call chknan(s0        , 's0       ', ndx)
 call chknan(s1        , 's1       ', ndx)
 call chknan(bbr       , 'bbr      ', ndx)
 call chknan(ccr       , 'ccr      ', ndx)
 call chknan(ddr       , 'ddr      ', ndx)
 call chknan(bb        , 'bb       ', ndx)
 call chknan(dd        , 'dd       ', ndx)
 call chknan(vol0      , 'vol0     ', ndx)
 call chknan(vol1      , 'vol1     ', ndx)
 call chknan(vol1_f    , 'vol1_f   ', ndx)
 call chknan(au        , 'au       ', ndx)
 call chknan(ba        , 'ba       ', ndx)
 call chknan(a1        , 'a1       ', ndx)
 call chknan(hu        , 'hu       ', ndx)
 call chknan(u0        , 'u0       ', ndx)
 call chknan(u1        , 'u1       ', ndx)

 call doclose(mdump)



 end subroutine checknans
