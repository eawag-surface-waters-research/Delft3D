subroutine qorifdif(hg,d,h1,h3,h2,qd)
implicit none
double precision :: hg, d, h1, h3, h2, qd
double precision :: ql, qr

call getq1(hg,d,h1,h2,ql)
call getq2(hg,d,h2,h3,qr)
qd = ql-qr
end
