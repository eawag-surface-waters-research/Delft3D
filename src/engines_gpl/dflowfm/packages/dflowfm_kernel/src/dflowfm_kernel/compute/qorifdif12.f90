subroutine qorifdif12(hg,d,a,h1,h2,qd)
implicit none
double precision :: hg,d,a,h1,h2,qd
double precision :: ql, qr

call getq1(hg,d,h1,h2,ql)
call getq3(hg,d,a,h1,h2,qr)
qd = ql-qr
end
