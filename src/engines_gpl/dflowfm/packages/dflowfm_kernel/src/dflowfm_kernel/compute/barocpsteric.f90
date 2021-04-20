 double precision function barocpsteric(kb)
 use m_flowgeom
 use m_flow

 integer          :: kb
 integer          :: kk
 double precision, external :: densfm
 double precision           :: rhokk

 barocpsteric = 0d0
 do kk = ktop(kb), kbot(kb), -1
    rhokk        = densfm(steric(1,kk), steric(2,kk))
    rhokk        = rhokk - rhosteric
    barocpsteric = barocpsteric + ag*rhokk*(zws(kk) - zws(kk-1))
 enddo
end function barocpsteric
