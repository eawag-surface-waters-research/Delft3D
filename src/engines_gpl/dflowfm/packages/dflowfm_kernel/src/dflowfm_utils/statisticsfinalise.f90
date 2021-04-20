 subroutine statisticsfinalise()
 use m_statistics
 implicit none
 if (numdif .ne. 0) then
    avedif    = avedif/numdif
    cumavedif = cumavedif + avedif
    rmsdif    = sqrt( sqadif / numdif )
    cumrmsdif = cumrmsdif + rmsdif
    dmxdif    = max(cumdmxdif, dmxdif)
    numcum    = numcum + 1
 endif
 end subroutine statisticsfinalise
