double precision function setrho(k)
use m_physcoef
use m_flow
use m_sediment
use sediment_basics_module, only: SEDTYP_NONCOHESIVE_TOTALLOAD
use m_transport, only: constituents, itemp, ISED1, ISEDN

implicit none
integer :: k, j, l, lsed
double precision, external :: densfm
double precision           :: rhom, sal, temp

if (jasal > 0) then
   saL = max(0d0, sa1(k))
else
   saL = backgroundsalinity
endif

if (jatem > 0) then
   temp  = max(0d0, constituents(itemp,k))
else
   temp = backgroundwatertemperature
endif

setrho = densfm(sal,temp)

if (jased > 0 .and. stm_included) then
   if (stmpar%morpar%densin) then     ! sediment effects
      l = ISED1
      do lsed = 1,stmpar%lsedtot
         if (stmpar%sedpar%sedtyp(lsed) /= SEDTYP_NONCOHESIVE_TOTALLOAD) then  ! suspended sand or mud
            setrho = setrho + constituents(l,k) - setrho*constituents(l,k)/stmpar%sedpar%rhosol(lsed)
            l = l+1
         end if
      end do
   end if
else if (jaseddenscoupling > 0) then ! jased < 4
   rhom = setrho
   do j = 1,mxgr
      setrho = setrho + sed(j,k)*(rhosed(j) - rhom)/rhosed(j)
   enddo
end if

setrho = min(setrho, 1250d0)           ! check overshoots at thin water layers
setrho = max(setrho,  990d0)           !

end function setrho
