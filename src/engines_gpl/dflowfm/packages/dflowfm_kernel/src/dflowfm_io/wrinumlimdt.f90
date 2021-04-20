 subroutine wrinumlimdt()
 use m_flowgeom, only : ndx, xz, yz
 use m_flow, only : numlimdt
 use unstruc_model, only : md_ident, getoutputdir
 implicit none
 integer :: mlim, k

 call newfil(mlim, trim(getoutputdir()) // trim(md_ident) // '_numlimdt.xyz')
 do k = 1, ndx
    if (numlimdt(k) > 0) then
       write(mlim, *) xz(k), yz(k), numlimdt(k)
    endif
 enddo
 call doclose(mlim)

 end subroutine wrinumlimdt
