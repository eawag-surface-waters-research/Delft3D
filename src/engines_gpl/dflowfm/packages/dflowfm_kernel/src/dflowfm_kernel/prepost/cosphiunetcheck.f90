subroutine cosphiunetcheck(jausererror)
 use m_flowgeom
 use network_data
 use m_alloc
 use unstruc_messages
 use unstruc_display, only: jaGUI
 use m_missing
 use m_partitioninfo
 use m_plotdots

 implicit none
 integer, intent(in) :: jausererror !< Whether or not (1/0) to topup a error message when bad ortho occurs.
 double precision, external :: cosphiunet
 double precision :: csph
 integer :: ndraw, L
 integer :: k1, k2
 integer :: i
 COMMON /DRAWTHIS/ ndraw(50)

 nlinkbadortho = 0
 nlinktoosmall = 0
 call realloc(linkbadqual, 1000)

 ! No checks if no cells are known yet.
 if (nump <= 0) then
    return
 end if

 if ( jampi == 0 ) then
    do L = numl1D+1,numl
       csph = cosphiunet(L)
       if (csph /= dmiss .and. abs(csph) > cosphiutrsh) then
          nlinkbadortho = nlinkbadortho+1
           linkbadqual(nlinkbadortho) = L
       endif
       if (nlinkbadortho >= 1000) exit
    enddo
 else

 !  do not check orthogonality in parallel runs (findcells may have created non-existing cells in ghost area)
 !  the check is done during partitioning

 end if

 if (nlinkbadortho > 0) then
     numdots = 0
     do i=1,nlinkbadortho
        L = linkbadqual(i)
        call adddot(0.5d0*(xk(kn(1,L)) + xk(kn(2,L))), 0.5d0*(yk(kn(1,L)) + yk(kn(2,L))))
     end do

     if (jausererror == 1) then
        if ( jagui == 1 ) then
            call qnerror('network is not orthogonal','increase cosphiu trsh in network params if you want to create flow model anyway ', ' ')
        else
            call mess(LEVEL_ERROR, 'network is not orthogonal')
        end if
     end if
     NDRAW(2)=5 !< Automatically set 'Display > Network + crossing/quality checks'
     call resetflow()
 end if

end subroutine cosphiunetcheck
