!
 Subroutine plusabs_flow(numchoice)
 use m_flow
 use m_flowgeom
 implicit none

 integer :: numchoice, k, kk, kb, kt

 ! locals
 integer :: key

 if (ndx == 0 .or. lnx == 0) then
    call qnerror('First reinitialise flow model, current dimensions are 0',' ',' ')
    return
 endif

 if (numchoice == 1) then
    call plusabsd(xz,yz,yz,ndx,key,s1); s1=max(s1,bl)
 else if (numchoice == 2) then
    if (.not. allocated (sa1) ) then
       CALL qnerror('first reinitialise with jasal=1',' ',' ')
       return
    endif
    call plusabsd(xz,yz,yz,ndx,key,sa1)
    if (kmx > 0) then
       do kk = 1,ndx
          call getkbotktop(kk,kb,kt)
          do k = kb,kt
             sa1(k) = sa1(kk)
          enddo
       enddo
    endif

    salmax = maxval(sa1)

 else if (numchoice == 3) then
    if (ibedlevtyp == 1) then
       call plusabsd(xz,yz,yz,ndx,key,bl)
    else if (ibedlevtyp == 2) then
       call plusabsd(xu,yu,yu,lnx,key,blu)
    else
       CALL qnerror('Specifying cell bottom levels bl (ibedlevtyp=1) or flow link bottom levels blu (ibedlevtyp=2)',' ',' ')
       CALL qnerror('Change parameter ibedlevtyp in Various, Change Geometry Parameters',' ',' ')
       return
    endif
    call setbobs()
    s1 = max(s1,bl)
 endif
 End subroutine plusabs_flow
