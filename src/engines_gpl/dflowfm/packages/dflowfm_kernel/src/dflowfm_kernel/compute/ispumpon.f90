 integer function ispumpon(n,s1k)
 use m_flowexternalforcings
 use m_missing
 use m_structures

 integer, intent(in) :: n
 double precision, intent(in) :: s1k
 ! this is for safety, check arrays before dereference
 if (.not.allocated(pumponoff)) then
     ispumpon = 1
     return
 endif

 if (pumponoff(1,n) == dmiss .and. pumponoff(2,n) == dmiss) then
    ispumpon = 1 ; return
 endif
 if (pumponoff(1,n) .ne. dmiss .and. s1k > pumponoff(1,n) ) then
     pumponoff(5,n) = 1
 endif
 if (pumponoff(2,n) .ne. dmiss .and. s1k < pumponoff(2,n) ) then
     pumponoff(5,n) = 0
 endif
 ispumpon = int(pumponoff(5,n)) ! no change
end function ispumpon
