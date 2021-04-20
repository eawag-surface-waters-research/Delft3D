subroutine reallocsrc(n)
 use m_transport, only: NUMCONST, ISALT, ITEMP, ISED1, ITRA1, itrac2const
 use m_flowexternalforcings
 use m_alloc
 use m_missing
 use m_polygon, only: npl
 implicit none
 integer :: n

 msrc = max(msrc, npl)
 call realloc (ksrc , (/ 6,n /), keepexisting=.true., fill=0 )
 call realloc (qsrc , n,         keepExisting = .true., fill=0d0)
 call realloc (tmsrc, n,         keepExisting = .true., fill=0d0)
 call realloc (sasrc, n,         keepExisting = .true., fill=0d0)
 call realloc (CCsrc,  (/ NUMCONST,n /), keepExisting = .true., fill=0d0)
 call realloc (arsrc, n,         keepExisting = .true., fill=0d0)
 call realloc (cssrc, (/ 2,n /), keepExisting = .true.)
 call realloc (snsrc, (/ 2,n /), keepExisting = .true.)
 call realloc (zsrc , (/ 2,n /), keepExisting = .true.)
 call realloc (zsrc2, (/ 2,n /), keepExisting = .true.) ; zsrc2 = dmiss
 ! call realloc (srsn , (/ 6,n /), keepExisting = .true.)
 call realloc (srsn , (/ 2*(NUMCONST+1),n /), keepExisting = .true.)
 call realloc (jamess, n,        keepExisting = .true.)
 call realloc (kdss , 3*n,       keepExisting = .true., fill=1)
 ! call realloc (qstss, 3*n,       keepExisting = .true., fill=0d0)
 call realloc (qstss, (NUMCONST+1)*n, keepExisting = .true., fill=0d0)
 call realloc (srcname, n,       keepExisting = .true., fill=' ')
 call realloc (xsrc , (/n, msrc/),keepExisting = .true., fill=dmiss)
 call realloc (ysrc , (/n, msrc/),keepExisting = .true., fill=dmiss)
 call realloc (nxsrc, n,          keepExisting = .true., fill=0)
 end subroutine reallocsrc
