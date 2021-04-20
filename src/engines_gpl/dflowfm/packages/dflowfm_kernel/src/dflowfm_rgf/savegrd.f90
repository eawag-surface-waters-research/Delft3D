     subroutine savegrd()
        use m_grid
        use m_missing
        use m_alloc

        implicit none

        integer, dimension(2) :: ibounds
        ! Possibly resize the help grid if the actual grid is larger.
        ibounds = ubound(xc)
        call realloc(xch, ibounds, fill=dmiss)
        ibounds = ubound(yc)
        call realloc(ych, ibounds, fill=dmiss)
        ibounds = ubound(zc)
        call realloc(zch, ibounds, fill=dmiss)
        xch = xc
        ych = yc
        zch = zc
        mch = mc
        nch = nc
     end subroutine savegrd
