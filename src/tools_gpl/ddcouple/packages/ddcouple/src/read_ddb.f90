      subroutine read_ddb(hyd)

      ! function : read the ddb file from the overall hydrodynamics

      ! (c) Deltares

      ! global declarations

      use hydmod
      implicit none

      ! declaration of the arguments

      type(t_hyd)         :: hyd                    ! description of the hydrodynamics

      ! local declarations

      integer             :: lunrep                 ! report file
      integer             :: i_domain               ! index in collection
      type(t_domain)      :: domain                 ! one domain description
      integer             :: i_dd_bound             ! index in collection
      type(t_dd_bound)    :: dd_bound               ! one dd_bound description
      character(len=255)  :: line                   ! line buffer input file
      character(len=255)  :: ctoken                 ! line buffer input file
      character(len=255)  :: filext                 ! file extension
      integer             :: extpos                 ! position file extension
      integer             :: extlen                 ! length file extension
      integer             :: i_swap                 ! swap help variable
      integer             :: ierr                   ! error indicator
      logical             :: token_used             ! token_used
      type(inputfilestack):: inpfil                 ! input file strucure with include stack

      call getmlu(lunrep)

      inpfil%inputf = 0
      call filestack_add(inpfil,hyd%file_com%name,ierr)
      if ( ierr .ne. 0 ) then
         write(lunrep,*) ' error opening ddbound file'
         write(lunrep,*) ' file: ',trim(hyd%file_com%name)
      endif
      inpfil%cchar  = '#'

      hyd%domain_coll%cursize = 0
      hyd%domain_coll%maxsize = 0
      hyd%dd_bound_coll%cursize = 0
      hyd%dd_bound_coll%maxsize = 0

      ! loop over all the tokens in the file

      do

         ! read first domain

         call dlwq_read_token( inpfil, dd_bound%name1, ierr)

         ! if end of file the exit loop

         if ( ierr .ne. 0 ) exit

         ! read m_begin1, n_begin1, m_end1, n_end1, domain name 2, m_begin2, n_begin2, m_end2, n_end2

         call dlwq_read_token( inpfil, dd_bound%m_begin1, ierr) ; if ( ierr .ne. 0 ) goto 900
         call dlwq_read_token( inpfil, dd_bound%n_begin1, ierr) ; if ( ierr .ne. 0 ) goto 900
         call dlwq_read_token( inpfil, dd_bound%m_end1, ierr)   ; if ( ierr .ne. 0 ) goto 900
         call dlwq_read_token( inpfil, dd_bound%n_end1, ierr)   ; if ( ierr .ne. 0 ) goto 900

         call dlwq_read_token( inpfil, dd_bound%name2, ierr)    ; if ( ierr .ne. 0 ) goto 900
         call dlwq_read_token( inpfil, dd_bound%m_begin2, ierr) ; if ( ierr .ne. 0 ) goto 900
         call dlwq_read_token( inpfil, dd_bound%n_begin2, ierr) ; if ( ierr .ne. 0 ) goto 900
         call dlwq_read_token( inpfil, dd_bound%m_end2, ierr)   ; if ( ierr .ne. 0 ) goto 900
         call dlwq_read_token( inpfil, dd_bound%n_end2, ierr)   ; if ( ierr .ne. 0 ) goto 900

         ! fuzzy: get rid of extension of domain name

         call dhfext(dd_bound%name1,filext, extpos, extlen)
         if (extpos.gt.1) dd_bound%name1(extpos:) = ' '
         call dhfext(dd_bound%name2,filext, extpos, extlen)
         if (extpos.gt.1) dd_bound%name2(extpos:) = ' '

         ! make sure the numbering is always increasing (postpone till overall hyd file is written?)

         if ( dd_bound%m_begin1 .gt. dd_bound%m_end1 ) then
            i_swap            = dd_bound%m_begin1
            dd_bound%m_begin1 = dd_bound%m_end1
            dd_bound%m_end1   = i_swap
         endif
         if ( dd_bound%n_begin1 .gt. dd_bound%n_end1 ) then
            i_swap            = dd_bound%n_begin1
            dd_bound%n_begin1 = dd_bound%n_end1
            dd_bound%n_end1   = i_swap
         endif
         if ( dd_bound%m_begin2 .gt. dd_bound%m_end2 ) then
            i_swap            = dd_bound%m_begin2
            dd_bound%m_begin2 = dd_bound%m_end2
            dd_bound%m_end2   = i_swap
         endif
         if ( dd_bound%n_begin2 .gt. dd_bound%n_end2 ) then
            i_swap            = dd_bound%n_begin2
            dd_bound%n_begin2 = dd_bound%n_end2
            dd_bound%n_end2   = i_swap
         endif

         ! add to dd_bound collection

         i_dd_bound = dd_bound_coll_add(hyd%dd_bound_coll, dd_bound)

      enddo

      return
 900  call dherrs('error reading dbb file, last line:'//trim(inpfil%linbuf(inpfil%inputf)),1)
      end subroutine read_ddb
