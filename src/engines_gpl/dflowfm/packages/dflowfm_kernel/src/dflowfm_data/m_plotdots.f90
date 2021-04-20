module m_plotdots
   implicit none
   integer                                     :: numdots      ! number of dots
   integer                                     :: NSIZE=0      ! array size
   double precision, dimension(:), allocatable :: xdots, ydots ! dot coordinates, dim(NSIZE)
   double precision, dimension(:), allocatable :: zdots        ! dot z-value
   integer,          dimension(:), allocatable :: colnumber    ! colour number
   double precision,               parameter   :: ZDOTDEFAULT = 0d0

   contains

   subroutine reallocdots(N)
      use m_alloc
      use m_missing
      implicit none

      integer, intent(in) :: N

      if ( N.gt.NSIZE) then
         NSIZE = 1+int(1.2d0*dble(N))

         call realloc(xdots, NSIZE, keepExisting=.true., fill=DMISS)
         call realloc(ydots, NSIZE, keepExisting=.true., fill=DMISS)
         call realloc(zdots, NSIZE, keepExisting=.true., fill=ZDOTDEFAULT)
         call realloc(colnumber, NSIZE, keepExisting=.true., fill=imiss)
      end if

      return
   end subroutine reallocdots

!> add a dot
   subroutine adddot(x,y,z, colournumber)
      implicit none

      double precision,           intent(in) :: x, y
      double precision, optional, intent(in) :: z
      integer,          optional, intent(in) :: colournumber
      double precision :: zloc

      zloc = ZDOTDEFAULT
      if ( present(z) ) then
         zloc = z
      end if

      numdots = numdots+1
      call reallocdots(numdots)
      xdots(numdots) = x
      ydots(numdots) = y
      zdots(numdots) = zloc
      if (present(colournumber)) then
         colnumber(numdots) = colournumber
      else
         colnumber(numdots) = 31 ! ncolhl
      endif

      return
   end subroutine adddot

!  write dots to sample file
   subroutine write_dots(FNAM, jawritten)
      implicit none

      character(len=*), intent(in)  :: FNAM
      integer,          intent(out) :: jawritten

      integer                       :: i, id

      jawritten=0

      if ( numdots.lt.1 ) goto 1234

      call newfil(id,FNAM)

      do i=1,numdots
         write(id, "(3E15.5)") xdots(i), ydots(i), zdots(i)
      end do

      call doclose(id)

      jawritten=1

 1234 continue

      return
   end subroutine

   subroutine deldots()
      implicit none

      Numdots = 0

      return
   end subroutine

end module m_plotdots
