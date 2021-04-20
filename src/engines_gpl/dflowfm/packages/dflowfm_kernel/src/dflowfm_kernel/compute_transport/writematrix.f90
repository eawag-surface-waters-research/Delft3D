   !> output matrix in CRS format to file
   subroutine writeMatrix(FNAM, N, ia, ja, a, VARNAM, jaappend)
      implicit none

      character(len=*),                       intent(in) :: FNAM         !< filename

      integer,                                intent(in) :: N            !< number of rows

      integer,          dimension(N+1),       intent(in) :: ia
      integer,          dimension(ia(N+1)-1), intent(in) :: ja
      double precision, dimension(ia(N+1)-1), intent(in) :: a            !< matrix in CRS format

      character(len=*),                       intent(in) :: VARNAM       !< variable name

      integer,                                intent(in) :: jaappend     !< append (1), or not (0)

      logical                                            :: Lexist

      integer                                            :: irow, icol, j, lunfil

      inquire(file=trim(FNAM), exist=Lexist)
      if ( jaappend.eq.0 .or. .not.Lexist ) then
         open(newunit=lunfil,file=trim(FNAM))
      else
         open(newunit=lunfil,file=trim(FNAM), access="append")
      end if


      write(lunfil,"('dum = [')")
      do irow=1,N
         do j=ia(irow),ia(irow+1)-1
            icol = ja(j)
            write(lunfil,"(2I7,E20.10)") irow, icol, a(j)
         end do
      end do
      write(lunfil,"('];')")
      write(lunfil,"(A16, '=sparse(dum(:,1), dum(:,2), dum(:,3));')") VARNAM

!     write rhs
!      write(lunfil,"('rhs = [')")
!      do irow=1,solver%numrows
!         write(lunfil,"(E15.5)") solver%rhs(irow)
!      end do
!      write(lunfil,"('];')")

      close(lunfil)
   end subroutine writeMatrix
