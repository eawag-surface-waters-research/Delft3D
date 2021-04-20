!> write matlab integer array to file
   subroutine matlab_write_int(matfile, varname, var, Ni, Nj)

      implicit none

      integer                             :: matfile      !< matlab file unit number
      character(len=*)                    :: varname      !< variable name
      integer                             :: Ni, Nj       !< array sizes
      integer,          dimension(Ni, Nj) :: var          !< variable

      integer i, j

      write(matfile, *) trim(varname), ' = ['
      do i=1,Ni
         do j=1,Nj
            if ( var(i,j).ne.-1234 ) then
               write(matfile, "(I20, $)") var(i,j)
            else
               write(matfile, "(' NaN', $)")
            end if
         end do
         write(matfile, *)
      end do
      write(matfile, "('];')")

   end subroutine matlab_write_int
