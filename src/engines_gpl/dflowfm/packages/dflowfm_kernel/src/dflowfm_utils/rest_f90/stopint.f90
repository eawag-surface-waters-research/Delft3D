      SUBROUTINE STOPINT()
      use unstruc_files
      use unstruc_netcdf, only: unc_closeall
      use m_partitioninfo
      implicit none
      CALL ISCREENCLOSE()
      call unc_closeall()
      call close_all_files()

      if ( jampi.eq.1 ) then
!        finalize before exit
         call partition_finalize()
      end if


!     SPvdP: close dia-file
      if ( mdia.ne.0 .and. mdia.lt.maxnum ) then
         close(mdia)
         mdia = 0
      end if

      STOP
      END
