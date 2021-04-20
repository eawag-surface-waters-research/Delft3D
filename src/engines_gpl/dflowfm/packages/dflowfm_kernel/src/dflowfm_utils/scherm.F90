  SUBROUTINE SCHERM()
   use m_netw
   use m_flowgeom
   use m_grid
   use unstruc_messages
   implicit none
   integer :: i
   integer :: omp_numt

   integer :: maxlin
   PARAMETER (MAXLIN = 11)
   integer :: nlevel
   COMMON /HELPNOW/  WRDKEY,NLEVEL
   CHARACTER TEX(MAXLIN)*70,WRDKEY*40
   integer, external ::omp_get_num_threads
!

   TEX(1) = 'ACTUAL AND MAXIMUM DIMENSIONS OF DATA                           '
   TEX(2) = '****************************************************************'
   TEX(3) = 'DATA TYPE                            :       ACTUAL      MAXIMUM'
   TEX(4) = 'NUMBER OF NETNODES                   :                          '
   TEX(5) = 'NUMBER OF NETLINKS                   :                          '
   TEX(6) = 'MAXIMUM NUMBER OF LINKS PER NODE     :                          '
   TEX(7) = 'land boundary                        :                          '
   TEX(8) = 'POLYGON                              :                          '
   TEX(9) = 'NUMBER OF FLOW CELLS                 :                          '
   TEX(10)= 'NUMBER OF FLOW LINKS                 :                          '
   TEX(11)= 'Grid m,n dimensions                  :                          '


!
   WRITE(TEX(4)(44:51),'(I8)')  NUMK
   WRITE(TEX(5)(44:51),'(I8)')  NUML
!  WRITE(TEX(6)(44:51),'(I8)')
   WRITE(TEX(7)(44:51),'(I8)')  MXLAN
   WRITE(TEX(8)(44:51),'(I8)')  NPL
   WRITE(TEX(9) (44:51),'(I8)')  NDX
   WRITE(TEX(10)(44:51),'(I8)')  LNX
   WRITE(TEX(11)(44:51),'(I8)')  mc

   WRITE(TEX(4)(57:64),'(I8)')  KMAX
   WRITE(TEX(5)(57:64),'(I8)')  LMAX
   WRITE(TEX(6)(57:64),'(I8)')  KNX
   WRITE(TEX(7)(57:64),'(I8)')  MAXLAN
   WRITE(TEX(8)(57:64),'(I8)')  MAXPOL
   WRITE(TEX(9) (57:64),'(I8)')  NDX
   WRITE(TEX(10)(57:64),'(I8)')  LNX
   WRITE(TEX(11)(57:64),'(I8)')  nc


!
   WRITE(msgbuf,'(A)'); call msg_flush()

   DO I = 1,MAXLIN
      WRITE(msgbuf,'(A)') TEX(I); call msg_flush()
   ENDDO


   omp_numt = 0
#ifdef _OPENMP
   WRITE(msgbuf,'(A,i8)') 'number of threads: ', omp_get_num_threads()  ; call msg_flush()
#else
   WRITE(msgbuf,'(A,i8)') 'number of threads: OMP disabled'  ; call msg_flush()
#endif


   WRDKEY = 'ACTUAL AND MAXIMUM DIMENSIONS OF DATA'
   NLEVEL = 2
   CALL HISTOR()

   RETURN
   END SUBROUTINE SCHERM
