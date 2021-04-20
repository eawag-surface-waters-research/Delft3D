      SUBROUTINE WRINET(MNET)
      use m_netw
      implicit none
      integer :: MNET

      integer :: i
      integer :: j
      integer :: k
      integer :: l
      integer :: lcdum

      write(mNET,'(A,I12)') 'NR of NETNODES  = ', numk    ! nump = ndx
      write(mNET,'(A,I12)') 'NR of NETLINKS  = ', numL    ! nump = ndx
      WRITE(MNET,'(A)') 'NODE LIST, X, Y COORDINATES'

      DO K = 1, NUMK
        WRITE(MNET,'(3F26.15)') XK(K), YK(K), ZK(K)
      ENDDO

      WRITE(MNET,'(A)') 'LINK LIST, LEFT AND RIGHT NODE NRS'

      LCDUM = 1
      DO L = 1, NUML
         WRITE(MNET,'(3I16)')  KN(1,L), KN(2,L), KN(3,L)
      ENDDO

      CALL DOCLOSE(MNET)
      RETURN
      END SUBROUTINE WRINET
