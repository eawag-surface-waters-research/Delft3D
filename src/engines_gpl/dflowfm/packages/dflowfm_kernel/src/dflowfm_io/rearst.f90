   !> Reads raw restart data from a formatted restart file by wrirst.
   !! Water levels and velocities are directly stored into the flow arrays.
   SUBROUTINE REARST(Mrst,JA)
   use unstruc_model
   USE UNSTRUC_MESSAGES
   USE M_FLOWTIMES
   USE M_FLOW
   USE M_FLOWGEOM
   implicit none
   INTEGER, intent(inout)  :: Mrst  !< Input file pointer (should already be open)
   integer, intent(out)    :: ja    !< Return status (0 = success)

   integer   :: k
   integer   :: l
   INTEGER   :: NDXR, LNXR       ! alleen binnen deze subroutine
   LOGICAL   :: JAWEL
   DOUBLE PRECISION    :: DUM

   ja = 0
 ! READ(Mrst,*)  REFDATLOC, TSTART_USERLOC, NDXR, LNXR
   READ(Mrst,*)  DUM , DUM,                 NDXR, LNXR

   IF (NDXR .NE. NDX .OR. LNXR .NE. LNX) THEN
       WRITE(MSGBUF, '(A)' ) 'DIMENSIONS ON RESTART FILE NOT EQUAL TO CURRENT MODEL DIMENSIONS'  ; CALL MSG_FLUSH()
       CALL QNERROR        ( 'DIMENSIONS ON RESTART FILE NOT EQUAL TO CURRENT MODEL DIMENSIONS' , ' ', ' ')
       ja = 1
   ENDIF

   READ(Mrst,*)
   DO K = 1,NDX
      READ(Mrst,*, END = 999, ERR = 888) S0(K)
   ENDDO
   S0 = MAX(BL, S0)
   s1 = s0

   READ(Mrst,*)

   DO L = 1,LNX
      READ(Mrst,*, END = 999) U0(L)
   ENDDO
   call doclose(mrst)
   u1 = u0

   RETURN

   888 ja = 1
   return
   999 CALL QNEOFERROR(MRST)
   call doclose(mrst)
   ja = 1

   END SUBROUTINE reaRST
