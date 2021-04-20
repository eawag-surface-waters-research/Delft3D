  SUBROUTINE CHANGENUMERICALPARAMETERS2()
   use m_netw
   USE M_FLOW
   use m_flowgeom
   USE m_sferic
   use m_wind
   use unstruc_display
   use m_reduce
   use unstruc_version_module, only : unstruc_company, unstruc_program
   use unstruc_messages
   use m_fixedweirs
   implicit none

   integer :: numpar, numfld, numparactual, numfldactual
   PARAMETER  (NUMPAR = 19, NUMFLD = 2*NUMPAR)
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   integer :: nlevel
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line
!
   integer :: ir, il, iw, ixp, iyp, ih, i, iuvfieldorg, ifexit, ifinit, key, ja, niadvec
   integer :: nbut, imp, inp

   NLEVEL     = 4
   OPTION( 1) = 'ITURBULENCEMODEL                     ( )' ; it(2* 1) = 2
   OPTION( 2) = 'JAUSTARINT                           ( )' ; it(2* 2) = 2
   OPTION( 3) = 'jabaroctimeint                          ' ; it(2* 3) = 2
   OPTION( 4) = 'JAVAKEPS                                ' ; it(2* 4) = 2
   OPTION( 5) = 'IDENSFORM                               ' ; it(2* 5) = 2
   OPTION( 6) = 'JARHOXU                                 ' ; it(2* 6) = 2
   OPTION( 7) = 'JAVASAL                                 ' ; it(2* 7) = 2
   OPTION( 8) = 'IFIXEDWEIRSCHEME                        ' ; it(2* 8) = 2
   OPTION( 9) = 'Tsigma                                  ' ; it(2* 9) = 6
   OPTION(10) = 'Local timestepping in transport(1)      ' ; it(2*10) = 2
   OPTION(11) = 'Cffacver                                ' ; it(2*11) = 6
   OPTION(12) = 'Javatem                                 ' ; it(2*12) = 2
   OPTION(13) = 'Javiuplus3D                             ' ; it(2*13) = 2
   OPTION(14) = 'Jaqaisq1                                ' ; it(2*14) = 2
   OPTION(15) = 'Addksources                             ' ; it(2*15) = 6
   OPTION(16) = 'Initialise rho, if not, first barocl = 0' ; it(2*16) = 2
   OPTION(17) = 'jaLogprofatubndin                       ' ; it(2*17) = 2
   OPTION(18) = 'javau                                   ' ; it(2*18) = 2
   OPTION(19) = 'jacomp                                  ' ; it(2*19) = 2

!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

   HELPM ( 1) = '0=no, 1 = constant, 2 = algebraic, 3 = k-eps, 4 = k-tau     '
   HELPM ( 2) = '0123                                                        '
   HELPM ( 3) = '1 = expl, -2; abashford, -3 = ab3, -5 = adv rho             '
   HELPM ( 4) = '0 = NO, 3 = VERT IMPL, HOR EXPL                             '
   HELPM ( 5) = '0 = no, 1 = eckart                                          '
   HELPM ( 6) = '0 = no, 1 = YES                                             '
   HELPM ( 7) = '0=No, 1=Upwe, 2=Cente, 3=Upwi, 4=Centi, 5=4,3, 6=MCexpl     '
   HELPM ( 8) = '0=No, 6=subgrid, 7=rajaratnam, 8=Tabelb, 9=Willemontenotyet '
   HELPM ( 9) = 'Sigma adaptation timescale, only for layertype == 4         '
   HELPM (10) = '1 = yes, 0 = no                                             '
   HELPM (11) = '0=never switch off ho term vertical                         '
   HELPM (12) = '0=No, 1=Upwe, 2=Cente, 3=Upwi, 4=Centi, 5=4,3, 6=MCexpl     '
   HELPM (13) = '0=no, 1 = yes                                               '
   HELPM (14) = '0=no, 1 = yes                                               '
   HELPM (15) = '0=no, 1 = yes                                               '
   HELPM (16) = '0=no, 1 = yes                                               '
   HELPM (17) = 'at ubnd in: 0 = uniform U1, 1 = log U1, 2 = also k-eps      '
   HELPM (18) = '0=no, 3 = impli upw, 5 = Quickest                           '
   HELPM (19) = '0=standard, 1 = use csu snu in weights, 2 = scalarx,y banf  '


   CALL SAVEKEYS()
   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      IX(IL) = 13
      IX(IR) = 95
      IY(IL) = 2*I
      IY(IR) = 2*I
      IS(IL) = 82
      IS(IR) = 10
      IT(IL) = 1001
   ENDDO

!  Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW     = NPOS(3)
   IXP    = NPOS(1) + (IWS-IW)/2
   IYP    = NPOS(2)
   IH     = IHS - 9

!  Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program) // ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!  Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = ., Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

!  Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)

   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

!  NUMWNH = InfoWindow(1)
!  CALL IWinSelect(NUMWNH)

!  Define a new form by supplying arrays containing
!  field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

!  Define a help field and define help strings
!  for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   CALL IFormPutINTEGER (2* 1 ,ITURBULENCEMODEL )
   CALL IFORMPUTINTEGER (2* 2 ,JAUSTARINT       )
   CALL IFORMPUTINTEGER (2* 3 ,jabaroctimeint   )
   CALL IFORMPUTINTEGER (2* 4 ,JAVAKEPS         )
   CALL IFORMPUTINTEGER (2* 5 ,IDENSFORM        )
   CALL IFORMPUTINTEGER (2* 6 ,JARHOXU          )
   CALL IFORMPUTINTEGER (2* 7 ,JAVASAL          )
   CALL IFORMPUTINTEGER (2* 8 ,ifixedweirscheme )
   CALL IFORMPUTdouble  (2* 9 ,Tsigma           , '(F7.3)' )
   CALL IFORMPUTINTEGER (2*10 ,JALTS            )
   CALL IFORMPUTdouble  (2*11 ,Cffacver         , '(F7.3)' )
   CALL IFORMPUTINTEGER (2*12 ,JAVATEM          )
   CALL IFORMputINTEGER (2*13 ,javiuplus3D      )
   CALL IFORMputINTEGER (2*14 ,jaqaisq1         )
   CALL IFORMputdouble  (2*15 ,addksources      , '(F7.3)' )
   CALL IFORMputINTEGER (2*16 ,jainirho         )
   CALL IFORMputINTEGER (2*17 ,jaLogprofatubndin)
   CALL IFORMputINTEGER (2*18 ,javau)
   CALL IFORMputINTEGER (2*19 ,jacomp)


   !  Display the form with numeric fields left justified
   !  and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!  check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.  &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN

          CALL IFORMGETINTEGER (2* 1 ,ITURBULENCEMODEL )
          CALL IFORMGETINTEGER (2* 2 ,JAUSTARINT       )
          CALL IFORMGETINTEGER (2* 3 ,jabaroctimeint   )
          CALL IFORMGETINTEGER (2* 4 ,JAVAKEPS         )
          CALL IFORMGETINTEGER (2* 5 ,IDENSFORM        )
          CALL IFORMGETINTEGER (2* 6 ,JARHOXU          )
          CALL IFORMGETINTEGER (2* 7 ,JAVASAL          )
          CALL IFORMGETINTEGER (2* 8 ,IFIXEDWEIRSCHEME )
          CALL IFORMGETdouble  (2* 9 ,Tsigma           )
          CALL IFORMGETINTEGER (2*10 ,JALTS            )
          CALL IFORMGETdouble  (2*11 ,Cffacver         )
          CALL IFORMGETINTEGER (2*12 ,JAVATEM          )
          CALL IFORMGETINTEGER (2*13 ,javiuplus3D      )
          CALL IFORMGETINTEGER (2*14 ,jaqaisq1         )
          CALL IFORMGETdouble  (2*15 ,addksources      )
          CALL IFORMGETINTEGER (2*16 ,jainirho         )
          CALL IFORMGETINTEGER (2*17 ,jaLogprofatubndin)
          CALL IFORMGETINTEGER (2*18 ,javau)
          CALL IFORMGETINTEGER (2*19 ,jacomp)


       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL RESTOREKEYS()
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

   END SUBROUTINE CHANGENUMERICALPARAMETERS2
