         IDENTIFICATION DIVISION.
         PROGRAM-ID.    VDCACFRA.
         SECURITY.
      ******************************************************************
      * ALTERA COMPONENTES EXTRAS FRETE                                *
      ******************************************************************
      *--------------  O B S E R V A C O E S  -------------------------*
      * -                                                              *
      *--------------  A L T E R A C O E S  ---------------------------*   VERSAO
      * 01/08/2016 REFERENTE A O.S. 6494                         SIDNEI*16.10.001
      * 07/10/2016 REFERENTE A O.S. XXXX                          KENDY*16.10.002
      *          - Recompilado, pois na SL VDCADCFR estava com uma     *
      *            chave alternada duplicada, se nao iria dar FS "39"  *
      *================================================================*
         ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
         SPECIAL-NAMES.
         COPY "J:\PRGNEW\GE\BOK\GESPECI.ENV".
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.

         COPY "J:\PRGNEW\VD\BOK\VDCADCFR.ESDA"
              REPLACING =='VDCADCFR'== BY ==VDCADCFR==.

      *================================================================*
      /
      *================================================================*
         DATA DIVISION.
         FILE SECTION.

         COPY "J:\PRGNEW\VD\BOK\VDCADCFR.EFDE"
              REPLACING =='VDCADCFR'== BY ==VDCADCFR==.

      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         WORKING-STORAGE SECTION.
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         77 WTPGM-HEADER        PIC  X(019) VALUE "=>CONTROLE VERSAO<=".
         77 WTPGM-VERSAO        PIC  X(009) VALUE "16.10.002".
         78 WX-SEPARA           VALUE x"0A".
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         COPY "J:\PRGNEW\GE\BOK\GEDIR.WOR".
         COPY "J:\PRGNEW\VD\BOK\VDDIR.WOR".
         COPY "J:\PRGNEW\GE\BOK\ACTIVEX.WOR".
         COPY "J:\PRGNEW\GE\BOK\ACUCOBOL.WOR".
         COPY "J:\PRGNEW\GE\BOK\ACUGUI.WOR".
         COPY "J:\PRGNEW\GE\BOK\CONTROLS.WOR".
         COPY "J:\PRGNEW\GE\BOK\FONTS.WOR".
         77  SALVA-COM-FUNCAO   PIC  X(002)    VALUE SPACES.
         77  WCANCSN-ANT        PIC  9(001)    VALUE ZEROS.
         77  WBOTAO-FUNCAO      PIC  A         VALUE SPACES.
         77  WCOR-CANCSN        PIC  9(005)    VALUE 0 COMP-3.

         COPY "J:\PRGNEW\GE\BOK\GEWORK.WOR".
         COPY "J:\PRGNEW\VD\BOK\VDWORK.WOR".
         01  GERAL.
         03  WDADOS-ENAB-VLRFIXO PIC  9(001) VALUE 1.
         03  WDADOS-ENAB-PERC    PIC  9(001) VALUE 1.
         03  WLOG-DADOS          PIC  X(080) VALUE SPACES.
         03  WLOG-DADOSR         REDEFINES WLOG-DADOS.
             05  WLOG-CODCFR     PIC  9(002).
         03  WTITULO.
             05  F               PIC  X(050)  VALUE
                 " Cadastro de Componentes Extras Frete ".
             05  F               PIC  X(020)  VALUE      SPACES.
             05  WTITULO-FUNCAO  PIC  X(032)  VALUE
                 "altera‡Æo".
         03  WX-ATI-INAT         PIC  X(009) VALUE SPACES.

      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\VD\BOK\VDCADCFR.EFDE"
              REPLACING ==XFD FILE="'VDCADCFR'.VFE"== BY == ==,
                        ==FD  'VDCADCFR'== BY == ==,
                        ==LABEL RECORD IS STANDARD.== BY == ==,
                        =='VDCADCFR'== BY ==LVDCADCFR==.
      *----------------------------------------------------------------*
         01  PARAMETROS.
         COPY "J:\PRGNEW\GE\BOK\GEGENAUD.EPRM"
              REPLACING =='GEGENAUD'== BY ==GEGENAUD==.
         COPY "J:\PRGNEW\VD\BOK\VDCADCFR.EPRM"
              REPLACING =='VDCADCFR'== BY ==VDCADCFR==.
      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\XXEXTSIS.EXT".
         COPY "J:\PRGNEW\VFEV99\AJUDA\WSPAD.".
         COPY "J:\PRGNEW\VFEV99\AJUDA\WSPAD2.".
      *----------------------------------------------------------------*
         LINKAGE SECTION.
         01  WPARAMETROS        PIC  X(128).
      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         SCREEN SECTION.
         01  T1.
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  BLANK SCREEN.
             05  FRAME
                 LINE   01,00 LINES 25,00 CELLS
                 COL    01,60 SIZE  94,00 CELLS
                 .
             05  FRAME
                 LINE + 00,60 LINES 02,50 CELLS
                 COL    02,40 SIZE  92,30 CELLS
                 LOWERED   ALTERNATE
                 .
             05  FRAME
                 LINE + 02,30 LINES 23,00 CELLS
                 COL    02,40 SIZE  92,30 CELLS
                 RAISED   ALTERNATE
                 .
      *................................................................*
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  LABEL "01-C¢digo Componente"
                 LINE   02,50 COL   04,00
                 .
      *................................................................*
             05  LABEL "02-Descri‡Æo"
                 LINE + 02,00 COL   04,00
                 .
             05  LABEL "03-Descri‡Æo Exporta‡Æo"
                 LINE + 02,00 COL   04,00
                 .
             05  LABEL "04-Forma de C lculo"
                 LINE + 02,00 COL   04,00
                 .
             05  LABEL "   "
                 LINE + 03,00 COL   05,00
                 .
             05  LABEL "05-"
                 LINE + 00,00 COL   25,00
                 .
             05  LABEL "   "
                 LINE + 03,60 COL   05,00
                 .
             05  LABEL "06-"
                 LINE + 00,00 COL   29,00
                 .
             05  LABEL "%"
                 LINE + 00,20 COL   42,50
                 .

             05  LABEL "07-Cancelado"
                 LINE + 04,70 COL   04,00
                 .
      *................................................................*
         01  C100.
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
/                LINE   02,60 COL   28,00 LINES 01,20 CELLS
                 PIC Z9       USING VDCADCFR-CODCFR
                 ENABLED = 0
                 3-D
                 AUTO
                 .
      *................................................................*
        01  D100 BEFORE B-D100.
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE   04,70 COL   28,00 LINES 01,20 CELLS
                 PIC X(030)           USING VDCADCFR-DESCR
                 AFTER A-102  THRU FA-102
                      ID 002
                 ENABLED = WDADOS-ENAB
                 3-D
                 AUTO
                 .
      *................................................................*
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE + 02,00 COL   28,00 LINES 01,20 CELLS
                 PIC X(015)       USING VDCADCFR-DESCR-EXPORT
                 AFTER A-1031 THRU FA-1031
                      ID 003
                 ENABLED = WDADOS-ENAB
                 3-D
                 AUTO
                 .
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  RADIO-BUTTON  "&1-Valor Fixo"
                 LINE + 05,10  COL   08,00
                 USING VDCADCFR-FORM-CALCULO
                 AFTER     A-104   THRU FA-104
                          ID 004
                 EXCEPTION E-104   THRU FE-104
                 ENABLED = WDADOS-ENAB
                 NOTIFY
                 GROUP = 001
                 GROUP-VALUE = 01
                 NO-GROUP-TAB
                 .

             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                               COL   28,00 LINES 01,20 CELLS
                 PIC ZZZ.ZZZ.ZZ9,99   USING VDCADCFR-FRM-CALC-VLFIXO
                 AFTER A-105 THRU FA-105
                      ID 005
                 ENABLED = WDADOS-ENAB-VLRFIXO
                 3-D
                 AUTO
                 .

             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  RADIO-BUTTON  "&2-Valor Vari vel"
                 LINE + 03,60  COL   08,00
                 USING VDCADCFR-FORM-CALCULO
                 AFTER     A-104   THRU FA-104
                          ID 004
                 EXCEPTION E-104   THRU FE-104
                 ENABLED = WDADOS-ENAB
                 NOTIFY
                 GROUP = 001
                 GROUP-VALUE = 02
                 NO-GROUP-TAB
                 .
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                               COL   32,00 LINES 01,20 CELLS
                 PIC ZZ9,9999   USING VDCADCFR-FRM-CALC-PERC
                 AFTER A-106 THRU FA-106
                      ID 006
                 ENABLED = WDADOS-ENAB-PERC
                 3-D
                 AUTO
                 .
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  RADIO-BUTTON  "Sobre Peso das NFes"
                 LINE - 01,00  COL   45,00
                 USING vdcadcfr-frm-calc-indic
                 AFTER     A-107   THRU FA-107
                          ID 007
                 EXCEPTION E-107   THRU FE-107
                 ENABLED = WDADOS-ENAB-PERC
                 NOTIFY
                 GROUP = 002
                 GROUP-VALUE = 1
                 NO-GROUP-TAB
                 .
             05  RADIO-BUTTON  "Sobre Valor Total das NFes"
                 LINE + 02,00  COL   45,00
                 USING vdcadcfr-frm-calc-indic
                 AFTER     A-107   THRU FA-107
                          ID 007
                 EXCEPTION E-107   THRU FE-107
                 ENABLED = WDADOS-ENAB-PERC
                 NOTIFY
                 GROUP = 002
                 GROUP-VALUE = 2
                 NO-GROUP-TAB
                 .
                 
      *................................................................*
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  CHECK-BOX "&Sim"
                 LINE + 03,70 COL   28,00
                 USING VDCADCFR-REG-CANC
                 AFTER     A-108  THRU FA-108
                 EXCEPTION E-108  THRU FE-108
                          ID 008
                 ENABLED = WDADOS-ENAB
                 NOTIFY
                 .
                 
             05  O108.
             07  COLOR IS WCOR-CANCSN.
             09  LABEL WX-ATI-INAT
                 LINE - 00,20 COL   35,00
                 .

      *................................................................*
             05  PUSH-BUTTON "  &OK   "
                 LINE + 03,50 LINES 02,00 CELLS
                 COL    28,00 SIZE  11,00 CELLS
                 TERMINATION-VALUE = 013
                      ID 009
                 ENABLED = WDADOS-ENAB
                 SELF-ACT
                 .
             05  PUSH-BUTTON "&CANCELA"
                              LINES 02,00 CELLS
                 COL  + 01,50 SIZE  11,00 CELLS
                 TERMINATION-VALUE = 027
                      ID 010
                 ENABLED = WDADOS-ENAB
                 SELF-ACT
                 .
             05  PUSH-BUTTON "&ANTERIOR"
                              LINES 02,00 CELLS
                 COL  + 01,50 SIZE  11,00 CELLS
                 TERMINATION-VALUE = 799
                      ID 011
                 ENABLED = WTBAR-ANTE
                 SELF-ACT
                 .
             05  PUSH-BUTTON "&PRàXIMO"
                              LINES 02,00 CELLS
                 COL  + 01,50 SIZE  11,00 CELLS
                 TERMINATION-VALUE = 798
                      ID 012
                 ENABLED = WTBAR-PROX
                 SELF-ACT
                 .
      *................................................................*
         COPY "J:\PRGNEW\GE\BOK\GETBAR.SCR".
         COPY "J:\PRGNEW\GE\BOK\GETBARC.SCR".
         COPY "J:\PRGNEW\GE\BOK\GETBARB.SCR".
      *      05  PUSH-BUTTON "Au&ditoria"
      *          COLUMN + 01,30 LINES 01,80 CELLS SIZE 16,00 CELLS
      *          SELF-ACT
      *          ENABLED = WTBAR-BOTAO01
      *          TERMINATION-VALUE = 701
      *          .
         COPY "J:\PRGNEW\GE\BOK\GESITMS.SCR".
         COPY "J:\PRGNEW\VD\BOK\VDSITMS.SCR".
      *================================================================*
      /
      *================================================================*
         PROCEDURE DIVISION USING WPARAMETROS.
         INICIO.
         COPY "J:\PRGNEW\GE\BOK\GEINICIO.PRO".
         COPY "J:\PRGNEW\VD\BOK\VDVERSA.PRO".
         COPY "J:\PRGNEW\GE\BOK\GENOSUB.PRO".

                MOVE 5620             TO XXEXTSIS-COM-CALLSUBNR.

         COPY "J:\PRGNEW\GE\BOK\GETSACE.PRO".

                DISPLAY FLOATING WINDOW
                 LINE -01,80 COL 02,00 LINES 25,50 SIZE 95,00
                 CONTROL FONT IS XXEXTSIS-COM-FONT-TELA
                 COLOR XXEXTSIS-USU-CORTEL
                 TITLE WTITULO
                 SYSTEM MENU
                 HANDLE IN WJANELA1.

                MOVE XXEXTSIS-COM-FUNCAO TO SALVA-COM-FUNCAO.

                IF XXEXTSIS-COM-FUNCAO = "IN" OR = "DU"
                    MOVE 0            TO WTBAR-ANTESN
                    MOVE 0            TO WTBAR-PROXSN.
         COPY "J:\PRGNEW\GE\BOK\GEDETBAR.PRO".
         COPY "J:\PRGNEW\GE\BOK\GETBAR.PRO".
                DISPLAY T1.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\VD\BOK\VDCADCFR.EMIO"
              REPLACING =='VDCADCFR'== BY ==VDCADCFR==.
      *----------------------------------------------------------------*
                MOVE WPARAMETROS         TO WPARMVDCADCFR.
                MOVE "altera‡Æo"         TO WTITULO-FUNCAO.
                IF XXEXTSIS-COM-FUNCAO = "DU"
                    MOVE "duplica  "     TO WTITULO-FUNCAO.
                IF XXEXTSIS-COM-FUNCAO = "IN"
                    MOVE "inclusÆo "     TO WTITULO-FUNCAO.
                MOVE "99"                TO XXEXTSIS-COM-RETCODE.
                MODIFY WJANELA1 TITLE WTITULO.
      *---------------------------------------------------------------*
         LE-VDCADCFR.
                INITIALIZE VDCADCFR-REGISTRO.
                MOVE WSPAD-CODEMP           TO VDCADCFR-NREMP.
                MOVE WPARMVDCADCFR-CODCFR   TO VDCADCFR-CODCFR.
                READ VDCADCFR.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE "VDCADCFR"      TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    GO TO RETORNA.
      *----------------------------------------------------------------*
         LEU-VDCADCFR.
              
                move vdcadcfr-frm-calc-indic to vdcadcfr-frm-calc-indic
         
         
                MOVE VDCADCFR-REGISTRO   TO LVDCADCFR-REGISTRO.
                MODIFY WJANELA1 TITLE WTITULO.
                MOVE XXEXTSIS-COM-CORMOS TO WCOR.
                MOVE 1                   TO WCHAVE-ENAB.
                DISPLAY C100.
                IF VDCADCFR-REG-CANC   = 0
                   MOVE  906            TO WCOR-CANCSN
                   MOVE "  ATIVO  "      TO WX-ATI-INAT
                ELSE
                   MOVE 432             TO WCOR-CANCSN
                   MOVE "CANCELADO"      TO WX-ATI-INAT
                END-IF

                DISPLAY O108.


                .

      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GELITBAR.PRO".
                MOVE ALL "N"             TO WSCR-CAMPO-OKT.
         PEGA-DADOS.
                PERFORM ABRE-FECHA-CAMPOS
                                       THRU ABRIU-FECHOU-CAMPOS.
                MOVE XXEXTSIS-USU-CORAC  TO WCOR.
                MOVE 1                   TO WDADOS-ENAB.
                DISPLAY D100.
                ACCEPT  D100 BEFORE TIME XXEXTSIS-USU-TIMEOUT.
                MOVE XXEXTSIS-USU-CORNAC TO WCOR.
                MOVE 0                   TO WDADOS-ENAB
                move 0                   to wdados-enab-vlrfixo
                                            wdados-enab-perc.
                DISPLAY D100.
                IF WTECLA-ESC
                 IF VDCADCFR-REGISTRO = LVDCADCFR-REGISTRO
                    GO TO RETORNA
                 ELSE
                    MOVE 100             TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
                    GO TO RETORNA.
                IF WTECLA-INATI
                    MOVE 033             TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
                    GO TO RETORNA.
                IF WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE
                 IF VDCADCFR-REGISTRO = LVDCADCFR-REGISTRO
                    GO TO RETORNA
                 ELSE
                    MOVE 100             TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
                    GO TO RETORNA.
                IF NOT WTECLA-ENTER AND NOT WTECLA-AUTO AND
                   NOT WTECLA-ANTE  AND NOT WTECLA-PROX
                    GO TO PEGA-DADOS.
                MOVE " "                 TO WBOTAO-FUNCAO.
                IF WTECLA-ANTE
                    MOVE "A"             TO WBOTAO-FUNCAO.
                IF WTECLA-PROX
                    MOVE "P"             TO WBOTAO-FUNCAO.
                MOVE SPACES              TO XXEXTSIS-COM-MSG.
                DISPLAY TMSGI UPON XXEXTSIS-COM-MWINDOW.

                MOVE 002                 TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      002        TO WSCR-ID
                    PERFORM A-102 THRU FA-102
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.

                ADD 1                    TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      003        TO WSCR-ID
                    PERFORM A-1031 THRU FA-1031
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.

                ADD  001                 TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      004        TO WSCR-ID
                    PERFORM A-104 THRU FA-104
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.

                ADD  001                 TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      005        TO WSCR-ID
                    PERFORM A-105 THRU FA-105
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.
                        
                ADD  001                 TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      006        TO WSCR-ID
                    PERFORM A-106 THRU FA-106
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.

                ADD  001                 TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      007        TO WSCR-ID
                    PERFORM A-107 THRU FA-107
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.

                ADD  001                 TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      008        TO WSCR-ID
                    PERFORM A-108 THRU FA-108
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.

      *----------------------------------------------------------------*
         CONFIRMA-PA.
                IF WBOTAO-FUNCAO NOT = "P" AND NOT = "A"
                    GO TO REGRAVA-VDCADCFR.
                IF VDCADCFR-REGISTRO = LVDCADCFR-REGISTRO
                    GO TO FGRAVA-LOG.
                MOVE "GRAVA ALTERA€åES DESTE REGISTRO ?                "
                                      TO XXEXTSIS-COM-MSG.
                MOVE 999              TO XXEXTSIS-COM-NRMSG.
                MOVE 26               TO XXEXTSIS-COM-LNHINI.
                MOVE 10               TO XXEXTSIS-COM-COLINI.
                CALL   "GERESPN".
                CANCEL "GERESPN".
                IF XXEXTSIS-COM-TECLA NOT = "S"
                    GO TO FGRAVA-LOG.
      *----------------------------------------------------------------*
         REGRAVA-VDCADCFR.
                IF WCANCSN-ANT         = 0
                  IF VDCADCFR-REG-CANC  NOT = 0
                    ACCEPT VDCADCFR-CANCDT    FROM CENTURY-DATE
                    ACCEPT WHORA              FROM TIME
                    MOVE WHORMIN                TO VDCADCFR-CANCHR
                    MOVE XXEXTSIS-USU-SIGLA     TO VDCADCFR-CANCSIGLA.
                ACCEPT VDCADCFR-LOGDT         FROM CENTURY-DATE.
                IF XXEXTSIS-COM-FUNCAO NOT = "DU" AND NOT = "IN"
                    ACCEPT VDCADCFR-ALTEDT    FROM CENTURY-DATE
                    ACCEPT WHORA              FROM TIME
                    MOVE WHORMIN                TO VDCADCFR-ALTEHR
                    MOVE XXEXTSIS-USU-SIGLA     TO VDCADCFR-ALTESIGLA.

                move vdcadcfr-frm-calc-indic to vdcadcfr-frm-calc-indic

                REWRITE VDCADCFR-REGISTRO.
                IF NOT XXEXTSIS-VALID-FSTATUS
                    MOVE "VDCADCFR"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    GO TO RETORNA.
                MOVE "00"             TO XXEXTSIS-COM-RETCODE.
                IF XXEXTSIS-COM-FUNCAO = "DU" OR = "IN"
                    GO TO RETORNA.
      *----------------------------------------------------------------*
         GRAVA-LOG.
                MOVE "A"              TO XXEXTSIS-LOG-OPERACAO.
                MOVE SPACES           TO WLOG-DADOS.
                MOVE VDCADCFR-CODCFR  TO WLOG-CODCFR.
                MOVE WLOG-DADOS       TO XXEXTSIS-LOG-DADOS.
                CALL "GELOG".
      *----------------------------------------------------------------*
         GRAVA-GEGENLAC.
                IF VDCADCFR-DESCR         NOT = LVDCADCFR-DESCR
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCADCFR"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Descri‡Æo           " TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCADCFR-DESCR        TO XXEXTSIS-LAC-VLRX-ANTE
                  MOVE VDCADCFR-DESCR         TO XXEXTSIS-LAC-VLRX-NOVO
                  CALL "GELAC".

                IF VDCADCFR-DESCR-EXPORT  NOT = LVDCADCFR-DESCR-EXPORT
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCADCFR"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Descri‡Æo Export    " TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCADCFR-DESCR-EXPORT TO XXEXTSIS-LAC-VLRX-ANTE
                  MOVE VDCADCFR-DESCR-EXPORT  TO XXEXTSIS-LAC-VLRX-NOVO
                  CALL "GELAC".
                  

                IF VDCADCFR-FRM-CALC-VLFIXO NOT = 
                   LVDCADCFR-FRM-CALC-VLFIXO
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCADCFR"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Valor Fixo (F.Calc) " TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCADCFR-FRM-CALC-VLFIXO  TO 
                       XXEXTSIS-LAC-VLR9-ANTE
                  MOVE VDCADCFR-FRM-CALC-VLFIXO   TO 
                       XXEXTSIS-LAC-VLR9-NOVO
                  CALL "GELAC".

                IF VDCADCFR-FRM-CALC-PERC NOT = LVDCADCFR-FRM-CALC-PERC
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCADCFR"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Percentual (F.Calc) " TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCADCFR-FRM-CALC-PERC  TO 
                       XXEXTSIS-LAC-VLR9-ANTE
                  MOVE VDCADCFR-FRM-CALC-PERC   TO 
                       XXEXTSIS-LAC-VLR9-NOVO
                  CALL "GELAC".

                IF VDCADCFR-FRM-CALC-INDIC NOT = 
                   LVDCADCFR-FRM-CALC-INDIC
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCADCFR"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Indicativo (F.Calc) " TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCADCFR-FRM-CALC-INDIC  TO 
                       XXEXTSIS-LAC-VLR9-ANTE
                  MOVE VDCADCFR-FRM-CALC-INDIC   TO 
                       XXEXTSIS-LAC-VLR9-NOVO
                  CALL "GELAC".

                IF VDCADCFR-REG-CANC NOT =  LVDCADCFR-REG-CANC
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCADCFR"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Indi.Cancelamento  "  TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCADCFR-REG-CANC     TO 
                       XXEXTSIS-LAC-VLR9-ANTE
                  MOVE VDCADCFR-REG-CANC      TO 
                       XXEXTSIS-LAC-VLR9-NOVO
                  CALL "GELAC".


         FGRAVA-GEGENLAC.
      *----------------------------------------------------------------*
         FGRAVA-LOG.
                IF WBOTAO-FUNCAO = "P"
                    GO TO MOSTRA-PROXIMO.
                IF WBOTAO-FUNCAO = "A"
                    GO TO MOSTRA-ANTERIOR.
                GO TO RETORNA.
      *----------------------------------------------------------------*
         MOSTRA-ANTERIOR.
                READ VDCADCFR PREVIOUS.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE "VDCADCFR"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    START VDCADCFR KEY >= VDCADCFR-CHAVE
                    READ VDCADCFR
                    GO TO LEU-VDCADCFR.
                IF VDCADCFR-NREMP NOT = WSPAD-CODEMP
                    MOVE "VDCADCFR"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    START VDCADCFR KEY >= VDCADCFR-CHAVE
                    READ VDCADCFR.
                GO TO LEU-VDCADCFR.
      *----------------------------------------------------------------*
         MOSTRA-PROXIMO.
                READ VDCADCFR NEXT.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE "VDCADCFR"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    START VDCADCFR KEY <= VDCADCFR-CHAVE
                    READ VDCADCFR
                    GO TO LEU-VDCADCFR.
                IF VDCADCFR-NREMP NOT = WSPAD-CODEMP
                    MOVE "VDCADCFR"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    START VDCADCFR KEY <= VDCADCFR-CHAVE
                    READ VDCADCFR.
                GO TO LEU-VDCADCFR.
      *----------------------------------------------------------------*
         B-D100.
         COPY "J:\PRGNEW\GE\BOK\GEDETBAR.PRO".

                IF XXEXTSIS-COM-FUNCAO NOT = "DU" AND NOT = "IN"
                    MOVE WTBAR-BOTAO01SN TO WTBAR-BOTAO01.

                INITIALIZE  XXEXTSIS-COM-MSG.
                EVALUATE WSCR-CAMPO
                WHEN 001
                  MOVE "DESCRI€ÇO DO COMPONENTE                        "
                                        TO XXEXTSIS-COM-MSG
                WHEN 002
                  MOVE "DESCRI€ÇO DO COMPONENTE P/ EXPORTA€ÇO          "
                                        TO XXEXTSIS-COM-MSG
                WHEN 003
                  MOVE "MARQUE PARA INFORMAR VALOR FIXO (FORM.CALCULO) "
                                       TO XXEXTSIS-COM-MSG
                  add  1               to wscr-campo
                WHEN 004
                  MOVE "INFORME O VALOR FIXO                           "
                                       TO XXEXTSIS-COM-MSG
                WHEN 005
                  MOVE "MARQUE P; INFORMAR VALOR VARIAVEL(FORM.CALCULO)"
                                       TO XXEXTSIS-COM-MSG
                WHEN 006
                  MOVE "INFORME O PERCENTUAL                           "
                                       TO XXEXTSIS-COM-MSG
                WHEN 007
                  MOVE "INFORME O TIPO PERCENTUAL VARIAVEL             "
                                       TO XXEXTSIS-COM-MSG
                WHEN 008
                  MOVE "INFORME O TIPO PERCENTUAL VARIAVEL             "
                                       TO XXEXTSIS-COM-MSG
                WHEN 009
                  MOVE "MARQUE PARA INFORMAR REGISTRO CANCELADO        "
                                       TO XXEXTSIS-COM-MSG
                WHEN 010
                  MOVE "TECLE ENTER OU CLICK NO BOTÇO PARA CONFIRMAR   "
                                        TO XXEXTSIS-COM-MSG
                WHEN 011
                  MOVE "TECLE ENTER OU CLICK NO BOTÇO PARA CANCELAR    "
                                        TO XXEXTSIS-COM-MSG
                WHEN 012
                 MOVE "CLICK NO BOTÇO PARA ATUALIZAR E IR PARA ANTERIOR"
                                        TO XXEXTSIS-COM-MSG
                WHEN 013
                  MOVE "CLICK NO BOTÇO PARA ATUALIZAR E IR PARA PRàXIMO"
                                        TO XXEXTSIS-COM-MSG
                END-EVALUATE.
                DISPLAY T101A UPON WTBAR.
                DISPLAY TMSGI UPON XXEXTSIS-COM-MWINDOW.
      *----------------------------------------------------------------*
         A-102.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-102.
                PERFORM VE-TECLA THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-102.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-102.
                IF VDCADCFR-DESCR = SPACES
                    MOVE 071            TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
                    MOVE 1              TO WSCR-ERRO
                    GO TO FA-102.
                MOVE "S"                TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-102.
                EXIT.
      *----------------------------------------------------------------*
         A-1031.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-1031.
                PERFORM VE-TECLA THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-1031.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-1031.
                if vdcadcfr-descr-export = spaces
                    move 071            to xxextsis-com-nrmsg
                    CALL "GEMSG"
                    move 4              to wscr-erro
                    go to fa-1031.
                DISPLAY D100.
                MOVE "S"                TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-1031.
                EXIT.
      *----------------------------------------------------------------*
         A-104.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-104.
                PERFORM VE-TECLA THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-104.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-104.
                if  vdcadcfr-form-calculo = 1
                    move 1              to wdados-enab-vlrfixo
                    move 0              to wdados-enab-perc
                                           vdcadcfr-frm-calc-indic
                                           vdcadcfr-frm-calc-perc
                    display d100
                    move "S"            to wscr-campo-ok (wscr-campo)
                    go to fa-104
                end-if

                if  vdcadcfr-form-calculo = 2
                    move 0              to wdados-enab-vlrfixo
                                           vdcadcfr-frm-calc-vlfixo
                    move 1              to wdados-enab-perc
                    display d100
                    move "S"            to wscr-campo-ok (wscr-campo)
                    go to fa-104
                end-if

                MOVE "S"                TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-104.
                EXIT.
      *----------------------------------------------------------------*
         E-104.
                if  vdcadcfr-form-calculo = 1
                    move 1              to wdados-enab-vlrfixo
                    move 0              to wdados-enab-perc
                                           vdcadcfr-frm-calc-indic
                                           vdcadcfr-frm-calc-perc
                end-if

                if  vdcadcfr-form-calculo = 2
                    move 0              to wdados-enab-vlrfixo
                                           vdcadcfr-frm-calc-vlfixo
                    move 1              to wdados-enab-perc
                    move 1              to vdcadcfr-frm-calc-indic
                end-if

                DISPLAY D100.
                MOVE 4                TO WSCR-ERRO.
                ADD  1                TO WSCR-ID.
                MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FE-104.
                EXIT.

      *----------------------------------------------------------------*
         A-105.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-105.
                PERFORM VE-TECLA THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-105.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-105.
                if  vdcadcfr-frm-calc-vlfixo  = zeros and
                    vdcadcfr-form-calculo = 1                
                    move 071            to xxextsis-com-nrmsg
                    CALL "GEMSG"
                    move 4              to wscr-erro
                    go to fa-105.
                DISPLAY D100.
                MOVE "S"                TO WSCR-CAMPO-OK(WSCR-CAMPO).
         FA-105.
                EXIT.
      *----------------------------------------------------------------*
         A-106.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-106.
                PERFORM VE-TECLA THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-106.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-106.
                if vdcadcfr-frm-calc-perc = zeros and
                   vdcadcfr-form-calculo = 2
                    move 071            to xxextsis-com-nrmsg
                    CALL "GEMSG"
                    move 4              to wscr-erro
                    go to fa-106.
                MOVE "S"                TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-106.
                EXIT.
      *----------------------------------------------------------------*
         A-114.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-114.
                PERFORM VE-TECLA THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-114.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-114.
                MOVE 1                TO WSCR-ERRO.
         FA-114.
                EXIT.
      *----------------------------------------------------------------*
         E-114.
                DISPLAY D100.
                MOVE 4                TO WSCR-ERRO.
                ADD  1                TO WSCR-ID.
                MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FE-114.
                EXIT.
      *----------------------------------------------------------------*
         A-107.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-107.
                PERFORM VE-TECLA THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-107.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-107.

                MOVE "S"                TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-107.
                EXIT.
      *----------------------------------------------------------------*
         E-107.

                DISPLAY D100.
                MOVE 4                TO WSCR-ERRO.
                ADD  1                TO WSCR-ID.
                MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FE-107.
                EXIT.
      *----------------------------------------------------------------*
         A-108.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-108.
                PERFORM VE-TECLA THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-108.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-108.
                IF VDCADCFR-REG-CANC = 0
                   MOVE  906            TO WCOR-CANCSN
                   MOVE "  ATIVO  "     TO WX-ATI-INAT
                ELSE
                   MOVE 432             TO WCOR-CANCSN
                   MOVE "CANCELADO"     TO WX-ATI-INAT
                END-IF.
                DISPLAY O108.
                MOVE "S"                TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-108.
                EXIT.
      *----------------------------------------------------------------*
         E-108.
                IF VDCADCFR-REG-CANC = 0          
                   MOVE  906            TO WCOR-CANCSN
                   MOVE "  ATIVO  "     TO WX-ATI-INAT
                ELSE
                   MOVE 432             TO WCOR-CANCSN
                   MOVE "CANCELADO"     TO WX-ATI-INAT
                END-IF.
                MOVE 4                  TO WSCR-ERRO.
                ADD  1                  TO WSCR-ID.
                DISPLAY O108.
                MOVE "S"                TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FE-108.
                EXIT.
      *----------------------------------------------------------------*

         TESTA-OPCAO-T1.

                IF WTECLA-BOTAO01
                   INITIALIZE WPARMGEGENAUD
                   MOVE WTITULO         TO WPARMGEGENAUD-TITULO
                   MOVE VDCADCFR-AUDIT  TO WPARMGEGENAUD-AUDIT
                   MOVE "GEGENAUD"      TO XXEXTSIS-COM-CALLPRX
                   MOVE XXEXTSIS-VER-GE TO XXEXTSIS-COM-CALLPRX (9:4)
                   CALL  XXEXTSIS-COM-CALLPRX USING WPARMGEGENAUD
                   CANCEL XXEXTSIS-COM-CALLPRX
                   MOVE 1 TO WSCR-ERRO
                   GO TO FTESTA-OPCAO-T1
                END-IF.

         FTESTA-OPCAO-T1.
                EXIT.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GETECLA.PRO".
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GEPERFOR.PRO".
      *----------------------------------------------------------------*
         RETORNA.
         COPY "J:\PRGNEW\VD\BOK\VDCADCFR.ECLO"
              REPLACING =='VDCADCFR'== BY ==VDCADCFR==.
         RETORNAF.
         COPY "J:\PRGNEW\VD\BOK\VDRESUB.PRO".
                EXIT PROGRAM.
      *================================================================*
         ABRE-FECHA-CAMPOS.
                
                if  vdcadcfr-form-calculo = 1
                    move 1              to wdados-enab-vlrfixo
                    move 0              to wdados-enab-perc
                end-if
                    
                if  vdcadcfr-form-calculo = 2
                    move 0              to wdados-enab-vlrfixo
                    move 1              to wdados-enab-perc
                end-if
                .

         ABRIU-FECHOU-CAMPOS.
                EXIT.
