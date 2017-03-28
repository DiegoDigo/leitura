      *================================================================*
         IDENTIFICATION DIVISION.
         PROGRAM-ID.    VDCACFRC.
         SECURITY.
      ******************************************************************
      * CONSULTA DE COMPONENTES EXTRAS FRETE                           *
      ******************************************************************
      *--------------  O B S E R V A C O E S  -------------------------*
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
         COPY "J:\PRGNEW\VD\BOK\VDCADCFR.ESSA"
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
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         COPY "J:\PRGNEW\GE\BOK\GEDIR.WOR".
         COPY "J:\PRGNEW\GE\BOK\ACTIVEX.WOR".
         COPY "J:\PRGNEW\GE\BOK\ACUCOBOL.WOR".
         COPY "J:\PRGNEW\GE\BOK\ACUGUI.WOR".
         COPY "J:\PRGNEW\GE\BOK\CONTROLS.WOR".
         COPY "J:\PRGNEW\GE\BOK\FONTS.WOR".

         COPY "J:\PRGNEW\GE\BOK\GEWORK.WOR".
         COPY "J:\PRGNEW\VD\BOK\VDWORK.WOR".
         01  GERAL.
         03  WLOG-DADOS         PIC  X(035) VALUE SPACES.
         03  WLOG-DADOSR        REDEFINES WLOG-DADOS.
             05  WLOG-TIPOCHAVE PIC  X(035).
         03  WTITULO            PIC  X(056) VALUE
             " Consulta Componentes Extras Frete ".

         03  WCODCFR            PIC  9(002) VALUE ZEROS.
         03  WDESCR             PIC  X(010) VALUE ZEROS.
         03  WDATA-INICIAL      PIC  9(008) VALUE ZEROS.
         03  FILLER REDEFINES WDATA-INICIAL.
             05  WDIA-INICIAL   PIC  9(002).
             05  WMES-INICIAL   PIC  9(002).
             05  WANO-INICIAL   PIC  9(004).
         03  WCLIENTE-INICIAL   PIC  9(008) VALUE ZEROS.
         03  FILLER REDEFINES WCLIENTE-INICIAL.
             05  WCLI-REGI-INI  PIC  9(004).
             05  WCLI-NUME-INI  PIC  9(004).
         03  WPRODUTO-INICIAL   PIC  9(006) VALUE ZEROS.
         03  FILLER REDEFINES WPRODUTO-INICIAL.
             05  WPRD-CFAM-INI  PIC  9(003).
             05  WPRD-NRO-INI   PIC  9(003).
         03  WTABELA-INICIAL    PIC  9(008) VALUE ZEROS.
         03  FILLER REDEFINES WTABELA-INICIAL.
             05  WTBPRD-ANO-INI PIC  9(004).
             05  WTBPRD-MES-INI PIC  9(002).
             05  WTBPRD-SEQ-INI PIC  9(002).

         03  WCHAVE             PIC  9(002) VALUE 01.
         03  WCHAVE-ENAB01      PIC  9      VALUE ZEROS.
         03  WCHAVE-ENAB02      PIC  9      VALUE ZEROS.

      *------ JANELA 1 ------------------------------------------------*
         03  WJ1-ENAB           PIC  9(001) VALUE ZEROS.
         03  WJ1-TOT-ITENS      PIC  9(007) VALUE 99999.
MREG     03  WJ1-QTD-ITENS      PIC  9(005) VALUE ZEROS.
MREG     03  WJ1-FIMARQ         PIC  A      VALUE "N".

      *+222+ --> TEM QUE SER NIVEL 01 (RECORD POSITION...)
         01  wj1-tel.
             05  wj1-tel-dados.
                 07  wj1-tel-codcfr         pic z9bbbbbbbbb.
                 07  wj1-tel-descr          pic x(030).
                 07  wj1-tel-descr-export   pic x(015).
                 07  wj1-tel-valor-fixo     pic z.zzz.zz9,99 
                     blank when zeroes.
                 07  wj1-tel-perc           pic zz9,9999
                     blank when zeroes.
                 07  wj1-tel-tipoperc       pic x(021).
                 07  wj1-tel-cancelado      pic x(005).
                 07  f                      pic x(020).
             05  wj1-chave.
                 07  wj1-cha-codcfr         pic 9(002).
         01  FWJ1-TEL.
         03  WJ1-CABEC.
             05  F PIC X(011) VALUE "   C¢digo ".
             05  F PIC X(052) VALUE " Descri‡Æo".
             05  F PIC X(020) VALUE "Descr.Export".
             05  F PIC X(014) VALUE " Valor Fixo".
             05  F PIC X(010) VALUE " % Var. ".
             05  F PIC X(026) VALUE "Tipo Percentual".
             05  F PIC X(006) VALUE "Status".
      *----------------------------------------------------------------*
         01 PARAMETROS.
         COPY "J:\PRGNEW\VD\BOK\VDCADCFR.EPRM"
              REPLACING =='VDCADCFR'== BY ==VDCADCFR==.
      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\XXEXTSIS.EXT".
         COPY "J:\PRGNEW\VFEV99\AJUDA\WSPAD.".
         COPY "J:\PRGNEW\VFEV99\AJUDA\WSPAD2.".
      *----------------------------------------------------------------*
      *+888+ LIBERAR PARA PESQUISA E ACERTAR O TAMANHO
      *  LINKAGE SECTION.
      *  01  WPARAMETROS        PIC  X(128).
      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         SCREEN SECTION.
         01  T1.
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  BLANK SCREEN.
             05  FRAME
                 LINE   01,00 LINES WTLINE-T1 CELLS
                 COL    01,60 SIZE  WTSIZE-T1 CELLS
                 .
             05  FRAME
                 LINE + 00,50 LINES 07,50 CELLS
                 COL    02,40 SIZE  WTSIZE-F1 CELLS
                 RAISED   ALTERNATE
                 .
             05  FRAME
                 LINE + 07,30 LINES WTLINE-F1 CELLS
                 COL    02,40 SIZE  WTSIZE-F1 CELLS
                 RAISED   ALTERNATE
                 .
             05  LABEL "1-Palavra a pesquisar"
                 LINE   02,30 COL   04,00
                 .
             05  LABEL "2-Sequˆncia"
                 LINE + 01,50 COL   04,00
                 .

         01  DD100.
             05  LABEL WJ1-CABEC
                 LINE   09,30 COL   04,00
                 .
      *................................................................*
         01  C100 BEFORE B-C100.
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE   02,50 COL   30,00 LINES 01,20 CELLS
                 PIC X(020)           USING WPALAVRA
                 AFTER A-101  THRU FA-101
                      ID 001
                 ENABLED = WCHAVE-ENAB
                 3-D
                 AUTO
                 UPPER
                 .
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  RADIO-BUTTON "&C¢digo"
                 LINE + 01,50 COL   14 LINES 00,90
                 USING WCHAVE
                 AFTER     A-102  THRU FA-102
                 EXCEPTION E-102  THRU FE-102
                          ID 002
                 ENABLED = WCHAVE-ENAB
                 GROUP-VALUE=1
                 NO-GROUP-TAB
                 NOTIFY
                 .
             05  RADIO-BUTTON "&Descri‡Æo"
                 LINE + 01,50 COL   14 LINES 00,90
                 USING WCHAVE
                 AFTER     A-102  THRU FA-102
                 EXCEPTION E-102  THRU FE-102
                          ID 002
                 GROUP-VALUE=2
                 ENABLED = WCHAVE-ENAB
                 NO-GROUP-TAB
                 NOTIFY
                 .
      *................................................................*
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE - 01,50 COL   30,00 LINES 01,20 CELLS
                 PIC Z9       USING WCODCFR
                 AFTER A-103  THRU FA-103
                      ID 003
                 ENABLED = WCHAVE-ENAB01
                 3-D
                 AUTO
                 UPPER
                 .
      *................................................................*
             05  ENTRY-FIELD
                 LINE + 01,50 COL   30,00 LINES 01,20 CELLS
                 PIC X(010)        USING WDESCR
                 AFTER A-104  THRU FA-104
                      ID 004
                 ENABLED = WCHAVE-ENAB02
                 3-D
                 RIGHT
                 .
      *................................................................*
             05  PUSH-BUTTON "  &OK   "
                 LINE   03,30 LINES 02,00 CELLS
                 COL    81,00 SIZE  11,00 CELLS
                 TERMINATION-VALUE = 013
                      ID 008
                 ENABLED = WCHAVE-ENAB
                 SELF-ACT
                 .
             05  PUSH-BUTTON "&CANCELA"
                 LINE + 02,20 LINES 02,00 CELLS
                 COL    81,00 SIZE  11,00 CELLS
                 TERMINATION-VALUE = 027
                      ID 009
                 ENABLED = WCHAVE-ENAB
                 SELF-ACT
                 .
      *................................................................*
         01  D100 LIST-BOX BEFORE B-D100 USING WJ1-TEL 3-D BOXED
                 UNSORTED
TodGri           FONT                  XXEXTSIS-COM-FONT-GRID
                 LINE   10,70 LINES WTLINE-D1 CELLS
                 COL    03,30 SIZE  WTSIZE-D1 CELLS
                 AFTER     A-111  THRU FA-111
                          ID 011
                 EXCEPTION E-111  THRU FE-111
                 NOTIFY-DBLCLICK
                 UNSORTED
                 ENABLED         = WJ1-ENAB
                 SELECTION-INDEX = 0001
                 ALIGNMENT       =
                 ( "R","L","L","R","R","L","L","L","L")
                 DISPLAY-COLUMNS =
                 (     11, 43, 58, 70, 78, 99, 105, 110)
      *             C   D   I   F   T   C   P   C    b
      *             O   E   N   I   B   L   R   A
      *             D   S   I   M   P   I   D   N
                 DIVIDERS        =
                 (      1,  1,  1,  1,  1,  1,  1,   1)
                 DATA-COLUMNS    =
                 (     11, 42, 57, 69, 77, 98, 104, 109).
      *+NAO+     DATA-COLUMNS    = ( record-position WJ1-TEL-CODFRE,
      *                              record-position WJ1-TEL-DESCR,
      *                              record-position WJ1-TEL-DTINI,
      *                              record-position WJ1-TEL-DTFIM,
      *                              record-position WJ1-TEL-TBPRD,
      *                              record-position WJ1-TEL-CLIENTE,
      *                              record-position WJ1-TEL-PRODUTO,
      *                              record-position WJ1-TEL-CANCSN)
      *          .
      *................................................................*
         COPY "J:\PRGNEW\GE\BOK\GETBAR.SCR".
MREG     COPY "J:\PRGNEW\GE\BOK\GETBAR10.SCR".
         COPY "J:\PRGNEW\GE\BOK\GETBAR3.SCR".
         COPY "J:\PRGNEW\GE\BOK\GETBAR5.SCR".
         COPY "J:\PRGNEW\GE\BOK\GETBAR6.SCR".
         COPY "J:\PRGNEW\GE\BOK\GETBAR7.SCR".
         COPY "J:\PRGNEW\GE\BOK\GETBAR8.SCR".
         COPY "J:\PRGNEW\GE\BOK\GETBAR9.SCR".
         COPY "J:\PRGNEW\GE\BOK\GESITMS.SCR".
      *++COPY "J:\PRGNEW\GE\BOK\VDSITMS.SCR".
      *================================================================*
      /
      *================================================================*
      *+888+ PROCEDURE DIVISION USING WPARAMETROS.
         PROCEDURE DIVISION.
         INICIO.
         COPY "J:\PRGNEW\GE\BOK\GEINICIO.PRO".
         COPY "J:\PRGNEW\VD\BOK\VDVERSA.PRO".
                                                                        01/12/05
                MOVE 007,50              TO WTLINE-T1.                  01/12/05
                MOVE 020,50              TO WTLINE-D1.                  01/12/05
                MOVE 095,10              TO WTSIZE-T1.                  01/12/05
                                                                        01/12/05
      *+888+ TIRAR GENOPGM.PRO e LIBERAR ESTES PARA SUB-ROTINA
      *  COPY "J:\PRGNEW\GE\BOK\GENOSUB.PRO".
      *
      *         MOVE 3653             TO XXEXTSIS-COM-CALLSUBNR.
      *
         COPY "J:\PRGNEW\GE\BOK\GENOPGM.PRO".
         COPY "J:\PRGNEW\GE\BOK\GETSACE.PRO".

                DISPLAY FLOATING WINDOW
      *+888+ PESQUISA LIBERA LINHA ABAIXO E SUPRIME A PRàXIMA
      *          LINE -01,80 COL 02,00 LINES WTLINE-W1 SIZE WTSIZE-W1
                 LINE -04,40 COL 01,40 LINES WTLINE-W1 SIZE WTSIZE-W1
                 CONTROL FONT IS XXEXTSIS-COM-FONT-TELA
                 COLOR XXEXTSIS-USU-CORTEL
                 TITLE WTITULO
                 SYSTEM MENU
                 HANDLE IN WJANELA1.

      *+333+                               VD
                IF XXEXTSIS-COM-NRSIS NOT = 06
                     MOVE 0           TO WTBAR-INCLSN
                     MOVE 0           TO WTBAR-ALTESN
                     MOVE 0           TO WTBAR-EXCLSN
                     MOVE 0           TO WTBAR-DUPLSN.
                MOVE 1                TO WTBAR-IMPRSN.

         COPY "J:\PRGNEW\GE\BOK\GETBAR.PRO".
                DISPLAY T1.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\VD\BOK\VDCADCFR.EMI"
              REPLACING =='VDCADCFR'== BY ==VDCADCFR==.
      *----------------------------------------------------------------*
         CONSULTA.
                DISPLAY T1.
                MOVE 1                   TO WCHAVE.
                INITIALIZE WCODCFR
                           WDESCR
                MOVE XXEXTSIS-USU-CORNAC TO WCOR.
                MOVE 0                   TO WJ1-ENAB.
                DISPLAY D100.
      *----------------------------------------------------------------*
                MOVE ALL "N"             TO WSCR-CAMPO-OKT.
         PEGA-CHAVE.
MREG            INITIALIZE WTNRREG.
MREG            DISPLAY TNRREG  UPON XXEXTSIS-COM-MWINDOW.
                INITIALIZE WTCODIGO9.
                DISPLAY TCODIGO9 UPON XXEXTSIS-COM-MWINDOW.
                MOVE XXEXTSIS-USU-CORAC  TO WCOR.
                MOVE 1                   TO WCHAVE-ENAB.
                MOVE 0                   TO WCHAVE-ENAB01
                                            WCHAVE-ENAB02
                DISPLAY C100.
                DISPLAY DD100.
                ACCEPT  C100 BEFORE TIME XXEXTSIS-USU-TIMEOUT.
                MOVE XXEXTSIS-USU-CORNAC TO WCOR.
                MOVE 0                   TO WCHAVE-ENAB
                                            WCHAVE-ENAB01
                                            WCHAVE-ENAB02
                DISPLAY C100.
                IF WTECLA-ESC
                    GO TO RETORNA.
                IF WTECLA-INATI
                    MOVE 033          TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
                    GO TO RETORNA.

                IF WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE
                    MOVE 081          TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
                    GO TO RETORNA.
                IF NOT WTECLA-ENTER AND NOT WTECLA-AUTO
                    GO TO PEGA-CHAVE.
                MOVE SPACES           TO XXEXTSIS-COM-MSG.
                DISPLAY TMSGI UPON XXEXTSIS-COM-MWINDOW.

                MOVE 001              TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      001     TO WSCR-ID
                    PERFORM A-101   THRU FA-101
                    IF WSCR-ERRO   NOT = ZEROS
                        GO TO PEGA-CHAVE.

                ADD  001              TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      002     TO WSCR-ID
                    PERFORM A-102   THRU FA-102
                    IF WSCR-ERRO   NOT = ZEROS
                        GO TO PEGA-CHAVE.

                ADD  001              TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      003     TO WSCR-ID
                    PERFORM A-103   THRU FA-103
                    IF WSCR-ERRO   NOT = ZEROS
                        GO TO PEGA-CHAVE.

                ADD  001              TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      004     TO WSCR-ID
                    PERFORM A-104   THRU FA-104
                    IF WSCR-ERRO   NOT = ZEROS
                        GO TO PEGA-CHAVE.

                ADD  001              TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      005     TO WSCR-ID
                    PERFORM A-105   THRU FA-105
                    IF WSCR-ERRO   NOT = ZEROS
                        GO TO PEGA-CHAVE.

      *----------------------------------------------------------------*
         RESTART-VDCADCFR.
                MOVE SPACES           TO WLOG-DADOS.
                IF WCHAVE = 01
                    STRING "COD-FRETE " WCODCFR
                           DELIMITED BY SIZE INTO WLOG-TIPOCHAVE
                    GO TO RESTART-VDCADCFR-COD.
                IF WCHAVE = 02
                    STRING "DESCRICAO " WDATA-INICIAL
                           DELIMITED BY SIZE INTO WLOG-TIPOCHAVE
                    GO TO RESTART-VDCADCFR-DESCRICAO.
                GO TO PEGA-CHAVE.
      *................................................................*
         RESTART-VDCADCFR-COD.
                INITIALIZE VDCADCFR-REGISTRO.
020611          MOVE WSPAD-CODEMP     TO VDCADCFR-NREMP.
                MOVE WCODCFR          TO VDCADCFR-CODCFR.
                START VDCADCFR KEY >= VDCADCFR-CHAVE
                  INVALID KEY
                    MOVE "VDCADCFR"   TO XXEXTSIS-COM-ARQUIVO
                    MOVE 029          TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG".
                GO TO GRAVA-LOG.
      *................................................................*
         RESTART-VDCADCFR-DESCRICAO.
                initialize vdcadcfr-registro.
                move wdescr           to vdcadcfr-descr
                start vdcadcfr key >= vdcadcfr-chave-descricao
                  invalid key
                    move "vdcadcfr"   to xxextsis-com-arquivo
                    move 029          to xxextsis-com-nrmsg
                    call "GEMSG".
                go to grava-log.
      *................................................................*
         GRAVA-LOG.
                MOVE "D"              TO XXEXTSIS-LOG-OPERACAO.
                MOVE WLOG-DADOS       TO XXEXTSIS-LOG-DADOS.
                CALL "GELOG".
      *----------------------------------------------------------------*
MREG            MOVE "N"                TO WJ1-FIMARQ.
                MOVE ZEROS              TO WTNRREG.
                MOVE XXEXTSIS-USU-CORAC TO WCOR.
                MOVE 1                  TO WJ1-ENAB.
                DISPLAY D100.
                MODIFY  D100 mass-update = 1 reset-list = 1.
                MODIFY  D100 mass-update = 0 reset-list = 0.
                PERFORM THREAD J1-MONTA-TAB THRU FJ1-MONTA-TAB
                  HANDLE IN WJANELAT
                .
      *+NAO+    MODIFY  D100 selection-index = 1.
      *----------------------------------------------------------------*
         J1-POSIC-ITEM.
                MOVE ALL "N"          TO WSCR-CAMPO-OKT.
                ACCEPT  D100 BEFORE TIME XXEXTSIS-USU-TIMEOUT.
                MOVE SPACES           TO XXEXTSIS-COM-MSG.
                DISPLAY TMSGI UPON XXEXTSIS-COM-MWINDOW.
                MOVE 001              TO WSCR-CAMPO.
                STOP THREAD WJANELAT.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      011     TO WSCR-ID
                    PERFORM A-111   THRU FA-111
                    IF WSCR-ERRO NOT= ZEROS
                        GO TO J1-POSIC-ITEM.
                IF WTECLA-ESC
      *             STOP THREAD WJANELAT
                    DESTROY D100
                    GO TO CONSULTA.
                IF WTECLA-INATI
                    MOVE 033          TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
      *             STOP THREAD WJANELAT
                    DESTROY D100
                    GO TO CONSULTA.
                IF WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE
      *             STOP THREAD WJANELAT
                    MOVE 081          TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
                    GO TO RETORNA.
                IF WJ1-CHA-CODCFR  NOT NUMERIC
                    MOVE ZEROS        TO WJ1-CHA-CODCFR.
      *+888+ ENTER USADO PARA SELECIONAR REGISTRO POSICIONADO PESQUISA
      *         IF WTECLA-ENTER
      *           IF WJ1-CHA-CODFRE NOT = ZEROS
      *             STOP THREAD WJANELAT
      *             GO TO FIM.
                IF WTECLA-ENTER OR WTECLA-VIEW
                 IF WTBAR-VIEW         = 1
                  IF WJ1-CHA-CODCFR NOT = ZEROS
                    INITIALIZE WPARMVDCADCFR
                    MOVE WJ1-CHA-CODCFR    TO WPARMVDCADCFR-CODCFR
                    MOVE "VDCACFRM"        TO XXEXTSIS-COM-CALLPRX
                    MOVE XXEXTSIS-VER-VD   TO XXEXTSIS-COM-CALLPRX (9:4)
                    CALL   XXEXTSIS-COM-CALLPRX USING WPARMVDCADCFR
                    CANCEL XXEXTSIS-COM-CALLPRX
                    CANCEL "VDCACFRM.PGM"
                    GO TO J1-POSIC-ITEM.
                IF WTECLA-INCL
                 IF WTBAR-INCL         = 1
      *+NAO+      IF WJ1-CHA-CODCFR NOT = ZEROS
                    INITIALIZE WPARMVDCADCFR
                    MOVE WJ1-CHA-CODCFR    TO WPARMVDCADCFR-CODCFR
                    MOVE "VDCACFRI"        TO XXEXTSIS-COM-CALLPRX
                    MOVE XXEXTSIS-VER-VD   TO XXEXTSIS-COM-CALLPRX (9:4)
                    CALL  XXEXTSIS-COM-CALLPRX
                    CANCEL XXEXTSIS-COM-CALLPRX
                    CANCEL "VDCACFRI.PGM"
      *+NAO+        STOP THREAD WJANELAT
                    DESTROY D100
                    GO TO RESTART-VDCADCFR.
                IF WTECLA-FECHAR
                 IF WEVE-TYPE          = CMD-DBLCLICK
                 IF WTBAR-ALTE         = 1
                  IF WJ1-CHA-CODCFR NOT = ZEROS
                    INITIALIZE WPARMVDCADCFR
                    MOVE WJ1-CHA-CODCFR    TO WPARMVDCADCFR-CODCFR
                    MOVE "VDCACFRA"        TO XXEXTSIS-COM-CALLPRX
                    MOVE XXEXTSIS-VER-VD   TO XXEXTSIS-COM-CALLPRX (9:4)
                    CALL   XXEXTSIS-COM-CALLPRX USING WPARMVDCADCFR
                    CANCEL XXEXTSIS-COM-CALLPRX
                    CANCEL "VDCACFRA.PGM"
      *+NAO+        STOP THREAD WJANELAT
                    DESTROY D100
                    GO TO RESTART-VDCADCFR.
                IF WTECLA-ALTE
                 IF WTBAR-ALTE         = 1
                  IF WJ1-CHA-CODCFR NOT = ZEROS
                    INITIALIZE WPARMVDCADCFR
                    MOVE WJ1-CHA-CODCFR    TO WPARMVDCADCFR-CODCFR
                    MOVE "VDCACFRA"        TO XXEXTSIS-COM-CALLPRX
                    MOVE XXEXTSIS-VER-VD   TO XXEXTSIS-COM-CALLPRX (9:4)
                    CALL   XXEXTSIS-COM-CALLPRX USING WPARMVDCADCFR
                    CANCEL XXEXTSIS-COM-CALLPRX
                    CANCEL "VDCACFRA.PGM"
      *+NAO+        STOP THREAD WJANELAT
                    DESTROY D100
                    GO TO RESTART-VDCADCFR.
                IF WTECLA-EXCL
                 IF WTBAR-EXCL         = 1
                  IF WJ1-CHA-CODCFR NOT = ZEROS
                    INITIALIZE WPARMVDCADCFR
                    MOVE WJ1-CHA-CODCFR    TO WPARMVDCADCFR-CODCFR
                    MOVE "VDCACFRE"        TO XXEXTSIS-COM-CALLPRX
                    MOVE XXEXTSIS-VER-VD   TO XXEXTSIS-COM-CALLPRX (9:4)
                    CALL   XXEXTSIS-COM-CALLPRX USING WPARMVDCADCFR
                    CANCEL XXEXTSIS-COM-CALLPRX
                    CANCEL "VDCACFRE.PGM"
      *+NAO+        STOP THREAD WJANELAT
                    DESTROY D100
                    GO TO RESTART-VDCADCFR.
                IF WTECLA-DUPL
                 IF WTBAR-DUPL         = 1
                  IF WJ1-CHA-CODCFR NOT = ZEROS
                    INITIALIZE WPARMVDCADCFR
                    MOVE WJ1-CHA-CODCFR    TO WPARMVDCADCFR-CODCFR
                    MOVE "VDCACFRD"        TO XXEXTSIS-COM-CALLPRX
                    MOVE XXEXTSIS-VER-VD   TO XXEXTSIS-COM-CALLPRX (9:4)
                    CALL   XXEXTSIS-COM-CALLPRX USING WPARMVDCADCFR
                    CANCEL XXEXTSIS-COM-CALLPRX
                    CANCEL "VDCACFRD.PGM"
      *+NAO+        STOP THREAD WJANELAT
                    DESTROY D100
                    GO TO RESTART-VDCADCFR.
                IF WTECLA-IMPR
                 IF WTBAR-IMPR         = 1
      *+NAO+      IF WJ1-CHA-CODCFR NOT = ZEROS
                    MOVE "VDCACFRL"         TO XXEXTSIS-COM-CALLPRX
                    MOVE XXEXTSIS-VER-VD    TO XXEXTSIS-COM-CALLPRX(9:4)
                    CALL  XXEXTSIS-COM-CALLPRX
                    CANCEL XXEXTSIS-COM-CALLPRX
                    CANCEL "VDCACFRL.PGM"
                    GO TO J1-POSIC-ITEM.
MREG            IF WTECLA-MREG
MREG             IF WTBAR-MREG           = 1
MREG                PERFORM THREAD J1-MONTA-TAB THRU FJ1-MONTA-TAB
MREG                  HANDLE IN WJANELAT
MREG                GO TO J1-POSIC-ITEM.

                IF WTECLA-HCPO
                   MOVE 011           TO WSCR-ID
                ELSE
                   MOVE ZEROS         TO WSCR-ID.
                PERFORM VE-TECLA    THRU FVE-TECLA.

                PERFORM THREAD J1-MONTA-TAB THRU FJ1-MONTA-TAB
                  HANDLE IN WJANELAT.

                GO TO J1-POSIC-ITEM.
      *----------------------------------------------------------------*
      *+888+ ACERTAR CAMPOS DO WPARM...
         FIM.
                MOVE WJ1-CHA-CODCFR   TO WPARMVDCADCFR-CODCFR.
                GO TO RETORNA.
      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         B-C100.
         COPY "J:\PRGNEW\GE\BOK\GEDETBAR.PRO".
                MOVE WTBAR-INCLSN     TO WTBAR-INCL.
                MOVE WTBAR-IMPRSN     TO WTBAR-IMPR.
                EVALUATE WSCR-CAMPO
                WHEN 001
                  MOVE "LETRAS OU PALAVRA A PESQUISAR NA DESCRI€ÇO     "
                                      TO XXEXTSIS-COM-MSG
                WHEN 002
                  MOVE "SELECIONE PARA PESQUISAR POR CODIGO COMPONENTE "
                                      TO XXEXTSIS-COM-MSG
                WHEN 003
                  MOVE "SELECIONE PARA PESQUISAR POR DESCRI€ÇO         "
                                      TO XXEXTSIS-COM-MSG
                WHEN 004
                  MOVE "SELECIONE PARA PESQUISAR POR CODIGO COMPONENTE "
                                      TO XXEXTSIS-COM-MSG
                WHEN 005
                  MOVE "TECLE ENTER OU CLICK NO BOTAO PARA CONFIRMAR   "
                                      TO XXEXTSIS-COM-MSG
                WHEN 006
                  MOVE "TECLE ENTER OU CLICK NO BOTAO PARA CANCELAR    "
                                      TO XXEXTSIS-COM-MSG
                END-EVALUATE.
                DISPLAY T101A UPON WTBAR.
                DISPLAY TMSGI UPON XXEXTSIS-COM-MWINDOW.
      *----------------------------------------------------------------*
         A-101.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-101.
                PERFORM VE-TECLA    THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-101.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-101.
                MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-101.
                EXIT.
      *----------------------------------------------------------------*
         A-102.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-102.
                PERFORM VE-TECLA    THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-102.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-102.
                IF WCHAVE              = 01
                    MOVE 1            TO WCHAVE-ENAB01
                    MOVE 0            TO WCHAVE-ENAB02
                    DISPLAY C100
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FA-102.
                IF WCHAVE              = 02
                    MOVE 0            TO WCHAVE-ENAB01
                    MOVE 1            TO WCHAVE-ENAB02
                    DISPLAY C100
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FA-102.
                IF WCHAVE              = 03
                    MOVE 0            TO WCHAVE-ENAB01
                    MOVE 0            TO WCHAVE-ENAB02
                    DISPLAY C100
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FA-102.
                IF WCHAVE              = 04
                    MOVE 0            TO WCHAVE-ENAB01
                    MOVE 0            TO WCHAVE-ENAB02
                    DISPLAY C100
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FA-102.
                IF WCHAVE              = 05
                    MOVE 0            TO WCHAVE-ENAB01
                    MOVE 0            TO WCHAVE-ENAB02
                    DISPLAY C100
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FA-102.
                MOVE 1                TO WSCR-ERRO.
         FA-102.
                EXIT.
      *----------------------------------------------------------------*
         E-102.
                if wchave              = 01
                    move 1            to wchave-enab01
                    move 0            to wchave-enab02
                    display c100
                    go to e-102-c.
                if wchave              = 02
                    move 0            to wchave-enab01
                    move 1            to wchave-enab02
                    display c100
                    go to e-102-c.
                if wchave              = 03
                    move 0            to wchave-enab01
                    move 0            to wchave-enab02
                    display c100
                    go to e-102-c.
                if wchave              = 04
                    move 0            to wchave-enab01
                    move 0            to wchave-enab02
                    display c100
                    go to e-102-c.
                if wchave              = 05
                    move 0            to wchave-enab01
                    move 0            to wchave-enab02
                    display c100
                    go to e-102-c.
         E-102-C.
                MOVE 4                TO WSCR-ERRO.
                ADD  1                TO WSCR-ID.
                MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FE-102.
                EXIT.
      *----------------------------------------------------------------*
         A-103.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-103.
                PERFORM VE-TECLA    THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-103.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-103.
                MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-103.
                EXIT.
      *----------------------------------------------------------------*
         A-104.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-104.
                PERFORM VE-TECLA    THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-104.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-104.
                MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-104.
                EXIT.
      *----------------------------------------------------------------*
         A-105.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-105.
                PERFORM VE-TECLA    THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-105.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-105.
                MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-105.
                EXIT.
      *----------------------------------------------------------------*
         B-D100.
         COPY "J:\PRGNEW\GE\BOK\GEDETBAR.PRO".
                EVALUATE WSCR-CAMPO
                WHEN 001
                  MOVE WTBAR-VIEWSN   TO WTBAR-VIEW
                  MOVE WTBAR-INCLSN   TO WTBAR-INCL
                  MOVE WTBAR-ALTESN   TO WTBAR-ALTE
                  MOVE WTBAR-EXCLSN   TO WTBAR-EXCL
                  MOVE WTBAR-DUPLSN   TO WTBAR-DUPL
                  MOVE WTBAR-IMPRSN   TO WTBAR-IMPR
                  MOVE "POSICIONE EM UM DOS REGISTROS PARA A SELE€ÇO   "
                                       TO XXEXTSIS-COM-MSG
                END-EVALUATE.
                DISPLAY T101A UPON WTBAR.
                DISPLAY TMSGI UPON XXEXTSIS-COM-MWINDOW.
      *----------------------------------------------------------------*
         A-111.
                IF WTECLA-VIEW    OR WTECLA-FOTO
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FA-111.
                IF WTECLA-INCL    OR WTECLA-ALTE    OR WTECLA-EXCL
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FA-111.
                IF WTECLA-DUPL    OR WTECLA-IMPR
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FA-111.
MREG            IF WTECLA-MREG
MREG                MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
MREG                GO TO FA-111.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-111.
                PERFORM VE-TECLA    THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    MOVE 0 TO WTBAR-BOTAO02
                    GO TO FA-111.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-111.
                MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-111.
                EXIT.
      *----------------------------------------------------------------*
         E-111.
                IF WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE
                    GO TO FE-111.
                IF NOT WTECLA-FECHAR
                    GO TO E-111-C.
                IF WEVE-TYPE           = CMD-CLOSE OR = CMD-DBLCLICK
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FE-111.
                MOVE 0                TO WSCR-ERRO.
                IF WEVE-TYPE           = NTF-PL-FIRST
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FE-111.
                IF WEVE-TYPE           = NTF-PL-PREVPAGE
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FE-111.
                IF WEVE-TYPE           = NTF-PL-PREV
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FE-111.
                IF WEVE-TYPE           = NTF-PL-NEXT
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FE-111.
                IF WEVE-TYPE           = NTF-PL-NEXTPAGE
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FE-111.
                IF WEVE-TYPE           = NTF-PL-LAST
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FE-111.
                IF WEVE-TYPE           = NTF-PL-SEARCH
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FE-111.
         E-111-C.
                MOVE 4                TO WSCR-ERRO.
                MOVE 002              TO WSCR-ID.
         FE-111.
                EXIT.
      *----------------------------------------------------------------*
         J1-MONTA-TAB.
MREG            IF WJ1-FIMARQ          = "S"
MREG                GO TO FLE-VDCADCFR.
MREG            MOVE WTLINE-D1        TO WJ1-TOT-ITENS.
MREG  *         ADD  WTLINE-D1        TO WJ1-TOT-ITENS.
MREG  * SE S/LIMITE
MREG  *         MOVE 99999            TO WJ1-TOT-ITENS.
MREG            MOVE ZEROS            TO WJ1-QTD-ITENS.
MREG            MOVE 0                TO WTBAR-MREG.
MREG            DISPLAY T101A UPON WTBAR.
         LE-VDCADCFR.
                READ VDCADCFR WITH NO LOCK AT END
MREG                MOVE "S"          TO WJ1-FIMARQ
                    GO TO FLE-VDCADCFR.
                IF XXEXTSIS-COM-FSTATUS = XXEXTSIS-COM-FSFIMARQ OR "46"
MREG                MOVE "S"          TO WJ1-FIMARQ
                    GO TO FLE-VDCADCFR.
                IF NOT XXEXTSIS-VALID-FSTATUS
                    MOVE "VDCADCFR"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    GO TO LE-VDCADCFR.
                IF VDCADCFR-NREMP NOT = WSPAD-CODEMP
                    GO TO LE-VDCADCFR.
                MOVE VDCADCFR-CODCFR  TO WTCODIGO9.
                DISPLAY TCODIGO9 UPON XXEXTSIS-COM-MWINDOW.
                IF WPALAVRA            = SPACES
                    GO TO LE-VDCADCFR-C.
                MOVE VDCADCFR-DESCR   TO WREGISTRO.
                CALL 'SRTOUPER' USING WREGISTRO.
                CALL 'SRCKNOME' USING WREGISTRO WPALAVRA WRESP.
                IF WRESP = "N"
                    GO TO LE-VDCADCFR.
      *................................................................*
         LE-VDCADCFR-C.
MREG            IF WJ1-QTD-ITENS > WJ1-TOT-ITENS
      *             MOVE
      *             "TABELA > 99999 REGISTROS EXCEDIDA AVISAR CONTROL"
      *                               TO XXEXTSIS-COM-MSG
      *             MOVE 999          TO XXEXTSIS-COM-NRMSG
      *             CALL "GEMSG"
MREG                MOVE 1            TO WTBAR-MREG
MREG                DISPLAY T101A UPON WTBAR
270410              READ VDCADCFR PREVIOUS
                    GO TO FLE-VDCADCFR.
                PERFORM J1-MONTA-REG THRU FJ1-MONTA-REG.
                GO TO LE-VDCADCFR.
         FLE-VDCADCFR.
      *+++++++++YIELD.
         FJ1-MONTA-TAB.
                EXIT.
      *----------------------------------------------------------------*
         J1-MONTA-REG.
      *................................................................*
      *+222+
      
                move vdcadcfr-codcfr        to wj1-cha-codcfr
                                               wj1-tel-codcfr.
                move vdcadcfr-descr         to wj1-tel-descr.
                move vdcadcfr-descr-export  to wj1-tel-descr-export
                if  vdcadcfr-form-calculo = 1
                    move vdcadcfr-frm-calc-vlfixo to wj1-tel-valor-fixo
                    move zeros                    to wj1-tel-perc
                else
                    move zeros                    to wj1-tel-valor-fixo
                    move vdcadcfr-frm-calc-perc   to wj1-tel-perc
                end-if     
                
                if  vdcadcfr-form-calculo = 2
                
                    if vdcadcfr-frm-calc-indic = 1
                       move "Sobre o Peso das NFEs" to wj1-tel-tipoperc
                    else
                       move "S/ o Valor Total NFEs" to wj1-tel-tipoperc
                    end-if       
                end-if
                
                move spaces               to wj1-tel-cancelado  

                if  vdcadcfr-reg-canc = 0
                    move "ATIVO"          to wj1-tel-cancelado
                else
                    move "CANC."          to wj1-tel-cancelado
                end-if    
                    

                modify d100 item-to-add = wj1-tel.
                MOVE SPACES            TO WJ1-TEL.
MREG            ADD  1  TO WJ1-QTD-ITENS.
                ADD  1  TO WTNRREG.
                DISPLAY TNRREG  UPON XXEXTSIS-COM-MWINDOW.
         FJ1-MONTA-REG.
                EXIT.
      *----------------------------------------------------------------*
         TESTA-OPCAO-T1.

                IF WTECLA-INCL
                    INITIALIZE WPARMVDCADCFR
                    MOVE "VDCACFRI"        TO XXEXTSIS-COM-CALLPRX
                    MOVE XXEXTSIS-VER-VD   TO XXEXTSIS-COM-CALLPRX (9:4)
                    CALL  XXEXTSIS-COM-CALLPRX
                    CANCEL XXEXTSIS-COM-CALLPRX
                    MOVE 1                 TO WSCR-ERRO
                    GO TO FTESTA-OPCAO-T1
                END-IF.

                IF WTECLA-IMPR
                    MOVE "VDCACFRL"        TO XXEXTSIS-COM-CALLPRX
                    MOVE XXEXTSIS-VER-VD   TO XXEXTSIS-COM-CALLPRX (9:4)
                    CALL  XXEXTSIS-COM-CALLPRX
                    CANCEL XXEXTSIS-COM-CALLPRX
                    MOVE 1                 TO WSCR-ERRO
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
                CANCEL 'SRTOUPER'.
                CANCEL 'SRCKNOME'.
         RETORNAF.
      *+888+  "G:\PRGNEW\VD\BOK\VDRESUB.PRO".
         COPY "J:\PRGNEW\VD\BOK\VDREPGM.PRO".
                EXIT PROGRAM.
      *================================================================*
