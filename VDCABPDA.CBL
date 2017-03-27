      *================================================================*
         IDENTIFICATION DIVISION.
         PROGRAM-ID.    VDCABPDA.
         SECURITY.
      ******************************************************************
      * ALTERA MOTIVOS DE BLOQUEIO DE PEDIDOS                          *
      ******************************************************************
      *--------------  O B S E R V A C O E S  -------------------------*
      * - *+FFF+ LIBERAR NO FUTURO                                     *
      * -                                                              *
      *--------------  A L T E R A C O E S  ---------------------------*
      * 09/09/2005 COPIADO DO VDCANCPA.CBL                    CRISTIANO*
      * 14/03/2006 INCLUSÇO DE TESTE PARA VERIFICAR SE FOR             *
      *            WTECLA-FECHAR NOS EXCEPTIONS               CRISTIANO*
      * 17/03/2006 INVERSÇO DA POSI€ÇO DOS BOTåES "PRàXIMO" E          *
      *            "ANTERIOR" NA TELA                         CRISTIANO*
      * 12/05/2006 INCLUIR RESTRI€ÇO PARA DESABILITAR O CAMPO          *
      *            BLOQ. PEDIDO QDO CàD. ESTIVER ENTRE 4 E 9  CRISTIANO*
      * 29/05/2006 ACERTO DAS ROTINAS "MOSTRA-ANTERIOR" E              *
      *            "MOSTRA-PROXIMO"                           CRISTIANO*
      * 22/09/2006 INVERTIDO HELP NO B-D100 DE ANTERIOR/PROXIMO ROBERTO*
      * 11/08/2010 CONVERSÇO VDCADBPD-COD DE PIC 9(2) PARA 9(3)  VALDIR*10.10.007
      * 23/05/2011 AJUSTE NAS FD, SELECT, PRM, OPENS, E CLOSE     KENDY*11.05.008
      *    PARA O PADRÇO NOVO                                          *
      * 02/06/2011 TRATAMENTO DE FAIXA RESTRITA                   KENDY*11.06.009
      * 27/07/2011 TRATAMENTO CAMPOS EXCLUSIVOS P/ FAB.SCHIN    MARCELO*11.07.010
      *            WDADOS-ENAB-BLOQ=0                                  *
      *            (REVENDAS SCHIN NAO PODERAO ACESSAR ESSES CAMPOS).  *
      * 02/04/2012 AJUSTE.                                      MARCELO*12.04.011
      * 12/07/2012 RECOMPILADO.                                 MARCELO*12.07.012
      * 23/07/2012 AJUSTE MENSAGEM DENTRO DA ROT GETSALTE.PRO   MARCELO*12.07.013
      *            QUANDO FOR FABRICA E ESTIVER ALTERANDO REGISTRO     *
      *            FORA DE SUA FAIXA, SERA APRESENTADA MSG:            *
      *            "ALTERACAO FABRICA FORA DE SUA FAIXA:"              *
      *            "FAIXA RESTRITA: XXXXXXXXX ATE XXXXXXXXX"           *
      * 08/03/2017 OS-8643: CONTROLE DE MANUTENCAO DE CADASTROS        *
      *                     CHAMADA AO VDCADFLC PARA REGISTRAR         *
      *                     INC/ALT/EXC/DUP PARA ALIMENTAR             *
      *                     ARQUIVO NOVO "VDCADFLC.VFE"                *
      *                     QUE SERVIRA TAMBEM PARA O PORTAL.     DIEGO*17.03.014
      *================================================================*
         ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
         SPECIAL-NAMES.
         COPY "J:\PRGNEW\GE\BOK\GESPECI.ENV".
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.ESDA"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.
         COPY "J:\PRGNEW\VD\BOK\VDCADBPD.ESDA"
              REPLACING =='VDCADBPD'== BY ==VDCADBPD==.
      *================================================================*
      /
      *================================================================*
         DATA DIVISION.
         FILE SECTION.
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.EFDE"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.
         COPY "J:\PRGNEW\VD\BOK\VDCADBPD.EFDE"
              REPLACING =='VDCADBPD'== BY ==VDCADBPD==.
      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         WORKING-STORAGE SECTION.
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         77 WTPGM-HEADER        PIC  X(019) VALUE "=>CONTROLE VERSAO<=".
         77 WTPGM-VERSAO        PIC  X(009) VALUE "17.03.014".
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         COPY "J:\PRGNEW\GE\BOK\GEDIR.WOR".
         COPY "J:\PRGNEW\VD\BOK\VDDIR.WOR".
         COPY "J:\PRGNEW\GE\BOK\ACTIVEX.WOR".
         COPY "J:\PRGNEW\GE\BOK\ACUCOBOL.WOR".
         COPY "J:\PRGNEW\GE\BOK\ACUGUI.WOR".
         COPY "J:\PRGNEW\GE\BOK\CONTROLS.WOR".
         COPY "J:\PRGNEW\GE\BOK\FONTS.WOR".
         77  WCANCSN-ANT          PIC  9(001)    VALUE ZEROS.
         77  WBOTAO-FUNCAO        PIC  A         VALUE SPACES.
         77  WCOR-EXCECAO-104     PIC  9(005)    VALUE 0 COMP-3.
         77  WDADOS-ENAB-104      PIC  9(001)    VALUE 1.
         COPY "J:\PRGNEW\GE\BOK\GEWORK.WOR".
         COPY "J:\PRGNEW\VD\BOK\VDWORK.WOR".
         01  GERAL.
         03  WLOG-DADOS           PIC  X(080)    VALUE SPACES.
         03  WLOG-DADOSR        REDEFINES WLOG-DADOS.
             05  WLOG-COD         PIC  9(003).
         03  WTITULO.
             05  F                PIC  X(050)    VALUE
                 " Motivos de Bloqueio de Pedidos".
             05  F                PIC  X(020)    VALUE SPACES.
             05  WTITULO-FUNCAO   PIC  X(032)    VALUE
                 "altera‡Æo".
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\VD\BOK\VDCADBPD.EFDE"
              REPLACING ==XFD FILE="'VDCADBPD'.VFE"== BY == ==,
                        ==FD  'VDCADBPD'== BY == ==,
                        ==LABEL RECORD IS STANDARD.== BY == ==,
                        =='VDCADBPD'== BY ==LVDCADBPD==.
                        
080317   01   AREAS-EPRM-VDCADFLC.
080317   COPY "J:\PRGNEW\VD\BOK\VDCADFLC.EPRM"
080317        REPLACING =='VDCADFLC'== BY ==VDCADFLC==.                        
      *----------------------------------------------------------------*
         01  PARAMETROS.
         COPY "J:\PRGNEW\GE\BOK\GEGENAUD.EPRM"
              REPLACING =='GEGENAUD'== BY ==GEGENAUD==.
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.EPRM"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.
         COPY "J:\PRGNEW\VD\BOK\VDCADBPD.EPRM"
              REPLACING =='VDCADBPD'== BY ==VDCADBPD==.
      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\XXEXTSIS.EXT".
020611   COPY "J:\PRGNEW\VFEV99\AJUDA\WSPAD.".
020611   COPY "J:\PRGNEW\VFEV99\AJUDA\WSPAD2.".
      *----------------------------------------------------------------*
         LINKAGE SECTION.
         01  WPARAMETROS        PIC  X(030).
      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         SCREEN SECTION.
         01  T1.
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  BLANK SCREEN.
             05  FRAME
                 LINE   01,00 LINES 14,05 CELLS
                 COL    01,60 SIZE  71,00 CELLS
                 .
             05  FRAME
                 LINE + 00,60 LINES 02,70 CELLS
                 COL    02,40 SIZE  69,30 CELLS
                 LOWERED   ALTERNATE
                 .
             05  FRAME
                 LINE + 02,50 LINES 10,30 CELLS
                 COL    02,40 SIZE  69,30 CELLS
                 RAISED   ALTERNATE
                 .
             04  COLOR IS XXEXTSIS-USU-CORNAC.
             05  FRAME "Pedido"
                 LINE + 03,80 LINES 03,00 CELLS
                 COL    03,00 SIZE  28,50 CELLS
                 RAISED   ALTERNATE
                 .
      *      05  FRAME
      *+FFF+     LINE + 03,80 LINES 04,00 CELLS
      *          COL    02,40 SIZE  82,00 CELLS
      *          LOWERED  ALTERNATE
      *          .
      *................................................................*
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  LABEL "01-C¢digo do Motivo"
                 LINE   02,50 COL   04,00
                 .
      *................................................................*
             05  LABEL "02-Descri‡Æo do Motivo"
                 LINE + 02,40 COL   04,00
                 .
             05  LABEL "03-Sigla"
                 LINE + 01,50 COL   04,00
                 .
      *................................................................*
             05  LABEL "04-Bloqueia"
                 LINE + 02,40 COL   04,00
                 .
      *................................................................*
      *      05  LABEL "incluso  "
      *+FFF+     LINE + 04,30 COL   06,00
      *          .
      *      05  LABEL "alterado "
      *          LINE + 01,00 COL   06,00
      *          .
      *      05  LABEL "cancelado"
      *          LINE + 01,00 COL   06,00
      *          .
      *................................................................*
         01  C100.
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE   02,60 COL   25,00 LINES 01,20 CELLS
                 PIC 9(003)           USING VDCADBPD-COD
                 ENABLED = 0
                 3-D
                 AUTO
                 .
      *................................................................*
        01  D100 BEFORE B-D100.
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE   05,00 COL   25,00 LINES 01,20 CELLS
                 PIC X(010)           USING VDCADBPD-DESCR
                 AFTER A-102  THRU FA-102
                      ID 002
270711           ENABLED = WDADOS-ENAB-BLOQ
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   25,00 LINES 01,20 CELLS
                 PIC X(003)           USING VDCADBPD-SIGLA
                 AFTER A-103  THRU FA-103
                      ID 003
                 ENABLED = WDADOS-ENAB
                 3-D
                 AUTO
                 .
      *................................................................*
      *      04  COLOR IS XXEXTSIS-USU-CORTEL.
             04  COLOR IS WCOR-EXCECAO-104.
             05  CHECK-BOX "&Sim"
                 LINE + 02,50 COL   25,00
                 USING VDCADBPD-BLQPED
                 AFTER     A-104  THRU FA-104
                          ID 004
                 EXCEPTION E-104  THRU FE-104
                 ENABLED = WDADOS-ENAB-104
                 NOTIFY
                 .
      *................................................................*
             05  PUSH-BUTTON "  &OK   "
                 LINE + 02,60 LINES 02,00 CELLS
                 COL    25,00 SIZE  11,00 CELLS
                 TERMINATION-VALUE = 013
                      ID 005
                 ENABLED = WDADOS-ENAB
                 SELF-ACT
                 .
             05  PUSH-BUTTON "&CANCELA"
                              LINES 02,00 CELLS
                 COL  + 01,50 SIZE  11,00 CELLS
                 TERMINATION-VALUE = 027
                      ID 006
                 ENABLED = WDADOS-ENAB
                 SELF-ACT
                 .
             05  PUSH-BUTTON "&ANTERIOR"
                              LINES 02,00 CELLS
                 COL  + 01,50 SIZE  11,00 CELLS
                 TERMINATION-VALUE = 799
                      ID 007
                 ENABLED = WTBAR-ANTE
                 SELF-ACT
                 .
             05  PUSH-BUTTON "&PRàXIMO"
                              LINES 02,00 CELLS
                 COL  + 01,50 SIZE  11,00 CELLS
                 TERMINATION-VALUE = 798
                      ID 008
                 ENABLED = WTBAR-PROX
                 SELF-ACT
                 .
      *................................................................*
      *  01  O197.
      *      04  COLOR IS XXEXTSIS-USU-CORDIS.
      *      05  LABEL WDT8-DED                LINE   17,30 COL   18,00.
      *      05  LABEL WHORA4ED                             COL   26,00.
      *      05  LABEL WSIGLA-USER                          COL   32,00.
      *      05  LABEL GEGENUSU-NOME                        COL   36,00.
      *  01  O198.
      *      04  COLOR IS XXEXTSIS-USU-CORDIS.
      *      05  LABEL WDT8-DED                LINE + 01,00 COL   18,00.
      *      05  LABEL WHORA4ED                             COL   26,00.
      *      05  LABEL WSIGLA-USER                          COL   32,00.
      *      05  LABEL GEGENUSU-NOME                        COL   36,00.
      *  01  O199.
      *      04  COLOR IS XXEXTSIS-USU-CORDIS.
      *      05  LABEL WDT8-DED                LINE   11,30 COL   18,00.
      *      05  LABEL WHORA4ED                             COL   26,00.
      *      05  LABEL WSIGLA-USER                          COL   32,00.
      *      05  LABEL GEGENUSU-NOME                        COL   36,00.
      *................................................................*
         COPY "J:\PRGNEW\GE\BOK\GETBAR.SCR".
      *++COPY "J:\PRGNEW\GE\BOK\GETBAR3.SCR".
         COPY "J:\PRGNEW\GE\BOK\GETBARC.SCR".
         COPY "J:\PRGNEW\GE\BOK\GETBARB.SCR".
         05  PUSH-BUTTON "Au&ditoria"
                 COLUMN + 01,30 LINES 01,80 CELLS SIZE 16,00 CELLS
                 SELF-ACT
                 ENABLED = WTBAR-BOTAO01
                 TERMINATION-VALUE = 701
                 .
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

                MOVE 4256             TO XXEXTSIS-COM-CALLSUBNR.

         COPY "J:\PRGNEW\GE\BOK\GETSACE.PRO".

                DISPLAY FLOATING WINDOW
                 LINE -01,80 COL 02,00 LINES 14,60 SIZE 72,20
                 CONTROL FONT IS XXEXTSIS-COM-FONT-TELA
                 COLOR XXEXTSIS-USU-CORTEL
                 TITLE WTITULO
                 SYSTEM MENU
                 HANDLE IN WJANELA1.

                IF XXEXTSIS-COM-FUNCAO = "IN" OR = "DU"
                    MOVE 0            TO WTBAR-ANTESN
                    MOVE 0            TO WTBAR-PROXSN.
         COPY "J:\PRGNEW\GE\BOK\GEDETBAR.PRO".
         COPY "J:\PRGNEW\GE\BOK\GETBAR.PRO".
                DISPLAY T1.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.EMI"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.
         COPY "J:\PRGNEW\VD\BOK\VDCADBPD.EMIO"
              REPLACING =='VDCADBPD'== BY ==VDCADBPD==.
      *----------------------------------------------------------------*
                MOVE WPARAMETROS         TO WPARMVDCADBPD.
                MOVE "altera‡Æo"         TO WTITULO-FUNCAO.
                IF XXEXTSIS-COM-FUNCAO = "DU"
                    MOVE "duplica  "     TO WTITULO-FUNCAO.
                IF XXEXTSIS-COM-FUNCAO = "IN"
                    MOVE "inclusÆo "     TO WTITULO-FUNCAO.
                MOVE "99"                TO XXEXTSIS-COM-RETCODE.
                MODIFY WJANELA1 TITLE WTITULO.
      *----------------------------------------------------------------*
         LE-VDCADBPD.
                MOVE WPARMVDCADBPD-COD     TO VDCADBPD-COD.
                READ VDCADBPD.
                IF NOT XXEXTSIS-VALID-FSTATUS
                    MOVE "VDCADBPD"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    GO TO RETORNA
                END-IF.
      *----------------------------------------------------------------*
         LEU-VDCADBPD.
020611*----------------------------------------------------------------*
020611          INITIALIZE WGEGEFCRR.
020611          MOVE "VDCADBPD"          TO WGEGEFCRR-ARQUIVO.
020611          MOVE "CODIGO"            TO WGEGEFCRR-CAMPO.
020611          MOVE VDCADBPD-COD        TO WGEGEFCRR-PARM.
020611   COPY "J:\PRGNEW\GE\BOK\GETSALT.PRO".
020611*----------------------------------------------------------------*
                MOVE VDCADBPD-REGISTRO   TO LVDCADBPD-REGISTRO.
                MODIFY WJANELA1 TITLE WTITULO.
                MOVE XXEXTSIS-COM-CORMOS TO WCOR.
                MOVE 1                   TO WCHAVE-ENAB.
                DISPLAY C100.
      *----------------------------------------------------------------*
         LE-USERS.
      *         MOVE VDCADBPD-INCLUSER   TO WSIGLA-USER.
      *         MOVE VDCADBPD-INCLDT     TO WDT8.
      *         MOVE WDIA8               TO WDIA8-D.
      *+FFF+    MOVE WMES8               TO WMES8-D.
      *         MOVE WANO8               TO WANO8-D.
      *         MOVE WDT8-D              TO WDT8-DED.
      *         MOVE VDCADBPD-INCLHR     TO WHORMIN.
      *         MOVE WHOR                TO WHORED.
      *         MOVE WMIN                TO WMINED.
      *         MOVE ":"                 TO W2PONTOSHR.
      *         IF WHORA4ED = '00:00'
      *             MOVE SPACES          TO WHORA4ED.
      *         PERFORM LE-GEGENUSU    THRU FLE-GEGENUSU.
      *         DISPLAY O197.
      *
      *         MOVE VDCADBPD-ALTEDT     TO WDT8.
      *         MOVE WDIA8               TO WDIA8-D.
      *         MOVE WMES8               TO WMES8-D.
      *         MOVE WANO8               TO WANO8-D.
      *         MOVE WDT8-D              TO WDT8-DED.
      *         MOVE VDCADBPD-ALTEHR     TO WHORMIN.
      *         MOVE WHOR                TO WHORED.
      *         MOVE WMIN                TO WMINED.
      *         MOVE ":"                 TO W2PONTOSHR.
      *         IF WHORA4ED = '00:00'
      *             MOVE SPACES          TO WHORA4ED.
      *         MOVE VDCADBPD-ALTEUSER   TO WSIGLA-USER.
      *         PERFORM LE-GEGENUSU    THRU FLE-GEGENUSU.
      *         DISPLAY O198.
      *
      *         MOVE VDCADBPD-CANCDT     TO WDT8.
      *         MOVE WDIA8               TO WDIA8-D.
      *         MOVE WMES8               TO WMES8-D.
      *         MOVE WANO8               TO WANO8-D.
      *         MOVE WDT8-D              TO WDT8-DED.
      *         MOVE VDCADBPD-CANCHR     TO WHORMIN.
      *         MOVE WHOR                TO WHORED.
      *         MOVE WMIN                TO WMINED.
      *         MOVE ":"                 TO W2PONTOSHR.
      *         IF WHORA4ED = '00:00'
      *             MOVE SPACES          TO WHORA4ED.
      *         MOVE VDCADBPD-CANCUSER   TO WSIGLA-USER.
      *         PERFORM LE-GEGENUSU    THRU FLE-GEGENUSU.
      *         DISPLAY O199.
      *----------------------------------------------------------------*
      *         IF VDCADBPD-CANCSN     = 1
      *+FFF+        MOVE 084          TO XXX99SIS-COM-NRMSG
      *             CALL "GEMSG".
      *         MOVE VDCADBPD-CANCSN  TO WCANCSN-ANT.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GELITBAR.PRO".
                MOVE ALL "N"             TO WSCR-CAMPO-OKT.
         PEGA-DADOS.
                IF VDCADBPD-COD        >= 4       AND
                   VDCADBPD-COD        <= 9
                   MOVE XXEXTSIS-USU-CORNAC TO WCOR-EXCECAO-104
                   MOVE 0                   TO WDADOS-ENAB-104
                ELSE
                   MOVE XXEXTSIS-USU-CORTEL TO WCOR-EXCECAO-104
                   MOVE 1                   TO WDADOS-ENAB-104
                END-IF.
                MOVE XXEXTSIS-USU-CORAC  TO WCOR.
                MOVE 1                   TO WDADOS-ENAB.
                DISPLAY D100.
                ACCEPT  D100 BEFORE TIME XXEXTSIS-USU-TIMEOUT.
                MOVE XXEXTSIS-USU-CORNAC TO WCOR.
                MOVE 0                   TO WDADOS-ENAB.
                DISPLAY D100.
                IF WTECLA-ESC
                 IF VDCADBPD-REGISTRO = LVDCADBPD-REGISTRO
                    GO TO RETORNA
                 ELSE
                    MOVE 100          TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
                    GO TO RETORNA.
                IF WTECLA-INATI
                    MOVE 033          TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
                    GO TO RETORNA.
                IF WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE
                 IF VDCADBPD-REGISTRO = LVDCADBPD-REGISTRO
                    GO TO RETORNA
                 ELSE
                    MOVE 100          TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
                    GO TO RETORNA.
                IF NOT WTECLA-ENTER AND NOT WTECLA-AUTO AND
                   NOT WTECLA-ANTE  AND NOT WTECLA-PROX
                    GO TO PEGA-DADOS.
                MOVE " "              TO WBOTAO-FUNCAO.
                IF WTECLA-ANTE
                    MOVE "A"          TO WBOTAO-FUNCAO.
                IF WTECLA-PROX
                    MOVE "P"          TO WBOTAO-FUNCAO.
                MOVE SPACES           TO XXEXTSIS-COM-MSG.
                DISPLAY TMSGI UPON XXEXTSIS-COM-MWINDOW.

                MOVE 001              TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      002     TO WSCR-ID
                    PERFORM A-102   THRU FA-102
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.
                ADD  001              TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      003     TO WSCR-ID
                    PERFORM A-103   THRU FA-103
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.
                ADD  001              TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      004     TO WSCR-ID
                    PERFORM A-104   THRU FA-104
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.
      *----------------------------------------------------------------*
         CONFIRMA-PA.
                IF WBOTAO-FUNCAO NOT = "P" AND NOT = "A"
                    GO TO REGRAVA-VDCADBPD.
                IF VDCADBPD-REGISTRO = LVDCADBPD-REGISTRO
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
         REGRAVA-VDCADBPD.
      *         IF WCANCSN-ANT         = 0
      *           IF VDCADBPD-CANCSN   = 1
      *             ACCEPT VDCADBPD-CANCDT    FROM CENTURY-DATE
      *             ACCEPT WHORA              FROM TIME
      *             MOVE WHORMIN                TO VDCADBPD-CANCHR
      *             MOVE XXEXTSIS-USU-SIGLA     TO VDCADBPD-CANCSIGLA.
                ACCEPT VDCADBPD-LOGDT        FROM CENTURY-DATE.
                IF XXEXTSIS-COM-FUNCAO NOT = "DU" AND NOT = "IN"
                   ACCEPT VDCADBPD-ALTEDT    FROM CENTURY-DATE
                   ACCEPT WHORA              FROM TIME
                   MOVE WHORMIN              TO VDCADBPD-ALTEHR
                   MOVE XXEXTSIS-USU-SIGLA   TO VDCADBPD-ALTESIGLA.
               REWRITE VDCADBPD-REGISTRO.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE "VDCADBPD"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    GO TO RETORNA.
                MOVE "00"             TO XXEXTSIS-COM-RETCODE.

080317          INITIALIZE WPARMVDCADFLC.
080317          MOVE "VDCADBPD"       TO WPARMVDCADFLC-NOME-TABELA.
080317          MOVE VDCADBPD-CHAVE   TO WPARMVDCADFLC-CODIGO-ERP.
080317          IF XXEXTSIS-COM-FUNCAO = "DU" OR "IN"
080317             MOVE 0             TO WPARMVDCADFLC-OPERACAO
080317          ELSE
080317             MOVE 1             TO WPARMVDCADFLC-OPERACAO
080317          END-IF.
080317          MOVE "VDCABPDA"       TO WPARMVDCADFLC-PROGRAMA.
080317          MOVE WTPGM-VERSAO     TO WPARMVDCADFLC-VERSAO-PRG.
080317          CALL "VDCADFLC" USING WPARMVDCADFLC
080317                          ON EXCEPTION
080317                             INITIALIZE WPARMVDCADFLC
080317          END-CALL.

                IF XXEXTSIS-COM-FUNCAO = "DU" OR = "IN"
                    GO TO RETORNA.
      *----------------------------------------------------------------*
         GRAVA-LOG.
                MOVE "A"              TO XXEXTSIS-LOG-OPERACAO.
                MOVE SPACES           TO WLOG-DADOS.
                MOVE VDCADBPD-COD     TO WLOG-COD.
                MOVE WLOG-DADOS       TO XXEXTSIS-LOG-DADOS.
                CALL "GELOG".
      *----------------------------------------------------------------*
         GRAVA-GEGENLAC.
                IF VDCADBPD-DESCR          NOT = LVDCADBPD-DESCR
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCADBPD"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Descr. Mot. Bloq Ped" TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCADBPD-DESCR        TO XXEXTSIS-LAC-VLRX-ANTE
                  MOVE VDCADBPD-DESCR         TO XXEXTSIS-LAC-VLRX-NOVO
                  CALL "GELAC".
                IF VDCADBPD-SIGLA          NOT = LVDCADBPD-SIGLA
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCADBPD"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Sigla Mot. Bloq. Ped" TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCADBPD-SIGLA        TO XXEXTSIS-LAC-VLRX-ANTE
                  MOVE VDCADBPD-SIGLA         TO XXEXTSIS-LAC-VLRX-NOVO
                  CALL "GELAC".
                IF VDCADBPD-BLQPED         NOT = LVDCADBPD-BLQPED
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCADBPD"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Flag Bloqueio Pedido" TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCADBPD-BLQPED       TO XXEXTSIS-LAC-VLR9-ANTE
                  MOVE VDCADBPD-BLQPED        TO XXEXTSIS-LAC-VLR9-NOVO
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
                READ VDCADBPD PREVIOUS.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE "VDCADBPD"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    START VDCADBPD KEY >= VDCADBPD-CHAVE
                    READ VDCADBPD.
                GO TO LEU-VDCADBPD.
      *----------------------------------------------------------------*
         MOSTRA-PROXIMO.
                READ VDCADBPD NEXT.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE "VDCADBPD"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    START VDCADBPD KEY <= VDCADBPD-CHAVE
                    READ VDCADBPD.
                GO TO LEU-VDCADBPD.
      *----------------------------------------------------------------*
         B-D100.
         COPY "J:\PRGNEW\GE\BOK\GEDETBAR.PRO".
                IF XXEXTSIS-COM-FUNCAO NOT = "DU" AND NOT = "IN"
                   MOVE WTBAR-BOTAO01SN TO WTBAR-BOTAO01.
                INITIALIZE  XXEXTSIS-COM-MSG.
                EVALUATE WSCR-CAMPO
                WHEN 001
                  MOVE "DESCRI€ÇO DO MOTIVO DE BLOQUEIO DE PEDIDOS     "
                                      TO XXEXTSIS-COM-MSG
                WHEN 002
                  MOVE "SIGLA DO MOTIVO DE BLOQUEIO DE PEDIDOS         "
                                      TO XXEXTSIS-COM-MSG
                WHEN 003
                  MOVE "FLAG PARA BLOQUEIO DE PEDIDOS - 'SIM' OU 'NÇO' "
                                      TO XXEXTSIS-COM-MSG
                WHEN 004
                  MOVE "TECLE ENTER OU CLICK NO BOTÇO PARA CONFIRMAR   "
                                      TO XXEXTSIS-COM-MSG
                WHEN 005
                  MOVE "TECLE ENTER OU CLICK NO BOTÇO PARA CANCELAR    "
                                      TO XXEXTSIS-COM-MSG
                WHEN 006
                 MOVE "CLICK NO BOTÇO PARA ATUALIZAR E IR PARA ANTERIOR"
                                      TO XXEXTSIS-COM-MSG
                WHEN 007
                  MOVE "CLICK NO BOTÇO PARA ATUALIZAR E IR PARA PRàXIMO"
                                      TO XXEXTSIS-COM-MSG
                END-EVALUATE.
                DISPLAY T101A UPON WTBAR.
                DISPLAY TMSGI UPON XXEXTSIS-COM-MWINDOW.
      *----------------------------------------------------------------*
         A-102.
                PERFORM TESTA-OPCAO-T1    THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO              NOT = ZEROS
                    GO TO FA-102.
                PERFORM VE-TECLA          THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-102.
                IF WSCR-ERRO              NOT = ZEROS
                    GO TO FA-102.
                IF VDCADBPD-DESCR         = SPACES
                    MOVE 071              TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
                    MOVE 1                TO WSCR-ERRO
                    GO TO FA-102.
                MOVE "S"                  TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-102.
                EXIT.
      *----------------------------------------------------------------*
         A-103.
                PERFORM TESTA-OPCAO-T1    THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO              NOT = ZEROS
                    GO TO FA-103.
                PERFORM VE-TECLA          THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-103.
                IF WSCR-ERRO              NOT = ZEROS
                    GO TO FA-103.
                MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-103.
                EXIT.
      *----------------------------------------------------------------*
         A-104.
                PERFORM TESTA-OPCAO-T1    THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO              NOT = ZEROS
                    GO TO FA-104.
                PERFORM VE-TECLA          THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-104.
                IF WSCR-ERRO              NOT = ZEROS
                    GO TO FA-104.
                MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-104.
                EXIT.
      *----------------------------------------------------------------*
         E-104.
                IF WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE
                 IF VDCADBPD-REGISTRO = LVDCADBPD-REGISTRO
                    GO TO RETORNA
                 ELSE
                    MOVE 100             TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
                    GO TO RETORNA
                 END-IF
                END-IF.
                MOVE 4                 TO WSCR-ERRO.
                ADD  1                 TO WSCR-ID.
                MOVE "S"               TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FE-104.
                EXIT.
      *----------------------------------------------------------------*
         TESTA-OPCAO-T1.
                IF WTECLA-BOTAO01
                   INITIALIZE WPARMGEGENAUD
                   MOVE WTITULO         TO WPARMGEGENAUD-TITULO
                   MOVE VDCADBPD-AUDIT  TO WPARMGEGENAUD-AUDIT
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
         COPY "J:\PRGNEW\GE\BOK\GEUSULER.PRO".
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GETECLA.PRO".
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GEPERFOR.PRO".
      *----------------------------------------------------------------*
         RETORNA.
         COPY "J:\PRGNEW\VD\BOK\VDCADBPD.ECLO"
              REPLACING =='VDCADBPD'== BY ==VDCADBPD==.
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.ECLO"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.

         RETORNAF.
         COPY "J:\PRGNEW\VD\BOK\VDRESUB.PRO".
                EXIT PROGRAM.
      *================================================================*
