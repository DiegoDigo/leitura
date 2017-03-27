         IDENTIFICATION DIVISION.
         PROGRAM-ID.    VDCABLOA.
         SECURITY.
      ******************************************************************
      * ALTERA MOTIVOS DE BLOQUEIO DE CLIENTES                         *
      ******************************************************************
      *--------------  O B S E R V A C O E S  -------------------------*
      * - *+FFF+ LIBERAR NO FUTURO                                     *
      * -                                                              *
      *--------------  A L T E R A C O E S  ---------------------------*
      * 06/09/2005 COPIADO DO VDCADEVA.CBL                    CRISTIANO*
      * 16/03/2006 ALTERA€ÇO DE PIC P/ O CPO VDCADBLO-COD     CRISTIANO*
      * 17/03/2006 INVERSÇO DA POSI€ÇO DOS BOTåES "PRàXIMO" E          *
      *            "ANTERIOR" NA TELA                         CRISTIANO*
      * 29/05/2006 ACERTO DAS ROTINAS "MOSTRA-ANTERIOR" E              *
      *            "MOSTRA-PROXIMO"                           CRISTIANO*
      * 29/06/2006 INCLUSÇO DO CPO IMPCLIPALM (IMPORTA€ÇO PARA         *
      *            PALMTOP) NO ARQUIVO E NA TELA              CRISTIANO*
      * 21/09/2006 INVERTIDO HELP NO B-D100 DE ANTERIOR/PROXIMO ROBERTO*
      * 29/12/2006 INCLUIDO CAMPO "PERMITIR VENDA"              MARCELO*
      * 05/09/2007 INCLUIDO CAMPO "BLOQUEADO MGM"               MARCELO*
      * 21/11/2007 CONFORME SOLICITADO NA O.S.141650            MARCELO*
      *            NAO PERMITIR ACESSAR O CAMPO "BLOQUEADO-MGM"        *
      *            NA INCLUSAO, SOMENTE SE FOR USUARIO CTR.            *
      *            QUANDO FOR ALTERACAO, NAO PERMITIR ACESSAR O CAMPO  *
      *            QUANDO FOR LICENCA NORSA E USUARIO DIFERENTE CTR.   *
      * 02/09/2008 QUANDO FOR WSPAD-LICENCA-DA-FABRICA          MARCELO*
      *            FOI COLOCADO FIXO OS CODCIA E ALGUNS                *
      *            CAMPOS FICARAM INACESSIVEIS COM DEFAULT DESMARCADOS.*
      * 24/09/2008 QUANDO FOR XXEXTSIS-CLI-BANDEIRA = 001       MARCELO*
      *            VALE A REGRA ACIMA.                                 *
      * 25/10/2008 AJUSTES                                      MARCELO*
      * 16/08/2010 CONVERSÇO VDCADBLO-COD DE X(1) PARA X(3)      VALDIR*10.10.016
      *                    E VDCADBLO-MOTBLQCIA DE X(1) PARA X(3)      *
      * 23/05/2011 AJUSTE NAS FD, SELECT, PRM, OPENS, E CLOSE     KENDY*11.05.017
      *    PARA O PADRÇO NOVO                                          *
      * 03/06/2011 TRATAMENTO DE FAIXA RESTRITA                   KENDY*11.06.018
      * 27/07/2011 TRATAMENTO CAMPOS EXCLUSIVOS P/ FAB.SCHIN    MARCELO*11.07.019
      *            WDADOS-ENAB-BLOQ=0                                  *
      *            (REVENDAS SCHIN NAO PODERAO ACESSAR ESSES CAMPOS).  *
      * 02/04/2012 AJUSTE.                                      MARCELO*12.04.020
      * 12/06/2012 NOVO CAMPO DE COD COMPANHIA                    KENDY*12.06.021
      * 12/07/2012 RECOMPILADO.                                 MARCELO*12.07.022
      * 23/07/2012 AJUSTE MENSAGEM DENTRO DA ROT GETSALTE.PRO   MARCELO*12.07.023
      *            QUANDO FOR FABRICA E ESTIVER ALTERANDO REGISTRO     *
      *            FORA DE SUA FAIXA, SERA APRESENTADA MSG:            *
      *            "ALTERACAO FABRICA FORA DE SUA FAIXA:"              *
      *            "FAIXA RESTRITA: XXXXXXXXX ATE XXXXXXXXX"           *
      * 05/10/2012 REFERENTE A O.S. - 384.176                     KENDY*12.10.025
      * 18/12/2013 RECOMPILADO                                    KENDY*13.12.026
      * 20/01/2014 OS-2581: BANDEIRA=012 (NORSA)                MARCELO*14.01.027
      *                     BANDEIRA=018 (SOLAR)                       *
      *                     BANDEIRA=015 (GUARARAPES)                  *
      *                     BANDEIRA=013 (RENOSA)                      *
      *                     BANDEIRA=014 (RENOSA-MT)                   *
      *                     BANDEIRA=021 (KIBON)                       *
      *                     BANDEIRA=010 (VONPAR)                      *
      *                     BANDEIRA=016 (SIMOES)                      *
      *                     BANDEIRA=031 (FEMSA)                       *
      
      * 08/03/2017 OS-8643: CONTROLE DE MANUTENCAO DE CADASTROS        *
      *                     CHAMADA AO VDCADFLC PARA REGISTRAR         *
      *                     INC/ALT/EXC/DUP PARA ALIMENTAR             *
      *                     ARQUIVO NOVO "VDCADFLC.VFE"                *
      *                     QUE SERVIRA TAMBEM PARA O PORTAL.     DIEGO*17.03.028
      *================================================================*
         ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
         SPECIAL-NAMES.
         COPY "J:\PRGNEW\GE\BOK\GESPECI.ENV".
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.ESDA"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.
         COPY "J:\PRGNEW\VD\BOK\VDCADBLO.ESDA"
              REPLACING =='VDCADBLO'== BY ==VDCADBLO==.
      *================================================================*
      /
      *================================================================*
         DATA DIVISION.
         FILE SECTION.
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.EFDE"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.
         COPY "J:\PRGNEW\VD\BOK\VDCADBLO.EFDE"
              REPLACING =='VDCADBLO'== BY ==VDCADBLO==.
      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         WORKING-STORAGE SECTION.
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         77 WTPGM-HEADER        PIC  X(019) VALUE "=>CONTROLE VERSAO<=".
         77 WTPGM-VERSAO        PIC  X(009) VALUE "17.03.028".
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
         77  WX-VDCADBLO-POSITIVA PIC 9(001)     VALUE ZEROS.
         77  WDADOS-ENAB-MGM      PIC 9(001)     VALUE ZEROS.
         77  WDADOS-ENAB-POSIT    PIC 9(001)     VALUE ZEROS.
         77  WDADOS-ENAB-CLIENTE  PIC 9(001)     VALUE ZEROS.
         77  WDADOS-ENAB-VENDA    PIC 9(001)     VALUE ZEROS.

         COPY "J:\PRGNEW\GE\BOK\GEWORK.WOR".
         COPY "J:\PRGNEW\VD\BOK\VDWORK.WOR".
         01  GERAL.
         03  WLOG-DADOS         PIC  X(080) VALUE SPACES.
         03  WLOG-DADOSR        REDEFINES WLOG-DADOS.
             05  WLOG-COD       PIC  X(003).
         03  WTITULO.
             05  F              PIC  X(050)  VALUE
                 " Motivos de Bloqueio de Clientes".
             05  F              PIC  X(020)  VALUE      SPACES.
             05  WTITULO-FUNCAO PIC  X(032)  VALUE
                 "altera‡Æo".
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\VD\BOK\VDCADBLO.EFDE"
              REPLACING ==XFD FILE="'VDCADBLO'.VFE"== BY == ==,
                        ==FD  'VDCADBLO'== BY == ==,
                        ==LABEL RECORD IS STANDARD.== BY == ==,
                        =='VDCADBLO'== BY ==LVDCADBLO==.
      *----------------------------------------------------------------*
         01  PARAMETROS.
         COPY "J:\PRGNEW\GE\BOK\GEGENAUD.EPRM"
              REPLACING =='GEGENAUD'== BY ==GEGENAUD==.
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.EPRM"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.
         COPY "J:\PRGNEW\VD\BOK\VDCADBLO.EPRM"
              REPLACING =='VDCADBLO'== BY ==VDCADBLO==.
              
              
080317   01   AREAS-EPRM-VDCADFLC.
080317   COPY "J:\PRGNEW\VD\BOK\VDCADFLC.EPRM"
080317        REPLACING =='VDCADFLC'== BY ==VDCADFLC==.
      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\XXEXTSIS.EXT".
         COPY "J:\PRGNEW\VFEV99\AJUDA\WSPAD.".
         COPY "J:\PRGNEW\VFEV99\AJUDA\WSPAD2.".
      *----------------------------------------------------------------*
         LINKAGE SECTION.
         01  WPARAMETROS        PIC  X(100).
      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         SCREEN SECTION.
         01  T1.
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  BLANK SCREEN.
             05  FRAME
120612           LINE   01,00 LINES 19,25 CELLS
                 COL    01,60 SIZE  71,00 CELLS
                 .
             05  FRAME
                 LINE + 00,60 LINES 02,70 CELLS
                 COL    02,40 SIZE  69,30 CELLS
                 LOWERED   ALTERNATE
                 .
             05  FRAME
120612           LINE + 02,50 LINES 15,50 CELLS
                 COL    02,40 SIZE  69,30 CELLS
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
             05  LABEL "03-Descri‡Æo Reduzida"
                 LINE + 01,50 COL   04,00
                 .
051012* *    05  LABEL "04-Positiva‡Æo"
051012       05  LABEL "04-Cobertura"
                 LINE + 01,50 COL   04,00
                 .
             05  LABEL "05-C¢d. de Bloq. da CIA"
                 LINE + 01,50 COL   04,00
                 .
             05  LABEL "06-Envia Cliente p/ Palm"
                 LINE + 01,50 COL   04,00
                 .
             05  LABEL "07-Permitir Venda"
                 LINE + 01,50 COL   04,00
                 .
             05  LABEL "08-Bloqueado MGM"
                 LINE + 01,50 COL   04,00
                 .
120612       05  LABEL "09-C¢digo companhia"
120612           LINE + 01,50 COL   04,00
120612           .
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
                 PIC X(003)           USING VDCADBLO-COD
                 ENABLED = 0
                 3-D
                 AUTO
                 .
      *................................................................*
        01  D100 BEFORE B-D100.
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE   05,00 COL   25,00 LINES 01,20 CELLS
                 PIC X(030)           USING VDCADBLO-DESCR
                 AFTER A-102  THRU FA-102
                      ID 002
270711           ENABLED = WDADOS-ENAB-BLOQ
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   25,00 LINES 01,20 CELLS
                 PIC X(015)           USING VDCADBLO-DESCRRED
                 AFTER A-103  THRU FA-103
                      ID 003
270711           ENABLED = WDADOS-ENAB-BLOQ
                 3-D
                 AUTO
                 .
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  CHECK-BOX "Si&m"
                 LINE + 01,50 COL   25,00
                 USING WX-VDCADBLO-POSITIVA
                 AFTER     A-104  THRU FA-104
                          ID 004
                 EXCEPTION E-104  THRU FE-104
                 ENABLED = WDADOS-ENAB-POSIT
                 NOTIFY
                 .
      *      04  COLOR IS XXEXTSIS-USU-CORTEL.
      *      05  ENTRY-FIELD
      *          LINE + 01,50 COL   25,00 LINES 01,20 CELLS
      *          PIC X(001)           USING VDCADBLO-POSITIVA
      *          AFTER A-104  THRU FA-104
      *               ID 004
      *          ENABLED = WDADOS-ENAB
      *          3-D
      *          AUTO
      *          .
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE + 01,50 COL   25,00 LINES 01,20 CELLS
                 PIC X(003)           USING VDCADBLO-MOTBLQCIA
                 AFTER A-105  THRU FA-105
                      ID 005
270711           ENABLED = WDADOS-ENAB-BLOQ
                 3-D
                 AUTO
                 UPPER
                 .
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  CHECK-BOX "&Sim"
                 LINE + 01,50 COL   25,00
                 USING VDCADBLO-IMPCLIPALM
                 AFTER     A-106  THRU FA-106
                          ID 006
                 EXCEPTION E-106  THRU FE-106
                 ENABLED = WDADOS-ENAB-CLIENTE
                 NOTIFY
                 .
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  CHECK-BOX "S&im"
                 LINE + 01,50 COL   25,00
                 USING VDCADBLO-VENDA
                 AFTER     A-107  THRU FA-107
                          ID 007
                 EXCEPTION E-107  THRU FE-107
                 ENABLED = WDADOS-ENAB-VENDA
                 NOTIFY
                 .
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  CHECK-BOX "Si&m"
                 LINE + 01,50 COL   25,00
                 USING VDCADBLO-BLOQ-MGM
                 AFTER     A-108  THRU FA-108
                          ID 008
                 EXCEPTION E-108  THRU FE-108
                 ENABLED = WDADOS-ENAB-MGM
                 NOTIFY
                 .
120612       04  COLOR IS WCOR.
120612       05  ENTRY-FIELD
120612           LINE + 01,50 COL   25,00 LINES 01,20 CELLS
120612           PIC X(003)           USING VDCADBLO-CODCIA
120612           AFTER A-109  THRU FA-109
120612                ID 009
120612           ENABLED = WDADOS-ENAB
120612           3-D
120612           AUTO
120612           UPPER
120612           .
      *................................................................*
             05  PUSH-BUTTON "  &OK   "
                 LINE + 01,60 LINES 02,00 CELLS
                 COL    25,00 SIZE  11,00 CELLS
                 TERMINATION-VALUE = 013
120612                ID 010
                 ENABLED = WDADOS-ENAB
                 SELF-ACT
                 .
             05  PUSH-BUTTON "&CANCELA"
                              LINES 02,00 CELLS
                 COL  + 01,50 SIZE  11,00 CELLS
                 TERMINATION-VALUE = 027
120612                ID 011
                 ENABLED = WDADOS-ENAB
                 SELF-ACT
                 .
             05  PUSH-BUTTON "&ANTERIOR"
                              LINES 02,00 CELLS
                 COL  + 01,50 SIZE  11,00 CELLS
                 TERMINATION-VALUE = 799
120612                ID 012
                 ENABLED = WTBAR-ANTE
                 SELF-ACT
                 .
             05  PUSH-BUTTON "&PRàXIMO"
                              LINES 02,00 CELLS
                 COL  + 01,50 SIZE  11,00 CELLS
                 TERMINATION-VALUE = 798
120612                ID 013
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

                MOVE 4238             TO XXEXTSIS-COM-CALLSUBNR.

         COPY "J:\PRGNEW\GE\BOK\GETSACE.PRO".

                DISPLAY FLOATING WINDOW
120612           LINE -01,80 COL 02,00 LINES 19,80 SIZE 72,20
                 CONTROL FONT IS XXEXTSIS-COM-FONT-TELA
                 COLOR XXEXTSIS-USU-CORTEL
                 TITLE WTITULO
                 SYSTEM MENU
                 HANDLE IN WJANELA1.

211107          MOVE 1                TO WDADOS-ENAB-MGM.
200114          IF XXEXTSIS-CLI-BANDEIRA = 012 OR                        (NORSA)
200114             XXEXTSIS-CLI-BANDEIRA = 018                           (SOLAR)
211107             IF (XXEXTSIS-COM-FUNCAO = "IN" OR = "DU") AND
211107                WSPAD-USUARIO NOT = "CTR"
211107                MOVE 0          TO WDADOS-ENAB-MGM.

200114          IF XXEXTSIS-CLI-BANDEIRA = 012 OR                        (NORSA)
200114             XXEXTSIS-CLI-BANDEIRA = 018                           (SOLAR)
211107             IF (XXEXTSIS-COM-FUNCAO NOT = "IN" AND "DU") AND
211107                WSPAD-USUARIO NOT = "CTR"
211107                MOVE 0          TO WDADOS-ENAB-MGM.

                IF XXEXTSIS-COM-FUNCAO = "IN" OR = "DU"
                    MOVE 0            TO WTBAR-ANTESN
                    MOVE 0            TO WTBAR-PROXSN.
         COPY "J:\PRGNEW\GE\BOK\GEDETBAR.PRO".
         COPY "J:\PRGNEW\GE\BOK\GETBAR.PRO".
                DISPLAY T1.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.EMI"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.
         COPY "J:\PRGNEW\VD\BOK\VDCADBLO.EMIO"
              REPLACING =='VDCADBLO'== BY ==VDCADBLO==.
      *----------------------------------------------------------------*
                MOVE WPARAMETROS         TO WPARMVDCADBLO.
                MOVE "altera‡Æo"         TO WTITULO-FUNCAO.
                IF XXEXTSIS-COM-FUNCAO = "DU"
                    MOVE "duplica  "     TO WTITULO-FUNCAO.
                IF XXEXTSIS-COM-FUNCAO = "IN"
                    MOVE "inclusÆo "     TO WTITULO-FUNCAO.
                MOVE "99"                TO XXEXTSIS-COM-RETCODE.
                MODIFY WJANELA1 TITLE WTITULO.
      *----------------------------------------------------------------*
         LE-VDCADBLO.
                MOVE WPARMVDCADBLO-COD     TO VDCADBLO-COD.
                READ VDCADBLO.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE "VDCADBLO"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    GO TO RETORNA.
      *----------------------------------------------------------------*
         LEU-VDCADBLO.
030611*----------------------------------------------------------------*
030611          INITIALIZE WGEGEFCRR.
030611          MOVE "VDCADBLO"          TO WGEGEFCRR-ARQUIVO.
030611          MOVE "CODIGO"            TO WGEGEFCRR-CAMPO.
030611          MOVE VDCADBLO-COD        TO WGEGEFCRR-PARM.
030611   COPY "J:\PRGNEW\GE\BOK\GETSALT.PRO".
030611*----------------------------------------------------------------*
      *---------Conforme OS.186972
                IF XXEXTSIS-CLI-BANDEIRA = 001 AND
                   VDCADBLO-MOTBLQCIA = "F"
                    MOVE "N" TO VDCADBLO-POSITIVA
                    MOVE 0   TO VDCADBLO-IMPCLIPALM
                                VDCADBLO-VENDA
                    ACCEPT VDCADBLO-LOGDT        FROM CENTURY-DATE
                    IF XXEXTSIS-COM-FUNCAO NOT = "DU" AND NOT = "IN"
                       ACCEPT VDCADBLO-ALTEDT    FROM CENTURY-DATE
                       ACCEPT WHORA              FROM TIME
                       MOVE WHORMIN              TO VDCADBLO-ALTEHR
                       MOVE XXEXTSIS-USU-SIGLA   TO VDCADBLO-ALTESIGLA
                    END-IF
                    REWRITE VDCADBLO-REGISTRO
                END-IF.

                MOVE VDCADBLO-REGISTRO   TO LVDCADBLO-REGISTRO.
                IF VDCADBLO-POSITIVA = "S"
                   MOVE 1 TO WX-VDCADBLO-POSITIVA
                ELSE
                   MOVE 0 TO WX-VDCADBLO-POSITIVA.
                MODIFY WJANELA1 TITLE WTITULO.
                MOVE XXEXTSIS-COM-CORMOS TO WCOR.
                MOVE 1                   TO WCHAVE-ENAB.
                DISPLAY C100.
      *----------------------------------------------------------------*
         LE-USERS.
      *         MOVE VDCADBLO-INCLUSER   TO WSIGLA-USER.
      *         MOVE VDCADBLO-INCLDT     TO WDT8.
      *         MOVE WDIA8               TO WDIA8-D.
      *+FFF+    MOVE WMES8               TO WMES8-D.
      *         MOVE WANO8               TO WANO8-D.
      *         MOVE WDT8-D              TO WDT8-DED.
      *         MOVE VDCADBLO-INCLHR     TO WHORMIN.
      *         MOVE WHOR                TO WHORED.
      *         MOVE WMIN                TO WMINED.
      *         MOVE ":"                 TO W2PONTOSHR.
      *         IF WHORA4ED = '00:00'
      *             MOVE SPACES          TO WHORA4ED.
      *         PERFORM LE-GEGENUSU    THRU FLE-GEGENUSU.
      *         DISPLAY O197.
      *
      *         MOVE VDCADBLO-ALTEDT     TO WDT8.
      *         MOVE WDIA8               TO WDIA8-D.
      *         MOVE WMES8               TO WMES8-D.
      *         MOVE WANO8               TO WANO8-D.
      *         MOVE WDT8-D              TO WDT8-DED.
      *         MOVE VDCADBLO-ALTEHR     TO WHORMIN.
      *         MOVE WHOR                TO WHORED.
      *         MOVE WMIN                TO WMINED.
      *         MOVE ":"                 TO W2PONTOSHR.
      *         IF WHORA4ED = '00:00'
      *             MOVE SPACES          TO WHORA4ED.
      *         MOVE VDCADBLO-ALTEUSER   TO WSIGLA-USER.
      *         PERFORM LE-GEGENUSU    THRU FLE-GEGENUSU.
      *         DISPLAY O198.
      *
      *         MOVE VDCADBLO-CANCDT     TO WDT8.
      *         MOVE WDIA8               TO WDIA8-D.
      *         MOVE WMES8               TO WMES8-D.
      *         MOVE WANO8               TO WANO8-D.
      *         MOVE WDT8-D              TO WDT8-DED.
      *         MOVE VDCADBLO-CANCHR     TO WHORMIN.
      *         MOVE WHOR                TO WHORED.
      *         MOVE WMIN                TO WMINED.
      *         MOVE ":"                 TO W2PONTOSHR.
      *         IF WHORA4ED = '00:00'
      *             MOVE SPACES          TO WHORA4ED.
      *         MOVE VDCADBLO-CANCUSER   TO WSIGLA-USER.
      *         PERFORM LE-GEGENUSU    THRU FLE-GEGENUSU.
      *         DISPLAY O199.
      *----------------------------------------------------------------*
      *         IF VDCADBLO-CANCSN     = 1
      *+FFF+        MOVE 084          TO XXX99SIS-COM-NRMSG
      *             CALL "GEMSG".
      *         MOVE VDCADBLO-CANCSN  TO WCANCSN-ANT.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GELITBAR.PRO".
                MOVE ALL "N"             TO WSCR-CAMPO-OKT.
         PEGA-DADOS.
                MOVE XXEXTSIS-USU-CORAC  TO WCOR.
                MOVE 1                   TO WDADOS-ENAB
                                            WDADOS-ENAB-POSIT
                                            WDADOS-ENAB-CLIENTE
                                            WDADOS-ENAB-VENDA.

                IF XXEXTSIS-CLI-BANDEIRA = 001 AND
                   VDCADBLO-MOTBLQCIA = "F"
                     MOVE 0           TO WDADOS-ENAB-POSIT
                                         WDADOS-ENAB-CLIENTE
                                         WDADOS-ENAB-VENDA
                     MOVE "N"         TO VDCADBLO-POSITIVA
                     MOVE 0           TO WX-VDCADBLO-POSITIVA
                                         VDCADBLO-IMPCLIPALM
                                         VDCADBLO-VENDA
                END-IF.

                DISPLAY D100.
                ACCEPT  D100 BEFORE TIME XXEXTSIS-USU-TIMEOUT.
                MOVE XXEXTSIS-USU-CORNAC TO WCOR.
                MOVE 0                   TO WDADOS-ENAB.
                DISPLAY D100.
                IF WTECLA-ESC
                 IF VDCADBLO-REGISTRO = LVDCADBLO-REGISTRO
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
                 IF VDCADBLO-REGISTRO = LVDCADBLO-REGISTRO
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
                ADD  001              TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      005     TO WSCR-ID
                    PERFORM A-105   THRU FA-105
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.
                ADD  001              TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      006     TO WSCR-ID
                    PERFORM A-106   THRU FA-106
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.
                ADD  001              TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      007     TO WSCR-ID
                    PERFORM A-107   THRU FA-107
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.
                ADD  001              TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      008     TO WSCR-ID
                    PERFORM A-108   THRU FA-108
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.
120612          ADD  001              TO WSCR-CAMPO.
120612          IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
120612              MOVE      009     TO WSCR-ID
120612              PERFORM A-109   THRU FA-109
120612              IF WSCR-ERRO NOT = ZEROS
120612                  GO TO PEGA-DADOS.
      *----------------------------------------------------------------*
         CONFIRMA-PA.
                IF WBOTAO-FUNCAO NOT = "P" AND NOT = "A"
                    GO TO REGRAVA-VDCADBLO.
                IF VDCADBLO-REGISTRO = LVDCADBLO-REGISTRO
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
         REGRAVA-VDCADBLO.
      *         IF WCANCSN-ANT         = 0
      *           IF VDCADBLO-CANCSN   = 1
      *             ACCEPT VDCADBLO-CANCDT    FROM CENTURY-DATE
      *             ACCEPT WHORA              FROM TIME
      *             MOVE WHORMIN                TO VDCADBLO-CANCHR
      *             MOVE XXEXTSIS-USU-SIGLA     TO VDCADBLO-CANCSIGLA.
                ACCEPT VDCADBLO-LOGDT        FROM CENTURY-DATE.
                IF XXEXTSIS-COM-FUNCAO NOT = "DU" AND NOT = "IN"
                   ACCEPT VDCADBLO-ALTEDT    FROM CENTURY-DATE
                   ACCEPT WHORA              FROM TIME
                   MOVE WHORMIN              TO VDCADBLO-ALTEHR
                   MOVE XXEXTSIS-USU-SIGLA   TO VDCADBLO-ALTESIGLA.
                IF WX-VDCADBLO-POSITIVA = 1
                   MOVE "S"            TO VDCADBLO-POSITIVA
                ELSE
                   MOVE "N"            TO VDCADBLO-POSITIVA.
                REWRITE VDCADBLO-REGISTRO.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE "VDCADBLO"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    GO TO RETORNA.
                MOVE "00"             TO XXEXTSIS-COM-RETCODE.
                
080317          INITIALIZE WPARMVDCADFLC.
080317          MOVE "VDCADBLO"       TO WPARMVDCADFLC-NOME-TABELA.
080317          MOVE VDCADBLO-CHAVE   TO WPARMVDCADFLC-CODIGO-ERP.
080317          IF XXEXTSIS-COM-FUNCAO = "DU" OR "IN"
080317             MOVE 0             TO WPARMVDCADFLC-OPERACAO
080317          ELSE
080317             MOVE 1             TO WPARMVDCADFLC-OPERACAO
080317          END-IF.
080317          MOVE "VDCABLOA"       TO WPARMVDCADFLC-PROGRAMA.
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
                MOVE VDCADBLO-COD     TO WLOG-COD.
                MOVE WLOG-DADOS       TO XXEXTSIS-LOG-DADOS.
                CALL "GELOG".
      *----------------------------------------------------------------*
         GRAVA-GEGENLAC.
                IF VDCADBLO-DESCR          NOT = LVDCADBLO-DESCR
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCADBLO"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Descr. Motivo Troca " TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCADBLO-DESCR        TO XXEXTSIS-LAC-VLRX-ANTE
                  MOVE VDCADBLO-DESCR         TO XXEXTSIS-LAC-VLRX-NOVO
                  CALL "GELAC".
                IF VDCADBLO-DESCRRED       NOT = LVDCADBLO-DESCRRED
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCADBLO"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "D.Red. Motivo Troca " TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCADBLO-DESCRRED     TO XXEXTSIS-LAC-VLRX-ANTE
                  MOVE VDCADBLO-DESCRRED      TO XXEXTSIS-LAC-VLRX-NOVO
                  CALL "GELAC".
                IF VDCADBLO-POSITIVA      NOT = LVDCADBLO-POSITIVA
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCADBLO"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Positiva‡Æo         " TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCADBLO-POSITIVA     TO XXEXTSIS-LAC-VLRX-ANTE
                  MOVE VDCADBLO-POSITIVA      TO XXEXTSIS-LAC-VLRX-NOVO
                  CALL "GELAC".
                IF VDCADBLO-MOTBLQCIA     NOT = LVDCADBLO-MOTBLQCIA
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCADBLO"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Motivo Bloq. da CIA " TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCADBLO-MOTBLQCIA    TO XXEXTSIS-LAC-VLRX-ANTE
                  MOVE VDCADBLO-MOTBLQCIA     TO XXEXTSIS-LAC-VLRX-NOVO
                  CALL "GELAC".
                IF VDCADBLO-IMPCLIPALM    NOT = LVDCADBLO-IMPCLIPALM
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCADBLO"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Imp. Cliente p/ Palm" TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCADBLO-IMPCLIPALM   TO XXEXTSIS-LAC-VLR9-ANTE
                  MOVE VDCADBLO-IMPCLIPALM    TO XXEXTSIS-LAC-VLR9-NOVO
                  CALL "GELAC".
                IF VDCADBLO-VENDA         NOT = LVDCADBLO-VENDA
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCADBLO"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Permitir Venda      " TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCADBLO-VENDA        TO XXEXTSIS-LAC-VLR9-ANTE
                  MOVE VDCADBLO-VENDA         TO XXEXTSIS-LAC-VLR9-NOVO
                  CALL "GELAC".
                IF VDCADBLO-BLOQ-MGM      NOT = LVDCADBLO-BLOQ-MGM
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCADBLO"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Bloqueado MGM       " TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCADBLO-BLOQ-MGM     TO XXEXTSIS-LAC-VLR9-ANTE
                  MOVE VDCADBLO-BLOQ-MGM      TO XXEXTSIS-LAC-VLR9-NOVO
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
                READ VDCADBLO PREVIOUS.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE "VDCADBLO"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    START VDCADBLO KEY >= VDCADBLO-CHAVE
                    READ VDCADBLO.
                GO TO LEU-VDCADBLO.
      *----------------------------------------------------------------*
         MOSTRA-PROXIMO.
                READ VDCADBLO NEXT.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE "VDCADBLO"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    START VDCADBLO KEY <= VDCADBLO-CHAVE
                    READ VDCADBLO.
                GO TO LEU-VDCADBLO.
      *----------------------------------------------------------------*
         B-D100.
         COPY "J:\PRGNEW\GE\BOK\GEDETBAR.PRO".
                IF XXEXTSIS-COM-FUNCAO NOT = "DU" AND NOT = "IN"
                   MOVE WTBAR-BOTAO01SN TO WTBAR-BOTAO01.
                INITIALIZE  XXEXTSIS-COM-MSG.
                EVALUATE WSCR-CAMPO
                WHEN 001
                  MOVE "DESCRI€ÇO DO MOTIVO DE BLOQUEIO DE CLIENTES    "
                                      TO XXEXTSIS-COM-MSG
                WHEN 002
                  MOVE "DESCR. REDUZIDA DO MOTIVO DE BLOQ. DE CLIENTES "
                                      TO XXEXTSIS-COM-MSG
                WHEN 003
                  MOVE "CONSIDERA POSITIVA€ÇO ('S' OU 'N')             "
                                      TO XXEXTSIS-COM-MSG
                WHEN 004
                  MOVE "CàDIGO DE BLOQUEIO DE CLIENTES DA CIA          "
                                      TO XXEXTSIS-COM-MSG
                WHEN 005
           MOVE "MARCAR (BARRA ESPACO OU CLICK) SE IMP. CLIENTE P/ PALM"
                                      TO XXEXTSIS-COM-MSG
                WHEN 006
                  MOVE "MARCAR (BARRA ESPACO OU CLICK) SE PERMITE VENDA"
                                      TO XXEXTSIS-COM-MSG
                WHEN 007
                MOVE "MARCAR SE ENVIARµ COMO BLOQUEADO NO MARGIN-MINDER"
                                      TO XXEXTSIS-COM-MSG
120612          WHEN 008
120612          MOVE "CàDIGO COMPANHIA                                 "
120612                                TO XXEXTSIS-COM-MSG
120612          WHEN 009
                  MOVE "TECLE ENTER OU CLICK NO BOTÇO PARA CONFIRMAR   "
                                      TO XXEXTSIS-COM-MSG
120612          WHEN 010
                  MOVE "TECLE ENTER OU CLICK NO BOTÇO PARA CANCELAR    "
                                      TO XXEXTSIS-COM-MSG
120612          WHEN 011
                 MOVE "CLICK NO BOTÇO PARA ATUALIZAR E IR PARA ANTERIOR"
                                      TO XXEXTSIS-COM-MSG
120612          WHEN 012
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
                IF VDCADBLO-DESCR         = SPACES
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
                 IF VDCADBLO-REGISTRO = LVDCADBLO-REGISTRO
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
      *  A-104.
      *         PERFORM TESTA-OPCAO-T1    THRU FTESTA-OPCAO-T1.
      *         IF WSCR-ERRO              NOT = ZEROS
      *             GO TO FA-104.
      *         PERFORM VE-TECLA          THRU FVE-TECLA.
      *         IF WTECLA-ESC OR
      *           (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
      *             GO TO FA-104.
      *         IF WSCR-ERRO              NOT = ZEROS
      *             GO TO FA-104.
      *         IF VDCADBLO-POSITIVA      NOT = "S" AND "N"
      *             MOVE 999          TO XXEXTSIS-COM-NRMSG
      *             MOVE "Campo Positiva‡Æo deve ser 'S' ou 'N'       "
      *                               TO XXEXTSIS-COM-MSG
      *             CALL "GEMSG"
      *             MOVE 1            TO WSCR-ERRO
      *             GO TO FA-104.
      *         MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
      *  FA-104.
      *         EXIT.
      *----------------------------------------------------------------*
         A-105.
                PERFORM TESTA-OPCAO-T1    THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO              NOT = ZEROS
                    GO TO FA-105.
                PERFORM VE-TECLA          THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-105.
                IF WSCR-ERRO              NOT = ZEROS
                    GO TO FA-105.

      *--------Conforme OS.186972
                IF XXEXTSIS-CLI-BANDEIRA = 001 AND
                   VDCADBLO-MOTBLQCIA = "F"
                    MOVE 0            TO WDADOS-ENAB-POSIT
                                         WDADOS-ENAB-CLIENTE
                                         WDADOS-ENAB-VENDA
                    MOVE "N"          TO VDCADBLO-POSITIVA
                    MOVE 0            TO WX-VDCADBLO-POSITIVA
                                         VDCADBLO-IMPCLIPALM
                                         VDCADBLO-VENDA
                ELSE
                    MOVE 1            TO WDADOS-ENAB-POSIT
                                         WDADOS-ENAB-CLIENTE
                                         WDADOS-ENAB-VENDA
                END-IF.
                DISPLAY D100.
                MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-105.
                EXIT.
      *----------------------------------------------------------------*
         A-106.
                PERFORM TESTA-OPCAO-T1    THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO              NOT = ZEROS
                    GO TO FA-106.
                PERFORM VE-TECLA          THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-106.
                IF WSCR-ERRO              NOT = ZEROS
                    GO TO FA-106.
                MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-106.
                EXIT.
      *----------------------------------------------------------------*
         E-106.
                IF WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE
                 IF VDCADBLO-REGISTRO = LVDCADBLO-REGISTRO
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
         FE-106.
                EXIT.
      *----------------------------------------------------------------*
         A-107.
                PERFORM TESTA-OPCAO-T1    THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO              NOT = ZEROS
                    GO TO FA-107.
                PERFORM VE-TECLA          THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-107.
                IF WSCR-ERRO              NOT = ZEROS
                    GO TO FA-107.
                MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-107.
                EXIT.
      *----------------------------------------------------------------*
         E-107.
                IF WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE
                 IF VDCADBLO-REGISTRO = LVDCADBLO-REGISTRO
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
         FE-107.
                EXIT.
      *----------------------------------------------------------------*
         A-108.
                PERFORM TESTA-OPCAO-T1    THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO              NOT = ZEROS
                    GO TO FA-108.
                PERFORM VE-TECLA          THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-108.
                IF WSCR-ERRO              NOT = ZEROS
                    GO TO FA-108.
                MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-108.
                EXIT.
      *----------------------------------------------------------------*
         E-108.
                IF WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE
                 IF VDCADBLO-REGISTRO = LVDCADBLO-REGISTRO
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
         FE-108.
                EXIT.
      *----------------------------------------------------------------*
120612   A-109.
120612          PERFORM TESTA-OPCAO-T1    THRU FTESTA-OPCAO-T1.
120612          IF WSCR-ERRO              NOT = ZEROS
120612              GO TO FA-109.
120612          PERFORM VE-TECLA          THRU FVE-TECLA.
120612          IF WTECLA-ESC OR
120612            (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
120612              GO TO FA-109.
120612          IF WSCR-ERRO              NOT = ZEROS
120612              GO TO FA-109.
120612
200114          IF XXEXTSIS-CLI-BANDEIRA = 021                          (KIBON)
120612             IF VDCADBLO-CODCIA EQUAL SPACES
120612                MOVE 071           TO XXEXTSIS-COM-NRMSG
120612                CALL "GEMSG"
120612                MOVE 1             TO WSCR-ERRO
120612                GO TO FA-109
120612             END-IF
120612          END-IF.
120612
120612          DISPLAY D100.
120612          MOVE "S"                 TO WSCR-CAMPO-OK (WSCR-CAMPO).
120612   FA-109.
120612          EXIT.
      *----------------------------------------------------------------*
         TESTA-OPCAO-T1.
                IF WTECLA-BOTAO01
                   INITIALIZE WPARMGEGENAUD
                   MOVE WTITULO         TO WPARMGEGENAUD-TITULO
                   MOVE VDCADBLO-AUDIT  TO WPARMGEGENAUD-AUDIT
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
         COPY "J:\PRGNEW\VD\BOK\VDCADBLO.ECLO"
              REPLACING =='VDCADBLO'== BY ==VDCADBLO==.
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.ECLO"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.

         RETORNAF.
         COPY "J:\PRGNEW\VD\BOK\VDRESUB.PRO".
                EXIT PROGRAM.
      *================================================================*
