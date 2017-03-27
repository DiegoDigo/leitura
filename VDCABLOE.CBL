         IDENTIFICATION DIVISION.
         PROGRAM-ID.    VDCABLOE.
         SECURITY.
      ******************************************************************
      * EXCLUI MOTIVOS DE BLOQUEIO DE CLIENTES                         *
      ******************************************************************
      *--------------  O B S E R V A C O E S  -------------------------*
      * - *+FFF+ LIBERAR NO FUTURO APOS CRIACAO DOS CAMPOS NO ARQUIVO  *
      * -                                                              *
      *--------------  A L T E R A C O E S  ---------------------------*
      * 06/09/2005 COPIADO DO VDCADEVE.CBL                    CRISTIANO*
      * 16/03/2006 ALTERA€ÇO DE PIC P/ O CPO VDCADBLO-COD     CRISTIANO*
      * 29/06/2006 ALTERA€ÇO DO TAMANHO DO PAR¶METRO          CRISTIANO*
      * 21/09/2006 ALT. CAMINHO XXRESUB.PRO                     ROBERTO*
      * 29/12/2006 RECOMPILADO                                  MARCELO*
      * 23/11/2007 CONFORME SOLICITADO NA O.S.141650            MARCELO*
      *            NAO PERMITIR EXCLUIR MOTIVO SE                      *
      *            "BLOQUEADO-MGM" MARCADO E USUARIO <> CTR.           *
      *            SOMENTE PARA NORSA.                                 *
      * 16/08/2010 CONVERSÇO VDCADBLO-COD DE X(1) PARA X(3)      VALDIR*10.10.007
      *                    E VDCADBLO-MOTBLQCIA DE X(1) PARA X(3)      *
      * 23/05/2011 AJUSTE NAS FD, SELECT, PRM, OPENS, E CLOSE     KENDY*11.05.008
      *    PARA O PADRÇO NOVO                                          *
      * 03/06/2011 TRATAMENTO DE FAIXA RESTRITA                   KENDY*11.06.009
      * 02/04/2012 AJUSTE.                                      MARCELO*12.04.010
      * 20/01/2014 OS-2581: BANDEIRA=012 (NORSA)                MARCELO*14.01.011
      *                     BANDEIRA=018 (SOLAR)                       *
      *                     BANDEIRA=015 (GUARARAPES)                  *
      *                     BANDEIRA=013 (RENOSA)                      *
      *                     BANDEIRA=014 (RENOSA-MT)                   *
      *                     BANDEIRA=021 (KIBON)                       *
      *                     BANDEIRA=010 (VONPAR)                      *
      *                     BANDEIRA=016 (SIMOES)                      *
      *                     BANDEIRA=031 (FEMSA)                       *

      * 09/03/2017 OS-8643: CONTROLE DE MANUTENCAO DE CADASTROS        *
      *                     CHAMADA AO VDCADFLC PARA REGISTRAR         *
      *                     INC/ALT/EXC/DUP PARA ALIMENTAR             *
      *                     ARQUIVO NOVO "VDCADFLC.VFE"                *
      *                     QUE SERVIRA TAMBEM PARA O PORTAL.     DIEGO*17.03.012     
      *================================================================*
         ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
         SPECIAL-NAMES.
         COPY "J:\PRGNEW\GE\BOK\GESPECI.ENV".
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.
         COPY "J:\PRGNEW\VD\BOK\VDCADBLO.ESDA"
               REPLACING =='VDCADBLO'== BY ==VDCADBLO==.
      *================================================================*
      /
      *================================================================*
         DATA DIVISION.
         FILE SECTION.
         COPY "J:\PRGNEW\VD\BOK\VDCADBLO.EFDE"
              REPLACING =='VDCADBLO'== BY ==VDCADBLO==.
      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         WORKING-STORAGE SECTION.
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         77 WTPGM-HEADER        PIC  X(019) VALUE "=>CONTROLE VERSAO<=".
         77 WTPGM-VERSAO        PIC  X(009) VALUE "17.03.012".
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         COPY "J:\PRGNEW\GE\BOK\GEDIR.WOR".
         COPY "J:\PRGNEW\VD\BOK\VDDIR.WOR".
         COPY "J:\PRGNEW\GE\BOK\ACTIVEX.WOR".
         COPY "J:\PRGNEW\GE\BOK\ACUCOBOL.WOR".
         COPY "J:\PRGNEW\GE\BOK\ACUGUI.WOR".
         COPY "J:\PRGNEW\GE\BOK\CONTROLS.WOR".
         COPY "J:\PRGNEW\GE\BOK\FONTS.WOR".

         COPY "J:\PRGNEW\GE\BOK\GEWORK.WOR".
         COPY "J:\PRGNEW\VD\BOK\VDWORK.WOR".
         01  GERAL.
         03  WLOG-DADOS         PIC  X(080) VALUE SPACES.
         03  WLOG-DADOSR        REDEFINES WLOG-DADOS.
             05  WLOG-COD       PIC  X(001).
         03  WTITULO.
             05  F              PIC  X(040)  VALUE
                 " Motivos de Bloqueio de Clientes".
             05  F              PIC  X(015)  VALUE      SPACES.
             05  WTITULO-FUNCAO PIC  X(022)  VALUE
                 "exclusÆo".
         03  WAUXILIARES.
             05  WVDCADBLO-COD   PIC  X(003) VALUE      SPACES.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\VD\BOK\VDCADBLO.EPRM"
               REPLACING =='VDCADBLO'== BY ==VDCADBLO==.
               
090317   01   AREAS-EPRM-VDCADFLC.
090317   COPY "J:\PRGNEW\VD\BOK\VDCADFLC.EPRM"
090317        REPLACING =='VDCADFLC'== BY ==VDCADFLC==.                  
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
                 LINE   01,00 LINES 06,80 CELLS
                 COL    01,60 SIZE  64,00 CELLS
                 .
             05  FRAME
                 LINE + 00,60 LINES 05,60 CELLS
                 COL    02,40 SIZE  62,30 CELLS
                 RAISED   ALTERNATE
                 .
      *................................................................*
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  LABEL "01-C¢digo do Motivo"
                 LINE   02,70 COL   04,00
                 .
      *................................................................*
         01  C100.
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE   02,90 COL   28,00 LINES 01,20 CELLS
                 PIC X(003)           USING VDCADBLO-COD
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             04  O101.
             05  COLOR IS XXEXTSIS-USU-CORDIS.
             07  LABEL VDCADBLO-DESCR
VG    *          LINE   02,70 COL   33,00
VG               LINE   02,70 COL   36,00
                 .
      *................................................................*
         01  D100 BEFORE B-D100.
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  PUSH-BUTTON "  &OK   "
                 LINE   04,60 LINES 02,00 CELLS
                 COL    28,00 SIZE  11,00 CELLS
                 AFTER A-101  THRU FA-101
                      ID 001
                 TERMINATION-VALUE = 013
                 ENABLED = WDADOS-ENAB
                 SELF-ACT
                 .
             05  PUSH-BUTTON "&CANCELA"
                              LINES 02,00 CELLS
                 COL  + 01,50 SIZE  11,00 CELLS
                 AFTER A-102  THRU FA-102
                      ID 002
                 TERMINATION-VALUE = 027
                 ENABLED = WDADOS-ENAB
                 SELF-ACT
                 .
      *................................................................*
         COPY "J:\PRGNEW\GE\BOK\GETBAR.SCR".
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

                MOVE 4242              TO XXEXTSIS-COM-CALLSUBNR.

         COPY "J:\PRGNEW\GE\BOK\GETSACE.PRO".

                DISPLAY FLOATING WINDOW
                 LINE -01,80 COL 02,00 LINES 07,30 SIZE 65,20
                 CONTROL FONT IS XXEXTSIS-COM-FONT-TELA
                 COLOR XXEXTSIS-USU-CORTEL
                 TITLE WTITULO
                 SYSTEM MENU
                 HANDLE IN WJANELA1.

         COPY "J:\PRGNEW\GE\BOK\GETBAR.PRO".
                DISPLAY T1.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\VD\BOK\VDCADBLO.EMIO"
               REPLACING =='VDCADBLO'== BY ==VDCADBLO==.
      *----------------------------------------------------------------*
                MOVE WPARAMETROS        TO WPARMVDCADBLO.
030611*----------------------------------------------------------------*
030611          INITIALIZE WGEGEFCRR.
030611          MOVE "VDCADBLO"          TO WGEGEFCRR-ARQUIVO.
030611          MOVE "CODIGO"            TO WGEGEFCRR-CAMPO.
030611          MOVE WPARMVDCADBLO-COD   TO WGEGEFCRR-PARM.
030611   COPY "J:\PRGNEW\GE\BOK\GETSEXC.PRO".
030611*----------------------------------------------------------------*
         LE-VDCADBLO.
                MOVE WPARMVDCADBLO-COD    TO VDCADBLO-COD.
                READ VDCADBLO.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE "VDCADBLO"     TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    GO TO RETORNA.
      *----------------------------------------------------------------*
         LEU-VDCADBLO.
                MODIFY WJANELA1 TITLE WTITULO.
                MOVE XXEXTSIS-COM-CORMOS TO WCOR.
                MOVE 1                   TO WCHAVE-ENAB.
                DISPLAY C100.
      *----------------------------------------------------------------*
      *         IF VDCADBLO-CANCSN     = 1
      *+FFF+        MOVE 084          TO XXEXTSIS-COM-NRMSG
      *             CALL "GEMSG"
      *             GO TO RETORNA.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GELITBAR.PRO".
                MOVE ALL "N"             TO WSCR-CAMPO-OKT.
                MOVE 1                   TO WSCR-ERRO.
                MOVE 002                 TO WSCR-CAMPO.
         PEGA-DADOS.
                MOVE XXEXTSIS-USU-CORAC  TO WCOR.
                MOVE 1                   TO WDADOS-ENAB.
                DISPLAY D100.
                ACCEPT  D100 BEFORE TIME XXEXTSIS-USU-TIMEOUT.
                MOVE XXEXTSIS-USU-CORNAC TO WCOR.
                MOVE 0                   TO WDADOS-ENAB.
                DISPLAY D100.
                IF WTECLA-ESC
                    GO TO RETORNA.
                IF WTECLA-INATI
                    MOVE 033          TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
                    GO TO RETORNA.
                IF WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE
                    GO TO RETORNA.
                IF NOT WTECLA-ENTER
                    GO TO PEGA-DADOS.
      *----------------------------------------------------------------*
200114          IF XXEXTSIS-CLI-BANDEIRA = 012 OR                        (NORSA)
200114             XXEXTSIS-CLI-BANDEIRA = 018                           (SOLAR)
231107             IF VDCADBLO-BLOQ-MGM = 1 AND
231107                WSPAD-USUARIO NOT = "CTR"
231107                MOVE 999          TO XXEXTSIS-COM-NRMSG
231107                MOVE
231107                "NÆo pode excluir BLOQ-MGM, somente usu rio CTR."
231107                                  TO XXEXTSIS-COM-MSG
231107                CALL "GEMSG"
231107                GO TO RETORNA.
      *----------------------------------------------------------------*
         REGRAVA-VDCADBLO.
      *         ACCEPT VDCADBLO-LOGDT  FROM CENTURY-DATE.
      *         ACCEPT VDCADBLO-CANCDT FROM CENTURY-DATE.
      *         ACCEPT WHORA           FROM TIME.
      *         MOVE WHORMIN             TO VDCADBLO-CANCHR.
      *         MOVE XXEXTSIS-USU-SIGLA  TO VDCADBLO-CANCNRUSER.
      *         MOVE 1                   TO VDCADBLO-CANCSN.
      *         IF VDCADBLO-NIVELAUDIT < 50
      *             MOVE 60           TO VDCADBLO-NIVELAUDIT.
      *         REWRITE VDCADBLO-REGISTRO.
      *+FFF+    IF XXEXTSIS-COM-FSTATUS NOT = "00"
      *             MOVE "VDCADBLO"   TO XXEXTSIS-COM-ARQUIVO
      *             CALL "GESTATU"
      *             GO TO RETORNA.
                DELETE VDCADBLO.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE "VDCADBLO"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    GO TO RETORNA.
                    
090317          INITIALIZE WPARMVDCADFLC.
090317          MOVE "VDCADBLO"       TO WPARMVDCADFLC-NOME-TABELA.
090317          MOVE VDCADBLO-CHAVE   TO WPARMVDCADFLC-CODIGO-ERP.
090317          MOVE 2                TO WPARMVDCADFLC-OPERACAO.
090317          MOVE "VDCABLOE"       TO WPARMVDCADFLC-PROGRAMA.
090317          MOVE WTPGM-VERSAO     TO WPARMVDCADFLC-VERSAO-PRG.
090317          CALL "VDCADFLC" USING WPARMVDCADFLC
090317                          ON EXCEPTION
090317                             INITIALIZE WPARMVDCADFLC
090317          END-CALL.                    
      *----------------------------------------------------------------*
         GRAVA-LOG.
                MOVE "E"                TO XXEXTSIS-LOG-OPERACAO.
                MOVE SPACES             TO WLOG-DADOS.
                MOVE WPARMVDCADBLO-COD    TO WLOG-COD.
                MOVE WLOG-DADOS         TO XXEXTSIS-LOG-DADOS.
                CALL "GELOG".
                GO TO RETORNA.
      *----------------------------------------------------------------*
         B-D100.
         COPY "J:\PRGNEW\GE\BOK\GEDETBAR.PRO".
                EVALUATE WSCR-CAMPO
                WHEN 001
                  MOVE "TECLE ENTER OU CLICK NO BOTÇO PARA CONFIRMAR   "
                                      TO XXEXTSIS-COM-MSG
                WHEN 002
                  MOVE "TECLE ENTER OU CLICK NO BOTÇO PARA CANCELAR    "
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
                MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-102.
                EXIT.
      *----------------------------------------------------------------*
         TESTA-OPCAO-T1.

         FTESTA-OPCAO-T1.
                    EXIT.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GETECLA.PRO".
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GEPERFOR.PRO".
      *----------------------------------------------------------------*
         RETORNA.
         COPY "J:\PRGNEW\VD\BOK\VDCADBLO.ECLO"
               REPLACING =='VDCADBLO'== BY ==VDCADBLO==.
         RETORNAF.
         COPY "J:\PRGNEW\VD\BOK\VDRESUB.PRO".
                EXIT PROGRAM.
      *================================================================*
