         IDENTIFICATION DIVISION.
         PROGRAM-ID.    VDCABLOP.
         SECURITY.
      ******************************************************************
      *+888+ PESQUISA...                                               *
      * PESQUISA MOTIVOS DE BLOQUEIO DE CLIENTES                       *
      ******************************************************************
      *--------------  O B S E R V A C O E S  -------------------------*
      * -                                                              *
      * - *+222+ INCL/ALTE/EXCL so se VD=VENDAS                        *
      * -                                                              *
      *                                                                *
      * OBS: este programa NÇO PODE ser tomado como modelo para futuros*
      * programas de CONSULTA, pois ‚ um caso particular               *
      *                                                                *
      *--------------  A L T E R A C O E S  ---------------------------*
      * 06/09/2005 COPIADO DO VDCABLOC.CBL                    CRISTIANO*
      * 16/03/2006 ALTERA€ÇO DE PIC P/ O CPO VDCADBLO-COD     CRISTIANO*
      * 29/06/2006 INCLUSÇO DE COLUNA NA GRID P/ O CPO IMPCLIPALM      *
      *            (IMPORTA€ÇO PARA PALMTOP)                  CRISTIANO*
      * 15/09/2006 CONTROLE PARA COROA                          MARCELO*
      * 21/09/2006 ALT. CAMINHO XXVERSA/XXRESUB                 ROBERTO*
      * 29/12/2006 INCLUIDO CAMPO "PERMITIR VENDA"              MARCELO*
      * 09/01/2007 TRATAMENTO PARA VONPAR                       MARCELO*
      * 28/05/2008 AJUSTE DE TELA                               MARCELO*
      * 29/05/2008 CONTROLE PARA NORSA                          MARCELO*
      * 16/08/2010 CONVERSÇO VDCADBLO-COD DE X(1) PARA X(3)      VALDIR*10.10.010
      *                    E VDCADBLO-MOTBLQCIA DE X(1) PARA X(3)      *
      * 23/05/2011 AJUSTE NAS FD, SELECT, PRM, OPENS, E CLOSE     KENDY*11.05.011
      *    PARA O PADRÇO NOVO                                          *
      * 20/01/2014 OS-2581: BANDEIRA=012 (NORSA)                MARCELO*14.01.013
      *                     BANDEIRA=018 (SOLAR)                       *
      *                     BANDEIRA=015 (GUARARAPES)                  *
      *                     BANDEIRA=013 (RENOSA)                      *
      *                     BANDEIRA=014 (RENOSA-MT)                   *
      *                     BANDEIRA=021 (KIBON)                       *
      *                     BANDEIRA=010 (VONPAR)                      *
      *                     BANDEIRA=016 (SIMOES)                      *
      *                     BANDEIRA=031 (FEMSA)                       *
      * 13/04/2016 OS-6796: TRATAMENTO BANDEIRA-018 SOLAR              *
      *                     COM NOVA LICENCA-FABRICA = 6455     MARCELO*16.04.014
      *================================================================*   VERSAO
         ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
         SPECIAL-NAMES.
         COPY "J:\PRGNEW\GE\BOK\GESPECI.ENV".
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.
         COPY "J:\PRGNEW\VD\BOK\VDCADBLO.ESSA"
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
         77 WTPGM-VERSAO        PIC  X(009) VALUE "16.04.014".
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
             05  WLOG-TIPOCHAVE PIC  X(035).
         03  WTITULO            PIC  X(056) VALUE
      *+888+   Pesquisa...
             " Pesquisa Motivos de Bloqueio de Clientes".

      *------ JANELA 1 ------------------------------------------------*
         03  WJ1-TOT-ITENS      PIC  9(007) VALUE 999999.
         03  WJ1-ENAB           PIC  9(001) VALUE ZEROS.

         03  WJ1-SIZE           PIC  9(003) VALUE 048.

      *--> TEM QUE SER NIVEL 01 (RECORD POSITION...)
         01  WJ1-TEL.
             05  WJ1-TEL-DADOS.
1                07  WJ1-TEL-COD            PIC X(003).
4                07  WJ1-TEL-DESCR          PIC X(030).
34               07  WJ1-TEL-DESCRRED       PIC X(015).
49               07  WJ1-TEL-POSITIVA       PIC X(001).
50               07  WJ1-TEL-MOTBLQCIA      PIC X(003).
53               07  WJ1-TEL-IMPCLIPALM     PIC X(003).
56               07  WJ1-TEL-VENDA          PIC X(003).
59               07  WJ1-TEL-F              PIC X(005).
             05  WJ1-CHAVE.
                 07  WJ1-CHA-COD            PIC X(003).
                 07  WJ1-CHA-DESCR          PIC X(030).
                 07  WJ1-CHA-DESCRRED       PIC X(015).
                 07  WJ1-CHA-POSITIVA       PIC X(001).
                 07  WJ1-CHA-MOTBLQCIA      PIC X(003).
                 07  WJ1-CHA-IMPCLIPALM     PIC 9(001).
                 07  WJ1-CHA-VENDA          PIC 9(001).
         01  FWJ1-TEL.

         03  WJ1-CABEC.
VG           05  F PIC X(005) VALUE 'C¢d'.
VG           05  F PIC X(046) VALUE 'Descri‡Æo'.
             05  F PIC X(017) VALUE 'Desc. Reduzida'.
             05  F PIC X(010) VALUE 'Cobertu.'.
             05  F PIC X(009) VALUE 'Bloq.CIA'.
             05  F PIC X(009) VALUE 'Imp.Palm'.
             05  F PIC X(005) VALUE 'Venda'.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\VD\BOK\VDCADBLO.EPRM"
               REPLACING =='VDCADBLO'== BY ==VDCADBLO==.
      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\XXEXTSIS.EXT".
         COPY "J:\PRGNEW\VFEV99\AJUDA\WSPAD.".
         COPY "J:\PRGNEW\VFEV99\AJUDA\WSPAD2.".
      *----------------------------------------------------------------*
      *+888+ LIBERAR PARA PESQUISA E ACERTAR O TAMANHO
         LINKAGE SECTION.
         01  WPARAMETROS        PIC  X(100).
      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         SCREEN SECTION.
         01  T1.
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  BLANK SCREEN.
      *+888+ TIRAR 10 LINHAS DO LINES
             05  FRAME
                 LINE   01,00 LINES 15,90 CELLS
                 COL    01,80 SIZE  74,30 CELLS
                 .
      *+888+ TIRAR 10 LINHAS DO LINES
             05  FRAME
                 LINE + 00,70 LINES 14,60 CELLS
                 COL    02,80 SIZE  72,20 CELLS
                 RAISED   ALTERNATE
                 .
             05  LABEL WJ1-CABEC
                 LINE + 00,70 COL   03,40
                 .
      *................................................................*
         01  D100 LIST-BOX BEFORE B-D100 USING WJ1-TEL
                 FONT                  XXEXTSIS-COM-FONT-GRID
      *+888+ TIRAR 10 LINHAS DO LINES
                 LINE   03,70 LINES 12,50 CELLS
                 COL    03,55 SIZE  70,40 CELLS
                 AFTER     A-101  THRU FA-101
                          ID 001
                 EXCEPTION E-101  THRU FE-101
                 UNSORTED
                 3-D
                 BOXED
                 NOTIFY-DBLCLICK
                 ENABLED         = WJ1-ENAB
                 ALIGNMENT       = ("L","L","L","L","L","L","L")
VG    *          DISPLAY-COLUMNS = ( 01  04  34  49  57  65  74  81)
VG               DISPLAY-COLUMNS = ( 01  05  35  50  58  66  75  82)
                 DIVIDERS        = (  1   1   1   1   1   1   1  1)
      *                               C   D   D   P   C   I   V  b
      *                               O   E   R   O   I   M   E
      *                               D   S   E   S   A   P   N
VG    *          DATA-COLUMNS    = ( 01  02  32  47  48  49  52  59)
VG               DATA-COLUMNS    = ( 01  04  34  49  50  53  56  59)
      *+NAO+     DATA-COLUMNS    = ( record-position WJ1-TEL-COD,
      *                              record-position WJ1-TEL-DESCR,
      *                              record-position WJ1-TEL-DESCRRED,
      *                              record-position WJ1-TEL-POSITIVA,
      *                              record-position WJ1-TEL-MOTBLQCIA,
      *                              record-position WJ1-TEL-IMPCLIPALM,
      *                              record-position WJ1-TEL-VENDA)
                 .
      *................................................................*
         COPY "J:\PRGNEW\GE\BOK\GETBAR.SCR".
         COPY "J:\PRGNEW\GE\BOK\GETBAR3.SCR".
         COPY "J:\PRGNEW\GE\BOK\GETBAR5.SCR".
         COPY "J:\PRGNEW\GE\BOK\GETBAR6.SCR".
         COPY "J:\PRGNEW\GE\BOK\GETBAR7.SCR".
         COPY "J:\PRGNEW\GE\BOK\GETBAR9.SCR".
         COPY "J:\PRGNEW\GE\BOK\GESITMS.SCR".
         COPY "J:\PRGNEW\VD\BOK\VDSITMS.SCR".
      *================================================================*
      /
      *================================================================*
         PROCEDURE DIVISION USING WPARAMETROS.
      *  PROCEDURE DIVISION.
         INICIO.
         COPY "J:\PRGNEW\GE\BOK\GEINICIO.PRO".
         COPY "J:\PRGNEW\VD\BOK\VDVERSA.PRO".
      *+888+ TIRAR GENOPGM.PRO e LIBERAR ESTES
         COPY "J:\PRGNEW\GE\BOK\GENOSUB.PRO".

                MOVE 4237             TO XXEXTSIS-COM-CALLSUBNR.

      *  COPY "J:\PRGNEW\GE\BOK\GENOPGM.PRO".
         COPY "J:\PRGNEW\GE\BOK\GETSACE.PRO".

      *+888+ TIRAR 10 LINHAS DO LINES
                DISPLAY FLOATING WINDOW
                 LINE -01,80 COL 02,00 LINES 16,50 SIZE 75,90
                 CONTROL FONT IS XXEXTSIS-COM-FONT-TELA
                 COLOR XXEXTSIS-USU-CORTEL
                 TITLE WTITULO
                 SYSTEM MENU
                 HANDLE IN WJANELA1.

      *+222+                                VD
                IF XXEXTSIS-COM-NRSIS NOT = 06
                     MOVE 0           TO WTBAR-INCLSN
                     MOVE 0           TO WTBAR-ALTESN
                     MOVE 0           TO WTBAR-EXCLSN
                     MOVE 0           TO WTBAR-DUPLSN.
                MOVE 1                TO WTBAR-IMPRSN.

200114          IF XXEXTSIS-CLI-BANDEIRA = 009 OR                        (COROA)
200114             (XXEXTSIS-CLI-BANDEIRA = 010 AND
200114                                          WSPAD-NUMLIC NOT = 6100) (VONPAR)
200114                                                                OR
200114             (XXEXTSIS-CLI-BANDEIRA = 012 AND
200114                                          WSPAD-NUMLIC NOT = 6400) (NORSA)
200114                                                                OR
200114             (XXEXTSIS-CLI-BANDEIRA = 018 AND
130416                                          WSPAD-NUMLIC NOT = 6455) (SOLAR)
                     MOVE 0           TO WTBAR-INCLSN
                     MOVE 0           TO WTBAR-ALTESN
                     MOVE 0           TO WTBAR-EXCLSN
                     MOVE 0           TO WTBAR-DUPLSN.

         COPY "J:\PRGNEW\GE\BOK\GETBAR.PRO".
                DISPLAY T1.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\VD\BOK\VDCADBLO.EMI"
               REPLACING =='VDCADBLO'== BY ==VDCADBLO==.
      *----------------------------------------------------------------*
         CONSULTA.
                DISPLAY T1.
                MOVE XXEXTSIS-USU-CORNAC TO WCOR.
                MOVE 0                   TO WJ1-ENAB.
                DISPLAY D100.
      *----------------------------------------------------------------*
         RESTART-VDCADBLO.
                MOVE "CODIGO        " TO WLOG-TIPOCHAVE
                MOVE LOW-VALUES       TO VDCADBLO-COD.
                START VDCADBLO KEY >= VDCADBLO-CHAVE
                  INVALID KEY
                    MOVE "VDCADBLO"   TO XXEXTSIS-COM-ARQUIVO
                    MOVE 029          TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG".
      *----------------------------------------------------------------*
         GRAVA-LOG.
                MOVE "D"              TO XXEXTSIS-LOG-OPERACAO.
                MOVE WLOG-DADOS       TO XXEXTSIS-LOG-DADOS.
                CALL "GELOG".
      *----------------------------------------------------------------*
                MOVE ZEROS              TO WTNRREG.
                MOVE XXEXTSIS-USU-CORAC TO WCOR.
                MOVE 1                  TO WJ1-ENAB.
                DISPLAY D100.
                MODIFY  D100 mass-update = 1 reset-list = 1.
                MODIFY  D100 mass-update = 0 reset-list = 0.
                PERFORM THREAD J1-MONTA-TAB THRU FJ1-MONTA-TAB
                  HANDLE IN WJANELAT.
      *----------------------------------------------------------------*
         J1-POSIC-ITEM.
                MOVE ALL "N"          TO WSCR-CAMPO-OKT.
                ACCEPT  D100 BEFORE TIME XXEXTSIS-USU-TIMEOUT.
                MOVE SPACES           TO XXEXTSIS-COM-MSG.
                DISPLAY TMSGI UPON XXEXTSIS-COM-MWINDOW.
                MOVE 001              TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      001     TO WSCR-ID
                    PERFORM A-101   THRU FA-101
                    IF WSCR-ERRO   NOT = ZEROS
                        GO TO J1-POSIC-ITEM.
                IF WTECLA-ESC
                    STOP THREAD WJANELAT
                    DESTROY D100
      *+++          GO TO CONSULTA.
                    GO TO RETORNA.
                IF WTECLA-INATI
                    MOVE 033          TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
                    STOP THREAD WJANELAT
                    DESTROY D100
      *+++          GO TO CONSULTA.
                    GO TO RETORNA.
                IF WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE
                    STOP THREAD WJANELAT
                    GO TO RETORNA.
      *+++NÇO   IF WJ1-CHA-COD      NOT NUMERIC
      *             MOVE ZEROS        TO WJ1-CHA-COD.
      *----------------------------------------------------------------*
      *+888+ ENTER USADO PARA SELECIONAR REGISTRO POSICIONADO PESQUISA
                IF WTECLA-ENTER
                  IF WJ1-CHA-COD   NOT = SPACES
                    STOP THREAD WJANELAT
                    GO TO FIM.
      *----------------------------------------------------------------*
                IF WTECLA-ENTER OR WTECLA-VIEW
                 IF WTBAR-VIEW         = 1
                  IF WJ1-CHA-COD       NOT = SPACES
                    MOVE WJ1-CHA-COD       TO WPARMVDCADBLO-COD
                    MOVE "VDCABLOM"        TO XXEXTSIS-COM-CALLPRX
                    MOVE XXEXTSIS-VER-VD   TO XXEXTSIS-COM-CALLPRX (9:4)
                    CALL  XXEXTSIS-COM-CALLPRX USING WPARMVDCADBLO
                    CANCEL XXEXTSIS-COM-CALLPRX
                    GO TO J1-POSIC-ITEM.
                IF WTECLA-INCL
                 IF WTBAR-INCL         = 1
      *+NAO+      IF WJ1-CHA-COD   NOT = SPACES
                    MOVE SPACES            TO WPARMVDCADBLO-COD
                    MOVE "VDCABLOI"        TO XXEXTSIS-COM-CALLPRX
                    MOVE XXEXTSIS-VER-VD   TO XXEXTSIS-COM-CALLPRX (9:4)
                    CALL  XXEXTSIS-COM-CALLPRX USING WPARMVDCADBLO
                    CALL  XXEXTSIS-COM-CALLPRX
                    CANCEL XXEXTSIS-COM-CALLPRX
                    STOP THREAD WJANELAT
                    DESTROY D100
                    GO TO RESTART-VDCADBLO.
                IF WTECLA-FECHAR
                 IF WEVE-TYPE          = CMD-DBLCLICK
                 IF WTBAR-ALTE         = 1
                  IF WJ1-CHA-COD   NOT = SPACES
                    MOVE WJ1-CHA-COD       TO WPARMVDCADBLO-COD
                    MOVE "VDCABLOA"        TO XXEXTSIS-COM-CALLPRX
                    MOVE XXEXTSIS-VER-VD   TO XXEXTSIS-COM-CALLPRX (9:4)
                    CALL  XXEXTSIS-COM-CALLPRX USING WPARMVDCADBLO
                    CANCEL XXEXTSIS-COM-CALLPRX
                    STOP THREAD WJANELAT
                    DESTROY D100
                    GO TO RESTART-VDCADBLO.
                IF WTECLA-ALTE
                 IF WTBAR-ALTE         = 1
                  IF WJ1-CHA-COD   NOT = SPACES
                    MOVE WJ1-CHA-COD       TO WPARMVDCADBLO-COD
                    MOVE "VDCABLOA"        TO XXEXTSIS-COM-CALLPRX
                    MOVE XXEXTSIS-VER-VD   TO XXEXTSIS-COM-CALLPRX (9:4)
                    CALL  XXEXTSIS-COM-CALLPRX USING WPARMVDCADBLO
                    CANCEL XXEXTSIS-COM-CALLPRX
                    STOP THREAD WJANELAT
                    DESTROY D100
                    GO TO RESTART-VDCADBLO.
                IF WTECLA-EXCL
                 IF WTBAR-EXCL         = 1
                  IF WJ1-CHA-COD   NOT = SPACES
                    MOVE WJ1-CHA-COD       TO WPARMVDCADBLO-COD
                    MOVE "VDCABLOE"        TO XXEXTSIS-COM-CALLPRX
                    MOVE XXEXTSIS-VER-VD   TO XXEXTSIS-COM-CALLPRX (9:4)
                    CALL  XXEXTSIS-COM-CALLPRX USING WPARMVDCADBLO
                    CANCEL XXEXTSIS-COM-CALLPRX
                    STOP THREAD WJANELAT
                    DESTROY D100
                    GO TO RESTART-VDCADBLO.
                IF WTECLA-IMPR
                 IF WTBAR-IMPR         = 1
      *+NAO+      IF WJ1-CHA-COD   NOT = SPACES
                    MOVE "VDCABLOL"         TO XXEXTSIS-COM-CALLPRX
                    MOVE XXEXTSIS-VER-VD    TO XXEXTSIS-COM-CALLPRX(9:4)
                    CALL  XXEXTSIS-COM-CALLPRX
                    CANCEL XXEXTSIS-COM-CALLPRX
                    GO TO J1-POSIC-ITEM.
                IF WTECLA-HCPO
                   MOVE 001           TO WSCR-ID
                ELSE
                   MOVE ZEROS         TO WSCR-ID.
                PERFORM VE-TECLA    THRU FVE-TECLA.
                GO TO J1-POSIC-ITEM.
      *----------------------------------------------------------------*
      *+888+ ACERTAR CAMPOS DO WPARM...
         FIM.
                MOVE WJ1-CHA-COD        TO WPARMVDCADBLO-COD.
                MOVE WJ1-CHA-DESCR      TO WPARMVDCADBLO-DESCR.
                MOVE WJ1-CHA-DESCRRED   TO WPARMVDCADBLO-DESCRRED.
                MOVE WJ1-CHA-POSITIVA   TO WPARMVDCADBLO-POSITIVA.
                MOVE WJ1-CHA-MOTBLQCIA  TO WPARMVDCADBLO-MOTBLQCIA.
                MOVE WJ1-CHA-IMPCLIPALM TO WPARMVDCADBLO-IMPCLIPALM.
                MOVE WJ1-CHA-VENDA      TO WPARMVDCADBLO-VENDA.
                GO TO RETORNA.
      *----------------------------------------------------------------*
      /
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
         A-101.
      *         STOP "".
                IF WTECLA-VIEW    OR WTECLA-FOTO
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FA-101.
                IF WTECLA-INCL    OR WTECLA-ALTE    OR WTECLA-EXCL
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FA-101.
                IF WTECLA-DUPL    OR WTECLA-IMPR
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FA-101.
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
         E-101.
      *         STOP "".
                IF WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE
                    GO TO FE-101.
                IF NOT WTECLA-FECHAR
                    GO TO E-101-C.
                IF WEVE-TYPE           = CMD-CLOSE OR = CMD-DBLCLICK
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FE-101.
                MOVE 0                TO WSCR-ERRO.
                IF WEVE-TYPE           = NTF-PL-FIRST
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FE-101.
                IF WEVE-TYPE           = NTF-PL-PREVPAGE
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FE-101.
                IF WEVE-TYPE           = NTF-PL-PREV
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FE-101.
                IF WEVE-TYPE           = NTF-PL-NEXT
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FE-101.
                IF WEVE-TYPE           = NTF-PL-NEXTPAGE
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FE-101.
                IF WEVE-TYPE           = NTF-PL-LAST
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FE-101.
                IF WEVE-TYPE           = NTF-PL-SEARCH
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FE-101.
         E-101-C.
                MOVE 4                TO WSCR-ERRO.
                MOVE 002              TO WSCR-ID.
         FE-101.
                EXIT.
      *----------------------------------------------------------------*
         J1-MONTA-TAB.
         LE-VDCADBLO.
                READ VDCADBLO AT END
                    GO TO FLE-VDCADBLO.
                IF XXEXTSIS-COM-FSTATUS = XXEXTSIS-COM-FSFIMARQ OR "46"
                    GO TO FLE-VDCADBLO.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE "VDCADBLO"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    GO TO LE-VDCADBLO.
                IF VDCADBLO-IMPCLIPALM  NOT NUMERIC
                    MOVE 0            TO VDCADBLO-IMPCLIPALM
                END-IF.
      *+888+ LIBERAR, PARA PESQUISA PULA SE CANCELADO
      *+NAO+    IF VDCADBLO-CANCSN     = 1
      *             GO TO LE-VDCADBLO.
      *+NAO+    IF XXEXTSIS-USU-NIVEL  < VDCADBLO-NIVELAUDIT
      *             GO TO LE-VDCADBLO.
                PERFORM J1-MONTA-REG THRU FJ1-MONTA-REG.
                GO TO LE-VDCADBLO.
         FLE-VDCADBLO.
      *+++++++++YIELD.
         FJ1-MONTA-TAB.
                EXIT.
      *----------------------------------------------------------------*
         J1-MONTA-REG.
      *................................................................*
                MOVE VDCADBLO-COD        TO WJ1-TEL-COD.
                MOVE VDCADBLO-DESCR      TO WJ1-TEL-DESCR.
                MOVE VDCADBLO-DESCRRED   TO WJ1-TEL-DESCRRED.
                MOVE VDCADBLO-POSITIVA   TO WJ1-TEL-POSITIVA.
                MOVE VDCADBLO-MOTBLQCIA  TO WJ1-TEL-MOTBLQCIA.
                EVALUATE VDCADBLO-IMPCLIPALM
                WHEN 0  MOVE SPACES      TO WJ1-TEL-IMPCLIPALM
                WHEN 1  MOVE "SIM"       TO WJ1-TEL-IMPCLIPALM
                END-EVALUATE.

                IF VDCADBLO-VENDA IS NOT NUMERIC
                   MOVE ZEROS TO VDCADBLO-VENDA
                END-IF

                EVALUATE VDCADBLO-VENDA
                WHEN 0  MOVE SPACES      TO WJ1-TEL-VENDA
                WHEN 1  MOVE "SIM"       TO WJ1-TEL-VENDA
                END-EVALUATE.
                MOVE SPACES              TO WJ1-TEL-F.

                MOVE VDCADBLO-COD        TO WJ1-CHA-COD.
                MOVE VDCADBLO-DESCR      TO WJ1-CHA-DESCR.
                MOVE VDCADBLO-DESCRRED   TO WJ1-CHA-DESCRRED.
                MOVE VDCADBLO-POSITIVA   TO WJ1-CHA-POSITIVA.
                MOVE VDCADBLO-MOTBLQCIA  TO WJ1-CHA-MOTBLQCIA.
                MOVE VDCADBLO-IMPCLIPALM TO WJ1-CHA-IMPCLIPALM.
                MOVE VDCADBLO-VENDA      TO WJ1-CHA-VENDA.

      *         MOVE "_"              TO WJ1-TEL-CANCSN.
      *+NAO+    IF VDCADBLO-CANCSN     = 1
      *             MOVE 'S'          TO WJ1-TEL-CANCSN.

                MODIFY  D100 item-to-add = WJ1-TEL.
                MOVE SPACES           TO WJ1-TEL.
                ADD  1  TO WTNRREG.
                DISPLAY TNRREG  UPON XXEXTSIS-COM-MWINDOW.
         FJ1-MONTA-REG.
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
                MOVE WPARMVDCADBLO      TO WPARAMETROS.
         RETORNAF.
      *+888+  "J:\PRGNEW\GE\BOK\GERESUB.PRO".
         COPY "J:\PRGNEW\VD\BOK\VDRESUB.PRO".
                EXIT PROGRAM.
      *================================================================*
