      *================================================================*
         IDENTIFICATION DIVISION.
         PROGRAM-ID.    ATCABEMM.
         SECURITY.
      ******************************************************************
      * MOSTRA CADASTRO DE BENS                                        *
      ******************************************************************
      *--------------  O B S E R V A C O E S  -------------------------*
      * - *+FFF+ LIBERAR NO FUTURO                                     *
      * -                                                              *
      *--------------  A L T E R A C O E S  ---------------------------*
      * 03/03/2017 COPIADO DO ATCABEMA.CBL                          LCA*
      *================================================================*   


         ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
         SPECIAL-NAMES.
         COPY "J:\PRGNEW\GE\BOK\GESPECI.ENV".
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.

         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.ESDA"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBEM.ESDA"
              REPLACING =='ATCADBEM'== BY ==ATCADBEM==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBEM.ESDA"
              REPLACING =='ATCADBEM'== BY ==ESPELHO-ATCADBEM==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBEM.ESDA"
              REPLACING =='ATCADBEM'== BY ==ESPELHO2-ATCADBEM==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBLQ.ESDA"
              REPLACING =='ATCADBLQ'== BY ==ATCADBLQ==.
         COPY "J:\PRGNEW\AT\BOK\ATCADGRC.ESDA"
              REPLACING =='ATCADGRC'== BY ==ATCADGRC==.
         COPY "J:\PRGNEW\AT\BOK\ATCADLOC.ESDA"
              REPLACING =='ATCADLOC'== BY ==ATCADLOC==.
         COPY "J:\PRGNEW\AT\BOK\ATCADUTL.ESDA"
              REPLACING =='ATCADUTL'== BY ==ATCADUTL==.
         COPY "J:\PRGNEW\CG\BOK\CGPLAPLA.ESDA"
              REPLACING =='CGPLAPLA'== BY ==CGPLAPLA==.
         COPY "J:\PRGNEW\CG\BOK\CGGENCCU.ESDA"
              REPLACING =='CGGENCCU'== BY ==CGGENCCU==.
         COPY "J:\PRGNEW\LF\BOK\LFMENREL.ESDA"
              REPLACING =='LFMENREL'== BY ==LFMENREL==.
         COPY "J:\PRGNEW\PG\BOK\PGCADFOR.ESDA"
              REPLACING =='PGCADFOR'== BY ==PGCADFOR==.
         COPY "J:\PRGNEW\VD\BOK\VDPRDPRD.ESDA"
              REPLACING =='VDPRDPRD'== BY ==VDPRDPRD==.
      *================================================================*
      /
      *================================================================*
         DATA DIVISION.
         FILE SECTION.
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.EFDE"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBEM.EFDE"
              REPLACING =='ATCADBEM'== BY ==ATCADBEM==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBEM.EFDE"
              REPLACING =='ATCADBEM'== BY ==ESPELHO-ATCADBEM==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBEM.EFDE"
              REPLACING =='ATCADBEM'== BY ==ESPELHO2-ATCADBEM==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBLQ.EFDE"
              REPLACING =='ATCADBLQ'== BY ==ATCADBLQ==.
         COPY "J:\PRGNEW\AT\BOK\ATCADGRC.EFDE"
              REPLACING =='ATCADGRC'== BY ==ATCADGRC==.
         COPY "J:\PRGNEW\AT\BOK\ATCADLOC.EFDE"
              REPLACING =='ATCADLOC'== BY ==ATCADLOC==.
         COPY "J:\PRGNEW\AT\BOK\ATCADUTL.EFDE"
              REPLACING =='ATCADUTL'== BY ==ATCADUTL==.
         COPY "J:\PRGNEW\CG\BOK\CGPLAPLA.EFDE"
              REPLACING =='CGPLAPLA'== BY ==CGPLAPLA==.
         COPY "J:\PRGNEW\CG\BOK\CGGENCCU.EFDE"
              REPLACING =='CGGENCCU'== BY ==CGGENCCU==.
         COPY "J:\PRGNEW\LF\BOK\LFMENREL.EFDE"
              REPLACING =='LFMENREL'== BY ==LFMENREL==.
         COPY "J:\PRGNEW\PG\BOK\PGCADFOR.EFDE"
              REPLACING =='PGCADFOR'== BY ==PGCADFOR==.
         COPY "J:\PRGNEW\VD\BOK\VDPRDPRD.EFDE"
              REPLACING =='VDPRDPRD'== BY ==VDPRDPRD==.

      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         WORKING-STORAGE SECTION.
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         77 WTPGM-HEADER        PIC  X(019) VALUE "=>CONTROLE VERSAO<=".
         77 WTPGM-VERSAO        PIC  X(009) VALUE "17.03.001".
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         COPY "J:\PRGNEW\GE\BOK\GEDIR.WOR".
         COPY "J:\PRGNEW\AT\BOK\ATDIR.WOR".
         COPY "J:\PRGNEW\GE\BOK\ACTIVEX.WOR".
         COPY "J:\PRGNEW\GE\BOK\ACUCOBOL.WOR".
         COPY "J:\PRGNEW\GE\BOK\ACUGUI.WOR".
         COPY "J:\PRGNEW\GE\BOK\CONTROLS.WOR".
         COPY "J:\PRGNEW\GE\BOK\FONTS.WOR".
         78 WX-SEPARA             VALUE x"0A".

         COPY "J:\PRGNEW\GE\BOK\GEWORK.WOR".
         COPY "J:\PRGNEW\AT\BOK\ATWORK.WOR".
         77  WDADOS-ENAB-01     PIC  9(001) VALUE 0.
         77  WDADOS-ENAB-02     PIC  9(001) VALUE 0.
         77  WPUSH-T1-IN-AL1    PIC  X(014) VALUE SPACES.
         77  WBOTAO-FUNCAO      PIC  A      VALUE SPACES.
         77  WRODAPE1-1         PIC  X(070) VALUE SPACES.
         77  WX-ERRO            PIC  X(001) VALUE SPACES.
         01  GERAL.
         03  WDT-INC-D          PIC  9(008) VALUE ZEROS.
         03  WDT-INC-DR         REDEFINES WDT-INC-D.
             05  WDIA-INC-D     PIC  99.
             05  WMES-INC-D     PIC  99.
             05  WANO-INC-D     PIC  9999.
         03  WDT-AQUI-D         PIC  9(008) VALUE ZEROS.
         03  WDT-AQUI-DR        REDEFINES WDT-AQUI-D.
             05  WDIA-AQ-D      PIC  99.
             05  WMES-AQ-D      PIC  99.
             05  WANO-AQ-D      PIC  9999.
         03  WDT-DEPRE-D        PIC  9(008) VALUE ZEROS.
         03  WDT-DEPRE-DR       REDEFINES WDT-DEPRE-D.
             05  WDIA-DEPRE-D   PIC  99.
             05  WMES-DEPRE-D   PIC  99.
             05  WANO-DEPRE-D   PIC  9999.
         03  WDT-EMINF-D        PIC  9(008) VALUE ZEROS.
         03  WDT-EMINF-DR       REDEFINES WDT-EMINF-D.
             05  WDIA-EMINF-D   PIC  99.
             05  WMES-EMINF-D   PIC  99.
             05  WANO-EMINF-D   PIC  9999.
         03  WN-TOTAL-ICMS      PIC  9(013)V99 VALUE ZEROS.
         03  WX-GRUPO           PIC  X(060) VALUE SPACES.
         03  WX-UTILIZACAO      PIC  X(050) VALUE SPACES.
         03  WX-CTA-CTBL        PIC  X(040) VALUE SPACES.
         03  WX-CCUSTO          PIC  X(030) VALUE SPACES.
         03  WX-LOCALIZACAO     PIC  X(050) VALUE SPACES.
         03  WX-MOTBLQ          PIC  X(030) VALUE SPACES.
         03  WX-DESCR-ORIGEM    PIC  X(020) VALUE SPACES.
         03  WX-FORNECEDOR      PIC  X(030) VALUE SPACES.
         03  WX-PRODUTO         PIC  X(040) VALUE SPACES.
         03  WS-CNPJ            PIC  9(015) VALUE ZEROS.
         03  FILLER             REDEFINES WS-CNPJ.
             05  WS-P1-CNPJ     PIC  9(009).
             05  WS-P2-CNPJ     PIC  9(004).
             05  WS-P2-CNPJ-X   REDEFINES WS-P2-CNPJ.
                 07  WS-P2-P1   PIC  9(002).
                 07  WS-P2-P2   PIC  9(002).
             05  WS-P3-CNPJ     PIC  9(002).
         03  LNK-DADOS-CGCCIC.
             05  WS-TIPO        PIC 9(001).
             05  WS-CPF-CGC     PIC 9(016).
             05  FILLER REDEFINES WS-CPF-CGC.
                 10  WS-BYTE OCCURS 16 TIMES     PIC 9(001).
             05  WS-DIGITOS.
                 10  WS-DV1     PIC 9(001).
                 10  WS-DV2     PIC 9(001).
         03  WLOG-DADOS         PIC  X(080) VALUE SPACES.
         03  WLOG-DADOSR        REDEFINES WLOG-DADOS.
             05  WLOG-CHAVE     PIC  X(015).
         03  WTITULO.
             05  F              PIC  X(050)  VALUE
                 "   Cadastro de Bens  ".
             05  F              PIC  X(020)  VALUE      SPACES.
             05  WTITULO-FUNCAO PIC  X(032)  VALUE
                 "mostra".

         03  AREAS-AUXILIARES.
             05  WS-COL1                 PIC  9(002)V99.
             05  WS-COL2                 PIC  9(002)V99.
             05  WN-CHECATELA            PIC  9(001)    VALUE 0.
             05  WN-PROX-ORELHINHA       PIC  9(001)    VALUE 0.
             05  WN-ANT-ORELHINHA        PIC  9(001)    VALUE 0.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\AT\BOK\ATCADBEM.EFDE"
              REPLACING ==XFD FILE="'ATCADBEM'.ATV"== BY == ==,
                        ==FD  'ATCADBEM'== BY == ==,
                        ==LABEL RECORD IS STANDARD.== BY == ==,
                        =='ATCADBEM'== BY ==LATCADBEM==.
      *----------------------------------------------------------------*
         01 PARAMETROS.
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.EPRM"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.
         COPY "J:\PRGNEW\AT\BOK\ATORBEMP.EPRM"
              REPLACING =='ATORBEMP'== BY ==ATORBEMP==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBEM.EPRM"
              REPLACING =='ATCADBEM'== BY ==ATCADBEM==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBEM.EPRM"
              REPLACING =='ATCADBEM'== BY ==ESPELHO-ATCADBEM==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBEM.EPRM"
              REPLACING =='ATCADBEM'== BY ==ESPELHO2-ATCADBEM==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBLQ.EPRM"
              REPLACING =='ATCADBLQ'== BY ==ATCADBLQ==.
         COPY "J:\PRGNEW\AT\BOK\ATCADGRC.EPRM"
              REPLACING =='ATCADGRC'== BY ==ATCADGRC==.
         COPY "J:\PRGNEW\AT\BOK\ATCADLOC.EPRM"
              REPLACING =='ATCADLOC'== BY ==ATCADLOC==.
         COPY "J:\PRGNEW\AT\BOK\ATCADUTL.EPRM"
              REPLACING =='ATCADUTL'== BY ==ATCADUTL==.
         COPY "J:\PRGNEW\CG\BOK\CGPLAPLA.EPRM"
              REPLACING =='CGPLAPLA'== BY ==CGPLAPLA==.
         COPY "J:\PRGNEW\CG\BOK\CGGENCCU.EPRM"
              REPLACING =='CGGENCCU'== BY ==CGGENCCU==.
         COPY "J:\PRGNEW\LF\BOK\LFMENREL.EPRM"
              REPLACING =='LFMENREL'== BY ==LFMENREL==.
         COPY "J:\PRGNEW\PG\BOK\PGCADFOR.EPRM"
              REPLACING =='PGCADFOR'== BY ==PGCADFOR==.
         COPY "J:\PRGNEW\VD\BOK\VDPRDPRD.EPRM"
              REPLACING =='VDPRDPRD'== BY ==VDPRDPRD==.
      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\XXEXTSIS.EXT".
         COPY "J:\PRGNEW\VFEV99\AJUDA\WSPAD.".
         COPY "J:\PRGNEW\VFEV99\AJUDA\WSPAD2.".
      *----------------------------------------------------------------*
         LINKAGE SECTION.
         01  WPARAMETROS        PIC  X(256).
      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         SCREEN SECTION.
         01  T1.
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  BLANK SCREEN.
             05  FRAME
                 LINE   01,00 LINES 52,40 CELLS
                 COL    01,60 SIZE  87,90 CELLS
                 .
             05  FRAME
                 LINE + 02,80 LINES 03,00 CELLS
                 COL    03,60 SIZE  84,00 CELLS
                 LOWERED  ALTERNATE
                 .
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  LABEL "C¢d. Bem/Inc.          " LINE   04,70 COL 06,00.
             05  LABEL "/                      "              COL 30,20.
             05  LABEL "102-Descri‡Æo          " LINE + 03,00 COL 06,00.
             05  LABEL "103-Dados Adicionais   " LINE + 03,50 COL 06,00.
             05  LABEL "104-Matr¡cula          " LINE + 06,50 COL 06,00.
             05  LABEL "105-Grupo              " LINE + 01,50 COL 06,00.
             05  LABEL "106-C¢d. Utiliza‡Æo    " LINE + 01,50 COL 06,00.
             05  LABEL "107-Data InclusÆo      " LINE + 01,50 COL 06,00.
             05  LABEL "108-Data Aquisi‡Æo     " LINE + 01,50 COL 06,00.
             05  LABEL "109-Deprecia ?         " LINE + 01,50 COL 06,00.
             05  LABEL "110-Data In¡cio Depre. " LINE + 01,50 COL 06,00.
             05  LABEL "111-Conta Cont bil     " LINE + 01,50 COL 06,00.
             05  LABEL "112-Centro Custo       " LINE + 01,50 COL 06,00.
             05  LABEL "113-C¢d. Localiza‡Æo   " LINE + 01,50 COL 06,00.
             05  LABEL "114-Motivo Bloqueio    " LINE + 01,50 COL 06,00.
             05  LABEL "115-Modelo NF          " LINE + 01,50 COL 06,00.
             05  LABEL "116-S‚rie"                            COL 41,00.
             05  LABEL "117-N£mero"                           COL 59,00.
             05  LABEL "118-CFOP               " LINE + 01,50 COL 06,00.
             05  LABEL "119-Data EmissÆo       " LINE + 01,50 COL 06,00.
             05  LABEL "120-Chave NFE          " LINE + 01,50 COL 06,00.
             05  LABEL "121-Origem Bem         " LINE + 01,50 COL 06,00.
             05  LABEL "122-CNPJ Fornecedor    " LINE + 01,50 COL 06,00.
             05  LABEL "123-1§ Propriet rio ?  " LINE + 01,50 COL 06,00.
             05  LABEL "124-Leasing ?          " LINE + 01,50 COL 06,00.
             05  LABEL "125-N§ Meses           " LINE + 01,50 COL 06,00.
             05  LABEL "126-C¢d. Produto       " LINE + 01,50 COL 06,00.
      *................................................................*
         01  D100 BEFORE B-D100.
             04  COLOR IS XXEXTSIS-USU-CORDIS.
             05  LABEL ATCADBEM-CODBEM
                 LINE   04,70 COL   20,00
                 .
             05  LABEL ATCADBEM-SUBCODBEM
                              COL   32,00
                 .
             04  COLOR IS WCOR.
             05  ENTRY-FIELD VALUE MULTIPLE ATCADBEM-DESCR
                 LINE   07,70 COL   26,00 LINES 02,00
                 SIZE   40,00 MAX-LINES = 02
                 ENABLED = 0
                 FONT WFONT-TIPO
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD VALUE MULTIPLE ATCADBEM-CDESCR
                 LINE + 04,00 COL   26,00 LINES 04,00
                 SIZE   50,00 MAX-LINES = 04
                 ENABLED = 0
                 FONT WFONT-TIPO
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 06,00 COL   26,00 LINES 01,20 CELLS
                 PIC 9(009)           USING ATCADBEM-MATRICULA
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   26,00 LINES 01,20 CELLS
                 PIC 9(006)           USING ATCADBEM-GRUPO
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             04  O105.
             05  COLOR IS XXEXTSIS-USU-CORDIS.
             07  LABEL WX-GRUPO
                 LINE - 00,20 COL   37,00
                 .
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE + 01,70 COL   26,00 LINES 01,20 CELLS
                 PIC 9(003)           USING ATCADBEM-COD-UTILIZACAO
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             04  O106.
             05  COLOR IS XXEXTSIS-USU-CORDIS.
             07  LABEL WX-UTILIZACAO
                 LINE - 00,20 COL   34,00
                 .
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE + 01,70 COL   26,00 LINES 01,20 CELLS
                 PIC 99/99/9999       USING WDT-INC-D
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   26,00 LINES 01,20 CELLS
                 PIC 99/99/9999       USING WDT-AQUI-D
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  CHECK-BOX "&Sim"
                 LINE + 01,50 COL   26,00
                 USING ATCADBEM-DEPRECIA
                 NOTIFY
                 ENABLED = 0
                 .
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE + 01,50 COL   26,00 LINES 01,20 CELLS
                 PIC 99/99/9999       USING WDT-DEPRE-D
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   26,00 LINES 01,20 CELLS
                 PIC 9(007)           USING ATCADBEM-CTA-CTBL
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             04  O111.
             05  COLOR IS XXEXTSIS-USU-CORDIS.
             07  LABEL WX-CTA-CTBL
                 LINE - 00,20 COL   38,90
                 .
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE + 01,70 COL   26,00 LINES 01,20 CELLS
                 PIC 9(003)           USING ATCADBEM-CCUSTO
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             04  O112.
             05  COLOR IS XXEXTSIS-USU-CORDIS.
             07  LABEL WX-CCUSTO
                 LINE - 00,20 COL   34,00
                 .
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE + 01,70 COL   26,00 LINES 01,20 CELLS
                 PIC 9(003)           USING ATCADBEM-COD-LOCALIZACAO
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             04  O113.
             05  COLOR IS XXEXTSIS-USU-CORDIS.
             07  LABEL WX-LOCALIZACAO
                 LINE - 00,20 COL   34,00
                 .
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE + 01,70 COL   26,00 LINES 01,20 CELLS
                 PIC 9(003)           USING ATCADBEM-MOTBLQ-BEM
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             04  O114.
             05  COLOR IS XXEXTSIS-USU-CORDIS.
             07  LABEL WX-MOTBLQ
                 LINE - 00,20 COL   34,00
                 .
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE + 01,70 COL   26,00 LINES 01,20 CELLS
                 PIC 9(002)           USING ATCADBEM-MODELO
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                              COL   50,00 LINES 01,20 CELLS
                 PIC X(003)           USING ATCADBEM-SERNF
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                              COL   70,00 LINES 01,20 CELLS
                 PIC 9(009)           USING ATCADBEM-NUMNF
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   26,00 LINES 01,20 CELLS
                 PIC 9(004)           USING ATCADBEM-CFOP
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   26,00 LINES 01,20 CELLS
                 PIC 99/99/9999       USING WDT-EMINF-D
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   26,00 LINES 01,20 CELLS
                 PIC X(044)           USING ATCADBEM-CHAVE-NFE
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   26,00 LINES 01,20 CELLS
                 PIC 9(002)           USING ATCADBEM-ORIGEM-BEM
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             04  O121.
             05  COLOR IS XXEXTSIS-USU-CORDIS.
             07  LABEL WX-DESCR-ORIGEM
                 LINE - 00,20 COL   34,00
                 .
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE + 01,70 COL   26,00 LINES 01,20 CELLS
                 PIC ZZZZZZZZ9        USING WS-P1-CNPJ
                 ENABLED = 0
                 3-D
                 RIGHT
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 00,00 COL   39,80 LINES 01,20 CELLS
                 PIC ZZZ9             USING WS-P2-CNPJ
                 ENABLED = 0
                 3-D
                 RIGHT
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 00,00 COL   47,80 LINES 01,20 CELLS
                 PIC Z9               USING WS-P3-CNPJ
                 ENABLED = 0
                 3-D
                 RIGHT
                 AUTO
                 .
             04  O122.
             05  COLOR IS XXEXTSIS-USU-CORDIS.
             07  LABEL WX-FORNECEDOR
                 LINE - 00,20 COL   56,00
                 .
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  CHECK-BOX "S&im"
                 LINE + 01,70 COL   26,00
                 USING ATCADBEM-PRIM-PROPRIETARIO
                 ENABLED = 0
                 .
             05  CHECK-BOX "Si&m"
                 LINE + 01,50 COL   26,00
                 USING ATCADBEM-LEASING
                 ENABLED = 0
                 .
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE + 01,50 COL   26,00 LINES 01,20 CELLS
                 PIC 9(003)           USING ATCADBEM-MESES-LEASING
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   26,00 LINES 01,20 CELLS
                 PIC ZZZZZZZZZ9       USING ATCADBEM-CODR-PRODUTO
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             04  O126.
             05  COLOR IS XXEXTSIS-USU-CORDIS.
             07  LABEL WX-PRODUTO
                 LINE - 00,20 COL   42,00
                 .
      *................................................................*
             05  PUSH-BUTTON "  &OK   "
                 LINE   49,70 COL    25,90 LINES 01,80 CELLS
                                           SIZE  13,00 CELLS
                 TERMINATION-VALUE = 900
                      ID 132
                 ENABLED = WDADOS-ENAB
                 SELF-ACT
                 .
             05  PUSH-BUTTON "&CANCELA"
                              COL +  01,50 LINES 01,80 CELLS
                                           SIZE  13,00 CELLS
                 TERMINATION-VALUE = 027
                      ID 133
                 ENABLED = WDADOS-ENAB
                 SELF-ACT
                 .
             05  PUSH-BUTTON "&ANTERIOR"
                              COL +  01,50 LINES 01,80 CELLS
                                           SIZE  13,00 CELLS
                 TERMINATION-VALUE = 799
                      ID 134
                 ENABLED = WTBAR-ANTE
                 SELF-ACT
                 .
             05  PUSH-BUTTON "&PRàXIMO"
                              COL  + 01,50 LINES 01,80 CELLS
                                           SIZE  13,00 CELLS
                 TERMINATION-VALUE = 798
                      ID 135
                 ENABLED = WTBAR-PROX
                 SELF-ACT
                 .
      *................................................................*
         01  T2.
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  BLANK SCREEN.
             05  FRAME
                 LINE   01,00 LINES 52,40 CELLS
                 COL    01,60 SIZE  87,90 CELLS
                 .
             05  FRAME
                 LINE + 02,80 LINES 03,00 CELLS
                 COL    03,60 SIZE  84,00 CELLS
                 LOWERED  ALTERNATE
                 .
             04  COLOR IS XXEXTSIS-USU-CORNAC.
             05  FRAME "Dados do Bem"
                 LINE   06,60 LINES 11,60 CELLS
                 COL    03,60 SIZE  83,00 CELLS
                 RAISED   ALTERNATE
                 .
             05  FRAME "CIAP"
                 LINE   18,60 LINES 15,60 CELLS
                 COL    03,60 SIZE  83,00 CELLS
                 RAISED   ALTERNATE
                 .

             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  LABEL "C¢d. Bem/Inc.            "
                                                 LINE   04,70 COL 06,00.
             05  LABEL "/                        "
                                                              COL 30,20.
             05  LABEL "202-Valor do Bem         "
                                                 LINE + 03,00 COL 06,00.
             05  LABEL "203-Valor Residual Ctbl  "
                                                 LINE + 01,50 COL 06,00.
             05  LABEL "204-Qtde Adquirida       "
                                                 LINE + 01,50 COL 06,00.
             05  LABEL "205-Taxa de Deprecia‡Æo  "
                                                 LINE + 01,50 COL 06,00.
             05  LABEL "206-Valor Deprec. Acumul."
                                                 LINE + 01,50 COL 06,00.
             05  LABEL "207-Meses j  Depreciado  "
                                                 LINE + 01,50 COL 06,00.
             05  LABEL "208-Base ICMS            "
                                                 LINE + 04,50 COL 06,00.
             05  LABEL "209-Valor do ICMS        "
                                                 LINE + 01,50 COL 06,00.
             05  LABEL "210-Diferencial de Aliq  "
                                                 LINE + 01,50 COL 06,00.
             05  LABEL "211-Valor do ICMS-ST     "
                                                 LINE + 01,50 COL 06,00.
             05  LABEL "212-Valor ICMS Frete     "
                                                 LINE + 01,50 COL 06,00.
             04  COLOR IS XXEXTSIS-USU-CORNAC.
             05  LABEL "213-Total do ICMS        "
                                                 LINE + 01,50 COL 06,00.
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  LABEL "214-Parcelas j  Creditada"
                                                 LINE + 01,50 COL 06,00.
             05  LABEL "215-N£mero Livro         "
                                                 LINE + 01,50 COL 06,00.
             05  LABEL "216-N£mero P gina        "
                                                 LINE + 01,50 COL 06,00.

      *................................................................*
         01  D200 BEFORE B-D200.
             04  COLOR IS XXEXTSIS-USU-CORDIS.
             05  LABEL ATCADBEM-CODBEM
                 LINE   04,70 COL   20,00
                 .
             05  LABEL ATCADBEM-SUBCODBEM
                              COL   32,00
                 .
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE   07,70 COL   28,00 LINES 01,20 CELLS
                 PIC Z.ZZZ.ZZZ.ZZ9,99 USING ATCADBEM-VLR-BEM
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   28,00 LINES 01,20 CELLS
                 PIC Z.ZZZ.ZZZ.ZZ9,99 USING ATCADBEM-VLR-RESIDUAL-CTBL
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   28,00 LINES 01,20 CELLS
                 PIC ZZZZZZZZZ9       USING ATCADBEM-QTDE-AQUIS
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   28,00 LINES 01,20 CELLS
                 PIC ZZZ,9999         USING ATCADBEM-TAXA-DEPREC
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   28,00 LINES 01,20 CELLS
                 PIC Z.ZZZ.ZZZ.ZZ9,99 USING ATCADBEM-VLR-DEPREC-ACU
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   28,00 LINES 01,20 CELLS
                 PIC ZZ9              USING ATCADBEM-MESES-JA-DEPREC
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 04,50 COL   28,00 LINES 01,20 CELLS
                 PIC Z.ZZZ.ZZZ.ZZ9,99 USING ATCADBEM-BASEICM
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   28,00 LINES 01,20 CELLS
                 PIC Z.ZZZ.ZZZ.ZZ9,99 USING ATCADBEM-VALICM
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   28,00 LINES 01,20 CELLS
                 PIC Z.ZZZ.ZZZ.ZZ9,99 USING ATCADBEM-VLR-DIFALIQ
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   28,00 LINES 01,20 CELLS
                 PIC Z.ZZZ.ZZZ.ZZ9,99 USING ATCADBEM-ICMS-ST
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   28,00 LINES 01,20 CELLS
                 PIC Z.ZZZ.ZZZ.ZZ9,99 USING ATCADBEM-ICMS-FRETE
                 ENABLED = 0
                 3-D
                 AUTO
                 .

             05  ENTRY-FIELD
                 LINE + 01,50 COL   28,00 LINES 01,20 CELLS
                 PIC Z.ZZZ.ZZZ.ZZ9,99 USING WN-TOTAL-ICMS
                 ENABLED = 0
                 3-D
                 AUTO
                 .

             05  ENTRY-FIELD
                 LINE + 01,50 COL   28,00 LINES 01,20 CELLS
                 PIC ZZ9            USING ATCADBEM-PARCELAS-JA-CRED-CIAP
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   28,00 LINES 01,20 CELLS
                 PIC ZZZ.ZZ9          USING ATCADBEM-NUMERO-LIVRO
                 ENABLED = 0
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   28,00 LINES 01,20 CELLS
                 PIC ZZZ.ZZ9          USING ATCADBEM-NUMERO-PAGINA
                 ENABLED = 0
                 3-D
                 AUTO
                 .
      *................................................................*
             05  PUSH-BUTTON "&OK"
                 LINE   34,70 COL  25,9    LINES 01,80 CELLS
                                           SIZE  13,00 CELLS
                 TERMINATION-VALUE = 900
                      ID 217
                 SELF-ACT
                 ENABLED = WDADOS-ENAB
                 .
             05  PUSH-BUTTON "&CANCELA"
                              COL +  01,50 LINES 01,80 CELLS
                                           SIZE  13,00 CELLS
                 TERMINATION-VALUE = 027
                      ID 218
                 SELF-ACT
                 ENABLED = WDADOS-ENAB
                 .
             05  PUSH-BUTTON "&ANTERIOR"
                              COL +  01,50 LINES 01,80 CELLS
                                           SIZE  13,00 CELLS
                 TERMINATION-VALUE = 799
                      ID 219
                 ENABLED = WTBAR-ANTE
                 SELF-ACT
                 .
             05  PUSH-BUTTON "&PRàXIMO"
                              COL  + 01,50 LINES 01,80 CELLS
                                           SIZE  13,00 CELLS
                 TERMINATION-VALUE = 798
                      ID 220
                 ENABLED = WTBAR-PROX
                 SELF-ACT
                 .
      *................................................................*
         COPY "J:\PRGNEW\GE\BOK\GETBAR.SCR".
         COPY "J:\PRGNEW\GE\BOK\GETBAR3.SCR".
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

      *          MOVE 4628                TO XXEXTSIS-COM-CALLSUBNR.

      *   COPY "J:\PRGNEW\GE\BOK\GETSACE.PRO".

                 DISPLAY FLOATING WINDOW
                 LINE -03,50 COL 01,00 LINES 52,80 SIZE 89,00
                 CONTROL FONT IS XXEXTSIS-COM-FONT-TELA
                 COLOR XXEXTSIS-USU-CORTEL
                 TITLE WTITULO
                 SYSTEM MENU
                 TITLE-BAR
                 HANDLE IN WJANELA1.

                 IF XXEXTSIS-COM-FUNCAO = "IN" OR "DU"
                     MOVE 0            TO WTBAR-ANTESN
                     MOVE 0            TO WTBAR-PROXSN
                  ELSE
                     MOVE 1            TO WTBAR-ANTESN
                     MOVE 1            TO WTBAR-PROXSN
                 END-IF.
                 MOVE WTBAR-ANTESN     TO WTBAR-ANTE.
                 MOVE WTBAR-PROXSN     TO WTBAR-PROX.

         COPY "J:\PRGNEW\GE\BOK\GETBAR.PRO".
      *----------------------------------------------------------------*
                  DISPLAY TAB-CONTROL UPON WJANELA1
                  LINE  02,00 COL 02,40 LINES 50,80 CELLS
                                        SIZE  86,30 CELLS
                  MULTILINE
                  HANDLE IN WTAB-CONTROL.
                  MODIFY WTAB-CONTROL TAB-TO-ADD =
                   ( '&Dados Cadastrais','&Valores Bem/Impostos').
      *----------------------------------------------------------------*
                DISPLAY T1.
                MOVE WPARAMETROS         TO WPARMATCADBEM.
                MODIFY WJANELA1 TITLE WTITULO.
                MOVE ZEROS            TO XXEXTSIS-COM-NRCPO.
                SET WTAB-VALUE TO 01.
                MODIFY WTAB-CONTROL, VALUE WTAB-VALUE.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.EMI"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBEM.EMIO"
              REPLACING =='ATCADBEM'== BY ==ATCADBEM==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBEM.EMI"
              REPLACING =='ATCADBEM'== BY ==ESPELHO-ATCADBEM==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBEM.EMI"
              REPLACING =='ATCADBEM'== BY ==ESPELHO2-ATCADBEM==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBLQ.EMI"
              REPLACING =='ATCADBLQ'== BY ==ATCADBLQ==.
         COPY "J:\PRGNEW\AT\BOK\ATCADGRC.EMI"
              REPLACING =='ATCADGRC'== BY ==ATCADGRC==.
         COPY "J:\PRGNEW\AT\BOK\ATCADLOC.EMI"
              REPLACING =='ATCADLOC'== BY ==ATCADLOC==.
         COPY "J:\PRGNEW\AT\BOK\ATCADUTL.EMI"
              REPLACING =='ATCADUTL'== BY ==ATCADUTL==.
         COPY "J:\PRGNEW\CG\BOK\CGPLAPLA.EMI"
              REPLACING =='CGPLAPLA'== BY ==CGPLAPLA==.
         COPY "J:\PRGNEW\CG\BOK\CGGENCCU.EMI"
              REPLACING =='CGGENCCU'== BY ==CGGENCCU==.
         COPY "J:\PRGNEW\LF\BOK\LFMENREL.EMI"
              REPLACING =='LFMENREL'== BY ==LFMENREL==.
         COPY "J:\PRGNEW\PG\BOK\PGCADFOR.EMI"
              REPLACING =='PGCADFOR'== BY ==PGCADFOR==.
         COPY "J:\PRGNEW\VD\BOK\VDPRDPRD.EMI"
              REPLACING =='VDPRDPRD'== BY ==VDPRDPRD==.

      *----------------------------------------------------------------*
         LE-ATCADBEM.
                MOVE WSPAD-CODEMP             TO ATCADBEM-NREMP.
                MOVE WPARMATCADBEM-CODBEM     TO ATCADBEM-CODBEM.
                MOVE WPARMATCADBEM-SUBCODBEM  TO ATCADBEM-SUBCODBEM.
                READ ATCADBEMWITH NO LOCK
                IF XXEXTSIS-COM-FSTATUS  NOT = "00"
                    MOVE "ATCADBEM"      TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    GO TO RETORNA.
      *----------------------------------------------------------------*
         LEU-ATCADBEM.
                MOVE ATCADBEM-REGISTRO   TO LATCADBEM-REGISTRO.
                MOVE ATCADBEM-DT-INCLUSAO      TO WDT8.
                MOVE WANO8                     TO WANO-INC-D.
                MOVE WMES8                     TO WMES-INC-D.
                MOVE WDIA8                     TO WDIA-INC-D.

                MOVE ATCADBEM-DT-AQUISICAO     TO WDT8.
                MOVE WANO8                     TO WANO-AQ-D.
                MOVE WMES8                     TO WMES-AQ-D.
                MOVE WDIA8                     TO WDIA-AQ-D.

                MOVE ATCADBEM-DT-INI-DEPRECIA  TO WDT8.
                MOVE WANO8                     TO WANO-DEPRE-D.
                MOVE WMES8                     TO WMES-DEPRE-D.
                MOVE WDIA8                     TO WDIA-DEPRE-D.

                MOVE ATCADBEM-DT-EMISSAO-NF    TO WDT8.
                MOVE WANO8                     TO WANO-EMINF-D.
                MOVE WMES8                     TO WMES-EMINF-D.
                MOVE WDIA8                     TO WDIA-EMINF-D.

                IF ATCADBEM-DEPRECIA NOT = 1
                   MOVE 0 TO WDADOS-ENAB-01
                ELSE
                   MOVE 1 TO WDADOS-ENAB-01
                END-IF

                IF ATCADBEM-LEASING NOT = 1
                   MOVE 0 TO WDADOS-ENAB-02
                ELSE
                   MOVE 1 TO WDADOS-ENAB-02
                END-IF

                MODIFY WJANELA1 TITLE WTITULO.
                MOVE XXEXTSIS-COM-CORMOS TO WCOR.
                MOVE 1                   TO WCHAVE-ENAB.
                MODIFY WJANELA1 TITLE WTITULO.
      *----------------------------------------------------------------*
         LE-ATCADGRC.
                IF ATCADBEM-GRUPO NOT = ZEROS
                    MOVE WSPAD-CODEMP   TO ATCADGRC-NREMP
                    MOVE ATCADBEM-GRUPO TO ATCADGRC-CODGRU
                    READ ATCADGRC
                    IF XXEXTSIS-COM-FSTATUS NOT = "00"
                        MOVE ALL '?'         TO WX-GRUPO
                    ELSE
                        MOVE ATCADGRC-DESGRU TO WX-GRUPO
                    END-IF
                ELSE
                    MOVE SPACES TO WX-GRUPO
                END-IF
                DISPLAY O105.
      *----------------------------------------------------------------*
         LE-ATCADUTL.
                IF ATCADBEM-COD-UTILIZACAO NOT = ZEROS
                    MOVE WSPAD-CODEMP            TO ATCADUTL-NREMP
                    MOVE ATCADBEM-COD-UTILIZACAO TO ATCADUTL-COD
                    READ ATCADUTL
                    IF XXEXTSIS-COM-FSTATUS NOT = "00"
                        MOVE ALL '?'        TO WX-UTILIZACAO
                    ELSE
                        MOVE ATCADUTL-DESCR TO WX-UTILIZACAO
                    END-IF
                ELSE
                    MOVE SPACES TO WX-UTILIZACAO
                END-IF
                DISPLAY O106.
      *----------------------------------------------------------------*
         LE-CGPLAPLA.
                IF ATCADBEM-CTA-CTBL NOT = ZEROS
                    MOVE ATCADBEM-CTA-CTBL TO CGPLAPLA-CODR
                    READ CGPLAPLA KEY IS CGPLAPLA-CHAVE-CODR
                    IF XXEXTSIS-COM-FSTATUS NOT = "00"
                        MOVE ALL '?'        TO WX-CTA-CTBL
                    ELSE
                        MOVE CGPLAPLA-HIST  TO WX-CTA-CTBL
                    END-IF
                ELSE
                    MOVE SPACES TO WX-CTA-CTBL
                END-IF
                DISPLAY O111.
      *----------------------------------------------------------------*
         LE-CGGENCCU.
                IF ATCADBEM-CCUSTO NOT = ZEROS
                    MOVE ATCADBEM-CCUSTO TO CGGENCCU-COD
                    READ CGGENCCU
                    IF XXEXTSIS-COM-FSTATUS NOT = "00"
                        MOVE ALL '?'            TO WX-CCUSTO
                    ELSE
                        MOVE CGGENCCU-DESCRICAO TO WX-CCUSTO
                    END-IF
                ELSE
                    MOVE SPACES TO WX-CCUSTO
                END-IF
                DISPLAY O112.
      *----------------------------------------------------------------*
         LE-ATCADLOC.
                IF ATCADBEM-COD-LOCALIZACAO NOT = ZEROS
                    MOVE WSPAD-CODEMP            TO ATCADLOC-NREMP
                    MOVE ATCADBEM-COD-LOCALIZACAO TO ATCADLOC-COD
                    READ ATCADLOC
                    IF XXEXTSIS-COM-FSTATUS NOT = "00"
                        MOVE ALL '?'        TO WX-LOCALIZACAO
                    ELSE
                        MOVE ATCADLOC-DESCR TO WX-LOCALIZACAO
                    END-IF
                ELSE
                    MOVE SPACES TO WX-LOCALIZACAO
                END-IF
                DISPLAY O113.
      *----------------------------------------------------------------*
         LE-ATCADBLQ.
                IF ATCADBEM-MOTBLQ-BEM NOT = ZEROS
                    MOVE WSPAD-CODEMP        TO ATCADBLQ-NREMP
                    MOVE ATCADBEM-MOTBLQ-BEM TO ATCADBLQ-COD
                    READ ATCADBLQ
                    IF XXEXTSIS-COM-FSTATUS NOT = "00"
                        MOVE ALL '?'        TO WX-MOTBLQ
                    ELSE
                        MOVE ATCADBLQ-DESCR TO WX-MOTBLQ
                    END-IF
                ELSE
                    MOVE SPACES TO WX-MOTBLQ
                END-IF
                DISPLAY O114.
      *----------------------------------------------------------------*
         LE-ORIGEM.
                IF ATCADBEM-ORIGEM-BEM NOT = ZEROS
                    IF XXEXTSIS-COM-FSTATUS NOT = "00"
                        MOVE ALL '?'        TO WX-DESCR-ORIGEM
                    ELSE
                        MOVE ATCADBLQ-DESCR TO WX-DESCR-ORIGEM
                    END-IF
                ELSE
                    MOVE SPACES TO WX-DESCR-ORIGEM
                END-IF
                DISPLAY O121.
      *----------------------------------------------------------------*
         LE-PGCADFOR.
                IF ATCADBEM-CNPJ-FORNEC NOT = ZEROS
                    MOVE ATCADBEM-CNPJ-FORNEC TO PGCADFOR-CGC
                                                 WS-CNPJ
                    READ PGCADFOR
                    IF XXEXTSIS-COM-FSTATUS NOT = "00"
                        MOVE ALL '?'        TO WX-FORNECEDOR
                    ELSE
                        MOVE PGCADFOR-RAZAO TO WX-FORNECEDOR
                    END-IF
                ELSE
                    MOVE SPACES TO WX-FORNECEDOR
                END-IF
                DISPLAY O122.
      *----------------------------------------------------------------*
         LE-VDPRDPRD.
                IF ATCADBEM-CODR-PRODUTO NOT = ZEROS
                    MOVE ATCADBEM-CODR-PRODUTO TO VDPRDPRD-CODR
                    READ VDPRDPRD KEY IS VDPRDPRD-CHAVE-CODR
                    IF XXEXTSIS-COM-FSTATUS NOT = "00"
                        MOVE ALL '?'        TO WX-PRODUTO
                    ELSE
                        MOVE VDPRDPRD-DESCR TO WX-PRODUTO
                    END-IF
                ELSE
                    MOVE SPACES TO WX-PRODUTO
                END-IF
                DISPLAY O126.
      *----------------------------------------------------------------*
         LE-ATORBEMP.
                MOVE "A"                 TO WPARMATORBEMP-MODAL
                MOVE ATCADBEM-ORIGEM-BEM TO WPARMATORBEMP-COD
                MOVE "ATORBEMP"          TO XXEXTSIS-COM-CALLPRX
                MOVE XXEXTSIS-VER-AT     TO XXEXTSIS-COM-CALLPRX (9:4)
                CALL  XXEXTSIS-COM-CALLPRX USING WPARMATORBEMP
                CANCEL XXEXTSIS-COM-CALLPRX
                MOVE WPARMATORBEMP-DESCR TO  WX-DESCR-ORIGEM
                IF ATCADBEM-ORIGEM-BEM   =  ZEROS
                   MOVE SPACES           TO  WX-DESCR-ORIGEM
                END-IF.
      *----------------------------------------------------------------*
                MODIFY WJANELA1 TITLE WTITULO.
      *----------------------------------------------------------------*
                MOVE XXEXTSIS-COM-CORMOS TO WCOR.
                MOVE 1                   TO WDADOS-ENAB.
      *----------------------------------------------------------------*
         CONFIRMA.
                MOVE ALL "N"          TO WSCR-CAMPO-OKT.
                MOVE 1                TO WDADOS-ENAB.
                DESTROY T1 T2.

                DESTROY D100 D200.

                DESTROY O105 O106 O111 O112 O113 O114 O121 O122 O126.
      *-----------------------------------------------------------------
         VALIDA-TAB.
                EVALUATE WTAB-VALUE

                WHEN 01
                    DISPLAY T1
                    DISPLAY D100
                    GO TO PEGA-DADOS-T1
      *----------------------------------------------------------------*
                WHEN 02
                    DISPLAY T2
                    COMPUTE WN-TOTAL-ICMS  =  ATCADBEM-VALICM         +
                                              ATCADBEM-VLR-DIFALIQ    +
                                              ATCADBEM-ICMS-ST        +
                                              ATCADBEM-ICMS-FRETE
                    DISPLAY D200
                    GO TO PEGA-DADOS-T2
      *----------------------------------------------------------------*
                WHEN 03
                    GO TO RETORNA

                END-EVALUATE.
      *----------------------------------------------------------------*
         PEGA-DADOS-T1.
                ACCEPT D100.
                SET ENVIRONMENT "CURSOR-MODE" TO XXEXTSIS-COM-CURSORORI.
                IF WTECLA-ENTER OR WTECLA-CANC OR WTECLA-OK
                    GO TO RETORNA.
                IF WTECLA-ESC
                    GO TO RETORNA.
                IF WTECLA-INATI
                    GO TO RETORNA.
                IF WTECLA-FECHAR AND
                   WEVE-TYPE = CMD-TABCHANGED
                   INQUIRE WTAB-CONTROL VALUE IN WTAB-VALUE
                   GO CONFIRMA
                END-IF.
                IF WTECLA-FECHAR
                    GO TO RETORNA.
                IF WTECLA-SETAB OR WTECLA-PROX
                    GO TO MOSTRA-PROXIMO.
                IF WTECLA-SETAC OR WTECLA-ANTE
                    GO TO MOSTRA-ANTERIOR.
                PERFORM VE-TECLA    THRU FVE-TECLA.
                GO TO CONFIRMA.
      *----------------------------------------------------------------*
         MOSTRA-ANTERIOR.
                READ ATCADBEM PREVIOUS.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE "ATCADBEM"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    START ATCADBEM KEY >= ATCADBEM-CHAVE
                    READ ATCADBEM
                    GO TO RETORNA
                END-IF.
                GO TO LEU-ATCADBEM.
      *----------------------------------------------------------------*
         MOSTRA-PROXIMO.
                READ ATCADBEM NEXT.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE "ATCADBEM"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    START ATCADBEM KEY <= ATCADBEM-CHAVE
                    READ ATCADBEM
                    GO TO RETORNA
                END-IF.
                GO TO LEU-ATCADBEM.
      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         B-D100.
         COPY "J:\PRGNEW\GE\BOK\GEDETBAR.PRO".
                EVALUATE WSCR-CAMPO
                WHEN 028
                  MOVE "TECLE ENTER OU CLICK NO BOTÇO PARA CONFIRMAR   "
                                      TO XXEXTSIS-COM-MSG
                WHEN 029
                  MOVE "TECLE ENTER OU CLICK NO BOTÇO PARA CANCELAR    "
                                      TO XXEXTSIS-COM-MSG
                WHEN 030
                 MOVE "CLICK NO BOTÇO PARA ATUALIZAR E IR PARA ANTERIOR"
                                      TO XXEXTSIS-COM-MSG
                WHEN 031
                  MOVE "CLICK NO BOTÇO PARA ATUALIZAR E IR PARA PRàXIMO"
                                      TO XXEXTSIS-COM-MSG
                END-EVALUATE.
                DISPLAY T101A UPON WTBAR.
                DISPLAY TMSGI UPON XXEXTSIS-COM-MWINDOW.
      *----------------------------------------------------------------*
         PEGA-DADOS-T2.
                ACCEPT D200.
                SET ENVIRONMENT "CURSOR-MODE" TO XXEXTSIS-COM-CURSORORI.
                IF WTECLA-ENTER OR WTECLA-OK
                    GO TO RETORNA.
                IF WTECLA-ESC
                    GO TO RETORNA.
                IF WTECLA-INATI
                    GO TO RETORNA.
                IF WTECLA-FECHAR AND
                   WEVE-TYPE = CMD-TABCHANGED
                   INQUIRE WTAB-CONTROL VALUE IN WTAB-VALUE
                   GO CONFIRMA
                END-IF.
                IF WTECLA-FECHAR
                    GO TO RETORNA.
                IF WTECLA-SETAB OR WTECLA-PROX
                    GO TO MOSTRA-PROXIMO.
                IF WTECLA-SETAC OR WTECLA-ANTE
                    GO TO MOSTRA-ANTERIOR.
                PERFORM VE-TECLA    THRU FVE-TECLA.
                GO TO CONFIRMA.
      *----------------------------------------------------------------*
         B-D200.
         COPY "J:\PRGNEW\GE\BOK\GEDETBAR.PRO".
                EVALUATE WSCR-CAMPO
                WHEN 016
                  MOVE "TECLE ENTER OU CLICK NO BOTÇO PARA CONFIRMAR   "
                                      TO XXEXTSIS-COM-MSG
                WHEN 017
                  MOVE "TECLE ENTER OU CLICK NO BOTÇO PARA CANCELAR    "
                                      TO XXEXTSIS-COM-MSG
                WHEN 018
                 MOVE "CLICK NO BOTÇO PARA ATUALIZAR E IR PARA ANTERIOR"
                                      TO XXEXTSIS-COM-MSG
                WHEN 019
                  MOVE "CLICK NO BOTÇO PARA ATUALIZAR E IR PARA PRàXIMO"
                                      TO XXEXTSIS-COM-MSG
                END-EVALUATE.
                DISPLAY T101A UPON WTBAR.
                DISPLAY TMSGI UPON XXEXTSIS-COM-MWINDOW.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GETECLA.PRO".
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GEPERFOR.PRO".
      *----------------------------------------------------------------*
         RETORNA.
         COPY "J:\PRGNEW\VD\BOK\VDPRDPRD.ECLO"
              REPLACING =='VDPRDPRD'== BY ==VDPRDPRD==.
         COPY "J:\PRGNEW\PG\BOK\PGCADFOR.ECLO"
              REPLACING =='PGCADFOR'== BY ==PGCADFOR==.
         COPY "J:\PRGNEW\LF\BOK\LFMENREL.ECLO"
              REPLACING =='LFMENREL'== BY ==LFMENREL==.
         COPY "J:\PRGNEW\CG\BOK\CGGENCCU.ECLO"
              REPLACING =='CGGENCCU'== BY ==CGGENCCU==.
         COPY "J:\PRGNEW\CG\BOK\CGPLAPLA.ECLO"
              REPLACING =='CGPLAPLA'== BY ==CGPLAPLA==.
         COPY "J:\PRGNEW\AT\BOK\ATCADUTL.ECLO"
              REPLACING =='ATCADUTL'== BY ==ATCADUTL==.
         COPY "J:\PRGNEW\AT\BOK\ATCADLOC.ECLO"
              REPLACING =='ATCADLOC'== BY ==ATCADLOC==.
         COPY "J:\PRGNEW\AT\BOK\ATCADGRC.ECLO"
              REPLACING =='ATCADGRC'== BY ==ATCADGRC==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBLQ.ECLO"
              REPLACING =='ATCADBLQ'== BY ==ATCADBLQ==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBEM.ECLO"
              REPLACING =='ATCADBEM'== BY ==ESPELHO2-ATCADBEM==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBEM.ECLO"
              REPLACING =='ATCADBEM'== BY ==ESPELHO-ATCADBEM==.
         COPY "J:\PRGNEW\AT\BOK\ATCADBEM.ECLO"
              REPLACING =='ATCADBEM'== BY ==ATCADBEM==.
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.ECLO"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.

         RETORNAF.
         COPY "J:\PRGNEW\VD\BOK\VDRESUB.PRO".
                EXIT PROGRAM.
      *================================================================*
