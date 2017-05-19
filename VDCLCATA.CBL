         IDENTIFICATION DIVISION.
         PROGRAM-ID.    VDCLCATX.
         SECURITY.
      ******************************************************************
      * ALTERA CANAL DE VENDAS                                         *
      ******************************************************************
      *--------------  O B S E R V A C O E S  -------------------------*
      * - *+FFF+ LIBERAR NO FUTURO                                     *
      * -                                                              *
      * - OBS: ESTE PROGRAMA CONTM CAMPOS RADIO SEGUIDOS. EM CASO DE  *
      * -      ALTERA€ÇO DE SCREEN, FAVOR REPOSICIONAR OS WSCR-CAMPO   *
      * -      NAS ROTINAS DE EVENT DESTES CAMPOS...                   *
      * -                                                              *
      *--------------  A L T E R A C O E S  ---------------------------*
      * 22/11/2005 COPIADO DO VDCATCOA.CBL                    CRISTIANO*
      * 14/03/2006 INCLUSÇO DE TESTE PARA VERIFICAR SE FOR             *
      *            WTECLA-FECHAR NOS EXCEPTIONS               CRISTIANO*
      * 16/03/2006 INCLUIR CµLCULO DE CASAS DECIMAIS PARA O CPO.       *
      *            XXEXTSIS-LAC-VLR9-ANTE                     CRISTIANO*
      * 17/03/2006 INVERSÇO DA POSI€ÇO DOS BOTåES "PRàXIMO" E          *
      *            "ANTERIOR" NA TELA                         CRISTIANO*
      * 22/03/2006 COLOCADO CAMPO NOVO VDCLICAT-DIVCAN (DIVISAO CANAL) *
      *            NA TELA E NO ARQUIVO. NAO OBRIGATORIO.              *
      *            SE INFORMADO, SERA CONSISTIDO NO VDCLIDIV    MARCELO*
      * 29/05/2006 ACERTO DAS ROTINAS "MOSTRA-ANTERIOR" E              *
      *            "MOSTRA-PROXIMO"                           CRISTIANO*
      * 25/08/2006 INCLUIDO CAMPO 09-DESPREZA EXPORTA€ÇO P/BW   ROBERTO*
      * 16/11/2006 VALIDAR SE REGISTRO CANCELADO DAR MSG 083    ROBERTO*
      * 27/08/2008 GUARDAR NO REGISTRO VDCLICAT                 MARCELO*
      *            A DESCRICAO COMPLEMENTAR DO CANAL.                  *
      * 03/09/2008 AJUSTE TELA                                  MARCELO*
      * 04/09/2008 DESC.COMPLEMENTAR NAO E OBRIGATORIO          MARCELO*
      * 10/09/2008 CORRECAO BOTAO PROXIMO/ANTERIOR              MARCELO*
      * 15/09/2008 AJUSTE TELA                                  MARCELO*
      * 21/12/2009 CAMPO NOVO REGULARIDADE (NO VDCLICAT)        MARCELO*09.12.016
      * 24/08/2010 CONVERSAO SCHINCARIOL.                       MARCELO*10.10.017
      * 24/08/2010 CONVERSAO SCHINCARIOL.                       MARCELO*10.10.017
      *            VDCLICAT-COD PASSA DE 9(02) P/ X(02).               *
      *            VDCLICAT-GRPCAN PASSA DE 9(02) P/ X(02).            *
      *            RETIRADO VDCLICAT, PASSANDO A GRAVAR OS CAMPOS      *
      *            VDCLICAT-DESCR-COMPL E VDCLICAT-LINHAS NO           *
      *            PROPRIO VDCLICAT.                                   *
      * 25/08/2010 CONVERSÇO VDCLIGCA-GRUPO DE PIC 9(2) P/ X(2)  VALDIR*10.10.017
      * 26/08/2010 CONVERSÇO VDCLICAT-COD DE 9(02) P/ X(02)      VALDIR*10.10.017
      *                      VDCLICAT-GRPCAN DE 9(02) P/ X(02)         *
      * 15/10/2010 LIMPEZA VDCLICLI SE DESMARCOU A REGULARI-    MARCELO*10.10.018
      *            DADE NO CANAL.                                      *
      * 22/10/2010 TRATAMENTO CAMPOS EXCLUSIVOS P/ FAB.SCHIN    MARCELO*10.10.019
      *            WDADOS-ENAB-BLOQ=0                                  *
      *            (REVENDAS SCHIN NAO PODERAO ACESSAR ESSES CAMPOS).  *
      * 15/03/2011 ATUALIZAR VERSÇO                              VALDIR*11.03.020
      * 16/05/2011 AJUSTE DEMARCAR TODOS CLIENTES DO CANAL      MARCELO*11.05.021
      *            (LIMPA CAMPOS CANDIDATA E REGULAR) SE O CANAL       *
      *            NAO TRATA REGULARIDADE.                             *
      * 30/05/2011 AJUSTE NAS FD, SELECT, PRM, OPENS, E CLOSE     KENDY*11.05.022
      *    PARA O PADRÇO NOVO                                          *
      * 01/08/2011 NAS LUPAS QUE SEJAM DE CADASTROS RESTRITOS,  MARCELO*11.08.023
      *            SERA VERIFICADO ESSA SITUACAO.                      *
      * 15/08/2011 INCLUSAO DE 2 NOVOS CAMPOS NA TELA             KENDY*11.08.024
      *               - TIPO NEGOCIO / - SEGMENTO                      *
      * 07/11/2011 AJUSTE ROTINA GETSFXRE.PRO (RECOMPILADO)     MARCELO*11.11.025
      * 02/04/2012 AJUSTE.                                      MARCELO*12.04.026
      * 12/07/2012 RECOMPILADO.                                 MARCELO*12.07.027
      * 23/07/2012 AJUSTE MENSAGEM DENTRO DA ROT GETSALTE.PRO   MARCELO*12.07.028
      *            QUANDO FOR FABRICA E ESTIVER ALTERANDO REGISTRO     *
      *            FORA DE SUA FAIXA, SERA APRESENTADA MSG:            *
      *            "ALTERACAO FABRICA FORA DE SUA FAIXA:"              *
      *            "FAIXA RESTRITA: XXXXXXXXX ATE XXXXXXXXX"           *
      * 18/12/2013 RECOMPILADO                                    KENDY*13.12.029
      * 30/12/2013 MODIFICAR CAMPO 107 DE CHECK-BOX P/RADIO-BUTTON ALAN*13.12.030
      * 28/01/2014 INCLUSAO CAMPO 14-TEMPO MEDIO VISITA         ROBERTO*14.01.031
      * 20/05/2014 OS-3164: RECOMPILADO POR CAUSA DA            MARCELO*14.05.033
      *                     BOOK GETSFXRE.PRO                          *
      * 21/10/2015 AJUSTE.                                      MARCELO*15.10.034
      *16/06/2016 OS-7036: INCLUIR CAMPO 15 E 16                ROBERTO*16.06.035
      
      * 08/03/2017 OS-8643: CONTROLE DE MANUTENCAO DE CADASTROS        *
      *                     CHAMADA AO VDCADFLC PARA REGISTRAR         *
      *                     INC/ALT/EXC/DUP PARA ALIMENTAR             *
      *                     ARQUIVO NOVO "VDCADFLC.VFE"                *
      *                     QUE SERVIRA TAMBEM PARA O PORTAL.     DIEGO*17.03.036
      *================================================================*   VERSAO
         ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
         SPECIAL-NAMES.
         COPY "J:\PRGNEW\GE\BOK\GESPECI.ENV".
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.ESDA"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.
         COPY "J:\PRGNEW\VD\BOK\VDCLICAT.ESDA"
              REPLACING =='VDCLICAT'== BY ==VDCLICAT==.
151010   COPY "J:\PRGNEW\VD\BOK\VDCLICLI.ESDA"
151010        REPLACING =='VDCLICLI'== BY ==VDCLICLI==.
         COPY "J:\PRGNEW\VD\BOK\VDCLIDIV.ESDA"
              REPLACING =='VDCLIDIV'== BY ==VDCLIDIV==.
         COPY "J:\PRGNEW\VD\BOK\VDCLIGCA.ESDA"
              REPLACING =='VDCLIGCA'== BY ==VDCLIGCA==.
150811   COPY "J:\PRGNEW\GE\BOK\GEGENCGM.ESDA"
150811        REPLACING =='GEGENCGM'== BY ==GEGENCGM==.
      *================================================================*
      /
      *================================================================*
         DATA DIVISION.
         FILE SECTION.
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.EFDE"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.
         COPY "J:\PRGNEW\VD\BOK\VDCLICAT.EFDE"
              REPLACING =='VDCLICAT'== BY ==VDCLICAT==.
151010   COPY "J:\PRGNEW\VD\BOK\VDCLICLI.EFDE"
151010        REPLACING =='VDCLICLI'== BY ==VDCLICLI==.
         COPY "J:\PRGNEW\VD\BOK\VDCLIDIV.EFDE"
              REPLACING =='VDCLIDIV'== BY ==VDCLIDIV==.
         COPY "J:\PRGNEW\VD\BOK\VDCLIGCA.EFDE"
              REPLACING =='VDCLIGCA'== BY ==VDCLIGCA==.
150811   COPY "J:\PRGNEW\GE\BOK\GEGENCGM.EFDE"
150811        REPLACING =='GEGENCGM'== BY ==GEGENCGM==.
      *---------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         WORKING-STORAGE SECTION.
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         77 WTPGM-HEADER        PIC  X(019) VALUE "=>CONTROLE VERSAO<=".
         77 WTPGM-VERSAO        PIC  X(009) VALUE "17.03.036".
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         COPY "J:\PRGNEW\GE\BOK\GEDIR.WOR".
         COPY "J:\PRGNEW\VD\BOK\VDDIR.WOR".
         COPY "J:\PRGNEW\GE\BOK\ACTIVEX.WOR".
         COPY "J:\PRGNEW\GE\BOK\ACUCOBOL.WOR".
         COPY "J:\PRGNEW\GE\BOK\ACUGUI.WOR".
         COPY "J:\PRGNEW\GE\BOK\CONTROLS.WOR".
         COPY "J:\PRGNEW\GE\BOK\FONTS.WOR".
         77  WCANCSN-ANT        PIC  9(001)    VALUE ZEROS.
         77  WBOTAO-FUNCAO      PIC  A         VALUE SPACES.
         77  WDADOS-ENAB-BLO1   PIC  9(001)    VALUE ZEROS.

         COPY "J:\PRGNEW\GE\BOK\GEWORK.WOR".
         COPY "J:\PRGNEW\VD\BOK\VDWORK.WOR".
         01  HORA-T-VISITA      PIC  9(004) VALUE ZEROS.
         01  HORA-T-VISITA-X  REDEFINES HORA-T-VISITA.
             03 HORA-VISITA       PIC  9(002).
             03 MINUTO-VISITA     PIC  9(002).

         01  ALTA-F-VISITA       PIC  9(004) VALUE ZEROS.
         01  ALTA-F-VISITA-X  REDEFINES ALTA-F-VISITA.
             03 HORA-AF-VISITA   PIC  9(002).
             03 MINUTO-AF-VISITA PIC  9(002).

         01  BAIXA-F-VISITA      PIC  9(004) VALUE ZEROS.
         01  BAIXA-F-VISITA-X  REDEFINES BAIXA-F-VISITA.
             03 HORA-BF-VISITA   PIC  9(002).
             03 MINUTO-BF-VISITA PIC  9(002).

         01  GERAL.
         03  WLOG-DADOS         PIC  X(080) VALUE SPACES.
         03  WLOG-DADOSR        REDEFINES WLOG-DADOS.
             05  WLOG-COD       PIC  X(002).
         03  WTITULO.
             05  F              PIC  X(050)  VALUE
                 " Canal de Vendas".
             05  F              PIC  X(020)  VALUE      SPACES.
             05  WTITULO-FUNCAO PIC  X(032)  VALUE
                 "altera‡Æo".
110811   03  WX-DESCR-SEGMENTO  PIC  X(050)  VALUE SPACES.
110811   03  WX-DESCR-NEGOCIO   PIC  X(050)  VALUE SPACES.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\VD\BOK\VDCLICAT.EFDE"
              REPLACING ==XFD FILE="'VDCLICAT'.VFE"== BY == ==,
                        ==FD  'VDCLICAT'== BY == ==,
                        ==LABEL RECORD IS STANDARD.== BY == ==,
                        =='VDCLICAT'== BY ==LVDCLICAT==.
      *----------------------------------------------------------------*
         01  PARAMETROS.
         COPY "J:\PRGNEW\GE\BOK\GEGENAUD.EPRM"
              REPLACING =='GEGENAUD'== BY ==GEGENAUD==.
150811   COPY "J:\PRGNEW\GE\BOK\GEGENCGM.EPRM"
150811        REPLACING =='GEGENCGM'== BY ==GEGENCGM==.
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.EPRM"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.
         COPY "J:\PRGNEW\VD\BOK\VDCLICAT.EPRM"
              REPLACING =='VDCLICAT'== BY ==VDCLICAT==.
151010   COPY "J:\PRGNEW\VD\BOK\VDCLICLI.EPRM"
151010        REPLACING =='VDCLICLI'== BY ==VDCLICLI==.
         COPY "J:\PRGNEW\VD\BOK\VDCLIDIV.EPRM"
              REPLACING =='VDCLIDIV'== BY ==VDCLIDIV==.
         COPY "J:\PRGNEW\VD\BOK\VDCLIGCA.EPRM"
              REPLACING =='VDCLIGCA'== BY ==VDCLIGCA==.
              
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
         01  WPARAMETROS        PIC  X(034).
      *----------------------------------------------------------------*
      /
      *----------------------------------------------------------------*
         SCREEN SECTION.
         01  T1.
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  BLANK SCREEN.
             05  FRAME
                 LINE   01,00 LINES 31,90 CELLS
                 COL    01,60 SIZE  81,50 CELLS
                 .
             05  FRAME
                 LINE + 00,60 LINES 02,50 CELLS
                 COL    02,40 SIZE  79,80 CELLS
                 LOWERED   ALTERNATE
                 .
             05  FRAME
                 LINE + 02,30 LINES 28,50 CELLS
                 COL    02,40 SIZE  79,80 CELLS
                 RAISED   ALTERNATE
                 .
      *      05  FRAME
      *+FFF+     LINE + 03,80 LINES 04,00 CELLS
      *          COL    02,40 SIZE  82,00 CELLS
      *          LOWERED  ALTERNATE
      *          .
      *................................................................*
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  LABEL "01-C¢digo do Canal"
                 LINE   02,50 COL   04,00
                 .
      *................................................................*
             05  LABEL "02-Nome do Canal"
                 LINE + 02,20 COL   04,00
                 .
             05  LABEL "03-ComissÆo (%)"
                 LINE + 01,50 COL   04,00
                 .
             05  LABEL "04-Grupo de Canal"
                 LINE + 01,50 COL   04,00
                 .
             05  LABEL "05-Limite de Cr‚dito"
                 LINE + 01,50 COL   04,00
                 .
             05  LABEL "06-Canal da Companhia"
                 LINE + 01,50 COL   04,00
                 .
             05  LABEL "07-Tipo (BI)   "
                 LINE + 01,50 COL   04,00
                 .
             05  LABEL "08-DivisÆo de Canal"
                 LINE + 01,50 COL   04,00
                 .
             05  LABEL "09-Despreza Exporta‡Æo p/BW"
                 LINE + 01,50 COL   04,00
                 .
             05  LABEL "10-Descri‡Æo Complementar"
                 LINE + 01,50 COL   04,00
                 .
             05  LABEL "11-Trata Regularidade"
                 LINE + 03,20 COL   04,00
                 .
150811       05  LABEL "12-Segmento"
150811           LINE + 01,50 COL   04,00
150811           .
150811       05  LABEL "13-Tipo Neg¢cio"
150811           LINE + 01,50 COL   04,00
150811           .
280114       05  LABEL "14-Dura‡Æo(m‚dia) Visita"
280114           LINE + 01,50 COL   04,00
280114           .
140616       05  LABEL "15-SLA-Alta Frequˆncia  "
140616           LINE + 01,50 COL   04,00
140616           .
140616       05  LABEL "16-SLA-Baixa Frequˆncia "
140616           LINE + 01,50 COL   04,00
140616           .
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
                 LINE   02,60 COL   28,90 LINES 01,20 CELLS
                 PIC X(002)           USING VDCLICAT-COD
                 ENABLED = 0
                 3-D
                 AUTO
                 .
      *................................................................*
        01  D100 BEFORE B-D100.
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE   04,90 COL   28,90 LINES 01,20 CELLS
                 PIC X(030)           USING VDCLICAT-NOME
                 AFTER A-102  THRU FA-102
                      ID 002
221010           ENABLED = WDADOS-ENAB-BLOQ
                 3-D
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   28,90 LINES 01,20 CELLS
                 PIC Z9,99            USING VDCLICAT-COMIS
                 AFTER A-103  THRU FA-103
                      ID 003
                 ENABLED = WDADOS-ENAB
                 3-D
                 RIGHT
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   28,90 LINES 01,20 CELLS
                 PIC X(002)           USING VDCLICAT-GRPCAN
                 AFTER A-104 THRU FA-104
                      ID 004
221010           ENABLED = WDADOS-ENAB-BLOQ
                 3-D
                 AUTO
                 .
             05  O104.
             07  COLOR IS XXEXTSIS-USU-CORDIS.
             09  LABEL VDCLIGCA-DESCR
                 LINE - 00,20 COL   38,00
                 .
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE + 01,70 COL   28,90 LINES 01,20 CELLS
                 PIC ZZZ.ZZZ.ZZ9,99   USING VDCLICAT-LIMITE
                 AFTER A-105 THRU FA-105
                      ID 005
                 ENABLED = WDADOS-ENAB
                 3-D
                 RIGHT
                 AUTO
                 .
             05  ENTRY-FIELD
                 LINE + 01,50 COL   28,90 LINES 01,20 CELLS
                 PIC X(002)           USING VDCLICAT-CANALCIA-X
                 AFTER A-106  THRU FA-106
                      ID 006
221010           ENABLED = WDADOS-ENAB-BLOQ
                 3-D
                 AUTO
                 .
      *................................................................*
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  RADIO-BUTTON  "&Frio"
                 LINE + 01,50 COL   28,80
                 USING VDCLICAT-AUTOSERV
                 AFTER     A-107   THRU FA-107
                          ID 007
                 EXCEPTION E-107   THRU FE-107
                 ENABLED = WDADOS-ENAB
                 GROUP = 001
                 GROUP-VALUE = 01
                 NO-GROUP-TAB
                 NOTIFY
                 .
             05  RADIO-BUTTON  "&Auto Serv."
                              COL   36,00
                 USING VDCLICAT-AUTOSERV
                 AFTER     A-107   THRU FA-107
                          ID 007
                 EXCEPTION E-107   THRU FE-107
                 ENABLED = WDADOS-ENAB
                 GROUP = 001
                 GROUP-VALUE = 02
                 NO-GROUP-TAB
                 NOTIFY
                 .
             05  RADIO-BUTTON  "&Depos.Bebidas"
                              COL   47,80
                 USING VDCLICAT-AUTOSERV
                 AFTER     A-107   THRU FA-107
                          ID 007
                 EXCEPTION E-107   THRU FE-107
                 ENABLED = WDADOS-ENAB
                 GROUP = 001
                 GROUP-VALUE = 03
                 NO-GROUP-TAB
                 NOTIFY
                 .
             05  RADIO-BUTTON  "&Especiais"
                              COL   62,80
                 USING VDCLICAT-AUTOSERV
                 AFTER     A-107   THRU FA-107
                          ID 007
                 EXCEPTION E-107   THRU FE-107
                 ENABLED = WDADOS-ENAB
                 GROUP = 001
                 GROUP-VALUE = 04
                 NO-GROUP-TAB
                 NOTIFY
                 .
             05  RADIO-BUTTON  "&Outros"
                              COL   73,80
                 USING VDCLICAT-AUTOSERV
                 AFTER     A-107   THRU FA-107
                          ID 007
                 EXCEPTION E-107   THRU FE-107
                 ENABLED = WDADOS-ENAB
                 GROUP = 001
                 GROUP-VALUE = 05
                 NO-GROUP-TAB
                 NOTIFY
                 .
      *................................................................*
             04  COLOR IS WCOR.
             05  ENTRY-FIELD
                 LINE + 01,50 COL   28,90 LINES 01,20 CELLS
                 PIC X(03)            USING VDCLICAT-DIVCAN
                 AFTER A-108 THRU FA-108
                      ID 008
221010           ENABLED = WDADOS-ENAB-BLOQ
                 3-D
                 AUTO
                 .
             05  O108.
             07  COLOR IS XXEXTSIS-USU-CORDIS.
             09  LABEL VDCLIDIV-DESCR
                 LINE - 00,20 COL   39,00
                 .
      *................................................................*
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  CHECK-BOX "&Sim"
                 LINE + 01,60 COL   28,80
                 USING VDCLICAT-DESPREZA-BW
                 AFTER     A-109  THRU FA-109
                 EXCEPTION E-109  THRU FE-109
                          ID 009
                 ENABLED = WDADOS-ENAB
                 NOTIFY
                 .
      *................................................................*
             04  COLOR IS WCOR.
             05  ENTRY-FIELD VALUE MULTIPLE VDCLICAT-LINHAS
                 LINE + 01,50 COL   28,80 LINES 02,00
                 SIZE   40,00 MAX-LINES = 02
                 AFTER A-110  THRU FA-110
                      ID 010
221010           ENABLED = WDADOS-ENAB-BLOQ
                 FONT WFONT-TIPO
                 3-D
                 AUTO
                 .
      *................................................................*
             04  COLOR IS XXEXTSIS-USU-CORTEL.
             05  CHECK-BOX "S&im"
                 LINE + 03,20 COL   28,80
                 USING VDCLICAT-REGULARIDADE
                 AFTER     A-111  THRU FA-111
                 EXCEPTION E-111  THRU FE-111
                          ID 011
                 ENABLED = WDADOS-ENAB
                 NOTIFY
                 .
      *................................................................*
150811       04  COLOR IS WCOR.
150811       05  ENTRY-FIELD
150811           LINE + 01,50 COL   28,90 LINES 01,20 CELLS
150811           PIC X(002)            USING VDCLICAT-CODSEG
150811           AFTER A-112 THRU FA-112
150811                ID 012
150811           ENABLED = WDADOS-ENAB
150811           3-D
150811           RIGHT
150811           AUTO
150811           .
150811       05  O112.
150811       07  COLOR IS XXEXTSIS-USU-CORDIS.
150811       09  LABEL WX-DESCR-SEGMENTO
150811           LINE - 00,20 COL   37,50
150811           .
150811       04  COLOR IS WCOR.
150811       05  ENTRY-FIELD
150811           LINE + 01,70 COL   28,90 LINES 01,20 CELLS
150811           PIC 9(003)            USING VDCLICAT-CODTPNEG
150811           AFTER A-113 THRU FA-113
150811                ID 013
150811           ENABLED = WDADOS-ENAB
150811           3-D
150811           RIGHT
150811           AUTO
150811           .
150811       05  O113.
150811       07  COLOR IS XXEXTSIS-USU-CORDIS.
150811       09  LABEL WX-DESCR-NEGOCIO
150811           LINE - 00,20 COL   37,50
150811           .
280114       04  COLOR IS WCOR.
280114       05  ENTRY-FIELD
280114           LINE + 01,70 COL   28,90 LINES 01,20 CELLS
280114           PIC 99.99            USING HORA-T-VISITA
280114           AFTER A-114  THRU FA-114
280114                ID 014
280114           ENABLED = WDADOS-ENAB
280114           3-D
280114           RIGHT
280114           AUTO
280114           .
140616       04  COLOR IS WCOR.
140616       05  ENTRY-FIELD
140616           LINE + 01,70 COL   28,90 LINES 01,20 CELLS
140616           PIC 99.99            USING ALTA-F-VISITA
140616           AFTER A-115  THRU FA-115
140616                ID 015
140616           ENABLED = WDADOS-ENAB-BLO1
140616           3-D
140616           RIGHT
140616           AUTO
140616           .
140616       04  COLOR IS WCOR.
140616       05  ENTRY-FIELD
140616           LINE + 01,70 COL   28,90 LINES 01,20 CELLS
140616           PIC 99.99            USING BAIXA-F-VISITA
140616           AFTER A-116  THRU FA-116
140616                ID 016
140616           ENABLED = WDADOS-ENAB-BLO1
140616           3-D
140616           RIGHT
140616           AUTO
140616           .
      *................................................................*
             05  PUSH-BUTTON "  &OK   "
                 LINE + 01,70 LINES 02,00 CELLS
                 COL    28,85 SIZE  11,00 CELLS
                 TERMINATION-VALUE = 013
                      ID 017
                 ENABLED = WDADOS-ENAB
                 SELF-ACT
                 .
             05  PUSH-BUTTON "&CANCELA"
                              LINES 02,00 CELLS
                 COL  + 01,50 SIZE  11,00 CELLS
                 TERMINATION-VALUE = 027
                      ID 018
                 ENABLED = WDADOS-ENAB
                 SELF-ACT
                 .
             05  PUSH-BUTTON "&ANTERIOR"
                              LINES 02,00 CELLS
                 COL  + 01,50 SIZE  11,00 CELLS
                 TERMINATION-VALUE = 799
                      ID 019
                 ENABLED = WTBAR-ANTE
                 SELF-ACT
                 .
             05  PUSH-BUTTON "&PRàXIMO"
                              LINES 02,00 CELLS
                 COL  + 01,50 SIZE  11,00 CELLS
                 TERMINATION-VALUE = 798
                      ID 020
                 ENABLED = WTBAR-PROX
                 SELF-ACT
                 .
      *................................................................*
         01  B100.
             04  COLOR IS WCOR.
             05  PUSH-BUTTON "pesquisar registros para este campo"
                 LINE   07,80 LINES 01,30 CELLS
                 COL    34,00 SIZE  02,00 CELLS
                 AFTER A-104 THRU FA-104
                      ID 004
221010           ENABLED = WDADOS-ENAB-BLOQ
                 SELF-ACT
                 BITMAP     BITMAP-NUMBER = 40
                 BITMAP-HANDLE = WTBAR-BMP
                 TERMINATION-VALUE = 801
                 .
             05  PUSH-BUTTON "pesquisar registros para este campo"
                 LINE   13,80 LINES 01,30 CELLS
                 COL    36,00 SIZE  02,00 CELLS
                 AFTER A-108 THRU FA-108
                      ID 008
221010           ENABLED = WDADOS-ENAB-BLOQ
                 SELF-ACT
                 BITMAP     BITMAP-NUMBER = 40
                 BITMAP-HANDLE = WTBAR-BMP
                 TERMINATION-VALUE = 802
                 .
150811       05  PUSH-BUTTON "pesquisar registros para este campo"
150811           LINE + 07,60 LINES 01,30 CELLS
150811           COL    34,00 SIZE  02,00 CELLS
150811           AFTER A-112 THRU FA-112
150811                ID 112
150811           ENABLED = WDADOS-ENAB
150811           SELF-ACT
150811           BITMAP     BITMAP-NUMBER = 40
150811           BITMAP-HANDLE = WTBAR-BMP
150811           TERMINATION-VALUE = 803
150811           .
150811       05  PUSH-BUTTON "pesquisar registros para este campo"
150811           LINE + 01,60 LINES 01,30 CELLS
150811           COL    34,00 SIZE  02,00 CELLS
150811           AFTER A-113 THRU FA-113
150811                ID 112
150811           ENABLED = WDADOS-ENAB
150811           SELF-ACT
150811           BITMAP     BITMAP-NUMBER = 40
150811           BITMAP-HANDLE = WTBAR-BMP
150811           TERMINATION-VALUE = 804
150811           .
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
         COPY "J:\PRGNEW\GE\BOK\GETBAR3.SCR".
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

                MOVE 4352                TO XXEXTSIS-COM-CALLSUBNR.

         COPY "J:\PRGNEW\GE\BOK\GETSACE.PRO".

                ACCEPT WFONT-TIPO FROM STANDARD OBJECT "FIXED-FONT".

                DISPLAY FLOATING WINDOW
                 LINE -01,80 COL 02,00 LINES 32,50 SIZE 82,50
                 CONTROL FONT IS XXEXTSIS-COM-FONT-TELA
                 COLOR XXEXTSIS-USU-CORTEL
                 TITLE WTITULO
                 SYSTEM MENU
                 HANDLE IN WJANELA1.

                IF XXEXTSIS-COM-FUNCAO = "IN" OR = "DU"
                    MOVE 0               TO WTBAR-ANTESN
                    MOVE 0               TO WTBAR-PROXSN.
         COPY "J:\PRGNEW\GE\BOK\GEDETBAR.PRO".
         COPY "J:\PRGNEW\GE\BOK\GETBAR.PRO".
                DISPLAY T1.
      *----------------------------------------------------------------*
150811   COPY "J:\PRGNEW\GE\BOK\GEGENCGM.EMI"
150811        REPLACING =='GEGENCGM'== BY ==GEGENCGM==.
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.EMI"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.
         COPY "J:\PRGNEW\VD\BOK\VDCLICAT.EMIO"
              REPLACING =='VDCLICAT'== BY ==VDCLICAT==.
151010   COPY "J:\PRGNEW\VD\BOK\VDCLICLI.EMIO"
151010        REPLACING =='VDCLICLI'== BY ==VDCLICLI==.
         COPY "J:\PRGNEW\VD\BOK\VDCLIDIV.EMI"
              REPLACING =='VDCLIDIV'== BY ==VDCLIDIV==.
         COPY "J:\PRGNEW\VD\BOK\VDCLIGCA.EMI"
              REPLACING =='VDCLIGCA'== BY ==VDCLIGCA==.
      *----------------------------------------------------------------*
                MOVE WPARAMETROS         TO WPARMVDCLICAT.
                MOVE "altera‡Æo"         TO WTITULO-FUNCAO.
                IF XXEXTSIS-COM-FUNCAO = "DU"
                    MOVE "duplica  "     TO WTITULO-FUNCAO.
                IF XXEXTSIS-COM-FUNCAO = "IN"
                    MOVE "inclusÆo "     TO WTITULO-FUNCAO.
                MOVE "99"                TO XXEXTSIS-COM-RETCODE.
                MODIFY WJANELA1 TITLE WTITULO.
      *----------------------------------------------------------------*
         LE-VDCLICAT.
                MOVE WPARMVDCLICAT-COD   TO VDCLICAT-COD.
                READ VDCLICAT.
                IF XXEXTSIS-COM-FSTATUS  NOT = "00"
                    MOVE "VDCLICAT"      TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    GO TO RETORNA.
                IF VDCLICAT-AUTOSERV IS NOT NUMERIC
                   MOVE ZEROS            TO VDCLICAT-AUTOSERV
                END-IF
                IF VDCLICAT-AUTOSERV < 1
                   MOVE 5                TO VDCLICAT-AUTOSERV.
      *----------------------------------------------------------------*
         LEU-VDCLICAT.
      *----------------------------------------------------------------*
030611          INITIALIZE WGEGEFCRR.
030611          MOVE "VDCLICAT"          TO WGEGEFCRR-ARQUIVO.
030611          MOVE "CODIGO"            TO WGEGEFCRR-CAMPO.
030611          MOVE VDCLICAT-COD        TO WGEGEFCRR-PARM.
030611   COPY "J:\PRGNEW\GE\BOK\GETSALT.PRO".
      *----------------------------------------------------------------*
                IF   VDCLICAT-DESPREZA-BW NOT NUMERIC
                     MOVE ZEROS TO VDCLICAT-DESPREZA-BW.
                MOVE VDCLICAT-REGISTRO   TO LVDCLICAT-REGISTRO.
                MOVE VDCLICAT-TEMPO-MED-VISITA TO HORA-T-VISITA.
                MOVE VDCLICAT-ALTA-FREQUENCIA  TO ALTA-F-VISITA.
                MOVE VDCLICAT-BAIXA-FREQUENCIA TO BAIXA-F-VISITA.
                MODIFY WJANELA1 TITLE WTITULO.
                MOVE XXEXTSIS-COM-CORMOS TO WCOR.
                DISPLAY C100.
      *----------------------------------------------------------------*
         LE-VDCLIGCA.
                IF VDCLICAT-GRPCAN        NOT = SPACES
                    MOVE VDCLICAT-GRPCAN   TO VDCLIGCA-GRUPO
                    READ VDCLIGCA
                    IF XXEXTSIS-COM-FSTATUS NOT = "00"
                       MOVE ALL '?'        TO VDCLIGCA-DESCR
                    END-IF
                ELSE
                    MOVE SPACES            TO VDCLIGCA-DESCR
                END-IF.
      *----------------------------------------------------------------*
         LE-VDCLIDIV.
                IF VDCLICAT-DIVCAN        NOT = SPACES
                    MOVE VDCLICAT-DIVCAN   TO VDCLIDIV-COD
                    READ VDCLIDIV
                    IF XXEXTSIS-COM-FSTATUS NOT = "00"
                       MOVE ALL '?'        TO VDCLIDIV-DESCR
                    END-IF
                ELSE
                    MOVE SPACES            TO VDCLIDIV-DESCR
                END-IF.
      *----------------------------------------------------------------*
280616             IF WSPAD-NUMLIC  = 6455
280616                MOVE 1 TO WDADOS-ENAB-BLO1
280616             ELSE 
280616                MOVE 0 TO WDADOS-ENAB-BLO1.
      *----------------------------------------------------------------*
150811   LE-GEGENCGM.
150811          INITIALIZE WX-DESCR-SEGMENTO WX-DESCR-NEGOCIO
150811          IF VDCLICAT-CODSEG   NOT = SPACES
150811              MOVE "SEGMENTO"        TO GEGENCGM-TIPO
150811              MOVE VDCLICAT-CODSEG   TO GEGENCGM-NRX
150811              READ GEGENCGM
150811              IF XXEXTSIS-COM-FSTATUS NOT = "00"
150811                 MOVE ALL '?'        TO WX-DESCR-SEGMENTO
150811              ELSE
150811                 MOVE GEGENCGM-DESCRICAO-1 (1:50) TO
150811                                                 WX-DESCR-SEGMENTO
150811              END-IF
150811          ELSE
150811              MOVE SPACES            TO WX-DESCR-SEGMENTO
150811          END-IF.
150811
150811          IF VDCLICAT-CODTPNEG NOT = ZEROS
150811              MOVE "TPNEGOCIO"        TO GEGENCGM-TIPO
150811              MOVE VDCLICAT-CODTPNEG  TO GEGENCGM-NR
150811              READ GEGENCGM
150811              IF XXEXTSIS-COM-FSTATUS NOT = "00"
150811                 MOVE ALL '?'        TO WX-DESCR-NEGOCIO
150811              ELSE
150811                 MOVE GEGENCGM-DESCRICAO-1 (1:50) TO
150811                                                 WX-DESCR-NEGOCIO
150811              END-IF
150811          ELSE
150811              MOVE SPACES            TO WX-DESCR-NEGOCIO
150811          END-IF.
      *----------------------------------------------------------------*
         LE-USERS.
      *         MOVE VDCLICAT-INCLUSER   TO WSIGLA-USER.
      *         MOVE VDCLICAT-INCLDT     TO WDT8.
      *         MOVE WDIA8               TO WDIA8-D.
      *+FFF+    MOVE WMES8               TO WMES8-D.
      *         MOVE WANO8               TO WANO8-D.
      *         MOVE WDT8-D              TO WDT8-DED.
      *         MOVE VDCLICAT-INCLHR     TO WHORMIN.
      *         MOVE WHOR                TO WHORED.
      *         MOVE WMIN                TO WMINED.
      *         MOVE ":"                 TO W2PONTOSHR.
      *         IF WHORA4ED = '00:00'
      *             MOVE SPACES          TO WHORA4ED.
      *         PERFORM LE-GEGENUSU    THRU FLE-GEGENUSU.
      *         DISPLAY O197.
      *
      *         MOVE VDCLICAT-ALTEDT     TO WDT8.
      *         MOVE WDIA8               TO WDIA8-D.
      *         MOVE WMES8               TO WMES8-D.
      *         MOVE WANO8               TO WANO8-D.
      *         MOVE WDT8-D              TO WDT8-DED.
      *         MOVE VDCLICAT-ALTEHR     TO WHORMIN.
      *         MOVE WHOR                TO WHORED.
      *         MOVE WMIN                TO WMINED.
      *         MOVE ":"                 TO W2PONTOSHR.
      *         IF WHORA4ED = '00:00'
      *             MOVE SPACES          TO WHORA4ED.
      *         MOVE VDCLICAT-ALTEUSER   TO WSIGLA-USER.
      *         PERFORM LE-GEGENUSU    THRU FLE-GEGENUSU.
      *         DISPLAY O198.
      *
      *         MOVE VDCLICAT-CANCDT     TO WDT8.
      *         MOVE WDIA8               TO WDIA8-D.
      *         MOVE WMES8               TO WMES8-D.
      *         MOVE WANO8               TO WANO8-D.
      *         MOVE WDT8-D              TO WDT8-DED.
      *         MOVE VDCLICAT-CANCHR     TO WHORMIN.
      *         MOVE WHOR                TO WHORED.
      *         MOVE WMIN                TO WMINED.
      *         MOVE ":"                 TO W2PONTOSHR.
      *         IF WHORA4ED = '00:00'
      *             MOVE SPACES          TO WHORA4ED.
      *         MOVE VDCLICAT-CANCUSER   TO WSIGLA-USER.
      *         PERFORM LE-GEGENUSU    THRU FLE-GEGENUSU.
      *         DISPLAY O199.
      *----------------------------------------------------------------*
      *         IF VDCLICAT-CANCSN     = 1
      *+FFF+        MOVE 084          TO XXX99SIS-COM-NRMSG
      *             CALL "GEMSG".
      *         MOVE VDCLICAT-CANCSN  TO WCANCSN-ANT.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GELITBAR.PRO".
                MOVE ALL "N"             TO WSCR-CAMPO-OKT.
         PEGA-DADOS.
                MOVE XXEXTSIS-USU-CORAC  TO WCOR.
                MOVE 1                   TO WDADOS-ENAB.
                DISPLAY D100.
                DISPLAY B100.
                ACCEPT  D100 BEFORE TIME XXEXTSIS-USU-TIMEOUT.
                MOVE XXEXTSIS-USU-CORNAC TO WCOR.
                MOVE 0                   TO WDADOS-ENAB.
                DISPLAY D100.
                DISPLAY B100.
                IF WTECLA-ESC
                 IF VDCLICAT-REGISTRO = LVDCLICAT-REGISTRO
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
                 IF VDCLICAT-REGISTRO = LVDCLICAT-REGISTRO
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

                MOVE 001                 TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      002        TO WSCR-ID
                    PERFORM A-102   THRU FA-102
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.
                ADD  001                 TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      003        TO WSCR-ID
                    PERFORM A-103   THRU FA-103
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.
                ADD  001                 TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      004        TO WSCR-ID
                    PERFORM A-104   THRU FA-104
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.
                ADD  001                 TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      005        TO WSCR-ID
                    PERFORM A-105  THRU FA-105
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.
                ADD  001                 TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      006        TO WSCR-ID
                    PERFORM A-106  THRU FA-106
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.
                ADD  001                 TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      007        TO WSCR-ID
                    PERFORM A-107   THRU FA-107
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.
                ADD  001                 TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      008        TO WSCR-ID
                    PERFORM A-108   THRU FA-108
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.
                ADD  001                 TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      009        TO WSCR-ID
                    PERFORM A-109   THRU FA-109
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.
                ADD  001                 TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      010        TO WSCR-ID
                    PERFORM A-110   THRU FA-110
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.
                ADD  001                 TO WSCR-CAMPO.
                IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
                    MOVE      011        TO WSCR-ID
                    PERFORM A-111   THRU FA-111
                    IF WSCR-ERRO NOT = ZEROS
                        GO TO PEGA-DADOS.
150811          ADD  001                 TO WSCR-CAMPO.
150811          IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
150811              MOVE      012        TO WSCR-ID
150811              PERFORM A-112   THRU FA-112
150811              IF WSCR-ERRO NOT = ZEROS
150811                  GO TO PEGA-DADOS.
150811          ADD  001                 TO WSCR-CAMPO.
150811          IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
150811              MOVE      013        TO WSCR-ID
150811              PERFORM A-113   THRU FA-113
150811              IF WSCR-ERRO NOT = ZEROS
150811                  GO TO PEGA-DADOS.
280114          ADD  001                 TO WSCR-CAMPO.
280114          IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
280114              MOVE      014        TO WSCR-ID
280114              PERFORM A-114   THRU FA-114
280114              IF WSCR-ERRO NOT = ZEROS
280114                  GO TO PEGA-DADOS.
140616          ADD  001                 TO WSCR-CAMPO.
140616          IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
140616              MOVE      015        TO WSCR-ID
140616              PERFORM A-115   THRU FA-115
140616              IF WSCR-ERRO NOT = ZEROS
140616                  GO TO PEGA-DADOS.
140616          ADD  001                 TO WSCR-CAMPO.
140616          IF WSCR-CAMPO-OK (WSCR-CAMPO) NOT = "S"
140616              MOVE      016        TO WSCR-ID
140616              PERFORM A-116   THRU FA-116
140616              IF WSCR-ERRO NOT = ZEROS
140616                  GO TO PEGA-DADOS.
      *----------------------------------------------------------------*
         CONFIRMA-PA.
                IF WBOTAO-FUNCAO NOT = "P" AND NOT = "A"
                    GO TO REGRAVA-VDCLICAT.
                IF VDCLICAT-REGISTRO = LVDCLICAT-REGISTRO
                    GO TO FGRAVA-LOG.
                MOVE "GRAVA ALTERA€åES DESTE REGISTRO ?                "
                                        TO XXEXTSIS-COM-MSG.
                MOVE 999                TO XXEXTSIS-COM-NRMSG.
                MOVE 26                 TO XXEXTSIS-COM-LNHINI.
                MOVE 10                 TO XXEXTSIS-COM-COLINI.
                CALL   "GERESPN".
                CANCEL "GERESPN".
                IF XXEXTSIS-COM-TECLA NOT = "S"
                    GO TO FGRAVA-LOG.
      *----------------------------------------------------------------*
         REGRAVA-VDCLICAT.
      *         IF WCANCSN-ANT         = 0
      *           IF VDCLICAT-CANCSN   = 1
      *             ACCEPT VDCLICAT-CANCDT    FROM CENTURY-DATE
      *             ACCEPT WHORA              FROM TIME
      *             MOVE WHORMIN                TO VDCLICAT-CANCHR
      *             MOVE XXEXTSIS-USU-SIGLA     TO VDCLICAT-CANCSIGLA.
                ACCEPT VDCLICAT-LOGDT         FROM CENTURY-DATE.
                IF XXEXTSIS-COM-FUNCAO NOT = "DU" AND NOT = "IN"
                    ACCEPT VDCLICAT-ALTEDT    FROM CENTURY-DATE
                    ACCEPT WHORA              FROM TIME
                    MOVE WHORMIN              TO VDCLICAT-ALTEHR
                    MOVE XXEXTSIS-USU-SIGLA   TO VDCLICAT-ALTESIGLA.
                REWRITE VDCLICAT-REGISTRO.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE "VDCLICAT"   TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    GO TO RETORNA.

080317          INITIALIZE WPARMVDCADFLC.
080317          MOVE "VDCLICAT"       TO WPARMVDCADFLC-NOME-TABELA.
080317          MOVE VDCLICAT-CHAVE   TO WPARMVDCADFLC-CODIGO-ERP.
080317          IF XXEXTSIS-COM-FUNCAO = "DU" OR "IN"
080317             MOVE 0             TO WPARMVDCADFLC-OPERACAO
080317          ELSE
080317             MOVE 1             TO WPARMVDCADFLC-OPERACAO
080317          END-IF.
080317          MOVE "VDCLCATA"       TO WPARMVDCADFLC-PROGRAMA.
080317          MOVE WTPGM-VERSAO     TO WPARMVDCADFLC-VERSAO-PRG.
080317          CALL "VDCADFLC" USING WPARMVDCADFLC
080317                          ON EXCEPTION
080317                             INITIALIZE WPARMVDCADFLC
080317          END-CALL.

      *----------------------------------------------------------------*
151010*---------Se desmarcou a regularidade do canal, avisara e pedira
151010*---------confirmacao para replicar o ajuste nos clientes desse
151010*---------canal.
151010          IF LVDCLICAT-REGULARIDADE = 1 AND                       ANTES - MARCADO
151010             VDCLICAT-REGULARIDADE  = 0                           DEPOIS - DESMARCADO
151010             MOVE
151010             "DESMARCAR REGULARIDADE EM TODOS CLIENTES DO CANAL? "
151010                       TO XXEXTSIS-COM-MSG
151010             MOVE 999  TO XXEXTSIS-COM-NRMSG
151010             MOVE 26   TO XXEXTSIS-COM-LNHINI
151010             MOVE 10   TO XXEXTSIS-COM-COLINI
151010             CALL   "GERESPN"
151010             CANCEL "GERESPN"
151010             IF XXEXTSIS-COM-TECLA = "S"
151010                PERFORM DESMARCAR-CLIENTES THRU
151010                        DESMARCOU-CLIENTES
151010             END-IF
151010          END-IF.
      *----------------------------------------------------------------*
                MOVE "00"             TO XXEXTSIS-COM-RETCODE.
                IF XXEXTSIS-COM-FUNCAO = "DU" OR = "IN"
                    GO TO RETORNA.
      *----------------------------------------------------------------*
         GRAVA-LOG.
                MOVE "A"               TO XXEXTSIS-LOG-OPERACAO.
                MOVE SPACES            TO WLOG-DADOS.
                MOVE VDCLICAT-COD      TO WLOG-COD.
                MOVE WLOG-DADOS        TO XXEXTSIS-LOG-DADOS.
                CALL "GELOG".
      *----------------------------------------------------------------*
         GRAVA-GEGENLAC.
                IF VDCLICAT-NOME          NOT = LVDCLICAT-NOME
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCLICAT"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Descri‡Æo do canal  " TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCLICAT-NOME         TO XXEXTSIS-LAC-VLRX-ANTE
                  MOVE VDCLICAT-NOME          TO XXEXTSIS-LAC-VLRX-NOVO
                  CALL "GELAC".
                IF VDCLICAT-COMIS         NOT = LVDCLICAT-COMIS
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCLICAT"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "ComissÆo do canal   " TO XXEXTSIS-LAC-DESCR-CPO
                  COMPUTE XXEXTSIS-LAC-VLR9-ANTE = LVDCLICAT-COMIS * 100
                  COMPUTE XXEXTSIS-LAC-VLR9-NOVO = VDCLICAT-COMIS * 100
                  CALL "GELAC".
                IF VDCLICAT-GRPCAN        NOT = LVDCLICAT-GRPCAN
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCLICAT"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Grupo do canal      " TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCLICAT-GRPCAN       TO XXEXTSIS-LAC-VLRX-ANTE
                  MOVE VDCLICAT-GRPCAN        TO XXEXTSIS-LAC-VLRX-NOVO
                  CALL "GELAC".
                IF VDCLICAT-LIMITE        NOT = LVDCLICAT-LIMITE
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCLICAT"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Limite cr‚dito canal" TO XXEXTSIS-LAC-DESCR-CPO
                  COMPUTE XXEXTSIS-LAC-VLR9-ANTE =
                          LVDCLICAT-LIMITE * 100
                  COMPUTE XXEXTSIS-LAC-VLR9-NOVO = VDCLICAT-LIMITE * 100
                  CALL "GELAC".
                IF VDCLICAT-CANALCIA-X     NOT = LVDCLICAT-CANALCIA-X
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCLICAT"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Canal da companhia  " TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCLICAT-CANALCIA-X   TO XXEXTSIS-LAC-VLRX-ANTE
                  MOVE VDCLICAT-CANALCIA-X    TO XXEXTSIS-LAC-VLRX-NOVO
                  CALL "GELAC".
                IF VDCLICAT-AUTOSERV       NOT = LVDCLICAT-AUTOSERV
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE "VDCLICAT"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "Flag auto-servi‡o   " TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCLICAT-AUTOSERV     TO XXEXTSIS-LAC-VLR9-ANTE
                  MOVE VDCLICAT-AUTOSERV      TO XXEXTSIS-LAC-VLR9-NOVO
                  CALL "GELAC".
                IF VDCLICAT-DIVCAN        NOT = LVDCLICAT-DIVCAN
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCLICAT"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "DivisÆo do canal    " TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCLICAT-DIVCAN       TO XXEXTSIS-LAC-VLRX-ANTE
                  MOVE VDCLICAT-DIVCAN        TO XXEXTSIS-LAC-VLRX-NOVO
                  CALL "GELAC".
                IF VDCLICAT-DESCR-COMPL   NOT = LVDCLICAT-DESCR-COMPL
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCLICAT"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Descr.Complementar  " TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCLICAT-DESCR-COMPL  TO XXEXTSIS-LAC-VLRX-ANTE
                  MOVE VDCLICAT-DESCR-COMPL   TO XXEXTSIS-LAC-VLRX-NOVO
                  CALL "GELAC".
                IF VDCLICAT-REGULARIDADE  NOT = LVDCLICAT-REGULARIDADE
                  INITIALIZE                     XXEXTSIS-GEGENLAC
                  MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
                  MOVE "VDCLICAT"             TO XXEXTSIS-COM-ARQUIVO
                  MOVE "Regularidade        " TO XXEXTSIS-LAC-DESCR-CPO
                  MOVE LVDCLICAT-REGULARIDADE TO XXEXTSIS-LAC-VLR9-ANTE
                  MOVE VDCLICAT-REGULARIDADE  TO XXEXTSIS-LAC-VLR9-NOVO
                  CALL "GELAC".
150811          IF VDCLICAT-CODSEG  NOT = LVDCLICAT-CODSEG
150811            INITIALIZE                     XXEXTSIS-GEGENLAC
150811            MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
150811            MOVE "VDCLICAT"             TO XXEXTSIS-COM-ARQUIVO
150811            MOVE "C¢digo Segmento     " TO XXEXTSIS-LAC-DESCR-CPO
150811            MOVE LVDCLICAT-CODSEG       TO XXEXTSIS-LAC-VLR9-ANTE
150811            MOVE VDCLICAT-CODSEG        TO XXEXTSIS-LAC-VLR9-NOVO
150811            CALL "GELAC".
150811          IF VDCLICAT-CODTPNEG  NOT = LVDCLICAT-CODTPNEG
150811            INITIALIZE                     XXEXTSIS-GEGENLAC
150811            MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
150811            MOVE "VDCLICAT"             TO XXEXTSIS-COM-ARQUIVO
150811            MOVE "C¢digo Tipo Negocio " TO XXEXTSIS-LAC-DESCR-CPO
150811            MOVE LVDCLICAT-CODTPNEG     TO XXEXTSIS-LAC-VLR9-ANTE
150811            MOVE VDCLICAT-CODTPNEG      TO XXEXTSIS-LAC-VLR9-NOVO
150811            CALL "GELAC".
280114          IF VDCLICAT-TEMPO-MED-VISITA  NOT =
280114                                        LVDCLICAT-TEMPO-MED-VISITA
280114            INITIALIZE                     XXEXTSIS-GEGENLAC
280114            MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
280114            MOVE "VDCLICAT"             TO XXEXTSIS-COM-ARQUIVO
280114            MOVE "Tempo Medio Visita  " TO XXEXTSIS-LAC-DESCR-CPO
280114            MOVE LVDCLICAT-TEMPO-MED-VISITA
280114                                        TO XXEXTSIS-LAC-VLR9-ANTE
280114            MOVE VDCLICAT-TEMPO-MED-VISITA
280114                                        TO XXEXTSIS-LAC-VLR9-NOVO
280114            CALL "GELAC".
280616          IF VDCLICAT-ALTA-FREQUENCIA  NOT =
280616                                        LVDCLICAT-ALTA-FREQUENCIA
280616            INITIALIZE                     XXEXTSIS-GEGENLAC
280616            MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
280616            MOVE "VDCLICAT"             TO XXEXTSIS-COM-ARQUIVO
280616            MOVE "Alta Frequˆncia     " TO XXEXTSIS-LAC-DESCR-CPO
280616            MOVE LVDCLICAT-ALTA-FREQUENCIA
280616                                        TO XXEXTSIS-LAC-VLR9-ANTE
280616            MOVE VDCLICAT-ALTA-FREQUENCIA
280616                                        TO XXEXTSIS-LAC-VLR9-NOVO
280616            CALL "GELAC".
280616          IF VDCLICAT-BAIXA-FREQUENCIA  NOT =
280616                                        LVDCLICAT-BAIXA-FREQUENCIA
280616            INITIALIZE                     XXEXTSIS-GEGENLAC
280616            MOVE WLOG-DADOS             TO XXEXTSIS-LAC-CHAVEALT
280616            MOVE "VDCLICAT"             TO XXEXTSIS-COM-ARQUIVO
280616            MOVE "Baixa Frequˆncia    " TO XXEXTSIS-LAC-DESCR-CPO
280616            MOVE LVDCLICAT-BAIXA-FREQUENCIA
280616                                        TO XXEXTSIS-LAC-VLR9-ANTE
280616            MOVE VDCLICAT-BAIXA-FREQUENCIA
280616                                        TO XXEXTSIS-LAC-VLR9-NOVO
280616            CALL "GELAC".
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
                READ VDCLICAT PREVIOUS.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE "VDCLICAT"    TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    START VDCLICAT KEY >= VDCLICAT-CHAVE
                    READ VDCLICAT.
                GO TO LEU-VDCLICAT.
      *----------------------------------------------------------------*
         MOSTRA-PROXIMO.
                READ VDCLICAT NEXT.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE "VDCLICAT"    TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    START VDCLICAT KEY <= VDCLICAT-CHAVE
                    READ VDCLICAT.
                GO TO LEU-VDCLICAT.
      *----------------------------------------------------------------*
         B-D100.
         COPY "J:\PRGNEW\GE\BOK\GEDETBAR.PRO".

                IF XXEXTSIS-COM-FUNCAO NOT = "DU" AND NOT = "IN"
                   MOVE WTBAR-BOTAO01SN TO WTBAR-BOTAO01.

                INITIALIZE  XXEXTSIS-COM-MSG.
                EVALUATE WSCR-CAMPO
                WHEN 001
                  MOVE "DESCRI€ÇO DO CANAL DE VENDA                    "
                                       TO XXEXTSIS-COM-MSG
                WHEN 002
                  MOVE "COMISSÇO DO CANAL DE VENDA (EM PORCENTAGEM)    "
                                       TO XXEXTSIS-COM-MSG
                WHEN 003
                  MOVE WTBAR-VIEWSN   TO WTBAR-VIEW
                  MOVE "GRUPO DO CANAL DE VENDA                        "
                                       TO XXEXTSIS-COM-MSG
                WHEN 004
                  MOVE "LIMITE DE CRDITO DO CANAL DE VENDA            "
                                       TO XXEXTSIS-COM-MSG
                WHEN 005
                  MOVE "CANAL DA COMPANHIA                             "
                                       TO XXEXTSIS-COM-MSG
                WHEN 006
                  MOVE
                      "MARCAR (BARRA DE ESPA€O OU CLICK) FRIO          "
                                       TO XXEXTSIS-COM-MSG
                WHEN 007
                  MOVE
                      "MARCAR (BARRA DE ESPA€O OU CLICK) AUTO-SERVI€O  "
                                       TO XXEXTSIS-COM-MSG
                WHEN 008
                  MOVE
                      "MARCAR (BARRA DE ESPA€O OU CLICK) DEPOS. BEBIDAS"
                                       TO XXEXTSIS-COM-MSG
                WHEN 009
                  MOVE
                      "MARCAR (BARRA DE ESPA€O OU CLICK) ESPECIAIS     "
                                       TO XXEXTSIS-COM-MSG
                WHEN 010
                  MOVE
                      "MARCAR (BARRA DE ESPA€O OU CLICK) OUTROS        "
                                       TO XXEXTSIS-COM-MSG
                WHEN 011
                  MOVE WTBAR-VIEWSN   TO WTBAR-VIEW
                  MOVE "DIVISÇO DO CANAL DE VENDA                      "
                                       TO XXEXTSIS-COM-MSG
                WHEN 012
                  MOVE
                      "MARCAR (BARRA DE ESPA€O OU CLICK) O DESPREZA EXPO
"     -               "RTA€ÇO P/ BW"
                                       TO XXEXTSIS-COM-MSG
                WHEN 013
                  MOVE "DESCRI€ÇO COMPLEMENTAR DO CANAL DE VENDAS      "
                                       TO XXEXTSIS-COM-MSG
                WHEN 014
                  MOVE
                  "MARCAR (BARRA DE ESPA€O OU CLICK) TRATA REGULARIDADE"
                                       TO XXEXTSIS-COM-MSG
                WHEN 015
                  MOVE
                  "SEGMENTO                                            "
                                       TO XXEXTSIS-COM-MSG
                WHEN 016
                  MOVE
                  "TIPO NEGàCIO                                        "
                                       TO XXEXTSIS-COM-MSG
                WHEN 017
                  MOVE
                  "DURA€ÇO MDIA VISITA   HHMM                         "
                                       TO XXEXTSIS-COM-MSG
                WHEN 018
                  MOVE
                  "DURA€ÇO MDIA VISITA   HHMM ALTA FREQUÒNCIA         "
                                       TO XXEXTSIS-COM-MSG
                WHEN 019
                  MOVE
                  "DURA€ÇO MDIA VISITA   HHMM BAIXA FREQUÒNCIA        "
                                       TO XXEXTSIS-COM-MSG
                WHEN 020
                  MOVE "TECLE ENTER OU CLICK NO BOTÇO PARA CONFIRMAR   "
                                       TO XXEXTSIS-COM-MSG
                WHEN 021
                  MOVE "TECLE ENTER OU CLICK NO BOTÇO PARA CANCELAR    "
                                       TO XXEXTSIS-COM-MSG
                WHEN 022
                 MOVE "CLICK NO BOTÇO PARA ATUALIZAR E IR PARA ANTERIOR"
                                       TO XXEXTSIS-COM-MSG
                WHEN 023
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
                PERFORM VE-TECLA    THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-102.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-102.
                IF VDCLICAT-NOME       = SPACES
                    MOVE 071           TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
                    MOVE 1             TO WSCR-ERRO
                    GO TO FA-102
                END-IF.
                MOVE "S"               TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-102.
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
                MOVE "S"               TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-103.
                EXIT.
      *----------------------------------------------------------------*
         A-104.
                IF WTECLA-VIEW
                    SET WTECLA-OPCAO01    TO TRUE.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-104.
                PERFORM VE-TECLA       THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-104.
                IF WSCR-ERRO              NOT = ZEROS
                    GO TO FA-104.
                IF VDCLICAT-GRPCAN        = SPACES
                    MOVE 071              TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
                    MOVE 1                TO WSCR-ERRO
                    GO TO FA-104
                END-IF.
                MOVE VDCLICAT-GRPCAN      TO VDCLIGCA-GRUPO
                READ VDCLIGCA.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE ALL '?'          TO VDCLIGCA-DESCR
                    DISPLAY O104
                    MOVE "VDCLIGCA"       TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    MOVE 1                TO WSCR-ERRO
                    GO TO FA-104
                END-IF.
                DISPLAY O104.

010811          INITIALIZE WGEGEFCRR.
fabr            IF XXEXTSIS-CLI-BANDCTRFX = "F" OR
faixa              XXEXTSIS-CLI-BANDCTRFX = "S"
010811             INITIALIZE WGEGEFCRR
010811             MOVE "VDCLIGCA"        TO WGEGEFCRR-ARQUIVO
010811             MOVE "CODIGO"          TO WGEGEFCRR-CAMPO
010811             MOVE VDCLIGCA-CHAVE    TO WGEGEFCRR-PARM
010811*------------A rotina esta dentro de \prgnew\ge\bok\getsfxre.pro
010811             PERFORM CHECAR-FXRESTRITA
010811                                  THRU CHECOU-FXRESTRITA
010811             IF WGEGEFCRR-MSG = "S"
010811                MOVE 1              TO WSCR-ERRO
010811                GO TO FA-104
010811             END-IF
010811          END-IF.

                MOVE "S"                  TO WSCR-CAMPO-OK (WSCR-CAMPO).
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
                MOVE "S"               TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-105.
                EXIT.
      *----------------------------------------------------------------*
         A-106.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-106.
                PERFORM VE-TECLA    THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-106.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-106.
                MOVE "S"               TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-106.
                EXIT.
      *----------------------------------------------------------------*
         A-107.
                PERFORM TESTA-OPCAO-T1      THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO                NOT = ZEROS
                    GO TO FA-107.
                PERFORM VE-TECLA            THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-107.
                IF WSCR-ERRO                NOT = ZEROS
                    GO TO FA-107.
                MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-107.
                EXIT.
      *----------------------------------------------------------------*
         E-107.
                IF WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE
                 IF VDCLICAT-REGISTRO = LVDCLICAT-REGISTRO
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
                IF WTECLA-VIEW
                    SET WTECLA-OPCAO02    TO TRUE.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-108.
                PERFORM VE-TECLA       THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-108.
                IF WSCR-ERRO              NOT = ZEROS
                    GO TO FA-108.
                IF VDCLICAT-DIVCAN        = SPACES
                    MOVE SPACES           TO VDCLIDIV-DESCR
                    DISPLAY O108
                    MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO)
                    GO TO FA-108
                END-IF.
                MOVE VDCLICAT-DIVCAN      TO VDCLIDIV-COD.
                READ VDCLIDIV.
                IF XXEXTSIS-COM-FSTATUS NOT = "00"
                    MOVE ALL '?'          TO VDCLIDIV-DESCR
                    DISPLAY O108
                    MOVE "VDCLIDIV"       TO XXEXTSIS-COM-ARQUIVO
                    CALL "GESTATU"
                    MOVE 1                TO WSCR-ERRO
                    GO TO FA-108
                END-IF.
                IF VDCLIDIV-CANCSN = 1
                    MOVE 083              TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG".
      *             MOVE ALL '?'          TO VDCLIDIV-DESCR
      *             MOVE 1                TO WSCR-ERRO
      *             GO TO FA-108.

                DISPLAY O108.

010811          INITIALIZE WGEGEFCRR.
fabr            IF XXEXTSIS-CLI-BANDCTRFX = "F" OR
faixa              XXEXTSIS-CLI-BANDCTRFX = "S"
010811             INITIALIZE WGEGEFCRR
010811             MOVE "VDCLIDIV"        TO WGEGEFCRR-ARQUIVO
010811             MOVE "CODIGO"          TO WGEGEFCRR-CAMPO
010811             MOVE VDCLIDIV-CHAVE    TO WGEGEFCRR-PARM
010811*------------A rotina esta dentro de \prgnew\ge\bok\getsfxre.pro
010811             PERFORM CHECAR-FXRESTRITA
010811                                  THRU CHECOU-FXRESTRITA
010811             IF WGEGEFCRR-MSG = "S"
010811                MOVE 1              TO WSCR-ERRO
010811                GO TO FA-108
010811             END-IF
010811          END-IF.

                MOVE "S"                  TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-108.
                EXIT.
      *----------------------------------------------------------------*
         A-109.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO            NOT = ZEROS
                    GO TO FA-109.
                PERFORM VE-TECLA       THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-109.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-109.
                MOVE "S"                  TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-109.
                EXIT.
      *----------------------------------------------------------------*
         E-109.
                IF WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE
                 IF VDCLICAT-REGISTRO = LVDCLICAT-REGISTRO
                    GO TO RETORNA
                 ELSE
                    MOVE 100              TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
                    GO TO RETORNA
                 END-IF
                END-IF.
                MOVE 4                    TO WSCR-ERRO.
                ADD  1                    TO WSCR-ID.
                MOVE "S"                  TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FE-109.
                EXIT.
      *----------------------------------------------------------------*
         A-110.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-110.
                PERFORM VE-TECLA    THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-110.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-110.
                IF VDCLICAT-DESCR-COMPL = SPACES
                    MOVE "S"          TO WSCR-CAMPO-OK (WSCR-CAMPO)
********************(Luiz Augusto pediu para nao ser campo obrigatorio)
********************MOVE 999          TO XXEXTSIS-COM-NRMSG
********************MOVE "Descri‡Æo Complementar ‚ obrigat¢ria!"
********************                  TO XXEXTSIS-COM-MSG
********************CALL "GEMSG"
********************MOVE 1            TO WSCR-ERRO
                    GO TO FA-110.
                DISPLAY D100.
                MOVE "S"              TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-110.
                EXIT.
      *----------------------------------------------------------------*
         A-111.
                PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
                IF WSCR-ERRO            NOT = ZEROS
                    GO TO FA-111.
                PERFORM VE-TECLA       THRU FVE-TECLA.
                IF WTECLA-ESC OR
                  (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
                    GO TO FA-111.
                IF WSCR-ERRO NOT = ZEROS
                    GO TO FA-111.
                MOVE "S"                  TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FA-111.
                EXIT.
      *----------------------------------------------------------------*
         E-111.
                IF WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE
                 IF VDCLICAT-REGISTRO = LVDCLICAT-REGISTRO
                    GO TO RETORNA
                 ELSE
                    MOVE 100              TO XXEXTSIS-COM-NRMSG
                    CALL "GEMSG"
                    GO TO RETORNA
                 END-IF
                END-IF.
                MOVE 4                    TO WSCR-ERRO.
                ADD  1                    TO WSCR-ID.
                MOVE "S"                  TO WSCR-CAMPO-OK (WSCR-CAMPO).
         FE-111.
                EXIT.
      *----------------------------------------------------------------*
150811   A-112.
150811          IF WTECLA-VIEW
150811              SET WTECLA-OPCAO03    TO TRUE.
150811          PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
150811          IF WSCR-ERRO NOT = ZEROS
150811              GO TO FA-112.
150811          PERFORM VE-TECLA       THRU FVE-TECLA.
150811          IF WTECLA-ESC OR
150811            (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
150811              GO TO FA-112.
150811          IF WSCR-ERRO              NOT = ZEROS
150811              GO TO FA-112.
150811
150811          IF XXEXTSIS-CLI-BANDEIRA = 021
150811             IF VDCLICAT-CODSEG       = SPACES
150811                MOVE 071              TO XXEXTSIS-COM-NRMSG
150811                CALL "GEMSG"
150811                MOVE 1                TO WSCR-ERRO
150811                GO TO FA-112
150811              END-IF
150811          ELSE
150811              IF VDCLICAT-CODSEG       = SPACES
150811                 MOVE SPACES        TO WX-DESCR-SEGMENTO
150811                 DISPLAY O112
150811                 MOVE "S"           TO WSCR-CAMPO-OK (WSCR-CAMPO)
150811                 GO TO FA-112
150811              END-IF
150811          END-IF.
150811
150811          INITIALIZE                   GEGENCGM-REGISTRO
150811          MOVE "SEGMENTO"           TO GEGENCGM-TIPO
150811          MOVE VDCLICAT-CODSEG      TO GEGENCGM-NRX
150811          READ GEGENCGM.
150811          IF XXEXTSIS-COM-FSTATUS NOT = "00"
150811              MOVE ALL '?'          TO WX-DESCR-SEGMENTO
150811              DISPLAY O112
150811              MOVE "GEGENCGM"       TO XXEXTSIS-COM-ARQUIVO
150811              CALL "GESTATU"
150811              MOVE 1                TO WSCR-ERRO
150811              GO TO FA-112
150811          END-IF.
150811          MOVE GEGENCGM-DESCRICAO-1 (1:50) TO WX-DESCR-SEGMENTO.
150811          DISPLAY O112.
150811          MOVE "S"                  TO WSCR-CAMPO-OK (WSCR-CAMPO).
150811   FA-112.
150811          EXIT.
      *----------------------------------------------------------------*
150811   A-113.
150811          IF WTECLA-VIEW
150811              SET WTECLA-OPCAO04    TO TRUE.
150811          PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
150811          IF WSCR-ERRO NOT = ZEROS
150811              GO TO FA-113.
150811          PERFORM VE-TECLA       THRU FVE-TECLA.
150811          IF WTECLA-ESC OR
150811            (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
150811              GO TO FA-113.
150811          IF WSCR-ERRO              NOT = ZEROS
150811              GO TO FA-113.
150811
150811          IF XXEXTSIS-CLI-BANDEIRA = 021
150811             IF VDCLICAT-CODTPNEG      = ZEROS
150811                MOVE 071              TO XXEXTSIS-COM-NRMSG
150811                CALL "GEMSG"
150811                MOVE 1                TO WSCR-ERRO
150811                GO TO FA-113
150811              END-IF
150811          ELSE
150811              IF VDCLICAT-CODTPNEG   = ZEROS
150811                 MOVE SPACES        TO WX-DESCR-NEGOCIO
150811                 DISPLAY O113
150811                 MOVE "S"           TO WSCR-CAMPO-OK (WSCR-CAMPO)
150811                 GO TO FA-113
150811              END-IF
150811          END-IF.
150811
150811          INITIALIZE                   GEGENCGM-REGISTRO
150811          MOVE "TPNEGOCIO"          TO GEGENCGM-TIPO
150811          MOVE VDCLICAT-CODTPNEG    TO GEGENCGM-NR
150811          READ GEGENCGM.
150811          IF XXEXTSIS-COM-FSTATUS NOT = "00"
150811              MOVE ALL '?'          TO WX-DESCR-NEGOCIO
150811              DISPLAY O113
150811              MOVE "GEGENCGM"       TO XXEXTSIS-COM-ARQUIVO
150811              CALL "GESTATU"
150811              MOVE 1                TO WSCR-ERRO
150811              GO TO FA-113
150811          END-IF.
150811          MOVE GEGENCGM-DESCRICAO-1 (1:50) TO WX-DESCR-NEGOCIO.
150811          DISPLAY O113.
150811          MOVE "S"                  TO WSCR-CAMPO-OK (WSCR-CAMPO).
150811   FA-113.
150811          EXIT.
      *----------------------------------------------------------------*
280114   A-114.
280114          PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
280114          IF WSCR-ERRO NOT = ZEROS
280114              GO TO FA-114.
280114          PERFORM VE-TECLA    THRU FVE-TECLA.
280114          IF WTECLA-ESC OR
280114            (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
280114              GO TO FA-114.
280114          IF WSCR-ERRO NOT = ZEROS
280114              GO TO FA-114.
280114          IF HORA-T-VISITA = ZEROS
280114             MOVE HORA-T-VISITA       TO VDCLICAT-TEMPO-MED-VISITA
280114             MOVE "S"                 TO WSCR-CAMPO-OK(WSCR-CAMPO)
280114             GO TO FA-114.
280114          IF HORA-VISITA > 23
280114             MOVE 999                 TO XXEXTSIS-COM-NRMSG
280114             MOVE "Informar Hora, intervalo de 00 as 23 "
280114                                      TO XXEXTSIS-COM-MSG
280114             CALL "GEMSG"
280114             MOVE 1                   TO WSCR-ERRO
280114             GO TO FA-114.
280114          IF MINUTO-VISITA > 59
280114             MOVE 999                 TO XXEXTSIS-COM-NRMSG
280114             MOVE "Informar minuto, intervalo de 00 as 59"
280114                                      TO XXEXTSIS-COM-MSG
280114             CALL "GEMSG"
280114             MOVE 1                   TO WSCR-ERRO
280114             GO TO FA-114.
280114          MOVE HORA-T-VISITA    TO VDCLICAT-TEMPO-MED-VISITA.
280114          MOVE "S"              TO WSCR-CAMPO-OK(WSCR-CAMPO).
280114   FA-114.
280114          EXIT.
      *----------------------------------------------------------------*
140616   A-115.
140616          PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
140616          IF WSCR-ERRO NOT = ZEROS
140616              GO TO FA-115.
140616          PERFORM VE-TECLA    THRU FVE-TECLA.
140616          IF WTECLA-ESC OR
140616            (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
140616              GO TO FA-115.
140616          IF WSCR-ERRO NOT = ZEROS
140616              GO TO FA-115.
140616          IF ALTA-F-VISITA = ZEROS
140616             MOVE ALTA-F-VISITA       TO VDCLICAT-ALTA-FREQUENCIA
140616             MOVE "S"                 TO WSCR-CAMPO-OK(WSCR-CAMPO)
140616             GO TO FA-115.
140616          IF HORA-AF-VISITA > 23
140616             MOVE 999                 TO XXEXTSIS-COM-NRMSG
140616             MOVE "Informar Hora, intervalo de 00 as 23 "
140616                                      TO XXEXTSIS-COM-MSG
140616             CALL "GEMSG"
140616             MOVE 1                   TO WSCR-ERRO
140616             GO TO FA-115.
140616          IF MINUTO-AF-VISITA > 59
140616             MOVE 999                 TO XXEXTSIS-COM-NRMSG
140616             MOVE "Informar minuto, intervalo de 00 as 59"
140616                                      TO XXEXTSIS-COM-MSG
140616             CALL "GEMSG"
140616             MOVE 1                   TO WSCR-ERRO
140616             GO TO FA-115.
140616          MOVE ALTA-F-VISITA    TO VDCLICAT-ALTA-FREQUENCIA.
140616          MOVE "S"              TO WSCR-CAMPO-OK(WSCR-CAMPO).
140616   FA-115.
140616          EXIT.
      *----------------------------------------------------------------*
140616   A-116.
140616          PERFORM TESTA-OPCAO-T1 THRU FTESTA-OPCAO-T1.
140616          IF WSCR-ERRO NOT = ZEROS
140616              GO TO FA-116.
140616          PERFORM VE-TECLA    THRU FVE-TECLA.
140616          IF WTECLA-ESC OR
140616            (WTECLA-FECHAR AND WEVE-TYPE = CMD-CLOSE)
140616              GO TO FA-116.
140616          IF WSCR-ERRO NOT = ZEROS
140616              GO TO FA-116.
140616          IF BAIXA-F-VISITA = ZEROS
140616             MOVE BAIXA-F-VISITA      TO VDCLICAT-BAIXA-FREQUENCIA
140616             MOVE "S"                 TO WSCR-CAMPO-OK(WSCR-CAMPO)
140616             GO TO FA-116.
140616          IF HORA-BF-VISITA > 23
140616             MOVE 999                 TO XXEXTSIS-COM-NRMSG
140616             MOVE "Informar Hora, intervalo de 00 as 23 "
140616                                      TO XXEXTSIS-COM-MSG
140616             CALL "GEMSG"
140616             MOVE 1                   TO WSCR-ERRO
140616             GO TO FA-116.
140616          IF MINUTO-BF-VISITA > 59
140616             MOVE 999                 TO XXEXTSIS-COM-NRMSG
140616             MOVE "Informar minuto, intervalo de 00 as 59"
140616                                      TO XXEXTSIS-COM-MSG
140616             CALL "GEMSG"
140616             MOVE 1                   TO WSCR-ERRO
140616             GO TO FA-116.
140616          MOVE BAIXA-F-VISITA   TO VDCLICAT-BAIXA-FREQUENCIA.
140616          MOVE "S"              TO WSCR-CAMPO-OK(WSCR-CAMPO).
140616   FA-116.
140616          EXIT.
      *----------------------------------------------------------------*
         TESTA-OPCAO-T1.

                IF WTECLA-BOTAO01
                   INITIALIZE WPARMGEGENAUD
                   MOVE WTITULO         TO WPARMGEGENAUD-TITULO
                   MOVE VDCLICAT-AUDIT  TO WPARMGEGENAUD-AUDIT
                   MOVE "GEGENAUD"      TO XXEXTSIS-COM-CALLPRX
                   MOVE XXEXTSIS-VER-GE TO XXEXTSIS-COM-CALLPRX (9:4)
                   CALL  XXEXTSIS-COM-CALLPRX USING WPARMGEGENAUD
                   CANCEL XXEXTSIS-COM-CALLPRX
                   MOVE 1 TO WSCR-ERRO
                   GO TO FTESTA-OPCAO-T1
                END-IF.

                IF WTECLA-OPCAO01
                   MOVE "VDCLGCAP"         TO XXEXTSIS-COM-CALLPRX
                   MOVE XXEXTSIS-VER-VD    TO XXEXTSIS-COM-CALLPRX (9:4)
                   CALL  XXEXTSIS-COM-CALLPRX USING WPARMVDCLIGCA
                   CANCEL XXEXTSIS-COM-CALLPRX
                   IF WPARMVDCLIGCA-GRUPO NOT = SPACES
                      MOVE WPARMVDCLIGCA-GRUPO TO VDCLICAT-GRPCAN
                      MOVE WPARMVDCLIGCA-DESCR TO VDCLIGCA-DESCR
      *            ELSE
      *               MOVE ALL "?"         TO VDCLIGCA-DESCR
                   END-IF
                   DISPLAY D100
                   MOVE 4                  TO WSCR-ERRO
                   MOVE 04                 TO WSCR-ID
                   GO TO FTESTA-OPCAO-T1
                END-IF.

                IF WTECLA-OPCAO02
                   MOVE "VDCLDIVP"         TO XXEXTSIS-COM-CALLPRX
                   MOVE XXEXTSIS-VER-VD    TO XXEXTSIS-COM-CALLPRX (9:4)
                   CALL  XXEXTSIS-COM-CALLPRX USING WPARMVDCLIDIV
                   CANCEL XXEXTSIS-COM-CALLPRX
                   IF WPARMVDCLIDIV-COD NOT = SPACES
                      MOVE WPARMVDCLIDIV-COD   TO VDCLICAT-DIVCAN
                      MOVE WPARMVDCLIDIV-DESCR TO VDCLIDIV-DESCR
      *            ELSE
      *               MOVE ALL "?"         TO VDCLIDIV-DESCR
                   END-IF
                   DISPLAY D100
                   MOVE 4                  TO WSCR-ERRO
                   MOVE 08                 TO WSCR-ID
                   GO TO FTESTA-OPCAO-T1
                END-IF.

150811          IF WTECLA-OPCAO03
150811             MOVE "VDGEDMSP"         TO XXEXTSIS-COM-CALLPRX
150811             MOVE XXEXTSIS-VER-VD    TO XXEXTSIS-COM-CALLPRX (9:4)
150811             INITIALIZE                 WPARMGEGENCGM
150811             MOVE "SEGMENTO"         TO WPARMGEGENCGM-TIPO
150811             CALL  XXEXTSIS-COM-CALLPRX USING WPARMGEGENCGM
150811             CANCEL XXEXTSIS-COM-CALLPRX
150811             IF WPARMGEGENCGM-NRX NOT = SPACES
150811                MOVE WPARMGEGENCGM-NRX   TO VDCLICAT-CODSEG
150811* **           ELSE
150811* **              MOVE ALL "?"         TO VDCLIDIV-DESCR
150811             END-IF
150811             DISPLAY D100
150811             MOVE 4                  TO WSCR-ERRO
150811             MOVE 12                 TO WSCR-ID
150811             GO TO FTESTA-OPCAO-T1
150811          END-IF.
150811
150811          IF WTECLA-OPCAO04
150811             MOVE "VDGEDMSP"         TO XXEXTSIS-COM-CALLPRX
150811             MOVE XXEXTSIS-VER-VD    TO XXEXTSIS-COM-CALLPRX (9:4)
150811             INITIALIZE                 WPARMGEGENCGM
150811             MOVE "TPNEGOCIO"        TO WPARMGEGENCGM-TIPO
150811             CALL  XXEXTSIS-COM-CALLPRX USING WPARMGEGENCGM
150811             CANCEL XXEXTSIS-COM-CALLPRX
150811             IF WPARMGEGENCGM-NR  NOT = ZEROS
150811                MOVE WPARMGEGENCGM-NR    TO VDCLICAT-CODTPNEG
150811* **           ELSE
150811* **              MOVE ALL "?"         TO VDCLIDIV-DESCR
150811             END-IF
150811             DISPLAY D100
150811             MOVE 4                  TO WSCR-ERRO
150811             MOVE 13                 TO WSCR-ID
150811             GO TO FTESTA-OPCAO-T1
150811          END-IF.
         FTESTA-OPCAO-T1.
                    EXIT.
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GEUSULER.PRO".
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GETECLA.PRO".
      *----------------------------------------------------------------*
         COPY "J:\PRGNEW\GE\BOK\GEPERFOR.PRO".
      *----------------------------------------------------------------*
010811   COPY "J:\PRGNEW\GE\BOK\GETSFXRE.PRO".
      *----------------------------------------------------------------*
         RETORNA.
         COPY "J:\PRGNEW\VD\BOK\VDCLIGCA.ECLO"
              REPLACING =='VDCLIGCA'== BY ==VDCLIGCA==.
         COPY "J:\PRGNEW\VD\BOK\VDCLIDIV.ECLO"
              REPLACING =='VDCLIDIV'== BY ==VDCLIDIV==.
151010   COPY "J:\PRGNEW\VD\BOK\VDCLICLI.ECLO"
151010        REPLACING =='VDCLICLI'== BY ==VDCLICLI==.
         COPY "J:\PRGNEW\VD\BOK\VDCLICAT.ECLO"
              REPLACING =='VDCLICAT'== BY ==VDCLICAT==.
         COPY "J:\PRGNEW\GE\BOK\GEGENUSU.ECLO"
              REPLACING =='GEGENUSU'== BY ==GEGENUSU==.
150811   COPY "J:\PRGNEW\GE\BOK\GEGENCGM.ECLO"
150811        REPLACING =='GEGENCGM'== BY ==GEGENCGM==.
         RETORNAF.
         COPY "J:\PRGNEW\VD\BOK\VDRESUB.PRO".
                EXIT PROGRAM.
      *================================================================*
      *
      *----------------------------------------------------------------*
160511*  DESMARCAR A REGULARIDADE EM TODOS OS CLIENTES                 *
160511*  DO CANAL QUE FOI DESMARCADO.                                  *
160511*----------------------------------------------------------------*
160511   DESMARCAR-CLIENTES.
160511          INITIALIZE VDCLICLI-REGISTRO.
160511          START VDCLICLI KEY >= VDCLICLI-CHAVE
160511              INVALID KEY
160511              GO TO DESMARCOU-CLIENTES.
160511   DESMARCANDO-CLIENTES.
160511          READ VDCLICLI NEXT.
160511          IF XXEXTSIS-COM-FSTATUS = XXEXTSIS-COM-FSFIMARQ OR "46"
160511              GO TO DESMARCOU-CLIENTES.
160511          IF NOT XXEXTSIS-VALID-FSTATUS
160511              MOVE "VDCLICLI" TO XXEXTSIS-COM-ARQUIVO
160511              CALL "GESTATU"
160511              GO TO DESMARCOU-CLIENTES.
160511          MOVE VDCLICLI-COD9  TO WTCODIGO9.
160511          DISPLAY TCODIGO9 UPON XXEXTSIS-COM-MWINDOW.
160511          IF VDCLICLI-CAT = VDCLICAT-COD
160511              NEXT SENTENCE
160511          ELSE
160511              GO TO DESMARCANDO-CLIENTES.
160511
160511          IF VDCLICLI-CANDIDATA = 1 OR VDCLICLI-REGULAR = 1
160511              MOVE ZEROS TO VDCLICLI-CANDIDATA
160511                            VDCLICLI-REGULAR
160511              IF VDCLICLI-NMESTB-ANT NOT = ZEROS
160511                 MOVE WSPAD-ANOTAB-CARGA  TO VDCLICLI-ANOTB
160511                 MOVE WSPAD-MESTAB-CARGA  TO VDCLICLI-MESTB
160511                 MOVE VDCLICLI-NMESTB-ANT TO VDCLICLI-NMESTB
160511              END-IF
160511              ACCEPT VDCLICLI-LOGDT     FROM CENTURY-DATE
160511              IF XXEXTSIS-COM-FUNCAO NOT = "DU" AND NOT = "IN"
160511                 ACCEPT VDCLICLI-ALTEDT FROM CENTURY-DATE
160511                 ACCEPT WHORA           FROM TIME
160511                 MOVE WHORMIN             TO VDCLICLI-ALTEHR
160511                 MOVE XXEXTSIS-USU-SIGLA  TO VDCLICLI-ALTESIGLA
160511              END-IF
160511              MOVE WSPAD-DATA    TO VDCLICLI-DTALT
160511              MOVE WSPAD-USUARIO TO VDCLICLI-USERALT
160511              MOVE "VDCLCATA"    TO VDCLICLI-PROGRAMA
160511              REWRITE VDCLICLI-REGISTRO
160511              IF NOT XXEXTSIS-VALID-FSTATUS
160511                 MOVE "VDCLICLI" TO XXEXTSIS-COM-ARQUIVO
160511                 CALL "GESTATU"
160511              END-IF
160511          END-IF.
160511          GO TO DESMARCANDO-CLIENTES.
160511   DESMARCOU-CLIENTES.
160511          EXIT.
