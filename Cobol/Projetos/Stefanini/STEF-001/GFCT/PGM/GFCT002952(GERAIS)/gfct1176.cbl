      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1176.
       AUTHOR.     ROBSON FAUSTO CESA.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1176                                    *
      *    PROGRAMADOR.:   ROBSON FAUSTO CESA      - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ELIAS AUGUSTO BOSCATO   - CPM PATO BRANCO   *
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK / GP 50 *
      *    DATA........:   02/02/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   SEPARA REGISTROS POR TIPO DE COBRANCA.      *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    ENTCOBRA                  I#GFCTIL          *
      *                    SAIMORAC                  I#MORAN1          *
      *                    SAIMORAR                  I#MORAN2          *
      *                    SAIPOUPA                  I#POUPAA          *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE COMUNICACAO COM A BRAD7100.              *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - MODULO PARA CONEXAO COM DB2                      *
      *    BRAD7100 - TRATAMENTO DE ERRO QUANDO PROGRAMA INVALIDO.     *
      *    POOL0431 - MODULO PARA CALCULAR DIGITO DE CONTROLE          *
      *================================================================*
      *                       A L T E R A C A O                        *
      *----------------------------------------------------------------*
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK / GP 50 *
      *    DATA........:   14/11/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVOS...:   INCLUIR PESQUISA GFCTB0A1 P/ BUSCAR DATA    *
      *                    INCLUIR TRATAMENTO DE DIGITO POOL0431       *
      *                    SUBSTITUIR DATA MOVIMENTO POR DPROCM-ATUAL  *
      *----------------------------------------------------------------*
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK / GP 50 *
      *    DATA........:   22/11/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVOS...:   MOVER 900 FIXO P/ MV-VENCIDO-COD-LANCAMENTO *
      *================================================================*
      *                       A L T E R A C A O                        *
      *----------------------------------------------------------------*
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK / GP 50 *
      *    DATA........:   01/02/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVOS...:   SUBSTITUIR DATAS DO SAIMORAC P/ DATA ATUAL  *
      *----------------------------------------------------------------*
      *================================================================*
      *                       A L T E R A C A O                        *
      *----------------------------------------------------------------*
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK / GP 50 *
      *    DATA........:   21/02/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVOS...:   ZERAR  MV-VENCIDO-VALOR(2) - PEDIDO DELGADO *
      *----------------------------------------------------------------*
      *================================================================*
      *                       A L T E R A C A O                        *
      *----------------------------------------------------------------*
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK / GP 50 *
      *    DATA........:   23/02/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVOS...:   MOVER SEG LINHA EXTRATO   P/ DESC-COML-HIST *
      *----------------------------------------------------------------*
      *================================================================*
      *                       A L T E R A C A O                        *
      *----------------------------------------------------------------*
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK / GP 50 *
      *    DATA........:   27/02/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVOS...:  PREENCHER CPOS DE MOEDA A PEDIDO DO DELGADO  *
      *                   MOVER  ' C' PARA MV-DEBTODIA-COD-MOEDA       *
      *                   MOVER   'C' PARA MV-VENCIDO-MOEDA            *
      *----------------------------------------------------------------*
      *================================================================*
      *                       A L T E R A C A O                        *
      *----------------------------------------------------------------*
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK / GP 50 *
      *    DATA........:   07/05/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVOS...:  VOLTAR A PERMITIR DIGITO 'P', ANTERIORMENTE  *
      *                   SUBSTITUIDO POR '0'                          *
      *----------------------------------------------------------------*
      *================================================================*
      *                       A L T E R A C A O                        *
      *----------------------------------------------------------------*
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK / GP 50 *
      *    DATA........:   19/09/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVOS...:  MULTIPLICAR O SUBGRUPO POR 10, P/ REGISTROS  *
      *                   ENVIADOS AO MORA                             *
      *----------------------------------------------------------------*
VJET  *================================================================*
VJET  *                       A L T E R A C A O                        *
VJET  *----------------------------------------------------------------*
VJET  *    ANALISTA....:   VINICIUS C. MADUREIRA - PROCKWORK / GP 50   *
VJET  *    DATA........:   29/11/2007                                  *
VJET  *----------------------------------------------------------------*
VJET  *    OBJETIVOS...:  MOVER DATA-EVTO P/ MV-DEBTODIA-SALDO-ANTERIOR*
VJET  *                   FORMATO EXEMPLO: 00000281107,00.             *
VJET  *    OBJETIVOS...:  MULTIPLICAR POR 100 E MOVER DATA-EVTO PARA   *
VJET  *                   MV-VENCIDO-SUCURSAL                          *
VJET  *                   FORMATO EXEMPLO: 028110700.                  *
VJET  *----------------------------------------------------------------*
0108  *================================================================*
0108  *                       A L T E R A C A O                        *
0108  *----------------------------------------------------------------*
0108  *    ANALISTA....:   WAGNER                - PROCKWORK / GP 50   *
0108  *    DATA........:   09/01/2008                                  *
0108  *----------------------------------------------------------------*
0108  *    OBJETIVO....:  ALTERAR FORMATACAO DOS CAMPOS ABAIXO:        *
0108  *                 - MOVER DATA-EVTO P/ MV-DEBTODIA-SALDO-ANTERIOR*
0108  *                   EXEMPLO: 28.11.2007 => 00000002811,07.       *
0108  *                 - DEIXAR DE MULTIPLICAR POR 100 A DATA-EVENTO  *
0108  *                   AO MANDAR P/ MV-VENCIDO-SUCURSAL             *
0108  *                   FORMATO  28.11.2007 => 000281107.            *
0108  *----------------------------------------------------------------*
1208  *================================================================*
1208  *                       A L T E R A C A O                        *
1208  *----------------------------------------------------------------*
1208  *    ANALISTA....:   WAGNER                - PROCKWORK / GP 50   *
1208  *    DATA........:   19/12/2008                                  *
1208  *----------------------------------------------------------------*
1208  *    OBJETIVO....:  ALTERAR FORMATACAO DOS CAMPOS ABAIXO:        *
1208  *                 - CHAMAR MODULO GFCT5318                       *
1208  *                   SE WRK-5318-CINDCD-NRO-DOCTO = S             *
1208  *                      MOVER CDOCTO_EXTRT       PARA             *
1208  *                                     MV-DEBTODIA-SALDO-ANTERIOR *
1208  *                                     MV-VENCIDO-SUCURSAL        *
1208  *                   SENAO                                        *
1208  *                      MANTER REGRA ANTERIOR                     *
1208  *----------------------------------------------------------------*
110309*================================================================*
110309*    S O N D A  P R O C W O R K  -  A  L  T  E  R  A  C  A  O    *
110309*----------------------------------------------------------------*
110309*    PROGRAMA....:   GFCT1176                                    *
110309*    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
110309*    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
110309*    DATA........:   10/03/2009                                  *
110309*                                                                *
110309*    OBJETIVO....:  RETIRAR ROTINA DE APURACAO DO NUMERO         *
110309*                   DO DOCUMENTO                                 *
110309*                                                                *
      *----------------------------------------------------------------*
      *                 U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
      *        SONDA IT       - CONSULTORIA - ALTERACAO - BI1113       *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA....:  UBIRAJARA          - SONDA IT                *
      *    DATA........:  NOV/2013                                     *
      *    OBJETIVO....:  INCLUIR TRATAMENTO DE TIPO DE CONTA = 3      *
      *                   ( CONTA BENEFICIO ) SERA GRAVADO UM NOVO     *
      *                   ARQUIVO PARA (PBEN).                         *
      *                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ENTCOBRA ASSIGN      TO UT-S-ENTCOBRA
           FILE STATUS                 IS WRK-FS-ENTCOBRA.

           SELECT SAIMORAC ASSIGN      TO UT-S-SAIMORAC
           FILE STATUS                 IS WRK-FS-SAIMORAC.

           SELECT SAIMORAR ASSIGN      TO UT-S-SAIMORAR
           FILE STATUS                 IS WRK-FS-SAIMORAR.

           SELECT SAIPOUPA ASSIGN      TO UT-S-SAIPOUPA
           FILE STATUS                 IS WRK-FS-SAIPOUPA.

BI1113     SELECT SAIPBENA ASSIGN      TO UT-S-SAIPBENA
BI1113     FILE STATUS                 IS WRK-FS-SAIPBENA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  EVENTOS A COBRAR                                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  ENTCOBRA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCTIL'.

      *----------------------------------------------------------------*
      *    OUTPUT: REG. COBRANCA MORA                                  *
      *            ORG. SEQUENCIAL     -   LRECL   =   109 A 303       *
      *----------------------------------------------------------------*

       FD  SAIMORAC
           RECORDING MODE IS V
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#MORAN1'.

      *----------------------------------------------------------------*
      *    OUTPUT: REG. REPIQUE MORA                                   *
      *            ORG. SEQUENCIAL     -   LRECL   =   101 A 305       *
      *----------------------------------------------------------------*

       FD  SAIMORAR
           RECORDING MODE IS V
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#MORAN2'.

      *----------------------------------------------------------------*
      *    OUTPUT: REG. COBRANCA POUP                                  *
      *            ORG. SEQUENCIAL     -   LRECL   =   130             *
      *----------------------------------------------------------------*

       FD  SAIPOUPA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#POUPAA'.

BI1113*----------------------------------------------------------------*
BI1113*    OUTPUT: REG. COBRANCA PBEN                                  *
BI1113*            ORG. SEQUENCIAL     -   LRECL   =   130             *
BI1113*----------------------------------------------------------------*
BI1113
BI1113 FD  SAIPBENA
BI1113     RECORDING MODE IS F
BI1113     LABEL RECORD IS STANDARD
BI1113     BLOCK CONTAINS  0 RECORDS.
BI1113
BI1113 01  FD-REG-SAIPBENA             PIC  X(130).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)          VALUE
           '*** GFCT1176 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-ENTCOBRA          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-SAIMORAC       PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-SAIMORAR       PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-SAIPOUPA       PIC  9(009) COMP-3  VALUE ZEROS.
BI1113 77  ACU-GRAVADOS-SAIPBENA       PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

1208   77  WRK-MODULO                  PIC  X(008) VALUE SPACES.

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.

       01  WRK-DATA-DB2.
           03  WRK-DIA-DB2             PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  WRK-MES-DB2             PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  WRK-ANO-DB2             PIC  X(004)         VALUE SPACES.

       01  WRK-DATA-AUX                PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-AUX-R              REDEFINES           WRK-DATA-AUX.
           03  WRK-ANO-AUX             PIC  9(004).
           03  WRK-MES-AUX             PIC  9(002).
           03  WRK-DIA-AUX             PIC  9(002).

       01  WRK-DATA-DB2-A.
           03  WRK-DIA-DB2-A           PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-MES-DB2-A           PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-ANO-DB2-A           PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-AUX-A              PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-AUX-R-A            REDEFINES         WRK-DATA-AUX-A.
           03  WRK-ANO-AUX-A           PIC  9(004).
           03  WRK-MES-AUX-A           PIC  9(002).
           03  WRK-DIA-AUX-A           PIC  9(002).

       01  WRK-CONTRATO.
           03  WRK-DT-ENVIO            PIC  9(008)         VALUE ZEROS.
           03  WRK-DIG-CONTRATO        PIC  X(001)         VALUE SPACES.
           03  WRK-NUM-CONTRATO        PIC  9(007)         VALUE ZEROS.
           03  WRK-TPO-ARQUIVO         PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(002)         VALUE SPACES.

       01  WRK-TIPO-SALDO.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  WRK-TIPO-SALDO-X        PIC  9(001)         VALUE ZEROS.

       01  WRK-RAZAO                   PIC  9(006)         VALUE ZEROS.
       01  WRK-RAZAO-R                 REDEFINES WRK-RAZAO.
           03  WRK-RAZAO-1             PIC  9(003).
           03  WRK-RAZAO-2             PIC  9(003).

       01  WRK-DATA-EVENTO.
           03  WRK-ANO-EVENTO          PIC  X(004)         VALUE SPACES.
           03  WRK-MES-EVENTO          PIC  X(002)         VALUE SPACES.
           03  WRK-DIA-EVENTO          PIC  X(002)         VALUE SPACES.

       01  WRK-DATA-EV.
           03  WRK-DIA-EV              PIC  X(002)         VALUE SPACES.
           03  WRK-MES-EV              PIC  X(002)         VALUE SPACES.
           03  WRK-ANO-EV              PIC  X(002)         VALUE SPACES.

       01  WRK-DATA-EVTO               PIC  9(006)         VALUE ZEROS.

0108   01  WRK-SD-ANTERIOR-X.
0108       05  FILLER                  PIC  9(007)         VALUE ZEROS.
0108       05  WRK-SD-ENTRA            PIC  9(006)         VALUE ZEROS.
0108   01  WRK-SD-ANTERIOR  REDEFINES  WRK-SD-ANTERIOR-X
                                       PIC  9(011)V99.
1208   01  WRK-SD-ANTERIOR-X-A.
1208       05  FILLER                  PIC  9(004)         VALUE ZEROS.
1208       05  WRK-SD-ENTRA-A          PIC S9(009)         VALUE ZEROS.
1208   01  WRK-SD-ANTERIOR-A REDEFINES WRK-SD-ANTERIOR-X-A
1208                                   PIC  9(011)V99.

1208   01  WRK-CDOCTO-EXTR             PIC S9(009) COMP-3  VALUE ZEROS.
1208   01  WRK-CDOCTO-EXTR-SS          REDEFINES   WRK-CDOCTO-EXTR
1208                                   PIC  9(009) COMP-3.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTES DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-FS-ENTCOBRA             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-SAIMORAC             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-SAIMORAR             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-SAIPOUPA             PIC  X(002)         VALUE SPACES.
BI1113 01  WRK-FS-SAIPBENA             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MENSAGEM DE ERRO DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-TEXTO.
           03  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-ARQUIVO             PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE
               ' - FILE STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

1208   01  WRK-ERRO-GFCT5318.
1208       03  FILLER                  PIC  X(052)         VALUE
1208           '*** ERRO NO ACESSO AO MODULO GFCT5318 - COD. RET. = '.
1208       03  WRK-COD-RET-X           PIC  9(002)         VALUE ZEROS.
1208       03  FILLER                  PIC  X(021)         VALUE ' ***'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL0431 ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-POOL0431.
           05  WRK-CONTA-0431          PIC  9(007)         VALUE ZEROS.

       01  WRK-DIGITO-0431             PIC  X              VALUE SPACES.
       01  WRK-TAMANHO-0431            PIC  9(002)         VALUE ZEROS.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'POL7100C'.

      *------------------- INCLUIDO EM 12/2008 ------------------------*
1208   01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA O MODULO GFCT5318 ***'.
      *----------------------------------------------------------------*

______*01__WRK-5318-AREA-ENTRADA.
______*____03__WRK-5318-AMBIENTE_______PIC__X(001)_____VALUE_SPACES.
______*____03__WRK-5318-CODIGO-TARIFA__PIC__9(005)_____VALUE_ZEROS.
______*____03__WRK-5318-DT-REFER_______PIC__X(010)_____VALUE_SPACES.
      *
______*01__WRK-5318-AREA-SAIDA.
______*____03__WRK-5318-COD-RETORNO____PIC__9(002)_____VALUE_ZEROS.
______*____03__WRK-5318-COD-SQL-ERRO___PIC__9(003)_____VALUE_ZEROS.
______*____03__WRK-5318-DESC-MSG_______PIC__X(070)_____VALUE_SPACES.
      *
______*01__WRK-5318-DADOS-RETORNO.
______*____03__WRK-5318-DADOS-PRINCIPAIS.
______*________05__WRK-5318-CSERVC-TARIF_______PIC__9(005)_VALUE_ZEROS.
______*________05__WRK-5318-CBCO-CCREN_________PIC__9(003)_VALUE_ZEROS.
______*________05__WRK-5318-CIDTFD-LCTO________PIC__9(005)_VALUE_ZEROS.
______*________05__WRK-5318-CIDTFD-SNAL-LCTO___PIC__9(001)_VALUE_ZEROS.
______*________05__WRK-5318-CEMPR-INC__________PIC__9(005)_VALUE_ZEROS.
______*________05__WRK-5318-CDEPDC_____________PIC__9(005)_VALUE_ZEROS.
______*________05__WRK-5318-CTPO-SERVC-TARIF___PIC__9(001)_VALUE_ZEROS.
______*________05__WRK-5318-RSERVC-TARIF_______PIC__X(255)_VALUE_SPACES.
______*________05__WRK-5318-RSERVC-TARIF-REDZD_PIC__X(100)_VALUE_SPACES.
______*________05__WRK-5318-CPSSOA-SERVC-TARIF_PIC__X(001)_VALUE_SPACES.
______*________05__WRK-5318-CTPO-CTA-DEB-TARIF_PIC__9(001)_VALUE_ZEROS.
______*________05__WRK-5318-CTPO-DEB-COBR______PIC__9(001)_VALUE_ZEROS.
______*________05__WRK-5318-RUND-COBR-SERVC____PIC__X(020)_VALUE_SPACES.
______*________05__WRK-5318-CLCTO-SERVC________PIC__9(005)_VALUE_ZEROS.
______*________05__WRK-5318-HIDTFD-MANUT-SERVC-R
______*________________________________________PIC__X(026)_VALUE_SPACES.
______*________05__WRK-5318-CSEQ-MANUT-SERVC-R_PIC__9(002)_VALUE_ZEROS.
______*________05__WRK-5318-CTPO-LCTO-COBR_____PIC__9(002)_VALUE_ZEROS.
______*________05__WRK-5318-CTPO-LCTO-ESTRN____PIC__9(002)_VALUE_ZEROS.
______*________05__WRK-5318-CCART-DEB-TARIF____PIC__9(005)_VALUE_ZEROS.
______*________05__WRK-5318-CINDCD-DEB-PCIAL___PIC__X(001)_VALUE_SPACES.
______*________05__WRK-5318-CINDCD-PCIAL-ULT___PIC__X(001)_VALUE_SPACES.
______*________05__WRK-5318-CINDCD-EVNTO-PCELD_PIC__X(001)_VALUE_SPACES.
______*________05__WRK-5318-CINDCD-QTD-REG_____PIC__X(001)_VALUE_SPACES.
RIC___*________05__WRK-5318-CINDCD-NRO-DOCTO___PIC__X(001)_VALUE_SPACES.
      *
______*_____03_WRK-5318-DADOS-DE-PACOTE__OCCURS___50.
______*________05__WRK-5318-DINIC-VGCIA-COMPS__PIC__X(010)_VALUE_SPACES.
______*________05__WRK-5318-CSEQ-COMPO-PCOTE___PIC__9(003)_VALUE_ZEROS.
______*________05__WRK-5318-DFIM-VGCIA-COMPS___PIC__X(010)_VALUE_SPACES.
______*________05__WRK-5318-CGRP-SERVC_________PIC__9(005)_VALUE_ZEROS.
______*________05__WRK-5318-DINIC-VGCIA-SERVC__PIC__X(010)_VALUE_SPACES.
______*________05__WRK-5318-CTARIF-COMPO-PCOTE_PIC__9(005)_VALUE_ZEROS.
______*________05__WRK-5318-QFRANQ-COMPO-PCOTE_PIC__9(003)_VALUE_ZEROS.
______*________05__WRK-5318-CPER-COBR-COMPO____PIC__9(001)_VALUE_ZEROS.
______*________05__WRK-5318-VEXCED-COMPO-PCOTE_PIC__9(011)V9(002)
______*____________________________________________________VALUE_ZEROS.
______*________05__WRK-5318-PEXCED-COMPO-PCOTE_PIC__9(007)V9(004)
______*____________________________________________________VALUE_ZEROS.
______*________05__WRK-5318-QDIA-COBR-COMPO____PIC__9(002)_VALUE_ZEROS.
______*________05__WRK-5318-RSEGDA-LIN-EXCED___PIC__X(032)_VALUE_SPACES.
______*________05__WRK-5318-HIDTFD-MANUT-SERVC_PIC__X(026)_VALUE_SPACES.
______*________05__WRK-5318-CSEQ-MANUT-SERVC___PIC__9(002)_VALUE_ZEROS.
      *
      *
______*____03__WRK-5318-DADOS-DE-PARAMENTROS.
______*________05__WRK-5318-DINIC-VGCIA-TARIF-P
______*________________________________________PIC__X(010)_VALUE_SPACES.
______*________05__WRK-5318-DFIM-VGCIA-TARIF___PIC__X(010)_VALUE_SPACES.
______*________05__WRK-5318-CNATUZ-SERVC_______PIC__9(003)_VALUE_ZEROS.
______*________05__WRK-5318-CCJTO-SERVC________PIC__9(003)_VALUE_ZEROS.
______*________05__WRK-5318-CINDCD-DEB-CONSL___PIC__X(001)_VALUE_SPACES.
______*________05__WRK-5318-CINDCD-EXCED-CONSL_PIC__X(001)_VALUE_SPACES.
______*________05__WRK-5318-QFRANQ-TARIF_______PIC__9(003)_VALUE_ZEROS.
______*________05__WRK-5318-QMAX-TENTV-DEB_____PIC__9(003)_VALUE_ZEROS.
______*________05__WRK-5318-CPER-FRANQ-TARIF___PIC__9(001)_VALUE_ZEROS.
______*________05__WRK-5318-CPER-CALC-TARIF____PIC__9(001)_VALUE_ZEROS.
______*________05__WRK-5318-CINIC-PER-CALC_____PIC__9(001)_VALUE_ZEROS.
______*________05__WRK-5318-DDIA-COBR-TARIF____PIC__9(002)_VALUE_ZEROS.
______*________05__WRK-5318-QDIA-CAREN-TARIF___PIC__9(002)_VALUE_ZEROS.
______*________05__WRK-5318-PDESC-CAREN-TARIF__PIC__9(007)V9(004)
______*____________________________________________________VALUE_ZEROS.
______*________05__WRK-5318-CINDCD-INIC-CAREN__PIC__9(001)_VALUE_ZEROS.
______*________05__WRK-5318-QDIA-RTCAO-COBR____PIC__9(002)_VALUE_ZEROS.
______*________05__WRK-5318-DINIC-CAREN-TARIF__PIC__X(010)_VALUE_SPACES.
______*________05__WRK-5318-RSEGDA-LIN-EXTRT___PIC__X(032)_VALUE_SPACES.
______*________05__WRK-5318-HIDTFD-MANUT-SERVC-P
______*________________________________________PIC__X(026)_VALUE_SPACES.
______*________05__WRK-5318-CSEQ-MANUT-SERVC-P_PIC__9(002)_VALUE_ZEROS.
______*________05__WRK-5318-CINIC-PER-FRANQ____PIC__9(001)_VALUE_ZEROS.
______*________05__WRK-5318-VMIN-ENVIO-COBR____PIC__9(009)V9(2)
______*____________________________________________________VALUE_ZEROS.
______*________05__WRK-5318-CINDCD-TARIF-PROML_PIC__X(001)_VALUE_SPACES.
______*________05__WRK-5318-CINDCD-TENTV-COBR__PIC__X(001)_VALUE_SPACES.
______*________05__WRK-5318-QTENTV-SEM-COBR____PIC__9(003)_VALUE_ZEROS.
______*________05__WRK-5318-CINDCD-RETOR-COBR__PIC__X(001)_VALUE_SPACES.
______*________05__WRK-5318-CINDCD-CALC-PRORT__PIC__X(001)_VALUE_SPACES.
RIC___*________05__WRK-5318-QDIA-COBR-TARIF____PIC__9(002)_VALUE_ZEROS.
RIC___*________05__WRK-5318-CIDTFD-COBR-FERI___PIC__X(001)_VALUE_SPACES.
______*________05__WRK-5318-CTPO-FRANQ-TARIF_PIC_9(001)__VALUE_ZEROS.
      *
______*____03__WRK-5318-DADOS-DE-VALORES_______OCCURS___50.
______*________05__WRK-5318-DINIC-VGCIA-TARIF__PIC__X(010)_VALUE_SPACES.
______*________05__WRK-5318-CSEQ-COMPO-TARIF___PIC__9(002)_VALUE_ZEROS.
______*________05__WRK-5318-CMOEDA-BACEN_______PIC__9(005)_VALUE_ZEROS.
______*________05__WRK-5318-RCOMPO-TARIF_______PIC__X(255)_VALUE_SPACES.
______*________05__WRK-5318-VCOMPO-TARIF_______PIC__9(011)V9(002)
______*____________________________________________________VALUE_ZEROS.
______*________05__WRK-5318-PSERVC-PTADO_______PIC__9(007)V9(004)
______*____________________________________________________VALUE_ZEROS.
_____**________05__WRK-5318-VMAX-COMPO-TARIF___PIC__9(011)V9(002)
______*____________________________________________________VALUE_ZEROS.
______*________05__WRK-5318-VMIN-COMPO-TARIF___PIC__9(011)V9(002)
______*____________________________________________________VALUE_ZEROS.
______*________05__WRK-5318-CMOMEN-CONVS-MOEDA_PIC__9(001)_VALUE_ZEROS.
      *
           EJECT
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT1176 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-VERIFICAR-VAZIO.

           PERFORM 300000-PROCESSAR    UNTIL
                   WRK-FS-ENTCOBRA     EQUAL '10'.

           PERFORM 400000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN  INPUT ENTCOBRA
                OUTPUT SAIMORAC
                       SAIMORAR
                       SAIPOUPA
BI1113                 SAIPBENA.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 111000-TESTAR-FS-ENTCOBRA.

           PERFORM 112000-TESTAR-FS-SAIMORAC.

           PERFORM 113000-TESTAR-FS-SAIMORAR.

           PERFORM 114000-TESTAR-FS-SAIPOUPA.

BI1113     PERFORM 1150-00-TESTAR-FS-SAIPBENA.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       111000-TESTAR-FS-ENTCOBRA       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENTCOBRA         NOT EQUAL '00'
               MOVE WRK-FS-ENTCOBRA    TO WRK-FILE-STATUS
               MOVE 'ENTCOBRA'         TO WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       112000-TESTAR-FS-SAIMORAC       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAIMORAC         NOT EQUAL '00' AND '04'
               MOVE WRK-FS-SAIMORAC    TO WRK-FILE-STATUS
               MOVE 'SAIMORAC'         TO WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       113000-TESTAR-FS-SAIMORAR       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAIMORAR         NOT EQUAL '00' AND '04'
               MOVE WRK-FS-SAIMORAR    TO WRK-FILE-STATUS
               MOVE 'SAIMORAR'         TO WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       113000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       114000-TESTAR-FS-SAIPOUPA       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAIPOUPA         NOT EQUAL '00'
               MOVE WRK-FS-SAIPOUPA    TO WRK-FILE-STATUS
               MOVE 'SAIPOUPA'         TO WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       114000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

BI1113*----------------------------------------------------------------*
BI1113 1150-00-TESTAR-FS-SAIPBENA       SECTION.
BI1113*----------------------------------------------------------------*
BI1113
BI1113     IF  WRK-FS-SAIPBENA         NOT EQUAL '00'
BI1113         MOVE WRK-FS-SAIPBENA    TO WRK-FILE-STATUS
BI1113         MOVE 'SAIPBENA'         TO WRK-ARQUIVO
BI1113         MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
BI1113         PERFORM 999999-PROCESSAR-ROTINA-ERRO
BI1113     END-IF.
BI1113
BI1113*----------------------------------------------------------------*
BI1113 1150-00-99-FIM.                  EXIT.
BI1113*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-LER-ENTCOBRA.

           IF  WRK-FS-ENTCOBRA         EQUAL '10'
               DISPLAY '**************** GFCT1176 *****************'
               DISPLAY '*                                         *'
               DISPLAY '*         ARQUIVO ENTCOBRA VAZIO          *'
               DISPLAY '*                                         *'
               DISPLAY '*         PROCESSAMENTO ENCERRADO         *'
               DISPLAY '*                                         *'
               DISPLAY '**************** GFCT1176 *****************'
           ELSE
               PERFORM 200100-BUSCAR-DATA-PROC
           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200100-BUSCAR-DATA-PROC        SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       200100-99-FIM.          EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-LER-ENTCOBRA             SECTION.
      *----------------------------------------------------------------*

           READ ENTCOBRA.

           IF  WRK-FS-ENTCOBRA         EQUAL '10'
               GO TO 210000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 111000-TESTAR-FS-ENTCOBRA.

           ADD 1                       TO ACU-LIDOS-ENTCOBRA.

110309     IF   COBR-TIPO-COBRANCA     EQUAL 1
110309          MOVE COBR-DATA-EVNTO-ORIG TO WRK-DATA-EVENTO
110309          MOVE WRK-ANO-EVENTO(3:2)  TO WRK-ANO-EV
110309          MOVE WRK-MES-EVENTO       TO WRK-MES-EV
110309          MOVE WRK-DIA-EVENTO       TO WRK-DIA-EV
110309          MOVE WRK-DATA-EV          TO WRK-DATA-EVTO
110309     ELSE
110309          MOVE COBR-DATA-EVNTO-ORIG TO WRK-DATA-EVTO
110309     END-IF.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 300100-CALCULAR-DIGITO.

           EVALUATE COBR-TIPO-ARQUIVO

               WHEN 1
                   PERFORM 310000-GRAVAR-SAIMORAC

               WHEN 2
                   PERFORM 320000-GRAVAR-SAIMORAR

               WHEN OTHER
                   PERFORM 330000-GRAVAR-SAIPOUPA

           END-EVALUATE.

           PERFORM 210000-LER-ENTCOBRA.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300100-CALCULAR-DIGITO          SECTION.
      *----------------------------------------------------------------*

           MOVE  COBR-CONTA-DEB        TO WRK-CONTA-0431.
           MOVE  07                    TO WRK-TAMANHO-0431.
           MOVE  '7'                   TO WRK-DIGITO-0431.

           CALL 'POOL0431'             USING WRK-CONTA-0431
                                             WRK-DIGITO-0431
                                             WRK-TAMANHO-0431.

      *----------------------------------------------------------------*
       300100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       310000-GRAVAR-SAIMORAC          SECTION.
      *----------------------------------------------------------------*

BI1113     IF  COBR-TIPO-CONTA         EQUAL   3
BI1113         PERFORM   330000-GRAVAR-SAIPOUPA
BI1113         GO TO     310000-99-FIM
BI1113     END-IF.

           MOVE COBR-AGENCIA-DEB       TO MV-DEBTODIA-AGENCIA.
           MOVE COBR-GRUPO-DEB         TO WRK-RAZAO-1.
070919     COMPUTE WRK-RAZAO-2         =  (COBR-SUBGRUPO-DEB * 10).
           MOVE WRK-RAZAO              TO MV-DEBTODIA-RAZAO.
           MOVE COBR-CONTA-DEB         TO MV-DEBTODIA-CONTA-CORRENTE.
           MOVE WRK-DIGITO-0431        TO MV-DEBTODIA-DIGITO-CONTA.
           MOVE COBR-CARTEIRA-DEB      TO MV-DEBTODIA-CARTEIRA.
           MOVE COBR-COD-LCTO-DEB      TO MV-DEBTODIA-COD-LANCAMENTO.
           MOVE COBR-NUM-CONTRATO      TO MV-DEBTODIA-NRO-CONTRATO.
           MOVE COBR-DIG-CONTRATO      TO MV-DEBTODIA-DIGITO-CONTRATO.

WS         MOVE DPROCM-ATUAL           TO WRK-DATA-DB2.
WS         MOVE WRK-DIA-DB2            TO WRK-DIA-AUX.
WS         MOVE WRK-MES-DB2            TO WRK-MES-AUX.
WS         MOVE WRK-ANO-DB2            TO WRK-ANO-AUX.
WS         MOVE WRK-DATA-AUX           TO MV-DEBTODIA-DATA-MOVTO-AMD
WS                                        MV-DEBTODIA-DATA-CONTRATO-AMD
WS                                        MV-DEBTODIA-DATA-VENCTO-AMD.

           MOVE COBR-VALOR-DEBITAR     TO MV-DEBTODIA-VALOR-DEBITO.
           MOVE COBR-SEG-LINHA-EXTRATO TO MV-DEBTODIA-HISTORICO.
           MOVE COBR-TIPO-SALDO        TO WRK-TIPO-SALDO.
DELG       MOVE SPACES                 TO MV-DEBTODIA-TIPO-SALDO.
           MOVE ZEROS                  TO MV-DEBTODIA-NUM-PRESTACAO
                                          MV-DEBTODIA-LIMITE-DEBITO
                                          MV-DEBTODIA-EXCECOES
                                          MV-DEBTODIA-INDICE
                                          MV-DEBTODIA-OCORRENCIAS.

0108       MOVE WRK-DATA-EVTO          TO WRK-SD-ENTRA.
0108       MOVE WRK-SD-ANTERIOR        TO MV-DEBTODIA-SALDO-ANTERIOR.

110309     IF   COBR-DOCTO-EXTRT       NOT EQUAL ZEROS
110309          MOVE COBR-DOCTO-EXTRT  TO  WRK-SD-ENTRA-A
110309          MOVE WRK-SD-ANTERIOR-A TO  MV-DEBTODIA-SALDO-ANTERIOR
110309     END-IF.

WS         MOVE 'C '                   TO MV-DEBTODIA-COD-MOEDA.

RRP        MOVE 2                      TO MV-DEBTODIA-INDICE.

RRP        MOVE 3                      TO MV-DEBTODIA-NRO-CAMPO(1).
RRP        MOVE 9                      TO MV-DEBTODIA-COD-GARANTIA(1).
RRP        MOVE COBR-VALOR-DEBITAR     TO MV-DEBTODIA-VALOR(1).
RRP        MOVE 27                     TO MV-DEBTODIA-NRO-CAMPO(2).
RRP        MOVE 8                      TO MV-DEBTODIA-COD-GARANTIA(2).
RRP        MOVE COBR-VALOR-DEBITAR     TO MV-DEBTODIA-VALOR(2).

           WRITE REG-MOVTO-DEBITODIA.

           PERFORM 112000-TESTAR-FS-SAIMORAC.

           ADD 1                       TO ACU-GRAVADOS-SAIMORAC.

      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       320000-GRAVAR-SAIMORAR          SECTION.
      *----------------------------------------------------------------*

BI1113     IF  COBR-TIPO-CONTA         EQUAL   3
BI1113         PERFORM   330000-GRAVAR-SAIPOUPA
BI1113         GO TO     320000-99-FIM
BI1113     END-IF.

           MOVE COBR-AGENCIA-DEB       TO MV-VENCIDO-AGENCIA.
           MOVE COBR-GRUPO-DEB         TO WRK-RAZAO-1.
070919     COMPUTE WRK-RAZAO-2         =  (COBR-SUBGRUPO-DEB * 10).
           MOVE WRK-RAZAO              TO MV-VENCIDO-RAZAO.
           MOVE COBR-CONTA-DEB         TO MV-VENCIDO-CONTA.
           MOVE WRK-DIGITO-0431        TO MV-VENCIDO-DIGITO-CONTA.
           MOVE COBR-CARTEIRA-DEB      TO MV-VENCIDO-CARTEIRA.
DELG       MOVE 900                    TO MV-VENCIDO-COD-LANCAMENTO.
           MOVE COBR-NUM-CONTRATO      TO MV-VENCIDO-NRO-CONTRATO.
           MOVE COBR-DIG-CONTRATO      TO MV-VENCIDO-DIGITO-CONTRATO.
           MOVE COBR-DATA-ENVIO        TO MV-VENCIDO-DATA-VENCTO-AMD.
           MOVE COBR-DATA-ORIG         TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-AUX.
           MOVE WRK-MES-DB2            TO WRK-MES-AUX.
           MOVE WRK-ANO-DB2            TO WRK-ANO-AUX.
WS         MOVE DPROCM-ATUAL           TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-AUX.
           MOVE WRK-MES-DB2            TO WRK-MES-AUX.
           MOVE WRK-ANO-DB2            TO WRK-ANO-AUX.
           MOVE WRK-DATA-AUX           TO MV-VENCIDO-DATA-MOVTO-AMD.
           MOVE COBR-VALOR-DEBITAR     TO MV-VENCIDO-VALOR-DEBITO.
           MOVE COBR-SEG-LINHA-EXTRATO TO MV-VENCIDO-HISTORICO.
           MOVE COBR-TIPO-SALDO        TO WRK-TIPO-SALDO.
DELG       MOVE SPACES                 TO MV-VENCIDO-TIPO-SALDO.
           MOVE ZEROS                  TO MV-VENCIDO-NUM-PRESTACAO
                                          MV-VENCIDO-VR-REAL-PARCELA
                                          MV-VENCIDO-DESBLOQ-CONSULTA
                                          MV-VENCIDO-LIMITE-DEBITO
                                          MV-VENCIDO-EXCECOES
                                          MV-VENCIDO-INDICE.

0108       MOVE    WRK-DATA-EVTO       TO MV-VENCIDO-SUCURSAL.

110309     IF COBR-DOCTO-EXTRT         NOT EQUAL ZEROS
110309         MOVE COBR-DOCTO-EXTRT   TO WRK-CDOCTO-EXTR
110309         MOVE WRK-CDOCTO-EXTR-SS TO MV-VENCIDO-SUCURSAL
110309     END-IF.

           IF  COBR-INCONDICIONAL      EQUAL 'S'
               MOVE 3                  TO MV-VENCIDO-TIPO-DEBITO
           ELSE
               MOVE COBR-DEB-PARCIAL   TO MV-VENCIDO-TIPO-DEBITO
           END-IF.

WS         MOVE 'C'                    TO MV-VENCIDO-CODIGO-MOEDA.

RRP        MOVE 2                      TO MV-VENCIDO-INDICE.

RRP        MOVE 28                     TO MV-VENCIDO-NRO-CAMPO(1).
RRP        MOVE 9                      TO MV-VENCIDO-COD-GARANTIA(1).
RRP        MOVE COBR-VALOR-DEBITAR     TO MV-VENCIDO-VALOR(1).
RRP        MOVE 29                     TO MV-VENCIDO-NRO-CAMPO(2).
RRP        MOVE 8                      TO MV-VENCIDO-COD-GARANTIA(2).
WS         MOVE ZEROS                  TO MV-VENCIDO-VALOR(2).

           WRITE REG-MOVTO-VENCIDOS.

           PERFORM 113000-TESTAR-FS-SAIMORAR.

           ADD 1                       TO ACU-GRAVADOS-SAIMORAR.

      *----------------------------------------------------------------*
       320000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       330000-GRAVAR-SAIPOUPA          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO REG-DEBPOUP.

           INITIALIZE REG-DEBPOUP.

           MOVE COBR-AGENCIA-DEB       TO POUP-AGENCIA.

BI1113     IF  COBR-TIPO-CONTA         EQUAL 1  OR  3
               MOVE COBR-GRUPO-DEB     TO POUP-GRUPO
               MOVE COBR-SUBGRUPO-DEB  TO POUP-SUBGRUPO
           ELSE
               MOVE 10                 TO POUP-GRUPO
               MOVE 51                 TO POUP-SUBGRUPO
           END-IF.

DELG       MOVE COBR-NUM-CONTRATO      TO POUP-NRO-DOCTO.
           MOVE COBR-CONTA-DEB         TO POUP-CONTA.
           MOVE WRK-DIGITO-0431        TO POUP-DIG-CTA.
WS         MOVE DPROCM-ATUAL           TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-AUX.
           MOVE WRK-MES-DB2            TO WRK-MES-AUX.
           MOVE WRK-ANO-DB2            TO WRK-ANO-AUX.
           MOVE WRK-DATA-AUX           TO POUP-DTMOV-AAAAMMDD.
           MOVE COBR-COD-LCTO-DEB      TO POUP-COD-LCTO.
           MOVE 1                      TO POUP-DEB-CRED.
           MOVE COBR-VALOR-DEBITAR     TO POUP-VLR-DEB.
           MOVE 'GFCT'                 TO POUP-CCUSTO.
WS         MOVE 'N'                    TO POUP-COMPL-HIST.
WS         MOVE 'S'                    TO POUP-COMPL-HIST.
WS         MOVE COBR-SEG-LINHA-EXTRATO TO POUP-DESC-COMPL-HIST.

           IF  COBR-DEB-PARCIAL        EQUAL 1
               MOVE 'P'                TO POUP-DEB-TOT-PAR
           ELSE
               MOVE 'T'                TO POUP-DEB-TOT-PAR
           END-IF.

           IF  COBR-TIPO-SALDO         EQUAL 1
               MOVE 'V'                TO POUP-DEB-VINC-DISP
           ELSE
               MOVE 'D'                TO POUP-DEB-VINC-DISP
           END-IF.

           MOVE COBR-DATA-ENVIO        TO WRK-DT-ENVIO.
           MOVE COBR-DIG-CONTRATO      TO WRK-DIG-CONTRATO.
           MOVE COBR-NUM-CONTRATO      TO WRK-NUM-CONTRATO.
           MOVE COBR-TIPO-ARQUIVO      TO WRK-TPO-ARQUIVO.
           MOVE WRK-CONTRATO           TO POUP-CONTRATO.
           MOVE 1                      TO POUP-NRO-PARCELA.
           MOVE 'N'                    TO POUP-SEQ-OBRIG-DEB.

BI1113     IF  COBR-TIPO-CONTA         EQUAL 3
BI1113         MOVE  REG-DEBPOUP   TO   FD-REG-SAIPBENA
BI1113         WRITE FD-REG-SAIPBENA
BI1113         PERFORM 1150-00-TESTAR-FS-SAIPBENA
BI1113         ADD 1          TO  ACU-GRAVADOS-SAIPBENA
BI1113         GO             TO  330000-99-FIM
BI1113     END-IF.


           WRITE REG-DEBPOUP.

           PERFORM 114000-TESTAR-FS-SAIPOUPA.

           ADD 1                       TO ACU-GRAVADOS-SAIPOUPA.

      *----------------------------------------------------------------*
       330000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       400000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 410000-EMITIR-DISPLAY.

           CLOSE ENTCOBRA
                 SAIMORAC
                 SAIMORAR
                 SAIPOUPA
BI1113           SAIPBENA.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

           GOBACK.

      *----------------------------------------------------------------*
       400000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       410000-EMITIR-DISPLAY           SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-ENTCOBRA     TO WRK-MASCARA.

           DISPLAY '**************** GFCT1176 *****************'.
           DISPLAY '*                                         *'.
           DISPLAY '*   TOTAL LIDOS    ENTCOBRA: ' WRK-MASCARA '  *'.
           DISPLAY '*                                         *'.

           MOVE ACU-GRAVADOS-SAIMORAC  TO WRK-MASCARA.

           DISPLAY '*   TOTAL GRAVADOS SAIMORAC: ' WRK-MASCARA '  *'.
           DISPLAY '*                                         *'.

           MOVE ACU-GRAVADOS-SAIMORAR  TO WRK-MASCARA.

           DISPLAY '*   TOTAL GRAVADOS SAIMORAR: ' WRK-MASCARA '  *'.
           DISPLAY '*                                         *'.

           MOVE ACU-GRAVADOS-SAIPOUPA  TO WRK-MASCARA.

           DISPLAY '*   TOTAL GRAVADOS SAIPOUPA: ' WRK-MASCARA '  *'.
           DISPLAY '*                                         *'.

BI1113     MOVE ACU-GRAVADOS-SAIPBENA  TO WRK-MASCARA.
BI1113
BI1113     DISPLAY '*   TOTAL GRAVADOS SAIPBENA: ' WRK-MASCARA '  *'.
BI1113     DISPLAY '*                                         *'.
           DISPLAY '**************** GFCT1176 *****************'.

      *----------------------------------------------------------------*
       410000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1176'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 04                     TO RETURN-CODE.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

