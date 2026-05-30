      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT3610.
       AUTHOR.     ANTONIO PAGNOCCA NETO.
      *================================================================*
      *      S O N D A  I T                                            *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT3610                                    *
      *    PROGRAMADOR.:   PAGNOCCA - SONDA IT - GR. 50                *
      *    ANALISTA....:   PAGNOCCA - SONDA IT - GR. 50                *
      *    DATA........:   ABR/2013                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CARGA MASSIVA - ADESAO INDIVIDUAL           *
      *                    CESTA TIPO CELULAR                          *
      *                    VIA ARQUIVO ISD - UADMASSC.TCT              *
      *                    CONSISTENCIA INICIAL                        *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
      *                     ENTRADA                      GFCTWBC0      *
      *                     SAIDA                        GFCTWBC4      *
      *                     REJEITAD                     GFCTWBC0      *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *                   DB2PRD.PARM_DATA_PROCM     GFCTB0A1          *
      *                   DB2PRD.PRMSS_ADSAO_PCOTE   GFCTB0C6          *
      *                   DB2PRD.PRMSS_GRP_DEPDC     GFCTB0C7          *
      *                   DB2PRD.PRMSS_GRP_EMPR      GFCTB0C8          *
      *                   DB2PRD.PRMSS_GRP_MUN       GFCTB0C9          *
      *                   DB2PRD.PRMSS_GRP_PAB       GFCTB0D0          *
      *                   DB2PRD.PRMSS_GRP_PSTAL     GFCTB0D1          *
      *                   DB2PRD.PRMSS_GRP_SGMTO     GFCTB0D2          *
      *                   DB2PRD.PRMSS_GRP_UF        GFCTB0D3          *
      *                   DB2PRD.SERVC_TARIF_PRINC   GFCTB0D8          *
      *                   DB2PRD.TPRMSS_GRP_CLI      GFCTB0I0          *
      *                   DB2PRD.V01CLIENTE_AGENCIA  CLIEV004          *
      *                   DB2PRD.V01CTA_POUPANCA     CLIEV007          *
      *                   DB2PRD.V01CTA_CORRENTE     CLIEV008          *
      *                   DB2PRD.PARM_SERVC_TARIF    GFCTB0A2          *
      *                   DB2PRD.ADSAO_INDVD_PCOTE   GFCTB009          *
      *                   DB2PRD.TARIF_CORRL_SIST    GFCTB0F0          *
      *                   DB2PRD.TASSOC_SGMTO_CLI    GFCTB0H3          *
      *                   DB2PRD.TTPO_SGMTO_GSTAO    GFCTB0H5          *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTRK - ENTRADA E SAIDA - OBTER COD UF E MUNICIPIO DA    *
      *               AGENCIA.                                         *
      *    I#GFCTJM - ENTRADA E SAIDA - OBTER COD SEG GESTAO TARIFA.   *
      *    I#SCCE91 - AREA DE COMUNICACAO - VALIDAR SITUACAO DA CONTA. *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *    POL7100C - AREA CHAMADA POOL7100 - CANCELAMENTO.            *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5527 - OBTER CPF/CNPJ/POSTO DE SERVICO.                 *
      *    GFCT5534 - OBTER CODIGO SEGMENTO GESTAO TARIFA.             *
      *    GFCT5564 - OBTER COD DA UF E MUNICIPIO DA AGENCIA.          *
      *    SCCE9100 - VERIFICAR SITUACAO DA CONTA.                     *
      *    POOL0025 - SOMA DIAS NA DATA.                               *
      *    POOL1640 - SOMA DE DIAS, MESES E ANOS EM UMA DETERMINADA    *
      *               DATA COM SECULO.                                 *
      *    POOL1050 - FAZ CONEXAO COM DB2.                             *
      *    POOL7100 - AREA PARA TRATAMENTO DE ERROS.                   *
      *    POOL7600 - OBTER DATA E HORA DO SISTEMA.                    *
      *                                                                *
BI0115*----------------------------------------------------------------*
BI0115*                 U L T I M A   A L T E R A C A O                *
BI0115*----------------------------------------------------------------*
BI0115*                 SONDA IT  - ALTERACAO - BI0115                 *
BI0115*----------------------------------------------------------------*
BI0115*                                                                *
BI0115*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA - IT                 *
BI0115*    DATA........:  01 / 2015                                    *
BI0115*    OBJETIVO....:  TRATAR TIPO DE TARIFA = 4                    *
BI0115*                   ( PACOTE PERSONALIZADO )                     *

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

           SELECT ENTRADA   ASSIGN     TO UT-S-ENTRADA
           FILE STATUS                 IS WRK-FS-ENTRADA.

           SELECT REJEITAD ASSIGN      TO UT-S-REJEITAD
           FILE STATUS                 IS WRK-FS-REJEITAD.

           SELECT SAIDA    ASSIGN      TO UT-S-SAIDA
           FILE STATUS                 IS WRK-FS-SAIDA.

           SELECT CONTROLE ASSIGN      TO UT-S-CONTROLE
           FILE STATUS                 IS WRK-FS-CONTROLE.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO ENTRADA ISD CADMASS                         *
      *            ORG. SEQUENCIAL     - LRECL = 178                   *
      *----------------------------------------------------------------*

       FD  ENTRADA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY 'GFCTWBC0'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO RGISTROS REJEITADOS.                        *
      *            ORG. SEQUENCIAL     - LRECL = 178                   *
      *----------------------------------------------------------------*

       FD  REJEITAD
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY 'GFCTWBC0'.

      *----------------------------------------------------------------*
      *    OUTPUT: REGISTROS DE SAIDA PARA INCLUSAO CADASTRO           *
      *            ORG. SEQUENCIAL     - LRECL = 579                   *
      *----------------------------------------------------------------*

       FD  SAIDA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY 'GFCTWBC4'.

      *----------------------------------------------------------------*
      *    OUTPUT: REGISTROS DE CONTROLE PARA INCLUSAO CADASTRO        *
      *            ORG. SEQUENCIAL     - LRECL = 164                   *
      *----------------------------------------------------------------*

       FD  CONTROLE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY 'GFCTWBC6'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(033)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(022)         VALUE
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-ENTRADA           PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-SAIDA          PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-REJEITAD       PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-CONTROLE       PIC  9(011) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(030)         VALUE
           'AREA PARA TESTE DE FILE STATUS'.
      *----------------------------------------------------------------*

       77  WRK-FS-ENTRADA              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SAIDA                PIC  X(002)         VALUE SPACES.
       77  WRK-FS-REJEITAD             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CONTROLE             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.

       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(030)         VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*
       77  WRK-CENTRO-CUSTO            PIC  X(004)         VALUE SPACE.
       77  WRK-AGEN                    PIC  9(005)         VALUE ZERO.
       77  WRK-CONTA                   PIC  9(007)         VALUE ZERO.
       77  WRK-CESTA                   PIC  9(005)         VALUE ZERO.
       77  WRK-AT-BASE                 PIC  X(001)         VALUE 'S'.
       77  WRK-FIM-CURSOR-H3           PIC  X(001)         VALUE 'N'.
       77  WRK-FIM-CURSOR-D2           PIC  X(001)         VALUE 'N'.
       77  WRK-FIM-CURSOR-M2           PIC  X(001)         VALUE 'N'.
       77  WRK-ACHOU-SEL               PIC  X(001)         VALUE 'N'.
       77  WRK-PERMISSAO-H3-OK         PIC  X(001)         VALUE 'N'.
       77  WRK-PERMISSAO-D2-OK         PIC  X(001)         VALUE 'N'.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC  ZZZZZ.ZZZ.999  VALUE ZEROS.
       77  WRK-TIPO-PESSOA-CLIENTE     PIC  X(001)         VALUE SPACES.
       77  WRK-TIPO-CONTA-CLIENTE      PIC  9(001)         VALUE ZEROS.
       77  WRK-ERRO                    PIC  X(001)         VALUE SPACES.
       77  WRK-MSG                     PIC  X(080)         VALUE SPACES.
       77  WRK-CSGMTO-CLI              PIC S9(3)V  COMP-3  VALUE ZEROS.
       77  WRK-PROX-AGRUPAMENTO        PIC  9(003)         VALUE ZEROS.
       77  WRK-SEQ-AGPTO               PIC S9(009) COMP-3  VALUE ZEROS.
       77  WRK-COUNT-DISPLAY           PIC S9(009) COMP-3  VALUE ZEROS.
       77  WRK-MES-CORR                PIC  9(002) COMP-3  VALUE ZEROS.
       77  WRK-MES-PROX                PIC  9(002) COMP-3  VALUE ZEROS.
       77  WRK-NUM-PK-2                PIC  9(002) COMP-3  VALUE ZEROS.
       77  WRK-NUM-PK-4                PIC  9(004) COMP-3  VALUE ZEROS.
      *
      *    DIA = DATA DO DIA
      *    INM = DATA INICIO MES
      *    PRM = DATA INICIO PROX. MES
      *    IVG = DATA INICIO VIGENCIA
      *    FVG = DATA FIM VIGENCIA
      *
       77  WRK-DATA-DIA-PK             PIC  9(008) COMP-3  VALUE ZEROS.
       77  WRK-DATA-INM-PK             PIC  9(008) COMP-3  VALUE ZEROS.
       77  WRK-DATA-PRM-PK             PIC  9(008) COMP-3  VALUE ZEROS.
       77  WRK-DATA-IVG-PK             PIC  9(008) COMP-3  VALUE ZEROS.
       77  WRK-DATA-FVG-PK             PIC  9(008) COMP-3  VALUE ZEROS.
      *
       77  WRK-DT-INICIO               PIC  X(010)         VALUE SPACES.
       77  WRK-FIM-AGRUPAMENTO         PIC  X(001)         VALUE SPACES.
       77  WRK-AGRUPAMENTO-VALIDO      PIC  X(001)         VALUE SPACES.
       77  WRK-EXISTE-AGRUPAMENTO      PIC  X(001)         VALUE SPACES.
______*77__WRK-MSG-PARM-VZIO___________PIC__X(042)_________VALUE
______*____'PARAMETRO_NAO_INFORMADO.PROGRAMA_CANCELADO'.
______*77__WRK-MESES-PARM-VZIO_________PIC__X(050)_________VALUE
______*____'NUMERO_DE_MESES_NAO_NUMERICO_PROGRAMA_CANCELADO'.
______*77__WRK-MESES-PARM-ZERO_________PIC__X(050)_________VALUE
______*____'PARAMETRO_CONTEM_ZERO_-_PROGRAMA_CANCELADO'.

ABR10  01  WRK-CESTA-CELULAR           PIC  X(001)         VALUE SPACES.

       01  WRK-NUM-CEL                 PIC +9(11)          VALUE ZEROS.
       01  FILLER            REDEFINES WRK-NUM-CEL.
           05 WRK-NUM-CEL-S            PIC  X(01).
           05 WRK-NUM-CEL-Z            PIC  9(02).
           05 WRK-NUM-CEL-9            PIC  9(01).
           05 WRK-NUM-CEL-8            PIC  9(08).
       01  FILLER            REDEFINES WRK-NUM-CEL.
           05 WRK-NUM-CEL-S2           PIC  X(01).
           05 WRK-NUM-CEL-11           PIC  9(11).

       01  WRK-CAMPO-TIRA-SINAL        PIC +9(017)         VALUE ZEROS.
       01  FILLER  REDEFINES           WRK-CAMPO-TIRA-SINAL.
           05  WRK-CAMPO-TIRA-SINAL-S  PIC X.
           05  WRK-CAMPO-TIRA-SINAL-17 PIC 9(17).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(01).
               10 WRK-CAMPO-TIRA-SINAL-16 PIC 9(16).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(02).
               10 WRK-CAMPO-TIRA-SINAL-15 PIC 9(15).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(03).
               10 WRK-CAMPO-TIRA-SINAL-14 PIC 9(14).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(04).
               10 WRK-CAMPO-TIRA-SINAL-13 PIC 9(13).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(05).
               10 WRK-CAMPO-TIRA-SINAL-12 PIC 9(12).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(06).
               10 WRK-CAMPO-TIRA-SINAL-11 PIC 9(11).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(07).
               10 WRK-CAMPO-TIRA-SINAL-10 PIC 9(10).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(08).
               10 WRK-CAMPO-TIRA-SINAL-09 PIC 9(09).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(09).
               10 WRK-CAMPO-TIRA-SINAL-08 PIC 9(08).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(10).
               10 WRK-CAMPO-TIRA-SINAL-07 PIC 9(07).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(11).
               10 WRK-CAMPO-TIRA-SINAL-06 PIC 9(06).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(12).
               10 WRK-CAMPO-TIRA-SINAL-05 PIC 9(05).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(13).
               10 WRK-CAMPO-TIRA-SINAL-04 PIC 9(04).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(14).
               10 WRK-CAMPO-TIRA-SINAL-03 PIC 9(03).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(15).
               10 WRK-CAMPO-TIRA-SINAL-02 PIC 9(02).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(16).
               10 WRK-CAMPO-TIRA-SINAL-01 PIC 9(01).

       01  WRK-CAMPO-POE-SINAL-01        PIC +9(01)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-01.
           05  WRK-CAMPO-POE-SINAL-01-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-01-C  PIC 9(01).

       01  WRK-CAMPO-POE-SINAL-02        PIC +9(02)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-02.
           05  WRK-CAMPO-POE-SINAL-02-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-02-C  PIC 9(02).

       01  WRK-CAMPO-POE-SINAL-03        PIC +9(03)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-03.
           05  WRK-CAMPO-POE-SINAL-03-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-03-C  PIC 9(03).

       01  WRK-CAMPO-POE-SINAL-04        PIC +9(04)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-04.
           05  WRK-CAMPO-POE-SINAL-04-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-04-C  PIC 9(04).

       01  WRK-CAMPO-POE-SINAL-05        PIC +9(05)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-05.
           05  WRK-CAMPO-POE-SINAL-05-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-05-C  PIC 9(05).

       01  WRK-CAMPO-POE-SINAL-06        PIC +9(06)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-06.
           05  WRK-CAMPO-POE-SINAL-06-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-06-C  PIC 9(06).

       01  WRK-CAMPO-POE-SINAL-07        PIC +9(07)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-07.
           05  WRK-CAMPO-POE-SINAL-07-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-07-C  PIC 9(07).

       01  WRK-CAMPO-POE-SINAL-08        PIC +9(08)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-08.
           05  WRK-CAMPO-POE-SINAL-08-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-08-C  PIC 9(08).

       01  WRK-CAMPO-POE-SINAL-09        PIC +9(09)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-09.
           05  WRK-CAMPO-POE-SINAL-09-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-09-C  PIC 9(09).

       01  WRK-CAMPO-POE-SINAL-10        PIC +9(10)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-10.
           05  WRK-CAMPO-POE-SINAL-10-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-10-C  PIC 9(10).

       01  WRK-CAMPO-POE-SINAL-11        PIC +9(11)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-11.
           05  WRK-CAMPO-POE-SINAL-11-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-11-C  PIC 9(11).

       01  WRK-CAMPO-POE-SINAL-12        PIC +9(12)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-12.
           05  WRK-CAMPO-POE-SINAL-12-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-12-C  PIC 9(12).

       01  WRK-CAMPO-POE-SINAL-13        PIC +9(13)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-13.
           05  WRK-CAMPO-POE-SINAL-13-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-13-C  PIC 9(13).

       01  WRK-CAMPO-POE-SINAL-14        PIC +9(14)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-14.
           05  WRK-CAMPO-POE-SINAL-14-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-14-C  PIC 9(14).

       01  WRK-CAMPO-POE-SINAL-15        PIC +9(15)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-15.
           05  WRK-CAMPO-POE-SINAL-15-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-15-C  PIC 9(15).

       01  WRK-CAMPO-POE-SINAL-16        PIC +9(16)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-16.
           05  WRK-CAMPO-POE-SINAL-16-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-16-C  PIC 9(16).

       01  WRK-CAMPO-POE-SINAL-17        PIC +9(17)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-17.
           05  WRK-CAMPO-POE-SINAL-17-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-17-C  PIC 9(17).


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(016)         VALUE
           'AREA PARA CHAVES'.
      *----------------------------------------------------------------*

______*01__WRK-CH-ANT.
______*____05__WRK-AGENCIA-ANT_________PIC_9(005).
______*____05__WRK-CONTA-ANT___________PIC_9(007).
______*____05__WRK-TARIFA-ANT__________PIC_9(005).

______*01__WRK-CH-ATU.
______*____05__WRK-AGENCIA-ATU_________PIC_9(005).
______*____05__WRK-CONTA-ATU___________PIC_9(007).
______*____05__WRK-TARIFA-ATU__________PIC_9(005).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(016)         VALUE
           'AREA DA POOL0025'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGEM-1205           PIC  X(050)         VALUE SPACES.


       01  WRK-AREA-POOL1205.
           03  WRK-DT-ENVIADA          PIC  9(008)  COMP-3 VALUE ZEROS.
           03  WRK-OPCAO               PIC  X(001)         VALUE SPACES.
           03  WRK-DT-JULIANA          PIC  9(007)  COMP-3 VALUE ZEROS.
           03  WRK-DT-EDITADA          PIC  X(010)         VALUE SPACES.
           03  WRK-DT-GREGORI          PIC  9(008)         VALUE ZEROS.
           03  WRK-DIA-SEMANA          PIC  X(013)         VALUE SPACES.
           03  WRK-MES-EXTENSO         PIC  X(009)         VALUE SPACES.
           03  WRK-DIA-UT-ANTER        PIC  9(008)         VALUE ZEROS.
           03  WRK-DIA-UT-POSTE        PIC  9(008)         VALUE ZEROS.

       01  WRK-DATA-AMD-R              PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-AMD REDEFINES WRK-DATA-AMD-R.
           05 WRK-DATA-AMD-A           PIC  9(004).
           05 WRK-DATA-AMD-M           PIC  9(002).
           05 WRK-DATA-AMD-D           PIC  9(002).


       01  WRK-DATA-DMA-R              PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-DMA REDEFINES WRK-DATA-DMA-R.
           05 WRK-DATA-DMA-D           PIC  9(002).
           05 WRK-DATA-DMA-M           PIC  9(002).
           05 WRK-DATA-DMA-A           PIC  9(004).

       01  WRK-DATA-DMAB               PIC  X(10)   VALUE '  .  .    '.
       01  FILLER REDEFINES WRK-DATA-DMAB.
           10 WRK-DATA-DMAB-D          PIC  9(002).
           10 FILLER                   PIC  X(001).
           10 WRK-DATA-DMAB-M          PIC  9(002).
           10 FILLER                   PIC  X(001).
           10 WRK-DATA-DMAB-A          PIC  9(004).


______*01__WRK-DATA____________________PIC__9(008)_________VALUE_ZEROS.
______*01__WRK-DATA-R__________________REDEFINES_WRK-DATA.
______*____05_WRK-DIA__________________PIC__9(002).
______*____05_WRK-MES__________________PIC__9(002).
______*____05_WRK-ANO__________________PIC__9(004).

______*01__WRK-DATA-ENTRADA____________PIC__9(009)_COMP-3__VALUE_ZEROS.
______*01__WRK-NUMERO-MESES____________PIC__9(003)_________VALUE_ZEROS.
______*01__WRK-DATA-SAIDA______________PIC__9(009)_COMP-3__VALUE_ZEROS.

______*01__WRK-DATA-SAI-POOL___________PIC__9(009)_________VALUE_ZEROS.
______*01__WRK-DATA-SAI-POOL-R_________REDEFINES_WRK-DATA-SAI-POOL.
______*____05_FILLER___________________PIC__X(001).
______*____05_WRK-DIA-SAI-POOL_________PIC__9(002).
______*____05_WRK-MES-SAI-POOL_________PIC__9(002).
______*____05_WRK-ANO-SAI-POOL_________PIC__9(004).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           'AREA PARA MENSAGENS'.
      *----------------------------------------------------------------*

______*01__WRK-MSG01___________________PIC__X(050)_________VALUE
______*____'REGISTRO_DUPLICADO_P/_ESTA_AG/CTA/TARIF'.

______*01__WRK-MSG02.
______*____03__WRK-MSG02-01____________PIC__X(040)_________VALUE
______*____'ERRO_MODULO_SCCE9100_-_RETURN_CODE_=_'.
______*____03__WRK-MSG02-CD____________PIC__X(004).

______*01__WRK-MSG03.
______*____03__WRK-MSG03-01____________PIC__X(041)_________VALUE
______*____'ERRO_MODULO_SCCE9100_-_CODIGO_RETORNO_=_'.
______*____03__WRK-MSG03-CD____________PIC__X(004).

______*01__WRK-MSG04___________________PIC__X(050)_________VALUE
______*____'CLIENTE_NAO_ENCONTRADO_NA_TABELA_DO_CLIE'.

______*01__WRK-MSG05___________________PIC__X(060)_________VALUE
______*____'TARIFA_NAO_ENCONTRADA_NO_CADASTRO_DE_TARIFAS_-_GFCTB0D8'.

______*01__WRK-MSG06___________________PIC__X(040)_________VALUE
______*____'JA_TEM_UMA_ADESAO_P/_ESTA_AG/CTA'.

______*01__WRK-MSG07.
______*____03__WRK-MSG07-01____________PIC__X(040)_________VALUE
______*____'ERRO_MODULO_GFCT5527_-_RETURN_CODE_=_'.
______*____03__WRK-MSG07-CD____________PIC__X(004).

______*01__WRK-MSG08.
______*____03__WRK-MSG08-01____________PIC__X(041)_________VALUE
______*____'ERRO_MODULO_GFCT5527_-_CODIGO_RETORNO_=_'.
______*____03__WRK-MSG08-CD____________PIC__X(004).

______*01__WRK-MSG09.
______*____03__WRK-MSG09-01____________PIC__X(040)_________VALUE
______*____'ERRO_MODULO_GFCT5534_-_RETURN_CODE_=_'.
______*____03__WRK-MSG09-CD____________PIC__X(004).

______*01__WRK-MSG10.
______*____03__WRK-MSG10-01____________PIC__X(041)_________VALUE
______*____'ERRO_MODULO_GFCT5534_-_CODIGO_RETORNO_=_'.
______*____03__WRK-MSG10-CD____________PIC__X(004).

______*01__WRK-MSG11___________________PIC__X(041)_________VALUE
______*____'AGENCIA/CONTA/TARIFA_SEM_AGRUPAMENTO____'.

______*01__WRK-MSG12___________________PIC__X(041)_________VALUE
______*____'CTPO-CTA-DEB-TARIF_TABELA_DIFERENTE_CLIE'.

______*01__WRK-MSG13.
______*____03__WRK-MSG13-01____________PIC__X(040)_________VALUE
______*____'ERRO_MODULO_GFCT5564_-_RETURN_CODE_=_'.
______*____03__WRK-MSG13-CD____________PIC__X(004).

______*01__WRK-MSG14.
______*____03__WRK-MSG14-01____________PIC__X(041)_________VALUE
______*____'ERRO_MODULO_GFCT5564_-_CODIGO_RETORNO_=_'.
______*____03__WRK-MSG14-CD____________PIC__X(004).

______*01__WRK-MSG15___________________PIC__X(041)_________VALUE
______*____'ERRO_NA_POOL0025_-_DADOS_INCONSISTENTES'.

______*01__WRK-MSG16___________________PIC__X(040)_________VALUE
______*____'TARIFA_PACOTE_NAO_EXISTE_TAB._GFCTB0A2'.

______*01__WRK-MSG17___________________PIC__X(040)_________VALUE
______*____'DT_INICIO_GERADA_MENOR_OU_IGUAL_DT_TAB'.

SP0404*01__WRK-MSG18___________________PIC__X(041)_________VALUE
SP0404*____'CPSSOA-SERVC-TARIF_TABELA_DIFERENTE_CLIE'.

231109*01__WRK-MSG2311_________________PIC__X(041)_________VALUE
231109*____'TARIFAS_CESTAS_CONTA_CELULAR_NAO_PERMIT.'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(015)         VALUE
           'AREA PARA DATAS'.
      *----------------------------------------------------------------*

______*01__WRK-DT-PROCM-ATUAL__________PIC__9(008)_________VALUE_ZEROS.
______*01__WRK-DT-PROCM-ATUAL-R________REDEFINES_WRK-DT-PROCM-ATUAL.
______*____03__WRK-ANO-PROCM-ATUAL_____PIC__9(004).
______*____03__WRK-MES-PROCM-ATUAL_____PIC__9(002).
______*____03__WRK-DIA-PROCM-ATUAL_____PIC__9(002).

______*01__WRK-DT-INI-PRMSS-PCOTE______PIC__9(008)_________VALUE_ZEROS.
______*01__WRK-DT-INI-PRMSS-PCOTE-R____REDEFINES_WRK-DT-INI-PRMSS-PCOTE.
______*____03__WRK-ANO-INI-PRMSS-PCOTE_PIC__9(004).
______*____03__WRK-MES-INI-PRMSS-PCOTE_PIC__9(002).
______*____03__WRK-DIA-INI-PRMSS-PCOTE_PIC__9(002).

______*01__WRK-DT-INI-ADSAO-INDVD______PIC__9(008)_________VALUE_ZEROS.
______*01__WRK-DT-INI-ADSAO-INDVD-R____REDEFINES_WRK-DT-INI-ADSAO-INDVD.
______*____03__WRK-AAAAMM-INI-ADSAO-INDVD
______*________________________________PIC__9(006).
______*____03__FILLER__________________REDEFINES
______*______________________________________WRK-AAAAMM-INI-ADSAO-INDVD.
______*________05__WRK-ANO-INI-ADSAO-INDVD
______*________________________________PIC__9(004).
______*________05__WRK-MES-INI-ADSAO-INDVD
______*________________________________PIC__9(002).
______*____03__WRK-DIA-INI-ADSAO-INDVD_PIC__9(002).

______*01__WRK-DT-FIM-ADSAO-INDVD______PIC__9(008)_________VALUE_ZEROS.
______*01__WRK-DT-FIM-ADSAO-INDVD-R____REDEFINES_WRK-DT-FIM-ADSAO-INDVD.
______*____03__WRK-AAAAMM-FIM-ADSAO-INDVD
______*________________________________PIC__9(006).
______*____03__FILLER__________________REDEFINES
______*______________________________________WRK-AAAAMM-FIM-ADSAO-INDVD.
______*________05__WRK-ANO-FIM-ADSAO-INDVD
______*________________________________PIC__9(004).
______*________05__WRK-MES-FIM-ADSAO-INDVD
______*________________________________PIC__9(002).
______*____03__WRK-DIA-FIM-ADSAO-INDVD_PIC__9(002).

______*01__WRK-DATA-INV________________PIC__9(008)_________VALUE_ZEROS.
______*01__WRK-DATA-INV-R______________REDEFINES_WRK-DATA-INV.
______*____05__WRK-ANO-INV_____________PIC__9(004).
______*____05__WRK-MES-INV_____________PIC__9(002).
______*____05__WRK-DIA-INV_____________PIC__9(002).

______*01__WRK-DATA-DB2________________PIC__X(010)_________VALUE_SPACES.
______*01__WRK-DATA-DB2-R______________REDEFINES_WRK-DATA-DB2.
______*____05__WRK-DIA-DB2_____________PIC__9(002).
______*____05__WRK-PTO1________________PIC__X(001).
______*____05__WRK-MES-DB2_____________PIC__9(002).
______*____05__WRK-PTO2________________PIC__X(001).
______*____05__WRK-ANO-DB2_____________PIC__9(004).

       01  WRK-DATA-INIC-ADESAO        PIC  X(010)         VALUE SPACES.
       01  WRK-DATA-INI-R              REDEFINES WRK-DATA-INIC-ADESAO.
           05  WRK-DIA-INI             PIC  9(002).
           05  WRK-PTO3                PIC  X(001).
           05  WRK-MES-INI             PIC  9(002).
           05  WRK-PTO4                PIC  X(001).
           05  WRK-ANO-INI             PIC  9(004).

______*01__WRK-DTINIC-INV______________PIC__X(008)_________VALUE_SPACES.
______*01__WRK-DTINIC-INV-R____________REDEFINES_WRK-DTINIC-INV.
______*____05__WRK-DTINIC-INV-ANO______PIC__9(004).
______*____05__WRK-DTINIC-INV-MES______PIC__9(002).
______*____05__WRK-DTINIC-INV-DIA______PIC__9(002).

______*01__WRK-DTFIM-INV_______________PIC__X(008)_________VALUE_SPACES.
______*01__WRK-DTFIM-INV-R_____________REDEFINES_WRK-DTFIM-INV.
______*____05__WRK-DTFIM-INV-ANO_______PIC__9(004).
______*____05__WRK-DTFIM-INV-MES_______PIC__9(002).
______*____05__WRK-DTFIM-INV-DIA_______PIC__9(002).

______*01__WRK-DTINIC-TAB-INV__________PIC__X(008)_________VALUE_SPACES.
______*01__WRK-DTINIC-TAB-INV-R________REDEFINES_WRK-DTINIC-TAB-INV.
______*____05__WRK-DTINIC-TAB-INV-ANO__PIC__9(004).
______*____05__WRK-DTINIC-TAB-INV-MES__PIC__9(002).
______*____05__WRK-DTINIC-TAB-INV-DIA__PIC__9(002).

______*01__WRK-DTFIM-TAB-INV__________PIC__X(008)_________VALUE_SPACES.
______*01__WRK-DTFIM-TAB-INV-R________REDEFINES_WRK-DTFIM-TAB-INV.
______*____05__WRK-DTFIM-TAB-INV-ANO__PIC__9(004).
______*____05__WRK-DTFIM-TAB-INV-MES__PIC__9(002).
______*____05__WRK-DTFIM-TAB-INV-DIA__PIC__9(002).

______*01__WRK-DINIC-ADSAO-DB2________PIC__X(010)_________VALUE_SPACES.
______*01__WRK-DINIC-ADSAO-DB2-R______REDEFINES_WRK-DINIC-ADSAO-DB2.
______*____05__WRK-DINIC-ADSAO-DIA____PIC__9(002).
______*____05__WRK-DINIC-ADSAO-PT1____PIC__X(001).
______*____05__WRK-DINIC-ADSAO-MES____PIC__9(002).
______*____05__WRK-DINIC-ADSAO-PT2____PIC__X(001).
______*____05__WRK-DINIC-ADSAO-ANO____PIC__9(004).

______*01__WRK-DDFIM-ADSAO-DB2________PIC__X(010)_________VALUE_SPACES.
______*01__WRK-DDFIM-ADSAO-DB2-R______REDEFINES_WRK-DDFIM-ADSAO-DB2.
______*____05__WRK-DDFIM-ADSAO-DIA____PIC__9(002).
______*____05__WRK-DDFIM-ADSAO-PT1____PIC__X(001).
______*____05__WRK-DDFIM-ADSAO-MES____PIC__9(002).
______*____05__WRK-DDFIM-ADSAO-PT2____PIC__X(001).
______*____05__WRK-DDFIM-ADSAO-ANO____PIC__9(004).

______*01__WRK-DATA-FIM-ADESAO_________PIC__X(010)_________VALUE_SPACES.
______*01__WRK-DATA-FIM-R______________REDEFINES_WRK-DATA-FIM-ADESAO.
______*____05__WRK-DIA-FIM_____________PIC__9(002).
______*____05__WRK-PTO5________________PIC__X(001).
______*____05__WRK-MES-FIM_____________PIC__9(002).
______*____05__WRK-PTO6________________PIC__X(001).
______*____05__WRK-ANO-FIM_____________PIC__9(004).

______*01__WRK-DATA-INIC-AD____________PIC__9(008)_________VALUE_ZEROS.
______*01__WRK-DATA-INIC-AD-R__________REDEFINES_WRK-DATA-INIC-AD.
______*____05__WRK-DIA-INI-AD__________PIC__9(002).
______*____05__WRK-MES-INI-AD__________PIC__9(002).
______*____05__WRK-ANO-INI-AD__________PIC__9(004).

______*01__WRK-DATA-FIM-AD_____________PIC__9(008)_________VALUE_ZEROS.
______*01__WRK-DATA-FIM-AD-R__________REDEFINES_WRK-DATA-FIM-AD.
______*____05__WRK-DIA-FIM-AD__________PIC__9(002).
______*____05__WRK-MES-FIM-AD__________PIC__9(002).
______*____05__WRK-ANO-FIM-AD__________PIC__9(004).

______*01__WRK-DPROCM-INV______________PIC__9(008)_________VALUE_ZEROS.
______*01__WRK-DINIC-INV_______________PIC__9(008)_________VALUE_ZEROS.

       01  WRK-TIMESTAMP-AUX.
           05  WRK-TMP-ANO             PIC  9(004)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-TMP-MES             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-TMP-DIA             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-TMP-HOR             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-TMP-MIN             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-TMP-SEG             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-TMP-MIL             PIC  9(006)         VALUE ZEROS.

       01  WRK-TIMESTAMP-AUX-R         REDEFINES WRK-TIMESTAMP-AUX
                                       PIC  X(026).

       01  WRK-DINIC-VGCIA-TARIF       PIC  X(010)         VALUE SPACES.
______*01__WRK-DFIM-VGCIA-TARIF________PIC__X(010)_________VALUE_SPACES.

______*01__WRK-DATA-FIM-TESTE.
______*____05__WRK-ANO-FIMT_____________PIC__9(004).
______*____05__WRK-MES-FIMT_____________PIC__9(002).
______*____05__WRK-DIA-FIMT_____________PIC__9(002).

______*01__WRK-DATA-INIC-TESTE.
______*____05__WRK-ANO-INIT_____________PIC__9(004).
______*____05__WRK-MES-INIT_____________PIC__9(002).
______*____05__WRK-DIA-INIT_____________PIC__9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(018)         VALUE
           'AREA PARA POOL1640'.
      *----------------------------------------------------------------*

______*01__1640-CAMPOS.
______*____02__1640-DATA-ENTRADA____PIC_X(10)__VALUE_SPACES.
______*____02__1640-FORMATO-ENTRADA_PIC_9(2)___VALUE_ZEROS.
______*____02__1640-TIPO-INCREMENTO_PIC_X(2)___VALUE_SPACES.
______*____02__1640-INCREMENTO______PIC_9(3)___VALUE_ZEROS.
______*____02__1640-FORMATO-SAIDA___PIC_9(02)__VALUE_ZEROS.
______*____02__1640-DATA-SAIDA______PIC_X(10)__VALUE_SPACES.
______*____02__1640-MENSAGEM________PIC_X(80)__VALUE_SPACES.

______*01__WRK-DATA-DB2-1640.
______*____05__WRK-DIA-DB2-1640________PIC__9(002).
______*____05__FILLER__________________PIC__X(001)_VALUE_'.'.
______*____05__WRK-MES-DB2-1640________PIC__9(002).
______*____05__FILLER__________________PIC__X(001)_VALUE_'.'.
______*____05__WRK-ANO-DB2-1640________PIC__9(004).

______*01__WRK-NUMERO-DIAS_____________PIC__S9(005)_COMP-3_VALUE_ZEROS.

______*01__WRK-DATA-SAIDA1640.
______*____05__WRK-DIA-SAIDA1640_______PIC__9(002)_VALUE_ZEROS.
______*____05__FILLER__________________PIC__X(001)_VALUE_'.'.
______*____05__WRK-MES-SAIDA1640_______PIC__9(002)_VALUE_ZEROS.
______*____05__FILLER__________________PIC__X(001)_VALUE_'.'.
______*____05__WRK-ANO-SAIDA1640_______PIC__9(004)_VALUE_ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(026)         VALUE
           'AREA DE ERRO - FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-POOL7100.
           05  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
               ' - FILE-STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DA POOL7600'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05  WRK-DATA-JULIANA        PIC  9(005) COMP-3  VALUE ZEROS.
           05  WRK-DATA-AAMMDD         PIC  9(007) COMP-3  VALUE ZEROS.
           05  WRK-DATA-AAAAMMDD       PIC  9(009) COMP-3  VALUE ZEROS.
           05  WRK-HORA-HHMMSS         PIC  9(007) COMP-3  VALUE ZEROS.
           05  WRK-HORA-HHMMSSMMMMMM   PIC  9(013) COMP-3  VALUE ZEROS.
           05  WRK-TIMESTAMP.
             10  WRK-ANO-TM            PIC  9(004)         VALUE ZEROS.
             10  WRK-MES-TM            PIC  9(002)         VALUE ZEROS.
             10  WRK-DIA-TM            PIC  9(002)         VALUE ZEROS.
             10  WRK-HOR               PIC  9(002)         VALUE ZEROS.
             10  WRK-MIN               PIC  9(002)         VALUE ZEROS.
             10  WRK-SEG               PIC  9(002)         VALUE ZEROS.
             10  WRK-MIL               PIC  9(006)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5527 *'.
      *----------------------------------------------------------------*

       01  WRK-5527-AREA-ENTRADA.
           05  WRK-5527-AMBIENTE           PIC  X(001) VALUE SPACE.
           05  WRK-5527-CHAMADOR           PIC  X(008) VALUE SPACE.
           05  WRK-5527-AGENCIA-CLIENTE    PIC  9(005) VALUE ZERO.
           05  WRK-5527-CONTA-CLIENTE      PIC  9(013) VALUE ZERO.

       01  WRK-5527-AREA-SAIDA.
           05  WRK-5527-COD-RETORNO        PIC  9(002) VALUE ZERO.
           05  WRK-5527-COD-SQL-ERRO       PIC S9(003) VALUE ZERO.
           05  WRK-5527-COD-MSG-GFCT       PIC  9(004) VALUE ZERO.
           05  WRK-5527-DESC-MSG           PIC  X(070) VALUE SPACE.
           05  WRK-5527-QTD-OCORR          PIC  9(005) VALUE ZERO.
           05  WRK-5527-DADOS-RETORNO.
ST25X6*     10  WRK-5527-CCGC-CPF         PIC  9(009) VALUE ZERO.
ST25X6*     10  WRK-5527-CFLIAL-CGC       PIC  9(004) VALUE ZERO.
ST25X6*     10  WRK-5527-CCTRL-CPF-CGC    PIC  9(002) VALUE ZERO.
ST25X6      10  WRK-5527-CCGC-CPF         PIC  X(009) VALUE SPACES.
ST25X6      10  WRK-5527-CFLIAL-CGC       PIC  X(004) VALUE SPACES.
ST25X6      10  WRK-5527-CCTRL-CPF-CGC    PIC  9(002) VALUE ZERO.
             10  WRK-5527-CPOSTO-SERVC     PIC  9(003) VALUE ZERO.
             10  WRK-5527-CSGMTO-CLI       PIC  9(003) VALUE ZERO.

       01  WRK-IO-PCB.
           03  WRK-IO-TERM              PIC  X(008) VALUE SPACE.
           03  FILLER                   PIC  X(002) VALUE SPACE.
           03  WRK-IO-STA               PIC  X(002) VALUE SPACE.
           03  FILLER                   PIC  X(012) VALUE SPACE.
           03  WRK-IO-MODNAME           PIC  X(008) VALUE SPACE.

       01  WRK-ALT-PCB.
           03  WRK-ALT-PCBNAME          PIC  X(008) VALUE SPACE.
           03  FILLER                   PIC  X(002) VALUE SPACE.
           03  WRK-ALT-STATUS           PIC  X(002) VALUE SPACE.
           03  FILLER                   PIC  X(012) VALUE SPACE.
           03  WRK-ALT-MODNAME          PIC  X(008) VALUE SPACE.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM SCCE9100 *'.
      *----------------------------------------------------------------*

       COPY 'I#SCCE91'.

______*----------------------------------------------------------------*
______*01__FILLER______________________PIC__X(050)_________VALUE
______*____'*_AREA_DE_COMUNICACAO_COM_GFCT5534_*'.
______*----------------------------------------------------------------*

______*COPY_'I#GFCTJM'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5564 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTRK'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA PARA POOL7100'.
      *----------------------------------------------------------------*

       COPY 'POL7100C'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA PARA ERROS'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT0M'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TABELA DB2'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB009
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C6
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C7
           END-EXEC.

______*____EXEC_SQL
______*______INCLUDE_GFCTB0C8
______*____END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C9
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D3
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0I0
           END-EXEC.

           EXEC SQL
             INCLUDE CLIEV004
           END-EXEC.

           EXEC SQL
             INCLUDE CLIEV007
           END-EXEC.

           EXEC SQL
             INCLUDE CLIEV008
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0F0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H3
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H5
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0N7
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0M4
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0M2
           END-EXEC.

           EXEC SQL
             DECLARE CSR01-GFCTB0M2 CURSOR  FOR
             SELECT CAG_BCRIA,
                    CCTA_CLI,
                    HINCL_REG
             FROM
                    DB2PRD.TADSAO_INDVD_CLULR
             WHERE
                    CDDD_FONE_MOVEL = :GFCTB0M2.CDDD-FONE-MOVEL AND
                    NLIN_TFONI      = :GFCTB0M2.NLIN-TFONI
              ORDER BY HINCL_REG DESC
           END-EXEC.


           EXEC SQL
             DECLARE CSR01-GFCTB0H3 CURSOR FOR
               SELECT CSGMTO_GSTAO_TARIF
               FROM   DB2PRD.TASSOC_SGMTO_CLI
               WHERE  DINIC_VGCIA_ASSOC  <= :GFCTB0H3.DINIC-VGCIA-ASSOC
                 AND  DFIM_VGCIA_ASSOC   >= :GFCTB0H3.DFIM-VGCIA-ASSOC
                 AND  CSGMTO_INIC_CLI    <= :GFCTB0H3.CSGMTO-INIC-CLI
                 AND  CSGMTO_FNAL_CLI    >= :GFCTB0H3.CSGMTO-FNAL-CLI
           END-EXEC.

           EXEC SQL
             DECLARE CSR02-GFCTB0D2 CURSOR FOR
               SELECT
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA
             FROM  DB2PRD.PRMSS_GRP_SGMTO
             WHERE CSERVC_TARIF       = :GFCTB0D2.CSERVC-TARIF
             AND   CAGPTO_CTA         = :GFCTB0D2.CAGPTO-CTA
             AND   CSGMTO_GSTAO_TARIF = :GFCTB0D2.CSGMTO-GSTAO-TARIF
           END-EXEC.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01 LNK-AREA-PARM.
          05  LNK-TAMA-PARM           PIC S9(004) COMP.
          05  LNK-AT-BASE             PIC  X(001).
      *
      *  PARM QUE INDICA SE ATUALIZA OU NAO A BASE N8 (S/N - DEF. 'S')
      *

      *================================================================*
       PROCEDURE                       DIVISION USING LNK-AREA-PARM.
      *================================================================*

           IF LNK-AT-BASE = 'N'
              MOVE 'N' TO WRK-AT-BASE
           ELSE
              MOVE 'S' TO WRK-AT-BASE
           END-IF.

           MOVE '+' TO
               WRK-CAMPO-TIRA-SINAL-S
               WRK-CAMPO-POE-SINAL-01-S
               WRK-CAMPO-POE-SINAL-02-S
               WRK-CAMPO-POE-SINAL-03-S
               WRK-CAMPO-POE-SINAL-04-S
               WRK-CAMPO-POE-SINAL-05-S
               WRK-CAMPO-POE-SINAL-06-S
               WRK-CAMPO-POE-SINAL-07-S
               WRK-CAMPO-POE-SINAL-08-S
               WRK-CAMPO-POE-SINAL-09-S
               WRK-CAMPO-POE-SINAL-10-S
               WRK-CAMPO-POE-SINAL-11-S
               WRK-CAMPO-POE-SINAL-12-S
               WRK-CAMPO-POE-SINAL-13-S
               WRK-CAMPO-POE-SINAL-14-S
               WRK-CAMPO-POE-SINAL-15-S
               WRK-CAMPO-POE-SINAL-16-S
               WRK-CAMPO-POE-SINAL-17-S.

           MOVE ZEROS TO
               WRK-CAMPO-TIRA-SINAL
               WRK-CAMPO-TIRA-SINAL-17
               WRK-CAMPO-TIRA-SINAL-16
               WRK-CAMPO-TIRA-SINAL-15
               WRK-CAMPO-TIRA-SINAL-14
               WRK-CAMPO-TIRA-SINAL-13
               WRK-CAMPO-TIRA-SINAL-12
               WRK-CAMPO-TIRA-SINAL-11
               WRK-CAMPO-TIRA-SINAL-10
               WRK-CAMPO-TIRA-SINAL-09
               WRK-CAMPO-TIRA-SINAL-08
               WRK-CAMPO-TIRA-SINAL-07
               WRK-CAMPO-TIRA-SINAL-06
               WRK-CAMPO-TIRA-SINAL-05
               WRK-CAMPO-TIRA-SINAL-04
               WRK-CAMPO-TIRA-SINAL-03
               WRK-CAMPO-TIRA-SINAL-02
               WRK-CAMPO-TIRA-SINAL-01
               WRK-CAMPO-POE-SINAL-01
               WRK-CAMPO-POE-SINAL-01-C
               WRK-CAMPO-POE-SINAL-02
               WRK-CAMPO-POE-SINAL-02-C
               WRK-CAMPO-POE-SINAL-03
               WRK-CAMPO-POE-SINAL-03-C
               WRK-CAMPO-POE-SINAL-04
               WRK-CAMPO-POE-SINAL-04-C
               WRK-CAMPO-POE-SINAL-05
               WRK-CAMPO-POE-SINAL-05-C
               WRK-CAMPO-POE-SINAL-06
               WRK-CAMPO-POE-SINAL-06-C
               WRK-CAMPO-POE-SINAL-07
               WRK-CAMPO-POE-SINAL-07-C
               WRK-CAMPO-POE-SINAL-08
               WRK-CAMPO-POE-SINAL-08-C
               WRK-CAMPO-POE-SINAL-09
               WRK-CAMPO-POE-SINAL-09-C
               WRK-CAMPO-POE-SINAL-10
               WRK-CAMPO-POE-SINAL-10-C
               WRK-CAMPO-POE-SINAL-11
               WRK-CAMPO-POE-SINAL-11-C
               WRK-CAMPO-POE-SINAL-12
               WRK-CAMPO-POE-SINAL-12-C
               WRK-CAMPO-POE-SINAL-13
               WRK-CAMPO-POE-SINAL-13-C
               WRK-CAMPO-POE-SINAL-14
               WRK-CAMPO-POE-SINAL-14-C
               WRK-CAMPO-POE-SINAL-15
               WRK-CAMPO-POE-SINAL-15-C
               WRK-CAMPO-POE-SINAL-16
               WRK-CAMPO-POE-SINAL-16-C
               WRK-CAMPO-POE-SINAL-17
               WRK-CAMPO-POE-SINAL-17-C.

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           MOVE WRK-ANO-TM             TO WRK-TMP-ANO.
           MOVE WRK-MES-TM             TO WRK-TMP-MES.
           MOVE WRK-DIA-TM             TO WRK-TMP-DIA.
           MOVE WRK-HOR                TO WRK-TMP-HOR.
           MOVE WRK-MIN                TO WRK-TMP-MIN.
           MOVE WRK-SEG                TO WRK-TMP-SEG.
           MOVE WRK-MIL                TO WRK-TMP-MIL.


      ******************************************************************
      *    CHAMA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO.               *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
               WRK-FS-ENTRADA          EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS INICIAIS DO PROGRAMA                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT  ENTRADA
                OUTPUT SAIDA
                       CONTROLE
                       REJEITAD.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA TESTE DE FILE-STATUS.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-ENTRADA.

           PERFORM 1120-TESTAR-FS-REJEITAD.

           PERFORM 1130-TESTAR-FS-SAIDA.

           PERFORM 1140-TESTAR-FS-CONTROLE.


      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO ENTRADA.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ENTRADA          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENTRADA          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ENTRADA'          TO WRK-NOME-ARQ
               MOVE WRK-FS-ENTRADA     TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO REJEITAD                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-REJEITAD          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-REJEITAD         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'REJEITAD'         TO WRK-NOME-ARQ
               MOVE WRK-FS-REJEITAD    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO SAIDA  .                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-SAIDA          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAIDA            NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'SAIDA'            TO WRK-NOME-ARQ
               MOVE WRK-FS-SAIDA       TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO CONTROLE                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1140-TESTAR-FS-CONTROLE       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CONTROLE         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'CONTROLE'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CONTROLE    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      *    VERIFICAR SE ARQUIVO ENTRADA ESTA VAZIO                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ENTRADA.

           IF  WRK-FS-ENTRADA EQUAL '10'
               DISPLAY ' '
               DISPLAY '=======GFCT3610========='
               DISPLAY 'ARQUIVO DE ENTRADA VAZIO'
               DISPLAY '=======GFCT3610========='
               DISPLAY ' '
               MOVE    'ARQUIVO DE ENTRADA VAZIO' TO WRK-MSG
               MOVE 8 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           ELSE
              PERFORM 2200-ACESSAR-DPROCM
              PERFORM 2220-VERIFICA-DISP-SISTEMA
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA LEITURA E TRATAMENTO DO ARQUIVO DE ENTRADA*
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-ENTRADA                SECTION.
      *----------------------------------------------------------------*

           READ ENTRADA.

           IF  WRK-FS-ENTRADA              EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA                TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-ENTRADA.

           ADD 1                           TO ACU-LIDOS-ENTRADA.

______*____MOVE_BC0-AGENCIA_OF_ENTRADA_______TO_WRK-AGENCIA-ATU.
______*____MOVE_BC0-CONTA_OF_ENTRADA________TO_WRK-CONTA-ATU.
           IF BC0-CESTA OF ENTRADA NUMERIC AND
              BC0-CESTA-R OF ENTRADA NOT EQUAL ZEROS
              IF BC0-CENTRO-CUSTO-R OF ENTRADA NOT EQUAL 'GFCT' AND
                 BC0-CENTRO-CUSTO-R OF ENTRADA NOT EQUAL SPACE
                 IF BC0-CENTRO-CUSTO-R OF ENTRADA
                    EQUAL WRK-CENTRO-CUSTO AND
                    BC0-CESTA-R OF ENTRADA EQUAL WRK-CESTA
                    MOVE CSERVC-TARIF OF GFCTB0F0 TO
                    WRK-CAMPO-TIRA-SINAL
                    MOVE WRK-CAMPO-TIRA-SINAL-05 TO
                         BC0-CESTA-R OF ENTRADA
                 ELSE
                    MOVE BC0-CENTRO-CUSTO-R OF ENTRADA
                    TO WRK-CENTRO-CUSTO
                    MOVE BC0-CESTA-R OF ENTRADA TO WRK-CESTA
                    PERFORM 2101-PESQ-CORRELATA
                    MOVE CSERVC-TARIF OF GFCTB0F0 TO
                    WRK-CAMPO-TIRA-SINAL
                    MOVE WRK-CAMPO-TIRA-SINAL-05 TO
                         BC0-CESTA-R OF ENTRADA
                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PESQUISA TARIFA CORRELATA                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2101-PESQ-CORRELATA SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CENTRO-CUSTO TO CSIST-CORRL OF GFCTB0F0.
           MOVE WRK-CESTA        TO CSERVC-TARIF-CORRL OF GFCTB0F0.


           EXEC SQL
               SELECT  CSERVC_TARIF
               INTO   :GFCTB0F0.CSERVC-TARIF
               FROM    DB2PRD.TARIF_CORRL_SIST
               WHERE   CSIST_CORRL = :GFCTB0F0.CSIST-CORRL
               AND     CSERVC_TARIF_CORRL =
                      :GFCTB0F0.CSERVC-TARIF-CORRL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
              IF WRK-COUNT-DISPLAY LESS 1000
                 ADD 1 TO WRK-COUNT-DISPLAY
                 DISPLAY 'ERRO ACESSO CORRELATA ' WRK-CENTRO-CUSTO ' '
                          WRK-CESTA
                 MOVE WRK-CESTA TO CSERVC-TARIF OF GFCTB0F0
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2101-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE LE TABELA GFCTBA1 E BUSCA DATA PROCM             *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-ACESSAR-DPROCM             SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ANTER,
                   DPROCM_ATUAL,
                   DPROCM_PROX
             INTO
                   :GFCTB0A1.DPROCM-ANTER,
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'PARM_DATA_PROCM' TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE  SQLCODE          TO ERR-SQL-CODE
                MOVE '0010'            TO ERR-LOCAL
                MOVE  SPACES           TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE DPROCM-ATUAL OF GFCTB0A1 TO WRK-DATA-DMAB.
           MOVE WRK-DATA-DMAB-M          TO WRK-MES-CORR.

           MOVE DPROCM-PROX OF GFCTB0A1 TO WRK-DATA-DMAB.
           MOVE WRK-DATA-DMAB-D          TO WRK-DATA-AMD-D.
           MOVE WRK-DATA-DMAB-M          TO WRK-DATA-AMD-M
                                            WRK-MES-PROX.
           MOVE WRK-DATA-DMAB-A          TO WRK-DATA-AMD-A.
           MOVE WRK-DATA-AMD-R           TO WRK-DATA-DIA-PK.

           MOVE 1                        TO WRK-DATA-AMD-D.

           MOVE WRK-DATA-AMD-R           TO WRK-DATA-INM-PK.


           IF WRK-MES-CORR EQUAL WRK-MES-PROX
              IF  WRK-DATA-AMD-M LESS 12
                  MOVE WRK-DATA-AMD-M TO WRK-NUM-PK-2
                  ADD  1 TO WRK-NUM-PK-2
                  MOVE WRK-NUM-PK-2 TO WRK-DATA-AMD-M
              ELSE
                  MOVE 1 TO WRK-DATA-AMD-M
                  MOVE WRK-DATA-AMD-A TO WRK-NUM-PK-4
                  ADD  1 TO WRK-NUM-PK-4
                  MOVE WRK-NUM-PK-4 TO WRK-DATA-AMD-A
              END-IF
           END-IF.

           MOVE WRK-DATA-AMD-R           TO WRK-DATA-PRM-PK.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA DISP. SISTEMA                                      *
      ******************************************************************
       2220-VERIFICA-DISP-SISTEMA SECTION.

           MOVE DPROCM-ATUAL OF GFCTB0A1 TO
                DINIC-VGCIA-GSTOR OF GFCTB0A0.

           EXEC SQL
              SELECT MAX(DINIC_VGCIA_GSTOR)
              INTO :GFCTB0A0.DINIC-VGCIA-GSTOR
              FROM DB2PRD.PARM_CTRL_GSTAO
              WHERE DINIC_VGCIA_GSTOR <= :GFCTB0A0.DINIC-VGCIA-GSTOR
              AND CDEPDC_GSTOR = 4220
           END-EXEC.

           IF (SQLCODE         NOT EQUAL ZEROS AND +100 AND -305) OR
              (SQLWARN0                EQUAL 'W')
               DISPLAY 'ERRO ACESSO BASE GFCTB0A0'
               MOVE    'ERRO ACESSO BASE GFCTB0A0' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF SQLCODE          EQUAL +100 OR -305
              DISPLAY 'CONTROLE DA BASE GFCTB0A0 NAO ENCONTRADO'
              MOVE    'CONTROLE DA BASE GFCTB0A0 NAO ENCONTRADO' TO
                       WRK-MSG
              MOVE 12 TO RETURN-CODE
              PERFORM 3456-ATUALIZA-CONTERRO
              CLOSE ENTRADA SAIDA REJEITAD CONTROLE
              STOP RUN
           END-IF.

           MOVE DPROCM-ATUAL OF GFCTB0A1 TO
                DFIM-VGCIA-GSTOR OF GFCTB0A0.

           EXEC SQL
             SELECT CSIT_ONLINE
             INTO :GFCTB0A0.CSIT-ONLINE
             FROM DB2PRD.PARM_CTRL_GSTAO
             WHERE DINIC_VGCIA_GSTOR = :GFCTB0A0.DINIC-VGCIA-GSTOR
             AND CDEPDC_GSTOR = 4220 AND
             DFIM_VGCIA_GSTOR >= :GFCTB0A0.DFIM-VGCIA-GSTOR
           END-EXEC.

           IF (SQLCODE         NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               DISPLAY 'ERRO ACESS. BASE GFCTB0A0'
               MOVE    'ERRO ACESS. BASE GFCTB0A0' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF SQLCODE          EQUAL +100
              DISPLAY 'CONTR. DA BASE GFCTB0A0 NAO ENCONTRADO'
              MOVE    'CONTR. DA BASE GFCTB0A0 NAO ENCONTRADO' TO
                       WRK-MSG
              MOVE 12 TO RETURN-CODE
              PERFORM 3456-ATUALIZA-CONTERRO
              CLOSE ENTRADA SAIDA REJEITAD CONTROLE
              STOP RUN
           END-IF.

           IF CSIT-ONLINE OF GFCTB0A0 = 0
              NEXT SENTENCE
           ELSE
              DISPLAY 'SISTEMA NAO LIBERADO. ONLINE FECHADO'
              MOVE    'SISTEMA NAO LIBERADO. ONLINE FECHADO' TO
                       WRK-MSG
              MOVE 8 TO RETURN-CODE
              PERFORM 3456-ATUALIZA-CONTERRO
              CLOSE ENTRADA SAIDA REJEITAD CONTROLE
              STOP RUN
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                   TO WRK-ERRO.
           MOVE SPACES                TO WRK-MSG.

           IF (BC0-AGENCIA OF ENTRADA NUMERIC              AND
               BC0-AGENCIA-R OF ENTRADA GREATER ZEROS)     AND
              (BC0-CONTA   OF ENTRADA NUMERIC              AND
               BC0-CONTA   OF ENTRADA GREATER ZEROS)       AND
              (BC0-CESTA   OF ENTRADA NUMERIC              AND
               BC0-CESTA   OF ENTRADA GREATER ZEROS)       AND
              (BC0-DT-INIC OF ENTRADA NUMERIC              AND
               BC0-DT-INIC-R-D OF ENTRADA   GREATER ZEROS  AND
               BC0-DT-INIC-R-M OF ENTRADA   GREATER ZEROS  AND
               BC0-DT-INIC-R-A OF ENTRADA   GREATER ZEROS) AND
              (BC0-DT-FIM OF ENTRADA NUMERIC               AND
               BC0-DT-FIM-R-D OF ENTRADA GREATER ZEROS     AND
               BC0-DT-FIM-R-M OF ENTRADA GREATER ZEROS     AND
               BC0-DT-FIM-R-A OF ENTRADA GREATER ZEROS)    AND
              (BC0-CENTRO-CUSTO OF ENTRADA NOT = SPACES)   AND
               BC0-OPERADORA OF ENTRADA    NUMERIC         AND
               BC0-OPERADORA-R OF ENTRADA  GREATER ZEROS   AND
               BC0-DDD OF ENTRADA          NUMERIC         AND
               BC0-DDD-R OF ENTRADA  GREATER ZEROS         AND
               BC0-NUM-CEL OF ENTRADA      NUMERIC         AND
               BC0-NUM-CEL-R OF ENTRADA  GREATER ZEROS
              NEXT SENTENCE
           ELSE
              MOVE 'S'                   TO WRK-ERRO
              MOVE 'CAMPOS DE ENTRADA INCONSISTENTES'
                 TO WRK-MSG
           END-IF.

           IF WRK-ERRO EQUAL 'N'
              MOVE BC0-DT-INIC-R-D OF ENTRADA TO WRK-DATA-AMD-D
              MOVE BC0-DT-INIC-R-M OF ENTRADA TO WRK-DATA-AMD-M
              MOVE BC0-DT-INIC-R-A OF ENTRADA TO WRK-DATA-AMD-A
              MOVE WRK-DATA-AMD-R  TO WRK-DATA-IVG-PK
              MOVE WRK-DATA-IVG-PK TO WRK-DT-ENVIADA
              MOVE SPACES          TO WRK-OPCAO
              CALL  'POOL1205'     USING WRK-AREA-POOL1205
                                         WRK-MENSAGEM-1205
              IF  RETURN-CODE          EQUAL ZEROS
                  MOVE WRK-DIA-UT-ANTER TO WRK-DATA-DMA-R
                  IF BC0-DT-INIC-R-M OF ENTRADA NOT EQUAL
                     WRK-DATA-DMA-M
                     NEXT SENTENCE
                  ELSE
                     MOVE 'S'                   TO WRK-ERRO
                     MOVE 'DATA INICIAL TEM QUE SER INICIO DO MES'
                     TO WRK-MSG
                  END-IF
              ELSE
                 IF  RETURN-CODE          EQUAL 4
                     MOVE 'S'                   TO WRK-ERRO
                     MOVE 'PARAMETROS INVALIDOS DT-INI-VIG NA POOL1205'
                     TO WRK-MSG
                 ELSE
                     MOVE 'S'                   TO WRK-ERRO
                     MOVE 'DATA INICIO DE VIGENCIA INVALIDA'
                     TO WRK-MSG
                 END-IF
              END-IF
           END-IF.


           IF WRK-ERRO EQUAL 'N'
              MOVE BC0-DT-FIM-R-D OF ENTRADA TO WRK-DATA-AMD-D
              MOVE BC0-DT-FIM-R-M OF ENTRADA TO WRK-DATA-AMD-M
              MOVE BC0-DT-FIM-R-A OF ENTRADA TO WRK-DATA-AMD-A
              MOVE WRK-DATA-AMD-R  TO WRK-DATA-FVG-PK
              MOVE WRK-DATA-FVG-PK TO WRK-DT-ENVIADA
              MOVE SPACES          TO WRK-OPCAO
              CALL  'POOL1205'     USING WRK-AREA-POOL1205
                                         WRK-MENSAGEM-1205
              IF  RETURN-CODE          EQUAL ZEROS
                  MOVE WRK-DIA-UT-POSTE TO WRK-DATA-DMA-R
                  IF BC0-DT-FIM-R-M OF ENTRADA NOT EQUAL
                     WRK-DATA-DMA-M
                     NEXT SENTENCE
                  ELSE
                     MOVE 'S'                   TO WRK-ERRO
                     MOVE 'DATA FINAL TEM QUE SER FINAL DO MES'
                     TO WRK-MSG
                  END-IF
              ELSE
                 IF  RETURN-CODE          EQUAL 4
                     MOVE 'S'                   TO WRK-ERRO
                     MOVE 'PARAMETROS INVALIDOS DT-FIM-VIG NA POOL1205'
                     TO WRK-MSG
                 ELSE
                     MOVE 'S'                   TO WRK-ERRO
                     MOVE 'DATA FIM DE VIGENCIA INVALIDA'
                     TO WRK-MSG
                 END-IF
              END-IF
           END-IF.

           IF WRK-ERRO EQUAL 'N'
              IF WRK-DATA-IVG-PK NOT LESS WRK-DATA-FVG-PK
                 MOVE 'S'                   TO WRK-ERRO
                 MOVE 'DATA FIM VIG. TEM QUE SER MAIOR QUE INIC. VIG.'
                 TO WRK-MSG
              END-IF
           END-IF.

           IF WRK-ERRO EQUAL 'N'
              IF WRK-DATA-IVG-PK LESS WRK-DATA-PRM-PK
                 MOVE 'S'                   TO WRK-ERRO
                 MOVE 'DATA INI VIG. MENOR QUE DATA GFCT'
                 TO WRK-MSG
              END-IF
           END-IF.


           IF WRK-ERRO EQUAL 'N'
              MOVE '  .  .    '               TO WRK-DATA-DMAB
              MOVE BC0-DT-INIC-R-D OF ENTRADA TO WRK-DATA-DMAB-D
              MOVE BC0-DT-INIC-R-M OF ENTRADA TO WRK-DATA-DMAB-M
              MOVE BC0-DT-INIC-R-A OF ENTRADA TO WRK-DATA-DMAB-A
              MOVE WRK-DATA-DMAB   TO WRK-DATA-INIC-ADESAO
              IF CSERVC-TARIF OF GFCTB0A2 NOT NUMERIC
                 MOVE WRK-DATA-DMAB       TO  WRK-DINIC-VGCIA-TARIF
                 PERFORM 3160-OBTER-DADOS-GFCTB0A2
              ELSE
                 MOVE CSERVC-TARIF OF GFCTB0A2 TO
                         WRK-CAMPO-TIRA-SINAL
                 IF (WRK-CAMPO-TIRA-SINAL-05 EQUAL
                     BC0-CESTA-R OF ENTRADA)  AND
                    (WRK-DINIC-VGCIA-TARIF    EQUAL WRK-DATA-DMAB)
                    NEXT SENTENCE
                 ELSE
                    MOVE WRK-DATA-DMAB       TO  WRK-DINIC-VGCIA-TARIF
                    PERFORM 3160-OBTER-DADOS-GFCTB0A2
                 END-IF
              END-IF
           END-IF.

           IF WRK-ERRO = 'N'
              IF WRK-CESTA-CELULAR = 'N'
                 MOVE 'S'                   TO WRK-ERRO
                 MOVE 'PERMITIDO APENAS CESTA TIPO CELULAR'
                 TO WRK-MSG
              ELSE
                 IF WRK-CESTA-CELULAR = 'E'
                    MOVE 'S'                   TO WRK-ERRO
                    MOVE 'CESTA NAO ENCONTRADA OU NAO EXISTE VIGENCIA'
                    TO WRK-MSG
                 END-IF
              END-IF
           END-IF.

           IF WRK-ERRO = 'N'
              MOVE BC0-AGENCIA-R OF ENTRADA   TO LKW-CJUNC-DEPC
              MOVE BC0-CONTA-R   OF ENTRADA   TO LKW-CTA-CORR
              MOVE 'SCCE9100'             TO WRK-MODULO
              CALL WRK-MODULO             USING LKW-AREA
              IF RETURN-CODE             NOT EQUAL ZEROS
                 MOVE 'S'                   TO WRK-ERRO
                 MOVE 'ERRO ACESSO MODULO SCCE9100'
                 TO WRK-MSG
              ELSE
                 IF LKW-CD-RETORNO          NOT EQUAL ZEROS
                    MOVE 'S'                   TO WRK-ERRO
                    MOVE 'CONTA COM PROBLEMAS DE CADASTRAMENTO CLIE'
                    TO WRK-MSG
                 END-IF
              END-IF
           END-IF.

           IF WRK-ERRO = 'N'
              PERFORM 3200-CONSISTE-AG-CTA
           END-IF.

           IF WRK-ERRO = 'N'
              PERFORM 3300-OBTER-DADOS-GFCTB0D8
           END-IF.

           IF WRK-ERRO = 'N'
              MOVE 009            TO WRK-PROX-AGRUPAMENTO
              MOVE 'N'            TO WRK-FIM-AGRUPAMENTO
              MOVE 'N'            TO WRK-AGRUPAMENTO-VALIDO
              PERFORM 3400-IDENTIFICAR-PERMISSAO UNTIL
                      WRK-FIM-AGRUPAMENTO        EQUAL 'S' OR
                      WRK-AGRUPAMENTO-VALIDO     EQUAL 'S'
              IF WRK-AGRUPAMENTO-VALIDO     EQUAL 'S'
                 NEXT SENTENCE
              ELSE
                 MOVE 'S'                   TO WRK-ERRO
                 MOVE 'NAO EXISTE PERMISSAO PARA ADESAO'
                 TO WRK-MSG
              END-IF
           END-IF.

           IF WRK-ERRO = 'N'
              PERFORM 5000-PERMISSAO-ADESAO-CEL
           END-IF.

           IF WRK-ERRO = 'N'
              PERFORM 5010-CONSISTE-OPERADORA
           END-IF.

           IF WRK-ERRO = 'N'
              PERFORM 5020-VERIFICA-CEL-ADERIDO
           END-IF.


           IF WRK-ERRO = 'S'
              MOVE GFCTWBC0-REG OF ENTRADA TO
                   GFCTWBC0-REG OF REJEITAD
              MOVE WRK-MSG TO BC0-MENSAGEM OF REJEITAD
              WRITE GFCTWBC0-REG OF REJEITAD
              ADD 1 TO ACU-GRAVADOS-REJEITAD
           ELSE
              NEXT SENTENCE
           END-IF.

           MOVE WRK-TIMESTAMP-AUX TO BC4-TMS.
           MOVE '+' TO WRK-CAMPO-TIRA-SINAL-S.
           MOVE ACU-LIDOS-ENTRADA TO WRK-CAMPO-TIRA-SINAL-17.
           MOVE WRK-CAMPO-TIRA-SINAL-09 TO BC4-SEQ.
           MOVE 5                 TO BC4-TP-OPER.
           MOVE 1                 TO BC4-SIT-OPER.
           MOVE SPACE             TO BC4-RESERVA.
           MOVE GFCTWBC0-REG OF ENTRADA TO
                BC4-REG-MASSV.

           WRITE GFCTWBC4-REG.
           ADD 1 TO ACU-GRAVADOS-SAIDA.


           PERFORM 2100-LER-ENTRADA.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER VIGENCIA DA TARIFA PACOTE                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3160-OBTER-DADOS-GFCTB0A2       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DATA-DMAB   TO DINIC-VGCIA-TARIF OF GFCTB0A2
                                   DFIM-VGCIA-TARIF  OF GFCTB0A2.

           MOVE BC0-CESTA-R OF ENTRADA TO CSERVC-TARIF      OF GFCTB0A2.

           EXEC SQL
             SELECT
                   DINIC_VGCIA_TARIF,
                   DFIM_VGCIA_TARIF,
                   CINDCD_CREDT_CLULR
             INTO
                   :GFCTB0A2.DINIC-VGCIA-TARIF,
                   :GFCTB0A2.DFIM-VGCIA-TARIF,
                   :GFCTB0A2.CINDCD-CREDT-CLULR
             FROM   DB2PRD.PARM_SERVC_TARIF
             WHERE
                   CSERVC_TARIF        = :GFCTB0A2.CSERVC-TARIF      AND
                  (DINIC_VGCIA_TARIF  <= :GFCTB0A2.DINIC-VGCIA-TARIF AND
                   DFIM_VGCIA_TARIF   >= :GFCTB0A2.DFIM-VGCIA-TARIF)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               DISPLAY 'ERRO ACESSO BASE GFCTB0A2'
               MOVE    'ERRO ACESSO BASE GFCTB0A2' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 'E' TO WRK-CESTA-CELULAR
           ELSE
              IF CINDCD-CREDT-CLULR OF GFCTB0A2 = 'S'
                 MOVE 'S' TO WRK-CESTA-CELULAR
              ELSE
                 MOVE 'N' TO WRK-CESTA-CELULAR
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       3160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*



      ******************************************************************
      * CHAMA MODULO SCCE9100 PARA VERIFICAR SE A AGENCIA/CONTA EH     *
      * VALIDA.                                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-CONSISTE-AG-CTA            SECTION.
      *----------------------------------------------------------------*

           MOVE BC0-AGENCIA-R OF ENTRADA TO
                                 CJUNC-DEPDC      OF CLIEV008.
           MOVE BC0-CONTA-R   OF ENTRADA TO
                                 CCTA-CLI         OF CLIEV008.

           EXEC SQL
             SELECT
                   CID_CLI,
                   CSGMTO_CLI
             INTO
                   :CLIEV008.CID-CLI,
                   :CLIEV008.CSGMTO-CLI
             FROM   DB2PRD.V01CTA_CORRENTE
             WHERE
                   CJUNC_DEPDC         = :CLIEV008.CJUNC-DEPDC AND
                   CCTA_CLI            = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
              DISPLAY 'ERRO ACESSO CLIEV008'
              MOVE    'ERRO ACESSO CLIEV008' TO WRK-MSG
              MOVE 12 TO RETURN-CODE
              PERFORM 3456-ATUALIZA-CONTERRO
              CLOSE ENTRADA SAIDA REJEITAD CONTROLE
              STOP RUN
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 3210-ACESSAR-CLIEV007
           ELSE
               MOVE CSGMTO-CLI OF CLIEV008 TO
                    WRK-CSGMTO-CLI
               MOVE 1                  TO WRK-TIPO-CONTA-CLIENTE
               MOVE CID-CLI            OF CLIEV008
                                       TO CID-CLI          OF CLIEV004
               PERFORM 3220-ACESSAR-CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR CLIEV007                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3210-ACESSAR-CLIEV007           SECTION.
      *----------------------------------------------------------------*
           MOVE BC0-AGENCIA-R OF ENTRADA TO
                                 CJUNC-DEPDC      OF CLIEV007.
           MOVE BC0-CONTA-R   OF ENTRADA TO
                                 CCTA-CLI         OF CLIEV007.

           EXEC SQL
             SELECT
                   CID_CLI,
                   CSGMTO_CLI
             INTO
                   :CLIEV007.CID-CLI,
                   :CLIEV007.CSGMTO-CLI
             FROM   DB2PRD.V01CTA_POUPANCA
             WHERE
                   CJUNC_DEPDC        = :CLIEV007.CJUNC-DEPDC AND
                   CCTA_CLI           = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
              IF WRK-COUNT-DISPLAY LESS 1000
                 ADD 1 TO WRK-COUNT-DISPLAY
                 DISPLAY 'ERRO ACESSO CLIEV007'
                 MOVE    'ERRO ACESSO CLIEV007' TO WRK-MSG
                 MOVE 12 TO RETURN-CODE
                 PERFORM 3456-ATUALIZA-CONTERRO
                 CLOSE ENTRADA SAIDA REJEITAD CONTROLE
                 STOP RUN
              END-IF
           END-IF.

           IF  SQLCODE                 EQUAL +100
               IF WRK-COUNT-DISPLAY LESS 1000
                  ADD 1 TO WRK-COUNT-DISPLAY
                  MOVE 'S'                   TO WRK-ERRO
                  MOVE 'CLIENTE NAO CADASTRADO NO CLIE'
                  TO WRK-MSG
               END-IF
           ELSE
               MOVE CSGMTO-CLI OF CLIEV007 TO
                    WRK-CSGMTO-CLI
               MOVE 2                  TO WRK-TIPO-CONTA-CLIENTE
               MOVE CID-CLI            OF CLIEV007
                                       TO CID-CLI          OF CLIEV004
               PERFORM 3220-ACESSAR-CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR CLIEV004 PARA OBTER DADOS DO CLIENTE                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3220-ACESSAR-CLIEV004           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                   CCGC_CPF,
                   CFLIAL_CGC,
                   CCTRL_CPF_CGC,
                   CTPO_PSSOA,
                   IPSSOA_COPLT
             INTO
                   :CLIEV004.CCGC-CPF,
                   :CLIEV004.CFLIAL-CGC,
                   :CLIEV004.CCTRL-CPF-CGC,
                   :CLIEV004.CTPO-PSSOA,
                   :CLIEV004.IPSSOA-COPLT
             FROM   DB2PRD.V01CLIENTE_AGENCIA
             WHERE
                   CID_CLI            = :CLIEV004.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               DISPLAY 'ERRO ACESSO CLIEV004'
               MOVE 12 TO RETURN-CODE
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                   TO WRK-ERRO
               MOVE 'CLIENTE NAO CADASTRADO NO CLIE'
               TO WRK-MSG
           ELSE
               IF  CFLIAL-CGC OF CLIEV004
                                       EQUAL ZEROS
                   MOVE 'F'            TO WRK-TIPO-PESSOA-CLIENTE
               ELSE
                   MOVE 'J'            TO WRK-TIPO-PESSOA-CLIENTE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA TARIFA.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-OBTER-DADOS-GFCTB0D8       SECTION.
      *----------------------------------------------------------------*

           MOVE BC0-CESTA-R OF ENTRADA TO CSERVC-TARIF     OF GFCTB0D8.

           EXEC SQL
             SELECT
                   CTPO_SERVC_TARIF,
                   CPSSOA_SERVC_TARIF,
                   CTPO_CTA_DEB_TARIF
             INTO
                   :GFCTB0D8.CTPO-SERVC-TARIF,
                   :GFCTB0D8.CPSSOA-SERVC-TARIF,
                   :GFCTB0D8.CTPO-CTA-DEB-TARIF
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               DISPLAY 'ERRO ACESSO BASE GFCTB0D8'
               MOVE 12 TO RETURN-CODE
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

BI0115     IF CTPO-SERVC-TARIF OF GFCTB0D8 NOT EQUAL 2  AND  4
              MOVE 'S'                   TO WRK-ERRO
              MOVE 'PERMITIDO ADESAO APENAS PARA CESTAS'
              TO WRK-MSG
           ELSE
              MOVE CTPO-CTA-DEB-TARIF OF GFCTB0D8 TO
                   WRK-CAMPO-TIRA-SINAL
              IF WRK-CAMPO-TIRA-SINAL-01 EQUAL
                                       WRK-TIPO-CONTA-CLIENTE AND
                 CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL '3'    OR
                 (CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL '1'    AND
                  WRK-TIPO-PESSOA-CLIENTE         EQUAL 'F') OR
                 (CPSSOA-SERVC-TARIF  OF GFCTB0D8 EQUAL '2'    AND
                  WRK-TIPO-PESSOA-CLIENTE         EQUAL 'J')
                  NEXT SENTENCE
              ELSE
                 MOVE 'S'                   TO WRK-ERRO
                 MOVE 'TIPO CONTA/CLIENTE NAO COMPATIVEL COM CESTA'
                 TO WRK-MSG
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      * IDENTIFICAR AGRUPAMENTO COM PERMISSAO DE ADESAO AO PACOTE.     *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-IDENTIFICAR-PERMISSAO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-EXISTE-AGRUPAMENTO.

           EVALUATE WRK-PROX-AGRUPAMENTO

             WHEN 004
                  PERFORM 3405-OBTER-CNPJ-CPF-POSTO
                  IF  WRK-5527-CCGC-CPF               NOT EQUAL ZEROS
                      PERFORM 3415-ACESSAR-GFCTB0I0

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3410-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 018         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 009
______*___________PERFORM_3420-OBTER-SEG-GESTAO
                  PERFORM 4200-OBTER-SEG-GESTAO

______*___________IF__GFCTJM-CSGMTO-GSTAO-TARIF_______NOT_EQUAL_ZEROS
______*_______________PERFORM_3425-ACESSAR-GFCTB0D2

______*_______________IF__WRK-EXISTE-AGRUPAMENTO______EQUAL_'S'
______*___________________PERFORM_3410-OBTER-DADOS-GFCTB0C6
______*_______________END-IF
______*___________END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 014         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 014
                  PERFORM 3430-ACESSAR-GFCTB0C7

                  IF  WRK-EXISTE-AGRUPAMENTO          EQUAL 'S'
                      PERFORM 3410-OBTER-DADOS-GFCTB0C6
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 015         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 015
                  PERFORM 3405-OBTER-CNPJ-CPF-POSTO

                  IF  WRK-5527-CPOSTO-SERVC           NOT EQUAL ZEROS
                      PERFORM 3435-ACESSAR-GFCTB0D0

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3410-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 017         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 017
                  IF  WRK-5527-CPOSTO-SERVC           NOT EQUAL ZEROS
                      PERFORM 3440-ACESSAR-GFCTB0D1

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3410-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 020         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 018
                  PERFORM 3442-OBTER-UF-MUNICIPIO

                  IF  GFCTRK-CMUN-RURAL               NOT EQUAL ZEROS
                      PERFORM 3445-ACESSAR-GFCTB0C9

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3410-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 019         TO WRK-PROX-AGRUPAMENTO
                  END-IF

             WHEN 019
                  IF  GFCTRK-CSGL-UF                  NOT EQUAL SPACES
                      PERFORM 3450-ACESSAR-GFCTB0D3

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3410-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 'S'         TO WRK-FIM-AGRUPAMENTO
                  END-IF

             WHEN 020
                  IF  WRK-5527-CPOSTO-SERVC           NOT EQUAL ZEROS
                      PERFORM 3440-ACESSAR-GFCTB0D1

                      IF  WRK-EXISTE-AGRUPAMENTO      EQUAL 'S'
                          PERFORM 3410-OBTER-DADOS-GFCTB0C6
                      END-IF
                  END-IF

                  IF  WRK-AGRUPAMENTO-VALIDO          EQUAL 'N'
                      MOVE 004         TO WRK-PROX-AGRUPAMENTO
                  END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5527 PARA OBTER CPF/CNPJ/POSTO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       3405-OBTER-CNPJ-CPF-POSTO       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO WRK-5527-AREA-SAIDA.

           INITIALIZE WRK-5527-AREA-SAIDA.

           MOVE 'GFCT3610'                 TO WRK-5527-CHAMADOR.
           MOVE BC0-AGENCIA-R OF ENTRADA    TO WRK-5527-AGENCIA-CLIENTE.
           MOVE BC0-CONTA-R   OF ENTRADA    TO WRK-5527-CONTA-CLIENTE.
           MOVE 'B'                        TO WRK-5527-AMBIENTE.

           MOVE 'GFCT5527'                 TO WRK-MODULO.

           CALL WRK-MODULO                 USING WRK-5527-AREA-ENTRADA
                                                 WRK-5527-AREA-SAIDA
                                                 GFCT0M-AREA-ERROS
                                                 WRK-IO-PCB
                                                 WRK-ALT-PCB.

           IF  RETURN-CODE                 NOT EQUAL ZEROS OR
               WRK-5527-COD-RETORNO        NOT EQUAL ZEROS
              IF WRK-COUNT-DISPLAY LESS 1000
                 ADD 1 TO WRK-COUNT-DISPLAY
                 DISPLAY 'ERRO ACESSO MODULO GFCT5527'
                 MOVE ZEROS                  TO WRK-5527-CCGC-CPF
                                                WRK-5527-CFLIAL-CGC
                                                WRK-5527-CCTRL-CPF-CGC
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3405-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA PERMISSAO DE ADESAO AO PACOTE.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3410-OBTER-DADOS-GFCTB0C6       SECTION.
      *----------------------------------------------------------------*

           MOVE BC0-CESTA-R OF ENTRADA TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE WRK-SEQ-AGPTO          TO CSEQ-AGPTO-CTA    OF GFCTB0C6.
           MOVE WRK-DATA-INIC-ADESAO   TO DINIC-PRMSS-PCOTE OF GFCTB0C6.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C6.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CINDCD_AGPTO_TOT
             INTO
                   :GFCTB0C6.CSEQ-AGPTO-CTA,
                   :GFCTB0C6.DINIC-PRMSS-PCOTE,
                   :GFCTB0C6.CINDCD-AGPTO-TOT
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
                   CSERVC_TARIF        = :GFCTB0C6.CSERVC-TARIF
               AND CAGPTO_CTA          = :GFCTB0C6.CAGPTO-CTA
               AND CSEQ_AGPTO_CTA      = :GFCTB0C6.CSEQ-AGPTO-CTA
               AND CINDCD_ADSAO_INDVD  IN ('I', 'A')
               AND DINIC_PRMSS_PCOTE  <= :GFCTB0C6.DINIC-PRMSS-PCOTE
               AND DFIM_PRMSS_PCOTE   >= :GFCTB0C6.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               DISPLAY 'ERRO ACESSO BASE GFCTB0C6'
               MOVE    'ERRO ACESSO BASE GFCTB0C6' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 'S'                TO WRK-AGRUPAMENTO-VALIDO
           END-IF.

      *----------------------------------------------------------------*
       3410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO CLIENTE.*
      ******************************************************************
      *----------------------------------------------------------------*
       3415-ACESSAR-GFCTB0I0           SECTION.
      *----------------------------------------------------------------*

           MOVE BC0-CESTA-R OF ENTRADA TO WRK-CAMPO-POE-SINAL-05-C.
           MOVE '+'                    TO WRK-CAMPO-POE-SINAL-05-S.
           MOVE WRK-CAMPO-POE-SINAL-05 TO
           CSERVC-TARIF      OF GFCTB0I0.
           MOVE WRK-PROX-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0I0.
           MOVE WRK-DATA-INIC-ADESAO   TO DINIC-PRMSS-PCOTE OF GFCTB0I0.
ST25X6*    MOVE WRK-5527-CCGC-CPF      TO CCGC-CPF          OF GFCTB0I0.
ST25X6     MOVE WRK-5527-CCGC-CPF      TO CCGC-CPF-ST       OF GFCTB0I0.
ST25X6*    MOVE WRK-5527-CFLIAL-CGC    TO CFLIAL-CGC        OF GFCTB0I0.
ST25X6     MOVE WRK-5527-CFLIAL-CGC    TO CFLIAL-CGC-ST     OF GFCTB0I0.
ST25X6*    MOVE WRK-5527-CCTRL-CPF-CGC TO CCTRL-CPF-CGC     OF GFCTB0I0.
ST25X6     MOVE WRK-5527-CCTRL-CPF-CGC TO CCTRL-CPF-CGC-ST  OF GFCTB0I0.

           EXEC SQL
                SELECT MAX(DINIC_PRMSS_PCOTE)
                INTO :GFCTB0I0.DINIC-PRMSS-PCOTE
                 FROM   DB2PRD.TPRMSS_GRP_CLI
                 WHERE
                   CSERVC_TARIF       = :GFCTB0I0.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA        AND
ST25X6*               CCGC_CPF           = :GFCTB0I0.CCGC-CPF          AND
ST25X6*               CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC        AND
ST25X6*               CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC     AND
ST25X6             CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST       AND
ST25X6             CFLIAL_CGC_ST      = :GFCTB0I0.CFLIAL-CGC-ST     AND
ST25X6             CCTRL_CPF_CGC_ST   = :GFCTB0I0.CCTRL-CPF-CGC-ST  AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0I0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               DISPLAY 'ERRO ACESSO BASE GFCTB0I0'
               MOVE    'ERRO ACESSO BASE GFCTB0I0' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF  SQLCODE                 EQUAL +100 OR -305
               MOVE '31.12.2090'       TO
               DINIC-PRMSS-PCOTE OF GFCTB0I0
           END-IF.


           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA
             INTO
                   :GFCTB0I0.CSEQ-AGPTO-CTA
             FROM   DB2PRD.TPRMSS_GRP_CLI
             WHERE
                   CSERVC_TARIF       = :GFCTB0I0.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA        AND
ST25X6*               CCGC_CPF           = :GFCTB0I0.CCGC-CPF          AND
ST25X6*               CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC        AND
ST25X6*               CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC     AND
ST25X6             CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST       AND
ST25X6             CFLIAL_CGC_ST      = :GFCTB0I0.CFLIAL-CGC-ST     AND
ST25X6             CCTRL_CPF_CGC_ST   = :GFCTB0I0.CCTRL-CPF-CGC-ST  AND
                   DINIC_PRMSS_PCOTE  = :GFCTB0I0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               DISPLAY 'ERRO ACESSO BASE GFCTB0I0'
               MOVE    'ERRO ACESSO BASE GFCTB0I0' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0I0 TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0I0 TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       3415-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

______******************************************************************
______*_ACESSAR_MODULO_GFCT5534_PARA_OBTER_SEGMENTO_GESTAO.____________*
______******************************************************************
______*----------------------------------------------------------------*
______*3420-OBTER-SEG-GESTAO___________SECTION.
______*----------------------------------------------------------------*

______*____MOVE_SPACES_________________TO_GFCTJM-AREA.

______*____INITIALIZE_GFCTJM-AREA.

______*____MOVE_'B'________________________TO_GFCTJM-TPO-AMBIENTE.
______*____MOVE_BC0-AGENCIA-R_OF_ENTRADA___TO_GFCTJM-CJUNC-DEPDC.
______*____MOVE_BC0-CONTA-R_OF_ENTRADA_____TO_GFCTJM-CCTA-CLI.

______*____MOVE_'GFCT5534'_________________TO_WRK-MODULO.

______*____CALL_WRK-MODULO_________________USING_GFCTJM-AREA
______*__________________________________________GFCT0M-AREA-ERROS.

______*____IF__RETURN-CODE_________________NOT_EQUAL_ZEROS_OR
______*________GFCTJM-COD-RETORNO__________EQUAL_99
______*________IF_WRK-COUNT-DISPLAY_LESS_1000
______*___________ADD_1_TO_WRK-COUNT-DISPLAY
______*___________DISPLAY_'PROBLEMAS_AO_OBTER_O_SEGMENTO_DO_CLIENNTE'
______*________END-IF
______*________MOVE_ZEROS______________TO_GFCTJM-CSGMTO-GSTAO-TARIF
______*____END-IF.

______*----------------------------------------------------------------*
______*3420-99-FIM.____________________EXIT.
______*----------------------------------------------------------------*

______******************************************************************
______*_VERIFICAR_EXISTENCIA_DE_INFORMACOES_DE_PERMISSAO_GRUPO_SEGMENTO*
______******************************************************************
______*----------------------------------------------------------------*
______*3425-ACESSAR-GFCTB0D2___________SECTION.
______*----------------------------------------------------------------*

______*____MOVE_BC0-CESTA-R_OF_ENTRADA_TO_WRK-CAMPO-POE-SINAL-05-C.
______*____MOVE_'+'____________________TO_WRK-CAMPO-POE-SINAL-05-S.
______*____MOVE_WRK-CAMPO-POE-SINAL-05_TO
______*____CSERVC-TARIF______OF_GFCTB0D2.

______*____MOVE_WRK-PROX-AGRUPAMENTO
______*________________________________TO_CAGPTO-CTA________OF_GFCTB0D2.
______*____MOVE_WRK-DATA-INIC-ADESAO___TO_DINIC-PRMSS-PCOTE_OF_GFCTB0D2.
______*____MOVE_GFCTJM-CSGMTO-GSTAO-TARIF
______*________________________________TO_CSGMTO-GSTAO-TARIF
______*_____________________________________________________OF_GFCTB0D2.

______*____EXEC_SQL
______*______SELECT_MAX(DINIC_PRMSS_PCOTE)
______*______INTO
______*____________:GFCTB0D2.DINIC-PRMSS-PCOTE
______*______FROM___DB2PRD.PRMSS_GRP_SGMTO
______*__________WHERE
______*___________CSERVC_TARIF_______=_:GFCTB0D2.CSERVC-TARIF_______AND
______*___________CAGPTO_CTA_________=_:GFCTB0D2.CAGPTO-CTA_________AND
______*___________CSGMTO_GSTAO_TARIF_=_:GFCTB0D2.CSGMTO-GSTAO-TARIF_AND
______*___________DINIC_PRMSS_PCOTE_<=_:GFCTB0D2.DINIC-PRMSS-PCOTE
______*____END-EXEC.

______*____IF_(SQLCODE_________________NOT_EQUAL_ZEROS_AND_+100
______*________________________________________________AND_-305)_OR
______*_______(SQLWARN0________________EQUAL_'W')
______*________IF_WRK-COUNT-DISPLAY_LESS_1000
______*___________ADD_1_TO_WRK-COUNT-DISPLAY
______*___________DISPLAY_'ERRO_ACESSO_GFCTB0D2'
______*___________CLOSE_ENTRADA_SAIDA_REJEITAD_CONTROLE
______*___________STOP_RUN
______*________END-IF
______*____END-IF.

______*____IF__SQLCODE_________________EQUAL_+100_OR_-305
______*________MOVE_'31.12.2099'_TO_DINIC-PRMSS-PCOTE__OF_GFCTB0D2
______*____END-IF.


______*____EXEC_SQL
______*______SELECT
______*____________CSEQ_AGPTO_CTA
______*______INTO
______*____________:GFCTB0D2.CSEQ-AGPTO-CTA
______*______FROM___DB2PRD.PRMSS_GRP_SGMTO
______*______WHERE
______*___________CSERVC_TARIF_______=_:GFCTB0D2.CSERVC-TARIF_______AND
______*___________CAGPTO_CTA_________=_:GFCTB0D2.CAGPTO-CTA_________AND
______*___________CSGMTO_GSTAO_TARIF_=_:GFCTB0D2.CSGMTO-GSTAO-TARIF_AND
______*___________DINIC_PRMSS_PCOTE__=_:GFCTB0D2.DINIC-PRMSS-PCOTE
______*____END-EXEC.

______*____IF_(SQLCODE_________________NOT_EQUAL_ZEROS_AND_+100)_OR
______*_______(SQLWARN0________________EQUAL_'W')
______*________IF_WRK-COUNT-DISPLAY_LESS_1000
______*___________ADD_1_TO_WRK-COUNT-DISPLAY
______*___________DISPLAY_'ERRO_ACESSO_GFCTB0D2'
______*___________CLOSE_ENTRADA_SAIDA_REJEITAD_CONTROLE
______*___________STOP_RUN
______*________END-IF
______*____END-IF.

______*____IF__SQLCODE_________________EQUAL_ZEROS
______*________MOVE_CSEQ-AGPTO-CTA_____OF_GFCTB0D2_TO_WRK-SEQ-AGPTO
______*________MOVE_DINIC-PRMSS-PCOTE__OF_GFCTB0D2_TO_WRK-DT-INICIO
______*________MOVE_'S'________________TO_WRK-EXISTE-AGRUPAMENTO
______*____END-IF.

______*----------------------------------------------------------------*
______*3425-99-FIM.____________________EXIT.
______*----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO AGENCIA.*
      ******************************************************************
      *----------------------------------------------------------------*
       3430-ACESSAR-GFCTB0C7           SECTION.
      *----------------------------------------------------------------*

           MOVE BC0-CESTA-R OF ENTRADA TO WRK-CAMPO-POE-SINAL-05-C.
           MOVE '+'                    TO WRK-CAMPO-POE-SINAL-05-S.
           MOVE WRK-CAMPO-POE-SINAL-05 TO
           CSERVC-TARIF      OF GFCTB0C7.

           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C7.
           MOVE WRK-DATA-INIC-ADESAO   TO DINIC-PRMSS-PCOTE OF GFCTB0C7.

           MOVE BC0-AGENCIA-R OF ENTRADA TO
                CDEPDC            OF GFCTB0C7.


           EXEC SQL
             SELECT MAX(DINIC_PRMSS_PCOTE)
             INTO
                :GFCTB0C7.DINIC-PRMSS-PCOTE
                 FROM   DB2PRD.PRMSS_GRP_DEPDC
                 WHERE
                   CSERVC_TARIF       = :GFCTB0C7.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA        AND
                  (CDEPDC             = 0                           OR
                   CDEPDC             = :GFCTB0C7.CDEPDC)           AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0C7.DINIC-PRMSS-PCOTE
           END-EXEC.


           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -305) OR
              (SQLWARN0                EQUAL 'W')
               DISPLAY 'ERRO ACESSO GFCTB0C7'
               MOVE    'ERRO ACESSO GFCTB0C7' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF  SQLCODE                 EQUAL +100 OR -305
               MOVE '31.12.2099' TO DINIC-PRMSS-PCOTE  OF GFCTB0C7
           END-IF.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA
             INTO
                   :GFCTB0C7.CSEQ-AGPTO-CTA
             FROM   DB2PRD.PRMSS_GRP_DEPDC
             WHERE
                   CSERVC_TARIF       = :GFCTB0C7.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA        AND
                  (CDEPDC             = 0                           OR
                   CDEPDC             = :GFCTB0C7.CDEPDC)           AND
                   DINIC_PRMSS_PCOTE  = :GFCTB0C7.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               DISPLAY 'ERRO ACESSO GFCTB0C7'
               MOVE    'ERRO ACESSO GFCTB0C7' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0C7 TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0C7 TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       3430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO PAB.    *
      ******************************************************************
      *----------------------------------------------------------------*
       3435-ACESSAR-GFCTB0D0           SECTION.
      *----------------------------------------------------------------*
           MOVE BC0-CESTA-R OF ENTRADA TO WRK-CAMPO-POE-SINAL-05-C.
           MOVE '+'                    TO WRK-CAMPO-POE-SINAL-05-S.
           MOVE WRK-CAMPO-POE-SINAL-05 TO
           CSERVC-TARIF      OF GFCTB0D0.

           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D0.
           MOVE WRK-DATA-INIC-ADESAO   TO DINIC-PRMSS-PCOTE OF GFCTB0D0.
           MOVE BC0-AGENCIA-R OF ENTRADA TO
                CDEPDC            OF GFCTB0D0.

           MOVE WRK-5527-CPOSTO-SERVC  TO CPOSTO-SERVC      OF GFCTB0D0


           EXEC SQL
             SELECT
                   MAX(DINIC_PRMSS_PCOTE)
             INTO
                   :GFCTB0D0.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_PAB
             WHERE
                   CSERVC_TARIF       = :GFCTB0D0.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA        AND
                   CDEPDC             = :GFCTB0D0.CDEPDC            AND
                  (CPOSTO_SERVC       = 0                           OR
                   CPOSTO_SERVC       = :GFCTB0D0.CPOSTO-SERVC)     AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0D0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               DISPLAY 'ERRO ACESSO GFCTB0D0'
               MOVE    'ERRO ACESSO GFCTB0D0' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF  SQLCODE                 EQUAL +100 OR -305
               MOVE '31.12.2099' TO DINIC-PRMSS-PCOTE  OF GFCTB0D0
           END-IF.


           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE
             INTO
                   :GFCTB0D0.CSEQ-AGPTO-CTA,
                   :GFCTB0D0.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_PAB
             WHERE
                   CSERVC_TARIF       = :GFCTB0D0.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA        AND
                   CDEPDC             = :GFCTB0D0.CDEPDC            AND
                  (CPOSTO_SERVC       = 0                           OR
                   CPOSTO_SERVC       = :GFCTB0D0.CPOSTO-SERVC)     AND
                   DINIC_PRMSS_PCOTE  = :GFCTB0D0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               DISPLAY 'ERRO ACESSO GFCTB0D0'
               MOVE    'ERRO ACESSO GFCTB0D0' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0D0 TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0D0 TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       3435-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO PSTAL.  *
      ******************************************************************
      *----------------------------------------------------------------*
       3440-ACESSAR-GFCTB0D1           SECTION.
      *----------------------------------------------------------------*
           MOVE BC0-CESTA-R OF ENTRADA TO WRK-CAMPO-POE-SINAL-05-C.
           MOVE '+'                    TO WRK-CAMPO-POE-SINAL-05-S.
           MOVE WRK-CAMPO-POE-SINAL-05 TO
           CSERVC-TARIF      OF GFCTB0D1.

           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D1.
           MOVE WRK-DATA-INIC-ADESAO   TO DINIC-PRMSS-PCOTE OF GFCTB0D1.
           MOVE BC0-AGENCIA-R OF ENTRADA TO
           CDEPDC            OF GFCTB0D1.

           MOVE WRK-5527-CPOSTO-SERVC  TO CPOSTO-SERVC      OF GFCTB0D1


           EXEC SQL
             SELECT
                   MAX(DINIC_PRMSS_PCOTE)
             INTO
                   :GFCTB0D1.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_PSTAL
                 WHERE
                   CSERVC_TARIF       = :GFCTB0D1.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA        AND
                  (CDEPDC             = 0                           OR
                   CDEPDC             = :GFCTB0D1.CDEPDC            AND
                   CPOSTO_SERVC       = :GFCTB0D1.CPOSTO-SERVC)     AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0D1.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               DISPLAY 'ERRO ACESSO GFCTB0D1'
               MOVE    'ERRO ACESSO GFCTB0D1' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF  SQLCODE                 EQUAL +100 OR -305
               MOVE '31.12.2099' TO DINIC-PRMSS-PCOTE  OF GFCTB0D1
           END-IF.



           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA
             INTO
                   :GFCTB0D1.CSEQ-AGPTO-CTA
             FROM   DB2PRD.PRMSS_GRP_PSTAL
             WHERE
                   CSERVC_TARIF       = :GFCTB0D1.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA        AND
                  (CDEPDC             = 0                           OR
                   CDEPDC             = :GFCTB0D1.CDEPDC            AND
                   CPOSTO_SERVC       = :GFCTB0D1.CPOSTO-SERVC)     AND
                   DINIC_PRMSS_PCOTE  = :GFCTB0D1.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               DISPLAY 'ERRO ACESSO GFCTB0D1'
               MOVE    'ERRO ACESSO GFCTB0D1' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0D1 TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0D1 TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       3440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5564 PARA OBTER UF E MUNICIPIO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       3442-OBTER-UF-MUNICIPIO         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTRK-AREA.

           INITIALIZE GFCTRK-AREA.

           MOVE 'B'                    TO GFCTRK-TPO-AMBIENTE.
           MOVE BC0-AGENCIA-R OF ENTRADA TO GFCTRK-CJUNC-DEPDC.

           MOVE 'GFCT5564'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTRK-AREA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS OR
               GFCTRK-COD-RETORNO          NOT EQUAL ZEROS
               IF WRK-COUNT-DISPLAY LESS 1000
                  ADD 1 TO WRK-COUNT-DISPLAY
                  DISPLAY 'ERRO ACESSO GFCT5564'
               END-IF
               MOVE ZEROS              TO GFCTRK-CMUN-RURAL
               MOVE SPACES             TO GFCTRK-CSGL-UF
           END-IF.

      *----------------------------------------------------------------*
       3442-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GP MUNICIPIO. *
      ******************************************************************
      *----------------------------------------------------------------*
       3445-ACESSAR-GFCTB0C9           SECTION.
      *----------------------------------------------------------------*
           MOVE BC0-CESTA-R OF ENTRADA TO WRK-CAMPO-POE-SINAL-05-C.
           MOVE '+'                    TO WRK-CAMPO-POE-SINAL-05-S.
           MOVE WRK-CAMPO-POE-SINAL-05 TO
           CSERVC-TARIF      OF GFCTB0C9.

           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C9.
           MOVE WRK-DATA-INIC-ADESAO   TO DINIC-PRMSS-PCOTE OF GFCTB0C9.
           MOVE GFCTRK-CMUN-RURAL      TO
                WRK-CAMPO-TIRA-SINAL-17.
           MOVE WRK-CAMPO-TIRA-SINAL-07 TO
                WRK-CAMPO-POE-SINAL-07-C.
           MOVE '+' TO WRK-CAMPO-POE-SINAL-07-S.
           MOVE WRK-CAMPO-POE-SINAL-07   TO
           CMUN-IBGE         OF GFCTB0C9.


           EXEC SQL
             SELECT
                   MAX(DINIC_PRMSS_PCOTE)
             INTO
                   :GFCTB0C9.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_MUN
                 WHERE
                   CSERVC_TARIF       = :GFCTB0C9.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0C9.CAGPTO-CTA        AND
                   CMUN_IBGE          = :GFCTB0C9.CMUN-IBGE         AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0C9.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               DISPLAY 'ERRO ACESSO GFCTB0C9'
               MOVE    'ERRO ACESSO GFCTB0C9' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF  SQLCODE                 EQUAL +100 OR -305
               MOVE '31.12.2099' TO DINIC-PRMSS-PCOTE  OF GFCTB0C9
           END-IF.


           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA
             INTO
                   :GFCTB0C9.CSEQ-AGPTO-CTA
             FROM   DB2PRD.PRMSS_GRP_MUN
             WHERE
                   CSERVC_TARIF       = :GFCTB0C9.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0C9.CAGPTO-CTA        AND
                   CMUN_IBGE          = :GFCTB0C9.CMUN-IBGE         AND
                   DINIC_PRMSS_PCOTE  = :GFCTB0C9.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               DISPLAY 'ERRO ACESSO GFCTB0C9'
               MOVE    'ERRO ACESSO GFCTB0C9' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0C9 TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0C9 TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       3445-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO UF.     *
      ******************************************************************
      *----------------------------------------------------------------*
       3450-ACESSAR-GFCTB0D3           SECTION.
      *----------------------------------------------------------------*
           MOVE BC0-CESTA-R OF ENTRADA TO WRK-CAMPO-POE-SINAL-05-C.
           MOVE '+'                    TO WRK-CAMPO-POE-SINAL-05-S.
           MOVE WRK-CAMPO-POE-SINAL-05 TO
           CSERVC-TARIF      OF GFCTB0D3.

           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D3.
           MOVE WRK-DATA-INIC-ADESAO   TO DINIC-PRMSS-PCOTE OF GFCTB0D3.
           MOVE GFCTRK-CSGL-UF         TO CSGL-UF           OF GFCTB0D3.


           EXEC SQL
             SELECT
                   MAX(DINIC_PRMSS_PCOTE)
             INTO
                   :GFCTB0D3.DINIC-PRMSS-PCOTE
             FROM   DB2PRD.PRMSS_GRP_UF
                 WHERE
                   CSERVC_TARIF       = :GFCTB0D3.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D3.CAGPTO-CTA        AND
                   CSGL_UF            = :GFCTB0D3.CSGL-UF           AND
                   DINIC_PRMSS_PCOTE <= :GFCTB0D3.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               DISPLAY 'ERRO ACESSO GFCTB0D3'
               MOVE    'ERRO ACESSO GFCTB0D3' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF  SQLCODE                 EQUAL +100 OR -305
               MOVE '31.12.2099' TO DINIC-PRMSS-PCOTE  OF GFCTB0D3
           END-IF.

           EXEC SQL
             SELECT
                   CSEQ_AGPTO_CTA
             INTO
                   :GFCTB0D3.CSEQ-AGPTO-CTA
             FROM   DB2PRD.PRMSS_GRP_UF
             WHERE
                   CSERVC_TARIF       = :GFCTB0D3.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB0D3.CAGPTO-CTA        AND
                   CSGL_UF            = :GFCTB0D3.CSGL-UF           AND
                   DINIC_PRMSS_PCOTE  = :GFCTB0D3.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               DISPLAY 'ERRO ACESSO GFCTB0D3'
               MOVE    'ERRO ACESSO GFCTB0D3' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0D3 TO WRK-SEQ-AGPTO
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0D3 TO WRK-DT-INICIO
               MOVE 'S'                TO WRK-EXISTE-AGRUPAMENTO
           END-IF.

      *----------------------------------------------------------------*
       3450-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      *    ROTINA QUE ATUALIZA A BASE DE CONTROLE                      *
      ******************************************************************
       3455-ATUALIZA-CONTROLE SECTION.

           MOVE WRK-TIMESTAMP-AUX TO BC6-HINCL-REG.
           MOVE 5                 TO BC6-CTPO-OPER-MASSV
           MOVE 1                 TO BC6-CSIT-PROCM-MASSV.
           MOVE SPACE TO             BC6-ROBS-PROCM-MASSV.
           MOVE ACU-LIDOS-ENTRADA TO WRK-CAMPO-TIRA-SINAL-17
           MOVE WRK-CAMPO-TIRA-SINAL-09 TO
                WRK-CAMPO-POE-SINAL-09-C
           MOVE '+' TO WRK-CAMPO-POE-SINAL-09-S
           MOVE WRK-CAMPO-POE-SINAL-09 TO BC6-QREG-LIDO-MASSV
                                          BC6-QREG-ACTAC-MASSV.
           MOVE ACU-GRAVADOS-REJEITAD TO WRK-CAMPO-TIRA-SINAL-17
           MOVE WRK-CAMPO-TIRA-SINAL-09 TO
                WRK-CAMPO-POE-SINAL-09-C
           MOVE '+' TO WRK-CAMPO-POE-SINAL-09-S
           MOVE WRK-CAMPO-POE-SINAL-09 TO BC6-QREG-REJTD-MASSV.
           MOVE ZEROS                 TO BC6-CFUNC-MANUT.
           MOVE '01.01.0001'          TO BC6-DPROCM-AGNDA.
           MOVE '0001-01-01-01.01.01.000001' TO
                BC6-HMANUT-REG.

           WRITE GFCTWBC6-REG.

           MOVE WRK-GRAVACAO TO WRK-OPERACAO.

           PERFORM 1140-TESTAR-FS-CONTROLE.

           ADD 1 TO ACU-GRAVADOS-CONTROLE.

      *----------------------------------------------------------------*
       3455-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE ATUALIZA A BASE DE CONTROLE PARA O CASO DE ERRO  *
      ******************************************************************
       3456-ATUALIZA-CONTERRO SECTION.

           MOVE WRK-TIMESTAMP-AUX TO BC6-HINCL-REG.
           MOVE 5                 TO BC6-CTPO-OPER-MASSV.
           MOVE 5                 TO BC6-CSIT-PROCM-MASSV.
           MOVE WRK-MSG           TO BC6-ROBS-PROCM-MASSV.
           MOVE ZEROS             TO WRK-CAMPO-TIRA-SINAL-17
           MOVE WRK-CAMPO-TIRA-SINAL-09 TO
                WRK-CAMPO-POE-SINAL-09-C
           MOVE '+' TO WRK-CAMPO-POE-SINAL-09-S
           MOVE WRK-CAMPO-POE-SINAL-09 TO BC6-QREG-LIDO-MASSV
                                          BC6-QREG-ACTAC-MASSV.
           MOVE ZEROS                 TO WRK-CAMPO-TIRA-SINAL-17
           MOVE WRK-CAMPO-TIRA-SINAL-09 TO
                WRK-CAMPO-POE-SINAL-09-C
           MOVE '+' TO WRK-CAMPO-POE-SINAL-09-S
           MOVE WRK-CAMPO-POE-SINAL-09 TO BC6-QREG-REJTD-MASSV.
           MOVE ZEROS                 TO BC6-CFUNC-MANUT.
           MOVE '01.01.0001'          TO BC6-DPROCM-AGNDA.
           MOVE '0001-01-01-01.01.01.000001' TO
                BC6-HMANUT-REG.

           WRITE GFCTWBC6-REG.

           MOVE WRK-GRAVACAO TO WRK-OPERACAO.

           PERFORM 1140-TESTAR-FS-CONTROLE.

           ADD 1 TO ACU-GRAVADOS-CONTROLE.

      *----------------------------------------------------------------*
       3456-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      *    ROTINA QUE EFETUA PROCESSAMENTO FINAL DO PROGRAMA           *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF WRK-AT-BASE = 'S'
              PERFORM 3455-ATUALIZA-CONTROLE
           END-IF.

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE ENTRADA
                 REJEITAD
                 CONTROLE
                 SAIDA.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           IF ACU-LIDOS-ENTRADA EQUAL ZEROS
              MOVE 8 TO RETURN-CODE
           ELSE
              IF ACU-GRAVADOS-REJEITAD GREATER ZEROS
                 MOVE 4 TO RETURN-CODE
              ELSE
                 MOVE 0 TO RETURN-CODE
              END-IF
           END-IF.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
______*____ROTINA_DE_EMISSAO_DE_DISPLAY_DE_TOTAIS______________________*
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS-ENTRADA           NOT EQUAL ZEROS
               DISPLAY '*-----------------------------------------------
      -        '--------*'
               DISPLAY '*                   PROGRAMA GFCT3610
      -        '        *'
               DISPLAY '*                   -----------------
      -        '        *'
               DISPLAY '*
      -        '        *'
               DISPLAY '*          PROCESSAMENTO  EFETUADO COM SUCESSO
      -        '        *'
               DISPLAY '* CONSISTENCIA INICIAL CARGA MASSIVA DE ADESAO
      -        '        *'
               DISPLAY '* ARQUIVO ISD: CADMASS.TXT
      -        '        *'
               DISPLAY '*-----------------------------------------------
      -        '--------*'
               DISPLAY '************************************************
      -        '*********'
               MOVE ACU-LIDOS-ENTRADA      TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS LIDOS              = '
                 WRK-MASCARA
                           ' *'
               MOVE ACU-GRAVADOS-SAIDA     TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS GRAVADOS  SAIDA    = '
                 WRK-MASCARA
                           ' *'
               MOVE ACU-GRAVADOS-REJEITAD  TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS REJEITADOS         = '
                   WRK-MASCARA
                            ' *'
               MOVE ACU-GRAVADOS-CONTROLE  TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS CONTROLE           = '
                   WRK-MASCARA
                            ' *'

               DISPLAY '************************************************
      -        '*********'
           ELSE
               DISPLAY '*-----------------------------------------------
      -        '*'
               DISPLAY '*               PROGRAMA GFCT3610
      -        '*'
               DISPLAY '*               -----------------
      -        '*'
               DISPLAY '*
      -        '*'
               DISPLAY '*           ARQUIVO DE ENTRADA VAZIO
      -        '*'
               DISPLAY '*-----------------------------------------------
      -        '*'
           END-IF.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA PERMISSAO POR SEGMENTO                             *
      ******************************************************************
       4200-OBTER-SEG-GESTAO SECTION.

           MOVE WRK-DATA-INIC-ADESAO   TO DINIC-VGCIA-ASSOC OF GFCTB0H3
                                          DFIM-VGCIA-ASSOC OF GFCTB0H3.

           MOVE WRK-CSGMTO-CLI         TO CSGMTO-INIC-CLI OF GFCTB0H3
                                          CSGMTO-FNAL-CLI OF GFCTB0H3.
           EXEC SQL
               OPEN CSR01-GFCTB0H3
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               DISPLAY 'ERRO ABERTURA GFCTB0H3'
               MOVE    'ERRO ABERTURA GFCTB0H3' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           MOVE 'N' TO WRK-FIM-CURSOR-H3
                       WRK-PERMISSAO-H3-OK.

           PERFORM 4210-PROCESSA-CURSOR-H3 UNTIL
                   WRK-FIM-CURSOR-H3 = 'S' OR
                   WRK-PERMISSAO-H3-OK = 'S'.

           IF WRK-PERMISSAO-H3-OK = 'S'
              MOVE 'S' TO WRK-AGRUPAMENTO-VALIDO
           END-IF.

      *----------------------------------------------------------------*
       4200-99-FIM.                     EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSA CURSOR H3                                *
      ******************************************************************
       4210-PROCESSA-CURSOR-H3 SECTION.

           EXEC SQL
               FETCH CSR01-GFCTB0H3
               INTO   :GFCTB0H3.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               DISPLAY 'ERRO LEITURA GFCTB0H3'
               MOVE    'ERRO LEITURA GFCTB0H3' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF SQLCODE = +100
              PERFORM 4230-CLOSE-CURSOR-H3
              MOVE 'S' TO WRK-FIM-CURSOR-H3
              GO TO 4210-99-FIM
           END-IF.

           MOVE CSGMTO-GSTAO-TARIF OF GFCTB0H3
                                   TO CSGMTO-GSTAO-TARIF OF GFCTB0H5.
           MOVE WRK-TIPO-PESSOA-CLIENTE TO
                CTPO-PSSOA OF GFCTB0H5.
           MOVE WRK-DATA-INIC-ADESAO
                                       TO DFIM-VGCIA-SGMTO  OF GFCTB0H5
                                         DINIC-VGCIA-SGMTO  OF GFCTB0H5.

           EXEC SQL
               SELECT CDEPDC_GSTOR_SGMTO
               INTO   :GFCTB0H5.CDEPDC-GSTOR-SGMTO
               FROM   DB2PRD.TTPO_SGMTO_GSTAO
               WHERE  DFIM_VGCIA_SGMTO   >= :GFCTB0H5.DFIM-VGCIA-SGMTO
                 AND  DINIC_VGCIA_SGMTO <= :GFCTB0H5.DINIC-VGCIA-SGMTO
                 AND  CSGMTO_GSTAO_TARIF  = :GFCTB0H5.CSGMTO-GSTAO-TARIF
                 AND  CTPO_PSSOA         IN ('A', :GFCTB0H5.CTPO-PSSOA)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               DISPLAY 'ERRO LEITURA GFCTB0H5'
               MOVE    'ERRO LEITURA GFCTB0H5' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF SQLCODE = +100
              PERFORM 4230-CLOSE-CURSOR-H3
              MOVE 'S' TO WRK-FIM-CURSOR-H3
              GO TO 4210-99-FIM
           END-IF.


           MOVE BC0-CESTA-R OF ENTRADA TO
                CSERVC-TARIF       OF GFCTB0D2.
           MOVE 9                TO CAGPTO-CTA         OF GFCTB0D2.
           MOVE CSGMTO-GSTAO-TARIF OF GFCTB0H3
                                 TO CSGMTO-GSTAO-TARIF OF GFCTB0D2.

           EXEC SQL
               OPEN CSR02-GFCTB0D2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               DISPLAY 'ERRO ABERTURA GFCTB0D2'
               MOVE    'ERRO ABERTURA GFCTB0D2' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.


           MOVE 'N' TO WRK-FIM-CURSOR-D2
                       WRK-PERMISSAO-D2-OK.

           PERFORM 4220-PROCESSA-CURSOR-D2 UNTIL
                   WRK-FIM-CURSOR-D2 = 'S' OR
                   WRK-PERMISSAO-D2-OK = 'S'.


      *----------------------------------------------------------------*
       4210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSA CURSOR D2                                *
      ******************************************************************
       4220-PROCESSA-CURSOR-D2 SECTION.

           EXEC SQL
               FETCH CSR02-GFCTB0D2
               INTO
                   :GFCTB0D2.DINIC-PRMSS-PCOTE,
                   :GFCTB0D2.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               DISPLAY 'ERRO LEITURA GFCTB0D2'
               MOVE    'ERRO LEITURA GFCTB0D2' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF SQLCODE = +100
              PERFORM 4240-CLOSE-CURSOR-D2
              MOVE 'S' TO WRK-FIM-CURSOR-D2
              GO TO 4220-99-FIM
           END-IF.

           MOVE BC0-CESTA-R OF ENTRADA TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE CSEQ-AGPTO-CTA OF GFCTB0D2 TO
                CSEQ-AGPTO-CTA OF GFCTB0C6.
           MOVE DINIC-PRMSS-PCOTE OF GFCTB0D2 TO
                DINIC-PRMSS-PCOTE OF GFCTB0C6.
           MOVE WRK-DATA-INIC-ADESAO TO
                DFIM-PRMSS-PCOTE OF GFCTB0C6.
           MOVE 9                      TO CAGPTO-CTA        OF GFCTB0C6.

           EXEC SQL
             SELECT
                   CINDCD_AGPTO_TOT
             INTO
                   :GFCTB0C6.CINDCD-AGPTO-TOT
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
                   CSERVC_TARIF        = :GFCTB0C6.CSERVC-TARIF
               AND CAGPTO_CTA          = :GFCTB0C6.CAGPTO-CTA
               AND CSEQ_AGPTO_CTA      = :GFCTB0C6.CSEQ-AGPTO-CTA
               AND CINDCD_ADSAO_INDVD  IN ('I', 'A')
               AND DINIC_PRMSS_PCOTE   = :GFCTB0C6.DINIC-PRMSS-PCOTE
               AND DFIM_PRMSS_PCOTE   >= :GFCTB0C6.DFIM-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               DISPLAY 'ERRO LEITURA GFCTB0C6'
               MOVE    'ERRO LEITURA GFCTB0C6' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF SQLCODE = ZEROS
              MOVE 'S' TO WRK-PERMISSAO-H3-OK
                          WRK-PERMISSAO-D2-OK
              PERFORM 4230-CLOSE-CURSOR-H3
              PERFORM 4240-CLOSE-CURSOR-D2
              GO TO 4220-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       4220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE FECHA O CURSOR H3                                *
      ******************************************************************
       4230-CLOSE-CURSOR-H3    SECTION.

           EXEC SQL
               CLOSE CSR01-GFCTB0H3
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               DISPLAY 'ERRO FECHTO CURSOR H3'
               MOVE    'ERRO FECHTO CURSOR H3' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

      *----------------------------------------------------------------*
       4230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE FECHA O CURSOR D2                                *
      ******************************************************************
       4240-CLOSE-CURSOR-D2    SECTION.

           EXEC SQL
               CLOSE CSR02-GFCTB0D2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               DISPLAY 'ERRO FECHTO CURSOR D2'
               MOVE    'ERRO FECHTO CURSOR D2' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

      *----------------------------------------------------------------*
       4240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *5000-PERMISSAO-ADESAO-CEL SECTION.
      *----------------------------------------------------------------*
       5000-PERMISSAO-ADESAO-CEL SECTION.

           MOVE BC0-CESTA-R OF ENTRADA TO WRK-CAMPO-POE-SINAL-05-C
           MOVE '+'                    TO WRK-CAMPO-POE-SINAL-05-S
           MOVE WRK-CAMPO-POE-SINAL-05 TO CSERVC-TARIF OF GFCTB0N7
           MOVE BC0-OPERADORA-R OF ENTRADA TO WRK-CAMPO-POE-SINAL-09-C
           MOVE '+'                    TO WRK-CAMPO-POE-SINAL-09-S
           MOVE WRK-CAMPO-POE-SINAL-09 TO COPER-FONE-MOVEL OF GFCTB0N7
                                          COPER-FONE-MOVEL  OF GFCTB0M4
                                          COPER-FONE-MOVEL  OF GFCTB0M2
           MOVE BC0-DDD-R OF ENTRADA   TO WRK-CAMPO-POE-SINAL-03-C
           MOVE '+'                    TO WRK-CAMPO-POE-SINAL-03-S
           MOVE WRK-CAMPO-POE-SINAL-03 TO CDDD-FONE-MOVEL  OF GFCTB0N7
                                          CDDD-FONE-MOVEL   OF GFCTB0M4
                                          CDDD-FONE-MOVEL   OF GFCTB0M2
           MOVE DPROCM-ATUAL                             OF GFCTB0A1 TO
                DINIC-VGCIA-REG                          OF GFCTB0N7
                DFIM-VGCIA-REG                           OF GFCTB0N7.

           EXEC SQL
             SELECT
                   QMAX_DIG_MOVEL
             INTO
                   :GFCTB0N7.QMAX-DIG-MOVEL
             FROM   DB2PRD.TOPER_PRMSS_CLULR
             WHERE CSERVC_TARIF       = :GFCTB0N7.CSERVC-TARIF
             AND   COPER_FONE_MOVEL   = :GFCTB0N7.COPER-FONE-MOVEL
             AND   CDDD_FONE_MOVEL    = :GFCTB0N7.CDDD-FONE-MOVEL
             AND   DINIC_VGCIA_REG    <= :GFCTB0N7.DINIC-VGCIA-REG
             AND   DFIM_VGCIA_REG     >= :GFCTB0N7.DFIM-VGCIA-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               DISPLAY 'ERRO ACESS. BASE GFCTB0N7'
               MOVE    'ERRO ACESS. BASE GFCTB0N7' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                   TO WRK-ERRO
               MOVE 'NAO EXISTE PERM. ADESAO CELULAR'
                    TO WRK-MSG
            END-IF.

            MOVE BC0-NUM-CEL-R OF ENTRADA TO WRK-NUM-CEL-11.
            MOVE '+'                      TO WRK-NUM-CEL-S2.

            IF (WRK-NUM-CEL-9     EQUAL ZEROS     AND
                QMAX-DIG-MOVEL OF GFCTB0N7 = 8) OR
               (WRK-NUM-CEL-9 NOT EQUAL ZEROS     AND
                QMAX-DIG-MOVEL OF GFCTB0N7 = 9)
                NEXT SENTENCE
            ELSE
               MOVE 'S'                   TO WRK-ERRO
               MOVE 'DIGITOS DO NUM. CELULAR NAO PEMITIDOS'
                    TO WRK-MSG
            END-IF.


       5000-99-FIM. EXIT.

      *----------------------------------------------------------------*
      *5010-CONSISTE-OPERADORA SECTION.
      *----------------------------------------------------------------*
       5010-CONSISTE-OPERADORA SECTION.

           EXEC SQL
               SELECT
                   ROPER_FONE_MOVEL
               INTO
                   :GFCTB0M4.ROPER-FONE-MOVEL
               FROM
                   DB2PRD.TOPER_FONE_CREDT
               WHERE
                   COPER_FONE_MOVEL   = :GFCTB0M4.COPER-FONE-MOVEL
               AND CDDD_FONE_MOVEL    = :GFCTB0M4.CDDD-FONE-MOVEL
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)OR
               (SQLWARN0               EQUAL 'W')
               DISPLAY 'ERRO ACESS. BASE GFCTB0M4'
               MOVE    'ERRO ACESS. BASE GFCTB0M4' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                   TO WRK-ERRO
               MOVE 'DDD/OPERADORA NAO CADASTRADOS'
                    TO WRK-MSG
           END-IF.

       5010-99-FIM. EXIT.

      ******************************************************************
      *5020-VERIFICA-CEL-ADERIDO SECTION.
      ******************************************************************
       5020-VERIFICA-CEL-ADERIDO SECTION.

           MOVE BC0-NUM-CEL-R OF ENTRADA TO WRK-CAMPO-POE-SINAL-11-C
           MOVE '+'                    TO WRK-CAMPO-POE-SINAL-11-S
           MOVE WRK-CAMPO-POE-SINAL-11 TO NLIN-TFONI OF GFCTB0M2.

           EXEC SQL
               OPEN CSR01-GFCTB0M2
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS )OR
               (SQLWARN0               EQUAL 'W')
               DISPLAY 'ERRO ABERTURA BASE GFCTB0M2'
               MOVE    'ERRO ABERTURA BASE GFCTB0M2' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           MOVE 'N' TO WRK-FIM-CURSOR-M2
                       WRK-ACHOU-SEL.

           PERFORM 5030-FETCH-M2 UNTIL
                   WRK-FIM-CURSOR-M2 EQUAL 'S' OR
                   WRK-ACHOU-SEL EQUAL 'S'.

           EXEC SQL
               CLOSE CSR01-GFCTB0M2
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS )OR
               (SQLWARN0               EQUAL 'W')
               DISPLAY 'ERRO FECHTO BASE GFCTB0M2'
               MOVE    'ERRO FECHTO BASE GFCTB0M2' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF WRK-ACHOU-SEL EQUAL 'S'
               MOVE 'S'                   TO WRK-ERRO
               MOVE 'CELULAR JAH ADERIDO'
                    TO WRK-MSG
           END-IF.

       5020-99-FIM. EXIT.

      ******************************************************************
      *5030-FETCH-M2 SECTION.
      ******************************************************************
       5030-FETCH-M2 SECTION.

           EXEC SQL
             FETCH CSR01-GFCTB0M2   INTO
                   :GFCTB0M2.CAG-BCRIA,
                   :GFCTB0M2.CCTA-CLI,
                   :GFCTB0M2.HINCL-REG
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
               DISPLAY 'ERRO ACESSO BASE GFCTB0M2'
               MOVE    'ERRO ACESSO BASE GFCTB0M2' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF SQLCODE = +100
              MOVE 'S' TO WRK-FIM-CURSOR-M2
              GO TO 5030-99-FIM
           END-IF.

           MOVE CAG-BCRIA     OF GFCTB0M2        TO
                CJUNC-DEPDC   OF GFCTB009.
           MOVE CCTA-CLI      OF GFCTB0M2        TO
                CCTA-CLI      OF GFCTB009.
           MOVE HINCL-REG     OF GFCTB0M2        TO
                HINCL-REG     OF GFCTB009.
           MOVE DPROCM-ATUAL  OF GFCTB0A1        TO
                DINIC-ADSAO-INDVD       OF GFCTB009
                DFIM-ADSAO-INDVD        OF GFCTB009.

           EXEC SQL
             SELECT
                CSERVC_TARIF
             INTO
                :GFCTB009.CSERVC-TARIF
             FROM DB2PRD.ADSAO_INDVD_PCOTE
             WHERE
                   CJUNC_DEPDC  = :GFCTB009.CJUNC-DEPDC
             AND   CCTA_CLI     = :GFCTB009.CCTA-CLI
             AND   HINCL_REG    = :GFCTB009.HINCL-REG
             AND   DINIC_ADSAO_INDVD <= :GFCTB009.DINIC-ADSAO-INDVD
             AND   DFIM_ADSAO_INDVD >=  :GFCTB009.DFIM-ADSAO-INDVD
             AND   CINDCD_EXCL_REG = 0
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
               DISPLAY 'ERRO ACESSO BASE GFCTB009'
               MOVE    'ERRO ACESSO BASE GFCTB009' TO WRK-MSG
               MOVE 12 TO RETURN-CODE
               PERFORM 3456-ATUALIZA-CONTERRO
               CLOSE ENTRADA SAIDA REJEITAD CONTROLE
               STOP RUN
           END-IF.

           IF SQLCODE EQUAL ZEROS
              MOVE CJUNC-DEPDC OF GFCTB009 TO WRK-CAMPO-TIRA-SINAL
              MOVE WRK-CAMPO-TIRA-SINAL-05 TO WRK-AGEN
              MOVE CCTA-CLI    OF GFCTB009 TO WRK-CAMPO-TIRA-SINAL
              MOVE WRK-CAMPO-TIRA-SINAL-07 TO WRK-CONTA
              MOVE CSERVC-TARIF OF GFCTB009 TO WRK-CAMPO-TIRA-SINAL
              MOVE WRK-CAMPO-TIRA-SINAL-05 TO WRK-CESTA
              IF BC0-AGENCIA-R OF ENTRADA EQUAL WRK-AGEN AND
                 BC0-CONTA-R   OF ENTRADA EQUAL WRK-CONTA AND
                 BC0-CESTA-R   OF ENTRADA EQUAL WRK-CESTA
                 NEXT SENTENCE
              ELSE
                 MOVE 'S' TO WRK-ACHOU-SEL
              END-IF
           END-IF.

       5030-99-FIM. EXIT.
      ******************************************************************
      *    ROTINA DE PROCESSAMENTE DE ERRRO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT3610'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
