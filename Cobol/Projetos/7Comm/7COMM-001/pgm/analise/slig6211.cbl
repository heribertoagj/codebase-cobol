      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. SLIG6211.
       AUTHOR.     FABIO AUGUSTO FINK.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   SLIG6211                                    *
      *    PROGRAMADOR.:   FABIO AUGUSTO FINK        - CPM PATO BRANCO *
      *    ANALISTA CPM:   EDINA FATIMA GNOATO       - CPM PATO BRANCO *
      *    ANALISTA....:   TERRY                     - DTS / GP. 75    *
      *    DATA........:   31/03/2004                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   UNIFICA OS ARQUIVOS DE MOVIMENTO DIARIO E   *
      *      DECENAL.                                                  *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                        INCLUDE/BOOK      *
      *                    MVTCHQSR                    I#SLIGC0        *
      *                    EMITCHEQ                    I#SLIGC0        *
      *                    SRELCTRL                       -            *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GRUP34 - AREA PARA ACESSO AO MODULO GRUP0930              *
      *    I#MESUM4 - AREA PARA ACESSO AO MODULO MESU9014              *
      *    CLIBW020 - AREA PARA RETORNO DO MODULO CLIB2000             *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    CLIB2000 - ACESSO AS BASES CLIE/MULTIBANCO                  *
      *    MESU9014 - OBTER DIRETORIA REGIONAL                         *
      *    MESU9023 - OBTER GERENCIA REGIONAL                          *
      *    GRUP0930 - CONSULTA DE EMPRESAS                             *
      *    POOL7100 - TRATAMENTO DE ERROS                              *
      *================================================================*
      *                    A L T E R A C A O                           *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:   LUIS VIACELLI FERNANDES   - CPM PATO BRANCO *
      *    ANALISTA CPM:   EDINA FATIMA GNOATO       - CPM PATO BRANCO *
      *    ANALISTA....:   TERRY                     - DTS / GP. 75    *
      *    DATA........:   08/04/2004                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INSERIR CONSISTENCIA NO ARQUIVO MVTCHQSR,   *
      *      POR ORDEM ASCENDENTE.                                     *
      *================================================================*
      *                    A L T E R A C A O                           *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:   LEANDRO DE OLIVEIRA THOMAZ  - DTS           *
      *    ANALISTA....:   LEANDRO DE OLIVEIRA THOMAZ  - DTS           *
      *    DATA........:   14/02/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INCLUSAO DE CAMPOS DE TOTAIS NO RELATORIO   *
      *      SRELCTR.                                                  *
      *================================================================*
DTSGP *                    A L T E R A C A O                           *
DTSGP *----------------------------------------------------------------*
DTSGP *    PROGRAMADOR.:   GLAUCO PADOA              - DTS             *
DTSGP *    ANALISTA....:   SOLANGE BARBEIRO          - DTS / GP. 75    *
DTSGP *    DATA........:   21/02/2005                                  *
DTSGP *----------------------------------------------------------------*
DTSGP *    OBJETIVO....:   SEPARAR O REGISTRO DA INC28 QUANDO O ULTIMO *
DTSGP *                    MOTIVO DE DEVOLUCAO CONSTAR DA LISTA DE     *
DTSGP *                    CODIGOS FORNECIDOS.                         *
DTSGP *                    FOI CRIADO NOVO DDNAME "MVCHDESP" PARA      *
DTSGP *                    GUARDAR ESTES REGISTROS E POR HORA ESTE     *
DTSGP *                    ARQUIVO NAO ESTA SENDO UTILIZADO PARA       *
DTSGP *                    NENHUM OUTRO PROCESSO.                      *
DTSGP *                    OBS : ESTE ARQUIVO USA A MESO LAYOUT DA     *
DTSGP *                          INC I#SLIG28, MAS NAO USA A INCLUDE.  *
DTSGP *================================================================*
DTSRA *                    A L T E R A C A O                           *
DTSRA *----------------------------------------------------------------*
DTSRA *    PROGRAMADOR.:   RODRIGO AUGUSTO           - DTS             *
DTSRA *    ANALISTA....:   SOLANGE BARBEIRO          - DTS / GP. 75    *
DTSRA *    DATA........:   23/03/2005                                  *
DTSRA *----------------------------------------------------------------*
DTSRA *    OBJETIVO....:   FOI CRIADO NOVO DDNAME "QTDEBCO" PARA       *
DTSRA *                    CALCULAR QUANTIDADE DE CHEQUES POR BANCOS   *
DTSRA *'===============================================================*
      *                    A L T E R A C A O                           *
      *----------------------------------------------------------------*
      *    PROGRAMADORA:   RICARDO PICCINI        - CPM PATO BRANCO    *
      *    ANALISTA CPM:   MARCIO CUSTIN          - CPM PATO BRANCO    *
      *    ANALISTA....:   LUIZ CARLOS CAETANO    - DTS / GP.73        *
      *    DATA........:   09/09/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   RETIRAR OS ARQUIVOS ARQDATAS, MVCHQREL E    *
      *      ARQERROS BEM COMO OS TRATAMENTOS A ELES  REFERENCIADOS.   *
      *      TROCAR A INC I#SLIG28 PELA I#SLIGC0.                      *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#SLIGC0 - LAYOUT ARQUIVO CHEQUE - DIARIA E DECENAL.        *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0300 - LER ARQUIVO COMPRIMIDO.                          *
      *    BRAD0315 - GRAVAR ARQUIVO COMPRIMIDO.                       *
      *================================================================*
           EJECT
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT SRELCTRL ASSIGN      TO UT-S-SRELCTRL
           FILE STATUS                 IS WRK-FS-SRELCTRL.

DTSRA      SELECT QTDEBCO  ASSIGN      TO UT-S-QTDEBCO
DTSRA      FILE STATUS                 IS WRK-FS-QTDEBCO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OUTPUT: RELATORIO DE CONTROLE OPERACIONAL                   *
      *            ORG. SEQUENCIAL     -    LRECL   =   081            *
      *----------------------------------------------------------------*

       FD  SRELCTRL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-SRELCTRL                PIC  X(081).

DTSRA *----------------------------------------------------------------*
DTSRA *    OUTPUT: ARQUIVO COM QUANTIDADE DE CHEQUES POR BANCO         *
DTSRA *            ORG. SEQUENCIAL     -    LRECL   =   022            *
DTSRA *----------------------------------------------------------------*
DTSRA
DTSRA  FD  QTDEBCO
DTSRA      RECORDING MODE IS F
DTSRA      LABEL RECORD IS STANDARD
DTSRA      BLOCK CONTAINS 0 RECORDS.
DTSRA
DTSRA  01  REG-QTDEBCO.
DTSRA      05  REG-QTDEBCO-CODBCO      PIC S9(003) COMP-3.
DTSRA      05  REG-QTDEBCO-QTDE        PIC S9(015) COMP-3.
DTSRA      05  FILLER                  PIC  X(012).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6211 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-MVTCHQSR          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-EMITCHEQ          PIC  9(009) COMP-3  VALUE ZEROS.
DTSGP  77  ACU-GRAVA-QTDEBCO           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-N-ENC-CLIB2000          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-ERROR-CLIB2000          PIC  9(009) COMP-3  VALUE ZEROS.
       77  WRK-FLAG                    PIC  9(001)         VALUE ZEROS.
LOT    77  ACU-EMITT-BCO-237           PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-EMITT-BCO-DIF-237       PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-EMITT-CGC-ZEROS-IN      PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-EMITT-CGC-DIF-ZEROS-IN  PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-ACESSOS-CLIB2000        PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-EMITT-CGC-ATUALZ-CC     PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-EMITT-CGC-ATUALZ-PP     PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-EMITT-NOME-ENC-CC       PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-EMITT-NOME-ENC-PP       PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-EMITT-NOME-N-ENC        PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-ACESSOS-GRUP0930        PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-EMITT-GRP-ENC           PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-EMITT-GRP-N-ENC         PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-EMITT-CGC-ZEROS-OUT     PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-EMITT-CGC-DIF-ZEROS-OUT PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-EMITT-NOME-N-ENC-OUT    PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-EMITT-NOME-SPACES-OUT   PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-EMITT-NOME-OK-OUT       PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-EMITT-GRP-ZEROS-OUT     PIC  9(015) COMP-3  VALUE ZEROS.
LOT    77  ACU-EMITT-GRP-DIF-ZEROS-OUT PIC  9(015) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE FILE-STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-MVTCHQSR             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SRELCTRL             PIC  X(002)         VALUE SPACES.
DTSRA  77  WRK-FS-QTDEBCO              PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

           EJECT
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-ARQUIVO                 PIC  X(008)         VALUE SPACES.
       77  WRK-SAI-EMITCHQ             PIC  X(008)         VALUE
           'EMITCHQ'.
       77  WRK-REG-EMITCHQ             PIC  X(1050)        VALUE SPACES.

       01  WRK-IO-PCB                  PIC  X(032)         VALUE SPACES.
       01  WRK-ALT-PCB                 PIC  X(032)         VALUE SPACES.

       01  WRK-CONTR-CGCCPF.
           03  WRK-CONTR-CGCCPF-ST     PIC  9(002)         VALUE ZEROS.

       01  WRK-BANCO-ATU-R.
           03  WRK-BANCO-ATU           PIC  9(003)         VALUE ZEROS.

       01  WRK-BANCO-ANT-R.
           03  WRK-BANCO-ANT           PIC  9(003)         VALUE ZEROS.

       01  WRK-CHV-ATU.
           03  WRK-BCO-ATU             PIC  9(003)         VALUE ZEROS.
           03  WRK-AGE-ATU             PIC  9(005)         VALUE ZEROS.
           03  WRK-CTA-ATU             PIC  9(013)         VALUE ZEROS.
           03  WRK-CHQ-ATU             PIC  9(007)         VALUE ZEROS.
           03  WRK-DIG-ATU             PIC  X(001)         VALUE ZEROS.
           03  WRK-NUM-ATU             PIC  9(007)         VALUE ZEROS.

       01  WRK-CHV-ANT.
           03  WRK-BCO-ANT             PIC  9(003)         VALUE ZEROS.
           03  WRK-AGE-ANT             PIC  9(005)         VALUE ZEROS.
           03  WRK-CTA-ANT             PIC  9(013)         VALUE ZEROS.
           03  WRK-CHQ-ANT             PIC  9(007)         VALUE ZEROS.
           03  WRK-DIG-ANT             PIC  X(001)         VALUE ZEROS.
           03  WRK-NUM-ANT             PIC  9(007)         VALUE ZEROS.

LOT    01  WRK-CHV-SLIG-CLIB-ATU.
LOT        03  WRK-BCO-SLIG-CLIB       PIC  9(003)         VALUE ZEROS.
LOT        03  WRK-AGE-SLIG-CLIB       PIC  9(005)         VALUE ZEROS.
LOT        03  WRK-CTA-SLIG-CLIB       PIC  9(013)         VALUE ZEROS.
LOT
LOT    01  WRK-CHV-SLIG-CLIB-ANT       PIC  X(021)         VALUE ZEROS.

       01  WRK-GUARDA.
           03  WRK-CGC-CPF-EMITT       PIC  X(009)         VALUE SPACES.
           03  WRK-FILIAL-EMITT        PIC  X(004)         VALUE SPACES.
           03  WRK-CONTROLE-EMITT      PIC  9(002) COMP-3  VALUE ZEROS.
           03  WRK-TIPO-PESSOA-EMITT   PIC  X(001)         VALUE SPACES.
           03  WRK-NOME-EMITT          PIC  X(040)         VALUE SPACES.
           03  WRK-BCO-EMITT           PIC  9(003) COMP-3  VALUE ZEROS.
           03  WRK-AGENCIA-EMITT       PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-CONTA-EMITT         PIC  9(013) COMP-3  VALUE ZEROS.
LOT        03  WRK-GRP-ECONOMICO-EMITT PIC  9(009) COMP-3  VALUE ZEROS.

DTSRA  01  WRK-QTDE-CHQ-BCO            PIC 9(015) COMP-3   VALUE ZEROS.

       01  WRK-CONTA-CLIE              PIC  9(013)         VALUE ZEROS.
       01  WRK-CC-CLIE  REDEFINES      WRK-CONTA-CLIE.
           03 FILLER                   PIC  9(007).
           03 WRK-CTA-CLIE             PIC  9(006).

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(053)         VALUE
           '*** MENSAGENS DE ERRO PARA BRAD0300 E BRAD0315 ***'.
      *---------------------------------------------------------------*

       01  WRK-ERRO-TEXTO.
           03  FILLER                  PIC  X(011)         VALUE
               '** ERRO NA '.
           03  WRK-NOME-POOL           PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(022)         VALUE
               ' NO ACESSO AO ARQUIVO '.
           03  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE
               ' - RETURN CODE = '.
           03  WRK-RETURN-CODE         PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(003)         VALUE
               ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MSG001              PIC  X(075)         VALUE
               '** ARQUIVO MVTCHQSR VAZIO **'.
           03  WRK-MSG-FORA-SEQ        PIC  X(075)         VALUE
               '** ARQUIVO MVTCHQSR FORA DE SEQUENCIA **'.

       01  WRK-ERRO-ARQUIVO.
           03  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQUIVO        PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE
               ' - FILE-STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAAAMMDD         PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC  9(013) COMP-3  VALUE ZEROS.
           03  WRK-TIMESTAMP.
               05  WRK-P7600-ANO       PIC  9(004)         VALUE ZEROS.
               05  WRK-P7600-MES       PIC  9(002)         VALUE ZEROS.
               05  WRK-P7600-DIA       PIC  9(002)         VALUE ZEROS.
               05  FILLER              PIC  X(012)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DEFINICAO DO RELATORIO - CABECALHOS ***'.
      *----------------------------------------------------------------*

       01  CABEC1.
           03  FILLER                  PIC  X(001)         VALUE '1'.
           03  FILLER                  PIC  X(023)         VALUE
               'BRADESCO'.
           03  FILLER                  PIC  X(047)         VALUE
               'SLIG - SISTEMA DE LIQUIDEZ GERAL'.
           03  FILLER                  PIC  X(010)         VALUE
               'FOL. UNICA'.

       01  CABEC2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(070)         VALUE
               'SLIG6211'.
           03  CB2-DIA                 PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-MES                 PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-ANO                 PIC  9(004)         VALUE ZEROS.

       01  CABEC3.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(018)         VALUE SPACES.
           03  FILLER                  PIC  X(043)         VALUE
               'UNIFICA ARQUIVOS DIARIA E DECENAL DE CHEQUE'.

       01  CABEC4.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  FILLER                  PIC  X(029)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'CONTROLE OPERACIONAL'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DEFINICAO DO RELATORIO - DETALHES ***'.
      *----------------------------------------------------------------*

       01  LINTOT1.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGISTROS LIDOS'.
           03  FILLER                  PIC  X(013)         VALUE
               '(MVTCHQSR) : '.
           03  LT1-TOT-LIDOS           PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT2.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGISTROS GRAVADOS'.
           03  FILLER                  PIC  X(013)         VALUE
               '(EMITCHEQ) : '.
           03  LT2-TOT-GRAVADOS        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

DTSGP  01  LINTOT4B.
DTSGP      03  FILLER                  PIC  X(001)         VALUE '0'.
DTSGP      03  FILLER                  PIC  X(010)         VALUE SPACES.
DTSGP      03  FILLER                  PIC  X(020)         VALUE
DTSGP          'REGISTROS GRAVADOS'.
DTSGP      03  FILLER                  PIC  X(013)         VALUE
DTSGP          '(QTDEBCO)  : '.
DTSGP      03  LT4B-TOT-GRAVADOS       PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT5.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGS N ENCONTRADOS'.
           03  FILLER                  PIC  X(013)         VALUE
               '(CLIB2000) : '.
           03  LT5-TOT-GRAVADOS        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT6.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGS COM ERROS    '.
           03  FILLER                  PIC  X(013)         VALUE
               '(CLIB2000) : '.
           03  LT6-TOT-GRAVADOS        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

LOT    01  LINTOT21.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL EMITENTES BCO '.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            '237        : '.
LOT        03  LT21-TOT-BCO-237        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT22.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL EMITENTES BCO '.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            'DIF. 237   : '.
LOT        03  LT22-TOT-BCO-DIF-237    PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT23.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL EMIT. CGC/CPF '.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            'ZEROS ENTR.: '.
LOT        03  LT23-TOT-CGC-ZEROS      PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT24.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL DIFERENTES DE '.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            'ZEROS ENTR.: '.
LOT        03  LT24-TOT-CGC-DIF-ZEROS  PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT25.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL DE ACESSOS AO '.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            'CLIB2000  .: '.
LOT        03  LT25-TOT-ACESS-CLIB2000 PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT26.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL EMITT CGC ATUA'.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            'LZ C/C    .: '.
LOT        03  LT26-TOT-CGC-ATUALZ-CC  PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT27.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL EMITT CGC ATUA'.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            'LZ POUP.  .: '.
LOT        03  LT27-TOT-CGC-ATUALZ-PP  PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT28.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL EMITT NOME ENC'.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            '. C/C     .: '.
LOT        03  LT28-TOT-NOME-ENC-CC    PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT29.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL EMITT NOME ENC'.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            '. POUP.   .: '.
LOT        03  LT29-TOT-NOME-ENC-PP    PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT30.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL EMITT NOME NAO'.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            ' ENCONTR. .: '.
LOT        03  LT30-TOT-NOME-N-ENC     PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT31.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL DE ACESSOS AO '.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            'GRUP0930  .: '.
LOT        03  LT31-TOT-ACESS-GRUP0930 PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT32.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL EMITT GRUPO EN'.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            'CONTR.    .: '.
LOT        03  LT32-TOT-GRP-ENC        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT33.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL EMITT GRUPO N '.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            'ENCONTR.  .: '.
LOT        03  LT33-TOT-GRP-N-ENC      PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT34.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL EMIT. CGC/CPF '.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            'ZEROS SAIDA: '.
LOT        03  LT34-TOT-CGC-ZEROS      PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT35.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL DIFERENTES DE '.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            'ZEROS SAIDA: '.
LOT        03  LT35-TOT-CGC-DIF-ZEROS  PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT36.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL EMITT NOME NAO'.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            ' ENC SAIDA.: '.
LOT        03  LT36-TOT-NOME-N-ENC     PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT37.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL EMITT NOME SPA'.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            'CES SAIDA .: '.
LOT        03  LT37-TOT-NOME-SPACES    PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT38.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL EMITT NOME OK '.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            'SAIDA     .: '.
LOT        03  LT38-TOT-NOME-OK        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT39.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL EMITT GRUPO ZE'.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            'ROS SAIDA .: '.
LOT        03  LT39-TOT-GRP-ZEROS      PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
LOT
LOT    01  LINTOT40.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(020)         VALUE
LOT            'TOTAL DIFERENTES DE '.
LOT        03  FILLER                  PIC  X(013)         VALUE
LOT            'ZEROS SAIDA: '.
LOT        03  LT40-TOT-GRP-DIF-ZEROS  PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

LOT    01  LINHIFEN.
LOT        03  FILLER                  PIC  X(001)         VALUE '0'.
LOT        03  FILLER                  PIC  X(010)         VALUE SPACES.
LOT        03  FILLER                  PIC  X(052)         VALUE
LOT            '----------------------------------------------------'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ARQUIVOS MVTCHQSR E EMITCHEQ ***'.
      *----------------------------------------------------------------*

-INC I#SLIGC0

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA RETORNO DO MODULO CLIB2000 ***'.
      *----------------------------------------------------------------*

       COPY CLIBW020.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO AO MODULO GRUP0930 ***'.
      *----------------------------------------------------------------*

       COPY GRUPW01S.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7100 ***'.
      *----------------------------------------------------------------*

-INC POL7100C

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6211 - FIM DA AREA DE WORKING ***'.
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
                   WRK-FS-MVTCHQSR     EQUAL '10'.

           PERFORM 400000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           OPEN OUTPUT SRELCTRL
DTSRA                  QTDEBCO.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 113000-TESTAR-FS-SRELCTRL.

DTSRA      PERFORM 118000-TESTAR-FS-QTDEBCO.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       113000-TESTAR-FS-SRELCTRL       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SRELCTRL         NOT EQUAL '00'
               MOVE 'SRELCTRL'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-SRELCTRL   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       113000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
DTSRA *----------------------------------------------------------------*
DTSRA  118000-TESTAR-FS-QTDEBCO        SECTION.
DTSRA *----------------------------------------------------------------*
DTSRA
DTSRA      IF  WRK-FS-QTDEBCO          NOT EQUAL '00'
DTSRA          MOVE 'QTDEBCO'          TO WRK-NOME-ARQUIVO
DTSRA          MOVE  WRK-FS-QTDEBCO    TO WRK-FILE-STATUS
DTSRA          MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
DTSRA          PERFORM 999999-PROCESSAR-ROTINA-ERRO
DTSRA      END-IF.
DTSRA
DTSRA *----------------------------------------------------------------*
DTSRA  118000-99-FIM.                  EXIT.
DTSRA *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-LER-MVTCHQSR.

           IF  WRK-FS-MVTCHQSR         EQUAL '10'
               DISPLAY '***************** SLIG6211 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO MVTCHQSR VAZIO          *'
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO  CANCELADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** SLIG6211 *****************'
               MOVE WRK-MSG001         TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           ELSE
               MOVE WRK-BANCO-ATU      TO WRK-BANCO-ANT
           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-LER-MVTCHQSR             SECTION.
      *----------------------------------------------------------------*

           MOVE 'MVTCHQSR'             TO WRK-ARQUIVO.

           CALL 'BRAD0300'             USING WRK-ARQUIVO
                                             REG-CHEQUE-OCOR-HISTORICA.

           IF  RETURN-CODE             EQUAL 04
               MOVE '10'               TO WRK-FS-MVTCHQSR
               GO TO 210000-99-FIM
           END-IF.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'BRAD0300'         TO WRK-NOME-POOL
               MOVE 'MVTCHQSR'         TO WRK-NOME-ARQ
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 212000-EMITIR-DISPLAY-ERRO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE SLIGC0-BCO-EMITT       TO WRK-BANCO-ATU
LOT                                       WRK-BCO-SLIG-CLIB
                                          WRK-BCO-ATU.
           MOVE SLIGC0-AGENCIA-EMITT   TO WRK-AGE-ATU
LOT                                       WRK-AGE-SLIG-CLIB.
           MOVE SLIGC0-CONTA-EMITT     TO WRK-CTA-ATU
LOT                                       WRK-CTA-SLIG-CLIB.
           MOVE SLIGC0-NUM-CHEQUE      TO WRK-CHQ-ATU.
           MOVE SLIGC0-DIG-CHEQUE      TO WRK-DIG-ATU.
           MOVE SLIGC0-NUM-LOTE        TO WRK-NUM-ATU.

           PERFORM 211000-TESTAR-SEQUENCIA.

           ADD 1                       TO ACU-LIDOS-MVTCHQSR.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       212000-EMITIR-DISPLAY-ERRO      SECTION.
      *---------------------------------------------------------------*

           DISPLAY '****************** SLIG6211 ******************'.
           DISPLAY '*                                            *'.
           DISPLAY '*         NOME DO ARQUIVO = ' WRK-NOME-ARQ '
      -            ' *'.
           DISPLAY '*                                            *'.
           DISPLAY '*              RETURN CODE = ' WRK-RETURN-CODE '
      -            '          *'.
           DISPLAY '*                                            *'.
           DISPLAY '*          PROCESSAMENTO  CANCELADO          *'.
           DISPLAY '*                                            *'.
           DISPLAY '****************** SLIG6211 ******************'.

      *---------------------------------------------------------------*
       212000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       211000-TESTAR-SEQUENCIA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-ATU             LESS WRK-CHV-ANT
               DISPLAY '***************** SLIG6211 *******************'
               DISPLAY '*                                            *'
               DISPLAY '*         PROCESSAMENTO INTERROMPIDO         *'
               DISPLAY '*                                            *'
               DISPLAY '*     ARQUIVO MVTCHQSR FORA DE SEQUENCIA     *'
               DISPLAY '*                                            *'
               DISPLAY '*     CHAVE ANTERIOR:                        *'
               DISPLAY '*                                            *'
               DISPLAY '*     BCO-EMITT       : ' WRK-BCO-ANT
                                                  '                  *'
               DISPLAY '*     AGENCIA-EMITT   : ' WRK-AGE-ANT
                                                    '                *'
               DISPLAY '*     CONTA-EMITT     : ' WRK-CTA-ANT
                                                            '        *'
               DISPLAY '*     CHEQUE-EMITT    : ' WRK-CHQ-ANT
                                                      '              *'
               DISPLAY '*                                            *'
               DISPLAY '*     CHAVE ATUAL:                           *'
               DISPLAY '*                                            *'
               DISPLAY '*     BCO-EMITT       : ' WRK-BCO-ATU
                                                  '                  *'
               DISPLAY '*     AGENCIA-EMITT   : ' WRK-AGE-ATU
                                                    '                *'
               DISPLAY '*     CONTA-EMITT     : ' WRK-CTA-ATU
                                                            '        *'
               DISPLAY '*     CHEQUE-EMITT    : ' WRK-CHQ-ATU
                                                      '              *'
               DISPLAY '*                                            *'
               DISPLAY '****************** SLIG6211 ******************'
               MOVE  WRK-MSG-FORA-SEQ  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-CHV-ATU            TO WRK-CHV-ANT.

      *----------------------------------------------------------------*
       211000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 310000-GUARDAR-CAMPOS.

DTSRA      IF WRK-BANCO-ATU-R          NOT EQUAL WRK-BANCO-ANT-R
DTSRA         PERFORM 350000-GRAVAR-QTDEBCO
              MOVE WRK-BANCO-ATU       TO WRK-BANCO-ANT
DTSRA         MOVE ZEROS               TO WRK-QTDE-CHQ-BCO
DTSRA      END-IF.

           PERFORM 320000-GRAVAR-EMITCHEQ.

           ADD 1                       TO ACU-GRAVA-EMITCHEQ
                                          WRK-QTDE-CHQ-BCO.

           PERFORM 210000-LER-MVTCHQSR.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       310000-GUARDAR-CAMPOS           SECTION.
      *----------------------------------------------------------------*

           MOVE SLIGC0-CGC-CPF-EMITT-ST
           TO WRK-CGC-CPF-EMITT.
           MOVE SLIGC0-FILIAL-EMITT-ST
           TO WRK-FILIAL-EMITT.
           MOVE SLIGC0-CONTROLE-EMITT-ST
           TO WRK-CONTROLE-EMITT.
           MOVE SLIGC0-BCO-EMITT         TO WRK-BCO-EMITT.
           MOVE SLIGC0-AGENCIA-EMITT     TO WRK-AGENCIA-EMITT.
           MOVE SLIGC0-CONTA-EMITT       TO WRK-CONTA-EMITT.

LOT   *--  ACUMULA TOTAL DE EMITENTES BANCO 237 E DIFERENTE DE 237  ---*
LOT
LOT        IF WRK-BCO-EMITT      EQUAL 237
LOT           ADD 1 TO ACU-EMITT-BCO-237
LOT        ELSE
LOT           ADD 1 TO ACU-EMITT-BCO-DIF-237
LOT        END-IF.
LOT
LOT   *--  ACUMULA TOTAL DE EMITENTES CGC ZERADOS NA ENTRADA  ---------*
LOT
LOT        IF WRK-CGC-CPF-EMITT EQUAL ZEROS OR SPACES
LOT           ADD 1 TO ACU-EMITT-CGC-ZEROS-IN
LOT        ELSE
LOT           ADD 1 TO ACU-EMITT-CGC-DIF-ZEROS-IN
LOT        END-IF.
LOT
LOT        IF WRK-CHV-SLIG-CLIB-ATU NOT EQUAL
LOT           WRK-CHV-SLIG-CLIB-ANT
LOT
LOT   *-----  BUSCA NOME DO EMITENTE E CNPJ/CPF  ----------------------*
LOT
LOT           IF WRK-BCO-EMITT      EQUAL 237
LOT              PERFORM 321000-OBTER-DADOS-EMITENTE
LOT           ELSE
LOT              MOVE SPACES        TO WRK-NOME-EMITT
LOT           END-IF
LOT
LOT   *-----  FORMATA TIPO DE PESSOA EMITENTE  ------------------------*
LOT
LOT           IF (WRK-CGC-CPF-EMITT NOT EQUAL ZEROS AND SPACES) AND
LOT              (WRK-FILIAL-EMITT      EQUAL ZEROS OR SPACES)
LOT              MOVE 'F'          TO WRK-TIPO-PESSOA-EMITT
LOT           ELSE
LOT              MOVE 'J'          TO WRK-TIPO-PESSOA-EMITT
LOT           END-IF
LOT
LOT   *-----  BUSCA GRUPO ECONOMICO DO EMITENTE  ----------------------*
LOT
LOT           IF WRK-CGC-CPF-EMITT NOT EQUAL ZEROS AND SPACES
LOT              PERFORM 326000-OBTER-GRUPO-EMITENTE
LOT           ELSE
LOT              MOVE ZEROS      TO WRK-GRP-ECONOMICO-EMITT
LOT           END-IF
LOT
LOT   *-----  FORMATA CHAVE SLIG CLIB ANTERIOR  -----------------------*
LOT
LOT           MOVE WRK-CHV-SLIG-CLIB-ATU TO  WRK-CHV-SLIG-CLIB-ANT
LOT
LOT        END-IF.

      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       321000-OBTER-DADOS-EMITENTE     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ECLIB-AREA-CLIBW020
                      SCLIB-AREA-CLIBW020.

LOT   *    MOVE SLIG28-BCO-EMITT        TO WRK-CLIB-BANCO.
LOT   *    MOVE SLIG28-AGENCIA-EMITT    TO WRK-CLIB-AGENCIA.
LOT   *    MOVE SLIG28-CONTA-EMITT      TO WRK-CONTA-CLIE.

LOT        MOVE WRK-BCO-EMITT           TO ECLIB-BANCO.
LOT        MOVE WRK-AGENCIA-EMITT       TO ECLIB-AGENCIA.
LOT        MOVE WRK-CONTA-EMITT         TO WRK-CONTA-CLIE.
LOT        MOVE WRK-CTA-CLIE            TO ECLIB-CONTA.

           MOVE    2                    TO WRK-FLAG
LOT        ADD 1 TO ACU-ACESSOS-CLIB2000.

           PERFORM 321100-ACESSAR-CLIB2000.

           IF  SCLIB-RC          EQUAL ZEROS
               IF  SCLIB-TIPO-CONTA-C-P EQUAL 'C'
LOT   *            MOVE NOME            TO SLIG28-NOME-EMITT
LOT                MOVE NOME            TO WRK-NOME-EMITT
LOT                ADD 1                TO ACU-EMITT-NOME-ENC-CC
LOT   *            IF SLIG28-CGC-CPF-EMITT EQUAL ZEROS
LOT                IF WRK-CGC-CPF-EMITT EQUAL ZEROS OR SPACES
LOT   *               MOVE NUM-CGCCPF    TO SLIG28-CGC-CPF-EMITT
LOT   *               MOVE FILIAL-CGCCPF TO SLIG28-FILIAL-EMITT
LOT   *               MOVE CONTR-CGCCPF  TO SLIG28-CONTROLE-EMITT
LOT                   MOVE NUM-CGCCPF-ST    TO WRK-CGC-CPF-EMITT
LOT                   MOVE FILIAL-CGCCPF-ST TO WRK-FILIAL-EMITT
LOT                   MOVE CONTR-CGCCPF-ST  TO WRK-CONTR-CGCCPF
LOT                   MOVE WRK-CONTR-CGCCPF-ST TO WRK-CONTROLE-EMITT

LOT                   ADD 1 TO ACU-EMITT-CGC-ATUALZ-CC
                   END-IF
               ELSE
LOT   *            MOVE POUPADOR-NOME-CLIE TO SLIG28-NOME-EMITT
LOT                MOVE POUPADOR-NOME-CLIE TO WRK-NOME-EMITT
LOT                ADD 1                   TO ACU-EMITT-NOME-ENC-PP
LOT   *            IF SLIG28-CGC-CPF-EMITT EQUAL ZEROS
LOT                IF WRK-CGC-CPF-EMITT EQUAL ZEROS OR SPACES
LOT   *               MOVE POUPADOR-CGC-CPF  TO SLIG28-CGC-CPF-EMITT
LOT   *               MOVE POUPADOR-FILIAL   TO SLIG28-FILIAL-EMITT
LOT   *               MOVE POUPADOR-CONTROLE TO SLIG28-CONTROLE-EMITT
LOT                   MOVE POUPADOR-CGC-ST    TO WRK-CGC-CPF-EMITT
LOT                   MOVE POUPADOR-FILIAL-ST TO WRK-FILIAL-EMITT
LOT                   MOVE POUPADOR-CONTR-ST  TO WRK-CONTR-CGCCPF
LOT                   MOVE WRK-CONTR-CGCCPF-ST TO WRK-CONTROLE-EMITT

LOT                   ADD 1 TO ACU-EMITT-CGC-ATUALZ-PP

                   END-IF
               END-IF
           ELSE
LOT   *        MOVE 'EMITENTE NAO ENCONTRADO'    TO SLIG28-NOME-EMITT
LOT            MOVE 'EMITENTE NAO ENCONTRADO'    TO WRK-NOME-EMITT
LOT            ADD 1                             TO ACU-EMITT-NOME-N-ENC
           END-IF.

      *----------------------------------------------------------------*
       321000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       321100-ACESSAR-CLIB2000         SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO ECLIB-TRANSACAO.
           MOVE 'VRS000'               TO ECLIB-VERSAO.
           MOVE 1                      TO ECLIB-PRODUTO.
           MOVE 'CLIBA100'             TO WRK-MODULO.

           CALL WRK-MODULO             USING ECLIB-AREA-CLIBW020
                                             SCLIB-AREA-CLIBW020.

           IF  SCLIB-RC    NOT EQUAL ZEROS
               PERFORM 321200-TRATA-RETORNO-NAO-OK
           END-IF.

      *----------------------------------------------------------------*
       321100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       321200-TRATA-RETORNO-NAO-OK     SECTION.
      *----------------------------------------------------------------*

           IF  SCLIB-RC         EQUAL 01
               ADD  1                  TO    ACU-N-ENC-CLIB2000

               IF ACU-N-ENC-CLIB2000   LESS  101
                  DISPLAY 'FLAG CLIENTE/EMITENTE : ' WRK-FLAG
                  DISPLAY
                  '%% CLIB2000 %%    BCO-AG/CTA NAO ENCONTRADA: '
                  ECLIB-BANCO '-' ECLIB-AGENCIA '/' ECLIB-CONTA
               END-IF
           ELSE
               ADD  1                  TO    ACU-ERROR-CLIB2000

               IF ACU-ERROR-CLIB2000   LESS  101
                  MOVE SCLIB-AREA-CLIBW020(1:75)  TO ERR-TEXTO
                  DISPLAY 'FLAG CLIENTE/EMITENTE : ' WRK-FLAG

                  DISPLAY '%% CLIB2000 %%    BCO-AG/CTA: '
                  ECLIB-BANCO '-' ECLIB-AGENCIA '/' ECLIB-CONTA

                  DISPLAY '%% CLIB2000 %%    RET-CODE  : '
                          SCLIB-RC    '-' ERR-TEXTO

               END-IF
           END-IF.

      *----------------------------------------------------------------*
       321200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       325100-ACESSAR-GRUP0930         SECTION.
      *----------------------------------------------------------------*

           MOVE 'SLIG6211'             TO GRUPW01S-PGM-CHAMADOR.
           MOVE 'GRUP201C'             TO WRK-MODULO.
           MOVE SPACES                 TO WRK-IO-PCB
                                          WRK-ALT-PCB.

           CALL WRK-MODULO             USING GRUPW01S-REG-EMPRESA
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.


      *----------------------------------------------------------------*
       325100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       326000-OBTER-GRUPO-EMITENTE     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GRUPW01S-REG-EMPRESA.
           INITIALIZE                     GRUPW01S-REG-EMPRESA.

LOT   *    MOVE SLIG28-CGC-CPF-EMITT   TO CGC-CPF-GRUPO-GP34.
LOT   *    MOVE SLIG28-FILIAL-EMITT    TO FILIAL-CGC-CPF-GRUPO-GP34.
LOT   *    MOVE SLIG28-CONTROLE-EMITT  TO CTRL-CGC-CPF-GRUPO-GP34.
LOT        MOVE '00'                   TO GRUPW01S-CGC-CPF(1:2).
LOT        MOVE WRK-CGC-CPF-EMITT      TO GRUPW01S-CGC-CPF(3:9).
LOT        MOVE WRK-FILIAL-EMITT       TO GRUPW01S-FILIAL-CGC-CPF.
LOT        MOVE WRK-CONTROLE-EMITT     TO GRUPW01S-CTRL-CGC-CPF.

           MOVE ZEROS                  TO GRUPW01S-NUMERO-GRUPO-ENV.

           PERFORM 325100-ACESSAR-GRUP0930.

LOT        ADD 1 TO ACU-ACESSOS-GRUP0930.

           IF  GRUPW01S-RETURN-CODE EQUAL ZEROS
LOT   *        MOVE NUMERO-GRUPO-RET-GP34 TO SLIG28-GRP-ECONOMICO-EMITT
LOT            MOVE GRUPW01S-RETURN-CODE
LOT                                       TO WRK-GRP-ECONOMICO-EMITT
LOT            ADD 1 TO ACU-EMITT-GRP-ENC
           ELSE
LOT   *        MOVE ZEROS                 TO SLIG28-GRP-ECONOMICO-EMITT
LOT            MOVE ZEROS                 TO WRK-GRP-ECONOMICO-EMITT
LOT            ADD 1 TO ACU-EMITT-GRP-N-ENC
           END-IF.

      *----------------------------------------------------------------*
       326000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       320000-GRAVAR-EMITCHEQ          SECTION.
      *----------------------------------------------------------------*

      *--- FORMATA CPF EMITENTE SAIDA E TIPO DE PESSOA ----------------*

           IF  WRK-CGC-CPF-EMITT       NOT EQUAL ZEROS AND SPACES
               MOVE WRK-CGC-CPF-EMITT  TO SLIGC0-CGC-CPF-EMITT-ST
               MOVE WRK-FILIAL-EMITT   TO SLIGC0-FILIAL-EMITT-ST
               MOVE WRK-CONTROLE-EMITT TO SLIGC0-CONTROLE-EMITT-ST
               MOVE WRK-TIPO-PESSOA-EMITT TO SLIGC0-TIPO-PESSOA-EMITT
           END-IF.

      *--- CONTABILIZA EMITENTE NA SAIDA -----------------------------*

           IF SLIGC0-CGC-CPF-EMITT-ST
           EQUAL ZEROS OR SPACES
               MOVE 'J'         TO SLIGC0-TIPO-PESSOA-EMITT
               ADD 1            TO ACU-EMITT-CGC-ZEROS-OUT
           ELSE
               ADD 1            TO ACU-EMITT-CGC-DIF-ZEROS-OUT
           END-IF.

      *--- CONTABILIZA NOME EMITENTE NA SAIDA E FORMATA NOME ----------*

           IF WRK-NOME-EMITT          NOT EQUAL SPACES
              MOVE WRK-NOME-EMITT     TO SLIGC0-NOME-EMITT

              IF WRK-NOME-EMITT EQUAL 'EMITENTE NAO ENCONTRADO'
                 ADD 1                    TO ACU-EMITT-NOME-N-ENC-OUT
              ELSE
                 ADD 1                   TO ACU-EMITT-NOME-OK-OUT
              END-IF

           ELSE
               ADD 1                   TO ACU-EMITT-NOME-SPACES-OUT
           END-IF.

      *--- CONTABILIZA GRUPO EMITENTE NA SAIDA E FORMATA GRUPO --------*

           IF  WRK-GRP-ECONOMICO-EMITT NOT EQUAL ZEROS
               MOVE WRK-GRP-ECONOMICO-EMITT
                                       TO SLIGC0-GRP-ECONOMICO-EMITT
               ADD 1                   TO ACU-EMITT-GRP-DIF-ZEROS-OUT
           ELSE
               ADD 1                   TO ACU-EMITT-GRP-ZEROS-OUT
           END-IF.

           MOVE  REG-CHEQUE-OCOR-HISTORICA TO  WRK-REG-EMITCHQ.

           CALL 'BRAD0315'             USING WRK-SAI-EMITCHQ
                                             WRK-REG-EMITCHQ.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'BRAD0315'         TO WRK-NOME-POOL
               MOVE 'EMITCHEQ'         TO WRK-NOME-ARQ
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 212000-EMITIR-DISPLAY-ERRO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       320000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
DTSRA *----------------------------------------------------------------*
DTSRA  350000-GRAVAR-QTDEBCO           SECTION.
DTSRA *----------------------------------------------------------------*
DTSRA
DTSRA      INITIALIZE REG-QTDEBCO.
DTSRA
DTSRA *--- DADOS DE STATUS DO CHEQUE ----------------------------------*
DTSRA
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
DTSRA      MOVE WRK-BANCO-ANT          TO REG-QTDEBCO-CODBCO.
DTSRA      MOVE WRK-QTDE-CHQ-BCO       TO REG-QTDEBCO-QTDE.
DTSRA
DTSRA      WRITE REG-QTDEBCO.
DTSRA      PERFORM 118000-TESTAR-FS-QTDEBCO
DTSRA      ADD   1                     TO  ACU-GRAVA-QTDEBCO.
DTSRA
DTSRA *----------------------------------------------------------------*
DTSRA  350000-99-FIM.                  EXIT.
DTSRA *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       400000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 350000-GRAVAR-QTDEBCO.

           PERFORM 410000-EMITIR-SRELCTRL.

           CALL 'FECHAARQ'             USING   WRK-SAI-EMITCHQ.

           CLOSE SRELCTRL
DTSRA            QTDEBCO.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

           GOBACK.

      *----------------------------------------------------------------*
       400000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       410000-EMITIR-SRELCTRL          SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL7600'             USING WRK-DATA-HORA.

      *--- MOVE DATAS PARA O CABECALHO --------------------------------*

           MOVE WRK-P7600-DIA          TO CB2-DIA.
           MOVE WRK-P7600-MES          TO CB2-MES.
           MOVE WRK-P7600-ANO          TO CB2-ANO.

      *--- MOVE QUANTIDADE DE LIDOS E GRAVADOS ------------------------*

           MOVE ACU-LIDOS-MVTCHQSR     TO LT1-TOT-LIDOS.
           MOVE ACU-GRAVA-EMITCHEQ     TO LT2-TOT-GRAVADOS.
DTSRA      MOVE ACU-GRAVA-QTDEBCO      TO LT4B-TOT-GRAVADOS.

      *--- MOVER QUANTIDADE ERRO E NAO ENCONTRADOS NO CLIB2000 --------*

           MOVE ACU-N-ENC-CLIB2000     TO LT5-TOT-GRAVADOS.
           MOVE ACU-ERROR-CLIB2000     TO LT6-TOT-GRAVADOS.

LOT        MOVE ACU-EMITT-BCO-237              TO
LOT             LT21-TOT-BCO-237.

LOT        MOVE ACU-EMITT-BCO-DIF-237          TO
LOT             LT22-TOT-BCO-DIF-237.

LOT        MOVE ACU-EMITT-CGC-ZEROS-IN         TO
LOT             LT23-TOT-CGC-ZEROS.

LOT        MOVE ACU-EMITT-CGC-DIF-ZEROS-IN     TO
LOT             LT24-TOT-CGC-DIF-ZEROS.

LOT        MOVE ACU-ACESSOS-CLIB2000           TO
LOT            LT25-TOT-ACESS-CLIB2000.

LOT        MOVE ACU-EMITT-CGC-ATUALZ-CC        TO
LOT             LT26-TOT-CGC-ATUALZ-CC.

LOT        MOVE ACU-EMITT-CGC-ATUALZ-PP        TO
LOT             LT27-TOT-CGC-ATUALZ-PP.

LOT        MOVE ACU-EMITT-NOME-ENC-CC          TO
LOT             LT28-TOT-NOME-ENC-CC.

LOT        MOVE ACU-EMITT-NOME-ENC-PP          TO
LOT             LT29-TOT-NOME-ENC-PP.

LOT        MOVE ACU-EMITT-NOME-N-ENC           TO
LOT             LT30-TOT-NOME-N-ENC.

LOT        MOVE ACU-ACESSOS-GRUP0930           TO
LOT             LT31-TOT-ACESS-GRUP0930.

LOT        MOVE ACU-EMITT-GRP-ENC              TO
LOT             LT32-TOT-GRP-ENC.

LOT        MOVE ACU-EMITT-GRP-N-ENC            TO
LOT             LT33-TOT-GRP-N-ENC.

LOT        MOVE ACU-EMITT-CGC-ZEROS-OUT        TO
LOT             LT34-TOT-CGC-ZEROS.

LOT        MOVE ACU-EMITT-CGC-DIF-ZEROS-OUT    TO
LOT             LT35-TOT-CGC-DIF-ZEROS.

LOT        MOVE ACU-EMITT-NOME-N-ENC-OUT       TO
LOT             LT36-TOT-NOME-N-ENC.

LOT        MOVE ACU-EMITT-NOME-SPACES-OUT      TO
LOT             LT37-TOT-NOME-SPACES.

LOT        MOVE ACU-EMITT-NOME-OK-OUT          TO
LOT             LT38-TOT-NOME-OK.

LOT        MOVE ACU-EMITT-GRP-ZEROS-OUT        TO
LOT             LT39-TOT-GRP-ZEROS.

LOT        MOVE ACU-EMITT-GRP-DIF-ZEROS-OUT    TO
LOT             LT40-TOT-GRP-DIF-ZEROS.

      *--- GRAVA RELATORIO DE CONTROLE - SRELCTRL ---------------------*

           WRITE REG-SRELCTRL          FROM CABEC1.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC2.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC3.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC4.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINHIFEN.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT1.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT2.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

DTSRA      WRITE REG-SRELCTRL          FROM LINTOT4B.

DTSRA      PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINHIFEN.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT25.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT5.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT6.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT28.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT29.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT30.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT36.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT37.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT38.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINHIFEN.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT21.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT22.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINHIFEN.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT23.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT24.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT26.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT27.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT34.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT35.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINHIFEN.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT31.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT32.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT33.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT39.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT        WRITE REG-SRELCTRL          FROM LINTOT40.

LOT        PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINHIFEN.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT   *    WRITE REG-SRELCTRL          FROM LINTOT5.

LOT   *    PERFORM 113000-TESTAR-FS-SRELCTRL.

LOT   *    WRITE REG-SRELCTRL          FROM LINTOT6.

LOT   *    PERFORM 113000-TESTAR-FS-SRELCTRL.


      *----------------------------------------------------------------*
       410000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'SLIG6211'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
