      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. SLIG6210.
       AUTHOR.     FABIO AUGUSTO FINK.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   SLIG6210                                    *
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
      *                    ARQDATAS                    I#SLIG00        *
      *                    MVTCHQSR                    I#SLIG27        *
      *                    EMITCHEQ                    I#SLIG28        *
      *                    MVCHQREL                    I#SLIG74        *
      *                    SRELCTRL                       -            *
      *                    MVCHDESP                    = INC I#SLIG28  *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GRUP34 - AREA PARA ACESSO AO MODULO GRUP201C              *
      *    I#MESUM4 - AREA PARA ACESSO AO MODULO MESU9014              *
      *    CLIBW020 - AREA PARA RETORNO DO MODULO CLIB2000             *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    CLIB2000 - ACESSO AS BASES CLIE/MULTIBANCO                  *
      *    MESU9014 - OBTER DIRETORIA REGIONAL                         *
      *    MESU9023 - OBTER GERENCIA REGIONAL                          *
      *    GRUP201C - CONSULTA DE EMPRESAS                             *
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
      *                    A L T E R A C A O                           *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:   GLAUCO PADOA              - DTS             *
      *    ANALISTA....:   SOLANGE BARBEIRO          - DTS / GP. 75    *
      *    DATA........:   21/02/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   SEPARAR O REGISTRO DA INC28 QUANDO O ULTIMO *
      *                    MOTIVO DE DEVOLUCAO CONSTAR DA LISTA DE     *
      *                    CODIGOS FORNECIDOS.                         *
      *                    FOI CRIADO NOVO DDNAME "MVCHDESP" PARA      *
      *                    GUARDAR ESTES REGISTROS E POR HORA ESTE     *
      *                    ARQUIVO NAO ESTA SENDO UTILIZADO PARA       *
      *                    NENHUM OUTRO PROCESSO.                      *
      *                    OBS : ESTE ARQUIVO USA A MESO LAYOUT DA     *
      *                          INC I#SLIG28, MAS NAO USA A INCLUDE.  *
      *================================================================*
      *                    A L T E R A C A O                           *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:   RODRIGO AUGUSTO           - DTS             *
      *    ANALISTA....:   SOLANGE BARBEIRO          - DTS / GP. 75    *
      *    DATA........:   23/03/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   FOI CRIADO NOVO DDNAME "QTDEBCO" PARA       *
      *                    CALCULAR QUANTIDADE DE CHEQUES POR BANCOS   *
      *================================================================*
      *================================================================*
      *                    A L T E R A C A O                           *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:   KINJI SASAO               - DTS             *
      *    ANALISTA....:   REINALDO PEDROSO DE AQUINO- DTS / GP. 75    *
      *    DATA........:   24/10/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INCLUIR ACESSO A TABELA SLIGB058/CHSFB026   *
      *                    PARA IDENTIFICAR REGISTROS DE CHEQUE DE-    *
      *                    VOLVIDO A SER DESPREZADO                    *
      *                    GERAR ARQUIVO MOTDEVOL INC I#SLIGC1         *
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

           SELECT ARQDATAS ASSIGN      TO UT-S-ARQDATAS
           FILE STATUS                 IS WRK-FS-ARQDATAS.

           SELECT MVTCHQSR ASSIGN      TO UT-S-MVTCHQSR
           FILE STATUS                 IS WRK-FS-MVTCHQSR.

           SELECT EMITCHEQ ASSIGN      TO UT-S-EMITCHEQ
           FILE STATUS                 IS WRK-FS-EMITCHEQ.

           SELECT SRELCTRL ASSIGN      TO UT-S-SRELCTRL
           FILE STATUS                 IS WRK-FS-SRELCTRL.

           SELECT ARQERROS ASSIGN      TO UT-S-ARQERROS
           FILE STATUS                 IS WRK-FS-ARQERROS.

           SELECT MVCHQREL ASSIGN      TO UT-S-MVCHQREL
           FILE STATUS                 IS WRK-FS-MVCHQREL.

           SELECT MVCHDESP ASSIGN      TO UT-S-MVCHDESP
           FILE STATUS                 IS WRK-FS-MVCHDESP.

           SELECT QTDEBCO  ASSIGN      TO UT-S-QTDEBCO
           FILE STATUS                 IS WRK-FS-QTDEBCO.

           SELECT MOTDEVOL ASSIGN      TO UT-S-MOTDEVOL
           FILE STATUS                 IS WRK-FS-MOTDEVOL.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE DATAS.                                   *
      *            ORG. SEQUENCIAL     -   LRECL = 100                 *
      *----------------------------------------------------------------*

       FD  ARQDATAS
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG00

           EJECT
      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO DE MOVIMENTO DIARIO E DECENAL DE CHEQUE     *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  MVTCHQSR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG27
           EJECT
      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE MOVIMENTO DIARIO E DECENAL DE CHEQUE     *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  EMITCHEQ
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG28

           EJECT
      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE MOVIMENTO DIARIO E DECENAL DE CHEQUE     *
      *            PARA RELATORIO                                      *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  MVCHQREL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG74

           EJECT
      *----------------------------------------------------------------*
      *    OUTPUT: RELATORIO DE CONTROLE OPERACIONAL                   *
      *            ORG. SEQUENCIAL     -    LRECL   =   081            *
      *----------------------------------------------------------------*

       FD  SRELCTRL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-SRELCTRL                PIC  X(081).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE ERROS - SITUACAO NAO PREVISTA            *
      *            ORG. SEQUENCIAL     -    LRECL   =   080            *
      *----------------------------------------------------------------*

       FD  ARQERROS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-ERROS.
           05  SLIG99-CHV-EMITENTE     PIC  X(028).
           05  SLIG99-CTPO-CHEQ        PIC  9(003).
           05  SLIG99-DT-DEPOSITO      PIC  9(008).
           05  SLIG99-DT-CONTABIL      PIC  9(008).
           05  FILLER                  PIC  X(033).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE MOVIMENTO DE DEVOLUCAO DESPREZADOS       *
      *            USA O MESMO LAYOUT DA INC I#SLIG28                  *
      *            ORG. SEQUENCIAL     -    LRECL   =   300            *
      *----------------------------------------------------------------*

       FD  MVCHDESP
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

      * I#SLIG28 (ESTA AQUI SOMENTE PARA EFEITO DE PESQUISA - O LAYOUT
      *           EH O MESMO)

       01  REG-CHEQUE-EMITENTE-DESPREZADO PIC X(300).


      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO COM QUANTIDADE DE CHEQUES POR BANCO         *
      *            ORG. SEQUENCIAL     -    LRECL   =   022            *
      *----------------------------------------------------------------*

       FD  QTDEBCO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-QTDEBCO.
           05  REG-QTDEBCO-CODBCO      PIC S9(003) COMP-3.
           05  REG-QTDEBCO-QTDE        PIC S9(015) COMP-3.
           05  FILLER                  PIC  X(012).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO COM MOTIVOS DE DEVOLUCAO DE CHEQUE          *
      *            ORG. SEQUENCIAL     -    LRECL   =   010            *
      *----------------------------------------------------------------*

       FD  MOTDEVOL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#SLIGC1'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6210 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-ARQDATAS          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-MVTCHQSR          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-EMITCHEQ          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-MVCHQREL          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-ARQERROS          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-MVCHDESP          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-QTDEBCO           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-MOTDEVOL          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-N-ENC-CLIB2000          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-ERROR-CLIB2000          PIC  9(009) COMP-3  VALUE ZEROS.
       77  WRK-FLAG                    PIC  9(001)         VALUE ZEROS.
       77  WRK-DEV-ANT-DESCONS         PIC  9(009) COMP-3  VALUE ZEROS.
       77  WRK-DEV-PER-DESCONS         PIC  9(009) COMP-3  VALUE ZEROS.
       77  WRK-DEV-FUT-DESCONS         PIC  9(009) COMP-3  VALUE ZEROS.
       77  WRK-RESP-ULTIMA-DEVOLUCAO   PIC  X(001)         VALUE SPACES.
       77  WRK-REAP-ULTIMA-DEVOLUCAO   PIC  X(001)         VALUE SPACES.
PHM    77  WRK-ACHOU                   PIC  X(001)         VALUE 'N'.
       77  WRK-FIM-CURSOR              PIC  X(001)         VALUE 'N'.
       77  WRK-IND                     PIC  9(003)         VALUE ZEROS.
       77  ACU-EMITT-BCO-237           PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMITT-BCO-DIF-237       PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMITT-CGC-ZEROS-IN      PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMITT-CGC-DIF-ZEROS-IN  PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-ACESSOS-CLIB2000        PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMITT-CGC-ATUALZ-CC     PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMITT-CGC-ATUALZ-PP     PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMITT-NOME-ENC-CC       PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMITT-NOME-ENC-PP       PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMITT-NOME-N-ENC        PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-ACESSOS-GRUP201C        PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMITT-GRP-ENC           PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMITT-GRP-N-ENC         PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMITT-CGC-ZEROS-OUT     PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMITT-CGC-DIF-ZEROS-OUT PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMITT-NOME-N-ENC-OUT    PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMITT-NOME-SPACES-OUT   PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMITT-NOME-OK-OUT       PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMITT-GRP-ZEROS-OUT     PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-EMITT-GRP-DIF-ZEROS-OUT PIC  9(015) COMP-3  VALUE ZEROS.
       77  WRK-ULTIMA-DEVOLUCAO        PIC  9(008)         VALUE ZEROS.
       77  WRK-DT-MOVIMENTO-ULT        PIC  9(008)         VALUE ZEROS.
       77  WRK-DT-CONTABIL-ULT         PIC  9(008)         VALUE ZEROS.
       77  WRK-DT-SITUACAO-ULT         PIC  9(008)         VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE FILE-STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-ARQDATAS             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-MVTCHQSR             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-EMITCHEQ             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SRELCTRL             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQERROS             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-MVCHQREL             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-MVCHDESP             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-QTDEBCO              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-MOTDEVOL             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

           EJECT
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       01  WRK-CGCCPF.
           03 WRK-CGCCPF-ST            PIC  9(002)         VALUE ZEROS.

       01  WRK-ULT-CTPO-CHEQ           PIC  9(003) COMP-3  VALUE ZEROS.
       01  WRK-IO-PCB                  PIC  X(032)         VALUE SPACES.
       01  WRK-ALT-PCB                 PIC  X(032)         VALUE SPACES.

       01  WRK-CODI-DEVOL-CHQ          PIC  9(002)         VALUE ZEROS.

       01  WRK-GUARDA.
           03  WRK-CGC-CPF-EMITT       PIC  X(009)         VALUE SPACES.
           03  WRK-FILIAL-EMITT        PIC  X(004)         VALUE SPACES.
           03  WRK-CONTROLE-EMITT      PIC  9(002) COMP-3  VALUE ZEROS.
           03  WRK-TIPO-PESSOA-EMITT   PIC  X(001)         VALUE SPACES.
           03  WRK-NOME-EMITT          PIC  X(040)         VALUE SPACES.
           03  WRK-BCO-EMITT           PIC  9(003) COMP-3  VALUE ZEROS.
           03  WRK-AGENCIA-EMITT       PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-CONTA-EMITT         PIC  9(013) COMP-3  VALUE ZEROS.
           03  WRK-NUM-CHEQUE          PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-DIG-CHEQUE          PIC  X(001)         VALUE SPACES.
           03  WRK-NUM-LOTE            PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-CARTEIRA            PIC  9(003) COMP-3  VALUE ZEROS.
           03  WRK-BCO-CLI             PIC  9(003) COMP-3  VALUE ZEROS.
           03  WRK-AGENCIA-CLI         PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-CONTA-CLI           PIC  9(013) COMP-3  VALUE ZEROS.
           03  WRK-DT-DEPOSITO         PIC  9(008)         VALUE ZEROS.
           03  WRK-DT-CONTABIL         PIC  9(008)         VALUE ZEROS.
           03  WRK-DT-ENTRADA-ORIGEM   PIC  9(008)         VALUE ZEROS.
           03  WRK-DT-SITUACAO         PIC  9(008)         VALUE ZEROS.
           03  WRK-CTPO-CHEQ           PIC  9(003) COMP-3  VALUE ZEROS.
           03  WRK-GRP-ECONOMICO-EMITT PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-VL-PRINC-CHEQ       PIC 9(015)V99 COMP-3 VALUE ZEROS.
           03  WRK--CODI-DEVOL-CHQ     PIC 9(02)            VALUE ZEROS.

       01  WRK-CHV-EMITENTE-ATU.
           03  WRK-BANCO-ATU-R.
               05  WRK-BANCO-ATU       PIC  9(003)         VALUE ZEROS.
           03  WRK-AGENCIA-ATU         PIC  9(005)         VALUE ZEROS.
           03  WRK-CONTA-ATU           PIC  9(013)         VALUE ZEROS.
           03  WRK-CHEQUE-ATU          PIC  9(007)         VALUE ZEROS.

       01  WRK-CHV-EMITENTE-ANT.
           03  WRK-BANCO-ANT-R.
               05  WRK-BANCO-ANT       PIC  9(003)         VALUE ZEROS.
           03  WRK-AGENCIA-ANT         PIC  9(005)         VALUE ZEROS.
           03  WRK-CONTA-ANT           PIC  9(013)         VALUE ZEROS.
           03  WRK-CHEQUE-ANT          PIC  9(007)         VALUE ZEROS.

       01  WRK-CHV-ATU.
           03  WRK-BCO-ATU             PIC  9(003)         VALUE ZEROS.
           03  WRK-AGE-ATU             PIC  9(005)         VALUE ZEROS.
           03  WRK-CTA-ATU             PIC  9(013)         VALUE ZEROS.
           03  WRK-CHQ-ATU             PIC  9(007)         VALUE ZEROS.
           03  WRK-DT-DEP-ATU          PIC  9(008)         VALUE ZEROS.
           03  WRK-DT-EVT-ATU          PIC  9(008)         VALUE ZEROS.

       01  WRK-CHV-ANT.
           03  WRK-BCO-ANT             PIC  9(003)         VALUE ZEROS.
           03  WRK-AGE-ANT             PIC  9(005)         VALUE ZEROS.
           03  WRK-CTA-ANT             PIC  9(013)         VALUE ZEROS.
           03  WRK-CHQ-ANT             PIC  9(007)         VALUE ZEROS.
           03  WRK-DT-DEP-ANT          PIC  9(008)         VALUE ZEROS.
           03  WRK-DT-EVT-ANT          PIC  9(008)         VALUE ZEROS.

       01  WRK-CHV-SLIG-CLIB-ATU.
           03  WRK-BCO-SLIG-CLIB       PIC  9(003)         VALUE ZEROS.
           03  WRK-AGE-SLIG-CLIB       PIC  9(005)         VALUE ZEROS.
           03  WRK-CTA-SLIG-CLIB       PIC  9(013)         VALUE ZEROS.

       01  WRK-CHV-SLIG-CLIB-ANT       PIC  X(021)         VALUE ZEROS.

       01  WRK-CONTA-CLIE              PIC  9(013)         VALUE ZEROS.
       01  WRK-CC-CLIE  REDEFINES      WRK-CONTA-CLIE.
           03 FILLER                   PIC  9(007).
           03 WRK-CTA-CLIE             PIC  9(006).

       01  WRK-ERRO-STATUS             PIC  X(001)         VALUE SPACES.

       01  WRK-DT-INI-PER-ATU          PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-FIM-PER-ATU          PIC  9(008)         VALUE ZEROS.

       01  WRK-DT-INI-PER-ULT          PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-FIM-PER-ULT          PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-INI-PER-PRX          PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-FIM-PER-PRX          PIC  9(008)         VALUE ZEROS.

       01  WRK-DT-VENCIMENTO           PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-DEPOSITO-ERRO        PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-CONTABIL-ERRO        PIC  9(008)         VALUE ZEROS.

       01  WRK-QTDE-CHQ-BCO          PIC 9(015) COMP-3   VALUE ZEROS.

       01  WRK-TOT-ANT-ABER          PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-ANT-COMP          PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-ANT-REAP-N-RESP   PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-ANT-REAP-S-RESP   PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-ANT-DEVL-S-RESP   PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-ANT-DEVL-S-REAP   PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-ANT-DEVL-N-REAP   PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-ANT-BAIX          PIC 9(003) COMP-3   VALUE ZEROS.

       01  WRK-TOT-PER-ABER          PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-PER-COMP          PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-PER-REAP-N-RESP   PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-PER-REAP-S-RESP   PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-PER-DEVL-S-RESP   PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-PER-DEVL-S-REAP   PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-PER-DEVL-N-REAP   PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-PER-BAIX          PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-PER-BAX-ANT       PIC 9(003) COMP-3   VALUE ZEROS.

       01  WRK-TOT-FUT-ABER          PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-FUT-COMP          PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-FUT-REAP-N-RESP   PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-FUT-REAP-S-RESP   PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-FUT-DEVL-S-RESP   PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-FUT-DEVL-S-REAP   PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-FUT-DEVL-N-REAP   PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-FUT-BAIX          PIC 9(003) COMP-3   VALUE ZEROS.
       01  WRK-TOT-FUT-BAX-ANT       PIC 9(003) COMP-3   VALUE ZEROS.

       01  WRK-ACU-TOT-ANT-ABER         PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-ANT-COMP         PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-ANT-REAP-N-RESP  PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-ANT-REAP-S-RESP  PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-ANT-DEVL-S-RESP  PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-ANT-DEVL-S-REAP  PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-ANT-DEVL-N-REAP  PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-ANT-BAIX         PIC 9(015) COMP-3   VALUE ZEROS.

       01  WRK-ACU-TOT-PER-ABER         PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-PER-COMP         PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-PER-REAP-S-RESP  PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-PER-REAP-N-RESP  PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-PER-DEVL-S-RESP  PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-PER-DEVL-S-REAP  PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-PER-DEVL-N-REAP  PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-PER-BAIX         PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-PER-BAX-ANT      PIC 9(015) COMP-3   VALUE ZEROS.

       01  WRK-ACU-TOT-FUT-ABER         PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-FUT-COMP         PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-FUT-REAP-N-RESP  PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-FUT-REAP-S-RESP  PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-FUT-DEVL-S-RESP  PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-FUT-DEVL-S-REAP  PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-FUT-DEVL-N-REAP  PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-FUT-BAIX         PIC 9(015) COMP-3   VALUE ZEROS.
       01  WRK-ACU-TOT-FUT-BAX-ANT      PIC 9(015) COMP-3   VALUE ZEROS.

       01  WRK-TABELA-DEVOLUCAO.
           03  WRK-TABELA OCCURS  99 TIMES.
               05  WRK-CTPO-DEVLC      PIC  9(003).
               05  WRK-RESP-DEVLC      PIC  X(001).
               05  WRK-POBLD-REAP      PIC  X(001).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MSG001              PIC  X(075)         VALUE
               '** ARQUIVO MVTCHQSR VAZIO **'.
           03  WRK-MSG002              PIC  X(075)         VALUE
               '** CLIENTE NAO ENCONTRADO NO CLIB **'.
           03  WRK-MSG003.
               05  FILLER              PIC  X(028)         VALUE
                   '** ERRO NO ACESSO AO MODULO '.
               05  WRK-NM-MODULO       PIC  X(008)         VALUE SPACES.
               05  FILLER              PIC  X(017)         VALUE
                   ' - RETURN CODE = '.
               05  WRK-RET-COD         PIC  9(002)         VALUE ZEROS.
               05  FILLER              PIC  X(003)         VALUE ' **'.
           03  WRK-MSG-FORA-SEQ        PIC  X(075)         VALUE
               '** ARQUIVO MVTCHQSR FORA DE SEQUENCIA **'.
           03  WRK-MSG-ERRO-REAP       PIC  X(075)         VALUE
               '** REAPRESENTACAO SEM DEVOLUCAO       **'.
           03  WRK-MSG-ERRO-DEVL       PIC  X(075)         VALUE
               '** CODIGO DE DEVOLUCAO NAO ENCONTRADO **'.
           03  WRK-MSG004              PIC  X(075)         VALUE
               '** ARQUIVO ARQDATAS VAZIO **'.

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
           '*** AREA PARA ACESSO AO MODULO MESU9023 ***'.
      *----------------------------------------------------------------*

       01  WRK-9023-AMBIENTE            PIC  X(001)        VALUE SPACES.
       01  WRK-9023-ERRO-AREA           PIC  X(107)        VALUE SPACES.

       01  WRK-9023-PARM.
           03  WRK-9023-ENTRADA.
               05  WRK-9023-NOME-PGM    PIC  X(008)        VALUE SPACES.
               05  WRK-9023-COD-RET     PIC  9(002)        VALUE ZEROS.
               05  WRK-9023-TPO-RELAC   PIC  9(003)        VALUE ZEROS.
               05  WRK-9023-BANCO       PIC  9(005)        VALUE ZEROS.
               05  WRK-9023-AGENCIA     PIC  9(005)        VALUE ZEROS.
           03  WRK-9023-SAIDA.
               05  WRK-9023-EMPR-RELAC  PIC  9(005)        VALUE ZEROS.
               05  WRK-9023-DEPTO-RELAC PIC  9(005)        VALUE ZEROS.
               05  WRK-9023-NOME-DEPDC  PIC  X(050)        VALUE SPACES.

           EJECT
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
               'SLIG6210'.
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

       01  CABEC5.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'PERIODO INICIAL'.
           03  FILLER                  PIC  X(014)         VALUE
               '           :  '.
           03  CB5-DT-INI-PER          PIC  9999/99/99.

       01  CABEC6.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'PERIODO FINAL'.
           03  FILLER                  PIC  X(014)         VALUE
               '           :  '.
           03  CB6-DT-FIM-PER          PIC  9999/99/99.

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

       01  LINTOT3.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGISTROS GRAVADOS'.
           03  FILLER                  PIC  X(013)         VALUE
               '(ARQERROS) : '.
           03  LT3-TOT-GRAVADOS        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT4.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGISTROS GRAVADOS'.
           03  FILLER                  PIC  X(013)         VALUE
               '(MVCHQREL) : '.
           03  LT4-TOT-GRAVADOS        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT4A.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGISTROS GRAVADOS'.
           03  FILLER                  PIC  X(013)         VALUE
               '(MVCHDESP) : '.
           03  LT4A-TOT-GRAVADOS       PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT4B.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGISTROS GRAVADOS'.
           03  FILLER                  PIC  X(013)         VALUE
               '(QTDEBCO)  : '.
           03  LT4B-TOT-GRAVADOS       PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT4C.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'REGISTROS GRAVADOS'.
           03  FILLER                  PIC  X(013)         VALUE
               '(MOTDEVOL) : '.
           03  LT4C-TOT-GRAVADOS       PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

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

       01  LINTOT7.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - COMPENSADO N'.
           03  FILLER                  PIC  X(013)         VALUE
               'O PERIODO  : '.
           03  LT7-TOT-PER-COMP        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT8.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - REAP S RESP '.
           03  FILLER                  PIC  X(013)         VALUE
               'NO PERIODO : '.
           03  LT8-TOT-PER-REAP-S-RESP PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT8A.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - REAP N RESP '.
           03  FILLER                  PIC  X(013)         VALUE
               'NO PERIODO : '.
           03  LT8A-TOT-PER-REAP-N-RESP PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT9.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - DEVL S RESP '.
           03  FILLER                  PIC  X(013)         VALUE
               'NO PERIODO : '.
           03  LT9-TOT-PER-DEVL-S-RESP PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT9A.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - DEVL N REAP '.
           03  FILLER                  PIC  X(013)         VALUE
               'NO PERIODO : '.
           03  LT9A-TOT-PER-DEVL-N-REAP PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT9B.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - DEVL S REAP '.
           03  FILLER                  PIC  X(013)         VALUE
               'NO PERIODO : '.
           03  LT9B-TOT-PER-DEVL-S-REAP PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT10.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - BAIXADOS NO '.
           03  FILLER                  PIC  X(013)         VALUE
               'PERIODO    : '.
           03  LT10-TOT-PER-BAIXS   PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT11.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - BAIXADOS NO '.
           03  FILLER                  PIC  X(013)         VALUE
               'PERIODO ANT: '.
           03  LT11-TOT-PER-BAX-ANT    PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT12.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - BAIXADOS FUT'.
           03  FILLER                  PIC  X(013)         VALUE
               'URO ANT.   : '.
           03  LT12-TOT-PER-BAX-F      PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT13.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - ABERTO NO FU'.
           03  FILLER                  PIC  X(013)         VALUE
               'TURO       : '.
           03  LT13-TOT-FUT-ABER     PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT14.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - REAP S RESP '.
           03  FILLER                  PIC  X(013)         VALUE
               'NO FUTURO  : '.
           03  LT14-TOT-FUT-REAP-S-RESP PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT14A.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - REAP N RESP '.
           03  FILLER                  PIC  X(013)         VALUE
               'NO FUTURO  : '.
           03  LT14A-TOT-FUT-REAP-N-RESP PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT14B.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - DEVL S RESP '.
           03  FILLER                  PIC  X(013)         VALUE
               'NO FUTURO  : '.
           03  LT14B-TOT-FUT-DEVL-S-RESP PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT14C.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - DEVL N REAP '.
           03  FILLER                  PIC  X(013)         VALUE
               'NO FUTURO  : '.
           03  LT14C-TOT-FUT-DEVL-N-REAP PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT14D.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - DEVL S REAP '.
           03  FILLER                  PIC  X(013)         VALUE
               'NO FUTURO  : '.
           03  LT14D-TOT-FUT-DEVL-S-REAP PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT15.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - COMPENSADO  '.
           03  FILLER                  PIC  X(013)         VALUE
               'ANTERIOR   : '.
           03  LT15-TOT-ANT-COMP PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT16.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - REAP S RESP '.
           03  FILLER                  PIC  X(013)         VALUE
               'ANTERIOR   : '.
           03  LT16-TOT-ANT-REAP-S-RESP PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT16A.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - REAP N RESP '.
           03  FILLER                  PIC  X(013)         VALUE
               'ANTERIOR   : '.
           03  LT16A-TOT-ANT-REAP-N-RESP PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT17.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - DEVL S RESP '.
           03  FILLER                  PIC  X(013)         VALUE
               'ANTERIOR   : '.
           03  LT17-TOT-ANT-DEVL-S-RESP  PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT17A.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - DEVL N REAP '.
           03  FILLER                  PIC  X(013)         VALUE
               'ANTERIOR   : '.
           03  LT17A-TOT-ANT-DEVL-N-REAP  PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT17B.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - DEVL S REAP '.
           03  FILLER                  PIC  X(013)         VALUE
               'ANTERIOR   : '.
           03  LT17B-TOT-ANT-DEVL-S-REAP  PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT18.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - BAIXADOS    '.
           03  FILLER                  PIC  X(013)         VALUE
               'ANTERIOR   : '.
           03  LT18-TOT-ANT-BAIX    PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT19.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - ABERTO      '.
           03  FILLER                  PIC  X(013)         VALUE
               'ANTERIOR   : '.
           03  LT19-TOT-ANT-ABER    PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT20.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - ABERTO      '.
           03  FILLER                  PIC  X(013)         VALUE
               'PERIODO    : '.
           03  LT20-TOT-PER-ABER    PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT21.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL EMITENTES BCO '.
           03  FILLER                  PIC  X(013)         VALUE
               '237        : '.
           03  LT21-TOT-BCO-237        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT22.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL EMITENTES BCO '.
           03  FILLER                  PIC  X(013)         VALUE
               'DIF. 237   : '.
           03  LT22-TOT-BCO-DIF-237    PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT23.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL EMIT. CGC/CPF '.
           03  FILLER                  PIC  X(013)         VALUE
               'ZEROS ENTR.: '.
           03  LT23-TOT-CGC-ZEROS      PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT24.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL DIFERENTES DE '.
           03  FILLER                  PIC  X(013)         VALUE
               'ZEROS ENTR.: '.
           03  LT24-TOT-CGC-DIF-ZEROS  PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT25.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL DE ACESSOS AO '.
           03  FILLER                  PIC  X(013)         VALUE
               'CLIB2000  .: '.
           03  LT25-TOT-ACESS-CLIB2000 PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.


       01  LINTOT26.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL EMITT CGC ATUA'.
           03  FILLER                  PIC  X(013)         VALUE
               'LZ C/C    .: '.
           03  LT26-TOT-CGC-ATUALZ-CC  PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT27.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL EMITT CGC ATUA'.
           03  FILLER                  PIC  X(013)         VALUE
               'LZ POUP.  .: '.
           03  LT27-TOT-CGC-ATUALZ-PP  PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT28.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL EMITT NOME ENC'.
           03  FILLER                  PIC  X(013)         VALUE
               '. C/C     .: '.
           03  LT28-TOT-NOME-ENC-CC    PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT29.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL EMITT NOME ENC'.
           03  FILLER                  PIC  X(013)         VALUE
               '. POUP.   .: '.
           03  LT29-TOT-NOME-ENC-PP    PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT30.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL EMITT NOME NAO'.
           03  FILLER                  PIC  X(013)         VALUE
               ' ENCONTR. .: '.
           03  LT30-TOT-NOME-N-ENC     PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT31.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL DE ACESSOS AO '.
           03  FILLER                  PIC  X(013)         VALUE
               'GRUP201C  .: '.
CNPJ7C     03  WRK-TOT-ACESS-GRUP201C  PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9 VALUE
                                            ZEROS.

       01  LINTOT32.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL EMITT GRUPO EN'.
           03  FILLER                  PIC  X(013)         VALUE
               'CONTR.    .: '.
           03  LT32-TOT-GRP-ENC        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT33.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL EMITT GRUPO N '.
           03  FILLER                  PIC  X(013)         VALUE
               'ENCONTR.  .: '.
           03  LT33-TOT-GRP-N-ENC      PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT34.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL EMIT. CGC/CPF '.
           03  FILLER                  PIC  X(013)         VALUE
               'ZEROS SAIDA: '.
           03  LT34-TOT-CGC-ZEROS      PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT35.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL DIFERENTES DE '.
           03  FILLER                  PIC  X(013)         VALUE
               'ZEROS SAIDA: '.
           03  LT35-TOT-CGC-DIF-ZEROS  PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT36.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL EMITT NOME NAO'.
           03  FILLER                  PIC  X(013)         VALUE
               ' ENC SAIDA.: '.
           03  LT36-TOT-NOME-N-ENC     PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT37.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL EMITT NOME SPA'.
           03  FILLER                  PIC  X(013)         VALUE
               'CES SAIDA .: '.
           03  LT37-TOT-NOME-SPACES    PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT38.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL EMITT NOME OK '.
           03  FILLER                  PIC  X(013)         VALUE
               'SAIDA     .: '.
           03  LT38-TOT-NOME-OK        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT39.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL EMITT GRUPO ZE'.
           03  FILLER                  PIC  X(013)         VALUE
               'ROS SAIDA .: '.
           03  LT39-TOT-GRP-ZEROS      PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT40.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL DIFERENTES DE '.
           03  FILLER                  PIC  X(013)         VALUE
               'ZEROS SAIDA: '.
           03  LT40-TOT-GRP-DIF-ZEROS  PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT41.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - COMPENSADO N'.
           03  FILLER                  PIC  X(013)         VALUE
               'O FUTURO   : '.
           03  LT41-TOT-FUT-COMP       PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT43.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'TOTAL - BAIXADOS NO '.
           03  FILLER                  PIC  X(013)         VALUE
               'FUTURO     : '.
           03  LT43-TOT-FUT-BAIXS      PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINHIFEN.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(052)         VALUE
               '----------------------------------------------------'.



           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA RETORNO DO MODULO CLIB2000 ***'.
      *----------------------------------------------------------------*

       COPY CLIBW020.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO AO MODULO MESU9014 ***'.
      *----------------------------------------------------------------*

-INC I#MESUM4

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO AO MODULO GRUP201C ***'.
      *----------------------------------------------------------------*

       COPY GRUPW01S.
           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7100 ***'.
      *----------------------------------------------------------------*

-INC POL7100C

           EJECT

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS PARA DB2 ***'.
      *---------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE SLIGB058
           END-EXEC.

           EXEC SQL
               INCLUDE CHSFB026
           END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(079)         VALUE
           '*** AREA PARA DECLARACAO DE CURSORES ***'.
      *---------------------------------------------------------------*

      *--- SELECIONA TODOS OS CODIGOS DE DEVOLUCAO ATIVOS ------------*

           EXEC SQL
               DECLARE CRS-B058-B026 CURSOR FOR
               SELECT  B058.CTPO_DEVLC_CHEQ,
                       B058.CINDCD_RESP_DEVLC,
                       B058.CINDCD_POBLD_REAP
               FROM    DB2PRD.TRESP_DEVLC_CHEQ B058 LEFT JOIN
                       DB2PRD.MOTVO_DEVLC_CHEQ B026 ON
                      (B058.CTPO_DEVLC_CHEQ = B026.CTPO_DEVLC_CHEQ)
               WHERE   B058.CSEQ_MOTVO_DEVLC =
              (SELECT  MAX(B58A.CSEQ_MOTVO_DEVLC)
               FROM    DB2PRD.TRESP_DEVLC_CHEQ B58A
               WHERE   B58A.CTPO_DEVLC_CHEQ = B058.CTPO_DEVLC_CHEQ)
                 AND  (B058.CSIT_MOTVO_DEVLC IN (1, 3, 4))
               ORDER   BY 1
           END-EXEC.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6210 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*
           CALL  'POOL1050'.

           PERFORM 100000-INICIALIZAR.

           PERFORM 150000-MONTAR-TABELA

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

           OPEN INPUT  ARQDATAS
                       MVTCHQSR
                OUTPUT EMITCHEQ
                       MVCHQREL
                       SRELCTRL
                       ARQERROS
                       MVCHDESP
                       QTDEBCO
                       MOTDEVOL.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

           INITIALIZE  WRK-TABELA-DEVOLUCAO.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 111000-TESTAR-FS-MVTCHQSR.

           PERFORM 112000-TESTAR-FS-EMITCHEQ.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           PERFORM 114000-TESTAR-FS-ARQERROS.

           PERFORM 115000-TESTAR-FS-ARQDATAS.

           PERFORM 116000-TESTAR-FS-MVCHQREL.

           PERFORM 117000-TESTAR-FS-MVCHDESP.

           PERFORM 118000-TESTAR-FS-QTDEBCO.

           PERFORM 119000-TESTAR-FS-MOTDEVOL.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       111000-TESTAR-FS-MVTCHQSR       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-MVTCHQSR         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'MVTCHQSR'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-MVTCHQSR   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       112000-TESTAR-FS-EMITCHEQ       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EMITCHEQ         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'EMITCHEQ'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-EMITCHEQ   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       113000-TESTAR-FS-SRELCTRL       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SRELCTRL         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'SRELCTRL'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-SRELCTRL   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       113000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       114000-TESTAR-FS-ARQERROS       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQERROS         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ARQERROS'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-ARQERROS   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       114000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       115000-TESTAR-FS-ARQDATAS       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQDATAS         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ARQDATAS'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-ARQDATAS   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       115000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       116000-TESTAR-FS-MVCHQREL       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-MVCHQREL         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'MVCHQREL'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-MVCHQREL   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       116000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       117000-TESTAR-FS-MVCHDESP       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-MVCHDESP         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'MVCHDESP'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-MVCHQREL   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       117000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       118000-TESTAR-FS-QTDEBCO        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-QTDEBCO          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'QTDEBCO'          TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-QTDEBCO    TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       118000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       119000-TESTAR-FS-MOTDEVOL       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-MOTDEVOL         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'MOTDEVOL'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-MOTDEVOL   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       119000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       150000-MONTAR-TABELA            SECTION.
      *----------------------------------------------------------------*

           PERFORM 150100-ABRIR-CURSOR

           PERFORM 150200-LER-CURSOR

           PERFORM 150300-MONTAR-OCORRENCIAS
                   VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 99 OR
                      WRK-FIM-CURSOR   EQUAL 'S'.

           IF  WRK-FIM-CURSOR          EQUAL 'N'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'TABELA  '         TO WRK-NOME-ARQUIVO
               MOVE 'ESTOURO DA TABELA DE DEVOLUCAO'
                                       TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 150400-FECHAR-CURSOR.

      *----------------------------------------------------------------*
       150000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       150100-ABRIR-CURSOR             SECTION.
      *---------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CURSOR.

           EXEC SQL
               OPEN CRS-B058-B026
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TRESP_DEVLC_CHEQ'  TO ERR-DBD-TAB
               MOVE 'OPEN'              TO ERR-FUN-COMANDO
               MOVE  SQLCODE            TO ERR-SQL-CODE
               MOVE '0010'              TO ERR-LOCAL
               MOVE  SPACES             TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       150100-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       150200-LER-CURSOR               SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
               FETCH  CRS-B058-B026
               INTO  :SLIGB058.CTPO-DEVLC-CHEQ,
                     :SLIGB058.CINDCD-RESP-DEVLC,
                     :SLIGB058.CINDCD-POBLD-REAP
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W'               )
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TRESP_DEVLC_CHEQ'  TO ERR-DBD-TAB
               MOVE 'FETCH'             TO ERR-FUN-COMANDO
               MOVE  SQLCODE            TO ERR-SQL-CODE
               MOVE '0020'              TO ERR-LOCAL
               MOVE  SPACES             TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
           END-IF.

      *---------------------------------------------------------------*
       150200-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       150300-MONTAR-OCORRENCIAS       SECTION.
      *---------------------------------------------------------------*

           MOVE  CTPO-DEVLC-CHEQ       OF SLIGB058
                                       TO WRK-CTPO-DEVLC(WRK-IND)
           MOVE  CINDCD-RESP-DEVLC     OF SLIGB058
                                       TO WRK-RESP-DEVLC(WRK-IND)
           MOVE  CINDCD-POBLD-REAP     OF SLIGB058
                                       TO WRK-POBLD-REAP(WRK-IND)

           PERFORM 150200-LER-CURSOR.

      *---------------------------------------------------------------*
       150300-99-FIM.                  EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       150400-FECHAR-CURSOR            SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
               CLOSE CRS-B058-B026
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TRESP_DEVLC_CHEQ'  TO ERR-DBD-TAB
               MOVE 'CLOSE'             TO ERR-FUN-COMANDO
               MOVE  SQLCODE            TO ERR-SQL-CODE
               MOVE '0030'              TO ERR-LOCAL
               MOVE  SPACES             TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       150400-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-LER-MVTCHQSR.

           IF  WRK-FS-MVTCHQSR         EQUAL '10'
               DISPLAY '***************** SLIG6210 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO MVTCHQSR VAZIO          *'
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO  CANCELADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** SLIG6210 *****************'
               MOVE WRK-MSG001         TO ERR-TEXTO
VOLTAR         PERFORM 400000-FINALIZAR
VOLTAR*        P*RFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 220000-LER-ARQDATAS.

           IF  WRK-FS-ARQDATAS         EQUAL '10'
               DISPLAY '***************** SLIG6210 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO ARQDATAS VAZIO          *'
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO  CANCELADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** SLIG6210 *****************'
               MOVE WRK-MSG004         TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 220000-LER-ARQDATAS UNTIL
                   WRK-FS-ARQDATAS     EQUAL '10' OR
                   SLIG00-CD-TP-PROCESSA EQUAL 2.

           IF  WRK-FS-ARQDATAS         EQUAL '10'
               DISPLAY '****************** SLIG6210 ******************'
               DISPLAY '*                                            *'
               DISPLAY '*     CAMPOS PESQUISADOS NAO ENCONTRADOS     *'
               DISPLAY '*                                            *'
               DISPLAY '*            NO ARQUIVO DE DATAS             *'
               DISPLAY '*                                            *'
               DISPLAY '****************** SLIG6210 ******************'
               MOVE WRK-MSG004         TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF SLIG00-CD-TP-PROCESSA EQUAL 2
                  MOVE     SLIG00-DT-INI-PER-ATU  TO
                           WRK-DT-INI-PER-ATU
                  MOVE     SLIG00-DT-FIM-PER-ATU  TO
                           WRK-DT-FIM-PER-ATU
                  MOVE     SLIG00-DT-INI-PER-ULT  TO
                           WRK-DT-INI-PER-ULT
                  MOVE     SLIG00-DT-FIM-PER-ULT  TO
                           WRK-DT-FIM-PER-ULT
                  MOVE     SLIG00-DT-INI-PER-PRX  TO
                           WRK-DT-INI-PER-PRX
                  MOVE     SLIG00-DT-FIM-PER-PRX  TO
                           WRK-DT-FIM-PER-PRX
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-LER-MVTCHQSR             SECTION.
      *----------------------------------------------------------------*

           READ MVTCHQSR.

           IF  WRK-FS-MVTCHQSR         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-EMITENTE-ATU
               GO TO 210000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 111000-TESTAR-FS-MVTCHQSR.


           MOVE SLIG27-BCO-EMITT       TO WRK-BANCO-ATU
                                          WRK-BCO-SLIG-CLIB
                                          WRK-BCO-ATU.

           MOVE SLIG27-AGENCIA-EMITT   TO WRK-AGENCIA-ATU
                                          WRK-AGE-SLIG-CLIB
                                          WRK-AGE-ATU.

           MOVE SLIG27-CONTA-EMITT     TO WRK-CONTA-ATU
                                          WRK-CTA-SLIG-CLIB
                                          WRK-CTA-ATU.

           MOVE SLIG27-NUM-CHEQUE      TO WRK-CHEQUE-ATU
                                          WRK-CHQ-ATU.

           MOVE SLIG27-DT-DEPOSITO     TO WRK-DT-DEP-ATU.
           MOVE SLIG27-DT-CONTABIL     TO WRK-DT-EVT-ATU.

           PERFORM 211000-TESTAR-SEQUENCIA.

           ADD 1                       TO ACU-LIDOS-MVTCHQSR.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       211000-TESTAR-SEQUENCIA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-ATU             LESS WRK-CHV-ANT
               DISPLAY '***************** SLIG6210 *******************'
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
                                                            '        *'
               DISPLAY '*     DATA-DEPOSITO   : ' WRK-DT-DEP-ANT
                                                            '        *'
               DISPLAY '*     DATA-EVENTO     : ' WRK-DT-EVT-ANT
                                                            '        *'
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
                                                            '        *'
               DISPLAY '*     DATA-DEPOSITO   : ' WRK-DT-DEP-ATU
                                                            '        *'
               DISPLAY '*     DATA-EVENTO     : ' WRK-DT-EVT-ATU
                                                            '        *'
               DISPLAY '*                                            *'
               DISPLAY '****************** SLIG6210 ******************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE  WRK-MSG-FORA-SEQ  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-CHV-ATU            TO WRK-CHV-ANT.

      *----------------------------------------------------------------*
       211000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       220000-LER-ARQDATAS             SECTION.
      *----------------------------------------------------------------*

           READ ARQDATAS.

           IF  WRK-FS-ARQDATAS         EQUAL '10'
               GO TO 210000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 115000-TESTAR-FS-ARQDATAS.

           ADD 1                       TO ACU-LIDOS-ARQDATAS.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       230000-VER-DEVOLUCAO            SECTION.
      *----------------------------------------------------------------*

           IF WRK-CTPO-DEVLC(WRK-IND) EQUAL ZEROS
              MOVE 100                   TO WRK-IND
              MOVE SPACES                TO WRK-RESP-ULTIMA-DEVOLUCAO
                                            WRK-REAP-ULTIMA-DEVOLUCAO
              MOVE ZEROS                 TO WRK-ULTIMA-DEVOLUCAO
           ELSE
              IF WRK-CTPO-DEVLC(WRK-IND) EQUAL SLIG27-CODI-DEVOL-CHQ
                 MOVE 'S'                   TO WRK-ACHOU

                 MOVE WRK-RESP-DEVLC(WRK-IND)
                                            TO WRK-RESP-ULTIMA-DEVOLUCAO
                 MOVE WRK-POBLD-REAP(WRK-IND)
                                            TO WRK-REAP-ULTIMA-DEVOLUCAO
                 MOVE SLIG27-DT-CONTABIL    TO WRK-ULTIMA-DEVOLUCAO
           END-IF.

      *----------------------------------------------------------------*
       230000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHV-EMITENTE-ATU TO WRK-CHV-EMITENTE-ANT.

           MOVE SPACES               TO WRK-RESP-ULTIMA-DEVOLUCAO
                                        WRK-REAP-ULTIMA-DEVOLUCAO
                                        WRK-ERRO-STATUS.

           MOVE 99999999             TO WRK-ULTIMA-DEVOLUCAO.

           MOVE ZEROS                TO WRK-ULT-CTPO-CHEQ
                                        WRK-DT-VENCIMENTO
                                        WRK-DT-DEPOSITO-ERRO
                                        WRK-DT-CONTABIL-ERRO
                                        WRK-TOT-ANT-ABER
                                        WRK-TOT-ANT-COMP
                                        WRK-TOT-ANT-REAP-N-RESP
                                        WRK-TOT-ANT-REAP-S-RESP
                                        WRK-TOT-ANT-DEVL-S-RESP
                                        WRK-TOT-ANT-DEVL-N-REAP
                                        WRK-TOT-ANT-DEVL-S-REAP
                                        WRK-TOT-ANT-BAIX
                                        WRK-TOT-PER-ABER
                                        WRK-TOT-PER-COMP
                                        WRK-TOT-PER-REAP-N-RESP
                                        WRK-TOT-PER-REAP-S-RESP
                                        WRK-TOT-PER-DEVL-S-RESP
                                        WRK-TOT-PER-DEVL-S-REAP
                                        WRK-TOT-PER-DEVL-N-REAP
                                        WRK-TOT-PER-BAIX
                                        WRK-TOT-PER-BAX-ANT
                                        WRK-TOT-FUT-COMP
                                        WRK-TOT-FUT-REAP-N-RESP
                                        WRK-TOT-FUT-REAP-S-RESP
                                        WRK-TOT-FUT-DEVL-S-RESP
                                        WRK-TOT-FUT-DEVL-N-REAP
                                        WRK-TOT-FUT-DEVL-S-REAP
                                        WRK-TOT-FUT-ABER
                                        WRK-TOT-FUT-BAIX
                                        WRK-TOT-FUT-BAX-ANT
                                        WRK-DT-MOVIMENTO-ULT
                                        WRK-DT-CONTABIL-ULT
                                        WRK-DT-SITUACAO-ULT.

      *--- VERIFICA HISTORICO DO CHEQUE ATE SUA QUEBRA OU ERRO NO -----*
      *--- STATUS. A 1A.    CARTEIRA ENCONTRADA DE ACORDO COM OS  -----*
      *--- EVENTOS SERAH CONSIDERADA PARA O CHEQUE.               -----*

           MOVE 99 TO WRK-CARTEIRA.

           PERFORM 330000-PROCESSAR-STATUS UNTIL
                   WRK-CHV-EMITENTE-ATU NOT EQUAL
                   WRK-CHV-EMITENTE-ANT OR
                   WRK-ERRO-STATUS EQUAL 'S'.

           IF WRK-BANCO-ATU-R           EQUAL WRK-BANCO-ANT-R
              ADD 1                     TO WRK-QTDE-CHQ-BCO
           ELSE
              ADD 1                     TO WRK-QTDE-CHQ-BCO
              PERFORM 350000-GRAVAR-QTDEBCO
              MOVE ZEROS                TO WRK-QTDE-CHQ-BCO
           END-IF.

           IF WRK-ERRO-STATUS  NOT EQUAL 'S'
              PERFORM 320000-GRAVAR-EMITCHEQ
           ELSE
              PERFORM 340000-TRATA-ERRO-SITUACAO
           END-IF.


      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       310000-GUARDAR-CAMPOS           SECTION.
      *----------------------------------------------------------------*

           MOVE SLIG27-CGC-CPF-EMITT-ST
           TO WRK-CGC-CPF-EMITT.
           MOVE SLIG27-FILIAL-EMITT-ST
           TO WRK-FILIAL-EMITT.
           MOVE SLIG27-CONTROLE-EMITT-ST
           TO WRK-CONTROLE-EMITT.
           MOVE SLIG27-BCO-EMITT         TO WRK-BCO-EMITT.
           MOVE SLIG27-AGENCIA-EMITT     TO WRK-AGENCIA-EMITT.
           MOVE SLIG27-CONTA-EMITT       TO WRK-CONTA-EMITT.
           MOVE SLIG27-NUM-CHEQUE        TO WRK-NUM-CHEQUE.
           MOVE SLIG27-DIG-CHEQUE        TO WRK-DIG-CHEQUE.
           MOVE SLIG27-NUM-LOTE          TO WRK-NUM-LOTE.
           MOVE SLIG27-BCO-CLI           TO WRK-BCO-CLI.
           MOVE SLIG27-AGENCIA-CLI       TO WRK-AGENCIA-CLI.
           MOVE SLIG27-CONTA-CLI         TO WRK-CONTA-CLI.
           MOVE SLIG27-DT-DEPOSITO       TO WRK-DT-DEPOSITO.
           MOVE SLIG27-DT-CONTABIL       TO WRK-DT-CONTABIL.
           MOVE SLIG27-DT-ENTRADA-ORIGEM TO WRK-DT-ENTRADA-ORIGEM.
           MOVE SLIG27-DT-SITUACAO       TO WRK-DT-SITUACAO.
           MOVE SLIG27-CTPO-CHEQ         TO WRK-CTPO-CHEQ.
           MOVE SLIG27-VL-PRINC-CHEQ     TO WRK-VL-PRINC-CHEQ.
           MOVE SLIG27-CODI-DEVOL-CHQ    TO WRK-CODI-DEVOL-CHQ.

      *--  CONSIDERA A PRIMEIRA CARTEIRA ENCONTRADA PARA O CHEQUE -----*

           IF WRK-CARTEIRA EQUAL 99
              MOVE SLIG27-CARTEIRA       TO WRK-CARTEIRA
           END-IF.

      *--  ACUMULA TOTAL DE EMITENTES BANCO 237 E DIFERENTE DE 237  ---*

           IF WRK-BCO-EMITT      EQUAL 237
              ADD 1 TO ACU-EMITT-BCO-237
           ELSE
              ADD 1 TO ACU-EMITT-BCO-DIF-237
           END-IF.

      *--  ACUMULA TOTAL DE EMITENTES CGC ZERADOS NA ENTRADA  ---------*

           IF WRK-CGC-CPF-EMITT EQUAL ZEROS
              ADD 1 TO ACU-EMITT-CGC-ZEROS-IN
           ELSE
              ADD 1 TO ACU-EMITT-CGC-DIF-ZEROS-IN
           END-IF.

           IF WRK-CHV-SLIG-CLIB-ATU NOT EQUAL
              WRK-CHV-SLIG-CLIB-ANT

      *-----  BUSCA NOME DO EMITENTE E CNPJ/CPF  ----------------------*

              IF WRK-BCO-EMITT      EQUAL 237
                 PERFORM 321000-OBTER-DADOS-EMITENTE
              ELSE
                 MOVE SPACES        TO WRK-NOME-EMITT
              END-IF

      *-----  FORMATA TIPO DE PESSOA EMITENTE  ------------------------*

              IF WRK-CGC-CPF-EMITT EQUAL ZEROS
                 MOVE 'J'          TO WRK-TIPO-PESSOA-EMITT
              ELSE
                 IF WRK-FILIAL-EMITT      EQUAL ZEROS
                    MOVE 'F'          TO WRK-TIPO-PESSOA-EMITT
                 ELSE
                    MOVE 'J'          TO WRK-TIPO-PESSOA-EMITT
                 END-IF
              END-IF

      *-----  BUSCA GRUPO ECONOMICO DO EMITENTE  ----------------------*

              IF WRK-CGC-CPF-EMITT NOT EQUAL ZEROS
                 PERFORM 326000-OBTER-GRUPO-EMITENTE
              ELSE
                 MOVE ZEROS      TO WRK-GRP-ECONOMICO-EMITT
              END-IF

      *-----  FORMATA CHAVE SLIG CLIB ANTERIOR  -----------------------*

              MOVE WRK-CHV-SLIG-CLIB-ATU TO  WRK-CHV-SLIG-CLIB-ANT

           END-IF.

           PERFORM 322000-GRAVAR-MVCHQREL.

      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       320000-GRAVAR-EMITCHEQ          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE REG-CHEQUE-EMITENTE.

      *--- DADOS DE STATUS DO CHEQUE ----------------------------------*

           MOVE WRK-ULT-CTPO-CHEQ         TO SLIG28-ULT-CTPO-CHEQ.

           MOVE WRK-TOT-ANT-ABER          TO SLIG28-TOT-ANT-ABER.
           MOVE WRK-TOT-ANT-COMP          TO SLIG28-TOT-ANT-COMP.
           MOVE WRK-TOT-ANT-REAP-S-RESP   TO SLIG28-TOT-ANT-REAP-S-RESP.
           MOVE WRK-TOT-ANT-REAP-N-RESP   TO SLIG28-TOT-ANT-REAP-N-RESP.
           MOVE WRK-TOT-ANT-DEVL-S-RESP   TO SLIG28-TOT-ANT-DEVL-S-RESP.
           MOVE WRK-TOT-ANT-DEVL-S-REAP   TO SLIG28-TOT-ANT-DEVL-S-REAP.
           MOVE WRK-TOT-ANT-DEVL-N-REAP   TO SLIG28-TOT-ANT-DEVL-N-REAP.
           MOVE WRK-TOT-ANT-BAIX          TO SLIG28-TOT-ANT-BAIX.
           MOVE WRK-TOT-PER-ABER          TO SLIG28-TOT-PER-ABER.
           MOVE WRK-TOT-PER-COMP          TO SLIG28-TOT-PER-COMP.
           MOVE WRK-TOT-PER-REAP-S-RESP   TO SLIG28-TOT-PER-REAP-S-RESP.
           MOVE WRK-TOT-PER-REAP-N-RESP   TO SLIG28-TOT-PER-REAP-N-RESP.
           MOVE WRK-TOT-PER-DEVL-S-RESP   TO SLIG28-TOT-PER-DEVL-S-RESP.
           MOVE WRK-TOT-PER-DEVL-S-REAP   TO SLIG28-TOT-PER-DEVL-S-REAP.
           MOVE WRK-TOT-PER-DEVL-N-REAP   TO SLIG28-TOT-PER-DEVL-N-REAP.
           MOVE WRK-TOT-PER-BAIX          TO SLIG28-TOT-PER-BAIX.
           MOVE WRK-TOT-PER-BAX-ANT       TO SLIG28-TOT-PER-BAX-ANT.
           MOVE WRK-TOT-FUT-ABER          TO SLIG28-TOT-FUT-ABER.
           MOVE WRK-TOT-FUT-COMP          TO SLIG28-TOT-FUT-COMP.
           MOVE WRK-TOT-FUT-REAP-S-RESP   TO SLIG28-TOT-FUT-REAP-S-RESP.
           MOVE WRK-TOT-FUT-REAP-N-RESP   TO SLIG28-TOT-FUT-REAP-N-RESP.
           MOVE WRK-TOT-FUT-DEVL-S-RESP   TO SLIG28-TOT-FUT-DEVL-S-RESP.
           MOVE WRK-TOT-FUT-DEVL-S-REAP   TO SLIG28-TOT-FUT-DEVL-S-REAP.
           MOVE WRK-TOT-FUT-DEVL-N-REAP   TO SLIG28-TOT-FUT-DEVL-N-REAP.
           MOVE WRK-TOT-FUT-BAIX          TO SLIG28-TOT-FUT-BAIX.
           MOVE WRK-TOT-FUT-BAX-ANT       TO SLIG28-TOT-FUT-BAX-ANT.

      *--- DADOS DO EMITENTE ------------------------------------------*

           MOVE WRK-CGC-CPF-EMITT       TO SLIG28-CGC-CPF-EMITT-ST.
           MOVE WRK-FILIAL-EMITT        TO SLIG28-FILIAL-EMITT-ST.
           MOVE WRK-CONTROLE-EMITT      TO SLIG28-CONTROLE-EMITT-ST.

           MOVE WRK-TIPO-PESSOA-EMITT   TO SLIG28-TIPO-PESSOA-EMITT.

           IF   WRK-CGC-CPF-EMITT  EQUAL  ZEROS
                MOVE 'J'                TO SLIG28-TIPO-PESSOA-EMITT.

           MOVE WRK-BCO-EMITT           TO SLIG28-BCO-EMITT.
           MOVE WRK-AGENCIA-EMITT       TO SLIG28-AGENCIA-EMITT.
           MOVE WRK-CONTA-EMITT         TO SLIG28-CONTA-EMITT.
           MOVE WRK-NOME-EMITT          TO SLIG28-NOME-EMITT.
           MOVE WRK-GRP-ECONOMICO-EMITT TO SLIG28-GRP-ECONOMICO-EMITT.

      *--- DADOS DO CHEQUE --------------------------------------------*

           MOVE WRK-NUM-CHEQUE         TO SLIG28-NUM-CHEQUE.
           MOVE WRK-DIG-CHEQUE         TO SLIG28-DIG-CHEQUE.
           MOVE WRK-NUM-LOTE           TO SLIG28-NUM-LOTE.
           MOVE WRK-CARTEIRA           TO SLIG28-CARTEIRA.
           MOVE WRK-VL-PRINC-CHEQ      TO SLIG28-VL-PRINC-CHEQ.

      *--- DADOS DO CLIENTE -------------------------------------------*

           MOVE WRK-BCO-CLI            TO SLIG28-BCO-CLI.
           MOVE WRK-AGENCIA-CLI        TO SLIG28-AGENCIA-CLI.
           MOVE WRK-CONTA-CLI          TO SLIG28-CONTA-CLI.

      *--- GRAVA EMITCHEQ - SLIG28 ------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           WRITE REG-CHEQUE-EMITENTE
           PERFORM 112000-TESTAR-FS-EMITCHEQ
           ADD 1                         TO ACU-GRAVA-EMITCHEQ.


      *----------------------------------------------------------------*
       320000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       321000-OBTER-DADOS-EMITENTE     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ECLIB-AREA-CLIBW020
                      SCLIB-AREA-CLIBW020.

           MOVE WRK-BCO-EMITT           TO ECLIB-BANCO.
           MOVE WRK-AGENCIA-EMITT       TO ECLIB-AGENCIA.
           MOVE WRK-CONTA-EMITT         TO WRK-CONTA-CLIE.
           MOVE WRK-CTA-CLIE            TO ECLIB-CONTA.

           MOVE    2                    TO WRK-FLAG
           ADD 1 TO ACU-ACESSOS-CLIB2000.

           PERFORM 321100-ACESSAR-CLIB2000.

           IF  SCLIB-RC        EQUAL ZEROS
               IF  SCLIB-TIPO-CONTA-C-P EQUAL 'C'
                   MOVE NOME            TO WRK-NOME-EMITT
                   ADD 1                TO ACU-EMITT-NOME-ENC-CC
                   IF WRK-CGC-CPF-EMITT EQUAL ZEROS
                      MOVE NUM-CGCCPF-ST    TO WRK-CGC-CPF-EMITT
                      MOVE FILIAL-CGCCPF-ST TO WRK-FILIAL-EMITT
                      MOVE CONTR-CGCCPF-ST  TO WRK-CGCCPF
                      MOVE WRK-CGCCPF-ST    TO WRK-CONTROLE-EMITT

                      ADD 1 TO ACU-EMITT-CGC-ATUALZ-CC
                   END-IF
               ELSE
                   MOVE POUPADOR-NOME-CLIE TO WRK-NOME-EMITT
                   ADD 1                   TO ACU-EMITT-NOME-ENC-PP
                   IF WRK-CGC-CPF-EMITT EQUAL ZEROS
                      MOVE POUPADOR-CGC-ST      TO WRK-CGC-CPF-EMITT
                      MOVE POUPADOR-FILIAL-ST   TO WRK-FILIAL-EMITT
                      MOVE POUPADOR-CONTR-ST    TO WRK-CGCCPF
                      MOVE WRK-CGCCPF-ST        TO WRK-CONTROLE-EMITT

                      ADD 1 TO ACU-EMITT-CGC-ATUALZ-PP

                   END-IF
               END-IF
           ELSE
               MOVE 'EMITENTE NAO ENCONTRADO'    TO WRK-NOME-EMITT
               ADD 1                             TO ACU-EMITT-NOME-N-ENC
               MOVE ZEROS                        TO WRK-CGC-CPF-EMITT
                                                    WRK-FILIAL-EMITT
                                                    WRK-CONTROLE-EMITT
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

           IF  SCLIB-RC   NOT EQUAL ZEROS
               PERFORM 321200-TRATA-RETORNO-NAO-OK
           END-IF.

      *----------------------------------------------------------------*
       321100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       321200-TRATA-RETORNO-NAO-OK     SECTION.
      *----------------------------------------------------------------*

           IF  SCLIB-RC       EQUAL 01
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
                  MOVE SCLIB-AREA-CLIBW020(1:75) TO ERR-TEXTO
                  DISPLAY 'FLAG CLIENTE/EMITENTE : ' WRK-FLAG

                  DISPLAY '%% CLIB2000 %%    BCO-AG/CTA: '
                  ECLIB-BANCO '-' ECLIB-AGENCIA '/' ECLIB-CONTA

                  DISPLAY '%% CLIB2000 %%    RET-CODE  : '
                          SCLIB-RC   '-' ERR-TEXTO

               END-IF
           END-IF.

      *----------------------------------------------------------------*
       321200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       322000-GRAVAR-MVCHQREL          SECTION.
      *----------------------------------------------------------------*


PHM        IF (SLIG27-CTPO-CHEQ  EQUAL   5 ) AND
PHM           (WRK-ACHOU         EQUAL  'N')
               GO TO  322000-99-FIM
           END-IF.

           INITIALIZE REG-CHEQUE-EMITENTE-RELATO.

      *--- DADOS DO EMITENTE ------------------------------------------*

           MOVE WRK-CGC-CPF-EMITT       TO SLIG74-CGC-CPF-EMITT-ST.
           MOVE WRK-FILIAL-EMITT        TO SLIG74-FILIAL-EMITT-ST.
           MOVE WRK-CONTROLE-EMITT      TO SLIG74-CONTROLE-EMITT-ST.
           MOVE WRK-BCO-EMITT           TO SLIG74-BCO-EMITT.
           MOVE WRK-AGENCIA-EMITT       TO SLIG74-AGENCIA-EMITT.
           MOVE WRK-CONTA-EMITT         TO SLIG74-CONTA-EMITT.
           MOVE WRK-NOME-EMITT          TO SLIG74-NOME-EMITT.
           MOVE WRK-GRP-ECONOMICO-EMITT TO SLIG74-GRP-ECONOMICO-EMITT.

           MOVE WRK-TIPO-PESSOA-EMITT   TO SLIG74-TIPO-PESSOA-EMITT.

           IF  WRK-CGC-CPF-EMITT  EQUAL  ZEROS
               MOVE 'J'                 TO SLIG74-TIPO-PESSOA-EMITT.


      *--- DADOS DO CHEQUE --------------------------------------------*

           MOVE WRK-CODI-DEVOL-CHQ        TO SLIG74-CODI-DEVOL-CHQ.
           MOVE WRK-CTPO-CHEQ             TO SLIG74-CTPO-CHEQ.
           MOVE WRK-NUM-CHEQUE            TO SLIG74-NUM-CHEQUE.
           MOVE WRK-DIG-CHEQUE            TO SLIG74-DIG-CHEQUE.
           MOVE WRK-NUM-LOTE              TO SLIG74-NUM-LOTE.
           MOVE WRK-CARTEIRA              TO SLIG74-CARTEIRA.
           MOVE WRK-VL-PRINC-CHEQ         TO SLIG74-VL-PRINC-CHEQ.
           MOVE ZEROS                     TO SLIG74-BCO-DIR-REGIONAL
                                             SLIG74-DIR-REGIONAL
                                             SLIG74-BCO-GERC-REGIONAL
                                             SLIG74-GERC-REGIONAL.

      *--- DADOS DO CLIENTE -------------------------------------------*

           MOVE WRK-BCO-CLI            TO SLIG74-BCO-CLI.
           MOVE WRK-AGENCIA-CLI        TO SLIG74-AGENCIA-CLI.
           MOVE WRK-CONTA-CLI          TO SLIG74-CONTA-CLI.

           MOVE ZEROS                  TO SLIG74-CGC-CPF-CLI-ST
                                          SLIG74-FILIAL-CLI-ST
                                          SLIG74-CONTROLE-CLI-ST.

           MOVE SPACES                 TO SLIG74-TIPO-PESSOA-CLI
                                          SLIG74-NOME-CLI.

      *--- DATAS DO CHEQUE --------------------------------------------*

           MOVE WRK-DT-DEPOSITO        TO SLIG74-DT-DEPOSITO.
           MOVE WRK-DT-CONTABIL        TO SLIG74-DT-CONTABIL.
           MOVE WRK-DT-ENTRADA-ORIGEM  TO SLIG74-DT-ENTRADA-ORIGEM.
           MOVE WRK-DT-SITUACAO        TO SLIG74-DT-SITUACAO.

      *--  ACUMULA TOTAL DE EMITENTES CGC ZERADOS NA SAIDA    ---------*

           IF SLIG74-CGC-CPF-EMITT-ST
           EQUAL ZEROS
              ADD 1 TO ACU-EMITT-CGC-ZEROS-OUT
           ELSE
              ADD 1 TO ACU-EMITT-CGC-DIF-ZEROS-OUT
           END-IF.

      *--  ACUMULA TOTAL DE NOMES EMITENTES  --------------------------*

           IF SLIG74-NOME-EMITT EQUAL 'EMITENTE NAO ENCONTRADO'
              ADD 1 TO ACU-EMITT-NOME-N-ENC-OUT
           ELSE
              IF SLIG74-NOME-EMITT EQUAL SPACES
                 ADD 1 TO ACU-EMITT-NOME-SPACES-OUT
              ELSE
                 ADD 1 TO ACU-EMITT-NOME-OK-OUT
              END-IF
           END-IF.

      *--  ACUMULA TOTAL DE GRUPOS EMITENTES  -------------------------*

           IF SLIG74-GRP-ECONOMICO-EMITT EQUAL ZEROS
              ADD 1 TO ACU-EMITT-GRP-ZEROS-OUT
           ELSE
              ADD 1 TO ACU-EMITT-GRP-DIF-ZEROS-OUT
           END-IF.

      *--- GRAVA MOVIMENTO PARA RELATORIO - MVCHQREL ------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           WRITE REG-CHEQUE-EMITENTE-RELATO.

           PERFORM 116000-TESTAR-FS-MVCHQREL.

PHM        MOVE 'N'                    TO WRK-ACHOU.

           ADD 1                       TO ACU-GRAVA-MVCHQREL.

      *----------------------------------------------------------------*
       322000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
CNPJ7C 0010-ACESSAR-GRUP201C         SECTION.
      *----------------------------------------------------------------*

           MOVE 'SLIG6210'             TO GRUPW01S-PGM-CHAMADOR.
           MOVE 'GRUP201C'             TO WRK-MODULO.
           MOVE SPACES                 TO WRK-IO-PCB
                                          WRK-ALT-PCB.

           CALL WRK-MODULO             USING GRUPW01S-REG-EMPRESA
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.

      *----------------------------------------------------------------*
CNPJ7C 0010-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       326000-OBTER-GRUPO-EMITENTE     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GRUPW01S-REG-EMPRESA.
           INITIALIZE                     GRUPW01S-REG-EMPRESA.

           MOVE '00'                   TO GRUPW01S-CGC-CPF(1:2)
           MOVE WRK-CGC-CPF-EMITT      TO GRUPW01S-CGC-CPF(3:9)
           MOVE WRK-FILIAL-EMITT       TO GRUPW01S-FILIAL-CGC-CPF.
           MOVE WRK-CONTROLE-EMITT     TO GRUPW01S-CTRL-CGC-CPF.

           MOVE ZEROS                  TO GRUPW01S-NUMERO-GRUPO-ENV.

CNPJ7C     PERFORM 0010-ACESSAR-GRUP201C.

           ADD 1 TO ACU-ACESSOS-GRUP201C.

           IF  GRUPW01S-RETURN-CODE EQUAL ZEROS
               MOVE GRUPW01S-NUMERO-GRUPO-ENV
                                          TO WRK-GRP-ECONOMICO-EMITT
               ADD 1 TO ACU-EMITT-GRP-ENC
           ELSE
               MOVE ZEROS                 TO WRK-GRP-ECONOMICO-EMITT
               ADD 1 TO ACU-EMITT-GRP-N-ENC
           END-IF.

      *----------------------------------------------------------------*
       326000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       330000-PROCESSAR-STATUS         SECTION.
      *----------------------------------------------------------------*

      *--- TRATAMENTO DOS STATUS ENCONTRADOS PARA O CHEQUES -----------*

           EVALUATE SLIG27-CTPO-CHEQ
              WHEN 1
                   PERFORM  330001-TRATA-TIPO-1
              WHEN 2
                   PERFORM  330002-TRATA-TIPO-2
              WHEN 3
                   PERFORM  330003-TRATA-TIPO-3
              WHEN 4
                   PERFORM  330004-TRATA-TIPO-4
              WHEN 5
                   PERFORM  330005-TRATA-TIPO-5
              WHEN OTHER
                 MOVE 'S'                 TO   WRK-ERRO-STATUS
                 MOVE SLIG27-DT-DEPOSITO  TO   WRK-DT-DEPOSITO-ERRO
                 MOVE SLIG27-DT-CONTABIL  TO   WRK-DT-CONTABIL-ERRO
           END-EVALUATE.

           PERFORM 310000-GUARDAR-CAMPOS.

      *--- GUARDA O ULTIMO STATUS ENCONTRADO PARA A CHAVE DO CHEQUE ---*

           PERFORM 330010-VER-ULTIMO-TIPO-CHEQUE.

           PERFORM 210000-LER-MVTCHQSR.

      *----------------------------------------------------------------*
       330000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       330001-TRATA-TIPO-1             SECTION.
      *----------------------------------------------------------------*

      *--- TRATA BAIXA NORMAL DO PERIODO ANTERIO ----------------------*

           IF SLIG27-DT-DEPOSITO < WRK-DT-INI-PER-ATU
              ADD   1           TO WRK-TOT-ANT-BAIX
                                   WRK-ACU-TOT-ANT-BAIX
           ELSE

      *--- TRATA BAIXA ANTECIPADA -------------------------------------*

           IF SLIG27-DT-CONTABIL < SLIG27-DT-DEPOSITO

              IF SLIG27-DT-CONTABIL NOT LESS
                 WRK-DT-INI-PER-ATU              AND
                 SLIG27-DT-CONTABIL NOT GREATER
                 WRK-DT-FIM-PER-ATU

      *--- TRATA BAIXA ANTECIPADA PERIODO ATUAL -----------------------*

                 IF SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ATU
                    ADD 1                 TO WRK-TOT-PER-BAX-ANT
                                             WRK-ACU-TOT-PER-BAX-ANT
                 ELSE

      *--- TRATA BAIXA ANTECIPADA PERIODO FUTURO ----------------------*

                    ADD  1                TO WRK-TOT-FUT-BAX-ANT
                                             WRK-ACU-TOT-FUT-BAX-ANT
                 END-IF
              END-IF
           ELSE

      *--- TRATA BAIXA NORMAL PERIODO ATUAL ---------------------------*

              IF SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ATU
                 ADD    1              TO WRK-TOT-PER-BAIX
                                          WRK-ACU-TOT-PER-BAIX
              ELSE

      *--- TRATA BAIXA NORMAL PERIODO FUTURO---------------------------*

                 ADD    1              TO WRK-TOT-FUT-BAIX
                                          WRK-ACU-TOT-FUT-BAIX
              END-IF
           END-IF
           END-IF.

      *----------------------------------------------------------------*
       330001-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       330002-TRATA-TIPO-2             SECTION.
      *----------------------------------------------------------------*

      *--- TRATA COMPENSACAO PERIODO ANTERIOR -------------------------*

           IF SLIG27-DT-CONTABIL < WRK-DT-INI-PER-ATU
              IF (SLIG27-DT-DEPOSITO NOT < WRK-DT-INI-PER-ATU AND
                  SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ATU)
                  ADD   1           TO WRK-TOT-PER-COMP
                                       WRK-ACU-TOT-PER-COMP
              END-IF

              IF (SLIG27-DT-DEPOSITO NOT < WRK-DT-INI-PER-ULT AND
                  SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ULT)
                  ADD   1           TO WRK-TOT-ANT-COMP
                                       WRK-ACU-TOT-ANT-COMP
              END-IF
           ELSE

      *--- TRATA COMPENSACAO PERIODO ATUAL ----------------------------*

           IF SLIG27-DT-CONTABIL NOT > WRK-DT-FIM-PER-ATU

              IF (SLIG27-DT-DEPOSITO NOT < WRK-DT-INI-PER-ATU AND
                  SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ATU)
                  ADD   1           TO WRK-TOT-PER-COMP
                                       WRK-ACU-TOT-PER-COMP
              END-IF

              IF (SLIG27-DT-DEPOSITO NOT < WRK-DT-INI-PER-ULT AND
                  SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ULT)
                  ADD   1           TO WRK-TOT-ANT-COMP
                                       WRK-ACU-TOT-ANT-COMP
              END-IF
           ELSE

      *--- TRATA COMPENSACAO PERIODO FUTURO ---------------------------*

              IF (SLIG27-DT-DEPOSITO NOT < WRK-DT-INI-PER-ATU AND
                  SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ATU)
                  ADD   1           TO WRK-TOT-PER-COMP
                                       WRK-ACU-TOT-PER-COMP
              END-IF

      *       IF (SLIG27-DT-DEPOSITO NOT < WRK-DT-INI-PER-PRX AND
      *           SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-PRX)
      *           ADD   1           TO WRK-TOT-FUT-COMP
      *                                WRK-ACU-TOT-FUT-COMP
      *       END-IF
           END-IF
           END-IF.

      *----------------------------------------------------------------*
       330002-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       330003-TRATA-TIPO-3             SECTION.
      *----------------------------------------------------------------*

      *--- IDENTIFICA REENTRADA QUANDO NAO HOUVE DEVOLUCAO ------------*

           IF WRK-TOT-ANT-DEVL-S-RESP EQUAL ZEROS AND
              WRK-TOT-ANT-DEVL-S-REAP EQUAL ZEROS AND
              WRK-TOT-PER-DEVL-S-RESP EQUAL ZEROS AND
              WRK-TOT-PER-DEVL-S-REAP EQUAL ZEROS AND
              WRK-TOT-FUT-DEVL-S-RESP EQUAL ZEROS AND
              WRK-TOT-FUT-DEVL-S-REAP EQUAL ZEROS

      *--- CONTABILIZA ABERTOS ----------------------------------------*

              MOVE 4               TO SLIG27-CTPO-CHEQ
              PERFORM                 330004-TRATA-TIPO-4
           ELSE

      *--- VERIFICA DT EVENTO DA ULTIMA DEVOLUCAO ENCONTRADA ----------*

           IF  WRK-ULTIMA-DEVOLUCAO EQUAL ZEROS
               PERFORM  330055-TRATA-ERRO-DEVOLUCAO
           END-IF

      *--- IDENTIFICA REENTRADA QUANDO HA DEVOLUCAO -------------------*

           IF SLIG27-DT-CONTABIL < WRK-ULTIMA-DEVOLUCAO

      *--- CONTABILIZA ABERTOS ----------------------------------------*

              MOVE 4            TO SLIG27-CTPO-CHEQ
              PERFORM              330004-TRATA-TIPO-4

           ELSE

      *--- VERIFICA RESPONSABILIDADE DA REAPRESENTACAO ----------------*

           IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL SPACES
              PERFORM              330033-TRATA-ERRO-REAP
           END-IF

      *--- TRATA REAPRESENTACAO PERIODO ANTERIOR ----------------------*

           IF SLIG27-DT-CONTABIL < WRK-DT-INI-PER-ATU

              IF (SLIG27-DT-DEPOSITO NOT < WRK-DT-INI-PER-ATU AND
                  SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ATU)

                  IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                     ADD   1              TO WRK-TOT-PER-REAP-S-RESP
                                             WRK-ACU-TOT-PER-REAP-S-RESP
                  ELSE
                     ADD   1              TO WRK-TOT-PER-REAP-N-RESP
                                             WRK-ACU-TOT-PER-REAP-N-RESP
                  END-IF
              END-IF

              IF (SLIG27-DT-DEPOSITO NOT < WRK-DT-INI-PER-ULT AND
                  SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ULT)

                  IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                     ADD   1              TO WRK-TOT-ANT-REAP-S-RESP
                                             WRK-ACU-TOT-ANT-REAP-S-RESP
                  ELSE
                     ADD   1              TO WRK-TOT-ANT-REAP-N-RESP
                                             WRK-ACU-TOT-ANT-REAP-N-RESP
                  END-IF
              END-IF
           ELSE

      *--- TRATA REAPRESENTACAO PERIODO ATUAL -------------------------*

           IF SLIG27-DT-CONTABIL NOT > WRK-DT-FIM-PER-ATU

              IF (SLIG27-DT-DEPOSITO NOT < WRK-DT-INI-PER-ATU AND
                  SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ATU)

                 IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                    ADD   1          TO WRK-TOT-PER-REAP-S-RESP
                                        WRK-ACU-TOT-PER-REAP-S-RESP
                 ELSE
                    ADD   1          TO WRK-TOT-PER-REAP-N-RESP
                                        WRK-ACU-TOT-PER-REAP-N-RESP
                 END-IF
              END-IF

              IF (SLIG27-DT-DEPOSITO NOT < WRK-DT-INI-PER-ULT AND
                  SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ULT)

                 IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                    ADD   1          TO WRK-TOT-ANT-REAP-S-RESP
                                        WRK-ACU-TOT-ANT-REAP-S-RESP
                 ELSE
                    ADD   1          TO WRK-TOT-ANT-REAP-N-RESP
                                        WRK-ACU-TOT-ANT-REAP-N-RESP
                 END-IF
              END-IF

           ELSE

      *--- TRATA REAPRESENTACAO PERIODO FUTURO ------------------------*

              IF (SLIG27-DT-DEPOSITO NOT < WRK-DT-INI-PER-ATU AND
                  SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ATU)

                 IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                    ADD   1             TO WRK-TOT-PER-REAP-S-RESP
                                           WRK-ACU-TOT-PER-REAP-S-RESP
                 ELSE
                    ADD   1             TO WRK-TOT-PER-REAP-N-RESP
                                           WRK-ACU-TOT-PER-REAP-N-RESP
                 END-IF
              END-IF

      *       IF (SLIG27-DT-DEPOSITO NOT < WRK-DT-INI-PER-PRX AND
      *           SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-PRX)
      *
      *          IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
      *             ADD   1             TO WRK-TOT-FUT-REAP-S-RESP
      *                                    WRK-ACU-TOT-FUT-REAP-S-RESP
      *          ELSE
      *             ADD   1             TO WRK-TOT-FUT-REAP-N-RESP
      *                                    WRK-ACU-TOT-FUT-REAP-N-RESP
      *          END-IF
      *       END-IF

           END-IF
           END-IF
           END-IF
           END-IF.

      *----------------------------------------------------------------*
       330003-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       330033-TRATA-ERRO-REAP          SECTION.
      *----------------------------------------------------------------*

           DISPLAY '***************** SLIG6210 *******************'
           DISPLAY '*                                            *'
           DISPLAY '*         PROCESSAMENTO INTERROMPIDO         *'
           DISPLAY '*                                            *'
           DISPLAY '*     REAPRESENTACAO SEM DEVOLUCAO           *'
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
                                                        '        *'
           DISPLAY '*     DATA-DEPOSITO   : ' WRK-DT-DEP-ATU
                                                        '        *'
           DISPLAY '*                                            *'
           DISPLAY '*     DATA-EVENTO     : ' WRK-DT-EVT-ATU
                                                        '        *'
           DISPLAY '*                                            *'
           DISPLAY '****************** SLIG6210 ******************'
           MOVE 'APL'              TO ERR-TIPO-ACESSO
           MOVE  WRK-MSG-ERRO-REAP TO ERR-TEXTO

           PERFORM 999999-PROCESSAR-ROTINA-ERRO.

      *----------------------------------------------------------------*
       330033-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       330004-TRATA-TIPO-4             SECTION.
      *----------------------------------------------------------------*

      *--- TRATA ABERTOS DO PERIODO ANTERIOR --------------------------*

           IF SLIG27-DT-DEPOSITO < WRK-DT-INI-PER-ATU
              ADD 1             TO WRK-TOT-ANT-ABER
                                   WRK-ACU-TOT-ANT-ABER
           ELSE

      *--- TRATA ABERTOS DO PERIODO ATUAL -----------------------------*

              IF SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ATU
                 ADD 1                TO  WRK-TOT-PER-ABER
                                          WRK-ACU-TOT-PER-ABER
              ELSE

      *--- TRATA ABERTOS DO PERIODO FUTURO ----------------------------*

                 ADD 1                TO  WRK-TOT-FUT-ABER
                                          WRK-ACU-TOT-FUT-ABER
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       330004-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       330005-TRATA-TIPO-5             SECTION.
      *----------------------------------------------------------------*

      *--- VERIFICA RESPONSABILIDA E POSSIBILADE DE REAPRESENTACAO ----*

           MOVE  'N'              TO   WRK-ACHOU.

           PERFORM  230000-VER-DEVOLUCAO  VARYING WRK-IND FROM 1 BY 1
                    UNTIL WRK-IND GREATER 99 OR WRK-ACHOU EQUAL 'S'.

           IF  WRK-ACHOU  EQUAL  'N'
               PERFORM  330055-TRATA-ERRO-DEVOLUCAO
           ELSE

      *--- TRATA DEVOLUCAO COM EVENTO NO PERIODO ANTERIOR -------------*

           IF SLIG27-DT-CONTABIL <   WRK-DT-INI-PER-ATU
               PERFORM  330006-TRATA-DEVL-ANT
           ELSE

      *--- TRATA DEVOLUCAO COM EVENTO NO PERIODO ATUAL ----------------*

           IF SLIG27-DT-CONTABIL NOT > WRK-DT-FIM-PER-ATU
               PERFORM  330007-TRATA-DEVL-ATU
           ELSE

      *--- TRATA DEVOLUCAO COM EVENTO NO PERIODO FUTURO ---------------*

               PERFORM  330008-TRATA-DEVL-FUT
           END-IF
           END-IF
           END-IF.

      *----------------------------------------------------------------*
       330005-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       330055-TRATA-ERRO-DEVOLUCAO     SECTION.
      *----------------------------------------------------------------*

           DISPLAY '***************** SLIG6210 *******************'
           DISPLAY '*                                            *'
           DISPLAY '*     C H E Q U E   D E S P R E Z A D O      *'
           DISPLAY '*                                            *'
           DISPLAY '*     CODIGO DE DEVOLUCAO NAO ENCONTADO      *'
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
           DISPLAY '*     DATA-DEPOSITO   : ' WRK-DT-DEP-ATU
                                                   '             *'
           DISPLAY '*                                            *'
           DISPLAY '*     DATA-EVENTO     : ' WRK-DT-EVT-ATU
                                                   '             *'
           DISPLAY '*                                            *'
           DISPLAY '*     COD-DEVOLUCAO   : ' SLIG27-CODI-DEVOL-CHQ
                                             '                   *'
           DISPLAY '*                                            *'
           DISPLAY '****************** SLIG6210 ******************'
           MOVE 'APL'              TO ERR-TIPO-ACESSO
           MOVE  WRK-MSG-ERRO-DEVL TO ERR-TEXTO.

******* PARA RESOLUCAO DO R.O 1281455 INICIO
PHM   *    PERFORM 999999-PROCESSAR-ROTINA-ERRO.
******* FIM

      *----------------------------------------------------------------*
       330055-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       330006-TRATA-DEVL-ANT           SECTION.
      *----------------------------------------------------------------*

           IF (SLIG27-DT-DEPOSITO NOT < WRK-DT-INI-PER-ATU AND
               SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ATU)

              IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                 ADD   1                TO WRK-TOT-PER-DEVL-S-RESP
                                           WRK-ACU-TOT-PER-DEVL-S-RESP
              ELSE
                 IF WRK-REAP-ULTIMA-DEVOLUCAO EQUAL 'S'
                    ADD   1             TO WRK-TOT-PER-DEVL-S-REAP
                                           WRK-ACU-TOT-PER-DEVL-S-REAP
                 ELSE
                    ADD   1             TO WRK-TOT-PER-DEVL-N-REAP
                                           WRK-ACU-TOT-PER-DEVL-N-REAP
                 END-IF
              END-IF
           END-IF.

           IF (SLIG27-DT-DEPOSITO NOT < WRK-DT-INI-PER-ULT AND
               SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ULT)

              IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                 ADD   1                TO WRK-TOT-ANT-DEVL-S-RESP
                                           WRK-ACU-TOT-ANT-DEVL-S-RESP
              ELSE
                 IF WRK-REAP-ULTIMA-DEVOLUCAO EQUAL 'S'
                    ADD   1             TO WRK-TOT-ANT-DEVL-S-REAP
                                           WRK-ACU-TOT-ANT-DEVL-S-REAP
                 ELSE
                    ADD   1             TO WRK-TOT-ANT-DEVL-N-REAP
                                           WRK-ACU-TOT-ANT-DEVL-N-REAP
                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       330006-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       330007-TRATA-DEVL-ATU           SECTION.
      *----------------------------------------------------------------*

           IF (SLIG27-DT-DEPOSITO NOT < WRK-DT-INI-PER-ATU AND
               SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ATU)

              IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                 ADD   1                TO WRK-TOT-PER-DEVL-S-RESP
                                           WRK-ACU-TOT-PER-DEVL-S-RESP
              ELSE
                 IF WRK-REAP-ULTIMA-DEVOLUCAO EQUAL 'S'
                    ADD   1             TO WRK-TOT-PER-DEVL-S-REAP
                                           WRK-ACU-TOT-PER-DEVL-S-REAP
                 ELSE
                    ADD   1             TO WRK-TOT-PER-DEVL-N-REAP
                                           WRK-ACU-TOT-PER-DEVL-N-REAP
                 END-IF
              END-IF
           END-IF.

           IF (SLIG27-DT-DEPOSITO NOT < WRK-DT-INI-PER-ULT AND
               SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ULT)

              IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                 ADD   1                TO WRK-TOT-ANT-DEVL-S-RESP
                                           WRK-ACU-TOT-ANT-DEVL-S-RESP
              ELSE
                 IF WRK-REAP-ULTIMA-DEVOLUCAO EQUAL 'S'
                    ADD   1             TO WRK-TOT-ANT-DEVL-S-REAP
                                           WRK-ACU-TOT-ANT-DEVL-S-REAP
                 ELSE
                    ADD   1             TO WRK-TOT-ANT-DEVL-N-REAP
                                           WRK-ACU-TOT-ANT-DEVL-N-REAP
                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       330007-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       330008-TRATA-DEVL-FUT           SECTION.
      *----------------------------------------------------------------*

           IF (SLIG27-DT-DEPOSITO NOT < WRK-DT-INI-PER-ATU AND
               SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ATU)

              IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                 ADD   1                TO WRK-TOT-PER-DEVL-S-RESP
                                           WRK-ACU-TOT-PER-DEVL-S-RESP
              ELSE
                 IF WRK-REAP-ULTIMA-DEVOLUCAO EQUAL 'S'
                    ADD   1             TO WRK-TOT-PER-DEVL-S-REAP
                                           WRK-ACU-TOT-PER-DEVL-S-REAP
                 ELSE
                    ADD   1             TO WRK-TOT-PER-DEVL-N-REAP
                                           WRK-ACU-TOT-PER-DEVL-N-REAP
                 END-IF
              END-IF
           END-IF.

      *    IF (SLIG27-DT-DEPOSITO NOT < WRK-DT-INI-PER-PRX AND
      *        SLIG27-DT-DEPOSITO NOT > WRK-DT-FIM-PER-PRX)
      *
      *       IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
      *          ADD   1                TO WRK-TOT-FUT-DEVL-S-RESP
      *                                    WRK-ACU-TOT-FUT-DEVL-S-RESP
      *       ELSE
      *          IF WRK-REAP-ULTIMA-DEVOLUCAO EQUAL 'S'
      *             ADD   1             TO WRK-TOT-FUT-DEVL-S-REAP
      *                                    WRK-ACU-TOT-FUT-DEVL-S-REAP
      *          ELSE
      *             ADD   1             TO WRK-TOT-FUT-DEVL-N-REAP
      *                                    WRK-ACU-TOT-FUT-DEVL-N-REAP
      *          END-IF
      *       END-IF
      *    END-IF.


      *----------------------------------------------------------------*
       330008-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       330010-VER-ULTIMO-TIPO-CHEQUE   SECTION.
      *----------------------------------------------------------------*

           IF SLIG27-DT-MOVIMENTO  GREATER  WRK-DT-MOVIMENTO-ULT
              PERFORM 330015-ATU-ULTIMO-TIPO-CHEQUE
           ELSE
              IF (SLIG27-DT-MOVIMENTO  EQUAL
                  WRK-DT-MOVIMENTO-ULT  AND
                  SLIG27-DT-CONTABIL  GREATER
                  WRK-DT-CONTABIL-ULT)
                  PERFORM 330015-ATU-ULTIMO-TIPO-CHEQUE
              ELSE
                  IF (SLIG27-DT-CONTABIL  EQUAL
                      WRK-DT-CONTABIL-ULT  AND
                      SLIG27-DT-SITUACAO  GREATER
                      WRK-DT-SITUACAO-ULT)
                      PERFORM 330015-ATU-ULTIMO-TIPO-CHEQUE
                  END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       330010-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       330015-ATU-ULTIMO-TIPO-CHEQUE   SECTION.
      *----------------------------------------------------------------*

           MOVE SLIG27-DT-MOVIMENTO  TO  WRK-DT-MOVIMENTO-ULT.
           MOVE SLIG27-DT-CONTABIL   TO  WRK-DT-CONTABIL-ULT.
           MOVE SLIG27-DT-SITUACAO   TO  WRK-DT-SITUACAO-ULT.
           MOVE SLIG27-CTPO-CHEQ     TO  WRK-ULT-CTPO-CHEQ.

      *----------------------------------------------------------------*
       330015-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       340000-TRATA-ERRO-SITUACAO      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     REG-ERROS.

           MOVE WRK-CHV-EMITENTE-ANT  TO  SLIG99-CHV-EMITENTE.
           MOVE WRK-ULT-CTPO-CHEQ     TO  SLIG99-CTPO-CHEQ.
           MOVE WRK-DT-DEPOSITO-ERRO  TO  SLIG99-DT-DEPOSITO.
           MOVE WRK-DT-EVT-ATU        TO  SLIG99-DT-CONTABIL.

           WRITE REG-ERROS.

           PERFORM 114000-TESTAR-FS-ARQERROS.

           ADD 1                      TO  ACU-GRAVA-ARQERROS.

           IF ACU-GRAVA-ARQERROS    LESS  101
              DISPLAY '***************** SLIG6210 *******************'
              DISPLAY '*                                            *'
              DISPLAY '*   SITUACAO OU DATA DE DEPOSITO DO CHEQUE   *'
              DISPLAY '*                 INESPERADA                 *'
              DISPLAY '*                                            *'
              DISPLAY '***********  WRK-CHV-EMITENTE-ANT  ***********'
              DISPLAY '*                                            *'
              DISPLAY '* WRK-BANCO-ANT   = ' WRK-BANCO-ANT
                                             '                      *'
              DISPLAY '* WRK-AGENCIA-ANT = ' WRK-AGENCIA-ANT
                                               '                    *'
              DISPLAY '* WRK-CONTA-ANT   = ' WRK-CONTA-ANT
                                                       '            *'
              DISPLAY '* WRK-CHEQUE-ANT  = ' WRK-CHEQUE-ANT
                                                 '                  *'
              DISPLAY '*                                            *'
              DISPLAY '* WRK-DT-VENCTO   = ' WRK-DT-DEPOSITO-ERRO
                                                 '                  *'
              DISPLAY '*                                            *'
              DISPLAY '* WRK-DT-CONTABIL = ' WRK-DT-CONTABIL
                                                  '                 *'
              DISPLAY '*                                            *'
              DISPLAY '*----------  SITUACAO  ENCONTRADA  ----------*'
              DISPLAY '* ' WRK-ULT-CTPO-CHEQ
                           '                                        *'
              DISPLAY '*                                            *'
              DISPLAY '**********************************************'
           END-IF.

      *----------------------------------------------------------------*
       340000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       350000-GRAVAR-QTDEBCO           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE REG-QTDEBCO.

      *--- DADOS DE STATUS DO CHEQUE ----------------------------------*

           MOVE WRK-BANCO-ANT          TO REG-QTDEBCO-CODBCO.
           MOVE WRK-QTDE-CHQ-BCO       TO REG-QTDEBCO-QTDE.

           WRITE REG-QTDEBCO.
           PERFORM 118000-TESTAR-FS-QTDEBCO
           ADD   1                     TO  ACU-GRAVA-QTDEBCO.

      *----------------------------------------------------------------*
       350000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       400000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 420000-GRAVAR-MOTDEVOL
                   VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 99.

           PERFORM 410000-EMITIR-SRELCTRL.

           CLOSE MVTCHQSR
                 EMITCHEQ
                 SRELCTRL
                 ARQERROS
                 MVCHDESP
                 QTDEBCO
                 MOTDEVOL.

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
           MOVE WRK-DT-INI-PER-ATU     TO CB5-DT-INI-PER.
           MOVE WRK-DT-FIM-PER-ATU     TO CB6-DT-FIM-PER.

      *--- MOVE QUANTIDADE DE LIDOS E GRAVADOS ------------------------*

           MOVE ACU-LIDOS-MVTCHQSR     TO LT1-TOT-LIDOS.
           MOVE ACU-GRAVA-EMITCHEQ     TO LT2-TOT-GRAVADOS.
           MOVE ACU-GRAVA-ARQERROS     TO LT3-TOT-GRAVADOS.
           MOVE ACU-GRAVA-MVCHQREL     TO LT4-TOT-GRAVADOS.
           MOVE ACU-GRAVA-MVCHDESP     TO LT4A-TOT-GRAVADOS.
           MOVE ACU-GRAVA-QTDEBCO      TO LT4B-TOT-GRAVADOS.
           MOVE ACU-GRAVA-MOTDEVOL     TO LT4C-TOT-GRAVADOS.

      *--- MOVE QUANTIDADE ERRO E NAO ENCONTRADOS NO CLIB2000 ---------*

           MOVE ACU-N-ENC-CLIB2000     TO LT5-TOT-GRAVADOS.
           MOVE ACU-ERROR-CLIB2000     TO LT6-TOT-GRAVADOS.

      *--- MOVE QUANTIDADE DE STATUS ENCONTRADOS NO PERIODO E APOS ----*

           MOVE WRK-ACU-TOT-PER-COMP           TO
                LT7-TOT-PER-COMP.

           MOVE WRK-ACU-TOT-PER-REAP-S-RESP    TO
                LT8-TOT-PER-REAP-S-RESP.

           MOVE WRK-ACU-TOT-PER-REAP-N-RESP    TO
                LT8A-TOT-PER-REAP-N-RESP.

           MOVE WRK-ACU-TOT-PER-DEVL-S-RESP    TO
                LT9-TOT-PER-DEVL-S-RESP.

           MOVE WRK-ACU-TOT-PER-DEVL-N-REAP    TO
                LT9A-TOT-PER-DEVL-N-REAP.

           MOVE WRK-ACU-TOT-PER-DEVL-S-REAP    TO
                LT9B-TOT-PER-DEVL-S-REAP.

           MOVE WRK-ACU-TOT-PER-BAIX           TO
                LT10-TOT-PER-BAIXS.

           MOVE WRK-ACU-TOT-PER-BAX-ANT        TO
                LT11-TOT-PER-BAX-ANT.

           MOVE WRK-ACU-TOT-FUT-BAX-ANT        TO
                LT12-TOT-PER-BAX-F.

           MOVE WRK-ACU-TOT-FUT-ABER           TO
                LT13-TOT-FUT-ABER.

           MOVE WRK-ACU-TOT-FUT-REAP-S-RESP    TO
                LT14-TOT-FUT-REAP-S-RESP.

           MOVE WRK-ACU-TOT-FUT-REAP-N-RESP    TO
                LT14A-TOT-FUT-REAP-N-RESP.

           MOVE WRK-ACU-TOT-FUT-DEVL-S-RESP    TO
                LT14B-TOT-FUT-DEVL-S-RESP.

           MOVE WRK-ACU-TOT-FUT-DEVL-N-REAP    TO
                LT14C-TOT-FUT-DEVL-N-REAP.

           MOVE WRK-ACU-TOT-FUT-DEVL-S-REAP    TO
                LT14D-TOT-FUT-DEVL-S-REAP.

           MOVE WRK-ACU-TOT-FUT-COMP           TO
                LT41-TOT-FUT-COMP.

           MOVE WRK-ACU-TOT-FUT-BAIX           TO
                LT43-TOT-FUT-BAIXS.

           MOVE WRK-ACU-TOT-ANT-COMP           TO
                LT15-TOT-ANT-COMP.

           MOVE WRK-ACU-TOT-ANT-REAP-S-RESP    TO
                LT16-TOT-ANT-REAP-S-RESP.

           MOVE WRK-ACU-TOT-ANT-REAP-N-RESP    TO
                LT16A-TOT-ANT-REAP-N-RESP.

           MOVE WRK-ACU-TOT-ANT-DEVL-S-RESP    TO
                LT17-TOT-ANT-DEVL-S-RESP

           MOVE WRK-ACU-TOT-ANT-DEVL-N-REAP    TO
                LT17A-TOT-ANT-DEVL-N-REAP

           MOVE WRK-ACU-TOT-ANT-DEVL-S-REAP    TO
                LT17B-TOT-ANT-DEVL-S-REAP

           MOVE WRK-ACU-TOT-ANT-BAIX           TO
                LT18-TOT-ANT-BAIX.

           MOVE WRK-ACU-TOT-ANT-ABER           TO
                LT19-TOT-ANT-ABER.

           MOVE WRK-ACU-TOT-PER-ABER           TO
                LT20-TOT-PER-ABER.

           MOVE ACU-EMITT-BCO-237              TO
                LT21-TOT-BCO-237.

           MOVE ACU-EMITT-BCO-DIF-237          TO
                LT22-TOT-BCO-DIF-237.

           MOVE ACU-EMITT-CGC-ZEROS-IN         TO
                LT23-TOT-CGC-ZEROS.

           MOVE ACU-EMITT-CGC-DIF-ZEROS-IN     TO
                LT24-TOT-CGC-DIF-ZEROS.

           MOVE ACU-ACESSOS-CLIB2000           TO
                LT25-TOT-ACESS-CLIB2000.

           MOVE ACU-EMITT-CGC-ATUALZ-CC        TO
                LT26-TOT-CGC-ATUALZ-CC.

           MOVE ACU-EMITT-CGC-ATUALZ-PP        TO
                LT27-TOT-CGC-ATUALZ-PP.

           MOVE ACU-EMITT-NOME-ENC-CC          TO
                LT28-TOT-NOME-ENC-CC.

           MOVE ACU-EMITT-NOME-ENC-PP          TO
                LT29-TOT-NOME-ENC-PP.

           MOVE ACU-EMITT-NOME-N-ENC           TO
                LT30-TOT-NOME-N-ENC.

           MOVE ACU-ACESSOS-GRUP201C           TO
CNPJ7C          WRK-TOT-ACESS-GRUP201C.

           MOVE ACU-EMITT-GRP-ENC              TO
                LT32-TOT-GRP-ENC.

           MOVE ACU-EMITT-GRP-N-ENC            TO
                LT33-TOT-GRP-N-ENC.

           MOVE ACU-EMITT-CGC-ZEROS-OUT        TO
                LT34-TOT-CGC-ZEROS.

           MOVE ACU-EMITT-CGC-DIF-ZEROS-OUT    TO
                LT35-TOT-CGC-DIF-ZEROS.

           MOVE ACU-EMITT-NOME-N-ENC-OUT       TO
                LT36-TOT-NOME-N-ENC.

           MOVE ACU-EMITT-NOME-SPACES-OUT      TO
                LT37-TOT-NOME-SPACES.

           MOVE ACU-EMITT-NOME-OK-OUT          TO
                LT38-TOT-NOME-OK.

           MOVE ACU-EMITT-GRP-ZEROS-OUT        TO
                LT39-TOT-GRP-ZEROS.

           MOVE ACU-EMITT-GRP-DIF-ZEROS-OUT    TO
                LT40-TOT-GRP-DIF-ZEROS.

      *--- GRAVA RELATORIO DE CONTROLE - SRELCTRL ---------------------*

           WRITE REG-SRELCTRL          FROM CABEC1.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC2.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC3.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC4.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC5.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC6.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINHIFEN.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT1.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT2.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT3.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT4.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT4A.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT4B.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT4C.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINHIFEN.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT25.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT5.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT6.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT28.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT29.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT30.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT36.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT37.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT38.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINHIFEN.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT21.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT22.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINHIFEN.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT23.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT24.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT26.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT27.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT34.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT35.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINHIFEN.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT31.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT32.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT33.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT39.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT40.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINHIFEN.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT7.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT8.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT8A.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT9.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT9A.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT9B.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT10.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT11.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT12.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT13.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT14.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT14A.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT14B.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT14C.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT14D.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT41.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT43.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT15.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT16.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT16A.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT17.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT17A.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT17B.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT18.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT19.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT20.
           PERFORM 113000-TESTAR-FS-SRELCTRL.

      *----------------------------------------------------------------*
       410000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       420000-GRAVAR-MOTDEVOL          SECTION.
      *----------------------------------------------------------------*

           IF   WRK-CTPO-DEVLC(WRK-IND)  EQUAL  ZEROS
                GO  TO  420000-99-FIM.

           MOVE WRK-CTPO-DEVLC(WRK-IND)      TO SLIGC1-CTPO-DEVLC
           MOVE WRK-RESP-DEVLC(WRK-IND)      TO SLIGC1-RESP-DEVLC
           MOVE WRK-POBLD-REAP(WRK-IND)      TO SLIGC1-POBLD-REAP

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO
           WRITE REG-MOVTO-DEVOLUCAO
           PERFORM 119000-TESTAR-FS-MOTDEVOL
           ADD   1                     TO ACU-GRAVA-MOTDEVOL.

      *----------------------------------------------------------------*
       420000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'SLIG6210'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             WRK-SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
