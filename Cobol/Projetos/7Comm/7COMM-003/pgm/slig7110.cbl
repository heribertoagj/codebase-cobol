      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. SLIG7110.
       AUTHOR.     REGINALDO PEDROSO.
      *================================================================*
      *                    D T S  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   SLIG7110                                    *
      *    PROGRAMADORA:   REGINALDO PEDROSO       - DTS               *
      *    ANALISTA....:   REGINALDO PEDROSO       - DTS               *
      *    DATA........:   10/03/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR RELATORIO DE ANALISE DE PERFORMANCE   *
      *      DOS PRODUTOS TITULOS, CHEQUES E VENDOR POR SACADO.        *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    ARQDATAS                  I#SLIG00          *
      *                    ARQDATAS                  I#SLIG00          *
      *                    MOVTOSEL                  I#SLIG62          *
      *                    MOTDEVOL                  I#SLIGC1
      *                    GW333333                     -              *
      *                    SRELCTRL                     -              *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C - LAYOUT PARA UTILIZACAO PELA POOL7100.            *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                TABLE                       INCLUDE/BOOK        *
      *                    DB2PRD.VLR_DISCT_COBR     CBGEB033          *
      *                    DB2PRD.ID_SACDO_LIQDZ     SLIGB030          *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    CBPG8010 - MODULO PARA CALCULO VALOR CONVERTIDO PELA NTAX.  *
      *    POOL1050 - MODULO QUE FAZ CONEXAO COM DB2.                  *
      *    POOL0025 - MODULO QUE SOMA DIAS NUMA DATA                   *
      *    POOL7100 - MODULO DE TRATAMENTO DE ERROS PARAMETROS.        *
      *    POOL7600 - MODULO PARA VERIFICAR HORA/DATA CORRENTE.        *
=COLS> ----+----1----+----2----+----3----+----4----+----5----+----6----+
      *----------------------------------------------------------------*
      *                      A L T E R A C A O                         *
      *----------------------------------------------------------------*
      *  ALTERADO POR : MARIO SERGIO P. NETO - DES                     *
      *  DATA........ : 10/2005                                        *
      *  ALTERACAO....: INCLUIR CHEQUES DEVOLUCAO PERIODO ANTERIOR NO  *
      *                 RELATORIO                                      *
      *----------------------------------------------------------------*
4SYS01*  ALTERADO POR : LUCAS DEL ROY   - FOURSYS                      *
4SYS01*  DATA........ : 08/05/2019                                     *
4SYS01*  ALTERACAO....: AJUSTAR A DATA DE CARENCIA DE 10 DIAS CORRIDOS *
4SYS01*                 PARA 3 DIAS UTEIS CONFORME PEDIDO DO GESTOR    *
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

           SELECT ARQDATAS ASSIGN      TO UT-S-ARQDATAS
           FILE STATUS                 IS WRK-FS-ARQDATAS.

           SELECT MOVTOSEL ASSIGN      TO UT-S-MOVTOSEL
           FILE STATUS                 IS WRK-FS-MOVTOSEL.

           SELECT GW333333 ASSIGN      TO UT-S-GW333333
           FILE STATUS                 IS WRK-FS-GW333333.

           SELECT SRELCTRL ASSIGN      TO UT-S-SRELCTRL
           FILE STATUS                 IS WRK-FS-SRELCTRL.

           SELECT MOTDEVOL ASSIGN      TO UT-S-MOTDEVOL
           FILE STATUS                 IS WRK-FS-MOTDEVOL.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO ARQDATAS.                                   *
      *            ORG. SEQUENCIAL     -   LRECL   =   100             *
      *----------------------------------------------------------------*

       FD  ARQDATAS
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG00

           EJECT
      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO MOVTOSEL.                                   *
      *            ORG. SEQUENCIAL     -   LRECL   =   308             *
      *----------------------------------------------------------------*

       FD  MOVTOSEL
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG62

           EJECT
      *----------------------------------------------------------------*
      *   OUTPUT:  RELATORIO DE ANALISE.                               *
      *            ORG. SEQUENCIAL     -   LRECL   =   150             *
      *----------------------------------------------------------------*

       FD  GW333333
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-GW333333                PIC  X(150).


      *----------------------------------------------------------------*
      *    OUTPUT: RELATORIO DE CONTROLE.                              *
      *            ORG. SEQUENCIAL     -   LRECL   =   081             *
      *----------------------------------------------------------------*

       FD  SRELCTRL
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-SRELCTRL                PIC  X(081).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO COM MOTIVOS DE DEVOLUCAO DE CHEQUE          *
      *            ORG. SEQUENCIAL     -    LRECL   =   010            *
      *----------------------------------------------------------------*

       FD  MOTDEVOL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIGC1

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)          VALUE
           '*** SLIG7110 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LINHAS                PIC  9(002)           VALUE 99.
       77  ACU-PAG                   PIC  9(005)    COMP-3 VALUE ZEROS.
       77  ACU-LIDOS-ARQDATAS        PIC  9(013)    COMP-3 VALUE ZEROS.
       77  ACU-LIDOS                 PIC  9(013)    COMP-3 VALUE ZEROS.
       77  ACU-IMPRESSOS             PIC  9(013)    COMP-3 VALUE ZEROS.
       77  ACU-INDICE-A              PIC  9(015)V99 COMP-3 VALUE ZEROS.
       77  ACU-INDICE-B              PIC  9(015)V99 COMP-3 VALUE ZEROS.
       77  ACU-INDICE-C              PIC  9(015)V99 COMP-3 VALUE ZEROS.
       77  ACU-INDICE-D              PIC  9(015)V99 COMP-3 VALUE ZEROS.
       77  ACU-INDICE-E              PIC  9(015)V99 COMP-3 VALUE ZEROS.
       77  ACU-INDICE-F              PIC  9(015)V99 COMP-3 VALUE ZEROS.
       77  ACU-INDICE-G              PIC  9(015)V99 COMP-3 VALUE ZEROS.
       77  ACU-INDICE-H              PIC  9(015)V99 COMP-3 VALUE ZEROS.
       77  ACU-INDICE-I              PIC  9(015)V99 COMP-3 VALUE ZEROS.
       77  ACU-INDICE-J              PIC  9(015)V99 COMP-3 VALUE ZEROS.
       77  ACU-INDICE-K              PIC  9(015)V99 COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTES DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-ARQDATAS             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-MOVTOSEL             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-MOTDEVOL             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-GW333333             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SRELCTRL             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

           EJECT
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       77  WRK-MSG01                   PIC  X(075)         VALUE
           '*** ARQUIVO MOVTOSEL VAZIO ***'.
       77  WRK-MSG004                  PIC  X(075)         VALUE
           '*** ARQUIVO ARQDATAS VAZIO ***'.
       77  WRK-MSG02                   PIC  X(075)         VALUE
           '*** SLIG62-PRODUTO-CTRL DIFERENTE DE ''A'' E ''B'' E ''C'' *
      -    '**'.
       77  WRK-MSG03                   PIC  X(075)         VALUE
           '*** VALOR DA MOEDA NAO ENCONTRADO PARA A DATA FORNECIDA ***'
      -    '''.
       77  WRK-MSG04                   PIC  X(075)         VALUE
           '*** VALOR DE MOEDA NAO CADASTRADO ***'.
       77  WRK-MSG05                   PIC  X(075)         VALUE
           '*** DADOS INVALIDOS PARA ACESSO AO MODULO CBPG8010 ***'.
       77  WRK-MSG06                   PIC  X(075)         VALUE
           '*** DADOS INVALIDOS PARA ACESSO AO MODULO CBPG8420 ***'.
       77  WRK-MSG07                   PIC  X(075)         VALUE
           '*** DADOS INVALIDOS PARA ACESSO AO MODULO CBPG8470 ***'.
       77  WRK-MSG08                   PIC  X(075)         VALUE
           '*** ERRO NO ACESSO A POOL0025 ***'.
       77  WRK-MSG09                   PIC  X(075)         VALUE
           '*** ERRO DE SEQUENCIA CONTROLE ARQUIVO ***'.
       77  WRK-MSG-ERRO-REAP           PIC  X(075)         VALUE
           '*** REAPRESENTACAO SEM DEVOLUCAO       **'.
       77  WRK-MSG-ERRO-DEVL           PIC  X(075)         VALUE
           '*** CODIGO DE DEVOLUCAO NAO ENCONTRADO **'.

       01  WRK-MENSA10.
           03  FILLER                  PIC  X(043)         VALUE
               '** ERRO NO ACESSO A POOL1640 RETURN-CODE = '.
           03  WRK-RET-COD-1640        PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(030)         VALUE ' **'.

       01  WRK-MENSA11.
           03  FILLER                  PIC  X(053)         VALUE
               '** ERRO NA LEITURA DO ARQUIVO CADACLIE - RETURN-CODE '.
           03  WRK-RET-COD-MSG11       PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(020)         VALUE ' **'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-RETURN-CODE             PIC  9(002)         VALUE ZEROS.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-TIT-CD-ORIGEM           PIC  9(003)         VALUE ZEROS.
       77  WRK-TP-PESS-SAC             PIC  X(001)         VALUE SPACES.
       77  WRK-CGC-CPF-CTRL            PIC  X(009)         VALUE SPACES.
       77  WRK-PRODUTO-CTRL            PIC  X(001)         VALUE SPACES.
       77  WRK-CARTEIRA-CTRL           PIC  9(001)         VALUE ZEROS.
       77  WRK-NOSNRO                  PIC  9(011)         VALUE ZEROS.
       77  WRK-DEV-ANT-DESCONS         PIC  9(009) COMP-3  VALUE ZEROS.
       77  WRK-DEV-PER-DESCONS         PIC  9(009) COMP-3  VALUE ZEROS.
       77  WRK-DEV-FUT-DESCONS         PIC  9(009) COMP-3  VALUE ZEROS.
       77  WRK-IND                     PIC  9(003)         VALUE ZEROS.
       77  WRK-ACHOU                   PIC  X(001)         VALUE SPACES.
       77  WRK-DT-CONTABIL-ULT         PIC  9(008)         VALUE ZEROS.
       77  WRK-DT-SITUACAO-ULT         PIC  9(008)         VALUE ZEROS.
       77  WRK-RESP-ULTIMA-DEVOLUCAO   PIC  X(001)         VALUE SPACES.
       77  WRK-REAP-ULTIMA-DEVOLUCAO   PIC  X(001)         VALUE SPACES.
       77  WRK-ULTIMA-DEVOLUCAO        PIC  9(008)         VALUE ZEROS.

       77  WRK-DT-INI-PER-ULT          PIC  9(008)         VALUE ZEROS.
       77  WRK-DT-FIM-PER-ULT          PIC  9(008)         VALUE ZEROS.

       77  WRK-DT-INI-PER-PRX          PIC  9(008)         VALUE ZEROS.
       77  WRK-DT-FIM-PER-PRX          PIC  9(008)         VALUE ZEROS.

       77  WRK-VL-PRINC-PROD-CONV      PIC  9(015)V99 COMP-3
                                                           VALUE ZEROS.
       77  WRK-OSCILACAO               PIC S9(015)V99 COMP-3
                                                           VALUE ZEROS.

       01  WRK-VL-PG-PROD              PIC  9(015)         COMP-3.
       01  WRK-VL-PG-PROD-R            REDEFINES
           WRK-VL-PG-PROD              PIC  9(013)V99      COMP-3.

       01  WRK-VALOR-ACUM              PIC  9(015)V9(02)   VALUE ZEROS.

       01  WRK-VALOR                   PIC  9(017)         VALUE ZEROS.
       01  WRK-VALOR-R                 REDEFINES
           WRK-VALOR                   PIC  9(015)V99.

       01  WRK-VALOR-PG                PIC  9(015)         VALUE ZEROS.
       01  WRK-VALOR-PG-R              REDEFINES
           WRK-VALOR-PG                PIC  9(013)V99.

       01  WRK-VALOR-ABAT              PIC  9(015)         VALUE ZEROS.
       01  WRK-VALOR-ABAT-R            REDEFINES
           WRK-VALOR-ABAT              PIC  9(013)V99.

       01  WRK-CAMPO-OBS               PIC  X(011)         VALUE SPACES.
       01  WRK-INDICES                 REDEFINES          WRK-CAMPO-OBS.
           03  WRK-INDICE-A            PIC  X(001).
           03  WRK-INDICE-B            PIC  X(001).
           03  WRK-INDICE-C            PIC  X(001).
           03  WRK-INDICE-D            PIC  X(001).
           03  WRK-INDICE-E            PIC  X(001).
           03  WRK-INDICE-F            PIC  X(001).
           03  WRK-INDICE-G            PIC  X(001).
           03  WRK-INDICE-H            PIC  X(001).
           03  WRK-INDICE-I            PIC  X(001).
           03  WRK-INDICE-J            PIC  X(001).
           03  WRK-INDICE-K            PIC  X(001).

       01  WRK-OBS-AUX                 PIC  X(011)         VALUE SPACES.
       01  WRK-INDICES-AUX             REDEFINES            WRK-OBS-AUX.
           03  WRK-IND-A-AUX           PIC  X(001).
           03  WRK-IND-B-AUX           PIC  X(001).
           03  WRK-IND-C-AUX           PIC  X(001).
           03  WRK-IND-D-AUX           PIC  X(001).
           03  WRK-IND-E-AUX           PIC  X(001).
           03  WRK-IND-F-AUX           PIC  X(001).
           03  WRK-IND-G-AUX           PIC  X(001).
           03  WRK-IND-H-AUX           PIC  X(001).
           03  WRK-IND-I-AUX           PIC  X(001).
           03  WRK-IND-J-AUX           PIC  X(001).
           03  WRK-IND-K-AUX           PIC  X(001).

       01  WRK-NM-CART.
           03  WRK-NOME-CARTEIRA.
               05  FILLER              PIC  X(010)         VALUE
                   'SIMPLES   '.
               05  FILLER              PIC  X(010)         VALUE
                   'CAUCIONADA'.
               05  FILLER              PIC  X(010)         VALUE
                   'DESCONTADA'.
           03  WRK-NOME-CARTEIRA-R     REDEFINES
               WRK-NOME-CARTEIRA       OCCURS 3 TIMES.
               05  WRK-CARTEIRA        PIC  X(010).

       01  WRK-TABELA-DEVOLUCAO.
           03  WRK-TABELA OCCURS  99 TIMES.
               05  WRK-CTPO-DEVLC      PIC  9(003).
               05  WRK-RESP-DEVLC      PIC  X(001).
               05  WRK-POBLD-REAP      PIC  X(001).


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE CONTROLE SEQUENCIA ARQUIVO ***'.
      *----------------------------------------------------------------*

       01  WRK-CONTROLE-ATU.
           03  WRK-CTRL-SEQ-ATU        PIC X(051)   VALUE ALL '0'.

       01  WRK-CONTROLE-ANT.
           03  WRK-CTRL-SEQ-ANT        PIC X(051)   VALUE ALL '0'.

       01  WRK-CONTROLE-CHEQUE-ATU.
           03  WRK-CTRL-CGC-CPF-CHQ            PIC X(009)  VALUE SPACES.
           03  WRK-CTRL-PRODUTO-CHQ            PIC X(001)  VALUE SPACE.
           03  WRK-CTRL-CARTEIRA-CHQ           PIC 9(001)  VALUE ZEROS.
           03  WRK-CTRL-CTRL-CHQ.
               05 WRK-CTRL-BANCO-EMITT-CHQ     PIC 9(003)  VALUE ZEROS.
               05 WRK-CTRL-AGENCIA-EMITT-CHQ   PIC 9(004)  VALUE ZEROS.
               05 WRK-CTRL-CONTA-EMITT-CHQ     PIC 9(013)  VALUE ZEROS.
               05 WRK-CTRL-NUM-CHEQUE-CHQ      PIC 9(007)  VALUE ZEROS.
           03  WRK-CTRL-DT-CTBL-CHQ            PIC 9(008)  VALUE ZEROS.

       01  WRK-CONTROLE-CHEQUE-ANT             PIC X(046) VALUE ALL '0'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE AUXILIARES PARA DATAS ***'.
      *----------------------------------------------------------------*

       01  WRK-DT-ENTRADA              PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-PGTO                 PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-ALT                  PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-CTBIL                PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-SITU                 PIC  9(008)         VALUE ZEROS.

       01  WRK-DT-AUX                  PIC  9(009)         VALUE ZEROS.
       01  WRK-DT-AUX-R                REDEFINES           WRK-DT-AUX.
           03  FILLER                  PIC  9(001).
           03  WRK-AA-AUX              PIC  9(004).
           03  WRK-MM-AUX              PIC  9(002).
           03  WRK-DD-AUX              PIC  9(002).

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

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7600 ***'.
      *---------------------------------------------------------------*

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

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA SRELCTRL ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA P/ VERIFICACAO STATUS CHEQUES ***'.
      *----------------------------------------------------------------*

       01  ACU-ERRO-CHEQUE             PIC  9(009) COMP-3  VALUE ZEROS.
       01  WRK-ULT-CTPO-CHEQ           PIC  9(003) COMP-3  VALUE ZEROS.
       01  WRK-ERRO-STATUS             PIC  X(001)         VALUE SPACES.
       01  WRK-LIT-STATUS              PIC  X(018)         VALUE SPACES.
       01  WRK-LIT-MOTDEV              PIC  X(002)         VALUE SPACES.

       01  WRK-DT-VENCIMENTO           PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-DEPOSITO-ERRO        PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-CONTABIL-ERRO        PIC  9(008)         VALUE ZEROS.

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

       01  WRK-CHV-EMITENTE-ATU.
           03  WRK-BANCO-ATU           PIC  9(003)         VALUE ZEROS.
           03  WRK-AGENCIA-ATU         PIC  9(005)         VALUE ZEROS.
           03  WRK-CONTA-ATU           PIC  9(013)         VALUE ZEROS.
           03  WRK-CHEQUE-ATU          PIC  9(007)         VALUE ZEROS.

       01  WRK-CHV-EMITENTE-ANT.
           03  WRK-BANCO-ANT           PIC  9(003)         VALUE ZEROS.
           03  WRK-AGENCIA-ANT         PIC  9(005)         VALUE ZEROS.
           03  WRK-CONTA-ANT           PIC  9(013)         VALUE ZEROS.
           03  WRK-CHEQUE-ANT          PIC  9(007)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA SRELCTRL ***'.
      *----------------------------------------------------------------*

       01  CABEC1-CTRL.
           03  FILLER                  PIC  X(001)         VALUE '1'.
           03  FILLER                  PIC  X(019)         VALUE
               'BRADESCO'.
           03  FILLER                  PIC  X(051)         VALUE
               'SISTEMA DE INDICE DE LIQUIDEZ'.
           03  FILLER                  PIC  X(010)         VALUE
               'PAG. UNICA'.

       01  CABEC2-CTRL.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(070)         VALUE
               'SLIG7110'.
           03  CB2-DD                  PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-MM                  PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-AA                  PIC  9999.

       01  CABEC3-CTRL.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  FILLER                  PIC  X(023)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'CONTROLE OPERACIONAL'.

       01  LINTOT1-CTRL.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  FILLER                  PIC  X(053)         VALUE
               'TOTAL DE REGISTROS LIDOS       (MOVTOSEL).........:'.
           03  LT1-LIDOS               PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT2-CTRL.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  FILLER                  PIC  X(053)         VALUE
               'TOTAL DE REGISTROS GRAVADOS    (GW333333).........:'.
           03  LT2-IMPRESSOS           PIC  ZZZ.ZZZ.ZZZ.ZZ9.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE CABECALHOS DO GW333333 ***'.
      *----------------------------------------------------------------*

       01  CABEC1.
           03  FILLER                  PIC  X(001)         VALUE '1'.
           03  FILLER                  PIC  X(013)         VALUE
               'SLIG7110'.
           03  CB1-DD                  PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB1-MM                  PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB1-AA                  PIC  9999.
           03  FILLER                  PIC  X(029)         VALUE SPACES.
           03  FILLER                  PIC  X(011)         VALUE
               'B A N C O'.
           03  FILLER                  PIC  X(017)         VALUE
               'B R A D E S C O'.
           03  FILLER                  PIC  X(058)         VALUE
               'S / A'.
           03  FILLER                  PIC  X(006)         VALUE
               'PAG.: '.
           03  CB1-PAG                 PIC  Z.ZZ9.

       01  CABEC2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(039)         VALUE SPACES.
           03  FILLER                  PIC  X(027)         VALUE
               'ANALISE DA PERFORMANCE DO'.
           03  FILLER                  PIC  X(028)         VALUE
               'SISTEMA - INDICE DE LIQUIDEZ'.

       01  CABEC3.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE
               'CNPJ/CPF: '.
           03  CB3-CNPJ                PIC  X(011)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  CB3-DESC-TP             PIC  X(012)         VALUE SPACES.
           03  CB3-NOME                PIC  X(040)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' PERIODO DE '.
           03  CB3-DD-INI              PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB3-MM-INI              PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB3-AA-INI              PIC  9999.
           03  FILLER                  PIC  X(003)         VALUE ' A '.
           03  CB3-DD-FIM              PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB3-MM-FIM              PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB3-AA-FIM              PIC  9999.
           03  FILLER                  PIC  X(011)         VALUE
               '   PRODUTO:'.
           03  CB3-PRODUTO             PIC  X(007)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               '   CARTEIRA:'.
           03  CB3-CARTEIRA            PIC  X(010)         VALUE SPACES.

       01  CABEC4.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(149)         VALUE ALL
               '-'.

       01  CABEC5.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(013)         VALUE
               'PROD AGEN'.
           03  FILLER                  PIC  X(009)         VALUE
               'CONTA'.
           03  FILLER                  PIC  X(015)         VALUE
               'NR.TITULO'.
           03  FILLER                  PIC  X(013)         VALUE
               'CNPJ CLI.'.
           03  FILLER                  PIC  X(030)         VALUE
               'DAT.ENTR. DAT.VENC. DAT.ALT.'.
           03  FILLER                  PIC  X(012)         VALUE
               'DAT.PAGTO'.
           03  FILLER                  PIC  X(049)         VALUE
               'VALOR TITULO VALOR DES/ABAT. MODO LIQUIDACAO'.
           03  FILLER                  PIC  X(003)         VALUE 'OBS'.

       01  CABEC6.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(016)         VALUE
               'PRD F CONTRATO'.
           03  FILLER                  PIC  X(010)         VALUE
               'SEQ PCLA'.
           03  FILLER                  PIC  X(011)         VALUE
               'NR.TITULO'.
           03  FILLER                  PIC  X(013)         VALUE
               'CNPJ FOR.'.
           03  FILLER                  PIC  X(030)         VALUE
               'DAT.ENTR. DAT.VENC. DAT.ALT.'.
           03  FILLER                  PIC  X(012)         VALUE
               'DAT.PAGTO'.
           03  FILLER                  PIC  X(049)         VALUE
               'VALOR TITULO VALOR DES/ABAT. MODO LIQUIDACAO'.
           03  FILLER                  PIC  X(003)         VALUE 'OBS'.

       01  CABEC7.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(005)         VALUE 'BCO'.
           03  FILLER                  PIC  X(011)         VALUE 'AGEN'.
           03  FILLER                  PIC  X(009)         VALUE
               'CONTA'.
           03  FILLER                  PIC  X(012)         VALUE
               'CHEQUE'.
           03  FILLER                  PIC  X(012)         VALUE
               'CNPJ CLI.'.
           03  FILLER                  PIC  X(011)         VALUE
               'DAT.DEPOS'.
           03  FILLER                  PIC  X(030)         VALUE
               'DAT.EVNT. DAT.SITU  BCO  AGEN'.
           03  FILLER                  PIC  X(017)         VALUE
               'CONTA CLIENTE '.
           03  FILLER                  PIC  X(014)         VALUE
               'VALOR CHEQUE'.
           03  FILLER                  PIC  X(019)         VALUE
               'MODO LIQUIDACAO'.
           03  FILLER                  PIC  X(003)         VALUE 'OBS'.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DETALHES DO GW333333 ***'.
      *----------------------------------------------------------------*

       01  LINDET1.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD1-PROD                PIC  99.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD1-AGEN                PIC  9999.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD1-CONTA               PIC  9999999.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD1-TITULO              PIC  99999999999.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD1-CNPJ-CLI            PIC  X(009)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD1-TIPO-CLI            PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD1-DT-ENT              PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD1-DT-VENC             PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD1-DT-ALT              PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD1-DT-PGTO             PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD1-VLR-TIT             PIC  ZZZZZZZZ.ZZ9,99.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD1-VLR-ABAT.
               05  LD1-VLR-ABAT-N      PIC  ZZZZZZZZ.ZZ9,99.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD1-LIQUID              PIC  X(018)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD1-OBS                 PIC  X(009)         VALUE SPACES.

       01  LINDET2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD2-PROD                PIC  999.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD2-FAML                PIC  9.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD2-CONTRATO            PIC  999999999.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD2-SEQ                 PIC  999.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD2-PCLA                PIC  999.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD2-TITULO              PIC  99999999999.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD2-CNPJ-CLI            PIC  X(009)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD2-TIPO-CLI            PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD2-DT-ENT              PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD2-DT-VENC             PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD2-DT-ALT              PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD2-DT-PGTO             PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD2-VLR-TIT             PIC  ZZZZZZZZ.ZZ9,99.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD2-VLR-ABAT.
               05  LD2-VLR-ABAT-R      PIC  ZZZZZZZZ.ZZ9,99.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD2-LIQUID              PIC  X(018)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD2-OBS                 PIC  X(009)         VALUE SPACES.

       01  LINDET3.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD3-BCO                 PIC  999.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD3-AGEN                PIC  9999.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD3-CONTA               PIC  9999999999999.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD3-CHEQUE              PIC  9999999.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD3-CNPJ-CLIE           PIC  X(009)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD3-TIPO-CLIE           PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD3-DT-DEP              PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD3-DT-CTBL             PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD3-DT-SITU             PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD3-BCO-CLI             PIC  ZZZ.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD3-AGEN-CLI            PIC  ZZZZZ.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD3-CONTA-CLI           PIC  ZZZZZZZZZZZZZ.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD3-VLR-CHQ             PIC  ZZZZZZZZ.ZZ9,99.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD3-LIQUID              PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD3-MOTDEV              PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD3-OBS                 PIC  X(011)         VALUE SPACES.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE TOTAIS DO GW333333 ***'.
      *----------------------------------------------------------------*

       01  LINTOT-BCO.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(149)         VALUE SPACES.

       01  LINTOT1.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(061)         VALUE SPACES.
           03  LT1-LITERAL             PIC  X(036)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LT1-VALOR               PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
           03  FILLER                  PIC  X(005)         VALUE
               '   ( '.
           03  LT1-TIPO                PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE ' )'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO A POOL0025 ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-0025.
           03  WRK-DT-ENTRADA-0025     PIC  9(008)         VALUE ZEROS.
           03  WRK-NUM-DIAS-0025       PIC  9(004)         VALUE ZEROS.
           03  WRK-DT-SAIDA-0025       PIC  9(008)         VALUE ZEROS.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(047)         VALUE
           '*** INCIO DA AREA DE CONTROLE DO CADACLIE   ***'.
      *----------------------------------------------------------------*

       01  WRK-CHV-TITULOS.
           03  WRK-CPFCGC-TITU         PIC  X(009)    VALUE SPACES.
           03  WRK-FILIAL-TITU         PIC  X(005)    VALUE SPACES.
           03  WRK-CONTROLE-TITU       PIC  9(002)    VALUE ZEROS.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO A POOL1640 ***'.
      *----------------------------------------------------------------*

       01  WRK-1640-AREA.
           03 WRK-1640-DATA-ENTRADA.
              05 FILLER                PIC  X(002)         VALUE SPACES.
              05 WRK-1640-DATA-ENTRADA-N
                                       PIC  9(008)         VALUE ZEROS.
           03 WRK-1640-FORMATO-ENTRADA PIC  9(002)         VALUE ZEROS.
           03 WRK-1640-TIPO-INCREMENTO PIC  X(002)         VALUE SPACES.
           03 WRK-1640-INCREMENTO      PIC  9(003)         VALUE ZEROS.
           03 WRK-1640-FORMATO-SAIDA   PIC  9(002)         VALUE ZEROS.
           03 WRK-1640-DATA-SAIDA      PIC  X(010)         VALUE SPACES.
           03 WRK-1640-MENSAGEM        PIC  X(080)         VALUE SPACES.

       01  WRK-DT-CARENCIA-RG.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  WRK-DT-CARENCIA         PIC  9(008)         VALUE ZEROS.

       01  WRK-DT-CARENCIA-R           REDEFINES
           WRK-DT-CARENCIA-RG          PIC  X(010).

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7100 ***'.
      *----------------------------------------------------------------*

-INC POL7100C

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EJECT
           EXEC SQL
             INCLUDE CBGEB033
           END-EXEC.

           EXEC SQL
               INCLUDE SLIGB030
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** SLIG7110 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
           EJECT
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-VERIFICAR-VAZIO.

           PERFORM 200001-CARREGA-TAB-DEVOLUCAO
                   VARYING WRK-IND     FROM 1 BY 1
                      UNTIL WRK-IND   GREATER 99 OR
                   WRK-FS-MOTDEVOL     EQUAL '10'.

           PERFORM 300000-PROCESSAR    UNTIL
                   WRK-FS-MOVTOSEL     EQUAL '10'.

           PERFORM 400000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN  INPUT ARQDATAS
                       MOVTOSEL
                       MOTDEVOL
                OUTPUT GW333333
                       SRELCTRL.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.
           PERFORM 120000-OBTER-DATA-HORA.

           MOVE WRK-CONTROLE-CHEQUE-ATU TO WRK-CONTROLE-CHEQUE-ANT.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 111000-TESTAR-FS-MOVTOSEL.
           PERFORM 112000-TESTAR-FS-GW333333.
           PERFORM 113000-TESTAR-FS-SRELCTRL.
           PERFORM 115000-TESTAR-FS-MOTDEVOL.
           PERFORM 116000-TESTAR-FS-ARQDATAS.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       111000-TESTAR-FS-MOVTOSEL       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-MOVTOSEL         NOT EQUAL '00'
               MOVE WRK-FS-MOVTOSEL    TO WRK-FILE-STATUS
               MOVE 'MOVTOSEL'         TO WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       112000-TESTAR-FS-GW333333       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GW333333         NOT EQUAL '00'
               MOVE WRK-FS-GW333333    TO WRK-FILE-STATUS
               MOVE 'GW333333'         TO WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       113000-TESTAR-FS-SRELCTRL       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SRELCTRL         NOT EQUAL '00'
               MOVE WRK-FS-SRELCTRL    TO WRK-FILE-STATUS
               MOVE 'SRELCTRL'         TO WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       113000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       115000-TESTAR-FS-MOTDEVOL       SECTION.
      *----------------------------------------------------------------*
           IF  WRK-FS-MOTDEVOL         NOT EQUAL '00'
               MOVE WRK-FS-MOTDEVOL    TO WRK-FILE-STATUS
               MOVE 'MOTDEVOL'         TO WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.
      *----------------------------------------------------------------*
       115000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       116000-TESTAR-FS-ARQDATAS       SECTION.
      *----------------------------------------------------------------*
           IF  WRK-FS-ARQDATAS         NOT EQUAL '00'
               MOVE WRK-FS-ARQDATAS    TO WRK-FILE-STATUS
               MOVE 'ARQDATAS'         TO WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.
      *----------------------------------------------------------------*
       116000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       120000-OBTER-DATA-HORA          SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           MOVE  WRK-P7600-DIA         TO    CB1-DD CB2-DD.
           MOVE  WRK-P7600-MES         TO    CB1-MM CB2-MM.
           MOVE  WRK-P7600-ANO         TO    CB1-AA CB2-AA.

      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-LER-MOVTOSEL.

           IF  WRK-FS-MOVTOSEL         EQUAL '10'
               DISPLAY '***************** SLIG7110 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*         ARQUIVO MOVTOSEL VAZIO           *'
               DISPLAY '*                                          *'
               DISPLAY '*        PROCESSAMENTO  CANCELADO          *'
               DISPLAY '*                                          *'
               DISPLAY '***************** SLIG7110 *****************'
               MOVE WRK-MSG01          TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 220000-LER-ARQDATAS UNTIL
                   WRK-FS-ARQDATAS     EQUAL '10' OR
                   SLIG00-CD-TP-PROCESSA EQUAL 2.

           IF  WRK-FS-ARQDATAS         EQUAL '10'
               DISPLAY '****************** SLIG7110 ******************'
               DISPLAY '*                                            *'
               DISPLAY '*     CAMPOS PESQUISADOS NAO ENCONTRADOS     *'
               DISPLAY '*                                            *'
               DISPLAY '*            NO ARQUIVO DE DATAS             *'
               DISPLAY '*                                            *'
               DISPLAY '****************** SLIG7110 ******************'
               MOVE WRK-MSG004         TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF SLIG00-CD-TP-PROCESSA EQUAL 2
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
       200001-CARREGA-TAB-DEVOLUCAO    SECTION.
      *----------------------------------------------------------------*
           READ MOTDEVOL.

           IF  WRK-FS-MOTDEVOL         EQUAL '10'
               GO   TO  200001-99-FIM.

           MOVE SLIGC1-CTPO-DEVLC   TO  WRK-CTPO-DEVLC(WRK-IND).
           MOVE SLIGC1-RESP-DEVLC   TO  WRK-RESP-DEVLC(WRK-IND).
           MOVE SLIGC1-POBLD-REAP   TO  WRK-POBLD-REAP(WRK-IND).

      *----------------------------------------------------------------*
       200001-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
       210000-LER-MOVTOSEL             SECTION.
      *----------------------------------------------------------------*

           IF SLIG62-PRODUTO-CTRL   EQUAL 'C'
              MOVE WRK-CONTROLE-CHEQUE-ATU  TO WRK-CONTROLE-CHEQUE-ANT
           ELSE
              MOVE WRK-CONTROLE-ATU       TO WRK-CONTROLE-ANT
           END-IF.

           READ MOVTOSEL.

           IF  WRK-FS-MOVTOSEL         EQUAL '10'
               GO TO 210000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 111000-TESTAR-FS-MOVTOSEL.

           IF  SLIG62-PRODUTO-CTRL
           NOT EQUAL 'A' AND 'B' AND 'C'
               PERFORM 211000-EMITIR-DISP-ERRO
               MOVE WRK-MSG02          TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SLIG62-PRODUTO-CTRL  NOT EQUAL 'C'
               MOVE SLIG62-CTRL  TO WRK-CONTROLE-ATU

               IF WRK-CONTROLE-ATU       LESS WRK-CONTROLE-ANT
                  PERFORM 212000-EMITIR-DISP-ERRO
                  MOVE WRK-MSG09          TO ERR-TEXTO
                  MOVE 'APL'              TO ERR-TIPO-ACESSO
                  PERFORM 999999-PROCESSAR-ROTINA-ERRO
               END-IF
           ELSE

      * *  CHEQUE  -  MONTA CHAVE DE CONTROLE DE SEQUENCIA
               MOVE SLIG62-CGC-CPF-CTRL-ST   TO  WRK-CTRL-CGC-CPF-CHQ
               MOVE SLIG62-PRODUTO-CTRL      TO  WRK-CTRL-PRODUTO-CHQ
               MOVE SLIG62-CARTEIRA-CTRL     TO  WRK-CTRL-CARTEIRA-CHQ
               MOVE SLIG62-BANCO-EMITT       TO
                                              WRK-CTRL-BANCO-EMITT-CHQ
               MOVE SLIG62-AGENCIA-EMITT-CTRL TO
                                             WRK-CTRL-AGENCIA-EMITT-CHQ
               MOVE SLIG62-CONTA-EMITT-CTRL   TO
                                              WRK-CTRL-CONTA-EMITT-CHQ
               MOVE SLIG62-NUM-CHEQUE-CTRL   TO
                                              WRK-CTRL-NUM-CHEQUE-CHQ
               MOVE SLIG62-CHQ-DT-CONTABIL   TO WRK-CTRL-DT-CTBL-CHQ


               MOVE SLIG62-CHQ-BCO-EMITT     TO WRK-BANCO-ATU
               MOVE SLIG62-CHQ-AGENCIA-EMITT TO WRK-AGENCIA-ATU
               MOVE SLIG62-CHQ-CONTA-EMITT   TO WRK-CONTA-ATU
               MOVE SLIG62-CHQ-NUM-CHEQUE    TO WRK-CHEQUE-ATU

               IF WRK-CONTROLE-CHEQUE-ATU LESS  WRK-CONTROLE-CHEQUE-ANT
                  PERFORM 212000-EMITIR-DISP-ERRO
                  MOVE WRK-MSG09          TO ERR-TEXTO
                  MOVE 'APL'              TO ERR-TIPO-ACESSO
                  PERFORM 999999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

           ADD 1                       TO ACU-LIDOS.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       211000-EMITIR-DISP-ERRO         SECTION.
      *----------------------------------------------------------------*

           DISPLAY '***************** SLIG7110 *****************'.
           DISPLAY '*                                          *'.
           DISPLAY '*      SLIG62-PRODUTO-CTRL  DIFERENTE      *'.
           DISPLAY '*            DE ''A'' E ''B'' E ''C''            *'.
           DISPLAY '*                                          *'.
           DISPLAY '*         PROCESSAMENTO  CANCELADO         *'.
           DISPLAY '*                                          *'.
           DISPLAY '***************** SLIG7110 *****************'.

      *----------------------------------------------------------------*
       211000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       212000-EMITIR-DISP-ERRO         SECTION.
      *----------------------------------------------------------------*

           DISPLAY '***************** SLIG7110 *****************'.
           DISPLAY '*                                          *'.
           DISPLAY '*    ARQUIVO MOVTOSEL FORA DE SEQUENCIA    *'.

           IF  SLIG62-PRODUTO-CTRL   EQUAL 'C'
               DISPLAY '*    CTRL-CHQ-ATUAL = ' WRK-CONTROLE-CHEQUE-ATU
               DISPLAY '*    CTRL-CHQ-ANTER = ' WRK-CONTROLE-CHEQUE-ANT
           ELSE
              DISPLAY '*    CONTROLE-ATUAL = ' WRK-CONTROLE-ATU
              DISPLAY '*    CONTROLE-ANTER = ' WRK-CONTROLE-ANT
           END-IF.

           DISPLAY '*                                          *'.
           DISPLAY '*         PROCESSAMENTO  CANCELADO         *'.
           DISPLAY '*                                          *'.
           DISPLAY '***************** SLIG7110 *****************'.

      *----------------------------------------------------------------*
       212000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       220000-LER-ARQDATAS             SECTION.
      *----------------------------------------------------------------*

           READ ARQDATAS.

           IF  WRK-FS-ARQDATAS         EQUAL '10'
               GO TO 220000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 116000-TESTAR-FS-ARQDATAS.

           ADD 1                       TO ACU-LIDOS-ARQDATAS.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SLIG62-CGC-CPF-CTRL-ST TO WRK-CGC-CPF-CTRL.
           MOVE SLIG62-PRODUTO-CTRL    TO WRK-PRODUTO-CTRL.
           MOVE SLIG62-CARTEIRA-CTRL  TO WRK-CARTEIRA-CTRL.

           PERFORM 310000-TRATAR-SACADO
             UNTIL WRK-FS-MOVTOSEL      EQUAL '10'                  OR
                   SLIG62-CGC-CPF-CTRL-ST
                   NOT EQUAL WRK-CGC-CPF-CTRL  OR
                   SLIG62-PRODUTO-CTRL
                   NOT EQUAL WRK-PRODUTO-CTRL  OR
                   SLIG62-CARTEIRA-CTRL
                   NOT EQUAL WRK-CARTEIRA-CTRL

           PERFORM 320000-IMPRIMIR-TOT-CART.
           PERFORM 330000-ZERAR-ACUMULADORES.

           MOVE 99                      TO ACU-LINHAS.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       310000-TRATAR-SACADO            SECTION.
      *----------------------------------------------------------------*

           EVALUATE SLIG62-PRODUTO-CTRL
               WHEN 'A'
                    PERFORM 311000-TRATAR-TITULOS
               WHEN 'B'
                   PERFORM 312000-TRATAR-VENDOR
               WHEN 'C'
                   PERFORM 313000-TRATAR-CHEQUES
           END-EVALUATE.

      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       311000-TRATAR-TITULOS           SECTION.
      *----------------------------------------------------------------*

           MOVE  SLIG62-TIT-CGC-CPF-SACADO-ST  TO  WRK-CPFCGC-TITU.
           MOVE  SLIG62-TIT-FILIAL-SACADO-ST   TO  WRK-FILIAL-TITU.
           MOVE  SLIG62-TIT-CONTROLE-SACADO-ST TO  WRK-CONTROLE-TITU.

           PERFORM 311100-ACESSAR-SLIGB030.

           MOVE SLIG62-CGC-CPF-CTRL-ST(1:3) TO CB3-CNPJ(1:3).
           MOVE SLIG62-CGC-CPF-CTRL-ST(4:3) TO CB3-CNPJ(5:3).
           MOVE SLIG62-CGC-CPF-CTRL-ST(7:3) TO CB3-CNPJ(9:3).
           MOVE '.'                         TO CB3-CNPJ(4:1).
           MOVE '.'                         TO CB3-CNPJ(8:1).

           MOVE SLIG62-DT-PER-INI         TO WRK-DT-AUX.
           MOVE WRK-DD-AUX                TO CB3-DD-INI.
           MOVE WRK-MM-AUX                TO CB3-MM-INI.
           MOVE WRK-AA-AUX                TO CB3-AA-INI.

           MOVE SLIG62-DT-PER-FIM         TO WRK-DT-AUX.
           MOVE WRK-DD-AUX                TO CB3-DD-FIM.
           MOVE WRK-MM-AUX                TO CB3-MM-FIM.
           MOVE WRK-AA-AUX                TO CB3-AA-FIM.

           MOVE 'TITULOS'                 TO CB3-PRODUTO.
           MOVE 'SACADO....: '            TO CB3-DESC-TP.
           MOVE WRK-CARTEIRA(SLIG62-CARTEIRA-CTRL)
                                          TO CB3-CARTEIRA.

           MOVE SLIG62-CD-PRODUTO-CLI     TO LD1-PROD.
           MOVE SLIG62-AGENCIA-CLI        TO LD1-AGEN.
           MOVE SLIG62-CONTA-CLI          TO LD1-CONTA.
           MOVE SLIG62-NUM-TIT            TO LD1-TITULO.
           MOVE SLIG62-TIT-CGC-CPF-CLIENTE-ST
                                          TO LD1-CNPJ-CLI.

           IF  SLIG62-TIT-FILIAL-CLIENTE-ST
                                          NOT EQUAL '00000'
               MOVE 'J'                   TO LD1-TIPO-CLI
           ELSE
               MOVE 'F'                   TO LD1-TIPO-CLI
           END-IF.

           MOVE SLIG62-TIT-DT-ENTRADA-SISTEMA
                                          TO WRK-DT-AUX.
           MOVE WRK-DD-AUX                TO LD1-DT-ENT(1:2).
           MOVE WRK-MM-AUX                TO LD1-DT-ENT(3:2).
           MOVE WRK-AA-AUX                TO LD1-DT-ENT(5:4).

           MOVE SLIG62-TIT-DT-VENCTO-PROD TO WRK-DT-AUX.
           MOVE WRK-DD-AUX                TO LD1-DT-VENC(1:2).
           MOVE WRK-MM-AUX                TO LD1-DT-VENC(3:2).
           MOVE WRK-AA-AUX                TO LD1-DT-VENC(5:4).

           MOVE SLIG62-TIT-CD-NOSNRO      TO WRK-NOSNRO.
           MOVE SLIG62-TIT-DT-ENTRADA-SISTEMA
                                          TO WRK-DT-ENTRADA.

           MOVE SLIG62-TIT-VL-PRINC-PROD  TO WRK-VALOR.
           MOVE SLIG62-TIT-VL-PAGO-PROD   TO WRK-VALOR-PG.
           MOVE SLIG62-TIT-VL-DESC-ABAT-PROD
                                          TO WRK-VALOR-ABAT.

      *--- CALCULA CARENCIA E TRATA TITULOS PAGOS/BAIXADOS/ -----------*
      *--- PRORROGADOS/NAO PAGOS ACUMULANDO VALORES -------------------*

           PERFORM 311200-TRATAR-NOSNRO

           IF  WRK-DT-ALT                 NOT EQUAL ZEROS
               MOVE WRK-DT-ALT            TO WRK-DT-AUX
               MOVE WRK-DD-AUX            TO LD1-DT-ALT(1:2)
               MOVE WRK-MM-AUX            TO LD1-DT-ALT(3:2)
               MOVE WRK-AA-AUX            TO LD1-DT-ALT(5:4)
               MOVE ZEROS                 TO WRK-DT-ALT
           ELSE
               MOVE SPACES                TO LD1-DT-ALT
           END-IF.

           IF  WRK-DT-PGTO                NOT EQUAL ZEROS
               MOVE WRK-DT-PGTO           TO WRK-DT-AUX
               MOVE WRK-DD-AUX            TO LD1-DT-PGTO(1:2)
               MOVE WRK-MM-AUX            TO LD1-DT-PGTO(3:2)
               MOVE WRK-AA-AUX            TO LD1-DT-PGTO(5:4)
               MOVE ZEROS                 TO WRK-DT-PGTO
           ELSE
               MOVE SPACES                TO LD1-DT-PGTO
           END-IF.

           MOVE WRK-VALOR-R               TO LD1-VLR-TIT.

           IF  WRK-VALOR-ABAT             NOT EQUAL ZEROS
               MOVE WRK-VALOR-ABAT-R      TO LD1-VLR-ABAT-N
               MOVE ZEROS                 TO WRK-VALOR-ABAT
           ELSE
               MOVE SPACES                TO LD1-VLR-ABAT
           END-IF.

           MOVE WRK-OBS-AUX               TO LD1-OBS.

           PERFORM 311300-ACESSAR-CBGEB033.
           PERFORM 311400-IMPRIMIR-DETALHE.

      *----------------------------------------------------------------*
       311000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       311100-ACESSAR-SLIGB030         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FILIAL-TITU     EQUAL ZEROS AND
              WRK-CPFCGC-TITU NOT EQUAL ZEROS
              MOVE 'F'              TO  WRK-TP-PESS-SAC
           ELSE
              MOVE 'J'              TO  WRK-TP-PESS-SAC
           END-IF.

           MOVE WRK-TP-PESS-SAC                 TO
                CINDCD-PSSOA-SACDO OF SLIGB030.

           MOVE WRK-CPFCGC-TITU                 TO
                CCGC-CPF-SACDO-ST OF SLIGB030.

           EXEC SQL
             SELECT
                   CCGC_CPF_SACDO_ST,
                   CINDCD_PSSOA_SACDO,
                   ICOPLT_SACDO
             INTO
                   :SLIGB030.CCGC-CPF-SACDO-ST,
                   :SLIGB030.CINDCD-PSSOA-SACDO,
                   :SLIGB030.ICOPLT-SACDO
             FROM   DB2PRD.ID_SACDO_LIQDZ
             WHERE
                   CCGC_CPF_SACDO_ST  = :SLIGB030.CCGC-CPF-SACDO-ST  AND
                   CINDCD_PSSOA_SACDO = :SLIGB030.CINDCD-PSSOA-SACDO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'ID_SACDO_LIQDZ  ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                    EQUAL +100
               MOVE 'NOME DO SACADO NAO ENCONTRADO'
                                         TO  CB3-NOME
           ELSE
               MOVE ICOPLT-SACDO OF SLIGB030 TO  CB3-NOME
           END-IF.

      *----------------------------------------------------------------*
       311100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       311200-TRATAR-NOSNRO            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-INDICES-AUX.
           INITIALIZE WRK-INDICES.

           PERFORM 311220-CALCULAR-DT-CARENCIA.

           PERFORM 311230-TRATAR-VALORES-TITULOS

           PERFORM 311270-ACUMULAR-VALORES.

           MOVE SLIG62-TIT-CD-ORIGEM         TO WRK-TIT-CD-ORIGEM.

           PERFORM 210000-LER-MOVTOSEL.

      *----------------------------------------------------------------*
       311200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       311220-CALCULAR-DT-CARENCIA     SECTION.
      *----------------------------------------------------------------*

           IF  SLIG62-PRODUTO-CTRL     = 'A'
               MOVE SLIG62-TIT-DT-VENCTO-PROD TO
                    WRK-1640-DATA-ENTRADA-N
           ELSE
               IF  SLIG62-PRODUTO-CTRL = 'B'
                   MOVE SLIG62-VND-DT-VENCTO-PARC TO
                        WRK-1640-DATA-ENTRADA-N.

           MOVE 10                        TO WRK-NUM-DIAS-0025.
           MOVE 04                        TO WRK-1640-FORMATO-ENTRADA
                                             WRK-1640-FORMATO-SAIDA.
4SYS01******OVE 'DC'                      TO WRK-1640-TIPO-INCREMENTO.
4SYS01******OVE 10                        TO WRK-1640-INCREMENTO.
4SYS01     MOVE 'DU'                      TO WRK-1640-TIPO-INCREMENTO.
4SYS01     MOVE 3                         TO WRK-1640-INCREMENTO.

           CALL 'POOL1640'             USING WRK-1640-DATA-ENTRADA
                                             WRK-1640-FORMATO-ENTRADA
                                             WRK-1640-TIPO-INCREMENTO
                                             WRK-1640-INCREMENTO
                                             WRK-1640-FORMATO-SAIDA
                                             WRK-1640-DATA-SAIDA
                                             WRK-1640-MENSAGEM.

           IF RETURN-CODE              EQUAL ZEROS
              MOVE WRK-1640-DATA-SAIDA TO WRK-DT-CARENCIA-R
           ELSE
              MOVE RETURN-CODE         TO WRK-RET-COD-1640
              DISPLAY '****************** SLIG7110 ******************'
              DISPLAY '*                                            *'
              DISPLAY '*         ERRO NO ACESSO A POOL1640          *'
              DISPLAY '*              RETURN-CODE = ' WRK-RET-COD-1640
                                                     '              *'
              DISPLAY '*          PROCESSAMENTO CANCELADO           *'
              DISPLAY '*                                            *'
              DISPLAY '****************** SLIG7110 ******************'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'POOL1640'          TO ERR-MODULO
              MOVE WRK-MENSA10         TO ERR-TEXTO
              PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       311220-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       311230-TRATAR-VALORES-TITULOS   SECTION.
      *----------------------------------------------------------------*

           MOVE SLIG62-TIT-DT-PAGTO-PROD TO WRK-DT-PGTO.
           MOVE ZEROS                    TO WRK-DT-ALT.

      *------> TITULOS COM VENCIMENTO ANTERIOR AO PERIODO

      *   1_  ACUMULA  VANTES-PER-NAO-PG
      *                TITULOS EM ABERTO OU BAIXADOS COM VENCIMENTO
      *                ANTERIOR AO PERIODO DE REFERENCIA

           IF  SLIG62-TIT-DT-VENCTO-PROD   LESS SLIG62-DT-PER-INI    AND
             ((SLIG62-TIT-DT-PAGTO-PROD    EQUAL ZEROS               AND
               SLIG62-TIT-VL-PAGO-PROD     EQUAL ZEROS            )  OR
              (SLIG62-TIT-DT-PAGTO-PROD    EQUAL ZEROS               AND
               SLIG62-TIT-CD-OCO-REM-L     EQUAL 002                 AND
              (SLIG62-TIT-CD-MOT-REM-SIS   NOT EQUAL 061 AND 062     AND
                                                     067 AND 066) ))

               MOVE 'A'                TO WRK-INDICE-A
                                          WRK-IND-A-AUX
               MOVE 'C'                TO WRK-INDICE-C
                                          WRK-IND-C-AUX
           END-IF.

      *------> TITULOS COM VENCIMENTO E PAGAMENTO NO PERIODO

      *   2_  ACUMULA  VVCTO-PER-PG
      *                TITULOS COM DATA DE VENCIMENTO NO PERIODO DE PAGO
      *                DENTRO DA CARENCIA NAO ANTECIPADO

           IF  SLIG62-TIT-DT-VENCTO-PROD NOT LESS
                                         SLIG62-DT-PER-INI           AND
               SLIG62-TIT-DT-VENCTO-PROD NOT GREATER
                                         SLIG62-DT-PER-FIM           AND
               SLIG62-TIT-DT-PAGTO-PROD  NOT LESS
                                         SLIG62-DT-PER-INI           AND
               SLIG62-TIT-DT-PAGTO-PROD  NOT GREATER
                                         WRK-DT-CARENCIA             AND
               SLIG62-TIT-VL-PAGO-PROD   NOT EQUAL ZEROS             AND
               SLIG62-TIT-DT-PAGTO-PROD  NOT LESS
                                         SLIG62-TIT-DT-VENCTO-PROD   AND
               SLIG62-TIT-CD-OCO-REM-L   EQUAL 002                   AND
              (SLIG62-TIT-CD-MOT-REM-SIS EQUAL 061 OR 062 OR 066 OR 067)

               MOVE 'A'                  TO WRK-INDICE-A
                                            WRK-IND-A-AUX
               MOVE 'B'                  TO WRK-INDICE-B
                                            WRK-IND-B-AUX
               MOVE 'D'                  TO WRK-INDICE-D
                                            WRK-IND-D-AUX
           END-IF.

      *------> TITULOS PENDENTES PAGOS FORA DA CARENCIA

      *   3_  ACUMULA  VVCTO-PER-NAO-PG
      *                TITULOS PENDENTES NO PERIODO E PAGOS FORA DA
      *                CARENCIA

           IF  SLIG62-TIT-DT-VENCTO-PROD   NOT LESS
                                           SLIG62-DT-PER-INI         AND
               SLIG62-TIT-DT-VENCTO-PROD   NOT GREATER
                                           SLIG62-DT-PER-FIM         AND
             ((SLIG62-TIT-DT-PAGTO-PROD    EQUAL ZEROS               AND
               SLIG62-TIT-VL-PAGO-PROD     EQUAL ZEROS   )           OR
              (SLIG62-TIT-VL-PAGO-PROD     NOT EQUAL ZEROS           AND
               SLIG62-TIT-DT-PAGTO-PROD    GREATER WRK-DT-CARENCIA   AND
               SLIG62-TIT-CD-OCO-REM-L     EQUAL 002                 AND
              (SLIG62-TIT-CD-MOT-REM-SIS   EQUAL 061 OR 062          OR
                                                 066 OR 067)))
               MOVE 'A'                  TO WRK-INDICE-A
                                            WRK-IND-A-AUX
               MOVE 'B'                  TO WRK-INDICE-B
                                            WRK-IND-B-AUX
           END-IF.

      *------> TITULOS BAIXADOS NAO ANTECIPADOS

      *   4_  ACUMULA  VVCTO-PER-BXADO
      *                TITULOS COM DATA DE VENCIMENTO NO PERIODO E
      *                BAIXADOS NAO ANTECIPADO

           IF  SLIG62-TIT-DT-VENCTO-PROD   NOT LESS
                                           SLIG62-DT-PER-INI         AND
               SLIG62-TIT-DT-VENCTO-PROD   NOT GREATER
                                           SLIG62-DT-PER-FIM         AND
               SLIG62-TIT-DT-ENTRADA-ORIGEM NOT LESS
                                           SLIG62-TIT-DT-VENCTO-PROD AND
               SLIG62-TIT-CD-OCO-REM-L     EQUAL 002                 AND
              (SLIG62-TIT-CD-MOT-REM-SIS   NOT EQUAL 061 AND 062     AND
                                                     067 AND 066)

               MOVE 'A'                  TO WRK-INDICE-A
                                            WRK-IND-A-AUX
               MOVE 'B'                  TO WRK-INDICE-B
                                            WRK-IND-B-AUX
           END-IF.

      *------> TITULOS COM PAGAMENTO ANTECIPADO

      *   5_  ACUMULA  VVCTO-APOS-PER-PG (ANTECIPADOS)
      *                TITULOS PAGOS NO PERIODO E ATENCIPADOS

           IF  SLIG62-TIT-VL-PAGO-PROD   NOT EQUAL ZEROS             AND
               SLIG62-TIT-CD-OCO-REM-L   EQUAL 002                   AND
              (SLIG62-TIT-CD-MOT-REM-SIS EQUAL 061 OR 062 OR 066 OR 067)
                                                                     AND
               SLIG62-TIT-DT-PAGTO-PROD  NOT LESS SLIG62-DT-PER-INI  AND
               SLIG62-TIT-DT-PAGTO-PROD  NOT GREATER
                                         SLIG62-DT-PER-FIM           AND
               SLIG62-TIT-DT-PAGTO-PROD  LESS
                                         SLIG62-TIT-DT-VENCTO-PROD

               MOVE 'A'                  TO WRK-INDICE-A
                                            WRK-IND-A-AUX
               MOVE 'B'                  TO WRK-INDICE-B
                                            WRK-IND-B-AUX
               MOVE 'H'                  TO WRK-INDICE-H
                                            WRK-IND-H-AUX
               MOVE 'D'                  TO WRK-INDICE-D
                                            WRK-IND-D-AUX
           END-IF.

      *------> TITULOS COM VENCIMENTO POSTERIOR AO PERIODO

      *   6_  ACUMULA  VVCTO-APOS-PER-NAO-PG
      *                TITULOS PENDENTES EM ABERTO COM VENCIMENTO FUTURO

           IF  SLIG62-TIT-DT-VENCTO-PROD GREATER SLIG62-DT-PER-FIM  AND
               SLIG62-TIT-DT-PAGTO-PROD  EQUAL ZEROS                AND
               SLIG62-TIT-VL-PAGO-PROD   EQUAL ZEROS
               MOVE 'A'                TO WRK-INDICE-A
                                          WRK-IND-A-AUX
           END-IF.

      *------> TITULOS COM BAIXA ANTECIPADA

      *   7_  ACUMULA  VVCTO-APOS-PER-BXADO (ANTECIPADOS)
      *                TITULOS BAIXADOS NO PERIODO E VENCIMENTO FUTURO
      *                ATENCIPADOS OU NAO

           IF  SLIG62-TIT-DT-VENCTO-PROD     GREATER
                                             SLIG62-DT-PER-FIM       AND
               SLIG62-TIT-DT-ENTRADA-ORIGEM  LESS
                                           SLIG62-TIT-DT-VENCTO-PROD AND
               SLIG62-TIT-CD-OCO-REM-L       EQUAL 002               AND
              (SLIG62-TIT-CD-MOT-REM-SIS     NOT EQUAL 061 AND 062   AND
                                                       067 AND 066)
               MOVE 'A'                  TO WRK-INDICE-A
                                            WRK-IND-A-AUX
               MOVE 'E'                  TO WRK-INDICE-E
                                            WRK-IND-E-AUX
           END-IF.

      *------> TITULOS PRORROGADOS

      *   8_  ACUMULA  VTOT-PRROG
      *                TITULOS PRORROGADOS COM DATA DE SOLICITACAO NO
      *                PERIODO, INDEPENDENTE DO VENCIMENTO

           IF (SLIG62-TIT-CD-OCO-REM-L       EQUAL 006 OR 031)       AND
               SLIG62-TIT-DT-ENTRADA-ORIGEM  NOT LESS
                                             SLIG62-DT-PER-INI       AND
               SLIG62-TIT-DT-ENTRADA-ORIGEM  NOT GREATER
                                             SLIG62-DT-PER-FIM

               MOVE SLIG62-TIT-DT-ENTRADA-ORIGEM
                                       TO WRK-DT-ALT

               MOVE 'F'                TO WRK-INDICE-F
                                          WRK-IND-F-AUX

               IF (SLIG62-TIT-DT-VENCTO-ANTERIOR NOT LESS
                   SLIG62-DT-PER-INI             AND
                   SLIG62-TIT-DT-VENCTO-ANTERIOR NOT GREATER
                   SLIG62-DT-PER-FIM)            AND
                  (SLIG62-TIT-DT-VENCTO-PROD     GREATER
                   SLIG62-DT-PER-FIM)
                  MOVE 'B'                TO WRK-INDICE-B
                                             WRK-IND-B-AUX
               END-IF
           END-IF.

      *------> TITULOS PAGOS COM ABATIMENTO

      *   9_  ACUMULA  VTOT-ABTMT
      *                TITULOS PAGOS DENTRO DO PERIODO ANTECIPADOS E
      *                PAGOS DENTRO DA CARENCIA NAO ANTECIPADOS,
      *                CUJA A DIFERENCA ENTRE O VALOR DEVIDO E O VALOR
      *                PAGO SEJA A MENOR

           MOVE ZEROS                   TO WRK-VL-PRINC-PROD-CONV
                                           WRK-VL-PG-PROD
                                           WRK-OSCILACAO.

           COMPUTE WRK-VL-PRINC-PROD-CONV =
                   SLIG62-TIT-VL-PRINC-PROD / 100.

           MOVE SLIG62-TIT-VL-PAGO-PROD TO WRK-VL-PG-PROD.

           COMPUTE WRK-OSCILACAO  = WRK-VL-PRINC-PROD-CONV
                                  - WRK-VL-PG-PROD-R.

           IF  WRK-OSCILACAO             GREATER ZEROS               AND
               SLIG62-TIT-CD-OCO-REM-L   EQUAL 002                   AND
              (SLIG62-TIT-CD-MOT-REM-SIS EQUAL 061 OR 062 OR 066 OR 067)
                                                                     AND
            ((SLIG62-TIT-DT-VENCTO-PROD   NOT LESS SLIG62-DT-PER-INI AND
              SLIG62-TIT-DT-VENCTO-PROD   NOT GREATER
                                              SLIG62-DT-PER-FIM      AND
               SLIG62-TIT-DT-PAGTO-PROD  NOT GREATER
                                         WRK-DT-CARENCIA             AND
               SLIG62-TIT-DT-PAGTO-PROD  NOT LESS
                                         SLIG62-TIT-DT-VENCTO-PROD )
                                                                     OR
              (SLIG62-TIT-DT-PAGTO-PROD NOT LESS SLIG62-DT-PER-INI   AND
               SLIG62-TIT-DT-PAGTO-PROD NOT GREATER
                                                 SLIG62-DT-PER-FIM   AND
               SLIG62-TIT-DT-PAGTO-PROD LESS
                                        SLIG62-TIT-DT-VENCTO-PROD  ))

               MOVE 'G'                  TO WRK-INDICE-G
                                            WRK-IND-G-AUX

               MOVE WRK-OSCILACAO        TO WRK-VALOR-ABAT-R

           END-IF.

      *----------------------------------------------------------------*
       311230-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       311270-ACUMULAR-VALORES         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INDICE-A             EQUAL 'A'
               ADD WRK-VALOR-R          TO ACU-INDICE-A
           END-IF.

           IF  WRK-INDICE-B             EQUAL 'B'
               ADD WRK-VALOR-R          TO ACU-INDICE-B
           END-IF.

           IF  WRK-INDICE-C             EQUAL 'C'
               ADD WRK-VALOR-R          TO ACU-INDICE-C
           END-IF.

           IF  WRK-INDICE-D             EQUAL 'D'
               ADD WRK-VALOR-R          TO ACU-INDICE-D
           END-IF.

           IF  WRK-INDICE-E             EQUAL 'E'
               ADD WRK-VALOR-R          TO ACU-INDICE-E
           END-IF.

           IF  WRK-INDICE-F             EQUAL 'F'
               ADD WRK-VALOR-R          TO ACU-INDICE-F
           END-IF.

           IF  WRK-INDICE-G             EQUAL 'G'
               IF  WRK-PRODUTO-CTRL     EQUAL 'A'
                   ADD WRK-VALOR-ABAT-R TO ACU-INDICE-G
               ELSE
                   ADD WRK-VALOR-R      TO ACU-INDICE-G
               END-IF
           END-IF.

           IF  WRK-INDICE-H             EQUAL 'H'
               ADD WRK-VALOR-R          TO ACU-INDICE-H
           END-IF.

           IF  WRK-INDICE-I             EQUAL 'I'
               ADD WRK-VALOR-R          TO ACU-INDICE-I
           END-IF.

           IF  WRK-INDICE-J             EQUAL 'J'
               ADD WRK-VALOR-R          TO ACU-INDICE-J
           END-IF.

           IF  WRK-INDICE-K             EQUAL 'K'
               ADD WRK-VALOR-R          TO ACU-INDICE-K
           END-IF.

      *----------------------------------------------------------------*
       311270-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       311300-ACESSAR-CBGEB033         SECTION.
      *----------------------------------------------------------------*

           MOVE 'CORIGE_COBR'          TO CELMTO-DISCT-COBR OF CBGEB033.
           MOVE WRK-TIT-CD-ORIGEM      TO CVLR-DISCT-COBR   OF CBGEB033.

           EXEC SQL
             SELECT RVLR_DISCT_COBR
             INTO  :CBGEB033.RVLR-DISCT-COBR
             FROM   DB2PRD.VLR_DISCT_COBR
             WHERE  CELMTO_DISCT_COBR  = :CBGEB033.CELMTO-DISCT-COBR
               AND  CVLR_DISCT_COBR    = :CBGEB033.CVLR-DISCT-COBR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'VLR_DISCT_COBR'   TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE RVLR-DISCT-COBR OF CBGEB033        TO LD1-LIQUID
           ELSE
               MOVE '**********'                       TO LD1-LIQUID
           END-IF.

      *----------------------------------------------------------------*
       311300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       311400-IMPRIMIR-DETALHE         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           IF  ACU-LINHAS              GREATER 58      OR
              (LD3-OBS               NOT EQUAL SPACES  AND
               ACU-LINHAS              GREATER 57      )
               PERFORM 311410-IMPRIMIR-CABEC
           END-IF.

           IF  WRK-PRODUTO-CTRL        EQUAL 'A'
               WRITE REG-GW333333      FROM LINDET1
           ELSE
               IF  WRK-PRODUTO-CTRL    EQUAL 'B'
                   WRITE REG-GW333333  FROM LINDET2
               ELSE
                   WRITE REG-GW333333  FROM LINDET3

      *            IF LD3-OBS          NOT EQUAL SPACES
                   IF WRK-CHV-EMITENTE-ATU NOT EQUAL
                      WRK-CHV-EMITENTE-ANT
                      ADD 1                TO    ACU-LINHAS
                      PERFORM 112000-TESTAR-FS-GW333333
                      WRITE REG-GW333333   FROM  CABEC4
                   END-IF
               END-IF
           END-IF.

           PERFORM 112000-TESTAR-FS-GW333333.

           ADD 1                       TO ACU-LINHAS
                                          ACU-IMPRESSOS.

      *----------------------------------------------------------------*
       311400-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       311410-IMPRIMIR-CABEC           SECTION.
      *----------------------------------------------------------------*

           PERFORM 311411-IMPRIMIR-CB-PRINC.

           WRITE REG-GW333333          FROM CABEC4.

           PERFORM 112000-TESTAR-FS-GW333333.

           IF  WRK-PRODUTO-CTRL        EQUAL 'A'
               WRITE REG-GW333333      FROM CABEC5
           ELSE
               IF  WRK-PRODUTO-CTRL    EQUAL 'B'
                   WRITE REG-GW333333  FROM CABEC6
               ELSE
                   WRITE REG-GW333333  FROM CABEC7
               END-IF
           END-IF.

           PERFORM 112000-TESTAR-FS-GW333333.

           WRITE REG-GW333333          FROM CABEC4.

           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 7                      TO ACU-LINHAS.

      *----------------------------------------------------------------*
       311410-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       311411-IMPRIMIR-CB-PRINC        SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAG.

           MOVE ACU-PAG                TO CB1-PAG.

           WRITE REG-GW333333          FROM CABEC1.

           PERFORM 112000-TESTAR-FS-GW333333.

           WRITE REG-GW333333          FROM CABEC2.

           PERFORM 112000-TESTAR-FS-GW333333.

           WRITE REG-GW333333          FROM CABEC3.

           PERFORM 112000-TESTAR-FS-GW333333.

      *----------------------------------------------------------------*
       311411-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       312000-TRATAR-VENDOR            SECTION.
      *----------------------------------------------------------------*

           MOVE SLIG62-CGC-CPF-CTRL-ST(1:3) TO CB3-CNPJ(1:3).
           MOVE SLIG62-CGC-CPF-CTRL-ST(4:3) TO CB3-CNPJ(5:3).
           MOVE SLIG62-CGC-CPF-CTRL-ST(7:3) TO CB3-CNPJ(9:3).
           MOVE '.'                         TO CB3-CNPJ(4:1).
           MOVE '.'                         TO CB3-CNPJ(8:1).
           MOVE SLIG62-VND-NOME-COMP      TO CB3-NOME.
           MOVE 'VENDOR '                 TO CB3-PRODUTO.
           MOVE 'COMPRADOR.: '            TO CB3-DESC-TP.

           MOVE SLIG62-DT-PER-INI         TO WRK-DT-AUX.
           MOVE WRK-DD-AUX                TO CB3-DD-INI.
           MOVE WRK-MM-AUX                TO CB3-MM-INI.
           MOVE WRK-AA-AUX                TO CB3-AA-INI.

           MOVE SLIG62-DT-PER-FIM         TO WRK-DT-AUX.
           MOVE WRK-DD-AUX                TO CB3-DD-FIM.
           MOVE WRK-MM-AUX                TO CB3-MM-FIM.
           MOVE WRK-AA-AUX                TO CB3-AA-FIM.

           MOVE WRK-CARTEIRA(SLIG62-CARTEIRA-CTRL)
                                          TO CB3-CARTEIRA.

           MOVE SLIG62-CPRODT-CLI         TO LD2-PROD.
           MOVE SLIG62-CFAML-CLI          TO LD2-FAML.
           MOVE SLIG62-CCONTR-CLI         TO LD2-CONTRATO.
           MOVE SLIG62-CSEQ-CLI           TO LD2-SEQ.
           MOVE SLIG62-CPARCELA-CLI       TO LD2-PCLA.
           MOVE SLIG62-NUM-DOCTO          TO LD2-TITULO.
           MOVE SLIG62-VND-CGC-CPF-FORN-ST
                                          TO LD2-CNPJ-CLI.

           IF  SLIG62-VND-FILIAL-FORN-ST  NOT EQUAL '0000'
               MOVE 'J'                   TO LD2-TIPO-CLI
           ELSE
               MOVE 'F'                   TO LD2-TIPO-CLI
           END-IF.

           MOVE SPACES                    TO LD2-DT-ENT LD2-DT-ALT.

           MOVE SLIG62-VND-DT-VENCTO-PARC TO WRK-DT-AUX.
           MOVE WRK-DD-AUX                TO LD2-DT-VENC(1:2).
           MOVE WRK-MM-AUX                TO LD2-DT-VENC(3:2).
           MOVE WRK-AA-AUX                TO LD2-DT-VENC(5:4).

           MOVE SLIG62-VND-DT-ENTRADA-ORIGEM TO WRK-DT-AUX.
           MOVE WRK-DD-AUX                TO LD2-DT-ENT(1:2).
           MOVE WRK-MM-AUX                TO LD2-DT-ENT(3:2).
           MOVE WRK-AA-AUX                TO LD2-DT-ENT(5:4).

           MOVE ZEROS                     TO WRK-VALOR-ACUM.
           MOVE SLIG62-VND-NOSSO-NUMERO   TO WRK-NOSNRO.

           INITIALIZE WRK-INDICES-AUX.

           PERFORM 312100-TRATAR-NOSNRO-VND

           IF  WRK-DT-PGTO                NOT EQUAL ZEROS
               MOVE WRK-DT-PGTO           TO WRK-DT-AUX
               MOVE WRK-DD-AUX            TO LD2-DT-PGTO(1:2)
               MOVE WRK-MM-AUX            TO LD2-DT-PGTO(3:2)
               MOVE WRK-AA-AUX            TO LD2-DT-PGTO(5:4)
               MOVE ZEROS                 TO WRK-DT-PGTO
           ELSE
               MOVE SPACES                TO LD2-DT-PGTO
           END-IF.

           MOVE WRK-VALOR-ACUM            TO LD2-VLR-TIT.

           MOVE SPACES                    TO LD2-VLR-ABAT
                                             LD2-LIQUID.
           MOVE WRK-OBS-AUX               TO LD2-OBS.

           PERFORM 311400-IMPRIMIR-DETALHE.

      *----------------------------------------------------------------*
       312000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       312100-TRATAR-NOSNRO-VND        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-INDICES.

           MOVE SLIG62-VND-DT-PAGTO-PARC    TO WRK-DT-PGTO.

      *------> VENDOR COM VENCIMENTO ANTERIOR AO PERIODO

           IF  SLIG62-VND-CTPO-SIT-PARC-VENC                        AND
               SLIG62-VND-DT-VENCTO-PARC LESS SLIG62-DT-PER-INI
               MOVE 'A'                  TO WRK-INDICE-A
                                            WRK-IND-A-AUX
               MOVE 'C'                  TO WRK-INDICE-C
                                            WRK-IND-C-AUX
           END-IF.

      *------> VENDOR COM VENCIMENTO NO PERIODO NAO PAGO

           IF  SLIG62-VND-CTPO-SIT-PARC-VENC                        AND
               SLIG62-VND-DT-VENCTO-PARC NOT LESS SLIG62-DT-PER-INI AND
               SLIG62-VND-DT-VENCTO-PARC NOT GREATER SLIG62-DT-PER-FIM

               MOVE 'A'                  TO WRK-INDICE-A WRK-IND-A-AUX
               MOVE 'B'                  TO WRK-INDICE-B WRK-IND-B-AUX

           END-IF.

      *------> VENDOR COM VENCIMENTO NO PERIODO PAGO

           IF  SLIG62-VND-CTPO-PAGTO-NORMAL                         AND
               SLIG62-VND-DT-VENCTO-PARC NOT LESS SLIG62-DT-PER-INI AND
               SLIG62-VND-DT-VENCTO-PARC NOT GREATER SLIG62-DT-PER-FIM

               MOVE 'A'          TO WRK-INDICE-A WRK-IND-A-AUX
               MOVE 'B'          TO WRK-INDICE-B WRK-IND-B-AUX

               PERFORM 311220-CALCULAR-DT-CARENCIA

               IF  WRK-DT-PGTO  LESS  WRK-DT-CARENCIA
                   IF SLIG62-VND-IND-PAGTO-COMP
                      MOVE 'D'   TO WRK-INDICE-D WRK-IND-D-AUX
                   ELSE
                      IF SLIG62-VND-IND-PAGTO-FORN
                         MOVE 'E' TO WRK-INDICE-E WRK-IND-E-AUX

                      END-IF
                   END-IF
               END-IF
           END-IF.

      *------> VENDOR COM VENCIMENTO ANTERIOR AO PERIODO (ATRASADO)

           IF  SLIG62-VND-CTPO-PAGTO-NORMAL                         AND
               SLIG62-VND-DT-VENCTO-PARC LESS SLIG62-DT-PER-INI

               MOVE 'A'          TO WRK-INDICE-A WRK-IND-A-AUX

               IF SLIG62-VND-IND-PAGTO-COMP
                  MOVE 'D'       TO WRK-INDICE-D WRK-IND-D-AUX
               ELSE
                  IF SLIG62-VND-IND-PAGTO-FORN
                     MOVE 'E'    TO WRK-INDICE-E WRK-IND-E-AUX

                  END-IF
               END-IF
           END-IF.

      *------> VENDOR COM VENCIMENTO NO PERIODO (ANTECIPADO)


           IF  SLIG62-VND-CTPO-PAGTO-ANTECI                         AND
               SLIG62-VND-DT-VENCTO-PARC NOT LESS SLIG62-DT-PER-INI AND
               SLIG62-VND-DT-VENCTO-PARC NOT GREATER SLIG62-DT-PER-FIM

               MOVE 'A'          TO WRK-INDICE-A WRK-IND-A-AUX
               MOVE 'B'          TO WRK-INDICE-B WRK-IND-B-AUX

               IF SLIG62-VND-IND-PAGTO-COMP
                  MOVE 'F'       TO WRK-INDICE-F WRK-IND-F-AUX
                  MOVE 'D'       TO WRK-INDICE-D WRK-IND-D-AUX
               ELSE
                  IF SLIG62-VND-IND-PAGTO-FORN
                     MOVE 'G'    TO WRK-INDICE-G WRK-IND-G-AUX
                     MOVE 'E'    TO WRK-INDICE-E WRK-IND-E-AUX

                  END-IF
               END-IF
           END-IF.

      *------> VENDOR COM PAGAMENTO NO PERIODO (ANTECIPADO)

           IF  SLIG62-VND-CTPO-PAGTO-ANTECI                         AND
               SLIG62-VND-DT-VENCTO-PARC GREATER SLIG62-DT-PER-FIM  AND
               WRK-DT-PGTO               NOT LESS SLIG62-DT-PER-INI AND
               WRK-DT-PGTO               NOT GREATER SLIG62-DT-PER-FIM

               MOVE 'A'                  TO WRK-INDICE-A WRK-IND-A-AUX
               MOVE 'B'                  TO WRK-INDICE-B WRK-IND-B-AUX

               IF  SLIG62-VND-IND-PAGTO-COMP
                   MOVE 'F'              TO WRK-INDICE-F WRK-IND-F-AUX
                   MOVE 'D'              TO WRK-INDICE-D WRK-IND-D-AUX
               ELSE
                   IF  SLIG62-VND-IND-PAGTO-FORN
                       MOVE 'G'          TO WRK-INDICE-G WRK-IND-G-AUX
                       MOVE 'E'          TO WRK-INDICE-E WRK-IND-E-AUX
                   END-IF
               END-IF
           END-IF.

      *------> VENDOR COM VENCIMENTO NO PERIODO BAIXADOS

           IF (SLIG62-VND-CTPO-BAIXA-NORMAL OR
               SLIG62-VND-CTPO-BAIXA-ANTECI)                        AND
               SLIG62-VND-DT-VENCTO-PARC NOT LESS SLIG62-DT-PER-INI AND
               SLIG62-VND-DT-VENCTO-PARC NOT GREATER SLIG62-DT-PER-FIM

               MOVE 'A'                  TO WRK-INDICE-A WRK-IND-A-AUX
               MOVE 'B'                  TO WRK-INDICE-B WRK-IND-B-AUX

           END-IF.

      *------> VENDOR COM BAIXA ANTECIPADA

           IF  SLIG62-VND-CTPO-BAIXA-ANTECI                         AND
               SLIG62-VND-DT-VENCTO-PARC GREATER SLIG62-DT-PER-FIM

               MOVE 'A'                  TO WRK-INDICE-A WRK-IND-A-AUX
               MOVE 'H'                  TO WRK-INDICE-H WRK-IND-H-AUX

           END-IF.

      *------> VENDOR COM VENCIMENTO POSTERIOR NAO PAGO

           IF  SLIG62-VND-CTPO-SIT-PARC-FUTU                        AND
               SLIG62-VND-DT-VENCTO-PARC GREATER SLIG62-DT-PER-FIM
               MOVE 'A'                  TO WRK-INDICE-A WRK-IND-A-AUX
           END-IF.

           MOVE SLIG62-VND-VL-PRINC-PARC TO WRK-VALOR-R.

           PERFORM 311270-ACUMULAR-VALORES.

           ADD  SLIG62-VND-VL-PRINC-PARC TO WRK-VALOR-ACUM.

           PERFORM 210000-LER-MOVTOSEL.

      *----------------------------------------------------------------*
       312100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       313000-TRATAR-CHEQUES           SECTION.
      *----------------------------------------------------------------*

           MOVE SLIG62-CGC-CPF-CTRL-ST(1:3) TO CB3-CNPJ(1:3).
           MOVE SLIG62-CGC-CPF-CTRL-ST(4:3) TO CB3-CNPJ(5:3).
           MOVE SLIG62-CGC-CPF-CTRL-ST(7:3) TO CB3-CNPJ(9:3).
           MOVE '.'                         TO CB3-CNPJ(4:1).
           MOVE '.'                         TO CB3-CNPJ(8:1).
           MOVE SLIG62-CHQ-NOME-EMITT     TO CB3-NOME.
           MOVE 'CHEQUES'                 TO CB3-PRODUTO.
           MOVE 'EMITENTE..: '            TO CB3-DESC-TP.

           MOVE SLIG62-DT-PER-INI         TO WRK-DT-AUX.
           MOVE WRK-DD-AUX                TO CB3-DD-INI.
           MOVE WRK-MM-AUX                TO CB3-MM-INI.
           MOVE WRK-AA-AUX                TO CB3-AA-INI.

           MOVE SLIG62-DT-PER-FIM         TO WRK-DT-AUX.
           MOVE WRK-DD-AUX                TO CB3-DD-FIM.
           MOVE WRK-MM-AUX                TO CB3-MM-FIM.
           MOVE WRK-AA-AUX                TO CB3-AA-FIM.

           MOVE WRK-CARTEIRA(SLIG62-CARTEIRA-CTRL)
                                          TO CB3-CARTEIRA.

           MOVE WRK-CHV-EMITENTE-ATU TO WRK-CHV-EMITENTE-ANT.

           MOVE SPACES               TO WRK-ERRO-STATUS
                                        WRK-RESP-ULTIMA-DEVOLUCAO
                                        WRK-REAP-ULTIMA-DEVOLUCAO
                                        WRK-LIT-STATUS
                                        WRK-LIT-MOTDEV.

           MOVE 99999999             TO WRK-ULTIMA-DEVOLUCAO.

           MOVE ZEROS                TO WRK-ULT-CTPO-CHEQ
                                        WRK-DT-VENCIMENTO
                                        WRK-DT-DEPOSITO-ERRO
                                        WRK-DT-CONTABIL-ERRO
                                        WRK-DT-CONTABIL-ULT
                                        WRK-DT-SITUACAO-ULT
                                        WRK-TOT-ANT-ABER
                                        WRK-TOT-ANT-COMP
                                        WRK-TOT-ANT-REAP-N-RESP
                                        WRK-TOT-ANT-REAP-S-RESP
                                        WRK-TOT-ANT-DEVL-S-RESP
                                        WRK-TOT-ANT-DEVL-S-REAP
                                        WRK-TOT-ANT-DEVL-N-REAP
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
                                        WRK-TOT-FUT-ABER
                                        WRK-TOT-FUT-COMP
                                        WRK-TOT-FUT-REAP-N-RESP
                                        WRK-TOT-FUT-REAP-S-RESP
                                        WRK-TOT-FUT-DEVL-S-RESP
                                        WRK-TOT-FUT-DEVL-S-REAP
                                        WRK-TOT-FUT-DEVL-N-REAP
                                        WRK-TOT-FUT-BAIX
                                        WRK-TOT-FUT-BAX-ANT.

           PERFORM 313010-PROCESSAR-STATUS
                   UNTIL WRK-FS-MOVTOSEL      EQUAL '10'   OR
                   WRK-CHV-EMITENTE-ATU NOT EQUAL WRK-CHV-EMITENTE-ANT
                   OR WRK-ERRO-STATUS EQUAL 'S'.

      *----------------------------------------------------------------*
       313000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       313010-PROCESSAR-STATUS         SECTION.
      *----------------------------------------------------------------*

           MOVE SLIG62-CHQ-BCO-EMITT      TO LD3-BCO.
           MOVE SLIG62-CHQ-AGENCIA-EMITT  TO LD3-AGEN.
           MOVE SLIG62-CHQ-CONTA-EMITT    TO LD3-CONTA.
           MOVE SLIG62-CHQ-NUM-CHEQUE     TO LD3-CHEQUE.
           MOVE SLIG62-CHQ-CGC-CPF-CLI-ST TO LD3-CNPJ-CLIE.

           IF (SLIG62-CHQ-CGC-CPF-CLI-ST  NOT EQUAL '000000000' AND
CNPJ7C                                               SPACES)    AND
              (SLIG62-CHQ-FILIAL-CLI-ST   NOT EQUAL '0000'  AND
CNPJ7C                                               SPACES)
               MOVE 'J'                   TO LD3-TIPO-CLIE
           ELSE
               IF  SLIG62-CHQ-CGC-CPF-CLI-ST
                                          NOT EQUAL '000000000'  AND
CNPJ7C                                               SPACES
                   MOVE 'F'               TO LD3-TIPO-CLIE
               ELSE
                   MOVE SPACES            TO LD3-TIPO-CLIE
               END-IF
           END-IF.

           MOVE SLIG62-CHQ-DT-DEPOSITO    TO WRK-DT-AUX.
           MOVE WRK-DD-AUX                TO LD3-DT-DEP(1:2).
           MOVE WRK-MM-AUX                TO LD3-DT-DEP(3:2).
           MOVE WRK-AA-AUX                TO LD3-DT-DEP(5:4).

           MOVE SLIG62-CHQ-DT-CONTABIL    TO WRK-DT-AUX.
           MOVE WRK-DD-AUX                TO LD3-DT-CTBL(1:2).
           MOVE WRK-MM-AUX                TO LD3-DT-CTBL(3:2).
           MOVE WRK-AA-AUX                TO LD3-DT-CTBL(5:4).

           MOVE SLIG62-CHQ-VL-PRINC-CHEQ  TO WRK-VALOR-R
           MOVE WRK-VALOR-R               TO LD3-VLR-CHQ.
           MOVE SLIG62-CHQ-NUM-CHEQUE     TO WRK-NOSNRO.

           MOVE SLIG62-CHQ-DT-CONTABIL TO WRK-DT-CTBIL.
           MOVE SLIG62-CHQ-DT-SITUACAO TO WRK-DT-SITU.

           MOVE SLIG62-CHQ-BCO-CLI     TO LD3-BCO-CLI
           MOVE SLIG62-CHQ-AGENCIA-CLI TO LD3-AGEN-CLI
           MOVE SLIG62-CHQ-CONTA-CLI   TO LD3-CONTA-CLI

           IF  WRK-DT-SITU                NOT EQUAL ZEROS
               MOVE WRK-DT-SITU           TO WRK-DT-AUX
               MOVE WRK-DD-AUX            TO LD3-DT-SITU(1:2)
               MOVE WRK-MM-AUX            TO LD3-DT-SITU(3:2)
               MOVE WRK-AA-AUX            TO LD3-DT-SITU(5:4)
               MOVE ZEROS                 TO WRK-DT-SITU
           ELSE
               MOVE SPACES                TO LD3-DT-SITU
           END-IF.

           PERFORM 313020-SUMARIZA-STATUS.

           PERFORM 313011-FORMATA-LIT-STATUS.

           PERFORM 210000-LER-MOVTOSEL.

           IF WRK-CHV-EMITENTE-ATU EQUAL WRK-CHV-EMITENTE-ANT AND
              WRK-FS-MOVTOSEL  NOT EQUAL '10'
              MOVE SPACES                    TO LD3-OBS
           ELSE
              PERFORM 313100-TRATAR-NOSNRO-CHQ

              MOVE WRK-OBS-AUX               TO LD3-OBS
           END-IF.

           IF WRK-ERRO-STATUS  NOT EQUAL 'S'
              PERFORM 311400-IMPRIMIR-DETALHE
           ELSE
              PERFORM 313050-TRATA-ERRO-SITUACAO
           END-IF.

      *----------------------------------------------------------------*
       313010-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       313011-FORMATA-LIT-STATUS       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                    TO WRK-LIT-MOTDEV

           EVALUATE SLIG62-CHQ-CTPO-CHEQ
              WHEN 1
                 MOVE 'BAIXADO'           TO WRK-LIT-STATUS
              WHEN 2
                 MOVE 'COMPENSADO'        TO WRK-LIT-STATUS
              WHEN 3
                 MOVE 'REAPRESENTADO'     TO WRK-LIT-STATUS
              WHEN 4
                 MOVE 'ABERTO'            TO WRK-LIT-STATUS
              WHEN 5
                 MOVE 'DEVOLVIDO  -  '       TO WRK-LIT-STATUS
                 MOVE SLIG62-CODI-DEVOL-CHQ  TO WRK-LIT-MOTDEV
              WHEN OTHER
                 MOVE SPACES              TO WRK-LIT-STATUS
           END-EVALUATE.

           MOVE WRK-LIT-STATUS            TO LD3-LIQUID.
           MOVE WRK-LIT-MOTDEV            TO LD3-MOTDEV.

      *----------------------------------------------------------------*
       313011-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       313020-SUMARIZA-STATUS          SECTION.
      *----------------------------------------------------------------*

      *--- TRATAMENTO DOS STATUS ENCONTRADOS PARA O CHEQUES -----------*

           EVALUATE SLIG62-CHQ-CTPO-CHEQ
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
                 MOVE 'S'                    TO WRK-ERRO-STATUS
                 MOVE SLIG62-CHQ-DT-DEPOSITO TO WRK-DT-DEPOSITO-ERRO
                 MOVE SLIG62-CHQ-DT-CONTABIL TO WRK-DT-CONTABIL-ERRO
           END-EVALUATE.

      *--- GUARDA O ULTIMO STATUS ENCONTRADO PARA A CHAVE DO CHEQUE ---*

           PERFORM  325000-VER-ULTIMO-TIPO-CHEQUE.

      *----------------------------------------------------------------*
       313020-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       330001-TRATA-TIPO-1             SECTION.
      *----------------------------------------------------------------*

      *--- TRATA BAIXA NORMAL DO PERIODO ANTERIO ----------------------*

           IF SLIG62-CHQ-DT-DEPOSITO < SLIG62-DT-PER-INI
              ADD   1               TO WRK-TOT-ANT-BAIX
           ELSE

      *--- TRATA BAIXA ANTECIPADA -------------------------------------*

           IF SLIG62-CHQ-DT-CONTABIL < SLIG62-CHQ-DT-DEPOSITO

              IF SLIG62-CHQ-DT-CONTABIL NOT LESS
                 SLIG62-DT-PER-INI          AND
                 SLIG62-CHQ-DT-CONTABIL NOT GREATER
                 SLIG62-DT-PER-FIM

      *--- TRATA BAIXA ANTECIPADA PERIODO ATUAL -----------------------*

                 IF SLIG62-CHQ-DT-DEPOSITO NOT > SLIG62-DT-PER-FIM
                    ADD 1                     TO WRK-TOT-PER-BAX-ANT
                 ELSE

      *--- TRATA BAIXA ANTECIPADA PERIODO FUTURO ----------------------*

                    ADD  1                    TO WRK-TOT-FUT-BAX-ANT
                 END-IF
              END-IF
           ELSE

      *--- TRATA BAIXA NORMAL PERIODO ATUAL ---------------------------*

              IF SLIG62-CHQ-DT-DEPOSITO NOT > SLIG62-DT-PER-FIM
                 ADD 1                     TO WRK-TOT-PER-BAIX
              ELSE

      *--- TRATA BAIXA NORMAL PERIODO FUTURO---------------------------*

                 ADD 1                     TO WRK-TOT-FUT-BAIX
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

           IF SLIG62-CHQ-DT-CONTABIL < SLIG62-DT-PER-INI
              IF (SLIG62-CHQ-DT-DEPOSITO NOT < SLIG62-DT-PER-INI AND
                  SLIG62-CHQ-DT-DEPOSITO NOT > SLIG62-DT-PER-FIM)
                  ADD 1                 TO WRK-TOT-PER-COMP
              END-IF

              IF (SLIG62-CHQ-DT-DEPOSITO NOT < WRK-DT-INI-PER-ULT AND
                  SLIG62-CHQ-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ULT)
                  ADD 1                 TO WRK-TOT-ANT-COMP
              END-IF

           ELSE

      *--- TRATA COMPENSACAO PERIODO ATUAL ----------------------------*

           IF SLIG62-CHQ-DT-CONTABIL NOT > SLIG62-DT-PER-FIM

              IF (SLIG62-CHQ-DT-DEPOSITO NOT < SLIG62-DT-PER-INI AND
                  SLIG62-CHQ-DT-DEPOSITO NOT > SLIG62-DT-PER-FIM)
                  ADD 1                 TO WRK-TOT-PER-COMP
              END-IF

              IF (SLIG62-CHQ-DT-DEPOSITO NOT < WRK-DT-INI-PER-ULT AND
                  SLIG62-CHQ-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ULT)
                  ADD 1                 TO WRK-TOT-ANT-COMP
              END-IF

           ELSE

      *--- TRATA COMPENSACAO PERIODO FUTURO ---------------------------*

              IF (SLIG62-CHQ-DT-DEPOSITO NOT < SLIG62-DT-PER-INI AND
                  SLIG62-CHQ-DT-DEPOSITO NOT > SLIG62-DT-PER-FIM)
                  ADD 1                 TO WRK-TOT-PER-COMP
              END-IF

      *       IF (SLIG62-CHQ-DT-DEPOSITO NOT < WRK-DT-INI-PER-PRX AND
      *           SLIG62-CHQ-DT-DEPOSITO NOT > WRK-DT-FIM-PER-PRX)
      *           ADD 1                 TO WRK-TOT-FUT-COMP
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

              MOVE 4               TO SLIG62-CHQ-CTPO-CHEQ
              PERFORM                 330004-TRATA-TIPO-4
           ELSE

      *--- VERIFICA DT EVENTO DA ULTIMA DEVOLUCAO ENCONTRADA ----------*

           IF  WRK-ULTIMA-DEVOLUCAO EQUAL ZEROS
               PERFORM  330055-TRATA-ERRO-DEVOLUCAO
           END-IF

      *--- IDENTIFICA REENTRADA QUANDO HA DEVOLUCAO -------------------*

           IF SLIG62-CHQ-DT-CONTABIL < WRK-ULTIMA-DEVOLUCAO

      *--- CONTABILIZA ABERTOS ----------------------------------------*

              MOVE 4            TO SLIG62-CHQ-CTPO-CHEQ
              PERFORM              330004-TRATA-TIPO-4

           ELSE

      *--- VERIFICA RESPONSABILIDADE DA REAPRESENTACAO ----------------*

           IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL SPACES
              PERFORM              330033-TRATA-ERRO-REAP
           END-IF

      *--- TRATA REAPRESENTACAO PERIODO ANTERIOR ----------------------*

           IF SLIG62-CHQ-DT-CONTABIL < SLIG62-DT-PER-INI

              IF (SLIG62-CHQ-DT-DEPOSITO NOT < SLIG62-DT-PER-INI AND
                  SLIG62-CHQ-DT-DEPOSITO NOT > SLIG62-DT-PER-FIM)
                  IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                     ADD   1              TO WRK-TOT-PER-REAP-S-RESP
                  ELSE
                     ADD   1              TO WRK-TOT-PER-REAP-N-RESP
                  END-IF
              END-IF

              IF (SLIG62-CHQ-DT-DEPOSITO NOT < WRK-DT-INI-PER-ULT AND
                  SLIG62-CHQ-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ULT)
                  IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                     ADD   1              TO WRK-TOT-ANT-REAP-S-RESP
                  ELSE
                     ADD   1              TO WRK-TOT-ANT-REAP-N-RESP
                  END-IF
              END-IF
           ELSE

      *--- TRATA REAPRESENTACAO PERIODO ATUAL -------------------------*

           IF SLIG62-CHQ-DT-CONTABIL NOT > SLIG62-DT-PER-FIM

              IF (SLIG62-CHQ-DT-DEPOSITO NOT < SLIG62-DT-PER-INI AND
                  SLIG62-CHQ-DT-DEPOSITO NOT > SLIG62-DT-PER-FIM)
                  IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                     ADD   1              TO WRK-TOT-PER-REAP-S-RESP
                  ELSE
                     ADD   1              TO WRK-TOT-PER-REAP-N-RESP
                  END-IF
              END-IF

              IF (SLIG62-CHQ-DT-DEPOSITO NOT < WRK-DT-INI-PER-ULT AND
                  SLIG62-CHQ-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ULT)
                  IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                     ADD   1              TO WRK-TOT-ANT-REAP-S-RESP
                  ELSE
                     ADD   1              TO WRK-TOT-ANT-REAP-N-RESP
                  END-IF
              END-IF

           ELSE

      *--- TRATA REAPRESENTACAO PERIODO FUTURO ------------------------*

              IF (SLIG62-CHQ-DT-DEPOSITO NOT < SLIG62-DT-PER-INI AND
                  SLIG62-CHQ-DT-DEPOSITO NOT > SLIG62-DT-PER-FIM)
                  IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                     ADD   1              TO WRK-TOT-PER-REAP-S-RESP
                  ELSE
                     ADD   1              TO WRK-TOT-PER-REAP-N-RESP
                  END-IF
              END-IF

      *       IF (SLIG62-CHQ-DT-DEPOSITO NOT < WRK-DT-INI-PER-PRX AND
      *           SLIG62-CHQ-DT-DEPOSITO NOT > WRK-DT-FIM-PER-PRX)
      *           IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
      *              ADD   1              TO WRK-TOT-FUT-REAP-S-RESP
      *           ELSE
      *              ADD   1              TO WRK-TOT-FUT-REAP-N-RESP
      *           END-IF
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

           DISPLAY '***************** SLIG7430 *******************'
           DISPLAY '*                                            *'
           DISPLAY '*         PROCESSAMENTO INTERROMPIDO         *'
           DISPLAY '*                                            *'
           DISPLAY '*     REAPRESENTACAO SEM DEVOLUCAO           *'
           DISPLAY '*                                            *'
           DISPLAY '*     CHAVE ATUAL:                           *'
           DISPLAY '*                                            *'
           DISPLAY '*     BCO-EMITT       : ' WRK-BANCO-ATU
                                              '                  *'
           DISPLAY '*     AGENCIA-EMITT   : ' WRK-AGENCIA-ATU
                                                '                *'
           DISPLAY '*     CONTA-EMITT     : ' WRK-CONTA-ATU
                                                        '        *'
           DISPLAY '*     CHEQUE-EMITT    : ' WRK-CHEQUE-ATU
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

           IF SLIG62-CHQ-DT-DEPOSITO < SLIG62-DT-PER-INI
              ADD 1             TO WRK-TOT-ANT-ABER
           ELSE

      *--- TRATA ABERTOS DO PERIODO ATUAL -----------------------------*

              IF SLIG62-CHQ-DT-DEPOSITO NOT > SLIG62-DT-PER-FIM
                 ADD 1                TO  WRK-TOT-PER-ABER
              ELSE

      *--- TRATA ABERTOS DO PERIODO FUTURO ----------------------------*

                 ADD 1                TO  WRK-TOT-FUT-ABER
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

           PERFORM  313060-VER-DEVOLUCAO  VARYING WRK-IND FROM 1 BY 1
                    UNTIL WRK-IND GREATER 99 OR WRK-ACHOU EQUAL 'S'.

           IF  WRK-ACHOU  EQUAL  'N'
               PERFORM  330055-TRATA-ERRO-DEVOLUCAO
           END-IF.

      *--- TRATA DEVOLUCAO COM EVENTO NO PERIODO ANTERIOR -------------*

           IF SLIG62-CHQ-DT-CONTABIL <   SLIG62-DT-PER-INI
               PERFORM  330006-TRATA-DEVL-ANT
           ELSE

      *--- TRATA DEVOLUCAO COM EVENTO NO PERIODO ATUAL ----------------*

           IF SLIG62-CHQ-DT-CONTABIL NOT > SLIG62-DT-PER-FIM
               PERFORM  330007-TRATA-DEVL-ATU
           ELSE

      *--- TRATA DEVOLUCAO COM EVENTO NO PERIODO FUTURO ---------------*

               PERFORM  330008-TRATA-DEVL-FUT
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
           DISPLAY '*         PROCESSAMENTO INTERROMPIDO         *'
           DISPLAY '*                                            *'
           DISPLAY '*     CODIGO DE DEVOLUCAO NAO ENCONTADO      *'
           DISPLAY '*                                            *'
           DISPLAY '*     CHAVE ATUAL:                           *'
           DISPLAY '*                                            *'
           DISPLAY '*     BCO-EMITT       : ' WRK-BANCO-ATU
                                              '                  *'
           DISPLAY '*     AGENCIA-EMITT   : ' WRK-AGENCIA-ATU
                                                '                *'
           DISPLAY '*     CONTA-EMITT     : ' WRK-CONTA-ATU
                                                        '        *'
           DISPLAY '*     CHEQUE-EMITT    : ' WRK-CHEQUE-ATU
                                                        '        *'
           DISPLAY '*                                            *'
           DISPLAY '****************** SLIG6210 ******************'
           MOVE 'APL'              TO ERR-TIPO-ACESSO
           MOVE  WRK-MSG-ERRO-DEVL TO ERR-TEXTO

           PERFORM 999999-PROCESSAR-ROTINA-ERRO.

      *----------------------------------------------------------------*
       330055-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       330006-TRATA-DEVL-ANT           SECTION.
      *----------------------------------------------------------------*

           IF (SLIG62-CHQ-DT-DEPOSITO NOT < SLIG62-DT-PER-INI AND
               SLIG62-CHQ-DT-DEPOSITO NOT > SLIG62-DT-PER-FIM)

               IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                  ADD   1                TO WRK-TOT-PER-DEVL-S-RESP
               ELSE
                  IF WRK-REAP-ULTIMA-DEVOLUCAO EQUAL 'S'
                     ADD   1                TO WRK-TOT-PER-DEVL-S-REAP
                  ELSE
                     ADD   1                TO WRK-TOT-PER-DEVL-N-REAP
                  END-IF
               END-IF
           END-IF.

           IF (SLIG62-CHQ-DT-DEPOSITO NOT < WRK-DT-INI-PER-ULT AND
               SLIG62-CHQ-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ULT)

               IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                  ADD   1                TO WRK-TOT-ANT-DEVL-S-RESP
               ELSE
                  IF WRK-REAP-ULTIMA-DEVOLUCAO EQUAL 'S'
                     ADD   1                TO WRK-TOT-ANT-DEVL-S-REAP
                  ELSE
                     ADD   1                TO WRK-TOT-ANT-DEVL-N-REAP
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

           IF (SLIG62-CHQ-DT-DEPOSITO NOT < SLIG62-DT-PER-INI AND
               SLIG62-CHQ-DT-DEPOSITO NOT > SLIG62-DT-PER-FIM)

               IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                  ADD   1                TO WRK-TOT-PER-DEVL-S-RESP
               ELSE
                  IF WRK-REAP-ULTIMA-DEVOLUCAO EQUAL 'S'
                     ADD   1                TO WRK-TOT-PER-DEVL-S-REAP
                  ELSE
                     ADD   1                TO WRK-TOT-PER-DEVL-N-REAP
                  END-IF
               END-IF
           END-IF.

           IF (SLIG62-CHQ-DT-DEPOSITO NOT < WRK-DT-INI-PER-ULT AND
               SLIG62-CHQ-DT-DEPOSITO NOT > WRK-DT-FIM-PER-ULT)

               IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                  ADD   1                TO WRK-TOT-ANT-DEVL-S-RESP
               ELSE
                  IF WRK-REAP-ULTIMA-DEVOLUCAO EQUAL 'S'
                     ADD   1                TO WRK-TOT-ANT-DEVL-S-REAP
                  ELSE
                     ADD   1                TO WRK-TOT-ANT-DEVL-N-REAP
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

           IF (SLIG62-CHQ-DT-DEPOSITO NOT < SLIG62-DT-PER-INI AND
               SLIG62-CHQ-DT-DEPOSITO NOT > SLIG62-DT-PER-FIM)

               IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
                  ADD   1                TO WRK-TOT-PER-DEVL-S-RESP
               ELSE
                  IF WRK-REAP-ULTIMA-DEVOLUCAO EQUAL 'S'
                     ADD   1                TO WRK-TOT-PER-DEVL-S-REAP
                  ELSE
                     ADD   1                TO WRK-TOT-PER-DEVL-N-REAP
                  END-IF
               END-IF
           END-IF.

      *    IF (SLIG62-CHQ-DT-DEPOSITO NOT < WRK-DT-INI-PER-PRX AND
      *        SLIG62-CHQ-DT-DEPOSITO NOT > WRK-DT-FIM-PER-PRX)
      *
      *        IF WRK-RESP-ULTIMA-DEVOLUCAO EQUAL 'S'
      *           ADD   1                TO WRK-TOT-FUT-DEVL-S-RESP
      *        ELSE
      *           IF WRK-REAP-ULTIMA-DEVOLUCAO EQUAL 'S'
      *              ADD   1                TO WRK-TOT-FUT-DEVL-S-REAP
      *           ELSE
      *              ADD   1                TO WRK-TOT-FUT-DEVL-N-REAP
      *           END-IF
      *        END-IF
      *    END-IF.


      *----------------------------------------------------------------*
       330008-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       313060-VER-DEVOLUCAO            SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CTPO-DEVLC(WRK-IND)  EQUAL  ZEROS
               MOVE  100  TO  WRK-IND
               MOVE SPACES      TO WRK-RESP-ULTIMA-DEVOLUCAO
                                   WRK-REAP-ULTIMA-DEVOLUCAO
               MOVE ZEROS       TO WRK-ULTIMA-DEVOLUCAO
           ELSE
             IF  WRK-CTPO-DEVLC(WRK-IND)  EQUAL  SLIG62-CODI-DEVOL-CHQ
                 MOVE  'S'  TO  WRK-ACHOU
                 MOVE WRK-RESP-DEVLC(WRK-IND)
                                         TO WRK-RESP-ULTIMA-DEVOLUCAO
                 MOVE WRK-POBLD-REAP(WRK-IND)
                                         TO WRK-REAP-ULTIMA-DEVOLUCAO
                 MOVE SLIG62-CHQ-DT-CONTABIL
                                         TO WRK-ULTIMA-DEVOLUCAO
           END-IF.

      *----------------------------------------------------------------*
       313060-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       313050-TRATA-ERRO-SITUACAO      SECTION.
      *----------------------------------------------------------------*

           ADD 1                      TO  ACU-ERRO-CHEQUE.

           IF ACU-ERRO-CHEQUE       LESS  101
              DISPLAY '***************** SLIG7110 *******************'
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
              DISPLAY '* WRK-DT-DEPOSITO = ' WRK-DT-DEPOSITO-ERRO
                                                  '                 *'
              DISPLAY '* WRK-DT-CONTABIL = ' WRK-DT-CONTABIL-ERRO
                                                  '                 *'
              DISPLAY '*                                            *'
              DISPLAY '*----------  SITUACAO  ENCONTRADA  ----------*'
              DISPLAY '* ' WRK-ULT-CTPO-CHEQ
                           '                                        *'
              DISPLAY '*                                            *'
              DISPLAY '**********************************************'
           END-IF.

      *----------------------------------------------------------------*
       313050-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       313100-TRATAR-NOSNRO-CHQ        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-INDICES-AUX WRK-INDICES.

      *--- (A) TOTAL DA CARTEIRA
      *--- (B) VENCIMENTO NA CARTEIRA
      *--- (C) BAIXA ANTECIPADA
      *--- (D) DEPOSITOS NO PERIODO
      *--- (E) CHEQUES PAGOS
      *--- (F) PRIMEIRA DEVOLUCAO
      *--- (G) REAPRESENTADOS
      *--- (H) REAPRESENTADOS PAGOS
      *--- (I) REAPRESENTADOS DEVOLVIDOS
      *--- (J) ANTERIORES REAPRESENTADOS/DEVOLVIDOS
      *--- (K) DEVOLUCAO PERIODO ANTERIOR

      *--- ( DEVOLUCAO PERIODO ANTERIOR)    --------------------------*

           PERFORM  313101-TRATA-DEVL-PER-ANT.

      *--- ( BAIXAS PERIODO ANTECIPADAS )      -----------------------*

           IF WRK-TOT-PER-BAX-ANT  NOT EQUAL  ZEROS OR
              WRK-TOT-FUT-BAX-ANT  NOT EQUAL  ZEROS
              MOVE 'C'                TO WRK-INDICE-C WRK-IND-C-AUX
              MOVE 'B'                TO WRK-INDICE-B WRK-IND-B-AUX
              MOVE 'A'                TO WRK-INDICE-A WRK-IND-A-AUX
           END-IF.

      *--- ( PAGOS PRIMEIRO DEPOSITO  ) ------------------------------*

           IF WRK-ULT-CTPO-CHEQ           EQUAL 2     AND
              WRK-TOT-PER-COMP        NOT EQUAL ZEROS AND
              WRK-TOT-PER-DEVL-S-RESP     EQUAL ZEROS AND
              WRK-TOT-PER-DEVL-S-REAP     EQUAL ZEROS AND
              WRK-TOT-PER-DEVL-N-REAP     EQUAL ZEROS AND
              WRK-TOT-FUT-DEVL-S-RESP     EQUAL ZEROS AND
              WRK-TOT-FUT-DEVL-S-REAP     EQUAL ZEROS AND
              WRK-TOT-FUT-DEVL-N-REAP     EQUAL ZEROS
              MOVE 'A'                TO WRK-INDICE-A WRK-IND-A-AUX
              MOVE 'B'                TO WRK-INDICE-B WRK-IND-B-AUX
              MOVE 'D'                TO WRK-INDICE-D WRK-IND-D-AUX
              MOVE 'E'                TO WRK-INDICE-E WRK-IND-E-AUX
           END-IF.

      *--- ( BAIXA NORMAL )-------------------------------------------*

           IF WRK-TOT-PER-BAIX        NOT EQUAL ZEROS
              MOVE 'A'                TO WRK-INDICE-A WRK-IND-A-AUX
              MOVE 'B'                TO WRK-INDICE-B WRK-IND-B-AUX
           END-IF.

      *--- ABERTO NO PERIODO -----------------------------------------*

           IF ((WRK-TOT-FUT-ABER     NOT EQUAL ZEROS    OR
                WRK-TOT-PER-ABER     NOT EQUAL ZEROS ) AND
                WRK-TOT-PER-DEVL-N-REAP  EQUAL ZEROS   AND
                WRK-TOT-PER-DEVL-S-REAP  EQUAL ZEROS   AND
                WRK-TOT-FUT-DEVL-N-REAP  EQUAL ZEROS   AND
                WRK-TOT-FUT-DEVL-S-REAP  EQUAL ZEROS)
              MOVE 'A'                TO WRK-INDICE-A WRK-IND-A-AUX
           END-IF.

      *--- ( PRIMEIRA DEVOLUCAO ) ------------------------------------*

           IF (WRK-TOT-PER-DEVL-S-RESP NOT EQUAL 0)
              MOVE 'A'                TO WRK-INDICE-A WRK-IND-A-AUX
              MOVE 'B'                TO WRK-INDICE-B WRK-IND-B-AUX
              MOVE 'D'                TO WRK-INDICE-D WRK-IND-D-AUX
              MOVE 'F'                TO WRK-INDICE-F WRK-IND-F-AUX
           END-IF.

      *--- ( DEPOSITO NO PERIODO                ----------------------*
      *--- ( DEPOSITO DE CHEQUES NO PERIODO ATUAL --------------------*

           IF (WRK-TOT-PER-ABER            NOT EQUAL 0  OR
               WRK-TOT-PER-COMP            NOT EQUAL 0  OR
               WRK-TOT-PER-REAP-S-RESP     NOT EQUAL 0  OR
               WRK-TOT-PER-DEVL-S-RESP     NOT EQUAL 0) AND
              (WRK-TOT-PER-BAIX            EQUAL 0      AND
               WRK-TOT-PER-BAX-ANT         EQUAL 0      AND
               WRK-TOT-PER-DEVL-N-REAP     EQUAL 0      AND
               WRK-TOT-PER-DEVL-S-REAP     EQUAL 0      AND
               WRK-TOT-FUT-DEVL-N-REAP     EQUAL 0      AND
               WRK-TOT-FUT-DEVL-S-REAP     EQUAL 0)
              MOVE 'A'                TO WRK-INDICE-A WRK-IND-A-AUX
              MOVE 'B'                TO WRK-INDICE-B WRK-IND-B-AUX
              MOVE 'D'                TO WRK-INDICE-D WRK-IND-D-AUX
           END-IF.

      *--- ( REAPRESENTADOS PAGOS ) ------------*

           IF (WRK-ULT-CTPO-CHEQ               EQUAL 2 OR
               WRK-ULT-CTPO-CHEQ               EQUAL 3 ) AND
             (WRK-TOT-PER-REAP-S-RESP     NOT EQUAL 0  OR
              WRK-TOT-PER-REAP-N-RESP     NOT EQUAL 0  OR
              WRK-TOT-FUT-REAP-S-RESP     NOT EQUAL 0  OR
              WRK-TOT-FUT-REAP-N-RESP     NOT EQUAL 0  )
              MOVE 'A'                TO WRK-INDICE-A WRK-IND-A-AUX
              MOVE 'B'                TO WRK-INDICE-B WRK-IND-B-AUX
              MOVE 'D'                TO WRK-INDICE-D WRK-IND-D-AUX
              MOVE 'G'                TO WRK-INDICE-G WRK-IND-G-AUX
              MOVE 'H'                TO WRK-INDICE-H WRK-IND-H-AUX
           END-IF.

      *--- ( REAPRESENTADOS DEVOLVIDOS )    --------------------------*

           IF  WRK-TOT-PER-DEVL-N-REAP     EQUAL 0 AND
             ((WRK-TOT-PER-REAP-S-RESP NOT EQUAL 0 AND
               WRK-TOT-PER-DEVL-S-RESP > WRK-TOT-PER-REAP-S-RESP) OR
              (WRK-TOT-PER-DEVL-S-RESP EQUAL WRK-TOT-PER-REAP-S-RESP AND
               WRK-TOT-PER-DEVL-S-RESP NOT EQUAL 0 AND
               WRK-TOT-FUT-DEVL-S-RESP NOT EQUAL 0 ))
              MOVE 'A'                TO WRK-INDICE-A WRK-IND-A-AUX
              MOVE 'B'                TO WRK-INDICE-B WRK-IND-B-AUX
              MOVE 'D'                TO WRK-INDICE-D WRK-IND-D-AUX
              MOVE 'G'                TO WRK-INDICE-G WRK-IND-G-AUX
              MOVE 'I'                TO WRK-INDICE-I WRK-IND-I-AUX
           ELSE
           IF  WRK-TOT-FUT-DEVL-N-REAP     EQUAL 0 AND
              (WRK-TOT-FUT-REAP-S-RESP NOT EQUAL 0 AND
               WRK-TOT-FUT-DEVL-S-RESP > WRK-TOT-FUT-REAP-S-RESP)
              MOVE 'A'                TO WRK-INDICE-A WRK-IND-A-AUX
              MOVE 'B'                TO WRK-INDICE-B WRK-IND-B-AUX
              MOVE 'D'                TO WRK-INDICE-D WRK-IND-D-AUX
              MOVE 'G'                TO WRK-INDICE-G WRK-IND-G-AUX
              MOVE 'I'                TO WRK-INDICE-I WRK-IND-I-AUX
           END-IF
           END-IF.

      *--- ( REAPRESENTADOS ) ----------------------------------------*

           IF (WRK-TOT-ANT-REAP-S-RESP     NOT EQUAL 0  OR
               WRK-TOT-PER-REAP-S-RESP     NOT EQUAL 0) AND
              (WRK-TOT-PER-DEVL-N-REAP     EQUAL 0      AND
               WRK-TOT-FUT-DEVL-N-REAP     EQUAL 0      AND
               WRK-TOT-PER-DEVL-S-REAP     EQUAL 0      AND
               WRK-TOT-FUT-DEVL-S-REAP     EQUAL 0)
              MOVE 'A'                TO WRK-INDICE-A WRK-IND-A-AUX
              MOVE 'B'                TO WRK-INDICE-B WRK-IND-B-AUX
              MOVE 'D'                TO WRK-INDICE-D WRK-IND-D-AUX
              MOVE 'G'                TO WRK-INDICE-G WRK-IND-G-AUX
           END-IF.
      *
           PERFORM 311270-ACUMULAR-VALORES.

      *----------------------------------------------------------------*
       313100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       313101-TRATA-DEVL-PER-ANT       SECTION.
      *----------------------------------------------------------------*

           IF ((WRK-TOT-ANT-REAP-S-RESP     EQUAL 0   AND
                WRK-TOT-ANT-REAP-N-RESP     EQUAL 0   AND
                WRK-TOT-ANT-DEVL-S-RESP     EQUAL 0   AND
                WRK-TOT-ANT-DEVL-S-REAP     EQUAL 0   AND
                WRK-TOT-ANT-DEVL-N-REAP     EQUAL 0   AND
                WRK-TOT-ANT-COMP        NOT EQUAL 0)  AND
               (WRK-TOT-PER-DEVL-S-RESP NOT EQUAL 0   OR
                WRK-TOT-PER-DEVL-S-REAP NOT EQUAL 0   OR
                WRK-TOT-PER-DEVL-N-REAP NOT EQUAL 0   OR
                WRK-TOT-FUT-DEVL-S-RESP NOT EQUAL 0   OR
                WRK-TOT-FUT-DEVL-S-REAP NOT EQUAL 0   OR
                WRK-TOT-FUT-DEVL-N-REAP NOT EQUAL 0))
              MOVE 'K'                TO WRK-INDICE-K WRK-IND-K-AUX
           ELSE
           IF WRK-TOT-ANT-DEVL-S-REAP = 0                       AND
              WRK-TOT-ANT-DEVL-N-REAP = 0                       AND
              WRK-TOT-ANT-REAP-S-RESP > 0                       AND
              WRK-TOT-ANT-REAP-S-RESP = WRK-TOT-ANT-DEVL-S-RESP AND
            ((WRK-TOT-PER-DEVL-S-RESP > WRK-TOT-ANT-DEVL-S-RESP OR
              WRK-TOT-FUT-DEVL-S-RESP > WRK-TOT-ANT-DEVL-S-RESP) OR
             (WRK-TOT-PER-DEVL-S-REAP > 0                       OR
              WRK-TOT-FUT-DEVL-S-REAP > 0                       OR
              WRK-TOT-PER-DEVL-N-REAP > 0                       OR
              WRK-TOT-FUT-DEVL-N-REAP > 0))
              MOVE 'J'                TO WRK-INDICE-J WRK-IND-J-AUX
           ELSE
           IF WRK-TOT-ANT-DEVL-N-REAP = 0 AND
              WRK-TOT-ANT-REAP-N-RESP > 0 AND
              WRK-TOT-ANT-REAP-N-RESP = WRK-TOT-ANT-DEVL-S-REAP  AND
            ((WRK-TOT-PER-DEVL-S-REAP > WRK-TOT-ANT-DEVL-S-REAP  OR
              WRK-TOT-FUT-DEVL-S-REAP > WRK-TOT-ANT-DEVL-S-REAP) OR
             (WRK-TOT-PER-DEVL-S-RESP > WRK-TOT-ANT-DEVL-S-RESP  OR
              WRK-TOT-FUT-DEVL-S-RESP > WRK-TOT-ANT-DEVL-S-RESP) OR
             (WRK-TOT-PER-DEVL-N-REAP > 0                        OR
              WRK-TOT-FUT-DEVL-N-REAP > 0))
              MOVE 'K'                TO WRK-INDICE-K WRK-IND-K-AUX
           END-IF
           END-IF
           END-IF.

      *----------------------------------------------------------------*
       313101-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       320000-IMPRIMIR-TOT-CART        SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO               TO WRK-OPERACAO.

           IF (WRK-PRODUTO-CTRL            EQUAL 'A'   AND
               ACU-LINHAS                  GREATER 57) OR
              (WRK-PRODUTO-CTRL            EQUAL 'B'   AND
               ACU-LINHAS                  GREATER 58) OR
              (WRK-PRODUTO-CTRL            EQUAL 'C'   AND
      *        ACU-LINHAS                  GREATER 51)
               ACU-LINHAS                  GREATER 50)
               PERFORM 311411-IMPRIMIR-CB-PRINC
           END-IF.

           WRITE REG-GW333333              FROM LINTOT-BCO.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'TOTAL CARTEIRA'           TO LT1-LITERAL.
           MOVE ACU-INDICE-A               TO LT1-VALOR.
           MOVE 'A'                        TO LT1-TIPO.

           WRITE REG-GW333333              FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'VENCIMENTOS DA CARTEIRA'  TO LT1-LITERAL.
           MOVE ACU-INDICE-B               TO LT1-VALOR.
           MOVE 'B'                        TO LT1-TIPO.

           WRITE REG-GW333333              FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           IF  WRK-PRODUTO-CTRL            EQUAL 'A'
               PERFORM 321000-IMPRIMIR-TOT-TIT
           ELSE
               IF  WRK-PRODUTO-CTRL        EQUAL 'B'
                   PERFORM 322000-IMPRIMIR-TOT-VND
               ELSE
                   PERFORM 323000-IMPRIMIR-TOT-CHQ
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       320000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       321000-IMPRIMIR-TOT-TIT         SECTION.
      *----------------------------------------------------------------*

           MOVE 'VENCIDOS ANTERIORES'    TO LT1-LITERAL.
           MOVE ACU-INDICE-C             TO LT1-VALOR.
           MOVE 'C'                      TO LT1-TIPO.

           WRITE REG-GW333333            FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'PAGAMENTOS DA CARTEIRA' TO LT1-LITERAL.
           MOVE ACU-INDICE-D             TO LT1-VALOR.
           MOVE 'D'                      TO LT1-TIPO.

           WRITE REG-GW333333            FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'BAIXAS ANTECIPADAS'     TO LT1-LITERAL.
           MOVE ACU-INDICE-E             TO LT1-VALOR.
           MOVE 'E'                      TO LT1-TIPO.

           WRITE REG-GW333333            FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'PRORROGACOES'           TO LT1-LITERAL.
           MOVE ACU-INDICE-F             TO LT1-VALOR.
           MOVE 'F'                      TO LT1-TIPO.

           WRITE REG-GW333333            FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'ABATIMENTOS'            TO LT1-LITERAL.
           MOVE ACU-INDICE-G             TO LT1-VALOR.
           MOVE 'G'                      TO LT1-TIPO.

           WRITE REG-GW333333            FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'PAGAMENTOS ANTECIPADOS' TO LT1-LITERAL.
           MOVE ACU-INDICE-H             TO LT1-VALOR.
           MOVE 'H'                      TO LT1-TIPO.

           WRITE REG-GW333333            FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

      *----------------------------------------------------------------*
       321000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       322000-IMPRIMIR-TOT-VND         SECTION.
      *----------------------------------------------------------------*

           MOVE 'VENCIDOS ANTERIORES'    TO LT1-LITERAL.
           MOVE ACU-INDICE-C             TO LT1-VALOR.
           MOVE 'C'                      TO LT1-TIPO.

           WRITE REG-GW333333            FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'PAGAMENTOS NO PERIODO DO COMPRADOR'
                                       TO LT1-LITERAL.
           MOVE ACU-INDICE-D           TO LT1-VALOR.
           MOVE 'D'                    TO LT1-TIPO.

           WRITE REG-GW333333          FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'PAGAMENTOS NO PERIODO DO FORNECEDOR'
                                       TO LT1-LITERAL.
           MOVE ACU-INDICE-E           TO LT1-VALOR.
           MOVE 'E'                    TO LT1-TIPO.

           WRITE REG-GW333333          FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'PAGAMENTOS ANTECIPADOS DO COMPRADOR'
                                       TO LT1-LITERAL.
           MOVE ACU-INDICE-F           TO LT1-VALOR.
           MOVE 'F'                    TO LT1-TIPO.

           WRITE REG-GW333333          FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'PAGAMENTOS ANTECIPADOS DO FORNECEDOR'
                                       TO LT1-LITERAL.
           MOVE ACU-INDICE-G           TO LT1-VALOR.
           MOVE 'G'                    TO LT1-TIPO.

           WRITE REG-GW333333          FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'BAIXAS ANTECIPADAS'   TO LT1-LITERAL.
           MOVE ACU-INDICE-H           TO LT1-VALOR.
           MOVE 'H'                    TO LT1-TIPO.

           WRITE REG-GW333333          FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

      *----------------------------------------------------------------*
       322000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       323000-IMPRIMIR-TOT-CHQ         SECTION.
      *----------------------------------------------------------------*

           MOVE 'BAIXAS ANTECIPADAS'   TO LT1-LITERAL.
           MOVE ACU-INDICE-C           TO LT1-VALOR.
           MOVE 'C'                    TO LT1-TIPO.

           WRITE REG-GW333333          FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'DEPOSITOS DE CHEQUES NO PERIODO'
                                       TO LT1-LITERAL.
           MOVE ACU-INDICE-D           TO LT1-VALOR.
           MOVE 'D'                    TO LT1-TIPO.

           WRITE REG-GW333333          FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'CHEQUES PAGOS'        TO LT1-LITERAL.
           MOVE ACU-INDICE-E           TO LT1-VALOR.
           MOVE 'E'                    TO LT1-TIPO.

           WRITE REG-GW333333          FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'CHEQUES PRIMEIRA DEVOLUCAO'
                                       TO LT1-LITERAL.
           MOVE ACU-INDICE-F           TO LT1-VALOR.
           MOVE 'F'                    TO LT1-TIPO.

           WRITE REG-GW333333          FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'CHEQUES REAPRESENTADOS'
                                       TO LT1-LITERAL.
           MOVE ACU-INDICE-G           TO LT1-VALOR.
           MOVE 'G'                    TO LT1-TIPO.

           WRITE REG-GW333333          FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'CHEQUES REAPRESENTADOS PAGOS'
                                       TO LT1-LITERAL.
           MOVE ACU-INDICE-H           TO LT1-VALOR.
           MOVE 'H'                    TO LT1-TIPO.

           WRITE REG-GW333333          FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'CHEQUES REAPRESENTADOS DEVOLVIDOS'
                                       TO LT1-LITERAL.
           MOVE ACU-INDICE-I           TO LT1-VALOR.
           MOVE 'I'                    TO LT1-TIPO.

           WRITE REG-GW333333          FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'CHEQUES ANTER. REAPRESENT./DEVOLV.'
                                       TO LT1-LITERAL.
           MOVE ACU-INDICE-J           TO LT1-VALOR.
           MOVE 'J'                    TO LT1-TIPO.

           WRITE REG-GW333333          FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

           MOVE 'CHEQUES DEVOLUCAO PERIODO ANTERIOR'
                                       TO LT1-LITERAL.
           MOVE ACU-INDICE-K           TO LT1-VALOR.
           MOVE 'K'                    TO LT1-TIPO.

           WRITE REG-GW333333          FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-GW333333.

      *----------------------------------------------------------------*
       323000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       325000-VER-ULTIMO-TIPO-CHEQUE   SECTION.
      *----------------------------------------------------------------*

           IF (SLIG62-CHQ-DT-CONTABIL      GREATER
               WRK-DT-CONTABIL-ULT)
               PERFORM 325001-ATU-ULTIMO-TIPO-CHEQUE
           ELSE
               IF (SLIG62-CHQ-DT-CONTABIL       EQUAL
                   WRK-DT-CONTABIL-ULT          AND
                   SLIG62-CHQ-DT-SITUACAO        GREATER
                   WRK-DT-SITUACAO-ULT)
                   PERFORM 325001-ATU-ULTIMO-TIPO-CHEQUE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       325000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       325001-ATU-ULTIMO-TIPO-CHEQUE   SECTION.
      *----------------------------------------------------------------*

           MOVE SLIG62-CHQ-DT-CONTABIL       TO WRK-DT-CONTABIL-ULT.
           MOVE SLIG62-CHQ-DT-SITUACAO       TO WRK-DT-SITUACAO-ULT.
           MOVE SLIG62-CHQ-CTPO-CHEQ         TO WRK-ULT-CTPO-CHEQ.

      *----------------------------------------------------------------*
       325001-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       330000-ZERAR-ACUMULADORES       SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO ACU-INDICE-A
                                          ACU-INDICE-B
                                          ACU-INDICE-C
                                          ACU-INDICE-D
                                          ACU-INDICE-E
                                          ACU-INDICE-F
                                          ACU-INDICE-G
                                          ACU-INDICE-H
                                          ACU-INDICE-I
                                          ACU-INDICE-J
                                          ACU-INDICE-K.

      *----------------------------------------------------------------*
       330000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       400000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 410000-EMITIR-SRELCTRL.

           CLOSE ARQDATAS
                 MOVTOSEL
                 GW333333
                 SRELCTRL
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

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-SRELCTRL          FROM CABEC1-CTRL.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC2-CTRL.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC3-CTRL.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           MOVE ACU-LIDOS              TO LT1-LIDOS.
           MOVE ACU-IMPRESSOS          TO LT2-IMPRESSOS.

           WRITE REG-SRELCTRL          FROM LINTOT1-CTRL.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT2-CTRL.

           PERFORM 113000-TESTAR-FS-SRELCTRL.

      *----------------------------------------------------------------*
       410000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'SLIG7110'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
