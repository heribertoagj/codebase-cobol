      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1515.
       AUTHOR.     ANDRE LUIZ LONGO.
      *================================================================*
      *                    C P M - S I S T E M A S                     *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1515                                    *
      *    PROGRAMADOR.:   ANDRE LUIZ LONGO        - CPM PATO BRANCO   *
      *    ANALISTA CPM:   JOSSIANE CARLA GAZZONI  - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO CREMM           - PROCKWORK/ GP. 50 *
      *    DATA........:   21/12/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   TRATAR ESTORNO DE AGRUPAMENTO DE CLIENTE DO *
      *                    DIA.                                        *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *                    DB2PRD.CTRL_ROTNA_GFCT      GFCTB000        *
      *                    DB2PRD.PARM_DATA_PROCM      GFCTB0A1        *
      *                    DB2PRD.PDIDO_ESTRN_CLI      GFCTB0A4        *
      *                    DB2PRD.PDIDO_ESTRN_GRP      GFCTB0A7        *
      *                    DB2PRD.MOVTO_EVNTO_CRRTT    GFCTB092        *
      *                    DB2PRD.MOVTO_ROTNA_ORIGE    GFCTB094        *
      *                    DB2PRD.V01CLIENTE_AGENCIA   CLIEV004        *
      *                    DB2PRD.V01CTA_POUPANCA      CLIEV007        *
      *                    DB2PRD.V01CTA_CORRENTE      CLIEV008        *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                  ENTCLIE                     I#GFCTRS          *
      *                  SAICLIE                     I#GFCTRS          *
      *                  RELATO                         -              *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100       *
      *    I#GFCT0M - INCLUDE QUE ACOMPANHA MODULO GFCT5525.           *
      *    I#GFCTRS - INC PARA ARQUIVO OS ARQUIVOS ENTCLIE E SAICLIE.  *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7600 - OBTEM DATA E HORA DO SISTEMA                     *
      *    POOL7100 - MODULO PARA CANCELAMENTO DO PROGRAMA             *
      *================================================================*
      *   ULTIMA ALTERACAO:                                            *
      *   =================                                            *
      *                                                                *
      *   MARCELO CREMM - PROCWORK - SETEMBRO-2006                     *
      *                                                                *
      *   ===> PASSAR A TRATAR TAMBEM CSIT-EVNTO-RECBD = 15, 16 E 17.  *
      *   ===> TRATAR TAMBEM OS RELATORIOS.                            *
      *                                                                *
      *================================================================*
080208*----------------------------------------------------------------*
080208*              SONDA PROCWORK - CONSULTORIA - ALTERACAO          *
080208*----------------------------------------------------------------*
080208*    ANALISTA....:   VINICIUS                         / GRUPO 50 *
080208*    DATA........:   08/02/2008                                  *
080208*    OBJETIVO....:   INCLUSAO DE TRATAMENTO PARA SITUACOES:      *
080208*                    18, 19 E NN.                                *
080208*----------------------------------------------------------------*

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

           SELECT ENTCLIE  ASSIGN      TO UT-S-ENTCLIE
           FILE STATUS                 IS WRK-FS-ENTCLIE.

           SELECT SAICLIE  ASSIGN      TO UT-S-SAICLIE
           FILE STATUS                 IS WRK-FS-SAICLIE.

           SELECT RELATO   ASSIGN      TO UT-S-RELATO
           FILE STATUS                 IS WRK-FS-RELATO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQ MOVTO F4 CONSISTENTES - ENTRADA QUANDO RESTART  *
      *            ORG. SEQUENCIAL     -   LRECL   =   111             *
      *----------------------------------------------------------------*

       FD  ENTCLIE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRS'.

      *----------------------------------------------------------------*
      *    INPUT:  ARQ MOVTO F4 CONSISTENTES - SAIDA                   *
      *            ORG. SEQUENCIAL     -   LRECL   =   111             *
      *----------------------------------------------------------------*

       FD  SAICLIE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRS'.

      *----------------------------------------------------------------*
      *    OUTPUT: RELATORIO TOTALIZADOR                               *
      *            ORG. SEQUENCIAL     -   LRECL   =   133             *
      *----------------------------------------------------------------*

       FD  RELATO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-RELATO                  PIC  X(133).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** GFCT1515 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INDEXADOR ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(011) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-PAGINAS              PIC  9(005)    COMP-3  VALUE ZEROS.
       77  ACU-LINHAS               PIC  9(001)            VALUE 9.
       77  ACU-OCORRENCIAS          PIC  9(009)    COMP-3  VALUE ZEROS.
       77  ACU-QTD-ESTORNO-TOT      PIC  9(009)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-ESTORNO-TOT      PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-QTD-CANCELAMENTO-TOT PIC  9(009)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-CANCELAMENTO-TOT PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-QTD-ESTORNO          PIC  9(009)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-ESTORNO          PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-QTD-CANCELAMENTO     PIC  9(009)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-CANCELAMENTO     PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-SIT-TOT              PIC  9(013)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-TOT              PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-SIT-TOT-GERAL        PIC  9(011)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-TOT-GERAL        PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-SIT-1                PIC  9(011)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-1                PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-SIT-2                PIC  9(011)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-2                PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-SIT-3                PIC  9(011)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-3                PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-SIT-4                PIC  9(011)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-4                PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-SIT-5                PIC  9(011)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-5                PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-SIT-6                PIC  9(011)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-6                PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-SIT-9                PIC  9(011)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-9                PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-SIT-10               PIC  9(011)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-10               PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-SIT-11               PIC  9(011)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-11               PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-SIT-12               PIC  9(011)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-12               PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-SIT-13               PIC  9(011)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-13               PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-SIT-14               PIC  9(011)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-14               PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-SIT-15               PIC  9(011)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-15               PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-SIT-16               PIC  9(011)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-16               PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-SIT-17               PIC  9(011)    COMP-3  VALUE ZEROS.
       77  ACU-VLR-17               PIC  9(011)V99 COMP-3  VALUE ZEROS.
080208 77  ACU-SIT-18               PIC  9(011)    COMP-3  VALUE ZEROS.
080208 77  ACU-VLR-18               PIC  9(011)V99 COMP-3  VALUE ZEROS.
080208 77  ACU-SIT-19               PIC  9(011)    COMP-3  VALUE ZEROS.
080208 77  ACU-VLR-19               PIC  9(011)V99 COMP-3  VALUE ZEROS.
080208 77  ACU-SIT-NN               PIC  9(011)    COMP-3  VALUE ZEROS.
080208 77  ACU-VLR-NN               PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS             PIC  9(009)    COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       01  WRK-FIM-C1                  PIC  X(001)         VALUE SPACES.
       01  WRK-FIM-C2                  PIC  X(001)         VALUE SPACES.
       01  WRK-FIM-C3                  PIC  X(001)         VALUE SPACES.
       01  WRK-FIM-C4                  PIC  X(001)         VALUE SPACES.
       01  WRK-FIM-C5                  PIC  X(001)         VALUE SPACES.
       01  WRK-RESTART                 PIC  X(001)         VALUE SPACES.
       01  WRK-MASCARA-SQL             PIC  -999.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTES DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-FS-RELATO               PIC  X(002)         VALUE SPACES.
       01  WRK-FS-ENTCLIE              PIC  X(002)         VALUE SPACES.
       01  WRK-FS-SAICLIE              PIC  X(002)         VALUE SPACES.

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
           '*** MENSAGEM DE ERRO DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGEM-ERRO.
           03  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE
               ' - FILE STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG01                   PIC  X(075)         VALUE
           'QUANTIDADE DE OCORRENCIAS ENVIADO PELO PARM, PARA COMMIT, IN
      -    'VALIDO'.
       01  WRK-MSG03                   PIC  X(075)         VALUE
           '** CANCELADO APOS O PRIMEIRO COMMIT **'.
       01  WRK-MSG04                   PIC  X(075)         VALUE
           'QTDE DE OCORR. DE RESTART MAIOR QUE O NUMERO DE REGISTROS A
      -    'SER REGRAVADO.'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA RESTART ***'.
      *----------------------------------------------------------------*

       01  WRK-MCB.
           03 WRK-AMBIENTE             PIC  X(005)         VALUE SPACES.
           03 WRK-NOME-JOB             PIC  X(008)         VALUE SPACES.
           03 WRK-PROG-POOL            PIC  X(008)         VALUE SPACES.
           03 WRK-TP-REG               PIC  X(003)         VALUE SPACES.
           03 WRK-NOME-PSB             PIC  X(008)         VALUE SPACES.
           03 WRK-COD-TRANS            PIC  X(008)         VALUE SPACES.
           03 WRK-BAC-DIN              PIC  X(001)         VALUE SPACES.
           03 WRK-OP-DBRC              PIC  X(001)         VALUE SPACES.
           03 WRK-SUB-SIST             PIC  X(004)         VALUE SPACES.
           03 WRK-RESERVA              PIC  X(034)         VALUE SPACES.

       01  WRK-PARM-AREA.
           03  WRK-QTDE-COMMIT         PIC  9(007)         VALUE ZEROS.

       01  WRK-CHAVE-RESTART.
           03  WRK-QUANTIDADE          PIC  9(010)         VALUE ZEROS.
           03  FILLER                  PIC  X(240)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DATA-JULIANA        PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-DATA-AAMMDD         PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-DATA-AAAAMMDD       PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-HORA-HHMMSS         PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-HORA-HHMMSSMMMMMM   PIC  9(013) COMP-3  VALUE ZEROS.
           03  WRK-TIMESTAMP.
               05  WRK-ANO             PIC  9(004)         VALUE ZEROS.
               05  WRK-MES             PIC  9(002)         VALUE ZEROS.
               05  WRK-DIA             PIC  9(002)         VALUE ZEROS.
               05  WRK-HOR             PIC  9(002)         VALUE ZEROS.
               05  WRK-MIN             PIC  9(002)         VALUE ZEROS.
               05  WRK-SEG             PIC  9(002)         VALUE ZEROS.
               05  WRK-MIL             PIC  9(006)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CABECALHOS ***'.
      *----------------------------------------------------------------*

       01  CABEC1.
           03  FILLER                  PIC  X(001)         VALUE '1'.
           03  CB1-DIA                 PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB1-MES                 PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB1-ANO                 PIC  9999.
           03  FILLER                  PIC  X(040)         VALUE SPACES.
           03  FILLER                  PIC  X(011)         VALUE
               'B A N C O'.
           03  FILLER                  PIC  X(017)         VALUE
               'B R A D E S C O'.
           03  FILLER                  PIC  X(043)         VALUE 'S/A'.
           03  CB1-HOR                 PIC  99.
           03  FILLER                  PIC  X(001)         VALUE ':'.
           03  CB1-MIN                 PIC  99.
           03  FILLER                  PIC  X(001)         VALUE ':'.
           03  CB1-SEG                 PIC  99.

       01  CABEC2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(036)         VALUE
               'GFCT1515'.
           03  FILLER                  PIC  X(045)         VALUE
               'GFCT - GESTAO, FLEXIBILIZACAO E COBRANCA DE'.
           03  FILLER                  PIC  X(040)         VALUE
               'TARIFAS'.
           03  FILLER                  PIC  X(004)         VALUE 'PAG.'.
           03  CB2-PAGINA              PIC  ZZ99.

       01  CABEC3.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(031)         VALUE SPACES.
           03  FILLER                  PIC  X(054)         VALUE
               'RELATORIO DE MOVIMENTO DIARIO DE ESTORNO POR AGRUPAMEN'.
           03  FILLER                  PIC  X(014)         VALUE
               'TO DE CLIENTES'.

       01  CABEC4.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(131)         VALUE ALL
               '-'.

       01  CABEC5.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(053)         VALUE SPACES.
           03  FILLER                  PIC  X(013)         VALUE
               'MOVIMENTO DE '.
           03  CB5-DIA-MOV             PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB5-MES-MOV             PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB5-ANO-MOV             PIC  9999.

       01  CABEC6.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(131)         VALUE ALL
               '-'.

       01  CABEC7.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(132)         VALUE SPACES.

       01  CABEC8.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(055)         VALUE SPACES.
           03  FILLER                  PIC  X(027)         VALUE
               'QUANTIDADES'.
           03  FILLER                  PIC  X(007)         VALUE
               'VALORES'.

       01  CABEC9.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(053)         VALUE SPACES.
           03  FILLER                  PIC  X(036)         VALUE ALL
               '='.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA LINHAS DE DETALHE ***'.
      *----------------------------------------------------------------*

       01  LINDET1.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  FILLER                  PIC  X(038)         VALUE
               '01 - A PROCESSAR'.
           03  LD1-QTD-1               PIC  Z.ZZZ.ZZZ.ZZ9.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD1-VLR-1               PIC  ZZ.ZZZ.ZZZ.ZZ9,99.

       01  LINDET2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  FILLER                  PIC  X(038)         VALUE
               '02 - AGENDADO'.
           03  LD2-QTD-2               PIC  Z.ZZZ.ZZZ.ZZ9.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD2-VLR-2               PIC  ZZ.ZZZ.ZZZ.ZZ9,99.

       01  LINDET3.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  FILLER                  PIC  X(038)         VALUE
               '03 - REPIQUE'.
           03  LD3-QTD-3               PIC  Z.ZZZ.ZZZ.ZZ9.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD3-VLR-3               PIC  ZZ.ZZZ.ZZZ.ZZ9,99.

       01  LINDET4.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  FILLER                  PIC  X(038)         VALUE
               '04 - COBRADO PARCIAL'.
           03  LD4-QTD-4               PIC  Z.ZZZ.ZZZ.ZZ9.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD4-VLR-4               PIC  ZZ.ZZZ.ZZZ.ZZ9,99.

       01  LINDET5.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  FILLER                  PIC  X(038)         VALUE
               '05 - COBRADO TOTAL'.
           03  LD5-QTD-5               PIC  Z.ZZZ.ZZZ.ZZ9.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD5-VLR-5               PIC  ZZ.ZZZ.ZZZ.ZZ9,99.

       01  LINDET6.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  FILLER                  PIC  X(038)         VALUE
               '06 - ESTORNADO PARCIAL'.
           03  LD6-QTD-6               PIC  Z.ZZZ.ZZZ.ZZ9.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD6-VLR-6               PIC  ZZ.ZZZ.ZZZ.ZZ9,99.

       01  LINDET7.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  FILLER                  PIC  X(038)         VALUE
               '09 - RETENCAO'.
           03  LD7-QTD-9               PIC  Z.ZZZ.ZZZ.ZZ9.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD7-VLR-9               PIC  ZZ.ZZZ.ZZZ.ZZ9,99.

       01  LINDET8.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  FILLER                  PIC  X(038)         VALUE
               '10 - GESTAO COBRANCA'.
           03  LD8-QTD-10              PIC  Z.ZZZ.ZZZ.ZZ9.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD8-VLR-10              PIC  ZZ.ZZZ.ZZZ.ZZ9,99.

       01  LINDET9.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  FILLER                  PIC  X(038)         VALUE
               '11 - GESTAO ESTORNO'.
           03  LD9-QTD-11              PIC  Z.ZZZ.ZZZ.ZZ9.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD9-VLR-11              PIC  ZZ.ZZZ.ZZZ.ZZ9,99.

       01  LINDET10.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  FILLER                  PIC  X(038)         VALUE
               '12 - NAO COBRADO'.
           03  LD10-QTD-12              PIC  Z.ZZZ.ZZZ.ZZ9.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD10-VLR-12              PIC  ZZ.ZZZ.ZZZ.ZZ9,99.

       01  LINDET11.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  FILLER                  PIC  X(038)         VALUE
               '13 - EM PROCESSO COBRANCA'.
           03  LD11-QTD-13              PIC  Z.ZZZ.ZZZ.ZZ9.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD11-VLR-13              PIC  ZZ.ZZZ.ZZZ.ZZ9,99.

       01  LINDET12.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  FILLER                  PIC  X(038)         VALUE
               '14 - ERRO NO PROCESSO'.
           03  LD12-QTD-14              PIC  Z.ZZZ.ZZZ.ZZ9.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD12-VLR-14              PIC  ZZ.ZZZ.ZZZ.ZZ9,99.

       01  LINDET13.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  FILLER                  PIC  X(038)         VALUE
               '15 - SUSPENSA        '.
           03  LD13-QTD-15              PIC  Z.ZZZ.ZZZ.ZZ9.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD13-VLR-15              PIC  ZZ.ZZZ.ZZZ.ZZ9,99.

       01  LINDET14.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  FILLER                  PIC  X(038)         VALUE
               '16 - ISENTO          '.
           03  LD14-QTD-16              PIC  Z.ZZZ.ZZZ.ZZ9.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD14-VLR-16              PIC  ZZ.ZZZ.ZZZ.ZZ9,99.

       01  LINDET15.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  FILLER                  PIC  X(038)         VALUE
               '17 - A CALCULAR      '.
           03  LD15-QTD-17              PIC  Z.ZZZ.ZZZ.ZZ9.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  LD15-VLR-17              PIC  ZZ.ZZZ.ZZZ.ZZ9,99.

080208 01  LINDET16.
080208     03  FILLER                  PIC  X(001)         VALUE SPACES.
080208     03  FILLER                  PIC  X(015)         VALUE SPACES.
080208     03  FILLER                  PIC  X(038)         VALUE
080208         '18 - BONIFICAR EXCEDENTE '.
080208     03  LD16-QTD-18              PIC  Z.ZZZ.ZZZ.ZZ9.
080208     03  FILLER                  PIC  X(006)         VALUE SPACES.
080208     03  LD16-VLR-18              PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
080208
080208 01  LINDET17.
080208     03  FILLER                  PIC  X(001)         VALUE SPACES.
080208     03  FILLER                  PIC  X(015)         VALUE SPACES.
080208     03  FILLER                  PIC  X(038)         VALUE
080208         '19 - FLEXIBILIZAR EXCEDENTE '.
080208     03  LD17-QTD-19              PIC  Z.ZZZ.ZZZ.ZZ9.
080208     03  FILLER                  PIC  X(006)         VALUE SPACES.
080208     03  LD17-VLR-19              PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
080208
080208 01  LINDET18.
080208     03  FILLER                  PIC  X(001)         VALUE SPACES.
080208     03  FILLER                  PIC  X(015)         VALUE SPACES.
080208     03  FILLER                  PIC  X(038)         VALUE
080208         'NN - OUTROS          '.
080208     03  LD18-QTD-NN              PIC  Z.ZZZ.ZZZ.ZZ9.
080208     03  FILLER                  PIC  X(006)         VALUE SPACES.
080208     03  LD18-VLR-NN              PIC  ZZ.ZZZ.ZZZ.ZZ9,99.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA LINHAS DE TOTAIS ***'.
      *----------------------------------------------------------------*

       01  LINTOT1.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE SPACES.
           03  FILLER                  PIC  X(007)         VALUE
               'TOTAL'.
           03  FILLER                  PIC  X(009)         VALUE
               'CLIENTE: '.
           03  LT1-TOT-CLIE-1          PIC  999999999.
           03  FILLER                  PIC  X(001)         VALUE '\'.
           03  LT1-TOT-CLIE-2          PIC  99999.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  LT1-TOT-CLIE-3          PIC  99.

       01  LINTOT2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(027)         VALUE SPACES.
           03  FILLER                  PIC  X(009)         VALUE
               'TARIFA:'.
           03  LT2-TOT-TARIFA          PIC  99999.

       01  LINTOT3.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(027)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE 'DT:'.
           03  LT3-DATA                PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(011)         VALUE SPACES.
           03  LT3-QTD-TOT             PIC  ZZZ.ZZZ.ZZZ.ZZ9.
           03  FILLER                  PIC  X(005)         VALUE SPACES.
           03  LT3-VLR-TOT             PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.

       01  LINTOT4.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE SPACES.
           03  FILLER                  PIC  X(030)         VALUE
               'TOTAL GERAL:'.
           03  LT4-QTD-TOT-GERAL       PIC  ZZZZ.ZZZ.ZZZ.ZZ9.
           03  FILLER                  PIC  X(004)         VALUE SPACES.
           03  LT4-VLR-TOT-GERAL       PIC  ZZZZ.ZZZ.ZZZ.ZZ9,99.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA COMUNICAO COM O MODULO GFCT5525 ***'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT0M'.

       01  WRK-5525-AREA-ENTRADA.
           03  WRK-5525-AMBIENTE       PIC  X(001)         VALUE SPACES.
           03  WRK-5525-CHAMADOR       PIC  X(008)         VALUE SPACES.
           03  WRK-5525-DENVIO-MOVTO   PIC  X(010)         VALUE SPACES.
           03  WRK-5525-CROTNA-ORIGE   PIC  X(004)         VALUE SPACES.
           03  WRK-5525-CNRO-ARQ       PIC  9(002)         VALUE ZEROS.
           03  WRK-5525-CSEQ-MOVTO     PIC  9(011)         VALUE ZEROS.
           03  WRK-5525-CSIT-EVNTO     PIC  9(002)         VALUE ZEROS.
           03  WRK-5525-CORRENTISTA    PIC  X(001)         VALUE SPACES.

       01  WRK-5525-AREA-SAIDA.
           03  WRK-5525-COD-RETORNO    PIC  9(002)         VALUE ZEROS.
           03  WRK-5525-COD-SQL-ERRO   PIC S9(003)         VALUE ZEROS.
           03  WRK-5525-DESC-MSG       PIC  X(070)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS DE NULO ***'.
      *----------------------------------------------------------------*

       01  WRK-GFCTB000-NULL.
           03 WRK-CUSUAR-SENHA-NULL          PIC S9(04) COMP VALUE +0.
           03 WRK-HATULZ-NULL                PIC S9(04) COMP VALUE +0.
           03 WRK-WCHAVE-RESTT-NULL          PIC S9(04) COMP VALUE +0.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB000
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A4
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A7
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB092
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB094
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

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DECLARACAO DE CURSOR ***'.
      *----------------------------------------------------------------*

           EXEC SQL
            DECLARE CSR01-GFCTB0A7 CURSOR WITH HOLD FOR
             SELECT A.CAGPTO_CTA,
                    A.CSERVC_TARIF,
                    A.DOCOR_EVNTO,
                    A.HIDTFD_ESTRN_AGPTO,
                    A.CSIT_SOLTC_ESTRN,
                    A.CSIT_PROCM_ESTRN,
                    A.CINDCD_AGPTO_TOT,
                    A.CDEPDC_SOLTC_ESTRN,
                    A.CFUNC_SOLTC_ESTRN,
                    A.HSOLTC_ESTRN,
                    A.CTPO_OPER_MOTVO,
                    A.CMOTVO_JUSTF,
                    A.RJUSTF_SOLTC_ESTRN,
                    A.CDEPDC_PAREC_ESTRN,
                    A.CFUNC_PAREC_ESTRN,
                    A.HPAREC_ESTRN,
                    A.RJUSTF_PAREC_ESTRN,
                    A.QESTRN_AGPTO,
                    A.VESTRN_SOLTD,
                    A.QCANCT_AGPTO,
                    A.VCANCT_SOLTD,
                    A.DEFETV_CREDT_ESTRN,
                    B.CAGPTO_CTA,
                    B.CSERVC_TARIF,
                    B.DOCOR_EVNTO,
                    B.HIDTFD_ESTRN_AGPTO,
                    B.CCGC_CPF,
                    B.CFLIAL_CGC,
                    B.CCTRL_CPF_CGC,
ST25X6              B.CCGC_CPF_ST,
ST25X6              B.CFLIAL_CGC_ST,
ST25X6              B.CCTRL_CPF_CGC_ST
               FROM DB2PRD.PDIDO_ESTRN_GRP A,
                    DB2PRD.PDIDO_ESTRN_CLI B
              WHERE A.CAGPTO_CTA         = B.CAGPTO_CTA
                AND A.CSERVC_TARIF       = B.CSERVC_TARIF
                AND A.DOCOR_EVNTO        = B.DOCOR_EVNTO
                AND A.HIDTFD_ESTRN_AGPTO = B.HIDTFD_ESTRN_AGPTO
                AND A.CAGPTO_CTA         IN (4)
                AND A.CSIT_SOLTC_ESTRN   IN (2)
                AND A.CSIT_PROCM_ESTRN   IN (3)
              ORDER BY B.CAGPTO_CTA,
                       B.CSERVC_TARIF,
                       B.DOCOR_EVNTO,
ST25X6                 B.CCGC_CPF,
ST25X6                 B.CFLIAL_CGC,
ST25X6                 B.CCTRL_CPF_CGC,
ST25X6                 B.CCGC_CPF_ST,
ST25X6                 B.CFLIAL_CGC_ST,
ST25X6                 B.CCTRL_CPF_CGC_ST
           END-EXEC.

           EXEC SQL
            DECLARE CSR02-CLIEV004 CURSOR WITH HOLD FOR
             SELECT CID_CLI
               FROM DB2PRD.V01CLIENTE_AGENCIA
              WHERE CCGC_CPF           = :CLIEV004.CCGC-CPF
                AND CFLIAL_CGC         = :CLIEV004.CFLIAL-CGC
                AND CCTRL_CPF_CGC      = :CLIEV004.CCTRL-CPF-CGC
           END-EXEC.

           EXEC SQL
            DECLARE CSR03-CLIEV007 CURSOR WITH HOLD FOR
             SELECT CJUNC_DEPDC,
                    CCTA_CLI
               FROM DB2PRD.V01CTA_POUPANCA
              WHERE CID_CLI            = :CLIEV007.CID-CLI
           END-EXEC.

           EXEC SQL
            DECLARE CSR04-CLIEV008 CURSOR WITH HOLD FOR
             SELECT CJUNC_DEPDC,
                    CCTA_CLI
               FROM DB2PRD.V01CTA_CORRENTE
              WHERE CID_CLI            = :CLIEV008.CID-CLI
           END-EXEC.

           EXEC SQL
            DECLARE CSR05-GFCTB092 CURSOR WITH HOLD FOR
             SELECT DENVIO_MOVTO_TARIF,
                    CROTNA_ORIGE_MOVTO,
                    CNRO_ARQ_MOVTO,
                    CSEQ_MOVTO,
                    CSERVC_TARIF,
                    CMIDIA_SERVC_MOVTO,
                    CPERIF_SERVC_MOVTO,
                    QEVNTO_AGRUP,
                    HPREST_SERVC_MOVTO,
                    CTPO_CTA_MOVTO,
                    CBCO_DSTNO_MOVTO,
                    CAG_DSTNO_MOVTO,
                    CCTA_DSTNO_MOVTO,
                    CPAB_DSTNO_MOVTO,
                    CAG_CTLZA_DEB,
                    CCTA_CTLZA_DEB,
                    VTARIF_BRUTO_MOVTO,
                    VTARIF_LIQ_MOVTO,
                    VTARIF_DEB_MOVTO,
                    DAGNDA_DEB_MOVTO,
                    DEFETV_DEB_MOVTO,
                    QMAX_TENTV_DEB,
                    QTENTV_DEB_MOVTO,
                    CSIT_EVNTO_RECBD,
                    CIDTFD_PGMC_COBR,
                    DOCOR_EVNTO,
                    VTARIF_CADTR_SIST
               FROM DB2PRD.MOVTO_EVNTO_CRRTT
              WHERE CSERVC_TARIF       = :GFCTB092.CSERVC-TARIF
                AND DOCOR_EVNTO        = :GFCTB092.DOCOR-EVNTO
                AND CAG_DSTNO_MOVTO    = :GFCTB092.CAG-DSTNO-MOVTO
                AND CCTA_DSTNO_MOVTO   = :GFCTB092.CCTA-DSTNO-MOVTO
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA POOL7100 ***'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT1515 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LKG-AREA-PARM.
           05  LKG-TAMA-PARM           PIC S9(004) COMP.
           05  LKG-QTDE-COMMIT         PIC  9(007).

      *================================================================*
       PROCEDURE                       DIVISION USING LKG-AREA-PARM.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           IF  LKG-QTDE-COMMIT         NOT NUMERIC OR
               LKG-QTDE-COMMIT         EQUAL ZEROS
               DISPLAY '*************** GFCT1515 ****************'
               DISPLAY '*                                       *'
               DISPLAY '*    QUANTIDADE DE OCORRENCIAS ENVIADO  *'
               DISPLAY '*    PELO PARM, PARA COMMIT, INVALIDO.  *'
               DISPLAY '*                                       *'
               DISPLAY '*    QUANTIDADE: ' WRK-QTDE-COMMIT
                                               '                *'
               DISPLAY '*                                       *'
               DISPLAY '*          PROGRAMA CANCELADO!          *'
               DISPLAY '*                                       *'
               DISPLAY '*************** GFCT1515 ****************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG01          TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE LKG-QTDE-COMMIT        TO WRK-QTDE-COMMIT.

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-VERIFICAR-VAZIO.

           PERFORM 300000-PROCESSAR    UNTIL
                   WRK-FIM-C1          EQUAL 'S'.

           PERFORM 400000-FECHAR-CURSOR1.

           PERFORM 500000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT  ENTCLIE
                OUTPUT SAICLIE
                       RELATO.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

           PERFORM 120000-ACESSAR-GFCTB0A1.

           PERFORM 130000-TRATAR-RESTART.

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           MOVE WRK-DIA                TO CB1-DIA
                                          CB5-DIA-MOV.
           MOVE WRK-MES                TO CB1-MES
                                          CB5-MES-MOV.
           MOVE WRK-ANO                TO CB1-ANO
                                          CB5-ANO-MOV.
           MOVE WRK-HOR                TO CB1-HOR.
           MOVE WRK-MIN                TO CB1-MIN.
           MOVE WRK-SEG                TO CB1-SEG.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 111000-TESTAR-FS-ENTCLIE.

           PERFORM 112000-TESTAR-FS-SAICLIE.

           PERFORM 113000-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       111000-TESTAR-FS-ENTCLIE        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENTCLIE          NOT EQUAL '00' AND '10'
               MOVE 'ENTCLIE '         TO WRK-NOME-ARQ
               MOVE WRK-FS-ENTCLIE     TO WRK-FILE-STATUS
               MOVE WRK-MENSAGEM-ERRO  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       112000-TESTAR-FS-SAICLIE        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAICLIE          NOT EQUAL '00'
               MOVE 'SAICLIE '         TO WRK-NOME-ARQ
               MOVE WRK-FS-SAICLIE     TO WRK-FILE-STATUS
               MOVE WRK-MENSAGEM-ERRO  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       113000-TESTAR-FS-RELATO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELATO           NOT EQUAL '00'
               MOVE 'RELATO  '         TO WRK-NOME-ARQ
               MOVE WRK-FS-RELATO      TO WRK-FILE-STATUS
               MOVE WRK-MENSAGEM-ERRO  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       113000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       120000-ACESSAR-GFCTB0A1         SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
             SELECT CSIST_PRINC,
                    DPROCM_ANTER,
                    DPROCM_ATUAL,
                    DPROCM_PROX
               INTO :GFCTB0A1.CSIST-PRINC,
                    :GFCTB0A1.DPROCM-ANTER,
                    :GFCTB0A1.DPROCM-ATUAL,
                    :GFCTB0A1.DPROCM-PROX
               FROM DB2PRD.PARM_DATA_PROCM
              WHERE CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
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
       120000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       130000-TRATAR-RESTART           SECTION.
      *----------------------------------------------------------------*

           PERFORM 131000-LER-ENTCLIE.

           PERFORM 132000-ACESSAR-GFCTB000.

           IF  WRK-RESTART                 EQUAL 'S'
               PERFORM 133000-GRAVAR-SAICLIE VARYING
                   IND-1 FROM 1 BY 1       UNTIL
                   IND-1                   GREATER WRK-QUANTIDADE  OR
                   WRK-FS-ENTCLIE          EQUAL '10'
               IF  WRK-FS-ENTCLIE          EQUAL '10' AND
                   IND-1                   NOT GREATER WRK-QUANTIDADE
                   DISPLAY '*************** GFCT1515 ****************'
                   DISPLAY '*                                       *'
                   DISPLAY '*    QUANTIDADE DE OCORRENCIAS DE       *'
                   DISPLAY '*    RESTART MAIOR QUE O NUMERO DE      *'
                   DISPLAY '*    REGISTROS A SER REGRAVADO.         *'
                   DISPLAY '*                                       *'
                   DISPLAY '*         PROGRAMA CANCELADO!           *'
                   DISPLAY '*                                       *'
                   DISPLAY '*************** GFCT1515 ****************'
                   MOVE 'APL'              TO ERR-TIPO-ACESSO
                   MOVE WRK-MSG04          TO ERR-TEXTO
                   PERFORM 999999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       130000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       131000-LER-ENTCLIE              SECTION.
      *----------------------------------------------------------------*

           READ ENTCLIE.

           IF  WRK-FS-ENTCLIE          EQUAL '10'
               GO TO 131000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 111000-TESTAR-FS-ENTCLIE.

      *----------------------------------------------------------------*
       131000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       132000-ACESSAR-GFCTB000         SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL7300'             USING WRK-MCB.

           MOVE 'GFCT1515'             TO CPROG            OF GFCTB000.
           MOVE WRK-NOME-JOB           TO CJOB             OF GFCTB000.
           MOVE ZEROS                  TO WCHAVE-RESTT-LEN OF GFCTB000.
           MOVE SPACES                 TO WCHAVE-RESTT-TEXT
                                                           OF GFCTB000.

           EXEC SQL
             SELECT WCHAVE_RESTT
               INTO :GFCTB000.WCHAVE-RESTT
                     :WRK-WCHAVE-RESTT-NULL
               FROM DB2PRD.CTRL_ROTNA_GFCT
              WHERE CPROG              = :GFCTB000.CPROG
                AND CJOB               = :GFCTB000.CJOB
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CTRL_ROTNA_GFCT ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               IF  WRK-WCHAVE-RESTT-NULL NOT EQUAL ZEROS
                   MOVE ZEROS          TO WCHAVE-RESTT-LEN  OF GFCTB000
                   MOVE SPACES         TO WCHAVE-RESTT-TEXT OF GFCTB000
               END-IF
               MOVE WCHAVE-RESTT-TEXT  OF GFCTB000 TO WRK-CHAVE-RESTART
               MOVE 'S'                TO WRK-RESTART
           END-IF.

      *----------------------------------------------------------------*
       132000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       133000-GRAVAR-SAICLIE           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRS-STATUS          OF ENTCLIE TO CSIT-EVNTO-RECBD
                                       OF GFCTB092.
           MOVE GFCTRS-VALOR           OF ENTCLIE TO VTARIF-DEB-MOVTO
                                          OF GFCTB092
                                          VTARIF-BRUTO-MOVTO
                                          OF GFCTB092.

           PERFORM 133100-ACUMULAR-RELATORIO.

           WRITE GFCTRS-ENTRADA        OF SAICLIE
                                       FROM GFCTRS-ENTRADA OF ENTCLIE.

           PERFORM 112000-TESTAR-FS-SAICLIE.

           PERFORM 131000-LER-ENTCLIE.

      *----------------------------------------------------------------*
       133000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       133100-ACUMULAR-RELATORIO       SECTION.
      *----------------------------------------------------------------*

           EVALUATE CSIT-EVNTO-RECBD OF GFCTB092

               WHEN 1
                    ADD 1                               TO ACU-SIT-1
                                                           ACU-SIT-TOT
                    ADD VTARIF-BRUTO-MOVTO OF GFCTB092  TO ACU-VLR-1
                                                           ACU-VLR-TOT
               WHEN 2
                    ADD 1                               TO ACU-SIT-2
                                                           ACU-SIT-TOT
                    ADD VTARIF-BRUTO-MOVTO OF GFCTB092  TO ACU-VLR-2
                                                           ACU-VLR-TOT
               WHEN 3
                    ADD 1                               TO ACU-SIT-3
                                                           ACU-SIT-TOT
                    ADD VTARIF-BRUTO-MOVTO OF GFCTB092  TO ACU-VLR-3
                                                           ACU-VLR-TOT
               WHEN 4
                    ADD 1                             TO ACU-SIT-4
                                                         ACU-SIT-TOT
                    ADD VTARIF-DEB-MOVTO OF GFCTB092  TO ACU-VLR-4
                                                         ACU-VLR-TOT
               WHEN 5
                    ADD 1                             TO ACU-SIT-5
                                                         ACU-SIT-TOT
                    ADD VTARIF-DEB-MOVTO OF GFCTB092  TO ACU-VLR-5
                                                         ACU-VLR-TOT
               WHEN 6
                    ADD 1                             TO ACU-SIT-6
                                                         ACU-SIT-TOT
                    ADD VTARIF-DEB-MOVTO OF GFCTB092  TO ACU-VLR-6
                                                         ACU-VLR-TOT
               WHEN 9
                    ADD 1                               TO ACU-SIT-9
                                                           ACU-SIT-TOT
                    ADD VTARIF-BRUTO-MOVTO OF GFCTB092  TO ACU-VLR-9
                                                           ACU-VLR-TOT

               WHEN 10
                    ADD 1                               TO ACU-SIT-10
                                                           ACU-SIT-TOT
                    ADD VTARIF-BRUTO-MOVTO OF GFCTB092  TO ACU-VLR-10
                                                           ACU-VLR-TOT

               WHEN 11
                    ADD 1                               TO ACU-SIT-11
                                                           ACU-SIT-TOT
                    ADD VTARIF-BRUTO-MOVTO OF GFCTB092  TO ACU-VLR-11
                                                           ACU-VLR-TOT

               WHEN 12
                    ADD 1                               TO ACU-SIT-12
                                                           ACU-SIT-TOT
                    ADD VTARIF-BRUTO-MOVTO OF GFCTB092  TO ACU-VLR-12
                                                           ACU-VLR-TOT

               WHEN 13
                    ADD 1                               TO ACU-SIT-13
                                                           ACU-SIT-TOT
                    ADD VTARIF-BRUTO-MOVTO OF GFCTB092  TO ACU-VLR-13
                                                           ACU-VLR-TOT

               WHEN 14
                    ADD 1                               TO ACU-SIT-14
                                                           ACU-SIT-TOT
                    ADD VTARIF-BRUTO-MOVTO OF GFCTB092  TO ACU-VLR-14
                                                           ACU-VLR-TOT

               WHEN 15
                    ADD 1                               TO ACU-SIT-15
                                                           ACU-SIT-TOT
                    ADD VTARIF-BRUTO-MOVTO OF GFCTB092  TO ACU-VLR-15
                                                           ACU-VLR-TOT

               WHEN 16
                    ADD 1                               TO ACU-SIT-16
                                                           ACU-SIT-TOT
                    ADD VTARIF-BRUTO-MOVTO OF GFCTB092  TO ACU-VLR-16
                                                           ACU-VLR-TOT

               WHEN 17
                    ADD 1                               TO ACU-SIT-17
                                                           ACU-SIT-TOT
                    ADD VTARIF-BRUTO-MOVTO OF GFCTB092  TO ACU-VLR-17
                                                           ACU-VLR-TOT

080208         WHEN 18
080208              ADD 1                               TO ACU-SIT-18
080208                                                     ACU-SIT-TOT
080208              ADD VTARIF-BRUTO-MOVTO OF GFCTB092  TO ACU-VLR-18
080208                                                     ACU-VLR-TOT
080208
080208         WHEN 19
080208              ADD 1                               TO ACU-SIT-19
080208                                                     ACU-SIT-TOT
080208              ADD VTARIF-BRUTO-MOVTO OF GFCTB092  TO ACU-VLR-19
080208                                                     ACU-VLR-TOT
080208
080208         WHEN OTHER
080208              ADD 1                               TO ACU-SIT-NN
080208                                                     ACU-SIT-TOT
080208              ADD VTARIF-BRUTO-MOVTO OF GFCTB092  TO ACU-VLR-NN
080208                                                     ACU-VLR-TOT

           END-EVALUATE.

      *----------------------------------------------------------------*
       133100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-ABRIR-CURSOR1.

           PERFORM 220000-LER-CURSOR1.

           IF  WRK-FIM-C1              EQUAL 'S'
               DISPLAY '************** GFCT1515 **************'
               DISPLAY '*                                    *'
               DISPLAY '*        NAO HA ESTORNOS POR         *'
               DISPLAY '*      AGRUPAMENTO CLIENTE PARA      *'
               DISPLAY '*       MOVIMENTO: '
                           DPROCM-ATUAL OF GFCTB0A1 '        *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT1515 **************'
           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-ABRIR-CURSOR1            SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-C1.

           EXEC SQL
               OPEN CSR01-GFCTB0A7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_GRP ' TO ERR-DBD-TAB
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       220000-LER-CURSOR1              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH CSR01-GFCTB0A7
               INTO
                   :GFCTB0A7.CAGPTO-CTA,
                   :GFCTB0A7.CSERVC-TARIF,
                   :GFCTB0A7.DOCOR-EVNTO,
                   :GFCTB0A7.HIDTFD-ESTRN-AGPTO,
                   :GFCTB0A7.CSIT-SOLTC-ESTRN,
                   :GFCTB0A7.CSIT-PROCM-ESTRN,
                   :GFCTB0A7.CINDCD-AGPTO-TOT,
                   :GFCTB0A7.CDEPDC-SOLTC-ESTRN,
                   :GFCTB0A7.CFUNC-SOLTC-ESTRN,
                   :GFCTB0A7.HSOLTC-ESTRN,
                   :GFCTB0A7.CTPO-OPER-MOTVO,
                   :GFCTB0A7.CMOTVO-JUSTF,
                   :GFCTB0A7.RJUSTF-SOLTC-ESTRN,
                   :GFCTB0A7.CDEPDC-PAREC-ESTRN,
                   :GFCTB0A7.CFUNC-PAREC-ESTRN,
                   :GFCTB0A7.HPAREC-ESTRN,
                   :GFCTB0A7.RJUSTF-PAREC-ESTRN,
                   :GFCTB0A7.QESTRN-AGPTO,
                   :GFCTB0A7.VESTRN-SOLTD,
                   :GFCTB0A7.QCANCT-AGPTO,
                   :GFCTB0A7.VCANCT-SOLTD,
                   :GFCTB0A7.DEFETV-CREDT-ESTRN,
                   :GFCTB0A4.CAGPTO-CTA,
                   :GFCTB0A4.CSERVC-TARIF,
                   :GFCTB0A4.DOCOR-EVNTO,
                   :GFCTB0A4.HIDTFD-ESTRN-AGPTO,
                   :GFCTB0A4.CCGC-CPF,
                   :GFCTB0A4.CFLIAL-CGC,
                   :GFCTB0A4.CCTRL-CPF-CGC,
ST25X6             :GFCTB0A4.CCGC-CPF-ST,
ST25X6             :GFCTB0A4.CFLIAL-CGC-ST,
ST25X6             :GFCTB0A4.CCTRL-CPF-CGC-ST
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_GRP ' TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0040'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-C1
           END-IF.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 310000-ABRIR-CURSOR2.

           PERFORM 320000-LER-CURSOR2.

           PERFORM 330000-PROCESSAR-CURSOR2 UNTIL
               WRK-FIM-C2                   EQUAL 'S'.

           PERFORM 340000-FECHAR-CURSOR2.

           PERFORM 350000-EFETUAR-UPDATE.

           PERFORM 360000-IMPRIMIR-RELATORIO.

           ADD 1                       TO ACU-OCORRENCIAS.

           IF  ACU-OCORRENCIAS         EQUAL WRK-QTDE-COMMIT
               PERFORM 370000-VERIFICAR-RESTART
               MOVE ZEROS              TO ACU-OCORRENCIAS
           END-IF.

           PERFORM 220000-LER-CURSOR1.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       310000-ABRIR-CURSOR2            SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                       TO WRK-FIM-C2.
ST25X6*    MOVE CCGC-CPF      OF GFCTB0A4 TO CCGC-CPF      OF CLIEV004.
ST25X6*    MOVE CFLIAL-CGC    OF GFCTB0A4 TO CFLIAL-CGC    OF CLIEV004.
ST25X6*    MOVE CCTRL-CPF-CGC OF GFCTB0A4 TO CCTRL-CPF-CGC OF CLIEV004.
ST25X6     MOVE CCGC-CPF-ST   OF GFCTB0A4 TO CCGC-CPF   OF CLIEV004.
ST25X6     MOVE CFLIAL-CGC-ST OF GFCTB0A4 TO CFLIAL-CGC OF CLIEV004.
ST25X6     MOVE CCTRL-CPF-CGC-ST OF GFCTB0A4 
                                           TO CCTRL-CPF-CGC 
                                           OF CLIEV004.

           EXEC SQL
               OPEN CSR02-CLIEV004
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CLIENTE_AGENCIA'  TO ERR-DBD-TAB
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0050'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       320000-LER-CURSOR2              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH CSR02-CLIEV004
               INTO :CLIEV004.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CLIENTE_AGENCIA'  TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0060'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-C2
           END-IF.

      *----------------------------------------------------------------*
       320000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       330000-PROCESSAR-CURSOR2        SECTION.
      *----------------------------------------------------------------*

           PERFORM 331000-PROCESSAR-CLIEV007.
           PERFORM 332000-PROCESSAR-CLIEV008.

           PERFORM 320000-LER-CURSOR2.

      *----------------------------------------------------------------*
       330000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331000-PROCESSAR-CLIEV007       SECTION.
      *----------------------------------------------------------------*

           PERFORM 331100-ABRIR-CURSOR3.

           PERFORM 331200-LER-CURSOR3.

           PERFORM 331300-PROCESSAR-CURSOR3 UNTIL
               WRK-FIM-C3                   EQUAL 'S'.

           PERFORM 331400-FECHAR-CURSOR3.

      *----------------------------------------------------------------*
       331000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331100-ABRIR-CURSOR3            SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-C3.
           MOVE CID-CLI OF CLIEV004    TO CID-CLI OF CLIEV007.

           EXEC SQL
               OPEN CSR03-CLIEV007
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_POUPANCA ' TO ERR-DBD-TAB
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0070'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       331100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331200-LER-CURSOR3              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH CSR03-CLIEV007
               INTO :CLIEV007.CJUNC-DEPDC,
                    :CLIEV007.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_POUPANCA ' TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0080'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-C3
           END-IF.

      *----------------------------------------------------------------*
       331200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331300-PROCESSAR-CURSOR3        SECTION.
      *----------------------------------------------------------------*

           MOVE CJUNC-DEPDC OF CLIEV007 TO CAG-DSTNO-MOVTO  OF GFCTB092.
           MOVE CCTA-CLI    OF CLIEV007 TO CCTA-DSTNO-MOVTO OF GFCTB092.

           PERFORM 331310-ABRIR-CURSOR5.

           PERFORM 331320-LER-CURSOR5.

           PERFORM 331330-PROCESSAR-CURSOR5  UNTIL
               WRK-FIM-C5                    EQUAL 'S'.

           PERFORM 331340-FECHAR-CURSOR5.

           PERFORM 331200-LER-CURSOR3.

      *----------------------------------------------------------------*
       331300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331310-ABRIR-CURSOR5            SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-C5.
           MOVE CSERVC-TARIF           OF GFCTB0A7
                                       TO CSERVC-TARIF OF GFCTB092.
           MOVE DOCOR-EVNTO            OF GFCTB0A7
                                       TO DOCOR-EVNTO  OF GFCTB092.

           EXEC SQL
               OPEN CSR05-GFCTB092
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'MOVTO_EVNTO_CRRTT' TO ERR-DBD-TAB
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0090'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       331310-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331320-LER-CURSOR5              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH CSR05-GFCTB092
               INTO :GFCTB092.DENVIO-MOVTO-TARIF,
                    :GFCTB092.CROTNA-ORIGE-MOVTO,
                    :GFCTB092.CNRO-ARQ-MOVTO,
                    :GFCTB092.CSEQ-MOVTO,
                    :GFCTB092.CSERVC-TARIF,
                    :GFCTB092.CMIDIA-SERVC-MOVTO,
                    :GFCTB092.CPERIF-SERVC-MOVTO,
                    :GFCTB092.QEVNTO-AGRUP,
                    :GFCTB092.HPREST-SERVC-MOVTO,
                    :GFCTB092.CTPO-CTA-MOVTO,
                    :GFCTB092.CBCO-DSTNO-MOVTO,
                    :GFCTB092.CAG-DSTNO-MOVTO,
                    :GFCTB092.CCTA-DSTNO-MOVTO,
                    :GFCTB092.CPAB-DSTNO-MOVTO,
                    :GFCTB092.CAG-CTLZA-DEB,
                    :GFCTB092.CCTA-CTLZA-DEB,
                    :GFCTB092.VTARIF-BRUTO-MOVTO,
                    :GFCTB092.VTARIF-LIQ-MOVTO,
                    :GFCTB092.VTARIF-DEB-MOVTO,
                    :GFCTB092.DAGNDA-DEB-MOVTO,
                    :GFCTB092.DEFETV-DEB-MOVTO,
                    :GFCTB092.QMAX-TENTV-DEB,
                    :GFCTB092.QTENTV-DEB-MOVTO,
                    :GFCTB092.CSIT-EVNTO-RECBD,
                    :GFCTB092.CIDTFD-PGMC-COBR,
                    :GFCTB092.DOCOR-EVNTO,
                    :GFCTB092.VTARIF-CADTR-SIST
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'MOVTO_EVNTO_CRRTT'   TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0100'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-C5
           END-IF.

      *----------------------------------------------------------------*
       331320-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331330-PROCESSAR-CURSOR5        SECTION.
      *----------------------------------------------------------------*

080208     IF  CSIT-EVNTO-RECBD        OF GFCTB092
080208                                 EQUAL 7 OR 8
080208         NEXT SENTENCE
080208     ELSE
080208       IF  CSIT-EVNTO-RECBD    OF GFCTB092
080208                             EQUAL 13
080208           PERFORM 133100-ACUMULAR-RELATORIO
080208       ELSE
080208         IF  CSIT-EVNTO-RECBD     OF GFCTB092
080208                                  EQUAL 4 OR 5 OR 6 OR 10
080208             PERFORM 331332-MONTAR-SAIDA
080208             PERFORM 133100-ACUMULAR-RELATORIO
080208         ELSE
080208             MOVE 'S'            TO WRK-5525-CORRENTISTA
080208             MOVE DENVIO-MOVTO-TARIF OF GFCTB092
080208                                     TO WRK-5525-DENVIO-MOVTO
080208             MOVE CROTNA-ORIGE-MOVTO OF GFCTB092
080208                                     TO WRK-5525-CROTNA-ORIGE
080208             MOVE CNRO-ARQ-MOVTO OF GFCTB092
080208                                     TO WRK-5525-CNRO-ARQ
080208             MOVE CSEQ-MOVTO     OF GFCTB092
080208                                     TO WRK-5525-CSEQ-MOVTO
080208             PERFORM 331331-GERAR-HISTORICO
080208             PERFORM 133100-ACUMULAR-RELATORIO
080208         END-IF
080208       END-IF
080208     END-IF.

           PERFORM 331320-LER-CURSOR5.

      *----------------------------------------------------------------*
       331330-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331331-GERAR-HISTORICO          SECTION.
      *----------------------------------------------------------------*

           MOVE 'B'                        TO WRK-5525-AMBIENTE.
           MOVE 'GFCT1515'                 TO WRK-5525-CHAMADOR.
           MOVE  8                         TO WRK-5525-CSIT-EVNTO.
           MOVE 'GFCT5525'                 TO WRK-MODULO.

           CALL WRK-MODULO                 USING WRK-5525-AREA-ENTRADA
                                                 WRK-5525-AREA-SAIDA
                                                 GFCT0M-AREA-ERROS.

           IF  WRK-5525-COD-RETORNO        NOT EQUAL ZEROS
               IF  WRK-5525-COD-RETORNO    EQUAL 99
                   MOVE WRK-5525-COD-SQL-ERRO
                                           TO WRK-MASCARA-SQL
                   DISPLAY '******************************** GFCT1515 **
      -                    '******************************'
                   DISPLAY '*
      -                    '                             *'
                   DISPLAY '*                          ERRO NO MODULO GF
      -                    'CT5525                       *'
                   DISPLAY '*
      -                    '                             *'
                   DISPLAY '* SQLCODE: ' WRK-MASCARA-SQL '
      -                    '                                        *'
                   DISPLAY '*
      -                    '                             *'
                   DISPLAY '* ' WRK-5525-DESC-MSG ' *'
                   DISPLAY '*
      -                    '                             *'
                   DISPLAY '******************************** GFCT1515 **
      -                    '******************************'
                   MOVE GFCT0M-ERRO-AREAS  TO ERRO-AREA
                   MOVE GFCT0M-SQLCA-AREA  TO SQLCA
               ELSE
                   MOVE 'APL'              TO ERR-TIPO-ACESSO
                   MOVE WRK-5525-DESC-MSG  TO ERR-TEXTO
               END-IF
               MOVE 'GFCT5525'             TO ERR-MODULO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                           TO ACU-QTD-CANCELAMENTO.
           ADD VTARIF-BRUTO-MOVTO            OF GFCTB092
                                           TO ACU-VLR-CANCELAMENTO.

      *----------------------------------------------------------------*
       331331-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331332-MONTAR-SAIDA             SECTION.
      *----------------------------------------------------------------*

           PERFORM 331339-OBTEM-DADOS-ORIGEM.

           MOVE SPACES                 TO GFCTRS-ENTRADA   OF SAICLIE.

           INITIALIZE GFCTRS-ENTRADA   OF SAICLIE.

           MOVE DENVIO-MOVTO-TARIF     OF GFCTB092
                                       TO GFCTRS-DT-ENVIO-MOVTO
                                       OF SAICLIE.
           MOVE CROTNA-ORIGE-MOVTO     OF GFCTB092
                                       TO GFCTRS-CD-ROTINA-ORIGEM
                                       OF SAICLIE.
           MOVE CNRO-ARQ-MOVTO         OF GFCTB092
                                       TO GFCTRS-CD-NUMERO-ARQUIVO
                                       OF SAICLIE.
           MOVE CSEQ-MOVTO             OF GFCTB092
                                       TO GFCTRS-CD-SEQ-MOVTO
                                       OF SAICLIE.
           MOVE 2                      TO GFCTRS-CD-ORIGEM-PEDIDO
                                       OF SAICLIE.
           MOVE 1                      TO GFCTRS-CD-TIPO-SOLTC-ESTRN
                                       OF SAICLIE.
           MOVE CSERVC-TARIF           OF GFCTB092
                                       TO GFCTRS-TARIFA OF SAICLIE.
           MOVE VTARIF-DEB-MOVTO       OF GFCTB092
                                       TO GFCTRS-VALOR  OF SAICLIE.
           MOVE CBCO-DSTNO-MOVTO       OF GFCTB092
                                       TO GFCTRS-BANCO  OF SAICLIE.
           MOVE CAG-DSTNO-MOVTO        OF GFCTB092
                                       TO GFCTRS-AGENCIA OF SAICLIE.
           MOVE CCTA-DSTNO-MOVTO       OF GFCTB092
                                       TO GFCTRS-CONTA   OF SAICLIE.
           MOVE CBCO-ORIGE-MOVTO       OF GFCTB094
                                       TO GFCTRS-BANCO-ORIGEM
                                       OF SAICLIE.
           MOVE CAG-ORIGE-MOVTO        OF GFCTB094
                                       TO GFCTRS-AGENCIA-ORIGEM
                                       OF SAICLIE.
           MOVE DOCOR-EVNTO            OF GFCTB092
                                       TO GFCTRS-DATA-EVENTO OF SAICLIE.
           MOVE CSIT-EVNTO-RECBD       OF GFCTB092
                                       TO GFCTRS-STATUS OF SAICLIE.
           MOVE CTPO-OPER-MOTVO        OF GFCTB0A7
                                       TO GFCTRS-CTPO-OPER-MOTVO
                                       OF SAICLIE.
           MOVE CMOTVO-JUSTF           OF GFCTB0A7
                                       TO GFCTRS-CMOTVO-JUSTF
                                       OF SAICLIE.
           MOVE 4                      TO GFCTRS-CAGPTO OF SAICLIE.
           MOVE DEFETV-DEB-MOVTO       OF GFCTB092
                                       TO GFCTRS-DATA-DEBITO OF SAICLIE.

           WRITE GFCTRS-ENTRADA        OF SAICLIE.

           PERFORM 112000-TESTAR-FS-SAICLIE.

           ADD 1                       TO ACU-QTD-ESTORNO
                                          ACU-GRAVADOS.
           ADD VTARIF-DEB-MOVTO        OF GFCTB092 TO ACU-VLR-ESTORNO.

      *----------------------------------------------------------------*
       331332-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331339-OBTEM-DADOS-ORIGEM       SECTION.
      *----------------------------------------------------------------*

           MOVE DENVIO-MOVTO-TARIF     OF GFCTB092
                                       TO DENVIO-MOVTO-TARIF
                                       OF GFCTB094.
           MOVE CROTNA-ORIGE-MOVTO     OF GFCTB092
                                       TO CROTNA-ORIGE-MOVTO
                                       OF GFCTB094.
           MOVE CNRO-ARQ-MOVTO         OF GFCTB092
                                       TO CNRO-ARQ-MOVTO OF GFCTB094.
           MOVE CSEQ-MOVTO             OF GFCTB092
                                       TO CSEQ-MOVTO OF GFCTB094.

           EXEC SQL
             SELECT CTARIF_MOVTO,
                    CBCO_ORIGE_MOVTO,
                    CAG_ORIGE_MOVTO
               INTO :GFCTB094.CTARIF-MOVTO,
                    :GFCTB094.CBCO-ORIGE-MOVTO,
                    :GFCTB094.CAG-ORIGE-MOVTO
               FROM DB2PRD.MOVTO_ROTNA_ORIGE
              WHERE DENVIO_MOVTO_TARIF = :GFCTB094.DENVIO-MOVTO-TARIF
                AND CROTNA_ORIGE_MOVTO = :GFCTB094.CROTNA-ORIGE-MOVTO
                AND CNRO_ARQ_MOVTO     = :GFCTB094.CNRO-ARQ-MOVTO
                AND CSEQ_MOVTO         = :GFCTB094.CSEQ-MOVTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'MOVTO_ROTNA_ORIGE' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0110'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       331339-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331340-FECHAR-CURSOR5           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR05-GFCTB092
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'MOVTO_EVNTO_CRRTT'   TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0120'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       331340-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331400-FECHAR-CURSOR3           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE CSR03-CLIEV007
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_POUPANCA ' TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0130'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       331400-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       332000-PROCESSAR-CLIEV008       SECTION.
      *----------------------------------------------------------------*

           PERFORM 332100-ABRIR-CURSOR4.

           PERFORM 332200-LER-CURSOR4.

           PERFORM 332300-PROCESSAR-CURSOR4  UNTIL
               WRK-FIM-C4                    EQUAL 'S'.

           PERFORM 332400-FECHAR-CURSOR4.

      *----------------------------------------------------------------*
       332000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       332100-ABRIR-CURSOR4            SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-C4.
           MOVE CID-CLI  OF CLIEV004   TO CID-CLI OF CLIEV008.

           EXEC SQL
               OPEN CSR04-CLIEV008
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_CORRENTE ' TO ERR-DBD-TAB
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0140'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       332100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       332200-LER-CURSOR4              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH CSR04-CLIEV008
               INTO :CLIEV008.CJUNC-DEPDC,
                    :CLIEV008.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_CORRENTE ' TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0150'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-C4
           END-IF.

      *----------------------------------------------------------------*
       332200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       332300-PROCESSAR-CURSOR4        SECTION.
      *----------------------------------------------------------------*

           MOVE CJUNC-DEPDC OF CLIEV008 TO CAG-DSTNO-MOVTO  OF GFCTB092.
           MOVE CCTA-CLI    OF CLIEV008 TO CCTA-DSTNO-MOVTO OF GFCTB092.

           PERFORM 331310-ABRIR-CURSOR5.

           PERFORM 331320-LER-CURSOR5.

           PERFORM 331330-PROCESSAR-CURSOR5  UNTIL
               WRK-FIM-C5                    EQUAL 'S'.

           PERFORM 331340-FECHAR-CURSOR5.

           PERFORM 332200-LER-CURSOR4.

      *----------------------------------------------------------------*
       332300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       332400-FECHAR-CURSOR4           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE CSR04-CLIEV008
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_CORRENTE ' TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0160'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       332400-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       340000-FECHAR-CURSOR2           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE CSR02-CLIEV004
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CLIENTE_AGENCIA'  TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0170'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       340000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       350000-EFETUAR-UPDATE           SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO CSIT-PROCM-ESTRN OF GFCTB0A7.
           MOVE DPROCM-ATUAL           OF GFCTB0A1 TO DEFETV-CREDT-ESTRN
                                                           OF GFCTB0A7.
           MOVE ACU-QTD-ESTORNO        TO QESTRN-AGPTO     OF GFCTB0A7.
           MOVE ACU-VLR-ESTORNO        TO VESTRN-SOLTD     OF GFCTB0A7.
           MOVE ACU-QTD-CANCELAMENTO   TO QCANCT-AGPTO     OF GFCTB0A7.
           MOVE ACU-VLR-CANCELAMENTO   TO VCANCT-SOLTD     OF GFCTB0A7.

           EXEC SQL
             UPDATE  DB2PRD.PDIDO_ESTRN_GRP
                SET  QESTRN_AGPTO       = :GFCTB0A7.QESTRN-AGPTO,
                     DEFETV_CREDT_ESTRN = :GFCTB0A7.DEFETV-CREDT-ESTRN,
                     VESTRN_SOLTD       = :GFCTB0A7.VESTRN-SOLTD,
                     QCANCT_AGPTO       = :GFCTB0A7.QCANCT-AGPTO,
                     VCANCT_SOLTD       = :GFCTB0A7.VCANCT-SOLTD,
                     CSIT_PROCM_ESTRN   = :GFCTB0A7.CSIT-PROCM-ESTRN
              WHERE  CAGPTO_CTA         = :GFCTB0A7.CAGPTO-CTA
                AND  CSERVC_TARIF       = :GFCTB0A7.CSERVC-TARIF
                AND  DOCOR_EVNTO        = :GFCTB0A7.DOCOR-EVNTO
                AND  HIDTFD_ESTRN_AGPTO = :GFCTB0A7.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_GRP ' TO ERR-DBD-TAB
               MOVE 'UPDATE  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0180'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD  ACU-QTD-ESTORNO        TO ACU-QTD-ESTORNO-TOT.
           ADD  ACU-VLR-ESTORNO        TO ACU-VLR-ESTORNO-TOT.
           ADD  ACU-QTD-CANCELAMENTO   TO ACU-QTD-CANCELAMENTO-TOT.
           ADD  ACU-VLR-CANCELAMENTO   TO ACU-VLR-CANCELAMENTO-TOT.

           MOVE ZEROS                  TO ACU-QTD-ESTORNO
                                          ACU-VLR-ESTORNO
                                          ACU-QTD-CANCELAMENTO
                                          ACU-VLR-CANCELAMENTO.

      *----------------------------------------------------------------*
       350000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       360000-IMPRIMIR-RELATORIO       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           IF  ACU-LINHAS              GREATER 3
               PERFORM 361000-IMPRIMI-CABECALHO
           END-IF.

           PERFORM 362000-FORMATAR-CAMPOS-REL.

           WRITE REG-RELATO            FROM CABEC8.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM CABEC9.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM LINDET1.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM LINDET2.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM LINDET3.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM LINDET4.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM LINDET5.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM LINDET6.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM LINDET7.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM LINDET8.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM LINDET9.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM LINDET10.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM LINDET11.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM LINDET12.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM LINDET13.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM LINDET14.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM LINDET15.
           PERFORM 113000-TESTAR-FS-RELATO.

080208     WRITE REG-RELATO            FROM LINDET16.
080208     PERFORM 113000-TESTAR-FS-RELATO.
080208
080208     WRITE REG-RELATO            FROM LINDET17.
080208     PERFORM 113000-TESTAR-FS-RELATO.
080208
080208     WRITE REG-RELATO            FROM LINDET18.
080208     PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM LINTOT1.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM LINTOT2.
           PERFORM 113000-TESTAR-FS-RELATO.

           MOVE ACU-SIT-TOT            TO LT3-QTD-TOT.
           ADD  ACU-SIT-TOT            TO ACU-SIT-TOT-GERAL.
           MOVE ACU-VLR-TOT            TO LT3-VLR-TOT.
           ADD  ACU-VLR-TOT            TO ACU-VLR-TOT-GERAL.

           MOVE ZEROS                  TO ACU-SIT-TOT
                                          ACU-VLR-TOT.

           WRITE REG-RELATO            FROM LINTOT3.
           PERFORM 113000-TESTAR-FS-RELATO.

           ADD  1                       TO ACU-LINHAS.

      *----------------------------------------------------------------*
       360000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       361000-IMPRIMI-CABECALHO        SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAGINAS.

           MOVE ACU-PAGINAS            TO CB2-PAGINA.

           WRITE REG-RELATO            FROM CABEC1.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM CABEC2.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM CABEC3.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM CABEC4.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM CABEC5.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM CABEC6.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM CABEC7.
           PERFORM 113000-TESTAR-FS-RELATO.

           MOVE ZEROS                  TO ACU-LINHAS.

      *----------------------------------------------------------------*
       361000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       362000-FORMATAR-CAMPOS-REL      SECTION.
      *----------------------------------------------------------------*

ST25X6*    MOVE CCGC-CPF               OF GFCTB0A4
ST25X6*                                TO LT1-TOT-CLIE-1.
ST25X6*    MOVE CFLIAL-CGC             OF GFCTB0A4
ST25X6*                                TO LT1-TOT-CLIE-2.
ST25X6*    MOVE CCTRL-CPF-CGC          OF GFCTB0A4
ST25X6*                                TO LT1-TOT-CLIE-3.
ST25X6     MOVE CCGC-CPF-ST            OF GFCTB0A4
ST25X6                                 TO LT1-TOT-CLIE-1.
ST25X6     MOVE CFLIAL-CGC-ST          OF GFCTB0A4
ST25X6                                 TO LT1-TOT-CLIE-2.
ST25X6     MOVE CCTRL-CPF-CGC-ST       OF GFCTB0A4
ST25X6                                 TO LT1-TOT-CLIE-3.
           MOVE CSERVC-TARIF           OF GFCTB0A4  TO LT2-TOT-TARIFA.
           MOVE DOCOR-EVNTO            OF GFCTB0A4  TO LT3-DATA.
           MOVE ACU-SIT-1              TO LD1-QTD-1.
           MOVE ACU-SIT-2              TO LD2-QTD-2.
           MOVE ACU-SIT-3              TO LD3-QTD-3.
           MOVE ACU-SIT-4              TO LD4-QTD-4.
           MOVE ACU-SIT-5              TO LD5-QTD-5.
           MOVE ACU-SIT-6              TO LD6-QTD-6.
           MOVE ACU-SIT-9              TO LD7-QTD-9.
           MOVE ACU-SIT-10             TO LD8-QTD-10.
           MOVE ACU-SIT-11             TO LD9-QTD-11.
           MOVE ACU-SIT-12             TO LD10-QTD-12.
           MOVE ACU-SIT-13             TO LD11-QTD-13.
           MOVE ACU-SIT-14             TO LD12-QTD-14.
           MOVE ACU-SIT-15             TO LD13-QTD-15.
           MOVE ACU-SIT-16             TO LD14-QTD-16.
           MOVE ACU-SIT-17             TO LD15-QTD-17.
080208     MOVE ACU-SIT-18             TO LD16-QTD-18.
080208     MOVE ACU-SIT-19             TO LD17-QTD-19.
080208     MOVE ACU-SIT-NN             TO LD18-QTD-NN.
           MOVE ACU-VLR-1              TO LD1-VLR-1.
           MOVE ACU-VLR-2              TO LD2-VLR-2.
           MOVE ACU-VLR-3              TO LD3-VLR-3.
           MOVE ACU-VLR-4              TO LD4-VLR-4.
           MOVE ACU-VLR-5              TO LD5-VLR-5.
           MOVE ACU-VLR-6              TO LD6-VLR-6.
           MOVE ACU-VLR-9              TO LD7-VLR-9.
           MOVE ACU-VLR-10             TO LD8-VLR-10.
           MOVE ACU-VLR-11             TO LD9-VLR-11.
           MOVE ACU-VLR-12             TO LD10-VLR-12.
           MOVE ACU-VLR-13             TO LD11-VLR-13.
           MOVE ACU-VLR-14             TO LD12-VLR-14.
           MOVE ACU-VLR-15             TO LD13-VLR-15.
           MOVE ACU-VLR-16             TO LD14-VLR-16.
           MOVE ACU-VLR-17             TO LD15-VLR-17.
080208     MOVE ACU-VLR-18             TO LD16-VLR-18.
080208     MOVE ACU-VLR-19             TO LD17-VLR-19.
080208     MOVE ACU-VLR-NN             TO LD18-VLR-NN.
           MOVE ACU-SIT-TOT            TO LT3-QTD-TOT.
           MOVE ACU-VLR-TOT            TO LT3-VLR-TOT.

           MOVE  ZEROS TO                 ACU-SIT-1
                                          ACU-VLR-1
                                          ACU-SIT-2
                                          ACU-VLR-2
                                          ACU-SIT-3
                                          ACU-VLR-3
                                          ACU-SIT-4
                                          ACU-VLR-4
                                          ACU-SIT-5
                                          ACU-VLR-5
                                          ACU-SIT-6
                                          ACU-VLR-6
                                          ACU-SIT-9
                                          ACU-VLR-9
                                          ACU-SIT-10
                                          ACU-VLR-10
                                          ACU-SIT-11
                                          ACU-VLR-11
                                          ACU-SIT-12
                                          ACU-VLR-12
                                          ACU-SIT-13
                                          ACU-VLR-13
                                          ACU-SIT-14
                                          ACU-VLR-14
                                          ACU-SIT-15
                                          ACU-VLR-15
                                          ACU-SIT-16
                                          ACU-VLR-16
                                          ACU-SIT-17
                                          ACU-VLR-17
080208                                    ACU-SIT-18
080208                                    ACU-VLR-18
080208                                    ACU-SIT-19
080208                                    ACU-VLR-19
080208                                    ACU-SIT-NN
080208                                    ACU-VLR-NN.

      *----------------------------------------------------------------*
       362000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       370000-VERIFICAR-RESTART        SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1515'             TO CPROG            OF GFCTB000.
           MOVE WRK-NOME-JOB           TO CJOB             OF GFCTB000.
           MOVE ZEROS                  TO WCHAVE-RESTT-LEN OF GFCTB000.
           MOVE SPACES                 TO WCHAVE-RESTT-TEXT
                                                           OF GFCTB000.
           EXEC SQL
             SELECT
                   CPROG,
                   CJOB,
                   QREG_COMIT,
                   CSIT_RESTT,
                   CUSUAR_SENHA,
                   HATULZ,
                   WCHAVE_RESTT
             INTO
                   :GFCTB000.CPROG,
                   :GFCTB000.CJOB,
                   :GFCTB000.QREG-COMIT,
                   :GFCTB000.CSIT-RESTT,
                   :GFCTB000.CUSUAR-SENHA
                     :WRK-CUSUAR-SENHA-NULL,
                   :GFCTB000.HATULZ
                     :WRK-HATULZ-NULL,
                   :GFCTB000.WCHAVE-RESTT
                     :WRK-WCHAVE-RESTT-NULL
             FROM   DB2PRD.CTRL_ROTNA_GFCT
             WHERE
                   CPROG              = :GFCTB000.CPROG  AND
                   CJOB               = :GFCTB000.CJOB
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CTRL_ROTNA_GFCT ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0190'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 371000-ATUALIZAR-RESTART
           ELSE
               PERFORM 372000-INSERIR-RESTART
           END-IF.

           PERFORM 373000-EXECUTAR-COMMIT.

      *----------------------------------------------------------------*
       370000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       371000-ATUALIZAR-RESTART        SECTION.
      *----------------------------------------------------------------*

           MOVE WCHAVE-RESTT-TEXT      OF GFCTB000
                                       TO WRK-CHAVE-RESTART.
           COMPUTE WRK-QUANTIDADE      = WRK-QUANTIDADE + ACU-GRAVADOS.
           MOVE ZEROS                  TO ACU-GRAVADOS.
           MOVE WRK-CHAVE-RESTART      TO WCHAVE-RESTT-TEXT OF GFCTB000.
           MOVE 250                    TO WCHAVE-RESTT-LEN  OF GFCTB000.

           EXEC SQL
             UPDATE   DB2PRD.CTRL_ROTNA_GFCT
               SET
                   HATULZ             = CURRENT TIMESTAMP,
                   WCHAVE_RESTT       = :GFCTB000.WCHAVE-RESTT
                                          :WRK-WCHAVE-RESTT-NULL
             WHERE
                   CPROG              = :GFCTB000.CPROG              AND
                   CJOB               = :GFCTB000.CJOB
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CTRL_ROTNA_GFCT ' TO ERR-DBD-TAB
               MOVE 'UPDATE  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0200'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       371000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       372000-INSERIR-RESTART          SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-GRAVADOS           TO WRK-QUANTIDADE.
           MOVE ZEROS                  TO ACU-GRAVADOS.
           MOVE WRK-CHAVE-RESTART      TO WCHAVE-RESTT-TEXT OF GFCTB000.
           MOVE 250                    TO WCHAVE-RESTT-LEN  OF GFCTB000.
           MOVE 'GFCT1515'             TO CPROG             OF GFCTB000.
           MOVE WRK-NOME-JOB           TO CJOB              OF GFCTB000.
           MOVE ZEROS                  TO QREG-COMIT        OF GFCTB000
                                          WRK-WCHAVE-RESTT-NULL
                                          WRK-HATULZ-NULL.
           MOVE 'N'                    TO CSIT-RESTT        OF GFCTB000.
           MOVE SPACES                 TO CUSUAR-SENHA      OF GFCTB000.
           MOVE -1                     TO WRK-CUSUAR-SENHA-NULL.

           EXEC SQL
             INSERT INTO DB2PRD.CTRL_ROTNA_GFCT
                  (CPROG,
                   CJOB,
                   QREG_COMIT,
                   CSIT_RESTT,
                   CUSUAR_SENHA,
                   HATULZ,
                   WCHAVE_RESTT)
               VALUES
                  (:GFCTB000.CPROG,
                   :GFCTB000.CJOB,
                   :GFCTB000.QREG-COMIT,
                   :GFCTB000.CSIT-RESTT,
                   :GFCTB000.CUSUAR-SENHA
                     :WRK-CUSUAR-SENHA-NULL,
                    CURRENT TIMESTAMP,
                   :GFCTB000.WCHAVE-RESTT
                     :WRK-WCHAVE-RESTT-NULL)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CTRL_ROTNA_GFCT ' TO ERR-DBD-TAB
               MOVE 'INSERT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0210'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       372000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       373000-EXECUTAR-COMMIT          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               COMMIT
           END-EXEC.

           IF  SQLCODE                 NOT EQUAL ZEROS
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'COMMIT'           TO ERR-DBD-TAB
                                          ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0220'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       373000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       400000-FECHAR-CURSOR1           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0A7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_GRP ' TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0230'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       400000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       500000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           IF  ACU-PAGINAS             GREATER ZEROS
               PERFORM 510000-EMITIR-TOTAIS
           END-IF.

           PERFORM 520000-DELETAR-RESTART.

           PERFORM 373000-EXECUTAR-COMMIT.

           CLOSE ENTCLIE
                 SAICLIE
                 RELATO.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

           DISPLAY '*************** GFCT1515 ***************'.
           DISPLAY '*                                      *'.
           DISPLAY '* PROCESSAMENTO ENCERRADO COM SUCESSO! *'.
           DISPLAY '*                                      *'.
           DISPLAY '*************** GFCT1515 ***************'.

           GOBACK.

      *----------------------------------------------------------------*
       500000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       510000-EMITIR-TOTAIS            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           MOVE ACU-SIT-TOT-GERAL      TO LT4-QTD-TOT-GERAL.
           MOVE ACU-VLR-TOT-GERAL      TO LT4-VLR-TOT-GERAL.

           WRITE REG-RELATO            FROM CABEC7.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM CABEC7.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM CABEC7.
           PERFORM 113000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM LINTOT4.
           PERFORM 113000-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       510000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       520000-DELETAR-RESTART          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1515'             TO CPROG    OF GFCTB000.
           MOVE WRK-NOME-JOB           TO CJOB     OF GFCTB000.

           EXEC SQL
             DELETE FROM    DB2PRD.CTRL_ROTNA_GFCT
             WHERE
                   CPROG              = :GFCTB000.CPROG              AND
                   CJOB               = :GFCTB000.CJOB
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CTRL_ROTNA_GFCT ' TO ERR-DBD-TAB
               MOVE 'DELETE  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0240'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       520000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1515'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL 'APL'
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
           ELSE
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
