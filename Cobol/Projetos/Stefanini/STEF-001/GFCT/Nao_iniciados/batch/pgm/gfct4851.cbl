      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT4851.
       AUTHOR.     SONDA IT.
      *================================================================*
      *      S O N D A   I T                                           *
      *================================================================*
      *                                                                *
      *    PROGRAMA....:   GFCT4851                                    *
      *    PROGRAMADOR.:   REGINA YAMASHITA        - SONDA IT          *
      *    ANALISTA....:   REGINA YAMASHITA        - SONDA IT          *
      *    DATA........:   10/06/2016                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERA ARQUIVO DE CONTAS ENCARTEIRADAS COM    *
      *      DIREITO A TEST DRIVE (ISENCAO OU DESCONTO) NA MENSALIDADE *
      *      DA CESTA EXCLUSIVE.                                       *
      *----------------------------------------------------------------*
      *    27/10/16 ...:   REGINA - SONDA IT                           *
      *    ACERTO NA OBTENCAO DA ADESAO ANTERIOR A VIGENTE             *
      *----------------------------------------------------------------*
      *    ARQUIVOS         :                                          *
      *      ARQGECTE                              I#GECTAF            *
      *      CTRLDATA                              I#GFCT06            *
      *      CTAEXCLS                              GFCTWP16            *
      *      CTAREJES                              GFCTWP17            *
      *                                                                *
      *    BANCO DE DADOS   :                                          *
      *     DB2                                                        *
      *      TABLE                               INCLUDE/BOOK          *
      *      DB2PRD.ADSAO_INDVD_PCOTE              GFCTB009            *
      *      DB2PRD.BONIF_SERVC_TARIF              GFCTB023            *
      *      DB2PRD.V01CLIENTE_AGENCIA             CLIEV004            *
      *      DB2PRD.V01CTA_POUPANCA                CLIEV007            *
      *      DB2PRD.V01CTA_CORRENTE                CLIEV008            *
      *      DB2PRD.MOVTO_EVNTO_CRRTT              GFCTB092            *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *    CKRS1000 - MODULO PARA INDICAR PROCESSAMWENTO               *
      *    CKRS0105 - MODULO PARA INICIAR E FINALIZAR CONEXAO DB2      *
      *    FRWK2999 - GRAVAR LOG DE ERRO P/ BATCH                      *
      *                                                                *
      *================================================================*
ATEND * IN2205176 - ATENDIMENTO AO INCIDENTE EM AMBIENTE DE PRODUÁ„O   *
ATEND * ALAN G HOOGEVOONINK - DISPONIBILIDADE MADRUGADA - 14/04/2018   *
ATEND * ALTERA«√O SOLICITADA PELA ANALISTA LAURA MIREILE KONKOL        *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       INPUT-OUTPUT                    SECTION.
      *================================================================*
       FILE-CONTROL.

           SELECT CTRLDATA ASSIGN      TO UT-S-CTRLDATA
                      FILE STATUS      IS WRK-FS-CTRLDATA.

           SELECT ARQGECTE ASSIGN      TO UT-S-ARQGECTE
                      FILE STATUS      IS WRK-FS-ARQGECTE.

           SELECT CTAEXCLS ASSIGN      TO UT-S-CTAEXCLS
                      FILE STATUS      IS WRK-FS-CTAEXCLS.

           SELECT CTAREJES ASSIGN      TO UT-S-CTAREJES
                      FILE STATUS      IS WRK-FS-CTAREJES.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *================================================================*
       FILE                            SECTION.
      *================================================================*

      *----------------------------------------------------------------*
      *   INPUT:  RELACAO DE CONTAS GERENCIADAS (GECT)                 *
      *           ORG.SEQUENCIAL  -  LRECL = 20                        *
      *----------------------------------------------------------------*

       FD  ARQGECTE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQGECTE                 PIC  X(020).

      *----------------------------------------------------------------*
      *   INPUT:  ARQUIVO DE DATAS DO GFCT                             *
      *           ORG.SEQUENCIAL  -  LRECL = 100                       *
      *----------------------------------------------------------------*

       FD  CTRLDATA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-CTRLDATA                 PIC  X(100).

      *----------------------------------------------------------------*
      *   OUTPUT: CONTAS DE TEST DRIVE CESTA EXCLUSIVE                 *
      *           ORG.SEQUENCIAL  -  LRECL = 110                       *
      *----------------------------------------------------------------*

       FD  CTAEXCLS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-CTAEXCLS                 PIC  X(110).

      *----------------------------------------------------------------*
      *   OUTPUT: CONTAS REJEITADAS (INCONSISTENTES)                   *
      *           ORG.SEQUENCIAL  -  LRECL = 70                        *
      *----------------------------------------------------------------*

       FD  CTAREJES
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-CTAREJES                 PIC  X(70).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

       01  FILLER                      PIC X(032)          VALUE
           '*   INICIO DA WORKING GFCT4851 *'.

      *----------------------------------------------------------------*
       77  WRK-FS-CTRLDATA             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQGECTE             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CTAEXCLS             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CTAREJES             PIC  X(002)         VALUE SPACES.
       77  WRK-FIM                     PIC  X(001)         VALUE 'N'.
       77  WRK-SQLCODE                 PIC S9(009) COMP    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-ARQGECTE      PIC S9(009)V COMP-3   VALUE ZEROS.
           05 ACU-GRAVA-CTAEXCLS      PIC S9(009)V COMP-3   VALUE ZEROS.
           05 ACU-GRAVA-CTAREJES      PIC S9(009)V COMP-3   VALUE ZEROS.
           05 ACU-SEM-ADESAO          PIC S9(009)V COMP-3   VALUE ZEROS.
           05 ACU-GECT-DUPLICADOS     PIC S9(009)V COMP-3   VALUE ZEROS.
           05 ACU-NAO-TEST-DRIVE      PIC S9(009)V COMP-3   VALUE ZEROS.
           05 ACU-GECT-DESPREZADOS    PIC S9(009)V COMP-3   VALUE ZEROS.
           05 WRK-EDIT                PIC ----.---.--9      VALUE ZEROS.
           05 ACU-ISENTO-REGRA1       PIC S9(009)V COMP-3   VALUE ZEROS.
           05 ACU-ISENTO-REGRA2       PIC S9(009)V COMP-3   VALUE ZEROS.
           05 ACU-DESCONTO-REGRA3     PIC S9(009)V COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*   AREA DE CHAVES             *'.
      *----------------------------------------------------------------*

       01  WRK-GECT-ATUAL.
         05  WRK-GECT-AGENCIA-ATU      PIC S9(005)         VALUE ZEROS.
         05  WRK-GECT-CONTA-ATU        PIC S9(007)         VALUE ZEROS.
         05  WRK-GECT-DTINCL-ATU       PIC S9(009)         VALUE ZEROS.

       01  WRK-GECT-ANT.
         05  WRK-GECT-AGENCIA-ANT      PIC S9(005)         VALUE ZEROS.
         05  WRK-GECT-CONTA-ANT        PIC S9(007)         VALUE ZEROS.
         05  WRK-GECT-DTINCL-ANT       PIC S9(009)         VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*          AUXILIARES          *'.
      *---------------------------------------------------------------*
       77  WRK-ERRO                    PIC  X(01)          VALUE SPACE.
       77  WRK-TEM-ADESAO              PIC  X(01)          VALUE SPACE.
       77  WRK-TEST-DRIVE              PIC  X(01)          VALUE SPACE.
       77  WRK-IND-CESTA-EXCLUSIVE     PIC  X(01)          VALUE SPACE.
       77  WRK-LOCAL                   PIC  X(004)         VALUE ZEROS.

       01  WRK-ABEND                   PIC  X(001)         VALUE SPACE.
           88 WRK-ABENDAR                                  VALUE 'S'.

       01  WRK-DATA-DMA.
           05  WRK-DATA-DMA-DIA-X.
             07 WRK-DATA-DMA-DIA       PIC  9(02)          VALUE ZEROS.
           05  WRK-DATA-DMA-MES-X.
             07 WRK-DATA-DMA-MES       PIC  9(02)          VALUE ZEROS.
           05  WRK-DATA-DMA-ANO-X.
             07 WRK-DATA-DMA-ANO       PIC  9(04)          VALUE ZEROS.

       01  WRK-DATA-AMD                PIC  9(008)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-DATA-AMD.
           05  WRK-DATA-AMD-ANO-X.
             07 WRK-DATA-AMD-ANO       PIC  9(04).
           05  WRK-DATA-AMD-MES-X.
             07 WRK-DATA-AMD-MES       PIC  9(02).
           05  WRK-DATA-AMD-DIA-X.
             07 WRK-DATA-AMD-DIA       PIC  9(02).

       01  WRK-DATA-CORTE              PIC S9(009)         VALUE ZEROS.

       01  WRK-MES-S9-2                PIC S9(02) COMP-3   VALUE ZEROS.

       01  WRK-MES-9-2-R               PIC +9(02)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-MES-9-2-R.
           05 FILLER                   PIC  X(01).
           05 WRK-MES-9-2              PIC  9(02).

       01  WRK-ANO-S9-4                PIC S9(04) COMP-3   VALUE ZEROS.

       01  WRK-ANO-9-4-R               PIC +9(04)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-ANO-9-4-R.
           05 FILLER                   PIC  X(01).
           05 WRK-ANO-9-4              PIC  9(04).

       01  WRK-DATA-S9                PIC +9(009)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-DATA-S9.
           05  FILLER                 PIC  X(002).
           05  WRK-DATA-8-ANO         PIC  9(004).
           05  WRK-DATA-8-MES         PIC  9(002).
           05  WRK-DATA-8-DIA         PIC  9(002).

       01  WRK-DATA-X10.
           05  WRK-DATA-X10-DIA       PIC  9(02)          VALUE ZEROS.
           05  FILLER                 PIC  X(01)          VALUE '.'.
           05  WRK-DATA-X10-MES       PIC  9(02)          VALUE ZEROS.
           05  FILLER                 PIC  X(01)          VALUE '.'.
           05  WRK-DATA-X10-ANO       PIC  9(04)          VALUE ZEROS.

       01  WRK-DOCOR-ANT-R.
           05  WRK-DOCOR-DIA          PIC  X(02)          VALUE SPACES.
           05  FILLER                 PIC  X(08)          VALUE SPACES.
       01  WRK-DOCOR-ANT REDEFINES WRK-DOCOR-ANT-R
                                      PIC  X(10).

       01  FILLER.
           05 WRK-DABERT-CTA           PIC S9(009) COMP-3  VALUE ZEROS.
           05 WRK-CSGMTO-CLI           PIC S9(003)         VALUE ZEROS.
           05 WRK-TIPO-PESSOA          PIC  X(001)         VALUE SPACES.
           05 WRK-CESTA-ANT            PIC S9(005) COMP-3  VALUE +0.
           05 WRK-DINIC-ADSAO-ANT      PIC  X(010)         VALUE ZEROS.
           05 WRK-DINIC-ADSAO-ANT-AMD  PIC S9(009)         VALUE ZEROS.
           05 WRK-DFIM-ADSAO-ANT       PIC  X(010)         VALUE ZEROS.
           05 WRK-DINIC-PERIODO        PIC  X(010)         VALUE ZEROS.
           05 WRK-DATA-PROC-AMD        PIC  9(009)         VALUE ZEROS.
           05 WRK-DINIC-ADSAO-AMD      PIC S9(009)         VALUE ZEROS.
           05 WRK-TP-DESCONTO          PIC  9(001)         VALUE ZEROS.
           05 WRK-CID-CLI              PIC  X(026)         VALUE ZEROS.
           05 WRK-VALOR-LIQUIDO        PIC S9(09)V99 COMP-3 VALUE ZEROS.

       01  WRK-S9-2                    PIC +9(02)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-2.
           05 FILLER                   PIC  X(01).
           05 WRK-9-2                  PIC  9(02).

       01  WRK-S9-3                    PIC +9(03)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-3.
           05 FILLER                   PIC  X(01).
           05 WRK-9-3                  PIC  9(03).

       01  WRK-S9-4                    PIC +9(04)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-4.
           05 FILLER                   PIC  X(01).
           05 WRK-9-4                  PIC  9(04).

       01  WRK-S9-5                    PIC +9(05)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-5.
           05 FILLER                   PIC  X(01).
           05 WRK-9-5                  PIC  9(05).

       01  WRK-S9-7                    PIC +9(07)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-7.
           05 FILLER                   PIC  X(01).
           05 WRK-9-7                  PIC  9(07).

       01  WRK-S9-9                    PIC +9(09)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-9.
           05 FILLER                   PIC  X(01).
           05 WRK-9-9                  PIC  9(09).
           05 FILLER REDEFINES WRK-9-9.
             07 FILLER                 PIC  9(001).
             07 WRK-9-8                PIC  9(008).

       01  WRK-S9-9V99                 PIC +9(09)V99       VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-9V99.
           05 FILLER                   PIC  X(01).
           05 WRK-9-9V99               PIC  9(09)V99.

       01  WRK-QTD-MESES-ISENC         PIC S9(004) COMP-3  VALUE 0.
       01  WRK-QTD-MESES-ISENC-1       PIC S9(004) COMP-3  VALUE 0.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE BOOKS '.
      *---------------------------------------------------------------*

       01  WRK-CKRS0105                PIC  X(008)         VALUE
           'CKRS0105'.

           COPY 'I#CKRS04'.

       01  WRK-FRWK2999                PIC  X(008)         VALUE
           'FRWK2999'.

       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO.
              10 WRK-CHAR-INFO-ERRO    PIC  X(001) OCCURS 0 TO 30000
                                       TIMES DEPENDING ON
                                       FRWKGHEA-TAM-DADOS.

       01  WRK-AREA-ERRO-ARQUIVO.
           COPY 'I#FRWKAR'.

       01  WRK-AREA-BRAD0450.
           05 WRK-0450-ABEND-BAT       PIC S9(004) COMP    VALUE +1111.
           05 WRK-0450-DUMP-BAT        PIC  X(001)         VALUE 'S'.

       01  WRK-AREA-ERRO-DB2.
           COPY 'I#FRWKDB'.

      *----------------------------------------------------------------*
       01  FILLER PIC X(050) VALUE '* AREAS DE BOOKS                 *'.
      *----------------------------------------------------------------*

           COPY 'I#GFCT06'.
           COPY 'I#GECTAF'.
           COPY 'GFCTWP17'.
           COPY 'GFCTWP16'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA DE TABELAS DB2         *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB023
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB092
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB009
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

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
      *    CHAMA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO.               *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 3000-PROCESSAR      UNTIL
               WRK-FIM                 EQUAL 'S'.

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS DO PROGRAMA                          *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------

           CALL 'CKRS1000'.

           PERFORM 7000-INICIAR-CKRS0105.

           INITIALIZE FRWKGHEA-REGISTRO
                      FRWKGARQ-REGISTRO.

           SET ARQ-OPEN                TO TRUE.

           OPEN INPUT  ARQGECTE
                       CTRLDATA
                OUTPUT CTAEXCLS
                       CTAREJES.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 2000-LER-CTRLDATA.


           PERFORM 3900-OBTER-QTDE-MESES.

           IF  WRK-QTD-MESES-ISENC     EQUAL ZEROS
               DISPLAY '============== GFCT4851 ======================='
               DISPLAY 'QTDE MESES TEST DRIVE NAO ENCONTRADA (GFCTB023)'
               DISPLAY '==============================================='
               MOVE '10'               TO WRK-FS-ARQGECTE
               GO TO 1000-99-FIM
           END-IF.

           PERFORM 2050-CALCULA-DATA-CORTE.

           PERFORM 2100-LER-ARQGECTE.

           IF  WRK-FS-ARQGECTE         EQUAL '10'
               DISPLAY '====== GFCT4851 ======='
               DISPLAY 'ARQUIVO DO GECT VAZIO'
               DISPLAY '======================='
               MOVE 'S'                TO WRK-FIM
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE TESTE DE FILE-STATUS                                 *
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-CTRLDATA.

           PERFORM 1120-TESTAR-FS-ARQGECTE.

           PERFORM 1130-TESTAR-FS-CTAEXCLS.

           PERFORM 1140-TESTAR-FS-CTAREJES.

      *----------------------------------------------------------------
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    TESTE DE FILE-STATUS DO ARQUIVO CTRLDATA                    *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-CTRLDATA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CTRLDATA         NOT EQUAL '00'
               MOVE 'CTRLDATA'         TO FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-CTRLDATA    TO FRWKGARQ-FILE-STATUS
               SET  WRK-ABENDAR        TO TRUE
               PERFORM 9100-TRATAR-ERRO-ARQUIVO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE-STATUS DO ARQUIVO ARQGECTE                    *
      *----------------------------------------------------------------*
       1120-TESTAR-FS-ARQGECTE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQGECTE         NOT EQUAL '00'
               MOVE 'ARQGECTE'         TO FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-ARQGECTE    TO FRWKGARQ-FILE-STATUS
               SET  WRK-ABENDAR        TO TRUE
               PERFORM 9100-TRATAR-ERRO-ARQUIVO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE-STATUS DO ARQUIVO CTAEXCLS                    *
      *----------------------------------------------------------------*
       1130-TESTAR-FS-CTAEXCLS         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CTAEXCLS         NOT EQUAL '00'
               MOVE 'CTAEXCLS'         TO FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-CTAEXCLS    TO FRWKGARQ-FILE-STATUS
               SET  WRK-ABENDAR        TO TRUE
               PERFORM 9100-TRATAR-ERRO-ARQUIVO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    TESTE DE FILE-STATUS DO ARQUIVO CTAREJES                    *
      *----------------------------------------------------------------*
       1140-TESTAR-FS-CTAREJES         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CTAREJES         NOT EQUAL '00'
               MOVE 'CTAREJES'         TO FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-CTAREJES    TO FRWKGARQ-FILE-STATUS
               SET  WRK-ABENDAR        TO TRUE
               PERFORM 9100-TRATAR-ERRO-ARQUIVO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     LE ARQUIVO DE DATAS DO GFCT
      *----------------------------------------------------------------*
       2000-LER-CTRLDATA               SECTION.
      *----------------------------------------------------------------*

           READ CTRLDATA               INTO GFCT06-REG

           IF  WRK-FS-CTRLDATA         EQUAL '10'
               DISPLAY '====== GFCT4851 ======='
               DISPLAY 'ARQUIVO DE DATAS VAZIO '
               DISPLAY '======================='
               SET  WRK-ABENDAR        TO TRUE
               PERFORM 9100-TRATAR-ERRO-ARQUIVO
           ELSE
               DISPLAY 'DATA DE PROCESSAMENTO ' GFCT06-DPROCM-ATUAL
           END-IF.

           SET ARQ-READ                TO TRUE.

           PERFORM 1110-TESTAR-FS-CTRLDATA.

           MOVE GFCT06-DPROCM-ATUAL(7:4)
                                       TO WRK-DATA-AMD-ANO-X.
           MOVE GFCT06-DPROCM-ATUAL(4:2)
                                       TO WRK-DATA-AMD-MES-X.
           MOVE GFCT06-DPROCM-ATUAL(1:2)
                                       TO WRK-DATA-AMD-DIA-X.

           MOVE WRK-DATA-AMD           TO WRK-DATA-PROC-AMD.


           MOVE GFCT06-DPROCM-PROX     TO WRK-DINIC-PERIODO.
           MOVE '01'                   TO WRK-DINIC-PERIODO(1:2).

           DISPLAY 'DATA PARA OBTER ADESAO VIGENTE' WRK-DINIC-PERIODO.


      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     CALCULA DATA DE CORTE CONTA NOVA E DATA DE CORTE           *
      *     EXEMPLO: SE DATA PROC = 31/05/2016 E QTDE MESES = 2        *
      *     DATA CORTE CONTA      = 01/03/2016                         *
      *     DATA CORTE 2          = 01/04/2016                         *
      *----------------------------------------------------------------*
       2050-CALCULA-DATA-CORTE         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT06-DPROCM-ATUAL(7:4)
                                       TO WRK-DATA-DMA-ANO-X.
           MOVE GFCT06-DPROCM-ATUAL(4:2)
                                       TO WRK-DATA-DMA-MES-X.
           MOVE GFCT06-DPROCM-ATUAL(1:2)
                                       TO WRK-DATA-DMA-DIA-X.

      *----------* CALCULA DATA DE CORTE *-----------------------------*

           MOVE WRK-DATA-DMA-MES       TO WRK-MES-S9-2
           MOVE WRK-DATA-DMA-ANO       TO WRK-ANO-S9-4

           COMPUTE WRK-QTD-MESES-ISENC-1 = WRK-QTD-MESES-ISENC - 1.
           COMPUTE WRK-MES-S9-2        = WRK-MES-S9-2
                                       - WRK-QTD-MESES-ISENC-1.

           IF WRK-MES-S9-2             <= ZEROS
              COMPUTE WRK-MES-S9-2 = 12 - WRK-QTD-MESES-ISENC-1
              COMPUTE WRK-ANO-S9-4 = WRK-ANO-S9-4 - 1
           END-IF.

           MOVE 01                     TO WRK-DATA-AMD-DIA.
           MOVE WRK-MES-S9-2           TO WRK-MES-9-2-R.
           MOVE WRK-MES-9-2            TO WRK-DATA-AMD-MES.
           MOVE WRK-ANO-S9-4           TO WRK-ANO-9-4-R.
           MOVE WRK-ANO-9-4            TO WRK-DATA-AMD-ANO.

           MOVE WRK-DATA-AMD           TO WRK-DATA-CORTE.
           DISPLAY 'WRK-DATA-CORTE'        WRK-DATA-CORTE.

      *----------------------------------------------------------------*
       2050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     LE ARQUIVO DE CONTAS GERENCIADAS DO GECT
      *----------------------------------------------------------------*
       2100-LER-ARQGECTE               SECTION.
      *----------------------------------------------------------------*

           READ ARQGECTE               INTO REG-CONTA-GERE

           IF  WRK-FS-ARQGECTE         EQUAL '10'
               MOVE 'S'                TO WRK-FIM
               GO                      TO 2100-99-FIM
           END-IF.

           SET ARQ-READ                TO TRUE.

           PERFORM 1120-TESTAR-FS-ARQGECTE.

           ADD  +1                     TO ACU-LIDOS-ARQGECTE.

           MOVE WRK-GECT-ATUAL         TO WRK-GECT-ANT.

           MOVE GERE-AGENCIA           TO WRK-GECT-AGENCIA-ATU
           MOVE GERE-CONTA             TO WRK-GECT-CONTA-ATU
           MOVE GERE-DT-INCLUSAO       TO WRK-GECT-DTINCL-ATU.


      *----* DESPREZA SE DATA ENCARTEIRAMENTO = ZEROS             *----*

           IF  GERE-DT-INCLUSAO        =  ZEROS OR
               GERE-DT-INCLUSAO        > WRK-DATA-PROC-AMD
               ADD  +1                 TO ACU-GECT-DESPREZADOS
               GO TO 2100-LER-ARQGECTE
           END-IF.

      *----* DESPREZA SE MESMA AGENCIA E CONTA. MANTEM O PRIMEIRO *----*
      *----* REGISTRO COM A MENOR DATA DE ENCARTEIRAMENTO         *----*

           IF   WRK-GECT-AGENCIA-ATU   =  WRK-GECT-AGENCIA-ANT
              AND WRK-GECT-CONTA-ATU   =  WRK-GECT-CONTA-ANT
              ADD  +1                  TO ACU-GECT-DUPLICADOS
              GO TO 2100-LER-ARQGECTE
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO                                              *
      *----------------------------------------------------------------*
       3000-PROCESSAR         SECTION.

           PERFORM 3500-OBTER-DADOS-CLIE.

           IF WRK-ERRO                 NOT EQUAL 'N'
              PERFORM 4000-GRAVA-CTAREJES
              GO TO 3000-10-LER
           END-IF.

           PERFORM 3200-OBTER-ADESAO-VIGENTE.

           IF WRK-TEM-ADESAO           = 'S'
              PERFORM 3300-VERIFICAR-ADESAO-ANTERIOR
           ELSE
              ADD +1                   TO ACU-SEM-ADESAO
              GO TO 3000-10-LER
           END-IF.

           PERFORM 3100-VERIFICA-TEST-DRIVE

           PERFORM 3400-OBTER-VALOR-LIQUIDO.

           IF WRK-TEST-DRIVE          = 'S'
              PERFORM 4200-GRAVA-CTAEXCLS
           ELSE
              ADD +1                  TO ACU-NAO-TEST-DRIVE
           END-IF.

      *----------------------------------------------------------------*
       3000-10-LER.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ARQGECTE.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-VERIFICA-TEST-DRIVE        SECTION.
      *----------------------------------------------------------------*

           IF WRK-CSGMTO-CLI           GREATER 99
              MOVE 'N'                 TO WRK-TEST-DRIVE
              MOVE ZEROS               TO WRK-TP-DESCONTO
              GO TO 3100-99-FIM
           END-IF.

      *----* REGRA 1 - CONTA NOVA                               *------*
      *----* - DATA DE ABERTURA DA CONTA APOS DATA CORTE        *------*

           IF WRK-DABERT-CTA           >=      WRK-DATA-CORTE
              MOVE 1                   TO WRK-TP-DESCONTO
              ADD  +1                  TO ACU-ISENTO-REGRA1
              MOVE 'S'                 TO WRK-TEST-DRIVE
              GO TO 3100-99-FIM
           END-IF.

      *----* REGRA 3 - CONTA JA EXISTENTE NO SEGTO EXCLUSIVE. *--------*
      *----* (CESTA ENQUADRADA NA CESTA EXCLUSIVE)            *--------*
      *----*                                                  *--------*
      *----*  1) SE SEGTO DO CLIE  = 0 A 99                   *--------*
      *----*  2) DATA DE ADESAO OCORREU APOS DATA DE CORTE    *--------*
      *----*  3) DATA ENCARTEIR/O OCORREU ANTES DATA DE CORTE *--------*

           IF WRK-GECT-DTINCL-ATU      < WRK-DATA-CORTE AND
              WRK-CESTA-ANT            NOT EQUAL ZEROS        AND
              WRK-IND-CESTA-EXCLUSIVE  EQUAL 'S'              AND
              WRK-DINIC-ADSAO-AMD      GREATER  WRK-DATA-CORTE
              MOVE 3                   TO WRK-TP-DESCONTO
              ADD  +1                  TO ACU-DESCONTO-REGRA3
              MOVE 'S'                 TO WRK-TEST-DRIVE
              GO TO 3100-99-FIM
           END-IF.

           IF WRK-GECT-DTINCL-ATU      < WRK-DATA-CORTE AND
              WRK-DINIC-ADSAO-AMD      GREATER  WRK-DATA-CORTE
              MOVE 3                   TO WRK-TP-DESCONTO
              ADD  +1                  TO ACU-DESCONTO-REGRA3
              MOVE 'S'                 TO WRK-TEST-DRIVE
              GO TO 3100-99-FIM
           END-IF.


      *----* REGRA 2 - CONTA JA EXISTENTE COM ALTERACAO PARA  *--------*
      *----* CESTA EXCLUSIVE                                  *--------*
      *----*                                                  *--------*
      *----*  1) SE SEGTO DO CLIE 0 A 99 NA DATA DO CALCULO   *--------*
      *----*  2) DATA ENCARTEIR/O OCORREU APOS DA DATA CORTE  *--------*

           IF WRK-GECT-DTINCL-ATU      >= WRK-DATA-CORTE
              MOVE 2                   TO WRK-TP-DESCONTO
              ADD  +1                  TO ACU-ISENTO-REGRA2
              MOVE 'S'                 TO WRK-TEST-DRIVE
              GO TO 3100-99-FIM
           END-IF.

           MOVE 'N'                    TO WRK-TEST-DRIVE.
           MOVE ZEROS                  TO WRK-TP-DESCONTO.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     VERIFICA SE TEM ADESAO VIGENTE A CESTA EXCLUSIVE           *
      *----------------------------------------------------------------*

       3200-OBTER-ADESAO-VIGENTE       SECTION.

           MOVE 'N'                    TO WRK-TEM-ADESAO.
           MOVE 'N'                    TO WRK-IND-CESTA-EXCLUSIVE.
           MOVE ZEROS                  TO WRK-CESTA-ANT.
           MOVE ZEROS                  TO WRK-TP-DESCONTO.

           MOVE WRK-GECT-AGENCIA-ATU   TO CJUNC-DEPDC     OF GFCTB009.
           MOVE WRK-GECT-CONTA-ATU     TO CCTA-CLI        OF GFCTB009.
           MOVE 0                      TO CINDCD-EXCL-REG OF GFCTB009.
           MOVE 008                    TO CTPO-BONIF      OF GFCTB023.
           MOVE 010                    TO CCRITR-BONIF    OF GFCTB023.

           EXEC SQL
              SELECT A.CSERVC_TARIF,
                     A.DINIC_ADSAO_INDVD,
                     A.DFIM_ADSAO_INDVD
              INTO   :GFCTB009.CSERVC-TARIF,
                     :GFCTB009.DINIC-ADSAO-INDVD,
                     :GFCTB009.DFIM-ADSAO-INDVD
              FROM DB2PRD.ADSAO_INDVD_PCOTE A,
                   DB2PRD.BONIF_SERVC_TARIF B
              WHERE A.CSERVC_TARIF      =   B.CSERVC_TARIF
               AND  A.CJUNC_DEPDC       =  :GFCTB009.CJUNC-DEPDC
               AND  A.CCTA_CLI          =  :GFCTB009.CCTA-CLI
               AND  A.DINIC_ADSAO_INDVD <= :WRK-DINIC-PERIODO
               AND  A.DFIM_ADSAO_INDVD  >= :WRK-DINIC-PERIODO
               AND  A.CINDCD_EXCL_REG   =  :GFCTB009.CINDCD-EXCL-REG
               AND  B.CSERVC_TARIF      >  0
               AND  B.DINIC_REGRA_BONIF <= :WRK-DINIC-PERIODO
               AND  B.CTPO_BONIF        =  :GFCTB023.CTPO-BONIF
               AND  B.CCRITR_BONIF      =  :GFCTB023.CCRITR-BONIF
               AND  B.DFIM_REGRA_BONIF  >= :WRK-DINIC-PERIODO
           END-EXEC.

           IF (SQLCODE          NOT EQUAL ZEROS AND +100
ATEND                           AND -811 AND -305 ) OR
               (SQLWARN0               EQUAL 'W'      )
281016          DISPLAY 'AG/CTA ' WRK-GECT-AGENCIA-ATU '/'
281016                            WRK-GECT-CONTA-ATU
                SET DB2-SELECT         TO TRUE
                MOVE '0010'            TO FRWKGDB2-LOCAL
                MOVE 'ADSAO_INDVD_PCOTE'
                                       TO FRWKGDB2-NOME-TABELA
                SET  ERRO-DB2          TO TRUE
                SET  WRK-ABENDAR       TO TRUE
                MOVE SQLCODE           TO WRK-SQLCODE
                PERFORM 9400-ERRO-DB2
           END-IF.

           IF SQLCODE                  EQUAL ZEROS
              MOVE DINIC-ADSAO-INDVD   OF GFCTB009(7:4)
                                       TO WRK-DATA-AMD-ANO-X
              MOVE DINIC-ADSAO-INDVD   OF GFCTB009(4:2)
                                       TO WRK-DATA-AMD-MES-X
              MOVE DINIC-ADSAO-INDVD   OF GFCTB009(1:2)
                                       TO WRK-DATA-AMD-DIA-X
              MOVE WRK-DATA-AMD        TO WRK-DINIC-ADSAO-AMD
              MOVE 'S'                 TO WRK-TEM-ADESAO
           ELSE
              MOVE ZEROS               TO WRK-CESTA-ANT
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     VERIFICA SE EXISTE ADESAO ANTERIOR A CESTA EXCLUSIVE       *
      *----------------------------------------------------------------*
       3300-VERIFICAR-ADESAO-ANTERIOR       SECTION.

           EXEC SQL
             SELECT MAX(DINIC_ADSAO_INDVD)
             INTO  :WRK-DINIC-ADSAO-ANT
             FROM   DB2PRD.ADSAO_INDVD_PCOTE
             WHERE  CJUNC_DEPDC        = :GFCTB009.CJUNC-DEPDC
               AND  CCTA_CLI           = :GFCTB009.CCTA-CLI
               AND  DINIC_ADSAO_INDVD  < :GFCTB009.DINIC-ADSAO-INDVD
271016         AND  CINDCD_EXCL_REG    = 0
           END-EXEC.

           IF (SQLCODE          NOT EQUAL ZEROS AND +100 AND -305)
                SET DB2-SELECT         TO TRUE
281016          DISPLAY 'AG/CTA ' WRK-GECT-AGENCIA-ATU '/'
281016                            WRK-GECT-CONTA-ATU
                MOVE '0020'            TO FRWKGDB2-LOCAL
                MOVE 'ADSAO_INDVD_PCOTE'
                                       TO FRWKGDB2-NOME-TABELA
                SET  ERRO-DB2          TO TRUE
                SET  WRK-ABENDAR       TO TRUE
                MOVE SQLCODE           TO WRK-SQLCODE
                PERFORM 9400-ERRO-DB2
            END-IF.

            IF SQLCODE                 EQUAL ZEROS
               PERFORM 3320-OBTER-CESTA-ANTERIOR
            END-IF.

            MOVE WRK-DINIC-ADSAO-ANT(7:4)
                                       TO WRK-DATA-AMD-ANO-X
            MOVE WRK-DINIC-ADSAO-ANT(4:2)
                                       TO WRK-DATA-AMD-MES-X
            MOVE WRK-DINIC-ADSAO-ANT(1:2)
                                       TO WRK-DATA-AMD-DIA-X
            MOVE WRK-DATA-AMD          TO WRK-DINIC-ADSAO-ANT-AMD.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     VERIFICA SE CESTA DA ADESAO ANTERIOR ERA EXCLUSIVE         *
      *----------------------------------------------------------------*
       3320-OBTER-CESTA-ANTERIOR             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT CSERVC_TARIF,  DFIM_ADSAO_INDVD
             INTO  :WRK-CESTA-ANT, :WRK-DFIM-ADSAO-ANT
             FROM   DB2PRD.ADSAO_INDVD_PCOTE
             WHERE  CJUNC_DEPDC         = :GFCTB009.CJUNC-DEPDC
               AND  CCTA_CLI            = :GFCTB009.CCTA-CLI
               AND  DINIC_ADSAO_INDVD   = :WRK-DINIC-ADSAO-ANT
271016         AND  CINDCD_EXCL_REG     = 0
281016         FETCH  FIRST ROW ONLY
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W'      )
281016          DISPLAY 'AG/CTA ' WRK-GECT-AGENCIA-ATU '/'
281016                            WRK-GECT-CONTA-ATU
                WRK-GECT-AGENCIA-ATU
                WRK-GECT-CONTA-ATU
                SET DB2-SELECT         TO TRUE
                MOVE '0030'            TO FRWKGDB2-LOCAL
                MOVE 'ADSAO_INDVD_PCOTE'
                                       TO FRWKGDB2-NOME-TABELA
                SET  ERRO-DB2          TO TRUE
                SET  WRK-ABENDAR       TO TRUE
                MOVE SQLCODE           TO WRK-SQLCODE
                PERFORM 9400-ERRO-DB2
           END-IF.

           EXEC SQL
             SELECT 'S'
                INTO :WRK-IND-CESTA-EXCLUSIVE
                FROM DB2PRD.BONIF_SERVC_TARIF
              WHERE CSERVC_TARIF       = :WRK-CESTA-ANT
                AND DINIC_REGRA_BONIF <= :GFCT06-DPROCM-ATUAL
                AND DFIM_REGRA_BONIF  >= :GFCT06-DPROCM-ATUAL
                AND CTPO_BONIF         = :GFCTB023.CTPO-BONIF
                AND CCRITR_BONIF       = :GFCTB023.CCRITR-BONIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND + 100) OR
               (SQLWARN0               EQUAL 'W'      )
281016          DISPLAY 'AG/CTA ' WRK-GECT-AGENCIA-ATU '/'
281016                            WRK-GECT-CONTA-ATU
                SET DB2-SELECT         TO TRUE
                MOVE '0035'            TO FRWKGDB2-LOCAL
                MOVE 'DB2PRD.BONIF_SERVC_TARIF'
                                       TO FRWKGDB2-NOME-TABELA
                SET  ERRO-DB2          TO TRUE
                SET  WRK-ABENDAR       TO TRUE
                MOVE SQLCODE           TO WRK-SQLCODE
                PERFORM 9400-ERRO-DB2
           END-IF.

           IF SQLCODE                  EQUAL ZEROS
              MOVE 'S'                 TO WRK-IND-CESTA-EXCLUSIVE
           ELSE
              MOVE 'N'                 TO WRK-IND-CESTA-EXCLUSIVE
           END-IF.

      *----------------------------------------------------------------*
       3320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     OBTER VALOR LIQUIDO COBRADO MENSALIDADE ANTERIOR           *
      *----------------------------------------------------------------*

       3400-OBTER-VALOR-LIQUIDO        SECTION.

           MOVE '3400'                 TO WRK-LOCAL.
           MOVE 'OBTER-VALOR-LIQUIDO'  TO FRWKGHEA-IDEN-PARAGRAFO.

      *----* CESTA ANTERIOR IGUAL A ZEROS QUANDO NAO      *------------*
      *----* EXISTE ADESAO ANTERIOR. NESTE CASO,          *------------*
      *----* VALOR LIQUIDO = ZEROS.                       *------------*

           MOVE ZEROS                  TO WRK-VALOR-LIQUIDO.

           IF WRK-TP-DESCONTO          NOT EQUAL 3
              GO TO 3400-99-FIM
           END-IF.

           IF WRK-CESTA-ANT            EQUAL ZEROS
              GO TO 3400-99-FIM
           END-IF.

           MOVE WRK-GECT-AGENCIA-ATU   TO CAG-DSTNO-MOVTO  OF GFCTB092.
           MOVE WRK-GECT-CONTA-ATU     TO CCTA-DSTNO-MOVTO OF GFCTB092.
           MOVE WRK-CESTA-ANT          TO CSERVC-TARIF     OF GFCTB092.
           MOVE 237                    TO CBCO-DSTNO-MOVTO OF GFCTB092.

           MOVE WRK-DFIM-ADSAO-ANT     TO WRK-DOCOR-ANT
           MOVE '01'                   TO WRK-DOCOR-DIA.

           EXEC SQL
              SELECT  VTARIF_LIQ_MOVTO
              INTO    :GFCTB092.VTARIF-LIQ-MOVTO
              FROM    DB2PRD.MOVTO_EVNTO_CRRTT
              WHERE   CAG_DSTNO_MOVTO    =  :GFCTB092.CAG-DSTNO-MOVTO
               AND    CCTA_DSTNO_MOVTO   =  :GFCTB092.CCTA-DSTNO-MOVTO
               AND    CSERVC_TARIF       =  :GFCTB092.CSERVC-TARIF
               AND    CBCO_DSTNO_MOVTO   =  :GFCTB092.CBCO-DSTNO-MOVTO
               AND    DOCOR_EVNTO        <= :WRK-DFIM-ADSAO-ANT
               AND    DOCOR_EVNTO        >= :WRK-DOCOR-ANT
               AND    CSIT_EVNTO_RECBD   IN (3, 4, 5)
               FETCH  FIRST ROW ONLY
           END-EXEC.

           IF (SQLCODE          NOT EQUAL ZEROS AND +100 AND -305) OR
               (SQLWARN0               EQUAL 'W'      )
281016          DISPLAY 'AG/CTA ' WRK-GECT-AGENCIA-ATU '/'
281016                            WRK-GECT-CONTA-ATU
                SET DB2-SELECT         TO TRUE
                MOVE '0036'            TO FRWKGDB2-LOCAL
                MOVE 'MOVTO_EVNTO_CRRTT'
                                       TO FRWKGDB2-NOME-TABELA
                SET  ERRO-DB2          TO TRUE
                SET  WRK-ABENDAR       TO TRUE
                MOVE SQLCODE           TO WRK-SQLCODE
                PERFORM 9400-ERRO-DB2
           END-IF.

           IF SQLCODE                  EQUAL ZEROS
              MOVE VTARIF-LIQ-MOVTO    OF GFCTB092
                                       TO WRK-S9-9V99
              MOVE WRK-9-9V99          TO WRK-VALOR-LIQUIDO
           ELSE
              MOVE ZEROS               TO WRK-VALOR-LIQUIDO
           END-IF.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     OBTEM SEGMENTO, DATA DE ABERTUR E CPF DO CLIENTE           *
      *----------------------------------------------------------------*
       3500-OBTER-DADOS-CLIE           SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-ERRO.

           IF  WRK-GECT-CONTA-ATU      GREATER 999999
               PERFORM 3600-ACESSAR-CLIEV007
           ELSE
               PERFORM 3700-ACESSAR-CLIEV008
           END-IF.

           PERFORM 3800-ACESSAR-CPF.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ACESSAR DADOS CONTA POUPANCA                               *
      *----------------------------------------------------------------*
       3600-ACESSAR-CLIEV007           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GECT-AGENCIA-ATU   TO CJUNC-DEPDC  OF CLIEV007.
           MOVE WRK-GECT-CONTA-ATU     TO CCTA-CLI     OF CLIEV007.

           EXEC SQL
             SELECT CSGMTO_CLI,
                    CTPO_CTA,
                    CID_CLI,
                    DABERT_CTA
             INTO  :CLIEV007.CSGMTO-CLI,
                   :CLIEV007.CTPO-CTA,
                   :CLIEV007.CID-CLI,
                   :CLIEV007.DABERT-CTA
             FROM   DB2PRD.V01CTA_POUPANCA
             WHERE CJUNC_DEPDC        = :CLIEV007.CJUNC-DEPDC AND
                   CCTA_CLI           = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0               EQUAL 'W'           )
281016          DISPLAY 'AG/CTA ' WRK-GECT-AGENCIA-ATU '/'
281016                            WRK-GECT-CONTA-ATU
              SET DB2-SELECT          TO TRUE
              MOVE '0040'             TO FRWKGDB2-LOCAL
              MOVE 'V01CLIENTE_AGENCIA'
                                      TO FRWKGDB2-NOME-TABELA
              SET  ERRO-DB2           TO TRUE
              SET  WRK-ABENDAR        TO TRUE
              MOVE SQLCODE            TO WRK-SQLCODE
              PERFORM 9400-ERRO-DB2
           END-IF.

           IF SQLCODE                 EQUAL +100
              MOVE '1'                TO WRK-ERRO
           END-IF.

           IF SQLCODE EQUAL ZEROS
              IF  CTPO-CTA OF CLIEV007 EQUAL 01 OR 04
                  MOVE 'J'            TO WRK-TIPO-PESSOA
              ELSE
                  MOVE 'F'            TO WRK-TIPO-PESSOA
              END-IF
              MOVE CSGMTO-CLI         OF CLIEV007
                                      TO WRK-CSGMTO-CLI
              MOVE CID-CLI            OF CLIEV007
                                      TO WRK-CID-CLI
              MOVE DABERT-CTA OF CLIEV007(1:2)
                                      TO WRK-DATA-AMD-DIA-X
              MOVE DABERT-CTA OF CLIEV007(4:2)
                                      TO WRK-DATA-AMD-MES-X
              MOVE DABERT-CTA OF CLIEV007(7:4)
                                      TO WRK-DATA-AMD-ANO-X
              MOVE WRK-DATA-AMD       TO WRK-DABERT-CTA
           END-IF.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *       ACESSAR DADOS DA CONTA CORRENTE                          *
      *----------------------------------------------------------------*
       3700-ACESSAR-CLIEV008           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GECT-AGENCIA-ATU   TO CJUNC-DEPDC  OF CLIEV008.
           MOVE WRK-GECT-CONTA-ATU     TO CCTA-CLI     OF CLIEV008.

           EXEC SQL
             SELECT CSGMTO_CLI,
                    CTPO_CTA,
                    CID_CLI,
                    DABERT_CTA
             INTO  :CLIEV008.CSGMTO-CLI,
                   :CLIEV008.CTPO-CTA,
                   :CLIEV008.CID-CLI,
                   :CLIEV008.DABERT-CTA
             FROM   DB2PRD.V01CTA_CORRENTE
             WHERE CJUNC_DEPDC        = :CLIEV008.CJUNC-DEPDC AND
                   CCTA_CLI           = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'           )
281016          DISPLAY 'AG/CTA ' WRK-GECT-AGENCIA-ATU '/'
281016                            WRK-GECT-CONTA-ATU
              SET DB2-SELECT           TO TRUE
              MOVE '0050'              TO FRWKGDB2-LOCAL
              MOVE 'V01CLIENTE_AGENCIA'
                                       TO FRWKGDB2-NOME-TABELA
              SET  ERRO-DB2            TO TRUE
              SET  WRK-ABENDAR         TO TRUE
              MOVE SQLCODE             TO WRK-SQLCODE
              PERFORM 9400-ERRO-DB2
           END-IF.

           IF SQLCODE                  EQUAL +100
              MOVE '2'                 TO WRK-ERRO
           END-IF.

           IF SQLCODE EQUAL ZEROS
              IF  CTPO-CTA OF CLIEV008 EQUAL 01 OR 04
                  MOVE 'J'             TO WRK-TIPO-PESSOA
              ELSE
                  MOVE 'F'             TO WRK-TIPO-PESSOA
              END-IF
              MOVE CSGMTO-CLI          OF CLIEV008
                                       TO WRK-CSGMTO-CLI
              MOVE CID-CLI             OF CLIEV008
                                       TO WRK-CID-CLI
              MOVE DABERT-CTA OF CLIEV008(1:2)
                                      TO WRK-DATA-AMD-DIA-X
              MOVE DABERT-CTA OF CLIEV008(4:2)
                                      TO WRK-DATA-AMD-MES-X
              MOVE DABERT-CTA OF CLIEV008(7:4)
                                      TO WRK-DATA-AMD-ANO-X
              MOVE WRK-DATA-AMD       TO WRK-DABERT-CTA
           END-IF.

      *----------------------------------------------------------------*
       3700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3800-ACESSAR-CPF                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CID-CLI            TO CID-CLI OF CLIEV004.

ST25X6*      EXEC SQL
ST25X6*        SELECT CCGC_CPF,
ST25X6*               CFLIAL_CGC,
ST25X6*               CCTRL_CPF_CGC
ST25X6*        INTO  :CLIEV004.CCGC-CPF,
ST25X6*              :CLIEV004.CFLIAL-CGC,
ST25X6*              :CLIEV004.CCTRL-CPF-CGC
ST25X6*        FROM   DB2PRD.V01CLIENTE_AGENCIA
ST25X6*        WHERE CID_CLI            = :CLIEV004.CID-CLI
ST25X6*      END-EXEC.

ST25X6       EXEC SQL
ST25X6         SELECT CCGC_CPF,
ST25X6                CFLIAL_CGC,
ST25X6                CCTRL_CPF_CGC,
ST25X6                CCGC_CPF_ST,
ST25X6                CFLIAL_CGC_ST,
ST25X6                CCTRL_CPF_CGC_ST
ST25X6         INTO  :CLIEV004.CCGC-CPF,
ST25X6               :CLIEV004.CFLIAL-CGC,
ST25X6               :CLIEV004.CCTRL-CPF-CGC,
ST25X6               :CLIEV004.CCGC-CPF-ST,
ST25X6               :CLIEV004.CFLIAL-CGC-ST,
ST25X6               :CLIEV004.CCTRL-CPF-CGC-ST
ST25X6         FROM   DB2PRD.V01CLIENTE_AGENCIA
ST25X6         WHERE CID_CLI            = :CLIEV004.CID-CLI
ST25X6       END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'           )
281016          DISPLAY 'AG/CTA ' WRK-GECT-AGENCIA-ATU '/'
281016                            WRK-GECT-CONTA-ATU
              SET DB2-SELECT           TO TRUE
              MOVE '0060'              TO FRWKGDB2-LOCAL
              MOVE 'V01CLIENTE_AGENCIA'
                                       TO FRWKGDB2-NOME-TABELA
              SET  ERRO-DB2            TO TRUE
              SET  WRK-ABENDAR         TO TRUE
              MOVE SQLCODE             TO WRK-SQLCODE
              PERFORM 9400-ERRO-DB2
           END-IF.

           IF SQLCODE                  EQUAL +100
              MOVE '3'                 TO WRK-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *      OBTER QTDE DE MESES DE TEST DRIVE DA TABELA GFCTB023      *
      *----------------------------------------------------------------*
       3900-OBTER-QTDE-MESES           SECTION.
      *----------------------------------------------------------------*

           MOVE 008                    TO CTPO-BONIF      OF GFCTB023.
           MOVE 010                    TO CCRITR-BONIF    OF GFCTB023.

           EXEC SQL
              SELECT QFRANQ_BONIF
              INTO   :WRK-QTD-MESES-ISENC
              FROM DB2PRD.BONIF_SERVC_TARIF
              WHERE CSERVC_TARIF      >  0
               AND  DINIC_REGRA_BONIF <= :WRK-DINIC-PERIODO
               AND  CTPO_BONIF        =  :GFCTB023.CTPO-BONIF
               AND  CCRITR_BONIF      =  :GFCTB023.CCRITR-BONIF
               AND  DFIM_REGRA_BONIF  >= :WRK-DINIC-PERIODO
               FETCH FIRST 1 ROWS ONLY
           END-EXEC.

           IF (SQLCODE          NOT EQUAL ZEROS AND +100 AND -305) OR
               (SQLWARN0               EQUAL 'W'      )
                SET DB2-SELECT         TO TRUE
                MOVE '0070'            TO FRWKGDB2-LOCAL
                MOVE 'BONIF_SERVC_TARIF'
                                       TO FRWKGDB2-NOME-TABELA
                SET  ERRO-DB2          TO TRUE
                SET  WRK-ABENDAR       TO TRUE
                MOVE SQLCODE           TO WRK-SQLCODE
                PERFORM 9400-ERRO-DB2
           END-IF.

           IF SQLCODE                  EQUAL 100
              MOVE ZEROS               TO WRK-QTD-MESES-ISENC
           ELSE
              IF WRK-QTD-MESES-ISENC   GREATER 9 OR
                 WRK-QTD-MESES-ISENC   LESS 2
                 DISPLAY '============================================='
                 DISPLAY 'QTDE MESES TEST DRIVE (GFCTB023) INVALIDA '
                           ' - ASSUMIDO 2 MESES'
                 DISPLAY '============================================='
                 MOVE 2                TO WRK-QTD-MESES-ISENC
              ELSE
                 DISPLAY 'QTDE MESES TEST DRIVE ' WRK-QTD-MESES-ISENC
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       3900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *       GRAVA ARQUIVO DE REJEITADOS/INCONSISTENTES               *
      *----------------------------------------------------------------*
       4000-GRAVA-CTAREJES             SECTION.
      *----------------------------------------------------------------*

           MOVE '4000'                     TO WRK-LOCAL.
           MOVE 'GRAVA-CTAREJES'           TO FRWKGHEA-IDEN-PARAGRAFO.

           MOVE WRK-GECT-AGENCIA-ATU       TO WRK-S9-5
           MOVE WRK-9-5                    TO P17-AGENCIA

           MOVE WRK-GECT-CONTA-ATU         TO WRK-S9-7
           MOVE WRK-9-7                    TO P17-CONTA

           MOVE WRK-GECT-DTINCL-ATU        TO WRK-DATA-S9
           MOVE WRK-DATA-8-ANO             TO WRK-DATA-X10-ANO
           MOVE WRK-DATA-8-MES             TO WRK-DATA-X10-MES
           MOVE WRK-DATA-8-DIA             TO WRK-DATA-X10-DIA
           MOVE WRK-DATA-X10               TO P17-DT-ENCART

           IF   WRK-ERRO = '3'
             MOVE ZEROS                    TO P17-CCGC-CPF
             MOVE ZEROS                    TO P17-CFLIAL-CGC
             MOVE ZEROS                    TO P17-CCTRL-CPF-CGC
           ELSE
ST25X6*      MOVE CCGC-CPF       OF CLIEV004 TO WRK-S9-9
ST25X6*      MOVE WRK-9-9                    TO P17-CCGC-CPF
ST25X6*      MOVE CFLIAL-CGC     OF CLIEV004 TO WRK-S9-4
ST25X6*      MOVE WRK-9-4                    TO P17-CFLIAL-CGC
ST25X6*      MOVE CCTRL-CPF-CGC  OF CLIEV004 TO WRK-S9-2
ST25X6*      MOVE WRK-9-2                    TO P17-CCTRL-CPF-CGC

ST25X6       MOVE CCGC-CPF-ST    OF CLIEV004 TO P17-CCGC-CPF
ST25X6       MOVE CFLIAL-CGC-ST  OF CLIEV004 TO P17-CFLIAL-CGC
ST25X6       MOVE CCTRL-CPF-CGC-ST OF CLIEV004 TO WRK-S9-2
ST25X6       MOVE WRK-9-2                    TO P17-CCTRL-CPF-CGC
           END-IF.

           IF   WRK-ERRO = '1'
                MOVE 'CONTA POUPANCA NAO ENCONTRADA'
                                           TO P17-DESC-ERRO
           ELSE
           IF WRK-ERRO = '2'
                MOVE 'CONTA CORRENTE NAO ENCONTRADA'
                                           TO P17-DESC-ERRO
           ELSE
           IF WRK-ERRO = '3'
                MOVE 'DADOS CLIENTE NAO ENCONTRADO'
                                           TO P17-DESC-ERRO
           END-IF
           END-IF
           END-IF.

           WRITE FD-CTAREJES               FROM GFCTWP17-REG.

           SET ARQ-WRITE                   TO TRUE.

           ADD 1                           TO ACU-GRAVA-CTAREJES.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *   GRAVA ARQUIVO COM CONTAS COM DIREITO A TEST DRIVE            *
      *----------------------------------------------------------------*
       4200-GRAVA-CTAEXCLS             SECTION.
      *----------------------------------------------------------------*

           MOVE '4200'                     TO WRK-LOCAL.
           MOVE 'GRAVA-CTAEXCLS'           TO FRWKGHEA-IDEN-PARAGRAFO.

           MOVE WRK-GECT-AGENCIA-ATU       TO WRK-S9-5
           MOVE WRK-9-5                    TO P16-AGENCIA

           MOVE WRK-GECT-CONTA-ATU         TO WRK-S9-7
           MOVE WRK-9-7                    TO P16-CONTA

           MOVE WRK-GECT-DTINCL-ATU        TO WRK-DATA-S9
           MOVE WRK-DATA-8-ANO             TO WRK-DATA-X10-ANO
           MOVE WRK-DATA-8-MES             TO WRK-DATA-X10-MES
           MOVE WRK-DATA-8-DIA             TO WRK-DATA-X10-DIA
           MOVE WRK-DATA-X10               TO P16-DT-ENCART

ST25X6*    MOVE CCGC-CPF       OF CLIEV004 TO WRK-S9-9.
ST25X6*    MOVE WRK-9-9                    TO P16-CCGC-CPF
ST25X6  
ST25X6*    MOVE CFLIAL-CGC     OF CLIEV004 TO WRK-S9-4.
ST25X6*    MOVE WRK-9-4                    TO P16-CFLIAL-CGC

ST25X6*    MOVE CCTRL-CPF-CGC  OF CLIEV004 TO WRK-S9-2.
ST25X6*    MOVE WRK-9-2                    TO P16-CCTRL-CPF-CGC

ST25X6     MOVE CCGC-CPF-ST    OF CLIEV004 TO P16-CCGC-CPF
ST25X6  
ST25X6     MOVE CFLIAL-CGC-ST  OF CLIEV004 TO P16-CFLIAL-CGC

ST25X6     MOVE CCTRL-CPF-CGC-ST OF CLIEV004 TO WRK-S9-2.
ST25X6     MOVE WRK-9-2                    TO P16-CCTRL-CPF-CGC

           MOVE WRK-DABERT-CTA             TO WRK-DATA-S9
           MOVE WRK-DATA-8-ANO             TO WRK-DATA-X10-ANO
           MOVE WRK-DATA-8-MES             TO WRK-DATA-X10-MES
           MOVE WRK-DATA-8-DIA             TO WRK-DATA-X10-DIA
           MOVE WRK-DATA-X10               TO P16-DT-ABERT

           MOVE WRK-CSGMTO-CLI             TO WRK-S9-3
           MOVE WRK-9-3                    TO P16-SEGM

           MOVE WRK-TIPO-PESSOA            TO P16-TP-PESSOA

           MOVE CSERVC-TARIF OF GFCTB009   TO WRK-S9-5
           MOVE WRK-9-5                    TO P16-CPCOTE-TARIF-VIG

           MOVE DINIC-ADSAO-INDVD OF GFCTB009
                                           TO P16-DINIC-ADSAO-INDVD

           MOVE DFIM-ADSAO-INDVD OF GFCTB009
                                           TO P16-DFIM-ADSAO-INDVD

           MOVE WRK-CESTA-ANT              TO WRK-S9-5
           MOVE WRK-9-5                    TO P16-CPCOTE-TARIF-ANT

           MOVE WRK-DATA-PROC-AMD          TO WRK-9-9
           MOVE WRK-9-8                    TO P16-DT-ATUALIZ

           MOVE WRK-TP-DESCONTO            TO P16-TP-DESCONTO.
           MOVE WRK-VALOR-LIQUIDO          TO WRK-S9-9V99
           MOVE WRK-9-9V99                 TO P16-VALOR-LIQUIDO.

           WRITE FD-CTAEXCLS               FROM GFCTWP16-REG.

           SET ARQ-WRITE                   TO TRUE.

           ADD 1                           TO ACU-GRAVA-CTAEXCLS.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7000-INICIAR-CKRS0105           SECTION.
      *----------------------------------------------------------------*

           MOVE '7000-INICIAR-CKRS0105' TO FRWKGHEA-IDEN-PARAGRAFO.

           MOVE 'C'                    TO PARM-OP.
           MOVE 'DB2'                  TO PARM-SSID.
           MOVE SPACES                 TO PARM-PLAN.

           CALL WRK-CKRS0105           USING PARM-CKRS0105.

      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7100-ENCERRAR-CKRS0105          SECTION.
      *----------------------------------------------------------------
           MOVE '7100-ENCERRAR-CKRS0105'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           MOVE 'D'                    TO PARM-OP.
           MOVE SPACES                 TO PARM-PLAN.

           CALL WRK-CKRS0105           USING PARM-CKRS0105.

      *----------------------------------------------------------------*
       7100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA QUE EFETUA PROCESSAMENTO FINAL DO PROGRAMA           *
      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 7100-ENCERRAR-CKRS0105.

           CLOSE CTRLDATA
                 ARQGECTE
                 CTAEXCLS
                 CTAREJES.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
      *     SE NAO FOR NA VIRADA DO MES, GERA MENSAGEM DE AVISO        *
      *----------------------------------------------------------------*

           IF   GFCT06-DPROCM-ATUAL(4:2)
                                       EQUAL GFCT06-DPROCM-PROX(4:2)
             DISPLAY ' '
             DISPLAY ' '
             DISPLAY '=========== GFCT4851 ==========================='
             DISPLAY 'PROCESSAMENTO SO DEVE OCORRER NA VIRADA DO MES '
             DISPLAY '==============================================='
             DISPLAY ' '
             MOVE ZEROS              TO RETURN-CODE
           ELSE
             DISPLAY ' '
             DISPLAY '=========== GFCT4851 ==========================='
             MOVE ACU-LIDOS-ARQGECTE     TO WRK-EDIT
             DISPLAY 'ARQGECTE - LIDOS                    ' WRK-EDIT
             MOVE ACU-GECT-DESPREZADOS   TO WRK-EDIT
             DISPLAY '  - DESPREZADOS (DT.ENCART.INV.)    ' WRK-EDIT
             MOVE ACU-SEM-ADESAO         TO WRK-EDIT
             DISPLAY '  - SEM ADESAO A CESTA EXCLUSIVE    ' WRK-EDIT
             MOVE ACU-GECT-DUPLICADOS    TO WRK-EDIT
             DISPLAY '  - AG/CTA DUPLICADA (DESPREZADO)   ' WRK-EDIT
             MOVE ACU-NAO-TEST-DRIVE    TO WRK-EDIT
             DISPLAY '  - NAO TEM DIREITO A TEST DRIVE    ' WRK-EDIT
             MOVE ACU-GRAVA-CTAEXCLS     TO WRK-EDIT
             DISPLAY 'CTAEXCLS - GRAVADOS (TEST DRIVE)    ' WRK-EDIT
             MOVE ACU-ISENTO-REGRA1      TO WRK-EDIT
             DISPLAY '  - ISENTOS (REGRA 1)               ' WRK-EDIT
             MOVE ACU-ISENTO-REGRA2      TO WRK-EDIT
             DISPLAY '  - ISENTOS (REGRA 2)               ' WRK-EDIT
             MOVE ACU-DESCONTO-REGRA3    TO WRK-EDIT
             DISPLAY '  - DESCONTOS (REGRA3)              ' WRK-EDIT
             MOVE ACU-GRAVA-CTAREJES     TO WRK-EDIT
             DISPLAY 'CTAREJES - GRAVADOS (ERRO)          ' WRK-EDIT
             DISPLAY '=========== GFCT4851 ==========================='
             DISPLAY ' '
           END-IF.

           STOP RUN.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9100-TRATAR-ERRO-ARQUIVO        SECTION.
      *----------------------------------------------------------------*

           SET ERRO-ARQUIVO            TO TRUE.

           MOVE FRWKGARQ-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.
           MOVE WRK-AREA-ERRO-ARQUIVO  TO WRK-BLOCO-INFO-ERRO(1:29).

           PERFORM 9900-TRATAR-ERRO.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9400-ERRO-DB2                   SECTION.
      *----------------------------------------------------------------*

           SET  ERRO-DB2               TO TRUE.
           MOVE FRWKGDB2-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.

           MOVE SQLCA                  TO FRWKGDB2-SQLCA.
           MOVE WRK-SQLCODE            TO FRWKGDB2-SQLCODE.
           MOVE SQLSTATE               TO FRWKGDB2-SQLSTATE.

           MOVE WRK-AREA-ERRO-DB2      TO WRK-BLOCO-INFO-ERRO.

           PERFORM 9900-TRATAR-ERRO.

      *----------------------------------------------------------------*
       9400-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
       9900-TRATAR-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT4851'             TO FRWKGHEA-NOME-PROGRAMA.

           PERFORM 9990-GRAVAR-LOG-ERRO.

           PERFORM 9999-ABENDAR-PROGRAMA.

           GOBACK.

      *----------------------------------------------------------------*
       9900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9990-GRAVAR-LOG-ERRO            SECTION.
      *----------------------------------------------------------------*

           CALL WRK-FRWK2999           USING WRK-AREA-ERRO.

      *----------------------------------------------------------------*
       9990-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ABENDAR-PROGRAMA           SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0450'             USING WRK-0450-ABEND-BAT
                                             WRK-0450-DUMP-BAT.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
