      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
      *
       PROGRAM-ID. GFCT4853.
       AUTHOR.     FABRICA TI.
      *================================================================*
      *                        S O N D A   I T                         *
      *================================================================*
      *                                                                *
      *    PROGRAMA....:   GFCT4853                                    *
      *    PROGRAMADOR.:   REGINA              - SONDA IT              *
      *    ANALISTA....:   REGINA              - SONDA IT              *
      *    DATA........:   JUNHO/2016                                  *
      *                                                                *
      *    OBJETIVO....:   GERAR RELATORIO DE CONTAS COM CESTA         *
      *                    EXCLUSIVE QUE TEM DIREITO A TEST DRIVE      *
      *                    (DESCONTO/ISENCAO).                         *
      *                                                                *
      * *------------------------------------------------------------* *
      * *------------------------------------------------------------* *
      * | ARQUIVO  |  BOOKS   | LRECL |DESCRICAO                     | *
      * *----------+----------+-------+------------------------------* *
      * | CTAEXCLE | GFCTWP16 | 0100  |CTAS TEST DRIVE CESTA EXCLUS. | *
      * | RELATO   | -------- | 0133  |RELATORIO FLEXIBILIZACAO      | *
      * *------------------------------------------------------------* *
      *                                                                *
      * *------------------------------------------------------------* *
      * |       TABELAS       |  DCLGEN  |DESCRICAO                  | *
      * *---------------------+-------+------------------------------* *
      * | THIST_FLEXZ_CONTR   | GFCTB05L | HISTORICO FLEX TARIFA     | *
      * *------------------------------------------------------------* *
      *                                                                *
      * *------------------------------------------------------------* *
      * | MODULOS.....:                                              | *
      * | CKRS1000 - INICIAR PROCESSAMENTO                           | *
      * | CKRS0105 - INICIAR E FINALIZAR CONEXAO DB2                 | *
      * | FRWK2999 - TRATAMENTO DE ERRO                              | *
      * | BOOKS ......:                                              | *
      * |            I#CKRS04 - BOOK DE CONEXAO DB2                  | *
      * |            I#FRWKDB - TRATAMENTO ERRO DE DB2               | *
      * |            I#FRWKAR - TRATAMENTO ERRO DE ARQUIVO           | *
      * |            I#FRWKMD - TRATAMENTO ERRO DE MODULO            | *
      * |            I#FRWKGE - TRATAMENTO ERRO DE MODULO            | *
      * *------------------------------------------------------------* *
      *                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT CTAEXCLE ASSIGN      TO UT-S-CTAEXCLE
                      FILE STATUS      IS WRK-FS-CTAEXCLE.

           SELECT RELATO   ASSIGN      TO UT-S-RELATO
                      FILE STATUS      IS WRK-FS-RELATO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     INPUT: CONTAS DE TEST DRIVE CESTA EXCLUSIVE                *
      *            ORG. SEQUENCIAL                    LRECL = 110      *
      *----------------------------------------------------------------*

       FD  CTAEXCLE
           BLOCK 0
           RECORDING F
           LABEL RECORD IS STANDARD.

       01  FD-CTAEXCLE                 PIC X(110).

      *----------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO NAO MIGRADOS                                *
      *            ORG. SEQUENCIAL                       LRECL = 133   *
      *----------------------------------------------------------------*

       FD  RELATO
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD IS STANDARD.

       01  FD-RELATO                   PIC  X(133).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER PIC X(050) VALUE '* INICIO DA WORKING - GFCT4853 *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER PIC X(050) VALUE '* AREA FILE STATUS *'.
      *----------------------------------------------------------------*

       77  WRK-FS-CTAEXCLE             PIC X(002) VALUE SPACES.
       77  WRK-FS-RELATO               PIC X(002) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER PIC X(050) VALUE '* AREA DE ACUMULADORES *'.
      *----------------------------------------------------------------*

       01  ACU-LIDOS-CTAEXCLE          PIC 9(009) COMP-3 VALUE ZEROS.
       01  ACU-GRA-RELATO              PIC 9(009) COMP-3 VALUE ZEROS.
       01  ACU-PAGINA                  PIC 9(003) COMP-3 VALUE ZEROS.
       01  WRK-LINHAS                  PIC 9(003) COMP-3 VALUE 99.

      *----------------------------------------------------------------*
       01  FILLER PIC X(050) VALUE '* AREAS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-ABEND-BAT               PIC S9(04) COMP VALUE +1111.
       77  WRK-DUMP-BAT                PIC X(001) VALUE 'S'.
       77  WRK-SQLCODE                 PIC S9(09) COMP-3 VALUE ZEROS.
       77  WRK-PROGRAMA                PIC X(008) VALUE 'GFCT4853'.

       01  WRK-ABEND                   PIC X(001) VALUE SPACES.
           88 WRK-ABENDAR                         VALUE 'S'.

       01  WRK-DATA-PROC               PIC X(010) VALUE SPACES.
       01  FILLER
           REDEFINES WRK-DATA-PROC.
           03 WRK-DIA-PROC             PIC 9(002).
           03 FILLER                   PIC X(001).
           03 WRK-MES-PROC             PIC 9(002).
           03 FILLER                   PIC X(001).
           03 WRK-ANO-PROC             PIC 9(004).

       01  WRK-HORA-PROC               PIC X(008) VALUE SPACES.
       01  FILLER
           REDEFINES WRK-HORA-PROC.
           03 WRK-HOR-PROC             PIC 9(002).
           03 FILLER                   PIC X(001).
           03 WRK-MIN-PROC             PIC 9(002).
           03 FILLER                   PIC X(001).
           03 WRK-SEG-PROC             PIC 9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA LAYOUT DOS CABECALHOS'.
      *----------------------------------------------------------------*

       01 CABEC1-REL1-1.
           03  CB1-CARRO            PIC X(001) VALUE '1'.
           03  CB1-DIA              PIC 9(002) VALUE ZEROS.
           03  FILLER               PIC X(001) VALUE '/'.
           03  CB1-MES              PIC 9(002) VALUE ZEROS.
           03  FILLER               PIC X(001) VALUE '/'.
           03  CB1-ANO              PIC 9(004) VALUE ZEROS.
           03  FILLER               PIC X(034) VALUE SPACES.
           03  CB1-EMPR-CONGL       PIC X(080) VALUE
               '        B A N C O  B R A D E S C O  S/A'.
           03  CB1-HORA             PIC 9(002) VALUE ZEROS.
           03  FILLER               PIC X(001) VALUE ':'.
           03  CB1-MINUTO           PIC 9(002) VALUE ZEROS.
           03  FILLER               PIC X(001) VALUE ':'.
           03  CB1-SEGUNDO          PIC 9(002) VALUE ZEROS.

       01  CABEC2-REL1-2.
           03  CB2-CARRO               PIC X(001) VALUE ' '.
           03  FILLER                  PIC X(042) VALUE 'GFCT4853'.
           03  FILLER                  PIC X(079) VALUE
               'GFCT - GESTAO, FLEXIBILIZACAO E COBRANCA DE TARIFAS'.
           03  FILLER                  PIC X(005) VALUE 'PAG. '.
           03  CB2-CONT-PAG            PIC ZZ.ZZ9 VALUE ZEROS.

       01  CABEC3-REL1-3.
           03  CB3-CARRO               PIC X(001) VALUE ' '.
           03  FILLER                  PIC X(016) VALUE SPACES.
           03  FILLER                  PIC X(115) VALUE
           'RELATORIO DE CONTAS COM DIREITO AO BENEFICIO DE ISENCAO OU D
      -    'ESCONTO NA MENSALIDADE DE CESTA EXCLUSIVE'.

       01  CABEC4-REL1-4.
           03  CB4-CARRO               PIC X(001) VALUE ' '.
           03  FILLER                  PIC X(132) VALUE ALL '-'.

       01  CABEC5-REL1-5.
           03 FILLER                   PIC  X(001)  VALUE ' '.
           03 FILLER                   PIC  X(132)  VALUE
              ' AGENCIA/CONTA      CPF      DT.ENCART.  SEGTO   TP  DT.A
      -       'BERTURA  CESTA   PERIODO VIGENCIA    CESTA ANTERIOR
      -       '        BENEFICIO'.

       01  CABEC6-REL1-6.
           03 FILLER                   PIC  X(001)  VALUE ' '.
           03 FILLER                   PIC  X(131)  VALUE
              ' -------------  ------------ ----------  -----   --  ----
      -       '-------  ----- --------------------- --------------------
      -       '------- ---------'.

       01  LD1-LINDET.
           03  FILLER                  PIC  X(002)  VALUE ' '.
           03  LD1-AGENCIA             PIC  9(005)  VALUE ZEROS.
           03  FILLER                  PIC  X(001)  VALUE '/'.
           03  LD1-CONTA               PIC  9(007)  VALUE ZEROS.
           03  FILLER                  PIC  X(002)  VALUE SPACES.
ST25X6*     03  LD1-CPF                 PIC  9(009)  VALUE ZEROS.
ST25X6      03  LD1-CPF                 PIC  X(009)  VALUE SPACES.
           03  FILLER                  PIC  X(001)  VALUE '-'.
ST25X6*     03  LD1-CPF-CTRL            PIC  9(002)  VALUE ZEROS.
ST25X6      03  LD1-CPF-CTRL            PIC  X(002)  VALUE SPACES.
           03  FILLER                  PIC  X(001)  VALUE SPACES.
           03  LD1-DT-ENCART           PIC  X(010)  VALUE SPACES.
           03  FILLER                  PIC  X(003)  VALUE SPACES.
           03  LD1-SEGTO               PIC  9(003)  VALUE ZEROS.
           03  FILLER                  PIC  X(004)  VALUE SPACES.
           03  LD1-TP-PESSOA           PIC  X(001)  VALUE ZEROS.
           03  FILLER                  PIC  X(003)  VALUE SPACES.
           03  LD1-DT-ABERT            PIC  X(010)  VALUE SPACES.
           03  FILLER                  PIC  X(003)  VALUE SPACES.
           03  LD1-CESTA-VIG           PIC  9(005)  VALUE ZEROS.
           03  FILLER                  PIC  X(001)  VALUE SPACES.
           03  LD1-DINIC-VGCIA-ADSAO   PIC  X(010)  VALUE SPACES.
           03  FILLER                  PIC  X(001)  VALUE SPACES.
           03  LD1-DFIM-VGCIA-ADSAO    PIC  X(010)  VALUE SPACES.
           03  FILLER                  PIC  X(001)  VALUE SPACES.
           03  LD1-CESTA-ANT-X         PIC  X(027)  VALUE SPACES.
           03  FILLER REDEFINES LD1-CESTA-ANT-X.
            05 LD1-CESTA-ANT           PIC  9(005).
            05 LD1-CESTA-ANT-TRACO     PIC  X(001).
            05 LD1-DESC-CESTA-ANT      PIC  X(021).
           03  FILLER                  PIC  X(001)  VALUE SPACES.
           03  LD1-BENEFICIO           PIC  X(008)  VALUE SPACES.

       01  LINTOT1-REL1-TOT1.
           03  FILLER                  PIC  X(001)  VALUE '0'.
           03  FILLER                  PIC  X(027)  VALUE
               ' TOTAL DE CONTAS  .......'.
           03  LT1-REL1-TOT-CONTAS     PIC  ZZZ.ZZZ.ZZ9 VALUE ZEROS.
           03  FILLER                  PIC  X(030)  VALUE SPACES.

       01  WRK-REL-VAZIO.
           05 FILLER                   PIC  X(001)  VALUE '0'.
           05 FILLER                   PIC  X(019)  VALUE SPACES.
           05 FILLER                   PIC  X(039)  VALUE
              '**** NAO HA DADOS A SEREM EXIBIDOS ****'.

      *----------------------------------------------------------------*
       01  FILLER PIC X(050) VALUE '* AREA PARA MODULOS *'.
      *----------------------------------------------------------------*

       77  WRK-GFCT4853                PIC X(008) VALUE 'GFCT4853'.
       77  WRK-FRWK2999                PIC X(008) VALUE 'FRWK2999'.

      *----------------------------------------------------------------*
       01  FILLER PIC X(050) VALUE '* AREA DA API - FRWK2999 *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO.
              10 WRK-CHAR-INFO-ERRO    PIC X(001)
                                OCCURS  0 TO 30000 TIMES
                                DEPENDING ON FRWKGHEA-TAM-DADOS.

      *----------------------------------------------------------------*
       01  FILLER PIC X(050) VALUE '* INFORMACOES DE ERRO - ARQUIVO *'.
      *----------------------------------------------------------------*

       01 WRK-AREA-ERRO-ARQ.
          COPY 'I#FRWKAR'.

      *----------------------------------------------------------------*
       01  FILLER PIC X(050) VALUE '* INFORMACOES DE ERRO - MODULO *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-ERRO-MOD.
           COPY 'I#FRWKMD'.

      *----------------------------------------------------------------*
       01  FILLER PIC X(050) VALUE '* INFORMACOES DE ERRO - DB2 *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-ERRO-DB2.
           COPY 'I#FRWKDB'.

      *----------------------------------------------------------------*
       01  FILLER PIC X(050) VALUE '* AREA DO ARQUIVOS CTAEXCLE *'.
      *----------------------------------------------------------------*

           COPY 'GFCTWP16'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM CKRS0105           ***'.
      *----------------------------------------------------------------*

       77  WRK-CKRS0105                PIC  X(008)         VALUE
           'CKRS0105'.

           COPY 'I#CKRS04'.
      *----------------------------------------------------------------*
       01  FILLER PIC X(050) VALUE '* AREA PARA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0D8
           END-EXEC.

      *----------------------------------------------------------------*
       77  FILLER PIC X(050) VALUE '* FIM DA WORKING - GFCT4853 *'.
      *----------------------------------------------------------------*
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      * ROTINA PRINCIPAL                                               *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           MOVE '0000-ROTINA-PRINCIPAL'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 1000-INICIALIZA.

           PERFORM 2000-VERIFICA-VAZIO.

           PERFORM 3000-PROCESSA       UNTIL
                   WRK-FS-CTAEXCLE     EQUAL '10'.

           PERFORM 4000-FINALIZA.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * INICIALIZA AREAS DO PROGRAMA                                   *
      *----------------------------------------------------------------*
       1000-INICIALIZA                 SECTION.
      *----------------------------------------------------------------*

           MOVE '1000-INICIALIZA'      TO FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 1050-ABRIR-CONEXAO.

           INITIALIZE FRWKGHEA-REGISTRO
                      FRWKGARQ-REGISTRO.

           OPEN  INPUT CTAEXCLE
                OUTPUT RELATO.

           SET ARQ-OPEN                TO TRUE.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 1200-OBTER-DATA-PROCESSAMENTO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * FAZ CONEXAO COM DB2                                            *
      *----------------------------------------------------------------*
       1050-ABRIR-CONEXAO              SECTION.
      *----------------------------------------------------------------*

           MOVE '1050-ABRIR-CONEXAO'   TO FRWKGHEA-IDEN-PARAGRAFO.

           CALL 'CKRS1000'.

           MOVE 'C'                    TO PARM-OP.
           MOVE 'DB2'                  TO PARM-SSID.
           MOVE SPACES                 TO PARM-PLAN.

           CALL WRK-CKRS0105           USING PARM-CKRS0105.

      *----------------------------------------------------------------*
       1050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTA FILE STATUS DOS ARQUIVOS                                 *
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           MOVE '1100-TESTAR-FILE-STATUS'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 1110-TESTAR-FS-CTAEXCLE.

           PERFORM 1130-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTA FILE STATUS DO ARQUIVO CTAEXCLE                          *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-CTAEXCLE         SECTION.
      *----------------------------------------------------------------*

           MOVE '1110-TESTAR-FS-CTAEXCLE'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           IF WRK-FS-CTAEXCLE          NOT EQUAL '00' AND '10'
              MOVE 'CTAEXCLE'          TO FRWKGARQ-NOME-ARQUIVO
              MOVE WRK-FS-CTAEXCLE     TO FRWKGARQ-FILE-STATUS
              SET  WRK-ABENDAR         TO TRUE
              SET  ERRO-ARQUIVO        TO TRUE
              PERFORM 9000-ERRO-ARQ
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * TESTA FILE STATUS DO ARQUIVO RELATO                            *
      *----------------------------------------------------------------*
       1130-TESTAR-FS-RELATO           SECTION.
      *----------------------------------------------------------------*

           MOVE '1130-TESTAR-FS-RELATO  '
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           IF WRK-FS-RELATO            NOT EQUAL '00'
              MOVE 'RELATO  '          TO FRWKGARQ-NOME-ARQUIVO
              MOVE WRK-FS-RELATO       TO FRWKGARQ-FILE-STATUS
              SET  WRK-ABENDAR         TO TRUE
              SET  ERRO-ARQUIVO        TO TRUE
              PERFORM 9000-ERRO-ARQ
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-OBTER-DATA-PROCESSAMENTO   SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-DATA-PROC
                      WRK-HORA-PROC.
      *
           EXEC SQL
                SET :WRK-DATA-PROC     = CURRENT DATE
           END-EXEC.
      *
           EXEC SQL
                SET :WRK-HORA-PROC     = CURRENT TIME
           END-EXEC.

           MOVE WRK-HOR-PROC           TO CB1-HORA.
           MOVE WRK-MIN-PROC           TO CB1-MINUTO.
           MOVE WRK-SEG-PROC           TO CB1-SEGUNDO.

           MOVE WRK-DIA-PROC           TO CB1-DIA.
           MOVE WRK-MES-PROC           TO CB1-MES.
           MOVE WRK-ANO-PROC           TO CB1-ANO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * VERIFICA SE ARQUIVO ESTA VAZIO                                 *
      *----------------------------------------------------------------*
       2000-VERIFICA-VAZIO             SECTION.
      *----------------------------------------------------------------*

           MOVE '2000-VERIFICA-VAZIO'  TO FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 2100-LER-CTAEXCLE.

           IF WRK-FS-CTAEXCLE          EQUAL '10'
              DISPLAY '************ GFCT4853 ************'
              DISPLAY '*                                *'
              DISPLAY '*     ARQUIVO CTAEXCLE VAZIO     *'
              DISPLAY '*       PROGRAMA ENCERRADO       *'
              DISPLAY '*                                *'
              DISPLAY '************ GFCT4853 ************'
              PERFORM 4000-FINALIZA
           END-IF.


      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * LEITURA DO ARQUIVO CTAEXCLE                                    *
      *----------------------------------------------------------------*
       2100-LER-CTAEXCLE               SECTION.
      *----------------------------------------------------------------*

           MOVE '2100-LER-CTAEXCLE'    TO FRWKGHEA-IDEN-PARAGRAFO.

           SET ARQ-READ                TO TRUE.

           READ CTAEXCLE               INTO GFCTWP16-REG.

           PERFORM 1110-TESTAR-FS-CTAEXCLE.

           IF   WRK-FS-CTAEXCLE        EQUAL '10'
             GO TO 2100-99-FIM
           END-IF.

           ADD    +1                   TO ACU-LIDOS-CTAEXCLE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PROCESSAMENTO DO PROGRAMA                                      *
      *----------------------------------------------------------------*
       3000-PROCESSA                   SECTION.
      *----------------------------------------------------------------*

           MOVE '3000-PROCESSA'        TO FRWKGHEA-IDEN-PARAGRAFO.

           IF P16-CPCOTE-TARIF-ANT     NOT EQUAL ZEROS
             PERFORM 3100-OBTER-DESCR-CESTA
           END-IF.

           PERFORM 3700-FORMATAR-RELATO.

           PERFORM 2100-LER-CTAEXCLE.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTEM ADESAO MAIS RECENTE DO GFCT (COM MAIOR DATA INICIO VIG)  *
      *----------------------------------------------------------------*
       3100-OBTER-DESCR-CESTA  SECTION.
      *----------------------------------------------------------------*

           MOVE P16-CPCOTE-TARIF-ANT   TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
             SELECT RSERVC_TARIF_REDZD
               INTO :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM DB2PRD.SERVC_TARIF_PRINC
               WHERE CSERVC_TARIF = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF SQLCODE                  NOT EQUAL ZEROS AND +100
              MOVE 'SERVC_TARIF_PRINC' TO FRWKGDB2-NOME-TABELA
              MOVE '3101'              TO FRWKGMOD-COD-ERRO
              SET  DB2-SELECT          TO TRUE
              MOVE SQLCODE             TO WRK-SQLCODE
              PERFORM 9400-ERRO-DB2
           END-IF.

           IF SQLCODE                  EQUAL +100
              MOVE 'DESCRICAO INEXISTENTE'  TO LD1-DESC-CESTA-ANT
           ELSE
              MOVE RSERVC-TARIF-REDZD(1:21) TO LD1-DESC-CESTA-ANT
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3700-FORMATAR-RELATO            SECTION.
      *----------------------------------------------------------------*

           MOVE '3700-FORMATAR-RELATO' TO FRWKGHEA-IDEN-PARAGRAFO.

           IF WRK-LINHAS               GREATER 57
              PERFORM 3710-IMPRIMIR-CABEC
           END-IF.

           PERFORM 3720-IMPRIMIR-LINDET.

      *----------------------------------------------------------------*
       3700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3710-IMPRIMIR-CABEC             SECTION.
      *----------------------------------------------------------------*

           MOVE '3600-IMPRIMIR-CABEC'  TO FRWKGHEA-IDEN-PARAGRAFO.

           MOVE ZEROS                  TO WRK-LINHAS.
           ADD 1                       TO ACU-PAGINA.
           MOVE ACU-PAGINA             TO CB2-CONT-PAG.

           MOVE CABEC1-REL1-1          TO FD-RELATO.
           PERFORM 3800-GRAVA-RELATO.

           MOVE CABEC2-REL1-2          TO FD-RELATO.
           PERFORM 3800-GRAVA-RELATO.

           MOVE CABEC3-REL1-3          TO FD-RELATO.
           PERFORM 3800-GRAVA-RELATO.

           MOVE CABEC4-REL1-4          TO FD-RELATO.
           PERFORM 3800-GRAVA-RELATO.

           MOVE SPACES                 TO FD-RELATO.
           PERFORM 3800-GRAVA-RELATO.

           MOVE CABEC5-REL1-5          TO FD-RELATO.
           PERFORM 3800-GRAVA-RELATO.

           MOVE CABEC6-REL1-6          TO FD-RELATO.
           PERFORM 3800-GRAVA-RELATO.

           ADD 7                       TO WRK-LINHAS.

      *----------------------------------------------------------------*
       3710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3720-IMPRIMIR-LINDET            SECTION.
      *----------------------------------------------------------------*

           MOVE P16-AGENCIA            TO LD1-AGENCIA
           MOVE P16-CONTA              TO LD1-CONTA
           MOVE P16-CCGC-CPF           TO LD1-CPF
           MOVE P16-CCTRL-CPF-CGC      TO LD1-CPF-CTRL
           MOVE P16-DT-ABERT           TO LD1-DT-ABERT
           MOVE P16-TP-PESSOA          TO LD1-TP-PESSOA
           MOVE P16-SEGM               TO LD1-SEGTO
           MOVE P16-DT-ENCART          TO LD1-DT-ENCART
           MOVE P16-CPCOTE-TARIF-VIG   TO LD1-CESTA-VIG
           MOVE P16-DINIC-ADSAO-INDVD  TO LD1-DINIC-VGCIA-ADSAO
           MOVE P16-DFIM-ADSAO-INDVD   TO LD1-DFIM-VGCIA-ADSAO

           IF   P16-CPCOTE-TARIF-ANT   EQUAL ZEROS
             MOVE SPACES               TO LD1-CESTA-ANT-X
           ELSE
             MOVE P16-CPCOTE-TARIF-ANT TO LD1-CESTA-ANT
             MOVE '-'                  TO LD1-CESTA-ANT-TRACO
           END-IF.

           IF   P16-TP-DESCONTO        EQUAL  1  OR  2
             MOVE 'ISENCAO'            TO LD1-BENEFICIO
           ELSE
             MOVE 'DESCONTO'           TO LD1-BENEFICIO
           END-IF.

           MOVE LD1-LINDET             TO FD-RELATO.
           PERFORM 3800-GRAVA-RELATO.

           ADD 1                       TO WRK-LINHAS.

      *----------------------------------------------------------------*
       3720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3730-IMPRIMIR-TOTAL             SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-CTAEXCLE     TO LT1-REL1-TOT-CONTAS.

           MOVE LINTOT1-REL1-TOT1      TO FD-RELATO
           PERFORM 3800-GRAVA-RELATO.

      *----------------------------------------------------------------*
       3730-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3740-GRAVA-RELATO-VAZIO         SECTION.
      *----------------------------------------------------------------*

           MOVE '4110-GRAVA-RELATO-VAZIO'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 3710-IMPRIMIR-CABEC.

           WRITE FD-RELATO             FROM WRK-REL-VAZIO.
           PERFORM 1130-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       3740-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3800-GRAVA-RELATO               SECTION.
      *----------------------------------------------------------------*

           SET ARQ-WRITE               TO TRUE.

           WRITE FD-RELATO.
           PERFORM 1130-TESTAR-FS-RELATO.

           ADD 1                       TO ACU-GRA-RELATO.

      *----------------------------------------------------------------*
       3800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * FINALIZA O PROCESSAMENTO                                       *
      *----------------------------------------------------------------*
       4000-FINALIZA                   SECTION.
      *----------------------------------------------------------------*

           MOVE '4000-FINALIZA'        TO FRWKGHEA-IDEN-PARAGRAFO.

           IF ACU-LIDOS-CTAEXCLE       GREATER ZEROS
              PERFORM 3730-IMPRIMIR-TOTAL
           ELSE
              PERFORM 3740-GRAVA-RELATO-VAZIO
           END-IF.

           PERFORM 4100-DISPLAY-TOTAIS.

           PERFORM 4200-FECHA-ARQUIVOS.

           PERFORM 9999-ROTINA-FINAL.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ESTATISTICA DE PROCESSAMENTO                                   *
      *----------------------------------------------------------------*
       4100-DISPLAY-TOTAIS             SECTION.
      *----------------------------------------------------------------*

           MOVE '4100-DISPLAY-TOTAIS'  TO FRWKGHEA-IDEN-PARAGRAFO.

           DISPLAY ' '
           DISPLAY '=========== GFCT4853 ==========================='
           DISPLAY 'ARQGECTE - LIDOS                '
                                       ACU-LIDOS-CTAEXCLE
           DISPLAY '=========== GFCT4853 ==========================='.
           DISPLAY ' '.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * FECHA ARQUIVOS                                                 *
      *----------------------------------------------------------------*
       4200-FECHA-ARQUIVOS             SECTION.
      *----------------------------------------------------------------*

           MOVE '4300-FECHA-ARQUIVOS'  TO FRWKGHEA-IDEN-PARAGRAFO.

           SET   ARQ-CLOSE             TO TRUE.

           CLOSE   CTAEXCLE
                   RELATO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 7100-ENCERRAR-CKRS0105.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       7100-ENCERRAR-CKRS0105          SECTION.
      *---------------------------------------------------------------*

           MOVE '7100-ENCERRAR-CKRS0105'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           MOVE 'D'                    TO PARM-OP.
           MOVE SPACES                 TO PARM-PLAN.

           CALL WRK-CKRS0105           USING PARM-CKRS0105.

      *---------------------------------------------------------------*
       7100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-ERRO-ARQ                   SECTION.
      *----------------------------------------------------------------*

      *--> CARREGAR DADOS NA AREA COMUM DE ERROS - HEADER (I#FRWKHE)
           SET ERRO-ARQUIVO            TO TRUE.

           MOVE WRK-GFCT4853           TO FRWKGHEA-NOME-PROGRAMA.
           MOVE FRWKGARQ-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.

      *--> MOVER AREA DE ERRO MODULO P/ BLOCO DE INFORM ESPECIF DO ERRO
           MOVE  WRK-AREA-ERRO-ARQ     TO WRK-BLOCO-INFO-ERRO(1:29).

           PERFORM 9200-API-ERROS.

      *---------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9200-API-ERROS                  SECTION.
      *---------------------------------------------------------------*

           PERFORM 4100-DISPLAY-TOTAIS

           PERFORM 9300-CHAMAR-FRWK2999.

           IF WRK-ABENDAR
              PERFORM 9900-ROTINA-ABEND
           END-IF.

           PERFORM 9999-ROTINA-FINAL.

      *---------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9300-CHAMAR-FRWK2999            SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-GFCT4853           TO FRWKGHEA-NOME-PROGRAMA.

           CALL WRK-FRWK2999           USING WRK-AREA-ERRO.

      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9400-ERRO-DB2                   SECTION.
      *----------------------------------------------------------------*

           SET  WRK-ABENDAR            TO TRUE.
           MOVE FRWKGDB2-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.

           SET  ERRO-DB2               TO TRUE
           MOVE WRK-PROGRAMA           TO FRWKGMOD-NOME-MODULO
           MOVE SQLCA                  TO FRWKGDB2-SQLCA.
           MOVE WRK-SQLCODE            TO FRWKGDB2-SQLCODE.
           MOVE SQLSTATE               TO FRWKGDB2-SQLSTATE.

           MOVE WRK-AREA-ERRO-DB2      TO WRK-BLOCO-INFO-ERRO.

           PERFORM 9200-API-ERROS.

      *----------------------------------------------------------------*
       9400-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       9900-ROTINA-ABEND               SECTION.
      *---------------------------------------------------------------*

           CALL 'BRAD0450'             USING WRK-ABEND-BAT
                                             WRK-DUMP-BAT.

      *---------------------------------------------------------------*
       9900-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9999-ROTINA-FINAL               SECTION.
      *---------------------------------------------------------------*

           STOP RUN.

      *---------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
