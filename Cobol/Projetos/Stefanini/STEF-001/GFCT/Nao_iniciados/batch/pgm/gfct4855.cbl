      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
      *
       PROGRAM-ID. GFCT4855.
       AUTHOR.     FABRICA TI.
      *================================================================*
      *                        S O N D A   I T                         *
      *================================================================*
      *                                                                *
      *    PROGRAMA....:   GFCT4855                                    *
      *    PROGRAMADOR.:   REGINA              - SONDA IT              *
      *    ANALISTA....:   REGINA              - SONDA IT              *
      *    DATA........:   JUNHO/2016                                  *
      *                                                                *
      *    OBJETIVO....:   GERAR RELATORIO DE INCONSISTENCIAS NO       *
      *                    TRATAMENTO DE TEST DRIVE DE CESTA EXCLUSIVE.*
      *                                                                *
      * *------------------------------------------------------------* *
      * *------------------------------------------------------------* *
      * | ARQUIVO  |  BOOKS   | LRECL |DESCRICAO                     | *
      * *----------+----------+-------+------------------------------* *
      * | CTAREJEE | GFCTWP17 | 0125  |CTAS TEST DRIVE CESTA EXCLUS. | *
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

           SELECT CTAREJEE ASSIGN      TO UT-S-CTAREJEE
                      FILE STATUS      IS WRK-FS-CTAREJEE.

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
      *            ORG. SEQUENCIAL                    LRECL = 070      *
      *----------------------------------------------------------------*

       FD  CTAREJEE
           BLOCK 0
           RECORDING F
           LABEL RECORD IS STANDARD.

       01  FD-CTAREJEE                 PIC X(070).

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
       77  FILLER PIC X(050) VALUE '* INICIO DA WORKING - GFCT4855 *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER PIC X(050) VALUE '* AREA FILE STATUS *'.
      *----------------------------------------------------------------*

       77  WRK-FS-CTAREJEE             PIC X(002) VALUE SPACES.
       77  WRK-FS-RELATO               PIC X(002) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER PIC X(050) VALUE '* AREA DE ACUMULADORES *'.
      *----------------------------------------------------------------*

       01  ACU-LIDOS-CTAREJEE          PIC 9(009) COMP-3 VALUE ZEROS.
       01  ACU-GRA-RELATO              PIC 9(009) COMP-3 VALUE ZEROS.
       01  ACU-PAGINA                  PIC 9(003) COMP-3 VALUE ZEROS.
       01  WRK-LINHAS                  PIC 9(003) COMP-3 VALUE 99.

      *----------------------------------------------------------------*
       01  FILLER PIC X(050) VALUE '* AREAS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-ABEND-BAT               PIC S9(04) COMP VALUE +1111.
       77  WRK-DUMP-BAT                PIC X(001) VALUE 'S'.

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
           03  FILLER                  PIC X(042) VALUE 'GFCT4855'.
           03  FILLER                  PIC X(079) VALUE
               'GFCT - GESTAO, FLEXIBILIZACAO E COBRANCA DE TARIFAS'.
           03  FILLER                  PIC X(005) VALUE 'PAG. '.
           03  CB2-CONT-PAG            PIC ZZ.ZZ9 VALUE ZEROS.

       01  CABEC3-REL1-3.
           03  CB3-CARRO               PIC X(001) VALUE ' '.
           03  FILLER                  PIC X(032) VALUE SPACES.
           03  FILLER                  PIC X(100) VALUE
           'RELATORIO DE INCONSISTENCIAS NO TRATAMENTO DA MENSALIDADE DE
      -    ' CESTA EXCLUSIVE'.

       01  CABEC4-REL1-4.
           03  CB4-CARRO               PIC X(001) VALUE ' '.
           03  FILLER                  PIC X(132) VALUE ALL '-'.

       01  CABEC5-REL1-5.
           03 FILLER                   PIC  X(014)  VALUE ' '.
           03 FILLER                   PIC  X(119)  VALUE
              'AGENCIA    CONTA     CPF          DT.ENCART.    MENSAGEM
      -       'DE ERRO'.

       01  CABEC6-REL1-6.
           03 FILLER                   PIC  X(014)  VALUE ' '.
           03 FILLER                   PIC  X(119)  VALUE
              '-------   -------  ------------   ----------    ---------
      -       '---------------------'.

       01  LD1-LINDET.
           03  FILLER                  PIC  X(015)  VALUE ' '.
           03  LD1-AGENCIA             PIC  9(005)  VALUE ZEROS.
           03  FILLER                  PIC  X(004)  VALUE SPACES.
           03  LD1-CONTA               PIC  9(007)  VALUE ZEROS.
           03  FILLER                  PIC  X(002)  VALUE SPACES.
           03  LD1-CPF-X.
ST25X6*      05 LD1-CPF                 PIC  9(009)  VALUE ZEROS.
ST25X6       05 LD1-CPF                 PIC  X(009)  VALUE SPACES.
             05 LD1-CPF-TRACO           PIC  X(001)  VALUE '-'.
ST25X6*      05 LD1-CPF-CTRL            PIC  9(002)  VALUE ZEROS.
ST25X6       05 LD1-CPF-CTRL            PIC  X(002)  VALUE SPACES.
           03  FILLER                  PIC  X(003)  VALUE SPACES.
           03  LD1-DT-ENCART           PIC  X(010)  VALUE SPACES.
           03  FILLER                  PIC  X(004)  VALUE SPACES.
           03  LD1-MSG-ERRO            PIC  X(030)  VALUE SPACES.

       01  LINTOT1-REL1-TOT1.
           03  FILLER                  PIC  X(014)  VALUE '0'.
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

       77  WRK-GFCT4855                PIC X(008) VALUE 'GFCT4855'.
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

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM CKRS0105           ***'.
      *----------------------------------------------------------------*

       77  WRK-CKRS0105                PIC  X(008)         VALUE
           'CKRS0105'.

           COPY 'I#CKRS04'.

      *----------------------------------------------------------------*
       01  FILLER PIC X(050) VALUE '* AREA DO ARQUIVOS CTAREJEE *'.
      *----------------------------------------------------------------*

           COPY 'GFCTWP17'.


      *----------------------------------------------------------------*
       01  FILLER PIC X(050) VALUE '* AREA PARA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

      *----------------------------------------------------------------*
       77  FILLER PIC X(050) VALUE '* FIM DA WORKING - GFCT4855 *'.
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
                   WRK-FS-CTAREJEE     EQUAL '10'.

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

           OPEN  INPUT CTAREJEE
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

           PERFORM 1110-TESTAR-FS-CTAREJEE.

           PERFORM 1130-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TESTA FILE STATUS DO ARQUIVO CTAREJEE                          *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-CTAREJEE         SECTION.
      *----------------------------------------------------------------*

           MOVE '1110-TESTAR-FS-CTAREJEE'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           IF WRK-FS-CTAREJEE          NOT EQUAL '00' AND '10'
              MOVE 'CTAREJEE'          TO FRWKGARQ-NOME-ARQUIVO
              MOVE WRK-FS-CTAREJEE     TO FRWKGARQ-FILE-STATUS
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

           PERFORM 2100-LER-CTAREJEE.

           IF WRK-FS-CTAREJEE          EQUAL '10'
              DISPLAY '************ GFCT4855 ************'
              DISPLAY '*                                *'
              DISPLAY '*     ARQUIVO CTAREJEE VAZIO     *'
              DISPLAY '*       PROGRAMA ENCERRADO       *'
              DISPLAY '*                                *'
              DISPLAY '************ GFCT4855 ************'
              PERFORM 4000-FINALIZA
           END-IF.


      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * LEITURA DO ARQUIVO CTAREJEE                                    *
      *----------------------------------------------------------------*
       2100-LER-CTAREJEE               SECTION.
      *----------------------------------------------------------------*

           MOVE '2100-LER-CTAREJEE'    TO FRWKGHEA-IDEN-PARAGRAFO.

           SET ARQ-READ                TO TRUE.

           READ CTAREJEE               INTO GFCTWP17-REG.

           PERFORM 1110-TESTAR-FS-CTAREJEE.

           IF   WRK-FS-CTAREJEE        EQUAL '10'
             GO TO 2100-99-FIM
           END-IF.

           ADD    +1                   TO ACU-LIDOS-CTAREJEE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PROCESSAMENTO DO PROGRAMA                                      *
      *----------------------------------------------------------------*
       3000-PROCESSA                   SECTION.
      *----------------------------------------------------------------*

           MOVE '3000-PROCESSA'        TO FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 3700-FORMATAR-RELATO.

           PERFORM 2100-LER-CTAREJEE.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
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

           MOVE P17-AGENCIA            TO LD1-AGENCIA
           MOVE P17-CONTA              TO LD1-CONTA

           IF   P17-DESC-ERRO          EQUAL
                'DADOS CLIENTE NAO ENCONTRADO'
             MOVE SPACES               TO LD1-CPF-X
           ELSE
             MOVE P17-CCGC-CPF         TO LD1-CPF
             MOVE P17-CCTRL-CPF-CGC    TO LD1-CPF-CTRL
             MOVE '-'                  TO LD1-CPF-TRACO
           END-IF.

           MOVE P17-DT-ENCART          TO LD1-DT-ENCART
           MOVE P17-DESC-ERRO          TO LD1-MSG-ERRO.

           MOVE LD1-LINDET             TO FD-RELATO.
           PERFORM 3800-GRAVA-RELATO.

           ADD 1                       TO WRK-LINHAS.

      *----------------------------------------------------------------*
       3720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3730-IMPRIMIR-TOTAL             SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-CTAREJEE     TO LT1-REL1-TOT-CONTAS.

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

           IF ACU-LIDOS-CTAREJEE       GREATER ZEROS
              PERFORM 3730-IMPRIMIR-TOTAL
           ELSE
              PERFORM 3740-GRAVA-RELATO-VAZIO
           END-IF.

           PERFORM 4100-DISPLAY-TOTAIS.

           PERFORM 4200-FECHA-ARQUIVOS.

           PERFORM 7100-ENCERRAR-CKRS0105.

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

           DISPLAY ' '.
           DISPLAY '=========== GFCT4855 ==========================='
           DISPLAY 'ARQGECTE - LIDOS                '
                                       ACU-LIDOS-CTAREJEE
           DISPLAY '=========== GFCT4855 ==========================='.
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

           CLOSE   CTAREJEE
                   RELATO.

           PERFORM 1100-TESTAR-FILE-STATUS.

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

           MOVE WRK-GFCT4855           TO FRWKGHEA-NOME-PROGRAMA.
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

           MOVE WRK-GFCT4855           TO FRWKGHEA-NOME-PROGRAMA.

           CALL WRK-FRWK2999           USING WRK-AREA-ERRO.

      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

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
