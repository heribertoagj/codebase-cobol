      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1284.
       AUTHOR.     JONAS RODRIGO BASSO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1284                                    *
      *    PROGRAMADOR.:   JONAS RODRIGO BASSO     - CPM PATO BRANCO   *
      *    ANALISTA CPM:   JOSSIANE GAZZONI        - CPM PATO BRANCO   *
      *    ANALISTA....:   RENATO TAMANAHA         - PROCWORK GP/50    *
      *    DATA........:   07/06/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   COMPLEMENTAR  OS  DADOS  PARA  EMISSAO  DE  *
      *        RELATORIO DE AGRUPAMENTO.                               *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    PEDRELE                   I#GFCTRN          *
      *                    PEDRELS                   I#GFCTRN          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                          INCLUDE/BOOK   *
      *                    DB2PRD.PDIDO_FLEXZ_UF          GFCTB0C1     *
      *                    DB2PRD.PDIDO_FLEXZ_AG          GFCTB0B1     *
      *                    DB2PRD.PDIDO_FLEXZ_PAB         GFCTB0B8     *
      *                    DB2PRD.PDIDO_FLEXZ_EMPR        GFCTB0B6     *
      *                    DB2PRD.PDIDO_FLEXZ_SGMTO       GFCTB0C0     *
      *                    DB2PRD.PDIDO_FLEXZ_MUN         GFCTB0B7     *
      *                    DB2PRD.PDIDO_FLEXZ_PSTAL       GFCTB0B9     *
      *                    DB2PRD.PDIDO_FLEXZ_CLI         GFCTB0B3     *
      *                    DB2PRD.TPO_AGPTO_CTA           GFCTB0F3     *
      *                    DB2PRD.TPDIDO_FLEXZ_RZ         GFCTB0M7     *
      *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C  -  AREA PARA TRATAMENTO DE ERRO PELA POOL7100.    *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100  -  TRATAMENTO DE ERRO QUANDO PROGRAMA INVALIDO.   *
      *    POOL7600  -  OBTEM DATA/HORA DO SISTEMA.                    *
      *    POOL1050  -  CONEXAO COM DB2.                               *
      *================================================================*
      *================================================================*
      *    ULTIMA ALTERACAO EM 20/08/2008 - VALERIA                    *
      *    ALTERACOES REFERENTES AOS AGRUPAMENTOS PERSONALIZADOS       *
      *    - INCLUSAO DO AGRUPAMENTO 07                                *
      *    - INCLUSAO DOS AGRUPAMENTOS PERSONALIZADOS                  *
      *================================================================*
      *   PROJETO - PWT-09-2711 - ALTERACAO JUNHO/2010                 *
      *   - INCLUSAO AGRUPAMENTOS 20-BRADESCO EXPRESSO                 *
      *                           26-RAZAO                             *
      *================================================================*
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

           SELECT PEDRELE  ASSIGN      TO UT-S-PEDRELE
           FILE STATUS                 IS WRK-FS-PEDRELE.

           SELECT PEDRELS  ASSIGN      TO UT-S-PEDRELS
           FILE STATUS                 IS WRK-FS-PEDRELS.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  PEDIDOS PARA RELATORIO                              *
      *            ORG. SEQUENCIAL     -   LRECL   =   150             *
      *----------------------------------------------------------------*

       FD  PEDRELE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRN'.

      *----------------------------------------------------------------*
      *    OUTPUT: PEDIDOS PARA RELATORIO                              *
      *            ORG. SEQUENCIAL     -   LRECL   =   150             *
      *----------------------------------------------------------------*

       FD  PEDRELS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRN'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS                   PIC  9(007) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS                PIC  9(007) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-FIM                     PIC  X(001)         VALUE SPACES.

       01  WRK-DATA-ATUAL.
           05 WRK-DIA-ATU              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '/'.
           05 WRK-MES-ATU              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '/'.
           05 WRK-ANO-ATU              PIC  9(004)         VALUE ZEROS.

       01  WRK-AGRUPAMENTO-GFCTB0B1-AUX.
           05 WRK-GFCTB0B1-CEMPR-INC   PIC  X(005)         VALUE SPACES.
           05 FILLER                   PIC  X(001)         VALUE '/'.
           05 WRK-GFCTB0B1-CDEPDC      PIC  X(005)         VALUE SPACES.
           05 FILLER                   PIC  X(009)         VALUE SPACES.

       01  WRK-AGRUPAMENTO-GFCTB0B8-AUX.
           05 WRK-GFCTB0B8-CEMPR-INC   PIC  X(005)         VALUE SPACES.
           05 FILLER                   PIC  X(001)         VALUE '/'.
           05 WRK-GFCTB0B8-CDEPDC      PIC  X(005)         VALUE SPACES.
           05 FILLER                   PIC  X(001)         VALUE '/'.
           05 WRK-GFCTB0B8-CPOSTO-SERVC PIC  X(005)        VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE SPACES.

       01  WRK-AGRUPAMENTO-GFCTB0B6-AUX.
           05 WRK-GFCTB0B6-CJUNC-DEPDC PIC  X(005)         VALUE SPACES.
           05 FILLER                   PIC  X(001)         VALUE '/'.
           05 WRK-GFCTB0B6-CCTA-CLI    PIC  X(007)         VALUE SPACES.
           05 FILLER                   PIC  X(007)         VALUE SPACES.

       01  WRK-AGRUPAMENTO-GFCTB0B9-AUX.
           05 WRK-GFCTB0B9-CEMPR-INC   PIC  X(005)         VALUE SPACES.
           05 FILLER                   PIC  X(001)         VALUE '/'.
           05 WRK-GFCTB0B9-CDEPDC      PIC  X(005)         VALUE SPACES.
           05 FILLER                   PIC  X(001)         VALUE '/'.
           05 WRK-GFCTB0B9-CPOSTO-SERVC PIC X(005)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE SPACES.

       01  WRK-AGRUPAMENTO-GFCTB0B3-AUX.
           05 WRK-GFCTB0B3-CCGC-CPF    PIC  X(009)         VALUE SPACES.
           05 FILLER                   PIC  X(001)         VALUE '/'.
           05 WRK-GFCTB0B3-CFLIAL-CGC  PIC  X(005)         VALUE SPACES.
           05 FILLER                   PIC  X(001)         VALUE '/'.
           05 WRK-GFCTB0B3-CPF-CGC     PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(002)         VALUE SPACES.

LFM    01  WRK-AGRUPAMENTO-GFCTB0M7-AUX.
LFM        05 WRK-GFCTB0M7-CGRP-RZ         PIC  X(003) VALUE SPACES.
LFM        05 FILLER     REDEFINES  WRK-GFCTB0M7-CGRP-RZ.
              10 WRK-GFCTB0M7-CGRP-TAB     PIC S9(003) COMP-3.
              10 FILLER                    PIC  X(001).
LFM        05 FILLER                       PIC  X(001)    VALUE '/'.
LFM        05 WRK-GFCTB0M7-CSGRP-RZ        PIC  X(003)    VALUE SPACES.
LFM        05 FILLER            REDEFINES WRK-GFCTB0M7-CSGRP-RZ.
              10 WRK-GFCTB0M7-CSGRP-TAB    PIC S9(3) COMP-3.
              10 FILLER                    PIC  X(01).

LFM        05 FILLER                       PIC  X(013)  VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TESTE DE FILE-STATUS *'.
      *----------------------------------------------------------------*

       01  WRK-FS-PEDRELE             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-PEDRELS             PIC  X(002)         VALUE SPACES.

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
           '* MENSAGENS DE ERRO *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-ARQUIVO.
           05  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQUIVO        PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(018)         VALUE
               ' - FILE STATUS =  '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA POOL7600 *'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05  WRK-DATA-JULIANA        PIC 9(005) COMP-3   VALUE ZEROS.
           05  WRK-DATA-AAMMDD         PIC 9(007) COMP-3   VALUE ZEROS.
           05  WRK-DATA-AAAAMMDD       PIC 9(009) COMP-3   VALUE ZEROS.
           05  WRK-HORA-HHMMSS         PIC 9(007) COMP-3   VALUE ZEROS.
           05  WRK-HORA-HHMMSSMMMMMM   PIC 9(013) COMP-3   VALUE ZEROS.
           05  WRK-TIMESTAMP.
               10  WRK-ANO             PIC 9(004)          VALUE ZEROS.
               10  WRK-MES             PIC 9(002)          VALUE ZEROS.
               10  WRK-DIA             PIC 9(002)          VALUE ZEROS.
               10  WRK-HOR             PIC 9(002)          VALUE ZEROS.
               10  WRK-MIN             PIC 9(002)          VALUE ZEROS.
               10  WRK-SEG             PIC 9(002)          VALUE ZEROS.
               10  WRK-MIL             PIC 9(006)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DA POOL7100 *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0C1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B8
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B6
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0C0
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B7
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B9
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B3
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0F3
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0M7
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      * INICIO DO PROCESSAMENTO DO PROGRAMA                            *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FIM             EQUAL 'F'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCEDIMENTOS INICIAIS DO PROGRAMA.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT  PEDRELE
                OUTPUT PEDRELS.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTAR FILE-STATUS DOS ARQUIVOS DE ENTRADA E SAIDA.            *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-PEDRELE.

           PERFORM 1120-TESTAR-FS-PEDRELS.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTAR FILE-STATUS DO ARQUIVO DE SAIDA PEDRELE.                *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-PEDRELE          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-PEDRELE          NOT EQUAL '00'
               MOVE 'PEDRELE'          TO WRK-NOME-ARQUIVO
               MOVE WRK-FS-PEDRELE     TO WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTAR FILE-STATUS DO ARQUIVO DE SAIDA PEDRELS.                *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-PEDRELS          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-PEDRELS          NOT EQUAL '00'
               MOVE 'PEDRELS'          TO WRK-NOME-ARQUIVO
               MOVE WRK-FS-PEDRELS     TO WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR SE O ARQUIVO PEDRELE ESTA VAZIO.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-PEDRELE.

           IF  WRK-FS-PEDRELE          EQUAL '10'
               CALL 'POOL7600'         USING WRK-DATA-HORA
               MOVE WRK-DIA            TO WRK-DIA-ATU
               MOVE WRK-MES            TO WRK-MES-ATU
               MOVE WRK-ANO            TO WRK-ANO-ATU
               DISPLAY '*************** GFCT1284 *************'
               DISPLAY '*                                    *'
               DISPLAY '*   ** NAO HA FLEXIBILIZACAO    **   *'
               DISPLAY '*   ** DE AGRUPAMENTO           **   *'
               DISPLAY '*   ** NESTA DATA: ' WRK-DATA-ATUAL
                                                    '   **   *'
               DISPLAY '*                                    *'
               DISPLAY '*************** GFCT1284 *************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LEITURA DO ARQUIVO DE ENTRADA PEDRELE.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-PEDRELE                SECTION.
      *----------------------------------------------------------------*

           READ PEDRELE.

           IF  WRK-FS-PEDRELE              EQUAL '10'
               MOVE 'F'                    TO WRK-FIM
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA                TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-PEDRELE.

           MOVE GFCTRN-DADOS OF PEDRELE    TO GFCTRN-DADOS OF PEDRELS.

           ADD 1                           TO ACU-LIDOS.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO PRINCIPAL DO PROGRAMA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-COMPLEMENTA-ARQUIVO.

           PERFORM 3200-GERAR-PEDRELS.

           PERFORM 2100-LER-PEDRELE.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  FAZ SELECAO DE DADOS E CHAMA SEU RESPECTIVO PROCESSAMENTO     *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-COMPLEMENTA-ARQUIVO        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTRN-TIPO OF PEDRELE  NOT EQUAL 2
               GO TO 3100-99-FIM
           END-IF.

           MOVE GFCTRN-CAGPTO-CTA OF PEDRELE
                                       TO CAGPTO-CTA       OF GFCTB0F3.

           EXEC SQL
             SELECT
                   RAGPTO_CTA
             INTO
                   :GFCTB0F3.RAGPTO-CTA
             FROM   DB2PRD.TPO_AGPTO_CTA
             WHERE
                   CAGPTO_CTA         = :GFCTB0F3.CAGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TPO_AGPTO_CTA   ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE SPACES             TO GFCTRN-DESC-TIPO-AGRUP
                                                           OF PEDRELS
           ELSE
               MOVE RAGPTO-CTA         TO GFCTRN-DESC-TIPO-AGRUP
                                                           OF PEDRELS
           END-IF.

           EVALUATE TRUE

               WHEN GFCTRN-CAGPTO-CTA  OF PEDRELE    EQUAL 19
                   PERFORM 3110-SELECT-GFCTB0C1

               WHEN GFCTRN-CAGPTO-CTA  OF PEDRELE    EQUAL 14
                   PERFORM 3120-SELECT-GFCTB0B1

               WHEN GFCTRN-CAGPTO-CTA  OF PEDRELE    EQUAL 15
                   PERFORM 3130-SELECT-GFCTB0B8

               WHEN GFCTRN-CAGPTO-CTA  OF PEDRELE    EQUAL 16
                   PERFORM 3140-SELECT-GFCTB0B6

               WHEN GFCTRN-CAGPTO-CTA  OF PEDRELE    EQUAL 9
                   PERFORM 3150-SELECT-GFCTB0C0

               WHEN GFCTRN-CAGPTO-CTA  OF PEDRELE    EQUAL 18
                   PERFORM 3160-SELECT-GFCTB0B7

               WHEN GFCTRN-CAGPTO-CTA  OF PEDRELE    EQUAL 17
                   PERFORM 3170-SELECT-GFCTB0B9

               WHEN GFCTRN-CAGPTO-CTA  OF PEDRELE    EQUAL 4
                   PERFORM 3180-SELECT-GFCTB0B3

LFM            WHEN GFCTRN-CAGPTO-CTA  OF PEDRELE    EQUAL 20
LFM                PERFORM 3170-SELECT-GFCTB0B9
LFM
LFM            WHEN GFCTRN-CAGPTO-CTA  OF PEDRELE    EQUAL 26
LFM                PERFORM 3190-SELECT-GFCTB0M7

200808         WHEN OTHER
200808             MOVE SPACES         TO GFCTRN-IDENT-AGRUPAMENTO
200808                                                     OF PEDRELS

           END-EVALUATE.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EXECUTAR SELECT NA TABELA GFCTB0C1                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3110-SELECT-GFCTB0C1            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRN-CAGPTO-CTA      OF PEDRELE
                                       TO CAGPTO-CTA       OF GFCTB0C1.
           MOVE GFCTRN-CSERVC-TARIF    OF PEDRELE
                                       TO CSERVC-TARIF     OF GFCTB0C1.
           MOVE GFCTRN-HIDTFD-FLEXZ-AGPTO  OF PEDRELE
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0C1.

           EXEC SQL
             SELECT
                   CSGL_UF
             INTO
                   :GFCTB0C1.CSGL-UF
             FROM   DB2PRD.PDIDO_FLEXZ_UF
             WHERE
                   CAGPTO_CTA         = :GFCTB0C1.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0C1.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0C1.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_UF  ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE SPACES             TO GFCTRN-IDENT-AGRUPAMENTO
                                                           OF PEDRELS
           ELSE
               MOVE CSGL-UF            TO GFCTRN-IDENT-AGRUPAMENTO
                                                           OF PEDRELS
           END-IF.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EXECUTAR SELECT NA TABELA GFCTB0B1                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3120-SELECT-GFCTB0B1            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRN-CAGPTO-CTA      OF PEDRELE
                                       TO CAGPTO-CTA       OF GFCTB0B1.
           MOVE GFCTRN-CSERVC-TARIF    OF PEDRELE
                                       TO CSERVC-TARIF     OF GFCTB0B1.
           MOVE GFCTRN-HIDTFD-FLEXZ-AGPTO  OF PEDRELE
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B1.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC
             INTO
                   :GFCTB0B1.CEMPR-INC,
                   :GFCTB0B1.CDEPDC
             FROM   DB2PRD.PDIDO_FLEXZ_AG
             WHERE
                   CAGPTO_CTA         = :GFCTB0B1.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B1.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B1.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AG  ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                     EQUAL +100
               MOVE SPACES                 TO GFCTRN-IDENT-AGRUPAMENTO
                                                           OF PEDRELS
           ELSE
               MOVE CEMPR-INC OF GFCTB0B1  TO WRK-GFCTB0B1-CEMPR-INC
               MOVE CDEPDC OF GFCTB0B1     TO WRK-GFCTB0B1-CDEPDC
               MOVE WRK-AGRUPAMENTO-GFCTB0B1-AUX
                                           TO GFCTRN-IDENT-AGRUPAMENTO
                                                           OF PEDRELS
           END-IF.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EXECUTAR SELECT NA TABELA GFCTB0B8                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3130-SELECT-GFCTB0B8            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRN-CAGPTO-CTA      OF PEDRELE
                                       TO CAGPTO-CTA       OF GFCTB0B8.
           MOVE GFCTRN-CSERVC-TARIF    OF PEDRELE
                                       TO CSERVC-TARIF     OF GFCTB0B8.
           MOVE GFCTRN-HIDTFD-FLEXZ-AGPTO  OF PEDRELE
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B8.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB0B8.CEMPR-INC,
                   :GFCTB0B8.CDEPDC,
                   :GFCTB0B8.CPOSTO-SERVC
             FROM   DB2PRD.PDIDO_FLEXZ_PAB
             WHERE
                   CAGPTO_CTA         = :GFCTB0B8.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B8.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B8.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_PAB ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0040'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE SPACES             TO GFCTRN-IDENT-AGRUPAMENTO
                                                           OF PEDRELS
           ELSE
               MOVE CEMPR-INC OF GFCTB0B8  TO WRK-GFCTB0B8-CEMPR-INC
               MOVE CDEPDC    OF GFCTB0B8  TO WRK-GFCTB0B8-CDEPDC
               MOVE CPOSTO-SERVC OF GFCTB0B8
                                           TO WRK-GFCTB0B8-CPOSTO-SERVC
               MOVE WRK-AGRUPAMENTO-GFCTB0B8-AUX
                                  TO GFCTRN-IDENT-AGRUPAMENTO OF PEDRELS
           END-IF.

      *----------------------------------------------------------------*
       3130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EXECUTAR SELECT NA TABELA GFCTB0B6                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3140-SELECT-GFCTB0B6            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRN-CAGPTO-CTA      OF PEDRELE
                                       TO CAGPTO-CTA       OF GFCTB0B6.
           MOVE GFCTRN-CSERVC-TARIF    OF PEDRELE
                                       TO CSERVC-TARIF     OF GFCTB0B6.
           MOVE GFCTRN-HIDTFD-FLEXZ-AGPTO  OF PEDRELE
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B6.

           EXEC SQL
             SELECT
                   CJUNC_DEPDC,
                   CCTA_CLI
             INTO
                   :GFCTB0B6.CJUNC-DEPDC,
                   :GFCTB0B6.CCTA-CLI
             FROM   DB2PRD.PDIDO_FLEXZ_EMPR
             WHERE
                   CAGPTO_CTA         = :GFCTB0B6.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B6.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B6.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_EMPR' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0050'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE SPACES             TO GFCTRN-IDENT-AGRUPAMENTO
                                                           OF PEDRELS
           ELSE
               MOVE CJUNC-DEPDC  OF GFCTB0B6 TO WRK-GFCTB0B6-CJUNC-DEPDC
               MOVE CCTA-CLI OF GFCTB0B6 TO WRK-GFCTB0B6-CCTA-CLI
               MOVE WRK-AGRUPAMENTO-GFCTB0B6-AUX
                                  TO GFCTRN-IDENT-AGRUPAMENTO OF PEDRELS
           END-IF.

      *----------------------------------------------------------------*
       3140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EXECUTAR SELECT NA TABELA GFCTB0C0                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3150-SELECT-GFCTB0C0            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRN-CAGPTO-CTA      OF PEDRELE
                                       TO CAGPTO-CTA       OF GFCTB0C0.
           MOVE GFCTRN-CSERVC-TARIF    OF PEDRELE
                                       TO CSERVC-TARIF     OF GFCTB0C0.
           MOVE GFCTRN-HIDTFD-FLEXZ-AGPTO  OF PEDRELE
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0C0.

           EXEC SQL
             SELECT
                   CSGMTO_GSTAO_TARIF
             INTO
                   :GFCTB0C0.CSGMTO-GSTAO-TARIF
             FROM   DB2PRD.PDIDO_FLEXZ_SGMTO
             WHERE
                   CAGPTO_CTA         = :GFCTB0C0.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0C0.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0C0.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_SGMTO' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0060'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE SPACES             TO GFCTRN-IDENT-AGRUPAMENTO
                                                           OF PEDRELS
           ELSE
               MOVE CSGMTO-GSTAO-TARIF TO GFCTRN-IDENT-AGRUPAMENTO
                                                           OF PEDRELS
           END-IF.

      *----------------------------------------------------------------*
       3150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EXECUTAR SELECT NA TABELA GFCTB0B7                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3160-SELECT-GFCTB0B7            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRN-CAGPTO-CTA      OF PEDRELE
                                       TO CAGPTO-CTA       OF GFCTB0B7.
           MOVE GFCTRN-CSERVC-TARIF    OF PEDRELE
                                       TO CSERVC-TARIF     OF GFCTB0B7.
           MOVE GFCTRN-HIDTFD-FLEXZ-AGPTO OF PEDRELE
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B7.

           EXEC SQL
             SELECT
                   CMUN_IBGE
             INTO
                   :GFCTB0B7.CMUN-IBGE
             FROM   DB2PRD.PDIDO_FLEXZ_MUN
             WHERE
                   CAGPTO_CTA         = :GFCTB0B7.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B7.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B7.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_MUN ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0070'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE SPACES             TO GFCTRN-IDENT-AGRUPAMENTO
                                                           OF PEDRELS
           ELSE
               MOVE CMUN-IBGE          TO GFCTRN-IDENT-AGRUPAMENTO
                                                           OF PEDRELS
           END-IF.

      *----------------------------------------------------------------*
       3160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EXECUTAR SELECT NA TABELA GFCTB0B9                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3170-SELECT-GFCTB0B9            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRN-CAGPTO-CTA      OF PEDRELE
                                       TO CAGPTO-CTA       OF GFCTB0B9.
           MOVE GFCTRN-CSERVC-TARIF    OF PEDRELE
                                       TO CSERVC-TARIF     OF GFCTB0B9.
           MOVE GFCTRN-HIDTFD-FLEXZ-AGPTO  OF PEDRELE
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B9.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB0B9.CEMPR-INC,
                   :GFCTB0B9.CDEPDC,
                   :GFCTB0B9.CPOSTO-SERVC
             FROM   DB2PRD.PDIDO_FLEXZ_PSTAL
             WHERE
                   CAGPTO_CTA         = :GFCTB0B9.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B9.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B9.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_PSTAL' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0080'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE SPACES             TO GFCTRN-IDENT-AGRUPAMENTO
                                                           OF PEDRELS
           ELSE
               MOVE CEMPR-INC OF GFCTB0B9 TO WRK-GFCTB0B9-CEMPR-INC
               MOVE CDEPDC OF GFCTB0B9 TO WRK-GFCTB0B9-CDEPDC
               MOVE CPOSTO-SERVC OF GFCTB0B9
                                           TO WRK-GFCTB0B9-CPOSTO-SERVC
               MOVE WRK-AGRUPAMENTO-GFCTB0B9-AUX
                                  TO GFCTRN-IDENT-AGRUPAMENTO OF PEDRELS
           END-IF.

      *----------------------------------------------------------------*
       3170-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EXECUTAR SELECT NA TABELA GFCTB0B3                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3180-SELECT-GFCTB0B3            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRN-CAGPTO-CTA      OF PEDRELE
                                       TO CAGPTO-CTA       OF GFCTB0B3.
           MOVE GFCTRN-CSERVC-TARIF    OF PEDRELE
                                       TO CSERVC-TARIF     OF GFCTB0B3.
           MOVE GFCTRN-HIDTFD-FLEXZ-AGPTO  OF PEDRELE
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B3.

           EXEC SQL
ST2506*      SELECT
ST2506*            CCGC_CPF,
ST2506*            CFLIAL_CGC,
ST2506*            CCTRL_CPF_CGC
ST2506*      INTO
ST2506*            :GFCTB0B3.CCGC-CPF,
ST2506*            :GFCTB0B3.CFLIAL-CGC,
ST2506*            :GFCTB0B3.CCTRL-CPF-CGC
ST2506       SELECT
ST2506             CCGC_CPF_ST,
ST2506             CFLIAL_CGC_ST,
ST2506             CCTRL_CPF_CGC_ST
ST2506       INTO
ST2506             :GFCTB0B3.CCGC-CPF-ST,
ST2506             :GFCTB0B3.CFLIAL-CGC-ST,
ST2506             :GFCTB0B3.CCTRL-CPF-CGC-ST
             FROM   DB2PRD.PDIDO_FLEXZ_CLI
             WHERE
                   CAGPTO_CTA         = :GFCTB0B3.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B3.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B3.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_CLI ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0090'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE SPACES             TO GFCTRN-IDENT-AGRUPAMENTO
                                                           OF PEDRELS
           ELSE
ST2506*        MOVE CCGC-CPF           TO WRK-GFCTB0B3-CCGC-CPF
ST2506*        MOVE CFLIAL-CGC         TO WRK-GFCTB0B3-CFLIAL-CGC
ST2506*        MOVE CCTRL-CPF-CGC      TO WRK-GFCTB0B3-CPF-CGC
ST2506         MOVE CCGC-CPF-ST           TO WRK-GFCTB0B3-CCGC-CPF
ST2506         MOVE CFLIAL-CGC-ST         TO WRK-GFCTB0B3-CFLIAL-CGC
ST2506         MOVE CCTRL-CPF-CGC-ST      TO WRK-GFCTB0B3-CPF-CGC
               MOVE WRK-AGRUPAMENTO-GFCTB0B3-AUX
                                  TO GFCTRN-IDENT-AGRUPAMENTO OF PEDRELS
           END-IF.

      *----------------------------------------------------------------*
       3180-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

LFM   ******************************************************************
LFM   * SELECIONAR REGS NA TABELA GFCTB0M7                             *
LFM   ******************************************************************
LFM   *----------------------------------------------------------------*
LFM    3190-SELECT-GFCTB0M7      SECTION.
LFM   *----------------------------------------------------------------*
LFM
LFM        MOVE GFCTRN-CSERVC-TARIF    OF PEDRELE
LFM                                    TO CSERVC-TARIF     OF GFCTB0M7.
LFM        MOVE GFCTRN-CAGPTO-CTA      OF PEDRELE
LFM                                    TO CAGPTO-CTA       OF GFCTB0M7.
LFM        MOVE GFCTRN-HIDTFD-FLEXZ-AGPTO  OF PEDRELE
LFM                                    TO HIDTFD-FLEXZ-AGPTO
LFM                                    OF GFCTB0M7.
LFM        EXEC SQL
LFM          SELECT
LFM                CGRP_CTBIL_ATUAL,
LFM                CSGRP_CTBIL,
LFM                CEMPR
LFM          INTO
LFM                :GFCTB0M7.CGRP-CTBIL-ATUAL,
LFM                :GFCTB0M7.CSGRP-CTBIL,
LFM                :GFCTB0M7.CEMPR
LFM          FROM   DB2PRD.TPDIDO_FLEXZ_RZ
LFM          WHERE
LFM                CAGPTO_CTA         = :GFCTB0M7.CAGPTO-CTA   AND
LFM                CSERVC_TARIF       = :GFCTB0M7.CSERVC-TARIF AND
LFM                HIDTFD_FLEXZ_AGPTO = :GFCTB0M7.HIDTFD-FLEXZ-AGPTO
LFM        END-EXEC.
LFM
LFM        IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
LFM           (SQLWARN0                EQUAL 'W'      )
LFM            MOVE 'DB2'              TO ERR-TIPO-ACESSO
LFM            MOVE 'PDIDO_FLEXZ_CLI ' TO ERR-DBD-TAB
LFM            MOVE 'SELECT  '         TO ERR-FUN-COMANDO
LFM            MOVE  SQLCODE           TO ERR-SQL-CODE
LFM            MOVE '0090'             TO ERR-LOCAL
LFM            MOVE  SPACES            TO ERR-SEGM
LFM            PERFORM 9999-PROCESSAR-ROTINA-ERRO
LFM        END-IF.
LFM
LFM        IF  SQLCODE                 EQUAL +100
LFM            MOVE SPACES             TO GFCTRN-IDENT-AGRUPAMENTO
LFM                                                        OF PEDRELS
LFM        ELSE
LFM            MOVE CGRP-CTBIL-ATUAL    OF GFCTB0M7
LFM                                        TO  WRK-GFCTB0M7-CGRP-TAB
LFM            MOVE CSGRP-CTBIL         OF GFCTB0M7
LFM                                        TO WRK-GFCTB0M7-CSGRP-TAB
LFM           MOVE WRK-AGRUPAMENTO-GFCTB0M7-AUX
LFM                              TO GFCTRN-IDENT-AGRUPAMENTO OF PEDRELS
LFM        END-IF.
LFM
LFM   *----------------------------------------------------------------*
LFM    3190-99-FIM.                    EXIT.
LFM   *----------------------------------------------------------------*

      ******************************************************************
      * GRAVAR ARQUIVO PEDRELS.                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-GERAR-PEDRELS              SECTION.
      *----------------------------------------------------------------*

           WRITE GFCTRN-DADOS  OF PEDRELS.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-PEDRELS.

           ADD 1                       TO ACU-GRAVADOS.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCEDIMENTOS FINAIS DO PROGRAMA.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE PEDRELE
                 PEDRELS.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           GOBACK.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EMITIR DISPLAY COM TOTAIS DOS ACUMULADORES DO PROGRAMA.        *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*----------------------------------------------*'.
           DISPLAY '*              PROGRAMA GFCT1284               *'.
           DISPLAY '*              -----------------               *'.
           DISPLAY '*                                              *'.
           DISPLAY '*      PROCESSAMENTO EFETUADO COM SUCESSO      *'.
           DISPLAY '*----------------------------------------------*'.
           DISPLAY '*----------------------------------------------*'.
           DISPLAY '*      TOTAL LIDOS EM PEDRELE:  ' ACU-LIDOS
                                                          '        *'.
           DISPLAY '*      TOTAL GRAVADOS PEDRELS:  ' ACU-GRAVADOS
                                                          '        *'.
           DISPLAY '*----------------------------------------------*'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CANCELAR PROGRAMA ATRAVES DA POOL7100 QUANDO ERRO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1284'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
