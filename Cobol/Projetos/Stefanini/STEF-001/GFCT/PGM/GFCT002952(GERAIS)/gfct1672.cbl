      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1672.
       AUTHOR.     DAYANE FILAKOSKI.
      *================================================================*
      *              C P M  B R A X I S  -  S I S T E M A S            *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT1672                                     *
      *    PROGRAMADORA:  DAYANE FILAKOSKI        - CPM BRAXIS - P BCO *
      *    ANALISTA CPM:  KELLI CALDATO           - CPM BRAXIS - P BCO *
      *    ANALISTA....:  VINICIUS C. MADUREIRA   - PROCKWORK  - GP 50 *
      *    DATA........:  20/09/2007                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  VERIFICAR SE O EVENTO ENQUADRA-SE NAS REGRAS *
      *      DA BONIFICACAO 2.7.                                       *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                  BONIFICA                    I#GFCTVA          *
      *                  SAID1672                    I#GFCTVA          *
      *                  SEGU1672                    I#GFCTZY          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                          INCLUDE/BOOK   *
      *                    DB2PRD.BONIF_SERVC_TARIF       GFCTB023     *
      *                    DB2PRD.SERVC_TARIF_PRINC       GFCTB0D8     *
      *                    DB2PRD.BONIF_SERVC_TARIF       GFCTB092     *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#BRAD7C - AREA UTILIZADA PELA BRAD7100.                    *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - TRATAMENTO DE ERRO.                              *
      *    BRAD7600 - OBTER DATA E HORA.                               *
      *    BRAD1050 - CONEXAO COM DB2.                                 *
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

           SELECT BONIFICA ASSIGN      TO UT-S-BONIFICA
           FILE STATUS                 IS WRK-FS-BONIFICA.

           SELECT SAID1672 ASSIGN      TO UT-S-SAID1672
           FILE STATUS                 IS WRK-FS-SAID1672.

           SELECT SEGU1672 ASSIGN      TO UT-S-SEGU1672
           FILE STATUS                 IS WRK-FS-SEGU1672.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA  -   BONIFICA                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   250             *
      *----------------------------------------------------------------*

       FD  BONIFICA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-BONIFICA                  PIC  X(250).

      *----------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO DE SAIDA    -   SAID1672                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   250             *
      *----------------------------------------------------------------*

       FD  SAID1672
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-SAID1672                  PIC  X(250).

      *----------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO DE SAIDA    -   SEGU1672                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   320             *
      *----------------------------------------------------------------*

       FD  SEGU1672
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-SEGU1672                  PIC  X(320).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'INICIO DA WORKING SECTION STORAGE'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-BONIFICA          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-SEGU1672          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-SAID1672          PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-BATCH             PIC  X(008)         VALUE
           'BATCH'.

       77  WRK-MASCARA           PIC  ZZZ.ZZZ.Z99          VALUE SPACES.
       77  WRK-TARIFA-ATU        PIC  S9(005)              VALUE ZEROS.
       77  WRK-TARIFA-ANT        PIC  S9(005)              VALUE ZEROS.
       77  WRK-DATA-REG-BONIF    PIC  X(010)               VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(031)         VALUE
           'AREA PARA TESTES DE FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-FS-BONIFICA             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-SAID1672             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-SEGU1672             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(031)         VALUE
           'MENSAGEM DE ERRO DE FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD7100.
           05 FILLER                   PIC  X(009)         VALUE
              '*** ERRO '.
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE
              ' - FILE-STATUS = '.
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(004)         VALUE ' ***'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA LAYOUT DE ARQUIVOS'.
      *----------------------------------------------------------------*

       COPY I#GFCTVA.
       COPY I#GFCTZY.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA BRAD7100'.
      *----------------------------------------------------------------*

       COPY I#BRAD7C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB023
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB092
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING SECTION STORAGE'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    PROCEDIMENTOS INICIAIS DO PROGRAMA.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-BONIFICA     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ABERTURA DOS ARQUIVOS E TESTE DE FILE-STATUS.               *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           OPEN INPUT  BONIFICA
                OUTPUT SAID1672
                       SEGU1672.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DOS ARQUIVOS.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-BONIFICA.

           PERFORM 1120-TESTAR-FS-SAID1672.

           PERFORM 1130-TESTAR-FS-SEGU1672.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO BONIFICA.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-BONIFICA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-BONIFICA         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'BONIFICA'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-BONIFICA   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO SAID1672.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-SAID1672         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAID1672         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'SAID1672'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-SAID1672   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO SEGU1672.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-SEGU1672         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SEGU1672         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'SEGU1672'         TO WRK-NOME-ARQ
               MOVE  WRK-FS-SEGU1672   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR SE ARQUIVO BONIFICA VAZIO.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-BONIFICA.

           IF  WRK-FS-BONIFICA         EQUAL '10'
               DISPLAY '***************** GFCT1672 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*         ARQUIVO BONIFICA VAZIO           *'
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO ENCERRADO          *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT1672 *****************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO BONIFICA.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-BONIFICA               SECTION.
      *----------------------------------------------------------------*

           READ BONIFICA               INTO GFCTVA-GFCTB092.

           IF  WRK-FS-BONIFICA         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-BONIFICA.

           MOVE GFCTVA-CSERVC-TARIF    TO WRK-TARIFA-ATU.

           ADD 1                       TO ACU-LIDOS-BONIFICA.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-TARIFA-ATU          EQUAL WRK-TARIFA-ANT
               PERFORM 3100-VERIF-EXIS-BONIF
           ELSE
               PERFORM 3200-BUSCA-CADAS-TARIF
           END-IF.

           MOVE WRK-TARIFA-ATU         TO WRK-TARIFA-ANT
           PERFORM 2100-LER-BONIFICA.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA CAUCULO E MOVIMENTACAO DE VALORES               *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-VERIF-EXIS-BONIF           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CSERVC-TARIF    TO CSERVC-TARIF     OF GFCTB023.
           MOVE 2                      TO CTPO-BONIF       OF GFCTB023.
           MOVE 7                      TO CCRITR-BONIF     OF GFCTB023.
           MOVE 1                      TO CSEQ-BONIF-SERVC OF GFCTB023.
           MOVE GFCTVA-DOCOR-EVNTO     TO WRK-DATA-REG-BONIF.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CTPO_BONIF,
                   CCRITR_BONIF,
                   DINIC_REGRA_BONIF,
                   CSEQ_BONIF_SERVC,
                   DFIM_REGRA_BONIF
             INTO
                   :GFCTB023.CSERVC-TARIF,
                   :GFCTB023.CTPO-BONIF,
                   :GFCTB023.CCRITR-BONIF,
                   :GFCTB023.DINIC-REGRA-BONIF,
                   :GFCTB023.CSEQ-BONIF-SERVC,
                   :GFCTB023.DFIM-REGRA-BONIF
             FROM   DB2PRD.BONIF_SERVC_TARIF
             WHERE
                   CSERVC_TARIF       = :GFCTB023.CSERVC-TARIF       AND
                   CTPO_BONIF         = :GFCTB023.CTPO-BONIF         AND
                   CCRITR_BONIF       = :GFCTB023.CCRITR-BONIF       AND
                   CSEQ_BONIF_SERVC   = :GFCTB023.CSEQ-BONIF-SERVC   AND
                   DINIC_REGRA_BONIF <= :WRK-DATA-REG-BONIF          AND
                   DFIM_REGRA_BONIF  >= :WRK-DATA-REG-BONIF
           END-EXEC.

           IF  (SQLCODE                    NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0                   EQUAL 'W')
                MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                MOVE 'BONIF_SERVC_TARIF '  TO ERR-DBD-TAB
                MOVE 'SELECT    '          TO ERR-FUN-COMANDO
                MOVE SQLCODE               TO ERR-SQL-CODE
                MOVE '0010'                TO ERR-LOCAL
                MOVE SPACES                TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 3110-GRAVAR-SAIDA1672
           ELSE
               PERFORM 3120-VERIF-QTD-EVTO
           END-IF.


      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MOVIMENTACAO DE DADOS PARA O ARQUIVO SEGU1672.              *
      ******************************************************************
      *----------------------------------------------------------------*
       3110-GRAVAR-SAIDA1672           SECTION.
      *----------------------------------------------------------------*

           WRITE FD-SAID1672           FROM GFCTVA-GFCTB092.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-SAID1672.

           ADD 1                       TO ACU-GRAVA-SAID1672.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR REGISTROS NO ARQUIVO SEGU1672                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3120-VERIF-QTD-EVTO             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-DENVIO-MOVTO-TARIF  TO DENVIO-MOVTO-TARIF
                                           OF GFCTB092.
           MOVE GFCTVA-CROTNA-ORIGE-MOVTO  TO CROTNA-ORIGE-MOVTO
                                           OF GFCTB092.
           MOVE GFCTVA-CNRO-ARQ-MOVTO      TO CNRO-ARQ-MOVTO
                                           OF GFCTB092.
           MOVE GFCTVA-CSEQ-MOVTO          TO CSEQ-MOVTO
                                           OF GFCTB092.

           EXEC SQL
             SELECT
                   QREG_INFMD_BONIF
             INTO
                   :GFCTB092.QREG-INFMD-BONIF
             FROM   DB2PRD.MOVTO_EVNTO_CRRTT
             WHERE
                   DENVIO_MOVTO_TARIF = :GFCTB092.DENVIO-MOVTO-TARIF AND
                   CROTNA_ORIGE_MOVTO = :GFCTB092.CROTNA-ORIGE-MOVTO AND
                   CNRO_ARQ_MOVTO     = :GFCTB092.CNRO-ARQ-MOVTO     AND
                   CSEQ_MOVTO         = :GFCTB092.CSEQ-MOVTO
           END-EXEC.

           IF  (SQLCODE                    NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0                   EQUAL 'W')
                MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                MOVE 'MOVTO_EVNTO_CRRTT '  TO ERR-DBD-TAB
                MOVE 'SELECT    '          TO ERR-FUN-COMANDO
                MOVE SQLCODE               TO ERR-SQL-CODE
                MOVE '0020'                TO ERR-LOCAL
                MOVE SPACES                TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100 OR
               QREG-INFMD-BONIF        OF GFCTB092
                                       EQUAL ZEROS
               PERFORM 3110-GRAVAR-SAIDA1672
           ELSE
               PERFORM 3130-MOVER-DADOS-GFCTZY
               PERFORM 3140-GRAVAR-SEGU1672
           END-IF.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MOVER DADOS PARA ARQUIVO SEGU1672                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3130-MOVER-DADOS-GFCTZY         SECTION.
      *----------------------------------------------------------------*

            MOVE GFCTVA-GFCTB092(1:235)
                                       TO ZY-GFCTB092.
            MOVE CSERVC-TARIF          OF GFCTB023
                                       TO ZY-23-CSERVC-TARIF.
            MOVE CTPO-BONIF            OF GFCTB023
                                       TO ZY-23-CTPO-BONIF.
            MOVE CCRITR-BONIF          OF GFCTB023
                                       TO ZY-23-CCRITR-BONIF.
            MOVE DINIC-REGRA-BONIF     OF GFCTB023
                                       TO ZY-23-DINIC-REGRA-BONIF.
            MOVE CSEQ-BONIF-SERVC      OF GFCTB023
                                       TO ZY-23-CSEQ-BONIF-SERVC.
            MOVE QREG-INFMD-BONIF      OF GFCTB092
                                       TO ZY-CONSE-DADO-CONS.

      *----------------------------------------------------------------*
       3130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVACAO DE REGISTROS NO ARQUIVO SAID1672                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3140-GRAVAR-SEGU1672            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-SEGU1672           FROM GFCTZY-REG.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-SEGU1672.

           ADD 1                       TO ACU-GRAVA-SEGU1672.

      *----------------------------------------------------------------*
       3140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    APLICACAO DE DESCONTO PERCENTUAL E MOVIMENTACAO DE VALORES  *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-BUSCA-CADAS-TARIF          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CSERVC-TARIF    TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CINDCD_QTD_REG
             INTO
                   :GFCTB0D8.CSERVC-TARIF,
                   :GFCTB0D8.CINDCD-QTD-REG
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF  (SQLCODE                    NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0                   EQUAL 'W')
                MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                MOVE 'SERVC_TARIF_PRINC '  TO ERR-DBD-TAB
                MOVE 'SELECT    '          TO ERR-FUN-COMANDO
                MOVE SQLCODE               TO ERR-SQL-CODE
                MOVE '0030'                TO ERR-LOCAL
                MOVE SPACES                TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.


           IF  SQLCODE                 EQUAL +100 OR
               CINDCD-QTD-REG          OF GFCTB0D8
                                       NOT EQUAL 'S'
               PERFORM 3110-GRAVAR-SAIDA1672
           ELSE
               PERFORM 3100-VERIF-EXIS-BONIF
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZAR PROGRAMA.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE BONIFICA
                 SAID1672
                 SEGU1672.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMISSAO DE TOTAIS DE LIDOS E GRAVADOS.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY            SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************** GFCT1672 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-BONIFICA     TO WRK-MASCARA.
           DISPLAY '* REGISTROS LIDOS EM BONIFICA   :    ' WRK-MASCARA
                                                                   ' *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVA-SAID1672     TO WRK-MASCARA.
           DISPLAY '* REGISTROS GRAVADOS NO SAID1672:    ' WRK-MASCARA
                                                                   ' *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVA-SEGU1672     TO WRK-MASCARA.
           DISPLAY '* REGISTROS GRAVADOS NO SEGU1672:    ' WRK-MASCARA
                                                                   ' *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT1672 ********************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAMENTO PARA CANCELAMENTO DO PROGRAMA.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1672'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
