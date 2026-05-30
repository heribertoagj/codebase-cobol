      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0959.
       AUTHOR.     LEANDRO DAL PIZZOL.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT0959                                     *
      *    PROGRAMADOR  : LEANDRO DAL PIZZOL            - CPM/FPOLIS   *
      *    ANALISTA CPM : MARCIO RODRIGO DA CUNHA       - CPM/FPOLIS   *
      *    ANALISTA     : FRANCISCO - PROCWORK          - GRUPO 50     *
      *    DATA         : 13/04/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      CANCELAMENTO DA SOLICITACAO DE LIBERACAO DE SUSPENSAO DE  *
      *      COBRANCA DE TARIFA.                                       *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                                 INCLUDE/BOOK        *
      *      DB2PRD.SUSP_TARIF_AGPTO                 GFCTB0E7          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5522 - VERIFICAR DISPONIBILIDADE ON LINE              *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *                                                                *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
        CONFIGURATION                  SECTION.
      *----------------------------------------------------------------*

        SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*  INICIO DA WORKING GFCT0959  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*           INDEXADORES        *'.
      *----------------------------------------------------------------*

       01  IND-INDEXADORES.
           05 IND-1                    PIC  9(03) COMP-3   VALUE ZEROS.
           05 IND-2                    PIC  9(03) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-MODULO               PIC  X(08)          VALUE SPACES.
           05 WRK-ERRO.
             10 WRK-COD-TABELA         PIC  X(08)          VALUE SPACES.
             10 WRK-NOME-TABELA        PIC  X(18)          VALUE SPACES.
             10 WRK-COMANDO-SQL        PIC  X(10)          VALUE SPACES.
             10 WRK-LOCAL-ERRO         PIC  X(04)          VALUE SPACES.
           05 WRK-TEXTO-X.
             10 WRK-TEXTO              PIC  X(75)          VALUE SPACES.
             10 FILLER                 PIC  X(04)          VALUE SPACES.
           05 WRK-TEXTO-X-70.
             10 WRK-TEXTO-70           PIC  X(70)          VALUE SPACES.
             10 FILLER                 PIC  X(09)          VALUE SPACES.
           05 WRK-CFUNC-BDSCO.
             10 FILLER                 PIC  9(02)          VALUE ZEROS.
             10 WRK-FUNC-BDSCO         PIC  X(07)          VALUE SPACES.
           05 WRK-CFUNC-BDSCO-R        REDEFINES  WRK-CFUNC-BDSCO
                                       PIC 9(09).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     AREA DE MENSAGENS        *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           05  WRK-MSG05.
             10 FILLER                 PIC  X(26)          VALUE
                'ERRO NA CHAMADA DO MODULO '.
             10 WRK-MODULO-MSG05       PIC  X(08)          VALUE ZEROS.
             10 FILLER                 PIC  X(41)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
            '*    AREA DO MODULO GFCT5522   *'.
      *----------------------------------------------------------------*

       COPY I#GFCTFZ.

       COPY I#GFCTG1.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
            '*    AREA DO MODULO GFCT5523   *'.
      *----------------------------------------------------------------*

       COPY I#GFCTG2.

       COPY I#GFCTG3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0E7
           END-EXEC.

      *----------------------------------------------------------------*
      *01  FILLER                      PIC  X(32)          VALUE
      *    '*    FIM DA WORKING GFCT0959   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCTWE.
       COPY I#GFCTWF.
       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTWE-ENTRADA
                                             GFCTWF-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL                                            *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 0100-INICIALIZAR.

           PERFORM 0900-PROCESSAR-DADOS.

           PERFORM 0500-FINALIZAR-PROGRAMA.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INICIALIZACAO DO PROGRAMA                                   *
      *----------------------------------------------------------------*
       0100-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA
                                          GFCTG3-SAIDA
                                          GFCTWF-SAIDA
                                          GFCTG2-ENTRADA
                                          GFCTFZ-ENTRADA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTG1-SAIDA
                      GFCTG3-SAIDA
                      GFCTWF-SAIDA
                      GFCTG2-ENTRADA
                      GFCTFZ-ENTRADA
                      GFCT0M-AREA-ERROS.

           PERFORM 0200-TRATAR-DISP-SISTEMA.

           PERFORM 0300-MONTAR-AREA-SAIDA.

           PERFORM 0600-CONSISTIR-DADOS.

           PERFORM 0700-CONSISTIR-OCORRENCIA.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VERIFICA DISPONIBILIDADE DO SISTEMA                         *
      *----------------------------------------------------------------*
       0200-TRATAR-DISP-SISTEMA        SECTION.
      *----------------------------------------------------------------*

           MOVE +100                   TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE GFCTWE-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTWE-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTWE-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTWF-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0959'         TO GFCT0M-TRANSACAO

               PERFORM 0500-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCTG1-ERRO             NOT EQUAL ZEROS
               MOVE 'GFCT0959'         TO GFCT0M-TRANSACAO
               MOVE GFCTG1-ERRO        TO GFCTWF-ERRO

               IF  GFCT0M-TIPO-ACESSO  EQUAL 'DB2'
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO GFCTWF-COD-SQL-ERRO
                   MOVE GFCTG1-COD-MSG-ERRO
                                       TO GFCTWF-COD-MSG-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCTWF-DESC-MSG-ERRO
               ELSE
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO WRK-TEXTO-X
                   MOVE WRK-TEXTO      TO GFCT0M-TEXTO
               END-IF

               PERFORM 0500-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO GFCTWF-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE 'GFCT0959'         TO GFCT0M-TRANSACAO

               PERFORM 0500-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INICIALIZA AREA DE SAIDA                                    *
      *----------------------------------------------------------------*
       0300-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE +415                   TO GFCTWF-LL.
           MOVE ZEROS                  TO GFCTWF-ZZ.
           MOVE GFCTWE-TRANSACAO       TO GFCTWF-TRANSACAO.
           MOVE GFCTWE-FUNCAO          TO GFCTWF-FUNCAO.
           MOVE GFCTWE-FILTRO          TO GFCTWF-FILTRO.
           MOVE GFCTWE-FUNC-BDSCO      TO GFCTWF-FUNC-BDSCO.
           MOVE 'S'                    TO GFCTWF-FIM.
           MOVE ZEROS                  TO GFCTWF-COD-SQL-ERRO
                                          GFCTWF-COD-MSG-ERRO
                                          GFCTWF-ERRO.
           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 0400-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO WRK-TEXTO-70.
           MOVE WRK-TEXTO-X-70         TO GFCTWF-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTEM DESCRICAO DA MENSAGEM                                 *
      *----------------------------------------------------------------*
       0400-OBTER-DESC-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTWE-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTWE-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTWE-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTWF-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0959'         TO GFCT0M-TRANSACAO

               PERFORM 0500-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 'GFCT0959'         TO GFCT0M-TRANSACAO
               MOVE GFCTG3-ERRO        TO GFCTWF-ERRO

               IF  GFCT0M-TIPO-ACESSO  EQUAL 'DB2'
                   MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTWF-COD-SQL-ERRO
                   MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTWF-COD-MSG-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTWF-DESC-MSG-ERRO
               ELSE
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE GFCTG3-DESC-MSG-ERRO
                                       TO WRK-TEXTO-X
                   MOVE WRK-TEXTO      TO GFCT0M-TEXTO
               END-IF

               PERFORM 0500-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE GFCTG3-ERRO        TO GFCTWF-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTWF-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTWF-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTWF-DESC-MSG-ERRO

               PERFORM 1200-MONTAR-OCOR-SAIDA
                   VARYING IND-1       FROM 1 BY 1
                   UNTIL IND-1         GREATER GFCTWE-QTDE-OCOR

               PERFORM 0500-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    RETORNA AO CHAMADOR                                         *
      *----------------------------------------------------------------*
       0500-FINALIZAR-PROGRAMA         SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CONSISTE CAMPOS DE ENTRADA                                  *
      *----------------------------------------------------------------*
       0600-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTWE-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTWE-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTWE-FL-COD-AGRUPAMENTO
                                       NOT NUMERIC)                OR
              (GFCTWE-FL-COD-AGRUPAMENTO
                                       EQUAL ZEROS)                OR
              (GFCTWE-FL-COD-TARIFA    NOT NUMERIC)                OR
              (GFCTWE-QTDE-OCOR        NOT NUMERIC)                OR
              (GFCTWE-QTDE-OCOR        EQUAL ZEROS)                OR
              (GFCTWE-QTDE-OCOR        GREATER 05)                 OR
              (GFCTWE-QTDE-TOT-REG     NOT NUMERIC)                OR
              (GFCTWE-FIM              NOT EQUAL 'S' AND 'N')
               MOVE 'S'                TO GFCTWF-FIM
               MOVE 1                  TO GFCTWF-ERRO
                                          GFCTWF-COD-MSG-ERRO
               MOVE ZEROS              TO GFCTWF-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG

               PERFORM 0400-OBTER-DESC-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WRK-TEXTO-70
               MOVE WRK-TEXTO-X-70     TO GFCTWF-DESC-MSG-ERRO

               PERFORM 1200-MONTAR-OCOR-SAIDA
                   VARYING IND-1       FROM 1 BY 1
                   UNTIL IND-1         GREATER 05

               PERFORM 0500-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CONSISTE CAMPOS DA OCORRENCIA                               *
      *----------------------------------------------------------------*
       0700-CONSISTIR-OCORRENCIA       SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING IND-1       FROM 1 BY 1
                   UNTIL IND-1         GREATER GFCTWE-QTDE-OCOR
               IF  GFCTWE-SEQ-AGRUPAMENTO (IND-1)
                                       NOT NUMERIC         OR
                   GFCTWE-COD-TARIFA (IND-1)
                                       NOT NUMERIC         OR
                   GFCTWE-COD-TARIFA (IND-1)
                                       EQUAL ZEROS         OR
                   GFCTWE-DESC-TARIFA(IND-1)
                                       EQUAL SPACES        OR
                   GFCTWE-DESC-TARIFA(IND-1)
                                       EQUAL LOW-VALUES    OR
                   GFCTWE-COD-AGENCIA(IND-1)
                                       NOT NUMERIC         OR
                   GFCTWE-COD-POSTO(IND-1)
                                       NOT NUMERIC         OR
ST25X6*            GFCTWE-COD-CNPJ-CPF(IND-1)
ST25X6*                                   NOT NUMERIC         OR
ST25X6             GFCTWE-COD-CNPJ-CPF(IND-1)
ST25X6                                   EQUAL SPACES        OR
ST25X6             GFCTWE-COD-CNPJ-CPF(IND-1)
ST25X6                                   EQUAL LOW-VALUES    OR
                   GFCTWE-COD-MUNICIPIO(IND-1)
                                       NOT NUMERIC         OR
ST25X6*           (GFCTWE-FL-COD-AGRUPAMENTO
ST25X6*                                  EQUAL 4            AND
ST25X6*            GFCTWE-COD-CNPJ-CPF(IND-1)
ST25X6*                                  EQUAL ZEROS)        OR
                  (GFCTWE-FL-COD-AGRUPAMENTO
                                       EQUAL 4            AND
                   (GFCTWE-COD-CNPJ-CPF(IND-1)
                                       EQUAL SPACES        OR
                    GFCTWE-COD-CNPJ-CPF(IND-1)
                                       EQUAL LOW-VALUES))   OR
                  (GFCTWE-FL-COD-AGRUPAMENTO
                                       EQUAL 15           AND
                  (GFCTWE-COD-AGENCIA(IND-1)
                                       EQUAL ZEROS         OR
                   GFCTWE-COD-POSTO(IND-1)
                                       EQUAL ZEROS))       OR
                  (GFCTWE-FL-COD-AGRUPAMENTO
                                       EQUAL 18           AND
                   GFCTWE-COD-MUNICIPIO(IND-1)
                                       EQUAL ZEROS)        OR
                  (GFCTWE-FL-COD-AGRUPAMENTO
                                       EQUAL 19           AND
                  (GFCTWE-COD-UF(IND-1)
                                       EQUAL SPACES        OR
                   GFCTWE-COD-UF(IND-1)
                                       EQUAL LOW-VALUES))  OR
                  (GFCTWE-FL-COD-AGRUPAMENTO
                                       NOT EQUAL 4        AND
                  (GFCTWE-DESC-COMPONENTE(IND-1)
                                       EQUAL SPACES        OR
                   GFCTWE-DESC-COMPONENTE(IND-1)
                                       EQUAL LOW-VALUES))  OR
                   GFCTWE-DATA-INIC-SUSP(IND-1)
                                       EQUAL SPACES        OR
                   GFCTWE-DATA-INIC-SUSP(IND-1)
                                       EQUAL LOW-VALUES    OR
                   GFCTWE-DATA-FIM-SUSP(IND-1)
                                       EQUAL SPACES        OR
                   GFCTWE-DATA-FIM-SUSP(IND-1)
                                       EQUAL LOW-VALUES    OR
                   GFCTWE-DATA-COBRANCA(IND-1)
                                       EQUAL SPACES        OR
                   GFCTWE-DATA-COBRANCA(IND-1)
                                       EQUAL LOW-VALUES
                   MOVE 1              TO GFCTWF-ERRO
                                          GFCTWF-COD-MSG-ERRO
                   MOVE ZEROS          TO GFCTWF-COD-SQL-ERRO
                   MOVE '0001'         TO GFCTG2-COD-MSG

                   PERFORM 0400-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WRK-TEXTO-70
                   MOVE WRK-TEXTO-X-70 TO GFCTWF-DESC-MSG-ERRO

                   PERFORM 1200-MONTAR-OCOR-SAIDA
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER GFCTWE-QTDE-OCOR

                   PERFORM 0500-FINALIZAR-PROGRAMA
TESTE          ELSE
TESTE              IF  GFCTWE-FL-COD-AGRUPAMENTO
TESTE                                  EQUAL 14 AND
TESTE                  GFCTWE-COD-AGENCIA(IND-1)
TESTE                                  EQUAL ZEROS
TESTE                  PERFORM 0710-CONSISTE-AGRUP-TOTAL
TESTE
TESTE                  IF  CINDCD-AGPTO-TOT OF GFCTB0E7
TESTE                                  NOT EQUAL 'S'
TESTE                      MOVE 1      TO GFCTWF-ERRO
TESTE                                  GFCTWF-COD-MSG-ERRO
TESTE                      MOVE ZEROS  TO GFCTWF-COD-SQL-ERRO
TESTE                      MOVE '0001' TO GFCTG2-COD-MSG
TESTE
TESTE                      PERFORM 0400-OBTER-DESC-MENSAGEM
TESTE
TESTE                      MOVE GFCTG3-DESC-MSG
TESTE                                  TO WRK-TEXTO-70
TESTE                      MOVE WRK-TEXTO-X-70
TESTE                                  TO GFCTWF-DESC-MSG-ERRO
TESTE
TESTE                      PERFORM 1200-MONTAR-OCOR-SAIDA
TESTE                      VARYING IND-1
TESTE                                  FROM 1 BY 1
TESTE                        UNTIL IND-1
TESTE                                  GREATER GFCTWE-QTDE-OCOR
TESTE
TESTE                      PERFORM 0500-FINALIZAR-PROGRAMA
TESTE                  END-IF
TESTE              END-IF
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

TESTE *----------------------------------------------------------------*
TESTE *    OBTEM INFORMACAO DE AGRUPAMENTO TOTAL DE AGENCIA            *
TESTE *----------------------------------------------------------------*
TESTE  0710-CONSISTE-AGRUP-TOTAL       SECTION.
TESTE *----------------------------------------------------------------*
TESTE
TESTE      MOVE  GFCTWE-COD-TARIFA(IND-1)
TESTE                                  TO  CSERVC-TARIF    OF  GFCTB0E7.
TESTE      MOVE  GFCTWE-FL-COD-AGRUPAMENTO
TESTE                                  TO  CAGPTO-CTA      OF  GFCTB0E7.
TESTE      MOVE  GFCTWE-DATA-INIC-SUSP(IND-1)
TESTE                                  TO  DINIC-SUSP-COBR OF  GFCTB0E7.
TESTE      MOVE  GFCTWE-SEQ-AGRUPAMENTO(IND-1)
TESTE                                  TO  CSEQ-AGPTO-CTA  OF  GFCTB0E7.
TESTE
TESTE      EXEC SQL
TESTE         SELECT
TESTE                  CINDCD_AGPTO_TOT
TESTE           INTO
TESTE                 :GFCTB0E7.CINDCD-AGPTO-TOT
TESTE
TESTE           FROM   DB2PRD.SUSP_TARIF_AGPTO
TESTE
TESTE           WHERE  CSERVC_TARIF    = :GFCTB0E7.CSERVC-TARIF
TESTE           AND    CAGPTO_CTA      = :GFCTB0E7.CAGPTO-CTA
TESTE           AND    DINIC_SUSP_COBR = :GFCTB0E7.DINIC-SUSP-COBR
TESTE           AND    CSEQ_AGPTO_CTA  = :GFCTB0E7.CSEQ-AGPTO-CTA
TESTE
TESTE      END-EXEC.
TESTE
TESTE      IF (SQLCODE                 NOT EQUAL ZEROS) OR
TESTE         (SQLWARN0                EQUAL 'W'      )
TESTE          MOVE SPACES             TO GFCT0M-ERRO-SQL
TESTE
TESTE          INITIALIZE              GFCT0M-ERRO-SQL
TESTE
TESTE          MOVE 'GFCTB0E7'         TO WRK-COD-TABELA
TESTE          MOVE 'SUSP_TARIF_AGPTO' TO WRK-NOME-TABELA
TESTE          MOVE 'SELECT'           TO WRK-COMANDO-SQL
TESTE          MOVE '0001'             TO WRK-LOCAL-ERRO
TESTE
TESTE          PERFORM 1300-MONTAR-AREA-COMUM-DB2
TESTE
TESTE          MOVE SQLCODE            TO GFCTWF-COD-SQL-ERRO
TESTE          MOVE 9                  TO GFCTWF-ERRO
TESTE          MOVE 0012               TO GFCTWF-COD-MSG-ERRO
TESTE          MOVE '0012'             TO GFCTG2-COD-MSG
TESTE
TESTE          PERFORM 0400-OBTER-DESC-MENSAGEM
TESTE
TESTE          PERFORM 0800-CONC-MSG
TESTE
TESTE          PERFORM 0500-FINALIZAR-PROGRAMA
TESTE      END-IF.
TESTE
TESTE *----------------------------------------------------------------*
TESTE  0710-99-FIM.                    EXIT.
TESTE *----------------------------------------------------------------

      *----------------------------------------------------------------*
      *    CONCATENA DESCRICAO DE MENSAGEM DE ERRO COM TABELA          *
      *----------------------------------------------------------------*
       0800-CONC-MSG                   SECTION.
      *----------------------------------------------------------------*

           STRING GFCTG3-DESC-MSG ' - '
                  WRK-NOME-TABELA
           DELIMITED BY '  '           INTO GFCTWF-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL                                     *
      *----------------------------------------------------------------*
       0900-PROCESSAR-DADOS            SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING IND-1       FROM 1 BY 1
                   UNTIL IND-1         GREATER GFCTWE-QTDE-OCOR
               IF  GFCTWE-SEQ-AGRUPAMENTO (IND-1)
                                       NOT EQUAL ZEROS
                   PERFORM 1000-MONTA-DADOS-CANCELAMENTO

                   PERFORM 1100-CANCELAR-SOLICITACAO
               END-IF
           END-PERFORM.

           IF  GFCTWF-ERRO         NOT EQUAL ZEROS
               MOVE GFCTWF-COD-SQL-ERRO-GEN (1)
                                       TO GFCTWF-COD-SQL-ERRO
               MOVE GFCTWF-COD-MSG-ERRO-GEN (1)
                                       TO GFCTWF-COD-MSG-ERRO
                                          GFCTG2-COD-MSG

               PERFORM 0400-OBTER-DESC-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WRK-TEXTO-70
               MOVE WRK-TEXTO-X-70     TO GFCTWF-DESC-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    MONTAR DADOS CANCELAMENTO SOLICITACAO, LIBERACAO, SUSPENCAO *
      *    COBRANCA BANCARIA                                           *
      *----------------------------------------------------------------*
       1000-MONTA-DADOS-CANCELAMENTO   SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWE-COD-TARIFA(IND-1)
                                       TO CSERVC-TARIF      OF GFCTB0E7.
           MOVE GFCTWE-FL-COD-AGRUPAMENTO
                                       TO CAGPTO-CTA        OF GFCTB0E7.
           MOVE GFCTWE-DATA-INIC-SUSP(IND-1)
                                       TO DINIC-SUSP-COBR   OF GFCTB0E7.
           MOVE GFCTWE-SEQ-AGRUPAMENTO(IND-1)
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0E7.

           MOVE 'C'                    TO CINDCD-LIBRC-COBR OF GFCTB0E7.
           MOVE '01.01.0001'           TO DCOBR-SUSP-LIBRD  OF GFCTB0E7.
           MOVE GFCTWE-FUNC-BDSCO      TO WRK-FUNC-BDSCO.
           MOVE WRK-CFUNC-BDSCO-R      TO CFUNC-MANUT       OF GFCTB0E7.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CANCELAR SOLICITACAO, LIBERACAO, SUSPENCAO COBRANCA TARIFA  *
      *----------------------------------------------------------------*
       1100-CANCELAR-SOLICITACAO       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               UPDATE DB2PRD.SUSP_TARIF_AGPTO
                  SET CINDCD_LIBRC_COBR
                                       = :GFCTB0E7.CINDCD-LIBRC-COBR,
                      DCOBR_SUSP_LIBRD = :GFCTB0E7.DCOBR-SUSP-LIBRD,
                      HMANUT_REG       =  CURRENT TIMESTAMP,
                      CFUNC_MANUT      = :GFCTB0E7.CFUNC-MANUT
                WHERE
                      CSERVC_TARIF     = :GFCTB0E7.CSERVC-TARIF
                  AND CAGPTO_CTA       = :GFCTB0E7.CAGPTO-CTA
                  AND DINIC_SUSP_COBR  = :GFCTB0E7.DINIC-SUSP-COBR
                  AND CSEQ_AGPTO_CTA   = :GFCTB0E7.CSEQ-AGPTO-CTA
           END-EXEC

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCTB0E7'         TO WRK-COD-TABELA
               MOVE 'SUSP_TARIF_AGPTO' TO WRK-NOME-TABELA
               MOVE 'UPDATE'           TO WRK-COMANDO-SQL
               MOVE '0001'             TO WRK-LOCAL-ERRO

               PERFORM 1300-MONTAR-AREA-COMUM-DB2

               MOVE SQLCODE            TO GFCTWF-COD-SQL-ERRO
               MOVE 9                  TO GFCTWF-ERRO
               MOVE 0019               TO GFCTWF-COD-MSG-ERRO
               MOVE '0019'             TO GFCTG2-COD-MSG

               PERFORM 0400-OBTER-DESC-MENSAGEM

               PERFORM 0800-CONC-MSG

               PERFORM 0500-FINALIZAR-PROGRAMA
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTWF-ERRO

               PERFORM 1200-MONTAR-OCOR-SAIDA

               MOVE 1246               TO GFCTWF-COD-MSG-ERRO-GEN(IND-2)
               MOVE SQLCODE            TO GFCTWF-COD-SQL-ERRO-GEN(IND-2)
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    MONTA OCORRENCIAS DA SAIDA                                  *
      *----------------------------------------------------------------*
       1200-MONTAR-OCOR-SAIDA          SECTION.
      *----------------------------------------------------------------*

           ADD  +119                   TO GFCTWF-LL.

           ADD 1                       TO GFCTWF-QTDE-TOT-REG
                                          IND-2.

           MOVE GFCTWE-SEQ-AGRUPAMENTO(IND-1)
                                       TO GFCTWF-SEQ-AGRUPAMENTO(IND-2).
           MOVE GFCTWE-COD-TARIFA(IND-1)
                                       TO GFCTWF-COD-TARIFA(IND-2).
           MOVE GFCTWE-DESC-TARIFA(IND-1)
                                       TO GFCTWF-DESC-TARIFA(IND-2).
           MOVE GFCTWE-COD-AGENCIA(IND-1)
                                       TO GFCTWF-COD-AGENCIA(IND-2).
           MOVE GFCTWE-COD-POSTO(IND-1)
                                       TO GFCTWF-COD-POSTO(IND-2).
           MOVE GFCTWE-COD-CNPJ-CPF(IND-1)
                                       TO GFCTWF-COD-CNPJ-CPF(IND-2).
           MOVE GFCTWE-COD-MUNICIPIO(IND-1)
                                       TO GFCTWF-COD-MUNICIPIO(IND-2).
           MOVE GFCTWE-COD-UF(IND-1)   TO GFCTWF-COD-UF(IND-2).
           MOVE GFCTWE-DESC-COMPONENTE(IND-1)
                                       TO GFCTWF-DESC-COMPONENTE(IND-2).
           MOVE GFCTWE-DATA-INIC-SUSP(IND-1)
                                       TO GFCTWF-DATA-INIC-SUSP(IND-2).
           MOVE GFCTWE-DATA-FIM-SUSP(IND-1)
                                       TO GFCTWF-DATA-FIM-SUSP(IND-2).
           MOVE GFCTWE-DATA-COBRANCA(IND-1)
                                       TO GFCTWF-DATA-COBRANCA(IND-2).

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    MONTA AREA COMUM DE ERRO DB2                                *
      *----------------------------------------------------------------*
       1300-MONTAR-AREA-COMUM-DB2      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT0959'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE WRK-NOME-TABELA        TO GFCT0M-NOME-TAB.
           MOVE WRK-COMANDO-SQL        TO GFCT0M-COMANDO-SQL.
           MOVE SQLCODE                TO GFCT0M-SQLCODE.
           MOVE WRK-LOCAL-ERRO         TO GFCT0M-LOCAL.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
