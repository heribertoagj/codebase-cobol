      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5574.
       AUTHOR.     EVANISE NUNES.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5574                                    *
      *    ANALISTA....:   EVANISE NUNES           - PROCWORK /GP.75   *
      *    DATA........:   01/07/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR   DADOS  PARA   COMBO  DE  AGRUPA-    *
      *     MENTOS PARA LIBERACAO DE SUSPENSAO DE TARIFAS              *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.SUSP_GRP_CLI           GFCTB0E3       *
      *                   DB2PRD.SUSP_GRP_DEPDC         GFCTB0E4       *
      *                   DB2PRD.SUSP_GRP_MUN           GFCTB0E5       *
      *                   DB2PRD.SUSP_GRP_UF            GFCTB0E6       *
      *                   DB2PRD.SUSP_TARIF_AGPTO       GFCTB0E7       *
      *                   DB2PRD.SUSP_TARIF_CTA         GFCTB0E8       *
      *                   DB2PRD.TSUSP_GRP_PAB          GFCTB0I1       *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCTY8 - AREA DE COMUNICACAO - ENTRADA.                   *
      *    I#GFCTY9 - AREA DE COMUNICACAO - SAIDA.                     *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA (DESCRICAO DA MSG).*
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA   (DESCRICAO DA MSG).*
      *    I#GFCTFX - AREA DE COMUNICACAO - ENTRADA (OBTEM NIVEL ACES).*
      *    I#GFCTFY - AREA DE COMUNICACAO - SAIDA   (OBTEM NIVEL ACES).*
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA (VERIFICA ONLINE). *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA   (VERIFICA ONLINE). *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5522 - VERIFICAR ONLINE.                                *
      *    GFCT5518 - OBTEM NIVEL DE ACESSO DE AGRUPAMENTO PARA USUAR. *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
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
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-FINALIZAR               PIC  X(001)         VALUE SPACES.
       77  WRK-NIVEL                   PIC  9(003)         VALUE ZEROS.

       01  WRK-IND-LIBERACAO-INICIAL   PIC  X(001)         VALUE SPACES.
       01  WRK-IND-LIBERACAO-FINAL     PIC  X(001)         VALUE SPACES.
       01  WRK-IND-LIBERACAO-EXCECAO   PIC  X(001)         VALUE SPACES.

       01  WRK-MSG005.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5518 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTFX.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5518 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTFY.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTFZ.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTG1.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTG2.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTG3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0E7
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0F3
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSORES *'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE  CSR01-GFCTB0E7  CURSOR   FOR

               SELECT DISTINCT(CAGPTO_CTA)

                 FROM DB2PRD.SUSP_TARIF_AGPTO

                WHERE CAGPTO_CTA        >= :GFCTB0E7.CAGPTO-CTA
                AND   DFIM_SUSP_COBR    <= :GFCTB0E7.DFIM-SUSP-COBR
                AND  (CINDCD_LIBRC_COBR >= :WRK-IND-LIBERACAO-INICIAL
                AND   CINDCD_LIBRC_COBR <= :WRK-IND-LIBERACAO-FINAL)
                AND   CINDCD_LIBRC_COBR IN
                                      (SELECT DISTINCT
                                              T2.CINDCD_LIBRC_COBR
                                       FROM   DB2PRD.SUSP_TARIF_AGPTO T2
                                       WHERE  T2.CINDCD_LIBRC_COBR <>
                                             :WRK-IND-LIBERACAO-EXCECAO)
             ORDER BY CAGPTO_CTA
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCTY8.

       COPY I#GFCTY9.

       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTY8-ENTRADA
                                                      GFCTY9-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *CHAMA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO                    *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCEDIMENTOS INICIAIS DO PROGRAMA                              *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS.

           INITIALIZE GFCT0M-AREA-ERROS
                      WRK-FINALIZAR
                      IND-1.

           PERFORM 1100-VERIFICAR-ON-LINE.

           PERFORM 1200-INICIALIZAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICA A SITUACAO DO SISTEMA ( LIBERADO OU BLOQUEADO )       *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-ON-LINE          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE +100                   TO GFCTFZ-LL.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE GFCTY8-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTY8-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTY8-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.


           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTY9-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG005         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5574'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE 9                  TO GFCTY9-ERRO

               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT5574'     TO GFCT0M-TRANSACAO
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCT0M-TEXTO
               END-IF

               PERFORM 3000-FINALIZAR
            END-IF.

            IF GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO GFCTY9-ERRO
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5574'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
            END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * INICIALIZA A AREA DE SAIDA DOS DADOS.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-INICIALIZAR-AREA-SAIDA     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTY9-SAIDA.

           INITIALIZE GFCTY9-SAIDA.

           MOVE +185                   TO GFCTY9-LL.
           MOVE ZEROS                  TO GFCTY9-ZZ.
           MOVE GFCTY8-TRANSACAO       TO GFCTY9-TRANSACAO.
           MOVE GFCTY8-FUNCAO          TO GFCTY9-FUNCAO.
           MOVE GFCTY8-FILTRO          TO GFCTY9-FILTRO.

           IF GFCTY8-QTDE-OCOR         NOT NUMERIC
               MOVE ZEROS              TO GFCTY9-QTDE-OCOR
           ELSE
               MOVE GFCTY8-QTDE-OCOR   TO GFCTY9-QTDE-OCOR
           END-IF.

           IF GFCTY8-QTDE-TOT-REG      NOT NUMERIC
               MOVE ZEROS              TO GFCTY9-QTDE-TOT-REG
           ELSE
               MOVE GFCTY8-QTDE-TOT-REG
                                       TO GFCTY9-QTDE-TOT-REG
           END-IF.

           MOVE GFCTY8-FUNC-BDSCO      TO GFCTY9-FUNC-BDSCO.
           MOVE GFCTY8-PONTEIRO        TO GFCTY9-PONTEIRO.

           MOVE 'N'                    TO GFCTY9-FIM.

           MOVE ZEROS                  TO GFCTY9-ERRO
                                          GFCTY9-COD-MSG-ERRO
                                          GFCTY9-COD-SQL-ERRO.

           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO GFCTY9-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FAZ A CONSISTENCIA DOS DADOS DE ENTRADA                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTY8-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTY8-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTY8-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTY8-IND-LIBERACAO    NOT EQUAL 'N' AND 'A' AND
                                                 'E' AND 'T')      OR
              (GFCTY8-COD-AGPTO        NOT NUMERIC) OR
              (GFCTY8-QTDE-OCOR        NOT NUMERIC) OR
              (GFCTY8-QTDE-TOT-REG     NOT NUMERIC) OR
              (GFCTY8-PONT-AGPTO       NOT NUMERIC) OR
              (GFCTY8-FIM              NOT EQUAL 'S' AND 'N')
               MOVE  1                 TO GFCTY9-ERRO
                                          GFCTY9-COD-MSG-ERRO
               MOVE  ZEROS             TO GFCTY9-COD-SQL-ERRO
               MOVE  '0001'            TO GFCTG2-COD-MSG
               MOVE  'S'               TO GFCTY9-FIM

               PERFORM 1310-OBTER-DESC-MENSAGEM

               MOVE  GFCTG3-DESC-MSG   TO GFCTY9-DESC-MSG-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTEM DESCRICAO DA MENSAGEM                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE 100                    TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTY8-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTY8-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTY8-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTY9-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG005         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5574'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTY9-ERRO

               IF GFCT0M-TIPO-ACESSO   NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
                   MOVE WRK-MSG005     TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT5574'     TO GFCT0M-TRANSACAO
               END-IF

               PERFORM 3000-FINALIZAR
           END-IF.

           IF GFCTG3-ERRO              EQUAL 1
               MOVE 'S'                TO GFCTY9-FIM
               MOVE 1                  TO GFCTY9-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTY9-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTY9-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTY9-DESC-MSG-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-OBTER-NIVEL-ACESSO.

           PERFORM 2200-OBTER-DATA-PROCESSAMENTO.

           PERFORM 2300-ABRIR-CURSOR1.

           PERFORM 2310-LER-CURSOR1.

           PERFORM
               UNTIL WRK-FINALIZAR     EQUAL 'S'
               PERFORM 2320-PROCESSAR-CURSOR-GFCTB0E7

               IF  WRK-FINALIZAR       EQUAL SPACES
                   PERFORM 2310-LER-CURSOR1
               END-IF
           END-PERFORM.

           PERFORM 2330-FECHAR-CURSOR1.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTEM O NIVEL DE ACESSO ATRAVES DO MODULO GFCT5518.            *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-OBTER-NIVEL-ACESSO         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTFY-SAIDA.

           INITIALIZE GFCTFY-SAIDA.

           MOVE 'O'                    TO GFCTFX-TIPO-PROC.
           MOVE +100                   TO GFCTFX-LL.
           MOVE GFCTY8-FUNC-BDSCO      TO GFCTFX-FUNC-BDSCO.
           MOVE GFCTY8-TRANSACAO       TO GFCTFX-TRANSACAO.
           MOVE GFCTY8-FUNCAO          TO GFCTFX-FUNCAO.
           MOVE 'N'                    TO GFCTFX-FIM.

           MOVE 'GFCT5518'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFX-ENTRADA
                                             GFCTFY-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF GFCTFY-ERRO              NOT EQUAL ZEROS
               IF GFCTFY-ERRO          EQUAL 1
                   MOVE 'S'            TO GFCTY9-FIM
                   MOVE 1              TO GFCTY9-ERRO
                   MOVE ZEROS          TO GFCTY9-COD-SQL-ERRO
                                          GFCTY9-COD-MSG-ERRO
                   MOVE GFCTFY-DESC-MSG-ERRO
                                       TO GFCTY9-DESC-MSG-ERRO
               ELSE
                   IF GFCTFY-ERRO      EQUAL 9
                       MOVE 'S'        TO GFCTY9-FIM
                       MOVE 1          TO GFCTY9-ERRO
                       MOVE GFCTFY-COD-SQL-ERRO
                                       TO GFCTY9-COD-SQL-ERRO
                       MOVE ZEROS      TO GFCTY9-COD-MSG-ERRO
                       MOVE GFCTFY-DESC-MSG-ERRO
                                       TO GFCTY9-DESC-MSG-ERRO
                   END-IF
               END-IF

               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTFY-NIVEL           TO WRK-NIVEL.

           IF WRK-NIVEL                EQUAL ZEROS
               MOVE 'S'                TO GFCTY9-FIM
               MOVE 1                  TO GFCTY9-ERRO
               MOVE ZEROS              TO GFCTY9-COD-SQL-ERRO
               MOVE 0522               TO GFCTY9-COD-MSG-ERRO
               MOVE '0522'             TO GFCTG2-COD-MSG

               PERFORM 1310-OBTER-DESC-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO GFCTY9-DESC-MSG-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTEM DATA DE PROCESSAMENTO.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-OBTER-DATA-PROCESSAMENTO   SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT5574'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0260'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA

               MOVE 9                  TO GFCTY9-ERRO
               MOVE SQLCODE            TO GFCTY9-COD-SQL-ERRO
               MOVE 0012               TO GFCTY9-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG

               PERFORM 1310-OBTER-DESC-MENSAGEM

               STRING  GFCTG3-DESC-MSG  ' - GFCTB0A1'
               DELIMITED BY '  '       INTO GFCTY9-DESC-MSG-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ABRE O CURSOR CSR01-GFCTB0E7.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-ABRIR-CURSOR1              SECTION.
      *----------------------------------------------------------------*

           IF GFCTY8-PONT-AGPTO        EQUAL ZEROS
               MOVE ZEROS              TO CAGPTO-CTA        OF GFCTB0E7
           ELSE
               MOVE GFCTY8-PONT-AGPTO
                                       TO CAGPTO-CTA OF GFCTB0E7
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DFIM-SUSP-COBR    OF GFCTB0E7.

           IF  GFCTY8-IND-LIBERACAO    EQUAL 'N'
               MOVE 'E'                TO WRK-IND-LIBERACAO-EXCECAO

               MOVE 'C'                TO WRK-IND-LIBERACAO-INICIAL
               MOVE 'N'                TO WRK-IND-LIBERACAO-FINAL
           ELSE
               MOVE ' '                TO WRK-IND-LIBERACAO-EXCECAO

               IF GFCTY8-IND-LIBERACAO EQUAL 'T'
                  MOVE 'A'             TO WRK-IND-LIBERACAO-INICIAL
                  MOVE 'Z'             TO WRK-IND-LIBERACAO-FINAL
               ELSE
                  MOVE GFCTY8-IND-LIBERACAO
                                       TO WRK-IND-LIBERACAO-INICIAL
                                          WRK-IND-LIBERACAO-FINAL
               END-IF
           END-IF.

           EXEC SQL
               OPEN CSR01-GFCTB0E7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT5574'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'SUSP_TARIF_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA

               MOVE 9                  TO GFCTY9-ERRO
               MOVE SQLCODE            TO GFCTY9-COD-SQL-ERRO
               MOVE 0005               TO GFCTY9-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG

               PERFORM 1310-OBTER-DESC-MENSAGEM

               STRING  GFCTG3-DESC-MSG  ' - GFCTB0E7'
               DELIMITED BY '  '       INTO GFCTY9-DESC-MSG-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FAZ A LEITURA DE CSR01-GFCTB0E7.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-LER-CURSOR1                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR01-GFCTB0E7  INTO
                   :GFCTB0E7.CAGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT5574'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'SUSP_TARIF_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA

               MOVE 9                  TO GFCTY9-ERRO
               MOVE SQLCODE            TO GFCTY9-COD-SQL-ERRO
               MOVE 0006               TO GFCTY9-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG

               PERFORM 1310-OBTER-DESC-MENSAGEM

               STRING  GFCTG3-DESC-MSG  ' - GFCTB0E7'
               DELIMITED BY '  '       INTO GFCTY9-DESC-MSG-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTY9-FIM
               IF  IND-1               EQUAL ZEROS AND
                   GFCTY8-PONT-AGPTO   EQUAL ZEROS
                   MOVE 1              TO GFCTY9-ERRO
                   MOVE ZEROS          TO GFCTY9-COD-SQL-ERRO
                   MOVE 1215           TO GFCTY9-COD-MSG-ERRO
                   MOVE '1215'         TO GFCTG2-COD-MSG

                   PERFORM 1310-OBTER-DESC-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTY9-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EXECUTA O PROCESSAMENTO DAS INFORMACOES OBTIDAS NOS CURSORES.  *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-PROCESSAR-CURSOR-GFCTB0E7  SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB0E7
                                       TO CAGPTO-CTA OF GFCTB0F3.

           PERFORM 2321-OBTER-DADOS-GFCTB0F3.

           IF WRK-NIVEL                NOT LESS CNVEL-ACSSO-AGPTO
                                       OF GFCTB0F3
               ADD 1                   TO IND-1

               IF IND-1                GREATER 25
                   MOVE 'S'            TO WRK-FINALIZAR

                   MOVE CAGPTO-CTA OF GFCTB0E7
                                       TO GFCTY9-PONTEIRO
               ELSE
                   ADD +33                 TO GFCTY9-LL

                   ADD 1                   TO GFCTY9-QTDE-TOT-REG

                   MOVE CAGPTO-CTA         OF GFCTB0E7
                                           TO GFCTY9-COD-AGPTO(IND-1)
                   MOVE RAGPTO-CTA         OF GFCTB0F3
                                           TO GFCTY9-DESC-AGPTO(IND-1)
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FECHA CURSOR CSR01-GFCTB0E7.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2330-FECHAR-CURSOR1             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0E7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT5574'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'SUSP_TARIF_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE '           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA

               MOVE 9                  TO GFCTY9-ERRO
               MOVE SQLCODE            TO GFCTY9-COD-SQL-ERRO
               MOVE 0011               TO GFCTY9-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG

               PERFORM 1310-OBTER-DESC-MENSAGEM

               STRING  GFCTG3-DESC-MSG  ' - GFCTB0E7'
               DELIMITED BY '  '       INTO GFCTY9-DESC-MSG-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FAZ SELECT NA TABELA GFCTB0F3.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2321-OBTER-DADOS-GFCTB0F3       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT  RAGPTO_CTA,
                     CNVEL_ACSSO_AGPTO
               INTO  :GFCTB0F3.RAGPTO-CTA,
                     :GFCTB0F3.CNVEL-ACSSO-AGPTO
               FROM  DB2PRD.TPO_AGPTO_CTA
              WHERE  CAGPTO_CTA         = :GFCTB0F3.CAGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS
                                       AND +100 AND -811) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT5574'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TPO_AGPTO_CTA   ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA

               MOVE 9                  TO GFCTY9-ERRO
               MOVE SQLCODE            TO GFCTY9-COD-SQL-ERRO
               MOVE 0012               TO GFCTY9-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG

               PERFORM 1310-OBTER-DESC-MENSAGEM

               STRING  GFCTG3-DESC-MSG  ' - GFCTB0F3'
               DELIMITED BY '  '       INTO GFCTY9-DESC-MSG-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO GFCTY9-FIM
               MOVE 1                  TO GFCTY9-ERRO
               MOVE ZEROS              TO GFCTY9-COD-SQL-ERRO
               MOVE 0834               TO GFCTY9-COD-MSG-ERRO
               MOVE '0834'             TO GFCTG2-COD-MSG

               PERFORM 1310-OBTER-DESC-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO GFCTY9-DESC-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FINALIZA O PROCESSAMENTO E RETORNA AO PROGRAMA CHAMADOR.        *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
