      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0457.
       AUTHOR.     MARCELO FARIA.
      *================================================================*
      *                     C P M - S I S T E M A S                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT0457                                     *
      *    PROGRAMADOR.:  MARCELO FARIA            - CPM PATO BRANCO   *
      *    ANALISTA CPM:  ELIAS BOSCATO            - CPM PATO BRANCO   *
      *    ANALISTA....:  RENATO TAMANAHA          - PROCKWORK - GP.50 *
      *    DATA........:  21/02/2006                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  UNIFICACAO DE DEBITO - INCLUSAO CPF/CNPJ.    *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                 TABLE                      INCLUDE/BOOK        *
      *                   DB2PRD.TDEB_UNFCA_CLI      GFCTB0H8          *
      *                   DB2PRD.PARM_DATA_PROCM     GFCTB0A1          *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *    I#GFCTIQ - UNIFICACAO DEBITO                                *
      *    I#GFCTIR - UNIFICACAO DEBITO                                *
      *    I#GFCTIU - TEXT NOME DO CLIENTE                             *
      *    I#GFCTIV - TEXT NOME DO CLIENTE                             *
      *    I#GFCTG2 - MODULO DE MENSAGEM                               *
      *    I#GFCTG3 - MODULO DE MENSAGEM                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS                     *
      *    GFCT5110 - TEXT NOME DO CLIENTE                             *
      *    POOL0081 - ALOCACAO DINAMICA DE UM MODULO                   *
      *    POOL7100 - MODULO PARA TRATAMENTO DE ERROS                  *
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
           '*** INICIO DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INDEXADOR ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC 9(003) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-CONT-UNIFICACAO         PIC S9(004) COMP    VALUE ZEROS.
       01  WRK-ACHOU-DUPLIC            PIC  X(001)         VALUE SPACES.
       01  WRK-NOME-TAB                PIC  X(011)         VALUE SPACES.
       01  WRK-TIMESTAMP               PIC  X(026)         VALUE SPACES.
       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       01  WRK-CFUNC-BDSCO             PIC  9(009)         VALUE ZEROS.
       01  FILER                       REDEFINES WRK-CFUNC-BDSCO.
           03  FILLER                  PIC  9(002).
           03  WRK-CFUNC-BDSCO-ALFA    PIC  X(007).

       01  WRK-CPF-CNPJ                PIC S9(009)V COMP-3 VALUE +0.
       01  WRK-FILIAL                  PIC S9(005)V COMP-3 VALUE +0.
       01  WRK-TARIFA                  PIC S9(005)V COMP-3 VALUE +0.
       01  WRK-HINCL                   PIC  X(026)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG01.
           03  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO '.
           03  WRK-NOME-MOD            PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO GFCT5110 ***'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTIU'.

       COPY 'I#GFCTIV'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO GFCT5523 ***'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TABELAS DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0H8
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTIQ'.

       COPY 'I#GFCTIR'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTIQ-ENTRADA
                                             GFCTIR-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      * ROTINA PRINCIPAL DO PROGRAMA.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA INICIALIZACAO DE VARIAVEIS.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTIR-SAIDA
                                          GFCTG3-SAIDA
                                          GFCTG2-ENTRADA
                                          GFCTIU-ENTRADA
                                          GFCTIV-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCT0M-AREA-ERROS
                      GFCTG3-SAIDA
                      GFCTG2-ENTRADA
                      GFCTIU-ENTRADA
                      GFCTIV-SAIDA
                      GFCTIR-SAIDA.

           MOVE 405                    TO GFCTIR-LL.
           MOVE GFCTIQ-TRANSACAO       TO GFCTIR-TRANSACAO.
           MOVE GFCTIQ-FUNCAO          TO GFCTIR-FUNCAO.
           MOVE GFCTIQ-FILTRO          TO GFCTIR-FILTRO.
           MOVE GFCTIQ-QTDE-OCOR       TO GFCTIR-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTIR-QTDE-TOT-REG.
           MOVE GFCTIQ-FUNC-BDSCO      TO GFCTIR-FUNC-BDSCO.
           MOVE GFCTIQ-TERMINAL        TO GFCTIR-TERMINAL.
           MOVE GFCTIQ-FIM             TO GFCTIR-FIM.
           MOVE ZEROS                  TO GFCTIR-ERRO
                                          GFCTIR-COD-SQL-ERRO
                                          GFCTIR-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.

           PERFORM 1100-OBTER-DESCRICAO.

           MOVE GFCTG3-DESC-MSG        TO GFCTIR-DESC-MSG-ERRO.

           MOVE 'S'                    TO GFCTIR-FIM.

           PERFORM 1200-CONSISTIR-DADOS.

           PERFORM 1300-OBTER-DATA-DPROCM.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA OBTER DESCRICAO DA MENSAGEM.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-OBTER-DESCRICAO            SECTION.
      *----------------------------------------------------------------*

           MOVE 100                    TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTIQ-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTIQ-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTIQ-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE GFCTIQ-FIM             TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0457'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                     EQUAL 9
               IF  GFCT0M-TIPO-ACESSO          NOT EQUAL 'DB2'
                   MOVE GFCTG3-DESC-MSG-ERRO   TO GFCT0M-TEXTO
                   MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG3-COD-SQL-ERRO    TO GFCTIR-COD-SQL-ERRO
               END-IF
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
               MOVE 'GFCT0457'                 TO GFCT0M-TRANSACAO
               MOVE GFCTG3-ERRO                TO GFCTIR-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSISTIR DADOS DE ENTRADA.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTIQ-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTIQ-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTIQ-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTIQ-QTDE-OCOR        NOT NUMERIC)                OR
              (GFCTIQ-QTDE-TOT-REG     NOT NUMERIC)                OR
              (GFCTIQ-CPF-CNPJ         NOT NUMERIC)                OR
              (GFCTIQ-CTRL             NOT NUMERIC)                OR
              (GFCTIQ-FILIAL           NOT NUMERIC)                OR
              (GFCTIQ-FIM              NOT EQUAL 'S' AND 'N')
               MOVE 1                  TO GFCTIR-ERRO
               MOVE ZEROS              TO GFCTIR-COD-SQL-ERRO
               MOVE SPACES             TO GFCTIR-DESC-MSG-ERRO
               MOVE 0001               TO GFCTIR-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1100-OBTER-DESCRICAO
               MOVE GFCTG3-DESC-MSG      TO GFCTIR-DESC-MSG-ERRO

               PERFORM 1210-MONTAR-OCORRENCIA-SAIDA

               PERFORM 3000-FINALIZAR
           END-IF.
      *
      *--- VERIFICAR EXISTENCIA DE UNIFICACAO
      *
           PERFORM 1220-VALIDAR-CPF-CNPJ.
      *
      *--- VERIFICAR EXISTENCIA DE UNIFICACAO
      *
           PERFORM 2200-CONTAR-UNIFICACOES.

           IF  WRK-CONT-UNIFICACAO     EQUAL ZEROS AND
               GFCTIQ-QTDE-OCOR        LESS 2
               MOVE 1                  TO GFCTIR-ERRO
               MOVE ZEROS              TO GFCTIR-COD-SQL-ERRO
               MOVE 0599               TO GFCTIR-COD-MSG-ERRO
               MOVE '0599'             TO GFCTG2-COD-MSG

               PERFORM 1100-OBTER-DESCRICAO

               MOVE GFCTG3-DESC-MSG    TO GFCTIR-DESC-MSG-ERRO

               PERFORM 1210-MONTAR-OCORRENCIA-SAIDA

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA MONTAR OCORRENCIAS DA AREA DE SAIDA.               *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-MONTAR-OCORRENCIA-SAIDA    SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTIQ-QTDE-TOT-REG    TO GFCTIR-QTDE-TOT-REG.

           PERFORM
           VARYING IND-1               FROM 1 BY 1
             UNTIL IND-1               GREATER GFCTIQ-QTDE-OCOR
                   ADD +121            TO GFCTIR-LL

                   MOVE GFCTIQ-TARIFA(IND-1)
                                       TO GFCTIR-TARIFA(IND-1)
                   MOVE GFCTIQ-DESC-TARIFA(IND-1)
                                       TO GFCTIR-DESC-TARIFA(IND-1)
                   MOVE ZEROS          TO
                                         GFCTIR-COD-MSG-ERRO-GEN(IND-1)
                   MOVE SPACES         TO
                                        GFCTIR-DESC-MSG-ERRO-GEN(IND-1)

           END-PERFORM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ACESSAR MODULO GFCT5110.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1220-VALIDAR-CPF-CNPJ           SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCTIQ-TRANSACAO      TO GFCTIU-TRANSACAO.
           MOVE  GFCTIQ-FUNCAO         TO GFCTIU-FUNCAO.
           MOVE  'N'                   TO GFCTIU-FIM.
           MOVE  GFCTIQ-FUNC-BDSCO     TO GFCTIU-FUNC-BDSCO.
ST25X6*    MOVE  GFCTIQ-CPF-CNPJ       TO GFCTIU-NRO-CPF-CNPJ.
ST25X6*    MOVE  GFCTIQ-FILIAL         TO GFCTIU-NRO-FILIAL.
ST25X6     MOVE  GFCTIQ-CPF-CNPJ       TO GFCTIU-COD-CPF-CNPJ.
ST25X6     MOVE  GFCTIQ-FILIAL         TO GFCTIU-COD-FILIAL.
           MOVE  GFCTIQ-CTRL           TO GFCTIU-CTRL-CPF-CNPJ.

           MOVE 'GFCT5110'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTIU-ENTRADA
                                             GFCTIV-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0457'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF

           IF  GFCTIV-ERRO                     EQUAL 9
               IF  GFCT0M-TIPO-ACESSO          NOT EQUAL 'DB2'
                   MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
                   MOVE GFCTIV-DESC-MSG-ERRO   TO GFCT0M-TEXTO
               ELSE
                   MOVE GFCTIV-COD-SQL-ERRO    TO GFCTIR-COD-SQL-ERRO
               END-IF
               MOVE GFCTG3-ERRO                TO GFCTIR-ERRO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
               MOVE 'GFCT0457'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTIV-ERRO                     EQUAL 1
               MOVE 1                          TO GFCTIR-ERRO
               MOVE GFCTIV-COD-MSG-ERRO        TO GFCTIR-COD-MSG-ERRO
               MOVE ZEROS                      TO GFCTIR-COD-SQL-ERRO
               MOVE GFCTIV-DESC-MSG-ERRO       TO GFCTIR-DESC-MSG-ERRO

               PERFORM 1210-MONTAR-OCORRENCIA-SAIDA

               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  GFCTIV-NOME-CLIENTE           TO GFCTIR-NOME.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA OBTER DATA DO PROCESSAMENTO.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-OBTER-DATA-DPROCM          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
               SELECT DPROCM_PROX
               INTO   :GFCTB0A1.DPROCM-PROX
               FROM   DB2PRD.PARM_DATA_PROCM
               WHERE
                   CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB0A1'      TO WRK-NOME-TAB
               MOVE 'GFCT0457'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTIR-ERRO
               MOVE  SQLCODE           TO GFCTIR-COD-SQL-ERRO
               MOVE SPACES             TO GFCTIR-DESC-MSG-ERRO
               MOVE  0012              TO GFCTIR-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1100-OBTER-DESCRICAO
               PERFORM 1310-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONCATENAR MENSAGEM.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-CONCATENAR-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           STRING GFCTG3-DESC-MSG      WRK-NOME-TAB

           DELIMITED BY '  '           INTO GFCTIR-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAMENTO DO PROGRAMA.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM
           VARYING IND-1               FROM 1 BY 1
             UNTIL IND-1               GREATER GFCTIQ-QTDE-OCOR
                   IF  GFCTIQ-TARIFA(IND-1)
                                       NOT EQUAL ZEROS
                       PERFORM 2100-PROCESSAR-INSERT
                   END-IF
           END-PERFORM.

           PERFORM 2200-CONTAR-UNIFICACOES.

           IF  WRK-CONT-UNIFICACAO     LESS 2
               MOVE 1                  TO GFCTIR-ERRO
               MOVE ZEROS              TO GFCTIR-COD-SQL-ERRO
               MOVE SPACES             TO GFCTIR-DESC-MSG-ERRO
               MOVE 0599               TO GFCTIR-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1100-OBTER-DESCRICAO
               MOVE GFCTG3-DESC-MSG      TO GFCTIR-DESC-MSG-ERRO

               PERFORM 1210-MONTAR-OCORRENCIA-SAIDA

               PERFORM 2010-PROCESSAR-DELETE

               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM
           VARYING IND-1               FROM 1 BY 1
             UNTIL IND-1               GREATER GFCTIQ-QTDE-OCOR
                OR GFCTIR-ERRO         EQUAL 1
                   IF  GFCTIR-COD-MSG-ERRO-GEN(IND-1)
                                       NOT EQUAL ZEROS
                       MOVE 1          TO GFCTIR-ERRO
                       MOVE ZEROS      TO GFCTIR-COD-SQL-ERRO
                       MOVE GFCTIR-COD-MSG-ERRO-GEN(IND-1)
                                       TO GFCTIR-COD-MSG-ERRO
                       MOVE GFCTIR-DESC-MSG-ERRO-GEN(IND-1)
                                       TO GFCTIR-DESC-MSG-ERRO

                       COMPUTE GFCTIR-LL
                                        = GFCTIR-LL
                                          + (GFCTIR-QTDE-TOT-REG * 121)
                   END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TRATAMENTO DE EXCLUSAO.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2010-PROCESSAR-DELETE           SECTION.
      *----------------------------------------------------------------*

ST25X6*    MOVE  WRK-CPF-CNPJ       TO CCGC-CPF         OF GFCTB0H8.
ST25X6*    MOVE  WRK-FILIAL         TO CFLIAL-CGC       OF GFCTB0H8.
ST25X6     MOVE  WRK-CPF-CNPJ       TO CCGC-CPF-ST      OF GFCTB0H8.
ST25X6     MOVE  WRK-FILIAL         TO CFLIAL-CGC-ST    OF GFCTB0H8.
           MOVE  WRK-TARIFA         TO CSERVC-TARIF     OF GFCTB0H8.
           MOVE  WRK-HINCL          TO HINCL-DEB-UNIC   OF GFCTB0H8.

           EXEC SQL DELETE
              FROM DB2PRD.TDEB_UNFCA_CLI
               WHERE
ST25X6*               CCGC_CPF            = :GFCTB0H8.CCGC-CPF       AND
ST25X6*               CFLIAL_CGC          = :GFCTB0H8.CFLIAL-CGC     AND
ST25X6                CCGC_CPF_ST         = :GFCTB0H8.CCGC-CPF-ST    AND
ST25X6                CFLIAL_CGC_ST       = :GFCTB0H8.CFLIAL-CGC-ST  AND
                   CSERVC_TARIF        = :GFCTB0H8.CSERVC-TARIF   AND
                   HINCL_DEB_UNIC      = :GFCTB0H8.HINCL-DEB-UNIC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB0H8'      TO WRK-NOME-TAB
               MOVE 'GFCT0457'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TDEB_UNFCA_CLI'   TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTIR-ERRO
               MOVE  SQLCODE           TO GFCTIR-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTIR-DESC-MSG-ERRO
               MOVE  0020              TO GFCTIR-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1100-OBTER-DESCRICAO
               PERFORM 1310-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TRATAMENTO PARA INSERCAO.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-PROCESSAR-INSERT           SECTION.
      *----------------------------------------------------------------*

ST25X6*    MOVE  GFCTIQ-CPF-CNPJ       TO CCGC-CPF         OF GFCTB0H8.
ST25X6*    MOVE  GFCTIQ-FILIAL         TO CFLIAL-CGC       OF GFCTB0H8.
ST25X6     MOVE  GFCTIQ-CPF-CNPJ       TO CCGC-CPF-ST      OF GFCTB0H8.
ST25X6     MOVE  GFCTIQ-FILIAL         TO CFLIAL-CGC-ST    OF GFCTB0H8.
           MOVE  GFCTIQ-TARIFA(IND-1)  TO CSERVC-TARIF     OF GFCTB0H8.
           MOVE  '0001-01-01-01.01.01.000001'
                                       TO HEXCL-DEB-UNIC   OF GFCTB0H8.

           PERFORM 2110-VERIFICAR-CHAVE-DUPLIC.

           IF  WRK-ACHOU-DUPLIC        EQUAL 'N'
               PERFORM 2120-OBTER-DATA-SISTEMA

               PERFORM 2130-INSERIR-GFCTB0H8

ST25X6*        MOVE  CCGC-CPF OF GFCTB0H8
ST25X6*                                TO  WRK-CPF-CNPJ
ST25X6*        MOVE  CFLIAL-CGC OF GFCTB0H8
ST25X6*                                TO  WRK-FILIAL
ST25X6         MOVE  CCGC-CPF-ST OF GFCTB0H8
ST25X6                                 TO  WRK-CPF-CNPJ
ST25X6         MOVE  CFLIAL-CGC-ST OF GFCTB0H8
ST25X6                                 TO  WRK-FILIAL
               MOVE  CSERVC-TARIF OF GFCTB0H8
                                       TO  WRK-TARIFA
               MOVE  HINCL-DEB-UNIC OF GFCTB0H8
                                       TO  WRK-HINCL
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VERIFICAR SE CHAVE ESTA DUPLICADA.                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-VERIFICAR-CHAVE-DUPLIC     SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-ACHOU-DUPLIC.

           EXEC SQL
               SELECT HINCL_DEB_UNIC
               INTO   :GFCTB0H8.HINCL-DEB-UNIC
               FROM   DB2PRD.TDEB_UNFCA_CLI
               WHERE
ST25X6*               CCGC_CPF            =  :GFCTB0H8.CCGC-CPF      AND
ST25X6*               CFLIAL_CGC          =  :GFCTB0H8.CFLIAL-CGC    AND
ST25X6                CCGC_CPF_ST         =  :GFCTB0H8.CCGC-CPF-ST   AND
ST25X6                CFLIAL_CGC_ST       =  :GFCTB0H8.CFLIAL-CGC-ST AND
                   CSERVC_TARIF        =  :GFCTB0H8.CSERVC-TARIF  AND
                   HEXCL_DEB_UNIC      =  :GFCTB0H8.HEXCL-DEB-UNIC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB0H8'      TO WRK-NOME-TAB
               MOVE 'GFCT0457'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TDEB_UNFCA_CLI'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTIR-ERRO
               MOVE  SQLCODE           TO GFCTIR-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTIR-DESC-MSG-ERRO
               MOVE  0012              TO GFCTIR-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1100-OBTER-DESCRICAO
               PERFORM 1310-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS OR -811
               MOVE 'S'                TO WRK-ACHOU-DUPLIC

               MOVE  0004              TO GFCTG2-COD-MSG
               PERFORM 1100-OBTER-DESCRICAO
               MOVE IND-1              TO GFCTIR-QTDE-TOT-REG
               MOVE GFCTIQ-TARIFA(IND-1)
                                       TO GFCTIR-TARIFA(IND-1)
               MOVE GFCTIQ-DESC-TARIFA(IND-1)
                                       TO GFCTIR-DESC-TARIFA(IND-1)
               MOVE 0004               TO GFCTIR-COD-MSG-ERRO-GEN(IND-1)
               MOVE GFCTG3-DESC-MSG    TO
                                         GFCTIR-DESC-MSG-ERRO-GEN(IND-1)
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA OBTER DATA DO SISTEMA.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-OBTER-DATA-SISTEMA         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SET :WRK-TIMESTAMP      = CURRENT TIMESTAMP
           END-EXEC.

           MOVE DPROCM-PROX OF GFCTB0A1(7:4)   TO WRK-TIMESTAMP(1:4).
           MOVE DPROCM-PROX OF GFCTB0A1(4:2)   TO WRK-TIMESTAMP(6:2).
           MOVE DPROCM-PROX OF GFCTB0A1(1:2)   TO WRK-TIMESTAMP(9:2).

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA INSERCAO NA TABELA GFCTB0H8.
      ******************************************************************
      *----------------------------------------------------------------*
       2130-INSERIR-GFCTB0H8           SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-TIMESTAMP         TO HINCL-DEB-UNIC   OF GFCTB0H8.
ST25X6*    MOVE  GFCTIQ-CTRL           TO CCTRL-CPF-CGC    OF GFCTB0H8.
ST25X6     MOVE  GFCTIQ-CTRL           TO CCTRL-CPF-CGC-ST OF GFCTB0H8.
           MOVE  GFCTIQ-FUNC-BDSCO     TO WRK-CFUNC-BDSCO-ALFA.
           MOVE  WRK-CFUNC-BDSCO       TO CFUNC-INCL-SIST  OF GFCTB0H8.
           MOVE  ZEROS                 TO CFUNC-EXCL-SIST  OF GFCTB0H8.

           EXEC SQL
               INSERT INTO DB2PRD.TDEB_UNFCA_CLI
                  (CCGC_CPF,
                   CFLIAL_CGC,
ST25X6             CCGC_CPF_ST,
ST25X6             CFLIAL_CGC_ST,
ST25X6             CCTRL_CPF_CGC_ST,
                   CSERVC_TARIF,
                   HINCL_DEB_UNIC,
                   CCTRL_CPF_CGC,
                   HEXCL_DEB_UNIC,
                   CFUNC_INCL_SIST,
                   CFUNC_EXCL_SIST)
               VALUES
                  (:GFCTB0H8.CCGC-CPF,
                   :GFCTB0H8.CFLIAL-CGC,
ST25X6             :GFCTB0H8.CCGC-CPF-ST,
ST25X6             :GFCTB0H8.CFLIAL-CGC-ST,
ST25X6             :GFCTB0H8.CCTRL-CPF-CGC-ST,
                   :GFCTB0H8.CSERVC-TARIF,
                   :GFCTB0H8.HINCL-DEB-UNIC,
                   :GFCTB0H8.CCTRL-CPF-CGC,
                   :GFCTB0H8.HEXCL-DEB-UNIC,
                   :GFCTB0H8.CFUNC-INCL-SIST,
                   :GFCTB0H8.CFUNC-EXCL-SIST)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB0H8'      TO WRK-NOME-TAB
               MOVE 'GFCT0457'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TDEB_UNFCA_CLI'   TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTIR-ERRO
               MOVE  SQLCODE           TO GFCTIR-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTIR-DESC-MSG-ERRO
               MOVE  0003              TO GFCTIR-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1100-OBTER-DESCRICAO
               PERFORM 1310-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONTAR UNIFICACOES DO CLIENTE.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-CONTAR-UNIFICACOES         SECTION.
      *----------------------------------------------------------------*

ST25X6*    MOVE  GFCTIQ-CPF-CNPJ       TO CCGC-CPF         OF GFCTB0H8.
ST25X6*    MOVE  GFCTIQ-FILIAL         TO CFLIAL-CGC       OF GFCTB0H8.
ST25X6*    MOVE  GFCTIQ-CTRL           TO CCTRL-CPF-CGC    OF GFCTB0H8.
ST25X6     MOVE  GFCTIQ-CPF-CNPJ       TO CCGC-CPF-ST      OF GFCTB0H8.
ST25X6     MOVE  GFCTIQ-FILIAL         TO CFLIAL-CGC-ST    OF GFCTB0H8.
ST25X6     MOVE  GFCTIQ-CTRL           TO CCTRL-CPF-CGC-ST OF GFCTB0H8.

           EXEC SQL
               SELECT COUNT(*)
               INTO   :WRK-CONT-UNIFICACAO
               FROM   DB2PRD.TDEB_UNFCA_CLI
               WHERE
ST25X6*               CCGC_CPF            = :GFCTB0H8.CCGC-CPF      AND
ST25X6*               CFLIAL_CGC          = :GFCTB0H8.CFLIAL-CGC    AND
ST25X6*               CCTRL_CPF_CGC       = :GFCTB0H8.CCTRL-CPF-CGC
ST25X6                CCGC_CPF_ST         = :GFCTB0H8.CCGC-CPF-ST   AND
ST25X6                CFLIAL_CGC_ST       = :GFCTB0H8.CFLIAL-CGC-ST AND
ST25X6                CCTRL_CPF_CGC_ST    = :GFCTB0H8.CCTRL-CPF-CGC-ST
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -310) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE ' - GFCTB0H8'      TO WRK-NOME-TAB
               MOVE 'GFCT0457'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TDEB_UNFCA_CLI'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTIR-ERRO
               MOVE  SQLCODE           TO GFCTIR-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTIR-DESC-MSG-ERRO
               MOVE  0005              TO GFCTIR-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 1100-OBTER-DESCRICAO
               PERFORM 1310-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -310
               MOVE ZEROS              TO WRK-CONT-UNIFICACAO
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FINALIZACAO DO PROGRAMA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
