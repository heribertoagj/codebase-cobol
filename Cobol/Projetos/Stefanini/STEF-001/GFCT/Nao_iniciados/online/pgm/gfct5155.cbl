      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5155.
       AUTHOR.     ROBSON FAUSTO CESA.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5155                                    *
      *    PROGRAMADOR.:   ROBSON FAUSTO CESA      - CPM PATO BRANCO   *
      *    ANALISTA CPM:   MARCIO CUSTIN           - CPM PATO BRANCO   *
      *    ANALISTA....:   RICARDO PEREIRA         - PROCWORK /GP.50   *
      *    DATA........:   27/04/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR DADOS PARA COMBO DE TARIFA            *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.SUSP_TARIF_AGPTO       GFCTB0E7       *
      *                   DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8       *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTPG - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTPH - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA   - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5507 - VERIFICA MASTER                                  *
      *    SENH0315 - OBTER JUNCAO                                     *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *    POOL7600 - OBTAR DATA E HORA DO SISTEMA                     *
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

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *---------------------------------------------------------------*

       77  IND-1                       PIC  9(003)         VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *---------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(008)         VALUE SPACES.
       77  WRK-FINALIZAR               PIC  X(001)         VALUE SPACES.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM SENH0315 *'.
      *---------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC  X(006)         VALUE SPACES.
       01  WRK-MENSAGEM-0315.
           05  WRK-COD-MENSAGEM-0315   PIC  9(003)         VALUE ZEROS.
           05  WRK-HIFEN-MENSAGEM-0315 PIC  X(001)         VALUE SPACES.
           05  WRK-TEXTO-MENSAGEM-0315 PIC  X(079)         VALUE SPACES.
       01  WRK-COD-FUNCIONAL-0315      PIC  X(007)         VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC  X(040)         VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
ST25X6*    05  WRK-CNPJ-PRI-0315       PIC  9(009)         VALUE ZEROS.
ST25X6*    05  WRK-CNPJ-FIL-0315       PIC  9(005)         VALUE ZEROS.
ST25X6*    05  WRK-CNPJ-CON-0315       PIC  9(002)         VALUE ZEROS.
           05  WRK-CNPJ-PRI-0315       PIC  X(009)         VALUE SPACES.
           05  WRK-CNPJ-FIL-0315       PIC  X(004)         VALUE SPACES.
           05  WRK-CNPJ-CON-0315       PIC  X(002)         VALUE SPACES.
       01  WRK-COD-JUNCAO-0315         PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-SECAO-0315              PIC  9(003)         VALUE ZEROS.
       01  WRK-STATUS-0315             PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5507 *'.
      *---------------------------------------------------------------*

       01  WRK-5507-AREA.
           05  WRK-5507-AREA-ENVIA.
               10  WRK-5507-AMBIENTE         PIC  X(001)  VALUE SPACES.
               10  WRK-5507-CJUNC-DEPDC-X.
                   15  WRK-5507-CJUNC-DEPDC  PIC  9(005)  VALUE ZEROS.
           05  WRK-5507-AREA-RECEBE.
               10  WRK-5507-COD-RETORNO      PIC  9(002)  VALUE ZEROS.
               10  WRK-5507-COD-SQL-ERRO     PIC  9(003)  VALUE ZEROS.
               10  WRK-5507-MSG-RETORNO      PIC  X(045)  VALUE SPACES.
               10  WRK-5507-IDTFD-MASTER     PIC  X(001)  VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
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
             INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSORES *'.
      *---------------------------------------------------------------*

           EXEC SQL
             DECLARE   CSR01-GFCTB0E7   CURSOR   FOR
               SELECT DISTINCT
                   CSERVC_TARIF
             FROM  DB2PRD.SUSP_TARIF_AGPTO
             WHERE
                   CSERVC_TARIF      >= :GFCTB0E7.CSERVC-TARIF       AND
                 ((DINIC_SUSP_COBR   >= :GFCTB0E7.DINIC-SUSP-COBR)   OR
                  ((DINIC_SUSP_COBR  <= :GFCTB0E7.DINIC-SUSP-COBR    AND
                    DFIM_SUSP_COBR   >= :GFCTB0E7.DFIM-SUSP-COBR)    OR
                  (DINIC_SUSP_COBR   >  :GFCTB0E7.DINIC-SUSP-COBR)))
             ORDER BY
                   CSERVC_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCTPG.

       COPY I#GFCTPH.

       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTPG-ENTRADA
                                                      GFCTPH-SAIDA
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

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTPH-SAIDA.

           INITIALIZE GFCTPH-SAIDA
                      GFCT0M-AREA-ERROS
                      IND-1.

           PERFORM 1100-VERIFICAR-DISP-SISTEMA.

           PERFORM 1200-MONTAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICA DISPONIBILIDADE DO SISTEMA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-DISP-SISTEMA     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE +100                   TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE GFCTPG-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTPG-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTPG-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTPH-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5155'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE 9                  TO GFCTPH-ERRO
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT5155'     TO GFCT0M-TRANSACAO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE          EQUAL 1
               MOVE 9                      TO GFCTPH-ERRO
               MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5155'             TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATA A AREA DE RETORNO AO PROGRAMA CHAMADOR                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE +170                       TO GFCTPH-LL.
           MOVE ZEROS                      TO GFCTPH-ZZ.
           MOVE GFCTPG-TRANSACAO           TO GFCTPH-TRANSACAO.
           MOVE GFCTPG-FUNCAO              TO GFCTPH-FUNCAO.
           MOVE GFCTPG-FILTRO              TO GFCTPH-FILTRO.

           IF  GFCTPG-QTDE-OCOR            NOT NUMERIC
               MOVE ZEROS                  TO GFCTPH-QTDE-OCOR
           ELSE
               MOVE GFCTPG-QTDE-OCOR       TO GFCTPH-QTDE-OCOR
           END-IF.

           IF  GFCTPG-QTDE-TOT-REG         NOT NUMERIC
               MOVE ZEROS                  TO GFCTPH-QTDE-TOT-REG
           ELSE
               MOVE GFCTPG-QTDE-TOT-REG    TO GFCTPH-QTDE-TOT-REG
           END-IF.

           MOVE GFCTPG-FUNC-BDSCO          TO GFCTPH-FUNC-BDSCO.
           MOVE GFCTPG-PONTEIRO-TARIFA     TO GFCTPH-PONTEIRO-TARIFA.

           MOVE 'N'                        TO GFCTPH-FIM.

           MOVE ZEROS                      TO GFCTPH-ERRO
                                              GFCTPH-COD-SQL-ERRO
                                              GFCTPH-COD-MSG-ERRO.
           MOVE '0000'                     TO GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG            TO GFCTPH-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTE DADOS                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTPG-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTPG-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTPG-FILTRO           NOT NUMERIC) OR
              (GFCTPG-QTDE-OCOR        NOT NUMERIC) OR
              (GFCTPG-QTDE-TOT-REG     NOT NUMERIC) OR
              (GFCTPG-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTPG-PONTEIRO-TARIFA  NOT NUMERIC) OR
              (GFCTPG-FIM              NOT EQUAL 'S' AND 'N')
               MOVE  1                 TO GFCTPH-ERRO
               MOVE  ZEROS             TO GFCTPH-COD-SQL-ERRO
               MOVE  0001              TO GFCTPH-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTPH-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTPH-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTPG-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTPG-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTPG-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTPH-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5155'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTPH-ERRO
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT5155'     TO GFCT0M-TRANSACAO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 'S'                TO GFCTPH-FIM
               MOVE 1                  TO GFCTPH-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTPH-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTPH-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTPH-DESC-MSG-ERRO
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

           PERFORM 2100-OBTER-COD-JUNCAO.

           PERFORM 2200-VERIFICA-MASTER.

           PERFORM 2300-OBTER-DATA-PROCM

           PERFORM 2400-TRATAR-CSR01-GFCTB0E7.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA O MODULO SENH0315                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-OBTER-COD-JUNCAO           SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO WRK-VERSAO-0315.
           MOVE GFCTPG-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.

           CALL 'SENH0315'             USING WRK-VERSAO-0315
                                             WRK-MENSAGEM-0315
                                             WRK-COD-FUNCIONAL-0315
                                             WRK-NOME-FUNC-0315
                                             WRK-COD-BANCO-0315
                                             WRK-NOME-BANCO-0315
                                             WRK-CNPJ-BANCO-0315
                                             WRK-COD-JUNCAO-0315
                                             WRK-NOME-DEPTO-0315
                                             WRK-SECAO-0315
                                             WRK-STATUS-0315.

           IF  RETURN-CODE             NOT EQUAL ZEROS AND 04
               MOVE 'S'                TO GFCTPH-FIM
               MOVE 1                  TO GFCTPH-ERRO
               MOVE RETURN-CODE        TO GFCTPH-COD-SQL-ERRO
               MOVE 0056               TO GFCTPH-COD-MSG-ERRO
               MOVE '0056'             TO GFCTPH-COD-MSG-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTPH-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               IF  WRK-COD-MENSAGEM-0315   EQUAL 001
                   MOVE 'S'                TO GFCTPH-FIM
                   MOVE 1                  TO GFCTPH-ERRO
                   MOVE ZEROS              TO GFCTPH-COD-SQL-ERRO
                   MOVE 0057               TO GFCTPH-COD-MSG-ERRO
                   MOVE '0057'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTPH-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 154
                   MOVE 'S'                TO GFCTPH-FIM
                   MOVE 1                  TO GFCTPH-ERRO
                   MOVE ZEROS              TO GFCTPH-COD-SQL-ERRO
                   MOVE 0058               TO GFCTPH-COD-MSG-ERRO
                   MOVE '0058'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTPH-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 242
                   MOVE 'S'                TO GFCTPH-FIM
                   MOVE 1                  TO GFCTPH-ERRO
                   MOVE ZEROS              TO GFCTPH-COD-SQL-ERRO
                   MOVE 0059               TO GFCTPH-COD-MSG-ERRO
                   MOVE '0059'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTPH-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 009
                   MOVE 'S'                TO GFCTPH-FIM
                   MOVE 1                  TO GFCTPH-ERRO
                   MOVE ZEROS              TO GFCTPH-COD-SQL-ERRO
                                              GFCTPH-COD-MSG-ERRO
                   MOVE '0000'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTPH-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 138
                   MOVE 'S'                TO GFCTPH-FIM
                   MOVE 1                  TO GFCTPH-ERRO
                   MOVE ZEROS              TO GFCTPH-COD-SQL-ERRO
                   MOVE 0060               TO GFCTPH-COD-MSG-ERRO
                   MOVE '0060'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTPH-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA O MODULO GFCT5507                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-VERIFICA-MASTER            SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                    TO WRK-5507-AMBIENTE.
           MOVE WRK-COD-JUNCAO-0315    TO WRK-5507-CJUNC-DEPDC.
           MOVE 'GFCT5507'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5507-AREA.

           IF  WRK-5507-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5507-COD-RETORNO    EQUAL 01
                   MOVE 'S'                TO GFCTPH-FIM
                   MOVE 1                  TO GFCTPH-ERRO
                   MOVE ZEROS              TO GFCTPH-COD-SQL-ERRO
                                              GFCTPH-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                           TO GFCTPH-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5507-COD-RETORNO    EQUAL 99
                       MOVE 'S'            TO GFCTPH-FIM
                       MOVE 1              TO GFCTPH-ERRO
                       MOVE WRK-5507-COD-SQL-ERRO TO GFCTPH-COD-SQL-ERRO
                       MOVE ZEROS          TO GFCTPH-COD-MSG-ERRO
                       MOVE WRK-5507-MSG-RETORNO
                                           TO GFCTPH-DESC-MSG-ERRO
                   END-IF
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DATA DO PROCESSAMENTO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-OBTER-DATA-PROCM           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ANTER,
                   DPROCM_PROX,
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ANTER,
                   :GFCTB0A1.DPROCM-PROX,
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_DATA_PROCM ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTPH-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOM-TAB
               PERFORM 2310-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATA ERROS DB2                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           MOVE  9                     TO GFCTPH-ERRO.
           MOVE 'GFCT5155'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
                                          GFCTPH-COD-SQL-ERRO.

           PERFORM 1310-OBTER-DESC-MSG.

           STRING GFCTG3-DESC-MSG ' - ' WRK-NOM-TAB
           DELIMITED BY '  '           INTO GFCTPH-DESC-MSG-ERRO.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR CURSOR                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-TRATAR-CSR01-GFCTB0E7      SECTION.
      *----------------------------------------------------------------*

           PERFORM 2410-ABRIR-CSR01-GFCTB0E7.

           PERFORM 2420-LER-CSR01-GFCTB0E7.

           PERFORM 2430-PROCESSAR-CSR01-GFCTB0E7   UNTIL
                   WRK-FINALIZAR                   EQUAL 'S'.

           PERFORM 2440-FECHAR-CSR01-GFCTB0E7.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR DA TABELA GFCTB0E7                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-ABRIR-CSR01-GFCTB0E7       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTPG-PONTEIRO-TARIFA TO CSERVC-TARIF     OF GFCTB0E7.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-SUSP-COBR  OF GFCTB0E7
                                          DFIM-SUSP-COBR   OF GFCTB0E7.
           MOVE 'N'                    TO WRK-FINALIZAR.

           EXEC SQL
               OPEN CSR01-GFCTB0E7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SUSP_TARIF_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTPH-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E7'         TO WRK-NOM-TAB
               PERFORM 2310-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA LEITURA NO CURSOR DA TABELA GFCTB0E7                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2420-LER-CSR01-GFCTB0E7         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH   CSR01-GFCTB0E7   INTO
                   :GFCTB0E7.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SUSP_TARIF_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTPH-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E7'         TO WRK-NOM-TAB
               PERFORM 2310-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTPH-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTPH-ERRO
                   MOVE ZEROS          TO GFCTPH-COD-SQL-ERRO
                   MOVE 0905           TO GFCTPH-COD-MSG-ERRO
                   MOVE '0905'         TO GFCTG2-COD-MSG
                   MOVE 'GFCTB0E7'     TO WRK-NOM-TAB
                   PERFORM 1310-OBTER-DESC-MSG
                   STRING GFCTG3-DESC-MSG ' - ' WRK-NOM-TAB
                   DELIMITED BY '  '           INTO GFCTPH-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSA REGISTROS DA TABELA GFCTB0E7                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2430-PROCESSAR-CSR01-GFCTB0E7   SECTION.
      *----------------------------------------------------------------*

           PERFORM 2431-ACESSAR-GFCTB0D8.

           IF  WRK-5507-IDTFD-MASTER   EQUAL 'N'
               IF  CDEPDC              OF GFCTB0D8
                                       NOT EQUAL WRK-COD-JUNCAO-0315
                   PERFORM 2420-LER-CSR01-GFCTB0E7
                   GO TO 2430-99-FIM
               END-IF
           END-IF.

           PERFORM 2432-ACESSAR-GFCTB0D8.

           COMPUTE GFCTPH-LL           = GFCTPH-LL + 105.

           ADD 1                       TO GFCTPH-QTDE-TOT-REG
                                          IND-1.

           MOVE CSERVC-TARIF           OF GFCTB0E7
                                       TO GFCTPH-COD-TARIFA(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTPH-DESC-TARIFA(IND-1).

           PERFORM 2420-LER-CSR01-GFCTB0E7.

           IF  WRK-FINALIZAR           EQUAL 'S'
               GO TO 2430-99-FIM
           END-IF.

           IF  IND-1                   EQUAL 8
               MOVE 'S'                TO WRK-FINALIZAR
               MOVE CSERVC-TARIF       OF GFCTB0E7
                                       TO GFCTPH-PONTEIRO-TARIFA
           END-IF.

      *----------------------------------------------------------------*
       2430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA TABELA GFCTB0D8 PARA OBTER A JUNCAO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2431-ACESSAR-GFCTB0D8           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0E7
                                       TO CSERVC-TARIF
                                       OF GFCTB0D8.

           EXEC SQL
             SELECT
                   CDEPDC
             INTO
                   :GFCTB0D8.CDEPDC
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTPH-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOM-TAB
               PERFORM 2310-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTPH-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTPH-ERRO
                   MOVE ZEROS          TO GFCTPH-COD-SQL-ERRO
                   MOVE 0242           TO GFCTPH-COD-MSG-ERRO
                   MOVE '0242'         TO GFCTG2-COD-MSG
                   MOVE 'GFCTB0D8'     TO WRK-NOM-TAB
                   PERFORM 1310-OBTER-DESC-MSG
                   STRING GFCTG3-DESC-MSG ' - ' WRK-NOM-TAB
                   DELIMITED BY '  '           INTO GFCTPH-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2431-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA TABELA GFCTB0D8 PARA OBTER DESCRICAO DA TARIFA           *
      ******************************************************************
      *----------------------------------------------------------------*
       2432-ACESSAR-GFCTB0D8           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0E7
                                       TO CSERVC-TARIF
                                       OF GFCTB0D8.

           EXEC SQL
             SELECT
                   RSERVC_TARIF_REDZD
             INTO
                   :GFCTB0D8.RSERVC-TARIF-REDZD
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                NOT EQUAL ZEROS AND +100 AND -811)
           OR (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTPH-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOM-TAB
               PERFORM 2310-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTPH-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTPH-ERRO
                   MOVE ZEROS          TO GFCTPH-COD-SQL-ERRO
                   MOVE 0906           TO GFCTPH-COD-MSG-ERRO
                   MOVE '0906'         TO GFCTG2-COD-MSG
                   MOVE 'GFCTB0D8'     TO WRK-NOM-TAB
                   PERFORM 1310-OBTER-DESC-MSG
                   STRING GFCTG3-DESC-MSG ' - ' WRK-NOM-TAB
                   DELIMITED BY '  '           INTO GFCTPH-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2432-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR DA TABELA GFCTB0E7                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2440-FECHAR-CSR01-GFCTB0E7      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE   CSR01-GFCTB0E7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SUSP_TARIF_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTPH-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0E7'         TO WRK-NOM-TAB
               PERFORM 2310-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2440-99-FIM.                    EXIT.
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
