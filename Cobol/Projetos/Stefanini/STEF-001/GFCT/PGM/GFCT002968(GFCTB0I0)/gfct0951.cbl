      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0951.
       AUTHOR.     CARLOS BLANCO.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT0951                                     *
      *    PROGRAMADOR  : CARLOS BLANCO                 - CPM/FPOLIS   *
      *    ANALISTA CPM : ALEXANDRE PEREIRA             - CPM/FPOLIS   *
      *    ANALISTA     : VALERIA - PROCWORK            - GRUPO 50     *
      *    DATA         : 22/03/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      FLEXIBILIZACAO VIP - CONSULTA DETALHADA DE FLEXIBILIZACAO *
      *      VIP PARA AGENCIA.                                         *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.TFLEXZ_VIP_AG                   GFCTB0J0          *
      *       DB2PRD.TPO_OPER_MOTVO                  GFCTB0G0          *
      *       DB2PRD.MOTVO_JUSTF_OPER                GFCTB089          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5006 - DESCRICAO DEPENDENCIA                          *
      *      GFCT5522 - VERIFICA DISPONIBILIDADE ON LINE               *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *      SENH0315 - OBTER DADOS DO FUNCIONARIO                     *
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
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT0951  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-MODULO              PIC X(08)           VALUE SPACES.
           05  WRK-NOME-TAB            PIC X(11)           VALUE SPACES.
           05  WRK-9-9-R               PIC 9(09)           VALUE ZEROS.
           05  FILLER                  REDEFINES           WRK-9-9-R.
               10  FILLER              PIC 9(06).
               10  WRK-9-3             PIC 9(03).
           05  WRK-DESC-50.
               10  WRK-DESC-40         PIC X(40)           VALUE SPACES.
               10  FILLER              PIC X(10)           VALUE SPACES.


       01  WRK-COD-FUNCIONARIO         PIC X(09).
       01  FILLER REDEFINES WRK-COD-FUNCIONARIO.
           05  WRK-FILLER              PIC X(02).
           05  WRK-COD-FUNC            PIC X(07).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA P/ MODULO SENH0315   *'.
      *----------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC  X(06)          VALUE SPACES.
       01  WRK-MENSAGEM-0315.
           03  WRK-COD-MSG-0315        PIC  9(03)          VALUE ZEROS.
           03  WRK-HIFEN-MENSAGEM-0315 PIC  X(01)          VALUE SPACES.
           03  WRK-TEXTO-MENSAGEM-0315 PIC  X(79)          VALUE SPACES.
       01  WRK-COD-FUNCIONAL-0315      PIC  X(07)          VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC  X(40)          VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC  9(05)          VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC  X(40)          VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
ST25X6*    05  WRK-CNPJ-PRI-0315       PIC  9(09)          VALUE ZEROS.
ST25X6*    05  WRK-CNPJ-FIL-0315       PIC  9(05)          VALUE ZEROS.
ST25X6     05  WRK-CNPJ-PRI-0315       PIC  X(09)          VALUE SPACES.
ST25X6     05  WRK-CNPJ-FIL-0315       PIC  X(04)          VALUE SPACES.
           05  WRK-CNPJ-CON-0315       PIC  9(02)          VALUE ZEROS.
       01  WRK-COD-JUNCAO-0315         PIC  9(05)          VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC  X(40)          VALUE SPACES.
       01  WRK-SECAO-0315              PIC  9(03)          VALUE ZEROS.
       01  WRK-STATUS-0315             PIC  X(01)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA DE MENSAGENS        *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           05  WRK-MSG01.
               10  FILLER               PIC X(26)          VALUE
                   'ERRO NA CHAMADA DO MODULO '.
               10  WRK-NOME-MOD         PIC X(08)          VALUE SPACES.
               10  FILLER               PIC X(41)          VALUE SPACES.

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
            '*    AREA DO MODULO GFCT5006   *'.
      *----------------------------------------------------------------*

       COPY I#GFCT1V.

       COPY I#GFCT1X.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0J0
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0G0
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB089
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*    FIM DA WORKING GFCT0951   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCTYU.
       COPY I#GFCTYV.
       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTYU-ENTRADA
                                             GFCTYV-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL                                            *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 0100-INICIALIZAR.

           PERFORM 0700-PROCESSAR-DADOS.

           PERFORM 0400-FINALIZAR-PROGRAMA.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA INICIAL DO PROGRAMA                                  *
      *----------------------------------------------------------------*
       0100-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA
                                          GFCTG3-SAIDA
                                          GFCTYV-SAIDA
                                          GFCTG2-ENTRADA
                                          GFCTFZ-ENTRADA
                                          GFCT0M-AREA-ERROS
                                          GFCT1V-ENTRADA
                                          GFCT1X-SAIDA.

           INITIALIZE GFCTG1-SAIDA
                      GFCTG3-SAIDA
                      GFCTYV-SAIDA
                      GFCTG2-ENTRADA
                      GFCTFZ-ENTRADA
                      GFCT0M-AREA-ERROS
                      GFCT1V-ENTRADA
                      GFCT1X-SAIDA.

           PERFORM 0200-INICIALIZAR-SAIDA.

           PERFORM 0500-TRATAR-DISP-SISTEMA.

           PERFORM 0600-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR AREA DE SAIDA                       *
      *----------------------------------------------------------------*
       0200-INICIALIZAR-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE 1010                   TO GFCTYV-LL.
           MOVE ZEROS                  TO GFCTYV-ZZ.
           MOVE GFCTYU-TRANSACAO       TO GFCTYV-TRANSACAO.
           MOVE GFCTYU-FUNCAO          TO GFCTYV-FUNCAO.
           MOVE GFCTYU-FUNC-BDSCO      TO GFCTYV-FUNC-BDSCO.
           MOVE 'S'                    TO GFCTYV-FIM.
           MOVE GFCTYU-TIPO            TO GFCTYV-TIPO.
           MOVE GFCTYU-AGENCIA         TO GFCTYV-AGENCIA.
           MOVE GFCTYU-INIC-VIG-FLEX   TO GFCTYV-INIC-VIG-FLEX.

           IF  GFCTYU-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCTYU-QTDE-OCOR
           ELSE
               MOVE GFCTYU-QTDE-OCOR   TO GFCTYV-QTDE-OCOR
           END-IF.

           IF  GFCTYU-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO GFCTYV-QTDE-TOT-REG
           ELSE
               MOVE GFCTYU-QTDE-TOT-REG
                                       TO GFCTYV-QTDE-TOT-REG
           END-IF.

           MOVE ZEROS                  TO GFCTYV-COD-SQL-ERRO
                                          GFCTYV-COD-MSG-ERRO
                                          GFCTYV-ERRO.
           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 0300-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO GFCTYV-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA OBTER DESCRICAO DA MENSAGEM                     *
      *----------------------------------------------------------------*
       0300-OBTER-DESC-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE 100                    TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTYU-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTYU-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTYU-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
                                          GFCTYV-DESC-MSG-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0951'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTYV-ERRO
               PERFORM 0400-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTYV-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
                                          GFCTYV-DESC-MSG-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0951'         TO GFCT0M-TRANSACAO
               PERFORM 0400-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO GFCTYV-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTYV-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTYV-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTYV-DESC-MSG-ERRO
               PERFORM 0400-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA RETORNAR AO CHAMADOR                            *
      *----------------------------------------------------------------*
       0400-FINALIZAR-PROGRAMA         SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA VERIFICAR DISPONIBILIDADE DO SISTEMA            *
      *----------------------------------------------------------------*
       0500-TRATAR-DISP-SISTEMA        SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5522'             TO WRK-MODULO.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE GFCTYU-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTYU-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE '0000000'              TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTYV-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
                                          GFCTYV-DESC-MSG-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0951'         TO GFCT0M-TRANSACAO
               PERFORM 0400-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO GFCTYV-COD-SQL-ERRO
               END-IF
               MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCTYV-DESC-MSG-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0951'         TO GFCT0M-TRANSACAO
               MOVE GFCTG1-ERRO        TO GFCTYV-ERRO
               PERFORM 0400-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO GFCTYV-ERRO
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
                                          GFCTYV-DESC-MSG-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0951'         TO GFCT0M-TRANSACAO
               PERFORM 0400-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONSISTIR CAMPOS DE ENTRADA                     *
      *----------------------------------------------------------------*
       0600-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTYU-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTYU-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTYU-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTYU-AGENCIA          NOT NUMERIC)                OR
              (GFCTYU-AGENCIA          EQUAL ZEROS)                OR
              (GFCTYU-TIPO             NOT EQUAL 1 AND 2 AND 3)    OR
              (GFCTYU-TIPO             NOT EQUAL 1 AND
               GFCTYU-INIC-VIG-FLEX    EQUAL SPACES)
               MOVE 1                  TO GFCTYV-ERRO
                                          GFCTYV-COD-MSG-ERRO
               MOVE ZEROS              TO GFCTYV-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 0300-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTYV-DESC-MSG-ERRO
               PERFORM 0400-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA PROCESSAR DADOS                                 *
      *----------------------------------------------------------------*
       0700-PROCESSAR-DADOS            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCTB0J0.

           IF  GFCTYU-TIPO             EQUAL  1
               PERFORM 0800-LER-RENOVACAO
           ELSE
               PERFORM 0900-LER-CANCELAMENTO
           END-IF.

           PERFORM 1100-PROCESSA.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA LER TABELA PARA UMA RENOVACAO                   *
      *----------------------------------------------------------------*
       0800-LER-RENOVACAO              SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CBCO         OF GFCTB0J0.
           MOVE GFCTYU-AGENCIA         TO CAG-BCRIA    OF GFCTB0J0.

           EXEC SQL
             SELECT CBCO,
                    CAG_BCRIA,
                    DINIC_VGCIA_VIP,
                    DFIM_VGCIA_VIP,
                    PDESC_VIP,
                    CTPO_OPER_MOTVO,
                    CMOTVO_JUSTF,
                    RCOMPL_JUSTF,
                    CFUNC_MANUT_INCL,
                    CFUNC_MANUT,
                    HINCL_REG,
                    CTPO_OPER_CANCT,
                    CMOTVO_CANCT,
                    RJUSTF_CANCT
             INTO  :GFCTB0J0.CBCO,
                   :GFCTB0J0.CAG-BCRIA,
                   :GFCTB0J0.DINIC-VGCIA-VIP,
                   :GFCTB0J0.DFIM-VGCIA-VIP,
                   :GFCTB0J0.PDESC-VIP,
                   :GFCTB0J0.CTPO-OPER-MOTVO,
                   :GFCTB0J0.CMOTVO-JUSTF,
                   :GFCTB0J0.RCOMPL-JUSTF,
                   :GFCTB0J0.CFUNC-MANUT-INCL,
                   :GFCTB0J0.CFUNC-MANUT,
                   :GFCTB0J0.HINCL-REG,
                   :GFCTB0J0.CTPO-OPER-CANCT,
                   :GFCTB0J0.CMOTVO-CANCT,
                   :GFCTB0J0.RJUSTF-CANCT
             FROM   DB2PRD.TFLEXZ_VIP_AG
             WHERE  CBCO               = :GFCTB0J0.CBCO           AND
                    CAG_BCRIA          = :GFCTB0J0.CAG-BCRIA      AND
                    DINIC_VGCIA_VIP    IN
                   (SELECT MAX(DINIC_VGCIA_VIP)
                       FROM   DB2PRD.TFLEXZ_VIP_AG
                       WHERE CBCO         = :GFCTB0J0.CBCO
                       AND   CAG_BCRIA    = :GFCTB0J0.CAG-BCRIA  )
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE '0010'             TO GFCT0M-LOCAL
               PERFORM 0300-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0J0'      TO WRK-NOME-TAB
               MOVE 'GFCT0951'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TFLEXZ_VIP_AG'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTYV-ERRO
               MOVE SQLCODE            TO WRK-9-9-R
               MOVE WRK-9-3            TO GFCTYV-COD-SQL-ERRO
               MOVE SPACES             TO GFCTYV-DESC-MSG-ERRO
               MOVE 0010               TO GFCTYV-COD-MSG-ERRO
               PERFORM 1000-CONC-MSG
               PERFORM 0400-FINALIZAR-PROGRAMA
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTYV-ERRO
               MOVE ZEROS              TO GFCTYV-COD-SQL-ERRO
               MOVE 1241               TO GFCTYV-COD-MSG-ERRO
               MOVE '1241'             TO GFCTG2-COD-MSG
               PERFORM 0300-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTYV-DESC-MSG-ERRO
               PERFORM 0400-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA LER TABELA PARA UM CANCELAMENTO OU CONSULTA     *
      *----------------------------------------------------------------*
       0900-LER-CANCELAMENTO           SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CBCO            OF GFCTB0J0.
           MOVE GFCTYU-AGENCIA         TO CAG-BCRIA       OF GFCTB0J0.
           MOVE GFCTYU-INIC-VIG-FLEX   TO DINIC-VGCIA-VIP OF GFCTB0J0.

           EXEC SQL
             SELECT CBCO,
                    CAG_BCRIA,
                    DINIC_VGCIA_VIP,
                    DFIM_VGCIA_VIP,
                    PDESC_VIP,
                    CTPO_OPER_MOTVO,
                    CMOTVO_JUSTF,
                    RCOMPL_JUSTF,
                    CFUNC_MANUT_INCL,
                    CFUNC_MANUT,
                    CTPO_OPER_CANCT,
                    CMOTVO_CANCT,
                    RJUSTF_CANCT
             INTO  :GFCTB0J0.CBCO,
                   :GFCTB0J0.CAG-BCRIA,
                   :GFCTB0J0.DINIC-VGCIA-VIP,
                   :GFCTB0J0.DFIM-VGCIA-VIP,
                   :GFCTB0J0.PDESC-VIP,
                   :GFCTB0J0.CTPO-OPER-MOTVO,
                   :GFCTB0J0.CMOTVO-JUSTF,
                   :GFCTB0J0.RCOMPL-JUSTF,
                   :GFCTB0J0.CFUNC-MANUT-INCL,
                   :GFCTB0J0.CFUNC-MANUT,
                   :GFCTB0J0.CTPO-OPER-CANCT,
                   :GFCTB0J0.CMOTVO-CANCT,
                   :GFCTB0J0.RJUSTF-CANCT
             FROM   DB2PRD.TFLEXZ_VIP_AG
             WHERE  CBCO               = :GFCTB0J0.CBCO           AND
                    CAG_BCRIA          = :GFCTB0J0.CAG-BCRIA      AND
                    DINIC_VGCIA_VIP    = :GFCTB0J0.DINIC-VGCIA-VIP
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE '0020'             TO GFCT0M-LOCAL
               PERFORM 0300-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0J0'      TO WRK-NOME-TAB
               MOVE 'GFCT0951'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TFLEXZ_VIP_AG'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTYV-ERRO
               MOVE SQLCODE            TO WRK-9-9-R
               MOVE WRK-9-3            TO GFCTYV-COD-SQL-ERRO
               MOVE SPACES             TO GFCTYV-DESC-MSG-ERRO
               MOVE 0010               TO GFCTYV-COD-MSG-ERRO
               PERFORM 1000-CONC-MSG
               PERFORM 0400-FINALIZAR-PROGRAMA
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTYV-ERRO
               MOVE ZEROS              TO GFCTYV-COD-SQL-ERRO
               MOVE 1241               TO GFCTYV-COD-MSG-ERRO
               MOVE '1241'             TO GFCTG2-COD-MSG
               PERFORM 0300-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTYV-DESC-MSG-ERRO
               PERFORM 0400-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONCATENAR DESCRICAO DE MENSAGEM DE ERRO        *
      *----------------------------------------------------------------*
       1000-CONC-MSG                   SECTION.
      *----------------------------------------------------------------*

           STRING GFCTG3-DESC-MSG
                  WRK-NOME-TAB
           DELIMITED BY '  '           INTO GFCTYV-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE PROCESSAMENTO                                     *
      *----------------------------------------------------------------*
       1100-PROCESSA                   SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTYU-AGENCIA         TO GFCTYV-AGENCIA.
           MOVE DINIC-VGCIA-VIP OF GFCTB0J0
                                       TO GFCTYV-INIC-VIG-FLEX.
           MOVE DFIM-VGCIA-VIP OF GFCTB0J0
                                       TO GFCTYV-FIM-VIG-FLEX.
           MOVE PDESC-VIP OF GFCTB0J0  TO GFCTYV-PERC-DESCTO.
           MOVE CTPO-OPER-MOTVO OF GFCTB0J0
                                       TO GFCTYV-CTIPO-OPER-MOTVO.
           MOVE CMOTVO-JUSTF OF GFCTB0J0
                                       TO GFCTYV-CMOTVO-JUSTF.
           MOVE RCOMPL-JUSTF-TEXT OF GFCTB0J0
                                       TO GFCTYV-RJUSTF-MANUT-SERVC.
           ADD  1                      TO GFCTYV-QTDE-TOT-REG.

           INITIALIZE GFCTB0G0.

           MOVE CTPO-OPER-MOTVO OF GFCTB0J0
                                       TO CTPO-OPER-MOTVO OF GFCTB0G0.

           PERFORM 1200-PESQUISA-OPERACAO.

           INITIALIZE GFCTB089.

           MOVE CTPO-OPER-MOTVO OF GFCTB0J0
                                       TO CTPO-OPER-MOTVO OF GFCTB089.
           MOVE CMOTVO-JUSTF OF GFCTB0J0
                                       TO CMOTVO-JUSTF OF GFCTB089.

           PERFORM 1300-PESQUISA-MOTIVO.

           MOVE RTPO-OPER-MOTVO OF GFCTB0G0
                                       TO GFCTYV-DESCR-TIPO.
           MOVE RMOTVO-JUSTF OF GFCTB089
                                       TO GFCTYV-DESCR-MOTIVO.

           PERFORM 1400-PESQUISA-AGENCIA.

           IF  CTPO-OPER-CANCT OF GFCTB0J0
                                       EQUAL ZEROS
               MOVE ZEROS              TO GFCTYV-CTIPO-OPER-CANC
                                          GFCTYV-CMOTVO-CANC
               MOVE SPACES             TO GFCTYV-RJUSTF-MANUT-CANC
                                          GFCTYV-DESCR-TP-CANC
                                          GFCTYV-DESCR-MTV-CANC
           ELSE
               MOVE CTPO-OPER-CANCT OF GFCTB0J0
                                       TO GFCTYV-CTIPO-OPER-CANC
               MOVE CMOTVO-CANCT OF GFCTB0J0
                                       TO GFCTYV-CMOTVO-CANC
               MOVE RJUSTF-CANCT-TEXT OF GFCTB0J0
                                       TO GFCTYV-RJUSTF-MANUT-CANC
               INITIALIZE GFCTB0G0
               MOVE CTPO-OPER-CANCT OF GFCTB0J0
                                       TO CTPO-OPER-MOTVO OF GFCTB0G0
               PERFORM 1200-PESQUISA-OPERACAO
               MOVE RTPO-OPER-MOTVO OF GFCTB0G0
                                       TO GFCTYV-DESCR-TP-CANC
               INITIALIZE GFCTB089
               MOVE CTPO-OPER-CANCT OF GFCTB0J0
                                       TO CTPO-OPER-MOTVO OF GFCTB089
               MOVE CMOTVO-CANCT OF GFCTB0J0
                                       TO CMOTVO-JUSTF OF GFCTB089
               PERFORM 1300-PESQUISA-MOTIVO
               MOVE RMOTVO-JUSTF OF GFCTB089
                                       TO GFCTYV-DESCR-MTV-CANC
           END-IF.

           MOVE CFUNC-MANUT-INCL OF GFCTB0J0
                                       TO WRK-COD-FUNCIONARIO

           MOVE WRK-COD-FUNC           TO GFCTYV-COD-FUNC
                                          WRK-COD-FUNCIONAL-0315.

           PERFORM 1500-PESQUISA-NOME-FUNC

           MOVE WRK-NOME-FUNC-0315     TO GFCTYV-NOME-FUNC

           IF  CFUNC-MANUT OF GFCTB0J0 NOT EQUAL ZEROS
               MOVE CFUNC-MANUT  OF GFCTB0J0
                                           TO WRK-COD-FUNCIONARIO

               MOVE WRK-COD-FUNC           TO GFCTYV-COD-FUNC-CANC
                                              WRK-COD-FUNCIONAL-0315
               PERFORM 1500-PESQUISA-NOME-FUNC
               MOVE WRK-NOME-FUNC-0315     TO GFCTYV-NOME-FUNC-CANC
           ELSE
               MOVE SPACES                 TO GFCTYV-NOME-FUNC-CANC
                                              GFCTYV-COD-FUNC-CANC
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA PESQUISAR NOME DO TIPO DE OPERACAO              *
      *----------------------------------------------------------------*
       1200-PESQUISA-OPERACAO          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT RTPO_OPER_MOTVO
             INTO  :GFCTB0G0.RTPO-OPER-MOTVO
             FROM   DB2PRD.TPO_OPER_MOTVO
             WHERE  CTPO_OPER_MOTVO    = :GFCTB0G0.CTPO-OPER-MOTVO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE '0030'             TO GFCT0M-LOCAL
               PERFORM 0300-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0G0'      TO WRK-NOME-TAB
               MOVE 'GFCT0951'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TPO_OPER_MOTVO'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTYV-ERRO
               MOVE SQLCODE            TO WRK-9-9-R
               MOVE WRK-9-3            TO GFCTYV-COD-SQL-ERRO
               MOVE SPACES             TO GFCTYV-DESC-MSG-ERRO
               MOVE 0010               TO GFCTYV-COD-MSG-ERRO
               PERFORM 1000-CONC-MSG
               PERFORM 0400-FINALIZAR-PROGRAMA
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE SPACES             TO RTPO-OPER-MOTVO OF GFCTB0G0
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA PESQUISAR NOME DO MOTIVO                        *
      *----------------------------------------------------------------*
       1300-PESQUISA-MOTIVO            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT RMOTVO_JUSTF
             INTO  :GFCTB089.RMOTVO-JUSTF
             FROM   DB2PRD.MOTVO_JUSTF_OPER
             WHERE  CTPO_OPER_MOTVO    = :GFCTB089.CTPO-OPER-MOTVO  AND
                    CMOTVO_JUSTF       = :GFCTB089.CMOTVO-JUSTF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE '0040'             TO GFCT0M-LOCAL
               PERFORM 0300-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB089'      TO WRK-NOME-TAB
               MOVE 'GFCT0951'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'MOTVO_JUSTF_OPER' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTYV-ERRO
               MOVE SQLCODE            TO WRK-9-9-R
               MOVE WRK-9-3            TO GFCTYV-COD-SQL-ERRO
               MOVE SPACES             TO GFCTYV-DESC-MSG-ERRO
               MOVE 0010               TO GFCTYV-COD-MSG-ERRO
               PERFORM 1000-CONC-MSG
               PERFORM 0400-FINALIZAR-PROGRAMA
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE SPACES             TO RMOTVO-JUSTF OF GFCTB089
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA PESQUISAR NOME DA AGENCIA                       *
      *----------------------------------------------------------------*
       1400-PESQUISA-AGENCIA           SECTION.
      *----------------------------------------------------------------*

           MOVE 660                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.
           MOVE GFCTYU-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTYU-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTYU-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.
           MOVE GFCTYU-AGENCIA         TO GFCT1V-COD-DEPDC(1)
           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTYV-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
                                          GFCTYV-DESC-MSG-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0951'         TO GFCT0M-TRANSACAO
               PERFORM 0400-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCT1X-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTYV-FIM
               MOVE GFCT1X-ERRO        TO GFCTYV-ERRO
               MOVE GFCT1X-COD-SQL-ERRO
                                       TO GFCTYV-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO
                                       TO GFCTYV-COD-MSG-ERRO
               IF  GFCT1X-ERRO         EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO
                                       TO GFCTYV-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006-'
                          GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTYV-DESC-MSG-ERRO
               END-IF
               PERFORM 0400-FINALIZAR-PROGRAMA
           END-IF.

           MOVE GFCT1X-DESC-DEPDC(1)   TO WRK-DESC-50.
           MOVE WRK-DESC-40            TO GFCTYV-NOME-AGENCIA.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA PESQUISAR NOME DO FUNCIONARIO                   *
      *----------------------------------------------------------------*
       1500-PESQUISA-NOME-FUNC         SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO WRK-VERSAO-0315.

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

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE SPACES             TO WRK-NOME-FUNC-0315
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
