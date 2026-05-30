      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0551.
       AUTHOR.     HELIO SANTONI NETO.
      *================================================================*
      *      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0551                                    *
      *    PROGRAMADOR.:   HELIO SANTONI           - PROCWORK - GP.50  *
      *    ANALISTA....:   VALERIA TORQUATO        - PROCWORK - GP.50  *
      *    DATA........:   01/06/2009                                  *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      LISTA DE EXCLUSAO DA AUTORIZACAO DE EMISSAO DE EXTRATO    *
      *      ANUAL DE TARIFAS VIA CORREIO.                             *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.TAUTRZ_ENVIO_AGPTO              GFCTB0L4          *
      *       DB2PRD.TAUTRZ_ENVIO_CLI                GFCTB0L5          *
      *       DB2PRD.TAUTRZ_ENVIO_SGMTO              GFCTB0L7          *
      *       DB2PRD.TAUTRZ_ENVIO_POSTO              GFCTB0L6          *
      *       DB2PRD.TTPO_SGMTO_GSTAO                GFCTB0H5          *
      *       DB2PRD.V01CLIENTE_AGENCIA              CLIEV004          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5522 - VERIFICACAO DE DISPONIBILIDADE ON-LINE         *
      *      GFCT5090 - OBTER DESCRICAO DO CORRESPONDENTE BANCARIO     *
      *                                                                *
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7548                                     *
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
           '*  INICIO DA WORKING GFCT0551  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       01  IND-1                       PIC 9(05)   COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-AUX-9-S                 PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-AUX-9-S.
           05 WRK-AUX-9                PIC  9(009).
           05 FILLER                   REDEFINES WRK-AUX-9.
              10 FILLER                PIC  X(006).
              10 WRK-AUX-9-X.
                 15 WRK-AUX-9-3        PIC  9(003).

       01  WRK-AUX-5-S                 PIC +9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-5-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-5               PIC  9(005).

       01  WRK-AUX-3-S                 PIC +9(003)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-3-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-3               PIC  9(003).

       01  WRK-AUX-2-S                 PIC +9(002)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-2-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-2               PIC  9(002).

       01  WRK-DESPREZA                PIC  9(005) COMP-3  VALUE ZEROS.
       01  WRK-WAGV-QTDE-TOT           PIC  9(006) COMP-3  VALUE ZEROS.
       01  WRK-WAGV-QTDE-OCOR          PIC  9(003) COMP-3  VALUE ZEROS.
       01  WRK-FINALIZAR               PIC  X(001)         VALUE SPACES.
       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       01  WRK-NOME-TAB                PIC  X(008)         VALUE SPACES.

       01  WRK-TIMESTAMP               PIC X(26)           VALUE
           '0001-01-01-01.01.01.000001'.

       01  WRK-HINCL.
           03  WRK-DATA.
               05  WRK-ANO             PIC 9(04)           VALUE ZEROS.
               05  FILLER              PIC X(01)           VALUE SPACES.
               05  WRK-MES             PIC 9(02)           VALUE ZEROS.
               05  FILLER              PIC X(01)           VALUE SPACES.
               05  WRK-DIA             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(16)           VALUE SPACES.

       01  WRK-DATA-AUX.
           03  WRK-DIA-AUX             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '.'.
           03  WRK-MES-AUX             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '.'.
           03  WRK-ANO-AUX             PIC 9(04)           VALUE ZEROS.

       01  WRK-MSG05.
           03  FILLER                  PIC X(26)           VALUE
           'ERRO NA CHAMADA DO MODULO '.
           03  WRK-MSG05-MODULO        PIC X(08)           VALUE SPACES.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA DO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(041)         VALUE SPACES.

       01  WRK-MSG002                  PIC  X(075)         VALUE
           'SISTEMA INDISPONIVEL'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           'AREA DE COMUNICACAO COM GFCT5523'.
      *----------------------------------------------------------------*

       COPY I#GFCTG2.

       COPY I#GFCTG3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           'AREA DE COMUNICACAO COM GFCT5522'.
      *----------------------------------------------------------------*

       COPY I#GFCTFZ.

       COPY I#GFCTG1.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DO MODULO GFCT5090   *'.
      *----------------------------------------------------------------*

       COPY I#GFCTGB.

       COPY I#GFCTGC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0L4
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0L5
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0L6
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0L7
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV004
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0H5
           END-EXEC.

      *--> '*     AREAS DECLARE CURSOR     *'

      *-->      SELECIONAR TODOS CLIENTES
           EXEC SQL
               DECLARE CSR01-GFCTB0L5 CURSOR WITH HOLD FOR
               SELECT  CAGPTO_CTA,
                       HINCL_REG_SIST,
                       CCNPJ_CPF,
                       CFLIAL_CGC,
                       CCTRL_CPF_CGC
               FROM    DB2PRD.TAUTRZ_ENVIO_CLI
               WHERE   CAGPTO_CTA         = :GFCTB0L5.CAGPTO-CTA
                 AND   CINDCD_AUTRZ_EMIS  = :GFCTB0L5.CINDCD-AUTRZ-EMIS
            ORDER BY   CCNPJ_CPF,
                       CFLIAL_CGC,
                       CCTRL_CPF_CGC,
                       HINCL_REG_SIST
           END-EXEC.

      *-->      SELECIONAR TODOS SEGMENTO
           EXEC SQL
               DECLARE CSR02-GFCTB0L7 CURSOR WITH HOLD FOR
               SELECT  CAGPTO_CTA,
                       HINCL_REG_SIST,
                       CSGMTO_INIC_CLI
               FROM    DB2PRD.TAUTRZ_ENVIO_SGMTO
               WHERE   CAGPTO_CTA         = :GFCTB0L7.CAGPTO-CTA
                 AND   CINDCD_AUTRZ_EMIS  = :GFCTB0L7.CINDCD-AUTRZ-EMIS
            ORDER BY   CSGMTO_INIC_CLI,
                       HINCL_REG_SIST
           END-EXEC.

      *-->      SELECIONAR TODOS POSTO
           EXEC SQL
               DECLARE CSR03-GFCTB0L6 CURSOR WITH HOLD FOR
               SELECT  CAGPTO_CTA,
                       HINCL_REG_SIST,
                       CDEPDC,
                       CPOSTO_SERVC
               FROM    DB2PRD.TAUTRZ_ENVIO_POSTO
               WHERE   CAGPTO_CTA         = :GFCTB0L6.CAGPTO-CTA
                 AND   CINDCD_AUTRZ_EMIS  = :GFCTB0L6.CINDCD-AUTRZ-EMIS
            ORDER BY   CDEPDC,
                       CPOSTO_SERVC,
                       HINCL_REG_SIST
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT0551   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY GFCTWAGU.

       COPY GFCTWAGV.

       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE DIVISION              USING WAGU-ENTRADA
                                             WAGV-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *  ROTINA PRINCIPAL DO PROGRAMA                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR

           PERFORM 2000-PROCESSAR

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  INICIALIZACAO DO PROGRAMA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WAGV-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  WAGV-SAIDA
                                       GFCT0M-AREA-ERROS
                                       SQLCA.

           MOVE 220                    TO WAGV-LL
           MOVE WAGU-TRANSACAO         TO WAGV-TRANSACAO
           MOVE WAGU-FUNCAO            TO WAGV-FUNCAO
           MOVE WAGU-FUNC-BDSCO        TO WAGV-FUNC-BDSCO
           MOVE WAGU-FILTRO            TO WAGV-FILTRO
           MOVE ZEROS                  TO WAGV-ERRO
                                          WAGV-COD-SQL-ERRO
                                          WAGV-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.
           MOVE ZEROS                  TO WAGV-QTDE-TOT-REG
                                          WRK-WAGV-QTDE-TOT
           MOVE 'N'                    TO WAGV-FIM
                                          WRK-FINALIZAR.

           IF  WAGU-QTDE-OCOR        IS NUMERIC
               MOVE WAGU-QTDE-OCOR     TO WAGV-QTDE-OCOR
                                          WRK-WAGV-QTDE-OCOR
           ELSE
               MOVE ZEROS              TO WAGV-QTDE-OCOR
           END-IF.

           PERFORM 1100-OBTER-DESC-MSG.

           PERFORM 1200-CONSISTIR-CAMPOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTER DESCRICAO DA MENSAGEM                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  GFCTG3-SAIDA

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE WAGU-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE WAGU-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE WAGU-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE SPACES             TO WAGV-DESC-MSG-ERRO
               MOVE 9                  TO WAGV-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0551'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE SPACES         TO WAGV-DESC-MSG-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG3-COD-SQL-ERRO
                                       TO WAGV-COD-SQL-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO
                                       TO WAGV-DESC-MSG-ERRO
               END-IF

               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0551'         TO GFCT0M-TRANSACAO
               MOVE GFCTG3-ERRO        TO WAGV-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE GFCTG3-ERRO        TO WAGV-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO WAGV-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO WAGV-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO WAGV-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTG3-DESC-MSG        TO WAGV-DESC-MSG-ERRO
                                         (1: LENGTH OF GFCTG3-DESC-MSG)
                                          GFCT0M-TEXTO
                                         (1: LENGTH OF GFCTG3-DESC-MSG).

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTER CONSISTENCIA DE CAMPOS                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-CONSISTIR-CAMPOS           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1210-VERIFICAR-ONLINE.

           IF (WAGU-TRANSACAO       EQUAL SPACES  OR LOW-VALUES) OR
              (WAGU-FUNCAO          EQUAL SPACES  OR LOW-VALUES) OR
              (WAGU-FUNC-BDSCO      EQUAL SPACES  OR LOW-VALUES) OR
              (WAGU-CAGPTO            NOT NUMERIC)               OR
              (WAGU-CAGPTO          EQUAL ZEROS)
               MOVE 1                  TO WAGV-ERRO
               MOVE ZEROS              TO WAGV-COD-SQL-ERRO
               MOVE 0001               TO WAGV-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO WAGV-FIM
               PERFORM 1100-OBTER-DESC-MSG
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  VERIFICA DISPONIBILIDADE ONLINE                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-VERIFICAR-ONLINE           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE +101                   TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE WAGU-TRANSACAO         TO GFCTFZ-TRANSACAO.
           MOVE WAGU-FUNCAO            TO GFCTFZ-FUNCAO.
           MOVE WAGU-FUNC-BDSCO        TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.
           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE SPACES             TO WAGV-DESC-MSG-ERRO
               MOVE 9                  TO WAGV-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0198'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             NOT EQUAL ZEROS
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE SPACES         TO WAGV-DESC-MSG-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO WAGV-COD-SQL-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO WAGV-DESC-MSG-ERRO
               END-IF

               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0198'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO WAGV-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO WAGV-ERRO
               MOVE SPACES             TO WAGV-DESC-MSG-ERRO
               MOVE WRK-MSG002         TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0198'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  PROCESSAMENTO PRINCIPAL                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO CINDCD-AUTRZ-EMIS  OF GFCTB0L5
                                          CINDCD-AUTRZ-EMIS  OF GFCTB0L6
                                          CINDCD-AUTRZ-EMIS  OF GFCTB0L7
           MOVE WAGU-CAGPTO            TO CAGPTO-CTA         OF GFCTB0L5
                                          CAGPTO-CTA         OF GFCTB0L6
                                          CAGPTO-CTA         OF GFCTB0L7

           IF  WAGU-CAGPTO          EQUAL 4
               PERFORM 2100-TRATAR-CLIENTE
           ELSE
             IF  WAGU-CAGPTO        EQUAL 9
                 PERFORM 2200-TRATAR-SEGMTO
             ELSE
                 PERFORM 2300-TRATAR-POSTO
             END-IF
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATAR CURSOR CSR01 OBTER CLIENTES                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-TRATAR-CLIENTE             SECTION.
      *----------------------------------------------------------------*

           PERFORM 2110-ABRIR-CSR01-GFCTB0L5

           MOVE WAGU-QTDE-OCOR         TO WRK-DESPREZA

           PERFORM 2120-LER-CSR01-GFCTB0L5

           PERFORM 2120-LER-CSR01-GFCTB0L5  VARYING  IND-1
                   FROM 1              BY 1
                   UNTIL IND-1         GREATER WRK-DESPREZA OR
                   WRK-FINALIZAR       EQUAL 'S'.

           MOVE ZEROS                  TO IND-1

           PERFORM 2130-PROCESSAR-CURSOR-1
                                       UNTIL WRK-FINALIZAR EQUAL 'S'

           PERFORM 2140-FECHAR-CSR01-GFCTB0L5.

           IF  WRK-WAGV-QTDE-TOT    EQUAL ZEROS
               MOVE 1                  TO WAGV-ERRO
               MOVE ZEROS              TO WAGV-COD-SQL-ERRO
               MOVE 1590               TO WAGV-COD-MSG-ERRO
               MOVE '1590'             TO GFCTG2-COD-MSG
               PERFORM 1100-OBTER-DESC-MSG
               MOVE 'S'                TO WAGV-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ABRIR CURSOR CSR01                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-ABRIR-CSR01-GFCTB0L5       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR01-GFCTB0L5
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TAUTRZ_ENVIO_CLI' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAGV-COD-SQL-ERRO
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE 0005               TO WAGV-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L5'         TO WRK-NOME-TAB
               PERFORM 2111-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  MOVER CAMPOS PARA ERRO DB2                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2111-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT0551'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE 9                      TO WAGV-ERRO.

           PERFORM 1100-OBTER-DESC-MSG.

           MOVE SPACES                 TO WAGV-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG   ' - '
                  WRK-NOME-TAB
           DELIMITED BY '   '          INTO WAGV-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       2111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  LER CURSOR CSR01                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-LER-CSR01-GFCTB0L5         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR01-GFCTB0L5
               INTO    :GFCTB0L5.CAGPTO-CTA,
                       :GFCTB0L5.HINCL-REG-SIST,
                       :GFCTB0L5.CCNPJ-CPF,
                       :GFCTB0L5.CFLIAL-CGC,
                       :GFCTB0L5.CCTRL-CPF-CGC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TAUTRZ_ENVIO_CLI' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAGV-COD-SQL-ERRO
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 0006               TO WAGV-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L5'         TO WRK-NOME-TAB
               PERFORM 2111-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          WAGV-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO WAGV-ERRO
                   MOVE ZEROS          TO WAGV-COD-SQL-ERRO
                   MOVE 1590           TO WAGV-COD-MSG-ERRO
                   MOVE '1590'         TO GFCTG2-COD-MSG
                   PERFORM 1100-OBTER-DESC-MSG
                   MOVE 'S'            TO WAGV-FIM
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  PROCESSAR CURSOR CSR01                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2130-PROCESSAR-CURSOR-1         SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0L5 TO CAGPTO-CTA        OF GFCTB0L4
           MOVE HINCL-REG-SIST         OF GFCTB0L5
                                       TO HINCL-REG-SIST    OF GFCTB0L4
           PERFORM 2131-ACESSAR-GFCTB0L4

           IF  SQLCODE              EQUAL ZEROS
               ADD 1                   TO IND-1
                                          WRK-WAGV-QTDE-TOT
                                          WRK-WAGV-QTDE-OCOR
               MOVE WRK-WAGV-QTDE-TOT  TO WAGV-QTDE-TOT-REG
               MOVE WRK-WAGV-QTDE-OCOR TO WAGV-QTDE-OCOR
               ADD 158                 TO WAGV-LL
               MOVE CAGPTO-CTA         OF GFCTB0L5
                                       TO WRK-AUX-3-S
               MOVE WRK-AUX-3          TO WAGV-CAGPTO(IND-1)
               MOVE CCNPJ-CPF          OF GFCTB0L5
                                       TO WRK-AUX-9-S
               MOVE WRK-AUX-9          TO WAGV-NUMERO(IND-1)
               MOVE CFLIAL-CGC         OF GFCTB0L5
                                       TO WRK-AUX-5-S
               MOVE WRK-AUX-5          TO WAGV-FILIAL(IND-1)
               MOVE CCTRL-CPF-CGC      OF GFCTB0L5
                                       TO WRK-AUX-2-S
               MOVE WRK-AUX-2          TO WAGV-CONTRL(IND-1)

               PERFORM 2132-ACESSAR-CLIEV004

               MOVE HINCL-REG-SIST     OF GFCTB0L5
                                       TO WAGV-HINCL(IND-1)
                                          WRK-HINCL
               MOVE WRK-ANO            TO WRK-ANO-AUX
               MOVE WRK-MES            TO WRK-MES-AUX
               MOVE WRK-DIA            TO WRK-DIA-AUX
               MOVE WRK-DATA-AUX       TO WAGV-DT-SOLC(IND-1)

           END-IF.

           PERFORM 2120-LER-CSR01-GFCTB0L5

           IF  IND-1                 EQUAL 5
               IF  WRK-FINALIZAR NOT EQUAL 'S'
                   MOVE 'S'            TO WRK-FINALIZAR
                   MOVE CAGPTO-CTA     OF GFCTB0L5
                                       TO WRK-AUX-3-S
                   MOVE WRK-AUX-3      TO WAGV-FTR-CAGPTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  VERIFICA SE JAH FOI REG. EXCLUIDO                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2131-ACESSAR-GFCTB0L4           SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO CINDCD-AUTRZ-EMIS OF GFCTB0L4
           MOVE WRK-TIMESTAMP          TO HEXCL-REG-SIST    OF GFCTB0L4

           EXEC SQL
               SELECT  HEXCL_REG_SIST
               INTO    :GFCTB0L4.HEXCL-REG-SIST
               FROM    DB2PRD.TAUTRZ_ENVIO_AGPTO
               WHERE  CAGPTO_CTA         = :GFCTB0L4.CAGPTO-CTA
                 AND  CINDCD_AUTRZ_EMIS  = :GFCTB0L4.CINDCD-AUTRZ-EMIS
                 AND  HINCL_REG_SIST     = :GFCTB0L4.HINCL-REG-SIST
                 AND  HEXCL_REG_SIST     = :GFCTB0L4.HEXCL-REG-SIST
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TAUTRZ_ENVIO_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAGV-COD-SQL-ERRO
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE 0012               TO WAGV-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L4'         TO WRK-NOME-TAB
               PERFORM 2111-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTER DESCRICAO DO CLIENTE                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2132-ACESSAR-CLIEV004           SECTION.
      *----------------------------------------------------------------*

           MOVE CCNPJ-CPF OF GFCTB0L5  TO CCGC-CPF          OF CLIEV004
           MOVE CFLIAL-CGC OF GFCTB0L5 TO CFLIAL-CGC        OF CLIEV004
           MOVE CCTRL-CPF-CGC          OF GFCTB0L5
                                       TO CCTRL-CPF-CGC     OF CLIEV004

           EXEC SQL
               SELECT  IPSSOA_COPLT
               INTO    :CLIEV004.IPSSOA-COPLT
               FROM    DB2PRD.V01CLIENTE_AGENCIA
               WHERE  CCGC_CPF           = :CLIEV004.CCGC-CPF
                 AND  CFLIAL_CGC         = :CLIEV004.CFLIAL-CGC
                 AND  CCTRL_CPF_CGC      = :CLIEV004.CCTRL-CPF-CGC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'V01CLIENTE_AGENCIA'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAGV-COD-SQL-ERRO
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE 0012               TO WAGV-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'CLIEV004'         TO WRK-NOME-TAB
               PERFORM 2111-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE IPSSOA-COPLT           OF CLIEV004 (1:30)
                                       TO WAGV-NM-CLIE(IND-1).

      *----------------------------------------------------------------*
       2132-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  FECHA CURSOR CSR01                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2140-FECHAR-CSR01-GFCTB0L5      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0L5
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TAUTRZ_ENVIO_CLI' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAGV-COD-SQL-ERRO
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE 0011               TO WAGV-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L5'         TO WRK-NOME-TAB
               PERFORM 2111-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATAR CURSOR CSR02 OBTER SEGMENTO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-TRATAR-SEGMTO              SECTION.
      *----------------------------------------------------------------*

           PERFORM 2210-ABRIR-CSR02-GFCTB0L7

           MOVE WAGU-QTDE-OCOR         TO WRK-DESPREZA

           PERFORM 2220-LER-CSR02-GFCTB0L7

           PERFORM 2220-LER-CSR02-GFCTB0L7  VARYING  IND-1
                   FROM 1              BY 1
                   UNTIL IND-1         GREATER WRK-DESPREZA OR
                   WRK-FINALIZAR       EQUAL 'S'.

           MOVE ZEROS                  TO IND-1

           PERFORM 2230-PROCESSAR-CURSOR-2
                                       UNTIL WRK-FINALIZAR EQUAL 'S'

           PERFORM 2240-FECHAR-CSR02-GFCTB0L7.

           IF  WRK-WAGV-QTDE-TOT    EQUAL ZEROS
               MOVE 1                  TO WAGV-ERRO
               MOVE ZEROS              TO WAGV-COD-SQL-ERRO
               MOVE 1590               TO WAGV-COD-MSG-ERRO
               MOVE '1590'             TO GFCTG2-COD-MSG
               PERFORM 1100-OBTER-DESC-MSG
               MOVE 'S'                TO WAGV-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ABRIR CURSOR CSR02                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-ABRIR-CSR02-GFCTB0L7       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR02-GFCTB0L7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TAUTRZ_ENVIO_SGMTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAGV-COD-SQL-ERRO
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE 0005               TO WAGV-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L7'         TO WRK-NOME-TAB
               PERFORM 2111-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  LER CURSOR CSR02                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2220-LER-CSR02-GFCTB0L7         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR02-GFCTB0L7
               INTO    :GFCTB0L7.CAGPTO-CTA,
                       :GFCTB0L7.HINCL-REG-SIST,
                       :GFCTB0L7.CSGMTO-INIC-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TAUTRZ_ENVIO_SGMTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAGV-COD-SQL-ERRO
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE 0006               TO WAGV-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L7'         TO WRK-NOME-TAB
               PERFORM 2111-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          WAGV-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO WAGV-ERRO
                   MOVE ZEROS          TO WAGV-COD-SQL-ERRO
                   MOVE 1590           TO WAGV-COD-MSG-ERRO
                   MOVE '1590'         TO GFCTG2-COD-MSG
                   PERFORM 1100-OBTER-DESC-MSG
                   MOVE 'S'            TO WAGV-FIM
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  PROCESSAR CURSOR CSR02                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2230-PROCESSAR-CURSOR-2         SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0L7 TO CAGPTO-CTA        OF GFCTB0L4
           MOVE HINCL-REG-SIST         OF GFCTB0L7
                                       TO HINCL-REG-SIST    OF GFCTB0L4
           PERFORM 2131-ACESSAR-GFCTB0L4

           IF  SQLCODE              EQUAL ZEROS
               ADD 1                   TO IND-1
                                          WRK-WAGV-QTDE-TOT
                                          WRK-WAGV-QTDE-OCOR
               MOVE WRK-WAGV-QTDE-TOT  TO WAGV-QTDE-TOT-REG
               MOVE WRK-WAGV-QTDE-OCOR TO WAGV-QTDE-OCOR
               ADD 158                 TO WAGV-LL
               MOVE CAGPTO-CTA         OF GFCTB0L7
                                       TO WRK-AUX-3-S
               MOVE WRK-AUX-3          TO WAGV-CAGPTO(IND-1)
               MOVE HINCL-REG-SIST     OF GFCTB0L7
                                       TO WAGV-HINCL(IND-1)
               MOVE CSGMTO-INIC-CLI    OF GFCTB0L7
                                       TO WRK-AUX-3-S
               MOVE WRK-AUX-3          TO WAGV-CSEGMTO(IND-1)

               MOVE HINCL-REG-SIST     OF GFCTB0L7
                                       TO WRK-HINCL
               MOVE WRK-ANO            TO WRK-ANO-AUX
               MOVE WRK-MES            TO WRK-MES-AUX
               MOVE WRK-DIA            TO WRK-DIA-AUX
               MOVE WRK-DATA-AUX       TO WAGV-DT-SOLC(IND-1)

               PERFORM 2231-OBTER-SEGMTO

           END-IF.

           PERFORM 2220-LER-CSR02-GFCTB0L7

           IF  IND-1                 EQUAL 5
               IF  WRK-FINALIZAR NOT EQUAL 'S'
                   MOVE 'S'            TO WRK-FINALIZAR
                   MOVE CAGPTO-CTA     OF GFCTB0L7
                                       TO WRK-AUX-3-S
                   MOVE WRK-AUX-3      TO WAGV-FTR-CAGPTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTEM NOME DO SEGMENTO                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2231-OBTER-SEGMTO               SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-INIC-CLI        OF GFCTB0L7
                                       TO CSGMTO-GSTAO-TARIF OF GFCTB0H5

           EXEC SQL
               SELECT ISGMTO_GSTAO_TARIF
               INTO  :GFCTB0H5.ISGMTO-GSTAO-TARIF
               FROM  DB2PRD.TTPO_SGMTO_GSTAO
               WHERE CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAGV-COD-SQL-ERRO
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE 0012               TO WAGV-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L3'         TO WRK-NOME-TAB
               PERFORM 2111-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE ISGMTO-GSTAO-TARIF     TO WAGV-NM-SEGMTO(IND-1).

      *----------------------------------------------------------------*
       2231-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  FECHA CURSOR CSR02                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2240-FECHAR-CSR02-GFCTB0L7      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR02-GFCTB0L7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TAUTRZ_ENVIO_SGMTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAGV-COD-SQL-ERRO
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE 0011               TO WAGV-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L7'         TO WRK-NOME-TAB
               PERFORM 2111-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATAR CURSOR CSR03 OBTER POSTO                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-TRATAR-POSTO               SECTION.
      *----------------------------------------------------------------*

           PERFORM 2310-ABRIR-CSR03-GFCTB0L6

           MOVE WAGU-QTDE-OCOR         TO WRK-DESPREZA

           PERFORM 2320-LER-CSR03-GFCTB0L6

           PERFORM 2320-LER-CSR03-GFCTB0L6  VARYING  IND-1
                   FROM 1              BY 1
                   UNTIL IND-1         GREATER WRK-DESPREZA OR
                   WRK-FINALIZAR       EQUAL 'S'.

           MOVE ZEROS                  TO IND-1

           PERFORM 2330-PROCESSAR-CURSOR-3
                                       UNTIL WRK-FINALIZAR EQUAL 'S'

           PERFORM 2340-FECHAR-CSR03-GFCTB0L6.

           IF  WRK-WAGV-QTDE-TOT    EQUAL ZEROS
               MOVE 1                  TO WAGV-ERRO
               MOVE ZEROS              TO WAGV-COD-SQL-ERRO
               MOVE 1590               TO WAGV-COD-MSG-ERRO
               MOVE '1590'             TO GFCTG2-COD-MSG
               PERFORM 1100-OBTER-DESC-MSG
               MOVE 'S'                TO WAGV-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ABRIR CURSOR CSR03                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-ABRIR-CSR03-GFCTB0L6       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR03-GFCTB0L6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TAUTRZ_ENVIO_POSTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAGV-COD-SQL-ERRO
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE 0005               TO WAGV-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L6'         TO WRK-NOME-TAB
               PERFORM 2111-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  LER CURSOR CSR03                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-LER-CSR03-GFCTB0L6         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR03-GFCTB0L6
               INTO    :GFCTB0L6.CAGPTO-CTA,
                       :GFCTB0L6.HINCL-REG-SIST,
                       :GFCTB0L6.CDEPDC,
                       :GFCTB0L6.CPOSTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TAUTRZ_ENVIO_POSTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAGV-COD-SQL-ERRO
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE 0006               TO WAGV-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L6'         TO WRK-NOME-TAB
               PERFORM 2111-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          WAGV-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO WAGV-ERRO
                   MOVE ZEROS          TO WAGV-COD-SQL-ERRO
                   MOVE 1590           TO WAGV-COD-MSG-ERRO
                   MOVE '1590'         TO GFCTG2-COD-MSG
                   PERFORM 1100-OBTER-DESC-MSG
                   MOVE 'S'            TO WAGV-FIM
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  PROCESSAR CURSOR CSR03                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2330-PROCESSAR-CURSOR-3         SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0L6 TO CAGPTO-CTA        OF GFCTB0L4
           MOVE HINCL-REG-SIST         OF GFCTB0L6
                                       TO HINCL-REG-SIST    OF GFCTB0L4
           PERFORM 2131-ACESSAR-GFCTB0L4

           IF  SQLCODE              EQUAL ZEROS
               ADD 1                   TO IND-1
                                          WRK-WAGV-QTDE-TOT
                                          WRK-WAGV-QTDE-OCOR
               MOVE WRK-WAGV-QTDE-TOT  TO WAGV-QTDE-TOT-REG
               MOVE WRK-WAGV-QTDE-OCOR TO WAGV-QTDE-OCOR
               ADD 158                 TO WAGV-LL
               MOVE CAGPTO-CTA         OF GFCTB0L6
                                       TO WRK-AUX-3-S
               MOVE WRK-AUX-3          TO WAGV-CAGPTO(IND-1)
               MOVE HINCL-REG-SIST     OF GFCTB0L6
                                       TO WAGV-HINCL(IND-1)
               MOVE CDEPDC OF GFCTB0L6 TO WRK-AUX-5-S
               MOVE WRK-AUX-5          TO WAGV-CAGEN(IND-1)
               MOVE CPOSTO-SERVC       OF GFCTB0L6
                                       TO WRK-AUX-5-S
               MOVE WRK-AUX-5          TO WAGV-CPAB(IND-1)

               MOVE HINCL-REG-SIST     OF GFCTB0L6
                                       TO WRK-HINCL
               MOVE WRK-ANO            TO WRK-ANO-AUX
               MOVE WRK-MES            TO WRK-MES-AUX
               MOVE WRK-DIA            TO WRK-DIA-AUX
               MOVE WRK-DATA-AUX       TO WAGV-DT-SOLC(IND-1)

               PERFORM 2331-OBTER-DESC-CBANC

           END-IF.

           PERFORM 2320-LER-CSR03-GFCTB0L6

           IF  IND-1                 EQUAL 5
               IF  WRK-FINALIZAR NOT EQUAL 'S'
                   MOVE 'S'            TO WRK-FINALIZAR
                   MOVE CAGPTO-CTA     OF GFCTB0L6
                                       TO WRK-AUX-3-S
                   MOVE WRK-AUX-3      TO WAGV-FTR-CAGPTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTEM DESCRICAO DO CORRESP. BANCARIO                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2331-OBTER-DESC-CBANC           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE WAGU-TRANSACAO         TO GFCTGB-TRANSACAO.
           MOVE WAGU-FUNCAO            TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE WAGU-FUNC-BDSCO        TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.

           MOVE CDEPDC OF GFCTB0L6     TO WRK-AUX-5-S
           MOVE WRK-AUX-5              TO GFCTGB-COD-DEPDC
           MOVE CPOSTO-SERVC           OF GFCTB0L6
                                       TO WRK-AUX-5-S
           MOVE WRK-AUX-5              TO GFCTGB-COD-POSTO

           MOVE SPACES                 TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS

           INITIALIZE                  GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5090'             TO WRK-MODULO.

           CALL  WRK-MODULO         USING GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE 9                  TO WAGV-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0551'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
           END-IF

           IF  GFCTGC-ERRO            NOT EQUAL ZEROS
               MOVE 'S'                TO WAGV-FIM
               MOVE GFCTGC-ERRO        TO WAGV-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO WAGV-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO WAGV-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO WAGV-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5090-'
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO WAGV-DESC-MSG-ERRO
               END-IF

               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTGC-DESC-POSTO(1)   TO WAGV-NM-PAB(IND-1).

      *----------------------------------------------------------------*
       2331-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  FECHA CURSOR CSR03                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2340-FECHAR-CSR03-GFCTB0L6      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR03-GFCTB0L6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TAUTRZ_ENVIO_POSTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAGV-COD-SQL-ERRO
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE 0011               TO WAGV-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L6'         TO WRK-NOME-TAB
               PERFORM 2111-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FINALIZA O PROCESSAMENTO E RETORNA AO PROGRAMA CHAMADOR.       *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
