      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT5302.
       AUTHOR.     SONDAPROCWORK.

      *================================================================*
      *                   S O N D A   P R O C W O R K                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5302                                    *
      *    ANALISTA....:   FRANCISCO LOURENCO         - SONDAPROCWORK  *
      *    DATA........:   NOV/2010                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR DADOS PARA A LISTA DE CONSULTA DE     *
      *                    IMPEDIMENTOS/RESTRICOES                     *
      *                                                                *
      *----------------------------------------------------------------*
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.TCTRL_NGTVC_CLI                 GFCTB0N2          *
      *                                                                *
      *----------------------------------------------------------------*
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5522 - VERIFICACAO DE DISPONIBILIDADE ON-LINE         *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      BRAD1470 - CONSISTENCIA DE DATAS                          *
      *                                                                *
      *----------------------------------------------------------------*
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7986                                     *
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
       01  FILLER                      PIC  X(050)         VALUE
           '*  INICIO DA WORKING GFCT5302  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       01  IND-1                       PIC  9(005) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  WRK-DESPREZA                PIC  9(005)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-FINALIZAR               PIC  X(001)         VALUE SPACES.

       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       01  WRK-TABELA                  PIC  X(008)         VALUE SPACES.

       01  WRK-WAPF-PTR-REG-DESPREZA   PIC  9(005) COMP-3  VALUE ZEROS.

       01  WRK-WAPF-QTDE-TOT-REG       PIC  9(006) COMP-3  VALUE ZEROS.

       01  WRK-CAG-DEB-MOVTO           PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CAG-DEB-MOVTO.
           05  WRK-COD-AGENCIA         PIC  9(005).

       01  WRK-CCTA-DEB-MOVTO          PIC S9(013)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CCTA-DEB-MOVTO.
           05  WRK-COD-CONTA           PIC  9(013).

ST25X6* 01  WRK-CCNPJ-CPF-DEB           PIC S9(009)         VALUE ZEROS.
ST25X6* 01  FILLER                      REDEFINES WRK-CCNPJ-CPF-DEB.
ST25X6*     05  WRK-COD-CNPJ-CPF        PIC  9(009).
ST25X6  01  WRK-CCNPJ-CPF-DEB           PIC  X(009)        VALUE SPACES.
ST25X6  01  FILLER                      REDEFINES WRK-CCNPJ-CPF-DEB.
ST25X6      05  WRK-COD-CNPJ-CPF        PIC  X(009).

ST25X6* 01  WRK-CFLIAL-CNPJ-DEB         PIC S9(005)         VALUE ZEROS.
ST25X6* 01  FILLER                      REDEFINES WRK-CFLIAL-CNPJ-DEB.
ST25X6*     05  FILLER                  PIC  9(001).
ST25X6*     05  WRK-COD-FILIAL-CNPJ     PIC  9(004).
ST25X6  01  WRK-CFLIAL-CNPJ-DEB         PIC  X(004)        VALUE SPACES.
ST25X6  01  FILLER                      REDEFINES WRK-CFLIAL-CNPJ-DEB.
ST25X6      05  WRK-COD-FILIAL-CNPJ     PIC  X(004).

       01  WRK-CCTRL-CNPJ-CPF-DEB      PIC S9(002)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CCTRL-CNPJ-CPF-DEB.
           05  WRK-CONTROLE-CNPJ-CPF   PIC  9(002).

       01  WRK-SQLCODE                 PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-SQLCODE.
           05  FILLER                  PIC  X(006).
           05  WRK-COD-SQL             PIC  9(003).

       01  WRK-DATA-DB2                PIC  X(010)         VALUE
           '01.01.0001'.
       01  FILLER                      REDEFINES WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-MES-DB2             PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-ANO-DB2             PIC  9(004).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MENSAGEM ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG01.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO '.
           05  WRK-MSG01-MODULO        PIC  X(008)         VALUE SPACES.

       01  WRK-MSG02                   PIC  X(075)         VALUE
           'SISTEMA INDISPONIVEL'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*   AREA DA BRAD1470    *'.
      *----------------------------------------------------------------*

       01  WRK-1470-DATA               PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-1470-DATA.
           05  WRK-1470-DIA            PIC  9(002).
           05  WRK-1470-MES            PIC  9(002).
           05  WRK-1470-ANO            PIC  9(004).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0N2
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*  CURSORES  *'.
      *----------------------------------------------------------------*

ST25X6      EXEC SQL
ST25X6       DECLARE
ST25X6                CSR01-GFCTB0N2 CURSOR FOR
ST25X6        SELECT
ST25X6                CBCO_DEB_MOVTO,
ST25X6                CAG_DEB_MOVTO,
ST25X6                CCTA_DEB_MOVTO,
ST25X6                DCONS_NGTVC_CLI,
ST25X6                CCNPJ_CPF_DEB,
ST25X6                CFLIAL_CNPJ_DEB,
ST25X6                CCTRL_CNPJ_CPF_DEB,
ST25X6                CCNPJ_CPF_DEB_ST,
ST25X6                CFLIAL_CNPJ_DEB_ST,
ST25X6                CCTRL_CNPJ_CPF_DEB_ST,
ST25X6                RRESUL_CONS_NGTVC
ST25X6          FROM
ST25X6                DB2PRD.TCTRL_NGTVC_CLI
ST25X6         WHERE
ST25X6                CROTNA_ORIGE_MOVTO
ST25X6                                   = :GFCTB0N2.CROTNA-ORIGE-MOVTO
ST25X6           AND  DENVIO_MOVTO_TARIF
ST25X6                                   = :GFCTB0N2.DENVIO-MOVTO-TARIF
ST25X6           AND  CNRO_ARQ_MOVTO     = :GFCTB0N2.CNRO-ARQ-MOVTO
ST25X6           AND  CSEQ_MOVTO         = :GFCTB0N2.CSEQ-MOVTO
ST25X6      ORDER BY
ST25X6                DCONS_NGTVC_CLI,
ST25X6                CBCO_DEB_MOVTO,
ST25X6                CAG_DEB_MOVTO,
ST25X6                CCTA_DEB_MOVTO
ST25X6      END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*    FIM DA WORKING GFCT5302   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'GFCTWAPE'.
       COPY 'GFCTWAPF'.
       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING WAPE-ENTRADA
                                             WAPF-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      * ROTINA PARA CONTROLAR O PROCESSAMENTO                          *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-CONTROLAR-PROCESSAMENTO    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR-PROCESSAMENTO.

           PERFORM 2000-EXECUTAR-PROCESSAMENTO.

           PERFORM 9999-FINALIZAR-PROCESSAMENTO.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA INICIALIZAR O PROCESSAMENTO                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR-PROCESSAMENTO  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-VERIFICAR-ONLINE.

           PERFORM 1200-INICIALIZAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA VERIFICAR DISPONIBILIDADE ONLINE DO SISTEMA       *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-ONLINE           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE +101                   TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE WAPE-TRANSACAO         TO GFCTFZ-TRANSACAO.
           MOVE WAPE-FUNCAO            TO GFCTFZ-FUNCAO.
           MOVE WAPE-FUNC-BDSCO        TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE SPACES             TO WAPF-DESC-MSG-ERRO
               MOVE 9                  TO WAPF-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG01-MODULO
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5302'         TO GFCT0M-TRANSACAO

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

           IF  GFCTG1-ERRO             NOT EQUAL ZEROS
               MOVE 9                  TO WAPF-ERRO

               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE SPACES         TO WAPF-DESC-MSG-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO WAPF-COD-SQL-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO WAPF-DESC-MSG-ERRO
               END-IF

               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT5302'         TO GFCT0M-TRANSACAO

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO WAPF-ERRO
               MOVE SPACES             TO WAPF-DESC-MSG-ERRO
               MOVE WRK-MSG02          TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5302'         TO GFCT0M-TRANSACAO

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA INICIALIZAR A AREA DE SAIDA                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-INICIALIZAR-AREA-SAIDA     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WAPF-SAIDA.

           INITIALIZE                  WAPF-SAIDA.

           MOVE +186                   TO WAPF-LL.
           MOVE WAPE-ZZ                TO WAPF-ZZ.
           MOVE WAPE-TRANSACAO         TO WAPF-TRANSACAO.
           MOVE WAPE-FUNCAO            TO WAPF-FUNCAO.
           MOVE WAPE-FILTRO            TO WAPF-FILTRO.
           MOVE WAPE-PONTEIRO          TO WAPF-PONTEIRO.
           MOVE WAPE-QTDE-OCOR         TO WAPF-QTDE-OCOR.
           MOVE WAPE-QTDE-TOT-REG      TO WAPF-QTDE-TOT-REG.
           MOVE WAPE-FUNC-BDSCO        TO WAPF-FUNC-BDSCO.

           MOVE 'N'                    TO WAPF-FIM.

           MOVE ZEROS                  TO WAPF-ERRO
                                          WAPF-COD-MSG-ERRO
                                          WAPF-COD-SQL-ERRO.

           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 9990-OBTER-DESCRICAO-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO WAPF-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA CONSISTIR DADOS RECEBIDOS                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (WAPE-TRANSACAO          EQUAL SPACES OR LOW-VALUES) OR
              (WAPE-FUNCAO             EQUAL SPACES OR LOW-VALUES) OR
              (WAPE-FLT-CROTNA-ORIGE-MOVTO
                                       EQUAL SPACES OR LOW-VALUES) OR
              (WAPE-FLT-DENVIO-MOVTO-TARIF(01:02)
                                       NOT NUMERIC)                OR
              (WAPE-FLT-DENVIO-MOVTO-TARIF(04:02)
                                       NOT NUMERIC)                OR
              (WAPE-FLT-DENVIO-MOVTO-TARIF(07:04)
                                       NOT NUMERIC)                OR
              (WAPE-FLT-CNRO-ARQ-MOVTO NOT NUMERIC)                OR
              (WAPE-FLT-CNRO-ARQ-MOVTO EQUAL ZEROS)                OR
              (WAPE-FLT-CSEQ-MOVTO     NOT NUMERIC)                OR
              (WAPE-FLT-CSEQ-MOVTO     EQUAL ZEROS)                OR
              (WAPE-PTR-REG-DESPREZA   NOT NUMERIC)                OR
              (WAPE-QTDE-OCOR          NOT NUMERIC)                OR
              (WAPE-QTDE-TOT-REG       NOT NUMERIC)                OR
              (WAPE-FUNC-BDSCO         EQUAL SPACES OR LOW-VALUES) OR
              (WAPE-FIM                NOT EQUAL 'S' AND 'N')
               MOVE 'S'                TO WAPF-FIM

               MOVE 1                  TO WAPF-ERRO
               MOVE ZEROS              TO WAPF-COD-SQL-ERRO
               MOVE 0001               TO WAPF-COD-MSG-ERRO

               MOVE '0001'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAPF-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

           MOVE WAPE-FLT-DENVIO-MOVTO-TARIF
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-1470-DIA.
           MOVE WRK-MES-DB2            TO WRK-1470-MES.
           MOVE WRK-ANO-DB2            TO WRK-1470-ANO.

           PERFORM 1310-CONSISTIR-DATA.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA CONSISTIR DATAS RECEBIDAS                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-CONSISTIR-DATA             SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1470'             USING WRK-1470-DATA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'S'                TO WAPF-FIM

               MOVE 1                  TO WAPF-ERRO
               MOVE ZEROS              TO WAPF-COD-SQL-ERRO
               MOVE 1282               TO WAPF-COD-MSG-ERRO

               MOVE '1282'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAPF-DESC-MSG-ERRO
               MOVE ZEROS              TO RETURN-CODE

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA EXECUTAR O PROCESSAMENTO                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-EXECUTAR-PROCESSAMENTO     SECTION.
      *----------------------------------------------------------------*

           MOVE WAPE-FLT-CROTNA-ORIGE-MOVTO
                                       TO CROTNA-ORIGE-MOVTO
                                                            OF GFCTB0N2.
           MOVE WAPE-FLT-DENVIO-MOVTO-TARIF
                                       TO DENVIO-MOVTO-TARIF
                                                            OF GFCTB0N2.

           MOVE WAPE-FLT-CNRO-ARQ-MOVTO
                                       TO CNRO-ARQ-MOVTO    OF GFCTB0N2.

           MOVE WAPE-FLT-CSEQ-MOVTO    TO CSEQ-MOVTO        OF GFCTB0N2.

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 2300-ABRIR-CSR-GFCTB0N2.

           MOVE WAPE-PTR-REG-DESPREZA  TO WRK-DESPREZA.

           PERFORM 2400-LER-CSR-GFCTB0N2.

           PERFORM 2400-LER-CSR-GFCTB0N2
           VARYING IND-1               FROM 1 BY 1
             UNTIL IND-1               GREATER WRK-DESPREZA
                OR WRK-FINALIZAR       EQUAL 'S'.

           IF  WRK-FINALIZAR           EQUAL 'S'
               IF  WAPF-PTR-REG-DESPREZA
                                       EQUAL ZEROS
                   MOVE 'S'            TO WAPF-FIM

                   MOVE 1              TO WAPF-ERRO
                   MOVE ZEROS          TO WAPF-COD-SQL-ERRO

                   MOVE 1817           TO WAPF-COD-MSG-ERRO

                   MOVE '1817'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAPF-DESC-MSG-ERRO
               END-IF
           END-IF.

           MOVE WAPF-PTR-REG-DESPREZA  TO WRK-WAPF-PTR-REG-DESPREZA.
           MOVE WAPF-QTDE-TOT-REG      TO WRK-WAPF-QTDE-TOT-REG.

           MOVE ZEROS                  TO IND-1.

           PERFORM 2200-MONTAR-OCORRENCIA-SAIDA
             UNTIL WRK-FINALIZAR       EQUAL 'S'.

           PERFORM 2500-FECHAR-CSR-GFCTB0N2.

           MOVE WRK-WAPF-PTR-REG-DESPREZA
                                       TO WAPF-PTR-REG-DESPREZA.
           MOVE WRK-WAPF-QTDE-TOT-REG  TO WAPF-QTDE-TOT-REG.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA MONTAR DADOS OCORRENCIA DE SAIDA                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-MONTAR-OCORRENCIA-SAIDA    SECTION.
      *----------------------------------------------------------------*

           ADD +103                    TO WAPF-LL.

           ADD  1                      TO WRK-WAPF-PTR-REG-DESPREZA.

           ADD  1                      TO WRK-WAPF-QTDE-TOT-REG.

           ADD  1                      TO IND-1.

           MOVE DCONS-NGTVC-CLI OF GFCTB0N2
                                       TO WAPF-DATA-CONS(IND-1).

           MOVE CAG-DEB-MOVTO OF GFCTB0N2
                                       TO WRK-CAG-DEB-MOVTO.
           MOVE WRK-COD-AGENCIA        TO WAPF-AGENCIA-CONS(IND-1).

           MOVE CCTA-DEB-MOVTO OF GFCTB0N2
                                       TO WRK-CCTA-DEB-MOVTO.
           MOVE WRK-COD-CONTA          TO WAPF-CONTA-CONS(IND-1).

ST25X6*     MOVE CCNPJ-CPF-DEB OF GFCTB0N2
ST25X6*                                 TO WRK-CCNPJ-CPF-DEB.
ST25X6*     MOVE WRK-COD-CNPJ-CPF       TO WAPF-CNPJ-CPF-CONS(IND-1).
ST25X6      MOVE CCNPJ-CPF-DEB-ST OF GFCTB0N2
ST25X6                                 TO WRK-CCNPJ-CPF-DEB.
ST25X6      MOVE WRK-COD-CNPJ-CPF       TO WAPF-CNPJ-CPF-CONS(IND-1).

ST25X6*     MOVE CFLIAL-CNPJ-DEB OF GFCTB0N2
ST25X6*                                 TO WRK-CFLIAL-CNPJ-DEB.
ST25X6*     MOVE WRK-COD-FILIAL-CNPJ    TO WAPF-FILIAL-CNPJ-CONS(IND-1).
ST25X6      MOVE CFLIAL-CNPJ-DEB-ST OF GFCTB0N2
ST25X6                                 TO WRK-CFLIAL-CNPJ-DEB.
ST25X6      MOVE WRK-COD-FILIAL-CNPJ    TO WAPF-FILIAL-CNPJ-CONS(IND-1).

ST25X6*     MOVE CCTRL-CNPJ-CPF-DEB OF GFCTB0N2
ST25X6*                                 TO WRK-CCTRL-CNPJ-CPF-DEB.
ST25X6*     MOVE WRK-CONTROLE-CNPJ-CPF  TO WAPF-CTRL-CNPJ-CPF-CONS
ST25X6*                                                          (IND-1).
ST25X6      MOVE CCTRL-CNPJ-CPF-DEB-ST OF GFCTB0N2
ST25X6                                 TO WRK-CCTRL-CNPJ-CPF-DEB.
ST25X6      MOVE WRK-CONTROLE-CNPJ-CPF  TO WAPF-CTRL-CNPJ-CPF-CONS
ST25X6                                                          (IND-1).

           MOVE RRESUL-CONS-NGTVC OF GFCTB0N2
                                       TO WAPF-RESULTADO-CONS(IND-1).

           PERFORM 2400-LER-CSR-GFCTB0N2.

           IF  IND-1                   GREATER 7
               MOVE 'S'                TO WRK-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA ABRIR O CURSOR                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-ABRIR-CSR-GFCTB0N2              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN
                     CSR01-GFCTB0N2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'TCTRL_NGTVC_CLI'  TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0001'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0005               TO WAPF-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAPF-COD-SQL-ERRO

               MOVE '0005'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0N2'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

           MOVE ZEROS                  TO WRK-DESPREZA.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA LER O CURSOR                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-LER-CSR-GFCTB0N2          SECTION.
      *----------------------------------------------------------------*

ST25X6      EXEC SQL
ST25X6         FETCH
ST25X6                CSR01-GFCTB0N2
ST25X6          INTO
ST25X6               :GFCTB0N2.CBCO-DEB-MOVTO,
ST25X6               :GFCTB0N2.CAG-DEB-MOVTO,
ST25X6               :GFCTB0N2.CCTA-DEB-MOVTO,
ST25X6               :GFCTB0N2.DCONS-NGTVC-CLI,
ST25X6               :GFCTB0N2.CCNPJ-CPF-DEB,
ST25X6               :GFCTB0N2.CFLIAL-CNPJ-DEB,
ST25X6               :GFCTB0N2.CCTRL-CNPJ-CPF-DEB,
ST25X6               :GFCTB0N2.CCNPJ-CPF-DEB-ST,
ST25X6               :GFCTB0N2.CFLIAL-CNPJ-DEB-ST,
ST25X6               :GFCTB0N2.CCTRL-CNPJ-CPF-DEB-ST,
ST25X6               :GFCTB0N2.RRESUL-CONS-NGTVC
ST25X6      END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'TCTRL_NGTVC_CLI'  TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0006               TO WAPF-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAPF-COD-SQL-ERRO

               MOVE '0006'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0N2'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WAPF-FIM,
                                          WRK-FINALIZAR

               IF  IND-1               EQUAL ZEROS
                   IF  WAPF-PTR-REG-DESPREZA
                                       EQUAL ZEROS
                       MOVE 1          TO WAPF-ERRO
                       MOVE ZEROS      TO WAPF-COD-SQL-ERRO
                       MOVE 1817       TO WAPF-COD-MSG-ERRO

                       MOVE '1817'     TO GFCTG2-COD-MSG

                       PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

                       MOVE GFCTG3-DESC-MSG
                                       TO WAPF-DESC-MSG-ERRO
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA FECHAR O CURSOR                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-FECHAR-CSR-GFCTB0N2       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE
                     CSR01-GFCTB0N2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'TCTRL_NGTVC_CLI'  TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0011               TO WAPF-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAPF-COD-SQL-ERRO

               MOVE '0011'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0N2'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA MONTAR OS DADOS DE ERROS NO ACESSO A TABELA       *
      ******************************************************************
      *----------------------------------------------------------------*
       9900-MOVER-ERRO-SQL            SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO WAPF-FIM.

           MOVE 9                      TO WAPF-ERRO.
           MOVE 'GFCT5302'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.

           PERFORM 9990-OBTER-DESCRICAO-MENSAGEM.

           STRING GFCTG3-DESC-MSG ' - ' WRK-TABELA
           DELIMITED BY '  '           INTO WAPF-DESC-MSG-ERRO.

           PERFORM 9999-FINALIZAR-PROCESSAMENTO.

      *----------------------------------------------------------------*
       9900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA CONSISTIR DADOS RECEBIDOS                         *
      ******************************************************************
      *----------------------------------------------------------------*
       9990-OBTER-DESCRICAO-MENSAGEM   SECTION.
      *----------------------------------------------------------------*

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE WAPE-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE WAPE-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE WAPE-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE                  GFCTG3-SAIDA.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO WAPF-ERRO

               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG01-MODULO

               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5302'         TO GFCT0M-TRANSACAO
               MOVE ZEROS              TO RETURN-CODE

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE GFCTG3-ERRO        TO WAPF-ERRO

               MOVE 'GFCT5302'         TO GFCT0M-TRANSACAO

               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE GFCTG3-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
               END-IF

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO WAPF-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO WAPF-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO WAPF-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO WAPF-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

      *----------------------------------------------------------------*
       9990-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA FINALIZAR O PROCESSAMENTO                         *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-FINALIZAR-PROCESSAMENTO    SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
