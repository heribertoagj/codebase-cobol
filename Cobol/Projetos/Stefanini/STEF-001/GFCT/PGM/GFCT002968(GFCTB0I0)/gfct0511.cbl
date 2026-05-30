      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0511.
       AUTHOR.     SONDA PROCWORK.
      *================================================================*
      *      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT0511                                     *
      *    ANALISTA     : MARIA VALERIA DE S TORQUATO - SONDA PROCWORK *
      *    PROGRAMADOR  : FRANCISCO LOURENCO          - SONDA PROCWORK *
      *    DATA         : NOV/2008                                     *
      *                                                                *
      *    OBJETIVO     : LISTA DO AGENCIA E CONTA PERTECENTES A UM    *
      *                   CPF/CNPJ                                     *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *         DB2PRD.V01CLIENTE_AGENCIA            CLIEV004          *
      *         DB2PRD.V01CTA_POUPANCA               CLIEV007          *
      *         DB2PRD.V01CTA_CORRENTE               CLIEV008          *
      *         DB2PRD.TEXTRT_ANO_TARIF              GFCTB0K5          *
      *                                                                *
      *    BOOK'S     :                                                *
      *      GFCTWABT - ENTRADA - LISTA DE AGENCIA E CONTA             *
      *      GFCTWABU - SAIDA   - LISTA DE AGENCIA E CONTA             *
      *      I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE *
      *      I#GFCTG1 - SAIDA   - VERIFICAR DISPONIBILIDADE DO ON-LINE *
      *      I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM          *
      *      I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM          *
      *      I#GFCT0M - SAIDA   - AREA DE COMUNICACAO DE ERROS         *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5505 - IDENTIFICA DEPARTAMENTO DO USUARIO             *
      *      GFCT5507 - IDENTIFICA DEPENDENCIA DO USUARIO              *
      *      GFCT5522 - VERIFICA ON-LINE                               *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *                                                                *
      *================================================================*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  * ***  HEXASOLUTION - 11/2009 - CONVERSAO FAIXA DE AGENCIAS  *** *
HEXA  *----------------------------------------------------------------*
HEXA  *

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
       01  FILLER                      PIC  X(032)         VALUE
           '*  INICIO DA WORKING GFCT0511  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       01  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

       01  IND-TEM-EXTRATO-ANUAL       PIC  X(001)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       01  WRK-SQLCODE                 PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-SQLCODE.
           05  FILLER                  PIC  9(006).
           05  WRK-COD-SQL             PIC  9(003).

       01  WRK-FIM-CURSOR-V004         PIC  X(001)         VALUE 'N'.
       01  WRK-FIM-CURSOR-V007         PIC  X(001)         VALUE 'N'.
       01  WRK-FIM-CURSOR-V008         PIC  X(001)         VALUE 'N'.

       01  WRK-QTDE-OCOR               PIC  9(003) COMP-3  VALUE ZEROS.

       01  WRK-QTDE-TOT-REG            PIC  9(006) COMP-3  VALUE ZEROS.

       01  WRK-GFCTWABT-CONTR-FIL      PIC  X(002)         VALUE '00'.
       01  FILLER                      REDEFINES WRK-GFCTWABT-CONTR-FIL.
           05  WRK-CONTR-CGC-CPF       PIC  9(002).

       01  WRK-CID-CLI                 PIC  X(026)         VALUE SPACES.

       01  WRK-CJUNC-DEPDC-INI         PIC S9(005) COMP-3  VALUE ZEROS.
       01  WRK-CJUNC-DEPDC-FIM         PIC S9(005) COMP-3  VALUE ZEROS.

       01  WRK-IPSSOA-COPLT            PIC  X(070)         VALUE SPACES.
       01  FILLER                      REDEFINES WRK-IPSSOA-COPLT.
           05  WRK-NOME-CLIENTE        PIC  X(040).
           05  FILLER                  PIC  X(030).

       01  WRK-CJUNC-DEPDC             PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CJUNC-DEPDC.
           05  WRK-COD-AGENCIA         PIC  9(005).

       01  WRK-CCTA-CLI                PIC S9(013)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CCTA-CLI.
           05  WRK-COD-CONTA           PIC  9(013).
HEXA  *
HEXA   77  WRK-TIPO-DEPDC-FIL          PIC  X(02)          VALUE SPACES.
HEXA  *

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*     AREA AUX. DE ERRO DB2    *'.
      *----------------------------------------------------------------*

       01  WRK-COD-TABELA              PIC  X(008)         VALUE SPACES.
       01  WRK-NOME-TABELA             PIC  X(018)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*     AREA DE MENSAGENS        *'.
      *----------------------------------------------------------------*

       01  WRK-MSG01.
           05  FILLER                  PIC  X(026)         VALUE
           'SISTEMA INDISPONIVEL'.
           05  FILLER                  PIC  X(049)         VALUE SPACES.

       01  WRK-MSG05.
           05  FILLER                  PIC  X(026)         VALUE
           'ERRO NA CHAMADA DO MODULO '.
           05  WRK-NOME-MOD            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(041)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*  AREA PARA MODULO GFCT5505   *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-5505.
           03  WRK-5505-AREA-ENTRADA.
               05  WRK-5505-AMBIENTE   PIC  X(001)         VALUE SPACES.
               05  WRK-5505-FUNC-BDSCO PIC  X(007)         VALUE SPACES.
           03  WRK-5505-AREA-SAIDA.
               05  WRK-5505-COD-RETORNO
                                       PIC  9(002)         VALUE ZEROS.
               05  WRK-5505-COD-SQL-ERRO
                                       PIC  9(003)         VALUE ZEROS.
               05  WRK-5505-DESC-MSG   PIC  X(070)         VALUE SPACES.
               05  WRK-5505-DADOS-RETORNO.
                   07  WRK-5505-COD-JUNCAO
                                       PIC  9(005)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*  AREA PARA MODULO GFCT5507   *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-5507.
           03  WRK-5507-AREA-ENTRADA.
               05  WRK-5507-AMBIENTE   PIC X(01)           VALUE SPACES.
               05  WRK-5507-CJUNC-DEPDC
                                       PIC 9(05)           VALUE ZEROS.
           03  WRK-5507-AREA-SAIDA.
               05  WRK-5507-COD-RETORNO
                                       PIC 9(02)           VALUE ZEROS.
               05  WRK-5507-COD-SQL-ERRO
                                       PIC 9(03)           VALUE ZEROS.
               05  WRK-5507-MSG-RETORNO
                                       PIC X(45)           VALUE SPACES.
               05  WRK-5507-IDTFD-MASTER
                                       PIC X(01)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*    AREA DO MODULO GFCT5522   *'.
      *----------------------------------------------------------------*

       COPY I#GFCTFZ.

       COPY I#GFCTG1.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*    AREA DO MODULO GFCT5523   *'.
      *----------------------------------------------------------------*

       COPY I#GFCTG2.

       COPY I#GFCTG3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV004
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV007
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV008
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0K5
           END-EXEC.

           EXEC SQL
            DECLARE  CSR01-CLIEV004 CURSOR FOR
             SELECT
                     CID_CLI     ,
                     IPSSOA_COPLT
               FROM
                     DB2PRD.V01CLIENTE_AGENCIA
              WHERE
                     CCGC_CPF           = :CLIEV004.CCGC-CPF
                AND  CFLIAL_CGC         = :CLIEV004.CFLIAL-CGC
                AND  CCTRL_CPF_CGC      = :CLIEV004.CCTRL-CPF-CGC
              ORDER BY
                     CID_CLI DESC
           END-EXEC.

HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_DB2
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  * HX_EXEC SQL
HEXA  *  HX_DECLARE  CSR02-CLIEV008 CURSOR FOR
HEXA  *   HX_SELECT
HEXA  *              CJUNC_DEPDC,
HEXA  *              CCTA_CLI   ,
HEXA  *              ICTA_MOVTC
HEXA  *        FROM
HEXA  *              DB2PRD.V01CTA_CORRENTE
HEXA  *       WHERE
HEXA  *              CID_CLI            = :CLIEV008.CID-CLI
HEXA  *         AND (CJUNC_DEPDC  BETWEEN :WRK-CJUNC-DEPDC-INI
HEXA  *                               AND :WRK-CJUNC-DEPDC-FIM)
HEXA  *       ORDER BY
HEXA  *              CJUNC_DEPDC,
HEXA  *              CCTA_CLI
HEXA  *    END-EXEC.
HEXA  *
HEXA       EXEC SQL
HEXA        DECLARE  CSR02-CLIEV008 CURSOR FOR
HEXA         SELECT
HEXA                 CJUNC_DEPDC,
HEXA                 CCTA_CLI   ,
HEXA                 ICTA_MOVTC
HEXA           FROM
HEXA                 DB2PRD.V01CTA_CORRENTE
HEXA          WHERE
HEXA                 CID_CLI            = :CLIEV008.CID-CLI
HEXA            AND ((CJUNC_DEPDC  BETWEEN :WRK-CJUNC-DEPDC-INI
HEXA                                  AND :WRK-CJUNC-DEPDC-FIM
HEXA            AND :WRK-TIPO-DEPDC-FIL <> 'AG')
HEXA            OR  (CJUNC_DEPDC  BETWEEN :WRK-CJUNC-DEPDC-INI
HEXA                                  AND :WRK-CJUNC-DEPDC-FIM
HEXA            AND  CJUNC_DEPDC       IN
HEXA                (SELECT MESUV010A.CDEPDC_B10
HEXA                 FROM   DB2PRD.V10DEPDC_BDSCO_ONL AS MESUV010A
HEXA                 WHERE  MESUV010A.CEMPR_INC_B10 = 237
HEXA            AND         MESUV010A.CNATUR_DEPDC_B10 IN ('A','T'))
HEXA            AND :WRK-TIPO-DEPDC-FIL = 'AG'))
HEXA          ORDER BY
HEXA                 CJUNC_DEPDC,
HEXA                 CCTA_CLI
HEXA       END-EXEC.
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_DB2
HEXA  *----------------------------------------------------------------*
HEXA  *

HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_DB2
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  * HX_EXEC SQL
HEXA  *  HX_DECLARE  CSR03-CLIEV007 CURSOR FOR
HEXA  *   HX_SELECT
HEXA  *              CJUNC_DEPDC,
HEXA  *              CCTA_CLI   ,
HEXA  *              ICTA_MOVTC
HEXA  *        FROM
HEXA  *              DB2PRD.V01CTA_POUPANCA
HEXA  *       WHERE
HEXA  *              CID_CLI            = :CLIEV007.CID-CLI
HEXA  *         AND (CJUNC_DEPDC  BETWEEN :WRK-CJUNC-DEPDC-INI
HEXA  *                               AND :WRK-CJUNC-DEPDC-FIM)
HEXA  *       ORDER BY
HEXA  *              CJUNC_DEPDC,
HEXA  *              CCTA_CLI
HEXA  *    END-EXEC.
HEXA  *
HEXA       EXEC SQL
HEXA        DECLARE  CSR03-CLIEV007 CURSOR FOR
HEXA         SELECT
HEXA                 CJUNC_DEPDC,
HEXA                 CCTA_CLI   ,
HEXA                 ICTA_MOVTC
HEXA           FROM
HEXA                 DB2PRD.V01CTA_POUPANCA
HEXA          WHERE
HEXA                 CID_CLI            = :CLIEV007.CID-CLI
HEXA            AND ((CJUNC_DEPDC  BETWEEN :WRK-CJUNC-DEPDC-INI
HEXA                                  AND :WRK-CJUNC-DEPDC-FIM
HEXA            AND :WRK-TIPO-DEPDC-FIL <> 'AG')
HEXA            OR  (CJUNC_DEPDC  BETWEEN :WRK-CJUNC-DEPDC-INI
HEXA                                  AND :WRK-CJUNC-DEPDC-FIM
HEXA            AND  CJUNC_DEPDC       IN
HEXA                (SELECT MESUV010A.CDEPDC_B10
HEXA                 FROM   DB2PRD.V10DEPDC_BDSCO_ONL AS MESUV010A
HEXA                 WHERE  MESUV010A.CEMPR_INC_B10 = 237
HEXA            AND         MESUV010A.CNATUR_DEPDC_B10 IN ('A','T'))
HEXA            AND :WRK-TIPO-DEPDC-FIL = 'AG'))
HEXA          ORDER BY
HEXA                 CJUNC_DEPDC,
HEXA                 CCTA_CLI
HEXA       END-EXEC.
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_DB2
HEXA  *----------------------------------------------------------------*
HEXA  *

HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE DECLARACAO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       COPY I#HEXA01.
HEXA  *
HEXA       COPY I#BRAD7C.
HEXA  *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*    FIM DA WORKING GFCT0511   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY GFCTWABT.

       COPY GFCTWABU.

       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTWABT-ENTRADA
                                             GFCTWABU-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
      * ROTINA PRINCIPAL                                               *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*
HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE MOVIMENTACAO DA ROTINA MESU E DE ABEND                *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       MOVE        'MESU9410'  TO          WRK-MODULO-MESUX0.
HEXA       MOVE        'BRAD7100'  TO          WRK-ABEND-MESUX0.
HEXA  *

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 9999-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA INICIALIZAR                                             *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-MONTAR-AREA-SAIDA.

           PERFORM 1200-TRATAR-DISP-SISTEMA.

           PERFORM 1300-CONSISTIR-DADOS-ENT.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA MONTAR AREA DE SAIDA                               *
      *----------------------------------------------------------------*
       1100-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTWABU-SAIDA

           INITIALIZE                  GFCTWABU-SAIDA

           MOVE +251                   TO GFCTWABU-LL.
           MOVE ZEROS                  TO GFCTWABU-ZZ.
           MOVE GFCTWABT-TRANSACAO     TO GFCTWABU-TRANSACAO.
           MOVE GFCTWABT-FUNCAO        TO GFCTWABU-FUNCAO.
           MOVE GFCTWABT-QTDE-OCOR     TO GFCTWABU-QTDE-OCOR.
           MOVE GFCTWABT-QTDE-TOT-REG  TO GFCTWABU-QTDE-TOT-REG.
           MOVE GFCTWABT-FUNC-BDSCO    TO GFCTWABU-FUNC-BDSCO.
           MOVE GFCTWABT-FILTRO        TO GFCTWABU-FILTRO.
           MOVE GFCTWABT-PONTEIRO      TO GFCTWABU-PONTEIRO.

           MOVE ZEROS                  TO GFCTWABU-ERRO
                                          GFCTWABU-COD-MSG-ERRO
                                          GFCTWABU-COD-SQL-ERRO.

           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO GFCTWABU-DESC-MSG-ERRO.

           MOVE 'N'                    TO GFCTWABU-FIM.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA VERIFICAR DISPONIBILIDADE DO SISTEMA               *
      *----------------------------------------------------------------*
       1200-TRATAR-DISP-SISTEMA        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA
                                          GFCTFZ-ENTRADA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTG1-SAIDA
                                       GFCTFZ-ENTRADA
                                       GFCT0M-AREA-ERROS.

           MOVE +100                   TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE GFCTWABT-TRANSACAO     TO GFCTFZ-TRANSACAO.
           MOVE GFCTWABT-FUNCAO        TO GFCTFZ-FUNCAO.
           MOVE GFCTWABT-FUNC-BDSCO    TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTWABU-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0511'         TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE GFCTG1-ERRO        TO GFCTWABU-ERRO

               IF  GFCT0M-TIPO-ACESSO  EQUAL 'DB2'
                   MOVE GFCTG1-COD-MSG-ERRO
                                       TO GFCTWABU-COD-MSG-ERRO
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO GFCTWABU-COD-SQL-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCTWABU-DESC-MSG-ERRO
               ELSE
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT0511'     TO GFCT0M-TRANSACAO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE GFCTG1-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
               END-IF

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO GFCTWABU-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0511'         TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE WRK-MSG01          TO GFCT0M-TEXTO

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA CONSISTIR CAMPOS DE ENTRADA                        *
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS-ENT        SECTION.
      *----------------------------------------------------------------*

           IF (GFCTWABT-TRANSACAO      EQUAL SPACES OR LOW-VALUES) OR
              (GFCTWABT-FUNCAO         EQUAL SPACES OR LOW-VALUES) OR
              (GFCTWABT-QTDE-OCOR      NOT NUMERIC)                OR
              (GFCTWABT-QTDE-TOT-REG   NOT NUMERIC)                OR
              (GFCTWABT-FUNC-BDSCO     EQUAL SPACES OR LOW-VALUES) OR
              (GFCTWABT-CPF-FIL        NOT NUMERIC)                OR
              (GFCTWABT-CPF-FIL        EQUAL ZEROS)                OR
              (GFCTWABT-FILIAL-FIL     NOT NUMERIC)                OR
              (GFCTWABT-CONTR-FIL      EQUAL SPACES OR LOW-VALUES) OR
              (GFCTWABT-COD-DEPTO-FIL  NOT NUMERIC)                OR
              (GFCTWABT-AGENCIA-PONT   NOT NUMERIC)                OR
              (GFCTWABT-CONTA-PONT     NOT NUMERIC)                OR
              (GFCTWABT-FIM            NOT EQUAL 'S' AND 'N')
               MOVE  1                 TO GFCTWABU-ERRO
               MOVE  ZEROS             TO GFCTWABU-COD-SQL-ERRO
               MOVE  0001              TO GFCTWABU-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG

               PERFORM 1310-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO GFCTWABU-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTWABU-FIM

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTWABT-TIPO-DEPDC-FIL EQUAL SPACES
               PERFORM 1320-OBTER-TIPO-DEPENDENCIA
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA OBTER DESCRICAO DA MENSAGEM                        *
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE                  GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTWABT-TRANSACAO     TO GFCTG2-TRANSACAO.
           MOVE GFCTWABT-FUNCAO        TO GFCTG2-FUNCAO.
           MOVE GFCTWABT-FUNC-BDSCO    TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTWABU-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0511'         TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO

               PERFORM 9999-FINALIZAR
           END-IF

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE GFCTG3-ERRO        TO GFCTWABU-ERRO

               IF  GFCT0M-TIPO-ACESSO  EQUAL 'DB2'
                   MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTWABU-COD-MSG-ERRO
                   MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTWABU-COD-SQL-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTWABU-DESC-MSG-ERRO
               ELSE
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT0511'     TO GFCT0M-TRANSACAO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE GFCTG3-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
               END-IF

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO GFCTWABU-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTWABU-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTWABU-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTWABU-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTWABU-FIM

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE VALIDACAO DE PERMISSAO DE ACESSO DO USUARIO          *
      ******************************************************************
      *----------------------------------------------------------------*
       1320-OBTER-TIPO-DEPENDENCIA     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-AREA-5505.

           MOVE 'GFCT5505'             TO WRK-MODULO.
           MOVE 'O'                    TO WRK-5505-AMBIENTE.
           MOVE GFCTWABT-FUNC-BDSCO    TO WRK-5505-FUNC-BDSCO.

           CALL WRK-MODULO             USING WRK-5505-AREA-ENTRADA
                                             WRK-5505-AREA-SAIDA.

           IF  WRK-5505-COD-RETORNO    NOT EQUAL ZEROS
               MOVE 9                  TO GFCTWABU-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0511'         TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO

               PERFORM 9999-FINALIZAR
           END-IF.

           MOVE WRK-5505-COD-JUNCAO    TO GFCTWABT-COD-DEPTO-FIL
                                          GFCTWABU-COD-DEPTO-FIL.

HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_IF
HEXA  *----------------------------------------------------------------*
HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA
HEXA  *
HEXA  * HX_IF  WRK-5505-COD-JUNCAO     LESS 4000
HEXA  *
HEXA       MOVE WRK-5505-COD-JUNCAO    TO      WRK-MESUX0-AGENCIA
HEXA       PERFORM     9990-10-CHAMA-ROTINA-FX-AG
HEXA  *
HEXA       IF  WRK-EH-FX-AG
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_IF
HEXA  *----------------------------------------------------------------*
HEXA  *
               MOVE 'AG'               TO GFCTWABT-TIPO-DEPDC-FIL
                                          GFCTWABU-TIPO-DEPDC-FIL
           ELSE
               INITIALIZE              WRK-AREA-5507

               MOVE 'GFCT5507'         TO WRK-MODULO
               MOVE 'O'                TO WRK-5507-AMBIENTE
               MOVE WRK-5505-COD-JUNCAO
                                       TO WRK-5507-CJUNC-DEPDC

               CALL WRK-MODULO         USING WRK-AREA-5507

               IF  WRK-5507-COD-RETORNO
                                       NOT EQUAL ZEROS
                   MOVE 9              TO GFCTWABU-ERRO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT0511'     TO GFCT0M-TRANSACAO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
                   MOVE WRK-MSG05      TO GFCT0M-TEXTO

                   PERFORM 9999-FINALIZAR
               END-IF

               IF  WRK-5507-IDTFD-MASTER
                                       EQUAL 'S'
                   MOVE 'GM'           TO GFCTWABT-TIPO-DEPDC-FIL
                                          GFCTWABU-TIPO-DEPDC-FIL
               ELSE
                   MOVE 'XX'           TO GFCTWABT-TIPO-DEPDC-FIL
                                          GFCTWABU-TIPO-DEPDC-FIL
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO                                        *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CURSOR-V004.

           PERFORM 2100-ABRIR-CSR-CLIEV004.

           IF  GFCTWABT-CID-CLI-PONT   EQUAL SPACES
               PERFORM 2200-LER-CSR-CLIEV004
           ELSE
               MOVE SPACES             TO WRK-CID-CLI

               PERFORM 2200-LER-CSR-CLIEV004
                 UNTIL WRK-FIM-CURSOR-V004
                                       EQUAL 'S'
                    OR WRK-CID-CLI     EQUAL GFCTWABT-CID-CLI-PONT
           END-IF.

           MOVE ZEROS                  TO IND-1.

           PERFORM 2400-TRATAR-CONTA-CLIENTE
             UNTIL WRK-FIM-CURSOR-V004 EQUAL 'S'
                OR IND-1               EQUAL 11.

           IF  WRK-FIM-CURSOR-V004     EQUAL 'S'
               IF  GFCTWABU-CID-CLI-PONT
                                       EQUAL SPACES AND
                   IND-1               EQUAL ZEROS
                   MOVE  1             TO GFCTWABU-ERRO
                   MOVE  ZEROS         TO GFCTWABU-COD-SQL-ERRO
                   MOVE  1551          TO GFCTWABU-COD-MSG-ERRO
                   MOVE '1551'         TO GFCTG2-COD-MSG

                   PERFORM 1310-OBTER-DESC-MSG

                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTWABU-DESC-MSG-ERRO
                   MOVE 'S'            TO GFCTWABU-FIM

                   PERFORM 9999-FINALIZAR
               END-IF
           ELSE
               MOVE CID-CLI OF CLIEV004
                                       TO GFCTWABU-CID-CLI-PONT
           END-IF.

           PERFORM 2300-FECHAR-CSR-CLIEV004.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ABRIR CURSOR CLIEV004                                         *
      *----------------------------------------------------------------*
       2100-ABRIR-CSR-CLIEV004         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWABT-CPF-FIL       TO CCGC-CPF OF CLIEV004.
           MOVE GFCTWABT-FILIAL-FIL    TO CFLIAL-CGC OF CLIEV004.

           MOVE GFCTWABT-CONTR-FIL     TO WRK-GFCTWABT-CONTR-FIL.
           MOVE WRK-CONTR-CGC-CPF      TO CCTRL-CPF-CGC OF CLIEV004.

           EXEC SQL
               OPEN CSR01-CLIEV004
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'CLIEV004'         TO WRK-COD-TABELA
               MOVE 'V01CLIENTE_AGENCIA'
                                       TO WRK-NOME-TABELA
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL

               PERFORM 9000-MONTAR-AREA-DB2

               MOVE 9                  TO GFCTWABU-ERRO
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE
               MOVE WRK-COD-SQL        TO GFCTWABU-COD-SQL-ERRO
               MOVE 0005               TO GFCTWABU-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG

               PERFORM 1310-OBTER-DESC-MSG

               STRING GFCTG3-DESC-MSG  '-'
                      WRK-COD-TABELA
               DELIMITED BY SIZE       INTO GFCTWABU-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  LER CURSOR CLIEV004                                           *
      *----------------------------------------------------------------*
       2200-LER-CSR-CLIEV004          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH  CSR01-CLIEV004
               INTO
                    :CLIEV004.CID-CLI     ,
                    :CLIEV004.IPSSOA-COPLT
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'CLIEV004'         TO WRK-COD-TABELA
               MOVE 'V01CLIENTE_AGENCIA'
                                       TO WRK-NOME-TABELA
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL

               PERFORM 9000-MONTAR-AREA-DB2

               MOVE 9                  TO GFCTWABU-ERRO
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE
               MOVE WRK-COD-SQL        TO GFCTWABU-COD-SQL-ERRO
               MOVE 0006               TO GFCTWABU-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG

               PERFORM 1310-OBTER-DESC-MSG

               STRING GFCTG3-DESC-MSG  '-'
                      WRK-COD-TABELA
               DELIMITED BY SIZE       INTO GFCTWABU-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR-V004
                                          GFCTWABU-FIM
           ELSE
               MOVE CID-CLI OF CLIEV004
                                       TO WRK-CID-CLI
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  FECHAR CURSOR CLIEV004                                        *
      *----------------------------------------------------------------*
       2300-FECHAR-CSR-CLIEV004        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE CSR01-CLIEV004
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'CLIEV004'         TO WRK-COD-TABELA
               MOVE 'V01CLIENTE_AGENCIA'
                                       TO WRK-NOME-TABELA
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL

               PERFORM 9000-MONTAR-AREA-DB2

               MOVE 9                  TO GFCTWABU-ERRO
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE
               MOVE WRK-COD-SQL        TO GFCTWABU-COD-SQL-ERRO
               MOVE 0011               TO GFCTWABU-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG

               PERFORM 1310-OBTER-DESC-MSG

               STRING GFCTG3-DESC-MSG  '-'
                      WRK-COD-TABELA
               DELIMITED BY SIZE       INTO GFCTWABU-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA TRATAR AS CONTAS DO CLIENTE
      *----------------------------------------------------------------*
       2400-TRATAR-CONTA-CLIENTE       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CURSOR-V008
                                          WRK-FIM-CURSOR-V007.

           IF  GFCTWABT-TABELA-PONT    EQUAL SPACES
               MOVE 'CLIEV008'         TO GFCTWABU-TABELA-PONT

               PERFORM 2410-TRATAR-CONTA-CORRENTE

               IF  WRK-FIM-CURSOR-V008 EQUAL 'S'
                   MOVE 'CLIEV007'     TO GFCTWABU-TABELA-PONT

                   PERFORM 2420-TRATAR-CONTA-POUPANCA
               END-IF
           ELSE
               IF  GFCTWABT-TABELA-PONT
                                       EQUAL 'CLIEV008'
                   PERFORM 2410-TRATAR-CONTA-CORRENTE

                   IF  WRK-FIM-CURSOR-V008
                                       EQUAL 'S'
                       MOVE 'CLIEV007' TO GFCTWABU-TABELA-PONT

                       PERFORM 2420-TRATAR-CONTA-POUPANCA
                   END-IF
               ELSE
                   MOVE 'S'            TO WRK-FIM-CURSOR-V008

                   PERFORM 2420-TRATAR-CONTA-POUPANCA
               END-IF
           END-IF.

           IF  WRK-FIM-CURSOR-V008     EQUAL 'S' AND
               WRK-FIM-CURSOR-V007     EQUAL 'S'
               MOVE SPACES             TO GFCTWABU-TABELA-PONT

               MOVE ZEROS              TO GFCTWABU-AGENCIA-PONT
                                          GFCTWABU-CONTA-PONT

               PERFORM 2200-LER-CSR-CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA TRATAR AS CONTAS CORRENTES DO CLIENTE              *
      *----------------------------------------------------------------*
       2410-TRATAR-CONTA-CORRENTE      SECTION.
      *----------------------------------------------------------------*

           PERFORM 2411-ABRIR-CSR-CLIEV008.

           IF  GFCTWABT-AGENCIA-PONT   EQUAL ZEROS AND
               GFCTWABT-CONTA-PONT     EQUAL ZEROS
               PERFORM 2412-LER-CSR-CLIEV008
           ELSE
               MOVE ZEROS              TO WRK-COD-AGENCIA
                                          WRK-COD-CONTA

               PERFORM 2412-LER-CSR-CLIEV008
                 UNTIL WRK-FIM-CURSOR-V008
                                       EQUAL 'S'
                    OR (WRK-COD-AGENCIA
                                       EQUAL GFCTWABT-AGENCIA-PONT
                   AND  WRK-COD-CONTA  EQUAL GFCTWABT-CONTA-PONT)
           END-IF.

           PERFORM 2414-MONTAR-CONTA-CORRENTE
             UNTIL WRK-FIM-CURSOR-V008 EQUAL 'S'
                OR IND-1               EQUAL 11.

           PERFORM 2413-FECHAR-CSR-CLIEV008.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ABRIR CURSOR CLIEV008                                         *
      *----------------------------------------------------------------*
       2411-ABRIR-CSR-CLIEV008         SECTION.
      *----------------------------------------------------------------*

           MOVE CID-CLI OF CLIEV004    TO CID-CLI OF CLIEV008.

           IF  GFCTWABT-TIPO-DEPDC-FIL EQUAL 'AG'
               IF  GFCTWABT-COD-DEPTO-FIL
                                       EQUAL 00001
                   MOVE 00001          TO WRK-CJUNC-DEPDC-INI
                                          WRK-CJUNC-DEPDC-FIM
               ELSE
                   MOVE 00002          TO WRK-CJUNC-DEPDC-INI
HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_MOVE
HEXA  *----------------------------------------------------------------*
HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA
HEXA  *
HEXA  *         HX_MOVE 03999          TO WRK-CJUNC-DEPDC-FIM
HEXA  *
HEXA               MOVE WRK-ULTIMA-AG  TO WRK-CJUNC-DEPDC-FIM
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_MOVE
HEXA  *----------------------------------------------------------------*
HEXA  *
               END-IF
           ELSE
               MOVE 00001              TO WRK-CJUNC-DEPDC-INI
               MOVE 99999              TO WRK-CJUNC-DEPDC-FIM
           END-IF.

HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA
HEXA  *
HEXA       MOVE GFCTWABT-TIPO-DEPDC-FIL TO WRK-TIPO-DEPDC-FIL.
HEXA  *
           EXEC SQL
               OPEN CSR02-CLIEV008
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'CLIEV008'         TO WRK-COD-TABELA
               MOVE 'V01CTA_CORRENTE'  TO WRK-NOME-TABELA
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL

               PERFORM 9000-MONTAR-AREA-DB2

               MOVE 9                  TO GFCTWABU-ERRO
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE
               MOVE WRK-COD-SQL        TO GFCTWABU-COD-SQL-ERRO
               MOVE 0005               TO GFCTWABU-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG

               PERFORM 1310-OBTER-DESC-MSG

               STRING GFCTG3-DESC-MSG  '-'
                      WRK-COD-TABELA
               DELIMITED BY SIZE       INTO GFCTWABU-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2411-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  LER CURSOR CLIEV008                                           *
      *----------------------------------------------------------------*
       2412-LER-CSR-CLIEV008          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH  CSR02-CLIEV008
               INTO
                    :CLIEV008.CJUNC-DEPDC,
                    :CLIEV008.CCTA-CLI   ,
                    :CLIEV008.ICTA-MOVTC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'CLIEV008'         TO WRK-COD-TABELA
               MOVE 'V01CTA_CORRENTE'  TO WRK-NOME-TABELA
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL

               PERFORM 9000-MONTAR-AREA-DB2

               MOVE 9                  TO GFCTWABU-ERRO
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE
               MOVE WRK-COD-SQL        TO GFCTWABU-COD-SQL-ERRO
               MOVE 0006               TO GFCTWABU-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG

               PERFORM 1310-OBTER-DESC-MSG

               STRING GFCTG3-DESC-MSG  '-'
                      WRK-COD-TABELA
               DELIMITED BY SIZE       INTO GFCTWABU-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR-V008
           ELSE
               MOVE CJUNC-DEPDC OF CLIEV008
                                       TO WRK-CJUNC-DEPDC
               MOVE WRK-COD-AGENCIA    TO GFCTWABU-AGENCIA-PONT

               MOVE CCTA-CLI OF CLIEV008
                                       TO WRK-CCTA-CLI
               MOVE WRK-COD-CONTA      TO GFCTWABU-CONTA-PONT
           END-IF.

      *----------------------------------------------------------------*
       2412-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  FECHAR CURSOR CLIEV008                                        *
      *----------------------------------------------------------------*
       2413-FECHAR-CSR-CLIEV008        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE CSR02-CLIEV008
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'CLIEV008'         TO WRK-COD-TABELA
               MOVE 'V01CTA_CORRENTE'  TO WRK-NOME-TABELA
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL

               PERFORM 9000-MONTAR-AREA-DB2

               MOVE 9                  TO GFCTWABU-ERRO
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE
               MOVE WRK-COD-SQL        TO GFCTWABU-COD-SQL-ERRO
               MOVE 0011               TO GFCTWABU-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG

               PERFORM 1310-OBTER-DESC-MSG

               STRING GFCTG3-DESC-MSG  '-'
                      WRK-COD-TABELA
               DELIMITED BY SIZE       INTO GFCTWABU-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2413-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  MONTAR INFORMACOES DAS CONTAS CORRENTE DO CLIENTE             *
      *----------------------------------------------------------------*
       2414-MONTAR-CONTA-CORRENTE      SECTION.
      *----------------------------------------------------------------*

           PERFORM 2500-VERIFICAR-EXTRATO-CLIENTE.

           IF  IND-TEM-EXTRATO-ANUAL   EQUAL 'S'
               ADD  1                  TO IND-1

               MOVE GFCTWABU-QTDE-OCOR TO WRK-QTDE-OCOR
               ADD  1                  TO WRK-QTDE-OCOR
               MOVE WRK-QTDE-OCOR      TO GFCTWABU-QTDE-OCOR

               MOVE GFCTWABU-QTDE-TOT-REG
                                       TO WRK-QTDE-TOT-REG
               ADD  1                  TO WRK-QTDE-TOT-REG
               MOVE WRK-QTDE-TOT-REG   TO GFCTWABU-QTDE-TOT-REG

               ADD  +69                TO GFCTWABU-LL

               MOVE WRK-COD-AGENCIA    TO GFCTWABU-AGENCIA-LISTA(IND-1)
               MOVE WRK-COD-CONTA      TO GFCTWABU-CONTA-LISTA(IND-1)

               MOVE IPSSOA-COPLT OF CLIEV004
                                       TO WRK-IPSSOA-COPLT
               MOVE WRK-NOME-CLIENTE   TO GFCTWABU-NOME-LISTA(IND-1)

               MOVE ZEROS              TO GFCTWABU-ERRO-OCORR(IND-1)
                                          GFCTWABU-COD-MSG-ERRO-OCORR
                                                                (IND-1)
                                          GFCTWABU-COD-SQL-ERRO-OCORR
                                                                (IND-1)
           END-IF.

           PERFORM 2412-LER-CSR-CLIEV008.

      *----------------------------------------------------------------*
       2414-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA TRATAR AS CONTAS CORRENTES DO CLIENTE              *
      *----------------------------------------------------------------*
       2420-TRATAR-CONTA-POUPANCA      SECTION.
      *----------------------------------------------------------------*

           PERFORM 2421-ABRIR-CSR-CLIEV007.

           IF  GFCTWABT-AGENCIA-PONT   EQUAL ZEROS AND
               GFCTWABT-CONTA-PONT     EQUAL ZEROS
               PERFORM 2422-LER-CSR-CLIEV007
           ELSE
               MOVE ZEROS              TO WRK-COD-AGENCIA
                                          WRK-COD-CONTA

               PERFORM 2422-LER-CSR-CLIEV007
                 UNTIL WRK-FIM-CURSOR-V007
                                       EQUAL 'S'
                    OR (WRK-COD-AGENCIA
                                       EQUAL GFCTWABT-AGENCIA-PONT
                   AND  WRK-COD-CONTA  EQUAL GFCTWABT-CONTA-PONT)
           END-IF.

           PERFORM 2424-MONTAR-CONTA-POUPANCA
             UNTIL WRK-FIM-CURSOR-V007 EQUAL 'S'
                OR IND-1               EQUAL 11.

           PERFORM 2423-FECHAR-CSR-CLIEV007.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ABRIR CURSOR CLIEV007                                         *
      *----------------------------------------------------------------*
       2421-ABRIR-CSR-CLIEV007         SECTION.
      *----------------------------------------------------------------*

           MOVE CID-CLI OF CLIEV004    TO CID-CLI OF CLIEV007.

           IF  GFCTWABT-TIPO-DEPDC-FIL EQUAL 'AG'
               IF  GFCTWABT-COD-DEPTO-FIL
                                       EQUAL 00001
                   MOVE 00001          TO WRK-CJUNC-DEPDC-INI
                                          WRK-CJUNC-DEPDC-FIM
               ELSE
                   MOVE 00002          TO WRK-CJUNC-DEPDC-INI
HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_MOVE
HEXA  *----------------------------------------------------------------*
HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA
HEXA  *
HEXA  *         HX_MOVE 03999          TO WRK-CJUNC-DEPDC-FIM
HEXA  *
HEXA               MOVE WRK-ULTIMA-AG  TO WRK-CJUNC-DEPDC-FIM
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_MOVE
HEXA  *----------------------------------------------------------------*
HEXA  *
               END-IF
           ELSE
               MOVE 00001              TO WRK-CJUNC-DEPDC-INI
               MOVE 99999              TO WRK-CJUNC-DEPDC-FIM
           END-IF.

HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA
HEXA  *
HEXA       MOVE GFCTWABT-TIPO-DEPDC-FIL TO WRK-TIPO-DEPDC-FIL.
HEXA  *
           EXEC SQL
               OPEN CSR03-CLIEV007
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'CLIEV007'         TO WRK-COD-TABELA
               MOVE 'V01CTA_POUPANCA'  TO WRK-NOME-TABELA
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL

               PERFORM 9000-MONTAR-AREA-DB2

               MOVE 9                  TO GFCTWABU-ERRO
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE
               MOVE WRK-COD-SQL        TO GFCTWABU-COD-SQL-ERRO
               MOVE 0005               TO GFCTWABU-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG

               PERFORM 1310-OBTER-DESC-MSG

               STRING GFCTG3-DESC-MSG  '-'
                      WRK-COD-TABELA
               DELIMITED BY SIZE       INTO GFCTWABU-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2421-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  LER CURSOR CLIEV007                                           *
      *----------------------------------------------------------------*
       2422-LER-CSR-CLIEV007           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH  CSR03-CLIEV007
               INTO
                    :CLIEV007.CJUNC-DEPDC,
                    :CLIEV007.CCTA-CLI   ,
                    :CLIEV007.ICTA-MOVTC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'CLIEV007'         TO WRK-COD-TABELA
               MOVE 'V01CTA_POUPANCA'  TO WRK-NOME-TABELA
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL

               PERFORM 9000-MONTAR-AREA-DB2

               MOVE 9                  TO GFCTWABU-ERRO
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE
               MOVE WRK-COD-SQL        TO GFCTWABU-COD-SQL-ERRO
               MOVE 0006               TO GFCTWABU-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG

               PERFORM 1310-OBTER-DESC-MSG

               STRING GFCTG3-DESC-MSG  '-'
                      WRK-COD-TABELA
               DELIMITED BY SIZE       INTO GFCTWABU-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR-V007
           ELSE
               MOVE CJUNC-DEPDC OF CLIEV007
                                       TO WRK-CJUNC-DEPDC
               MOVE WRK-COD-AGENCIA    TO GFCTWABU-AGENCIA-PONT

               MOVE CCTA-CLI OF CLIEV007
                                       TO WRK-CCTA-CLI
               MOVE WRK-COD-CONTA      TO GFCTWABU-CONTA-PONT
           END-IF.

      *----------------------------------------------------------------*
       2422-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  FECHAR CURSOR CLIEV007                                        *
      *----------------------------------------------------------------*
       2423-FECHAR-CSR-CLIEV007        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE CSR03-CLIEV007
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'CLIEV007'         TO WRK-COD-TABELA
               MOVE 'V01CTA_POUPANCA'  TO WRK-NOME-TABELA
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL

               PERFORM 9000-MONTAR-AREA-DB2

               MOVE 9                  TO GFCTWABU-ERRO
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE
               MOVE WRK-COD-SQL        TO GFCTWABU-COD-SQL-ERRO
               MOVE 0011               TO GFCTWABU-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG

               PERFORM 1310-OBTER-DESC-MSG

               STRING GFCTG3-DESC-MSG  '-'
                      WRK-COD-TABELA
               DELIMITED BY SIZE       INTO GFCTWABU-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2423-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  MONTAR INFORMACOES DAS CONTAS POUPANCA DO CLIENTE             *
      *----------------------------------------------------------------*
       2424-MONTAR-CONTA-POUPANCA      SECTION.
      *----------------------------------------------------------------*

           PERFORM 2500-VERIFICAR-EXTRATO-CLIENTE.

           IF  IND-TEM-EXTRATO-ANUAL   EQUAL 'S'
               ADD  1                  TO IND-1

               MOVE GFCTWABU-QTDE-OCOR TO WRK-QTDE-OCOR
               ADD  1                  TO WRK-QTDE-OCOR
               MOVE WRK-QTDE-OCOR      TO GFCTWABU-QTDE-OCOR

               MOVE GFCTWABU-QTDE-TOT-REG
                                       TO WRK-QTDE-TOT-REG
               ADD  1                  TO WRK-QTDE-TOT-REG
               MOVE WRK-QTDE-TOT-REG   TO GFCTWABU-QTDE-TOT-REG

               ADD  +69                TO GFCTWABU-LL

               MOVE WRK-COD-AGENCIA    TO GFCTWABU-AGENCIA-LISTA(IND-1)
               MOVE WRK-COD-CONTA      TO GFCTWABU-CONTA-LISTA(IND-1)

               MOVE IPSSOA-COPLT OF CLIEV004
                                       TO WRK-IPSSOA-COPLT
               MOVE WRK-NOME-CLIENTE   TO GFCTWABU-NOME-LISTA(IND-1)

               MOVE ZEROS              TO GFCTWABU-ERRO-OCORR(IND-1)
                                          GFCTWABU-COD-MSG-ERRO-OCORR
                                                                (IND-1)
                                          GFCTWABU-COD-SQL-ERRO-OCORR
                                                                (IND-1)
           END-IF.

           PERFORM 2422-LER-CSR-CLIEV007.

      *----------------------------------------------------------------*
       2424-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR A EXISTENCIA DE EXTRATO DO CLIENTE PARA CONSULTA     *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-VERIFICAR-EXTRATO-CLIENTE  SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CBCO            OF GFCTB0K5.
           MOVE WRK-COD-AGENCIA        TO CAG-BCRIA       OF GFCTB0K5.
           MOVE WRK-COD-CONTA          TO CCTA-BCRIA-CLI  OF GFCTB0K5.

           EXEC SQL
             SELECT
                     DANO_REFT_EXTRT
               INTO
                    :GFCTB0K5.DANO-REFT-EXTRT
               FROM
                     DB2PRD.TEXTRT_ANO_TARIF
              WHERE
                     CBCO               = :GFCTB0K5.CBCO
                AND  CAG_BCRIA          = :GFCTB0K5.CAG-BCRIA
                AND  CCTA_BCRIA_CLI     = :GFCTB0K5.CCTA-BCRIA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCTB0K5'         TO WRK-COD-TABELA
               MOVE 'TEXTRT_ANO_TARIF' TO WRK-NOME-TABELA
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL

               PERFORM 9000-MONTAR-AREA-DB2

               MOVE 9                  TO GFCTWABU-ERRO
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE
               MOVE WRK-COD-SQL        TO GFCTWABU-COD-SQL-ERRO
               MOVE 0012               TO GFCTWABU-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG

               PERFORM 1310-OBTER-DESC-MSG

               STRING GFCTG3-DESC-MSG  '-'
                      WRK-COD-TABELA
               DELIMITED BY SIZE       INTO GFCTWABU-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'N'                TO IND-TEM-EXTRATO-ANUAL
           ELSE
               MOVE 'S'                TO IND-TEM-EXTRATO-ANUAL
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA MONTAR AREA COMUM ERRO DB2                         *
      *----------------------------------------------------------------*
       9000-MONTAR-AREA-DB2            SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT0511'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE WRK-NOME-TABELA        TO GFCT0M-NOME-TAB.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA RETORNAR AO CHAMADOR                               *
      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
HEXA  *----------------------------------------------------------------*
HEXA  * SECTION PARA CHAMADA DA ROTINA DE CONVERSAO DE AGENCIA
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9990-10-CHAMA-ROTINA-FX-AG SECTION.
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA       CALL        WRK-MODULO-MESUX0 USING WRK-AREA-MESUX0.
HEXA  *
HEXA       IF          WRK-EH-ERRO-MESU-ABEND
HEXA           MOVE    'APL'       TO          ERR-TIPO-ACESSO
HEXA           MOVE    'ERRO NA CHAMADA DA ROTINA DE FAIXAS DE AGENCIA'
HEXA             TO    ERR-TEXTO
HEXA           PERFORM 9991-10-CHAMA-ROTINA-ABEND
HEXA       ELSE
HEXA           MOVE    ZEROS       TO          WRK-MESUX0-AGENCIA
HEXA       END-IF.
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9990-90-EXIT. EXIT.
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  * SECTION PARA CHAMADA DA ROTINA DE ABEND
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9991-10-CHAMA-ROTINA-ABEND SECTION.
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA       MOVE    'GFCT0511'      TO          ERR-PGM.
HEXA  *
HEXA       CALL    WRK-ABEND-MESUX0 USING      WRK-BATCH-MESUX0
HEXA                                           ERRO-AREA.
HEXA  *
HEXA       GOBACK.
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9991-90-EXIT. EXIT.
HEXA  *----------------------------------------------------------------*
HEXA  *
