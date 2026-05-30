      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0425.
       AUTHOR.     DAYANE FILAKOSKI.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0425                                    *
      *    PROGRAMADORA:   DAYANE FILAKOSKI        - CPM PATO BRANCO   *
      *    ANALISTA CPM:   KELLI CALDATO           - CPM PATO BRANCO   *
      *    ANALISTA....:   VALERIA                 - PROCWORK /GP.50   *
      *    DATA........:   05/05/2006                                  *
      *                                                                *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   LISTA DE CONSULTA PARA DEFERIMENTO DE FLE-  *
      *      XIBILIZACAO POR AGRUPAMENTO.                              *
      *                                                                *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                     INCLUDE/BOOK        *
      *                   DB2PRD.PDIDO_FLEXZ_AGPTO   GFCTB0B2          *
      *                   DB2PRD.V01UF               DCITV002          *
      *                   DB2PRD.PDIDO_FLEXZ_CLI     GFCTB0B3          *
      *                   DB2PRD.PDIDO_FLEXZ_AG      GFCTB0B1          *
      *                   DB2PRD.PDIDO_FLEXZ_SGMTO   GFCTB0C0          *
      *                   DB2PRD.PDIDO_FLEXZ_PAB     GFCTB0B8          *
      *                   DB2PRD.PDIDO_FLEXZ_PSTAL   GFCTB0B9          *
      *                   DB2PRD.PDIDO_FLEXZ_MUN     GFCTB0B7          *
      *                   DB2PRD.PDIDO_FLEXZ_UF      GFCTB0C1          *
120510*                   DB2PRD.TPDIDO_FLEXZ_RZ     GFCTB0M7          *
120510*                   DB2PRD.RAZAO_CONTABIL      PLCCB004          *
      *                   DB2PRD.TTPO_SGMTO_GSTAO    GFCTB0H5          *
      *                   DB2PRD.SERVC_TARIF_PRINC   GFCTB0D8          *
      *                                                                *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTNN - ENTRADA                                          *
      *    I#GFCTNO - SAIDA                                            *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA   - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *    I#GFCTGB - ENTRADA - OBTER DESCRICAO DE PAB.                *
      *    I#GFCTGC - SAIDA   - OBTER DESCRICAO DE PAB.                *
      *    I#GFCT1V - ENTRADA - OBTER DESCRICAO DE AGENCIA.            *
      *    I#GFCT1X - SAIDA   - OBTER DESCRICAO DE AGENCIA.            *
      *    I#RURC88 - AREA PARA MODULO RURC9020.                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5505 - OBTER JUNCAO.                                    *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *    POOL1470 - CONSISTENCIA DE DATAS.                           *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5109 - OBTER DESCRICAO DE PAB.                          *
      *    GFCT5090 - OBTER DESCRICAO DE POSTO.                        *
      *    GFCT5006 - OBTER DESCRICAO DE AGENCIA.                      *
      *    DCIT8000 - OBTER DESCRICAO DE UF.                           *
      *    RURC9020 - OBTER DESCRICAO DE MUNICIPIO.                    *
      *                                                                *
      *----------------------------------------------------------------*
      *    ULTIMA ALTERACAO EM 11/06/2008 - VALERIA                    *
      *    ALTERACOES REFERENTES AOS AGRUPAMENTOS PERSONALIZADOS       *
      *    - INCLUSAO DOS AGRUPAMENTOS PERSONALIZADOS                  *
      *                                                                *
      *----------------------------------------------------------------*
0410  *    ULTIMA ALTERACAO EM 19/04/2010 - HELIO SANTONI NETO         *
0410  *    ALTERACOES REFERENTES AOS AGRUPAMENTOS PERSONALIZADOS       *
0410  *    - INCLUSAO DOS AGRUPAMENTOS BRADESCO EXPRESSO E RAZAO.      *
      *                                                                *
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

       77  IND-1                       PIC  9(003)  COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-MODULO-MSG              PIC  X(008)         VALUE SPACES.
       77  WRK-FINALIZAR               PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-CSR01-GFCTB0B2      PIC  X(001)         VALUE SPACES.
       77  WRK-DESPREZA                PIC  9(009)         VALUE ZEROS.

       01  WRK-COUNT                   PIC S9(009)  COMP   VALUE ZEROS.

       01  WRK-CDEPDC                  PIC S9(005)  COMP   VALUE ZEROS.
       01  WRK-CPOSTO-SERVC            PIC S9(005)  COMP   VALUE ZEROS.

       01  WRK-MSG-TAB.
           05 FILLER                   PIC  X(003)         VALUE
              ' - '.
           05 WRK-NOME-TABELA          PIC  X(008)         VALUE SPACES.

       01  WRK-TIMESTAMP.
           05 WRK-DATA-TIMESTAMP.
             10 WRK-ANO-TIMESTAMP      PIC  9(004)         VALUE ZEROS.
             10 FILLER                 PIC  X(001)         VALUE '-'.
             10 WRK-MES-TIMESTAMP      PIC  9(002)         VALUE ZEROS.
             10 FILLER                 PIC  X(001)         VALUE '-'.
             10 WRK-DIA-TIMESTAMP      PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '-'.
           05 WRK-HORA-TIMESTAMP.
             10 WRK-HR-TIMESTAMP       PIC  9(002)         VALUE ZEROS.
             10 FILLER                 PIC  X(001)         VALUE '.'.
             10 WRK-MIN-TIMESTAMP      PIC  9(002)         VALUE ZEROS.
             10 FILLER                 PIC  X(001)         VALUE '.'.
             10 WRK-SEG-TIMESTAMP      PIC  9(002)         VALUE ZEROS.
             10 FILLER                 PIC  X(001)         VALUE '.'.
             10 WRK-MIL-TIMESTAMP      PIC  9(006)         VALUE ZEROS.

       01  WRK-MSG05.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA DO MODULO '.
           05 WRK-NOME-MODULO          PIC  X(008)         VALUE SPACES.

       01  WRK-DATA                    PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-R                  REDEFINES WRK-DATA.
           05 WRK-DIA                  PIC  9(002).
           05 WRK-MES                  PIC  9(002).
           05 WRK-ANO                  PIC  9(004).

HEXA   01  WRK-COD-SQL-ERRO            PIC S9(09)     VALUE ZEROS.
       01  FILLER                      REDEFINES      WRK-COD-SQL-ERRO.
           05  FILLER                  PIC  X(006).
           05  WRK-COD-SQL-ERRO-2      PIC  9(003).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA POOL1470 *'.
      *----------------------------------------------------------------*

       01  WRK-DATA-1470               PIC  9(008)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO DO MODULO. DCIT8000 *'.
      *----------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           05  WRK-AREA-DCLGEN-DCITV002    PIC  X(070)     VALUE SPACES.
           05  WRK-DCIT-CODIGO-RETORNO     PIC  9(002)     VALUE ZEROS.

       01  WRK-POOL7100                    PIC  X(107)     VALUE SPACES.
       01  WRK-SQLCA                       PIC  X(136)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO DO MODULO GFCT5505 *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-5505.
           05  WRK-5505-AREA-ENTRADA.
             10 WRK-5505-AMBIENTE      PIC  X(001)         VALUE SPACES.
             10 WRK-5505-FUNC-BDSCO    PIC  X(007)         VALUE SPACES.
           05  WRK-5505-AREA-SAIDA.
             10 WRK-5505-COD-RETORNO   PIC  9(002)         VALUE ZEROS.
             10 WRK-5505-COD-SQL-ERRO  PIC  9(003)         VALUE ZEROS.
             10 WRK-5505-DESC-MSG      PIC  X(070)         VALUE SPACES.
             10 WRK-5505-DADOS-RETORNO.
               15 WRK-5505-COD-JUNCAO  PIC  9(005)         VALUE ZEROS.

       01  WRK-IO-PCB                  PIC  X(032)         VALUE SPACES.
       01  WRK-ALT-PCB                 PIC  X(032)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5109 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTGB'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5109 - SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTGC'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5006 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT1V'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5006 - SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT1X'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM RURC9020 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY 'I#RURC88'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B3
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B9
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B7
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H5
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

120510     EXEC SQL
             INCLUDE GFCTB0M7
           END-EXEC.

120510     EXEC SQL
             INCLUDE PLCCB004
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSORES *'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-GFCTB0B2 CURSOR FOR
               SELECT
                 CAGPTO_CTA,
                 CSERVC_TARIF,
                 HIDTFD_FLEXZ_AGPTO,
                 CDEPDC_ORIGE_FLEXZ,
                 HSOLTC_FLEXZ,
                 CSIT_SOLTC_FLEXZ,
                 CINDCD_AGPTO_TOT
             FROM DB2PRD.PDIDO_FLEXZ_AGPTO
             WHERE
                 CAGPTO_CTA          = :GFCTB0B2.CAGPTO-CTA
             AND HIDTFD_FLEXZ_AGPTO >= :GFCTB0B2.HIDTFD-FLEXZ-AGPTO
             AND CSIT_SOLTC_FLEXZ    = :GFCTB0B2.CSIT-SOLTC-FLEXZ
             ORDER BY
                 CSERVC_TARIF,
                 HSOLTC_FLEXZ
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTNN'.

       COPY 'I#GFCTNO'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTNN-ENTRADA
                                                      GFCTNO-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      * ROTINA PRINCIPAL.                                              *
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
      * ROTINA PARA INICIALIZACAO DO PROGRAMA.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTNO-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTNO-SAIDA
                      GFCT0M-AREA-ERROS.

           PERFORM 1100-VERIFICAR-ON-LINE.

           PERFORM 1200-MONTAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICA SITUACAO DO SISTEMA, LIBERADO OU BLOQUEADO.           *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-ON-LINE          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE 100                    TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE GFCTNN-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTNN-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTNN-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0425'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTNO-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE 'GFCT0425'         TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO
                                       TO  GFCT0M-SQLCODE
               MOVE GFCTG1-ERRO        TO  GFCTNO-ERRO
                                           GFCTNO-COD-SQL-ERRO
               MOVE GFCTG1-COD-MSG-ERRO
                                       TO  GFCTNO-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO
                                       TO  GFCTNO-DESC-MSG-ERRO
                                           GFCT0M-TEXTO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0425'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTNO-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA INICIALIZACAO E MONTAGEM DA AREA DE SAIDA A PARTIR *
      * DA AREA DE ENTRADA.                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.
120510     MOVE +278                   TO GFCTNO-LL.
           MOVE ZEROS                  TO GFCTNO-ZZ.
           MOVE GFCTNN-TRANSACAO       TO GFCTNO-TRANSACAO.
           MOVE GFCTNN-FUNCAO          TO GFCTNO-FUNCAO.

           IF  GFCTNN-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCTNO-QTDE-OCOR
           ELSE
               MOVE GFCTNN-QTDE-OCOR   TO GFCTNO-QTDE-OCOR
           END-IF.

           IF  GFCTNN-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO GFCTNO-QTDE-TOT-REG
           ELSE
               MOVE GFCTNN-QTDE-TOT-REG
                                       TO GFCTNO-QTDE-TOT-REG
           END-IF.

           MOVE GFCTNN-FUNC-BDSCO      TO GFCTNO-FUNC-BDSCO.
           MOVE GFCTNN-FILTRO          TO GFCTNO-FILTRO.
           MOVE GFCTNN-PONTEIRO        TO GFCTNO-PONTEIRO.

           MOVE 'N'                    TO GFCTNO-FIM.

           MOVE ZEROS                  TO GFCTNO-ERRO
                                          GFCTNO-COD-SQL-ERRO
                                          GFCTNO-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO GFCTNO-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSISTENCIA DOS DADOS DE ENTRADA.                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTNN-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTNN-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTNN-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTNN-QTDE-OCOR        NOT NUMERIC)                OR
              (GFCTNN-QTDE-TOT-REG     NOT NUMERIC)                OR
              (GFCTNN-AGRUPAMENTO-FIL  NOT NUMERIC                 OR
               GFCTNN-AGRUPAMENTO-FIL  EQUAL ZEROS)                OR
              (GFCTNN-AGRUPAMENTO-PONT NOT NUMERIC)                OR
              (GFCTNN-TARIFA-PONT      NOT NUMERIC)                OR
110608        (GFCTNN-SEQ-PONT         NOT NUMERIC)
110608*       (GFCTNN-AGRUPAMENTO-FIL  NOT EQUAL 004               AND
110608*                                          009               AND
110608*                                          014               AND
110608*                                          015               AND
110608*                                          017               AND
110608*                                          018               AND
110608*                                          019)
               MOVE  1                 TO GFCTNO-ERRO
                                          GFCTNO-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE  ZEROS             TO GFCTNO-COD-SQL-ERRO
               MOVE 'S'                TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTNN-DATA-FIL         NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 1320-VALIDAR-DATA
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ACESSO AO MODULO DE DESCRICOES DE MENSAGENS.       *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE 100                    TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.

           MOVE GFCTNN-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTNN-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTNN-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
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
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0425'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTNO-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTNO-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0425'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO GFCTNO-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTNO-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTNO-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTNO-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTNO-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VALIDAR DATA.                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1320-VALIDAR-DATA               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-DATA-1470.

           MOVE GFCTNN-DATA-FIL(1:2)   TO WRK-DIA.
           MOVE GFCTNN-DATA-FIL(4:2)   TO WRK-MES.
           MOVE GFCTNN-DATA-FIL(7:4)   TO WRK-ANO.

           MOVE WRK-DATA               TO WRK-DATA-1470.

           CALL 'POOL1470'             USING WRK-DATA-1470.

           IF  RETURN-CODE             NOT EQUAL ZEROS AND
               RETURN-CODE             NOT EQUAL 04
               MOVE 9                  TO  GFCTNO-ERRO
               MOVE 'APL'              TO  GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0425'         TO  GFCT0M-PROGRAMA
               MOVE 'POOL1570'         TO  WRK-MODULO-MSG
               MOVE WRK-MSG05          TO  GFCT0M-TEXTO
                                           GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               MOVE 1                  TO GFCTNO-ERRO
               MOVE ZEROS              TO GFCTNO-COD-SQL-ERRO
               MOVE 0689               TO GFCTNO-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE PROCESSAMENTO PRINCIPAL DO PROGRAMA.                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-ACESSAR-GFCT5505.

           PERFORM 2200-PROCESSAR-CSR-GFCTB0B2.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSA O MODULO GFCT5505.                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-ACESSAR-GFCT5505           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-AREA-5505.

           MOVE 'GFCT5505'             TO WRK-MODULO.
           MOVE 'O'                    TO WRK-5505-AMBIENTE.
           MOVE GFCTNN-FUNC-BDSCO      TO WRK-5505-FUNC-BDSCO.

           CALL 'POOL0081'             USING   WRK-MODULO
                                               WRK-5505-AREA-ENTRADA
                                               WRK-5505-AREA-SAIDA
                                               WRK-IO-PCB
                                               WRK-ALT-PCB.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO  GFCTNO-ERRO
               MOVE WRK-MODULO         TO  WRK-MODULO-MSG
                                           GFCT0M-PROGRAMA
               MOVE WRK-MSG05          TO  GFCT0M-TEXTO
               MOVE 'APL'              TO  GFCT0M-TIPO-ACESSO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-5505-COD-RETORNO    NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNO-ERRO
               MOVE ZEROS              TO GFCTNO-COD-SQL-ERRO
               MOVE 'S'                TO GFCTNO-FIM
               MOVE WRK-5505-DESC-MSG
                                       TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE WRK-5505-COD-JUNCAO    TO
                                       CDEPDC-ORIGE-FLEXZ OF GFCTB0B2.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE PROCESSO DA TABELA GFCTB0B2.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-PROCESSAR-CSR-GFCTB0B2     SECTION.
      *----------------------------------------------------------------*

           COMPUTE WRK-DESPREZA = GFCTNN-QTDE-OCOR + 1.

           PERFORM 2210-OPEN-CSR01-GFCTB0B2.
           PERFORM 2220-FETCH-CSR01-GFCTB0B2   VARYING IND-1
                   FROM 1              BY 1
                   UNTIL IND-1         GREATER WRK-DESPREZA OR
                   WRK-FIM-CSR01-GFCTB0B2
                                       EQUAL 'S'.

           PERFORM 2230-MONTAR-SAIDA   VARYING IND-1
                   FROM 1              BY 1
120510             UNTIL IND-1         GREATER 4 OR
                   WRK-FINALIZAR       EQUAL 'S'.

           IF  WRK-FIM-CSR01-GFCTB0B2  NOT EQUAL 'S'
               MOVE 'N'                TO GFCTNO-FIM
           END-IF.

           PERFORM 2240-CLOSE-CSR01-GFCTB0B2.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ABRE CURSOR DA TABELA GFCTB0B2.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-OPEN-CSR01-GFCTB0B2        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                  TO WRK-FIM-CSR01-GFCTB0B2.

           MOVE GFCTNN-AGRUPAMENTO-FIL  TO CAGPTO-CTA       OF GFCTB0B2.
           MOVE WRK-5505-COD-JUNCAO     TO CDEPDC-ORIGE-FLEXZ
                                                            OF GFCTB0B2.
           MOVE 01                      TO CSIT-SOLTC-FLEXZ OF GFCTB0B2.

           MOVE '0001-01-01-01.01.01.000001'
                                       TO WRK-TIMESTAMP.

           IF  GFCTNN-DATA-FIL         NOT EQUAL SPACES AND LOW-VALUES
               MOVE GFCTNN-DATA-FIL(1:2)
                                       TO WRK-DIA-TIMESTAMP
               MOVE GFCTNN-DATA-FIL(4:2)
                                       TO WRK-MES-TIMESTAMP
               MOVE GFCTNN-DATA-FIL(7:4)
                                       TO WRK-ANO-TIMESTAMP
           END-IF.

           MOVE WRK-TIMESTAMP          TO
                                         HIDTFD-FLEXZ-AGPTO OF GFCTB0B2.

           EXEC SQL
               OPEN CSR01-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W' )
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE 'GFCT0425'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '          TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE  '0010'             TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNO-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNO-COD-SQL-ERRO
               MOVE  0005               TO GFCTNO-COD-MSG-ERRO
               MOVE '0005'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG     TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2211-PROCESSAR-COUNT.

           IF  WRK-COUNT               GREATER 150
               MOVE 1                  TO GFCTNO-ERRO
               MOVE 0633               TO GFCTNO-COD-MSG-ERRO
               MOVE '0633'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSA CONTADOR DO CURSOR.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2211-PROCESSAR-COUNT            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT  COUNT(*)
             INTO
                   :WRK-COUNT
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO
             WHERE
                 CAGPTO_CTA          = :GFCTB0B2.CAGPTO-CTA
             AND HIDTFD_FLEXZ_AGPTO >= :GFCTB0B2.HIDTFD-FLEXZ-AGPTO
             AND CSIT_SOLTC_FLEXZ    = :GFCTB0B2.CSIT-SOLTC-FLEXZ
             AND CDEPDC_ORIGE_FLEXZ  = :GFCTB0B2.CDEPDC-ORIGE-FLEXZ
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W' )
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE 'GFCT0425'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '          TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE  '0020'             TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNO-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNO-COD-SQL-ERRO
               MOVE  0010               TO GFCTNO-COD-MSG-ERRO
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG     TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LE CURSOR DA TABELA GFCTB0B2.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2220-FETCH-CSR01-GFCTB0B2       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH  CSR01-GFCTB0B2  INTO
                   :GFCTB0B2.CAGPTO-CTA,
                   :GFCTB0B2.CSERVC-TARIF,
                   :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B2.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B2.HSOLTC-FLEXZ,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
                   :GFCTB0B2.CINDCD-AGPTO-TOT
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE 'GFCT0425'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '          TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE  '0030'             TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNO-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNO-COD-SQL-ERRO
               MOVE  0006               TO GFCTNO-COD-MSG-ERRO
               MOVE '0006'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL 100
               MOVE 'S'                TO WRK-FIM-CSR01-GFCTB0B2
                                          WRK-FINALIZAR
                                          GFCTNO-FIM
               IF  IND-1               EQUAL ZEROS
                   PERFORM 2240-CLOSE-CSR01-GFCTB0B2
                   MOVE 1              TO  GFCTNO-ERRO
                   MOVE ZEROS          TO  GFCTNO-COD-SQL-ERRO
                   MOVE 0690           TO  GFCTNO-COD-MSG-ERRO
                   MOVE '0690'         TO  GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTNO-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * MONTA AREA DE SAIDA DO PROGRAMA.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2230-MONTAR-SAIDA               SECTION.
      *----------------------------------------------------------------*

120510     COMPUTE GFCTNO-LL = GFCTNO-LL + 183.

           MOVE ZEROS                  TO GFCTNO-CONTROLE(IND-1).
           MOVE CSERVC-TARIF OF GFCTB0B2
                                       TO GFCTNO-COD-TARIFA(IND-1).
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO GFCTNO-TIMESTAMP(IND-1).
           MOVE HSOLTC-FLEXZ           OF GFCTB0B2
                                       TO WRK-TIMESTAMP
           MOVE WRK-DIA-TIMESTAMP      TO GFCTNO-DATA-SOLIC(IND-1)(1:2).
           MOVE WRK-MES-TIMESTAMP      TO GFCTNO-DATA-SOLIC(IND-1)(4:2).
           MOVE WRK-ANO-TIMESTAMP      TO GFCTNO-DATA-SOLIC(IND-1)(7:4).
           MOVE '.'                    TO GFCTNO-DATA-SOLIC(IND-1)(3:1).
           MOVE '.'                    TO GFCTNO-DATA-SOLIC(IND-1)(6:1).

           PERFORM 2231-PESQUISAR-TARIFA.

           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTNO-DESCR-TARIFA(IND-1).

           EVALUATE GFCTNN-AGRUPAMENTO-FIL
               WHEN 15
                   PERFORM 2232-CONSULTAR-PAB
               WHEN 14
                   PERFORM 2233-CONSULTAR-AGENCIA
               WHEN 17
                   PERFORM 2234-CONSULTAR-POSTAL
               WHEN 4
                   PERFORM 2235-CONSULTAR-CLIENTE
               WHEN 9
                   PERFORM 2236-CONSULTAR-SEGMENTO
               WHEN 18
                   PERFORM 2237-CONSULTAR-MUNICIPIO
               WHEN 19
                   PERFORM 2238-CONSULTAR-UF
0410           WHEN 20
0410               PERFORM 2234-CONSULTAR-POSTAL
0410           WHEN 26
0410               PERFORM 2239-CONSULTAR-RAZAO
110608         WHEN OTHER
110608             MOVE ZEROS          TO GFCTNO-CGC-CPF(IND-1)
110608                                    GFCTNO-FILIAL(IND-1)
110608                                    GFCTNO-SEGMENTO(IND-1)
110608                                    GFCTNO-AGENCIA(IND-1)
110608                                    GFCTNO-POSTO(IND-1)
110608                                    GFCTNO-MUNICIPIO(IND-1)
0410                                      GFCTNO-GRUPO-CTBIL (IND-1)
0410                                      GFCTNO-SUB-GRUPO-CTBIL (IND-1)
110608             MOVE SPACES         TO GFCTNO-UF(IND-1)
110608                                    GFCTNO-CONTROLE(IND-1)
110608                                    GFCTNO-DESCR-AGPTO(IND-1)
120510                                    GFCTNO-DESC-CTBIL(IND-1)
           END-EVALUATE.

           ADD 1                       TO GFCTNO-QTDE-TOT-REG
                                          GFCTNO-QTDE-OCOR.

           PERFORM 2220-FETCH-CSR01-GFCTB0B2.

120510     IF  IND-1                   EQUAL 4 AND
               WRK-FIM-CSR01-GFCTB0B2  NOT EQUAL 'S'
               MOVE 'S'                TO WRK-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE PESQUISA DE TARIFA, TABELA GFCTB0D8.                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2231-PESQUISAR-TARIFA           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
             SELECT
                   RSERVC_TARIF_REDZD
             INTO
                   :GFCTB0D8.RSERVC-TARIF-REDZD
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W'      )
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE 'GFCT0425'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '          TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE  '0040'             TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNO-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNO-COD-SQL-ERRO
               MOVE  0011               TO GFCTNO-COD-MSG-ERRO
               MOVE '0011'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG     TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2231-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CONSULTA A TABELA PAB - GFCTB0B8.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2232-CONSULTAR-PAB              SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B8.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0B8.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B8.

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
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT0425'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PEDIDO_FLEXZ_PAB' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0050'            TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTNO-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNO-COD-SQL-ERRO
               MOVE  0010              TO GFCTNO-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTNO-ERRO
               MOVE ZEROS              TO GFCTNO-COD-SQL-ERRO
               MOVE 0926               TO GFCTNO-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  CDEPDC OF GFCTB0B8      NOT EQUAL WRK-CDEPDC
           OR  CPOSTO-SERVC OF GFCTB0B8
                                       NOT EQUAL WRK-CPOSTO-SERVC
               PERFORM 2251-OBTER-DESC-PAB
               MOVE CDEPDC OF GFCTB0B8 TO WRK-CDEPDC
               MOVE CPOSTO-SERVC OF GFCTB0B8
                                       TO WRK-CPOSTO-SERVC
           END-IF.

           MOVE GFCTGC-DESC-POSTO(1)   TO GFCTNO-DESCR-AGPTO(IND-1).
           MOVE CDEPDC OF GFCTB0B8     TO GFCTNO-AGENCIA(IND-1).
           MOVE CPOSTO-SERVC OF GFCTB0B8
                                       TO GFCTNO-POSTO(IND-1).

      *----------------------------------------------------------------*
       2232-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSULTAR AGENCIA.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2233-CONSULTAR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B1.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0B1.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B1.
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
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT0425'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PEDIDO_FLEXZ_AG'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0060'            TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTNO-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNO-COD-SQL-ERRO
               MOVE  0010              TO GFCTNO-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTNO-ERRO
               MOVE ZEROS              TO GFCTNO-COD-SQL-ERRO
               MOVE 0926               TO GFCTNO-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.


           MOVE CDEPDC OF GFCTB0B1     TO GFCTNO-AGENCIA(IND-1).

           IF  CINDCD-AGPTO-TOT        OF GFCTB0B2
                                       EQUAL 'S'
               MOVE 'TODAS'            TO GFCTNO-DESCR-AGPTO(IND-1)
           ELSE
               IF  CDEPDC OF GFCTB0B1  NOT EQUAL WRK-CDEPDC
                   MOVE CDEPDC OF GFCTB0B1
                                       TO GFCT1V-COD-DEPDC(1)
                   PERFORM 2252-OBTER-DESC-AGENCIA
                   MOVE CDEPDC OF GFCTB0B1
                                       TO WRK-CDEPDC
               END-IF

               MOVE GFCT1X-DESC-DEPDC(1)
                                       TO GFCTNO-DESCR-AGPTO(IND-1)
           END-IF.

      *----------------------------------------------------------------*
       2233-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSULTAR POSTAL.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2234-CONSULTAR-POSTAL           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B9.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0B9.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B9.

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

           IF (SQLCODE                   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                  EQUAL 'W'      )
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE                GFCT0M-ERRO-SQL
               MOVE 'GFCT0425'           TO GFCT0M-PROGRAMA
               MOVE 'DB2'                TO GFCT0M-TIPO-ACESSO
               MOVE 'PEDIDO_FLEXZ_PSTAL' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE             TO GFCT0M-SQLCODE
               MOVE  '0070'              TO GFCT0M-LOCAL
               MOVE  SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE  9                   TO GFCTNO-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNO-COD-SQL-ERRO
               MOVE  0010                TO GFCTNO-COD-MSG-ERRO
               MOVE '0010'               TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG      TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                   EQUAL +100
               MOVE 1                    TO GFCTNO-ERRO
               MOVE ZEROS                TO GFCTNO-COD-SQL-ERRO
               MOVE 0926                 TO GFCTNO-COD-MSG-ERRO
               MOVE '0926'               TO GFCTG2-COD-MSG
               MOVE 'S'                  TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG      TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  CDEPDC OF GFCTB0B9        NOT EQUAL WRK-CDEPDC
           OR  CPOSTO-SERVC OF GFCTB0B9  NOT EQUAL WRK-CPOSTO-SERVC
               PERFORM 2253-OBTER-DESC-CORRESPONDENTE
               MOVE CDEPDC OF GFCTB0B9   TO WRK-CDEPDC
               MOVE CPOSTO-SERVC OF GFCTB0B9
                                         TO WRK-CPOSTO-SERVC
           END-IF.

           MOVE GFCTGC-DESC-POSTO(1)     TO GFCTNO-DESCR-AGPTO(IND-1).

           MOVE CDEPDC        OF GFCTB0B9
                                         TO GFCTNO-AGENCIA(IND-1).
           MOVE CPOSTO-SERVC  OF GFCTB0B9
                                         TO GFCTNO-POSTO(IND-1).

      *----------------------------------------------------------------*
       2234-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSULTAR CLIENTE.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2235-CONSULTAR-CLIENTE          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B3.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0B3.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B3.

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
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT0425'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PEDIDO_FLEXZ_CLI' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0080'            TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTNO-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNO-COD-SQL-ERRO
               MOVE  0010              TO GFCTNO-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 1                   TO GFCTNO-ERRO
               MOVE ZEROS               TO GFCTNO-COD-SQL-ERRO
               MOVE 926                 TO GFCTNO-COD-MSG-ERRO
               MOVE '0926'              TO GFCTG2-COD-MSG
               MOVE 'S'                 TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG     TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO GFCTNO-DESCR-AGPTO(IND-1)

ST2506*    MOVE CCGC-CPF      OF GFCTB0B3
ST2506*                                TO GFCTNO-CGC-CPF(IND-1).
ST2506*    MOVE CCTRL-CPF-CGC OF GFCTB0B3
ST2506*                                TO GFCTNO-CONTROLE(IND-1).
ST2506*    MOVE CFLIAL-CGC    OF GFCTB0B3
ST2506*                                TO GFCTNO-FILIAL(IND-1).
ST2506     MOVE CCGC-CPF-ST      OF GFCTB0B3
ST2506                                 TO GFCTNO-CGC-CPF(IND-1).
ST2506     MOVE CCTRL-CPF-CGC-ST OF GFCTB0B3
ST2506                                 TO GFCTNO-CONTROLE(IND-1).
ST2506     MOVE CFLIAL-CGC-ST    OF GFCTB0B3
ST2506                                 TO GFCTNO-FILIAL(IND-1).

      *----------------------------------------------------------------*
       2235-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSULTAR SEGMENTO.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2236-CONSULTAR-SEGMENTO         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0C0.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0C0.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0C0.

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

           IF (SQLCODE                   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                  EQUAL 'W'      )
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE                GFCT0M-ERRO-SQL
               MOVE 'GFCT0425'           TO GFCT0M-PROGRAMA
               MOVE 'DB2'                TO GFCT0M-TIPO-ACESSO
               MOVE 'PEDIDO_FLEXZ_SGMTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE             TO GFCT0M-SQLCODE
               MOVE  '0090'              TO GFCT0M-LOCAL
               MOVE  SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE  9                   TO GFCTNO-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNO-COD-SQL-ERRO
               MOVE  0010                TO GFCTNO-COD-MSG-ERRO
               MOVE '0010'               TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG      TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 1                   TO GFCTNO-ERRO
               MOVE ZEROS               TO GFCTNO-COD-SQL-ERRO
               MOVE 0926                TO GFCTNO-COD-MSG-ERRO
               MOVE '0926'              TO GFCTG2-COD-MSG
               MOVE 'S'                 TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG     TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2254-OBTER-DESC-SEGMENTO.

           MOVE CSGMTO-GSTAO-TARIF       OF GFCTB0C0
                                         TO GFCTNO-SEGMENTO(IND-1).
           MOVE ISGMTO-GSTAO-TARIF       OF GFCTB0H5
                                         TO GFCTNO-DESCR-AGPTO(IND-1).

      *----------------------------------------------------------------*
       2236-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSULTAR MUNICIPIO.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2237-CONSULTAR-MUNICIPIO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B7.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0B7.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B7.

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
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT0425'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEZ_MUN'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0100'            TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTNO-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNO-COD-SQL-ERRO
               MOVE  0010              TO GFCTNO-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTNO-ERRO
               MOVE ZEROS              TO GFCTNO-COD-SQL-ERRO
               MOVE 0926               TO GFCTNO-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           INITIALIZE                  AREA-RURC88.
           MOVE CMUN-IBGE OF GFCTB0B7  TO COD-MUNIC-RURC88.
           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL 'POOL0081'             USING   WRK-MODULO
                                               AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNO-ERRO
               MOVE ZEROS              TO GFCTNO-COD-SQL-ERRO
               MOVE 0732               TO GFCTNO-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 10
               MOVE 1                  TO GFCTNO-ERRO
               MOVE ZEROS              TO GFCTNO-COD-SQL-ERRO
               MOVE  675               TO GFCTNO-COD-MSG-ERRO
               MOVE '0675'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 20
               MOVE 1                  TO GFCTNO-ERRO
               MOVE ZEROS              TO GFCTNO-COD-SQL-ERRO
               MOVE 733                TO GFCTNO-COD-MSG-ERRO
               MOVE '0733'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 99
               MOVE 1                  TO GFCTNO-ERRO
               MOVE ZEROS              TO GFCTNO-COD-SQL-ERRO
               MOVE 734                TO GFCTNO-COD-MSG-ERRO
               MOVE '0734'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CMUN-IBGE OF GFCTB0B7  TO GFCTNO-MUNICIPIO(IND-1).
           MOVE NOME-MUNIC-RURC88      TO GFCTNO-DESCR-AGPTO(IND-1).

      *----------------------------------------------------------------*
       2237-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSULTAR UF.                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2238-CONSULTAR-UF               SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0C1.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0C1.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0C1.

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
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT0425'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEZ_UF'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0110'            TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTNO-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNO-COD-SQL-ERRO
               MOVE  0010              TO GFCTNO-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTNO-ERRO
               MOVE ZEROS              TO GFCTNO-COD-SQL-ERRO
               MOVE 0926               TO GFCTNO-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CSGL-UF OF GFCTB0C1    TO CSGL-UF OF DCITV002.
           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNO-ERRO
               MOVE ZEROS              TO GFCTNO-COD-SQL-ERRO
               MOVE 735                TO GFCTNO-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 1
               MOVE 1                  TO GFCTNO-ERRO
               MOVE ZEROS              TO GFCTNO-COD-SQL-ERRO
               MOVE 736                TO GFCTNO-COD-MSG-ERRO
               MOVE '0736'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 2
               MOVE 1                  TO GFCTNO-ERRO
               MOVE ZEROS              TO GFCTNO-COD-SQL-ERRO
               MOVE 737                TO GFCTNO-COD-MSG-ERRO
               MOVE '0737'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 3
               MOVE 1                  TO GFCTNO-ERRO
               MOVE ZEROS              TO GFCTNO-COD-SQL-ERRO
               MOVE 738                TO GFCTNO-COD-MSG-ERRO
               MOVE '0738'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 4
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'DCITV002'         TO WRK-NOME-TABELA
               MOVE 'V01_UF'           TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTNO-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 9                  TO GFCTNO-ERRO
               MOVE 'GFCT0425'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-SQLCA          TO SQLCA
                                          GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
HEXA                                      WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNO-COD-SQL-ERRO
               MOVE SPACES             TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.


           MOVE CSGL-UF OF GFCTB0C1    TO GFCTNO-UF(IND-1).
           MOVE WRK-AREA-DCLGEN-DCITV002
                                       TO DCITV002.
           MOVE IUF     OF DCITV002    TO GFCTNO-DESCR-AGPTO(IND-1).

      *----------------------------------------------------------------*
       2238-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

0410  ******************************************************************
0410  * ROTINA PARA CONSULTAR RAZAO                                    *
0410  ******************************************************************
0410  *----------------------------------------------------------------*
0410   2239-CONSULTAR-RAZAO            SECTION.
0410  *----------------------------------------------------------------*
0410
0410       MOVE CSERVC-TARIF           OF GFCTB0B2
0410                                   TO CSERVC-TARIF     OF GFCTB0M7.
0410       MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
0410                                   TO HIDTFD-FLEXZ-AGPTO
0410                                                       OF GFCTB0M7.
0410       MOVE CAGPTO-CTA             OF GFCTB0B2
0410                                   TO CAGPTO-CTA       OF GFCTB0M7.
0410
0410       EXEC SQL
0410         SELECT
0410               CGRP_CTBIL_ATUAL,
0410               CSGRP_CTBIL
0410         INTO
0410               :GFCTB0M7.CGRP-CTBIL-ATUAL,
0410               :GFCTB0M7.CSGRP-CTBIL
0410         FROM   DB2PRD.TPDIDO_FLEXZ_RZ
0410         WHERE
0410               CAGPTO_CTA         = :GFCTB0M7.CAGPTO-CTA         AND
0410               CSERVC_TARIF       = :GFCTB0M7.CSERVC-TARIF       AND
0410               HIDTFD_FLEXZ_AGPTO = :GFCTB0M7.HIDTFD-FLEXZ-AGPTO
0410       END-EXEC.
0410
0410       IF (SQLCODE                   NOT EQUAL ZEROS AND +100) OR
0410          (SQLWARN0                  EQUAL 'W'      )
0410           MOVE SPACES               TO GFCT0M-ERRO-SQL
0410           INITIALIZE                GFCT0M-ERRO-SQL
0410           MOVE 'GFCT0425'           TO GFCT0M-PROGRAMA
0410           MOVE 'DB2'                TO GFCT0M-TIPO-ACESSO
0410           MOVE 'TPDIDO_FLEXZ_RZ  '  TO GFCT0M-NOME-TAB
0410           MOVE 'SELECT  '           TO GFCT0M-COMANDO-SQL
0410           MOVE  SQLCODE             TO GFCT0M-SQLCODE
0410           MOVE  '0070'              TO GFCT0M-LOCAL
0410           MOVE  SQLCA               TO GFCT0M-SQLCA-AREA
0410           MOVE  9                   TO GFCTNO-ERRO
HEXA           MOVE  SQLCODE             TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2  TO GFCTNO-COD-SQL-ERRO
0410           MOVE  0010                TO GFCTNO-COD-MSG-ERRO
0410           MOVE '0010'               TO GFCTG2-COD-MSG
0410           PERFORM 1310-OBTER-DESC-MSG
0410           MOVE GFCTG3-DESC-MSG      TO GFCTNO-DESC-MSG-ERRO
0410           PERFORM 3000-FINALIZAR
0410       END-IF.
0410
0410       IF  SQLCODE                   EQUAL +100
0410           MOVE 1                    TO GFCTNO-ERRO
0410           MOVE ZEROS                TO GFCTNO-COD-SQL-ERRO
0410           MOVE 0926                 TO GFCTNO-COD-MSG-ERRO
0410           MOVE '0926'               TO GFCTG2-COD-MSG
0410           MOVE 'S'                  TO GFCTNO-FIM
0410           PERFORM 1310-OBTER-DESC-MSG
0410           MOVE GFCTG3-DESC-MSG      TO GFCTNO-DESC-MSG-ERRO
0410           PERFORM 3000-FINALIZAR
0410       END-IF.
0410
0410       MOVE CGRP-CTBIL-ATUAL       OF GFCTB0M7
0410                                   TO GFCTNO-GRUPO-CTBIL    (IND-1)
0410       MOVE CSGRP-CTBIL            OF GFCTB0M7
0410                                   TO GFCTNO-SUB-GRUPO-CTBIL(IND-1).
0410
0510       PERFORM 2255-BUSCAR-RAZAO-CONTABIL.
0510
0510       MOVE IRZ-CTBIL              OF PLCCB004
0510                                   TO GFCTNO-DESC-CTBIL(IND-1).
0510
0410  *----------------------------------------------------------------*
0410   2239-99-FIM.                    EXIT.
0410  *----------------------------------------------------------------*

      ******************************************************************
      * FECHA CURSOR DA TABELA GFCTB0B2.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2240-CLOSE-CSR01-GFCTB0B2       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR01-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W'      )
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE 'GFCT0425'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '          TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE  '0140'             TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNO-ERRO
HEXA           MOVE  SQLCODE             TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2  TO GFCTNO-COD-SQL-ERRO
               MOVE  0011               TO GFCTNO-COD-MSG-ERRO
               MOVE '0011'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTEM DESCRICAO DO POSTO BANCARIO.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2251-OBTER-DESC-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.
           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE GFCTNN-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTNN-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE GFCTNN-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.

           MOVE CDEPDC                 OF GFCTB0B8
                                       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC           OF GFCTB0B8
                                       TO GFCTGB-COD-POSTO.

           MOVE SPACES                 TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5109'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0425'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTNO-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTNO-FIM
               MOVE GFCTGC-ERRO        TO GFCTNO-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO GFCTNO-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO GFCTNO-COD-MSG-ERRO
               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTNO-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109-'
                           GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTNO-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2251-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTEM DESCRICAO DA AGENCIA.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2252-OBTER-DESC-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE 660                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.

           MOVE GFCTNN-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTNN-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTNN-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.

           MOVE SPACES                 TO GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCT1X-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0425'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTNO-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCT1X-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTNO-FIM
               MOVE GFCT1X-ERRO        TO GFCTNO-ERRO
               MOVE GFCT1X-COD-SQL-ERRO
                                       TO GFCTNO-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO
                                       TO GFCTNO-COD-MSG-ERRO
               IF  GFCT1X-ERRO         EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO
                                       TO GFCTNO-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006-'
                           GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTNO-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2252-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTEM DESCRICAO DO CORRESPONDENTE BANCARIO.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2253-OBTER-DESC-CORRESPONDENTE  SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.
           INITIALIZE                     GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE GFCTNN-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTNN-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE GFCTNN-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.
           MOVE CDEPDC                 OF GFCTB0B9
                                       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC           OF GFCTB0B9
                                       TO GFCTGB-COD-POSTO.

           INITIALIZE                  GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5090'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0425'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTNO-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTNO-FIM
               MOVE GFCTGC-ERRO        TO GFCTNO-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO GFCTNO-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO GFCTNO-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTNO-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5090-'
                           GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTNO-DESC-MSG-ERRO
               END-IF

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2253-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTEM DESCRICAO DO SEGMENTO.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2254-OBTER-DESC-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-GSTAO-TARIF    OF GFCTB0C0
                                      TO CSGMTO-GSTAO-TARIF OF GFCTB0H5.
           EXEC SQL
             SELECT
                   ISGMTO_GSTAO_TARIF
             INTO
                   :GFCTB0H5.ISGMTO-GSTAO-TARIF
             FROM   DB2PRD.TTPO_SGMTO_GSTAO
             WHERE
                   CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                  EQUAL 'W'      )
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE                GFCT0M-ERRO-SQL
               MOVE 'GFCT0425'           TO GFCT0M-PROGRAMA
               MOVE 'DB2'                TO GFCT0M-TIPO-ACESSO
               MOVE 'TTPO_SGMTO_GFTAO'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE             TO GFCT0M-SQLCODE
               MOVE  '0130'              TO GFCT0M-LOCAL
               MOVE  SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE  9                   TO GFCTNO-ERRO
HEXA           MOVE  SQLCODE             TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2  TO GFCTNO-COD-SQL-ERRO
               MOVE  0010                TO GFCTNO-COD-MSG-ERRO
               MOVE '0010'               TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG      TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 1                   TO GFCTNO-ERRO
               MOVE ZEROS               TO GFCTNO-COD-SQL-ERRO
               MOVE 0500                TO GFCTNO-COD-MSG-ERRO
               MOVE '0500'              TO GFCTG2-COD-MSG
               MOVE 'S'                 TO GFCTNO-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG     TO GFCTNO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2254-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *ACESSAR PLCC PARA BUSCAR DESCRICAO RAZAO CONTABIL               *
      ******************************************************************
0510  *----------------------------------------------------------------*
0510   2255-BUSCAR-RAZAO-CONTABIL SECTION.
0510  *----------------------------------------------------------------*
0510
0410       MOVE CGRP-CTBIL-ATUAL       OF GFCTB0M7
0410                                   TO CGRP-CTBIL-ATUAL OF PLCCB004.
0410       MOVE CSGRP-CTBIL            OF GFCTB0M7
0410                                   TO CSGRP-CTBIL      OF PLCCB004.
0510       EXEC SQL
0510           SELECT  IRZ_CTBIL
0510           INTO   :IRZ-CTBIL
0510           FROM    DB2PRD.RAZAO_CONTABIL
0510           WHERE   CEMPR = 237
0510           AND     CGRP_CTBIL_ATUAL =:GFCTB0M7.CGRP-CTBIL-ATUAL
0510           AND     CSGRP_CTBIL      =:GFCTB0M7.CSGRP-CTBIL
0510       END-EXEC.
0510
0510       IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0510          (SQLWARN0                EQUAL 'W'      )
0510           INITIALIZE              GFCT0M-ERRO-SQL
0510           MOVE '0010'             TO GFCTG2-COD-MSG
0510           PERFORM 1310-OBTER-DESC-MSG
0510           MOVE 'RAZAO_CONTABIL   '
0510                                   TO GFCT0M-NOME-TAB
0510           MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0510           MOVE '0075'             TO GFCT0M-LOCAL
0510           MOVE  0010              TO GFCTNO-COD-MSG-ERRO
HEXA           MOVE  SQLCODE             TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2  TO GFCTNO-COD-SQL-ERRO
0510           MOVE  9                 TO GFCTNO-ERRO
0510           MOVE 'S'                TO GFCTNO-FIM
0510           MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
0510           PERFORM 3000-FINALIZAR
0510       END-IF.
0510
0510       IF  SQLCODE                 EQUAL +100
0510           MOVE  1                 TO GFCTNO-ERRO
0510           MOVE ZEROS              TO GFCTNO-COD-SQL-ERRO
0510           MOVE  1690              TO GFCTNO-COD-MSG-ERRO
0510           MOVE '1690'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
0510           MOVE 'S'                TO GFCTNO-FIM
0510           MOVE GFCTG3-DESC-MSG    TO GFCTNO-DESC-MSG-ERRO
0510           PERFORM 3000-FINALIZAR
0510       END-IF.
0510
0510  *----------------------------------------------------------------*
0510   2255-99-FIM.                    EXIT.
0510  *----------------------------------------------------------------*

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
