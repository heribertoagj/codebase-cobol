      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0882.
       AUTHOR.     LEANDRO MENDES.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0882                                    *
      *    PROGRAMADOR.:   LEANDRO MENDES          - CPM PATO BRANCO   *
      *    ANALISTA CPM:   MARCIO CUSTIN           - CPM PATO BRANCO   *
      *    ANALISTA....:   VALERIA                 - PROCWORK /GP.50   *
      *    DATA........:   10/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   FLEXIBILIZACAO - LISTA DE FLEXIBILIZACAO    *
      *      PARA AGRUPAMENTO (TABELA QUENTE)                          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *                   DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8       *
      *                   DB2PRD.FLEXZ_GRP_UF           GFCTB057       *
      *                   DB2PRD.FLEXZ_GRP_MUN          GFCTB053       *
      *                   DB2PRD.FLEXZ_GRP_PSTAL        GFCTB055       *
      *                   DB2PRD.FLEXZ_GRP_PAB          GFCTB054       *
      *                   DB2PRD.FLEXZ_GRP_SGMTO        GFCTB056       *
      *                   DB2PRD.FLEXZ_GRP_DEPDC        GFCTB051       *
      *                   DB2PRD.FLEXZ_GRP_CLI          GFCTB050       *
110510*                   DB2PRD.TFLEXZ_GRP_RZ          GFCTB0M8       *
      *                   DB2PRD.FLEXZ_AGPTO_CTA        GFCTB049       *
      *                   DB2PRD.FLEXZ_GRP_SGMTO        GFCTB0H5       *
      *                   DB2PRD.FLEXZ_AGPTO_CTA        DCITV002       *
110510*                   DB2PRD.RAZAO_CONTABIL         PLCCB004       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTNF - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTNG - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA   - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTGB - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTGC - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCT1V - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCT1X - AREA DE COMUNICACAO - SAIDA                      *
      *    I#RURC88 - AREA PARA MODULO RURC9020                        *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *    GFCT5109 - OBTER DESCRICAO DE PAB              .            *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5006 - DESCRICAO DEPENDENCIA                            *
      *    DCIT8000 - OBTEM UF                                         *
      *    RURC9020 - OBTEM MUNICIPIO                                  *
      *    GFCT5090 - OBTEM DESCRICAO POSTO                            *
      *================================================================*
      *    ULTIMA ALTERACAO EM 11/06/2008 - VALERIA                    *
      *    ALTERACOES REFERENTES AOS AGRUPAMENTOS PERSONALIZADOS       *
      *    - INCLUSAO DOS AGRUPAMENTOS PERSONALIZADOS                  *
      *----------------------------------------------------------------*
0310  *    ULTIMA ALTERACAO EM 29/03/2010 - HELIO SANTONI              *
0310  *    - ACRESCENTAR TRATAMENTO DOS AGRUPAMENTOS 20 E 26.          *
      *                                                                *
      *================================================================*
VI0818*    RETIRAR LIMITE DE RETORNO DE OCORRENCIAS, DE 150.           *
VI0818*                                                                *
VI0818*================================================================*

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

       77  WRK-GFCTNF-QTDE-OCOR        PIC  9(007) COMP-3 VALUE ZERO.
       77  WRK-GFCTNG-QTDE-TOT-REG     PIC  9(009) COMP-3 VALUE ZERO.
       77  WRK-GFCTNG-QTDE-OCOR        PIC  9(007) COMP-3 VALUE ZERO.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *---------------------------------------------------------------*

HEXA   77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.
       77  IND-2                       PIC  9(007)         VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *---------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(008)         VALUE SPACES.
       77  WRK-DESPREZA                PIC  9(007)         VALUE ZEROS.
       77  WRK-DESPREZA-PK             PIC  9(007) COMP-3  VALUE ZEROS.
       77  WRK-POOL7100                PIC  X(107)         VALUE SPACES.
       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.
       77  WRK-FIM-CURSOR              PIC  X(001)         VALUE SPACES.
       01  WRK-COUNT                   PIC S9(009)  COMP   VALUE ZEROS.
       01  WRK-TARIFA-INI              PIC S9(005) COMP-3  VALUE ZEROS.
       01  WRK-TARIFA-FIM              PIC S9(005) COMP-3  VALUE ZEROS.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.

HEXA   01  WRK-COD-SQL-ERRO            PIC S9(09)     VALUE ZEROS.
       01  FILLER                      REDEFINES      WRK-COD-SQL-ERRO.
           05  FILLER                  PIC  X(006).
           05  WRK-COD-SQL-ERRO-2      PIC  9(003).

HEXA   01  WRK-GRUPO-CTBIL             PIC S9(03)        VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-GRUPO-CTBIL.
           05  WRK-GRUPO-CTBIL-2       PIC  9(003).

HEXA   01  WRK-SGRUPO-CTBIL             PIC S9(03)        VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-SGRUPO-CTBIL.
           05  WRK-SGRUPO-CTBIL-2       PIC  9(003).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM DCIT8000 *'.
      *---------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           05  WRK-AREA-DCLGEN-DCITV002 PIC X(070)         VALUE SPACES.
           05  WRK-DCIT-CODIGO-RETORNO  PIC 9(002)         VALUE ZEROS.

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
           '* AREA PARA INCLUDES  *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

       COPY 'I#RURC88'.

       COPY 'I#GFCTGB'.

       COPY 'I#GFCTGC'.

       COPY 'I#GFCT1V'.

       COPY 'I#GFCT1X'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB057
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB053
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB055
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB054
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB056
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB051
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB050
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB049
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H5
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
           END-EXEC.

0510       EXEC SQL
0510         INCLUDE PLCCB004
0510       END-EXEC.

110510     EXEC SQL
110510       INCLUDE GFCTB0M8
110510     END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSORES *'.
      *---------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-GFCTB049 CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   HIDTFD_FLEXZ_AGPTO,
                   DINIC_FLEXZ_AGPTO,
                   DFIM_FLEXZ_AGPTO,
                   CINDCD_AGPTO_TOT
             FROM  DB2PRD.FLEXZ_AGPTO_CTA
             WHERE
                   CAGPTO_CTA         = :GFCTB049.CAGPTO-CTA         AND
                  (CSERVC_TARIF      >= :WRK-TARIFA-INI              AND
                   CSERVC_TARIF      <= :WRK-TARIFA-FIM)             AND
                  (DINIC_FLEXZ_AGPTO <= :GFCTB049.DINIC-FLEXZ-AGPTO  AND
                   DFIM_FLEXZ_AGPTO  >= :GFCTB049.DFIM-FLEXZ-AGPTO)  AND
                   CDEPDC_ENCRR_VGCIA = 0                            AND
                   CFUNC_ENCRR_VGCIA  = 0
             ORDER BY CAGPTO_CTA,
                      CSERVC_TARIF,
                      DINIC_FLEXZ_AGPTO DESC
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTNF'.

       COPY 'I#GFCTNG'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTNF-ENTRADA
                                                      GFCTNG-SAIDA
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
                                          GFCTNG-SAIDA.

           INITIALIZE GFCTNG-SAIDA
                      GFCT0M-AREA-ERROS
                      WRK-FIM-CURSOR
                      IND-1
                      IND-2.

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

           MOVE +415                   TO GFCTFZ-LL.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE GFCTNF-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTNF-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE '0000000'              TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.
           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTNG-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0882'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                 EQUAL 9
               MOVE 'GFCT0882'             TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO    TO GFCT0M-SQLCODE
                                              GFCTNG-COD-SQL-ERRO
               MOVE GFCTG1-ERRO            TO GFCTNG-ERRO
               MOVE GFCTG1-COD-MSG-ERRO    TO GFCTNG-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO   TO GFCTNG-DESC-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO(1:75) TO
                                              GFCT0M-TEXTO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE          EQUAL 1
               MOVE 9                      TO GFCTNG-ERRO
               MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0882'             TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATA AREA DE RETORNO AO CHAMADOR                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCTNG-SAIDA.

0510       MOVE 214                        TO GFCTNG-LL.
           MOVE GFCTNF-ZZ                  TO GFCTNG-ZZ.
           MOVE GFCTNF-TRANSACAO           TO GFCTNG-TRANSACAO.
           MOVE GFCTNF-FUNC-BDSCO          TO GFCTNG-FUNC-BDSCO.
           MOVE GFCTNF-FUNCAO              TO GFCTNG-FUNCAO.
           MOVE GFCTNF-FILTRO              TO GFCTNG-FILTRO.
           MOVE GFCTNF-PONTEIRO            TO GFCTNG-PONTEIRO.

           IF  GFCTNF-QTDE-OCOR            NOT NUMERIC
               MOVE ZEROS                  TO GFCTNG-QTDE-OCOR
           ELSE
               MOVE GFCTNF-QTDE-OCOR       TO GFCTNG-QTDE-OCOR
           END-IF.

           IF  GFCTNF-QTDE-TOT-REG         NOT NUMERIC
               MOVE ZEROS                  TO GFCTNG-QTDE-TOT-REG
           ELSE
               MOVE GFCTNF-QTDE-TOT-REG    TO GFCTNG-QTDE-TOT-REG
           END-IF.

           MOVE 'N'                        TO GFCTNG-FIM.
           MOVE ZEROS                      TO GFCTNG-ERRO
                                              GFCTNG-COD-SQL-ERRO
                                              GFCTNG-COD-MSG-ERRO.
           MOVE '0000'                     TO GFCTG2-COD-MSG.

           PERFORM 1210-OBTER-DESC-MSG.

           MOVE  GFCTG3-DESC-MSG           TO GFCTNG-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA MODULO GFCT5523 PARA OBTER A DESCRICAO DA MENSAGEM       *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTNF-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTNF-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTNF-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTNG-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0882'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTNG-ERRO
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT0882'     TO GFCT0M-TRANSACAO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                 EQUAL 1
               MOVE 1                      TO GFCTNG-ERRO
               MOVE GFCTG3-COD-SQL-ERRO    TO GFCTNG-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO    TO GFCTNG-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO   TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTE DADOS                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTNF-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTNF-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTNF-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTNF-AGRUPMTO-FIL     NOT NUMERIC)                OR
              (GFCTNF-AGRUPMTO-FIL     EQUAL ZEROS)                OR
              (GFCTNF-TARIFA-FIL       NOT NUMERIC)                OR
              (GFCTNF-AGRUPMTO-PONT    NOT NUMERIC)                OR
              (GFCTNF-TARIFA-PONT      NOT NUMERIC)
               MOVE  1                 TO GFCTNG-ERRO
               MOVE  ZEROS             TO GFCTNG-COD-SQL-ERRO
               MOVE  0001              TO GFCTNG-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNG-FIM
               PERFORM 1210-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-OBTER-DATA-PROCM.

           PERFORM 2200-CONTAR-REGISTROS.

           PERFORM 2300-ABRIR-CURSOR.

           MOVE GFCTNF-QTDE-OCOR TO WRK-GFCTNF-QTDE-OCOR.
           COMPUTE WRK-DESPREZA-PK     = WRK-GFCTNF-QTDE-OCOR + 1.
           MOVE WRK-DESPREZA-PK TO WRK-DESPREZA.

           PERFORM 2400-LER-CURSOR     VARYING IND-2  FROM 1 BY 1 UNTIL
                   IND-2               GREATER WRK-DESPREZA OR
                   WRK-FIM-CURSOR      EQUAL 'S'.

           PERFORM 2500-PROCESSAR-CURSOR  UNTIL
0510               IND-1                  GREATER 4 OR
                   WRK-FIM-CURSOR         EQUAL 'S'.

           PERFORM 2600-FECHAR-CURSOR.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA CONSULTA NA TABELA GFCTB0A1                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-OBTER-DATA-PROCM           SECTION.
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
               MOVE  0010              TO GFCTNG-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATA AREA DE ERRO EM ACESSO A TABELAS                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*


           MOVE  9                     TO GFCTNG-ERRO.
           MOVE 'GFCT0882'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
HEXA                                      WRK-COD-SQL-ERRO
.          MOVE   WRK-COD-SQL-ERRO-2 TO   GFCTNG-COD-SQL-ERRO.

           PERFORM 1210-OBTER-DESC-MSG.

           STRING GFCTG3-DESC-MSG ' - ' WRK-NOM-TAB
           DELIMITED BY '  '           INTO GFCTNG-DESC-MSG-ERRO.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONTAR REGISTROS DO CURSOR                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-CONTAR-REGISTROS           SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB049
                                          DFIM-FLEXZ-AGPTO  OF GFCTB049.

           MOVE GFCTNF-AGRUPMTO-FIL    TO CAGPTO-CTA OF GFCTB049.

           IF  GFCTNF-TARIFA-FIL       EQUAL ZEROS
               MOVE ZEROS              TO WRK-TARIFA-INI
               MOVE 9999999            TO WRK-TARIFA-FIM
           ELSE
               MOVE GFCTNF-TARIFA-FIL  TO WRK-TARIFA-INI
                                          WRK-TARIFA-FIM
           END-IF.

           EXEC SQL
             SELECT
                   COUNT (*)
             INTO
                   :WRK-COUNT
             FROM   DB2PRD.FLEXZ_AGPTO_CTA
             WHERE
                   CAGPTO_CTA         = :GFCTB049.CAGPTO-CTA         AND
                  (CSERVC_TARIF      >= :WRK-TARIFA-INI              AND
                   CSERVC_TARIF      <= :WRK-TARIFA-FIM)             AND
                  (DINIC_FLEXZ_AGPTO <= :GFCTB049.DINIC-FLEXZ-AGPTO  AND
                   DFIM_FLEXZ_AGPTO  >= :GFCTB049.DFIM-FLEXZ-AGPTO)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_AGPTO_CTA ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNG-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB049'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

VI0818*____IF__WRK-COUNT_______________GREATER_150
VI0818*________MOVE_1__________________TO_GFCTNG-ERRO
VI0818*________MOVE_ZEROS______________TO_GFCTNG-COD-SQL-ERRO
VI0818*________MOVE_0633_______________TO_GFCTNG-COD-MSG-ERRO
VI0818*________MOVE_'0633'_____________TO_GFCTG2-COD-MSG
VI0818*________MOVE_'S'________________TO_GFCTNG-FIM
VI0818*________PERFORM_1210-OBTER-DESC-MSG
VI0818*________MOVE__GFCTG3-DESC-MSG___TO_GFCTNG-DESC-MSG-ERRO
VI0818*________PERFORM_3000-FINALIZAR
VI0818*____END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR O CURSOR DA TABELA GFCTB049                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR01-GFCTB049
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_AGPTO_CTA ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNG-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB049'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR O CURSOR DA TABELA GFCTB049                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*


           EXEC SQL
             FETCH CSR01-GFCTB049  INTO
                   :GFCTB049.CSERVC-TARIF,
                   :GFCTB049.CAGPTO-CTA,
                   :GFCTB049.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB049.DINIC-FLEXZ-AGPTO,
                   :GFCTB049.DFIM-FLEXZ-AGPTO,
                   :GFCTB049.CINDCD-AGPTO-TOT
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                    EQUAL 'W'      )
               MOVE SPACES                 TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_AGPTO_CTA '     TO GFCT0M-NOME-TAB
               MOVE 'FETCH'                TO GFCT0M-COMANDO-SQL
               MOVE '0040'                 TO GFCT0M-LOCAL
               MOVE  0006                  TO GFCTNG-COD-MSG-ERRO
               MOVE '0006'                 TO GFCTG2-COD-MSG
               MOVE 'GFCTB049'             TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                     EQUAL +100
               MOVE 'S'                    TO GFCTNG-FIM
                                              WRK-FIM-CURSOR
               IF  IND-1                   EQUAL ZEROS
                   MOVE 0                  TO GFCTNG-ERRO
                   MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
                   MOVE 0834               TO GFCTNG-COD-MSG-ERRO
                   MOVE '0834'             TO GFCTG2-COD-MSG
                   PERFORM 1210-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAR CURSOR                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-PROCESSAR-CURSOR           SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO IND-1.
0710       ADD 199                     TO GFCTNG-LL.

           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO GFCTNG-TIMESTAMP(IND-1).
           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO GFCTNG-COD-TARIFA(IND-1).
           MOVE DINIC-FLEXZ-AGPTO      OF GFCTB049
                                       TO GFCTNG-DATA-INIC(IND-1).
           MOVE 'DEFERIDA VIGENTE'     TO GFCTNG-SITUACAO(IND-1).

           PERFORM 2510-PESQUISAR-TARIFA.

           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTNG-DESCR-TARIFA(IND-1).

           EVALUATE GFCTNF-AGRUPMTO-FIL
               WHEN 15
                   PERFORM 2520-SELECIONAR-PAB
               WHEN 14
                   PERFORM 2530-SELECIONAR-AGENCIA
               WHEN 17
                   PERFORM 2540-SELECIONAR-POSTAL
               WHEN 04
                   PERFORM 2550-SELECIONAR-CLIENTE
               WHEN 09
                   PERFORM 2560-SELECIONAR-SEGMENTO
               WHEN 18
                   PERFORM 2570-SELECIONAR-MUNICIPIO
               WHEN 19
                   PERFORM 2580-SELECIONAR-UF
0310           WHEN 20
0310               PERFORM 2540-SELECIONAR-POSTAL
0310           WHEN 26
0310               PERFORM 2590-SELECIONAR-RAZAO
110608         WHEN OTHER
ST2506*            MOVE ZEROS          TO GFCTNG-CGC-CPF(IND-1)
ST2506*                                   GFCTNG-FILIAL(IND-1)
110608             MOVE ZEROS          TO GFCTNG-SEGMENTO(IND-1)
110608                                    GFCTNG-AGENCIA(IND-1)
110608                                    GFCTNG-POSTO(IND-1)
110608                                    GFCTNG-MUNICIPIO(IND-1)
0310                                      GFCTNG-GRUPO-CTBIL (IND-1)
0310                                      GFCTNG-SUB-GRUPO-CTBIL(IND-1)
ST2506             MOVE SPACES         TO GFCTNG-CGC-CPF(IND-1)
ST2506                                    GFCTNG-FILIAL(IND-1)
110608                                    GFCTNG-UF(IND-1)
110608                                    GFCTNG-CONTROLE(IND-1)
0510                                      GFCTNG-DESC-CTBIL(IND-1)
110608                                    GFCTNG-DESCR-AGPTO(IND-1)
           END-EVALUATE.

           MOVE GFCTNG-QTDE-TOT-REG    TO WRK-GFCTNG-QTDE-TOT-REG.
           MOVE GFCTNG-QTDE-OCOR       TO WRK-GFCTNG-QTDE-OCOR.

           ADD 1                       TO WRK-GFCTNG-QTDE-TOT-REG
                                          WRK-GFCTNG-QTDE-OCOR.

           MOVE WRK-GFCTNG-QTDE-TOT-REG    TO GFCTNG-QTDE-TOT-REG.
           MOVE WRK-GFCTNG-QTDE-OCOR       TO GFCTNG-QTDE-OCOR.

           PERFORM 2400-LER-CURSOR.

0510       IF  IND-1                   EQUAL 4
               MOVE 'S'                TO WRK-FIM-CURSOR
               MOVE CAGPTO-CTA         OF GFCTB049
                                       TO GFCTNG-AGRUPMTO-PONT
               MOVE DINIC-FLEXZ-AGPTO  OF GFCTB049
                                       TO GFCTNG-DATA-PONT
               MOVE CSERVC-TARIF       OF GFCTB049
                                       TO GFCTNG-TARIFA-PONT
               MOVE HIDTFD-FLEXZ-AGPTO OF GFCTB049
                                       TO GFCTNG-TIMESTAMP-PONT
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR TABELA GFCTB0D8                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2510-PESQUISAR-TARIFA           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF               OF GFCTB049
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

260708     IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                    EQUAL 'W'      )
               MOVE SPACES                 TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT'               TO GFCT0M-COMANDO-SQL
               MOVE '0050'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNG-COD-MSG-ERRO
               MOVE '0010'                 TO GFCTG2-COD-MSG
               MOVE 'GFCTB0D8'             TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

260708     IF  SQLCODE                     EQUAL +100
260708         MOVE SPACES                 TO  RSERVC-TARIF-REDZD
260708                                     OF  GFCTB0D8
260708     END-IF.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR PAB                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2520-SELECIONAR-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF   OF GFCTB049 TO CSERVC-TARIF OF GFCTB054.
           MOVE HIDTFD-FLEXZ-AGPTO         OF GFCTB049
                                           TO HIDTFD-FLEXZ-AGPTO
                                           OF GFCTB054.
           MOVE CAGPTO-CTA     OF GFCTB049 TO CAGPTO-CTA   OF GFCTB054.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB054.CEMPR-INC,
                   :GFCTB054.CDEPDC,
                   :GFCTB054.CPOSTO-SERVC
             FROM   DB2PRD.FLEXZ_GRP_PAB
             WHERE
                   CSERVC_TARIF       = :GFCTB054.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB054.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB054.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_GRP_PAB   ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNG-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB054'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTNG-ERRO
               MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
               MOVE 0926               TO GFCTNG-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNG-FIM
               PERFORM 1210-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CDEPDC OF GFCTB054     TO GFCTNG-AGENCIA(IND-1).
           MOVE CPOSTO-SERVC           OF GFCTB054
                                       TO GFCTNG-POSTO(IND-1).

           PERFORM 2521-OBTER-DESC-PAB.

           MOVE GFCTGC-DESC-POSTO(01)  TO GFCTNG-DESCR-AGPTO(IND-1).

      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA MODULO GFCT5109                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2521-OBTER-DESC-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTGB-ENTRADA
                      GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.
           MOVE GFCTNF-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTNF-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR
                                          GFCTGB-QTDE-TOT-REG.
           MOVE CDEPDC                 OF GFCTB054
                                       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC           OF GFCTB054
                                       TO GFCTGB-COD-POSTO.
           MOVE GFCTNF-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.
           MOVE 'GFCT5109'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTNG-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0882'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTNG-FIM
               MOVE GFCTGC-ERRO        TO GFCTNG-ERRO
               MOVE GFCTGC-COD-SQL-ERRO    TO GFCTNG-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO    TO GFCTNG-COD-MSG-ERRO
               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTNG-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109 - ' GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTNG-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2521-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONA AGENCIA                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2530-SELECIONAR-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF OF GFCTB051.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB051.
           MOVE CAGPTO-CTA OF GFCTB049 TO CAGPTO-CTA        OF GFCTB051.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC
             INTO
                   :GFCTB051.CEMPR-INC,
                   :GFCTB051.CDEPDC
             FROM   DB2PRD.FLEXZ_GRP_DEPDC
             WHERE
                   CSERVC_TARIF       = :GFCTB051.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB051.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB051.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_GRP_DEPDC ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNG-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB051'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTNG-ERRO
               MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
               MOVE 0926               TO GFCTNG-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNG-FIM
               PERFORM 1210-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CDEPDC OF GFCTB051     TO GFCTNG-AGENCIA(IND-1).

           IF  CDEPDC OF GFCTB051      EQUAL ZEROS
               MOVE 'TODAS'            TO GFCTNG-DESCR-AGPTO(IND-1)
           ELSE
               PERFORM 2531-OBTER-DESC-AGE
               MOVE GFCT1X-DESC-DEPDC(1)
                                       TO GFCTNG-DESCR-AGPTO(IND-1)
           END-IF.

      *----------------------------------------------------------------*
       2530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA MODULO GFCT5006                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2531-OBTER-DESC-AGE             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1V-ENTRADA
                                          GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCT1V-ENTRADA
                      GFCT1X-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 660                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.
           MOVE GFCTNF-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTNF-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTNF-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.
           MOVE CDEPDC                 OF GFCTB051
                                       TO GFCT1V-COD-DEPDC(1).
           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTNG-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0882'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCT1X-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTNG-FIM
               MOVE GFCT1X-ERRO        TO GFCTNG-ERRO
               MOVE GFCT1X-COD-SQL-ERRO    TO GFCTNG-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO    TO GFCTNG-COD-MSG-ERRO
               IF  GFCT1X-ERRO         EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO
                                       TO GFCTNG-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006 - ' GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTNG-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2531-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR POSTAL                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2540-SELECIONAR-POSTAL          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF OF GFCTB055.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB055.
           MOVE CAGPTO-CTA OF GFCTB049 TO CAGPTO-CTA   OF GFCTB055.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB055.CEMPR-INC,
                   :GFCTB055.CDEPDC,
                   :GFCTB055.CPOSTO-SERVC
             FROM   DB2PRD.FLEXZ_GRP_PSTAL
             WHERE
                   CSERVC_TARIF       = :GFCTB055.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB055.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB055.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_GRP_PSTAL ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNG-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB055'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTNG-ERRO
               MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
               MOVE 0926               TO GFCTNG-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNG-FIM
               PERFORM 1210-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.


           PERFORM 2541-OBTER-DESC-POSTAL.

           MOVE GFCTGC-DESC-POSTO(01)  TO GFCTNG-DESCR-AGPTO(IND-1)
           MOVE CDEPDC OF GFCTB055     TO GFCTNG-AGENCIA(IND-1).
           MOVE CPOSTO-SERVC           OF GFCTB055
                                       TO GFCTNG-POSTO(IND-1).

      *----------------------------------------------------------------*
       2540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA MODULO GFCT5090                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2541-OBTER-DESC-POSTAL          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTGB-ENTRADA
                      GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.
           MOVE GFCTNF-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTNF-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR
                                          GFCTGB-QTDE-TOT-REG.
           MOVE CDEPDC                 OF GFCTB055
                                       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC           OF GFCTB055
                                       TO GFCTGB-COD-POSTO.
           MOVE GFCTNF-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.
           MOVE 'GFCT5090'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTNG-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0882'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTNG-FIM
               MOVE GFCTGC-ERRO        TO GFCTNG-ERRO
               MOVE GFCTGC-COD-SQL-ERRO    TO GFCTNG-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO    TO GFCTNG-COD-MSG-ERRO
               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTNG-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5090 - ' GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTNG-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2541-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONA CLIENTE                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2550-SELECIONAR-CLIENTE         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF OF GFCTB050.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB050.
           MOVE CAGPTO-CTA OF GFCTB049 TO CAGPTO-CTA   OF GFCTB050.

           EXEC SQL
             SELECT
ST2506*            CCGC_CPF,
ST2506*            CFLIAL_CGC,
ST2506*            CCTRL_CPF_CGC
ST2506             CCGC_CPF_ST,
ST2506             CFLIAL_CGC_ST,
ST2506             CCTRL_CPF_CGC_ST
             INTO
ST2506*            :GFCTB050.CCGC-CPF,
ST2506*            :GFCTB050.CFLIAL-CGC,
ST2506*            :GFCTB050.CCTRL-CPF-CGC
ST2506             :GFCTB050.CCGC-CPF-ST,
ST2506             :GFCTB050.CFLIAL-CGC-ST,
ST2506             :GFCTB050.CCTRL-CPF-CGC-ST
             FROM   DB2PRD.FLEXZ_GRP_CLI
             WHERE
                   CSERVC_TARIF       = :GFCTB050.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB050.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB050.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_GRP_CLI   ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNG-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB050'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTNG-ERRO
               MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
               MOVE 0926               TO GFCTNG-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNG-FIM
               PERFORM 1210-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

ST2506*    M0VE CCGC-CPF OF GFCTB050   TO GFCTNG-CGC-CPF(IND-1)
ST2506*    M0VE CFLIAL-CGC             OF GFCTB050
ST2506*                                TO GFCTNG-FILIAL(IND-1).
ST2506*    M0VE CCTRL-CPF-CGC          OF GFCTB050
ST2506     MOVE CCGC-CPF-ST            OF GFCTB050   
                                       TO GFCTNG-CGC-CPF(IND-1)
ST2506     MOVE CFLIAL-CGC-ST          OF GFCTB050
ST2506                                 TO GFCTNG-FILIAL(IND-1).
ST2506     MOVE CCTRL-CPF-CGC-ST       OF GFCTB050
                                       TO GFCTNG-CONTROLE(IND-1).

      *----------------------------------------------------------------*
       2550-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONA SEGMENTO                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2560-SELECIONAR-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF OF GFCTB056.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB056.
           MOVE CAGPTO-CTA OF GFCTB049 TO CAGPTO-CTA   OF GFCTB056.

           EXEC SQL
             SELECT
                   CSGMTO_GSTAO_TARIF
             INTO
                   :GFCTB056.CSGMTO-GSTAO-TARIF
             FROM   DB2PRD.FLEXZ_GRP_SGMTO
             WHERE
                   CSERVC_TARIF       = :GFCTB056.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB056.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB056.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_GRP_SGMTO ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNG-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB056'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTNG-ERRO
               MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
               MOVE 0926               TO GFCTNG-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNG-FIM
               PERFORM 1210-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CSGMTO-GSTAO-TARIF     OF GFCTB056
                                       TO CSGMTO-GSTAO-TARIF
                                       OF GFCTB0H5
                                          GFCTNG-SEGMENTO(IND-1).

           PERFORM 2561-OBTER-DESC-SEGMENTO.

           MOVE ISGMTO-GSTAO-TARIF     OF GFCTB0H5
                                       TO GFCTNG-DESCR-AGPTO(IND-1).

      *----------------------------------------------------------------*
       2560-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONA DESCRICAO SEGMENTO                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2561-OBTER-DESC-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                   ISGMTO_GSTAO_TARIF
             INTO
                   :GFCTB0H5.ISGMTO-GSTAO-TARIF
             FROM   DB2PRD.TTPO_SGMTO_GSTAO
             WHERE
                   CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNG-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0H5'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTNG-ERRO
               MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
               MOVE 0926               TO GFCTNG-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNG-FIM
               PERFORM 1210-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2561-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONA MUNICIPIO                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2570-SELECIONAR-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF OF GFCTB053.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB053.
           MOVE CAGPTO-CTA OF GFCTB049 TO CAGPTO-CTA   OF GFCTB053.

           EXEC SQL
             SELECT
                   CMUN_IBGE
             INTO
                   :GFCTB053.CMUN-IBGE
             FROM   DB2PRD.FLEXZ_GRP_MUN
             WHERE
                   CSERVC_TARIF       = :GFCTB053.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB053.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB053.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_GRP_MUN   ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNG-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB053'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTNG-ERRO
               MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
               MOVE 0926               TO GFCTNG-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNG-FIM
               PERFORM 1210-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2571-OBTER-DESC-MUNICIPIO.

           MOVE NOME-MUNIC-RURC88      TO GFCTNG-DESCR-AGPTO(IND-1).
           MOVE CMUN-IBGE OF GFCTB053  TO GFCTNG-MUNICIPIO(IND-1).

      *----------------------------------------------------------------*
       2570-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA MODULO RURC9020                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2571-OBTER-DESC-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE AREA-RURC88.

           MOVE CMUN-IBGE              OF GFCTB053
                                       TO COD-MUNIC-RURC88.
           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNG-ERRO
               MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
               MOVE  0732              TO GFCTNG-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 10
               MOVE 1                  TO GFCTNG-ERRO
               MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
               MOVE  0675              TO GFCTNG-COD-MSG-ERRO
               MOVE '0675'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 20
               MOVE 1                  TO GFCTNG-ERRO
               MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
               MOVE  0733              TO GFCTNG-COD-MSG-ERRO
               MOVE '0733'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 99
               MOVE 1                  TO GFCTNG-ERRO
               MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
               MOVE  0734              TO GFCTNG-COD-MSG-ERRO
               MOVE '0734'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2571-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONA UF                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2580-SELECIONAR-UF              SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF OF GFCTB057.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB057.
           MOVE CAGPTO-CTA OF GFCTB049 TO CAGPTO-CTA   OF GFCTB057.

           EXEC SQL
             SELECT
                   CSGL_UF
             INTO
                   :GFCTB057.CSGL-UF
             FROM   DB2PRD.FLEXZ_GRP_UF
             WHERE
                   CSERVC_TARIF       = :GFCTB057.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB057.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB057.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_GRP_UF    ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNG-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB057'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTNG-ERRO
               MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
               MOVE 0926               TO GFCTNG-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNG-FIM
               PERFORM 1210-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2581-OBTER-DESC-UF.

           MOVE WRK-AREA-DCLGEN-DCITV002
                                       TO DCITV002.
           MOVE IUF OF DCITV002        TO GFCTNG-DESCR-AGPTO(IND-1).
           MOVE CSGL-UF    OF DCITV002 TO GFCTNG-UF(IND-1).

      *----------------------------------------------------------------*
       2580-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA MODULO DCIT8000                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2581-OBTER-DESC-UF              SECTION.
      *----------------------------------------------------------------*

           MOVE CSGL-UF                OF GFCTB057
                                       TO CSGL-UF          OF DCITV002.
           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNG-ERRO
               MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
               MOVE  0735              TO GFCTNG-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 1
               MOVE 1                  TO GFCTNG-ERRO
               MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
               MOVE  0736              TO GFCTNG-COD-MSG-ERRO
               MOVE '0736'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 2
               MOVE 1                  TO GFCTNG-ERRO
               MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
               MOVE  0737              TO GFCTNG-COD-MSG-ERRO
               MOVE '0737'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 3
               MOVE 1                  TO GFCTNG-ERRO
               MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
               MOVE  0738              TO GFCTNG-COD-MSG-ERRO
               MOVE '0738'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 4
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'V01_UF'           TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTNG-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 9                  TO GFCTNG-ERRO
               MOVE 'GFCT0882'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-SQLCA          TO GFCT0M-SQLCA-AREA
                                          SQLCA
               MOVE SQLCODE              TO GFCT0M-SQLCODE
HEXA                                        WRK-COD-SQL-ERRO
.              MOVE   WRK-COD-SQL-ERRO-2 TO GFCTNG-COD-SQL-ERRO
               MOVE SPACES               TO GFCTNG-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2581-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

0310  ******************************************************************
0310  * SELECIONAR RAZAO                                               *
0310  ******************************************************************
0310  *----------------------------------------------------------------*
0310   2590-SELECIONAR-RAZAO           SECTION.
0310  *----------------------------------------------------------------*
0310
0310       MOVE CAGPTO-CTA OF GFCTB049 TO CAGPTO-CTA         OF GFCTB0M8
0310       MOVE CSERVC-TARIF           OF GFCTB049
0310                                   TO CSERVC-TARIF       OF GFCTB0M8
0310       MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
0310                                   TO HIDTFD-FLEXZ-AGPTO OF GFCTB0M8
0310
0310       EXEC SQL
0310         SELECT
0310               CGRP_CTBIL_ATUAL,
0310               CSGRP_CTBIL
0310         INTO
0310               :GFCTB0M8.CGRP-CTBIL-ATUAL,
0310               :GFCTB0M8.CSGRP-CTBIL
0310         FROM   DB2PRD.TFLEXZ_GRP_RZ
0310         WHERE
0310               CSERVC_TARIF       = :GFCTB0M8.CSERVC-TARIF       AND
0310               CAGPTO_CTA         = :GFCTB0M8.CAGPTO-CTA         AND
0310               HIDTFD_FLEXZ_AGPTO = :GFCTB0M8.HIDTFD-FLEXZ-AGPTO
0310       END-EXEC.
0310
0310       IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0310          (SQLWARN0                EQUAL 'W'      )
0310           MOVE SPACES             TO GFCT0M-ERRO-SQL
0310           INITIALIZE GFCT0M-ERRO-SQL
0310           MOVE 'TFLEXZ_GRP_RZ   ' TO GFCT0M-NOME-TAB
0310           MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0310           MOVE '0080'             TO GFCT0M-LOCAL
0310           MOVE  0010              TO GFCTNG-COD-MSG-ERRO
0310           MOVE '0010'             TO GFCTG2-COD-MSG
0310           MOVE 'GFCTB0M8'         TO WRK-NOM-TAB
0310           PERFORM 2110-FORMATAR-ERRO-DB2
0310       END-IF.
0310
0310       IF  SQLCODE                 EQUAL +100
0310           MOVE 1                  TO GFCTNG-ERRO
0310           MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
0310           MOVE 0926               TO GFCTNG-COD-MSG-ERRO
0310           MOVE '0926'             TO GFCTG2-COD-MSG
0310           MOVE 'S'                TO GFCTNG-FIM
0310           PERFORM 1210-OBTER-DESC-MSG
0310           MOVE  GFCTG3-DESC-MSG   TO GFCTNG-DESC-MSG-ERRO
0310           PERFORM 3000-FINALIZAR
0310       END-IF.
0310
0510       PERFORM 2595-BUSCAR-RAZAO-CONTABIL
0510
0310       MOVE CGRP-CTBIL-ATUAL       OF GFCTB0M8
HEXA                                   TO WRK-GRUPO-CTBIL
HEXA       MOVE WRK-GRUPO-CTBIL-2      TO GFCTNG-GRUPO-CTBIL (IND-1)
0310       MOVE CSGRP-CTBIL            OF GFCTB0M8
HEXA                                   TO WRK-SGRUPO-CTBIL
HEXA       MOVE WRK-SGRUPO-CTBIL-2     TO GFCTNG-SUB-GRUPO-CTBIL(IND-1)
0510       MOVE IRZ-CTBIL              OF PLCCB004
0310                                   TO GFCTNG-DESC-CTBIL(IND-1).
0310
0310  *----------------------------------------------------------------*
0310   2590-99-FIM.                    EXIT.
0310  *----------------------------------------------------------------*
      ******************************************************************
      *ACESSAR PLCC PARA BUSCAR DESCRICAO RAZAO CONTABIL               *
      ******************************************************************
0510  *----------------------------------------------------------------*
0510   2595-BUSCAR-RAZAO-CONTABIL SECTION.
0510  *----------------------------------------------------------------*
0510
0510       EXEC SQL
0510           SELECT  IRZ_CTBIL
0510           INTO   :IRZ-CTBIL
0510           FROM    DB2PRD.RAZAO_CONTABIL
0510           WHERE   CEMPR = 237
0510           AND     CGRP_CTBIL_ATUAL =:GFCTB0M8.CGRP-CTBIL-ATUAL
0510           AND     CSGRP_CTBIL      =:GFCTB0M8.CSGRP-CTBIL
0510       END-EXEC.
0510
0510       IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0510          (SQLWARN0                EQUAL 'W'      )
0510           INITIALIZE              GFCT0M-ERRO-SQL
0510           MOVE '0010'             TO GFCTG2-COD-MSG
0310           PERFORM 1210-OBTER-DESC-MSG
0510           MOVE 'PLCCB004'      TO WRK-NOM-TAB
0510           MOVE 'RAZAO_CONTABIL   '
0510                                   TO GFCT0M-NOME-TAB
0510           MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0510           MOVE '0075'             TO GFCT0M-LOCAL
0510           MOVE  0010              TO GFCTNG-COD-MSG-ERRO
HEXA           MOVE SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE WRK-COD-SQL-ERRO-2 TO GFCTNG-COD-SQL-ERRO
0510           MOVE  9                 TO GFCTNG-ERRO
0310           PERFORM 2110-FORMATAR-ERRO-DB2
0510           MOVE 'S'                TO GFCTNG-FIM
0510           MOVE GFCTG3-DESC-MSG    TO GFCTNG-DESC-MSG-ERRO
0510           PERFORM 3000-FINALIZAR
0510       END-IF.
0510
0510       IF  SQLCODE                 EQUAL +100
0510           MOVE  1                 TO GFCTNG-ERRO
0510           MOVE ZEROS              TO GFCTNG-COD-SQL-ERRO
0510           MOVE  1690              TO GFCTNG-COD-MSG-ERRO
0510           MOVE '1690'             TO GFCTG2-COD-MSG
0310           PERFORM 1210-OBTER-DESC-MSG
0510           MOVE 'S'                TO GFCTNG-FIM
0510           MOVE GFCTG3-DESC-MSG    TO GFCTNG-DESC-MSG-ERRO
0510           PERFORM 3000-FINALIZAR
0510       END-IF.
0510
0510  *----------------------------------------------------------------*
0510   2595-99-FIM.                    EXIT.
0510  *----------------------------------------------------------------*
      ******************************************************************
      *FECHA CURSOR DA TABELA GFCTB049                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR01-GFCTB049
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_AGPTO_CTA ' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0150'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTNG-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB049'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FINALIZA O PROCESSAMENTO E RETORNA AO PROGRAMA CHAMADOR.        *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
