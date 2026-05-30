      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5169.
       AUTHOR.     HELIO SANTONI NETO.
      *================================================================*
      *      S O N D A   P R O C W O R K   O U T S O U R C I N G.      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5169                                    *
      *    PROGRAMADOR.:   HELIO SANTONI           - PROCWORK - GP.50  *
      *    ANALISTA....:   FRANCISCO LOURENCO      - PROCWORK - GP.50  *
      *    DATA........:   21/12/2009                                  *
      *                                                                *
      *----------------------------------------------------------------*
      *    OBJETIVO     :                                              *
      *      COMBO DE CONJUNTO DE SERVICOS DE FLEXIBILIZACAO PARA      *
      *      AGRUPAMENTO.                                              *
      *                                                                *
      *----------------------------------------------------------------*
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *           DB2PRD.PARM_DATA_PROCM             GFCTB0A1          *
      *           DB2PRD.PARM_SERVC_TARIF            GFCTB0A2          *
      *           DB2PRD.PDIDO_FLEXZ_AG              GFCTB0B1          *
      *           DB2PRD.PDIDO_FLEXZ_AGPTO           GFCTB0B2          *
      *           DB2PRD.PDIDO_FLEXZ_CLI             GFCTB0B3          *
      *           DB2PRD.PDIDO_FLEXZ_MUN             GFCTB0B7          *
      *           DB2PRD.PDIDO_FLEXZ_PAB             GFCTB0B8          *
      *           DB2PRD.PDIDO_FLEXZ_PSTAL           GFCTB0B9          *
      *           DB2PRD.PDIDO_FLEXZ_SGMTO           GFCTB0C0          *
      *           DB2PRD.PDIDO_FLEXZ_UF              GFCTB0C1          *
0410  *           DB2PRD.TPDIDO_TFLEXZ_RZ            GFCTB0M7          *
      *           DB2PRD.CJTO_SERVC_TARIF            GFCTB026          *
      *                                                                *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    GFCTWAJQ - AREA DE COMUNICACAO - ENTRADA                    *
      *    GFCTWAJR - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA   - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *                                                                *
      *----------------------------------------------------------------*
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5522 - VERIFICACAO DE DISPONIBILIDADE ON-LINE         *
      *                                                                *
      *----------------------------------------------------------------*
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7407                                     *
      *                                                                *
      *----------------------------------------------------------------*
0410  *    ULTIMA ALTERACAO EM 30/03/2010 - HELIO SANTONI              *
0410  *    - ACRESCENTAR TRATAMENTO DOS AGRUPAMENTOS 20 E 26.          *
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
           '*  INICIO DA WORKING GFCT5169  *'.
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

       01  WRK-TIPO-PESQUISA           PIC  9(002)         VALUE ZEROS.
       01  WRK-WAJR-QTDE-TOT           PIC  9(009) COMP-3  VALUE ZEROS.
       01  WRK-WAJR-QTDE-OCOR          PIC  9(007) COMP-3  VALUE ZEROS.
       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       01  WRK-NOME-TAB                PIC  X(008)         VALUE SPACES.
       01  WRK-FIM-CURSOR-1            PIC  X(001)         VALUE 'N'.
       01  WRK-FIM-CURSOR-2            PIC  X(001)         VALUE 'N'.
       01  WRK-TEM-FLEXZ-VALIDA        PIC  X(001)         VALUE 'N'.
       01  WRK-CCJTO-SERVC-INI         PIC S9(003) COMP-3  VALUE ZEROS.
       01  WRK-CCJTO-SERVC-FIM         PIC S9(003) COMP-3  VALUE ZEROS.

       01  WRK-CCJTO-SERVC             PIC +9(003)         VALUE ZEROS.
       01  FILLER                      REDEFINES       WRK-CCJTO-SERVC.
           05  FILLER                  PIC  X(001).
           05  WRK-COD-CJTO-SERVC      PIC  9(003).

       01  WRK-AUX-9-S                 PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-AUX-9-S.
           05 WRK-AUX-9                PIC  9(009).
           05 FILLER                   REDEFINES WRK-AUX-9.
              10 FILLER                PIC  X(006).
              10 WRK-AUX-9-X.
                 15 WRK-AUX-9-3        PIC  9(003).

       01  WRK-AUX-3-S                 PIC +9(003)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-3-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-3               PIC  9(003).

       01  WRK-DPROCM-ATUAL-INV        PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-DPROCM-ATUAL-INV.
           05  WRK-ANO-PROCM-ATUAL-INV PIC  9(004).
           05  WRK-MES-PROCM-ATUAL-INV PIC  9(002).
           05  WRK-DIA-PROCM-ATUAL-INV PIC  9(002).

       01  WRK-DINIC-FLEXZ-INV         PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES   WRK-DINIC-FLEXZ-INV.
           05  WRK-ANO-INIC-FLEXZ-INV  PIC  9(004).
           05  WRK-MES-INIC-FLEXZ-INV  PIC  9(002).
           05  WRK-DIA-INIC-FLEXZ-INV  PIC  9(002).

       01  WRK-DFIM-FLEXZ-INV          PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES   WRK-DFIM-FLEXZ-INV.
           05  WRK-ANO-FIM-FLEXZ-INV   PIC  9(004).
           05  WRK-MES-FIM-FLEXZ-INV   PIC  9(002).
           05  WRK-DIA-FIM-FLEXZ-INV   PIC  9(002).

       01  WRK-DATA-DB2.
           03  WRK-DIA-DB2             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '.'.
           03  WRK-MES-DB2             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '.'.
           03  WRK-ANO-DB2             PIC 9(04)           VALUE ZEROS.

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

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           'AREA DE COMUNICACAO COM GFCT5522'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A2
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0B1
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0B2
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0B3
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0B7
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0B8
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0B9
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0C0
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0C1
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB026
           END-EXEC.

110510     EXEC SQL
110510          INCLUDE GFCTB0M7
110510     END-EXEC.

           EXEC SQL
               DECLARE CSR01-GFCTB0B2-JN CURSOR FOR
               SELECT  DISTINCT
                       (T2.CCJTO_SERVC)
               FROM    DB2PRD.PDIDO_FLEXZ_AGPTO T1,
                       DB2PRD.PARM_SERVC_TARIF  T2
               WHERE
                       T1.CAGPTO_CTA        = :GFCTB0B2.CAGPTO-CTA
                 AND   T1.CSERVC_TARIF      =  T2.CSERVC_TARIF
                 AND   T2.CCJTO_SERVC BETWEEN :WRK-CCJTO-SERVC-INI
                                          AND :WRK-CCJTO-SERVC-FIM
            ORDER BY
                       T2.CCJTO_SERVC
           END-EXEC.

           EXEC SQL
               DECLARE CSR01-GFCTB0B3-JN CURSOR FOR
               SELECT  DISTINCT
                       (T2.CCJTO_SERVC)
               FROM    DB2PRD.PDIDO_FLEXZ_CLI  T1,
                       DB2PRD.PARM_SERVC_TARIF T2
               WHERE
                       T1.CAGPTO_CTA        = :GFCTB0B3.CAGPTO-CTA
ST2506*          AND   T1.CCGC_CPF          = :GFCTB0B3.CCGC-CPF
ST2506*          AND   T1.CFLIAL_CGC        = :GFCTB0B3.CFLIAL-CGC
ST2506*          AND   T1.CCTRL_CPF_CGC     = :GFCTB0B3.CCTRL-CPF-CGC
ST2506           AND   T1.CCGC_CPF_ST      = :GFCTB0B3.CCGC-CPF-ST
ST2506           AND   T1.CFLIAL_CGC_ST    = :GFCTB0B3.CFLIAL-CGC-ST
ST2506           AND   T1.CCTRL_CPF_CGC_ST = :GFCTB0B3.CCTRL-CPF-CGC-ST
                 AND   T1.CSERVC_TARIF      =  T2.CSERVC_TARIF
                 AND   T2.CCJTO_SERVC BETWEEN :WRK-CCJTO-SERVC-INI
                                          AND :WRK-CCJTO-SERVC-FIM
            ORDER BY
                       T2.CCJTO_SERVC
           END-EXEC.

           EXEC SQL
               DECLARE CSR01-GFCTB0C0-JN CURSOR FOR
               SELECT  DISTINCT
                       (T2.CCJTO_SERVC)
               FROM    DB2PRD.PDIDO_FLEXZ_SGMTO T1,
                       DB2PRD.PARM_SERVC_TARIF  T2
               WHERE
                       T1.CAGPTO_CTA        = :GFCTB0C0.CAGPTO-CTA
                 AND   T1.CSGMTO_GSTAO_TARIF
                                          = :GFCTB0C0.CSGMTO-GSTAO-TARIF
                 AND   T1.CSERVC_TARIF      =  T2.CSERVC_TARIF
                 AND   T2.CCJTO_SERVC BETWEEN :WRK-CCJTO-SERVC-INI
                                          AND :WRK-CCJTO-SERVC-FIM
            ORDER BY
                       T2.CCJTO_SERVC
           END-EXEC.

           EXEC SQL
               DECLARE CSR01-GFCTB0B1-JN CURSOR FOR
               SELECT  DISTINCT
                       (T2.CCJTO_SERVC)
               FROM    DB2PRD.PDIDO_FLEXZ_AG   T1,
                       DB2PRD.PARM_SERVC_TARIF T2
               WHERE
                       T1.CAGPTO_CTA        = :GFCTB0B1.CAGPTO-CTA
                 AND   T1.CDEPDC            = :GFCTB0B1.CDEPDC
                 AND   T1.CSERVC_TARIF      =  T2.CSERVC_TARIF
                 AND   T2.CCJTO_SERVC BETWEEN :WRK-CCJTO-SERVC-INI
                                          AND :WRK-CCJTO-SERVC-FIM
            ORDER BY
                       T2.CCJTO_SERVC
           END-EXEC.

           EXEC SQL
               DECLARE CSR01-GFCTB0B8-JN CURSOR FOR
               SELECT  DISTINCT
                       (T2.CCJTO_SERVC)
               FROM    DB2PRD.PDIDO_FLEXZ_PAB  T1,
                       DB2PRD.PARM_SERVC_TARIF T2
               WHERE
                       T1.CAGPTO_CTA        = :GFCTB0B8.CAGPTO-CTA
                 AND   T1.CDEPDC            = :GFCTB0B8.CDEPDC
                 AND   T1.CPOSTO_SERVC      = :GFCTB0B8.CPOSTO-SERVC
                 AND   T1.CSERVC_TARIF      =  T2.CSERVC_TARIF
                 AND   T2.CCJTO_SERVC BETWEEN :WRK-CCJTO-SERVC-INI
                                          AND :WRK-CCJTO-SERVC-FIM
            ORDER BY
                       T2.CCJTO_SERVC
           END-EXEC.

           EXEC SQL
               DECLARE CSR01-GFCTB0B9-JN CURSOR FOR
               SELECT  DISTINCT
                       (T2.CCJTO_SERVC)
               FROM    DB2PRD.PDIDO_FLEXZ_PSTAL T1,
                       DB2PRD.PARM_SERVC_TARIF  T2
               WHERE
                       T1.CAGPTO_CTA        = :GFCTB0B9.CAGPTO-CTA
                 AND   T1.CDEPDC            = :GFCTB0B9.CDEPDC
                 AND   T1.CPOSTO_SERVC      = :GFCTB0B9.CPOSTO-SERVC
                 AND   T1.CSERVC_TARIF      =  T2.CSERVC_TARIF
                 AND   T2.CCJTO_SERVC BETWEEN :WRK-CCJTO-SERVC-INI
                                          AND :WRK-CCJTO-SERVC-FIM
            ORDER BY
                       T2.CCJTO_SERVC
           END-EXEC.

           EXEC SQL
               DECLARE CSR01-GFCTB0B7-JN CURSOR FOR
               SELECT  DISTINCT
                       (T2.CCJTO_SERVC)
               FROM    DB2PRD.PDIDO_FLEXZ_MUN  T1,
                       DB2PRD.PARM_SERVC_TARIF T2
               WHERE
                       T1.CAGPTO_CTA        = :GFCTB0B7.CAGPTO-CTA
                 AND   T1.CMUN_IBGE         = :GFCTB0B7.CMUN-IBGE
                 AND   T1.CSERVC_TARIF      =  T2.CSERVC_TARIF
                 AND   T2.CCJTO_SERVC BETWEEN :WRK-CCJTO-SERVC-INI
                                          AND :WRK-CCJTO-SERVC-FIM
            ORDER BY
                       T2.CCJTO_SERVC
           END-EXEC.

           EXEC SQL
               DECLARE CSR01-GFCTB0C1-JN CURSOR FOR
               SELECT  DISTINCT
                       (T2.CCJTO_SERVC)
               FROM    DB2PRD.PDIDO_FLEXZ_UF   T1,
                       DB2PRD.PARM_SERVC_TARIF T2
               WHERE
                       T1.CAGPTO_CTA        = :GFCTB0C1.CAGPTO-CTA
                 AND   T1.CSGL_UF           = :GFCTB0C1.CSGL-UF
                 AND   T1.CSERVC_TARIF      =  T2.CSERVC_TARIF
                 AND   T2.CCJTO_SERVC BETWEEN :WRK-CCJTO-SERVC-INI
                                          AND :WRK-CCJTO-SERVC-FIM
            ORDER BY
                       T2.CCJTO_SERVC
           END-EXEC.

           EXEC SQL
               DECLARE CSR02-GFCTB0B2-JN CURSOR FOR
               SELECT
                       T1.CSERVC_TARIF,
                       T1.DINIC_FLEXZ_AGPTO,
                       T1.DFIM_FLEXZ_AGPTO,
                       T1.CSIT_SOLTC_FLEXZ
               FROM    DB2PRD.PDIDO_FLEXZ_AGPTO T1,
                       DB2PRD.PARM_SERVC_TARIF  T2
               WHERE
                       T1.CAGPTO_CTA        = :GFCTB0B2.CAGPTO-CTA
                 AND   T1.CSERVC_TARIF      =  T2.CSERVC_TARIF
                 AND   T2.CCJTO_SERVC       = :GFCTB0A2.CCJTO-SERVC
            ORDER BY
                       T2.CCJTO_SERVC
           END-EXEC.

0410       EXEC SQL
0410           DECLARE CSR01-GFCTB0M7-JN CURSOR FOR
0410           SELECT  DISTINCT
0410                   (T2.CCJTO_SERVC)
0410           FROM    DB2PRD.TPDIDO_FLEXZ_RZ   T1,
0410                   DB2PRD.PARM_SERVC_TARIF T2
0410           WHERE
0410                   T1.CAGPTO_CTA        = :GFCTB0M7.CAGPTO-CTA
0410             AND   T1.CGRP_CTBIL_ATUAL  = :GFCTB0M7.CGRP-CTBIL-ATUAL
0410             AND   T1.CSGRP_CTBIL       = :GFCTB0M7.CSGRP-CTBIL
0410             AND   T1.CSERVC_TARIF      =  T2.CSERVC_TARIF
0410             AND   T2.CCJTO_SERVC BETWEEN :WRK-CCJTO-SERVC-INI
0410                                      AND :WRK-CCJTO-SERVC-FIM
0410        ORDER BY
0410                   T2.CCJTO_SERVC
0410       END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT5169   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'GFCTWAJQ'.

       COPY 'GFCTWAJR'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING WAJQ-ENTRADA
                                             WAJR-SAIDA
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

           MOVE SPACES                 TO WAJR-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  WAJR-SAIDA
                                       GFCT0M-AREA-ERROS
                                       SQLCA.

           MOVE 350                    TO WAJR-LL
           MOVE WAJQ-TRANSACAO         TO WAJR-TRANSACAO
           MOVE WAJQ-FUNCAO            TO WAJR-FUNCAO
           MOVE WAJQ-FUNC-BDSCO        TO WAJR-FUNC-BDSCO
           MOVE WAJQ-FILTRO            TO WAJR-FILTRO
           MOVE WAJQ-PONTEIRO          TO WAJR-PONTEIRO
           MOVE ZEROS                  TO WAJR-ERRO
                                          WAJR-COD-SQL-ERRO
                                          WAJR-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.
           MOVE ZEROS                  TO WAJR-QTDE-TOT-REG
                                          WRK-WAJR-QTDE-TOT
           MOVE 'N'                    TO WAJR-FIM
                                          WRK-FIM-CURSOR-1
                                          WRK-FIM-CURSOR-2.

           IF  WAJQ-QTDE-OCOR        IS NUMERIC
               MOVE WAJQ-QTDE-OCOR     TO WAJR-QTDE-OCOR
           ELSE
               MOVE ZEROS              TO WAJR-QTDE-OCOR
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
           MOVE WAJQ-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE WAJQ-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE WAJQ-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE SPACES             TO WAJR-DESC-MSG-ERRO
               MOVE 9                  TO WAJR-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5169'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE SPACES         TO WAJR-DESC-MSG-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG3-COD-SQL-ERRO
                                       TO WAJR-COD-SQL-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO
                                       TO WAJR-DESC-MSG-ERRO
               END-IF

               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT5169'         TO GFCT0M-TRANSACAO
               MOVE GFCTG3-ERRO        TO WAJR-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE GFCTG3-ERRO        TO WAJR-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO WAJR-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO WAJR-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO WAJR-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTG3-DESC-MSG        TO WAJR-DESC-MSG-ERRO
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

           IF (WAJQ-TRANSACAO       EQUAL SPACES  OR LOW-VALUES)      OR
              (WAJQ-FUNCAO          EQUAL SPACES  OR LOW-VALUES)      OR
              (WAJQ-FUNC-BDSCO      EQUAL SPACES  OR LOW-VALUES)      OR
              (WAJQ-FIM               NOT EQUAL  'S' AND 'N')         OR
              (WAJQ-FLT-TIPO-HIST     NOT EQUAL  'T' AND 'P' AND 'D') OR
              (WAJQ-FLT-CAGPTO-CTA    NOT NUMERIC)                    OR
              (WAJQ-FLT-CAGPTO-CTA  EQUAL ZEROS  )                    OR
ST2506*       (WAJQ-FLT-CCGC-CPF      NOT NUMERIC)                    OR
ST2506*       (WAJQ-FLT-CFLIAL-CGC    NOT NUMERIC)                    OR
ST2506        (WAJQ-FLT-CCGC-CPF      EQUAL LOW-VALUES )              OR
ST2506        (WAJQ-FLT-CFLIAL-CGC    EQUAL LOW-VALUES)               OR
              (WAJQ-FLT-CCTRL-CPF-CGC NOT NUMERIC)                    OR
              (WAJQ-FLT-CDEPDC        NOT NUMERIC)                    OR
              (WAJQ-FLT-CPOSTO-SERVC  NOT NUMERIC)                    OR
              (WAJQ-FLT-CSGMTO-TARIF  NOT NUMERIC)                    OR
              (WAJQ-FLT-CMUN-IBGE     NOT NUMERIC)                    OR
              (WAJQ-PTR-CCJTO-SERVC   NOT NUMERIC)                    OR
0410          (WAJQ-FLT-GPO-CTBIL NOT NUMERIC)                    OR
0410          (WAJQ-FLT-SUB-GPO-CTBIL NOT NUMERIC)
               MOVE 1                  TO WAJR-ERRO
               MOVE ZEROS              TO WAJR-COD-SQL-ERRO
               MOVE 0001               TO WAJR-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO WAJR-FIM
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
           MOVE WAJQ-TRANSACAO         TO GFCTFZ-TRANSACAO.
           MOVE WAJQ-FUNCAO            TO GFCTFZ-FUNCAO.
           MOVE WAJQ-FUNC-BDSCO        TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.
           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE SPACES             TO WAJR-DESC-MSG-ERRO
               MOVE 9                  TO WAJR-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0198'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             NOT EQUAL ZEROS
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE SPACES         TO WAJR-DESC-MSG-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO WAJR-COD-SQL-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO WAJR-DESC-MSG-ERRO
               END-IF

               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0198'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO WAJR-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO WAJR-ERRO
               MOVE SPACES             TO WAJR-DESC-MSG-ERRO
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

           PERFORM 2100-OBTER-DATA-PROCM

           IF (WAJQ-FLT-CAGPTO-CTA   EQUAL 04       AND
               WAJQ-FLT-CCGC-CPF     EQUAL ZEROS)    OR
              (WAJQ-FLT-CAGPTO-CTA   EQUAL 09       AND
               WAJQ-FLT-CSGMTO-TARIF EQUAL ZEROS)    OR
              (WAJQ-FLT-CAGPTO-CTA   EQUAL 14       AND
               WAJQ-FLT-CDEPDC       EQUAL ZEROS)    OR
              (WAJQ-FLT-CAGPTO-CTA   EQUAL 15       AND
               WAJQ-FLT-CDEPDC       EQUAL ZEROS    AND
               WAJQ-FLT-CPOSTO-SERVC EQUAL ZEROS)    OR
              (WAJQ-FLT-CAGPTO-CTA   EQUAL 17       AND
               WAJQ-FLT-CDEPDC       EQUAL ZEROS    AND
               WAJQ-FLT-CPOSTO-SERVC EQUAL ZEROS)    OR
              (WAJQ-FLT-CAGPTO-CTA   EQUAL 18       AND
               WAJQ-FLT-CMUN-IBGE    EQUAL ZEROS)    OR
              (WAJQ-FLT-CAGPTO-CTA   EQUAL 19       AND
               WAJQ-FLT-CSGL-UF      EQUAL SPACES)  OR
0410          (WAJQ-FLT-CAGPTO-CTA   EQUAL 20       AND
0410           WAJQ-FLT-CDEPDC       EQUAL ZEROS    AND
0410           WAJQ-FLT-CPOSTO-SERVC EQUAL ZEROS)    OR
0410          (WAJQ-FLT-CAGPTO-CTA   EQUAL 26       AND
0410           WAJQ-FLT-GPO-CTBIL    EQUAL ZEROS)
               MOVE 1                  TO WRK-TIPO-PESQUISA
           END-IF.

           IF (WAJQ-FLT-CAGPTO-CTA       EQUAL 04       AND
              WAJQ-FLT-CCGC-CPF      NOT EQUAL SPACES)
              MOVE 2                  TO WRK-TIPO-PESQUISA
           END-IF.

           IF (WAJQ-FLT-CAGPTO-CTA       EQUAL 09       AND
              WAJQ-FLT-CSGMTO-TARIF  NOT EQUAL ZEROS)
              MOVE 3                  TO WRK-TIPO-PESQUISA
           END-IF.

           IF WAJQ-FLT-CAGPTO-CTA        EQUAL 14       AND
              WAJQ-FLT-CDEPDC        NOT EQUAL ZEROS
              MOVE 4                  TO WRK-TIPO-PESQUISA
           END-IF.

           IF WAJQ-FLT-CAGPTO-CTA        EQUAL 15       AND
              WAJQ-FLT-CDEPDC        NOT EQUAL ZEROS    AND
              WAJQ-FLT-CPOSTO-SERVC  NOT EQUAL ZEROS
              MOVE 5                  TO WRK-TIPO-PESQUISA
           END-IF.

           IF WAJQ-FLT-CAGPTO-CTA        EQUAL 17       AND
              WAJQ-FLT-CDEPDC        NOT EQUAL ZEROS    AND
              WAJQ-FLT-CPOSTO-SERVC  NOT EQUAL ZEROS
              MOVE 6                  TO WRK-TIPO-PESQUISA
           END-IF.

           IF WAJQ-FLT-CAGPTO-CTA        EQUAL 18       AND
              WAJQ-FLT-CMUN-IBGE     NOT EQUAL ZEROS
              MOVE 7                  TO WRK-TIPO-PESQUISA
           END-IF.

           IF WAJQ-FLT-CAGPTO-CTA        EQUAL 19       AND
              WAJQ-FLT-CSGL-UF       NOT EQUAL SPACES
              MOVE 8                  TO WRK-TIPO-PESQUISA
           END-IF.

0410       IF WAJQ-FLT-CAGPTO-CTA        EQUAL 20       AND
0410          WAJQ-FLT-CDEPDC        NOT EQUAL ZEROS    AND
0410          WAJQ-FLT-CPOSTO-SERVC  NOT EQUAL ZEROS
0410          MOVE 6                  TO WRK-TIPO-PESQUISA
0410       END-IF.
0410
0410       IF WAJQ-FLT-CAGPTO-CTA        EQUAL 26       AND
0410          WAJQ-FLT-GPO-CTBIL  NOT EQUAL ZEROS
0410          MOVE 9                  TO WRK-TIPO-PESQUISA
0410       END-IF.

           PERFORM 2200-ABRIR-CURSOR

           PERFORM 2300-LER-CURSOR

           MOVE ZEROS                  TO IND-1

           PERFORM 2400-PROCESSAR-CURSOR-1
                                  UNTIL WRK-FIM-CURSOR-1  EQUAL  'S'.

           PERFORM 2500-FECHAR-CURSOR.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATAR DATA DE REFERENCIA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-OBTER-DATA-PROCM           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

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
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAJR-COD-SQL-ERRO
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE 0012               TO WAJR-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB

               PERFORM 2110-MOVER-ERRO-SQL

               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  DPROCM-ATUAL          TO WRK-DATA-DB2
           MOVE  WRK-ANO-DB2           TO WRK-ANO-PROCM-ATUAL-INV
           MOVE  WRK-MES-DB2           TO WRK-MES-PROCM-ATUAL-INV
           MOVE  WRK-DIA-DB2           TO WRK-DIA-PROCM-ATUAL-INV.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  MOVER CAMPOS PARA ERRO DB2                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5169'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE 9                      TO WAJR-ERRO.

           PERFORM 1100-OBTER-DESC-MSG.

           MOVE SPACES                 TO WAJR-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG   ' - '
                  WRK-NOME-TAB
           DELIMITED BY '   '          INTO WAJR-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATAR ABERTURA DE CURSORES                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           IF  WAJQ-PTR-CCJTO-SERVC  EQUAL ZEROS
               MOVE 1                  TO WRK-CCJTO-SERVC-INI
           ELSE
               MOVE WAJQ-PTR-CCJTO-SERVC
                                       TO WRK-CCJTO-SERVC-INI
           END-IF.

           MOVE  999                   TO WRK-CCJTO-SERVC-FIM


IN         IF WRK-TIPO-PESQUISA NOT NUMERIC OR
              WRK-TIPO-PESQUISA EQUAL ZEROS

               MOVE  8                 TO WAJR-ERRO
               MOVE ZEROS              TO WAJR-COD-SQL-ERRO
               MOVE 0001               TO WAJR-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO WAJR-FIM
               PERFORM 1100-OBTER-DESC-MSG
               PERFORM 3000-FINALIZAR
IN         END-IF.



           EVALUATE WRK-TIPO-PESQUISA
               WHEN 1
                    MOVE WAJQ-FLT-CAGPTO-CTA
                                       TO CAGPTO-CTA OF GFCTB0B2
                    EXEC SQL
                         OPEN CSR01-GFCTB0B2-JN
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_AGPTO'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0B2'    TO WRK-NOME-TAB

               WHEN 2
                    MOVE WAJQ-FLT-CAGPTO-CTA
                                       TO CAGPTO-CTA     OF GFCTB0B3
ST2605*             MOVE WAJQ-FLT-CCGC-CPF
ST2605*                                TO CCGC-CPF       OF GFCTB0B3
ST2605*             MOVE WAJQ-FLT-CFLIAL-CGC
ST2605*                                TO CFLIAL-CGC     OF GFCTB0B3
ST2605*             MOVE WAJQ-FLT-CCTRL-CPF-CGC
ST2605*                                TO CCTRL-CPF-CGC  OF GFCTB0B3
ST2605              MOVE WAJQ-FLT-CCGC-CPF
ST2605                                 TO CCGC-CPF-ST       OF GFCTB0B3
ST2605              MOVE WAJQ-FLT-CFLIAL-CGC
ST2605                                 TO CFLIAL-CGC-ST     OF GFCTB0B3
ST2605              MOVE WAJQ-FLT-CCTRL-CPF-CGC
ST2605                                 TO CCTRL-CPF-CGC-ST  OF GFCTB0B3
                    EXEC SQL
                         OPEN CSR01-GFCTB0B3-JN
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_CLI'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0B3'    TO WRK-NOME-TAB

               WHEN 3
                    MOVE WAJQ-FLT-CAGPTO-CTA
                                       TO CAGPTO-CTA OF GFCTB0C0
                    MOVE WAJQ-FLT-CSGMTO-TARIF
                                       TO CSGMTO-GSTAO-TARIF OF GFCTB0C0
                    EXEC SQL
                         OPEN CSR01-GFCTB0C0-JN
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_SGMTO'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0C0'    TO WRK-NOME-TAB

               WHEN 4
                    MOVE WAJQ-FLT-CAGPTO-CTA
                                       TO CAGPTO-CTA OF GFCTB0B1
                    MOVE WAJQ-FLT-CDEPDC
                                       TO CDEPDC     OF GFCTB0B1
                    EXEC SQL
                         OPEN CSR01-GFCTB0B1-JN
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_AG'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0B1'    TO WRK-NOME-TAB

               WHEN 5
                    MOVE WAJQ-FLT-CAGPTO-CTA
                                       TO CAGPTO-CTA OF GFCTB0B8
                    MOVE WAJQ-FLT-CDEPDC
                                       TO CDEPDC     OF GFCTB0B8
                    MOVE WAJQ-FLT-CPOSTO-SERVC
                                       TO CPOSTO-SERVC OF GFCTB0B8
                    EXEC SQL
                         OPEN CSR01-GFCTB0B8-JN
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_PAB'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0B8'    TO WRK-NOME-TAB

               WHEN 6
                    MOVE WAJQ-FLT-CAGPTO-CTA
                                       TO CAGPTO-CTA OF GFCTB0B9
                    MOVE WAJQ-FLT-CDEPDC
                                       TO CDEPDC     OF GFCTB0B9
                    MOVE WAJQ-FLT-CPOSTO-SERVC
                                       TO CPOSTO-SERVC OF GFCTB0B9
                    EXEC SQL
                         OPEN CSR01-GFCTB0B9-JN
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_PSTAL'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0B9'    TO WRK-NOME-TAB

               WHEN 7
                    MOVE WAJQ-FLT-CAGPTO-CTA
                                       TO CAGPTO-CTA OF GFCTB0B7
                    MOVE WAJQ-FLT-CMUN-IBGE
                                       TO CMUN-IBGE OF GFCTB0B7
                    EXEC SQL
                         OPEN CSR01-GFCTB0B7-JN
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_MUN'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0B7'    TO WRK-NOME-TAB

               WHEN 8
                    MOVE WAJQ-FLT-CAGPTO-CTA
                                       TO CAGPTO-CTA OF GFCTB0C1
                    MOVE WAJQ-FLT-CSGL-UF
                                       TO CSGL-UF    OF GFCTB0C1
                    EXEC SQL
                         OPEN CSR01-GFCTB0C1-JN
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_UF'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0C1'    TO WRK-NOME-TAB

0410           WHEN 9
0410                MOVE WAJQ-FLT-CAGPTO-CTA
0410                                   TO CAGPTO-CTA        OF GFCTB0M7
0410                MOVE WAJQ-FLT-GPO-CTBIL
0410                                   TO CGRP-CTBIL-ATUAL  OF GFCTB0M7
0410                MOVE WAJQ-FLT-SUB-GPO-CTBIL
0410                                   TO CSGRP-CTBIL       OF GFCTB0M7
0410                EXEC SQL
0410                     OPEN CSR01-GFCTB0M7-JN
0410                END-EXEC
0410
0410                MOVE 'TPDIDO_FLEXZ_RZ'
0410                                   TO GFCT0M-NOME-TAB
0410                MOVE 'GFCTB0M7'    TO WRK-NOME-TAB

           END-EVALUATE.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAJR-COD-SQL-ERRO
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 0005               TO WAJR-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG

               PERFORM 2110-MOVER-ERRO-SQL

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATAR ABERTURA DE CURSORES                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*

           EVALUATE WRK-TIPO-PESQUISA
               WHEN 1
                    EXEC SQL
                         FETCH   CSR01-GFCTB0B2-JN
                         INTO    :GFCTB0A2.CCJTO-SERVC
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_AGPTO'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0B2'    TO WRK-NOME-TAB

               WHEN 2
                    EXEC SQL
                         FETCH   CSR01-GFCTB0B3-JN
                         INTO    :GFCTB0A2.CCJTO-SERVC
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_CLI'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0B3'    TO WRK-NOME-TAB

               WHEN 3
                    EXEC SQL
                         FETCH   CSR01-GFCTB0C0-JN
                         INTO    :GFCTB0A2.CCJTO-SERVC
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_SGMTO'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0C0'    TO WRK-NOME-TAB

               WHEN 4
                    EXEC SQL
                         FETCH   CSR01-GFCTB0B1-JN
                         INTO    :GFCTB0A2.CCJTO-SERVC
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_AG'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0B1'    TO WRK-NOME-TAB

               WHEN 5
                    EXEC SQL
                         FETCH   CSR01-GFCTB0B8-JN
                         INTO    :GFCTB0A2.CCJTO-SERVC
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_PAB'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0B8'    TO WRK-NOME-TAB

               WHEN 6
                    EXEC SQL
                         FETCH   CSR01-GFCTB0B9-JN
                         INTO    :GFCTB0A2.CCJTO-SERVC
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_PSTAL'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0B9'    TO WRK-NOME-TAB

               WHEN 7
                    EXEC SQL
                         FETCH   CSR01-GFCTB0B7-JN
                         INTO    :GFCTB0A2.CCJTO-SERVC
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_MUN'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0B7'    TO WRK-NOME-TAB

               WHEN 8
                    EXEC SQL
                         FETCH   CSR01-GFCTB0C1-JN
                         INTO    :GFCTB0A2.CCJTO-SERVC
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_UF'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0C1'    TO WRK-NOME-TAB

0410           WHEN 9
0410                EXEC SQL
0410                     FETCH   CSR01-GFCTB0M7-JN
0410                     INTO    :GFCTB0A2.CCJTO-SERVC
0410                END-EXEC
0410
0410                MOVE 'TPDIDO_FLEXZ_RZ'
0410                                   TO GFCT0M-NOME-TAB
0410                MOVE 'GFCTB0M7'    TO WRK-NOME-TAB

           END-EVALUATE.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'FETCH '           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAJR-COD-SQL-ERRO
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE 0006               TO WAJR-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG

               PERFORM 2110-MOVER-ERRO-SQL

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR-1
                                          WAJR-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO WAJR-ERRO
                   MOVE ZEROS          TO WAJR-COD-SQL-ERRO
                   MOVE 1632           TO WAJR-COD-MSG-ERRO
                   MOVE '1632'         TO GFCTG2-COD-MSG

                   PERFORM 1100-OBTER-DESC-MSG

                   MOVE 'S'            TO WAJR-FIM

                   PERFORM 3000-FINALIZAR
               END-IF
           ELSE
               MOVE CCJTO-SERVC        OF GFCTB0A2
                                       TO WRK-CCJTO-SERVC
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  PROCESSAR CURSOR CSR01                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-PROCESSAR-CURSOR-1         SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CURSOR-2

           PERFORM 2410-ABRIR-CSR02

           PERFORM 2420-LER-CSR02

           MOVE  'N'                   TO WRK-TEM-FLEXZ-VALIDA

           PERFORM 2430-PROCESSAR-CURSOR-2
                             UNTIL WRK-FIM-CURSOR-2      EQUAL 'S'
                                OR WRK-TEM-FLEXZ-VALIDA  EQUAL 'S'.

           PERFORM 2440-FECHAR-CSR02

           PERFORM 2300-LER-CURSOR

           IF  WRK-FIM-CURSOR-1     EQUAL 'N'
               IF  IND-1            EQUAL 20
                   MOVE 'S'            TO WRK-FIM-CURSOR-1

                   MOVE CCJTO-SERVC    OF GFCTB0A2
                                       TO WRK-CCJTO-SERVC
                   MOVE WRK-COD-CJTO-SERVC
                                       TO WAJR-PTR-CCJTO-SERVC
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ABRIR CURSOR CSR02                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-ABRIR-CSR02                SECTION.
      *----------------------------------------------------------------*

           MOVE WAJQ-FLT-CAGPTO-CTA    TO CAGPTO-CTA  OF GFCTB0B2
           MOVE WRK-CCJTO-SERVC        TO CCJTO-SERVC OF GFCTB0A2

           EXEC SQL
               OPEN CSR02-GFCTB0B2-JN
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'PDIDO_FLEXZ_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAJR-COD-SQL-ERRO
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE 0005               TO WAJR-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0B2'         TO WRK-NOME-TAB

               PERFORM 2110-MOVER-ERRO-SQL

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  LER CURSOR CSR02                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2420-LER-CSR02                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR02-GFCTB0B2-JN
               INTO    :GFCTB0B2.CSERVC-TARIF,
                       :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                       :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                       :GFCTB0B2.CSIT-SOLTC-FLEXZ
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'PDIDO_FLEXZ_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAJR-COD-SQL-ERRO
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE 0006               TO WAJR-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0B2'         TO WRK-NOME-TAB

               PERFORM 2110-MOVER-ERRO-SQL

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR-2
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  PROCESSAR CURSOR CSR02                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2430-PROCESSAR-CURSOR-2         SECTION.
      *----------------------------------------------------------------*

           IF  WAJQ-FLT-TIPO-HIST         EQUAL 'T'
               MOVE  'S'               TO WRK-TEM-FLEXZ-VALIDA

               MOVE WAJR-QTDE-OCOR     TO WRK-WAJR-QTDE-OCOR
               ADD 1                   TO IND-1
                                          WRK-WAJR-QTDE-TOT
                                          WRK-WAJR-QTDE-OCOR
               MOVE WRK-WAJR-QTDE-OCOR TO WAJR-QTDE-OCOR
               MOVE WRK-WAJR-QTDE-TOT  TO WAJR-QTDE-TOT-REG

               ADD 33                  TO WAJR-LL

               MOVE CCJTO-SERVC        OF GFCTB0A2
                                       TO WRK-AUX-3-S
               MOVE WRK-AUX-3          TO WAJR-COD-CONJ-SERVC (IND-1)

               PERFORM 2431-PESQUISAR-CONJ-SERVC
           ELSE
            IF  WAJQ-FLT-TIPO-HIST        EQUAL 'P'
               IF  CSIT-SOLTC-FLEXZ    OF GFCTB0B2
                                          EQUAL 01
                   MOVE  'S'           TO WRK-TEM-FLEXZ-VALIDA

                   MOVE WAJR-QTDE-OCOR TO WRK-WAJR-QTDE-OCOR
                   ADD 1               TO IND-1
                                          WRK-WAJR-QTDE-TOT
                                          WRK-WAJR-QTDE-OCOR
                   MOVE WRK-WAJR-QTDE-OCOR
                                       TO WAJR-QTDE-OCOR
                   MOVE WRK-WAJR-QTDE-TOT
                                       TO WAJR-QTDE-TOT-REG

                   ADD 33              TO WAJR-LL

                   MOVE CCJTO-SERVC    OF GFCTB0A2
                                       TO WRK-AUX-3-S
                   MOVE WRK-AUX-3      TO WAJR-COD-CONJ-SERVC (IND-1)

                   PERFORM 2431-PESQUISAR-CONJ-SERVC
               END-IF
            ELSE
             IF  WAJQ-FLT-TIPO-HIST    EQUAL 'D'
               MOVE  DINIC-FLEXZ-AGPTO TO WRK-DATA-DB2
               MOVE  WRK-ANO-DB2       TO WRK-ANO-INIC-FLEXZ-INV
               MOVE  WRK-MES-DB2       TO WRK-MES-INIC-FLEXZ-INV
               MOVE  WRK-DIA-DB2       TO WRK-DIA-INIC-FLEXZ-INV
               MOVE  DFIM-FLEXZ-AGPTO  TO WRK-DATA-DB2
               MOVE  WRK-ANO-DB2       TO WRK-ANO-FIM-FLEXZ-INV
               MOVE  WRK-MES-DB2       TO WRK-MES-FIM-FLEXZ-INV
               MOVE  WRK-DIA-DB2       TO WRK-DIA-FIM-FLEXZ-INV

               IF  WRK-DINIC-FLEXZ-INV GREATER WRK-DPROCM-ATUAL-INV AND
                   CSIT-SOLTC-FLEXZ OF GFCTB0B2
                                       EQUAL 02
                   MOVE  'S'           TO WRK-TEM-FLEXZ-VALIDA

                   MOVE WAJR-QTDE-OCOR TO WRK-WAJR-QTDE-OCOR
                   ADD 1               TO IND-1
                                          WRK-WAJR-QTDE-TOT
                                          WRK-WAJR-QTDE-OCOR
                   MOVE WRK-WAJR-QTDE-OCOR
                                       TO WAJR-QTDE-OCOR
                   MOVE WRK-WAJR-QTDE-TOT
                                       TO WAJR-QTDE-TOT-REG

                   ADD 33              TO WAJR-LL

                   MOVE CCJTO-SERVC    OF GFCTB0A2
                                       TO WRK-AUX-3-S
                   MOVE WRK-AUX-3      TO WAJR-COD-CONJ-SERVC (IND-1)

                   PERFORM 2431-PESQUISAR-CONJ-SERVC
               END-IF
             END-IF
            END-IF
           END-IF.

           PERFORM 2420-LER-CSR02

           IF  WRK-FIM-CURSOR-2     EQUAL 'N'
               IF  IND-1            EQUAL 20
                   MOVE  'S'           TO WRK-FIM-CURSOR-2
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  PESQUISAR NOME DO CONJUNTO DE SERVICOS                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2431-PESQUISAR-CONJ-SERVC       SECTION.
      *----------------------------------------------------------------*

           MOVE CCJTO-SERVC            OF GFCTB0A2
                                       TO CCJTO-SERVC OF GFCTB026

           EXEC SQL
             SELECT  RCJTO_SERVC
             INTO    :GFCTB026.RCJTO-SERVC
             FROM    DB2PRD.CJTO_SERVC_TARIF
             WHERE   CCJTO_SERVC        = :GFCTB026.CCJTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS ) OR
              (SQLWARN0                EQUAL 'W'       )
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'CJTO_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAJR-COD-SQL-ERRO
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE 0012               TO WAJR-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB026'         TO WRK-NOME-TAB

               PERFORM 2110-MOVER-ERRO-SQL

               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE RCJTO-SERVC            OF GFCTB026
                                       TO WAJR-DESCR-CONJ-SERVC(IND-1).

      *----------------------------------------------------------------*
       2431-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  FECHAR CURSOR CSR02                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2440-FECHAR-CSR02               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR02-GFCTB0B2-JN
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'PDIDO_FLEXZ_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAJR-COD-SQL-ERRO
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE 0011               TO WAJR-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L4'         TO WRK-NOME-TAB

               PERFORM 2110-MOVER-ERRO-SQL

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATAR FECHAMENTO DE CURSORES                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EVALUATE WRK-TIPO-PESQUISA
               WHEN 1
                    EXEC SQL
                         CLOSE CSR01-GFCTB0B2-JN
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_AGPTO'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0B2'    TO WRK-NOME-TAB

               WHEN 2
                    EXEC SQL
                         CLOSE CSR01-GFCTB0B3-JN
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_CLI'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0B3'    TO WRK-NOME-TAB

               WHEN 3
                    EXEC SQL
                         CLOSE CSR01-GFCTB0C0-JN
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_SGMTO'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0C0'    TO WRK-NOME-TAB

               WHEN 4
                    EXEC SQL
                         CLOSE CSR01-GFCTB0B1-JN
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_AG'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0B1'    TO WRK-NOME-TAB

               WHEN 5
                    EXEC SQL
                         CLOSE CSR01-GFCTB0B8-JN
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_PAB'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0B8'    TO WRK-NOME-TAB

               WHEN 6
                    EXEC SQL
                         CLOSE CSR01-GFCTB0B9-JN
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_PSTAL'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0B9'    TO WRK-NOME-TAB

               WHEN 7
                    EXEC SQL
                         CLOSE CSR01-GFCTB0B7-JN
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_MUN'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0B7'    TO WRK-NOME-TAB

               WHEN 8
                    EXEC SQL
                         CLOSE CSR01-GFCTB0C1-JN
                    END-EXEC

                    MOVE 'PDIDO_FLEXZ_UF'
                                       TO GFCT0M-NOME-TAB
                    MOVE 'GFCTB0C1'    TO WRK-NOME-TAB

0410           WHEN 9
0410                EXEC SQL
0410                     CLOSE CSR01-GFCTB0M7-JN
0410                END-EXEC
0410
0410                MOVE 'TPDIDO_FLEXZ_RZ'
0410                                   TO GFCT0M-NOME-TAB
0410                MOVE 'GFCTB0M7'    TO WRK-NOME-TAB

           END-EVALUATE.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAJR-COD-SQL-ERRO
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE 0011               TO WAJR-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG

               PERFORM 2110-MOVER-ERRO-SQL

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
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
      *----------------------------------------------------------------*
