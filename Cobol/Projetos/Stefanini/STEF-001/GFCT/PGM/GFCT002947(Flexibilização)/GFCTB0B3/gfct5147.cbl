      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5147.
       AUTHOR.     DIEGO DE SOUZA.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5147                                    *
      *    PROGRAMADOR.:   DIEGO DE SOUZA            - CPM PATO BRANCO *
      *    ANALISTA CPM:   ELIAS AUGUSTO BOSCATO     - CPM PATO BRANCO *
      *    ANALISTA....:   RICARDO PEREIRA           - PROCWORK /GP.50 *
      *    DATA........:   05/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR DADOS PARA COMBO DE TARIFA COM        *
      *      FLEXIBILIZACAO.                                           *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.PDIDO_FLEXZ_AGPTO      GFCTB0B2       *
      *                   DB2PRD.FLEXZ_AGPTO_CTA        GFCTB049       *
      *                   DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8       *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
090807*                   DB2PRD.TPRMSS_FLEXZ_AGPTO     GFCTB0J7       *
110510*                   DB2PRD.TPDIDO_FLEXZ_RZ        GFCTB0M7       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTOO - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTOP - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA   - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *    POOL7600 - OBTAR DATA E HORA DO SISTEMA.                    *
      *================================================================*
      *    ULTIMA ALTERACAO EM 09/07/2008 - VALERIA                    *
      *  - INCLUSAO DOS CAMPOS PERIODO DE COBRANCA E INICIO DO PERIODO *
      *    DE COBRANCA.                                                *
      *  - INCLUSAO DA TABELAS DE PERMISSAO NA MONTAGEM DA COMBO       *
      *    DE TARIFAS COM PERMISSAO NA OPCAO DE RENOVACAO              *
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

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-NOM-TAB                 PIC  X(008)         VALUE SPACES.


       77  WRK-GFCTOP-QTDE-TOT-REG     PIC  9(009)  COMP-3 VALUE ZEROS.

090708 77  WRK-TEM-PERMSS              PIC  X(001)         VALUE SPACES.

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-FINALIZAR               PIC  X(001)         VALUE SPACES.

       01  WRK-CSIT                    PIC S9(002)V COMP-3 VALUE ZEROS.
       01  WRK-CSIT-AUX                PIC S9(002)V COMP-3 VALUE ZEROS.

       01  WRK-MSG05.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO'.
           05  WRK-NOME-MODULO         PIC  X(008)         VALUE SPACES.

110510 01  WRK-CSERVC-TARIF-INI        PIC S9(005) COMP-3  VALUE ZEROS.
110510 01  WRK-CSERVC-TARIF-FIM        PIC S9(005) COMP-3  VALUE ZEROS.

HEXA   01  WRK-QTDE-TOT-REG            PIC  9(009) COMP-3  VALUE ZEROS.

0710   01  WRK-DPROCM-ATUAL-INV        PIC  9(008)         VALUE ZEROS.
.      01  FILLER                      REDEFINES  WRK-DPROCM-ATUAL-INV.
.          05  WRK-ANO-PROCM-ATUAL-INV PIC  9(004).
.          05  WRK-MES-PROCM-ATUAL-INV PIC  9(002).
.          05  WRK-DIA-PROCM-ATUAL-INV PIC  9(002).
.
.      01  WRK-DINIC-FLEXZ-INV         PIC  9(008)         VALUE ZEROS.
.      01  FILLER                      REDEFINES   WRK-DINIC-FLEXZ-INV.
.          05  WRK-ANO-INIC-FLEXZ-INV  PIC  9(004).
.          05  WRK-MES-INIC-FLEXZ-INV  PIC  9(002).
.          05  WRK-DIA-INIC-FLEXZ-INV  PIC  9(002).
.
.      01  WRK-DATA-DB2.
.          03  WRK-DIA-DB2             PIC 9(02)           VALUE ZEROS.
.          03  FILLER                  PIC X(01)           VALUE '.'.
.          03  WRK-MES-DB2             PIC 9(02)           VALUE ZEROS.
.          03  FILLER                  PIC X(01)           VALUE '.'.
.          03  WRK-ANO-DB2             PIC 9(04)           VALUE ZEROS.
.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
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
           '* AREA PARA TABELA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B9
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B3
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B7
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB049
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

090708     EXEC SQL
090708       INCLUDE GFCTB0J7
090708     END-EXEC.

110510     EXEC SQL
110510       INCLUDE GFCTB0M7
110510     END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSORES *'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-GFCTB0B2 CURSOR FOR
               SELECT
                   DISTINCT CSERVC_TARIF
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO
             WHERE
                   CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA         AND
                  (DINIC_FLEXZ_AGPTO <= :GFCTB0B2.DINIC-FLEXZ-AGPTO  AND
                   DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)  AND
                  (CSIT_SOLTC_FLEXZ   = :WRK-CSIT                    OR
                   CSIT_SOLTC_FLEXZ   = :WRK-CSIT-AUX)               AND
                   CSERVC_TARIF      >= :GFCTB0B2.CSERVC-TARIF
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR02-GFCTB0B2 CURSOR FOR
               SELECT
                   DISTINCT CSERVC_TARIF
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO
             WHERE
                   CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
             AND  (CSIT_SOLTC_FLEXZ   =  01
              OR  (CSIT_SOLTC_FLEXZ  IN (02)
             AND   DINIC_FLEXZ_AGPTO  > :GFCTB0B2.DINIC-FLEXZ-AGPTO))
             AND   CSERVC_TARIF      >= :GFCTB0B2.CSERVC-TARIF
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR03-GFCTB049 CURSOR FOR
               SELECT
                   DISTINCT CSERVC_TARIF
             FROM  DB2PRD.FLEXZ_AGPTO_CTA
             WHERE
                   CAGPTO_CTA         = :GFCTB049.CAGPTO-CTA         AND
                   DINIC_FLEXZ_AGPTO <= :GFCTB049.DINIC-FLEXZ-AGPTO  AND
                   DFIM_FLEXZ_AGPTO  >= :GFCTB049.DFIM-FLEXZ-AGPTO   AND
                   CSERVC_TARIF      >= :GFCTB049.CSERVC-TARIF       AND
                   CDEPDC_ENCRR_VGCIA = 0                            AND
                   CFUNC_ENCRR_VGCIA  = 0
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR04-GFCTB0B2 CURSOR FOR
               SELECT
                   DISTINCT CSERVC_TARIF
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO
             WHERE
                   CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA         AND
                   CSERVC_TARIF      >= :GFCTB0B2.CSERVC-TARIF
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR05-GFCTB0B2 CURSOR FOR
               SELECT
                   DISTINCT CSERVC_TARIF
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO
             WHERE
                   CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA         AND
                  (CSIT_SOLTC_FLEXZ   = :WRK-CSIT                    OR
                   CSIT_SOLTC_FLEXZ   = :WRK-CSIT-AUX)               AND
                   CSERVC_TARIF      >= :GFCTB0B2.CSERVC-TARIF
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR06-GFCTB0B2 CURSOR FOR
               SELECT
                   DISTINCT CSERVC_TARIF
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO
             WHERE
                   CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA         AND
                  (DFIM_FLEXZ_AGPTO   < :GFCTB0B2.DFIM-FLEXZ-AGPTO   OR
                   DINIC_FLEXZ_AGPTO  > :GFCTB0B2.DINIC-FLEXZ-AGPTO) AND
                   CSIT_SOLTC_FLEXZ   = :WRK-CSIT                    AND
                   CSERVC_TARIF      >= :GFCTB0B2.CSERVC-TARIF
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR07-GFCTB049 CURSOR FOR
               SELECT
                   DISTINCT CSERVC_TARIF
             FROM  DB2PRD.FLEXZ_AGPTO_CTA
             WHERE
                   CAGPTO_CTA         = :GFCTB049.CAGPTO-CTA
             AND   CSERVC_TARIF      >= :GFCTB049.CSERVC-TARIF
             AND  (DINIC_FLEXZ_AGPTO <= :GFCTB049.DINIC-FLEXZ-AGPTO
             AND   DFIM_FLEXZ_AGPTO  >= :GFCTB049.DFIM-FLEXZ-AGPTO)
             AND   CDEPDC_ENCRR_VGCIA = 0
             AND   CFUNC_ENCRR_VGCIA  = 0
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR08-GFCTB0B2 CURSOR FOR
               SELECT
                   DISTINCT A.CSERVC_TARIF
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO A,
                   DB2PRD.PDIDO_FLEXZ_PAB   B
             WHERE
              A.CAGPTO_CTA           = B.CAGPTO_CTA                  AND
              A.CSERVC_TARIF         = B.CSERVC_TARIF                AND
              A.HIDTFD_FLEXZ_AGPTO   = B.HIDTFD_FLEXZ_AGPTO          AND
              B.CAGPTO_CTA           = :GFCTB0B8.CAGPTO-CTA          AND
              B.CDEPDC               = :GFCTB0B8.CDEPDC              AND
              B.CPOSTO_SERVC         = :GFCTB0B8.CPOSTO-SERVC        AND
              A.CSERVC_TARIF        >= :GFCTB0B2.CSERVC-TARIF        AND
              ((A.CSIT_SOLTC_FLEXZ  IN (02)                          AND
               (A.DINIC_FLEXZ_AGPTO <= :GFCTB0B2.DINIC-FLEXZ-AGPTO)) OR
                A.CSIT_SOLTC_FLEXZ  IN  (05, 08))
             ORDER BY A.CSERVC_TARIF
           END-EXEC.


           EXEC SQL
             DECLARE CSR09-GFCTB0B2 CURSOR FOR
               SELECT
                   DISTINCT A.CSERVC_TARIF
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO A,
                   DB2PRD.PDIDO_FLEXZ_AG    B
             WHERE
              A.CAGPTO_CTA           = B.CAGPTO_CTA                  AND
              A.CSERVC_TARIF         = B.CSERVC_TARIF                AND
              A.HIDTFD_FLEXZ_AGPTO   = B.HIDTFD_FLEXZ_AGPTO          AND
              B.CAGPTO_CTA           = :GFCTB0B1.CAGPTO-CTA          AND
              B.CDEPDC               = :GFCTB0B1.CDEPDC              AND
              A.CSERVC_TARIF        >= :GFCTB0B2.CSERVC-TARIF        AND
              ((A.CSIT_SOLTC_FLEXZ  IN (02)                          AND
               (A.DINIC_FLEXZ_AGPTO <= :GFCTB0B2.DINIC-FLEXZ-AGPTO)) OR
                A.CSIT_SOLTC_FLEXZ  IN (05, 08))
             ORDER BY A.CSERVC_TARIF
           END-EXEC.


           EXEC SQL
             DECLARE CSR10-GFCTB0B2 CURSOR FOR
               SELECT
                   DISTINCT A.CSERVC_TARIF
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO A,
                   DB2PRD.PDIDO_FLEXZ_PSTAL B
             WHERE
              A.CAGPTO_CTA           = B.CAGPTO_CTA                  AND
              A.CSERVC_TARIF         = B.CSERVC_TARIF                AND
              A.HIDTFD_FLEXZ_AGPTO   = B.HIDTFD_FLEXZ_AGPTO          AND
              B.CAGPTO_CTA           = :GFCTB0B9.CAGPTO-CTA          AND
              B.CDEPDC               = :GFCTB0B9.CDEPDC              AND
              B.CPOSTO_SERVC         = :GFCTB0B9.CPOSTO-SERVC        AND
              A.CSERVC_TARIF        >= :GFCTB0B2.CSERVC-TARIF        AND
              ((A.CSIT_SOLTC_FLEXZ  IN (02)                          AND
               (A.DINIC_FLEXZ_AGPTO <= :GFCTB0B2.DINIC-FLEXZ-AGPTO)) OR
                A.CSIT_SOLTC_FLEXZ  IN (05, 08))
             ORDER BY A.CSERVC_TARIF
           END-EXEC.


           EXEC SQL
             DECLARE CSR11-GFCTB0B2 CURSOR FOR
               SELECT
                   DISTINCT A.CSERVC_TARIF
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO A,
                   DB2PRD.PDIDO_FLEXZ_CLI   B
             WHERE
              A.CAGPTO_CTA           = B.CAGPTO_CTA                  AND
              A.CSERVC_TARIF         = B.CSERVC_TARIF                AND
              A.HIDTFD_FLEXZ_AGPTO   = B.HIDTFD_FLEXZ_AGPTO          AND
              B.CAGPTO_CTA           = :GFCTB0B3.CAGPTO-CTA          AND
ST2506*       B.CCGC_CPF             = :GFCTB0B3.CCGC-CPF            AND
ST2506*       B.CFLIAL_CGC           = :GFCTB0B3.CFLIAL-CGC          AND
ST2506*       B.CCTRL_CPF_CGC        = :GFCTB0B3.CCTRL-CPF-CGC       AND
ST2506        B.CCGC_CPF_ST          = :GFCTB0B3.CCGC-CPF-ST         AND
ST2506        B.CFLIAL_CGC_ST        = :GFCTB0B3.CFLIAL-CGC-ST       AND
ST2506        B.CCTRL_CPF_CGC_ST     = :GFCTB0B3.CCTRL-CPF-CGC-ST    AND
              A.CSERVC_TARIF        >= :GFCTB0B2.CSERVC-TARIF        AND
              ((A.CSIT_SOLTC_FLEXZ  IN (02)                          AND
               (A.DINIC_FLEXZ_AGPTO <= :GFCTB0B2.DINIC-FLEXZ-AGPTO)) OR
                A.CSIT_SOLTC_FLEXZ  IN (05, 08))
             ORDER BY A.CSERVC_TARIF
           END-EXEC.


           EXEC SQL
             DECLARE CSR12-GFCTB0B2 CURSOR FOR
               SELECT
                   DISTINCT A.CSERVC_TARIF
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO A,
                   DB2PRD.PDIDO_FLEXZ_SGMTO B
             WHERE
              A.CAGPTO_CTA           = B.CAGPTO_CTA                  AND
              A.CSERVC_TARIF         = B.CSERVC_TARIF                AND
              A.HIDTFD_FLEXZ_AGPTO   = B.HIDTFD_FLEXZ_AGPTO          AND
              B.CAGPTO_CTA           = :GFCTB0C0.CAGPTO-CTA          AND
              B.CSGMTO_GSTAO_TARIF   = :GFCTB0C0.CSGMTO-GSTAO-TARIF  AND
              A.CSERVC_TARIF        >= :GFCTB0B2.CSERVC-TARIF        AND
              ((A.CSIT_SOLTC_FLEXZ  IN (02)                          AND
               (A.DINIC_FLEXZ_AGPTO <= :GFCTB0B2.DINIC-FLEXZ-AGPTO)) OR
                A.CSIT_SOLTC_FLEXZ  IN (05, 08))
             ORDER BY A.CSERVC_TARIF
           END-EXEC.


           EXEC SQL
             DECLARE CSR13-GFCTB0B2 CURSOR FOR
               SELECT
                   DISTINCT A.CSERVC_TARIF
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO A,
                   DB2PRD.PDIDO_FLEXZ_MUN   B
             WHERE
              A.CAGPTO_CTA           = B.CAGPTO_CTA                  AND
              A.CSERVC_TARIF         = B.CSERVC_TARIF                AND
              A.HIDTFD_FLEXZ_AGPTO   = B.HIDTFD_FLEXZ_AGPTO          AND
              B.CAGPTO_CTA           = :GFCTB0B7.CAGPTO-CTA          AND
              B.CMUN_IBGE            = :GFCTB0B7.CMUN-IBGE           AND
              A.CSERVC_TARIF        >= :GFCTB0B2.CSERVC-TARIF        AND
              ((A.CSIT_SOLTC_FLEXZ  IN (02)                          AND
               (A.DINIC_FLEXZ_AGPTO <= :GFCTB0B2.DINIC-FLEXZ-AGPTO)) OR
                A.CSIT_SOLTC_FLEXZ  IN (05, 08))
             ORDER BY A.CSERVC_TARIF
           END-EXEC.


           EXEC SQL
             DECLARE CSR14-GFCTB0B2 CURSOR FOR
               SELECT
                   DISTINCT A.CSERVC_TARIF
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO A,
                   DB2PRD.PDIDO_FLEXZ_UF    B
             WHERE
              A.CAGPTO_CTA           = B.CAGPTO_CTA                  AND
              A.CSERVC_TARIF         = B.CSERVC_TARIF                AND
              A.HIDTFD_FLEXZ_AGPTO   = B.HIDTFD_FLEXZ_AGPTO          AND
              B.CAGPTO_CTA           = :GFCTB0C1.CAGPTO-CTA          AND
              B.CSGL_UF              = :GFCTB0C1.CSGL-UF             AND
              A.CSERVC_TARIF        >= :GFCTB0B2.CSERVC-TARIF        AND
              ((A.CSIT_SOLTC_FLEXZ  IN (02)                          AND
               (A.DINIC_FLEXZ_AGPTO <= :GFCTB0B2.DINIC-FLEXZ-AGPTO)) OR
                A.CSIT_SOLTC_FLEXZ  IN (05, 08))
             ORDER BY A.CSERVC_TARIF
           END-EXEC.

090708* CURSOR PARA AGRUPAMENTOS PERSONALIZADOS

090708     EXEC SQL
090708       DECLARE CSR15-GFCTB0B2 CURSOR FOR
090708         SELECT
090708             DISTINCT CSERVC_TARIF
090708       FROM  DB2PRD.PDIDO_FLEXZ_AGPTO
090708       WHERE
090708        CAGPTO_CTA             = :GFCTB0B2.CAGPTO-CTA          AND
090708        CSERVC_TARIF          >= :GFCTB0B2.CSERVC-TARIF        AND
090708        ((CSIT_SOLTC_FLEXZ    IN (02)                          AND
090708         (DINIC_FLEXZ_AGPTO   <= :GFCTB0B2.DINIC-FLEXZ-AGPTO)) OR
090708          CSIT_SOLTC_FLEXZ    IN (05, 08))
090708       ORDER BY CSERVC_TARIF
090708     END-EXEC.


110510     EXEC SQL
110510       DECLARE CSR16-GFCTB0B2-JN CURSOR FOR
110510         SELECT
110510             A.CSERVC_TARIF,
110510             A.DINIC_FLEXZ_AGPTO,
110510             A.CSIT_SOLTC_FLEXZ
110510       FROM  DB2PRD.PDIDO_FLEXZ_AGPTO A,
110510             DB2PRD.TPDIDO_FLEXZ_RZ   B
110510       WHERE
110510        A.CAGPTO_CTA           = B.CAGPTO_CTA                  AND
110510        A.CSERVC_TARIF         = B.CSERVC_TARIF                AND
110510        A.HIDTFD_FLEXZ_AGPTO   = B.HIDTFD_FLEXZ_AGPTO          AND
110510        B.CAGPTO_CTA           = :GFCTB0M7.CAGPTO-CTA          AND
110510        B.CGRP_CTBIL_ATUAL     = :GFCTB0M7.CGRP-CTBIL-ATUAL    AND
110510        B.CSGRP_CTBIL          = :GFCTB0M7.CSGRP-CTBIL         AND
110510       (A.CSERVC_TARIF   BETWEEN  :WRK-CSERVC-TARIF-INI        AND
110510                                  :WRK-CSERVC-TARIF-FIM)
110510       ORDER BY A.CSERVC_TARIF
110510     END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTOO'.

       COPY 'I#GFCTOP'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTOO-ENTRADA
                                                      GFCTOP-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

______*____IF_GFCTOP-QTDE-TOT-REG_NUMERIC
______*_______MOVE_GFCTOP-QTDE-TOT-REG_TO_WRK-GFCTOP-QTDE-TOT-REG
______*____ELSE
______*_______MOVE_ZEROS_TO_GFCTOP-QTDE-TOT-REG
______*____END-IF.

      ******************************************************************
      *    ROTINA DE INICIALIZACAO DE PROCESSAMENTOS.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

260808     IF  WRK-FINALIZAR           EQUAL 'S'
260808         IF  WRK-GFCTOP-QTDE-TOT-REG EQUAL ZEROS
260808             MOVE 1              TO GFCTOP-ERRO
260808             MOVE ZEROS          TO GFCTOP-COD-SQL-ERRO
260808             MOVE 0859           TO GFCTOP-COD-MSG-ERRO
260808             MOVE '0859'         TO GFCTG2-COD-MSG
260808             PERFORM 1310-OBTER-MENSAGEM
260808             MOVE GFCTG3-DESC-MSG TO GFCTOP-DESC-MSG-ERRO
260808         END-IF
260808     END-IF.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA OS PROCEDIMENTOS INICIAIS DO PROGRAMA.    *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTOP-SAIDA.

           INITIALIZE GFCTOP-SAIDA
                      GFCT0M-AREA-ERROS.

           PERFORM 1100-VERIFICAR-ONLINE.

           PERFORM 1200-MONTAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE VERIFICA DISPONIBILIDADE DO SISTEMA.             *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-ONLINE           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE +100                   TO GFCTFZ-LL.
           MOVE GFCTOO-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTOO-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTOO-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTOP-ERRO
               MOVE WRK-MODULO         TO WRK-NOME-MODULO
                                          GFCT0M-PROGRAMA
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5147'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF (GFCTG1-ERRO             NOT EQUAL ZEROS)
               MOVE GFCTG1-ERRO        TO GFCTOP-ERRO
               IF (GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2')
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'GFCT5147'     TO GFCT0M-TRANSACAO
               END-IF
               MOVE GFCTG1-COD-SQL-ERRO  TO GFCT0M-SQLCODE
                                            GFCTOP-COD-SQL-ERRO
               MOVE GFCTG1-COD-MSG-ERRO  TO GFCTOP-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO TO GFCT0M-TEXTO
                                            GFCTOP-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF (GFCTG1-CSIT-ONLINE      EQUAL 1)
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
                                          GFCTOP-DESC-MSG-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5147'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTOP-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA FORMATACAO DA AREA DE RETORNO AO PROGRAMA       *
      *    CHAMADOR.                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

090708*    MOVE +170                   TO GFCTOP-LL.
090708     MOVE +210                   TO GFCTOP-LL.
           MOVE ZEROS                  TO GFCTOP-ZZ.
           MOVE GFCTOO-TRANSACAO       TO GFCTOP-TRANSACAO.
           MOVE GFCTOO-FUNCAO          TO GFCTOP-FUNCAO.
           MOVE GFCTOO-CAGPTO-CTA      TO GFCTOP-CAGPTO-CTA.

           IF GFCTOO-QTDE-OCOR         NOT NUMERIC
              MOVE ZEROS               TO GFCTOP-QTDE-OCOR
           ELSE
              MOVE GFCTOO-QTDE-OCOR    TO GFCTOP-QTDE-OCOR
           END-IF.

           IF GFCTOO-QTDE-TOT-REG      NOT NUMERIC
              MOVE ZEROS               TO WRK-GFCTOP-QTDE-TOT-REG
           ELSE
              MOVE GFCTOO-QTDE-TOT-REG TO WRK-GFCTOP-QTDE-TOT-REG
           END-IF.

           MOVE GFCTOO-FUNC-BDSCO      TO GFCTOP-FUNC-BDSCO.
           MOVE GFCTOO-PONTEIRO-TARIFA TO GFCTOP-PONTEIRO-TARIFA.
PROC       MOVE GFCTOO-FILTRO          TO GFCTOP-FILTRO.

           MOVE ZEROS                  TO GFCTOP-ERRO
                                          GFCTOP-COD-SQL-ERRO
                                          GFCTOP-COD-MSG-ERRO.
           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO GFCTOP-DESC-MSG-ERRO.

           MOVE 'N'                    TO GFCTOP-FIM.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA A CONSISTECIA DE DADOS.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTOO-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTOO-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTOO-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTOO-QTDE-OCOR        NOT NUMERIC) OR
              (GFCTOO-CAGPTO-CTA       NOT NUMERIC) OR
090708*       (GFCTOO-CAGPTO-CTA       NOT EQUAL 004 AND 009 AND
090708*                                          014 AND 015 AND
090708*                                                  017 AND
090708*                                          018 AND 019)      OR
              (GFCTOO-QTDE-TOT-REG     NOT NUMERIC) OR
              (GFCTOO-PONTEIRO-TARIFA  NOT NUMERIC) OR
              (GFCTOO-FIM              NOT EQUAL 'S' AND 'N') OR
              (GFCTOO-TIPO-VIGENCIA    NOT EQUAL 'V' AND 'N' AND
                                                 'T') OR
              (GFCTOO-TIPO-FUNCAO      NOT EQUAL 'R' AND 'C' AND
                                                 'E' AND 'T' AND
                                                 'D' AND 'V' AND 'P')
               MOVE 'S'                TO GFCTOP-FIM
               MOVE 1                  TO GFCTOP-ERRO
                                          GFCTOP-COD-MSG-ERRO
               MOVE ZEROS              TO GFCTOP-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTOP-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE OBTEM DESCRICAO DA MENSAGEM.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-MENSAGEM             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-ZZ.
           MOVE ZEROS                  TO GFCTG2-LL.
           MOVE GFCTOO-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTOO-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTOO-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTOP-ERRO
               MOVE WRK-MODULO         TO WRK-NOME-MODULO
                                          GFCT0M-PROGRAMA
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5147'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF (GFCTG3-ERRO             NOT EQUAL ZEROS)
               MOVE GFCTG3-ERRO        TO GFCTOP-ERRO
               IF (GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2')
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'GFCT5147'     TO GFCT0M-TRANSACAO
               END-IF
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          GFCTOP-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTOP-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCT0M-TEXTO
                                          GFCTOP-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-OBTER-DATA.

           EVALUATE TRUE
               WHEN GFCTOO-TIPO-VIGENCIA EQUAL 'V' AND
                    GFCTOO-TIPO-FUNCAO   EQUAL 'R'
                    PERFORM 2200-TRATAR-CURSOR-1

               WHEN GFCTOO-TIPO-VIGENCIA EQUAL 'N' AND
                    GFCTOO-TIPO-FUNCAO   EQUAL 'C'
                    PERFORM 2300-TRATAR-CURSOR-2

               WHEN GFCTOO-TIPO-VIGENCIA EQUAL 'V' AND
                    GFCTOO-TIPO-FUNCAO   EQUAL 'E'
                    PERFORM 2400-TRATAR-CURSOR-3

               WHEN GFCTOO-TIPO-VIGENCIA EQUAL 'T' AND
                    GFCTOO-TIPO-FUNCAO   EQUAL 'T'
                    PERFORM 2500-TRATAR-CURSOR-4

               WHEN GFCTOO-TIPO-VIGENCIA EQUAL 'N' AND
                    GFCTOO-TIPO-FUNCAO   EQUAL 'P'
                    PERFORM 2600-TRATAR-CURSOR-5

               WHEN GFCTOO-TIPO-VIGENCIA EQUAL 'N' AND
                    GFCTOO-TIPO-FUNCAO   EQUAL 'D'
                    PERFORM 2700-TRATAR-CURSOR-6

               WHEN GFCTOO-TIPO-VIGENCIA EQUAL 'T' AND
                    GFCTOO-TIPO-FUNCAO   EQUAL 'V'
                    PERFORM 2800-TRATAR-CURSOR-7

               WHEN OTHER
                    MOVE 'S'             TO GFCTOP-FIM
                    MOVE 1               TO GFCTOP-ERRO
                                            GFCTOP-COD-MSG-ERRO
                    MOVE ZEROS           TO GFCTOP-COD-SQL-ERRO
                    MOVE '0001'          TO GFCTG2-COD-MSG
                    PERFORM 1310-OBTER-MENSAGEM
                    MOVE GFCTG3-DESC-MSG TO GFCTOP-DESC-MSG-ERRO
                    PERFORM 3000-FINALIZAR

           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA OBTENCAO DE DATA.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-OBTER-DATA                 SECTION.
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
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0A1'         TO WRK-NOM-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOP-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

0710       MOVE  DPROCM-ATUAL          TO WRK-DATA-DB2
.          MOVE  WRK-ANO-DB2           TO WRK-ANO-PROCM-ATUAL-INV
.          MOVE  WRK-MES-DB2           TO WRK-MES-PROCM-ATUAL-INV
.          MOVE  WRK-DIA-DB2           TO WRK-DIA-PROCM-ATUAL-INV.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TRATAMENTO DO CURSOR 01.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-TRATAR-CURSOR-1            SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTOO-CAGPTO-CTA
               WHEN 15
                   PERFORM 2210-TRATAR-PAB

               WHEN 14
                   IF  GFCTOO-ARG5             EQUAL 'S'
                       MOVE  ZEROS         TO  GFCTOO-ARG6
                   END-IF
                   PERFORM 2220-TRATAR-AGENCIA

               WHEN 17
                   PERFORM 2230-TRATAR-POSTAL

               WHEN 4
                   PERFORM 2240-TRATAR-CLIENTE

               WHEN 9
                   PERFORM 2250-TRATAR-SEGMENTO

               WHEN 18
                   PERFORM 2260-TRATAR-MUNICIPIO

               WHEN 19
                   PERFORM 2270-TRATAR-UF

110510         WHEN 20
110510             PERFORM 2230-TRATAR-POSTAL
110510
110510         WHEN 26
110510             PERFORM 2850-TRATAR-RAZAO

090708         WHEN OTHER
090708             PERFORM 2280-TRATAR-PERSONALIZADOS

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-TRATAR-PAB                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 2211-ABRIR-CSR08-GFCTB0B2.

           PERFORM 2212-LER-CSR08-GFCTB0B2.

           PERFORM 2213-PROCESSAR-CSR08-GFCTB0B2   VARYING IND-1 FROM
                   1 BY 1                          UNTIL
                   IND-1                           GREATER 8 OR
                   WRK-FINALIZAR                   EQUAL 'S'.

           IF  WRK-FINALIZAR           NOT EQUAL 'S' AND
               IND-1                   GREATER 8
               MOVE CSERVC-TARIF       OF GFCTB0B2
                                       TO GFCTOP-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2214-FECHAR-CSR08-GFCTB0B2.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE ABERTURA DO CURSOR 08 DA TABELA GFCTB0B2.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2211-ABRIR-CSR08-GFCTB0B2       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.
           MOVE GFCTOO-PONTEIRO-TARIFA TO CSERVC-TARIF OF GFCTB0B2.

           MOVE  GFCTOO-CAGPTO-CTA     TO CAGPTO-CTA       OF GFCTB0B8.
           MOVE  GFCTOO-ARG2           TO CDEPDC           OF GFCTB0B8.
           MOVE  GFCTOO-ARG6           TO CPOSTO-SERVC     OF GFCTB0B8.


           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA OF GFCTB0B2.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
                                          DFIM-FLEXZ-AGPTO OF GFCTB0B2.

           EXEC SQL
               OPEN CSR08-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTOP-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE LEITURA DO CURSOR 01 DA TABELA GFCTB0B2.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2212-LER-CSR08-GFCTB0B2         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR08-GFCTB0B2  INTO
                   :GFCTB0B2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTOP-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTOP-ERRO
                   MOVE ZEROS          TO GFCTOP-COD-SQL-ERRO
                   MOVE 0859           TO GFCTOP-COD-MSG-ERRO
                   MOVE '0859'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTOP-DESC-MSG-ERRO
               END-IF
               GO TO 2212-99-FIM
           END-IF.

      ******************************************************************
      *    VERIFICA SE A FLEXIBILIZACAO JAH FOI RENOVADA               *
      ******************************************************************

           MOVE  GFCTOO-CAGPTO-CTA     TO CAGPTO-CTA       OF GFCTB0B8.
           MOVE  GFCTOO-ARG2           TO CDEPDC           OF GFCTB0B8.
           MOVE  GFCTOO-ARG6           TO CPOSTO-SERVC     OF GFCTB0B8.

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA OF GFCTB0B2.
           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B8.

           EXEC SQL
             SELECT
                   A.DINIC_FLEXZ_AGPTO,
                   A.DFIM_FLEXZ_AGPTO,
                   A.CSIT_SOLTC_FLEXZ
             INTO
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_PAB   B
             WHERE
                   A.CAGPTO_CTA          = B.CAGPTO_CTA
             AND   A.CSERVC_TARIF        = B.CSERVC_TARIF
             AND   A.HIDTFD_FLEXZ_AGPTO  = B.HIDTFD_FLEXZ_AGPTO
             AND   A.CAGPTO_CTA          = :GFCTB0B2.CAGPTO-CTA
             AND   B.CDEPDC              = :GFCTB0B8.CDEPDC
             AND   B.CPOSTO_SERVC        = :GFCTB0B8.CPOSTO-SERVC
             AND   B.CSERVC_TARIF        = :GFCTB0B8.CSERVC-TARIF
             AND   A.CSIT_SOLTC_FLEXZ    =  01
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               GO TO 2212-LER-CSR08-GFCTB0B2
           END-IF.

      *----------------------------------------------------------------*
       2212-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTO DO CURSOR 01 DA TABELA GFCTB0B2.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2213-PROCESSAR-CSR08-GFCTB0B2   SECTION.
      *----------------------------------------------------------------*

090708     MOVE 'N' TO WRK-TEM-PERMSS
090708
090708     PERFORM 2215-VERIFICA-PERMISSAO.
090708
090708     IF WRK-TEM-PERMSS = 'S'
090708
090708*            COMPUTE GFCTOP-LL = (GFCTOP-LL + 105)
090708             COMPUTE GFCTOP-LL = (GFCTOP-LL + 100)
090708             MOVE  CINDCD-PRMSS-AGNDA    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-AGNDA (IND-1)
090708             MOVE  CINDCD-PRMSS-VLR      OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-VALOR (IND-1)
090708             MOVE  CINDCD-PRMSS-PERC     OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-PERCENT(IND-1)
090708             MOVE  CINDCD-PRMSS-FRANQ    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-FRANQ (IND-1)
090708             MOVE  CINDCD-PRMSS-PERDC    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-PERDC (IND-1)

                   ADD 1               TO WRK-GFCTOP-QTDE-TOT-REG

                   MOVE CSERVC-TARIF OF GFCTB0B2
                                       TO GFCTOP-COD-TARIFA(IND-1)
                                          CSERVC-TARIF OF GFCTB0D8

                   PERFORM 2900-OBTER-DADOS-GFCTB0D8

                   MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTOP-DESC-TARIFA(IND-1)

090708     END-IF

           PERFORM 2212-LER-CSR08-GFCTB0B2.

      *----------------------------------------------------------------*
       2213-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE FECHAMENTO DO CURSOR 01 DA TABELA GFCTB0B2.       *
      ******************************************************************
      *----------------------------------------------------------------*
       2214-FECHAR-CSR08-GFCTB0B2      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR08-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTOP-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2214-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

090708*----------------------------------------------------------------*
090708 2215-VERIFICA-PERMISSAO         SECTION.
090708*----------------------------------------------------------------*
090708
090708* PARA CADA REGISTRO LIDO NO CURSOR, VAI VERIFICAR
090708* SE TEM PERMISSAO DE FLEXIBILIZACAO PARA A TARIFA
090708
090708     MOVE GFCTOO-CAGPTO-CTA     TO CAGPTO-CTA        OF GFCTB0J7.
090708     MOVE CSERVC-TARIF          OF GFCTB0B2
090708                                TO CSERVC-TARIF      OF GFCTB0J7
090708     MOVE DPROCM-ATUAL          OF GFCTB0A1
090708                                TO DINIC-PRMSS-FLEXZ OF GFCTB0J7
090708                                   DFIM-PRMSS-FLEXZ  OF GFCTB0J7.
090708     EXEC SQL
090708       SELECT
090708             CINDCD_PRMSS_AGNDA,
090708             CINDCD_PRMSS_VLR,
090708             CINDCD_PRMSS_PERC,
090708             CINDCD_PRMSS_FRANQ,
090708             CINDCD_PRMSS_PERDC
090708       INTO
090708             :GFCTB0J7.CINDCD-PRMSS-AGNDA,
090708             :GFCTB0J7.CINDCD-PRMSS-VLR,
090708             :GFCTB0J7.CINDCD-PRMSS-PERC,
090708             :GFCTB0J7.CINDCD-PRMSS-FRANQ,
090708             :GFCTB0J7.CINDCD-PRMSS-PERDC
090708       FROM  DB2PRD.TPRMSS_FLEXZ_AGPTO
090708       WHERE
090708             CAGPTO_CTA         = :GFCTB0J7.CAGPTO-CTA        AND
090708             CSERVC_TARIF       = :GFCTB0J7.CSERVC-TARIF      AND
090708             DFIM_PRMSS_FLEXZ  >= :GFCTB0J7.DFIM-PRMSS-FLEXZ  AND
090708             CFUNC_ENCRR_VGCIA  = 0
090708     END-EXEC.
090708
090708
090708     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
090708        (SQLWARN0                EQUAL 'W'      )
090708         MOVE SPACES             TO GFCT0M-ERRO-SQL
090708         INITIALIZE GFCT0M-ERRO-SQL
090708         MOVE 'TPRMSS_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
090708         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
090708         MOVE '0060'             TO GFCT0M-LOCAL
090708         MOVE  0006              TO GFCTOP-COD-MSG-ERRO
090708         MOVE '0006'             TO GFCTG2-COD-MSG
090708         MOVE 'GFCTB0J7'         TO WRK-NOM-TAB
090708         PERFORM 2999-FORMATAR-ERRO-DB2
090708         PERFORM 3000-FINALIZAR
090708     END-IF.
090708
090708     IF  SQLCODE                 EQUAL ZEROS
090708         MOVE 'S' TO WRK-TEM-PERMSS
090708     END-IF.
090708
090708*----------------------------------------------------------------*
090708 2215-99-FIM.                    EXIT.
090708*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2220-TRATAR-AGENCIA             SECTION.
      *----------------------------------------------------------------*

           PERFORM 2221-ABRIR-CSR09-GFCTB0B2.

           PERFORM 2222-LER-CSR09-GFCTB0B2.

           PERFORM 2223-PROCESSAR-CSR09-GFCTB0B2   VARYING IND-1 FROM
                   1 BY 1                          UNTIL
                   IND-1                           GREATER 8 OR
                   WRK-FINALIZAR                   EQUAL 'S'.

           IF  WRK-FINALIZAR           NOT EQUAL 'S' AND
               IND-1                   GREATER 8
               MOVE CSERVC-TARIF       OF GFCTB0B2
                                       TO GFCTOP-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2224-FECHAR-CSR09-GFCTB0B2.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE ABERTURA DO CURSOR 09 DA TABELA GFCTB0B2.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2221-ABRIR-CSR09-GFCTB0B2       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.
           MOVE GFCTOO-PONTEIRO-TARIFA TO CSERVC-TARIF OF GFCTB0B2.

           MOVE  GFCTOO-CAGPTO-CTA     TO CAGPTO-CTA       OF GFCTB0B1.
           MOVE  GFCTOO-ARG6           TO CDEPDC           OF GFCTB0B1.

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA OF GFCTB0B2.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
                                          DFIM-FLEXZ-AGPTO OF GFCTB0B2.

           EXEC SQL
               OPEN CSR09-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTOP-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2221-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE LEITURA DO CURSOR 09 DA TABELA GFCTB0B2.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2222-LER-CSR09-GFCTB0B2         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR09-GFCTB0B2  INTO
                   :GFCTB0B2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTOP-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTOP-ERRO
                   MOVE ZEROS          TO GFCTOP-COD-SQL-ERRO
                   MOVE 0859           TO GFCTOP-COD-MSG-ERRO
                   MOVE '0859'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTOP-DESC-MSG-ERRO
               END-IF
               GO TO 2222-99-FIM
           END-IF.

      ******************************************************************
      *    VERIFICA SE A FLEXIBILIZACAO JAH FOI RENOVADA               *
      ******************************************************************

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA       OF GFCTB0B1.
           MOVE GFCTOO-ARG6            TO CDEPDC           OF GFCTB0B1.

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA       OF GFCTB0B2.
           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B1.

           EXEC SQL
             SELECT
                   A.DINIC_FLEXZ_AGPTO,
                   A.DFIM_FLEXZ_AGPTO,
                   A.CSIT_SOLTC_FLEXZ
             INTO
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_AG    B
             WHERE
                   A.CAGPTO_CTA          = B.CAGPTO_CTA
             AND   A.CSERVC_TARIF        = B.CSERVC_TARIF
             AND   A.HIDTFD_FLEXZ_AGPTO  = B.HIDTFD_FLEXZ_AGPTO
             AND   A.CAGPTO_CTA          = :GFCTB0B2.CAGPTO-CTA
             AND   B.CDEPDC              = :GFCTB0B1.CDEPDC
             AND   B.CSERVC_TARIF        = :GFCTB0B1.CSERVC-TARIF
             AND   A.CSIT_SOLTC_FLEXZ    =  01
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               GO TO 2222-LER-CSR09-GFCTB0B2
           END-IF.

      *----------------------------------------------------------------*
       2222-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTO DO CURSOR 09 DA TABELA GFCTB0B2.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2223-PROCESSAR-CSR09-GFCTB0B2   SECTION.
      *----------------------------------------------------------------*

090708     MOVE 'N' TO WRK-TEM-PERMSS
090708
090708     PERFORM 2215-VERIFICA-PERMISSAO.
090708
090708     IF WRK-TEM-PERMSS = 'S'
090708
090708*            COMPUTE GFCTOP-LL = (GFCTOP-LL + 105)
090708             COMPUTE GFCTOP-LL = (GFCTOP-LL + 100)
090708             MOVE  CINDCD-PRMSS-AGNDA    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-AGNDA (IND-1)
090708             MOVE  CINDCD-PRMSS-VLR      OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-VALOR (IND-1)
090708             MOVE  CINDCD-PRMSS-PERC     OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-PERCENT(IND-1)
090708             MOVE  CINDCD-PRMSS-FRANQ    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-FRANQ (IND-1)
090708             MOVE  CINDCD-PRMSS-PERDC    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-PERDC (IND-1)

                   ADD 1               TO WRK-GFCTOP-QTDE-TOT-REG

                   MOVE CSERVC-TARIF OF GFCTB0B2
                                       TO GFCTOP-COD-TARIFA(IND-1)
                                          CSERVC-TARIF OF GFCTB0D8

                   PERFORM 2900-OBTER-DADOS-GFCTB0D8

                   MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTOP-DESC-TARIFA(IND-1)

090708     END-IF

           PERFORM 2222-LER-CSR09-GFCTB0B2.

      *----------------------------------------------------------------*
       2223-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE FECHAMENTO DO CURSOR 09 DA TABELA GFCTB0B2.       *
      ******************************************************************
      *----------------------------------------------------------------*
       2224-FECHAR-CSR09-GFCTB0B2      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR09-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTOP-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2224-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       2230-TRATAR-POSTAL              SECTION.
      *----------------------------------------------------------------*

           PERFORM 2231-ABRIR-CSR10-GFCTB0B2.

           PERFORM 2232-LER-CSR10-GFCTB0B2.

           PERFORM 2233-PROCESSAR-CSR10-GFCTB0B2   VARYING IND-1 FROM
                   1 BY 1                          UNTIL
                   IND-1                           GREATER 8 OR
                   WRK-FINALIZAR                   EQUAL 'S'.

           IF  WRK-FINALIZAR           NOT EQUAL 'S' AND
               IND-1                   GREATER 8
               MOVE CSERVC-TARIF       OF GFCTB0B2
                                       TO GFCTOP-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2234-FECHAR-CSR10-GFCTB0B2.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE ABERTURA DO CURSOR 10 DA TABELA GFCTB0B2.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2231-ABRIR-CSR10-GFCTB0B2       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.
           MOVE GFCTOO-PONTEIRO-TARIFA TO CSERVC-TARIF OF GFCTB0B2.

           MOVE  GFCTOO-CAGPTO-CTA     TO CAGPTO-CTA       OF GFCTB0B9.
           MOVE  GFCTOO-ARG2           TO CDEPDC           OF GFCTB0B9.
           MOVE  GFCTOO-ARG6           TO CPOSTO-SERVC     OF GFCTB0B9.

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA OF GFCTB0B2.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
                                          DFIM-FLEXZ-AGPTO OF GFCTB0B2.

           EXEC SQL
               OPEN CSR10-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTOP-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2231-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE LEITURA DO CURSOR 10 DA TABELA GFCTB0B2.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2232-LER-CSR10-GFCTB0B2         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR10-GFCTB0B2  INTO
                   :GFCTB0B2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTOP-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTOP-ERRO
                   MOVE ZEROS          TO GFCTOP-COD-SQL-ERRO
                   MOVE 0859           TO GFCTOP-COD-MSG-ERRO
                   MOVE '0859'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTOP-DESC-MSG-ERRO
               END-IF
               GO TO 2232-99-FIM
           END-IF.

      ******************************************************************
      *    VERIFICA SE A FLEXIBILIZACAO JAH FOI RENOVADA               *
      ******************************************************************

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA       OF GFCTB0B9.
           MOVE GFCTOO-ARG2            TO CDEPDC           OF GFCTB0B9.
           MOVE GFCTOO-ARG6            TO CPOSTO-SERVC     OF GFCTB0B9.

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA       OF GFCTB0B2.
           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B9.

           EXEC SQL
             SELECT
                   A.DINIC_FLEXZ_AGPTO,
                   A.DFIM_FLEXZ_AGPTO,
                   A.CSIT_SOLTC_FLEXZ
             INTO
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_PSTAL B
             WHERE
                   A.CAGPTO_CTA          = B.CAGPTO_CTA
             AND   A.CSERVC_TARIF        = B.CSERVC_TARIF
             AND   A.HIDTFD_FLEXZ_AGPTO  = B.HIDTFD_FLEXZ_AGPTO
             AND   A.CAGPTO_CTA          = :GFCTB0B2.CAGPTO-CTA
             AND   B.CDEPDC              = :GFCTB0B9.CDEPDC
             AND   B.CPOSTO_SERVC        = :GFCTB0B9.CPOSTO-SERVC
             AND   B.CSERVC_TARIF        = :GFCTB0B9.CSERVC-TARIF
             AND   A.CSIT_SOLTC_FLEXZ    =  01
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               GO TO 2232-LER-CSR10-GFCTB0B2
           END-IF.

      *----------------------------------------------------------------*
       2232-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTO DO CURSOR 10 DA TABELA GFCTB0B2.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2233-PROCESSAR-CSR10-GFCTB0B2   SECTION.
      *----------------------------------------------------------------*

090708     MOVE 'N' TO WRK-TEM-PERMSS
090708
090708     PERFORM 2215-VERIFICA-PERMISSAO.
090708
090708     IF WRK-TEM-PERMSS = 'S'
090708
090708*            COMPUTE GFCTOP-LL = (GFCTOP-LL + 105)
090708             COMPUTE GFCTOP-LL = (GFCTOP-LL + 100)
090708             MOVE  CINDCD-PRMSS-AGNDA    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-AGNDA (IND-1)
090708             MOVE  CINDCD-PRMSS-VLR      OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-VALOR (IND-1)
090708             MOVE  CINDCD-PRMSS-PERC     OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-PERCENT(IND-1)
090708             MOVE  CINDCD-PRMSS-FRANQ    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-FRANQ (IND-1)
090708             MOVE  CINDCD-PRMSS-PERDC    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-PERDC (IND-1)

                   ADD 1               TO WRK-GFCTOP-QTDE-TOT-REG

                   MOVE CSERVC-TARIF OF GFCTB0B2
                                       TO GFCTOP-COD-TARIFA(IND-1)
                                          CSERVC-TARIF OF GFCTB0D8

                   PERFORM 2900-OBTER-DADOS-GFCTB0D8

                   MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTOP-DESC-TARIFA(IND-1)

090708     END-IF

           PERFORM 2232-LER-CSR10-GFCTB0B2.

      *----------------------------------------------------------------*
       2233-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE FECHAMENTO DO CURSOR 10 DA TABELA GFCTB0B2.       *
      ******************************************************************
      *----------------------------------------------------------------*
       2234-FECHAR-CSR10-GFCTB0B2      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR10-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTOP-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2234-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       2240-TRATAR-CLIENTE             SECTION.
      *----------------------------------------------------------------*

           PERFORM 2241-ABRIR-CSR11-GFCTB0B2.

           PERFORM 2242-LER-CSR11-GFCTB0B2.

           PERFORM 2243-PROCESSAR-CSR11-GFCTB0B2   VARYING IND-1 FROM
                   1 BY 1                          UNTIL
                   IND-1                           GREATER 8 OR
                   WRK-FINALIZAR                   EQUAL 'S'.

           IF  WRK-FINALIZAR           NOT EQUAL 'S' AND
               IND-1                   GREATER 8
               MOVE CSERVC-TARIF       OF GFCTB0B2
                                       TO GFCTOP-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2244-FECHAR-CSR11-GFCTB0B2.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE ABERTURA DO CURSOR 11 DA TABELA GFCTB0B2.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2241-ABRIR-CSR11-GFCTB0B2       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.
           MOVE GFCTOO-PONTEIRO-TARIFA TO CSERVC-TARIF OF GFCTB0B2.

           MOVE  GFCTOO-CAGPTO-CTA     TO CAGPTO-CTA       OF GFCTB0B3.
ST2506*    MOVE  GFCTOO-ARG1           TO CCGC-CPF         OF GFCTB0B3.
ST2506*    MOVE  GFCTOO-ARG2           TO CFLIAL-CGC       OF GFCTB0B3.
ST2506*    MOVE  GFCTOO-ARG3           TO CCTRL-CPF-CGC    OF GFCTB0B3.
ST2506     MOVE  GFCTOO-ARG1           TO CCGC-CPF-ST      OF GFCTB0B3.
ST2506     MOVE  GFCTOO-ARG2           TO CFLIAL-CGC-ST    OF GFCTB0B3.
ST2506     MOVE  GFCTOO-ARG3           TO CCTRL-CPF-CGC-ST OF GFCTB0B3.

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA OF GFCTB0B2.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
                                          DFIM-FLEXZ-AGPTO OF GFCTB0B2.

           EXEC SQL
               OPEN CSR11-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0150'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTOP-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2241-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE LEITURA DO CURSOR 11 DA TABELA GFCTB0B2.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2242-LER-CSR11-GFCTB0B2         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR11-GFCTB0B2  INTO
                   :GFCTB0B2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0160'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTOP-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTOP-ERRO
                   MOVE ZEROS          TO GFCTOP-COD-SQL-ERRO
                   MOVE 0859           TO GFCTOP-COD-MSG-ERRO
                   MOVE '0859'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTOP-DESC-MSG-ERRO
               END-IF
               GO TO 2242-99-FIM
           END-IF.

      ******************************************************************
      *    VERIFICA SE A FLEXIBILIZACAO JAH FOI RENOVADA               *
      ******************************************************************

           MOVE  GFCTOO-CAGPTO-CTA     TO CAGPTO-CTA       OF GFCTB0B3.
ST2506*    MOVE  GFCTOO-ARG1           TO CCGC-CPF         OF GFCTB0B3.
ST2506*    MOVE  GFCTOO-ARG2           TO CFLIAL-CGC       OF GFCTB0B3.
ST2506*    MOVE  GFCTOO-ARG3           TO CCTRL-CPF-CGC    OF GFCTB0B3.
ST2506     MOVE  GFCTOO-ARG1           TO CCGC-CPF-ST      OF GFCTB0B3.
ST2506     MOVE  GFCTOO-ARG2           TO CFLIAL-CGC-ST    OF GFCTB0B3.
ST2506     MOVE  GFCTOO-ARG3           TO CCTRL-CPF-CGC-ST OF GFCTB0B3.

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA       OF GFCTB0B2.
           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B3.

           EXEC SQL
             SELECT
                   A.DINIC_FLEXZ_AGPTO,
                   A.DFIM_FLEXZ_AGPTO,
                   A.CSIT_SOLTC_FLEXZ
             INTO
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_CLI   B
             WHERE
                   A.CAGPTO_CTA          = B.CAGPTO_CTA
             AND   A.CSERVC_TARIF        = B.CSERVC_TARIF
             AND   A.HIDTFD_FLEXZ_AGPTO  = B.HIDTFD_FLEXZ_AGPTO
             AND   A.CAGPTO_CTA          = :GFCTB0B2.CAGPTO-CTA
ST2506*      AND   B.CCGC_CPF            = :GFCTB0B3.CCGC-CPF
ST2506*      AND   B.CFLIAL_CGC          = :GFCTB0B3.CFLIAL-CGC
ST2506*      AND   B.CCTRL_CPF_CGC       = :GFCTB0B3.CCTRL-CPF-CGC
ST2506       AND   B.CCGC_CPF_ST         = :GFCTB0B3.CCGC-CPF-ST
ST2506       AND   B.CFLIAL_CGC_ST       = :GFCTB0B3.CFLIAL-CGC-ST
ST2506       AND   B.CCTRL_CPF_CGC_ST    = :GFCTB0B3.CCTRL-CPF-CGC-ST
             AND   B.CSERVC_TARIF        = :GFCTB0B3.CSERVC-TARIF
             AND   A.CSIT_SOLTC_FLEXZ    =  01
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0170'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               GO TO 2242-LER-CSR11-GFCTB0B2
           END-IF.

      *----------------------------------------------------------------*
       2242-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTO DO CURSOR 11 DA TABELA GFCTB0B2.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2243-PROCESSAR-CSR11-GFCTB0B2   SECTION.
      *----------------------------------------------------------------*

090708     MOVE 'N' TO WRK-TEM-PERMSS
090708
090708     PERFORM 2215-VERIFICA-PERMISSAO.
090708
090708     IF WRK-TEM-PERMSS = 'S'
090708
090708*            COMPUTE GFCTOP-LL = (GFCTOP-LL + 105)
090708             COMPUTE GFCTOP-LL = (GFCTOP-LL + 100)
090708             MOVE  CINDCD-PRMSS-AGNDA    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-AGNDA (IND-1)
090708             MOVE  CINDCD-PRMSS-VLR      OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-VALOR (IND-1)
090708             MOVE  CINDCD-PRMSS-PERC     OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-PERCENT(IND-1)
090708             MOVE  CINDCD-PRMSS-FRANQ    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-FRANQ (IND-1)
090708             MOVE  CINDCD-PRMSS-PERDC    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-PERDC (IND-1)

                   ADD 1               TO WRK-GFCTOP-QTDE-TOT-REG

                   MOVE CSERVC-TARIF OF GFCTB0B2
                                       TO GFCTOP-COD-TARIFA(IND-1)
                                          CSERVC-TARIF OF GFCTB0D8

                   PERFORM 2900-OBTER-DADOS-GFCTB0D8

                   MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTOP-DESC-TARIFA(IND-1)

090708     END-IF

           PERFORM 2242-LER-CSR11-GFCTB0B2.

      *----------------------------------------------------------------*
       2243-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE FECHAMENTO DO CURSOR 11 DA TABELA GFCTB0B2.       *
      ******************************************************************
      *----------------------------------------------------------------*
       2244-FECHAR-CSR11-GFCTB0B2      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR11-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0180'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTOP-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2244-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       2250-TRATAR-SEGMENTO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2251-ABRIR-CSR12-GFCTB0B2.

           PERFORM 2252-LER-CSR12-GFCTB0B2.

           PERFORM 2253-PROCESSAR-CSR12-GFCTB0B2   VARYING IND-1 FROM
                   1 BY 1                          UNTIL
                   IND-1                           GREATER 8 OR
                   WRK-FINALIZAR                   EQUAL 'S'.

           IF  WRK-FINALIZAR           NOT EQUAL 'S' AND
               IND-1                   GREATER 8
               MOVE CSERVC-TARIF       OF GFCTB0B2
                                       TO GFCTOP-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2254-FECHAR-CSR12-GFCTB0B2.

      *----------------------------------------------------------------*
       2250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE ABERTURA DO CURSOR 12 DA TABELA GFCTB0B2.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2251-ABRIR-CSR12-GFCTB0B2       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.
           MOVE GFCTOO-PONTEIRO-TARIFA TO CSERVC-TARIF      OF GFCTB0B2.

           MOVE  GFCTOO-CAGPTO-CTA     TO CAGPTO-CTA        OF GFCTB0C0.
           MOVE  GFCTOO-ARG4           TO CSGMTO-GSTAO-TARIF
                                                            OF GFCTB0C0.

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA        OF GFCTB0B2.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
                                          DFIM-FLEXZ-AGPTO  OF GFCTB0B2.

           EXEC SQL
               OPEN CSR12-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0190'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTOP-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2251-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE LEITURA DO CURSOR 12 DA TABELA GFCTB0B2.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2252-LER-CSR12-GFCTB0B2         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR12-GFCTB0B2  INTO
                   :GFCTB0B2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0200'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTOP-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTOP-ERRO
                   MOVE ZEROS          TO GFCTOP-COD-SQL-ERRO
                   MOVE 0859           TO GFCTOP-COD-MSG-ERRO
                   MOVE '0859'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTOP-DESC-MSG-ERRO
               END-IF
               GO TO 2252-99-FIM
           END-IF.

      ******************************************************************
      *    VERIFICA SE A FLEXIBILIZACAO JAH FOI RENOVADA               *
      ******************************************************************

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA       OF GFCTB0C0.
           MOVE GFCTOO-ARG4            TO CSGMTO-GSTAO-TARIF
                                                           OF GFCTB0C0.

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA       OF GFCTB0B2.
           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0C0.

           EXEC SQL
             SELECT
                   A.DINIC_FLEXZ_AGPTO,
                   A.DFIM_FLEXZ_AGPTO,
                   A.CSIT_SOLTC_FLEXZ
             INTO
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_SGMTO B
             WHERE
                   A.CAGPTO_CTA          = B.CAGPTO_CTA
             AND   A.CSERVC_TARIF        = B.CSERVC_TARIF
             AND   A.HIDTFD_FLEXZ_AGPTO  = B.HIDTFD_FLEXZ_AGPTO
             AND   A.CAGPTO_CTA          = :GFCTB0B2.CAGPTO-CTA
             AND   B.CSGMTO_GSTAO_TARIF  = :GFCTB0C0.CSGMTO-GSTAO-TARIF
             AND   B.CSERVC_TARIF        = :GFCTB0C0.CSERVC-TARIF
             AND   A.CSIT_SOLTC_FLEXZ    =  01
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0210'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               GO TO 2252-LER-CSR12-GFCTB0B2
           END-IF.

      *----------------------------------------------------------------*
       2252-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTO DO CURSOR 12 DA TABELA GFCTB0B2.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2253-PROCESSAR-CSR12-GFCTB0B2   SECTION.
      *----------------------------------------------------------------*

090708     MOVE 'N' TO WRK-TEM-PERMSS
090708
090708     PERFORM 2215-VERIFICA-PERMISSAO.
090708
090708     IF WRK-TEM-PERMSS = 'S'
090708
090708*            COMPUTE GFCTOP-LL = (GFCTOP-LL + 105)
090708             COMPUTE GFCTOP-LL = (GFCTOP-LL + 100)
090708             MOVE  CINDCD-PRMSS-AGNDA    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-AGNDA (IND-1)
090708             MOVE  CINDCD-PRMSS-VLR      OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-VALOR (IND-1)
090708             MOVE  CINDCD-PRMSS-PERC     OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-PERCENT(IND-1)
090708             MOVE  CINDCD-PRMSS-FRANQ    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-FRANQ (IND-1)
090708             MOVE  CINDCD-PRMSS-PERDC    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-PERDC (IND-1)

                   ADD 1               TO WRK-GFCTOP-QTDE-TOT-REG

                   MOVE CSERVC-TARIF OF GFCTB0B2
                                       TO GFCTOP-COD-TARIFA(IND-1)
                                          CSERVC-TARIF OF GFCTB0D8

                   PERFORM 2900-OBTER-DADOS-GFCTB0D8

                   MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTOP-DESC-TARIFA(IND-1)

090708     END-IF

           PERFORM 2252-LER-CSR12-GFCTB0B2.

      *----------------------------------------------------------------*
       2253-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE FECHAMENTO DO CURSOR 12 DA TABELA GFCTB0B2.       *
      ******************************************************************
      *----------------------------------------------------------------*
       2254-FECHAR-CSR12-GFCTB0B2      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR12-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0220'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTOP-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2254-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       2260-TRATAR-MUNICIPIO           SECTION.
      *----------------------------------------------------------------*

           PERFORM 2261-ABRIR-CSR13-GFCTB0B2.

           PERFORM 2262-LER-CSR13-GFCTB0B2.

           PERFORM 2263-PROCESSAR-CSR13-GFCTB0B2   VARYING IND-1 FROM
                   1 BY 1                          UNTIL
                   IND-1                           GREATER 8 OR
                   WRK-FINALIZAR                   EQUAL 'S'.

           IF  WRK-FINALIZAR           NOT EQUAL 'S' AND
               IND-1                   GREATER 8
               MOVE CSERVC-TARIF       OF GFCTB0B2
                                       TO GFCTOP-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2264-FECHAR-CSR13-GFCTB0B2.

      *----------------------------------------------------------------*
       2260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE ABERTURA DO CURSOR 13 DA TABELA GFCTB0B2.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2261-ABRIR-CSR13-GFCTB0B2       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.
           MOVE GFCTOO-PONTEIRO-TARIFA TO CSERVC-TARIF      OF GFCTB0B2.

           MOVE  GFCTOO-CAGPTO-CTA     TO CAGPTO-CTA        OF GFCTB0B7.
           MOVE  GFCTOO-ARG7           TO CMUN-IBGE         OF GFCTB0B7.

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA        OF GFCTB0B2.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
                                          DFIM-FLEXZ-AGPTO  OF GFCTB0B2.

           EXEC SQL
               OPEN CSR13-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0230'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTOP-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2261-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE LEITURA DO CURSOR 13 DA TABELA GFCTB0B2.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2262-LER-CSR13-GFCTB0B2         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR13-GFCTB0B2  INTO
                   :GFCTB0B2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0240'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTOP-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTOP-ERRO
                   MOVE ZEROS          TO GFCTOP-COD-SQL-ERRO
                   MOVE 0859           TO GFCTOP-COD-MSG-ERRO
                   MOVE '0859'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTOP-DESC-MSG-ERRO
               END-IF
               GO TO 2262-99-FIM
           END-IF.

      ******************************************************************
      *    VERIFICA SE A FLEXIBILIZACAO JAH FOI RENOVADA               *
      ******************************************************************

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA       OF GFCTB0B7.
           MOVE GFCTOO-ARG7            TO CMUN-IBGE        OF GFCTB0B7.

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA       OF GFCTB0B2.
           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B7.

           EXEC SQL
             SELECT
                   A.DINIC_FLEXZ_AGPTO,
                   A.DFIM_FLEXZ_AGPTO,
                   A.CSIT_SOLTC_FLEXZ
             INTO
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_MUN   B
             WHERE
                   A.CAGPTO_CTA          = B.CAGPTO_CTA
             AND   A.CSERVC_TARIF        = B.CSERVC_TARIF
             AND   A.HIDTFD_FLEXZ_AGPTO  = B.HIDTFD_FLEXZ_AGPTO
             AND   A.CAGPTO_CTA          = :GFCTB0B2.CAGPTO-CTA
             AND   B.CMUN_IBGE           = :GFCTB0B7.CMUN-IBGE
             AND   B.CSERVC_TARIF        = :GFCTB0B7.CSERVC-TARIF
             AND   A.CSIT_SOLTC_FLEXZ    =  01
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0250'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               GO TO 2262-LER-CSR13-GFCTB0B2
           END-IF.

      *----------------------------------------------------------------*
       2262-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTO DO CURSOR 13 DA TABELA GFCTB0B2.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2263-PROCESSAR-CSR13-GFCTB0B2   SECTION.
      *----------------------------------------------------------------*

090708     MOVE 'N' TO WRK-TEM-PERMSS
090708
090708     PERFORM 2215-VERIFICA-PERMISSAO.
090708
090708     IF WRK-TEM-PERMSS = 'S'
090708
090708*            COMPUTE GFCTOP-LL = (GFCTOP-LL + 105)
090708             COMPUTE GFCTOP-LL = (GFCTOP-LL + 100)
090708             MOVE  CINDCD-PRMSS-AGNDA    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-AGNDA (IND-1)
090708             MOVE  CINDCD-PRMSS-VLR      OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-VALOR (IND-1)
090708             MOVE  CINDCD-PRMSS-PERC     OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-PERCENT(IND-1)
090708             MOVE  CINDCD-PRMSS-FRANQ    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-FRANQ (IND-1)
090708             MOVE  CINDCD-PRMSS-PERDC    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-PERDC (IND-1)

                   ADD 1               TO WRK-GFCTOP-QTDE-TOT-REG

                   MOVE CSERVC-TARIF OF GFCTB0B2
                                       TO GFCTOP-COD-TARIFA(IND-1)
                                          CSERVC-TARIF OF GFCTB0D8

                   PERFORM 2900-OBTER-DADOS-GFCTB0D8

                   MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTOP-DESC-TARIFA(IND-1)

090708     END-IF

           PERFORM 2262-LER-CSR13-GFCTB0B2.

      *----------------------------------------------------------------*
       2263-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE FECHAMENTO DO CURSOR 13 DA TABELA GFCTB0B2.       *
      ******************************************************************
      *----------------------------------------------------------------*
       2264-FECHAR-CSR13-GFCTB0B2      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR13-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0260'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTOP-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2264-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2270-TRATAR-UF                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2271-ABRIR-CSR14-GFCTB0B2.

           PERFORM 2272-LER-CSR14-GFCTB0B2.

           PERFORM 2273-PROCESSAR-CSR14-GFCTB0B2   VARYING IND-1 FROM
                   1 BY 1                          UNTIL
                   IND-1                           GREATER 8 OR
                   WRK-FINALIZAR                   EQUAL 'S'.

           IF  WRK-FINALIZAR           NOT EQUAL 'S' AND
               IND-1                   GREATER 8
               MOVE CSERVC-TARIF       OF GFCTB0B2
                                       TO GFCTOP-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2274-FECHAR-CSR14-GFCTB0B2.

      *----------------------------------------------------------------*
       2270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE ABERTURA DO CURSOR 14 DA TABELA GFCTB0B2.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2271-ABRIR-CSR14-GFCTB0B2       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.
           MOVE GFCTOO-PONTEIRO-TARIFA TO CSERVC-TARIF      OF GFCTB0B2.

           MOVE  GFCTOO-CAGPTO-CTA     TO CAGPTO-CTA        OF GFCTB0C1.
           MOVE  GFCTOO-ARG3           TO CSGL-UF           OF GFCTB0C1.

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA        OF GFCTB0B2.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
                                          DFIM-FLEXZ-AGPTO  OF GFCTB0B2.

           EXEC SQL
               OPEN CSR14-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0270'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTOP-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2271-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE LEITURA DO CURSOR 14 DA TABELA GFCTB0B2.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2272-LER-CSR14-GFCTB0B2         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR14-GFCTB0B2  INTO
                   :GFCTB0B2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0280'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTOP-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTOP-ERRO
                   MOVE ZEROS          TO GFCTOP-COD-SQL-ERRO
                   MOVE 0859           TO GFCTOP-COD-MSG-ERRO
                   MOVE '0859'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTOP-DESC-MSG-ERRO
               END-IF
               GO TO 2272-99-FIM
           END-IF.

      ******************************************************************
      *    VERIFICA SE A FLEXIBILIZACAO JAH FOI RENOVADA               *
      ******************************************************************

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA       OF GFCTB0C1.
           MOVE GFCTOO-ARG3            TO CSGL-UF          OF GFCTB0C1.

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA       OF GFCTB0B2.
           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0C1.

           EXEC SQL
             SELECT
                   A.DINIC_FLEXZ_AGPTO,
                   A.DFIM_FLEXZ_AGPTO,
                   A.CSIT_SOLTC_FLEXZ
             INTO
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_UF    B
             WHERE
                   A.CAGPTO_CTA          = B.CAGPTO_CTA
             AND   A.CSERVC_TARIF        = B.CSERVC_TARIF
             AND   A.HIDTFD_FLEXZ_AGPTO  = B.HIDTFD_FLEXZ_AGPTO
             AND   A.CAGPTO_CTA          = :GFCTB0B2.CAGPTO-CTA
             AND   B.CSGL_UF             = :GFCTB0C1.CSGL-UF
             AND   B.CSERVC_TARIF        = :GFCTB0C1.CSERVC-TARIF
             AND   A.CSIT_SOLTC_FLEXZ    =  01
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0290'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               GO TO 2272-LER-CSR14-GFCTB0B2
           END-IF.

      *----------------------------------------------------------------*
       2272-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTO DO CURSOR 14 DA TABELA GFCTB0B2.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2273-PROCESSAR-CSR14-GFCTB0B2   SECTION.
      *----------------------------------------------------------------*

090708     MOVE 'N' TO WRK-TEM-PERMSS
090708
090708     PERFORM 2215-VERIFICA-PERMISSAO.
090708
090708     IF WRK-TEM-PERMSS = 'S'
090708
090708*            COMPUTE GFCTOP-LL = (GFCTOP-LL + 105)
090708             COMPUTE GFCTOP-LL = (GFCTOP-LL + 100)
090708             MOVE  CINDCD-PRMSS-AGNDA    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-AGNDA (IND-1)
090708             MOVE  CINDCD-PRMSS-VLR      OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-VALOR (IND-1)
090708             MOVE  CINDCD-PRMSS-PERC     OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-PERCENT(IND-1)
090708             MOVE  CINDCD-PRMSS-FRANQ    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-FRANQ (IND-1)
090708             MOVE  CINDCD-PRMSS-PERDC    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-PERDC (IND-1)

                   ADD 1               TO WRK-GFCTOP-QTDE-TOT-REG

                   MOVE CSERVC-TARIF OF GFCTB0B2
                                       TO GFCTOP-COD-TARIFA(IND-1)
                                          CSERVC-TARIF OF GFCTB0D8

                   PERFORM 2900-OBTER-DADOS-GFCTB0D8

                   MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTOP-DESC-TARIFA(IND-1)

090708     END-IF

           PERFORM 2272-LER-CSR14-GFCTB0B2.

      *----------------------------------------------------------------*
       2273-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE FECHAMENTO DO CURSOR 14 DA TABELA GFCTB0B2.       *
      ******************************************************************
      *----------------------------------------------------------------*
       2274-FECHAR-CSR14-GFCTB0B2      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR14-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0300'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTOP-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2274-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

090708*----------------------------------------------------------------*
090708 2280-TRATAR-PERSONALIZADOS      SECTION.
090708*----------------------------------------------------------------*
090708
090708     PERFORM 2281-ABRIR-CSR15-GFCTB0B2.
090708
090708     PERFORM 2282-LER-CSR15-GFCTB0B2.
090708
090708     PERFORM 2283-PROCESSAR-CSR15-GFCTB0B2   VARYING IND-1 FROM
090708             1 BY 1                          UNTIL
090708             IND-1                           GREATER 8 OR
090708             WRK-FINALIZAR                   EQUAL 'S'.
090708
090708     IF  WRK-FINALIZAR           NOT EQUAL 'S' AND
090708         IND-1                   GREATER 8
090708         MOVE CSERVC-TARIF       OF GFCTB0B2
090708                                 TO GFCTOP-PONTEIRO-TARIFA
090708     END-IF.
090708
090708     PERFORM 2284-FECHAR-CSR15-GFCTB0B2.
090708
090708*----------------------------------------------------------------*
090708 2280-99-FIM.                    EXIT.
090708*----------------------------------------------------------------*
090708
090708******************************************************************
090708*    ROTINA DE ABERTURA DO CURSOR 15 DA TABELA GFCTB0B2.         *
090708******************************************************************
090708*----------------------------------------------------------------*
090708 2281-ABRIR-CSR15-GFCTB0B2       SECTION.
090708*----------------------------------------------------------------*
090708
090708     MOVE 'N'                    TO WRK-FINALIZAR.
090708     MOVE GFCTOO-PONTEIRO-TARIFA TO CSERVC-TARIF      OF GFCTB0B2.
090708
090708     MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA        OF GFCTB0B2.
090708     MOVE DPROCM-ATUAL OF GFCTB0A1
090708                                 TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
090708                                    DFIM-FLEXZ-AGPTO  OF GFCTB0B2.
090708
090708     EXEC SQL
090708         OPEN CSR15-GFCTB0B2
090708     END-EXEC.
090708
090708     IF (SQLCODE                 NOT EQUAL ZEROS) OR
090708        (SQLWARN0                EQUAL 'W'      )
090708         MOVE SPACES             TO GFCT0M-ERRO-SQL
090708         INITIALIZE GFCT0M-ERRO-SQL
090708         MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
090708         MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
090708         MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
090708         MOVE '0310'             TO GFCT0M-LOCAL
090708         MOVE 0005               TO GFCTOP-COD-MSG-ERRO
090708         MOVE '0005'             TO GFCTG2-COD-MSG
090708         PERFORM 2999-FORMATAR-ERRO-DB2
090708         PERFORM 3000-FINALIZAR
090708     END-IF.
090708
090708*----------------------------------------------------------------*
090708 2281-99-FIM.                    EXIT.
090708*----------------------------------------------------------------*
090708
090708******************************************************************
090708*    ROTINA DE LEITURA DO CURSOR 15 DA TABELA GFCTB0B2.          *
090708******************************************************************
090708*----------------------------------------------------------------*
090708 2282-LER-CSR15-GFCTB0B2         SECTION.
090708*----------------------------------------------------------------*
090708
090708     EXEC SQL
090708         FETCH CSR15-GFCTB0B2  INTO
090708             :GFCTB0B2.CSERVC-TARIF
090708     END-EXEC.
090708
090708     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
090708        (SQLWARN0                EQUAL 'W'      )
090708         MOVE SPACES             TO GFCT0M-ERRO-SQL
090708         INITIALIZE GFCT0M-ERRO-SQL
090708         MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
090708         MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
090708         MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
090708         MOVE '0320'             TO GFCT0M-LOCAL
090708         MOVE 0006               TO GFCTOP-COD-MSG-ERRO
090708         MOVE '0006'             TO GFCTG2-COD-MSG
090708         PERFORM 2999-FORMATAR-ERRO-DB2
090708         PERFORM 3000-FINALIZAR
090708     END-IF.
090708
090708     IF  SQLCODE                 EQUAL +100
090708         MOVE 'S'                TO WRK-FINALIZAR
090708                                    GFCTOP-FIM
090708         IF  IND-1               EQUAL ZEROS
090708             MOVE 1              TO GFCTOP-ERRO
090708             MOVE ZEROS          TO GFCTOP-COD-SQL-ERRO
090708             MOVE 0859           TO GFCTOP-COD-MSG-ERRO
090708             MOVE '0859'         TO GFCTG2-COD-MSG
090708             PERFORM 1310-OBTER-MENSAGEM
090708             MOVE GFCTG3-DESC-MSG TO GFCTOP-DESC-MSG-ERRO
090708         END-IF
090708         GO TO 2282-99-FIM
090708     END-IF.
090708
090708******************************************************************
090708*    VERIFICA SE A FLEXIBILIZACAO JAH FOI RENOVADA               *
090708******************************************************************
090708
090708     MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA       OF GFCTB0B2.
090708     MOVE CSERVC-TARIF           OF GFCTB0B2
090708                                 TO CSERVC-TARIF     OF GFCTB0B2.
090708
090708     EXEC SQL
090708       SELECT
090708             DINIC_FLEXZ_AGPTO,
090708             DFIM_FLEXZ_AGPTO,
090708             CSIT_SOLTC_FLEXZ
090708       INTO
090708             :GFCTB0B2.DINIC-FLEXZ-AGPTO,
090708             :GFCTB0B2.DFIM-FLEXZ-AGPTO,
090708             :GFCTB0B2.CSIT-SOLTC-FLEXZ
090708       FROM   DB2PRD.PDIDO_FLEXZ_AGPTO
090708       WHERE
090708             CAGPTO_CTA            = :GFCTB0B2.CAGPTO-CTA
090708       AND   CSERVC_TARIF          = :GFCTB0B2.CSERVC-TARIF
090708       AND   CSIT_SOLTC_FLEXZ      =  01
090708     END-EXEC.
090708
090708     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
090708        (SQLWARN0                EQUAL 'W'      )
090708         MOVE SPACES             TO GFCT0M-ERRO-SQL
090708         INITIALIZE GFCT0M-ERRO-SQL
090708         MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
090708         MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
090708         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
090708         MOVE '0330'             TO GFCT0M-LOCAL
090708         MOVE 0006               TO GFCTOP-COD-MSG-ERRO
090708         MOVE '0006'             TO GFCTG2-COD-MSG
090708         PERFORM 2999-FORMATAR-ERRO-DB2
090708         PERFORM 3000-FINALIZAR
090708     END-IF.
090708
090708     IF  SQLCODE                 EQUAL ZEROS
090708         GO TO 2282-LER-CSR15-GFCTB0B2
090708     END-IF.
090708
090708*----------------------------------------------------------------*
090708 2282-99-FIM.                    EXIT.
090708*----------------------------------------------------------------*
090708
090708******************************************************************
090708*    ROTINA DE PROCESSAMENTO DO CURSOR 15 DA TABELA GFCTB0B2.    *
090708******************************************************************
090708*----------------------------------------------------------------*
090708 2283-PROCESSAR-CSR15-GFCTB0B2   SECTION.
090708*----------------------------------------------------------------*
090708
090708     MOVE 'N' TO WRK-TEM-PERMSS
090708
090708     PERFORM 2215-VERIFICA-PERMISSAO.
090708
090708     IF WRK-TEM-PERMSS = 'S'
090708
090708*            COMPUTE GFCTOP-LL = (GFCTOP-LL + 105)
090708             COMPUTE GFCTOP-LL = (GFCTOP-LL + 100)
090708             MOVE  CINDCD-PRMSS-AGNDA    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-AGNDA (IND-1)
090708             MOVE  CINDCD-PRMSS-VLR      OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-VALOR (IND-1)
090708             MOVE  CINDCD-PRMSS-PERC     OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-PERCENT(IND-1)
090708             MOVE  CINDCD-PRMSS-FRANQ    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-FRANQ (IND-1)
090708             MOVE  CINDCD-PRMSS-PERDC    OF GFCTB0J7
090708                                 TO GFCTOP-PRMSS-PERDC (IND-1)
090708
090708             ADD 1               TO WRK-GFCTOP-QTDE-TOT-REG
090708
090708             MOVE CSERVC-TARIF OF GFCTB0B2
090708                                 TO GFCTOP-COD-TARIFA(IND-1)
090708                                    CSERVC-TARIF OF GFCTB0D8
090708
090708             PERFORM 2900-OBTER-DADOS-GFCTB0D8
090708
090708             MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
090708                                 TO GFCTOP-DESC-TARIFA(IND-1)
090708
090708     END-IF
090708
090708     PERFORM 2282-LER-CSR15-GFCTB0B2.
090708
090708*----------------------------------------------------------------*
090708 2283-99-FIM.                    EXIT.
090708*----------------------------------------------------------------*
090708
090708******************************************************************
090708*    ROTINA DE FECHAMENTO DO CURSOR 15 DA TABELA GFCTB0B2.       *
090708******************************************************************
090708*----------------------------------------------------------------*
090708 2284-FECHAR-CSR15-GFCTB0B2      SECTION.
090708*----------------------------------------------------------------*
090708
090708     EXEC SQL
090708       CLOSE CSR15-GFCTB0B2
090708     END-EXEC.
090708
090708     IF (SQLCODE                 NOT EQUAL ZEROS) OR
090708        (SQLWARN0                EQUAL 'W'      )
090708         MOVE SPACES             TO GFCT0M-ERRO-SQL
090708         INITIALIZE GFCT0M-ERRO-SQL
090708         MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
090708         MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
090708         MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
090708         MOVE '0340'             TO GFCT0M-LOCAL
090708         MOVE 0011               TO GFCTOP-COD-MSG-ERRO
090708         MOVE '0011'             TO GFCTG2-COD-MSG
090708         PERFORM 2999-FORMATAR-ERRO-DB2
090708         PERFORM 3000-FINALIZAR
090708     END-IF.
090708
090708*----------------------------------------------------------------*
090708 2284-99-FIM.                    EXIT.
090708*----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TRATAMENTO DO CURSOR 02.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-TRATAR-CURSOR-2            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2310-ABRIR-CSR02-GFCTB0B2.

           PERFORM 2320-LER-CSR02-GFCTB0B2.

           PERFORM 2330-PROCESSAR-CSR02-GFCTB0B2   VARYING IND-1 FROM
                   1 BY 1                          UNTIL
                   IND-1                           GREATER 8 OR
                   WRK-FINALIZAR                   EQUAL 'S'.

           IF  WRK-FINALIZAR           NOT EQUAL 'S' AND
               IND-1                   GREATER 8
               MOVE CSERVC-TARIF       OF GFCTB0B2
                                       TO GFCTOP-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2340-FECHAR-CSR02-GFCTB0B2.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE ABERTURA DO CURSOR 02 DA TABELA GFCTB0B2.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-ABRIR-CSR02-GFCTB0B2       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.
           MOVE GFCTOO-PONTEIRO-TARIFA TO CSERVC-TARIF OF GFCTB0B2.

           MOVE 01                     TO WRK-CSIT.
           MOVE 02                     TO WRK-CSIT-AUX.
           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA OF GFCTB0B2.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
                                          DFIM-FLEXZ-AGPTO OF GFCTB0B2.

           EXEC SQL
               OPEN CSR02-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0350'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTOP-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE LEITURA DO CURSOR 02 DA TABELA GFCTB0B2.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-LER-CSR02-GFCTB0B2         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR02-GFCTB0B2  INTO
                   :GFCTB0B2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'FETCH '           TO GFCT0M-COMANDO-SQL
               MOVE '0360'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTOP-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTOP-ERRO
                   MOVE ZEROS          TO GFCTOP-COD-SQL-ERRO
                   MOVE 0860           TO GFCTOP-COD-MSG-ERRO
                   MOVE '0860'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTOP-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTO DO CURSOR 02 DA TABELA GFCTB0B2.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2330-PROCESSAR-CSR02-GFCTB0B2   SECTION.
      *----------------------------------------------------------------*

090708*    COMPUTE GFCTOP-LL = (GFCTOP-LL + 105)
090708     COMPUTE GFCTOP-LL = (GFCTOP-LL + 100)

           ADD 1                       TO WRK-GFCTOP-QTDE-TOT-REG.

           MOVE CSERVC-TARIF OF GFCTB0B2
                                       TO GFCTOP-COD-TARIFA(IND-1)
                                          CSERVC-TARIF OF GFCTB0D8.

           PERFORM 2900-OBTER-DADOS-GFCTB0D8.

           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTOP-DESC-TARIFA(IND-1)

           PERFORM 2320-LER-CSR02-GFCTB0B2.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE FECHAMENTO DO CURSOR 02 DA TABELA GFCTB0B2.       *
      ******************************************************************
      *----------------------------------------------------------------*
       2340-FECHAR-CSR02-GFCTB0B2      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR02-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'CLOSE '           TO GFCT0M-COMANDO-SQL
               MOVE '0370'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTOP-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TRATAMENTO DO CURSOR 03.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-TRATAR-CURSOR-3            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2410-ABRIR-CSR03-GFCTB049.

           PERFORM 2420-LER-CSR03-GFCTB049.

           PERFORM 2430-PROCESSAR-CSR03-GFCTB049   VARYING IND-1 FROM
                   1 BY 1                          UNTIL
                   IND-1                           GREATER 8 OR
                   WRK-FINALIZAR                   EQUAL 'S'.

           IF  WRK-FINALIZAR           NOT EQUAL 'S' AND
               IND-1                   GREATER 8
               MOVE CSERVC-TARIF       OF GFCTB049
                                       TO GFCTOP-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2440-FECHAR-CSR03-GFCTB049.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE ABERTURA DO CURSOR 03 DA TABELA GFCTB049.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-ABRIR-CSR03-GFCTB049       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.
           MOVE GFCTOO-PONTEIRO-TARIFA TO CSERVC-TARIF OF GFCTB049.

           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA OF GFCTB049.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB049
                                          DFIM-FLEXZ-AGPTO OF GFCTB049.

           EXEC SQL
               OPEN CSR03-GFCTB049
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_AGPTO_CTA'  TO GFCT0M-NOME-TAB
               MOVE 'GFCTB049'         TO WRK-NOM-TAB
               MOVE 'OPEN  '           TO GFCT0M-COMANDO-SQL
               MOVE '0380'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTOP-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE LEITURA DO CURSOR 03 DA TABELA GFCTB049.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2420-LER-CSR03-GFCTB049         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR03-GFCTB049  INTO
                   :GFCTB049.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_AGPTO_CTA'  TO GFCT0M-NOME-TAB
               MOVE 'GFCTB049'         TO WRK-NOM-TAB
               MOVE 'FETCH '           TO GFCT0M-COMANDO-SQL
               MOVE '0390'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTOP-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTOP-ERRO
                   MOVE ZEROS          TO GFCTOP-COD-SQL-ERRO
                   MOVE 0861           TO GFCTOP-COD-MSG-ERRO
                   MOVE '0861'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG  TO GFCTOP-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTO DO CURSOR 03 DA TABELA GFCTB049.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2430-PROCESSAR-CSR03-GFCTB049   SECTION.
      *----------------------------------------------------------------*

090708*    COMPUTE GFCTOP-LL = (GFCTOP-LL + 105)
090708     COMPUTE GFCTOP-LL = (GFCTOP-LL + 100)

           ADD 1                       TO WRK-GFCTOP-QTDE-TOT-REG.

           MOVE CSERVC-TARIF OF GFCTB049
                                       TO GFCTOP-COD-TARIFA(IND-1)
                                          CSERVC-TARIF OF GFCTB0D8.

           PERFORM 2900-OBTER-DADOS-GFCTB0D8.

           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTOP-DESC-TARIFA(IND-1)

           PERFORM 2420-LER-CSR03-GFCTB049.

      *----------------------------------------------------------------*
       2430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE FECHAMENTO DO CURSOR 03 DA TABELA GFCTB049.       *
      ******************************************************************
      *----------------------------------------------------------------*
       2440-FECHAR-CSR03-GFCTB049      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR03-GFCTB049
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_APGTO_CTA'  TO GFCT0M-NOME-TAB
               MOVE 'GFCTB049'         TO WRK-NOM-TAB
               MOVE 'CLOSE '           TO GFCT0M-COMANDO-SQL
               MOVE '0400'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTOP-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TRATAMENTO DO CURSOR 04.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-TRATAR-CURSOR-4            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2510-ABRIR-CSR04-GFCTB0B2.

           PERFORM 2520-LER-CSR04-GFCTB0B2.

           PERFORM 2530-PROCESSAR-CSR04-GFCTB0B2   VARYING IND-1 FROM
                   1 BY 1                          UNTIL
                   IND-1                           GREATER 8 OR
                   WRK-FINALIZAR                   EQUAL 'S'.

           IF  WRK-FINALIZAR           NOT EQUAL 'S' AND
               IND-1                   GREATER 8
               MOVE CSERVC-TARIF       OF GFCTB0B2
                                       TO GFCTOP-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2540-FECHAR-CSR04-GFCTB0B2.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE ABERTURA DO CURSOR 04 DA TABELA GFCTB0B2.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2510-ABRIR-CSR04-GFCTB0B2       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.
           MOVE GFCTOO-PONTEIRO-TARIFA TO CSERVC-TARIF OF GFCTB0B2.
           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA OF GFCTB0B2.

           EXEC SQL
               OPEN CSR04-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'OPEN  '           TO GFCT0M-COMANDO-SQL
               MOVE '0410'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTOP-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE LEITURA DO CURSOR 04 DA TABELA GFCTB0B2.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2520-LER-CSR04-GFCTB0B2         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR04-GFCTB0B2  INTO
                   :GFCTB0B2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'FETCH '           TO GFCT0M-COMANDO-SQL
               MOVE '0420'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTOP-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTOP-ERRO
                   MOVE ZEROS          TO GFCTOP-COD-SQL-ERRO
                   MOVE 0862           TO GFCTOP-COD-MSG-ERRO
                   MOVE '0862'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG  TO GFCTOP-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTO DO CURSOR 04 DA TABELA GFCTB0B2.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2530-PROCESSAR-CSR04-GFCTB0B2   SECTION.
      *----------------------------------------------------------------*

090708*    COMPUTE GFCTOP-LL = (GFCTOP-LL + 105)
090708     COMPUTE GFCTOP-LL = (GFCTOP-LL + 100)

           ADD 1                       TO WRK-GFCTOP-QTDE-TOT-REG.

           MOVE CSERVC-TARIF OF GFCTB0B2
                                       TO GFCTOP-COD-TARIFA(IND-1)
                                          CSERVC-TARIF OF GFCTB0D8.

           PERFORM 2900-OBTER-DADOS-GFCTB0D8.

           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTOP-DESC-TARIFA(IND-1)

           PERFORM 2520-LER-CSR04-GFCTB0B2.

      *----------------------------------------------------------------*
       2530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE FECHAMENTO DO CURSOR 04 DA TABELA GFCTB0B2.       *
      ******************************************************************
      *----------------------------------------------------------------*
       2540-FECHAR-CSR04-GFCTB0B2      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR04-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'CLOSE '           TO GFCT0M-COMANDO-SQL
               MOVE '0430'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTOP-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TRATAMENTO DO CURSOR 05.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-TRATAR-CURSOR-5            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2610-ABRIR-CSR05-GFCTB0B2.

           PERFORM 2620-LER-CSR05-GFCTB0B2.

           PERFORM 2630-PROCESSAR-CSR05-GFCTB0B2   VARYING IND-1 FROM
                   1 BY 1                          UNTIL
                   IND-1                           GREATER 8 OR
                   WRK-FINALIZAR                   EQUAL 'S'.

           IF  WRK-FINALIZAR           NOT EQUAL 'S' AND
               IND-1                   GREATER 8
               MOVE CSERVC-TARIF       OF GFCTB0B2
                                       TO GFCTOP-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2640-FECHAR-CSR05-GFCTB0B2.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE ABERTURA DO CURSOR 05 DA TABELA GFCTB0B2.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2610-ABRIR-CSR05-GFCTB0B2       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.
           MOVE GFCTOO-PONTEIRO-TARIFA TO CSERVC-TARIF OF GFCTB0B2.
           MOVE 01                     TO WRK-CSIT.
           MOVE 01                     TO WRK-CSIT-AUX.
           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA OF GFCTB0B2.

           EXEC SQL
               OPEN CSR05-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'OPEN  '           TO GFCT0M-COMANDO-SQL
               MOVE '0440'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTOP-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE LEITURA DO CURSOR 05 DA TABELA GFCTB0B2.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2620-LER-CSR05-GFCTB0B2         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR05-GFCTB0B2  INTO
                   :GFCTB0B2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'FETCH '           TO GFCT0M-COMANDO-SQL
               MOVE '0450'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTOP-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTOP-ERRO
                   MOVE ZEROS          TO GFCTOP-COD-SQL-ERRO
                   MOVE 0863           TO GFCTOP-COD-MSG-ERRO
                   MOVE '0863'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG  TO GFCTOP-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTO DO CURSOR 05 DA TABELA GFCTB0B2.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2630-PROCESSAR-CSR05-GFCTB0B2   SECTION.
      *----------------------------------------------------------------*

090708*    COMPUTE GFCTOP-LL = (GFCTOP-LL + 105)
090708     COMPUTE GFCTOP-LL = (GFCTOP-LL + 100)

           ADD 1                       TO WRK-GFCTOP-QTDE-TOT-REG.

           MOVE CSERVC-TARIF OF GFCTB0B2
                                       TO GFCTOP-COD-TARIFA(IND-1)
                                          CSERVC-TARIF OF GFCTB0D8.

           PERFORM 2900-OBTER-DADOS-GFCTB0D8.

           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTOP-DESC-TARIFA(IND-1)

           PERFORM 2620-LER-CSR05-GFCTB0B2.

      *----------------------------------------------------------------*
       2630-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE FECHAMENTO DO CURSOR 05 DA TABELA GFCTB0B2.       *
      ******************************************************************
      *----------------------------------------------------------------*
       2640-FECHAR-CSR05-GFCTB0B2      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR05-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'CLOSE '           TO GFCT0M-COMANDO-SQL
               MOVE '0460'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTOP-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2640-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TRATAMENTO DO CURSOR 06.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2700-TRATAR-CURSOR-6            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2710-ABRIR-CSR06-GFCTB0B2.

           PERFORM 2720-LER-CSR06-GFCTB0B2.

           PERFORM 2730-PROCESSAR-CSR06-GFCTB0B2   VARYING IND-1 FROM
                   1 BY 1                          UNTIL
                   IND-1                           GREATER 8 OR
                   WRK-FINALIZAR                   EQUAL 'S'.

           IF  WRK-FINALIZAR           NOT EQUAL 'S' AND
               IND-1                   GREATER 8
               MOVE CSERVC-TARIF       OF GFCTB0B2
                                       TO GFCTOP-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2740-FECHAR-CSR06-GFCTB0B2.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE ABERTURA DO CURSOR 06 DA TABELA GFCTB0B2.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2710-ABRIR-CSR06-GFCTB0B2       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.
           MOVE GFCTOO-PONTEIRO-TARIFA TO CSERVC-TARIF OF GFCTB0B2.
           MOVE 02                     TO WRK-CSIT.
           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA OF GFCTB0B2.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
                                          DFIM-FLEXZ-AGPTO OF GFCTB0B2.

           EXEC SQL
               OPEN CSR06-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'OPEN  '           TO GFCT0M-COMANDO-SQL
               MOVE '0470'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTOP-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      *    ROTINA DE LEITURA DO CURSOR 06 DA TABELA GFCTB0B2.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2720-LER-CSR06-GFCTB0B2         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR06-GFCTB0B2  INTO
                   :GFCTB0B2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'FETCH '           TO GFCT0M-COMANDO-SQL
               MOVE '0480'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTOP-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTOP-ERRO
                   MOVE ZEROS          TO GFCTOP-COD-SQL-ERRO
                   MOVE 0864           TO GFCTOP-COD-MSG-ERRO
                   MOVE '0864'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG  TO GFCTOP-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTO DO CURSOR 06 DA TABELA GFCTB0B2.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2730-PROCESSAR-CSR06-GFCTB0B2   SECTION.
      *----------------------------------------------------------------*

090708*    COMPUTE GFCTOP-LL = (GFCTOP-LL + 105)
090708     COMPUTE GFCTOP-LL = (GFCTOP-LL + 100)

           ADD 1                       TO WRK-GFCTOP-QTDE-TOT-REG.

           MOVE CSERVC-TARIF OF GFCTB0B2
                                       TO GFCTOP-COD-TARIFA(IND-1)
                                          CSERVC-TARIF OF GFCTB0D8.

           PERFORM 2900-OBTER-DADOS-GFCTB0D8.

           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTOP-DESC-TARIFA(IND-1)

           PERFORM 2720-LER-CSR06-GFCTB0B2.

      *----------------------------------------------------------------*
       2730-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE FECHAMENTO DO CURSOR 06 DA TABELA GFCTB0B2.       *
      ******************************************************************
      *----------------------------------------------------------------*
       2740-FECHAR-CSR06-GFCTB0B2      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR06-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'CLOSE '           TO GFCT0M-COMANDO-SQL
               MOVE '0490'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTOP-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2740-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TRATAMENTO DO CURSOR 07.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2800-TRATAR-CURSOR-7            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2810-ABRIR-CSR07-GFCTB049.

           PERFORM 2820-LER-CSR07-GFCTB049.

           PERFORM 2830-PROCESSAR-CSR07-GFCTB049   VARYING IND-1 FROM
                   1 BY 1                          UNTIL
                   IND-1                           GREATER 8 OR
                   WRK-FINALIZAR                   EQUAL 'S'.

           IF  WRK-FINALIZAR           NOT EQUAL 'S' AND
               IND-1                   GREATER 8
               MOVE CSERVC-TARIF       OF GFCTB049
                                       TO GFCTOP-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2840-FECHAR-CSR07-GFCTB049.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE ABERTURA DO CURSOR 07 DA TABELA GFCTB049.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2810-ABRIR-CSR07-GFCTB049       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.
           MOVE GFCTOO-PONTEIRO-TARIFA TO CSERVC-TARIF OF GFCTB049.
           MOVE GFCTOO-CAGPTO-CTA      TO CAGPTO-CTA OF GFCTB049.

           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB049
                                          DFIM-FLEXZ-AGPTO OF GFCTB049.

           EXEC SQL
               OPEN CSR07-GFCTB049
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_AGPTO_CTA'  TO GFCT0M-NOME-TAB
               MOVE 'GFCTB049'         TO WRK-NOM-TAB
               MOVE 'OPEN  '           TO GFCT0M-COMANDO-SQL
               MOVE '0500'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTOP-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE LEITURA DO CURSOR 07 DA TABELA GFCTB049.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2820-LER-CSR07-GFCTB049         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR07-GFCTB049  INTO
                   :GFCTB049.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_AGPTO_CTA'  TO GFCT0M-NOME-TAB
               MOVE 'GFCTB049'         TO WRK-NOM-TAB
               MOVE 'FETCH '           TO GFCT0M-COMANDO-SQL
               MOVE '0510'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTOP-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTOP-ERRO
                   MOVE ZEROS          TO GFCTOP-COD-SQL-ERRO
                   MOVE 0865           TO GFCTOP-COD-MSG-ERRO
                   MOVE '0865'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG  TO GFCTOP-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2820-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTO DO CURSOR 07 DA TABELA GFCTB049.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2830-PROCESSAR-CSR07-GFCTB049   SECTION.
      *----------------------------------------------------------------*

090708*    COMPUTE GFCTOP-LL = (GFCTOP-LL + 105)
090708     COMPUTE GFCTOP-LL = (GFCTOP-LL + 100)

           ADD 1                       TO WRK-GFCTOP-QTDE-TOT-REG.

           MOVE CSERVC-TARIF OF GFCTB049
                                       TO GFCTOP-COD-TARIFA(IND-1)
                                          CSERVC-TARIF OF GFCTB0D8.

           PERFORM 2900-OBTER-DADOS-GFCTB0D8.

           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTOP-DESC-TARIFA(IND-1)

           PERFORM 2820-LER-CSR07-GFCTB049.

      *----------------------------------------------------------------*
       2830-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE FECHAMENTO DO CURSOR 07 DA TABELA GFCTB049.       *
      ******************************************************************
      *----------------------------------------------------------------*
       2840-FECHAR-CSR07-GFCTB049      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR07-GFCTB049
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_AGPTO_CTA'  TO GFCT0M-NOME-TAB
               MOVE 'GFCTB049'         TO WRK-NOM-TAB
               MOVE 'CLOSE '           TO GFCT0M-COMANDO-SQL
               MOVE '0520'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTOP-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2840-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

110510*----------------------------------------------------------------*
       2850-TRATAR-RAZAO               SECTION.
      *----------------------------------------------------------------*

           PERFORM 2855-ABRIR-CSR16.

           PERFORM 2860-LER-CSR16.

           PERFORM 2870-PROCESSAR-CSR16           VARYING IND-1 FROM
                   1 BY 1                          UNTIL
                   IND-1                           GREATER 8 OR
                   WRK-FINALIZAR                   EQUAL 'S'.

           IF  WRK-FINALIZAR           NOT EQUAL 'S' AND
               IND-1                   GREATER 8
               MOVE CSERVC-TARIF       OF GFCTB0B2
                                       TO GFCTOP-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2880-FECHAR-CSR16.

      *----------------------------------------------------------------*
       2850-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE ABERTURA DO CURSOR 16 DA TABELA GFCTB0B2.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2855-ABRIR-CSR16                SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           MOVE  GFCTOO-CAGPTO-CTA     TO CAGPTO-CTA       OF GFCTB0M7.
           MOVE  GFCTOO-ARG8           TO CGRP-CTBIL-ATUAL OF GFCTB0M7.
           MOVE  GFCTOO-ARG9           TO CSGRP-CTBIL      OF GFCTB0M7.

           IF  GFCTOO-PONTEIRO-TARIFA EQUAL ZEROS
               MOVE 1                      TO WRK-CSERVC-TARIF-INI
           ELSE
               MOVE GFCTOO-PONTEIRO-TARIFA
                                           TO WRK-CSERVC-TARIF-INI
           END-IF.

           MOVE  99999                     TO WRK-CSERVC-TARIF-FIM.

           EXEC SQL
               OPEN CSR16-GFCTB0B2-JN
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0530'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTOP-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2855-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE LEITURA DO CURSOR 16 DA TABELA GFCTB0XX.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2860-LER-CSR16                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR16-GFCTB0B2-JN  INTO
                   :GFCTB0B2.CSERVC-TARIF,
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0540'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTOP-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTOP-ERRO
                   MOVE ZEROS          TO GFCTOP-COD-SQL-ERRO
                   MOVE 0859           TO GFCTOP-COD-MSG-ERRO
                   MOVE '0859'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTOP-DESC-MSG-ERRO
               END-IF
               GO TO 2860-99-FIM
           END-IF.

           MOVE 'N'              TO WRK-TEM-PERMSS

0710       MOVE  DINIC-FLEXZ-AGPTO OF GFCTB0B2 TO WRK-DATA-DB2
.          MOVE  WRK-ANO-DB2       TO WRK-ANO-INIC-FLEXZ-INV
.          MOVE  WRK-MES-DB2       TO WRK-MES-INIC-FLEXZ-INV
.          MOVE  WRK-DIA-DB2       TO WRK-DIA-INIC-FLEXZ-INV

010610     IF  CSIT-SOLTC-FLEXZ OF GFCTB0B2 EQUAL 02 AND
0710          (WRK-DINIC-FLEXZ-INV NOT GREATER WRK-DPROCM-ATUAL-INV)
010610         PERFORM 2865-VERIFICA-RENOVACAO
010610     ELSE
010610       IF  CSIT-SOLTC-FLEXZ OF GFCTB0B2 EQUAL 05 OR 08
010610           PERFORM 2865-VERIFICA-RENOVACAO
010610       ELSE
010610           GO TO 2860-LER-CSR16
010610       END-IF
010610     END-IF.

           IF  WRK-TEM-PERMSS    EQUAL  'S'
               GO    TO 2860-LER-CSR16
           END-IF.

      *----------------------------------------------------------------*
       2860-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA SE A FLEXIBILIZACAO JAH FOI RENOVADA               *
      ******************************************************************
      *----------------------------------------------------------------*
       2865-VERIFICA-RENOVACAO         SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCTOO-CAGPTO-CTA     TO CAGPTO-CTA       OF GFCTB0B2.
           MOVE  GFCTOO-ARG8           TO CGRP-CTBIL-ATUAL OF GFCTB0M7.
           MOVE  GFCTOO-ARG9           TO CSGRP-CTBIL      OF GFCTB0M7.

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0M7.

           EXEC SQL
             SELECT
                   A.DINIC_FLEXZ_AGPTO,
                   A.DFIM_FLEXZ_AGPTO,
                   A.CSIT_SOLTC_FLEXZ
             INTO
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.TPDIDO_FLEXZ_RZ   B
             WHERE
                   A.CAGPTO_CTA          = B.CAGPTO_CTA
             AND   A.CSERVC_TARIF        = B.CSERVC_TARIF
             AND   A.HIDTFD_FLEXZ_AGPTO  = B.HIDTFD_FLEXZ_AGPTO
             AND   A.CAGPTO_CTA           = :GFCTB0B2.CAGPTO-CTA
             AND   B.CGRP_CTBIL_ATUAL     = :GFCTB0M7.CGRP-CTBIL-ATUAL
             AND   B.CSGRP_CTBIL          = :GFCTB0M7.CSGRP-CTBIL
             AND   B.CSERVC_TARIF         = :GFCTB0M7.CSERVC-TARIF
             AND   A.CSIT_SOLTC_FLEXZ     =  01
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0550'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOP-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 'S'                TO WRK-TEM-PERMSS
           END-IF.

      *----------------------------------------------------------------*
       2865-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTO DO CURSOR 16 DA TABELA GFCTB0XX.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2870-PROCESSAR-CSR16            SECTION.
      *----------------------------------------------------------------*

           MOVE 'N' TO WRK-TEM-PERMSS

           PERFORM 2215-VERIFICA-PERMISSAO.

           IF  WRK-TEM-PERMSS          EQUAL 'S'

               COMPUTE GFCTOP-LL = (GFCTOP-LL + 100)
               MOVE  CINDCD-PRMSS-AGNDA    OF GFCTB0J7
                                       TO GFCTOP-PRMSS-AGNDA (IND-1)
               MOVE  CINDCD-PRMSS-VLR      OF GFCTB0J7
                                       TO GFCTOP-PRMSS-VALOR (IND-1)
               MOVE  CINDCD-PRMSS-PERC     OF GFCTB0J7
                                       TO GFCTOP-PRMSS-PERCENT(IND-1)
               MOVE  CINDCD-PRMSS-FRANQ    OF GFCTB0J7
                                       TO GFCTOP-PRMSS-FRANQ (IND-1)
               MOVE  CINDCD-PRMSS-PERDC    OF GFCTB0J7
                                       TO GFCTOP-PRMSS-PERDC (IND-1)

0610XS         MOVE WRK-GFCTOP-QTDE-TOT-REG
0610XS                                 TO WRK-QTDE-TOT-REG
HEXA           ADD  1                  TO WRK-QTDE-TOT-REG
.              MOVE WRK-QTDE-TOT-REG   TO WRK-GFCTOP-QTDE-TOT-REG

               MOVE CSERVC-TARIF OF GFCTB0B2
                                       TO GFCTOP-COD-TARIFA(IND-1)
                                          CSERVC-TARIF OF GFCTB0D8

               PERFORM 2900-OBTER-DADOS-GFCTB0D8

HEXA           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8 (1:90)
                                       TO GFCTOP-DESC-TARIFA(IND-1)

           END-IF.

           PERFORM 2860-LER-CSR16.

      *----------------------------------------------------------------*
       2870-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE FECHAMENTO DO CURSOR 16 DA TABELA GFCTB0XX.       *
      ******************************************************************
      *----------------------------------------------------------------*
       2880-FECHAR-CSR16               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR16-GFCTB0B2-JN
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PDIDO_FLEXZ_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0B2'         TO WRK-NOM-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0560'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTOP-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2880-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA OBTER DADOS DA TABELA.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2900-OBTER-DADOS-GFCTB0D8       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                   RSERVC_TARIF_REDZD
             INTO
                   :GFCTB0D8.RSERVC-TARIF-REDZD
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0D8'         TO WRK-NOM-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0570'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOP-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 2999-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE FORMATACAO DE ERROS DB2.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2999-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           MOVE SQLCODE                 TO GFCTOP-COD-SQL-ERRO.
           MOVE SPACES                  TO GFCTOP-DESC-MSG-ERRO.
           MOVE 9                       TO GFCTOP-ERRO.

           PERFORM 1310-OBTER-MENSAGEM.

           STRING GFCTG3-DESC-MSG       ' - ' WRK-NOM-TAB
           DELIMITED BY '  '            INTO GFCTOP-DESC-MSG-ERRO.

           MOVE 'GFCT5147'              TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                   TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                   TO GFCT0M-SQLCA-AREA.
           MOVE SQLCODE                 TO GFCT0M-SQLCODE.

      *----------------------------------------------------------------*
       2999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZA O PROCESSAMENTO E RETORNA AO PROGRAMA CHAMADOR.    *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
           MOVE WRK-GFCTOP-QTDE-TOT-REG TO GFCTOP-QTDE-TOT-REG.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
