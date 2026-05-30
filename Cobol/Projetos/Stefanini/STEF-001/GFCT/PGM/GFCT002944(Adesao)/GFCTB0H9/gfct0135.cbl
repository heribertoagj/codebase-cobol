      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0135.
       AUTHOR.     ANDRE LUIZ LONGO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0135                                    *
      *    PROGRAMADOR.:   ANDRE LUIZ LONGO        - CPM PATO BRANCO   *
      *    ANALISTA CPM:   KHARUZO I. LEITE        - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO CREMM           - PROCWORK /GP.50   *
      *    DATA........:   24/03/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTA ADESAO COMPULSORIA. LISTAR ATE 5   *
      *      OCORRENCIAS NA SAIDA E MONTAR RESTART, CASO HAJA, COM A   *
      *      CHAVE DA PROXIMA OCORRENCIA VALIDA DO CURSOR.             *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.ADSAO_COMP_PCOTE       GFCTB001       *
      *                   DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8       *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *                   DB2PRD.ADSAO_GRP_DEPDC        GFCTB002       *
      *                   DB2PRD.ADSAO_GRP_EMPR         GFCTB003       *
      *                   DB2PRD.ADSAO_GRP_MUN          GFCTB004       *
      *                   DB2PRD.ADSAO_GRP_PAB          GFCTB005       *
      *                   DB2PRD.ADSAO_GRP_PSTAL        GFCTB006       *
      *                   DB2PRD.ADSAO_GRP_SGMTO        GFCTB007       *
      *                   DB2PRD.ADSAO_GRP_UF           GFCTB008       *
      *                   DB2PRD.ADSAO_CPF?             GFCTB0??       *
      *                   DB2PRD.TTPO_SGMTO_GSTAO       GFCTB0H5       *
      *                   DB2PRD.V01UF                  DCITV002       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTLL - ENTRADA                                          *
      *    I#GFCTLM - SAIDA                                            *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA   - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTGB - ENTRADA - ENTRADA DO MODULO GFCT5109 E GFCT5090  *
      *    I#GFCTGC - SAIDA   - SAIDA DO MODULO GFCT5109 E GFCT5090    *
      *    I#GFCT1V - ENTRADA - ENTRADA DO MODULO GFCT5006.            *
      *    I#GFCT1X - SAIDA   - SAIDA DO MODULO GFCT5006.              *
      *    I#RURC88 - SAIDA   - SAIDA DO MODULO RURC9020.              *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5109 - OBTER DESCRICAO DE PAB                           *
      *    GFCT5006 - DESCRICAO DEPENDENCIA                            *
      *    GFCT5090 - DESCRICAO POSTO SERVICO                          *
      *    SENH0315 - OBTER JUNCAO                                     *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *    DCIT8000 - OBTEM UF                                         *
      *    RURC9020 - OBTEM MUNICIPIO                                  *
      *================================================================*
BI0810*                    A L T E R A C A O                           *
BI0810*----------------------------------------------------------------*
BI0810*                                                                *
BI0810*    ANALISTA     : UBIRAJARA MIRANDA(BIRA)  - PROCWORK          *
BI0810*    DATA         : AGO/2010                                     *
BI0810*                                                                *
BI0810*    OBJETIVO     :                                              *
BI0810*    - ENVIAR PARA WEB O CPO. (TIPO DE PESSOA) DA TABELA GFCTB001*
BI0810*                                                                *
BI0810*================================================================*

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
           '* INICIO DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *---------------------------------------------------------------*

BI0810 77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *---------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(011)         VALUE SPACES.
       77  WRK-FIM                     PIC  X(001)         VALUE SPACES.
       77  WRK-FLAG                    PIC  X(001)         VALUE SPACES.

       01  WRK-DATA-INV.
           05  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-AUX.
           05  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-INIC               PIC  X(008)         VALUE SPACES.
       01  WRK-DATA-FIM                PIC  X(008)         VALUE SPACES.
       01  WRK-DPROCM                  PIC  X(008)         VALUE SPACES.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTG2'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM RURC9020 *'.
      *----------------------------------------------------------------*

       COPY 'I#RURC88'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5109 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTGB'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5109 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTGC'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5006 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCT1V'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5006 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCT1X'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM DCIT8000  *'.
      *----------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           05  WRK-AREA-DCLGEN-DCITV002    PIC  X(070)     VALUE SPACES.
           05  WRK-DCIT-CODIGO-RETORNO     PIC  9(002)     VALUE ZEROS.

       01  WRK-ERRO-AREA                   PIC  X(107)     VALUE SPACES.
       01  WRK-SQLCA-DCIT                  PIC  X(136)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5507  *'.
      *---------------------------------------------------------------*

       01  WRK-5507-AREA.
           05  WRK-5507-AREA-ENTRADA.
             10  WRK-5507-AMBIENTE     PIC  X(001)         VALUE SPACES.
             10  WRK-5507-CJUNC-DEPDC  PIC  9(005)         VALUE ZEROS.
           05  WRK-5507-AREA-SAIDA.
             10  WRK-5507-COD-RETORNO  PIC  9(002)         VALUE ZEROS.
             10  WRK-5507-COD-SQL-ERRO PIC  9(003)         VALUE ZEROS.
             10  WRK-5507-MSG-RETORNO  PIC  X(045)         VALUE SPACES.
             10  WRK-5507-IDTFD-MASTER PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM SENH0315  *'.
      *---------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC  X(006)         VALUE SPACES.
       01  WRK-MENSAGEM-0315.
           05  WRK-COD-MSG-0315        PIC  9(003)         VALUE ZEROS.
           05  WRK-HIFEN-MENSAGEM-0315 PIC  X(001)         VALUE SPACES.
           05  WRK-TEXTO-MENSAGEM-0315 PIC  X(079)         VALUE SPACES.

       01  WRK-COD-FUNCIONAL-0315      PIC  X(007)         VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC  X(040)         VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
           05  WRK-CNPJ-PRI-0315       PIC  9(009)         VALUE ZEROS.
           05  WRK-CNPJ-FIL-0315       PIC  9(005)         VALUE ZEROS.
           05  WRK-CNPJ-CON-0315       PIC  9(002)         VALUE ZEROS.

       01  WRK-COD-JUNCAO-0315         PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-SECAO-0315              PIC  9(003)         VALUE ZEROS.
       01  WRK-STATUS-0315             PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS DE NULO *'.
      *---------------------------------------------------------------*

       01  WRK-GFCTB007-NULL.
           05 WRK-CSGMTO-GSTAO-TARIF-NULL    PIC S9(04) COMP VALUE +0.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB001
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB002
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB003
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB004
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB005
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB006
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB007
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB008
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H5
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H9
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA CURSORES *'.
      *---------------------------------------------------------------*

           EXEC SQL
             DECLARE   CSR01-GFCTB005   CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CDEPDC,
                   CPOSTO_SERVC
             FROM  DB2PRD.ADSAO_GRP_PAB
             WHERE
                   CAGPTO_CTA         = :GFCTB005.CAGPTO-CTA         AND
                 ((CDEPDC             = :GFCTB005.CDEPDC             AND
                   CPOSTO_SERVC       = :GFCTB005.CPOSTO-SERVC       AND
                   CSERVC_TARIF       = :GFCTB005.CSERVC-TARIF       AND
                   DINIC_ADSAO_COMP  >= :GFCTB005.DINIC-ADSAO-COMP)  OR
                  (CDEPDC             = :GFCTB005.CDEPDC             AND
                   CPOSTO_SERVC       = :GFCTB005.CPOSTO-SERVC       AND
                   CSERVC_TARIF       > :GFCTB005.CSERVC-TARIF)      OR
                  (CDEPDC             = :GFCTB005.CDEPDC             AND
                   CPOSTO_SERVC       > :GFCTB005.CPOSTO-SERVC)      OR
                  (CDEPDC             > :GFCTB005.CDEPDC))
               ORDER BY
                   CDEPDC,
                   CPOSTO_SERVC,
                   CSERVC_TARIF,
                   DINIC_ADSAO_COMP
           END-EXEC.

           EXEC SQL
             DECLARE  CSR02-GFCTB002   CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CDEPDC
             FROM  DB2PRD.ADSAO_GRP_DEPDC
             WHERE
                   CAGPTO_CTA         = :GFCTB002.CAGPTO-CTA         AND
                 ((CDEPDC             = :GFCTB002.CDEPDC             AND
                   CSERVC_TARIF       = :GFCTB002.CSERVC-TARIF       AND
                   DINIC_ADSAO_COMP  >= :GFCTB002.DINIC-ADSAO-COMP)  OR
                  (CDEPDC             = :GFCTB002.CDEPDC             AND
                   CSERVC_TARIF       > :GFCTB002.CSERVC-TARIF)      OR
                  (CDEPDC             > :GFCTB002.CDEPDC))
               ORDER BY
                   CDEPDC,
                   CSERVC_TARIF,
                   DINIC_ADSAO_COMP
           END-EXEC.

           EXEC SQL
             DECLARE  CSR03-GFCTB003   CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CAG_EMPR,
                   CNRO_CTA_DEB
             FROM  DB2PRD.ADSAO_GRP_EMPR
             WHERE
                   CAGPTO_CTA         = :GFCTB003.CAGPTO-CTA         AND
                 ((CAG_EMPR           = :GFCTB003.CAG-EMPR           AND
                   CNRO_CTA_DEB       = :GFCTB003.CNRO-CTA-DEB       AND
                   CSERVC_TARIF       = :GFCTB003.CSERVC-TARIF       AND
                   DINIC_ADSAO_COMP  >= :GFCTB003.DINIC-ADSAO-COMP)  OR
                  (CAG_EMPR           = :GFCTB003.CAG-EMPR           AND
                   CNRO_CTA_DEB       = :GFCTB003.CNRO-CTA-DEB       AND
                   CSERVC_TARIF       > :GFCTB003.CSERVC-TARIF)      OR
                  (CAG_EMPR           = :GFCTB003.CAG-EMPR           AND
                   CNRO_CTA_DEB       > :GFCTB003.CNRO-CTA-DEB)      OR
                  (CAG_EMPR           > :GFCTB003.CAG-EMPR       ))
               ORDER BY
                   CAG_EMPR,
                   CNRO_CTA_DEB,
                   CSERVC_TARIF,
                   DINIC_ADSAO_COMP
           END-EXEC.

           EXEC SQL
             DECLARE CSR04-GFCTB006   CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CDEPDC,
                   CPOSTO_SERVC
             FROM  DB2PRD.ADSAO_GRP_PSTAL
             WHERE
                   CAGPTO_CTA         = :GFCTB006.CAGPTO-CTA         AND
                 ((CDEPDC             = :GFCTB006.CDEPDC             AND
                   CPOSTO_SERVC       = :GFCTB006.CPOSTO-SERVC       AND
                   CSERVC_TARIF       = :GFCTB006.CSERVC-TARIF       AND
                   DINIC_ADSAO_COMP  >= :GFCTB006.DINIC-ADSAO-COMP)  OR
                  (CDEPDC             = :GFCTB006.CDEPDC             AND
                   CPOSTO_SERVC       = :GFCTB006.CPOSTO-SERVC       AND
                   CSERVC_TARIF       > :GFCTB006.CSERVC-TARIF)      OR
                  (CDEPDC             = :GFCTB006.CDEPDC             AND
                   CPOSTO_SERVC       > :GFCTB006.CPOSTO-SERVC)      OR
                  (CDEPDC             > :GFCTB006.CDEPDC))
               ORDER BY
                   CDEPDC,
                   CPOSTO_SERVC,
                   CSERVC_TARIF,
                   DINIC_ADSAO_COMP
           END-EXEC.

           EXEC SQL
             DECLARE CSR05-GFCTB0H9   CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
ST2506*            CCGC_CPF,
ST2506*            CFLIAL_CGC,
ST2506*            CCTRL_CPF_CGC
ST2506             CCGC_CPF_ST,
ST2506             CFLIAL_CGC_ST,
ST2506             CCTRL_CPF_CGC_ST
             FROM  DB2PRD.TADSAO_GRP_CLI
             WHERE
                   CAGPTO_CTA         = :GFCTB0H9.CAGPTO-CTA         AND
ST2509*          ((CCGC_CPF           = :GFCTB0H9.CCGC-CPF           AND
ST2509           ((CCGC_CPF_ST        = :GFCTB0H9.CCGC-CPF-ST        AND
                   CSERVC_TARIF       = :GFCTB0H9.CSERVC-TARIF       AND
                   DINIC_ADSAO_COMP  >= :GFCTB0H9.DINIC-ADSAO-COMP)  OR
ST2509*           (CCGC_CPF           = :GFCTB0H9.CCGC-CPF           AND
ST2509            (CCGC_CPF_ST        = :GFCTB0H9.CCGC-CPF-ST        AND
                   CSERVC_TARIF       > :GFCTB0H9.CSERVC-TARIF)      OR
ST2509*           (CCGC_CPF           > :GFCTB0H9.CCGC-CPF))
ST2509            (CCGC_CPF_ST        > :GFCTB0H9.CCGC-CPF-ST))
               ORDER BY
ST2509*            CCGC_CPF,
ST2509             CCGC_CPF_ST,
                   CSERVC_TARIF,
                   DINIC_ADSAO_COMP
           END-EXEC.

           EXEC SQL
             DECLARE CSR06-GFCTB007   CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CSGMTO_GSTAO_TARIF
             FROM  DB2PRD.ADSAO_GRP_SGMTO
             WHERE
                   CAGPTO_CTA         = :GFCTB007.CAGPTO-CTA         AND
                 ((CSGMTO_GSTAO_TARIF = :GFCTB007.CSGMTO-GSTAO-TARIF AND
                   CSERVC_TARIF       = :GFCTB007.CSERVC-TARIF       AND
                   DINIC_ADSAO_COMP  >= :GFCTB007.DINIC-ADSAO-COMP)  OR
                  (CSGMTO_GSTAO_TARIF = :GFCTB007.CSGMTO-GSTAO-TARIF AND
                   CSERVC_TARIF       > :GFCTB007.CSERVC-TARIF)      OR
                  (CSGMTO_GSTAO_TARIF > :GFCTB007.CSGMTO-GSTAO-TARIF))
               ORDER BY
                   CSGMTO_GSTAO_TARIF,
                   CSERVC_TARIF,
                   DINIC_ADSAO_COMP
           END-EXEC.

           EXEC SQL
             DECLARE  CSR07-GFCTB004   CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CMUN_IBGE
             FROM  DB2PRD.ADSAO_GRP_MUN
             WHERE
                   CAGPTO_CTA         = :GFCTB004.CAGPTO-CTA         AND
                 ((CMUN_IBGE          = :GFCTB004.CMUN-IBGE          AND
                   CSERVC_TARIF       = :GFCTB004.CSERVC-TARIF       AND
                   DINIC_ADSAO_COMP  >= :GFCTB004.DINIC-ADSAO-COMP)  OR
                  (CMUN_IBGE          = :GFCTB004.CMUN-IBGE          AND
                   CSERVC_TARIF       > :GFCTB004.CSERVC-TARIF)      OR
                  (CMUN_IBGE          > :GFCTB004.CMUN-IBGE))
               ORDER BY
                   CMUN_IBGE,
                   CSERVC_TARIF,
                   DINIC_ADSAO_COMP
           END-EXEC.

           EXEC SQL
             DECLARE CSR08-GFCTB008   CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CSGL_UF
             FROM  DB2PRD.ADSAO_GRP_UF
             WHERE
                   CAGPTO_CTA         = :GFCTB008.CAGPTO-CTA         AND
                 ((CSGL_UF            = :GFCTB008.CSGL-UF            AND
                   CSERVC_TARIF       = :GFCTB008.CSERVC-TARIF       AND
                   DINIC_ADSAO_COMP  >= :GFCTB008.DINIC-ADSAO-COMP)  OR
                  (CSGL_UF            = :GFCTB008.CSGL-UF            AND
                   CSERVC_TARIF       > :GFCTB008.CSERVC-TARIF)      OR
                  (CSGL_UF            > :GFCTB008.CSGL-UF))
               ORDER BY
                   CSGL_UF,
                   CSERVC_TARIF,
                   DINIC_ADSAO_COMP
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTLL'.

       COPY 'I#GFCTLM'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTLL-ENTRADA
                                                      GFCTLM-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *INICIA O PROGRAMA                                               *
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
      *MONTA AREA DE SAIDA, VERIFICA DISPONIBILIDADE DO SISTEMA E CON- *
      *SISTE DADOS.                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTLM-SAIDA
                                          WRK-FLAG.
           MOVE ZEROS                  TO IND-1.

           INITIALIZE GFCTLM-SAIDA
                      GFCT0M-AREA-ERROS.

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

           MOVE 'GFCT0135'             TO GFCTFZ-TRANSACAO.
           MOVE GFCTLL-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTLL-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTLM-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0135'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                 EQUAL 9
               MOVE 9                      TO GFCTLM-ERRO
               MOVE 'GFCT0135'             TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO    TO GFCT0M-SQLCODE
                                              GFCTLM-COD-SQL-ERRO
               MOVE GFCTG1-ERRO            TO GFCTLM-ERRO
               MOVE GFCTG1-COD-MSG-ERRO    TO GFCTLM-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO   TO GFCT0M-TEXTO
                                              GFCTLM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE          EQUAL 1
               MOVE 9                      TO GFCTLM-ERRO
               MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0135'             TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTA AREA DE SAIDA                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE 180                        TO GFCTLM-LL.
           MOVE ZEROS                      TO GFCTLM-ZZ.
           MOVE GFCTLL-TRANSACAO           TO GFCTLM-TRANSACAO.
           MOVE GFCTLL-FUNCAO              TO GFCTLM-FUNCAO.

           IF  GFCTLL-QTDE-OCOR            NOT NUMERIC
               MOVE ZEROS                  TO GFCTLM-QTDE-OCOR
           ELSE
               MOVE GFCTLL-QTDE-OCOR       TO GFCTLM-QTDE-OCOR
           END-IF.

           IF  GFCTLL-QTDE-TOT-REG         NOT NUMERIC
               MOVE ZEROS                  TO GFCTLM-QTDE-TOT-REG
           ELSE
               MOVE GFCTLL-QTDE-TOT-REG    TO GFCTLM-QTDE-TOT-REG
           END-IF.

           MOVE GFCTLL-FUNC-BDSCO          TO GFCTLM-FUNC-BDSCO.
           MOVE GFCTLL-PONTEIRO            TO GFCTLM-PONTEIRO.
           MOVE GFCTLL-IND-DATA            TO GFCTLM-IND-DATA.

           MOVE ZEROS                      TO GFCTLM-ERRO
                                              GFCTLM-COD-SQL-ERRO
                                              GFCTLM-COD-MSG-ERRO
                                              GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG            TO GFCTLM-DESC-MSG-ERRO.

           MOVE 'N'                        TO GFCTLM-FIM.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTE DADOS                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTLL-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTLL-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTLL-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTLL-QTDE-OCOR        NOT NUMERIC)                OR
              (GFCTLL-QTDE-TOT-REG     NOT NUMERIC)                OR
              (GFCTLL-IND-DATA         NOT EQUAL 'P' AND 'T')      OR
              (GFCTLL-FIM              NOT EQUAL 'S' AND 'N')      OR
              (GFCTLL-PONT-PACOTE      NOT NUMERIC)                OR
              (GFCTLL-PONT-CH-NUM5     NOT NUMERIC)                OR
              (GFCTLL-PONT-CH-NUM13    NOT NUMERIC)                OR
              (GFCTLL-PONT-AGPTO       NOT EQUAL 004 AND 009 AND
                                                 014 AND 015 AND
                                                 016 AND 017 AND
                                                 018 AND 019 AND
                                                 020)
               MOVE 'S'                TO GFCTLM-FIM
               MOVE  1                 TO GFCTLM-ERRO
               MOVE  ZEROS             TO GFCTLM-COD-SQL-ERRO
               MOVE  0001              TO GFCTLM-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTLM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 1320-SELECIONAR-GFCTB0A1.

           PERFORM 1330-OBTER-JUNCAO.

           PERFORM 1340-OBTER-MASTER.

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
           MOVE GFCTLL-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTLL-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTLL-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTLM-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0135'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTLM-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0135'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                 EQUAL 1
               MOVE 'S'                    TO GFCTLM-FIM
               MOVE 1                      TO GFCTLM-ERRO
               MOVE GFCTG3-COD-SQL-ERRO    TO GFCTLM-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO    TO GFCTLM-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO   TO GFCTLM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR GFCTB0A1                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1320-SELECIONAR-GFCTB0A1        SECTION.
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
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0A1'      TO WRK-NOM-TAB
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-AUX.
           MOVE CORR WRK-DATA-AUX      TO WRK-DATA-INV.
           MOVE WRK-DATA-INV           TO WRK-DPROCM.

      *----------------------------------------------------------------*
       1320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATA ERROS DB2                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1321-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           MOVE  9                     TO GFCTLM-ERRO.
           MOVE 'GFCT0135'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
                                          GFCTLM-COD-SQL-ERRO.

           STRING GFCTG3-DESC-MSG WRK-NOM-TAB
           DELIMITED BY '  '           INTO GFCTLM-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM JUNCAO                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1330-OBTER-JUNCAO               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTLL-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.
           MOVE 'VRS001'               TO WRK-VERSAO-0315.

      * TODO - VERIFICAR MODULO PARA SUBSTITUICAO
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
               MOVE 'S'                TO GFCTLM-FIM
               MOVE 1                  TO GFCTLM-ERRO
               MOVE 0056               TO GFCTLM-COD-MSG-ERRO
               MOVE '0056'             TO GFCTG2-COD-MSG
               MOVE RETURN-CODE        TO GFCTLM-COD-SQL-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG      TO GFCTLM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               IF  WRK-COD-MSG-0315    EQUAL 001
                   MOVE 'S'            TO GFCTLM-FIM
                   MOVE 1              TO GFCTLM-ERRO
                   MOVE 0057           TO GFCTLM-COD-MSG-ERRO
                   MOVE '0057'         TO GFCTG2-COD-MSG
                   MOVE RETURN-CODE    TO GFCTLM-COD-SQL-ERRO
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG      TO GFCTLM-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
               IF  WRK-COD-MSG-0315    EQUAL 154
                   MOVE 'S'            TO GFCTLM-FIM
                   MOVE 1              TO GFCTLM-ERRO
                   MOVE 0058           TO GFCTLM-COD-MSG-ERRO
                   MOVE '0058'         TO GFCTG2-COD-MSG
                   MOVE RETURN-CODE    TO GFCTLM-COD-SQL-ERRO
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG      TO GFCTLM-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
               IF  WRK-COD-MSG-0315    EQUAL 242
                   MOVE 'S'            TO GFCTLM-FIM
                   MOVE 1              TO GFCTLM-ERRO
                   MOVE 0059           TO GFCTLM-COD-MSG-ERRO
                   MOVE '0059'         TO GFCTG2-COD-MSG
                   MOVE RETURN-CODE    TO GFCTLM-COD-SQL-ERRO
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG      TO GFCTLM-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
               IF  WRK-COD-MSG-0315    EQUAL 009
                   MOVE 'S'            TO GFCTLM-FIM
                   MOVE 1              TO GFCTLM-ERRO
                                          GFCTLM-COD-MSG-ERRO
                   MOVE '0001'         TO GFCTG2-COD-MSG
                   MOVE RETURN-CODE    TO GFCTLM-COD-SQL-ERRO
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG      TO GFCTLM-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
               IF  WRK-COD-MSG-0315    EQUAL 138
                   MOVE 'S'            TO GFCTLM-FIM
                   MOVE 1              TO GFCTLM-ERRO
                   MOVE 0060           TO GFCTLM-COD-MSG-ERRO
                   MOVE '0060'         TO GFCTG2-COD-MSG
                   MOVE RETURN-CODE    TO GFCTLM-COD-SQL-ERRO
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG      TO GFCTLM-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM MASTER                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1340-OBTER-MASTER               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5507-AREA-SAIDA.

           MOVE WRK-COD-JUNCAO-0315    TO WRK-5507-CJUNC-DEPDC.
           MOVE 'O'                    TO WRK-5507-AMBIENTE.
           MOVE 'GFCT5507'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5507-AREA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTLM-ERRO
               MOVE 'GFCT5093'         TO GFCT0M-PROGRAMA
               MOVE 'GFCT5507'         TO WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-5507-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5507-COD-RETORNO EQUAL 01
                   MOVE 1              TO GFCTLM-ERRO
                   MOVE ZEROS          TO GFCTLM-COD-SQL-ERRO
                                          GFCTLM-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                        TO GFCTLM-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5507-COD-RETORNO
                                       EQUAL 99
                       MOVE 1          TO GFCTLM-ERRO
                       MOVE WRK-5507-COD-SQL-ERRO
                                       TO GFCTLM-COD-SQL-ERRO
                       MOVE ZEROS      TO GFCTLM-COD-MSG-ERRO
                       MOVE WRK-5507-MSG-RETORNO
                                       TO GFCTLM-DESC-MSG-ERRO
                   END-IF
               END-IF
               MOVE 'S'                TO GFCTLM-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTLL-PONT-AGPTO
               WHEN 15
                   PERFORM 2100-CONSULTAR-PAB
               WHEN 14
                   PERFORM 2200-CONSULTAR-AGENCIA
               WHEN 16
                   PERFORM 2300-CONSULTAR-EMPRESA
               WHEN 17
                   PERFORM 2400-CONSULTAR-POSTAL-EXP
               WHEN 20
                   PERFORM 2400-CONSULTAR-POSTAL-EXP
               WHEN 4
                   PERFORM 2500-CONSULTAR-CLIENTE
               WHEN 9
                   PERFORM 2600-CONSULTAR-SEGMENTO
               WHEN 18
                   PERFORM 2700-CONSULTAR-MUNICIPIO
               WHEN 19
                   PERFORM 2800-CONSULTAR-UF
           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSULTAR PAB                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-CONSULTAR-PAB              SECTION.
      *----------------------------------------------------------------*

           PERFORM 2110-ABRIR-CURSOR-GFCTB005.

           PERFORM 2120-LER-CURSOR-GFCTB005.

           PERFORM 2130-PROCESSAR-CURSOR-GFCTB005 UNTIL
               WRK-FIM                 EQUAL 'S'.

           PERFORM 2140-FECHAR-CURSOR-GFCTB005.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR GFCTB005                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-ABRIR-CURSOR-GFCTB005      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.
           MOVE 'N'                    TO WRK-FIM.
           MOVE GFCTLL-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB005.
           MOVE GFCTLL-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB005.
           MOVE GFCTLL-PONT-CH-NUM5    TO CDEPDC            OF GFCTB005.
           MOVE GFCTLL-PONT-CH-NUM13   TO CPOSTO-SERVC      OF GFCTB005.

           IF  GFCTLL-PONT-CH-DATA     EQUAL SPACES
               MOVE '01.01.0001'       TO DINIC-ADSAO-COMP  OF GFCTB005
           ELSE
               MOVE GFCTLL-PONT-CH-DATA TO DINIC-ADSAO-COMP OF GFCTB005
           END-IF.

           EXEC SQL
               OPEN CSR01-GFCTB005
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB005'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_PAB'    TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR GFCTB005                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-LER-CURSOR-GFCTB005        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH  CSR01-GFCTB005   INTO
                   :GFCTB005.CSERVC-TARIF,
                   :GFCTB005.CAGPTO-CTA,
                   :GFCTB005.CSEQ-AGPTO-CTA,
                   :GFCTB005.DINIC-ADSAO-COMP,
                   :GFCTB005.CDEPDC,
                   :GFCTB005.CPOSTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB005'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_PAB'    TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM
                                          GFCTLM-FIM
               PERFORM 2121-VE-CSR-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICA SE CURSOR VAZIO                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2121-VE-CSR-VAZIO               SECTION.
      *----------------------------------------------------------------*

           IF  IND-1                   EQUAL 1
               MOVE 1                  TO GFCTLM-ERRO
               MOVE 0798               TO GFCTLM-COD-MSG-ERRO
               MOVE '0798'             TO GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTLM-COD-SQL-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG-ERRO   TO GFCTLM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAR CURSOR GFCTB005                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2130-PROCESSAR-CURSOR-GFCTB005  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FLAG                EQUAL SPACES
               MOVE CSERVC-TARIF       OF GFCTB005
                                       TO CSERVC-TARIF     OF GFCTB001
               MOVE CAGPTO-CTA         OF GFCTB005
                                       TO CAGPTO-CTA       OF GFCTB001
               MOVE DINIC-ADSAO-COMP   OF GFCTB005
                                       TO DINIC-ADSAO-COMP OF GFCTB001
               MOVE CSEQ-AGPTO-CTA     OF GFCTB005
                                       TO CSEQ-AGPTO-CTA   OF GFCTB001
               PERFORM 2131-ACESSAR-GFCTB001
               PERFORM 2132-VER-DESPREZA
           END-IF.

           PERFORM 2120-LER-CURSOR-GFCTB005.

           IF  WRK-FIM                     EQUAL 'N'
               IF  IND-1                   GREATER 5
                   MOVE CSERVC-TARIF       OF GFCTB005
                                           TO CSERVC-TARIF   OF GFCTB001
                   MOVE CAGPTO-CTA         OF GFCTB005
                                           TO CAGPTO-CTA     OF GFCTB001
                   MOVE DINIC-ADSAO-COMP   OF GFCTB005
                                         TO DINIC-ADSAO-COMP OF GFCTB001
                   MOVE CSEQ-AGPTO-CTA     OF GFCTB005
                                           TO CSEQ-AGPTO-CTA OF GFCTB001
                   PERFORM 2131-ACESSAR-GFCTB001
                   IF  GFCTLL-IND-DATA     EQUAL 'P'
                       IF  WRK-5507-IDTFD-MASTER EQUAL 'S' OR
                           CDEPDC          OF GFCTB0D8
                                           EQUAL WRK-COD-JUNCAO-0315
                           IF (WRK-DATA-INIC LESS  WRK-DPROCM AND
                               WRK-DATA-FIM  LESS  WRK-DPROCM)
                               MOVE 'X'    TO WRK-FLAG
                               GO TO 2130-99-FIM
                           END-IF
                       ELSE
                           MOVE 'X'        TO WRK-FLAG
                           GO TO 2130-99-FIM
                       END-IF
                   END-IF
                   MOVE 'S'                TO WRK-FIM
                   MOVE CSERVC-TARIF       OF GFCTB005
                                           TO GFCTLM-PONT-PACOTE
                   MOVE CAGPTO-CTA         OF GFCTB005
                                           TO GFCTLM-PONT-AGPTO
                   MOVE CDEPDC             OF GFCTB005
                                           TO GFCTLM-PONT-CH-NUM5
                   MOVE CPOSTO-SERVC       OF GFCTB005
                                           TO GFCTLM-PONT-CH-NUM13
                   MOVE DINIC-ADSAO-COMP   OF GFCTB005
                                           TO GFCTLM-PONT-CH-DATA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR GFCTB001                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2131-ACESSAR-GFCTB001           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                  A.CSERVC_TARIF,
                  A.CAGPTO_CTA,
                  A.CSEQ_AGPTO_CTA,
                  A.DINIC_ADSAO_COMP,
                  A.DFIM_ADSAO_COMP,
                  A.CFUNC_MANUT_INCL,
                  A.CFUNC_MANUT_EXCL,
                  A.CINDCD_AGPTO_TOT,
                  A.HMANUT_REG,
BI0810            A.CPSSOA_SERVC_TARIF,
                  B.CDEPDC,
                  B.RSERVC_TARIF_REDZD
             INTO
                   :GFCTB001.CSERVC-TARIF,
                   :GFCTB001.CAGPTO-CTA,
                   :GFCTB001.CSEQ-AGPTO-CTA,
                   :GFCTB001.DINIC-ADSAO-COMP,
                   :GFCTB001.DFIM-ADSAO-COMP,
                   :GFCTB001.CFUNC-MANUT-INCL,
                   :GFCTB001.CFUNC-MANUT-EXCL,
                   :GFCTB001.CINDCD-AGPTO-TOT,
                   :GFCTB001.HMANUT-REG,
BI0810             :GFCTB001.CPSSOA-SERVC-TARIF,
                   :GFCTB0D8.CDEPDC,
                   :GFCTB0D8.RSERVC-TARIF-REDZD
             FROM   DB2PRD.ADSAO_COMP_PCOTE  A,
                    DB2PRD.SERVC_TARIF_PRINC B
             WHERE
                 A.CSERVC_TARIF       = :GFCTB001.CSERVC-TARIF       AND
                 A.CAGPTO_CTA         = :GFCTB001.CAGPTO-CTA         AND
                 A.CSEQ_AGPTO_CTA     = :GFCTB001.CSEQ-AGPTO-CTA     AND
                 A.DINIC_ADSAO_COMP   = :GFCTB001.DINIC-ADSAO-COMP   AND
                 A.CSERVC_TARIF       = B.CSERVC_TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB001'      TO WRK-NOM-TAB
               MOVE 'ADSAO_COMP_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DINIC-ADSAO-COMP       OF GFCTB001
                                       TO WRK-DATA-AUX.
           MOVE CORR WRK-DATA-AUX      TO WRK-DATA-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-INIC.
           MOVE DFIM-ADSAO-COMP        OF GFCTB001
                                       TO WRK-DATA-AUX.
           MOVE CORR WRK-DATA-AUX      TO WRK-DATA-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-FIM.

      *----------------------------------------------------------------*
       2131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR SE REGISTRO VAI SER DESPREZADO OU NAO                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2132-VER-DESPREZA               SECTION.
      *----------------------------------------------------------------*

           IF  GFCTLL-IND-DATA             EQUAL 'P'
               IF  WRK-5507-IDTFD-MASTER   EQUAL 'S' OR
                   CDEPDC                  OF GFCTB0D8
                                           EQUAL WRK-COD-JUNCAO-0315
                   IF (WRK-DATA-INIC       LESS WRK-DPROCM AND
                       WRK-DATA-FIM        LESS WRK-DPROCM)
                       GO TO 2132-99-FIM
                   END-IF
               ELSE
                   GO TO 2132-99-FIM
               END-IF
           END-IF.

           MOVE CSEQ-AGPTO-CTA         OF GFCTB005
                                       TO GFCTLM-SEQUENCIA(IND-1).
           MOVE CSERVC-TARIF           OF GFCTB005
                                       TO GFCTLM-PACOTE(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTLM-DESC-PACOTE(IND-1).
           MOVE CDEPDC                 OF GFCTB005
                                       TO GFCTLM-AGENCIA(IND-1).
           MOVE CPOSTO-SERVC           OF GFCTB005
                                       TO GFCTLM-POSTO(IND-1).

           IF  CINDCD-AGPTO-TOT        OF GFCTB001
                                       EQUAL 'S'
               MOVE 'TODOS'            TO GFCTLM-DESC-NOME(IND-1)
           ELSE
              PERFORM 21321-OBTER-DESC-PAB

              MOVE GFCTGC-DESC-POSTO(1) TO GFCTLM-DESC-NOME(IND-1)
           END-IF.

           MOVE DINIC-ADSAO-COMP       OF GFCTB005
                                       TO GFCTLM-DATA-INICIO(IND-1).
           MOVE DFIM-ADSAO-COMP        OF GFCTB001
                                       TO GFCTLM-DATA-FIM(IND-1).
BI0810     MOVE CPSSOA-SERVC-TARIF     OF GFCTB001
BI0810                                 TO GFCTLM-CPSSOA-TARIFA(IND-1).

           IF  HMANUT-REG OF GFCTB001
                                 EQUAL '0001-01-01-01.01.01.000001'
              MOVE 'N'                 TO GFCTLM-CANCELADO(IND-1)
           ELSE
              MOVE 'S'                 TO GFCTLM-CANCELADO(IND-1)
           END-IF.

           ADD 166                     TO GFCTLM-LL.
           ADD 1                       TO GFCTLM-QTDE-TOT-REG
                                          IND-1.

      *----------------------------------------------------------------*
       2132-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTER DESCRICAO PAB                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       21321-OBTER-DESC-PAB            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.
           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.
           MOVE GFCTLL-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTLL-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE GFCTLL-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.
           MOVE CDEPDC                 OF GFCTB005
                                       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC           OF GFCTB005
                                       TO GFCTGB-COD-POSTO.

           INITIALIZE GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 'GFCT5109'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTLM-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0135'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTLM-FIM
               MOVE GFCTGC-ERRO        TO GFCTLM-ERRO
               MOVE GFCTGC-COD-SQL-ERRO TO GFCTLM-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO TO GFCTLM-COD-MSG-ERRO
               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTLM-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109 - '
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTLM-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       21321-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR GFCTB005                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2140-FECHAR-CURSOR-GFCTB005     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE  CSR01-GFCTB005
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB005'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_PAB'    TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSULTAR AGENCIA                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-CONSULTAR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2210-ABRIR-CURSOR-GFCTB002.

           PERFORM 2220-LER-CURSOR-GFCTB002.

           PERFORM 2230-PROCESSAR-CURSOR-GFCTB002 UNTIL
               WRK-FIM                 EQUAL 'S'.

           PERFORM 2240-FECHAR-CURSOR-GFCTB002.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR GFCTB002                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-ABRIR-CURSOR-GFCTB002      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.
           MOVE 'N'                    TO WRK-FIM.
           MOVE GFCTLL-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB002.
           MOVE GFCTLL-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB002.
           MOVE GFCTLL-PONT-CH-NUM5    TO CDEPDC            OF GFCTB002.

           IF  GFCTLL-PONT-CH-DATA     EQUAL SPACES
               MOVE '01.01.0001'       TO DINIC-ADSAO-COMP  OF GFCTB002
           ELSE
               MOVE GFCTLL-PONT-CH-DATA TO DINIC-ADSAO-COMP OF GFCTB002
           END-IF.

           EXEC SQL
               OPEN CSR02-GFCTB002
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB002'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_DEPDC'  TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR GFCTB002                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2220-LER-CURSOR-GFCTB002        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH  CSR02-GFCTB002   INTO
                   :GFCTB002.CSERVC-TARIF,
                   :GFCTB002.CAGPTO-CTA,
                   :GFCTB002.CSEQ-AGPTO-CTA,
                   :GFCTB002.DINIC-ADSAO-COMP,
                   :GFCTB002.CDEPDC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB002'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_DEPDC'  TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM
                                          GFCTLM-FIM
               PERFORM 2121-VE-CSR-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAR CURSOR GFCTB002                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2230-PROCESSAR-CURSOR-GFCTB002  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FLAG                EQUAL SPACES
               MOVE CSERVC-TARIF       OF GFCTB002
                                       TO CSERVC-TARIF     OF GFCTB001
               MOVE CAGPTO-CTA         OF GFCTB002
                                       TO CAGPTO-CTA       OF GFCTB001
               MOVE DINIC-ADSAO-COMP   OF GFCTB002
                                       TO DINIC-ADSAO-COMP OF GFCTB001
               MOVE CSEQ-AGPTO-CTA     OF GFCTB002
                                       TO CSEQ-AGPTO-CTA   OF GFCTB001
               PERFORM 2131-ACESSAR-GFCTB001
               PERFORM 2231-VER-DESPREZA
           END-IF.

           PERFORM 2220-LER-CURSOR-GFCTB002.

           IF  WRK-FIM                     EQUAL 'N'
               IF  IND-1                   GREATER 5
                   MOVE CSERVC-TARIF       OF GFCTB002
                                           TO CSERVC-TARIF   OF GFCTB001
                   MOVE CAGPTO-CTA         OF GFCTB002
                                           TO CAGPTO-CTA     OF GFCTB001
                   MOVE DINIC-ADSAO-COMP   OF GFCTB002
                                         TO DINIC-ADSAO-COMP OF GFCTB001
                   MOVE CSEQ-AGPTO-CTA     OF GFCTB002
                                           TO CSEQ-AGPTO-CTA OF GFCTB001
                   PERFORM 2131-ACESSAR-GFCTB001
                   IF  GFCTLL-IND-DATA     EQUAL 'P'
                       IF  WRK-5507-IDTFD-MASTER EQUAL 'S' OR
                           CDEPDC          OF GFCTB0D8
                                           EQUAL WRK-COD-JUNCAO-0315
                           IF (WRK-DATA-INIC LESS WRK-DPROCM AND
                               WRK-DATA-FIM  LESS WRK-DPROCM)
                               MOVE 'X'    TO WRK-FLAG
                               GO TO 2230-99-FIM
                           END-IF
                       ELSE
                           MOVE 'X'    TO WRK-FLAG
                           GO TO 2230-99-FIM
                       END-IF
                   END-IF
                   MOVE 'S'                TO WRK-FIM
                   MOVE CSERVC-TARIF       OF GFCTB002
                                           TO GFCTLM-PONT-PACOTE
                   MOVE CAGPTO-CTA         OF GFCTB002
                                           TO GFCTLM-PONT-AGPTO
                   MOVE CDEPDC             OF GFCTB002
                                           TO GFCTLM-PONT-CH-NUM5
                   MOVE DINIC-ADSAO-COMP   OF GFCTB002
                                           TO GFCTLM-PONT-CH-DATA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR SE O REGISTRO VAI SER DESPREZADO OU NAO               *
      ******************************************************************
      *----------------------------------------------------------------*
       2231-VER-DESPREZA               SECTION.
      *----------------------------------------------------------------*

           IF  GFCTLL-IND-DATA             EQUAL 'P'
               IF  WRK-5507-IDTFD-MASTER   EQUAL 'S' OR
                   CDEPDC                  OF GFCTB0D8
                                           EQUAL WRK-COD-JUNCAO-0315
                   IF (WRK-DATA-INIC       LESS WRK-DPROCM AND
                       WRK-DATA-FIM        LESS WRK-DPROCM)
                       GO TO 2231-99-FIM
                   END-IF
               ELSE
                   GO TO 2231-99-FIM
               END-IF
           END-IF.

           MOVE CSEQ-AGPTO-CTA         OF GFCTB002
                                       TO GFCTLM-SEQUENCIA(IND-1).
           MOVE CSERVC-TARIF           OF GFCTB002
                                       TO GFCTLM-PACOTE(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTLM-DESC-PACOTE(IND-1).
           MOVE CDEPDC                 OF GFCTB002
                                       TO GFCTLM-AGENCIA(IND-1).

           IF  CINDCD-AGPTO-TOT        OF GFCTB001
                                       EQUAL 'S'
               MOVE 'TODAS'            TO GFCTLM-DESC-NOME(IND-1)
           ELSE
               PERFORM 22311-OBTER-DESC-DEPDC

BI0810         MOVE GFCT1X-DESC-DEPDC(1)(1:40)
BI0810                               TO GFCTLM-DESC-NOME(IND-1)
           END-IF.

           MOVE DINIC-ADSAO-COMP       OF GFCTB002
                                       TO GFCTLM-DATA-INICIO(IND-1).
           MOVE DFIM-ADSAO-COMP        OF GFCTB001
                                       TO GFCTLM-DATA-FIM(IND-1).
BI0810     MOVE CPSSOA-SERVC-TARIF     OF GFCTB001
BI0810                                 TO GFCTLM-CPSSOA-TARIFA(IND-1).

           IF  HMANUT-REG OF GFCTB001
                                 EQUAL '0001-01-01-01.01.01.000001'
              MOVE 'N'                 TO GFCTLM-CANCELADO(IND-1)
           ELSE
              MOVE 'S'                 TO GFCTLM-CANCELADO(IND-1)
           END-IF.

           ADD 166                     TO GFCTLM-LL.
           ADD 1                       TO GFCTLM-QTDE-TOT-REG
                                          IND-1.

      *----------------------------------------------------------------*
       2231-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTER DESCRICAO DEPDC                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       22311-OBTER-DESC-DEPDC          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1V-ENTRADA.
           MOVE SPACES                 TO GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCT1X-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 660                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.
           MOVE GFCTLL-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTLL-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTLL-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.
           MOVE CDEPDC OF GFCTB002     TO GFCT1V-COD-DEPDC(1).
           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL 'POOL0081'              USING WRK-MODULO
                                              GFCT1V-ENTRADA
                                              GFCT1X-SAIDA
                                              GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTLM-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0135'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCT1X-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTLM-FIM
               MOVE GFCT1X-ERRO        TO GFCTLM-ERRO
               MOVE GFCT1X-COD-SQL-ERRO TO GFCTLM-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO TO GFCTLM-COD-MSG-ERRO
               IF  GFCT1X-ERRO         EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO
                                       TO GFCTLM-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006 - '
                          GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTLM-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       22311-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR GFCTB002                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2240-FECHAR-CURSOR-GFCTB002     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE  CSR02-GFCTB002
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB002'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_DEPDC'  TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'             TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSULTAR EMPRESA                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-CONSULTAR-EMPRESA          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2310-ABRIR-CURSOR-GFCTB003.

           PERFORM 2320-LER-CURSOR-GFCTB003.

           PERFORM 2330-PROCESSAR-CURSOR-GFCTB003 UNTIL
               WRK-FIM                 EQUAL 'S'.

           PERFORM 2340-FECHAR-CURSOR-GFCTB003.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR GFCTB003                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-ABRIR-CURSOR-GFCTB003      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.
           MOVE 'N'                    TO WRK-FIM.
           MOVE GFCTLL-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB003.
           MOVE GFCTLL-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB003.
           MOVE GFCTLL-PONT-CH-NUM5    TO CAG-EMPR          OF GFCTB003.
           MOVE GFCTLL-PONT-CH-NUM13   TO CNRO-CTA-DEB      OF GFCTB003.

           IF  GFCTLL-PONT-CH-DATA     EQUAL SPACES
               MOVE '01.01.0001'       TO DINIC-ADSAO-COMP  OF GFCTB003
           ELSE
               MOVE GFCTLL-PONT-CH-DATA TO DINIC-ADSAO-COMP OF GFCTB003
           END-IF.

           EXEC SQL
               OPEN CSR03-GFCTB003
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB003'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_EMPR'   TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR GFCTB003                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-LER-CURSOR-GFCTB003        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH  CSR03-GFCTB003   INTO
                   :GFCTB003.CSERVC-TARIF,
                   :GFCTB003.CAGPTO-CTA,
                   :GFCTB003.CSEQ-AGPTO-CTA,
                   :GFCTB003.DINIC-ADSAO-COMP,
                   :GFCTB003.CAG-EMPR,
                   :GFCTB003.CNRO-CTA-DEB
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB003'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_EMPR'   TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM
                                          GFCTLM-FIM
               PERFORM 2121-VE-CSR-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAR CURSOR GFCTB003                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2330-PROCESSAR-CURSOR-GFCTB003  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FLAG                EQUAL SPACES
               MOVE CSERVC-TARIF       OF GFCTB003
                                       TO CSERVC-TARIF     OF GFCTB001
               MOVE CAGPTO-CTA         OF GFCTB003
                                       TO CAGPTO-CTA       OF GFCTB001
               MOVE DINIC-ADSAO-COMP   OF GFCTB003
                                       TO DINIC-ADSAO-COMP OF GFCTB001
               MOVE CSEQ-AGPTO-CTA     OF GFCTB003
                                       TO CSEQ-AGPTO-CTA   OF GFCTB001
               PERFORM 2131-ACESSAR-GFCTB001
               PERFORM 2331-VER-DESPREZA
           END-IF.

           PERFORM 2320-LER-CURSOR-GFCTB003.

           IF  WRK-FIM                     EQUAL 'N'
               IF  IND-1                   GREATER 5
                   MOVE CSERVC-TARIF       OF GFCTB003
                                           TO CSERVC-TARIF   OF GFCTB001
                   MOVE CAGPTO-CTA         OF GFCTB003
                                           TO CAGPTO-CTA     OF GFCTB001
                   MOVE DINIC-ADSAO-COMP   OF GFCTB003
                                         TO DINIC-ADSAO-COMP OF GFCTB001
                   MOVE CSEQ-AGPTO-CTA     OF GFCTB003
                                           TO CSEQ-AGPTO-CTA OF GFCTB001
                   PERFORM 2131-ACESSAR-GFCTB001
                   IF  GFCTLL-IND-DATA     EQUAL 'P'
                       IF  WRK-5507-IDTFD-MASTER EQUAL 'S' OR
                           CDEPDC          OF GFCTB0D8
                                           EQUAL WRK-COD-JUNCAO-0315
                           IF (WRK-DATA-INIC LESS WRK-DPROCM AND
                               WRK-DATA-FIM  LESS WRK-DPROCM)
                               MOVE 'X'    TO WRK-FLAG
                               GO TO 2330-99-FIM
                           END-IF
                       ELSE
                          MOVE 'X'         TO WRK-FLAG
                          GO TO 2330-99-FIM
                       END-IF
                   END-IF
                   MOVE 'S'                TO WRK-FIM
                   MOVE CSERVC-TARIF       OF GFCTB003
                                           TO GFCTLM-PONT-PACOTE
                   MOVE CAGPTO-CTA         OF GFCTB003
                                           TO GFCTLM-PONT-AGPTO
                   MOVE CAG-EMPR           OF GFCTB003
                                           TO GFCTLM-PONT-CH-NUM5
                   MOVE CNRO-CTA-DEB       OF GFCTB003
                                           TO GFCTLM-PONT-CH-NUM13
                   MOVE DINIC-ADSAO-COMP   OF GFCTB003
                                           TO GFCTLM-PONT-CH-DATA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR SE O REGISTRO VAI SER DESPREZADO OU NAO               *
      ******************************************************************
      *----------------------------------------------------------------*
       2331-VER-DESPREZA               SECTION.
      *----------------------------------------------------------------*

           IF  GFCTLL-IND-DATA             EQUAL 'P'
               IF  WRK-5507-IDTFD-MASTER   EQUAL 'S' OR
                   CDEPDC                  OF GFCTB0D8
                                           EQUAL WRK-COD-JUNCAO-0315
                   IF (WRK-DATA-INIC       LESS WRK-DPROCM AND
                       WRK-DATA-FIM        LESS WRK-DPROCM)
                       GO TO 2331-99-FIM
                   END-IF
               ELSE
                   GO TO 2331-99-FIM
               END-IF
           END-IF.

           MOVE CSEQ-AGPTO-CTA         OF GFCTB003
                                       TO GFCTLM-SEQUENCIA(IND-1).
           MOVE CSERVC-TARIF           OF GFCTB003
                                       TO GFCTLM-PACOTE(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTLM-DESC-PACOTE(IND-1).
           MOVE CAG-EMPR               OF GFCTB003
                                       TO GFCTLM-AGENCIA(IND-1).
           MOVE CNRO-CTA-DEB           OF GFCTB003
                                       TO GFCTLM-CONTA(IND-1).
           MOVE DINIC-ADSAO-COMP       OF GFCTB003
                                       TO GFCTLM-DATA-INICIO(IND-1).
           MOVE DFIM-ADSAO-COMP        OF GFCTB001
                                       TO GFCTLM-DATA-FIM(IND-1).
BI0810     MOVE CPSSOA-SERVC-TARIF     OF GFCTB001
BI0810                                 TO GFCTLM-CPSSOA-TARIFA(IND-1).

           IF  HMANUT-REG              OF GFCTB001
                                      EQUAL '0001-01-01-01.01.01.000001'
               MOVE 'N'                TO GFCTLM-CANCELADO(IND-1)
           ELSE
               MOVE 'S'                TO GFCTLM-CANCELADO(IND-1)
           END-IF.

           ADD 166                     TO GFCTLM-LL.
           ADD 1                       TO GFCTLM-QTDE-TOT-REG
                                          IND-1.

      *----------------------------------------------------------------*
       2331-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR GFCTB003                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2340-FECHAR-CURSOR-GFCTB003     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE  CSR03-GFCTB003
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB003'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_EMPR'   TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSULTAR POSTAL                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-CONSULTAR-POSTAL-EXP       SECTION.
      *----------------------------------------------------------------*

           PERFORM 2410-ABRIR-CURSOR-GFCTB006.

           PERFORM 2420-LER-CURSOR-GFCTB006.

           PERFORM 2430-PROCESSAR-CURSOR-GFCTB006 UNTIL
               WRK-FIM                 EQUAL 'S'.

           PERFORM 2440-FECHAR-CURSOR-GFCTB006.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR GFCTB006                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-ABRIR-CURSOR-GFCTB006      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.
           MOVE 'N'                    TO WRK-FIM.
           MOVE GFCTLL-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB006.
           MOVE GFCTLL-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB006.
           MOVE GFCTLL-PONT-CH-NUM5    TO CDEPDC            OF GFCTB006.
           MOVE GFCTLL-PONT-CH-NUM13   TO CPOSTO-SERVC      OF GFCTB006.

           IF  GFCTLL-PONT-CH-DATA     EQUAL SPACES
               MOVE '01.01.0001'       TO DINIC-ADSAO-COMP  OF GFCTB006
           ELSE
               MOVE GFCTLL-PONT-CH-DATA TO DINIC-ADSAO-COMP OF GFCTB006
           END-IF.

           EXEC SQL
               OPEN  CSR04-GFCTB006
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB006'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_PSTAL'  TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR GFCTB006                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2420-LER-CURSOR-GFCTB006        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH   CSR04-GFCTB006   INTO
                   :GFCTB006.CSERVC-TARIF,
                   :GFCTB006.CAGPTO-CTA,
                   :GFCTB006.CSEQ-AGPTO-CTA,
                   :GFCTB006.DINIC-ADSAO-COMP,
                   :GFCTB006.CDEPDC,
                   :GFCTB006.CPOSTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB006'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_PSTAL'  TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM
                                          GFCTLM-FIM
               PERFORM 2121-VE-CSR-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAR CURSOR GCTB006                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2430-PROCESSAR-CURSOR-GFCTB006  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FLAG                EQUAL SPACES
               MOVE CSERVC-TARIF       OF GFCTB006
                                       TO CSERVC-TARIF     OF GFCTB001
               MOVE CAGPTO-CTA         OF GFCTB006
                                       TO CAGPTO-CTA       OF GFCTB001
               MOVE DINIC-ADSAO-COMP   OF GFCTB006
                                       TO DINIC-ADSAO-COMP OF GFCTB001
               MOVE CSEQ-AGPTO-CTA     OF GFCTB006
                                       TO CSEQ-AGPTO-CTA   OF GFCTB001
               PERFORM 2131-ACESSAR-GFCTB001
               PERFORM 2431-VER-DESPREZA
           END-IF.

           PERFORM 2420-LER-CURSOR-GFCTB006.

           IF  WRK-FIM                     EQUAL 'N'
               IF  IND-1                   GREATER 5
                   MOVE CSERVC-TARIF       OF GFCTB006
                                           TO CSERVC-TARIF   OF GFCTB001
                   MOVE CAGPTO-CTA         OF GFCTB006
                                           TO CAGPTO-CTA     OF GFCTB001
                   MOVE DINIC-ADSAO-COMP   OF GFCTB006
                                         TO DINIC-ADSAO-COMP OF GFCTB001
                   MOVE CSEQ-AGPTO-CTA     OF GFCTB006
                                           TO CSEQ-AGPTO-CTA OF GFCTB001
                   PERFORM 2131-ACESSAR-GFCTB001
                   IF  GFCTLL-IND-DATA     EQUAL 'P'
                       IF  WRK-5507-IDTFD-MASTER EQUAL 'S' OR
                           CDEPDC          OF GFCTB0D8
                                           EQUAL WRK-COD-JUNCAO-0315
                           IF (WRK-DATA-INIC LESS WRK-DPROCM AND
                               WRK-DATA-FIM  LESS WRK-DPROCM)
                               MOVE 'X'    TO WRK-FLAG
                               GO TO 2430-99-FIM
                           END-IF
                       ELSE
                           MOVE 'X'    TO WRK-FLAG
                           GO TO 2430-99-FIM
                       END-IF
                   END-IF
                   MOVE 'S'                TO WRK-FIM
                   MOVE CSERVC-TARIF       OF GFCTB006
                                           TO GFCTLM-PONT-PACOTE
                   MOVE CAGPTO-CTA         OF GFCTB006
                                           TO GFCTLM-PONT-AGPTO
                   MOVE CDEPDC             OF GFCTB006
                                           TO GFCTLM-PONT-CH-NUM5
                   MOVE CPOSTO-SERVC       OF GFCTB006
                                           TO GFCTLM-PONT-CH-NUM13
                   MOVE DINIC-ADSAO-COMP   OF GFCTB006
                                           TO GFCTLM-PONT-CH-DATA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR SE O REGISTRO VAI SER DESPREZADO OU NAO               *
      ******************************************************************
      *----------------------------------------------------------------*
       2431-VER-DESPREZA               SECTION.
      *----------------------------------------------------------------*

           IF  GFCTLL-IND-DATA             EQUAL 'P'
               IF  WRK-5507-IDTFD-MASTER   EQUAL 'S' OR
                   CDEPDC                  OF GFCTB0D8
                                           EQUAL WRK-COD-JUNCAO-0315
                   IF (WRK-DATA-INIC       LESS WRK-DPROCM AND
                       WRK-DATA-FIM        LESS WRK-DPROCM)
                       GO TO 2431-99-FIM
                   END-IF
               ELSE
                   GO TO 2431-99-FIM
               END-IF
           END-IF.

           MOVE CSEQ-AGPTO-CTA         OF GFCTB006
                                       TO GFCTLM-SEQUENCIA(IND-1).
           MOVE CSERVC-TARIF           OF GFCTB006
                                       TO GFCTLM-PACOTE(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTLM-DESC-PACOTE(IND-1).
           MOVE CDEPDC                 OF GFCTB006
                                       TO GFCTLM-AGENCIA(IND-1).
           MOVE CPOSTO-SERVC           OF GFCTB006
                                       TO GFCTLM-POSTO(IND-1).

           IF  CINDCD-AGPTO-TOT        OF GFCTB001
                                       EQUAL 'S'
               MOVE 'TODOS'            TO GFCTLM-DESC-NOME(IND-1)
           ELSE
               PERFORM 24311-OBTER-DESC-POSTO

               MOVE GFCTGC-DESC-POSTO(1) TO GFCTLM-DESC-NOME(IND-1)
           END-IF.

           MOVE DINIC-ADSAO-COMP       OF GFCTB006
                                       TO GFCTLM-DATA-INICIO(IND-1).
           MOVE DFIM-ADSAO-COMP        OF GFCTB001
                                       TO GFCTLM-DATA-FIM(IND-1).
BI0810     MOVE CPSSOA-SERVC-TARIF     OF GFCTB001
BI0810                                 TO GFCTLM-CPSSOA-TARIFA(IND-1).

           IF  HMANUT-REG              OF GFCTB001
                                      EQUAL '0001-01-01-01.01.01.000001'
               MOVE 'N'                TO GFCTLM-CANCELADO(IND-1)
           ELSE
               MOVE 'S'                TO GFCTLM-CANCELADO(IND-1)
           END-IF.

           ADD 166                     TO GFCTLM-LL.
           ADD 1                       TO GFCTLM-QTDE-TOT-REG
                                          IND-1.

      *----------------------------------------------------------------*
       2431-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTER DESCRICAO POSTO                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       24311-OBTER-DESC-POSTO          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.
           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.
           MOVE GFCTLL-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTLL-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE GFCTLL-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.
           MOVE CDEPDC    OF GFCTB006  TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC           OF GFCTB006
                                       TO GFCTGB-COD-POSTO.
           MOVE SPACES                 TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 'GFCT5090'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTLM-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0135'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTLM-FIM
               MOVE GFCTGC-ERRO        TO GFCTLM-ERRO
               MOVE GFCTGC-COD-SQL-ERRO TO GFCTLM-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO TO GFCTLM-COD-MSG-ERRO
               IF  GFCTGC-ERRO          EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                        TO GFCTLM-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5090 - '
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTLM-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       24311-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ***************************************************************
      *FECHAR CURSOR GFCTB006                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2440-FECHAR-CURSOR-GFCTB006     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE  CSR04-GFCTB006
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB006'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_PSTAL'  TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'             TO GFCT0M-COMANDO-SQL
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSULTAR CLIENTE                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-CONSULTAR-CLIENTE          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2510-ABRIR-CURSOR-GFCTB0H9.

           PERFORM 2520-LER-CURSOR-GFCTB0H9.

           PERFORM 2530-PROCESSAR-CURSOR-GFCTB0H9 UNTIL
               WRK-FIM                 EQUAL 'S'.

           PERFORM 2540-FECHAR-CURSOR-GFCTB0H9.

      *----------------------------------------------------------------*
      *2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR GFCTB0H9                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2510-ABRIR-CURSOR-GFCTB0H9      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                       TO IND-1.
           MOVE 'N'                     TO WRK-FIM.
           MOVE GFCTLL-PONT-PACOTE      TO CSERVC-TARIF     OF GFCTB0H9.
           MOVE GFCTLL-PONT-AGPTO       TO CAGPTO-CTA       OF GFCTB0H9.
    
      * TODO - MOVIMENTAÇÃO DE CAMPO NUMERICO MAIOR QUE O RECEPTOR 
ST2506*    MOVE GFCTLL-PONT-CH-NUM13    TO CCGC-CPF         OF GFCTB0H9.
ST2506     MOVE GFCTLL-PONT-CH-CCGC-CPF TO CCGC-CPF-ST      OF GFCTB0H9.

           IF  GFCTLL-PONT-CH-DATA      EQUAL SPACES
               MOVE '01.01.0001'        TO DINIC-ADSAO-COMP OF GFCTB0H9
           ELSE
               MOVE GFCTLL-PONT-CH-DATA TO DINIC-ADSAO-COMP OF GFCTB0H9
           END-IF.

           EXEC SQL
             OPEN  CSR05-GFCTB0H9
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0H9'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_CLI'    TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0150'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR GFCTB0H9                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2520-LER-CURSOR-GFCTB0H9        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH   CSR05-GFCTB0H9   INTO
                   :GFCTB0H9.CSERVC-TARIF,
                   :GFCTB0H9.CAGPTO-CTA,
                   :GFCTB0H9.CSEQ-AGPTO-CTA,
                   :GFCTB0H9.DINIC-ADSAO-COMP,
ST2509*            :GFCTB0H9.CCGC-CPF,
ST2509*            :GFCTB0H9.CFLIAL-CGC,
ST2506*            :GFCTB0H9.CCTRL-CPF-CGC
ST2509             :GFCTB0H9.CCGC-CPF-ST,
ST2509             :GFCTB0H9.CFLIAL-CGC-ST,
ST2506             :GFCTB0H9.CCTRL-CPF-CGC-ST
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0H9'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_CLI'    TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0160'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 'S'                 TO WRK-FIM
                                           GFCTLM-FIM
               PERFORM 2121-VE-CSR-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAR CURSOR GFCTB0H9                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2530-PROCESSAR-CURSOR-GFCTB0H9  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FLAG                EQUAL SPACES
               MOVE CSERVC-TARIF       OF GFCTB0H9
                                       TO CSERVC-TARIF     OF GFCTB001
               MOVE CAGPTO-CTA         OF GFCTB0H9
                                       TO CAGPTO-CTA       OF GFCTB001
               MOVE DINIC-ADSAO-COMP   OF GFCTB0H9
                                       TO DINIC-ADSAO-COMP OF GFCTB001
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0H9
                                       TO CSEQ-AGPTO-CTA   OF GFCTB001
               PERFORM 2131-ACESSAR-GFCTB001
               PERFORM 2531-VER-DESPREZA
           END-IF.

           PERFORM 2520-LER-CURSOR-GFCTB0H9.

           IF  WRK-FIM                 EQUAL 'N'
               IF  IND-1               GREATER 5
                   MOVE CSERVC-TARIF   OF GFCTB0H9
                                       TO CSERVC-TARIF     OF GFCTB001
                   MOVE CAGPTO-CTA     OF GFCTB0H9
                                       TO CAGPTO-CTA       OF GFCTB001
                   MOVE DINIC-ADSAO-COMP OF GFCTB0H9
                                       TO DINIC-ADSAO-COMP OF GFCTB001
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0H9
                                       TO CSEQ-AGPTO-CTA   OF GFCTB001
                   PERFORM 2131-ACESSAR-GFCTB001
                   IF  GFCTLL-IND-DATA  EQUAL  'P'
                       IF  WRK-5507-IDTFD-MASTER EQUAL 'S' OR
                           CDEPDC OF GFCTB0D8 EQUAL WRK-COD-JUNCAO-0315
                           IF (WRK-DATA-INIC LESS WRK-DPROCM AND
                               WRK-DATA-FIM  LESS WRK-DPROCM)
                               MOVE 'X' TO WRK-FLAG
                               GO TO 2530-99-FIM
                           END-IF
                       ELSE
                           MOVE 'X'    TO WRK-FLAG
                           GO TO 2530-99-FIM
                       END-IF
                   END-IF
                   MOVE 'S'             TO WRK-FIM
                   MOVE CSERVC-TARIF    OF GFCTB0H9
                                        TO GFCTLM-PONT-PACOTE
                   MOVE CAGPTO-CTA      OF GFCTB0H9
                                        TO GFCTLM-PONT-AGPTO
ST2509*            M0VE CCGC-CPF        OF GFCTB0H9
ST2509*                                 TO GFCTLM-PONT-CH-NUM13
ST2509             MOVE CCGC-CPF-ST     OF GFCTB0H9
ST2509                                  TO GFCTLM-PONT-CH-CCGC-CPF 
                   MOVE DINIC-ADSAO-COMP OF GFCTB0H9
                                        TO GFCTLM-PONT-CH-DATA
                END-IF
           END-IF.

      *----------------------------------------------------------------*
       2530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR SE O REGISTRO SERA DESPREZADO OU NAO                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2531-VER-DESPREZA               SECTION.
      *----------------------------------------------------------------*

           IF  GFCTLL-IND-DATA         EQUAL  'P'
               IF  WRK-5507-IDTFD-MASTER EQUAL 'S' OR
                   CDEPDC OF GFCTB0D8  EQUAL WRK-COD-JUNCAO-0315
                   IF  (WRK-DATA-INIC  LESS WRK-DPROCM AND
                        WRK-DATA-FIM   LESS WRK-DPROCM)
                        GO TO 2531-99-FIM
                   END-IF
               ELSE
                   GO TO 2531-99-FIM
               END-IF
           END-IF.

           MOVE CSEQ-AGPTO-CTA         OF GFCTB0H9
                                       TO GFCTLM-SEQUENCIA(IND-1).
           MOVE CSERVC-TARIF           OF GFCTB0H9
                                       TO GFCTLM-PACOTE(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTLM-DESC-PACOTE(IND-1).
ST2506*    MOVE CCGC-CPF               OF GFCTB0H9
ST2506     MOVE CCGC-CPF-ST            OF GFCTB0H9
                                       TO GFCTLM-CPF-CNPJ(IND-1).
ST2506*    MOVE CFLIAL-CGC             OF GFCTB0H9
ST2506     MOVE CFLIAL-CGC-ST          OF GFCTB0H9
                                       TO GFCTLM-FILIAL(IND-1).
ST2506*    MOVE CCTRL-CPF-CGC          OF GFCTB0H9
ST2506     MOVE CCTRL-CPF-CGC-ST       OF GFCTB0H9
                                       TO GFCTLM-CONTROLE(IND-1).
           MOVE DINIC-ADSAO-COMP       OF GFCTB0H9
                                       TO GFCTLM-DATA-INICIO(IND-1).
           MOVE DFIM-ADSAO-COMP        OF GFCTB001
                                       TO GFCTLM-DATA-FIM(IND-1).
BI0810     MOVE CPSSOA-SERVC-TARIF     OF GFCTB001
BI0810                                 TO GFCTLM-CPSSOA-TARIFA(IND-1).

           IF  HMANUT-REG OF GFCTB001
                                 EQUAL '0001-01-01-01.01.01.000001'
              MOVE 'N'                 TO GFCTLM-CANCELADO(IND-1)
           ELSE
              MOVE 'S'                 TO GFCTLM-CANCELADO(IND-1)
           END-IF.

           ADD 166                     TO GFCTLM-LL.
           ADD 1                       TO GFCTLM-QTDE-TOT-REG
                                          IND-1.

      *----------------------------------------------------------------*
       2531-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR GFCTB0H9                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2540-FECHAR-CURSOR-GFCTB0H9     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR05-GFCTB0H9
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL    ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0H9'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_CLI'    TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0170'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSULTAR SEGMENTO                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-CONSULTAR-SEGMENTO         SECTION.
      *----------------------------------------------------------------*

           PERFORM 2610-ABRIR-CURSOR-GFCTB007.

           PERFORM 2620-LER-CURSOR-GFCTB007.

           PERFORM 2630-PROCESSAR-CURSOR-GFCTB007 UNTIL
               WRK-FIM                 EQUAL 'S'.

           PERFORM 2640-FECHAR-CURSOR-GFCTB007.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR GFCTB007                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2610-ABRIR-CURSOR-GFCTB007      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.
           MOVE 'N'                    TO WRK-FIM.
           MOVE GFCTLL-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB007.
           MOVE GFCTLL-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB007.
           MOVE GFCTLL-PONT-CH-NUM5   TO CSGMTO-GSTAO-TARIF OF GFCTB007.

           IF  GFCTLL-PONT-CH-DATA     EQUAL SPACES
               MOVE '01.01.0001'       TO DINIC-ADSAO-COMP  OF GFCTB007
           ELSE
               MOVE GFCTLL-PONT-CH-DATA TO DINIC-ADSAO-COMP OF GFCTB007
           END-IF.

           EXEC SQL
               OPEN  CSR06-GFCTB007
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB007'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_SGMTO'  TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0180'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR GFCTB007                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2620-LER-CURSOR-GFCTB007        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH  CSR06-GFCTB007   INTO
                   :GFCTB007.CSERVC-TARIF,
                   :GFCTB007.CAGPTO-CTA,
                   :GFCTB007.CSEQ-AGPTO-CTA,
                   :GFCTB007.DINIC-ADSAO-COMP,
                   :GFCTB007.CSGMTO-GSTAO-TARIF
                     :WRK-CSGMTO-GSTAO-TARIF-NULL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB007'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_SGMTO'  TO GFCT0M-NOME-TAB
               MOVE 'FETCH'             TO GFCT0M-COMANDO-SQL
               MOVE '0190'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM
                                          GFCTLM-FIM
               PERFORM 2121-VE-CSR-VAZIO
           ELSE
               IF  WRK-CSGMTO-GSTAO-TARIF-NULL LESS ZEROS
                   MOVE ZEROS          TO CSGMTO-GSTAO-TARIF
                                       OF GFCTB007
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAR CURSOR GFCTB007                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2630-PROCESSAR-CURSOR-GFCTB007  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FLAG                EQUAL SPACES
               MOVE CSERVC-TARIF       OF GFCTB007
                                       TO CSERVC-TARIF     OF GFCTB001
               MOVE CAGPTO-CTA         OF GFCTB007
                                       TO CAGPTO-CTA       OF GFCTB001
               MOVE DINIC-ADSAO-COMP   OF GFCTB007
                                       TO DINIC-ADSAO-COMP OF GFCTB001
               MOVE CSEQ-AGPTO-CTA     OF GFCTB007
                                       TO CSEQ-AGPTO-CTA   OF GFCTB001
               PERFORM 2131-ACESSAR-GFCTB001
               PERFORM 2631-VER-DESPREZA
           END-IF.

           PERFORM 2620-LER-CURSOR-GFCTB007.

           IF  WRK-FIM                     EQUAL 'N'
               IF  IND-1                   GREATER 5
                   MOVE CSERVC-TARIF       OF GFCTB007
                                           TO CSERVC-TARIF   OF GFCTB001
                   MOVE CAGPTO-CTA         OF GFCTB007
                                           TO CAGPTO-CTA     OF GFCTB001
                   MOVE DINIC-ADSAO-COMP   OF GFCTB007
                                         TO DINIC-ADSAO-COMP OF GFCTB001
                   MOVE CSEQ-AGPTO-CTA     OF GFCTB007
                                           TO CSEQ-AGPTO-CTA OF GFCTB001
                   PERFORM 2131-ACESSAR-GFCTB001
                   IF  GFCTLL-IND-DATA     EQUAL 'P'
                       IF  WRK-5507-IDTFD-MASTER EQUAL 'S' OR
                           CDEPDC          OF GFCTB0D8
                                           EQUAL WRK-COD-JUNCAO-0315
                           IF (WRK-DATA-INIC LESS WRK-DPROCM AND
                               WRK-DATA-FIM  LESS WRK-DPROCM)
                               MOVE 'X'    TO WRK-FLAG
                               GO TO 2630-99-FIM
                           END-IF
                       ELSE
                           MOVE 'X'        TO WRK-FLAG
                           GO TO 2630-99-FIM
                       END-IF
                   END-IF
                   MOVE 'S'                TO WRK-FIM
                   MOVE CSERVC-TARIF       OF GFCTB007
                                           TO GFCTLM-PONT-PACOTE
                   MOVE CAGPTO-CTA         OF GFCTB007
                                           TO GFCTLM-PONT-AGPTO
                   MOVE CSGMTO-GSTAO-TARIF OF GFCTB007
                                           TO GFCTLM-PONT-CH-NUM5
                   MOVE DINIC-ADSAO-COMP   OF GFCTB007
                                           TO GFCTLM-PONT-CH-DATA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2630-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR SE O REGISTRO SERA DESPREZADO OU NAO                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2631-VER-DESPREZA               SECTION.
      *----------------------------------------------------------------*

           IF  GFCTLL-IND-DATA             EQUAL 'P'
               IF  WRK-5507-IDTFD-MASTER   EQUAL 'S' OR
                   CDEPDC                  OF GFCTB0D8
                                           EQUAL WRK-COD-JUNCAO-0315
                   IF (WRK-DATA-INIC       LESS WRK-DPROCM AND
                       WRK-DATA-FIM        LESS WRK-DPROCM)
                       GO TO 2631-99-FIM
                   END-IF
               ELSE
                   GO TO 2631-99-FIM
               END-IF
           END-IF.

           MOVE CSEQ-AGPTO-CTA         OF GFCTB007
                                       TO GFCTLM-SEQUENCIA(IND-1).
           MOVE CSERVC-TARIF           OF GFCTB007
                                       TO GFCTLM-PACOTE(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTLM-DESC-PACOTE(IND-1).
           MOVE CSGMTO-GSTAO-TARIF     OF GFCTB007
                                       TO GFCTLM-SEGMENTO(IND-1).

           PERFORM 26311-OBTER-DESC-SEGMENTO.

           IF  HMANUT-REG OF GFCTB001
                                 EQUAL '0001-01-01-01.01.01.000001'
              MOVE 'N'                 TO GFCTLM-CANCELADO(IND-1)
           ELSE
              MOVE 'S'                 TO GFCTLM-CANCELADO(IND-1)
           END-IF.

           MOVE ISGMTO-GSTAO-TARIF     OF GFCTB0H5
                                       TO GFCTLM-DESC-NOME(IND-1).

           MOVE DINIC-ADSAO-COMP       OF GFCTB007
                                       TO GFCTLM-DATA-INICIO(IND-1).
           MOVE DFIM-ADSAO-COMP        OF GFCTB001
                                       TO GFCTLM-DATA-FIM(IND-1).
BI0810     MOVE CPSSOA-SERVC-TARIF     OF GFCTB001
BI0810                                 TO GFCTLM-CPSSOA-TARIFA(IND-1).

           ADD 166                     TO GFCTLM-LL.
           ADD 1                       TO GFCTLM-QTDE-TOT-REG
                                          IND-1.

      *----------------------------------------------------------------*
       2631-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTER DESCRICAO SEGMENTO                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       26311-OBTER-DESC-SEGMENTO       SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-GSTAO-TARIF    OF GFCTB007
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

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0H5'      TO WRK-NOM-TAB
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0200'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       26311-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR GFCTB007                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2640-FECHAR-CURSOR-GFCTB007     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE  CSR06-GFCTB007
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB007'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_SGMTO'  TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0210'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2640-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSULTAR MUNICIPIO                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2700-CONSULTAR-MUNICIPIO        SECTION.
      *----------------------------------------------------------------*

           PERFORM 2710-ABRIR-CURSOR-GFCTB004.

           PERFORM 2720-LER-CURSOR-GFCTB004.

           PERFORM 2730-PROCESSAR-CURSOR-GFCTB004 UNTIL
               WRK-FIM                 EQUAL 'S'.

           PERFORM 2740-FECHAR-CURSOR-GFCTB004.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR GFCTB004                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2710-ABRIR-CURSOR-GFCTB004      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.
           MOVE 'N'                    TO WRK-FIM.
           MOVE GFCTLL-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB004.
           MOVE GFCTLL-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB004.
           MOVE GFCTLL-PONT-CH-NUM5    TO CMUN-IBGE         OF GFCTB004.

           IF  GFCTLL-PONT-CH-DATA     EQUAL SPACES
               MOVE '01.01.0001'       TO DINIC-ADSAO-COMP  OF GFCTB004
           ELSE
               MOVE GFCTLL-PONT-CH-DATA TO DINIC-ADSAO-COMP OF GFCTB004
           END-IF.

           EXEC SQL
              OPEN CSR07-GFCTB004
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB004'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_MUN'    TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0220'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR GFCTB004                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2720-LER-CURSOR-GFCTB004        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR07-GFCTB004   INTO
                   :GFCTB004.CSERVC-TARIF,
                   :GFCTB004.CAGPTO-CTA,
                   :GFCTB004.CSEQ-AGPTO-CTA,
                   :GFCTB004.DINIC-ADSAO-COMP,
                   :GFCTB004.CMUN-IBGE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB004'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_MUN'    TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0230'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM
                                          GFCTLM-FIM
               PERFORM 2121-VE-CSR-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       2720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAR CURSOR GFCTB004                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2730-PROCESSAR-CURSOR-GFCTB004  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FLAG                EQUAL SPACES
               MOVE CSERVC-TARIF       OF GFCTB004
                                       TO CSERVC-TARIF     OF GFCTB001
               MOVE CAGPTO-CTA         OF GFCTB004
                                       TO CAGPTO-CTA       OF GFCTB001
               MOVE DINIC-ADSAO-COMP   OF GFCTB004
                                       TO DINIC-ADSAO-COMP OF GFCTB001
               MOVE CSEQ-AGPTO-CTA     OF GFCTB004
                                       TO CSEQ-AGPTO-CTA   OF GFCTB001
               PERFORM 2131-ACESSAR-GFCTB001
               PERFORM 2731-VER-DESPREZA
           END-IF.

           PERFORM 2720-LER-CURSOR-GFCTB004.

           IF  WRK-FIM                     EQUAL 'N'
               IF  IND-1                   GREATER 5
                   MOVE CSERVC-TARIF       OF GFCTB004
                                           TO CSERVC-TARIF   OF GFCTB001
                   MOVE CAGPTO-CTA         OF GFCTB004
                                           TO CAGPTO-CTA     OF GFCTB001
                   MOVE DINIC-ADSAO-COMP   OF GFCTB004
                                         TO DINIC-ADSAO-COMP OF GFCTB001
                   MOVE CSEQ-AGPTO-CTA     OF GFCTB004
                                           TO CSEQ-AGPTO-CTA OF GFCTB001
                   PERFORM 2131-ACESSAR-GFCTB001
                   IF  GFCTLL-IND-DATA     EQUAL 'P'
                       IF  WRK-5507-IDTFD-MASTER EQUAL 'S' OR
                           CDEPDC          OF GFCTB0D8
                                           EQUAL WRK-COD-JUNCAO-0315
                           IF (WRK-DATA-INIC LESS WRK-DPROCM AND
                               WRK-DATA-FIM  LESS WRK-DPROCM)
                               MOVE 'X'    TO WRK-FLAG
                               GO TO 2730-99-FIM
                           END-IF
                       ELSE
                           MOVE 'X'        TO WRK-FLAG
                           GO TO 2730-99-FIM
                       END-IF
                   END-IF
                   MOVE 'S'                TO WRK-FIM
                   MOVE CSERVC-TARIF       OF GFCTB004
                                           TO GFCTLM-PONT-PACOTE
                   MOVE CAGPTO-CTA         OF GFCTB004
                                           TO GFCTLM-PONT-AGPTO
                   MOVE CMUN-IBGE          OF GFCTB004
                                           TO GFCTLM-PONT-CH-NUM5
                   MOVE DINIC-ADSAO-COMP   OF GFCTB004
                                           TO GFCTLM-PONT-CH-DATA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2730-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR SE O REGISTRO SERA DESPREZADO OU NAO                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2731-VER-DESPREZA               SECTION.
      *----------------------------------------------------------------*

           IF  GFCTLL-IND-DATA             EQUAL 'P'
               IF  WRK-5507-IDTFD-MASTER   EQUAL 'S' OR
                   CDEPDC                  OF GFCTB0D8
                                           EQUAL WRK-COD-JUNCAO-0315
                   IF (WRK-DATA-INIC       LESS WRK-DPROCM AND
                       WRK-DATA-FIM        LESS WRK-DPROCM)
                       GO TO 2731-99-FIM
                   END-IF
               ELSE
                   GO TO 2731-99-FIM
               END-IF
           END-IF.

           MOVE CSEQ-AGPTO-CTA         OF GFCTB004
                                       TO GFCTLM-SEQUENCIA(IND-1).
           MOVE CSERVC-TARIF           OF GFCTB004
                                       TO GFCTLM-PACOTE(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTLM-DESC-PACOTE(IND-1).
           MOVE CMUN-IBGE              OF GFCTB004
                                       TO GFCTLM-MUNICIPIO(IND-1).

           PERFORM 27311-OBTER-DESC-MUNICIPIO.
           IF  HMANUT-REG OF GFCTB001
                                 EQUAL '0001-01-01-01.01.01.000001'
              MOVE 'N'                 TO GFCTLM-CANCELADO(IND-1)
           ELSE
              MOVE 'S'                 TO GFCTLM-CANCELADO(IND-1)
           END-IF.

           MOVE NOME-MUNIC-RURC88      TO GFCTLM-DESC-NOME(IND-1).

           MOVE DINIC-ADSAO-COMP       OF GFCTB004
                                       TO GFCTLM-DATA-INICIO(IND-1).
           MOVE DFIM-ADSAO-COMP        OF GFCTB001
                                       TO GFCTLM-DATA-FIM(IND-1).
BI0810     MOVE CPSSOA-SERVC-TARIF     OF GFCTB001
BI0810                                 TO GFCTLM-CPSSOA-TARIFA(IND-1).

           ADD 166                     TO GFCTLM-LL.
           ADD 1                       TO GFCTLM-QTDE-TOT-REG
                                          IND-1.

      *----------------------------------------------------------------*
       2731-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTER DESCRICAO MUNICIPIO                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       27311-OBTER-DESC-MUNICIPIO      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE AREA-RURC88.

           MOVE CMUN-IBGE              OF GFCTB004
                                       TO COD-MUNIC-RURC88.
           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL 'POOL0081'              USING WRK-MODULO
                                              AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTLM-ERRO
               MOVE ZEROS              TO GFCTLM-COD-SQL-ERRO
               MOVE 732                TO GFCTLM-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG-ERRO  TO GFCTLM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 10
               MOVE 1                  TO GFCTLM-ERRO
               MOVE ZEROS              TO GFCTLM-COD-SQL-ERRO
               MOVE 675                TO GFCTLM-COD-MSG-ERRO
               MOVE '0675'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG-ERRO  TO GFCTLM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL  20
               MOVE 1                  TO GFCTLM-ERRO
               MOVE ZEROS              TO GFCTLM-COD-SQL-ERRO
               MOVE 733                TO GFCTLM-COD-MSG-ERRO
               MOVE '0733'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG-ERRO  TO GFCTLM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 99
               MOVE 1                  TO GFCTLM-ERRO
               MOVE SQLCODE-RURC88     TO GFCTLM-COD-SQL-ERRO
               MOVE 734                TO GFCTLM-COD-MSG-ERRO
               MOVE '0734'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG-ERRO  TO GFCTLM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       27311-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR GFCTB004                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2740-FECHAR-CURSOR-GFCTB004     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR07-GFCTB004
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB004'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_MUN'    TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'             TO GFCT0M-COMANDO-SQL
               MOVE '0240'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2740-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSULTAR UF                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2800-CONSULTAR-UF               SECTION.
      *----------------------------------------------------------------*

           PERFORM 2810-ABRIR-CURSOR-GFCTB008.

           PERFORM 2820-LER-CURSOR-GFCTB008.

           PERFORM 2830-PROCESSAR-CURSOR-GFCTB008 UNTIL
               WRK-FIM                 EQUAL 'S'.

           PERFORM 2840-FECHAR-CURSOR-GFCTB008.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR GFCTB008                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2810-ABRIR-CURSOR-GFCTB008      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.
           MOVE 'N'                    TO WRK-FIM.
           MOVE GFCTLL-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB008.
           MOVE GFCTLL-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB008.
           MOVE GFCTLL-PONT-CH-ALP     TO CSGL-UF           OF GFCTB008.

           IF  GFCTLL-PONT-CH-DATA     EQUAL SPACES
               MOVE '01.01.0001'       TO DINIC-ADSAO-COMP  OF GFCTB008
           ELSE
               MOVE GFCTLL-PONT-CH-DATA TO DINIC-ADSAO-COMP OF GFCTB008
           END-IF.

           EXEC SQL
              OPEN CSR08-GFCTB008
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB008'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_UF'     TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0250'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR GFCTB008                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2820-LER-CURSOR-GFCTB008        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH  CSR08-GFCTB008   INTO
                   :GFCTB008.CSERVC-TARIF,
                   :GFCTB008.CAGPTO-CTA,
                   :GFCTB008.CSEQ-AGPTO-CTA,
                   :GFCTB008.DINIC-ADSAO-COMP,
                   :GFCTB008.CSGL-UF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB008'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_UF'     TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0260'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM
                                          GFCTLM-FIM
               PERFORM 2121-VE-CSR-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       2820-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAR CURSOR GFCTB008                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2830-PROCESSAR-CURSOR-GFCTB008  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FLAG                EQUAL SPACES
               MOVE CSERVC-TARIF       OF GFCTB008
                                       TO CSERVC-TARIF     OF GFCTB001
               MOVE CAGPTO-CTA         OF GFCTB008
                                       TO CAGPTO-CTA       OF GFCTB001
               MOVE DINIC-ADSAO-COMP   OF GFCTB008
                                       TO DINIC-ADSAO-COMP OF GFCTB001
               MOVE CSEQ-AGPTO-CTA     OF GFCTB008
                                       TO CSEQ-AGPTO-CTA   OF GFCTB001
               PERFORM 2131-ACESSAR-GFCTB001
               PERFORM 2831-VER-DESPREZA
           END-IF.

           PERFORM 2820-LER-CURSOR-GFCTB008.

           IF  WRK-FIM                     EQUAL 'N'
               IF  IND-1                   GREATER 5
                   MOVE CSERVC-TARIF       OF GFCTB008
                                           TO CSERVC-TARIF   OF GFCTB001
                   MOVE CAGPTO-CTA         OF GFCTB008
                                           TO CAGPTO-CTA     OF GFCTB001
                   MOVE DINIC-ADSAO-COMP   OF GFCTB008
                                         TO DINIC-ADSAO-COMP OF GFCTB001
                   MOVE CSEQ-AGPTO-CTA     OF GFCTB008
                                           TO CSEQ-AGPTO-CTA OF GFCTB001
                   PERFORM 2131-ACESSAR-GFCTB001
                   IF  GFCTLL-IND-DATA     EQUAL 'P'
                       IF  WRK-5507-IDTFD-MASTER EQUAL 'S' OR
                           CDEPDC          OF GFCTB0D8
                                           EQUAL WRK-COD-JUNCAO-0315
                           IF (WRK-DATA-INIC LESS WRK-DPROCM AND
                               WRK-DATA-FIM  LESS WRK-DPROCM)
                               MOVE 'X'    TO WRK-FLAG
                               GO TO 2830-99-FIM
                           END-IF
                       ELSE
                           MOVE 'X'        TO WRK-FLAG
                           GO TO 2830-99-FIM
                       END-IF
                   END-IF
                   MOVE 'S'                TO WRK-FIM
                   MOVE CSERVC-TARIF       OF GFCTB008
                                           TO GFCTLM-PONT-PACOTE
                   MOVE CAGPTO-CTA         OF GFCTB008
                                           TO GFCTLM-PONT-AGPTO
                   MOVE CSGL-UF            OF GFCTB008
                                           TO GFCTLM-PONT-CH-ALP
                   MOVE DINIC-ADSAO-COMP   OF GFCTB008
                                           TO GFCTLM-PONT-CH-DATA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2830-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR SE O REGISTRO SERA DESPREZADO OU NAO                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2831-VER-DESPREZA               SECTION.
      *----------------------------------------------------------------*

           IF  GFCTLL-IND-DATA             EQUAL 'P'
               IF  WRK-5507-IDTFD-MASTER   EQUAL 'S' OR
                   CDEPDC                  OF GFCTB0D8
                                           EQUAL WRK-COD-JUNCAO-0315
                   IF (WRK-DATA-INIC       LESS WRK-DPROCM AND
                       WRK-DATA-FIM        LESS WRK-DPROCM)
                       GO TO 2831-99-FIM
                   END-IF
               ELSE
                   GO TO 2831-99-FIM
               END-IF
           END-IF.

           MOVE CSEQ-AGPTO-CTA         OF GFCTB008
                                       TO GFCTLM-SEQUENCIA(IND-1).
           MOVE CSERVC-TARIF           OF GFCTB008
                                       TO GFCTLM-PACOTE(IND-1).
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTLM-DESC-PACOTE(IND-1).
           MOVE CSGL-UF                OF GFCTB008
                                       TO GFCTLM-UF(IND-1).

           IF  HMANUT-REG OF GFCTB001
                                 EQUAL '0001-01-01-01.01.01.000001'
              MOVE 'N'                 TO GFCTLM-CANCELADO(IND-1)
           ELSE
              MOVE 'S'                 TO GFCTLM-CANCELADO(IND-1)
           END-IF.

           PERFORM 28311-OBTER-DESC-UF.

           MOVE WRK-AREA-DCLGEN-DCITV002 TO DCITV002.
           MOVE IUF  OF DCITV002       TO GFCTLM-DESC-NOME(IND-1).

           MOVE DINIC-ADSAO-COMP       OF GFCTB008
                                       TO GFCTLM-DATA-INICIO(IND-1).
           MOVE DFIM-ADSAO-COMP        OF GFCTB001
                                       TO GFCTLM-DATA-FIM(IND-1).
BI0810     MOVE CPSSOA-SERVC-TARIF     OF GFCTB001
BI0810                                 TO GFCTLM-CPSSOA-TARIFA(IND-1).

           ADD 166                     TO GFCTLM-LL
           ADD 1                       TO GFCTLM-QTDE-TOT-REG
                                          IND-1.

      *----------------------------------------------------------------*
       2831-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTER DESCRICAO UF                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       28311-OBTER-DESC-UF             SECTION.
      *----------------------------------------------------------------*

           MOVE CSGL-UF                OF GFCTB008
                                       TO CSGL-UF OF DCITV002.
           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-ERRO-AREA
                                             WRK-SQLCA-DCIT.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTLM-ERRO
               MOVE ZEROS              TO GFCTLM-COD-SQL-ERRO
               MOVE 0735               TO GFCTLM-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG-ERRO TO GFCTLM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 1
               MOVE 1                  TO GFCTLM-ERRO
               MOVE ZEROS              TO GFCTLM-COD-SQL-ERRO
               MOVE 0736               TO GFCTLM-COD-MSG-ERRO
               MOVE '0736'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG-ERRO TO GFCTLM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 2
               MOVE 1                  TO GFCTLM-ERRO
               MOVE ZEROS              TO GFCTLM-COD-SQL-ERRO
               MOVE 0737               TO GFCTLM-COD-MSG-ERRO
               MOVE '0737'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG-ERRO TO GFCTLM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 3
               MOVE 1                  TO GFCTLM-ERRO
               MOVE ZEROS              TO GFCTLM-COD-SQL-ERRO
               MOVE 0738               TO GFCTLM-COD-MSG-ERRO
               MOVE '0738'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG-ERRO TO GFCTLM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL  4
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'DCITV002'         TO WRK-NOM-TAB
               MOVE 'V01_UF'           TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0270'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTLM-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 9                  TO GFCTLM-ERRO
               MOVE 'GFCT0135'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-SQLCA-DCIT     TO SQLCA
                                          GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTLM-COD-SQL-ERRO
               MOVE SPACES             TO GFCTLM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       28311-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR GFCTB008                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2840-FECHAR-CURSOR-GFCTB008     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR08-GFCTB008
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB008'      TO WRK-NOM-TAB
               MOVE 'ADSAO_GRP_UF'     TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0280'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTLM-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2840-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *RETORNA AO CHAMADOR                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
