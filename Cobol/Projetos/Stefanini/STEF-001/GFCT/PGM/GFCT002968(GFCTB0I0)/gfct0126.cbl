      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0126.
       AUTHOR.     JEFERSON PAULO DALPONTE.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0126                                    *
      *    PROGRAMADOR.:   JEFERSON PAULO DALPONTE - CPM PATO BRANCO   *
      *    ANALISTA CPM:   MARCIO CRISTIANO CUSTIN - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO CREMM           - PROCWORK /GP.50   *
      *    DATA........:   15/03/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTA SOLICITACOES DE PERMISSAO PARA     *
      *      ADESAO.                                                   *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *                   DB2PRD.V01UF                  DCITV002       *
      *                   DB2PRD.PRMSS_GRP_DEPDC        GFCTB0C7       *
      *                   DB2PRD.PRMSS_GRP_EMPR         GFCTB0C8       *
      *                   DB2PRD.PRMSS_GRP_MUN          GFCTB0C9       *
      *                   DB2PRD.PRMSS_GRP_PAB          GFCTB0D0       *
      *                   DB2PRD.PRMSS_GRP_PSTAL        GFCTB0D1       *
      *                   DB2PRD.PRMSS_GRP_SGMTO        GFCTB0D2       *
      *                   DB2PRD.PRMSS_GRP_UF           GFCTB0D3       *
      *                   DB2PRD.TPRMSS_GRP_CLI         GFCTB0I0       *
      *                   DB2PRD.TTPO_SGMTO_GSTAO       GFCTB0H5       *
BI0810*                   DB2PRD.PRMSS_ADSAO_PCOTE      GFCTB0C6       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTKB - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTKC - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5523 *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA DO MODULO GFCT5523   *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5522 *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA DO MODULO GFCT5522   *
      *    I#GFCTGB - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5109 *
      *                                     ENTRADA DO MODULO GFCT5090 *
      *    I#GFCTGC - AREA DE COMUNICACAO - SAIDA DO MODULO GFCT5109   *
      *                                     SAIDA DO MODULO GFCT5090   *
      *    I#GFCT1V - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5006 *
      *    I#GFCT1X - AREA DE COMUNICACAO - SAIDA DO MODULO GFCT5006   *
      *    I#RURC88 - AREA DE COMUNICACAO COM MODULO RURC9020          *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5109 - OBTER DESCRICAO PAB.                             *
      *    GFCT5006 - OBTER DESCRICAO DEPDC.                           *
      *    GFCT5090 - OBTER DESCRICAO POSTO.                           *
      *    RURC9020 - OBTER DESCRICAO MUNICIPIO.                       *
      *    DCITV002 - OBTER DESCRICAO UF.                              *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *================================================================*
      *                    A L T E R A C A O                           *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA     : MARCUS VINICIUS          - PROCWORK          *
      *    DATA         : 20/10/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      TRATAR O CAMPO DE ORIGEM DA SOLICITACAO PARA PERMISSAO    *
      *      INDIVIDUAL, PODENDO SER:                                  *
      *      LEGADO, TARIFAS OU AMBOS.                                 *
      *                                                                *
BI0810*================================================================*
BI0810*                    A L T E R A C A O                           *
BI0810*----------------------------------------------------------------*
BI0810*                                                                *
BI0810*    ANALISTA     : UBIRAJARA MIRANDA(BIRA)  - PROCWORK          *
BI0810*    DATA         : AGO/2010                                     *
BI0810*                                                                *
BI0810*    OBJETIVO     :                                              *
BI0810*    - ENVIAR PARA WEB O CPO. (TIPO DE PESSOA) DA TABELA GFCTB0C6*
BI0810*                                                                *
BI0511*----------------------------------------------------------------*
BI0511*                 U L T I M A   A L T E R A C A O                *
BI0511*----------------------------------------------------------------*
BI0511*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI0511       *
BI0511*----------------------------------------------------------------*
BI0511*                                                                *
BI0511*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA/PROCWORK             *
BI0511*    DATA........:  05 / 2011                                    *
BI0511*    OBJETIVO....:  PROJETO CARTAO BONUS CELULAR - FASE II       *
BI0511*                                                                *
BI0412*================================================================*
BI0412*----------------------------------------------------------------*
BI0412*                 U L T I M A   A L T E R A C A O                *
BI0412*----------------------------------------------------------------*
BI0412*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI0412       *
BI0412*----------------------------------------------------------------*
BI0412*                                                                *
BI0412*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA - IT                 *
BI0412*    DATA........:  04 / 2012                                    *
BI0412*    OBJETIVO....:  SETAR FLAG PARA PERMITIR VIZUALIZACAO        *
BI0412*                   DE ADESAO COMPULSORIA, A ADESAO COMPULSORIA  *
BI0412*                   ASSINALADA NO ONLINE SERA INSERIDA NA        *
BI0412*                   BASE GFCTB009 DE ADESAO INDIVIDUAL.          *
BI0412*                                                                *
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
           '* INICIO DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *---------------------------------------------------------------*

MAR10  77  IND-1                       PIC  9(001) COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *---------------------------------------------------------------*

       77  WRK-DINIC                   PIC  9(008)         VALUE ZEROS.
       77  WRK-DFIM                    PIC  9(008)         VALUE ZEROS.
       77  WRK-DPROCM                  PIC  9(008)         VALUE ZEROS.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOME-TAB                PIC  X(011)         VALUE SPACES.
       77  WRK-FIM-CURSOR1             PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-CURSOR2             PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-CURSOR3             PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-CURSOR4             PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-CURSOR5             PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-CURSOR6             PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-CURSOR7             PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-CURSOR8             PIC  X(001)         VALUE SPACES.

       01  WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-INV.
           05  WRK-ANO-INV             PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-INV             PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-INV             PIC  9(002)         VALUE ZEROS.

       01  WRK-MSG01.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-NOME-MOD             PIC  X(008)         VALUE SPACES.

       01  WRK-TAM-01-COM-S            PIC -9(001)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-01-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-01-SEM-S        PIC  9(001).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA MODULO DCIT8000 *'.
      *---------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           03  WRK-AREA-DCLGEN-DCITV002 PIC X(070)         VALUE SPACES.
           03  WRK-DCIT-CODIGO-RETORNO  PIC 9(002)         VALUE ZEROS.

       01  WRK-POOL7100                 PIC X(107)         VALUE SPACES.
       01  WRK-SQLCA                    PIC X(136)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA INCLUDES *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

       COPY 'I#RURC88'.

       COPY 'I#GFCT1V'.

       COPY 'I#GFCT1X'.

       COPY 'I#GFCTGB'.

       COPY 'I#GFCTGC'.

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
             INCLUDE GFCTB0C6
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H5
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C7
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0I0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C9
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D3
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSORES *'.
      *---------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-GFCTB0D0 CURSOR FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CDEPDC,
                   CPOSTO_SERVC
             FROM  DB2PRD.PRMSS_GRP_PAB
             WHERE CSERVC_TARIF       = :GFCTB0D0.CSERVC-TARIF
             AND   CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA
             AND ((CDEPDC             = :GFCTB0D0.CDEPDC
             AND   CPOSTO_SERVC       = :GFCTB0D0.CPOSTO-SERVC
             AND   DINIC_PRMSS_PCOTE >= :GFCTB0D0.DINIC-PRMSS-PCOTE)
              OR  (CDEPDC             = :GFCTB0D0.CDEPDC
             AND   CPOSTO_SERVC       > :GFCTB0D0.CPOSTO-SERVC)
              OR  (CDEPDC             > :GFCTB0D0.CDEPDC))
             ORDER BY
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CDEPDC,
                   CPOSTO_SERVC,
                   DINIC_PRMSS_PCOTE
           END-EXEC.

           EXEC SQL
             DECLARE CSR02-GFCTB0C7 CURSOR FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CDEPDC
             FROM  DB2PRD.PRMSS_GRP_DEPDC
             WHERE CSERVC_TARIF       = :GFCTB0C7.CSERVC-TARIF
             AND   CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA
             AND ((CDEPDC             = :GFCTB0C7.CDEPDC
             AND   DINIC_PRMSS_PCOTE >= :GFCTB0C7.DINIC-PRMSS-PCOTE)
              OR  (CDEPDC             > :GFCTB0C7.CDEPDC))
             ORDER BY
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CDEPDC,
                   DINIC_PRMSS_PCOTE
           END-EXEC.

           EXEC SQL
             DECLARE CSR03-GFCTB0C8 CURSOR FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CJUNC_DEPDC_DEB,
                   CNRO_CTA_DEB
             FROM  DB2PRD.PRMSS_GRP_EMPR
             WHERE CSERVC_TARIF       = :GFCTB0C8.CSERVC-TARIF
             AND   CAGPTO_CTA         = :GFCTB0C8.CAGPTO-CTA
             AND ((CJUNC_DEPDC_DEB    = :GFCTB0C8.CJUNC-DEPDC-DEB
             AND   CNRO_CTA_DEB       = :GFCTB0C8.CNRO-CTA-DEB
             AND   DINIC_PRMSS_PCOTE >= :GFCTB0C8.DINIC-PRMSS-PCOTE)
              OR  (CJUNC_DEPDC_DEB    = :GFCTB0C8.CJUNC-DEPDC-DEB
             AND   CNRO_CTA_DEB       > :GFCTB0C8.CNRO-CTA-DEB)
              OR  (CJUNC_DEPDC_DEB    > :GFCTB0C8.CJUNC-DEPDC-DEB))
             ORDER BY
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CJUNC_DEPDC_DEB,
                   CNRO_CTA_DEB,
                   DINIC_PRMSS_PCOTE
           END-EXEC.

           EXEC SQL
             DECLARE CSR04-GFCTB0D1 CURSOR FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CDEPDC,
                   CPOSTO_SERVC
             FROM  DB2PRD.PRMSS_GRP_PSTAL
             WHERE CSERVC_TARIF       = :GFCTB0D1.CSERVC-TARIF
             AND   CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA
             AND ((CDEPDC             = :GFCTB0D1.CDEPDC
             AND   CPOSTO_SERVC       = :GFCTB0D1.CPOSTO-SERVC
             AND   DINIC_PRMSS_PCOTE >= :GFCTB0D1.DINIC-PRMSS-PCOTE)
              OR  (CDEPDC             = :GFCTB0D1.CDEPDC
             AND   CPOSTO_SERVC       > :GFCTB0D1.CPOSTO-SERVC)
              OR  (CDEPDC             > :GFCTB0D1.CDEPDC))
             ORDER BY
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CDEPDC,
                   CPOSTO_SERVC,
                   DINIC_PRMSS_PCOTE
           END-EXEC.

           EXEC SQL
             DECLARE CSR05-GFCTB0I0 CURSOR FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CCGC_CPF,
                   CFLIAL_CGC,
                   CCTRL_CPF_CGC,
ST25X6             CCGC_CPF_ST,
ST25X6             CFLIAL_CGC_ST,
ST25X6             CCTRL_CPF_CGC_ST
             FROM  DB2PRD.TPRMSS_GRP_CLI
             WHERE CSERVC_TARIF       = :GFCTB0I0.CSERVC-TARIF
             AND   CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA
ST25X6*      AND ((CCGC_CPF           = :GFCTB0I0.CCGC-CPF
ST25X6*      AND   DINIC_PRMSS_PCOTE >= :GFCTB0I0.DINIC-PRMSS-PCOTE)
ST25X6*       OR  (CCGC_CPF           > :GFCTB0I0.CCGC-CPF))
ST25X6       AND ((CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST
ST25X6       AND   DINIC_PRMSS_PCOTE >= :GFCTB0I0.DINIC-PRMSS-PCOTE)
ST25X6        OR  (CCGC_CPF_ST        > :GFCTB0I0.CCGC-CPF-ST))
             ORDER BY
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CCGC_CPF,
                   CFLIAL_CGC,
                   CCTRL_CPF_CGC,
                   DINIC_PRMSS_PCOTE
           END-EXEC.

           EXEC SQL
             DECLARE CSR06-GFCTB0D2 CURSOR FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CSGMTO_GSTAO_TARIF
             FROM  DB2PRD.PRMSS_GRP_SGMTO
             WHERE CSERVC_TARIF       = :GFCTB0D2.CSERVC-TARIF
             AND   CAGPTO_CTA         = :GFCTB0D2.CAGPTO-CTA
             AND ((CSGMTO_GSTAO_TARIF = :GFCTB0D2.CSGMTO-GSTAO-TARIF
             AND   DINIC_PRMSS_PCOTE >= :GFCTB0D2.DINIC-PRMSS-PCOTE)
              OR  (CSGMTO_GSTAO_TARIF > :GFCTB0D2.CSGMTO-GSTAO-TARIF))
             ORDER BY
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSGMTO_GSTAO_TARIF,
                   DINIC_PRMSS_PCOTE
           END-EXEC.

           EXEC SQL
             DECLARE CSR07-GFCTB0C9 CURSOR FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CMUN_IBGE
             FROM  DB2PRD.PRMSS_GRP_MUN
             WHERE CSERVC_TARIF       = :GFCTB0C9.CSERVC-TARIF
             AND   CAGPTO_CTA         = :GFCTB0C9.CAGPTO-CTA
             AND ((CMUN_IBGE          = :GFCTB0C9.CMUN-IBGE
             AND   DINIC_PRMSS_PCOTE >= :GFCTB0C9.DINIC-PRMSS-PCOTE)
              OR  (CMUN_IBGE          > :GFCTB0C9.CMUN-IBGE))
             ORDER BY
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CMUN_IBGE,
                   DINIC_PRMSS_PCOTE
           END-EXEC.
           EXEC SQL
             DECLARE CSR08-GFCTB0D3 CURSOR FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CSGL_UF
             FROM  DB2PRD.PRMSS_GRP_UF
             WHERE CSERVC_TARIF       = :GFCTB0D3.CSERVC-TARIF
             AND   CAGPTO_CTA         = :GFCTB0D3.CAGPTO-CTA
             AND ((CSGL_UF            = :GFCTB0D3.CSGL-UF
             AND   DINIC_PRMSS_PCOTE >= :GFCTB0D3.DINIC-PRMSS-PCOTE)
              OR  (CSGL_UF            > :GFCTB0D3.CSGL-UF))
             ORDER BY
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSGL_UF,
                   DINIC_PRMSS_PCOTE
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTKB'.

       COPY 'I#GFCTKC'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTKB-ENTRADA
                                                      GFCTKC-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *ROTINA INICIAL DO PROGRAMA                                      *
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
                                          GFCTKC-SAIDA.

           INITIALIZE GFCTKC-SAIDA
                      IND-1
                      GFCT0M-AREA-ERROS.

           PERFORM 1100-MONTAR-AREA-SAIDA.

           PERFORM 1200-CONSISTIR-DADOS.

           PERFORM 1300-VERIFICAR-DISP-SISTEMA.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *----------------------------------------------------------------*
       1100-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO GFCTKC-SAIDA.

           INITIALIZE GFCTKC-SAIDA.

BI0412     MOVE 385                        TO GFCTKC-LL.
           MOVE ZEROS                      TO GFCTKC-ZZ.
           MOVE GFCTKB-TRANSACAO           TO GFCTKC-TRANSACAO.
           MOVE GFCTKB-FUNCAO              TO GFCTKC-FUNCAO.

           IF  GFCTKB-QTDE-OCOR            NOT NUMERIC
               MOVE ZEROS                  TO GFCTKC-QTDE-OCOR
           ELSE
               MOVE GFCTKB-QTDE-OCOR       TO GFCTKC-QTDE-OCOR
           END-IF.

           IF  GFCTKB-QTDE-TOT-REG         NOT NUMERIC
               MOVE ZEROS                  TO GFCTKC-QTDE-TOT-REG
           ELSE
               MOVE GFCTKB-QTDE-TOT-REG    TO GFCTKC-QTDE-TOT-REG
           END-IF.

           MOVE GFCTKB-FUNC-BDSCO          TO GFCTKC-FUNC-BDSCO.
           MOVE GFCTKB-PONTEIRO            TO GFCTKC-PONTEIRO.
           MOVE GFCTKB-IND-DATA            TO GFCTKC-IND-DATA.

           MOVE 'N'                        TO GFCTKC-FIM.

           MOVE ZEROS                      TO GFCTKC-ERRO
                                              GFCTKC-COD-SQL-ERRO
                                              GFCTKC-COD-MSG-ERRO.
           MOVE '0000'                     TO GFCTG2-COD-MSG.

           PERFORM 1110-OBTER-DESC-MSG.

           MOVE  GFCTG3-DESC-MSG           TO GFCTKC-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTKB-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTKB-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTKB-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS OR
               GFCTG3-ERRO                     EQUAL 9
               MOVE 9                          TO GFCTKC-ERRO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
                                                  WRK-NOME-MOD
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0126'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                     EQUAL 1
               MOVE 1                          TO GFCTKC-ERRO
               MOVE 'S'                        TO GFCTKC-FIM
               MOVE GFCTG3-COD-SQL-ERRO        TO GFCTKC-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO        TO GFCTKC-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO       TO GFCTKC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR DADOS                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKB-TRANSACAO        EQUAL LOW-VALUES        OR
               GFCTKB-TRANSACAO        EQUAL SPACES            OR
               GFCTKB-FUNCAO           EQUAL LOW-VALUES        OR
               GFCTKB-FUNCAO           EQUAL SPACES            OR
               GFCTKB-FUNC-BDSCO       EQUAL LOW-VALUES        OR
               GFCTKB-FUNC-BDSCO       EQUAL SPACES            OR
              (GFCTKB-IND-DATA         NOT EQUAL 'P' AND 'T')  OR
               GFCTKB-PONT-PACOTE      NOT NUMERIC             OR
               GFCTKB-PONT-PACOTE      EQUAL ZEROS             OR
              (GFCTKB-PONT-AGPTO       NOT EQUAL  4 AND  9 AND
                                                 14 AND 15 AND
                                                 16 AND 17 AND
                                                 18 AND 19 AND
                                                 20)
               MOVE 1                  TO GFCTKC-ERRO
                                          GFCTKC-COD-MSG-ERRO
               MOVE ZEROS              TO GFCTKC-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTKC-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTKC-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 1210-ACESSAR-GFCTB0A1.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ACESSAR GFCTB0A1 PARA BUSCAR DATA                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-ACESSAR-GFCTB0A1           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO  :GFCTB0A1.DPROCM-ATUAL
             FROM  DB2PRD.PARM_DATA_PROCM
             WHERE CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0A1'      TO WRK-NOME-TAB
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV.
           MOVE WRK-MES-DB2            TO WRK-MES-INV.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DPROCM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA MOVER ERRO SQL                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1211-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE  9                     TO GFCTKC-ERRO.
           MOVE 'GFCT0126'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
                                          GFCTKC-COD-SQL-ERRO.
           MOVE SPACES                 TO GFCTKC-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG      WRK-NOME-TAB

           DELIMITED BY '  '           INTO GFCTKC-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICA DISPONIBILIDADE DO SISTEMA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-VERIFICAR-DISP-SISTEMA     SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5522'                     TO WRK-MODULO.
           MOVE 'GFCT'                         TO GFCTFZ-SISTEMA.
           MOVE GFCTKB-TRANSACAO               TO GFCTFZ-TRANSACAO.
           MOVE GFCTKB-FUNCAO                  TO GFCTFZ-FUNCAO.
           MOVE ZEROS                          TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                            TO GFCTFZ-FIM.

           CALL 'POOL0081'                     USING WRK-MODULO
                                                     GFCTFZ-ENTRADA
                                                     GFCTG1-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTKC-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0126'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                     EQUAL 9
               IF  GFCT0M-TIPO-ACESSO          NOT EQUAL 'DB2'
                   MOVE GFCTG1-DESC-MSG-ERRO   TO GFCT0M-TEXTO
                   MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO    TO GFCTKC-COD-SQL-ERRO
               END-IF
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
               MOVE 'GFCT0126'                 TO GFCT0M-TRANSACAO
               MOVE GFCTG1-ERRO                TO GFCTKC-ERRO
               MOVE GFCTG1-COD-MSG-ERRO        TO GFCTKC-COD-MSG-ERRO
               MOVE 'S'                        TO GFCTKC-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE              EQUAL 1
               MOVE 9                          TO GFCTKC-ERRO
               MOVE 'SISTEMA INDISPONIVEL'     TO GFCT0M-TEXTO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0126'                 TO GFCT0M-TRANSACAO
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

           EVALUATE GFCTKB-PONT-AGPTO
               WHEN 15
                   PERFORM 2100-CONSULTAR-PAB

               WHEN 14
                   PERFORM 2200-CONSULTAR-AGENCIA

               WHEN 16
                   PERFORM 2300-CONSULTAR-EMPRESA

               WHEN 17
                   PERFORM 2400-CONS-POSTAL-EXPRESSO

               WHEN 20
                   PERFORM 2400-CONS-POSTAL-EXPRESSO

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
      *ROTINA PARA TRATAR CURSOR1                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-CONSULTAR-PAB              SECTION.
      *----------------------------------------------------------------*

           PERFORM 2110-ABRIR-CURSOR1.

           PERFORM 2120-LER-CURSOR1.

           PERFORM 2130-PROCESSAR-CURSOR1  UNTIL
               IND-1                       NOT LESS 5 OR
               WRK-FIM-CURSOR1             EQUAL 'S'.

           IF  WRK-FIM-CURSOR1             NOT EQUAL 'S'
               MOVE 'N'                    TO GFCTKC-FIM
               MOVE CSERVC-TARIF           OF GFCTB0D0
                                           TO GFCTKC-PONT-PACOTE
               MOVE CAGPTO-CTA             OF GFCTB0D0
                                           TO GFCTKC-PONT-AGPTO
               MOVE CDEPDC                 OF GFCTB0D0
                                           TO GFCTKC-PONT-CH-NUM5
               MOVE CPOSTO-SERVC           OF GFCTB0D0
                                           TO GFCTKC-PONT-CH-NUM7
               MOVE DINIC-PRMSS-PCOTE      OF GFCTB0D0
                                           TO GFCTKC-PONT-CH-DATA
           END-IF.

           PERFORM 2140-FECHAR-CURSOR1.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ABRIR CURSOR1                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-ABRIR-CURSOR1              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-FIM-CURSOR1.
           MOVE GFCTKB-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB0D0.
           MOVE GFCTKB-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB0D0.
           MOVE GFCTKB-PONT-CH-NUM5    TO CDEPDC            OF GFCTB0D0.
           MOVE GFCTKB-PONT-CH-NUM7    TO CPOSTO-SERVC      OF GFCTB0D0.

           IF  GFCTKB-PONT-CH-DATA     EQUAL SPACES
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0D0
           ELSE
               MOVE GFCTKB-PONT-CH-DATA TO DINIC-PRMSS-PCOTE OF GFCTB0D0
           END-IF.

           EXEC SQL
               OPEN CSR01-GFCTB0D0
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0D0'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_PAB   ' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA LEITURA DO CURSOR1                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-LER-CURSOR1                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-GFCTB0D0 INTO
                   :GFCTB0D0.CSERVC-TARIF,
                   :GFCTB0D0.CAGPTO-CTA,
                   :GFCTB0D0.DINIC-PRMSS-PCOTE,
                   :GFCTB0D0.CSEQ-AGPTO-CTA,
                   :GFCTB0D0.CDEPDC,
                   :GFCTB0D0.CPOSTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0D0'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_PAB   ' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 NOT EQUAL ZEROS
               MOVE 'S'                TO WRK-FIM-CURSOR1
                                          GFCTKC-FIM
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA PROCESSAR CURSOR1                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2130-PROCESSAR-CURSOR1          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0D0
                                       TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE CAGPTO-CTA             OF GFCTB0D0
                                       TO CAGPTO-CTA        OF GFCTB0C6.
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0D0
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0D0
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C6.

           PERFORM 2131-ACESSAR-GFCTB0C6.

           IF (GFCTKB-IND-DATA         EQUAL 'T')             OR
             ((WRK-DINIC               NOT GREATER WRK-DPROCM AND
               WRK-DFIM                NOT LESS WRK-DPROCM)   OR
              (WRK-DINIC               NOT LESS WRK-DPROCM))
               ADD 1                   TO IND-1
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0D0
                                       TO GFCTKC-DATA-INICIO(IND-1)
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0D0
                                       TO GFCTKC-SEQUENCIA(IND-1)
               MOVE CDEPDC             OF GFCTB0D0
                                       TO GFCTKC-AGENCIA(IND-1)
               MOVE CPOSTO-SERVC       OF GFCTB0D0
                                       TO GFCTKC-POSTO(IND-1)
               MOVE DFIM-PRMSS-PCOTE   OF GFCTB0C6
                                       TO GFCTKC-DATA-FIM(IND-1)
               MOVE CINDCD-ADSAO-INDVD OF GFCTB0C6
                                       TO GFCTKC-TP-ADESAO(IND-1)

SMS            IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'C'
BI0810             MOVE CPSSOA-SERVC-TARIF OF GFCTB0C6
BI0810                                 TO GFCTKC-CPSSOA-TARIFA(IND-1)
BI0412             MOVE CINDCD-VSLAO-COMP  OF GFCTB0C6
BI0412                                 TO GFCTKC-VISUAL-COMPULS(IND-1)
SMS                IF  CINDCD-EXCEC-ADSAO OF GFCTB0C6  EQUAL 'S'
SMS                   MOVE  1          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
SMS                ELSE
SMS                   MOVE  0          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             END-IF
BI0412         END-IF

BI0412         IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'I'
SMS                MOVE CIDTFD-PRMSS-INDVD OF GFCTB0C6
SMS                                    TO GFCTKC-PRMSS-INDVD(IND-1)
SMS            END-IF

BI0412         IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'A'
BI0412             MOVE CPSSOA-SERVC-TARIF OF GFCTB0C6
BI0412                                 TO GFCTKC-CPSSOA-TARIFA(IND-1)
BI0412             MOVE CIDTFD-PRMSS-INDVD OF GFCTB0C6
BI0412                                 TO GFCTKC-PRMSS-INDVD(IND-1)
BI0412             MOVE CINDCD-VSLAO-COMP  OF GFCTB0C6
BI0412                                 TO GFCTKC-VISUAL-COMPULS(IND-1)
BI0412             IF  CINDCD-EXCEC-ADSAO OF GFCTB0C6  EQUAL 'S'
BI0412                MOVE  1          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             ELSE
BI0412                MOVE  0          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             END-IF
BI0412         END-IF

               IF  CINDCD-AGPTO-TOT    OF GFCTB0C6
                                       EQUAL 'S'
                   MOVE 'TODOS'        TO GFCTKC-DESC-NOME(IND-1)
               ELSE
                   PERFORM 2132-OBTER-DESC-PAB
               END-IF

BI0412         ADD 125                 TO GFCTKC-LL
               ADD 1                   TO GFCTKC-QTDE-TOT-REG
           END-IF.

           PERFORM 2120-LER-CURSOR1.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ACESSAR GFCTB0C6                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2131-ACESSAR-GFCTB0C6           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CFUNC_MANUT_INCL,
                   CFUNC_MANUT,
                   DFIM_PRMSS_PCOTE,
                   CINDCD_ADSAO_INDVD,
                   CINDCD_AGPTO_TOT,
                   HMANUT_REG,
                   CINDCD_EXCEC_ADSAO,
MAR10              CIDTFD_PRMSS_INDVD,
BI0412             CINDCD_VSLAO_COMP,
BI0810             CPSSOA_SERVC_TARIF
             INTO
                   :GFCTB0C6.CSERVC-TARIF,
                   :GFCTB0C6.CAGPTO-CTA,
                   :GFCTB0C6.CSEQ-AGPTO-CTA,
                   :GFCTB0C6.DINIC-PRMSS-PCOTE,
                   :GFCTB0C6.CFUNC-MANUT-INCL,
                   :GFCTB0C6.CFUNC-MANUT,
                   :GFCTB0C6.DFIM-PRMSS-PCOTE,
                   :GFCTB0C6.CINDCD-ADSAO-INDVD,
                   :GFCTB0C6.CINDCD-AGPTO-TOT,
                   :GFCTB0C6.HMANUT-REG,
                   :GFCTB0C6.CINDCD-EXCEC-ADSAO,
MAR10              :GFCTB0C6.CIDTFD-PRMSS-INDVD,
BI0412             :GFCTB0C6.CINDCD-VSLAO-COMP,
BI0810             :GFCTB0C6.CPSSOA-SERVC-TARIF
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE CSERVC_TARIF       = :GFCTB0C6.CSERVC-TARIF
              AND  CAGPTO_CTA         = :GFCTB0C6.CAGPTO-CTA
              AND  CSEQ_AGPTO_CTA     = :GFCTB0C6.CSEQ-AGPTO-CTA
              AND  DINIC_PRMSS_PCOTE  = :GFCTB0C6.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0C6'      TO WRK-NOME-TAB
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV.
           MOVE WRK-MES-DB2            TO WRK-MES-INV.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DINIC.
           MOVE DFIM-PRMSS-PCOTE       OF GFCTB0C6
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV.
           MOVE WRK-MES-DB2            TO WRK-MES-INV.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DFIM.

      *----------------------------------------------------------------*
       2131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2132-OBTER-DESC-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                         TO GFCTGB-ENTRADA
                                                  GFCTGC-SAIDA
                                                  GFCT0M-AREA-ERROS.

           INITIALIZE                          GFCTGB-ENTRADA
                                               GFCTGC-SAIDA
                                               GFCT0M-AREA-ERROS.

           MOVE 100                            TO GFCTGB-LL.
           MOVE ZEROS                          TO GFCTGB-ZZ.
           MOVE GFCTKB-TRANSACAO               TO GFCTGB-TRANSACAO.
           MOVE GFCTKB-FUNCAO                  TO GFCTGB-FUNCAO.
           MOVE ZEROS                          TO GFCTGB-QTDE-OCOR
                                                  GFCTGB-QTDE-TOT-REG.
           MOVE GFCTKB-FUNC-BDSCO              TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                            TO GFCTGB-FIM.
           MOVE CDEPDC             OF GFCTB0D0 TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC       OF GFCTB0D0 TO GFCTGB-COD-POSTO.
           MOVE 'GFCT5109'                     TO WRK-MODULO.

           CALL 'POOL0081'                     USING WRK-MODULO
                                                     GFCTGB-ENTRADA
                                                     GFCTGC-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO WRK-MODULO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0126'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO                     NOT EQUAL ZEROS
               MOVE 'S'                        TO GFCTKC-FIM
               MOVE GFCTGC-ERRO                TO GFCTKC-ERRO
               MOVE GFCTGC-COD-SQL-ERRO        TO GFCTKC-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO        TO GFCTKC-COD-MSG-ERRO
               IF  GFCTGC-ERRO                 EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO   TO GFCTKC-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109-'          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE           INTO GFCTKC-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTGC-DESC-POSTO(1)         TO GFCTKC-DESC-NOME(IND-1).

      *----------------------------------------------------------------*
       2132-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FECHAR CURSOR1                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2140-FECHAR-CURSOR1             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR01-GFCTB0D0
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0D0'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_PAB   ' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TRATAR CURSOR2                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-CONSULTAR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2210-ABRIR-CURSOR2.

           PERFORM 2220-LER-CURSOR2.

           PERFORM 2230-PROCESSAR-CURSOR2  UNTIL
               IND-1                       NOT LESS 5 OR
               WRK-FIM-CURSOR2             EQUAL 'S'.

           IF  WRK-FIM-CURSOR2             NOT EQUAL 'S'
               MOVE 'N'                    TO GFCTKC-FIM
               MOVE CSERVC-TARIF           OF GFCTB0C7
                                           TO GFCTKC-PONT-PACOTE
               MOVE CAGPTO-CTA             OF GFCTB0C7
                                           TO GFCTKC-PONT-AGPTO
               MOVE CDEPDC                 OF GFCTB0C7
                                           TO GFCTKC-PONT-CH-NUM5
               MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C7
                                           TO GFCTKC-PONT-CH-DATA
           END-IF.

           PERFORM 2240-FECHAR-CURSOR2.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ABRIR CURSOR2                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-ABRIR-CURSOR2              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-FIM-CURSOR2.
           MOVE GFCTKB-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB0C7.
           MOVE GFCTKB-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB0C7.
           MOVE GFCTKB-PONT-CH-NUM5    TO CDEPDC            OF GFCTB0C7.

           IF  GFCTKB-PONT-CH-DATA     EQUAL SPACES
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0C7
           ELSE
               MOVE GFCTKB-PONT-CH-DATA TO DINIC-PRMSS-PCOTE OF GFCTB0C7
           END-IF.

           EXEC SQL
               OPEN CSR02-GFCTB0C7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0C7'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_DEPDC ' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA LEITURA DO CURSOR2                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2220-LER-CURSOR2                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR02-GFCTB0C7 INTO
                   :GFCTB0C7.CSERVC-TARIF,
                   :GFCTB0C7.CAGPTO-CTA,
                   :GFCTB0C7.DINIC-PRMSS-PCOTE,
                   :GFCTB0C7.CSEQ-AGPTO-CTA,
                   :GFCTB0C7.CDEPDC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0C7'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_DEPDC ' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 NOT EQUAL ZEROS
               MOVE 'S'                TO WRK-FIM-CURSOR2
                                          GFCTKC-FIM
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA PROCESSAR CURSOR2                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2230-PROCESSAR-CURSOR2          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C7
                                       TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE CAGPTO-CTA             OF GFCTB0C7
                                       TO CAGPTO-CTA        OF GFCTB0C6.
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C7
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C7
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C6.

           PERFORM 2131-ACESSAR-GFCTB0C6.

           IF (GFCTKB-IND-DATA         EQUAL 'T')             OR
             ((WRK-DINIC               NOT GREATER WRK-DPROCM AND
               WRK-DFIM                NOT LESS WRK-DPROCM)   OR
              (WRK-DINIC               NOT LESS WRK-DPROCM))
               ADD 1                   TO IND-1
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0C7
                                       TO GFCTKC-DATA-INICIO(IND-1)
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0C7
                                       TO GFCTKC-SEQUENCIA(IND-1)
               MOVE CDEPDC             OF GFCTB0C7
                                       TO GFCTKC-AGENCIA(IND-1)
               MOVE DFIM-PRMSS-PCOTE   OF GFCTB0C6
                                       TO GFCTKC-DATA-FIM(IND-1)
               MOVE CINDCD-ADSAO-INDVD OF GFCTB0C6
                                       TO GFCTKC-TP-ADESAO(IND-1)

SMS            IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'C'
BI0810             MOVE CPSSOA-SERVC-TARIF OF GFCTB0C6
BI0810                                 TO GFCTKC-CPSSOA-TARIFA(IND-1)
BI0412             MOVE CINDCD-VSLAO-COMP OF GFCTB0C6
BI0412                                 TO GFCTKC-VISUAL-COMPULS(IND-1)
SMS                IF  CINDCD-EXCEC-ADSAO OF GFCTB0C6  EQUAL 'S'
SMS                   MOVE  1          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
SMS                ELSE
SMS                   MOVE  0          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             END-IF
BI0412         END-IF

BI0412         IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'I'
SMS                MOVE CIDTFD-PRMSS-INDVD OF GFCTB0C6
SMS                                    TO GFCTKC-PRMSS-INDVD(IND-1)
SMS            END-IF

BI0412         IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'A'
BI0412             MOVE CPSSOA-SERVC-TARIF OF GFCTB0C6
BI0412                                 TO GFCTKC-CPSSOA-TARIFA(IND-1)
BI0412             MOVE CIDTFD-PRMSS-INDVD OF GFCTB0C6
BI0412                                 TO GFCTKC-PRMSS-INDVD(IND-1)
BI0412             MOVE CINDCD-VSLAO-COMP  OF GFCTB0C6
BI0412                                 TO GFCTKC-VISUAL-COMPULS(IND-1)
BI0412             IF  CINDCD-EXCEC-ADSAO OF GFCTB0C6  EQUAL 'S'
BI0412                MOVE  1          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             ELSE
BI0412                MOVE  0          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             END-IF
BI0412         END-IF

               IF  CINDCD-AGPTO-TOT    OF GFCTB0C6
                                       EQUAL 'S'
                   MOVE 'TODAS'        TO GFCTKC-DESC-NOME(IND-1)
               ELSE
                   PERFORM 2231-OBTER-DESC-DEPDC
               END-IF

BI0412         ADD 125                 TO GFCTKC-LL
               ADD 1                   TO GFCTKC-QTDE-TOT-REG
           END-IF.

           PERFORM 2220-LER-CURSOR2.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2231-OBTER-DESC-DEPDC           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1V-ENTRADA
                                          GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                     GFCT1V-ENTRADA
                                          GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           MOVE 660                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.
           MOVE GFCTKB-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTKB-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTKB-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.
           MOVE CDEPDC OF GFCTB0C7     TO GFCT1V-COD-DEPDC(1).

           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO WRK-MODULO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0126'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO                     NOT EQUAL ZEROS
               MOVE 'S'                        TO GFCTKC-FIM
               MOVE GFCT1X-ERRO                TO GFCTKC-ERRO
               MOVE GFCT1X-COD-SQL-ERRO        TO GFCTKC-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO        TO GFCTKC-COD-MSG-ERRO
               IF  GFCT1X-ERRO                 EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO   TO GFCTKC-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006-'          GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE           INTO GFCTKC-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

MAR10      MOVE GFCT1X-DESC-DEPDC(1)(1:40)   TO GFCTKC-DESC-NOME(IND-1).

      *----------------------------------------------------------------*
       2231-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FECHAR CURSOR2                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2240-FECHAR-CURSOR2             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR02-GFCTB0C7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0C7'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_DEPDC ' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TRATAR CURSOR3                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-CONSULTAR-EMPRESA          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2310-ABRIR-CURSOR3.

           PERFORM 2320-LER-CURSOR3.

           PERFORM 2330-PROCESSAR-CURSOR3  UNTIL
               IND-1                       NOT LESS 5 OR
               WRK-FIM-CURSOR3             EQUAL 'S'.

           IF  WRK-FIM-CURSOR3             NOT EQUAL 'S'
               MOVE 'N'                    TO GFCTKC-FIM
               MOVE CSERVC-TARIF           OF GFCTB0C8
                                           TO GFCTKC-PONT-PACOTE
               MOVE CAGPTO-CTA             OF GFCTB0C8
                                           TO GFCTKC-PONT-AGPTO
               MOVE CJUNC-DEPDC-DEB        OF GFCTB0C8
                                           TO GFCTKC-PONT-CH-NUM5
               MOVE CNRO-CTA-DEB           OF GFCTB0C8
                                           TO GFCTKC-PONT-CH-NUM7
               MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C8
                                           TO GFCTKC-PONT-CH-DATA
           END-IF.

           PERFORM 2340-FECHAR-CURSOR3.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ABRIR CURSOR3                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-ABRIR-CURSOR3              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-FIM-CURSOR3.
           MOVE GFCTKB-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB0C8.
           MOVE GFCTKB-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB0C8.
           MOVE GFCTKB-PONT-CH-NUM5    TO CJUNC-DEPDC-DEB   OF GFCTB0C8.
           MOVE GFCTKB-PONT-CH-NUM7    TO CNRO-CTA-DEB      OF GFCTB0C8.

           IF  GFCTKB-PONT-CH-DATA     EQUAL SPACES
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0C8
           ELSE
               MOVE GFCTKB-PONT-CH-DATA TO DINIC-PRMSS-PCOTE OF GFCTB0C8
           END-IF.

           EXEC SQL
               OPEN CSR03-GFCTB0C8
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0C8'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_EMPR  ' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA LEITURA DO CURSOR3                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-LER-CURSOR3                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR03-GFCTB0C8 INTO
                   :GFCTB0C8.CSERVC-TARIF,
                   :GFCTB0C8.CAGPTO-CTA,
                   :GFCTB0C8.DINIC-PRMSS-PCOTE,
                   :GFCTB0C8.CSEQ-AGPTO-CTA,
                   :GFCTB0C8.CJUNC-DEPDC-DEB,
                   :GFCTB0C8.CNRO-CTA-DEB
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0C8'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_EMPR  ' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 NOT EQUAL ZEROS
               MOVE 'S'                TO WRK-FIM-CURSOR3
                                          GFCTKC-FIM
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA PROCESSAR CURSOR3                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2330-PROCESSAR-CURSOR3          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C8
                                       TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE CAGPTO-CTA             OF GFCTB0C8
                                       TO CAGPTO-CTA        OF GFCTB0C6.
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C8
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C8
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C6.

           PERFORM 2131-ACESSAR-GFCTB0C6.

           IF (GFCTKB-IND-DATA         EQUAL 'T')             OR
             ((WRK-DINIC               NOT GREATER WRK-DPROCM AND
               WRK-DFIM                NOT LESS WRK-DPROCM)   OR
              (WRK-DINIC               NOT LESS WRK-DPROCM))
               ADD 1                   TO IND-1
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0C8
                                       TO GFCTKC-DATA-INICIO(IND-1)
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0C8
                                       TO GFCTKC-SEQUENCIA(IND-1)
               MOVE CJUNC-DEPDC-DEB    OF GFCTB0C8
                                       TO GFCTKC-AGENCIA(IND-1)
               MOVE CNRO-CTA-DEB       OF GFCTB0C8
                                       TO GFCTKC-CONTA(IND-1)
               MOVE DFIM-PRMSS-PCOTE   OF GFCTB0C6
                                       TO GFCTKC-DATA-FIM(IND-1)
               MOVE CINDCD-ADSAO-INDVD OF GFCTB0C6
                                       TO GFCTKC-TP-ADESAO(IND-1)

SMS            IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'C'
BI0810             MOVE CPSSOA-SERVC-TARIF OF GFCTB0C6
BI0810                                 TO GFCTKC-CPSSOA-TARIFA(IND-1)
BI0412             MOVE CINDCD-VSLAO-COMP  OF GFCTB0C6
BI0412                                 TO GFCTKC-VISUAL-COMPULS(IND-1)
SMS                IF  CINDCD-EXCEC-ADSAO OF GFCTB0C6  EQUAL 'S'
SMS                   MOVE  1          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
SMS                ELSE
SMS                   MOVE  0          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             END-IF
BI0412         END-IF

BI0412         IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'I'
SMS                MOVE CIDTFD-PRMSS-INDVD OF GFCTB0C6
SMS                                    TO GFCTKC-PRMSS-INDVD(IND-1)
SMS            END-IF

BI0412         IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'A'
BI0412             MOVE CPSSOA-SERVC-TARIF OF GFCTB0C6
BI0412                                 TO GFCTKC-CPSSOA-TARIFA(IND-1)
BI0412             MOVE CIDTFD-PRMSS-INDVD OF GFCTB0C6
BI0412                                 TO GFCTKC-PRMSS-INDVD(IND-1)
BI0412             MOVE CINDCD-VSLAO-COMP  OF GFCTB0C6
BI0412                                 TO GFCTKC-VISUAL-COMPULS(IND-1)
BI0412             IF  CINDCD-EXCEC-ADSAO OF GFCTB0C6  EQUAL 'S'
BI0412                MOVE  1          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             ELSE
BI0412                MOVE  0          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             END-IF
BI0412         END-IF

BI0412         ADD 125                 TO GFCTKC-LL
               ADD 1                   TO GFCTKC-QTDE-TOT-REG
           END-IF.

           PERFORM 2320-LER-CURSOR3.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FECHAR CURSOR3                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2340-FECHAR-CURSOR3             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR03-GFCTB0C8
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0C8'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_DEPDC ' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TRATAR CURSOR4                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-CONS-POSTAL-EXPRESSO       SECTION.
      *----------------------------------------------------------------*

           PERFORM 2410-ABRIR-CURSOR4.

           PERFORM 2420-LER-CURSOR4.

           PERFORM 2430-PROCESSAR-CURSOR4  UNTIL
               IND-1                       NOT LESS 5 OR
               WRK-FIM-CURSOR4             EQUAL 'S'.

           IF  WRK-FIM-CURSOR4             NOT EQUAL 'S'
               MOVE 'N'                    TO GFCTKC-FIM
               MOVE CSERVC-TARIF           OF GFCTB0D1
                                           TO GFCTKC-PONT-PACOTE
               MOVE CAGPTO-CTA             OF GFCTB0D1
                                           TO GFCTKC-PONT-AGPTO
               MOVE CDEPDC                 OF GFCTB0D1
                                           TO GFCTKC-PONT-CH-NUM5
               MOVE CPOSTO-SERVC           OF GFCTB0D1
                                           TO GFCTKC-PONT-CH-NUM7
               MOVE DINIC-PRMSS-PCOTE      OF GFCTB0D1
                                           TO GFCTKC-PONT-CH-DATA
           END-IF.

           PERFORM 2440-FECHAR-CURSOR4.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ABRIR CURSOR4                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-ABRIR-CURSOR4              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-FIM-CURSOR4.
           MOVE GFCTKB-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB0D1.
           MOVE GFCTKB-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB0D1.
           MOVE GFCTKB-PONT-CH-NUM5    TO CDEPDC            OF GFCTB0D1.
           MOVE GFCTKB-PONT-CH-NUM7    TO CPOSTO-SERVC      OF GFCTB0D1.

           IF  GFCTKB-PONT-CH-DATA     EQUAL SPACES
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0D1
           ELSE
               MOVE GFCTKB-PONT-CH-DATA TO DINIC-PRMSS-PCOTE OF GFCTB0D1
           END-IF.

           EXEC SQL
               OPEN CSR04-GFCTB0D1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0D1'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_PSTAL'  TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA LEITURA DO CURSOR4                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2420-LER-CURSOR4                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR04-GFCTB0D1 INTO
                   :GFCTB0D1.CSERVC-TARIF,
                   :GFCTB0D1.CAGPTO-CTA,
                   :GFCTB0D1.DINIC-PRMSS-PCOTE,
                   :GFCTB0D1.CSEQ-AGPTO-CTA,
                   :GFCTB0D1.CDEPDC,
                   :GFCTB0D1.CPOSTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0D1'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_PSTAL'  TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 NOT EQUAL ZEROS
               MOVE 'S'                TO WRK-FIM-CURSOR4
                                          GFCTKC-FIM
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA PROCESSAR CURSOR4                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2430-PROCESSAR-CURSOR4          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0D1
                                       TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE CAGPTO-CTA             OF GFCTB0D1
                                       TO CAGPTO-CTA        OF GFCTB0C6.
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0D1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0D1
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C6.

           PERFORM 2131-ACESSAR-GFCTB0C6.

           IF (GFCTKB-IND-DATA         EQUAL 'T')             OR
             ((WRK-DINIC               NOT GREATER WRK-DPROCM AND
               WRK-DFIM                NOT LESS WRK-DPROCM)   OR
              (WRK-DINIC               NOT LESS WRK-DPROCM))
               ADD 1                   TO IND-1
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0D1
                                       TO GFCTKC-DATA-INICIO(IND-1)
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0D1
                                       TO GFCTKC-SEQUENCIA(IND-1)
               MOVE CDEPDC             OF GFCTB0D1
                                       TO GFCTKC-AGENCIA(IND-1)
               MOVE CPOSTO-SERVC       OF GFCTB0D1
                                       TO GFCTKC-POSTO(IND-1)
               MOVE DFIM-PRMSS-PCOTE   OF GFCTB0C6
                                       TO GFCTKC-DATA-FIM(IND-1)
               MOVE CINDCD-ADSAO-INDVD OF GFCTB0C6
                                       TO GFCTKC-TP-ADESAO(IND-1)

SMS            IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'C'
BI0810             MOVE CPSSOA-SERVC-TARIF OF GFCTB0C6
BI0810                                 TO GFCTKC-CPSSOA-TARIFA(IND-1)
BI0412             MOVE CINDCD-VSLAO-COMP OF GFCTB0C6
BI0412                                 TO GFCTKC-VISUAL-COMPULS(IND-1)
SMS                IF  CINDCD-EXCEC-ADSAO OF GFCTB0C6  EQUAL 'S'
SMS                   MOVE  1          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
SMS                ELSE
SMS                   MOVE  0          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             END-IF
BI0412         END-IF

BI0412         IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'I'
SMS                MOVE CIDTFD-PRMSS-INDVD OF GFCTB0C6
SMS                                    TO GFCTKC-PRMSS-INDVD(IND-1)
SMS            END-IF

BI0412         IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'A'
BI0412             MOVE CPSSOA-SERVC-TARIF OF GFCTB0C6
BI0412                                 TO GFCTKC-CPSSOA-TARIFA(IND-1)
BI0412             MOVE CIDTFD-PRMSS-INDVD OF GFCTB0C6
BI0412                                 TO GFCTKC-PRMSS-INDVD(IND-1)
BI0412             MOVE CINDCD-VSLAO-COMP  OF GFCTB0C6
BI0412                                 TO GFCTKC-VISUAL-COMPULS(IND-1)
BI0412             IF  CINDCD-EXCEC-ADSAO OF GFCTB0C6  EQUAL 'S'
BI0412                MOVE  1          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             ELSE
BI0412                MOVE  0          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             END-IF
BI0412         END-IF

               IF  CINDCD-AGPTO-TOT    OF GFCTB0C6
                                       EQUAL 'S'
                   MOVE 'TODOS'        TO GFCTKC-DESC-NOME(IND-1)
               ELSE
                   PERFORM 2431-OBTER-DESC-POSTO
               END-IF

BI0412         ADD 125                 TO GFCTKC-LL
               ADD 1                   TO GFCTKC-QTDE-TOT-REG
           END-IF.

           PERFORM 2420-LER-CURSOR4.

      *----------------------------------------------------------------*
       2430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2431-OBTER-DESC-POSTO           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                         TO GFCTGB-ENTRADA
                                                  GFCTGC-SAIDA
                                                  GFCT0M-AREA-ERROS.

           INITIALIZE                          GFCTGB-ENTRADA
                                               GFCTGC-SAIDA
                                               GFCT0M-AREA-ERROS.

           MOVE 100                            TO GFCTGB-LL.
           MOVE ZEROS                          TO GFCTGB-ZZ.
           MOVE GFCTKB-TRANSACAO               TO GFCTGB-TRANSACAO.
           MOVE GFCTKB-FUNCAO                  TO GFCTGB-FUNCAO.
           MOVE ZEROS                          TO GFCTGB-QTDE-OCOR
                                                  GFCTGB-QTDE-TOT-REG.
           MOVE GFCTKB-FUNC-BDSCO              TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                            TO GFCTGB-FIM.
           MOVE CDEPDC             OF GFCTB0D1 TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC       OF GFCTB0D1 TO GFCTGB-COD-POSTO.
           MOVE 'GFCT5090'                     TO WRK-MODULO.

           CALL 'POOL0081'                     USING WRK-MODULO
                                                     GFCTGB-ENTRADA
                                                     GFCTGC-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO WRK-MODULO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0126'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO                     NOT EQUAL ZEROS
               MOVE 'S'                        TO GFCTKC-FIM
               MOVE GFCTGC-ERRO                TO GFCTKC-ERRO
               MOVE GFCTGC-COD-SQL-ERRO        TO GFCTKC-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO        TO GFCTKC-COD-MSG-ERRO
               IF  GFCTGC-ERRO                 EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO   TO GFCTKC-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5090-'          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE           INTO GFCTKC-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTGC-DESC-POSTO(1)         TO GFCTKC-DESC-NOME(IND-1).

      *----------------------------------------------------------------*
       2431-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FECHAR CURSOR4                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2440-FECHAR-CURSOR4             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR04-GFCTB0D1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0D1'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_PSTAL'  TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TRATAR CURSOR5                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-CONSULTAR-CLIENTE          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2510-ABRIR-CURSOR5.

           PERFORM 2520-LER-CURSOR5.

           PERFORM 2530-PROCESSAR-CURSOR5  UNTIL
               IND-1                       NOT LESS 5 OR
               WRK-FIM-CURSOR5             EQUAL 'S'.

           IF  WRK-FIM-CURSOR5             NOT EQUAL 'S'
               MOVE 'N'                    TO GFCTKC-FIM
               MOVE CSERVC-TARIF           OF GFCTB0I0
                                           TO GFCTKC-PONT-PACOTE
               MOVE CAGPTO-CTA             OF GFCTB0I0
                                           TO GFCTKC-PONT-AGPTO
ST25X6*        MOVE CCGC-CPF               OF GFCTB0I0
ST25X6*                                    TO GFCTKC-PONT-CH-NUM7
ST25X6         MOVE CCGC-CPF-ST            OF GFCTB0I0
                                           TO GFCTKC-PONT-CH-NUM7
               MOVE DINIC-PRMSS-PCOTE      OF GFCTB0I0
                                           TO GFCTKC-PONT-CH-DATA
           END-IF.

           PERFORM 2540-FECHAR-CURSOR5.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ABRIR CURSOR5                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2510-ABRIR-CURSOR5              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-FIM-CURSOR5.
           MOVE GFCTKB-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB0I0.
           MOVE GFCTKB-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB0I0.
ST25X6*    MOVE GFCTKB-PONT-CH-NUM7    TO CCGC-CPF          OF GFCTB0I0.
ST25X6     MOVE GFCTKB-PONT-CH-NUM7    TO CCGC-CPF-ST       OF GFCTB0I0.

           IF  GFCTKB-PONT-CH-DATA     EQUAL SPACES
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0I0
           ELSE
               MOVE GFCTKB-PONT-CH-DATA TO DINIC-PRMSS-PCOTE OF GFCTB0I0
           END-IF.

           EXEC SQL
               OPEN CSR05-GFCTB0I0
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0I0'      TO WRK-NOME-TAB
               MOVE 'TPRMSS_GRP_CLI'   TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0150'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA LEITURA DO CURSOR5                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2520-LER-CURSOR5                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR05-GFCTB0I0 INTO
                   :GFCTB0I0.CSERVC-TARIF,
                   :GFCTB0I0.CAGPTO-CTA,
                   :GFCTB0I0.DINIC-PRMSS-PCOTE,
                   :GFCTB0I0.CSEQ-AGPTO-CTA,
                   :GFCTB0I0.CCGC-CPF,
                   :GFCTB0I0.CFLIAL-CGC,
                   :GFCTB0I0.CCTRL-CPF-CGC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0I0'      TO WRK-NOME-TAB
               MOVE 'TPRMSS_GRP_CLI'   TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0160'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 NOT EQUAL ZEROS
               MOVE 'S'                TO WRK-FIM-CURSOR5
                                          GFCTKC-FIM
           END-IF.

      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA PROCESSAR CURSOR5                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2530-PROCESSAR-CURSOR5          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0I0
                                       TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE CAGPTO-CTA             OF GFCTB0I0
                                       TO CAGPTO-CTA        OF GFCTB0C6.
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0I0
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0I0
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C6.

           PERFORM 2131-ACESSAR-GFCTB0C6.

           IF (GFCTKB-IND-DATA         EQUAL 'T')             OR
             ((WRK-DINIC               NOT GREATER WRK-DPROCM AND
               WRK-DFIM                NOT LESS WRK-DPROCM)   OR
              (WRK-DINIC               NOT LESS WRK-DPROCM))
               ADD 1                   TO IND-1
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0I0
                                       TO GFCTKC-DATA-INICIO(IND-1)
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0I0
                                       TO GFCTKC-SEQUENCIA(IND-1)
ST25X6*        MOVE CCGC-CPF           OF GFCTB0I0
ST25X6*                                TO GFCTKC-CPF-CNPJ(IND-1)
ST25X6        MOVE CCGC-CPF-ST        OF GFCTB0I0
ST25X6                                TO GFCTKC-CPF-CNPJ(IND-1)
ST25X6*        MOVE CFLIAL-CGC         OF GFCTB0I0
ST25X6*                                TO GFCTKC-FILIAL(IND-1)
ST25X6        MOVE CFLIAL-CGC-ST      OF GFCTB0I0
ST25X6                               TO GFCTKC-FILIAL(IND-1)
ST25X6*        MOVE CCTRL-CPF-CGC      OF GFCTB0I0
ST25X6*                                TO GFCTKC-CONTROLE(IND-1)
ST25X6        MOVE CCTRL-CPF-CGC-ST   OF GFCTB0I0
ST25X6                               TO GFCTKC-CONTROLE(IND-1)
               MOVE DFIM-PRMSS-PCOTE   OF GFCTB0C6
                                       TO GFCTKC-DATA-FIM(IND-1)
               MOVE CINDCD-ADSAO-INDVD OF GFCTB0C6
                                       TO GFCTKC-TP-ADESAO(IND-1)

SMS            IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'C'
BI0810             MOVE CPSSOA-SERVC-TARIF OF GFCTB0C6
BI0810                                 TO GFCTKC-CPSSOA-TARIFA(IND-1)
BI0412             MOVE CINDCD-VSLAO-COMP OF GFCTB0C6
BI0412                                 TO GFCTKC-VISUAL-COMPULS(IND-1)
SMS                IF  CINDCD-EXCEC-ADSAO OF GFCTB0C6  EQUAL 'S'
SMS                   MOVE  1          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
SMS                ELSE
SMS                   MOVE  0          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             END-IF
BI0412         END-IF

BI0412         IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'I'
SMS                MOVE CIDTFD-PRMSS-INDVD OF GFCTB0C6
SMS                                    TO GFCTKC-PRMSS-INDVD(IND-1)
SMS            END-IF

BI0412         IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'A'
BI0412             MOVE CPSSOA-SERVC-TARIF OF GFCTB0C6
BI0412                                 TO GFCTKC-CPSSOA-TARIFA(IND-1)
BI0412             MOVE CIDTFD-PRMSS-INDVD OF GFCTB0C6
BI0412                                 TO GFCTKC-PRMSS-INDVD(IND-1)
BI0412             MOVE CINDCD-VSLAO-COMP  OF GFCTB0C6
BI0412                                 TO GFCTKC-VISUAL-COMPULS(IND-1)
BI0412             IF  CINDCD-EXCEC-ADSAO OF GFCTB0C6  EQUAL 'S'
BI0412                MOVE  1          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             ELSE
BI0412                MOVE  0          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             END-IF
BI0412         END-IF

BI0412         ADD 125                 TO GFCTKC-LL
               ADD 1                   TO GFCTKC-QTDE-TOT-REG
           END-IF.

           PERFORM 2520-LER-CURSOR5.

      *----------------------------------------------------------------*
       2530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      ******************************************************************
      *ROTINA PARA FECHAR CURSOR5                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2540-FECHAR-CURSOR5             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR05-GFCTB0I0
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0I0'      TO WRK-NOME-TAB
               MOVE 'TPRMSS_GRP_CLI'   TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0170'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.
      *
      *----------------------------------------------------------------*
       2540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TRATAR CURSOR6                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-CONSULTAR-SEGMENTO         SECTION.
      *----------------------------------------------------------------*

           PERFORM 2610-ABRIR-CURSOR6.

           PERFORM 2620-LER-CURSOR6.

           PERFORM 2630-PROCESSAR-CURSOR6  UNTIL
               IND-1                       NOT LESS 5 OR
               WRK-FIM-CURSOR6             EQUAL 'S'.

           IF  WRK-FIM-CURSOR6             NOT EQUAL 'S'
               MOVE 'N'                    TO GFCTKC-FIM
               MOVE CSERVC-TARIF           OF GFCTB0D2
                                           TO GFCTKC-PONT-PACOTE
               MOVE CAGPTO-CTA             OF GFCTB0D2
                                           TO GFCTKC-PONT-AGPTO
               MOVE CSGMTO-GSTAO-TARIF     OF GFCTB0D2
                                           TO GFCTKC-PONT-CH-NUM5
               MOVE DINIC-PRMSS-PCOTE      OF GFCTB0D2
                                           TO GFCTKC-PONT-CH-DATA
           END-IF.

           PERFORM 2640-FECHAR-CURSOR6.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ABRIR CURSOR6                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2610-ABRIR-CURSOR6              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-FIM-CURSOR6.
           MOVE GFCTKB-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB0D2.
           MOVE GFCTKB-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB0D2.
           MOVE GFCTKB-PONT-CH-NUM5   TO CSGMTO-GSTAO-TARIF OF GFCTB0D2.

           IF  GFCTKB-PONT-CH-DATA     EQUAL SPACES
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0D2
           ELSE
               MOVE GFCTKB-PONT-CH-DATA TO DINIC-PRMSS-PCOTE OF GFCTB0D2
           END-IF.

           EXEC SQL
               OPEN CSR06-GFCTB0D2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0D2'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_SGMTO'  TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0180'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA LEITURA DO CURSOR6                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2620-LER-CURSOR6                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR06-GFCTB0D2 INTO
                   :GFCTB0D2.CSERVC-TARIF,
                   :GFCTB0D2.CAGPTO-CTA,
                   :GFCTB0D2.DINIC-PRMSS-PCOTE,
                   :GFCTB0D2.CSEQ-AGPTO-CTA,
                   :GFCTB0D2.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0D2'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_SGMTO'  TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0190'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 NOT EQUAL ZEROS
               MOVE 'S'                TO WRK-FIM-CURSOR6
                                          GFCTKC-FIM
           END-IF.

      *----------------------------------------------------------------*
       2620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA PROCESSAR CURSOR6                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2630-PROCESSAR-CURSOR6          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0D2
                                       TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE CAGPTO-CTA             OF GFCTB0D2
                                       TO CAGPTO-CTA        OF GFCTB0C6.
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0D2
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0D2
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C6.

           PERFORM 2131-ACESSAR-GFCTB0C6.

           IF (GFCTKB-IND-DATA         EQUAL 'T')             OR
             ((WRK-DINIC               NOT GREATER WRK-DPROCM AND
               WRK-DFIM                NOT LESS WRK-DPROCM)   OR
              (WRK-DINIC               NOT LESS WRK-DPROCM))
               ADD 1                   TO IND-1
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0D2
                                       TO GFCTKC-DATA-INICIO(IND-1)
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0D2
                                       TO GFCTKC-SEQUENCIA(IND-1)
               MOVE CSGMTO-GSTAO-TARIF OF GFCTB0D2
                                       TO GFCTKC-SEGMENTO(IND-1)
               MOVE DFIM-PRMSS-PCOTE   OF GFCTB0C6
                                       TO GFCTKC-DATA-FIM(IND-1)
               MOVE CINDCD-ADSAO-INDVD OF GFCTB0C6
                                       TO GFCTKC-TP-ADESAO(IND-1)

SMS            IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'C'
BI0810             MOVE CPSSOA-SERVC-TARIF OF GFCTB0C6
BI0810                                 TO GFCTKC-CPSSOA-TARIFA(IND-1)
BI0412             MOVE CINDCD-VSLAO-COMP OF GFCTB0C6
BI0412                                 TO GFCTKC-VISUAL-COMPULS(IND-1)
SMS                IF  CINDCD-EXCEC-ADSAO OF GFCTB0C6  EQUAL 'S'
SMS                   MOVE  1          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
SMS                ELSE
SMS                   MOVE  0          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             END-IF
BI0412         END-IF

BI0412         IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'I'
SMS                MOVE CIDTFD-PRMSS-INDVD OF GFCTB0C6
SMS                                    TO GFCTKC-PRMSS-INDVD(IND-1)
SMS            END-IF

BI0412         IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'A'
BI0412             MOVE CPSSOA-SERVC-TARIF OF GFCTB0C6
BI0412                                 TO GFCTKC-CPSSOA-TARIFA(IND-1)
BI0412             MOVE CIDTFD-PRMSS-INDVD OF GFCTB0C6
BI0412                                 TO GFCTKC-PRMSS-INDVD(IND-1)
BI0412             MOVE CINDCD-VSLAO-COMP  OF GFCTB0C6
BI0412                                 TO GFCTKC-VISUAL-COMPULS(IND-1)
BI0412             IF  CINDCD-EXCEC-ADSAO OF GFCTB0C6  EQUAL 'S'
BI0412                MOVE  1          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             ELSE
BI0412                MOVE  0          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             END-IF
BI0412         END-IF

               PERFORM 2631-OBTER-DESC-SEGMENTO

BI0412         ADD 125                 TO GFCTKC-LL
               ADD 1                   TO GFCTKC-QTDE-TOT-REG
           END-IF.

           PERFORM 2620-LER-CURSOR6.

      *----------------------------------------------------------------*
       2630-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2631-OBTER-DESC-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-GSTAO-TARIF OF GFCTB0D2 TO CSGMTO-GSTAO-TARIF
                                       OF GFCTB0H5.

           EXEC SQL
             SELECT
                   ISGMTO_GSTAO_TARIF
             INTO  :GFCTB0H5.ISGMTO-GSTAO-TARIF
             FROM  DB2PRD.TTPO_SGMTO_GSTAO
             WHERE CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0H5'      TO WRK-NOME-TAB
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0200'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE ISGMTO-GSTAO-TARIF     OF GFCTB0H5
                                       TO GFCTKC-DESC-NOME(IND-1).

      *----------------------------------------------------------------*
       2631-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FECHAR CURSOR6                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2640-FECHAR-CURSOR6             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR06-GFCTB0D2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0D3'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_SGMTO'  TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0210'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2640-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TRATAR CURSOR7                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2700-CONSULTAR-MUNICIPIO        SECTION.
      *----------------------------------------------------------------*

           PERFORM 2710-ABRIR-CURSOR7.

           PERFORM 2720-LER-CURSOR7.

           PERFORM 2730-PROCESSAR-CURSOR7  UNTIL
               IND-1                       NOT LESS 5 OR
               WRK-FIM-CURSOR7             EQUAL 'S'.

           IF  WRK-FIM-CURSOR7             NOT EQUAL 'S'
               MOVE 'N'                    TO GFCTKC-FIM
               MOVE CSERVC-TARIF           OF GFCTB0C9
                                           TO GFCTKC-PONT-PACOTE
               MOVE CAGPTO-CTA             OF GFCTB0C9
                                           TO GFCTKC-PONT-AGPTO
               MOVE CMUN-IBGE              OF GFCTB0C9
                                           TO GFCTKC-PONT-CH-NUM5
               MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C9
                                           TO GFCTKC-PONT-CH-DATA
           END-IF.

           PERFORM 2740-FECHAR-CURSOR7.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ABRIR CURSOR7                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2710-ABRIR-CURSOR7              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-FIM-CURSOR7.
           MOVE GFCTKB-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB0C9.
           MOVE GFCTKB-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB0C9.
           MOVE GFCTKB-PONT-CH-NUM5    TO CMUN-IBGE         OF GFCTB0C9.

           IF  GFCTKB-PONT-CH-DATA     EQUAL SPACES
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0C9
           ELSE
               MOVE GFCTKB-PONT-CH-DATA TO DINIC-PRMSS-PCOTE OF GFCTB0C9
           END-IF.

           EXEC SQL
               OPEN CSR07-GFCTB0C9
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0C9'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_CMUN'   TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0220'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA LEITURA DO CURSOR7                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2720-LER-CURSOR7                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR07-GFCTB0C9 INTO
                   :GFCTB0C9.CSERVC-TARIF,
                   :GFCTB0C9.CAGPTO-CTA,
                   :GFCTB0C9.DINIC-PRMSS-PCOTE,
                   :GFCTB0C9.CSEQ-AGPTO-CTA,
                   :GFCTB0C9.CMUN-IBGE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0C9'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_CMUN'   TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0230'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 NOT EQUAL ZEROS
               MOVE 'S'                TO WRK-FIM-CURSOR7
                                          GFCTKC-FIM
           END-IF.

      *----------------------------------------------------------------*
       2720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA PROCESSAR CURSOR7                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2730-PROCESSAR-CURSOR7          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0C9
                                       TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE CAGPTO-CTA             OF GFCTB0C9
                                       TO CAGPTO-CTA        OF GFCTB0C6.
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C9
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C9
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C6.

           PERFORM 2131-ACESSAR-GFCTB0C6.

           IF (GFCTKB-IND-DATA         EQUAL 'T')             OR
             ((WRK-DINIC               NOT GREATER WRK-DPROCM AND
               WRK-DFIM                NOT LESS WRK-DPROCM)   OR
              (WRK-DINIC               NOT LESS WRK-DPROCM))
               ADD 1                   TO IND-1
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0C9
                                       TO GFCTKC-DATA-INICIO(IND-1)
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0C9
                                       TO GFCTKC-SEQUENCIA(IND-1)
               MOVE CMUN-IBGE          OF GFCTB0C9
                                       TO GFCTKC-MUNICIPIO(IND-1)
               MOVE DFIM-PRMSS-PCOTE   OF GFCTB0C6
                                       TO GFCTKC-DATA-FIM(IND-1)
               MOVE CINDCD-ADSAO-INDVD OF GFCTB0C6
                                       TO GFCTKC-TP-ADESAO(IND-1)

SMS            IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'C'
BI0810             MOVE CPSSOA-SERVC-TARIF OF GFCTB0C6
BI0810                                 TO GFCTKC-CPSSOA-TARIFA(IND-1)
BI0412             MOVE CINDCD-VSLAO-COMP OF GFCTB0C6
BI0412                                 TO GFCTKC-VISUAL-COMPULS(IND-1)
SMS                IF  CINDCD-EXCEC-ADSAO OF GFCTB0C6  EQUAL 'S'
SMS                   MOVE  1          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
SMS                ELSE
SMS                   MOVE  0          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             END-IF
BI0412         END-IF

BI0412         IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'I'
SMS                MOVE CIDTFD-PRMSS-INDVD OF GFCTB0C6
SMS                                    TO GFCTKC-PRMSS-INDVD(IND-1)
SMS            END-IF

BI0412         IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'A'
BI0412             MOVE CPSSOA-SERVC-TARIF OF GFCTB0C6
BI0412                                 TO GFCTKC-CPSSOA-TARIFA(IND-1)
BI0412             MOVE CIDTFD-PRMSS-INDVD OF GFCTB0C6
BI0412                                 TO GFCTKC-PRMSS-INDVD(IND-1)
BI0412             MOVE CINDCD-VSLAO-COMP  OF GFCTB0C6
BI0412                                 TO GFCTKC-VISUAL-COMPULS(IND-1)
BI0412             IF  CINDCD-EXCEC-ADSAO OF GFCTB0C6  EQUAL 'S'
BI0412                MOVE  1          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             ELSE
BI0412                MOVE  0          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             END-IF
BI0412         END-IF

               PERFORM 2731-OBTER-DESC-MUNICIPIO

BI0412         ADD 125                 TO GFCTKC-LL
               ADD 1                   TO GFCTKC-QTDE-TOT-REG
           END-IF.

           PERFORM 2720-LER-CURSOR7.

      *----------------------------------------------------------------*
       2730-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO MUNICIPIO                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2731-OBTER-DESC-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO AREA-RURC88.

           INITIALIZE AREA-RURC88.

           MOVE CMUN-IBGE OF GFCTB0C9  TO COD-MUNIC-RURC88.
           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTKC-ERRO
               MOVE 'S'                TO GFCTKC-FIM
               MOVE ZEROS              TO GFCTKC-COD-SQL-ERRO
               MOVE 0732               TO GFCTKC-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTKC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 10
               MOVE 1                  TO GFCTKC-ERRO
               MOVE 'S'                TO GFCTKC-FIM
               MOVE ZEROS              TO GFCTKC-COD-SQL-ERRO
               MOVE 0675               TO GFCTKC-COD-MSG-ERRO
               MOVE '0675'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTKC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 20
               MOVE 1                  TO GFCTKC-ERRO
               MOVE 'S'                TO GFCTKC-FIM
               MOVE ZEROS              TO GFCTKC-COD-SQL-ERRO
               MOVE 0733               TO GFCTKC-COD-MSG-ERRO
               MOVE '0733'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTKC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 99
               MOVE 1                  TO GFCTKC-ERRO
               MOVE 'S'                TO GFCTKC-FIM
               MOVE ZEROS              TO GFCTKC-COD-SQL-ERRO
               MOVE 0734               TO GFCTKC-COD-MSG-ERRO
               MOVE '0734'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTKC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE NOME-MUNIC-RURC88      TO GFCTKC-DESC-NOME(IND-1).

      *----------------------------------------------------------------*
       2731-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FECHAR CURSOR7                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2740-FECHAR-CURSOR7             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR07-GFCTB0C9
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0C9'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_CMUN'   TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0240'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2740-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TRATAR CURSOR8                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2800-CONSULTAR-UF               SECTION.
      *----------------------------------------------------------------*

           PERFORM 2810-ABRIR-CURSOR8.

           PERFORM 2820-LER-CURSOR8.

           PERFORM 2830-PROCESSAR-CURSOR8  UNTIL
               IND-1                       NOT LESS 5 OR
               WRK-FIM-CURSOR8             EQUAL 'S'.

           IF  WRK-FIM-CURSOR8             NOT EQUAL 'S'
               MOVE 'N'                    TO GFCTKC-FIM
               MOVE CSERVC-TARIF           OF GFCTB0D3
                                           TO GFCTKC-PONT-PACOTE
               MOVE CAGPTO-CTA             OF GFCTB0D3
                                           TO GFCTKC-PONT-AGPTO
               MOVE CSGL-UF                OF GFCTB0D3
                                           TO GFCTKC-PONT-CH-ALP
               MOVE DINIC-PRMSS-PCOTE      OF GFCTB0D3
                                           TO GFCTKC-PONT-CH-DATA
           END-IF.

           PERFORM 2840-FECHAR-CURSOR8.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ABRIR CURSOR8                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2810-ABRIR-CURSOR8              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-FIM-CURSOR7.
           MOVE GFCTKB-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB0D3.
           MOVE GFCTKB-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB0D3.
           MOVE GFCTKB-PONT-CH-ALP     TO CSGL-UF           OF GFCTB0D3.

           IF  GFCTKB-PONT-CH-DATA     EQUAL SPACES
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0D3
           ELSE
               MOVE GFCTKB-PONT-CH-DATA TO DINIC-PRMSS-PCOTE OF GFCTB0D3
           END-IF.

           EXEC SQL
               OPEN CSR08-GFCTB0D3
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0D3'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_UF'     TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0250'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA LEITURA DO CURSOR8                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2820-LER-CURSOR8                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR08-GFCTB0D3 INTO
                   :GFCTB0D3.CSERVC-TARIF,
                   :GFCTB0D3.CAGPTO-CTA,
                   :GFCTB0D3.DINIC-PRMSS-PCOTE,
                   :GFCTB0D3.CSEQ-AGPTO-CTA,
                   :GFCTB0D3.CSGL-UF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0D3'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_UF'     TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0260'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 NOT EQUAL ZEROS
               MOVE 'S'                TO WRK-FIM-CURSOR8
                                          GFCTKC-FIM
           END-IF.

      *----------------------------------------------------------------*
       2820-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA PROCESSAR CURSOR8                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2830-PROCESSAR-CURSOR8          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0D3
                                       TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE CAGPTO-CTA             OF GFCTB0D3
                                       TO CAGPTO-CTA        OF GFCTB0C6.
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0D3
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0D3
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C6.

           PERFORM 2131-ACESSAR-GFCTB0C6.

           IF (GFCTKB-IND-DATA         EQUAL 'T')             OR
             ((WRK-DINIC               NOT GREATER WRK-DPROCM AND
               WRK-DFIM                NOT LESS WRK-DPROCM)   OR
              (WRK-DINIC               NOT LESS WRK-DPROCM))
               ADD 1                   TO IND-1
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0D3
                                       TO GFCTKC-DATA-INICIO(IND-1)
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0D3
                                       TO GFCTKC-SEQUENCIA(IND-1)
               MOVE CSGL-UF            OF GFCTB0D3
                                       TO GFCTKC-UF(IND-1)
               MOVE DFIM-PRMSS-PCOTE   OF GFCTB0C6
                                       TO GFCTKC-DATA-FIM(IND-1)
               MOVE CINDCD-ADSAO-INDVD OF GFCTB0C6
                                       TO GFCTKC-TP-ADESAO(IND-1)

SMS            IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'C'
BI0810             MOVE CPSSOA-SERVC-TARIF OF GFCTB0C6
BI0810                                 TO GFCTKC-CPSSOA-TARIFA(IND-1)
BI0412             MOVE CINDCD-VSLAO-COMP OF GFCTB0C6
BI0412                                 TO GFCTKC-VISUAL-COMPULS(IND-1)
SMS                IF  CINDCD-EXCEC-ADSAO OF GFCTB0C6  EQUAL 'S'
SMS                   MOVE  1          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
SMS                ELSE
SMS                   MOVE  0          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             END-IF
BI0412         END-IF

BI0412         IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'I'
SMS                MOVE CIDTFD-PRMSS-INDVD OF GFCTB0C6
SMS                                    TO GFCTKC-PRMSS-INDVD(IND-1)
SMS            END-IF

BI0412         IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'A'
BI0412             MOVE CPSSOA-SERVC-TARIF OF GFCTB0C6
BI0412                                 TO GFCTKC-CPSSOA-TARIFA(IND-1)
BI0412             MOVE CIDTFD-PRMSS-INDVD OF GFCTB0C6
BI0412                                 TO GFCTKC-PRMSS-INDVD(IND-1)
BI0412             MOVE CINDCD-VSLAO-COMP  OF GFCTB0C6
BI0412                                 TO GFCTKC-VISUAL-COMPULS(IND-1)
BI0412             IF  CINDCD-EXCEC-ADSAO OF GFCTB0C6  EQUAL 'S'
BI0412                MOVE  1          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             ELSE
BI0412                MOVE  0          TO GFCTKC-SOLICITA-EXCECAO(IND-1)
BI0412             END-IF
BI0412         END-IF

               PERFORM 2831-OBTER-DESC-UF

BI0412         ADD 125                 TO GFCTKC-LL
               ADD 1                   TO GFCTKC-QTDE-TOT-REG
           END-IF.

           PERFORM 2820-LER-CURSOR8.

      *----------------------------------------------------------------*
       2830-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO UF                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2831-OBTER-DESC-UF              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE AREA-RURC88.

           MOVE CSGL-UF   OF GFCTB0D3  TO CSGL-UF OF DCITV002.
           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTKC-ERRO
               MOVE 'S'                TO GFCTKC-FIM
               MOVE ZEROS              TO GFCTKC-COD-SQL-ERRO
               MOVE 0735               TO GFCTKC-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTKC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 1
               MOVE 1                  TO GFCTKC-ERRO
               MOVE 'S'                TO GFCTKC-FIM
               MOVE ZEROS              TO GFCTKC-COD-SQL-ERRO
               MOVE 0736               TO GFCTKC-COD-MSG-ERRO
               MOVE '0736'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTKC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 2
               MOVE 1                  TO GFCTKC-ERRO
               MOVE 'S'                TO GFCTKC-FIM
               MOVE ZEROS              TO GFCTKC-COD-SQL-ERRO
               MOVE 0737               TO GFCTKC-COD-MSG-ERRO
               MOVE '0737'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTKC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 3
               MOVE 1                  TO GFCTKC-ERRO
               MOVE 'S'                TO GFCTKC-FIM
               MOVE ZEROS              TO GFCTKC-COD-SQL-ERRO
               MOVE 0738               TO GFCTKC-COD-MSG-ERRO
               MOVE '0738'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTKC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 4
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - DCITV002'      TO WRK-NOME-TAB
               MOVE 'V01_UF'           TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0270'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTKC-COD-MSG-ERRO
               MOVE  9                 TO GFCTKC-ERRO
               MOVE 'GFCT0126'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE  WRK-SQLCA         TO SQLCA
               MOVE  WRK-SQLCA         TO GFCT0M-SQLCA-AREA
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTKC-COD-SQL-ERRO
               MOVE SPACES             TO GFCTKC-DESC-MSG-ERRO
               STRING GFCTG3-DESC-MSG  WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTKC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE WRK-AREA-DCLGEN-DCITV002 TO DCITV002.
           MOVE IUF OF DCITV002        TO GFCTKC-DESC-NOME(IND-1).

      *----------------------------------------------------------------*
       2831-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FECHAR CURSOR8                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2840-FECHAR-CURSOR8             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR08-GFCTB0D3
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0D3'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_UF'     TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0280'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTKC-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2840-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FINALIZAR PROGRAMA                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

