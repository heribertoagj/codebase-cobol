      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5136.
       AUTHOR.     LEANDRO MENDES.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5136                                    *
      *    PROGRAMADOR.:   LEANDRO MENDES          - CPM PATO BRANCO   *
      *    ANALISTA CPM:   KHARUZO INOCENCIO LEITE - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO CREM            - PROCWORK /GP.50   *
      *    DATA........:   27/03/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   COMBO - LISTA DE PERMISSAO COMPULSORIA.     *
      *      LISTAR ATE 10 OCORRENCIAS NA SAIDA E MONTAR RESTART, CASO *
      *      HAJA, COM A CHAVE DA PROXIMA OCORRENCIA  VALIDA DO CURSOR *
      *      (DO AGRUPAMENTO).                                         *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.PRMSS_ADSAO_PCOTE      GFCTB0C6       *
      *                   DB2PRD.PARM_SERVC_TARIF       GFCTB0A2       *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *                   DB2PRD.PRMSS_GRP_DEPDC        GFCTB0C7       *
      *                   DB2PRD.PRMSS_GRP_EMPR         GFCTB0C8       *
      *                   DB2PRD.PRMSS_GRP_MUN          GFCTB0C9       *
      *                   DB2PRD.PRMSS_GRP_PAB          GFCTB0D0       *
      *                   DB2PRD.PRMSS_GRP_PST          GFCTB0D1       *
      *                   DB2PRD.PRMSS_GRP_SGMTO        GFCTB0D2       *
      *                   DB2PRD.PRMSS_GRP_UF           GFCTB0D3       *
      *                   DB2PRD.TPRMSS_GRP_CLI         GFCTB0I0       *
      *                   DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTN7 - ENTRADA - AREA DE COMUNICACAO.                   *
      *    I#GFCTN8 - SAIDA   - AREA DE COMUNICACAO.                   *
      *    I#GFCTFZ - ENTRADA - AREA DE COMUNICACAO GFCT5522.          *
      *    I#GFCTG1 - SAIDA   - AREA DE COMUNICACAO GFCT5522.          *
      *    I#GFCTG2 - ENTRADA - AREA DE COMUNICACAO GFCT5523.          *
      *    I#GFCTG3 - SAIDA   - AREA DE COMUNICACAO GFCT5523.          *
      *    I#GFCT0M - ERROS   - AREA DE COMUNICACAO.                   *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    CBLTDLI  - OBTER E ENVIAR MENSAGENS PARA IMS                *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS                     *
      *    GFCT5522 - VERIFICA ON-LINE                                 *
      *    POOL0025 - SOMAR/SUBTRAIR NUMERO DE DIAS DE UMA DATA        *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                   *
      *    POOL7600 - OBTER DATA E HORA DO SISTEMA                     *
      *================================================================*
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

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *----------------------------------------------------------------*

BI0810 77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(011)         VALUE SPACES.
       77  WRK-FIM                     PIC  X(001)         VALUE SPACES.
       77  WRK-LISTA                   PIC  X(001)         VALUE SPACES.
       77  WRK-ACHOU-DUPLICIDADE       PIC  X(001)         VALUE SPACES.
       77  WRK-SEQUENCIA               PIC  9(009)         VALUE ZEROS.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.

BI0412 01  WRK-CINDCD-ADSAO-INDVD      PIC  X(001)         VALUE 'C'.
BI0412 01  WRK-CINDCD-ADSAO-AMBAS      PIC  X(001)         VALUE 'A'.

       01  WRK-PESSOA-1                PIC  X(001)         VALUE SPACES.
       01  WRK-PESSOA-2                PIC  X(001)         VALUE SPACES.
       01  WRK-PESSOA-3                PIC  X(001)         VALUE SPACES.

       01  W-NUMERO.
           05 W-DE9                    PIC  9(009)         VALUE ZEROS.
           05 W-DE4                    PIC  X(004)         VALUE ZEROS.
       01  W-NUM13 REDEFINES W-NUMERO  PIC  9(13).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA AUXILIARES DE DATA *'.
      *----------------------------------------------------------------*

       01  WRK-DATA-AUX.
           05  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-AUX1.
           05  WRK-ANO-AUX1            PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-AUX1            PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-AUX1            PIC  9(002)         VALUE ZEROS.

       01  WRK-AAMM-ATUA.
           05  WRK-AA-ATUA             PIC  9(004)         VALUE ZEROS.
           05  WRK-MM-ATUA             PIC  9(002)         VALUE ZEROS.

       01  WRK-AAMM-PROX.
           05  WRK-AA-PROX             PIC  9(004)         VALUE ZEROS.
           05  WRK-MM-PROX             PIC  9(002)         VALUE ZEROS.

       01  WRK-ANO-COMP                PIC  9(004) COMP-3  VALUE ZEROS.
       01  WRK-MES-COMP                PIC  9(002) COMP-3  VALUE ZEROS.

       01  WRK-DATA-NUM                PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-FIM                PIC  9(008)         VALUE ZEROS.
       01  WRK-DINIC                   PIC  9(008)         VALUE ZEROS.
       01  WRK-DFIM                    PIC  9(008)         VALUE ZEROS.
       01  WRK-DPROCM                  PIC  9(008)         VALUE ZEROS.
       01  WRK-INI-DPROCM              PIC  9(008)         VALUE ZEROS.
       01  WRK-INI-PERMISSAO           PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-DE-INICIO          PIC  X(010)         VALUE ZEROS.
       01  WRK-DATA-DE-FIM             PIC  X(010)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA POOL0025 *'.
      *----------------------------------------------------------------*

       01  WRK-DATA-ABERT              PIC  9(009) COMP-3  VALUE ZEROS.
       01  WRK-PRAZO                   PIC S9(005) COMP-3  VALUE ZEROS.
       01  WRK-DATA-VENC               PIC  9(009) COMP-3  VALUE ZEROS.

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
           '* AREA TO TABELA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C6
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C7
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C9
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D3
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0I0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A2
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA TO CURSORES *'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-GFCTB0C7 CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CEMPR_INC,
                   CDEPDC
             FROM  DB2PRD.PRMSS_GRP_DEPDC
             WHERE
                   CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA         AND
                   CDEPDC             = :GFCTB0C7.CDEPDC             AND
                   CSERVC_TARIF      >= :GFCTB0C7.CSERVC-TARIF
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR02-GFCTB0D0 CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             FROM  DB2PRD.PRMSS_GRP_PAB
             WHERE
                   CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA         AND
                   CDEPDC             = :GFCTB0D0.CDEPDC             AND
                   CPOSTO_SERVC       = :GFCTB0D0.CPOSTO-SERVC       AND
                   CSERVC_TARIF      >= :GFCTB0D0.CSERVC-TARIF
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR03-GFCTB0D1 CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             FROM  DB2PRD.PRMSS_GRP_PSTAL
             WHERE
                   CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA         AND
                   CDEPDC             = :GFCTB0D1.CDEPDC             AND
                   CPOSTO_SERVC       = :GFCTB0D1.CPOSTO-SERVC       AND
                   CSERVC_TARIF      >= :GFCTB0D1.CSERVC-TARIF
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR04-GFCTB0C9 CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CMUN_IBGE
             FROM  DB2PRD.PRMSS_GRP_MUN
             WHERE
                   CAGPTO_CTA         = :GFCTB0C9.CAGPTO-CTA     AND
                   CMUN_IBGE          = :GFCTB0C9.CMUN-IBGE      AND
                   CSERVC_TARIF      >= :GFCTB0C9.CSERVC-TARIF
             ORDER BY CSERVC_TARIF
           END-EXEC.


           EXEC SQL
             DECLARE CSR05-GFCTB0D3 CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CSGL_UF
             FROM  DB2PRD.PRMSS_GRP_UF
             WHERE
                   CAGPTO_CTA         = :GFCTB0D3.CAGPTO-CTA         AND
                   CSGL_UF            = :GFCTB0D3.CSGL-UF            AND
                   CSERVC_TARIF      >= :GFCTB0D3.CSERVC-TARIF
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR06-GFCTB0C8 CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CJUNC_DEPDC_DEB,
                   CRZ_CTA_DEB,
                   CNRO_CTA_DEB,
                   CLCTO_CTA_DEB
             FROM  DB2PRD.PRMSS_GRP_EMPR
             WHERE
                   CAGPTO_CTA         = :GFCTB0C8.CAGPTO-CTA         AND
                   CNRO_CTA_DEB       = :GFCTB0C8.CNRO-CTA-DEB       AND
                   CJUNC_DEPDC_DEB    = :GFCTB0C8.CJUNC-DEPDC-DEB    AND
                   CSERVC_TARIF      >= :GFCTB0C8.CSERVC-TARIF
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR07-GFCTB0D2 CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CSGMTO_GSTAO_TARIF
             FROM  DB2PRD.PRMSS_GRP_SGMTO
             WHERE
                   CAGPTO_CTA         = :GFCTB0D2.CAGPTO-CTA         AND
                   CSGMTO_GSTAO_TARIF = :GFCTB0D2.CSGMTO-GSTAO-TARIF AND
                   CSERVC_TARIF      >= :GFCTB0D2.CSERVC-TARIF
             ORDER BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CSR08-GFCTB0I0 CURSOR   FOR
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
             WHERE
                   CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA         AND
ST25X6*               CCGC_CPF           = :GFCTB0I0.CCGC-CPF        AND
ST25X6               CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST      AND
                   CSERVC_TARIF      >= :GFCTB0I0.CSERVC-TARIF
             ORDER BY CSERVC_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTN7'.

       COPY 'I#GFCTN8'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTN7-ENTRADA
                                                      GFCTN8-SAIDA
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
                                          GFCTN8-SAIDA.

           INITIALIZE GFCTN8-SAIDA
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

           MOVE 'GFCT5136'             TO GFCTFZ-SISTEMA.
           MOVE GFCTN7-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTN7-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTN7-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTN8-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5136'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                 EQUAL 9
               MOVE 9                      TO GFCTN8-ERRO
               MOVE 'GFCT5136'             TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO    TO GFCT0M-SQLCODE
                                              GFCTN8-COD-SQL-ERRO
               MOVE GFCTG1-ERRO            TO GFCTN8-ERRO
               MOVE GFCTG1-COD-MSG-ERRO    TO GFCTN8-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO   TO GFCT0M-TEXTO
                                              GFCTN8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE          EQUAL 1
               MOVE 9                      TO GFCTN8-ERRO
               MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5136'             TO GFCT0M-TRANSACAO
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

           MOVE 360                        TO GFCTN8-LL.
           MOVE ZEROS                      TO GFCTN8-ZZ.
           MOVE GFCTN7-TRANSACAO           TO GFCTN8-TRANSACAO.
           MOVE GFCTN7-FUNCAO              TO GFCTN8-FUNCAO.

           IF  GFCTN7-QTDE-OCOR            IS NUMERIC
               MOVE GFCTN7-QTDE-OCOR       TO GFCTN8-QTDE-OCOR
           ELSE
               MOVE ZEROS                  TO GFCTN8-QTDE-OCOR
           END-IF.

           IF  GFCTN7-QTDE-TOT-REG         IS NUMERIC
               MOVE GFCTN7-QTDE-TOT-REG    TO GFCTN8-QTDE-TOT-REG
           ELSE
               MOVE ZEROS                  TO GFCTN8-QTDE-TOT-REG
           END-IF.

           MOVE GFCTN7-FUNC-BDSCO          TO GFCTN8-FUNC-BDSCO.
           MOVE GFCTN7-PONT-AGPTO          TO GFCTN8-PONT-AGPTO.
           MOVE GFCTN7-PONT-PACOTE         TO GFCTN8-PONT-PACOTE.
           MOVE GFCTN7-PONT-CH-NUM5        TO GFCTN8-PONT-CH-NUM5.
           MOVE GFCTN7-PONT-CH-NUM13       TO GFCTN8-PONT-CH-NUM13.
           MOVE GFCTN7-PONT-CH-ALP         TO GFCTN8-PONT-CH-ALP.
           MOVE GFCTN7-IND-PESSOA          TO GFCTN8-IND-PESSOA.

           MOVE 'N'                        TO GFCTN8-FIM.

           MOVE ZEROS                      TO GFCTN8-ERRO
                                              GFCTN8-COD-SQL-ERRO
                                              GFCTN8-COD-MSG-ERRO
                                              GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG            TO GFCTN8-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTE DADOS                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTN7-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTN7-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTN7-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTN7-PONT-CH-NUM5     NOT NUMERIC)                OR
              (GFCTN7-PONT-CH-NUM13    NOT NUMERIC)                OR
              (GFCTN7-PONT-PACOTE      NOT NUMERIC)                OR
              (GFCTN7-IND-PESSOA       NOT EQUAL 'F' AND 'J'
                                                     AND 'A')      OR
              (GFCTN7-PONT-AGPTO       NOT EQUAL  4 AND  9 AND
                                                 14 AND 15 AND
                                                 16 AND 17 AND
                                                 18 AND 19 AND
                                                 20)
               MOVE  1                 TO GFCTN8-ERRO
               MOVE  ZEROS             TO GFCTN8-COD-SQL-ERRO
               MOVE  0001              TO GFCTN8-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTN8-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTN8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 1320-SELECIONAR-GFCTB0A1.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                       TO GFCTG2-LL.
           MOVE ZEROS                      TO GFCTG2-ZZ.
           MOVE GFCTN7-TRANSACAO           TO GFCTG2-TRANSACAO.
           MOVE GFCTN7-FUNCAO              TO GFCTG2-FUNCAO.
           MOVE GFCTN7-FUNC-BDSCO          TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                        TO GFCTG2-FIM.
           MOVE 'O'                        TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'                 TO WRK-MODULO.

           CALL 'POOL0081'                 USING WRK-MODULO
                                                 GFCTG2-ENTRADA
                                                 GFCTG3-SAIDA
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 9                      TO GFCTN8-ERRO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
                                              WRK-MODULO-MSG
               MOVE WRK-MSG001             TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5136'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                 EQUAL 9
               MOVE 9                      TO GFCTN8-ERRO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
                                              WRK-MODULO-MSG
               MOVE WRK-MSG001             TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5136'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                 EQUAL 1
               MOVE 'S'                    TO GFCTN8-FIM
               MOVE 1                      TO GFCTN8-ERRO
               MOVE GFCTG3-COD-SQL-ERRO    TO GFCTN8-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO    TO GFCTN8-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO   TO GFCTN8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR DATA ATUAL DA TABELA GFCTB0A1                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1320-SELECIONAR-GFCTB0A1        SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL,
                   DPROCM_PROX
             INTO
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
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
               MOVE  0012              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-AUX1.
           MOVE WRK-MES-AUX            TO WRK-MES-AUX1
                                          WRK-MM-ATUA.
           MOVE WRK-ANO-AUX            TO WRK-ANO-AUX1
                                          WRK-AA-ATUA.
           MOVE WRK-DATA-AUX1          TO WRK-DPROCM.

      *----------------------------------------------------------------*
       1320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATA ERROS DB2                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1321-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5136'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  9                     TO GFCTN8-ERRO.
           MOVE  SQLCODE               TO GFCTN8-COD-SQL-ERRO
                                          GFCT0M-SQLCODE.
           MOVE SPACES                 TO GFCTN8-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG WRK-NOM-TAB
           DELIMITED BY '  '           INTO GFCTN8-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTN7-PONT-AGPTO
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

           PERFORM 2110-ABRIR-CURSOR-GFCTB0D0.

           PERFORM 2120-LER-CURSOR-GFCTB0D0.

           PERFORM 2130-PROCESSAR-CURSOR-GFCTB0D0 UNTIL
               WRK-FIM                 EQUAL 'S'.

           PERFORM 2140-FECHAR-CURSOR-GFCTB0D0.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR GFCTB0D0                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-ABRIR-CURSOR-GFCTB0D0      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.
           MOVE 'N'                    TO WRK-FIM.
           MOVE GFCTN8-PONT-PACOTE     TO CSERVC-TARIF OF GFCTB0D0.
           MOVE GFCTN8-PONT-AGPTO      TO CAGPTO-CTA   OF GFCTB0D0.
           MOVE GFCTN8-PONT-CH-NUM5    TO CDEPDC       OF GFCTB0D0.
           MOVE GFCTN8-PONT-CH-NUM13   TO CPOSTO-SERVC OF GFCTB0D0.

           EXEC SQL
               OPEN CSR02-GFCTB0D0
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D0'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_PAB   ' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTN8-COD-MSG-ERRO
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER O CURSOR GFCTB0D0                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-LER-CURSOR-GFCTB0D0        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR02-GFCTB0D0 INTO
                   :GFCTB0D0.CSERVC-TARIF,
                   :GFCTB0D0.CAGPTO-CTA,
                   :GFCTB0D0.DINIC-PRMSS-PCOTE,
                   :GFCTB0D0.CSEQ-AGPTO-CTA,
                   :GFCTB0D0.CEMPR-INC,
                   :GFCTB0D0.CDEPDC,
                   :GFCTB0D0.CPOSTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D0'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_PAB   ' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '         TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM
                                          GFCTN8-FIM
               PERFORM 2121-VER-VAZIO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VER SE O CURSOR ESTA VAZIO                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2121-VER-VAZIO                  SECTION.
      *----------------------------------------------------------------*

           IF  IND-1                   EQUAL 1
               MOVE 1                  TO GFCTN8-ERRO
               MOVE 799                TO GFCTN8-COD-MSG-ERRO
               MOVE '0799'             TO GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTN8-COD-SQL-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTN8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR GFCTB0D0                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2130-PROCESSAR-CURSOR-GFCTB0D0  SECTION.
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

       LE-DENOVO-1.

           PERFORM 2120-LER-CURSOR-GFCTB0D0.

           IF  WRK-FIM                 EQUAL 'N'
               IF  IND-1               GREATER 10
                   MOVE CSERVC-TARIF   OF GFCTB0D0
                                       TO CSERVC-TARIF      OF GFCTB0C6
                   MOVE CAGPTO-CTA     OF GFCTB0D0
                                       TO CAGPTO-CTA        OF GFCTB0C6
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D0
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0D0
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C6
                   PERFORM 2131-ACESSAR-GFCTB0C6
                   IF  WRK-LISTA       EQUAL 'N'
                       GO TO LE-DENOVO-1
                   END-IF
                   MOVE 'S'            TO WRK-FIM
                   MOVE CSERVC-TARIF   OF GFCTB0D0
                                       TO GFCTN8-PONT-PACOTE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA A TABELA GFCTB0C6                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2131-ACESSAR-GFCTB0C6           SECTION.
      *----------------------------------------------------------------*

BI0412     MOVE 'C'                    TO WRK-CINDCD-ADSAO-INDVD.
BI0412     MOVE 'A'                    TO WRK-CINDCD-ADSAO-AMBAS.

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
                   HINCL_REG_SIST,
                   CINDCD_EXCEC_ADSAO,
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
                   :GFCTB0C6.HINCL-REG-SIST,
                   :GFCTB0C6.CINDCD-EXCEC-ADSAO,
BI0810             :GFCTB0C6.CPSSOA-SERVC-TARIF
             FROM   DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
                   CSERVC_TARIF        = :GFCTB0C6.CSERVC-TARIF      AND
                   CAGPTO_CTA          = :GFCTB0C6.CAGPTO-CTA        AND
                   CSEQ_AGPTO_CTA      = :GFCTB0C6.CSEQ-AGPTO-CTA    AND
                   DINIC_PRMSS_PCOTE   = :GFCTB0C6.DINIC-PRMSS-PCOTE AND
BI0412            (CINDCD_ADSAO_INDVD  = :WRK-CINDCD-ADSAO-INDVD OR
BI0412             CINDCD_ADSAO_INDVD  = :WRK-CINDCD-ADSAO-AMBAS)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C6'      TO WRK-NOM-TAB
               MOVE 'PRMSS_ADSAO_PCOTE'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE DINIC-PRMSS-PCOTE  OF GFCTB0C6
                                       TO WRK-DATA-AUX
               MOVE WRK-DIA-AUX        TO WRK-DIA-AUX1
               MOVE WRK-MES-AUX        TO WRK-MES-AUX1
               MOVE WRK-ANO-AUX        TO WRK-ANO-AUX1
               MOVE WRK-DATA-AUX1      TO WRK-DINIC
               MOVE DFIM-PRMSS-PCOTE   OF GFCTB0C6
                                       TO WRK-DATA-AUX
               MOVE WRK-DIA-AUX        TO WRK-DIA-AUX1
               MOVE WRK-MES-AUX        TO WRK-MES-AUX1
               MOVE WRK-ANO-AUX        TO WRK-ANO-AUX1
               MOVE WRK-DATA-AUX1      TO WRK-DFIM
               PERFORM 2132-TRATAR-OCORRENCIA
           ELSE
               MOVE 'N'                TO WRK-LISTA
           END-IF.

      *----------------------------------------------------------------*
       2131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CARREGA AS OCORRENCIAS PARA A AREA DE SAIDA                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2132-TRATAR-OCORRENCIA          SECTION.
      *----------------------------------------------------------------*

           IF (WRK-DINIC               NOT GREATER WRK-DPROCM AND
               WRK-DFIM                NOT LESS    WRK-DPROCM ) OR
               WRK-DINIC               NOT LESS    WRK-DPROCM
               PERFORM 2133-VER-DATAS


               IF  WRK-LISTA           EQUAL 'S'
                   PERFORM 2136-OBTER-DESC-PACOTE
                   IF  IND-1           NOT GREATER 10
                       MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTN8-DESC-PACOTE(IND-1)
                       MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO GFCTN8-PACOTE(IND-1)
                       INSPECT WRK-DATA-DE-INICIO
                                           REPLACING ALL ' ' BY '.'
                       MOVE WRK-DATA-DE-INICIO
                                       TO GFCTN8-DATA-INICIO(IND-1)
                       INSPECT WRK-DATA-DE-FIM
                                           REPLACING ALL ' ' BY '.'
                       MOVE WRK-DATA-DE-FIM
                                       TO GFCTN8-DATA-FIM   (IND-1)
BI0810                 MOVE CPSSOA-SERVC-TARIF     OF GFCTB0C6
BI0810                                 TO GFCTN8-CPSSOA-TARIFA(IND-1)
BI0810                 ADD 66          TO GFCTN8-LL
                       ADD 1           TO GFCTN8-QTDE-TOT-REG
                                          IND-1
                   END-IF
               END-IF
           ELSE
               MOVE 'N'                TO WRK-LISTA
           END-IF.

      *----------------------------------------------------------------*
       2132-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR DATAS                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2133-VER-DATAS                  SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-PROX            OF GFCTB0A1
                                       TO WRK-DATA-AUX.

           MOVE WRK-MES-AUX            TO WRK-MM-PROX.
           MOVE WRK-ANO-AUX            TO WRK-AA-PROX.

           IF  WRK-DIA-AUX             NOT EQUAL 01
               MOVE 01                 TO WRK-DIA-AUX
               IF  WRK-AAMM-ATUA       EQUAL  WRK-AAMM-PROX
                   IF WRK-MES-AUX      LESS 12
                       MOVE  WRK-MES-AUX   TO WRK-MES-COMP
                       ADD 01          TO WRK-MES-COMP
                       MOVE  WRK-MES-COMP   TO WRK-MES-AUX
                   ELSE
                       MOVE 01         TO WRK-MES-AUX
                       MOVE  WRK-ANO-AUX   TO WRK-ANO-COMP
                       ADD 01          TO WRK-ANO-COMP
                       MOVE  WRK-ANO-COMP   TO WRK-ANO-AUX
                   END-IF
               END-IF
           END-IF.

           MOVE WRK-DIA-AUX            TO WRK-DIA-AUX1.
           MOVE WRK-MES-AUX            TO WRK-MES-AUX1.
           MOVE WRK-ANO-AUX            TO WRK-ANO-AUX1.
           MOVE WRK-DATA-AUX1          TO WRK-INI-DPROCM.

           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO WRK-DATA-AUX.

           IF  WRK-DIA-AUX             NOT EQUAL 01
               MOVE 01                 TO WRK-DIA-AUX
               IF  WRK-MES-AUX         LESS 12
                   ADD  01             TO WRK-MES-AUX
               ELSE
                   MOVE 01             TO WRK-MES-AUX
                   ADD  01             TO WRK-ANO-AUX
               END-IF
           END-IF.

           MOVE WRK-DIA-AUX            TO WRK-DIA-AUX1.
           MOVE WRK-MES-AUX            TO WRK-MES-AUX1.
           MOVE WRK-ANO-AUX            TO WRK-ANO-AUX1.
           MOVE WRK-DATA-AUX1          TO WRK-INI-PERMISSAO.

           IF  WRK-INI-DPROCM          GREATER WRK-INI-PERMISSAO
               MOVE WRK-INI-DPROCM     TO WRK-DATA-AUX1
CREMM!                                    WRK-DINIC
               MOVE WRK-DIA-AUX1       TO WRK-DIA-AUX
               MOVE WRK-MES-AUX1       TO WRK-MES-AUX
               MOVE WRK-ANO-AUX1       TO WRK-ANO-AUX
               MOVE WRK-DATA-AUX       TO WRK-DATA-DE-INICIO
           ELSE
               MOVE WRK-INI-PERMISSAO  TO WRK-DATA-AUX1
                                          WRK-DINIC
               MOVE WRK-DIA-AUX1       TO WRK-DIA-AUX
               MOVE WRK-MES-AUX1       TO WRK-MES-AUX
               MOVE WRK-ANO-AUX1       TO WRK-ANO-AUX
               MOVE WRK-DATA-AUX       TO WRK-DATA-DE-INICIO
           END-IF.

           PERFORM 2134-OBTER-DATA-FINAL.

      *----------------------------------------------------------------*
       2133-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTER ULTIMO DIA DO MES                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2134-OBTER-DATA-FINAL           SECTION.
      *----------------------------------------------------------------*

CREMM***** MOVE DPROCM-PROX            OF GFCTB0A1
CREMM      MOVE DFIM-PRMSS-PCOTE       OF GFCTB0C6
                                       TO WRK-DATA-AUX.
           MOVE 01                     TO WRK-DIA-AUX1.

           IF  WRK-MES-AUX             LESS 12
               COMPUTE WRK-MES-AUX1    = WRK-MES-AUX + 1
               MOVE WRK-ANO-AUX        TO WRK-ANO-AUX1
           ELSE
               MOVE 01                 TO WRK-MES-AUX1
               COMPUTE WRK-ANO-AUX1    = WRK-ANO-AUX + 1
           END-IF.

           MOVE WRK-DATA-AUX1          TO WRK-DATA-NUM.
           MOVE WRK-DATA-NUM           TO WRK-DATA-ABERT.
           MOVE -1                     TO WRK-PRAZO.

           CALL 'POOL0025'             USING WRK-DATA-ABERT
                                             WRK-PRAZO
                                             WRK-DATA-VENC.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE  1                 TO GFCTN8-ERRO
               MOVE  ZEROS             TO GFCTN8-COD-SQL-ERRO
               MOVE  0767              TO GFCTN8-COD-MSG-ERRO
               MOVE '0767'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTN8-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTN8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE WRK-DATA-VENC          TO WRK-DATA-NUM.
           MOVE WRK-DATA-NUM           TO WRK-DATA-AUX1.

           IF  WRK-DIA-AUX             EQUAL WRK-DIA-AUX1
               MOVE DFIM-PRMSS-PCOTE   OF GFCTB0C6
                                       TO WRK-DATA-DE-FIM
               MOVE WRK-DATA-AUX1      TO WRK-DATA-FIM
           ELSE
               PERFORM 2135-OBTER-DATA-FINAL
           END-IF.

           IF  WRK-DINIC               GREATER WRK-DATA-FIM
               MOVE 'N'                TO WRK-LISTA
           ELSE
               MOVE 'S'                TO WRK-LISTA
           END-IF.

      *----------------------------------------------------------------*
       2134-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTER ULTIMO DIA DO MES ANTERIOR                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2135-OBTER-DATA-FINAL           SECTION.
      *----------------------------------------------------------------*

CREMM******MOVE DPROCM-PROX            OF GFCTB0A1
CREMM      MOVE DFIM-PRMSS-PCOTE       OF GFCTB0C6
                                       TO WRK-DATA-AUX.
           MOVE 01                     TO WRK-DIA-AUX1.
           MOVE WRK-MES-AUX            TO WRK-MES-AUX1.
           MOVE WRK-ANO-AUX            TO WRK-ANO-AUX1.
           MOVE WRK-DATA-AUX1          TO WRK-DATA-NUM.
           MOVE WRK-DATA-NUM           TO WRK-DATA-ABERT.
           MOVE -1                     TO WRK-PRAZO.

           CALL 'POOL0025'             USING WRK-DATA-ABERT
                                             WRK-PRAZO
                                             WRK-DATA-VENC.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE  1                 TO GFCTN8-ERRO
               MOVE  ZEROS             TO GFCTN8-COD-SQL-ERRO
               MOVE  0767              TO GFCTN8-COD-MSG-ERRO
               MOVE '0767'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTN8-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTN8-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE WRK-DATA-VENC          TO WRK-DATA-NUM.
           MOVE WRK-DATA-NUM           TO WRK-DATA-AUX1.
           MOVE WRK-DIA-AUX1           TO WRK-DIA-AUX.
           MOVE WRK-MES-AUX1           TO WRK-MES-AUX.
           MOVE WRK-ANO-AUX1           TO WRK-ANO-AUX.
           MOVE WRK-DATA-AUX1          TO WRK-DATA-FIM.
           MOVE WRK-DATA-AUX           TO WRK-DATA-DE-FIM.

      *----------------------------------------------------------------*
       2135-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTER DESCRICAO PACOTE                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2136-OBTER-DESC-PACOTE          SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTN7-IND-PESSOA
               WHEN 'F'
                   MOVE '1'            TO WRK-PESSOA-1
                   MOVE '3'            TO WRK-PESSOA-2
                   MOVE '3'            TO WRK-PESSOA-3
               WHEN 'J'
                   MOVE '2'            TO WRK-PESSOA-1
                   MOVE '3'            TO WRK-PESSOA-2
                   MOVE '3'            TO WRK-PESSOA-3
               WHEN OTHER
                   MOVE '1'            TO WRK-PESSOA-1
                   MOVE '2'            TO WRK-PESSOA-2
                   MOVE '3'            TO WRK-PESSOA-3
           END-EVALUATE.

           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
             SELECT
                   RSERVC_TARIF_REDZD
             INTO
                   :GFCTB0D8.RSERVC-TARIF-REDZD
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF       AND
                  (CPSSOA_SERVC_TARIF = :WRK-PESSOA-1                OR
                   CPSSOA_SERVC_TARIF = :WRK-PESSOA-2                OR
                   CPSSOA_SERVC_TARIF = :WRK-PESSOA-3)
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0012'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C6'       TO WRK-NOM-TAB
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '          TO GFCT0M-COMANDO-SQL
               MOVE '0050'              TO GFCT0M-LOCAL
               MOVE  0012               TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES              TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
               MOVE 'S'                 TO WRK-LISTA
           ELSE
               MOVE 'N'                 TO WRK-LISTA
           END-IF.

      *----------------------------------------------------------------*
       2136-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR GFCTB0D0                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2140-FECHAR-CURSOR-GFCTB0D0     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR02-GFCTB0D0
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D0'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_PAB   ' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
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

           PERFORM 2210-ABRIR-CURSOR-GFCTB0C7.

           PERFORM 2220-LER-CURSOR-GFCTB0C7.

           PERFORM 2230-PROCESSAR-CURSOR-GFCTB0C7 UNTIL
               WRK-FIM              EQUAL 'S'.

           PERFORM 2240-FECHAR-CURSOR-GFCTB0C7.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR GFCTB0C7                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-ABRIR-CURSOR-GFCTB0C7      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.
           MOVE 'N'                    TO WRK-FIM.
           MOVE GFCTN7-PONT-PACOTE     TO CSERVC-TARIF OF GFCTB0C7.
           MOVE GFCTN7-PONT-AGPTO      TO CAGPTO-CTA   OF GFCTB0C7.
           MOVE GFCTN7-PONT-CH-NUM5    TO CDEPDC       OF GFCTB0C7.

           EXEC SQL
               OPEN CSR01-GFCTB0C7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C7'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_DEPDC ' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '         TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LE O CURSOR GFCTB0C7                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2220-LER-CURSOR-GFCTB0C7        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-GFCTB0C7 INTO
                   :GFCTB0C7.CSERVC-TARIF,
                   :GFCTB0C7.CAGPTO-CTA,
                   :GFCTB0C7.DINIC-PRMSS-PCOTE,
                   :GFCTB0C7.CSEQ-AGPTO-CTA,
                   :GFCTB0C7.CEMPR-INC,
                   :GFCTB0C7.CDEPDC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C7'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_DEPDC ' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '         TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM
                                          GFCTN8-FIM
               PERFORM 2121-VER-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSA O CURSOR GFCTB0C7                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2230-PROCESSAR-CURSOR-GFCTB0C7  SECTION.
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

       LE-DENOVO-2.

           PERFORM 2220-LER-CURSOR-GFCTB0C7.

           IF  WRK-FIM              EQUAL 'N'
               IF  IND-1               GREATER 10
                   MOVE CSERVC-TARIF   OF GFCTB0C7
                                       TO CSERVC-TARIF      OF GFCTB0C6
                   MOVE CAGPTO-CTA     OF GFCTB0C7
                                       TO CAGPTO-CTA        OF GFCTB0C6
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0C7
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0C7
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C6
                   PERFORM 2131-ACESSAR-GFCTB0C6
                   IF  WRK-LISTA       EQUAL 'N'
                       GO TO LE-DENOVO-2
                   END-IF
                   MOVE 'S'            TO WRK-FIM
                   MOVE CSERVC-TARIF   OF GFCTB0C7
                                       TO GFCTN8-PONT-PACOTE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR GFCTB0C7                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2240-FECHAR-CURSOR-GFCTB0C7     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR01-GFCTB0C7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C7'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_DEPDC ' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
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

           PERFORM 2310-ABRIR-CURSOR-GFCTB0C8.

           PERFORM 2320-LER-CURSOR-GFCTB0C8.

           PERFORM 2330-PROCESSAR-CURSOR-GFCTB0C8 UNTIL
               WRK-FIM                 EQUAL 'S'.

           PERFORM 2340-FECHAR-CURSOR-GFCTB0C8.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR GFCTB0C8                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-ABRIR-CURSOR-GFCTB0C8      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.
           MOVE 'N'                    TO WRK-FIM.
           MOVE GFCTN7-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB0C8.
           MOVE GFCTN7-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB0C8.
           MOVE GFCTN7-PONT-CH-NUM5    TO CJUNC-DEPDC-DEB   OF GFCTB0C8.
           MOVE GFCTN7-PONT-CH-NUM13   TO CNRO-CTA-DEB      OF GFCTB0C8.

           EXEC SQL
               OPEN CSR06-GFCTB0C8
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C8'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_EMPR  ' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '         TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LE O CURSOR GFCTB0C8                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-LER-CURSOR-GFCTB0C8        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR06-GFCTB0C8 INTO
                   :GFCTB0C8.CSERVC-TARIF,
                   :GFCTB0C8.CAGPTO-CTA,
                   :GFCTB0C8.DINIC-PRMSS-PCOTE,
                   :GFCTB0C8.CSEQ-AGPTO-CTA,
                   :GFCTB0C8.CJUNC-DEPDC-DEB,
                   :GFCTB0C8.CRZ-CTA-DEB,
                   :GFCTB0C8.CNRO-CTA-DEB,
                   :GFCTB0C8.CLCTO-CTA-DEB
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND + 100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C8'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_EMPR  ' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '         TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM
                                          GFCTN8-FIM
               PERFORM 2121-VER-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LE O CURSOR GFCTB0C8                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2330-PROCESSAR-CURSOR-GFCTB0C8  SECTION.
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

       LE-DENOVO-3.

           PERFORM 2320-LER-CURSOR-GFCTB0C8.

           IF  WRK-FIM              EQUAL 'N'
               IF  IND-1               GREATER 10
                   MOVE CSERVC-TARIF   OF GFCTB0C8
                                       TO CSERVC-TARIF      OF GFCTB0C6
                   MOVE CAGPTO-CTA     OF GFCTB0C8
                                       TO CAGPTO-CTA        OF GFCTB0C6
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0C8
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0C8
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C6
                   PERFORM 2131-ACESSAR-GFCTB0C6
                   IF  WRK-LISTA       EQUAL 'N'
                       GO TO LE-DENOVO-3
                   END-IF
                   MOVE 'S'            TO WRK-FIM
                   MOVE CSERVC-TARIF   OF GFCTB0C8
                                       TO GFCTN8-PONT-PACOTE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR GFCTB0C8                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2340-FECHAR-CURSOR-GFCTB0C8     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR06-GFCTB0C8
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C8'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_EMPR  ' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
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

           PERFORM 2410-ABRIR-CURSOR-GFCTB0D1.

           PERFORM 2420-LER-CURSOR-GFCTB0D1.

           PERFORM 2430-PROCESSAR-CURSOR-GFCTB0D1 UNTIL
               WRK-FIM              EQUAL 'S'.

           PERFORM 2440-FECHAR-CURSOR-GFCTB0D1.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR GFCTB0D1                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-ABRIR-CURSOR-GFCTB0D1      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.
           MOVE 'N'                    TO WRK-FIM.
           MOVE GFCTN7-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB0D1.
           MOVE GFCTN7-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB0D1.
           MOVE GFCTN7-PONT-CH-NUM5    TO CDEPDC            OF GFCTB0D1.
           MOVE GFCTN7-PONT-CH-NUM13   TO CPOSTO-SERVC      OF GFCTB0D1.

           EXEC SQL
               OPEN CSR03-GFCTB0D1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D1'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_PSTAL ' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '         TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LE O CURSOR GFCTB0D1                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2420-LER-CURSOR-GFCTB0D1        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR03-GFCTB0D1 INTO
                   :GFCTB0D1.CSERVC-TARIF,
                   :GFCTB0D1.CAGPTO-CTA,
                   :GFCTB0D1.DINIC-PRMSS-PCOTE,
                   :GFCTB0D1.CSEQ-AGPTO-CTA,
                   :GFCTB0D1.CEMPR-INC,
                   :GFCTB0D1.CDEPDC,
                   :GFCTB0D1.CPOSTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D1'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_PSTAL ' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '         TO GFCT0M-COMANDO-SQL
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM
                                          GFCTN8-FIM
               PERFORM 2121-VER-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAR GFCTB0D1                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2430-PROCESSAR-CURSOR-GFCTB0D1  SECTION.
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

       LE-DENOVO-4.

           PERFORM 2420-LER-CURSOR-GFCTB0D1.

           IF  WRK-FIM              EQUAL 'N'
               IF  IND-1               GREATER 10
                   MOVE CSERVC-TARIF   OF GFCTB0D1
                                       TO CSERVC-TARIF      OF GFCTB0C6
                   MOVE CAGPTO-CTA     OF GFCTB0D1
                                       TO CAGPTO-CTA        OF GFCTB0C6
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0D1
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C6
                   PERFORM 2131-ACESSAR-GFCTB0C6
                   IF  WRK-LISTA       EQUAL 'N'
                       GO TO LE-DENOVO-4
                   END-IF
                   MOVE 'S'            TO WRK-FIM
                   MOVE CSERVC-TARIF   OF GFCTB0D1
                                       TO GFCTN8-PONT-PACOTE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR GFCTB0D1                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2440-FECHAR-CURSOR-GFCTB0D1     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR03-GFCTB0D1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D1'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_PSTAL ' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
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

           PERFORM 2510-ABRIR-CURSOR-GFCTB0I0.

           PERFORM 2520-LER-CURSOR-GFCTB0I0.

           PERFORM 2530-PROCESSAR-CURSOR-GFCTB0I0 UNTIL
               WRK-FIM              EQUAL 'S'.

           PERFORM 2540-FECHAR-CURSOR-GFCTB0I0.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR GFCTB0I0                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2510-ABRIR-CURSOR-GFCTB0I0      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.
           MOVE 'N'                    TO WRK-FIM.
           MOVE GFCTN7-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB0I0.
           MOVE GFCTN7-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB0I0.
           MOVE GFCTN7-PONT-CH-NUM13   TO W-NUM13.

           IF GFCTN7-IND-PESSOA        EQUAL 'J'
ST25X6*       MOVE W-DE9               TO CCGC-CPF          OF GFCTB0I0
ST25X6        MOVE W-DE9               TO CCGC-CPF-ST       OF GFCTB0I0
           ELSE
ST25X6*       MOVE W-NUM13             TO CCGC-CPF          OF GFCTB0I0
ST25X6        MOVE W-NUM13             TO CCGC-CPF-ST       OF GFCTB0I0
           END-IF.

           EXEC SQL
               OPEN CSR08-GFCTB0I0
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0I0'      TO WRK-NOM-TAB
               MOVE 'TPRMSS_GRP_CLI  ' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '         TO GFCT0M-COMANDO-SQL
               MOVE '0160'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LE O CURSOR GFCTB0I0                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2520-LER-CURSOR-GFCTB0I0        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR08-GFCTB0I0 INTO
                   :GFCTB0I0.CSERVC-TARIF,
                   :GFCTB0I0.CAGPTO-CTA,
                   :GFCTB0I0.DINIC-PRMSS-PCOTE,
                   :GFCTB0I0.CSEQ-AGPTO-CTA,
                   :GFCTB0I0.CCGC-CPF,
                   :GFCTB0I0.CFLIAL-CGC,
                   :GFCTB0I0.CCTRL-CPF-CGC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0I0'      TO WRK-NOM-TAB
               MOVE 'TPRMSS_GRP_CLI  ' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '         TO GFCT0M-COMANDO-SQL
               MOVE '0170'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM
                                          GFCTN8-FIM
               PERFORM 2121-VER-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LE O CURSOR GFCTB0I0                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2530-PROCESSAR-CURSOR-GFCTB0I0  SECTION.
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

       LE-DENOVO-5.

           PERFORM 2520-LER-CURSOR-GFCTB0I0.

           IF  WRK-FIM              EQUAL 'N'
               IF  IND-1               GREATER 10
                   MOVE CSERVC-TARIF   OF GFCTB0I0
                                       TO CSERVC-TARIF      OF GFCTB0C6
                   MOVE CAGPTO-CTA     OF GFCTB0I0
                                       TO CAGPTO-CTA        OF GFCTB0C6
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0I0
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0I0
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C6
                   PERFORM 2131-ACESSAR-GFCTB0C6
                   IF  WRK-LISTA       EQUAL 'N'
                       GO TO LE-DENOVO-5
                   END-IF
                   MOVE 'S'            TO WRK-FIM
                   MOVE CSERVC-TARIF   OF GFCTB0I0
                                       TO GFCTN8-PONT-PACOTE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR GFCTB0I0                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2540-FECHAR-CURSOR-GFCTB0I0     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR08-GFCTB0I0
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0I0'      TO WRK-NOM-TAB
               MOVE 'TPRMSS_GRP_CLI  ' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0180'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
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

           PERFORM 2610-ABRIR-CURSOR-GFCTB0D2.

           PERFORM 2620-LER-CURSOR-GFCTB0D2.

           PERFORM 2630-PROCESSAR-CURSOR-GFCTB0D2 UNTIL
               WRK-FIM                 EQUAL 'S'.

           PERFORM 2640-FECHAR-CURSOR-GFCTB0D2.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR GFCTB0D2                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2610-ABRIR-CURSOR-GFCTB0D2      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.
           MOVE 'N'                    TO WRK-FIM.
           MOVE GFCTN7-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB0D2.
           MOVE GFCTN7-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB0D2.
           MOVE GFCTN7-PONT-CH-NUM5    TO CSGMTO-GSTAO-TARIF
           OF GFCTB0D2.

           EXEC SQL
               OPEN CSR07-GFCTB0D2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D2'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_SGMTO ' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '         TO GFCT0M-COMANDO-SQL
               MOVE '0190'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LE O CURSOR GFCTB0D2                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2620-LER-CURSOR-GFCTB0D2        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR07-GFCTB0D2 INTO
                   :GFCTB0D2.CSERVC-TARIF,
                   :GFCTB0D2.CAGPTO-CTA,
                   :GFCTB0D2.DINIC-PRMSS-PCOTE,
                   :GFCTB0D2.CSEQ-AGPTO-CTA,
                   :GFCTB0D2.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D2'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_SGMTO ' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '         TO GFCT0M-COMANDO-SQL
               MOVE '0200'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM
                                          GFCTN8-FIM
               PERFORM 2121-VER-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       2620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAR O CURSOR GFCTB0D2                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2630-PROCESSAR-CURSOR-GFCTB0D2  SECTION.
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

       LE-DENOVO-6.

           PERFORM 2620-LER-CURSOR-GFCTB0D2.

           IF  WRK-FIM              EQUAL 'N'
               IF  IND-1               GREATER 10
                   MOVE CSERVC-TARIF   OF GFCTB0D2
                                       TO CSERVC-TARIF      OF GFCTB0C6
                   MOVE CAGPTO-CTA     OF GFCTB0D2
                                       TO CAGPTO-CTA        OF GFCTB0C6
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D2
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0D2
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6
                   PERFORM 2131-ACESSAR-GFCTB0C6
                   IF  WRK-LISTA       EQUAL 'N'
                       GO TO LE-DENOVO-6
                   END-IF
                   MOVE 'S'            TO WRK-FIM
                   MOVE CSERVC-TARIF   OF GFCTB0D2
                                       TO GFCTN8-PONT-PACOTE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2630-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR GFCTB0D2                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2640-FECHAR-CURSOR-GFCTB0D2     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR07-GFCTB0D2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D2'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_SGMTO ' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0220'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
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

           PERFORM 2710-ABRIR-CURSOR-GFCTB0C9.

           PERFORM 2720-LER-CURSOR-GFCTB0C9.

           PERFORM 2730-PROCESSAR-CURSOR-GFCTB0C9 UNTIL
               WRK-FIM              EQUAL 'S'.

           PERFORM 2740-FECHAR-CURSOR-GFCTB0C9.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR GFCTB0C9                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2710-ABRIR-CURSOR-GFCTB0C9      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.
           MOVE 'N'                    TO WRK-FIM.
           MOVE GFCTN7-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB0C9.
           MOVE GFCTN7-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB0C9.
           MOVE GFCTN7-PONT-CH-NUM5    TO CMUN-IBGE         OF GFCTB0C9.

           EXEC SQL
               OPEN CSR04-GFCTB0C9
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C9'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_MUN   ' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '         TO GFCT0M-COMANDO-SQL
               MOVE '0230'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LE O CURSOR GFCTB0C9                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2720-LER-CURSOR-GFCTB0C9        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR04-GFCTB0C9 INTO
                   :GFCTB0C9.CSERVC-TARIF,
                   :GFCTB0C9.CAGPTO-CTA,
                   :GFCTB0C9.DINIC-PRMSS-PCOTE,
                   :GFCTB0C9.CSEQ-AGPTO-CTA,
                   :GFCTB0C9.CMUN-IBGE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D2'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_MUN   ' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '         TO GFCT0M-COMANDO-SQL
               MOVE '0240'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM
                                          GFCTN8-FIM
               PERFORM 2121-VER-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       2720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAR CURSOR GFCTB0C9                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2730-PROCESSAR-CURSOR-GFCTB0C9  SECTION.
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

       LE-DENOVO-7.

           PERFORM 2720-LER-CURSOR-GFCTB0C9.

           IF  WRK-FIM              EQUAL 'N'
               IF  IND-1               GREATER 10
                   MOVE CSERVC-TARIF   OF GFCTB0C9
                                       TO CSERVC-TARIF      OF GFCTB0C6
                   MOVE CAGPTO-CTA     OF GFCTB0C9
                                       TO CAGPTO-CTA        OF GFCTB0C6
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0C9
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0C9
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6
                   PERFORM 2131-ACESSAR-GFCTB0C6
                   IF  WRK-LISTA       EQUAL 'N'
                       GO TO LE-DENOVO-7
                   END-IF
                   MOVE 'S'            TO WRK-FIM
                   MOVE CSERVC-TARIF   OF GFCTB0C9
                                       TO GFCTN8-PONT-PACOTE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2730-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR GFCTB0C9                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2740-FECHAR-CURSOR-GFCTB0C9     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR04-GFCTB0C9
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C9'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_MUN   ' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0250'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
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

           PERFORM 2810-ABRIR-CURSOR-GFCTB0D3.

           PERFORM 2820-LER-CURSOR-GFCTB0D3.

           PERFORM 2830-PROCESSAR-CURSOR-GFCTB0D3 UNTIL
               WRK-FIM                 EQUAL 'S'.

           PERFORM 2840-FECHAR-CURSOR-GFCTB0D3.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR GFCTB0D3                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2810-ABRIR-CURSOR-GFCTB0D3      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.
           MOVE 'N'                    TO WRK-FIM.
           MOVE GFCTN7-PONT-PACOTE     TO CSERVC-TARIF      OF GFCTB0D3.
           MOVE GFCTN7-PONT-AGPTO      TO CAGPTO-CTA        OF GFCTB0D3.
           MOVE GFCTN7-PONT-CH-ALP     TO CSGL-UF           OF GFCTB0D3.

           EXEC SQL
               OPEN CSR05-GFCTB0D3
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D3'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_UF    ' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '         TO GFCT0M-COMANDO-SQL
               MOVE '0280'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LE O CURSOR GFCTB0D3                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2820-LER-CURSOR-GFCTB0D3        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR05-GFCTB0D3 INTO
                   :GFCTB0D3.CSERVC-TARIF,
                   :GFCTB0D3.CAGPTO-CTA,
                   :GFCTB0D3.DINIC-PRMSS-PCOTE,
                   :GFCTB0D3.CSEQ-AGPTO-CTA,
                   :GFCTB0D3.CSGL-UF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D3'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_UF    ' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '         TO GFCT0M-COMANDO-SQL
               MOVE '0290'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM
                                          GFCTN8-FIM
               PERFORM 2121-VER-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       2820-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAR CURSOR                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2830-PROCESSAR-CURSOR-GFCTB0D3  SECTION.
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

       LE-DENOVO-8.

           PERFORM 2820-LER-CURSOR-GFCTB0D3.

           IF  WRK-FIM                 EQUAL 'N'
               IF  IND-1               GREATER 10
                   MOVE CSERVC-TARIF   OF GFCTB0D3
                                       TO CSERVC-TARIF      OF GFCTB0C6
                   MOVE CAGPTO-CTA     OF GFCTB0D3
                                       TO CAGPTO-CTA        OF GFCTB0C6
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D3
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0D3
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6
                   PERFORM 2131-ACESSAR-GFCTB0C6
                   IF  WRK-LISTA       EQUAL 'N'
                       GO TO LE-DENOVO-8
                   END-IF
                   MOVE 'S'            TO WRK-FIM
                   MOVE CSERVC-TARIF   OF GFCTB0D3
                                       TO GFCTN8-PONT-PACOTE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2830-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR GFCTB0C9                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2840-FECHAR-CURSOR-GFCTB0D3     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR05-GFCTB0D3
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D3'      TO WRK-NOM-TAB
               MOVE 'PRMSS_GRP_UF    ' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0250'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTN8-COD-MSG-ERRO
               PERFORM 1321-FORMATAR-ERRO-DB2
??????         MOVE SPACES             TO GFCTN8-DADOS-RST
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
