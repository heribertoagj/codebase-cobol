      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0482.
       AUTHOR.     EVERTON LEANDRO FROZZA.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0482                                    *
      *    PROGRAMADOR.:   EVERTON LEANDRO FROZZA  - CPM PATO BRANCO   *
      *    ANALISTA CPM:   JOSSIANE CARLA GAZZONI  - CPM PATO BRANCO   *
      *    ANALISTA....:   VALERIA                 - PROCWORK /GP.50   *
      *    DATA........:   28/06/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   LISTA 2 DE CONSULTA DE PEDIDOS FLEXIBILIZA -*
      *      CAO PARA CONTRATO.                                        *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.PDIDO_FLEXZ_CONTR      GFCTB0B4       *
      *                   DB2PRD.FLEXZ_NRO_CONTR        GFCTB059       *
      *                   DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTS8    AREA DE COMUNICACAO   ENTRADA                   *
      *    I#GFCTS9    AREA DE COMUNICACAO   SAIDA                     *
      *    I#GFCT75    AREA DE COMUNICACAO   ENTRADA DO MODULO GFCT5579*
      *    I#GFCT76    AREA DE COMUNICACAO   SAIDA DO MODULO GFCT5579  *
      *    I#GFCTG2    AREA DE COMUNICACAO   ENTRADA DO MODULO GFCT5523*
      *    I#GFCTG3    AREA DE COMUNICACAO   SAIDA   DO MODULO GFCT5523*
      *    I#GFCTFZ    AREA DE COMUNICACAO   ENTRADA DO MODULO GFCT5522*
      *    I#GFCTG1    AREA DE COMUNICACAO   SAIDA   DO MODULO GFCT5522*
      *    I#GFCT74    AREA DE COMUNICACAO - ENTRDA E SAIDA DO GFCT5578*
      *    I#GFCT0M    AREA DE COMUNICACAO - ERROS                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5537   VERIFICA SE O USUARIO TEM PERMISSAO DE CONSULTA  *
      *    GFCT5579 - CLONE DO GFCT5537                                *
      *    GFCT5523   OBTER DSCRICAO DE MENSAGENS                      *
      *    GFCT5522   VERIFICA DISPONIBILIDADE ON LINE                 *
      *    GFCT5515   OBTER IDENTIFICACAO DO SOLICITANTE DE FLEXIBILI -*
      *    ZACAO.                                                      *
      *    GFCT5578 - CLONE DO GFCT5515                                *
      *    POOL0081   ALOCAR DINAMICAMENTE UM MODULO                   *
      *================================================================*
      *================================================================*
      *    ULTIMA ALTERACAO EM 31/10/2007 - VALERIA                    *
      *  - SUBSTITUICAO DO MODULO GFCT5515 PELO GFCT5578 PARA DEPTO    *
      *    AUTORIZADO.                                                 *
      *  - SUBSTITUICAO DO MODULO GFCT5537 PELO GFCT5579 PARA DEPTO    *
      *    AUTORIZADO.                                                 *
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

       77  IND-1                       PIC  9(001)         VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *---------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOME-TAB                PIC  X(011)         VALUE SPACES.
       77  WRK-SITUACAO                PIC  9(002)         VALUE ZEROS.
       77  WRK-FIM-CRS                 PIC  X(001)         VALUE SPACES.
       77  WRK-CODIGO-JUNCAO           PIC  9(005) COMP-3  VALUE ZEROS.
       77  WRK-TIPO-DEPENDENCIA        PIC  X(002)         VALUE SPACES.
       77  WRK-DESPREZA                PIC  9(003)         VALUE ZEROS.

       01  WRK-IO-PCB.
           05 IO-LTERM                 PIC X(08)           VALUE SPACES.
           05 FILLER                   PIC X(02)           VALUE SPACES.
           05 IO-STATUS                PIC X(02)           VALUE SPACES.
           05 FILLER                   PIC X(12)           VALUE SPACES.
           05 IO-MODNAME               PIC X(08)           VALUE SPACES.

       01  WRK-ALT-PCB.
           05 ALT-LTERM                PIC X(08)           VALUE SPACES.
           05 FILLER                   PIC X(02)           VALUE SPACES.
           05 ALT-STATUS               PIC X(02)           VALUE SPACES.
           05 FILLER                   PIC X(12)           VALUE SPACES.
           05 ALT-MODNAME              PIC X(08)           VALUE SPACES.

       01  WRK-DEP-ORIGE-INI           PIC S9(005) COMP-3  VALUE ZEROS.
       01  WRK-DEP-ORIGE-FIM           PIC S9(005) COMP-3  VALUE ZEROS.

       01  WRK-MSG01.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-NOME-MOD             PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA INCLUDES *'.
      *---------------------------------------------------------------*

       COPY I#GFCTFZ.

       COPY I#GFCTG1.

       COPY I#GFCTG2.

       COPY I#GFCTG3.

       COPY I#GFCT74.

       COPY I#GFCT75.

       COPY I#GFCT76.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B4
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB059
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSORES *'.
      *---------------------------------------------------------------*

           EXEC SQL
             DECLARE   CSR01-GFCTB0B4   CURSOR   FOR
               SELECT
                     CJUNC_DEPDC,
                     CCTA_CLI,
                     CROTNA_ORIGE_CONTR,
                     CIDTFD_CONTR_FLEXZ,
                     CSERVC_TARIF,
                     HIDTFD_FLEXZ_CONTR,
                     VNEGOC_FLEXZ_CONTR,
                     PDESC_FLEXZ_CONTR,
                     QDIA_FLEXZ_CONTR,
                     CEMPR_ORIGE_FLEXZ,
                     CDEPDC_ORIGE_FLEXZ,
                     CEMPR_PAREC_FLEXZ,
                     CDEPDC_PAREC_FLEXZ,
                     CSIT_SOLTC_FLEXZ
             FROM  DB2PRD.PDIDO_FLEXZ_CONTR
             WHERE
                   CBCO_CTA_CLI       = :GFCTB0B4.CBCO-CTA-CLI       AND
                   CJUNC_DEPDC        = :GFCTB0B4.CJUNC-DEPDC        AND
                   CCTA_CLI           = :GFCTB0B4.CCTA-CLI           AND
                   CIDTFD_CONTR_FLEXZ = :GFCTB0B4.CIDTFD-CONTR-FLEXZ AND
                   CROTNA_ORIGE_CONTR = :GFCTB0B4.CROTNA-ORIGE-CONTR AND
                  (CDEPDC_ORIGE_FLEXZ >= :WRK-DEP-ORIGE-INI          AND
                   CDEPDC_ORIGE_FLEXZ <= :WRK-DEP-ORIGE-FIM)
             ORDER BY
                   CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE   CSR02-GFCTB0B4   CURSOR   FOR
               SELECT
                     CJUNC_DEPDC,
                     CCTA_CLI,
                     CROTNA_ORIGE_CONTR,
                     CIDTFD_CONTR_FLEXZ,
                     CSERVC_TARIF,
                     HIDTFD_FLEXZ_CONTR,
                     VNEGOC_FLEXZ_CONTR,
                     PDESC_FLEXZ_CONTR,
                     QDIA_FLEXZ_CONTR,
                     CEMPR_ORIGE_FLEXZ,
                     CDEPDC_ORIGE_FLEXZ,
                     CEMPR_PAREC_FLEXZ,
                     CDEPDC_PAREC_FLEXZ,
                     CSIT_SOLTC_FLEXZ
             FROM  DB2PRD.PDIDO_FLEXZ_CONTR
             WHERE
                   CBCO_CTA_CLI       = :GFCTB0B4.CBCO-CTA-CLI     AND
                   CJUNC_DEPDC        = :GFCTB0B4.CJUNC-DEPDC      AND
                   CCTA_CLI           = :GFCTB0B4.CCTA-CLI         AND
                   CIDTFD_CONTR_FLEXZ = :GFCTB0B4.CIDTFD-CONTR-FLEXZ AND
                  (CDEPDC_ORIGE_FLEXZ >= :WRK-DEP-ORIGE-INI       AND
                   CDEPDC_ORIGE_FLEXZ <= :WRK-DEP-ORIGE-FIM)      AND
                   CSIT_SOLTC_FLEXZ   IN (01, 07)
             ORDER BY
                   CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE   CSR03-GFCTB0B4   CURSOR   FOR
               SELECT
                     CJUNC_DEPDC,
                     CCTA_CLI,
                     CROTNA_ORIGE_CONTR,
                     CIDTFD_CONTR_FLEXZ,
                     CSERVC_TARIF,
                     HIDTFD_FLEXZ_CONTR,
                     VNEGOC_FLEXZ_CONTR,
                     PDESC_FLEXZ_CONTR,
                     QDIA_FLEXZ_CONTR,
                     CEMPR_ORIGE_FLEXZ,
                     CDEPDC_ORIGE_FLEXZ,
                     CEMPR_PAREC_FLEXZ,
                     CDEPDC_PAREC_FLEXZ,
                     CSIT_SOLTC_FLEXZ
             FROM  DB2PRD.PDIDO_FLEXZ_CONTR
             WHERE
                   CBCO_CTA_CLI       = :GFCTB0B4.CBCO-CTA-CLI     AND
                   CJUNC_DEPDC        = :GFCTB0B4.CJUNC-DEPDC      AND
                   CCTA_CLI           = :GFCTB0B4.CCTA-CLI         AND
                   CIDTFD_CONTR_FLEXZ = :GFCTB0B4.CIDTFD-CONTR-FLEXZ AND
                  (CDEPDC_ORIGE_FLEXZ >= :WRK-DEP-ORIGE-INI        AND
                   CDEPDC_ORIGE_FLEXZ <= :WRK-DEP-ORIGE-FIM)       AND
                   CSIT_SOLTC_FLEXZ   IN (02)
             ORDER BY
                   CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE   CSR04-GFCTB0B4   CURSOR   FOR
               SELECT
                     CJUNC_DEPDC,
                     CCTA_CLI,
                     CROTNA_ORIGE_CONTR,
                     CIDTFD_CONTR_FLEXZ,
                     CSERVC_TARIF,
                     HIDTFD_FLEXZ_CONTR,
                     VNEGOC_FLEXZ_CONTR,
                     PDESC_FLEXZ_CONTR,
                     QDIA_FLEXZ_CONTR,
                     CEMPR_ORIGE_FLEXZ,
                     CDEPDC_ORIGE_FLEXZ,
                     CEMPR_PAREC_FLEXZ,
                     CDEPDC_PAREC_FLEXZ,
                     CSIT_SOLTC_FLEXZ
             FROM  DB2PRD.PDIDO_FLEXZ_CONTR
            WHERE
                  CBCO_CTA_CLI       = :GFCTB0B4.CBCO-CTA-CLI
            AND   CJUNC_DEPDC        = :GFCTB0B4.CJUNC-DEPDC
            AND   CCTA_CLI           = :GFCTB0B4.CCTA-CLI
            AND   CIDTFD_CONTR_FLEXZ = :GFCTB0B4.CIDTFD-CONTR-FLEXZ
            AND ((CDEPDC_ORIGE_FLEXZ >= :WRK-DEP-ORIGE-INI
            AND   CDEPDC_ORIGE_FLEXZ <= :WRK-DEP-ORIGE-FIM)
            AND   CSIT_SOLTC_FLEXZ   IN (01 , 02))
              OR  (CSIT_SOLTC_FLEXZ      = 07
             AND   ((CDEPDC_PAREC_FLEXZ <> 0
             AND     CDEPDC_PAREC_FLEXZ  =
                                      :GFCTB0B4.CDEPDC-PAREC-FLEXZ)
              OR    (CDEPDC_PAREC_FLEXZ  = 0
              AND     CDEPDC_ORIGE_FLEXZ  =
                                      :GFCTB0B4.CDEPDC-ORIGE-FLEXZ)))
             ORDER BY
                   CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE   CSR05-GFCTB0B4   CURSOR   FOR
               SELECT
                     T1.CJUNC_DEPDC,
                     T1.CCTA_CLI,
                     T1.CROTNA_ORIGE_CONTR,
                     T1.CIDTFD_CONTR_FLEXZ,
                     T1.CSERVC_TARIF,
                     T1.HIDTFD_FLEXZ_CONTR,
                     T1.VNEGOC_FLEXZ_CONTR,
                     T1.PDESC_FLEXZ_CONTR,
                     T1.QDIA_FLEXZ_CONTR,
                     T1.CEMPR_ORIGE_FLEXZ,
                     T1.CDEPDC_ORIGE_FLEXZ,
                     T1.CEMPR_PAREC_FLEXZ,
                     T1.CDEPDC_PAREC_FLEXZ,
                     T1.CSIT_SOLTC_FLEXZ
             FROM  DB2PRD.PDIDO_FLEXZ_CONTR T1,
                   DB2PRD.SERVC_TARIF_PRINC T2
             WHERE
                T1.CBCO_CTA_CLI       = :GFCTB0B4.CBCO-CTA-CLI AND
                T1.CJUNC_DEPDC        = :GFCTB0B4.CJUNC-DEPDC  AND
                T1.CCTA_CLI           = :GFCTB0B4.CCTA-CLI     AND
                T1.CIDTFD_CONTR_FLEXZ = :GFCTB0B4.CIDTFD-CONTR-FLEXZ AND
               (T1.CDEPDC_ORIGE_FLEXZ >= :WRK-DEP-ORIGE-INI    AND
                T1.CDEPDC_ORIGE_FLEXZ <= :WRK-DEP-ORIGE-FIM)   AND
                T1.CSERVC_TARIF       = T2.CSERVC_TARIF        AND
                T2.CDEPDC             = :GFCTB0D8.CDEPDC
             ORDER BY
                T1.CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE   CSR06-GFCTB0B4   CURSOR   FOR
               SELECT
                     T1.CJUNC_DEPDC,
                     T1.CCTA_CLI,
                     T1.CROTNA_ORIGE_CONTR,
                     T1.CIDTFD_CONTR_FLEXZ,
                     T1.CSERVC_TARIF,
                     T1.HIDTFD_FLEXZ_CONTR,
                     T1.VNEGOC_FLEXZ_CONTR,
                     T1.PDESC_FLEXZ_CONTR,
                     T1.QDIA_FLEXZ_CONTR,
                     T1.CEMPR_ORIGE_FLEXZ,
                     T1.CDEPDC_ORIGE_FLEXZ,
                     T1.CEMPR_PAREC_FLEXZ,
                     T1.CDEPDC_PAREC_FLEXZ,
                     T1.CSIT_SOLTC_FLEXZ
             FROM  DB2PRD.PDIDO_FLEXZ_CONTR T1,
                   DB2PRD.SERVC_TARIF_PRINC T2
             WHERE
                T1.CBCO_CTA_CLI      = :GFCTB0B4.CBCO-CTA-CLI       AND
                T1.CJUNC_DEPDC       = :GFCTB0B4.CJUNC-DEPDC        AND
                T1.CCTA_CLI          = :GFCTB0B4.CCTA-CLI           AND
                T1.CIDTFD_CONTR_FLEXZ = :GFCTB0B4.CIDTFD-CONTR-FLEXZ AND
               (T1.CDEPDC_ORIGE_FLEXZ >= :WRK-DEP-ORIGE-INI         AND
                T1.CDEPDC_ORIGE_FLEXZ <= :WRK-DEP-ORIGE-FIM)        AND
                T1.CSERVC_TARIF       = T2.CSERVC_TARIF             AND
                T2.CDEPDC             = :GFCTB0D8.CDEPDC            AND
                T1.CSIT_SOLTC_FLEXZ   IN (01, 07)                   AND
                T2.CDEPDC             = :GFCTB0D8.CDEPDC
             ORDER BY
                T1.CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE   CSR07-GFCTB0B4   CURSOR   FOR
               SELECT
                     T1.CJUNC_DEPDC,
                     T1.CCTA_CLI,
                     T1.CROTNA_ORIGE_CONTR,
                     T1.CIDTFD_CONTR_FLEXZ,
                     T1.CSERVC_TARIF,
                     T1.HIDTFD_FLEXZ_CONTR,
                     T1.VNEGOC_FLEXZ_CONTR,
                     T1.PDESC_FLEXZ_CONTR,
                     T1.QDIA_FLEXZ_CONTR,
                     T1.CEMPR_ORIGE_FLEXZ,
                     T1.CDEPDC_ORIGE_FLEXZ,
                     T1.CEMPR_PAREC_FLEXZ,
                     T1.CDEPDC_PAREC_FLEXZ,
                     T1.CSIT_SOLTC_FLEXZ
             FROM  DB2PRD.PDIDO_FLEXZ_CONTR T1,
                   DB2PRD.SERVC_TARIF_PRINC T2
             WHERE
                T1.CBCO_CTA_CLI      = :GFCTB0B4.CBCO-CTA-CLI       AND
                T1.CJUNC_DEPDC       = :GFCTB0B4.CJUNC-DEPDC        AND
                T1.CCTA_CLI          = :GFCTB0B4.CCTA-CLI           AND
                T1.CIDTFD_CONTR_FLEXZ = :GFCTB0B4.CIDTFD-CONTR-FLEXZ AND
               (T1.CDEPDC_ORIGE_FLEXZ >= :WRK-DEP-ORIGE-INI         AND
                T1.CDEPDC_ORIGE_FLEXZ <= :WRK-DEP-ORIGE-FIM)        AND
                T1.CSERVC_TARIF       = T2.CSERVC_TARIF             AND
                T2.CDEPDC             = :GFCTB0D8.CDEPDC            AND
                T1.CSIT_SOLTC_FLEXZ   IN (02)                       AND
                T2.CDEPDC             = :GFCTB0D8.CDEPDC
             ORDER BY
                T1.CSERVC_TARIF
           END-EXEC.

           EXEC SQL
             DECLARE   CSR08-GFCTB0B4   CURSOR   FOR
               SELECT
                     T1.CJUNC_DEPDC,
                     T1.CCTA_CLI,
                     T1.CROTNA_ORIGE_CONTR,
                     T1.CIDTFD_CONTR_FLEXZ,
                     T1.CSERVC_TARIF,
                     T1.HIDTFD_FLEXZ_CONTR,
                     T1.VNEGOC_FLEXZ_CONTR,
                     T1.PDESC_FLEXZ_CONTR,
                     T1.QDIA_FLEXZ_CONTR,
                     T1.CEMPR_ORIGE_FLEXZ,
                     T1.CDEPDC_ORIGE_FLEXZ,
                     T1.CEMPR_PAREC_FLEXZ,
                     T1.CDEPDC_PAREC_FLEXZ,
                     T1.CSIT_SOLTC_FLEXZ
             FROM  DB2PRD.PDIDO_FLEXZ_CONTR T1,
                   DB2PRD.SERVC_TARIF_PRINC T2
             WHERE
                T1.CBCO_CTA_CLI      = :GFCTB0B4.CBCO-CTA-CLI        AND
                T1.CJUNC_DEPDC       = :GFCTB0B4.CJUNC-DEPDC         AND
                T1.CCTA_CLI          = :GFCTB0B4.CCTA-CLI            AND
                T1.CIDTFD_CONTR_FLEXZ = :GFCTB0B4.CIDTFD-CONTR-FLEXZ AND
                T1.CSERVC_TARIF       = T2.CSERVC_TARIF              AND
                T2.CDEPDC             = :GFCTB0D8.CDEPDC             AND
              ((T1.CDEPDC_ORIGE_FLEXZ >= :WRK-DEP-ORIGE-INI          AND
                T1.CDEPDC_ORIGE_FLEXZ <= :WRK-DEP-ORIGE-FIM)         AND
                T1.CSIT_SOLTC_FLEXZ   IN (01 , 02))                  OR
               (T1.CSIT_SOLTC_FLEXZ    = 07                          AND
                   ((T1.CDEPDC_PAREC_FLEXZ <> 0                     AND
                     T1.CDEPDC_PAREC_FLEXZ  =
                                      :GFCTB0B4.CDEPDC-PAREC-FLEXZ)  OR
                    (T1.CDEPDC_PAREC_FLEXZ  = 0                      AND
                      T1.CDEPDC_ORIGE_FLEXZ =
                                      :GFCTB0B4.CDEPDC-ORIGE-FLEXZ)))
             ORDER BY
                T1.CSERVC_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCTS8.

       COPY I#GFCTS9.

       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTS8-ENTRADA
                                                      GFCTS9-SAIDA
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
                                          GFCTS9-SAIDA.

           INITIALIZE GFCTS9-SAIDA
                      IND-1
                      GFCT0M-AREA-ERROS.

           PERFORM 1100-VERIFICAR-DISP-SISTEMA.

           PERFORM 1200-MONTAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

0907       PERFORM 1400-ACESSA-MOD-GFCT5578.

0907       PERFORM 1500-ACESSA-MOD-GFCT5579.

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

           MOVE +100                   TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.

           MOVE 'GFCT5522'             TO WRK-MODULO.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE GFCTS8-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTS8-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTS8-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTS9-ERRO
               MOVE WRK-MODULO         TO WRK-NOME-MOD
                                          GFCT0M-PROGRAMA
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0482'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE 'GFCT0482'         TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          GFCTS9-COD-SQL-ERRO
               MOVE GFCTG1-ERRO        TO GFCTS9-ERRO
               MOVE GFCTG1-COD-MSG-ERRO
                                       TO GFCTS9-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCTS9-DESC-MSG-ERRO
                                          GFCT0M-TEXTO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0482'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTS9-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VALORIZA SAIDA                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE +387                   TO GFCTS9-LL.
           MOVE GFCTS8-ZZ              TO GFCTS9-ZZ.
           MOVE GFCTS8-TRANSACAO       TO GFCTS9-TRANSACAO.
           MOVE GFCTS8-FUNCAO          TO GFCTS9-FUNCAO.

           IF  GFCTS8-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCTS9-QTDE-OCOR
           ELSE
               MOVE GFCTS8-QTDE-OCOR   TO GFCTS9-QTDE-OCOR
           END-IF.

           IF  GFCTS8-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO GFCTS9-QTDE-TOT-REG
           ELSE
               MOVE GFCTS8-QTDE-TOT-REG
                                       TO GFCTS9-QTDE-TOT-REG
           END-IF.

           MOVE GFCTS8-FUNC-BDSCO      TO GFCTS9-FUNC-BDSCO.
           MOVE GFCTS8-FILTRO          TO GFCTS9-FILTRO.
           MOVE GFCTS8-PONTEIRO        TO GFCTS9-PONTEIRO.

           MOVE 'N'                    TO GFCTS9-FIM.
           MOVE GFCTS8-TIPO-HISTORICO  TO GFCTS9-TIPO-HISTORICO.

           MOVE ZEROS                  TO GFCTS9-ERRO
                                          GFCTS9-COD-SQL-ERRO
                                          GFCTS9-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO GFCTS9-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR DADOS                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  GFCTS8-TRANSACAO        EQUAL SPACES OR
               GFCTS8-FUNCAO           EQUAL SPACES OR
               GFCTS8-FUNC-BDSCO       EQUAL SPACES OR
               GFCTS8-CCUSTO-FIL       EQUAL SPACES OR
               GFCTS8-CONTRATO-FIL     EQUAL SPACES OR
               GFCTS8-QTDE-OCOR        NOT NUMERIC  OR
               GFCTS8-QTDE-TOT-REG     NOT NUMERIC  OR
               GFCTS8-AGENCIA-FIL      NOT NUMERIC  OR
               GFCTS8-AGENCIA-FIL      EQUAL ZEROS  OR
               GFCTS8-CONTA-FIL        NOT NUMERIC  OR
               GFCTS8-CONTA-FIL        EQUAL ZEROS  OR
               GFCTS8-AGENCIA-PONT     NOT NUMERIC  OR
               GFCTS8-CONTA-PONT       NOT NUMERIC  OR
               GFCTS8-TARIFA-PONT      NOT NUMERIC  OR
              (GFCTS8-TIPO-HISTORICO   NOT EQUAL 'T' AND 'P' AND
                                                 'D' AND 'C')
               MOVE 'S'                TO GFCTS9-FIM
               MOVE 1                  TO GFCTS9-ERRO
                                          GFCTS9-COD-MSG-ERRO
               MOVE ZEROS              TO GFCTS9-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTS9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.

           MOVE GFCTS8-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTS8-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTS8-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0482'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTS9-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTS9-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0482'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTS9-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 'S'                TO GFCTS9-FIM
               MOVE 1                  TO GFCTS9-ERRO
               MOVE 'GFCT0482'         TO GFCT0M-TRANSACAO
               MOVE  GFCTG3-COD-SQL-ERRO
                                       TO GFCTS9-COD-SQL-ERRO
               MOVE  GFCTG3-COD-MSG-ERRO
                                       TO GFCTS9-COD-MSG-ERRO
               MOVE  GFCTG3-DESC-MSG-ERRO
                                       TO GFCTS9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
0907*** ROTINA PARA ACESSAR MODULO GFCT5515 - GESTOR LOGADO.           *
0907  * ROTINA PARA ACESSAR MODULO GFCT5578 - GESTOR LOGADO.           *
      ******************************************************************
      *----------------------------------------------------------------*
       1400-ACESSA-MOD-GFCT5578        SECTION.
      *----------------------------------------------------------------*

0907       INITIALIZE                  GFCT74-GFCT5578.

           MOVE GFCTS8-AGENCIA-FIL     TO GFCT74-AGENCIA.
           MOVE GFCTS8-CONTA-FIL       TO GFCT74-CONTA.
           MOVE GFCTS8-FUNC-BDSCO      TO GFCT74-COD-FUNC.

0907       MOVE 'GFCT5578'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
0907                                         GFCT74-GFCT5578.

           IF  RETURN-CODE             NOT EQUAL ZERO
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE 'GFCT0482'         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
                                          GFCTS9-DESC-MSG-ERRO
               MOVE 9                  TO GFCTS9-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF GFCT74-COD-RETORNO        NOT EQUAL ZEROS AND 10
0907           MOVE 'GFCT5578'          TO WRK-NOME-MOD
               MOVE 1                   TO GFCTS9-ERRO
               MOVE 'S'                 TO GFCTS9-FIM

           EVALUATE GFCT74-COD-RETORNO
               WHEN 02
               MOVE 0435               TO GFCTS9-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               WHEN 03
               MOVE 0436               TO GFCTS9-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               WHEN 04
               MOVE 0437               TO GFCTS9-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               WHEN 06
               MOVE 0438               TO GFCTS9-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               WHEN 07
               MOVE 0439               TO GFCTS9-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               WHEN 08
               MOVE 0440               TO GFCTS9-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               WHEN 09
               MOVE 0441               TO GFCTS9-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               WHEN 11
               MOVE 0810               TO GFCTS9-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
           END-EVALUATE
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTS9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF

           MOVE GFCT74-DPTO                TO WRK-CODIGO-JUNCAO.
           MOVE GFCT74-TIPO-DEPENDENCIA    TO WRK-TIPO-DEPENDENCIA.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
0907***ACESSA MODULO GFCT5537                                          *
0907  *ACESSA MODULO GFCT5579                                          *
      ******************************************************************
      *----------------------------------------------------------------*
0907   1500-ACESSA-MOD-GFCT5579        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT75-ENTRADA
                                          GFCT76-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCT75-ENTRADA
                                       GFCT76-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE +100                   TO GFCT75-LL.
           MOVE ZEROS                  TO GFCT75-ZZ.
           MOVE GFCTS8-TRANSACAO       TO GFCT75-TRANSACAO.
           MOVE GFCTS8-FUNCAO          TO GFCT75-FUNCAO.
           MOVE GFCTS8-QTDE-OCOR       TO GFCT75-QTDE-OCOR.
           MOVE GFCTS8-QTDE-TOT-REG    TO GFCT75-QTDE-TOT-REG.
           MOVE WRK-CODIGO-JUNCAO      TO GFCT75-CJUNC-DEPDC-FUNC.
           MOVE WRK-TIPO-DEPENDENCIA   TO GFCT75-CIDTFD-DEPDC-FUNC.

           IF  GFCT75-CIDTFD-DEPDC-FUNC
                                       EQUAL 'GM'
               MOVE 'S'                TO GFCT75-MASTER
           ELSE
               MOVE 'N'                TO GFCT75-MASTER
           END-IF.

           MOVE GFCTS8-FUNC-BDSCO      TO GFCT75-FUNC-BDSCO.
           MOVE GFCTS8-AGENCIA-FIL     TO GFCT75-AGENCIA-FL.
           MOVE GFCTS8-CONTA-FIL       TO GFCT75-CONTA-FL.
0907       MOVE 'GFCT5579'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT75-ENTRADA
                                             GFCT76-SAIDA
                                             GFCT0M-AREA-ERROS
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.

           IF  GFCT76-ERRO             EQUAL 9
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0482'         TO GFCT0M-PROGRAMA
0907           MOVE 'GFCT5579'         TO WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
                                          GFCTS9-DESC-MSG-ERRO
               MOVE 9                  TO GFCTS9-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.


           IF  GFCT76-ERRO             NOT EQUAL ZEROS
0907           MOVE 'GFCT5579'         TO WRK-NOME-MOD
               MOVE 'S'                TO GFCTS9-FIM
               MOVE GFCT76-ERRO           TO GFCTS9-ERRO
               MOVE GFCT76-COD-SQL-ERRO   TO GFCTS9-COD-SQL-ERRO
               MOVE GFCT76-COD-MSG-ERRO   TO GFCTS9-COD-MSG-ERRO
               MOVE GFCT76-DESC-MSG-ERRO  TO GFCTS9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCT76-CIDTFD-DEPDC-FUNC EQUAL 'AG'
               MOVE GFCT76-CJUNC-DEPDC-FUNC
                                       TO GFCTS8-AGENCIA-FIL
                                          WRK-CODIGO-JUNCAO
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-EFETUAR-ACESSOS.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUAR ACESSOS DB2                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-EFETUAR-ACESSOS            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2110-ABRIR-CURSORES.

           MOVE 1                      TO IND-1.

           PERFORM 2130-PROCESSOS-GERAIS
               UNTIL IND-1             GREATER 7 OR
                   WRK-FIM-CRS         EQUAL 'S'.

           PERFORM 2140-FECHAR-CURSORES.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSORES                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-ABRIR-CURSORES             SECTION.
      *----------------------------------------------------------------*

           PERFORM 2111-VALORIZAR-CAMPOS.

           MOVE 'N'                        TO WRK-FIM-CRS.

           IF  WRK-TIPO-DEPENDENCIA        EQUAL 'DP'
               MOVE WRK-CODIGO-JUNCAO      TO CDEPDC OF GFCTB0D8

               EVALUATE GFCTS8-TIPO-HISTORICO
                 WHEN 'T'
                   MOVE 05                 TO WRK-SITUACAO
                   PERFORM 2112-ABRIR-CURSOR-05
                 WHEN 'P'
                   MOVE 06                 TO WRK-SITUACAO
                   PERFORM 2113-ABRIR-CURSOR-06
                 WHEN 'D'
                   MOVE 07                 TO WRK-SITUACAO
                   PERFORM 2114-ABRIR-CURSOR-07
                 WHEN OTHER
                   MOVE 08                 TO WRK-SITUACAO
                   PERFORM 2115-ABRIR-CURSOR-08
               END-EVALUATE

           ELSE

               EVALUATE GFCTS8-TIPO-HISTORICO
                 WHEN 'T'
                   MOVE 01                 TO WRK-SITUACAO
                   PERFORM 2116-ABRIR-CURSOR-01
                 WHEN 'P'
                   MOVE 02                 TO WRK-SITUACAO
                   PERFORM 2117-ABRIR-CURSOR-02
                 WHEN 'D'
                   MOVE 03                 TO WRK-SITUACAO
                   PERFORM 2118-ABRIR-CURSOR-03
                 WHEN OTHER
                   MOVE 04                 TO WRK-SITUACAO
                   PERFORM 2119-ABRIR-CURSOR-04
               END-EVALUATE

           END-IF.

           PERFORM 2120-DESPREZAR-JA-ENVIADOS.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VALORIZAR CAMPOS                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2111-VALORIZAR-CAMPOS           SECTION.
      *----------------------------------------------------------------*

           MOVE 237                     TO CBCO-CTA-CLI   OF GFCTB0B4.
           MOVE GFCTS8-AGENCIA-FIL      TO CJUNC-DEPDC    OF GFCTB0B4.
           MOVE GFCTS8-CONTA-FIL        TO CCTA-CLI       OF GFCTB0B4.
           MOVE GFCTS8-CONTRATO-FIL     TO CIDTFD-CONTR-FLEXZ
                                        OF GFCTB0B4.
           MOVE GFCTS8-CCUSTO-FIL       TO CROTNA-ORIGE-CONTR
                                        OF GFCTB0B4.

0907       IF  WRK-TIPO-DEPENDENCIA     EQUAL 'GM' OR 'DA'
               MOVE 00000               TO WRK-DEP-ORIGE-INI
               MOVE 99999               TO WRK-DEP-ORIGE-FIM
           ELSE
               MOVE WRK-CODIGO-JUNCAO   TO WRK-DEP-ORIGE-INI
               MOVE WRK-CODIGO-JUNCAO   TO WRK-DEP-ORIGE-FIM
           END-IF.

           MOVE WRK-CODIGO-JUNCAO       TO CDEPDC-ORIGE-FLEXZ
                                                          OF GFCTB0B4.

           MOVE WRK-CODIGO-JUNCAO       TO CDEPDC-PAREC-FLEXZ
                                                          OF GFCTB0B4.

      *----------------------------------------------------------------*
       2111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR 05                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2112-ABRIR-CURSOR-05            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN  CSR05-GFCTB0B4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '         TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2112-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR 06                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2113-ABRIR-CURSOR-06            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN  CSR06-GFCTB0B4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '         TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2113-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR 07                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2114-ABRIR-CURSOR-07            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN  CSR07-GFCTB0B4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '         TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2114-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR 08                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2115-ABRIR-CURSOR-08            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN  CSR08-GFCTB0B4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '         TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2115-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR 01                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2116-ABRIR-CURSOR-01            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN  CSR01-GFCTB0B4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '         TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2116-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR 02                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2117-ABRIR-CURSOR-02            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN  CSR02-GFCTB0B4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '         TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2117-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR 03                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2118-ABRIR-CURSOR-03            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN  CSR03-GFCTB0B4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '         TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2118-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR 04                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2119-ABRIR-CURSOR-04            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN  CSR04-GFCTB0B4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '         TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2119-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *DESPREZAR REGISTROS JA ENVIADOS                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-DESPREZAR-JA-ENVIADOS      SECTION.
      *----------------------------------------------------------------*

           COMPUTE WRK-DESPREZA        = GFCTS8-QTDE-OCOR + 1.

           PERFORM 2121-LER-CURSORES
               VARYING IND-1           FROM 1 BY 1
                UNTIL IND-1            GREATER WRK-DESPREZA OR
                WRK-FIM-CRS            EQUAL 'S'.

           IF  WRK-FIM-CRS             EQUAL 'S'
               MOVE 'S'                TO GFCTS9-FIM
               MOVE  ZEROS             TO GFCTS9-ERRO
                                          GFCTS9-COD-SQL-ERRO
               EVALUATE WRK-SITUACAO
                   WHEN 01
                   WHEN 04
                       MOVE 0827       TO GFCTS9-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   WHEN 02
                   WHEN 05
                       MOVE 0828       TO GFCTS9-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   WHEN 03
                   WHEN 06
                       MOVE 0829       TO GFCTS9-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               END-EVALUATE
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTS9-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSORES                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2121-LER-CURSORES               SECTION.
      *----------------------------------------------------------------*

           EVALUATE WRK-SITUACAO

               WHEN 01
               PERFORM 2122-LER-CURSOR-01

               WHEN 02
               PERFORM 2123-LER-CURSOR-02

               WHEN 03
               PERFORM 2124-LER-CURSOR-03

               WHEN 04
               PERFORM 2125-LER-CURSOR-04

               WHEN 05
               PERFORM 2126-LER-CURSOR-05

               WHEN 06
               PERFORM 2127-LER-CURSOR-06

               WHEN 07
               PERFORM 2128-LER-CURSOR-07

               WHEN 08
               PERFORM 2129-LER-CURSOR-08

           END-EVALUATE.

      *----------------------------------------------------------------*
       2121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR 01                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2122-LER-CURSOR-01              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-GFCTB0B4  INTO
                   :GFCTB0B4.CJUNC-DEPDC,
                   :GFCTB0B4.CCTA-CLI,
                   :GFCTB0B4.CROTNA-ORIGE-CONTR,
                   :GFCTB0B4.CIDTFD-CONTR-FLEXZ,
                   :GFCTB0B4.CSERVC-TARIF,
                   :GFCTB0B4.HIDTFD-FLEXZ-CONTR,
                   :GFCTB0B4.VNEGOC-FLEXZ-CONTR,
                   :GFCTB0B4.PDESC-FLEXZ-CONTR,
                   :GFCTB0B4.QDIA-FLEXZ-CONTR,
                   :GFCTB0B4.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B4.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B4.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B4.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B4.CSIT-SOLTC-FLEXZ
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '         TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CRS
                                          GFCTS9-FIM
           END-IF.

      *----------------------------------------------------------------*
       2122-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR 02                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2123-LER-CURSOR-02              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR02-GFCTB0B4  INTO
                   :GFCTB0B4.CJUNC-DEPDC,
                   :GFCTB0B4.CCTA-CLI,
                   :GFCTB0B4.CROTNA-ORIGE-CONTR,
                   :GFCTB0B4.CIDTFD-CONTR-FLEXZ,
                   :GFCTB0B4.CSERVC-TARIF,
                   :GFCTB0B4.HIDTFD-FLEXZ-CONTR,
                   :GFCTB0B4.VNEGOC-FLEXZ-CONTR,
                   :GFCTB0B4.PDESC-FLEXZ-CONTR,
                   :GFCTB0B4.QDIA-FLEXZ-CONTR,
                   :GFCTB0B4.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B4.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B4.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B4.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B4.CSIT-SOLTC-FLEXZ
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '         TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CRS
                                          GFCTS9-FIM
           END-IF.

      *----------------------------------------------------------------*
       2123-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR 03                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2124-LER-CURSOR-03              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR03-GFCTB0B4  INTO
                   :GFCTB0B4.CJUNC-DEPDC,
                   :GFCTB0B4.CCTA-CLI,
                   :GFCTB0B4.CROTNA-ORIGE-CONTR,
                   :GFCTB0B4.CIDTFD-CONTR-FLEXZ,
                   :GFCTB0B4.CSERVC-TARIF,
                   :GFCTB0B4.HIDTFD-FLEXZ-CONTR,
                   :GFCTB0B4.VNEGOC-FLEXZ-CONTR,
                   :GFCTB0B4.PDESC-FLEXZ-CONTR,
                   :GFCTB0B4.QDIA-FLEXZ-CONTR,
                   :GFCTB0B4.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B4.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B4.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B4.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B4.CSIT-SOLTC-FLEXZ
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '         TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CRS
                                          GFCTS9-FIM
           END-IF.

      *----------------------------------------------------------------*
       2124-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR 04                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2125-LER-CURSOR-04              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR04-GFCTB0B4  INTO
                   :GFCTB0B4.CJUNC-DEPDC,
                   :GFCTB0B4.CCTA-CLI,
                   :GFCTB0B4.CROTNA-ORIGE-CONTR,
                   :GFCTB0B4.CIDTFD-CONTR-FLEXZ,
                   :GFCTB0B4.CSERVC-TARIF,
                   :GFCTB0B4.HIDTFD-FLEXZ-CONTR,
                   :GFCTB0B4.VNEGOC-FLEXZ-CONTR,
                   :GFCTB0B4.PDESC-FLEXZ-CONTR,
                   :GFCTB0B4.QDIA-FLEXZ-CONTR,
                   :GFCTB0B4.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B4.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B4.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B4.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B4.CSIT-SOLTC-FLEXZ
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '         TO GFCT0M-COMANDO-SQL
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CRS
                                          GFCTS9-FIM
           END-IF.

      *----------------------------------------------------------------*
       2125-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR 05                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2126-LER-CURSOR-05              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR05-GFCTB0B4  INTO
                   :GFCTB0B4.CJUNC-DEPDC,
                   :GFCTB0B4.CCTA-CLI,
                   :GFCTB0B4.CROTNA-ORIGE-CONTR,
                   :GFCTB0B4.CIDTFD-CONTR-FLEXZ,
                   :GFCTB0B4.CSERVC-TARIF,
                   :GFCTB0B4.HIDTFD-FLEXZ-CONTR,
                   :GFCTB0B4.VNEGOC-FLEXZ-CONTR,
                   :GFCTB0B4.PDESC-FLEXZ-CONTR,
                   :GFCTB0B4.QDIA-FLEXZ-CONTR,
                   :GFCTB0B4.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B4.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B4.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B4.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B4.CSIT-SOLTC-FLEXZ
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '         TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CRS
                                          GFCTS9-FIM
           END-IF.

      *----------------------------------------------------------------*
       2126-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR 06                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2127-LER-CURSOR-06              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR06-GFCTB0B4  INTO
                   :GFCTB0B4.CJUNC-DEPDC,
                   :GFCTB0B4.CCTA-CLI,
                   :GFCTB0B4.CROTNA-ORIGE-CONTR,
                   :GFCTB0B4.CIDTFD-CONTR-FLEXZ,
                   :GFCTB0B4.CSERVC-TARIF,
                   :GFCTB0B4.HIDTFD-FLEXZ-CONTR,
                   :GFCTB0B4.VNEGOC-FLEXZ-CONTR,
                   :GFCTB0B4.PDESC-FLEXZ-CONTR,
                   :GFCTB0B4.QDIA-FLEXZ-CONTR,
                   :GFCTB0B4.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B4.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B4.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B4.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B4.CSIT-SOLTC-FLEXZ
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '         TO GFCT0M-COMANDO-SQL
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CRS
                                          GFCTS9-FIM
           END-IF.

      *----------------------------------------------------------------*
       2127-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR 07                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2128-LER-CURSOR-07              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR07-GFCTB0B4  INTO
                   :GFCTB0B4.CJUNC-DEPDC,
                   :GFCTB0B4.CCTA-CLI,
                   :GFCTB0B4.CROTNA-ORIGE-CONTR,
                   :GFCTB0B4.CIDTFD-CONTR-FLEXZ,
                   :GFCTB0B4.CSERVC-TARIF,
                   :GFCTB0B4.HIDTFD-FLEXZ-CONTR,
                   :GFCTB0B4.VNEGOC-FLEXZ-CONTR,
                   :GFCTB0B4.PDESC-FLEXZ-CONTR,
                   :GFCTB0B4.QDIA-FLEXZ-CONTR,
                   :GFCTB0B4.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B4.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B4.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B4.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B4.CSIT-SOLTC-FLEXZ
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '         TO GFCT0M-COMANDO-SQL
               MOVE '0150'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CRS
                                          GFCTS9-FIM
           END-IF.

      *----------------------------------------------------------------*
       2128-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR 08                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2129-LER-CURSOR-08              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR08-GFCTB0B4  INTO
                   :GFCTB0B4.CJUNC-DEPDC,
                   :GFCTB0B4.CCTA-CLI,
                   :GFCTB0B4.CROTNA-ORIGE-CONTR,
                   :GFCTB0B4.CIDTFD-CONTR-FLEXZ,
                   :GFCTB0B4.CSERVC-TARIF,
                   :GFCTB0B4.HIDTFD-FLEXZ-CONTR,
                   :GFCTB0B4.VNEGOC-FLEXZ-CONTR,
                   :GFCTB0B4.PDESC-FLEXZ-CONTR,
                   :GFCTB0B4.QDIA-FLEXZ-CONTR,
                   :GFCTB0B4.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B4.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B4.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B4.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B4.CSIT-SOLTC-FLEXZ
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '         TO GFCT0M-COMANDO-SQL
               MOVE '0160'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CRS
                                          GFCTS9-FIM
           END-IF.

      *----------------------------------------------------------------*
       2129-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO GERAL                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2130-PROCESSOS-GERAIS           SECTION.
      *----------------------------------------------------------------*

           PERFORM 2131-CONSISTE-DEPTO-E-DEFER.

           PERFORM 2121-LER-CURSORES.

           IF  IND-1                   GREATER 7
               MOVE 'S'                TO WRK-FIM-CRS
               MOVE CJUNC-DEPDC        OF GFCTB0B4
                                       TO GFCTS9-AGENCIA-PONT
               MOVE CCTA-CLI           OF GFCTB0B4
                                       TO GFCTS9-CONTA-PONT
               MOVE CIDTFD-CONTR-FLEXZ OF GFCTB0B4
                                       TO GFCTS9-CONTRATO-PONT
               MOVE CROTNA-ORIGE-CONTR OF GFCTB0B4
                                       TO GFCTS9-CCUSTO-PONT
               MOVE CSERVC-TARIF       OF GFCTB0B4
                                       TO GFCTS9-TARIFA-PONT
               MOVE HIDTFD-FLEXZ-CONTR OF GFCTB0B4
                                       TO GFCTS9-TIMESTAMP-PONT
           END-IF.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTENCIA DE DEPARTAMENTO E DEFERIDOS                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2131-CONSISTE-DEPTO-E-DEFER     SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTS8-TIPO-HISTORICO
                 WHEN 'D'
                   PERFORM 2134-PESQ-TAB-QUENTE
                 WHEN OTHER
                   PERFORM 2132-MONTAR-OCORR
           END-EVALUATE.

      *----------------------------------------------------------------*
       2131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTA AS OCORRENCIAS                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2132-MONTAR-OCORR               SECTION.
      *----------------------------------------------------------------*

           ADD 89                      TO GFCTS9-LL.

           ADD 1                       TO GFCTS9-QTDE-TOT-REG.

           MOVE CSERVC-TARIF           OF GFCTB0B4
                                       TO GFCTS9-TARIFA-LISTA(IND-1).
           MOVE HIDTFD-FLEXZ-CONTR     OF GFCTB0B4
                                       TO GFCTS9-TIMESTAMP(IND-1).
           MOVE VNEGOC-FLEXZ-CONTR     OF GFCTB0B4
                                       TO GFCTS9-VR-NEGOCIADO(IND-1).
           MOVE PDESC-FLEXZ-CONTR      OF GFCTB0B4
                                       TO GFCTS9-PERC-DESCTO(IND-1).
           MOVE QDIA-FLEXZ-CONTR       OF GFCTB0B4
                                       TO GFCTS9-DIAS-RETENCAO(IND-1).
           MOVE CSIT-SOLTC-FLEXZ       OF GFCTB0B4
                                       TO GFCTS9-SITUACAO(IND-1).
           MOVE CDEPDC-ORIGE-FLEXZ     OF GFCTB0B4
                                       TO GFCTS9-DEPEND-SOLIC(IND-1).

           PERFORM 2133-PESQUISAR-NOME-TARIF.

           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTS9-NOME-TARIFA(IND-1).

           EVALUATE CSIT-SOLTC-FLEXZ   OF GFCTB0B4

               WHEN 1
               MOVE 'PED'              TO GFCTS9-DESC-SITUACAO(IND-1)

               WHEN 2
               MOVE 'DEF'              TO GFCTS9-DESC-SITUACAO(IND-1)

               WHEN 3
               WHEN 4
               MOVE 'IND'              TO GFCTS9-DESC-SITUACAO(IND-1)

               WHEN 5
               WHEN 6
               MOVE 'CAN'              TO GFCTS9-DESC-SITUACAO(IND-1)

               WHEN OTHER
               MOVE 'PAS'              TO GFCTS9-DESC-SITUACAO(IND-1)

           END-EVALUATE.

           ADD 1                       TO IND-1.

      *----------------------------------------------------------------*
       2132-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PESQUISA NOME TARIFA                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2133-PESQUISAR-NOME-TARIF       SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0B4 TO CSERVC-TARIF OF GFCTB0D8.


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
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D8'      TO WRK-NOME-TAB
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0170'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2133-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PESQUISA TABELA QUENTE                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2134-PESQ-TAB-QUENTE            SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CBCO-CTA-CLI     OF GFCTB059.
           MOVE  CJUNC-DEPDC           OF GFCTB0B4
                                       TO CDEPDC-CTA-CLI   OF GFCTB059.
           MOVE  CCTA-CLI              OF GFCTB0B4
                                       TO CNRO-CTA-CLI     OF GFCTB059.
           MOVE  CROTNA-ORIGE-CONTR    OF GFCTB0B4
                                       TO CROTNA-ORIGE-CONTR
                                       OF GFCTB059.
           MOVE  CIDTFD-CONTR-FLEXZ    OF GFCTB0B4
                                       TO CIDTFD-CONTR-FLEXZ
                                       OF GFCTB059.
           MOVE  CSERVC-TARIF          OF GFCTB0B4
                                       TO CSERVC-TARIF     OF GFCTB059.
           MOVE  HIDTFD-FLEXZ-CONTR    OF GFCTB0B4
                                       TO HIDTFD-FLEXZ-CONTR
                                       OF GFCTB059.
           MOVE '01.01.3333'           TO DUTILZ-FLEXZ-CONTR
                                       OF GFCTB059.

           EXEC SQL
             SELECT
                   CBCO_CTA_CLI,
                   CDEPDC_CTA_CLI,
                   CNRO_CTA_CLI,
                   CSERVC_TARIF,
                   CROTNA_ORIGE_CONTR,
                   CIDTFD_CONTR_FLEXZ,
                   HIDTFD_FLEXZ_CONTR,
                   VNEGOC_FLEXZ_CONTR,
                   PDESC_FLEXZ_CONTR,
                   QDIA_FLEXZ_CONTR,
                   DUTILZ_FLEXZ_CONTR
             INTO
                   :GFCTB059.CBCO-CTA-CLI,
                   :GFCTB059.CDEPDC-CTA-CLI,
                   :GFCTB059.CNRO-CTA-CLI,
                   :GFCTB059.CSERVC-TARIF,
                   :GFCTB059.CROTNA-ORIGE-CONTR,
                   :GFCTB059.CIDTFD-CONTR-FLEXZ,
                   :GFCTB059.HIDTFD-FLEXZ-CONTR,
                   :GFCTB059.VNEGOC-FLEXZ-CONTR,
                   :GFCTB059.PDESC-FLEXZ-CONTR,
                   :GFCTB059.QDIA-FLEXZ-CONTR,
                   :GFCTB059.DUTILZ-FLEXZ-CONTR
             FROM   DB2PRD.FLEXZ_NRO_CONTR
                WHERE  CBCO_CTA_CLI       = :GFCTB059.CBCO-CTA-CLI
                 AND   CDEPDC_CTA_CLI     = :GFCTB059.CDEPDC-CTA-CLI
                 AND   CNRO_CTA_CLI       = :GFCTB059.CNRO-CTA-CLI
                 AND   CROTNA_ORIGE_CONTR = :GFCTB059.CROTNA-ORIGE-CONTR
                 AND   CSERVC_TARIF       = :GFCTB059.CSERVC-TARIF
                 AND   HIDTFD_FLEXZ_CONTR = :GFCTB059.HIDTFD-FLEXZ-CONTR
                 AND   DUTILZ_FLEXZ_CONTR = :GFCTB059.DUTILZ-FLEXZ-CONTR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                       AND -811) OR
              (SQLWARN0                EQUAL 'W' )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB059'      TO WRK-NOME-TAB
               MOVE 'FLEXZ_NRO_CONTR'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0180'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 NOT EQUAL +100
               PERFORM 2132-MONTAR-OCORR
           END-IF.

      *----------------------------------------------------------------*
       2134-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSORES                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2140-FECHAR-CURSORES            SECTION.
      *----------------------------------------------------------------*

           EVALUATE WRK-SITUACAO

               WHEN 01
               PERFORM 2141-FECHAR-CURSOR-01

               WHEN 02
               PERFORM 2142-FECHAR-CURSOR-02

               WHEN 03
               PERFORM 2143-FECHAR-CURSOR-03

               WHEN 04
               PERFORM 2144-FECHAR-CURSOR-04

               WHEN 05
               PERFORM 2145-FECHAR-CURSOR-05

               WHEN 06
               PERFORM 2146-FECHAR-CURSOR-06

               WHEN 07
               PERFORM 2147-FECHAR-CURSOR-07

               WHEN 08
               PERFORM 2148-FECHAR-CURSOR-08

           END-EVALUATE.

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR 01                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2141-FECHAR-CURSOR-01           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0B4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)  OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0190'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2141-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR 02                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2142-FECHAR-CURSOR-02           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR02-GFCTB0B4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)  OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0200'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2142-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR 03                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2143-FECHAR-CURSOR-03           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR03-GFCTB0B4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)  OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0210'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2143-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR 04                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2144-FECHAR-CURSOR-04           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR04-GFCTB0B4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)  OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0220'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2144-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR 05                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2145-FECHAR-CURSOR-05           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR05-GFCTB0B4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)  OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0230'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2145-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR 06                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2146-FECHAR-CURSOR-06           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR06-GFCTB0B4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)  OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0240'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2146-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR 07                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2147-FECHAR-CURSOR-07           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR07-GFCTB0B4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)  OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0250'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2147-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR 08                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2148-FECHAR-CURSOR-08           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR08-GFCTB0B4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)  OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B4'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CONTR' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0260'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTS9-COD-MSG-ERRO
               PERFORM 2999-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2148-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA MOVER ERRO SQL                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2999-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE  9                     TO GFCTS9-ERRO.
           MOVE 'GFCT0428'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
                                          GFCTS9-COD-SQL-ERRO.
           MOVE SPACES                 TO GFCTS9-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG      WRK-NOME-TAB

           DELIMITED BY '  '           INTO GFCTS9-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       2999-99-FIM.                    EXIT.
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
