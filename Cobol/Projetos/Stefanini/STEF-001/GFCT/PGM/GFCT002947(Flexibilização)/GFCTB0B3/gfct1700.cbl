      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1700.
       AUTHOR.     ALLAN POLETTO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1700                                    *
      *    PROGRAMADOR.:   ALLAN POLETTO           - CPM PATO BRANCO   *
      *    ANALISTA CPM:   JOSSIANE CARLA GAZZONI  - CPM PATO BRANCO   *
      *    ANALISTA....:   MARIA V. S. TORQUATO    - PROCWORK GP/50    *
      *    DATA........:   14/06/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GRAVACAO DE ARQUIVO COM OS PEDIDOS DEFERIDOS*
      *      DE FLEXIBILIZACAO POR AGRUPAMENTO.                        *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    DEFERIDO                  I#GFCTT3          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                          INCLUDE/BOOK   *
      *                    DB2PRD.PARM_DATA_PROCM         GFCTB0A1     *
      *                    DB2PRD.PDIDO_FLEXZ_AG          GFCTB0B1     *
      *                    DB2PRD.PDIDO_FLEXZ_AGPTO       GFCTB0B2     *
      *                    DB2PRD.PDIDO_FLEXZ_CLI         GFCTB0B3     *
      *                    DB2PRD.PDIDO_FLEXZ_EMPR        GFCTB0B6     *
      *                    DB2PRD.PDIDO_FLEXZ_MUN         GFCTB0B7     *
      *                    DB2PRD.PDIDO_FLEXZ_PAB         GFCTB0B8     *
      *                    DB2PRD.PDIDO_FLEXZ_PSTAL       GFCTB0B9     *
      *                    DB2PRD.PDIDO_FLEXZ_SGMTO       GFCTB0C0     *
      *                    DB2PRD.PDIDO_FLEXZ_UF          GFCTB0C1     *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C  -  AREA PARA TRATAMENTO DE ERRO PELA POOL7100.    *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100  -  TRATAMENTO DE ERRO QUANDO PROGRAMA INVALIDO    *
      *    POOL1050  -  CONEXAO COM DB2.                               *
      *================================================================*
      *----------------------------------------------------------------*
      *    ULTIMA ALTERACAO - VALERIA MARCO/2007                       *
      *    ALTERACAO NO WHERE DO CURSOR PARA SELECIONAR FLEXIBILIZACOES*
      *    COM DATA DE INICIO EM FINAL DE SEMANA.                      *
      *================================================================*
080826*----------------------------------------------------------------*
080826*    ULTIMA ALTERACAO - VINICIUS CRISTIAN MADUREIRA - SONDPROCW  *
080826*    INCLUSAO DE CAMPOS NA TABELA B2 E ALTERACAO DA INC GFCTT3   *
080826*================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT DEFERIDO ASSIGN      TO UT-S-DEFERIDO
           FILE STATUS                 IS WRK-FS-DEFERIDO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE FLEXIBILIZACOES DEFERIDAS                *
      *            ORG. SEQUENCIAL     -   LRECL   =   400             *
      *----------------------------------------------------------------*

       FD  DEFERIDO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTT3'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-GRAVADOS                PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC  ZZZZ.ZZZ.999   VALUE ZEROS.
       77  WRK-FIM-CURSOR              PIC  X(001)         VALUE SPACES.

       01  WRK-TIMESTAMP.
           05 WRK-ANO                  PIC  9(004)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE SPACES.
           05 WRK-MES                  PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE SPACES.
           05 WRK-DIA                  PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-AUX.
           05 WRK-DIA                  PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-MES                  PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-ANO                  PIC  9(004)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTE DE FILE-STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-FS-DEFERIDO             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** MENSAGENS DE ERRO ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-ARQUIVO.
           05  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQUIVO        PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(018)         VALUE
               ' - FILE STATUS =  '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7100 ***'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
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
               INCLUDE GFCTB0B6
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
               INCLUDE GFCTB0M7
           END-EXEC.

           EXEC SQL
             DECLARE CSR01-GFCTB0B2 CURSOR FOR
               SELECT
               CAGPTO_CTA,
               CSERVC_TARIF,
               HIDTFD_FLEXZ_AGPTO,
               CFUNC_SOLTC_FLEXZ,
               CFUNC_PAREC_FLEXZ,
               CTPO_OPER_MOTVO,
               CMOTVO_JUSTF,
               CEMPR_ORIGE_FLEXZ,
               CDEPDC_ORIGE_FLEXZ,
               CEMPR_PAREC_FLEXZ,
               CDEPDC_PAREC_FLEXZ,
               DINIC_FLEXZ_AGPTO,
               DFIM_FLEXZ_AGPTO,
               QFRANQ_FLEXZ_AGPTO,
               CPER_FRANQ_AGPTO,
               VNEGOC_FLEXZ_AGPTO,
               PDESC_FLEXZ_AGPTO,
               DDIA_AGNDA,
               QDIA_FLEXZ_AGPTO,
               HSOLTC_FLEXZ,
               RJUSTF_SOLTC_FLEXZ,
               HPAREC_FLEXZ,
               RJUSTF_PAREC_FLEXZ,
               CSIT_SOLTC_FLEXZ,
               CINDCD_AGPTO_TOT,
               CINIC_PER_FRANQ,
               CFUNC_EXCL_SIST,
080826         CPER_COBR_TARIF,
080826         CINIC_PER_COBR,
LFM            CCJTO_SERVC
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO
             WHERE
                   CSIT_SOLTC_FLEXZ   =  :GFCTB0B2.CSIT-SOLTC-FLEXZ AND
                   DINIC_FLEXZ_AGPTO  >  :GFCTB0A1.DPROCM-ATUAL     AND
                   DINIC_FLEXZ_AGPTO  <= :GFCTB0A1.DPROCM-PROX
             ORDER BY
                   CAGPTO_CTA,
                   CSERVC_TARIF,
                   HIDTFD_FLEXZ_AGPTO,
                   DINIC_FLEXZ_AGPTO
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      * INICIO DO PROCESSAMENTO DO PROGRAMA                            *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
               WRK-FIM-CURSOR          EQUAL 'S'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCEDIMENTOS INICIAIS DO PROGRAMA.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN OUTPUT DEFERIDO.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FS-DEFERIDO.

           PERFORM 1200-OBTER-DATA-PROC.

           PERFORM 1300-ABRIR-CURSOR.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTAR FILE-STATUS DO ARQUIVO DEFERIDO.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FS-DEFERIDO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DEFERIDO         NOT EQUAL '00'
               MOVE 'DEFERIDO'         TO WRK-NOME-ARQUIVO
               MOVE WRK-FS-DEFERIDO    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DATA DE PROCESSAMENTO ATUAL.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-OBTER-DATA-PROC            SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL,
                   DPROCM_PROX
             INTO
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ABRIR CURSOR DA TABELA GFCTB0B2.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CURSOR.
           MOVE DPROCM-PROX            OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO
                                       OF GFCTB0B2.
           MOVE 02                     TO CSIT-SOLTC-FLEXZ OF GFCTB0B2.

           EXEC SQL
               OPEN CSR01-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO ERR-DBD-TAB
               MOVE 'OPEN'              TO ERR-FUN-COMANDO
               MOVE  SQLCODE            TO ERR-SQL-CODE
               MOVE '0020'              TO ERR-LOCAL
               MOVE  SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR SE HA REGISTROS QUE ATENDAM A CONDICAO DO CURSOR.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-CURSOR.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LEITURA DO CURSOR DA TABELA GFCTB0B2.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-GFCTB0B2 INTO
                   :GFCTB0B2.CAGPTO-CTA,
                   :GFCTB0B2.CSERVC-TARIF,
                   :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B2.CFUNC-SOLTC-FLEXZ,
                   :GFCTB0B2.CFUNC-PAREC-FLEXZ,
                   :GFCTB0B2.CTPO-OPER-MOTVO,
                   :GFCTB0B2.CMOTVO-JUSTF,
                   :GFCTB0B2.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B2.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B2.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B2.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB0B2.CPER-FRANQ-AGPTO,
                   :GFCTB0B2.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB0B2.PDESC-FLEXZ-AGPTO,
                   :GFCTB0B2.DDIA-AGNDA,
                   :GFCTB0B2.QDIA-FLEXZ-AGPTO,
                   :GFCTB0B2.HSOLTC-FLEXZ,
                   :GFCTB0B2.RJUSTF-SOLTC-FLEXZ,
                   :GFCTB0B2.HPAREC-FLEXZ,
                   :GFCTB0B2.RJUSTF-PAREC-FLEXZ,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
                   :GFCTB0B2.CINDCD-AGPTO-TOT,
                   :GFCTB0B2.CINIC-PER-FRANQ,
                   :GFCTB0B2.CFUNC-EXCL-SIST,
080826             :GFCTB0B2.CPER-COBR-TARIF,
080826             :GFCTB0B2.CINIC-PER-COBR,
LFM                :GFCTB0B2.CCJTO-SERVC
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO ERR-DBD-TAB
               MOVE 'FETCH'             TO ERR-FUN-COMANDO
               MOVE  SQLCODE            TO ERR-SQL-CODE
               MOVE '0030'              TO ERR-LOCAL
               MOVE  SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO PRINCIPAL DO PROGRAMA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCTT3-REGISTRO.

           MOVE SPACES                 TO GFCTT3-REGISTRO.
           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO GFCTT3-CSERVC-TARIF.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO GFCTT3-CAGPTO-CTA.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO GFCTT3-HIDTFD-FLEXZ-AGPTO.
           MOVE DINIC-FLEXZ-AGPTO      OF GFCTB0B2
                                       TO GFCTT3-DINIC-FLEXZ-AGPTO.
           MOVE DFIM-FLEXZ-AGPTO       OF GFCTB0B2
                                       TO GFCTT3-DFIM-FLEXZ-AGPTO.
           MOVE QFRANQ-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO GFCTT3-QFRANQ-FLEXZ-AGPTO.
           MOVE CPER-FRANQ-AGPTO       OF GFCTB0B2
                                       TO GFCTT3-CPER-FRANQ-AGPTO.
           MOVE VNEGOC-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO GFCTT3-VNEGOC-FLEXZ-AGPTO.
           MOVE PDESC-FLEXZ-AGPTO      OF GFCTB0B2
                                       TO GFCTT3-PDESC-FLEXZ-AGPTO.
           MOVE DDIA-AGNDA             OF GFCTB0B2
                                       TO GFCTT3-DDIA-AGNDA.
           MOVE QDIA-FLEXZ-AGPTO       OF GFCTB0B2
                                       TO GFCTT3-QDIA-FLEXZ-AGPTO.
           MOVE CINIC-PER-FRANQ        OF GFCTB0B2
                                       TO GFCTT3-CINIC-PER-FRANQ.
           MOVE CINDCD-AGPTO-TOT       OF GFCTB0B2
                                       TO GFCTT3-CINDCD-AGPTO-TOT.
           MOVE CTPO-OPER-MOTVO        OF GFCTB0B2
                                       TO GFCTT3-CTPO-OPER-MOTVO.
           MOVE CMOTVO-JUSTF           OF GFCTB0B2
                                       TO GFCTT3-CMOTVO-JUSTF.
           MOVE CDEPDC-ORIGE-FLEXZ     OF GFCTB0B2
                                       TO GFCTT3-CDEPDC-ORIGE-FLEXZ.
           MOVE CDEPDC-PAREC-FLEXZ     OF GFCTB0B2
                                       TO GFCTT3-CDEPDC-PAREC-FLEXZ.
           MOVE ZEROS                  TO GFCTT3-CDEPDC-ENCRR-VGCIA
                                          GFCTT3-CFUNC-ENCRR-VGCIA.
           MOVE '0001-01-01-01.01.01.000001'
                                       TO GFCTT3-HVGCIA-REGRA.

           MOVE HSOLTC-FLEXZ           OF GFCTB0B2
                                       TO WRK-TIMESTAMP.
           MOVE CORR WRK-TIMESTAMP     TO WRK-DATA-AUX.
           MOVE WRK-DATA-AUX           TO GFCTT3-DATA-SOLTC-FLEXZ.

           MOVE HPAREC-FLEXZ           OF GFCTB0B2
                                       TO WRK-TIMESTAMP
           MOVE CORR WRK-TIMESTAMP     TO WRK-DATA-AUX.
           MOVE WRK-DATA-AUX           TO GFCTT3-DATA-PAREC-FLEXZ.

080826     MOVE CPER-COBR-TARIF        OF GFCTB0B2
080826                                 TO GFCTT3-CPER-COBR-TARIF.
080826     MOVE CINIC-PER-COBR         OF GFCTB0B2
080826                                 TO GFCTT3-CINIC-PER-COBR.

           EVALUATE CAGPTO-CTA         OF GFCTB0B2
               WHEN 4
                   PERFORM 3100-SELEC-PEDIDO-CLIE
               WHEN 14
                   PERFORM 3200-SELEC-PEDIDO-AGEN
               WHEN 9
                   PERFORM 3300-SELEC-PEDIDO-SEGM
               WHEN 15
                   PERFORM 3400-SELEC-PEDIDO-PAB
               WHEN 16
                   PERFORM 3500-SELEC-PEDIDO-EMPR
               WHEN 17
                   PERFORM 3600-SELEC-PEDIDO-POST
               WHEN 18
                   PERFORM 3700-SELEC-PEDIDO-MUNI
               WHEN 19
                   PERFORM 3800-SELEC-PEDIDO-UF
               WHEN 20
                   PERFORM 3600-SELEC-PEDIDO-POST
               WHEN 26
                   PERFORM 3810-SELEC-PEDIDO-RAZAO
           END-EVALUATE.

           PERFORM 3900-GRAVAR-DEFERIDO.

           PERFORM 2100-LER-CURSOR.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * SELECIONAR PEDIDO CLIENTE.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-SELEC-PEDIDO-CLIE          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B3.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B3.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB0B3.

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
                   CAGPTO_CTA         = :GFCTB0B3.CAGPTO-CTA   AND
                   CSERVC_TARIF       = :GFCTB0B3.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B3.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_CLI'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0040'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

ST2506*    MOVE CCGC-CPF               OF GFCTB0B3
ST2506*                                TO GFCTT3-CCGC-CPF04.
ST2506*    MOVE CFLIAL-CGC             OF GFCTB0B3
ST2506*                                TO GFCTT3-CFLIAL-CGC04.
ST2506*    MOVE CCTRL-CPF-CGC          OF GFCTB0B3
ST2506*                                TO GFCTT3-CCTRL-CPF-CGC04.
ST2506     MOVE CCGC-CPF-ST            OF GFCTB0B3
ST2506                                 TO GFCTT3-CCGC-CPF04.
ST2506     MOVE CFLIAL-CGC-ST          OF GFCTB0B3
ST2506                                 TO GFCTT3-CFLIAL-CGC04.
ST2506     MOVE CCTRL-CPF-CGC-ST       OF GFCTB0B3
ST2506                                 TO GFCTT3-CCTRL-CPF-CGC04.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * SELECIONAR PEDIDO AGENCIA.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-SELEC-PEDIDO-AGEN          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B1.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B1.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB0B1.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC
             INTO
                   :GFCTB0B1.CEMPR-INC,
                   :GFCTB0B1.CDEPDC
             FROM   DB2PRD.PDIDO_FLEXZ_AG
             WHERE
                   CAGPTO_CTA         = :GFCTB0B1.CAGPTO-CTA   AND
                   CSERVC_TARIF       = :GFCTB0B1.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B1.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AG'   TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0050'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CEMPR-INC              OF GFCTB0B1
                                       TO GFCTT3-CEMPR-INC05.
           MOVE CDEPDC                 OF GFCTB0B1
                                       TO GFCTT3-CDEPDC05.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * SELECIONAR PEDIDO SEGMENTO.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-SELEC-PEDIDO-SEGM          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0C0.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0C0.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB0C0.

           EXEC SQL
             SELECT
                   CSGMTO_GSTAO_TARIF
             INTO
                   :GFCTB0C0.CSGMTO-GSTAO-TARIF
             FROM   DB2PRD.PDIDO_FLEXZ_SGMTO
             WHERE
                   CAGPTO_CTA         = :GFCTB0C0.CAGPTO-CTA   AND
                   CSERVC_TARIF       = :GFCTB0C0.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0C0.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_SGMTO' TO ERR-DBD-TAB
               MOVE 'SELECT  '          TO ERR-FUN-COMANDO
               MOVE  SQLCODE            TO ERR-SQL-CODE
               MOVE '0060'              TO ERR-LOCAL
               MOVE  SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CSGMTO-GSTAO-TARIF     OF GFCTB0C0
                                       TO GFCTT3-CSGMTO-GSTAO-TARIF09.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * SELECIONAR PEDIDO PAB.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-SELEC-PEDIDO-PAB           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B8.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B8.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB0B8.

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
                   CAGPTO_CTA         = :GFCTB0B8.CAGPTO-CTA   AND
                   CSERVC_TARIF       = :GFCTB0B8.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B8.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_PAB'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0070'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CEMPR-INC              OF GFCTB0B8
                                       TO GFCTT3-CEMPR-INC15.
           MOVE CDEPDC                 OF GFCTB0B8
                                       TO GFCTT3-CDEPDC15.
           MOVE CPOSTO-SERVC           OF GFCTB0B8
                                       TO GFCTT3-CPOSTO-SERVC15.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * SELECIONAR PEDIDO EMPRESA.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-SELEC-PEDIDO-EMPR          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B6.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B6.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB0B6.

           EXEC SQL
             SELECT
                   CJUNC_DEPDC,
                   CCTA_CLI,
                   CAG_EMPR,
                   CRZ_CTA_EMPR,
                   CNRO_CTA_DEB,
                   CLCTO_EMPR,
                   PPGTO_RTEIO_EMPR,
                   RSEGDA_LIN_EXTRT
             INTO
                   :GFCTB0B6.CJUNC-DEPDC,
                   :GFCTB0B6.CCTA-CLI,
                   :GFCTB0B6.CAG-EMPR,
                   :GFCTB0B6.CRZ-CTA-EMPR,
                   :GFCTB0B6.CNRO-CTA-DEB,
                   :GFCTB0B6.CLCTO-EMPR,
                   :GFCTB0B6.PPGTO-RTEIO-EMPR,
                   :GFCTB0B6.RSEGDA-LIN-EXTRT
             FROM   DB2PRD.PDIDO_FLEXZ_EMPR
             WHERE
                   CAGPTO_CTA         = :GFCTB0B6.CAGPTO-CTA   AND
                   CSERVC_TARIF       = :GFCTB0B6.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B6.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_EMPR' TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0080'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CJUNC-DEPDC            OF GFCTB0B6
                                       TO GFCTT3-CJUNC-DEPDC16.
           MOVE CCTA-CLI               OF GFCTB0B6
                                       TO GFCTT3-CCTA-CLI16.
           MOVE CAG-EMPR               OF GFCTB0B6
                                       TO GFCTT3-CAG-EMPR16.
           MOVE CRZ-CTA-EMPR           OF GFCTB0B6
                                       TO GFCTT3-CRZ-CTA-EMPR16.
           MOVE CNRO-CTA-DEB           OF GFCTB0B6
                                       TO GFCTT3-CNRO-CTA-DEB16.
           MOVE CLCTO-EMPR             OF GFCTB0B6
                                       TO GFCTT3-CLCTO-EMPR16.
           MOVE PPGTO-RTEIO-EMPR       OF GFCTB0B6
                                       TO GFCTT3-PPGTO-RTEIO-EMPR16.
           MOVE RSEGDA-LIN-EXTRT       OF GFCTB0B6
                                       TO GFCTT3-RSEGDA-LIN-EXTRT16.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * SELECIONAR PEDIDO POSTAL.                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3600-SELEC-PEDIDO-POST          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B9.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B9.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB0B9.

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
                   CAGPTO_CTA         = :GFCTB0B9.CAGPTO-CTA   AND
                   CSERVC_TARIF       = :GFCTB0B9.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B9.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_PSTAL' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE  SQLCODE            TO ERR-SQL-CODE
               MOVE '0090'              TO ERR-LOCAL
               MOVE  SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CEMPR-INC              OF GFCTB0B9
                                       TO GFCTT3-CEMPR-INC17.
           MOVE CDEPDC                 OF GFCTB0B9
                                       TO GFCTT3-CDEPDC17.
           MOVE CPOSTO-SERVC           OF GFCTB0B9
                                       TO GFCTT3-CPOSTO-SERVC17.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * SELECIONAR PEDIDO MUNICIPIO.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3700-SELEC-PEDIDO-MUNI          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B7.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B7.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB0B7.

           EXEC SQL
             SELECT
                   CMUN_IBGE
             INTO
                   :GFCTB0B7.CMUN-IBGE
             FROM   DB2PRD.PDIDO_FLEXZ_MUN
             WHERE
                   CAGPTO_CTA         = :GFCTB0B7.CAGPTO-CTA   AND
                   CSERVC_TARIF       = :GFCTB0B7.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B7.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_MUN'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0100'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CMUN-IBGE              OF GFCTB0B7
                                       TO GFCTT3-CMUN-IBGE18.

      *----------------------------------------------------------------*
       3700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * SELECIONAR PEDIDO UF.                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3800-SELEC-PEDIDO-UF            SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0C1.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0C1.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB0C1.

           EXEC SQL
             SELECT
                   CSGL_UF
             INTO
                   :GFCTB0C1.CSGL-UF
             FROM   DB2PRD.PDIDO_FLEXZ_UF
             WHERE
                   CAGPTO_CTA         = :GFCTB0C1.CAGPTO-CTA   AND
                   CSERVC_TARIF       = :GFCTB0C1.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0C1.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_UF'   TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0110'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CSGL-UF                OF GFCTB0C1
                                       TO GFCTT3-CSGL-UF19.

      *----------------------------------------------------------------*
       3800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
LFM   ******************************************************************
LFM   * SELECIONAR PEDIDO RAZAO.                                       *
LFM   ******************************************************************
LFM   *----------------------------------------------------------------*
LFM    3810-SELEC-PEDIDO-RAZAO         SECTION.
LFM   *----------------------------------------------------------------*
LFM
LFM        MOVE CSERVC-TARIF           OF GFCTB0B2
LFM                                    TO CSERVC-TARIF     OF GFCTB0M7
LFM        MOVE CAGPTO-CTA             OF GFCTB0B2
LFM                                    TO CAGPTO-CTA       OF GFCTB0M7
LFM        MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
LFM                                    TO HIDTFD-FLEXZ-AGPTO
LFM                                    OF GFCTB0M7.
LFM
LFM        EXEC SQL
LFM             SELECT
LFM                   CGRP_CTBIL_ATUAL,
LFM                   CSGRP_CTBIL,
LFM                   CEMPR
LFM             INTO
LFM                   :GFCTB0M7.CGRP-CTBIL-ATUAL,
LFM                   :GFCTB0M7.CSGRP-CTBIL,
LFM                   :GFCTB0M7.CEMPR
LFM             FROM   DB2PRD.TPDIDO_FLEXZ_RZ
LFM             WHERE
LFM                   CAGPTO_CTA         = :GFCTB0M7.CAGPTO-CTA   AND
LFM                   CSERVC_TARIF       = :GFCTB0M7.CSERVC-TARIF AND
LFM                   HIDTFD_FLEXZ_AGPTO = :GFCTB0M7.HIDTFD-FLEXZ-AGPTO
LFM           END-EXEC.
LFM
LFM              IF (SQLCODE                 NOT EQUAL ZEROS) OR
LFM                 (SQLWARN0                 EQUAL 'W')
LFM                  MOVE 'DB2'               TO ERR-TIPO-ACESSO
LFM                  MOVE 'TPDIDO_FLEXZ_RZ'   TO ERR-DBD-TAB
LFM                  MOVE 'SELECT'            TO ERR-FUN-COMANDO
LFM                  MOVE  SQLCODE            TO ERR-SQL-CODE
LFM                  MOVE '0090'              TO ERR-LOCAL
LFM                  MOVE  SPACES             TO ERR-SEGM
LFM                  PERFORM 9999-PROCESSAR-ROTINA-ERRO
LFM                END-IF.
LFM
LFM             MOVE CEMPR                  OF GFCTB0M7
LFM                                         TO GFCTT3-EMPR-CTBIL26.
LFM             MOVE CGRP-CTBIL-ATUAL       OF GFCTB0M7
LFM                                         TO GFCTT3-GRP-CTBIL26.
LFM             MOVE CSGRP-CTBIL            OF GFCTB0M7
LFM                                         TO GFCTT3-SGRP-CTBIL26.
LFM
LFM   *----------------------------------------------------------------*
LFM    3810-99-FIM.                    EXIT.
LFM   *----------------------------------------------------------------*

      ******************************************************************
      * GRAVAR ARQUIVO DE SAIDA DEFERIDO.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3900-GRAVAR-DEFERIDO             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE GFCTT3-REGISTRO.

           PERFORM 1100-TESTAR-FS-DEFERIDO.

           ADD 1                       TO ACU-GRAVADOS.

      *----------------------------------------------------------------*
       3900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCEDIMENTOS FINAIS DO PROGRAMA.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-GRAVADOS            GREATER ZEROS
               PERFORM 4100-DISPLAY-SUCESSO
               PERFORM 4200-DISPLAY-TOTAIS
           ELSE
               PERFORM 4300-DISPLAY-VAZIO
           END-IF.

           PERFORM 4400-FECHAR-CURSOR.

           CLOSE DEFERIDO.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FS-DEFERIDO.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EMITIR DISPLAY PARA PROCESSAMENTO EFETUADO COM SUCESSO.        *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-DISPLAY-SUCESSO            SECTION.
      *----------------------------------------------------------------*

           DISPLAY '***************** GFCT1700 ****************'.
           DISPLAY '*                                         *'.
           DISPLAY '*            PROGRAMA GFCT1700            *'.
           DISPLAY '*                                         *'.
           DISPLAY '*    PROCESSAMENTO EFETUADO COM SUCESSO   *'.
           DISPLAY '*                                         *'.
           DISPLAY '***************** GFCT1700 ****************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EMITIR DISPLAY COM TOTAIS DOS ACUMULADORES DO PROGRAMA.        *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-DISPLAY-TOTAIS             SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-GRAVADOS           TO WRK-MASCARA.

           DISPLAY '****************** GFCT1700 *******************'.
           DISPLAY '*                                             *'.
           DISPLAY '* TOTAL REGS. GRAVADOS DEFERIDO: ' WRK-MASCARA
                                                               ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '****************** GFCT1700 *******************'.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EMITIR DISPLAY CASO ARQUIVO DEFERIDO ESTEJA VAZIO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       4300-DISPLAY-VAZIO              SECTION.
      *----------------------------------------------------------------*

           DISPLAY '***************** GFCT1700 ****************'.
           DISPLAY '*                                         *'.
           DISPLAY '*            PROGRAMA GFCT1700            *'.
           DISPLAY '*                                         *'.
           DISPLAY '*        ARQUIVO DE DEFERIDO VAZIO        *'.
           DISPLAY '*                                         *'.
           DISPLAY '***************** GFCT1700 ****************'.

      *----------------------------------------------------------------*
       4300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FECHAR CURSOR DA TABELA GFCTB0B2.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       4400-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR01-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO ERR-DBD-TAB
               MOVE 'CLOSE'             TO ERR-FUN-COMANDO
               MOVE  SQLCODE            TO ERR-SQL-CODE
               MOVE '0120'              TO ERR-LOCAL
               MOVE  SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CANCELAR PROGRAMA ATRAVES DA POOL7100 QUANDO ERRO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1700'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
