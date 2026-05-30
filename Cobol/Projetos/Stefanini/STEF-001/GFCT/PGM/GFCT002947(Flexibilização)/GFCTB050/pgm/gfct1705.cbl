      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1705.
       AUTHOR.     WINISSIUS LUIZ JANCZESKI.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1705                                    *
      *    PROGRAMADOR.:   WINISSIUS LUIZ JANCZESKI - CPM PATO BRANCO  *
      *    ANALISTA CPM:   MARCIO CRISTIANO CUSTIN  - CPM PATO BRANCO  *
      *    ANALISTA....:   MARIA VALERIA TORQUATO   - PROCWORK / GP.50 *
      *    DATA........:   14/06/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   LEITURA DO ARQUIVO DE PEDIDOS DEFERIDOS,    *
      *      PESQUISA NA TABELA VIGENTE PARA FINALIZAR AS FLEXIBILIZA- *
      *      COES QUE AINDA ESTEJAM VIGENTES.                          *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    DEFERIDO                  I#GFCTT3          *
      *                    DEFERATU                  I#GFCTT6          *
      *                    FLEXZFIM                  I#GFCTT5          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.PARM_DATA_PROCM        GFCTB0A1      *
      *                    DB2PRD.FLEXZ_GRP_UF           GFCTB057      *
      *                    DB2PRD.FLEXZ_GRP_MUN          GFCTB053      *
      *                    DB2PRD.FLEXZ_GRP_PSTAL        GFCTB055      *
      *                    DB2PRD.FLEXZ_GRP_PAB          GFCTB054      *
      *                    DB2PRD.FLEXZ_GRP_SGMTO        GFCTB056      *
      *                    DB2PRD.FLEXZ_GRP_DEPDC        GFCTB051      *
      *                    DB2PRD.FLEXZ_GRP_CLI          GFCTB050      *
      *                    DB2PRD.FLEXZ_AGPTO_CTA        GFCTB049      *
      *                    DB2PRD.FLEXZ_GRP_EMPR         GFCTB052      *
      *                    DB2PRD.TFLEXZ_GRP_RZ          GFCTB0M8      *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100.     *
      *    I#GFCTT3 - LAY-OUT DO ARQUIVO DE FLEXIBILIZACOES DEFERIDAS. *
      *    I#GFCTT5 - LAY-OUT DO ARQUIVO DE FLEXZ COM VIGENCIA A SER   *
      *               ALTERADAS.                                       *
      *    I#GFCTT6 - LAY-OUT DO ARQUIVO DE FLEXIBILIZACOES DEFERIDAS  *
      *               ATUALIZADAS.                                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - CONEXAO COM DB2.                                 *
      *    POOL7100 - TRATAMENTO DE ERROS PADRAO BRADESCO.             *
      *================================================================*
080826*----------------------------------------------------------------*
080826*    ULTIMA ALTERACAO - VINICIUS MADUREIRA                       *
080826*    INCLUSAO DE CAMPOS NA TABELA GFCTB0B2 E GFCTB049            *
080826*    E ALTERACAO DA INC GFCTT3                                   *
080826*================================================================*
      *----------------------------------------------------------------*
      *    PROJETO - PWT-09-2711 - ALTERACAO JUNHO/2010                *
      *    INCLUSAO VALIDACAO DA TABELA GFCTB0M8 - TFLEXZ_GRP_RZ       *
      *    E ATUALIZAR ARQUIVO I#GFCTT6                                *
      *================================================================*

      *================================================================
       ENVIRONMENT                     DIVISION.
      *================================================================
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

           SELECT DEFERATU ASSIGN      TO UT-S-DEFERATU
           FILE STATUS                 IS WRK-FS-DEFERATU.

           SELECT FLEXZFIM ASSIGN      TO UT-S-FLEXZFIM
           FILE STATUS                 IS WRK-FS-FLEXZFIM.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO DE FLEXIBILIZACOES DEFERIDAS.               *
      *            ORG. SEQUENCIAL     -   LRECL = 400                 *
      *----------------------------------------------------------------*

       FD  DEFERIDO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTT3'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE FLEXIBILIZACOES DEFERIDAS ATUALIZADAS.   *
      *            ORG. SEQUENCIAL     -   LRECL = 200                 *
      *----------------------------------------------------------------*

       FD  DEFERATU
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTT6'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE FLEXZ COM VIGENCIA A SER ALTERADA.       *
      *            ORG. SEQUENCIAL     -   LRECL = 100                 *
      *----------------------------------------------------------------*

       FD  FLEXZFIM
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTT5'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-DEFERIDO          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-DEFERATU          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-FLEXZFIM          PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE TESTES DE FILE STATUS *'.
      *----------------------------------------------------------------*

       77  WRK-FS-DEFERIDO             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-DEFERATU             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-FLEXZFIM             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MASCARA                 PIC  ZZZZ.ZZZ.999   VALUE ZEROS.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       01  WRK-DATA-DB2.
           05 WRK-DIA-DB2              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE SPACES.
           05 WRK-MES-DB2              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE SPACES.
           05 WRK-ANO-DB2              PIC  9(004)         VALUE ZEROS.

       01  WRK-DFIM-FLEX               PIC  9(008)         VALUE ZEROS.
       01  WRK-DFIM-FLEX-R             REDEFINES          WRK-DFIM-FLEX
                                       PIC  X(008).
       01  WRK-DPROCM                  PIC  9(008)         VALUE ZEROS.
       01  WRK-DPROCM-R                REDEFINES           WRK-DPROCM
                                       PIC  X(008).

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* MENSAGEM DE ERRO DE FILE-STATUS *'.
      *---------------------------------------------------------------*

       01  WRK-ERRO-TEXTO.
           05 FILLER                   PIC  X(008)         VALUE
              '** ERRO '.
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE
              ' - FILE STATUS = '.
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TRATAMENTO DE ERRO *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
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
             INCLUDE GFCTB052
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0M8
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS A SEREM PROCESSADAS                        *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-DEFERIDO     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCEDIMENTOS INICIAIS E BUSCAR DATA DE PROCESSAMENTO.         *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           PERFORM 1100-DATA-PROCESSAMENTO.

           OPEN INPUT  DEFERIDO
                OUTPUT DEFERATU
                       FLEXZFIM.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    BUSCAR DATA PROCESSAMENTO.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-DATA-PROCESSAMENTO         SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ANTER,
                   DPROCM_ATUAL,
                   DPROCM_PROX
             INTO
                   :GFCTB0A1.DPROCM-ANTER,
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-DB2.

           STRING WRK-ANO-DB2 WRK-MES-DB2 WRK-DIA-DB2
           DELIMITED BY SIZE         INTO WRK-DPROCM-R.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DOS ARQUIVOS                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1210-TESTAR-FS-DEFERIDO.

           PERFORM 1220-TESTAR-FS-DEFERATU.

           PERFORM 1230-TESTAR-FS-FLEXZFIM.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO  ARQUIVO DEFERIDO                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-TESTAR-FS-DEFERIDO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DEFERIDO         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-DEFERIDO    TO WRK-FILE-STATUS
               MOVE 'DEFERIDO'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO  ARQUIVO DEFERATU                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1220-TESTAR-FS-DEFERATU         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DEFERATU         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-DEFERATU    TO WRK-FILE-STATUS
               MOVE 'DEFERATU'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO  ARQUIVO FLEXZFIM                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1230-TESTAR-FS-FLEXZFIM         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-FLEXZFIM         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-FLEXZFIM    TO WRK-FILE-STATUS
               MOVE 'FLEXZFIM'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA SE ARQUIVO ESTA VAZIO.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-DEFERIDO.

           IF WRK-FS-DEFERIDO         EQUAL '10'
              DISPLAY '******************** GFCT1705 ******************'
              DISPLAY '*                                              *'
              DISPLAY '*              PROGRAMA GFCT1705               *'
              DISPLAY '*              -----------------               *'
              DISPLAY '*                                              *'
              DISPLAY '*          ARQUIVO DE DEFERIDO VAZIO           *'
              DISPLAY '*                                              *'
              DISPLAY '******************** GFCT1705 ******************'
              PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO DEFERIDO                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-DEFERIDO               SECTION.
      *----------------------------------------------------------------*

           READ DEFERIDO.

           IF  WRK-FS-DEFERIDO         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1210-TESTAR-FS-DEFERIDO.

           ADD 1                       TO ACU-LIDOS-DEFERIDO.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT3-DADOS-CHAVE     TO GFCTT6-DADOS-CHAVE.
           MOVE GFCTT3-DADOS           TO GFCTT6-DADOS.
           MOVE GFCTT3-DADOS-AGPTO04   TO GFCTT6-DADOS-AGPTO04.
           MOVE GFCTT3-DADOS-AGPTO05   TO GFCTT6-DADOS-AGPTO05.
           MOVE GFCTT3-DADOS-AGPTO09   TO GFCTT6-DADOS-AGPTO09.
           MOVE GFCTT3-DADOS-AGPTO15   TO GFCTT6-DADOS-AGPTO15.
           MOVE GFCTT3-DADOS-AGPTO16   TO GFCTT6-DADOS-AGPTO16.
           MOVE GFCTT3-DADOS-AGPTO17   TO GFCTT6-DADOS-AGPTO17.
           MOVE GFCTT3-DADOS-AGPTO18   TO GFCTT6-DADOS-AGPTO18.
           MOVE GFCTT3-DADOS-AGPTO19   TO GFCTT6-DADOS-AGPTO19.
080826     MOVE GFCTT3-DADOS-2         TO GFCTT6-DADOS-2.
LFM        MOVE GFCTT3-DADOS-AGPTO26   TO GFCTT6-DADOS-AGPTO26.
           MOVE 'MIGRADA'              TO GFCTT6-MENSAGEM.

           PERFORM 3100-GRAVAR-DEFERATU.

           EVALUATE GFCTT3-CAGPTO-CTA
               WHEN 15
                   PERFORM 3200-ACESSAR-GFCTB054
               WHEN 14
                   PERFORM 3300-ACESSAR-GFCTB051
               WHEN 17
                   PERFORM 3400-ACESSAR-GFCTB055
               WHEN 4
                   PERFORM 3500-ACESSAR-GFCTB050
               WHEN 9
                   PERFORM 3600-ACESSAR-GFCTB056
               WHEN 18
                   PERFORM 3700-ACESSAR-GFCTB053
               WHEN 19
                   PERFORM 3800-ACESSAR-GFCTB057
               WHEN 16
                   PERFORM 3900-ACESSAR-GFCTB052
LFM            WHEN 20
LFM                PERFORM 3400-ACESSAR-GFCTB055
LFM            WHEN 26
LFM                PERFORM 3220-ACESSAR-GFCTB0M8
               WHEN OTHER
080826             PERFORM 3950-ACESSAR-GFCTB049
           END-EVALUATE.

           PERFORM 2100-LER-DEFERIDO.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR ARQUIVO DEFERATU.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-GRAVAR-DEFERATU            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE GFCTT6-REGISTRO.

           PERFORM 1220-TESTAR-FS-DEFERATU.

           ADD 1                       TO ACU-GRAVA-DEFERATU.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR SE JA TEM UMA FLEX AGRUPAMENTO PAB                *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-ACESSAR-GFCTB054           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT3-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB054.
           MOVE GFCTT3-CAGPTO-CTA     TO CAGPTO-CTA         OF GFCTB054.
           MOVE GFCTT3-CEMPR-INC15    TO CEMPR-INC          OF GFCTB054.
           MOVE GFCTT3-CDEPDC15       TO CDEPDC             OF GFCTB054.
           MOVE GFCTT3-CPOSTO-SERVC15 TO CPOSTO-SERVC       OF GFCTB054.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   HIDTFD_FLEXZ_AGPTO,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
               INTO
                   :GFCTB054.CSERVC-TARIF,
                   :GFCTB054.CAGPTO-CTA,
                   :GFCTB054.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB054.CEMPR-INC,
                   :GFCTB054.CDEPDC,
                   :GFCTB054.CPOSTO-SERVC
               FROM DB2PRD.FLEXZ_GRP_PAB
               WHERE
                   CSERVC_TARIF       = :GFCTB054.CSERVC-TARIF
               AND CAGPTO_CTA         = :GFCTB054.CAGPTO-CTA
               AND CEMPR_INC          = :GFCTB054.CEMPR-INC
               AND CDEPDC             = :GFCTB054.CDEPDC
               AND CPOSTO_SERVC       = :GFCTB054.CPOSTO-SERVC
               AND HIDTFD_FLEXZ_AGPTO IN
                   (SELECT MAX(HIDTFD_FLEXZ_AGPTO)
                    FROM DB2PRD.FLEXZ_GRP_PAB
                    WHERE
                        CSERVC_TARIF  = :GFCTB054.CSERVC-TARIF
                    AND CAGPTO_CTA    = :GFCTB054.CAGPTO-CTA
                    AND CEMPR_INC     = :GFCTB054.CEMPR-INC
                    AND CDEPDC        = :GFCTB054.CDEPDC
                    AND CPOSTO_SERVC  = :GFCTB054.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_PAB'    TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSERVC-TARIF       OF GFCTB054
                                       TO CSERVC-TARIF       OF GFCTB049
               MOVE CAGPTO-CTA         OF GFCTB054
                                       TO CAGPTO-CTA         OF GFCTB049
               MOVE HIDTFD-FLEXZ-AGPTO OF GFCTB054
                                       TO HIDTFD-FLEXZ-AGPTO OF GFCTB049
               PERFORM 3210-VERIFICAR-VIGENCIA
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR VIGENCIA - ACESSO GFCTB049.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3210-VERIFICAR-VIGENCIA         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                    CSERVC_TARIF,
                    CAGPTO_CTA,
                    HIDTFD_FLEXZ_AGPTO,
                    DINIC_FLEXZ_AGPTO,
                    DFIM_FLEXZ_AGPTO,
                    QFRANQ_FLEXZ_AGPTO,
                    CPER_FRANQ_AGPTO,
                    VNEGOC_FLEXZ_AGPTO,
                    PDESC_FLEXZ_AGPTO,
                    QDIA_FLEXZ_AGPTO,
                    CINDCD_AGPTO_TOT,
                    DDIA_AGNDA,
                    CINIC_PER_FRANQ,
                    CTPO_OPER_MOTVO,
                    CMOTVO_JUSTF,
                    CEMPR_INC,
                    CDEPDC_ENCRR_VGCIA,
                    CFUNC_ENCRR_VGCIA,
                    HVGCIA_REGRA,
080826              CPER_COBR_TARIF,
080826              CINIC_PER_COBR
             INTO
                    :GFCTB049.CSERVC-TARIF,
                    :GFCTB049.CAGPTO-CTA,
                    :GFCTB049.HIDTFD-FLEXZ-AGPTO,
                    :GFCTB049.DINIC-FLEXZ-AGPTO,
                    :GFCTB049.DFIM-FLEXZ-AGPTO,
                    :GFCTB049.QFRANQ-FLEXZ-AGPTO,
                    :GFCTB049.CPER-FRANQ-AGPTO,
                    :GFCTB049.VNEGOC-FLEXZ-AGPTO,
                    :GFCTB049.PDESC-FLEXZ-AGPTO,
                    :GFCTB049.QDIA-FLEXZ-AGPTO,
                    :GFCTB049.CINDCD-AGPTO-TOT,
                    :GFCTB049.DDIA-AGNDA,
                    :GFCTB049.CINIC-PER-FRANQ,
                    :GFCTB049.CTPO-OPER-MOTVO,
                    :GFCTB049.CMOTVO-JUSTF,
                    :GFCTB049.CEMPR-INC,
                    :GFCTB049.CDEPDC-ENCRR-VGCIA,
                    :GFCTB049.CFUNC-ENCRR-VGCIA,
                    :GFCTB049.HVGCIA-REGRA,
080826              :GFCTB049.CPER-COBR-TARIF,
080826              :GFCTB049.CINIC-PER-COBR
             FROM   DB2PRD.FLEXZ_AGPTO_CTA
             WHERE
                   CSERVC_TARIF       = :GFCTB049.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB049.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB049.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_AGPTO_CTA ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE DFIM-FLEXZ-AGPTO       OF GFCTB049
                                       TO WRK-DATA-DB2.

           STRING WRK-ANO-DB2 WRK-MES-DB2 WRK-DIA-DB2
           DELIMITED BY SIZE         INTO WRK-DFIM-FLEX-R.

           MOVE GFCTT3-DADOS-CHAVE     TO GFCTT6-DADOS-CHAVE.
           MOVE GFCTT3-DADOS           TO GFCTT6-DADOS.
           MOVE GFCTT3-DADOS-AGPTO04   TO GFCTT6-DADOS-AGPTO04.
           MOVE GFCTT3-DADOS-AGPTO05   TO GFCTT6-DADOS-AGPTO05.
           MOVE GFCTT3-DADOS-AGPTO09   TO GFCTT6-DADOS-AGPTO09.
           MOVE GFCTT3-DADOS-AGPTO15   TO GFCTT6-DADOS-AGPTO15.
           MOVE GFCTT3-DADOS-AGPTO16   TO GFCTT6-DADOS-AGPTO16.
           MOVE GFCTT3-DADOS-AGPTO17   TO GFCTT6-DADOS-AGPTO17.
           MOVE GFCTT3-DADOS-AGPTO18   TO GFCTT6-DADOS-AGPTO18.
           MOVE GFCTT3-DADOS-AGPTO19   TO GFCTT6-DADOS-AGPTO19.
080826     MOVE GFCTT3-DADOS-2         TO GFCTT6-DADOS-2.
LFM        MOVE GFCTT3-DADOS-AGPTO26   TO GFCTT6-DADOS-AGPTO26

           IF  WRK-DFIM-FLEX           GREATER WRK-DPROCM
               MOVE 'FINALIZADA'       TO GFCTT6-MENSAGEM
               MOVE CSERVC-TARIF       OF GFCTB049
                                       TO GFCTT5-CSERVC-TARIF
               MOVE CAGPTO-CTA         OF GFCTB049
                                       TO GFCTT5-CAGPTO-CTA
               MOVE HIDTFD-FLEXZ-AGPTO OF GFCTB049
                                       TO GFCTT5-HIDTFD-FLEXZ-AGPTO
               MOVE DINIC-FLEXZ-AGPTO  OF GFCTB049
                                       TO GFCTT5-DATA-INICIO
               MOVE DFIM-FLEXZ-AGPTO   OF GFCTB049
                                       TO GFCTT5-DATA-FIM
               MOVE DPROCM-ATUAL       OF GFCTB0A1
                                       TO GFCTT5-DATA-FIM-ATU
               PERFORM 3211-GRAVAR-FLEXZFIM
           ELSE
               MOVE 'NAO ATUALIZADA'   TO GFCTT6-MENSAGEM
           END-IF.

           PERFORM 3100-GRAVAR-DEFERATU.

      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR ARQUIVO FLEXZFIM.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3211-GRAVAR-FLEXZFIM            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE GFCTT5-REGISTRO.

           PERFORM 1230-TESTAR-FS-FLEXZFIM.

           ADD 1                       TO ACU-GRAVA-FLEXZFIM.

      *----------------------------------------------------------------*
       3211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
LFM   ******************************************************************
LFM   *    VERIFICAR AGRUPAMENTO POR RAZAO                             *
LFM   ******************************************************************
LFM   *----------------------------------------------------------------*
LFM    3220-ACESSAR-GFCTB0M8        SECTION.
LFM   *----------------------------------------------------------------*
LFM
LFM        PERFORM 3222-MAX-PED-RAZAO
LFM
LFM        IF SQLCODE           EQUAL -305
LFM           GO TO 3220-99-FIM
LFM        END-IF.
LFM
LFM        EXEC SQL
LFM         SELECT
LFM               CSERVC_TARIF,
LFM               CAGPTO_CTA,
LFM               HIDTFD_FLEXZ_AGPTO,
LFM               CEMPR,
LFM               CGRP_CTBIL_ATUAL,
LFM               CSGRP_CTBIL
LFM           INTO
LFM               :GFCTB0M8.CSERVC-TARIF,
LFM               :GFCTB0M8.CAGPTO-CTA,
LFM               :GFCTB0M8.HIDTFD-FLEXZ-AGPTO,
LFM               :GFCTB0M8.CEMPR,
LFM               :GFCTB0M8.CGRP-CTBIL-ATUAL,
LFM               :GFCTB0M8.CSGRP-CTBIL
LFM           FROM DB2PRD.TFLEXZ_GRP_RZ
LFM           WHERE
LFM                 CSERVC_TARIF       = :GFCTB0M8.CSERVC-TARIF
LFM             AND CAGPTO_CTA         = :GFCTB0M8.CAGPTO-CTA
LFM             AND HIDTFD_FLEXZ_AGPTO = :GFCTB0M8.HIDTFD-FLEXZ-AGPTO
LFM             AND CGRP_CTBIL_ATUAL   = :GFCTB0M8.CGRP-CTBIL-ATUAL
LFM             AND CSGRP_CTBIL        = :GFCTB0M8.CSGRP-CTBIL
LFM             AND CEMPR              = :GFCTB0M8.CEMPR
LFM         END-EXEC.
LFM
LFM         IF (SQLCODE               NOT EQUAL ZEROS AND +100) OR
LFM          (SQLWARN0                EQUAL 'W'      )
LFM           MOVE 'DB2'              TO ERR-TIPO-ACESSO
LFM           MOVE 'TFLEXZ_GRP_RZ '   TO ERR-DBD-TAB
LFM           MOVE 'SELECT'           TO ERR-FUN-COMANDO
LFM           MOVE  SQLCODE           TO ERR-SQL-CODE
LFM           MOVE '0035'             TO ERR-LOCAL
LFM           MOVE  SPACES            TO ERR-SEGM
LFM           PERFORM 9999-PROCESSAR-ROTINA-ERRO
LFM         END-IF.
LFM
LFM         IF SQLCODE                 EQUAL ZEROS
LFM           MOVE CSERVC-TARIF       OF GFCTB0M8
LFM                                   TO CSERVC-TARIF       OF GFCTB049
LFM           MOVE CAGPTO-CTA         OF GFCTB0M8
LFM                                   TO CAGPTO-CTA         OF GFCTB049
LFM           MOVE HIDTFD-FLEXZ-AGPTO OF GFCTB0M8
LFM                                   TO HIDTFD-FLEXZ-AGPTO OF GFCTB049
LFM           PERFORM 3210-VERIFICAR-VIGENCIA
LFM         END-IF.
LFM
LFM   *----------------------------------------------------------------*
LFM    3220-99-FIM.                    EXIT.
LFM   *----------------------------------------------------------------*
LFM
LFM   ******************************************************************
LFM   *    OBTER MAIOR-TIMES-TAMP DA TABELA GFCTB0M8                   *
LFM   ******************************************************************
LFM   *----------------------------------------------------------------*
LFM    3222-MAX-PED-RAZAO  SECTION.
LFM   *----------------------------------------------------------------*
LFM
LFM        MOVE GFCTT3-CSERVC-TARIF   TO CSERVC-TARIF     OF GFCTB0M8.
LFM        MOVE GFCTT3-CAGPTO-CTA     TO CAGPTO-CTA       OF GFCTB0M8.
LFM        MOVE GFCTT3-GRP-CTBIL26    TO CGRP-CTBIL-ATUAL OF GFCTB0M8.
LFM        MOVE GFCTT3-SGRP-CTBIL26   TO CSGRP-CTBIL      OF GFCTB0M8.
LFM        MOVE GFCTT3-EMPR-CTBIL26   TO CEMPR            OF GFCTB0M8.
LFM
LFM        EXEC SQL
LFM         SELECT MAX(HIDTFD_FLEXZ_AGPTO)
LFM         INTO   :GFCTB0M8.HIDTFD-FLEXZ-AGPTO
LFM         FROM DB2PRD.TFLEXZ_GRP_RZ
LFM         WHERE
LFM                CSERVC_TARIF   = :GFCTB0M8.CSERVC-TARIF
LFM            AND CAGPTO_CTA     = :GFCTB0M8.CAGPTO-CTA
LFM            AND CEMPR          = :GFCTB0M8.CEMPR
LFM            AND CGRP_CTBIL_ATUAL
LFM                               = :GFCTB0M8.CGRP-CTBIL-ATUAL
LFM            AND CSGRP_CTBIL    = :GFCTB0M8.CSGRP-CTBIL
LFM        END-EXEC.
LFM
LFM        IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
LFM           (SQLWARN0                EQUAL 'W')
LFM            MOVE 'DB2'              TO ERR-TIPO-ACESSO
LFM            MOVE 'TFLEXZ_GRP_RZ'    TO ERR-DBD-TAB
LFM            MOVE 'SELECT  '         TO ERR-FUN-COMANDO
LFM            MOVE  SQLCODE           TO ERR-SQL-CODE
LFM            MOVE '0045'             TO ERR-LOCAL
LFM            MOVE  SPACES            TO ERR-SEGM
LFM            PERFORM 9999-PROCESSAR-ROTINA-ERRO
LFM        END-IF.
LFM
LFM   *----------------------------------------------------------------*
LFM    3222-99-FIM.                    EXIT.
LFM   *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR SE JA TEM UMA FLEX AGRUPAMENTO AGENCIA            *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-ACESSAR-GFCTB051           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT3-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB051.
           MOVE GFCTT3-CAGPTO-CTA     TO CAGPTO-CTA         OF GFCTB051.

           IF  GFCTT3-CINDCD-AGPTO-TOT EQUAL 'S'
               MOVE ZEROS              TO CEMPR-INC         OF GFCTB051
                                          CDEPDC            OF GFCTB051
           ELSE
               MOVE GFCTT3-CEMPR-INC05 TO CEMPR-INC         OF GFCTB051
               MOVE GFCTT3-CDEPDC05    TO CDEPDC            OF GFCTB051
           END-IF.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   HIDTFD_FLEXZ_AGPTO,
                   CEMPR_INC,
                   CDEPDC
               INTO
                   :GFCTB051.CSERVC-TARIF,
                   :GFCTB051.CAGPTO-CTA,
                   :GFCTB051.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB051.CEMPR-INC,
                   :GFCTB051.CDEPDC
               FROM DB2PRD.FLEXZ_GRP_DEPDC
               WHERE
                   CSERVC_TARIF       = :GFCTB051.CSERVC-TARIF
               AND CAGPTO_CTA         = :GFCTB051.CAGPTO-CTA
               AND CEMPR_INC          = :GFCTB051.CEMPR-INC
               AND CDEPDC             = :GFCTB051.CDEPDC
               AND HIDTFD_FLEXZ_AGPTO IN
                   (SELECT MAX(HIDTFD_FLEXZ_AGPTO)
                    FROM DB2PRD.FLEXZ_GRP_DEPDC
                    WHERE
                        CSERVC_TARIF  = :GFCTB051.CSERVC-TARIF
                    AND CAGPTO_CTA    = :GFCTB051.CAGPTO-CTA
                    AND CEMPR_INC     = :GFCTB051.CEMPR-INC
                    AND CDEPDC        = :GFCTB051.CDEPDC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_DEPDC'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0040'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSERVC-TARIF       OF GFCTB051
                                       TO CSERVC-TARIF       OF GFCTB049
               MOVE CAGPTO-CTA         OF GFCTB051
                                       TO CAGPTO-CTA         OF GFCTB049
               MOVE HIDTFD-FLEXZ-AGPTO OF GFCTB051
                                       TO HIDTFD-FLEXZ-AGPTO OF GFCTB049
               PERFORM 3210-VERIFICAR-VIGENCIA
           END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR SE JA TEM UMA FLEX AGRUPAMENTO POSTAL             *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-ACESSAR-GFCTB055           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT3-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB055.
           MOVE GFCTT3-CAGPTO-CTA     TO CAGPTO-CTA         OF GFCTB055.
           MOVE GFCTT3-CEMPR-INC17    TO CEMPR-INC          OF GFCTB055.
           MOVE GFCTT3-CDEPDC17       TO CDEPDC             OF GFCTB055.
           MOVE GFCTT3-CPOSTO-SERVC17 TO CPOSTO-SERVC       OF GFCTB055.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   HIDTFD_FLEXZ_AGPTO,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
               INTO
                   :GFCTB055.CSERVC-TARIF,
                   :GFCTB055.CAGPTO-CTA,
                   :GFCTB055.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB055.CEMPR-INC,
                   :GFCTB055.CDEPDC,
                   :GFCTB055.CPOSTO-SERVC
               FROM DB2PRD.FLEXZ_GRP_PSTAL
               WHERE
                   CSERVC_TARIF       = :GFCTB055.CSERVC-TARIF
               AND CAGPTO_CTA         = :GFCTB055.CAGPTO-CTA
               AND CEMPR_INC          = :GFCTB055.CEMPR-INC
               AND CDEPDC             = :GFCTB055.CDEPDC
               AND CPOSTO_SERVC       = :GFCTB055.CPOSTO-SERVC
               AND HIDTFD_FLEXZ_AGPTO IN
                   (SELECT MAX(HIDTFD_FLEXZ_AGPTO)
                    FROM DB2PRD.FLEXZ_GRP_PSTAL
                    WHERE
                        CSERVC_TARIF  = :GFCTB055.CSERVC-TARIF
                    AND CAGPTO_CTA    = :GFCTB055.CAGPTO-CTA
                    AND CEMPR_INC     = :GFCTB055.CEMPR-INC
                    AND CDEPDC        = :GFCTB055.CDEPDC
                    AND CPOSTO_SERVC  = :GFCTB055.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_PSTAL'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0050'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSERVC-TARIF       OF GFCTB055
                                       TO CSERVC-TARIF       OF GFCTB049
               MOVE CAGPTO-CTA         OF GFCTB055
                                       TO CAGPTO-CTA         OF GFCTB049
               MOVE HIDTFD-FLEXZ-AGPTO OF GFCTB055
                                       TO HIDTFD-FLEXZ-AGPTO OF GFCTB049
               PERFORM 3210-VERIFICAR-VIGENCIA
           END-IF.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR SE JA TEM UMA FLEX AGRUPAMENTO CLIENTE.           *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-ACESSAR-GFCTB050           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT3-CSERVC-TARIF    TO CSERVC-TARIF     OF GFCTB050.
           MOVE GFCTT3-CAGPTO-CTA      TO CAGPTO-CTA       OF GFCTB050.
ST2506*    MOVE GFCTT3-CCGC-CPF04      TO CCGC-CPF         OF GFCTB050.
ST2506*    MOVE GFCTT3-CFLIAL-CGC04    TO CFLIAL-CGC       OF GFCTB050.
ST2506*    MOVE GFCTT3-CCTRL-CPF-CGC04 TO CCTRL-CPF-CGC    OF GFCTB050.
ST2506     MOVE GFCTT3-CCGC-CPF04      TO CCGC-CPF-ST      OF GFCTB050.
ST2506     MOVE GFCTT3-CFLIAL-CGC04    TO CFLIAL-CGC-ST    OF GFCTB050.
ST2506     MOVE GFCTT3-CCTRL-CPF-CGC04 TO CCTRL-CPF-CGC-ST OF GFCTB050.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   HIDTFD_FLEXZ_AGPTO,
ST2506*            CCGC_CPF,
ST2506*            CFLIAL_CGC,
ST2506*            CCTRL_CPF_CGC
ST2506             CCGC_CPF_ST,
ST2506             CFLIAL_CGC_ST,
ST2506             CCTRL_CPF_CGC_ST
               INTO
                   :GFCTB050.CSERVC-TARIF,
                   :GFCTB050.CAGPTO-CTA,
                   :GFCTB050.HIDTFD-FLEXZ-AGPTO,
ST2506*            :GFCTB050.CCGC-CPF,
ST2506*            :GFCTB050.CFLIAL-CGC,
ST2506*            :GFCTB050.CCTRL-CPF-CGC
ST2506             :GFCTB050.CCGC-CPF-ST,
ST2506             :GFCTB050.CFLIAL-CGC-ST,
ST2506             :GFCTB050.CCTRL-CPF-CGC-ST
               FROM DB2PRD.FLEXZ_GRP_CLI
               WHERE
                   CSERVC_TARIF       = :GFCTB050.CSERVC-TARIF
               AND CAGPTO_CTA         = :GFCTB050.CAGPTO-CTA
ST2506*        AND CCGC_CPF           = :GFCTB050.CCGC-CPF
ST2506*        AND CFLIAL_CGC         = :GFCTB050.CFLIAL-CGC
ST2506*        AND CCTRL_CPF_CGC      = :GFCTB050.CCTRL-CPF-CGC
ST2506         AND CCGC_CPF_ST        = :GFCTB050.CCGC-CPF-ST
ST2506         AND CFLIAL_CGC_ST      = :GFCTB050.CFLIAL-CGC-ST
ST2506         AND CCTRL_CPF_CGC_ST   = :GFCTB050.CCTRL-CPF-CGC-ST
               AND HIDTFD_FLEXZ_AGPTO IN
                   (SELECT MAX(HIDTFD_FLEXZ_AGPTO)
                    FROM DB2PRD.FLEXZ_GRP_CLI
                    WHERE
                        CSERVC_TARIF  = :GFCTB050.CSERVC-TARIF
                    AND CAGPTO_CTA    = :GFCTB050.CAGPTO-CTA
ST2506*             AND CCGC_CPF      = :GFCTB050.CCGC-CPF
ST2506*             AND CFLIAL_CGC    = :GFCTB050.CFLIAL-CGC
ST2506*             AND CCTRL_CPF_CGC = :GFCTB050.CCTRL-CPF-CGC)
ST2506              AND CCGC_CPF_ST      = :GFCTB050.CCGC-CPF-ST
ST2506              AND CFLIAL_CGC_ST    = :GFCTB050.CFLIAL-CGC-ST
ST2506              AND CCTRL_CPF_CGC_ST = :GFCTB050.CCTRL-CPF-CGC-ST)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_CLI'    TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0060'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSERVC-TARIF       OF GFCTB050
                                       TO CSERVC-TARIF       OF GFCTB049
               MOVE CAGPTO-CTA         OF GFCTB050
                                       TO CAGPTO-CTA         OF GFCTB049
               MOVE HIDTFD-FLEXZ-AGPTO OF GFCTB050
                                       TO HIDTFD-FLEXZ-AGPTO OF GFCTB049
               PERFORM 3210-VERIFICAR-VIGENCIA
           END-IF.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR SE JA TEM UMA FLEX AGRUPAMENTO SEGMENTO           *
      ******************************************************************
      *----------------------------------------------------------------*
       3600-ACESSAR-GFCTB056           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT3-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB056.
           MOVE GFCTT3-CAGPTO-CTA     TO CAGPTO-CTA         OF GFCTB056.
           MOVE GFCTT3-CSGMTO-GSTAO-TARIF09
                                      TO CSGMTO-GSTAO-TARIF OF GFCTB056.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   HIDTFD_FLEXZ_AGPTO,
                   CSGMTO_GSTAO_TARIF
               INTO
                   :GFCTB056.CSERVC-TARIF,
                   :GFCTB056.CAGPTO-CTA,
                   :GFCTB056.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB056.CSGMTO-GSTAO-TARIF
               FROM DB2PRD.FLEXZ_GRP_SGMTO
               WHERE
                   CSERVC_TARIF       = :GFCTB056.CSERVC-TARIF
               AND CAGPTO_CTA         = :GFCTB056.CAGPTO-CTA
               AND CSGMTO_GSTAO_TARIF = :GFCTB056.CSGMTO-GSTAO-TARIF
               AND HIDTFD_FLEXZ_AGPTO IN
                   (SELECT MAX(HIDTFD_FLEXZ_AGPTO)
                    FROM DB2PRD.FLEXZ_GRP_SGMTO
                    WHERE
                        CSERVC_TARIF  = :GFCTB056.CSERVC-TARIF
                    AND CAGPTO_CTA    = :GFCTB056.CAGPTO-CTA
                    AND CSGMTO_GSTAO_TARIF
                                      = :GFCTB056.CSGMTO-GSTAO-TARIF)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_SGMTO'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0070'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSERVC-TARIF       OF GFCTB056
                                       TO CSERVC-TARIF       OF GFCTB049
               MOVE CAGPTO-CTA         OF GFCTB056
                                       TO CAGPTO-CTA         OF GFCTB049
               MOVE HIDTFD-FLEXZ-AGPTO OF GFCTB056
                                       TO HIDTFD-FLEXZ-AGPTO OF GFCTB049
               PERFORM 3210-VERIFICAR-VIGENCIA
           END-IF.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR SE JA TEM UMA FLEX AGRUPAMENTO MUNICIPIO.         *
      ******************************************************************
      *----------------------------------------------------------------*
       3700-ACESSAR-GFCTB053           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT3-CSERVC-TARIF  TO CSERVC-TARIF       OF GFCTB053.
           MOVE GFCTT3-CAGPTO-CTA    TO CAGPTO-CTA         OF GFCTB053.
           MOVE GFCTT3-CMUN-IBGE18   TO CMUN-IBGE          OF GFCTB053.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   HIDTFD_FLEXZ_AGPTO,
                   CMUN_IBGE
               INTO
                   :GFCTB053.CSERVC-TARIF,
                   :GFCTB053.CAGPTO-CTA,
                   :GFCTB053.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB053.CMUN-IBGE
               FROM DB2PRD.FLEXZ_GRP_MUN
               WHERE
                   CSERVC_TARIF       = :GFCTB053.CSERVC-TARIF
               AND CAGPTO_CTA         = :GFCTB053.CAGPTO-CTA
               AND CMUN_IBGE          = :GFCTB053.CMUN-IBGE
               AND HIDTFD_FLEXZ_AGPTO IN
                   (SELECT MAX(HIDTFD_FLEXZ_AGPTO)
                    FROM DB2PRD.FLEXZ_GRP_MUN
                    WHERE
                        CSERVC_TARIF  = :GFCTB053.CSERVC-TARIF
                    AND CAGPTO_CTA    = :GFCTB053.CAGPTO-CTA
                    AND CMUN_IBGE     = :GFCTB053.CMUN-IBGE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_MUN'    TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0080'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSERVC-TARIF       OF GFCTB053
                                       TO CSERVC-TARIF       OF GFCTB049
               MOVE CAGPTO-CTA         OF GFCTB053
                                       TO CAGPTO-CTA         OF GFCTB049
               MOVE HIDTFD-FLEXZ-AGPTO OF GFCTB053
                                       TO HIDTFD-FLEXZ-AGPTO OF GFCTB049
               PERFORM 3210-VERIFICAR-VIGENCIA
           END-IF.

      *----------------------------------------------------------------*
       3700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR SE JA TEM UMA FLEX AGRUPAMENTO UF                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3800-ACESSAR-GFCTB057           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT3-CSERVC-TARIF TO CSERVC-TARIF       OF GFCTB057.
           MOVE GFCTT3-CAGPTO-CTA   TO CAGPTO-CTA         OF GFCTB057.
           MOVE GFCTT3-CSGL-UF19    TO CSGL-UF            OF GFCTB057.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   HIDTFD_FLEXZ_AGPTO,
                   CSGL_UF
               INTO
                   :GFCTB057.CSERVC-TARIF,
                   :GFCTB057.CAGPTO-CTA,
                   :GFCTB057.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB057.CSGL-UF
               FROM DB2PRD.FLEXZ_GRP_UF
               WHERE
                   CSERVC_TARIF       = :GFCTB057.CSERVC-TARIF
               AND CAGPTO_CTA         = :GFCTB057.CAGPTO-CTA
               AND CSGL_UF            = :GFCTB057.CSGL-UF
               AND HIDTFD_FLEXZ_AGPTO IN
                   (SELECT MAX(HIDTFD_FLEXZ_AGPTO)
                    FROM DB2PRD.FLEXZ_GRP_UF
               WHERE
                   CSERVC_TARIF       = :GFCTB057.CSERVC-TARIF
               AND CAGPTO_CTA         = :GFCTB057.CAGPTO-CTA
               AND CSGL_UF            = :GFCTB057.CSGL-UF)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_UF'     TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0090'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSERVC-TARIF       OF GFCTB057
                                       TO CSERVC-TARIF       OF GFCTB049
               MOVE CAGPTO-CTA         OF GFCTB057
                                       TO CAGPTO-CTA         OF GFCTB049
               MOVE HIDTFD-FLEXZ-AGPTO OF GFCTB057
                                       TO HIDTFD-FLEXZ-AGPTO OF GFCTB049
               PERFORM 3210-VERIFICAR-VIGENCIA
           END-IF.

      *----------------------------------------------------------------*
       3800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR SE JA TEM UMA FLEX AGRUPAMENTO EMPRESA.           *
      ******************************************************************
      *----------------------------------------------------------------*
       3900-ACESSAR-GFCTB052           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT3-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB052.
           MOVE GFCTT3-CAGPTO-CTA     TO CAGPTO-CTA         OF GFCTB052.
           MOVE GFCTT3-CJUNC-DEPDC16  TO CJUNC-DEPDC        OF GFCTB052.
           MOVE GFCTT3-CCTA-CLI16     TO CCTA-CLI           OF GFCTB052.
           MOVE GFCTT3-CAG-EMPR16     TO CAG-EMPR           OF GFCTB052.
           MOVE GFCTT3-CNRO-CTA-DEB16 TO CNRO-CTA-DEB       OF GFCTB052.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   HIDTFD_FLEXZ_AGPTO,
                   CJUNC_DEPDC,
                   CCTA_CLI,
                   CAG_EMPR,
                   CRZ_CTA_EMPR,
                   CNRO_CTA_DEB,
                   CLCTO_EMPR,
                   PPGTO_RTEIO_EMPR,
                   RSEGDA_LIN_EXTRT
               INTO
                   :GFCTB052.CSERVC-TARIF,
                   :GFCTB052.CAGPTO-CTA,
                   :GFCTB052.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB052.CJUNC-DEPDC,
                   :GFCTB052.CCTA-CLI,
                   :GFCTB052.CAG-EMPR,
                   :GFCTB052.CRZ-CTA-EMPR,
                   :GFCTB052.CNRO-CTA-DEB,
                   :GFCTB052.CLCTO-EMPR,
                   :GFCTB052.PPGTO-RTEIO-EMPR,
                   :GFCTB052.RSEGDA-LIN-EXTRT
               FROM DB2PRD.FLEXZ_GRP_EMPR
               WHERE
                   CSERVC_TARIF       = :GFCTB052.CSERVC-TARIF
               AND CAGPTO_CTA         = :GFCTB052.CAGPTO-CTA
               AND CJUNC_DEPDC        = :GFCTB052.CJUNC-DEPDC
               AND CCTA_CLI           = :GFCTB052.CCTA-CLI
               AND CAG_EMPR           = :GFCTB052.CAG-EMPR
               AND CNRO_CTA_DEB       = :GFCTB052.CNRO-CTA-DEB
               AND HIDTFD_FLEXZ_AGPTO IN
                   (SELECT MAX(HIDTFD_FLEXZ_AGPTO)
                    FROM DB2PRD.FLEXZ_GRP_EMPR
                    WHERE
                        CSERVC_TARIF  = :GFCTB052.CSERVC-TARIF
                    AND CAGPTO_CTA    = :GFCTB052.CAGPTO-CTA
                    AND CJUNC_DEPDC   = :GFCTB052.CJUNC-DEPDC
                    AND CCTA_CLI      = :GFCTB052.CCTA-CLI
                    AND CAG_EMPR      = :GFCTB052.CAG-EMPR
                    AND CNRO_CTA_DEB  = :GFCTB052.CNRO-CTA-DEB)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_EMPR'   TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0100'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSERVC-TARIF       OF GFCTB052
                                       TO CSERVC-TARIF       OF GFCTB049
               MOVE CAGPTO-CTA         OF GFCTB052
                                       TO CAGPTO-CTA         OF GFCTB049
               MOVE HIDTFD-FLEXZ-AGPTO OF GFCTB052
                                       TO HIDTFD-FLEXZ-AGPTO OF GFCTB049
               PERFORM 3210-VERIFICAR-VIGENCIA
           END-IF.

      *----------------------------------------------------------------*
       3900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

080826*----------------------------------------------------------------*
080826 3950-ACESSAR-GFCTB049           SECTION.
080826*----------------------------------------------------------------*
080826
080826     MOVE GFCTT3-CSERVC-TARIF       TO CSERVC-TARIF OF GFCTB049.
080826     MOVE GFCTT3-CAGPTO-CTA         TO CAGPTO-CTA   OF GFCTB049.
080826     MOVE GFCTT3-HIDTFD-FLEXZ-AGPTO TO HIDTFD-FLEXZ-AGPTO
080826                                                    OF GFCTB049.
080826     EXEC SQL
080826       SELECT
080826              CSERVC_TARIF,
080826              CAGPTO_CTA,
080826              HIDTFD_FLEXZ_AGPTO,
080826              DINIC_FLEXZ_AGPTO,
080826              DFIM_FLEXZ_AGPTO,
080826              QFRANQ_FLEXZ_AGPTO,
080826              CPER_FRANQ_AGPTO,
080826              VNEGOC_FLEXZ_AGPTO,
080826              PDESC_FLEXZ_AGPTO,
080826              QDIA_FLEXZ_AGPTO,
080826              CINDCD_AGPTO_TOT,
080826              DDIA_AGNDA,
080826              CINIC_PER_FRANQ,
080826              CTPO_OPER_MOTVO,
080826              CMOTVO_JUSTF,
080826              CEMPR_INC,
080826              CDEPDC_ENCRR_VGCIA,
080826              CFUNC_ENCRR_VGCIA,
080826              HVGCIA_REGRA,
080826              CPER_COBR_TARIF,
080826              CINIC_PER_COBR
080826       INTO
080826              :GFCTB049.CSERVC-TARIF,
080826              :GFCTB049.CAGPTO-CTA,
080826              :GFCTB049.HIDTFD-FLEXZ-AGPTO,
080826              :GFCTB049.DINIC-FLEXZ-AGPTO,
080826              :GFCTB049.DFIM-FLEXZ-AGPTO,
080826              :GFCTB049.QFRANQ-FLEXZ-AGPTO,
080826              :GFCTB049.CPER-FRANQ-AGPTO,
080826              :GFCTB049.VNEGOC-FLEXZ-AGPTO,
080826              :GFCTB049.PDESC-FLEXZ-AGPTO,
080826              :GFCTB049.QDIA-FLEXZ-AGPTO,
080826              :GFCTB049.CINDCD-AGPTO-TOT,
080826              :GFCTB049.DDIA-AGNDA,
080826              :GFCTB049.CINIC-PER-FRANQ,
080826              :GFCTB049.CTPO-OPER-MOTVO,
080826              :GFCTB049.CMOTVO-JUSTF,
080826              :GFCTB049.CEMPR-INC,
080826              :GFCTB049.CDEPDC-ENCRR-VGCIA,
080826              :GFCTB049.CFUNC-ENCRR-VGCIA,
080826              :GFCTB049.HVGCIA-REGRA,
080826              :GFCTB049.CPER-COBR-TARIF,
080826              :GFCTB049.CINIC-PER-COBR
080826       FROM   DB2PRD.FLEXZ_AGPTO_CTA
080826         WHERE
080826             CSERVC_TARIF       = :GFCTB049.CSERVC-TARIF
080826         AND CAGPTO_CTA         = :GFCTB049.CAGPTO-CTA
080826         AND HIDTFD_FLEXZ_AGPTO IN
080826             (SELECT MAX(HIDTFD_FLEXZ_AGPTO)
080826              FROM DB2PRD.FLEXZ_AGPTO_CTA
080826              WHERE
080826                  CSERVC_TARIF  = :GFCTB049.CSERVC-TARIF
080826              AND CAGPTO_CTA    = :GFCTB049.CAGPTO-CTA)
080826     END-EXEC.
080826
080826     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
080826        (SQLWARN0                EQUAL 'W'      )
080826         MOVE 'DB2'              TO ERR-TIPO-ACESSO
080826         MOVE 'FLEXZ_AGPTO_CTA ' TO ERR-DBD-TAB
080826         MOVE 'SELECT  '         TO ERR-FUN-COMANDO
080826         MOVE  SQLCODE           TO ERR-SQL-CODE
080826         MOVE '0095'             TO ERR-LOCAL
080826         MOVE  SPACES            TO ERR-SEGM
080826         PERFORM 9999-PROCESSAR-ROTINA-ERRO
080826     END-IF.
080826
080826     IF  SQLCODE                 EQUAL +100
080826         GO TO 3950-99-FIM
080826     END-IF.
080826
080826     MOVE DFIM-FLEXZ-AGPTO       OF GFCTB049
080826                                 TO WRK-DATA-DB2.
080826
080826     STRING WRK-ANO-DB2 WRK-MES-DB2 WRK-DIA-DB2
080826     DELIMITED BY SIZE         INTO WRK-DFIM-FLEX-R.
080826
080826     MOVE GFCTT3-DADOS-CHAVE     TO GFCTT6-DADOS-CHAVE.
080826     MOVE GFCTT3-DADOS           TO GFCTT6-DADOS.
080826     MOVE GFCTT3-DADOS-AGPTO04   TO GFCTT6-DADOS-AGPTO04.
080826     MOVE GFCTT3-DADOS-AGPTO05   TO GFCTT6-DADOS-AGPTO05.
080826     MOVE GFCTT3-DADOS-AGPTO09   TO GFCTT6-DADOS-AGPTO09.
080826     MOVE GFCTT3-DADOS-AGPTO15   TO GFCTT6-DADOS-AGPTO15.
080826     MOVE GFCTT3-DADOS-AGPTO16   TO GFCTT6-DADOS-AGPTO16.
080826     MOVE GFCTT3-DADOS-AGPTO17   TO GFCTT6-DADOS-AGPTO17.
080826     MOVE GFCTT3-DADOS-AGPTO18   TO GFCTT6-DADOS-AGPTO18.
080826     MOVE GFCTT3-DADOS-AGPTO19   TO GFCTT6-DADOS-AGPTO19.
080826     MOVE GFCTT3-DADOS-2         TO GFCTT6-DADOS-2.
080826
080826     IF  WRK-DFIM-FLEX           GREATER WRK-DPROCM
080826         MOVE 'FINALIZADA'       TO GFCTT6-MENSAGEM
080826         MOVE CSERVC-TARIF       OF GFCTB049
080826                                 TO GFCTT5-CSERVC-TARIF
080826         MOVE CAGPTO-CTA         OF GFCTB049
080826                                 TO GFCTT5-CAGPTO-CTA
080826         MOVE HIDTFD-FLEXZ-AGPTO OF GFCTB049
080826                                 TO GFCTT5-HIDTFD-FLEXZ-AGPTO
080826         MOVE DINIC-FLEXZ-AGPTO  OF GFCTB049
080826                                 TO GFCTT5-DATA-INICIO
080826        MOVE DFIM-FLEXZ-AGPTO   OF GFCTB049
080826                                 TO GFCTT5-DATA-FIM
080826         MOVE DPROCM-ATUAL       OF GFCTB0A1
080826                                 TO GFCTT5-DATA-FIM-ATU
080826         PERFORM 3211-GRAVAR-FLEXZFIM
080826     ELSE
080826         MOVE 'NAO ATUALIZADA'   TO GFCTT6-MENSAGEM
080826     END-IF.
080826
080826     PERFORM 3100-GRAVAR-DEFERATU.
080826
080826*----------------------------------------------------------------*
080826 3950-99-FIM.                    EXIT.
080826*----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS REALIZADOS NO FINAL DO PROCESSAMENTO          *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS-DEFERIDO      GREATER ZEROS
               PERFORM 4100-EMITIR-TOTAIS
           END-IF.

           CLOSE DEFERIDO
                 DEFERATU
                 FLEXZFIM.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITIR TOTAIS                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-TOTAIS              SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-DEFERIDO     TO WRK-MASCARA.

           DISPLAY '******************** GFCT1705 ******************'.
           DISPLAY '*                                              *'.
           DISPLAY '*              PROGRAMA GFCT1705               *'.
           DISPLAY '*              -----------------               *'.
           DISPLAY '*                                              *'.
           DISPLAY '*      PROCESSAMENTO EFETUADO COM SUCESSO      *'.
           DISPLAY '*                                              *'.
           DISPLAY '*  LIDOS NO ARQUIVO DEFERIDO...: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-DEFERATU     TO WRK-MASCARA.

           DISPLAY '*  GRAVADOS NO ARQUIVO DEFERATU: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-FLEXZFIM     TO WRK-MASCARA.

           DISPLAY '*  GRAVADOS NO ARQUIVO FLEXZFIM: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.
           DISPLAY '******************** GFCT1705 ******************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXECUTA PROCEDIMENTO DE CANCELAMENTO DO PGM EM CASO DE ERRO *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1705'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
