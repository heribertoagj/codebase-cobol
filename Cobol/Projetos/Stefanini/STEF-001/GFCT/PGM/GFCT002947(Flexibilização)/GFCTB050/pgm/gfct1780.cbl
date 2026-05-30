      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1780.
       AUTHOR.     TEREZA RACHEL MAFIOLETI.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1780                                    *
      *    PROGRAMADORA:   TEREZA RACHEL MAFIOLETI - CPM PATO BRANCO   *
      *    ANALISTA CPM:   FELIPE ALEX SOMMER      - CPM PATO BRANCO   *
      *    ANALISTA....:   MARIA V. S. TORQUATO    - PROCWORK / GP.50  *
      *    DATA........:   10/07/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GRAVACAO DE ARQUIVO COM AS FLEXIBILIZACOES  *
      *      POR ARGUMENTO A VENCER NO PROXIMO MES, PARA A EMISSAO DE  *
      *      RELATORIO MENSAL.                                         *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    FLEXAGPT                  I#GFCTT9          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.PARM_DATA_PROCM        GFCTB0A1      *
      *                    DB2PRD.PDIDO_FLEXZ_AGPTO      GFCTB0B2      *
      *                    DB2PRD.FLEXZ_GRP_UF           GFCTB057      *
      *                    DB2PRD.FLEXZ_GRP_MUN          GFCTB053      *
      *                    DB2PRD.FLEXZ_GRP_PSTAL        GFCTB055      *
      *                    DB2PRD.FLEXZ_GRP_PAB          GFCTB054      *
      *                    DB2PRD.FLEXZ_GRP_SGMTO        GFCTB056      *
      *                    DB2PRD.FLEXZ_GRP_DEPDC        GFCTB051      *
      *                    DB2PRD.FLEXZ_GRP_CLI          GFCTB050      *
      *                    DB2PRD.FLEXZ_GRP_EMPR         GFCTB052      *
      *                    DB2PRD.FLEXZ_AGPTO_CTA        GFCTB049      *
      *                    DB2PRD.TFLEXZ_GRP_RZ          GFCTB0M8      *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100.     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - CONEXAO COM DB2.                                 *
      *    POOL7100 - TRATAMENTO DE ERROS PADRAO BRADESCO.             *
      *================================================================*
080828*----------------------------------------------------------------*
080828*    ULTIMA ALTERACAO - VINICIUS MADUREIRA                       *
080828*    INCLUSAO DE CAMPOS NA TABELA GFCTB0B2 E GFCTB049            *
080828*    E ALTERACAO DA INC GFCTT3                                   *
080828*================================================================*
      *   PROJETO - PWT-09-2711 - ALTERACAO JUNHO/2010                 *
      *   - INCLUSAO DA TABELA GFCTB0M8 - TFLEXZ_GRP_RZ                *
      *   - GRAVAR INFORMACOES PARA OS NOVOS RELATORIOS:               *
      *              AGRUPAMENTOS 20-BRADESCO EXPRESSO E               *
      *                           26-RAZAO                             *
      *================================================================*
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

           SELECT FLEXAGPT ASSIGN      TO UT-S-FLEXAGPT
           FILE STATUS                 IS WRK-FS-FLEXAGPT.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE FLEXIBILIZACOES DEFERIDAS  -  FLEXAGPT   *
      *            ORG. SEQUENCIAL                    -  LRECL = 300   *
      *----------------------------------------------------------------*

       FD  FLEXAGPT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTT9'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS                   PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS                PIC  9(011) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA DE TESTES DE FILE STATUS'.
      *----------------------------------------------------------------*

       77  WRK-FS-FLEXAGPT             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-Q                       PIC  9(004)         VALUE ZEROS.
       77  WRK-R                       PIC  9(004)         VALUE ZEROS.
       77  WRK-MASC                    PIC  Z.ZZZ.ZZZ.999  VALUE ZEROS.
       77  WRK-FIM-CSR01-GFCTB049      PIC  X(001)         VALUE SPACES.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       01  WRK-CPF-AUX.
           05 WRK-CPF                  PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-INI                PIC  X(010)         VALUE SPACES.
       01  WRK-DATA-INI-R              REDEFINES           WRK-DATA-INI.
           05 WRK-DIA-INI              PIC  9(002).
           05 FILLER                   PIC  X(001).
           05 WRK-MES-INI              PIC  9(002).
           05 FILLER                   PIC  X(001).
           05 WRK-ANO-INI              PIC  9(004).

       01  WRK-DATA-FIM                PIC  X(010)         VALUE SPACES.
       01  WRK-DATA-FIM-R              REDEFINES           WRK-DATA-FIM.
           05 WRK-DIA-FIM              PIC  9(002).
           05 FILLER                   PIC  X(001).
           05 WRK-MES-FIM              PIC  9(002).
           05 FILLER                   PIC  X(001).
           05 WRK-ANO-FIM              PIC  9(004).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ERRO - FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-TEXTO.
           05 FILLER                   PIC  X(008)         VALUE
              '** ERRO '.
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE
              ' - FILE STATUS ='.
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE ' **'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TRATAMENTO DE ERRO'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA DB2'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B2
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
             INCLUDE GFCTB052
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB049
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0M8
           END-EXEC.

           EXEC SQL
             DECLARE CSR01-GFCTB049 CURSOR FOR
080828        SELECT CSERVC_TARIF,
080828               CAGPTO_CTA,
080828               HIDTFD_FLEXZ_AGPTO,
080828               DINIC_FLEXZ_AGPTO,
080828               DFIM_FLEXZ_AGPTO,
080828               QFRANQ_FLEXZ_AGPTO,
080828               CPER_FRANQ_AGPTO,
080828               VNEGOC_FLEXZ_AGPTO,
080828               PDESC_FLEXZ_AGPTO,
080828               QDIA_FLEXZ_AGPTO,
080828               CINDCD_AGPTO_TOT,
080828               DDIA_AGNDA,
080828               CINIC_PER_FRANQ,
080828               CTPO_OPER_MOTVO,
080828               CMOTVO_JUSTF,
080828               CEMPR_INC,
080828               CDEPDC_ENCRR_VGCIA,
080828               CFUNC_ENCRR_VGCIA,
080828               HVGCIA_REGRA,
080828               CPER_COBR_TARIF,
080828               CINIC_PER_COBR
                FROM DB2PRD.FLEXZ_AGPTO_CTA
               WHERE CDEPDC_ENCRR_VGCIA = :GFCTB049.CDEPDC-ENCRR-VGCIA
                 AND DFIM_FLEXZ_AGPTO   >= :WRK-DATA-INI
                 AND DFIM_FLEXZ_AGPTO   <= :WRK-DATA-FIM
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS A SEREM PROCESSADAS.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR      UNTIL
               WRK-FIM-CSR01-GFCTB049  EQUAL 'F'.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONEXAO COM DB2, ABERTURA DE ARQUIVOS E OBTENCAO DE DATA.   *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN OUTPUT FLEXAGPT.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.
           PERFORM 1100-TESTAR-FS-FLEXAGPT.

           PERFORM 1200-OBTER-DPROCM.

           PERFORM 1300-OPEN-CSR01-GFCTB049.

           PERFORM 1400-FETCH-CSR01-GFCTB049.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO FLEXAGPT.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FS-FLEXAGPT         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-FLEXAGPT         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-FLEXAGPT    TO WRK-FILE-STATUS
               MOVE 'FLEXAGPT'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DPROCM ATUAL DA TABELA GFCTB0A1.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-OBTER-DPROCM               SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
             SELECT DPROCM_ATUAL
               INTO :GFCTB0A1.DPROCM-ATUAL
               FROM DB2PRD.PARM_DATA_PROCM
              WHERE CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 1210-OBTER-DATA-INI-FIM.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DATA INICIAL E FINAL PARA O CURSOR.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-OBTER-DATA-INI-FIM         SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-INI.
           MOVE 01                     TO WRK-DIA-INI.

           IF  WRK-MES-INI             NOT EQUAL 12
               ADD 01                  TO WRK-MES-INI
           ELSE
               MOVE 01                 TO WRK-MES-INI
               ADD 01                  TO WRK-ANO-INI
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-FIM.
           MOVE 31                     TO WRK-DIA-FIM.

           IF  WRK-MES-FIM             NOT EQUAL 12
               ADD 01                  TO WRK-MES-FIM
           ELSE
               MOVE 01                 TO WRK-MES-FIM
               ADD 01                  TO WRK-ANO-FIM
           END-IF.

           IF  WRK-MES-FIM             EQUAL 02
               MOVE ZEROS              TO WRK-Q
                                          WRK-R
               DIVIDE WRK-ANO-FIM      BY 4
                                       GIVING WRK-Q
                                       REMAINDER WRK-R
               IF  WRK-R               EQUAL ZEROS
                   MOVE 29             TO WRK-DIA-FIM
               ELSE
                   MOVE 28             TO WRK-DIA-FIM
               END-IF
           ELSE
               IF  WRK-MES-FIM         EQUAL 04 OR 06 OR 09 OR 11
                   MOVE 30             TO WRK-DIA-FIM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ABERTURA DO CURSOR DA TABELA GFCTB049.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-OPEN-CSR01-GFCTB049        SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO CDEPDC-ENCRR-VGCIA
                                       OF GFCTB049.

           EXEC SQL
               OPEN CSR01-GFCTB049
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_AGPTO_CTA ' TO ERR-DBD-TAB
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DA TABELA GFCTB049.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1400-FETCH-CSR01-GFCTB049       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH CSR01-GFCTB049
080828         INTO :GFCTB049.CSERVC-TARIF,
080828              :GFCTB049.CAGPTO-CTA,
080828              :GFCTB049.HIDTFD-FLEXZ-AGPTO,
080828              :GFCTB049.DINIC-FLEXZ-AGPTO,
080828              :GFCTB049.DFIM-FLEXZ-AGPTO,
080828              :GFCTB049.QFRANQ-FLEXZ-AGPTO,
080828              :GFCTB049.CPER-FRANQ-AGPTO,
080828              :GFCTB049.VNEGOC-FLEXZ-AGPTO,
080828              :GFCTB049.PDESC-FLEXZ-AGPTO,
080828              :GFCTB049.QDIA-FLEXZ-AGPTO,
080828              :GFCTB049.CINDCD-AGPTO-TOT,
080828              :GFCTB049.DDIA-AGNDA,
080828              :GFCTB049.CINIC-PER-FRANQ,
080828              :GFCTB049.CTPO-OPER-MOTVO,
080828              :GFCTB049.CMOTVO-JUSTF,
080828              :GFCTB049.CEMPR-INC,
080828              :GFCTB049.CDEPDC-ENCRR-VGCIA,
080828              :GFCTB049.CFUNC-ENCRR-VGCIA,
080828              :GFCTB049.HVGCIA-REGRA,
080828              :GFCTB049.CPER-COBR-TARIF,
080828              :GFCTB049.CINIC-PER-COBR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_AGPTO_CTA ' TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'F'                TO WRK-FIM-CSR01-GFCTB049
           ELSE
               ADD 1                   TO ACU-LIDOS
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL PARA A GRAVACAO DE FLEXAGPT.        *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-PESQUISAR-PEDIDO.

           PERFORM 2200-VALORIZAR-FLEXAGPT.

           EVALUATE CAGPTO-CTA         OF GFCTB049
               WHEN 004
                    PERFORM 2300-VERIFICAR-AGRU-CLI
               WHEN 014
                    PERFORM 2400-VERIFICAR-AGRU-AGE
               WHEN 009
                    PERFORM 2500-VERIFICAR-AGRU-SEG
               WHEN 015
                    PERFORM 2600-VERIFICAR-AGRU-PAB
               WHEN 016
                    PERFORM 2700-VERIFICAR-AGRU-EMP
               WHEN 017
LFM                 PERFORM 2800-VERIFICAR-AGRU-POS-EXP
               WHEN 018
                    PERFORM 2900-VERIFICAR-AGRU-MUN
               WHEN 019
                    PERFORM 2910-VERIFICAR-AGRU-UF
LFM            WHEN 020
LFM                 PERFORM 2800-VERIFICAR-AGRU-POS-EXP
LFM            WHEN 026
LFM                 PERFORM 2920-VERIFICAR-AGRU-RAZAO
           END-EVALUATE.

           WRITE GFCTT9-REGISTRO.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 1100-TESTAR-FS-FLEXAGPT.

           ADD 1                       TO ACU-GRAVADOS.

           PERFORM 1400-FETCH-CSR01-GFCTB049.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PESQUISA PEDIDO - GFCTB0B2.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-PESQUISAR-PEDIDO           SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA   OF GFCTB0B2.
           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF OF GFCTB0B2.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB0B2.

           EXEC SQL
             SELECT CDEPDC_ORIGE_FLEXZ
               INTO :GFCTB0B2.CDEPDC-ORIGE-FLEXZ
               FROM DB2PRD.PDIDO_FLEXZ_AGPTO
              WHERE CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
                AND CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
                AND HIDTFD_FLEXZ_AGPTO = :GFCTB0B2.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0040'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE ZEROS              TO CDEPDC-ORIGE-FLEXZ
                                       OF GFCTB0B2
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VALORIZA CAMPOS DO ARQUIVO DE SAIDA FLEXAGPT.               *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-VALORIZAR-FLEXAGPT         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTT9-REGISTRO
           INITIALIZE GFCTT9-REGISTRO.

           MOVE CDEPDC-ORIGE-FLEXZ     OF GFCTB0B2
                                       TO GFCTT9-DEPEND-SOL.
           MOVE DINIC-FLEXZ-AGPTO      OF GFCTB049
                                       TO GFCTT9-DINIC-FLEXZ.
           MOVE DFIM-FLEXZ-AGPTO       OF GFCTB049
                                       TO GFCTT9-DFIM-FLEXZ.
           MOVE 04                     TO GFCTT9-TIPO-FLEX.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO GFCTT9-CAGPTO-CTA.
           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO GFCTT9-CSERVC-TARIF.
           MOVE QFRANQ-FLEXZ-AGPTO     OF GFCTB049
                                       TO GFCTT9-QFRANQ-FLEXZ.
           MOVE VNEGOC-FLEXZ-AGPTO     OF GFCTB049
                                       TO GFCTT9-VNEGOC-FLEXZ.
           MOVE PDESC-FLEXZ-AGPTO      OF GFCTB049
                                       TO GFCTT9-PDESC-FLEXZ.
           MOVE CTPO-OPER-MOTVO        OF GFCTB049
                                       TO GFCTT9-CTPO-OPER-MOTVO.
           MOVE CMOTVO-JUSTF           OF GFCTB049
                                       TO GFCTT9-CMOTVO-JUSTF.
           MOVE ZEROS                  TO GFCTT9-AGENCIA
                                          GFCTT9-CONTA
                                          GFCTT9-AG-VIP
                                          GFCTT9-CTA-VIP
                                          GFCTT9-SEGMTO
ST2506*                                   GFCTT9-CCGC-AGP04
ST2506*                                   GFCTT9-CFLIAL-AGP04
                                          GFCTT9-CSGMTO-AGP09
                                          GFCTT9-CEMPR-AGP14
                                          GFCTT9-CDEPDC-AGP14
                                          GFCTT9-CEMPR-AGP15
                                          GFCTT9-CDEPDC-AGP15
                                          GFCTT9-CPOSTO-AGP15
                                          GFCTT9-CJUNC-AGP16
                                          GFCTT9-CCTA-AGP16
                                          GFCTT9-CEMPR-AGP17
                                          GFCTT9-CDEPDC-AGP17
                                          GFCTT9-CPOSTO-AGP17
                                          GFCTT9-CMUN-AGP18.
           MOVE SPACES                 TO GFCTT9-CCTRL-AGP04
ST2506                                    GFCTT9-CCGC-AGP04
ST2506                                    GFCTT9-CFLIAL-AGP04
                                          GFCTT9-UF-AGP19
                                          GFCTT9-DESCR-MUN-AGP18
                                          GFCTT9-DESCR-UF-AGP19.

080828     MOVE CPER-COBR-TARIF OF GFCTB049 TO GFCTT9-CPER-COBR-TARIF.
080828     MOVE CINIC-PER-COBR  OF GFCTB049 TO GFCTT9-CINIC-PER-COBR.

LFM        MOVE ZEROS                       TO GFCTT9-GRP-CTBIL-AGP26
LFM                                            GFCTT9-SGRP-CTBIL-AGP26.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA AGRUPAMENTO CLIENTE - GFCTB050.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-VERIFICAR-AGRU-CLI         SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA   OF GFCTB050.
           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF OF GFCTB050.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB050.

           EXEC SQL
ST2506*      SELECT CCGC_CPF,
ST2506*             CFLIAL_CGC,
ST2506*             CCTRL_CPF_CGC
ST2506*        INTO :GFCTB050.CCGC-CPF,
ST2506*             :GFCTB050.CFLIAL-CGC,
ST2506*             :GFCTB050.CCTRL-CPF-CGC
ST2506       SELECT CCGC_CPF_ST,
ST2506              CFLIAL_CGC_ST,
ST2506              CCTRL_CPF_CGC_ST
ST2506         INTO :GFCTB050.CCGC-CPF-ST,
ST2506              :GFCTB050.CFLIAL-CGC-ST,
ST2506              :GFCTB050.CCTRL-CPF-CGC-ST
               FROM DB2PRD.FLEXZ_GRP_CLI
              WHERE CSERVC_TARIF       = :GFCTB050.CSERVC-TARIF
                AND CAGPTO_CTA         = :GFCTB050.CAGPTO-CTA
                AND HIDTFD_FLEXZ_AGPTO = :GFCTB050.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_CLI   ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0050'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

ST2506*    MOVE CCGC-CPF               OF GFCTB050
ST2506*                                TO GFCTT9-CCGC-AGP04.
ST2506*    MOVE CFLIAL-CGC             OF GFCTB050
ST2506*                                TO GFCTT9-CFLIAL-AGP04.
ST2506*    MOVE CCTRL-CPF-CGC          OF GFCTB050
ST2506     MOVE CCGC-CPF-ST            OF GFCTB050
ST2506                                 TO GFCTT9-CCGC-AGP04.
ST2506     MOVE CFLIAL-CGC-ST          OF GFCTB050
ST2506                                 TO GFCTT9-CFLIAL-AGP04.
ST2506     MOVE CCTRL-CPF-CGC-ST       OF GFCTB050
                                       TO WRK-CPF.
           MOVE WRK-CPF-AUX            TO GFCTT9-CCTRL-AGP04.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA AGRUPAMENTO AGENCIA - GFCTB051.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-VERIFICAR-AGRU-AGE         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF OF GFCTB051.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA   OF GFCTB051.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB051.
           EXEC SQL
             SELECT CEMPR_INC,
                    CDEPDC
               INTO :GFCTB051.CEMPR-INC,
                    :GFCTB051.CDEPDC
               FROM DB2PRD.FLEXZ_GRP_DEPDC
              WHERE CSERVC_TARIF       = :GFCTB051.CSERVC-TARIF
                AND CAGPTO_CTA         = :GFCTB051.CAGPTO-CTA
                AND HIDTFD_FLEXZ_AGPTO = :GFCTB051.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_DEPDC ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0060'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CEMPR-INC OF GFCTB051  TO GFCTT9-CEMPR-AGP14.
           MOVE CDEPDC    OF GFCTB051  TO GFCTT9-CDEPDC-AGP14.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA AGRUPAMENTO SEGMENTO - GFCTB056.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-VERIFICAR-AGRU-SEG         SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA OF GFCTB056.
           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF OF GFCTB056.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB056.

           EXEC SQL
             SELECT CSGMTO_GSTAO_TARIF
               INTO :GFCTB056.CSGMTO-GSTAO-TARIF
               FROM DB2PRD.FLEXZ_GRP_SGMTO
              WHERE CSERVC_TARIF       = :GFCTB056.CSERVC-TARIF
                AND CAGPTO_CTA         = :GFCTB056.CAGPTO-CTA
                AND HIDTFD_FLEXZ_AGPTO = :GFCTB056.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_SGMTO ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0070'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CSGMTO-GSTAO-TARIF     OF GFCTB056
                                       TO GFCTT9-CSGMTO-AGP09.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA AGRUPAMENTO PAB - GFCTB054.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-VERIFICAR-AGRU-PAB         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF  OF GFCTB054.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA    OF GFCTB054.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB054.

           EXEC SQL
             SELECT CEMPR_INC,
                    CDEPDC,
                    CPOSTO_SERVC
               INTO :GFCTB054.CEMPR-INC,
                    :GFCTB054.CDEPDC,
                    :GFCTB054.CPOSTO-SERVC
               FROM DB2PRD.FLEXZ_GRP_PAB
              WHERE CSERVC_TARIF       = :GFCTB054.CSERVC-TARIF
                AND CAGPTO_CTA         = :GFCTB054.CAGPTO-CTA
                AND HIDTFD_FLEXZ_AGPTO = :GFCTB054.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_PAB   ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0080'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CEMPR-INC              OF GFCTB054
                                       TO GFCTT9-CEMPR-AGP15.
           MOVE CDEPDC                 OF GFCTB054
                                       TO GFCTT9-CDEPDC-AGP15.
           MOVE CPOSTO-SERVC           OF GFCTB054
                                       TO GFCTT9-CPOSTO-AGP15.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA ARGUMENTO EMPRESA - GFCTB052.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2700-VERIFICAR-AGRU-EMP         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF  OF GFCTB052.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA    OF GFCTB052.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB052.
           EXEC SQL
             SELECT CJUNC_DEPDC,
                    CCTA_CLI
               INTO :GFCTB052.CJUNC-DEPDC,
                    :GFCTB052.CCTA-CLI
               FROM DB2PRD.FLEXZ_GRP_EMPR
              WHERE CSERVC_TARIF       = :GFCTB052.CSERVC-TARIF
                AND CAGPTO_CTA         = :GFCTB052.CAGPTO-CTA
                AND HIDTFD_FLEXZ_AGPTO = :GFCTB052.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_EMPR  ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0090'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CJUNC-DEPDC            OF GFCTB052
                                       TO GFCTT9-CJUNC-AGP16.
           MOVE CCTA-CLI               OF GFCTB052
                                       TO GFCTT9-CCTA-AGP16.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA ARGUMENTO POSTAL - GFCTB055.                       *
      ******************************************************************
      *----------------------------------------------------------------*
LFM    2800-VERIFICAR-AGRU-POS-EXP SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF  OF GFCTB055.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA    OF GFCTB055.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB055.

           EXEC SQL
             SELECT CEMPR_INC,
                    CDEPDC,
                    CPOSTO_SERVC
               INTO :GFCTB055.CEMPR-INC,
                    :GFCTB055.CDEPDC,
                    :GFCTB055.CPOSTO-SERVC
               FROM DB2PRD.FLEXZ_GRP_PSTAL
              WHERE CSERVC_TARIF       = :GFCTB055.CSERVC-TARIF
                AND CAGPTO_CTA         = :GFCTB055.CAGPTO-CTA
                AND HIDTFD_FLEXZ_AGPTO = :GFCTB055.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_PSTAL ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0100'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CEMPR-INC              OF GFCTB055
                                       TO GFCTT9-CEMPR-AGP17.
           MOVE CDEPDC                 OF GFCTB055
                                       TO GFCTT9-CDEPDC-AGP17.
           MOVE CPOSTO-SERVC           OF GFCTB055
                                       TO GFCTT9-CPOSTO-AGP17.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA AGRUPAMENTO MUNICIPIO - GFCTB053.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2900-VERIFICAR-AGRU-MUN         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF  OF GFCTB053.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA    OF GFCTB053.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB053.

           EXEC SQL
             SELECT CMUN_IBGE
               INTO :GFCTB053.CMUN-IBGE
               FROM DB2PRD.FLEXZ_GRP_MUN
              WHERE CSERVC_TARIF       = :GFCTB053.CSERVC-TARIF
                AND CAGPTO_CTA         = :GFCTB053.CAGPTO-CTA
                AND HIDTFD_FLEXZ_AGPTO = :GFCTB053.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_MUN   ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0110'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CMUN-IBGE OF GFCTB053  TO GFCTT9-CMUN-AGP18.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA AGRUPAMENTO UF - GFCTB057.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2910-VERIFICAR-AGRU-UF          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF  OF GFCTB057.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA    OF GFCTB057.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB057.

           EXEC SQL
             SELECT CSGL_UF
               INTO :GFCTB057.CSGL-UF
               FROM DB2PRD.FLEXZ_GRP_UF
              WHERE CSERVC_TARIF       = :GFCTB057.CSERVC-TARIF
                AND CAGPTO_CTA         = :GFCTB057.CAGPTO-CTA
                AND HIDTFD_FLEXZ_AGPTO = :GFCTB057.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_UF    ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0120'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CSGL-UF OF GFCTB057    TO GFCTT9-UF-AGP19.

      *----------------------------------------------------------------*
       2910-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

LFM   ******************************************************************
LFM   *    VERIFICA AGRUPAMENTO RAZAO- GFCTB0M8
LFM   ******************************************************************
LFM   *----------------------------------------------------------------*
LFM    2920-VERIFICAR-AGRU-RAZAO       SECTION.
LFM   *----------------------------------------------------------------*
LFM
LFM        MOVE CSERVC-TARIF          OF GFCTB049
LFM                                   TO CSERVC-TARIF  OF GFCTB0M8.
LFM        MOVE CAGPTO-CTA            OF GFCTB049
LFM                                   TO CAGPTO-CTA    OF GFCTB0M8.
LFM        MOVE HIDTFD-FLEXZ-AGPTO    OF GFCTB049
LFM                                   TO HIDTFD-FLEXZ-AGPTO OF GFCTB0M8.
LFM
LFM        EXEC SQL
LFM          SELECT CGRP_CTBIL_ATUAL,
LFM                 CSGRP_CTBIL
LFM             INTO :GFCTB0M8.CGRP-CTBIL-ATUAL,
LFM                  :GFCTB0M8.CSGRP-CTBIL
LFM             FROM DB2PRD.TFLEXZ_GRP_RZ
LFM           WHERE CSERVC_TARIF       = :GFCTB0M8.CSERVC-TARIF
LFM             AND CAGPTO_CTA         = :GFCTB0M8.CAGPTO-CTA
LFM             AND HIDTFD_FLEXZ_AGPTO = :GFCTB0M8.HIDTFD-FLEXZ-AGPTO
LFM        END-EXEC.
LFM
LFM        IF (SQLCODE                 NOT EQUAL ZEROS) OR
LFM           (SQLWARN0                EQUAL 'W')
LFM            MOVE 'DB2'              TO ERR-TIPO-ACESSO
LFM            MOVE 'FLEXZ_GRP_UF    ' TO ERR-DBD-TAB
LFM            MOVE 'SELECT  '         TO ERR-FUN-COMANDO
LFM            MOVE  SQLCODE           TO ERR-SQL-CODE
LFM            MOVE '0120'             TO ERR-LOCAL
LFM            MOVE  SPACES            TO ERR-SEGM
LFM            PERFORM 9999-PROCESSAR-ROTINA-ERRO
LFM        END-IF.
LFM
LFM        MOVE CGRP-CTBIL-ATUAL       OF GFCTB0M8
LFM                                    TO GFCTT9-GRP-CTBIL-AGP26.
LFM        MOVE CSGRP-CTBIL            OF GFCTB0M8
LFM                                    TO GFCTT9-SGRP-CTBIL-AGP26.
LFM
LFM   *----------------------------------------------------------------*
LFM    2920-99-FIM.                    EXIT.
LFM   *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS REALIZADOS NO FINAL DO PROCESSAMENTO.         *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-CLOSE-CSR01-GFCTB049.

           IF  ACU-GRAVADOS            NOT EQUAL ZEROS
               PERFORM 3200-EMITIR-DISPLAY-TOTAL
           ELSE
               PERFORM 3300-EMITIR-DISPLAY-VAZIO
           END-IF.

           CLOSE FLEXAGPT.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.
           PERFORM 1100-TESTAR-FS-FLEXAGPT.

           STOP RUN.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FECHA O CURSOR GFCTB049.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-CLOSE-CSR01-GFCTB049       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR01-GFCTB049
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_AGPTO_CTA ' TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0130'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITE DISPLAY FINAL COM TOTAIS.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-EMITIR-DISPLAY-TOTAL       SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*-----------------------------------------------*'.
           DISPLAY '*           PROGRAMA GFCT1780                   *'.
           DISPLAY '*           -----------------                   *'.
           DISPLAY '*                                               *'.
           DISPLAY '*     PROCESSAMENTO EFETUADO COM SUCESSO        *'.
           DISPLAY '*-----------------------------------------------*'.
           DISPLAY '*-----------------------------------------------*'.

           DISPLAY '****************************************************
      -            '*************'.
           MOVE ACU-LIDOS              TO WRK-MASC.
           DISPLAY '*    TOTAL DE REGISTROS LIDOS    EM FLEXIBILIZ = '
                    WRK-MASC '  *'.
           MOVE ACU-GRAVADOS           TO WRK-MASC.
           DISPLAY '*    TOTAL DE REGISTROS GRAVADOS EM FLEXAGPT   = '
                    WRK-MASC '  *'.
           DISPLAY '****************************************************
      -            '*************'.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITE DISPLAY FINAL DE ARQUIVO DE SAIDA VAZIO.              *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-EMITIR-DISPLAY-VAZIO       SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*-----------------------------------------------*'.
           DISPLAY '*           PROGRAMA GFCT1780                   *'.
           DISPLAY '*           -----------------                   *'.
           DISPLAY '*                                               *'.
           DISPLAY '*        ARQUIVO DE FLEXAGPT VAZIO              *'.
           DISPLAY '*-----------------------------------------------*'.
           DISPLAY '*-----------------------------------------------*'.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXECUTA PROCEDIMENTO DE CANCELAMENTO DO PGM EM CASO DE ERRO *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1780'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

