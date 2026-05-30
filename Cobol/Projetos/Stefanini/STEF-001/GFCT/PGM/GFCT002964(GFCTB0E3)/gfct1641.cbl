      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1641.
       AUTHOR.     CARLOS BLANCO.
      *================================================================*
      *                     C P M   S I S T E M A S                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT1641                                     *
      *    PROGRAMADOR  : CARLOS BLANCO                  - CPM/FPOLIS  *
      *    ANALISTA CPM : ALEXANDRE PEREIRA              - CPM/FPOLIS  *
      *    ANALISTA     : MARIA V. TORQUATO - PROCWORK   - GRUPO 50    *
      *    DATA         : 31/01/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      SELECIONA AS TARIFAS LIBERADAS DA SUSPENSAO EM  D+1  DA   *
      *    TABELA GFCTB0E7 - DB2PRD.SUSP_TARIF_AGPTO POR AGRUPAMENTO.  *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                              INCLUDE/BOOK          *
      *      ARQTB0E7                              I#GFCTYK            *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *      TABLE                               INCLUDE/BOOK          *
      *      DB2PRD.PARM_DATA_PROCM                GFCTB0A1            *
      *      DB2PRD.SUSP_TARIF_AGPTO               GFCTB0E7            *
      *      DB2PRD.SUSP_GRP_CLI                   GFCTB0E3            *
      *      DB2PRD.SUSP_GRP_DEPDC                 GFCTB0E4            *
      *      DB2PRD.TSUSP_GRP_PAB                  GFCTB0I1            *
      *      DB2PRD.SUSP_GRP_MUN                   GFCTB0E5            *
      *      DB2PRD.SUSP_GRP_UF                    GFCTB0E6            *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL1050 - CONEXAO COM DB2.                               *
      *      POOL7100 - MODULO TRATAMENTO DE ERRO.                     *
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
       INPUT-OUTPUT                    SECTION.
      *================================================================*

       FILE-CONTROL.

           SELECT ARQTB0E7 ASSIGN      TO UT-S-ARQTB0E7
                      FILE STATUS      IS WRK-FS-ARQTB0E7.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *================================================================*
       FILE                            SECTION.
      *================================================================*

      *----------------------------------------------------------------*
      *   OUTPUT: REGISTROS TARIFAS LIBERADAS AGRUPAMENTO              *
      *           ORG.SEQUENCIAL  -  LRECL = 067                       *
      *----------------------------------------------------------------*

       FD  ARQTB0E7
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ARQTB0E7             PIC X(67).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   INICIO DA WORKING GFCT1641 *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05  ACU-LIDOS-CURSOR        PIC 9(10) COMP-3    VALUE ZEROS.
           05  ACU-GRAVA-ARQTB0E7      PIC 9(10) COMP-3    VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *---------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-BATCH               PIC X(08)           VALUE
               'BATCH'.
           05  WRK-MASCARA             PIC ZZZZ.ZZZ.999    VALUE ZEROS.
           05  WRK-FIM                 PIC X(01)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA TO FILE-STATUS       *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ARQTB0E7             PIC  X(02)          VALUE SPACES.
       01  WRK-ABERTURA                PIC  X(13)          VALUE
           'NA ABERTURA'.
       01  WRK-GRAVACAO                PIC  X(13)          VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC  X(13)          VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  MENSAGENS DE ERRO           *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-FS.
           05  FILLER                  PIC X(07)           VALUE SPACES.
           05  FILLER                  PIC X(07)           VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC X(13)           VALUE SPACES.
           05  FILLER                  PIC X(12)           VALUE
              ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC X(08)           VALUE SPACES.
           05  FILLER                  PIC X(17)           VALUE
              ' - FILE-STATUS = '.
           05  WRK-FS                  PIC X(02)           VALUE SPACES.
           05  FILLER                  PIC X(02)           VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA DO ARQUIVO ARQTB0E7     *'.
      *----------------------------------------------------------------*

       COPY I#GFCTYK.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA DO BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA DE TABELAS DB2         *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E7
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E3
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E4
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0I1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E5
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E6
           END-EXEC.

           EXEC SQL
             DECLARE CSR01-GFCTB0E7 CURSOR FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_SUSP_COBR,
                   CSEQ_AGPTO_CTA,
                   DFIM_SUSP_COBR,
                   CINDCD_LIBRC_COBR,
                   DCOBR_SUSP_LIBRD,
                   CINDCD_AGPTO_TOT
             FROM  DB2PRD.SUSP_TARIF_AGPTO
             WHERE DCOBR_SUSP_LIBRD  = :GFCTB0E7.DCOBR-SUSP-LIBRD
             AND   CINDCD_LIBRC_COBR = :GFCTB0E7.CINDCD-LIBRC-COBR
           END-EXEC.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL                                            *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0500-ABRIR-CURSOR.

           PERFORM 0600-LER-CURSOR.

           PERFORM 0700-PROCESSAR-DADOS
                                       UNTIL
                   WRK-FIM             EQUAL 'F'.

           PERFORM 1500-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA INICIAL DO PROGRAMA                                  *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           PERFORM 0200-PESQUISAR-DPROCM.

           OPEN OUTPUT ARQTB0E7.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA PESQUISAR DPROCM                               *
      *----------------------------------------------------------------*
       0200-PESQUISAR-DPROCM           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
                SELECT DPROCM_PROX
                INTO  :GFCTB0A1.DPROCM-PROX
                FROM   DB2PRD.PARM_DATA_PROCM
                WHERE  CSIST_PRINC     = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'PARM_DATA_PROCM   '
                                       TO ERR-DBD-TAB
                MOVE 'SELECT    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0010'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *      TESTE DE FILE STATUS DOS ARQUIVOS                         *
      *----------------------------------------------------------------*
       0300-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0400-TESTAR-FS-ARQTB0E7.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO ARQTB0E7                    *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-ARQTB0E7         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ARQTB0E7          NOT EQUAL '00'
              MOVE 'ARQTB0E7'          TO WRK-NOME-ARQ
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-ARQTB0E7     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA ABRIR CURSOR                                   *
      *----------------------------------------------------------------*
       0500-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-PROX OF GFCTB0A1
                                       TO DCOBR-SUSP-LIBRD  OF GFCTB0E7.

           MOVE 'A'                    TO CINDCD-LIBRC-COBR OF GFCTB0E7.

           EXEC SQL
               OPEN CSR01-GFCTB0E7
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'SUSP_TARIF_AGPTO  '
                                       TO ERR-DBD-TAB
                MOVE 'OPEN      '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0020'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA LER CURSOR                                     *
      *----------------------------------------------------------------*
       0600-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH  CSR01-GFCTB0E7     INTO
                   :GFCTB0E7.CSERVC-TARIF,
                   :GFCTB0E7.CAGPTO-CTA,
                   :GFCTB0E7.DINIC-SUSP-COBR,
                   :GFCTB0E7.CSEQ-AGPTO-CTA,
                   :GFCTB0E7.DFIM-SUSP-COBR,
                   :GFCTB0E7.CINDCD-LIBRC-COBR,
                   :GFCTB0E7.DCOBR-SUSP-LIBRD,
                   :GFCTB0E7.CINDCD-AGPTO-TOT
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'SUSP_TARIF_AGPTO  '
                                       TO ERR-DBD-TAB
                MOVE 'FETCH     '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0030'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'F'                TO WRK-FIM
           ELSE
               ADD 1                   TO ACU-LIDOS-CURSOR
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO                                              *
      *----------------------------------------------------------------*
       0700-PROCESSAR-DADOS            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0800-MOVER-DADOS.

           EVALUATE CAGPTO-CTA OF GFCTB0E7
               WHEN 4
                   PERFORM 0900-AGRUP-CLIENTE
               WHEN 14
                   PERFORM 1000-AGRUP-AGENCIA
               WHEN 15
                   PERFORM 1100-AGRUP-PAB
               WHEN 18
                   PERFORM 1200-AGRUP-MUNICIPIO
               WHEN 19
                   PERFORM 1300-AGRUP-UF
           END-EVALUATE.

           PERFORM 1400-GRAVAR-ARQTB0E7.

           PERFORM 0600-LER-CURSOR.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA REALIZAR MOVIMENTACOES                          *
      *----------------------------------------------------------------*
       0800-MOVER-DADOS                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCTYK-GFCTB0E7.

           MOVE CSERVC-TARIF OF GFCTB0E7
                                       TO GFCTYK-CSERVC-TARIF.
           MOVE CAGPTO-CTA OF GFCTB0E7 TO GFCTYK-CAGPTO-CTA.
           MOVE DINIC-SUSP-COBR OF GFCTB0E7
                                       TO GFCTYK-DINIC-SUSP-COBR.
           MOVE CSEQ-AGPTO-CTA OF GFCTB0E7
                                       TO GFCTYK-CSEQ-AGPTO-CTA.
           MOVE DFIM-SUSP-COBR OF GFCTB0E7
                                       TO GFCTYK-DFIM-SUSP-COBR.
           MOVE CINDCD-LIBRC-COBR OF GFCTB0E7
                                       TO GFCTYK-CINDCD-LIBRC-COBR.
           MOVE DCOBR-SUSP-LIBRD OF GFCTB0E7
                                       TO GFCTYK-DCOBR-SUSP-LIBRD.
           MOVE CINDCD-AGPTO-TOT OF GFCTB0E7
                                       TO GFCTYK-CINDCD-AGPTO-TOT.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA AGRUPAMENTO CLIENTE                             *
      *----------------------------------------------------------------*
       0900-AGRUP-CLIENTE              SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0E7
                                       TO CSERVC-TARIF OF GFCTB0E3.
           MOVE CAGPTO-CTA OF GFCTB0E7 TO CAGPTO-CTA OF GFCTB0E3.
           MOVE DINIC-SUSP-COBR OF GFCTB0E7
                                       TO DINIC-SUSP-COBR OF GFCTB0E3.
           MOVE CSEQ-AGPTO-CTA OF GFCTB0E7
                                       TO CSEQ-AGPTO-CTA OF GFCTB0E3.

           EXEC SQL
             SELECT CCGC_CPF,
                    CFLIAL_CGC,
                    CCTRL_CPF_CGC,
ST25X6              CCGC_CPF_ST,
ST25X6              CFLIAL_CGC_ST,
ST25X6              CCTRL_CPF_CGC_ST
             INTO  :GFCTB0E3.CCGC-CPF,
                   :GFCTB0E3.CFLIAL-CGC,
                   :GFCTB0E3.CCTRL-CPF-CGC,
ST25X6             :GFCTB0E3.CCGC-CPF-ST,
ST25X6             :GFCTB0E3.CFLIAL-CGC-ST,
ST25X6             :GFCTB0E3.CCTRL-CPF-CGC-ST
             FROM   DB2PRD.SUSP_GRP_CLI
             WHERE  CSERVC_TARIF       = :GFCTB0E3.CSERVC-TARIF    AND
                    CAGPTO_CTA         = :GFCTB0E3.CAGPTO-CTA      AND
                    DINIC_SUSP_COBR    = :GFCTB0E3.DINIC-SUSP-COBR AND
                    CSEQ_AGPTO_CTA     = :GFCTB0E3.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'SUSP_GRP_CLI      '
                                       TO ERR-DBD-TAB
               MOVE 'SELECT    '       TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0040'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
ST25X6*        MOVE CCGC-CPF OF GFCTB0E3
ST25X6*                                TO GFCTYK-CCGC-CPF
ST25X6         MOVE CCGC-CPF-ST OF GFCTB0E3
ST25X6                                 TO GFCTYK-CCGC-CPF
ST25X6*        MOVE CFLIAL-CGC OF GFCTB0E3
ST25X6*                                TO GFCTYK-CFLIAL-CGC
ST25X6         MOVE CFLIAL-CGC-ST OF GFCTB0E3
ST25X6                                 TO GFCTYK-CFLIAL-CGC
ST25X6*        MOVE CCTRL-CPF-CGC OF GFCTB0E3
ST25X6*                                TO GFCTYK-CCTRL-CPF-CGC
ST25X6         MOVE CCTRL-CPF-CGC-ST OF GFCTB0E3
ST25X6                                 TO GFCTYK-CCTRL-CPF-CGC
           END-IF.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA AGRUPAMENTO AGENCIA                             *
      *----------------------------------------------------------------*
       1000-AGRUP-AGENCIA              SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0E7
                                       TO CSERVC-TARIF OF GFCTB0E4.
           MOVE CAGPTO-CTA OF GFCTB0E7 TO CAGPTO-CTA OF GFCTB0E4.
           MOVE DINIC-SUSP-COBR OF GFCTB0E7
                                       TO DINIC-SUSP-COBR OF GFCTB0E4.
           MOVE CSEQ-AGPTO-CTA OF GFCTB0E7
                                       TO CSEQ-AGPTO-CTA OF GFCTB0E4.

           EXEC SQL
             SELECT CEMPR_INC,
                    CDEPDC
             INTO  :GFCTB0E4.CEMPR-INC,
                   :GFCTB0E4.CDEPDC
             FROM   DB2PRD.SUSP_GRP_DEPDC
             WHERE  CSERVC_TARIF       = :GFCTB0E4.CSERVC-TARIF    AND
                    CAGPTO_CTA         = :GFCTB0E4.CAGPTO-CTA      AND
                    DINIC_SUSP_COBR    = :GFCTB0E4.DINIC-SUSP-COBR AND
                    CSEQ_AGPTO_CTA     = :GFCTB0E4.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'SUSP_GRP_DEPDC    '
                                       TO ERR-DBD-TAB
               MOVE 'SELECT    '       TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0050'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CEMPR-INC OF GFCTB0E4
                                       TO GFCTYK-CEMPR-INC
               MOVE CDEPDC OF GFCTB0E4 TO GFCTYK-CDEPDC
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA AGRUPAMENTO PAB                                 *
      *----------------------------------------------------------------*
       1100-AGRUP-PAB              SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0E7
                                       TO CSERVC-TARIF OF GFCTB0I1.
           MOVE CAGPTO-CTA OF GFCTB0E7 TO CAGPTO-CTA OF GFCTB0I1.
           MOVE DINIC-SUSP-COBR OF GFCTB0E7
                                       TO DINIC-SUSP-COBR OF GFCTB0I1.
           MOVE CSEQ-AGPTO-CTA OF GFCTB0E7
                                       TO CSEQ-AGPTO-CTA OF GFCTB0I1.

           EXEC SQL
             SELECT CEMPR_INC,
                    CDEPDC,
                    CPOSTO_SERVC
             INTO  :GFCTB0I1.CEMPR-INC,
                   :GFCTB0I1.CDEPDC,
                   :GFCTB0I1.CPOSTO-SERVC
             FROM   DB2PRD.TSUSP_GRP_PAB
             WHERE  CSERVC_TARIF       = :GFCTB0I1.CSERVC-TARIF    AND
                    CAGPTO_CTA         = :GFCTB0I1.CAGPTO-CTA      AND
                    DINIC_SUSP_COBR    = :GFCTB0I1.DINIC-SUSP-COBR AND
                    CSEQ_AGPTO_CTA     = :GFCTB0I1.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TSUSP_GRP_PAB     '
                                       TO ERR-DBD-TAB
               MOVE 'SELECT    '       TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0060'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CEMPR-INC OF GFCTB0I1
                                       TO GFCTYK-CEMPR-INC
               MOVE CDEPDC OF GFCTB0I1 TO GFCTYK-CDEPDC
               MOVE CPOSTO-SERVC OF GFCTB0I1
                                       TO GFCTYK-CPOSTO-SERVC
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA AGRUPAMENTO MUNICIPIO                           *
      *----------------------------------------------------------------*
       1200-AGRUP-MUNICIPIO              SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0E7
                                       TO CSERVC-TARIF OF GFCTB0E5.
           MOVE CAGPTO-CTA OF GFCTB0E7 TO CAGPTO-CTA OF GFCTB0E5.
           MOVE DINIC-SUSP-COBR OF GFCTB0E7
                                       TO DINIC-SUSP-COBR OF GFCTB0E5.
           MOVE CSEQ-AGPTO-CTA OF GFCTB0E7
                                       TO CSEQ-AGPTO-CTA OF GFCTB0E5.

           EXEC SQL
             SELECT CMUN_IBGE
             INTO  :GFCTB0E5.CMUN-IBGE
             FROM   DB2PRD.SUSP_GRP_MUN
             WHERE  CSERVC_TARIF       = :GFCTB0E5.CSERVC-TARIF    AND
                    CAGPTO_CTA         = :GFCTB0E5.CAGPTO-CTA      AND
                    DINIC_SUSP_COBR    = :GFCTB0E5.DINIC-SUSP-COBR AND
                    CSEQ_AGPTO_CTA     = :GFCTB0E5.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'SUSP_GRP_MUN      '
                                       TO ERR-DBD-TAB
               MOVE 'SELECT    '       TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0070'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CMUN-IBGE OF GFCTB0E5
                                       TO GFCTYK-CMUN-IBGE
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA AGRUPAMENTO UF                                  *
      *----------------------------------------------------------------*
       1300-AGRUP-UF                   SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0E7
                                       TO CSERVC-TARIF OF GFCTB0E6.
           MOVE CAGPTO-CTA OF GFCTB0E7 TO CAGPTO-CTA OF GFCTB0E6.
           MOVE DINIC-SUSP-COBR OF GFCTB0E7
                                       TO DINIC-SUSP-COBR OF GFCTB0E6.
           MOVE CSEQ-AGPTO-CTA OF GFCTB0E7
                                       TO CSEQ-AGPTO-CTA OF GFCTB0E6.

           EXEC SQL
             SELECT CSGL_UF
             INTO  :GFCTB0E6.CSGL-UF
             FROM   DB2PRD.SUSP_GRP_UF
             WHERE  CSERVC_TARIF       = :GFCTB0E6.CSERVC-TARIF    AND
                    CAGPTO_CTA         = :GFCTB0E6.CAGPTO-CTA      AND
                    DINIC_SUSP_COBR    = :GFCTB0E6.DINIC-SUSP-COBR AND
                    CSEQ_AGPTO_CTA     = :GFCTB0E6.CSEQ-AGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'SUSP_GRP_UF       '
                                       TO ERR-DBD-TAB
               MOVE 'SELECT    '       TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0080'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CSGL-UF OF GFCTB0E6
                                       TO GFCTYK-CSGL-UF
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA GRAVAR ARQUIVO ARQTB0E7                         *
      *----------------------------------------------------------------*
       1400-GRAVAR-ARQTB0E7            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-REG-ARQTB0E7       FROM GFCTYK-GFCTB0E7.

           PERFORM 0400-TESTAR-FS-ARQTB0E7.

           ADD 1                       TO ACU-GRAVA-ARQTB0E7.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZAR PROGRAMA                              *
      *----------------------------------------------------------------*
       1500-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1600-FECHAR-CURSOR.

           PERFORM 1700-EMITIR-DISPLAY.

           CLOSE ARQTB0E7.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FECHAR CURSOR                                   *
      *----------------------------------------------------------------*
       1600-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0E7
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'SUSP_TARIF_AGPTO  '
                                       TO ERR-DBD-TAB
                MOVE 'CLOSE     '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0090'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA EMITIR ESTATISTICAS DE PROCESSAMENTO            *
      *----------------------------------------------------------------*
       1700-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS-CURSOR        NOT EQUAL ZEROS
             DISPLAY '*-----------------------------------------------*'
             DISPLAY '*           PROGRAMA GFCT1641                   *'
             DISPLAY '*           -----------------                   *'
             DISPLAY '*                                               *'
             DISPLAY '*     PROCESSAMENTO EFETUADO COM SUCESSO        *'
             DISPLAY '*-----------------------------------------------*'
             DISPLAY '*-----------------------------------------------*'
             DISPLAY '*************************************************'
                     '*************************'
             MOVE ACU-LIDOS-CURSOR     TO WRK-MASCARA
             DISPLAY '*        TOTAL DE REGISTROS LIDOS       NO CURSOR'
                     '        = ' WRK-MASCARA '  *'
             MOVE ACU-GRAVA-ARQTB0E7   TO WRK-MASCARA
             DISPLAY '*        TOTAL DE REGISTROS GRAVADOS    EM ARQTB0'
                     'E7      = ' WRK-MASCARA '  *'
             DISPLAY '*************************************************'
                     '*************************'
           ELSE
             DISPLAY '*-----------------------------------------------*'
             DISPLAY '*           PROGRAMA GFCT1641                   *'
             DISPLAY '*           -----------------                   *'
             DISPLAY '*                                               *'
             DISPLAY '*          CURSOR TARIFAS VAZIO                 *'
             DISPLAY '*-----------------------------------------------*'
           END-IF.

      *----------------------------------------------------------------*
       1700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1641'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
