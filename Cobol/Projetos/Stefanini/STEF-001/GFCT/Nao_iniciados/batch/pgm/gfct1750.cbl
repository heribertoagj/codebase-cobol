      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1750.
       AUTHOR.     VITOR VIGANO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT1750                                     *
      *    PROGRAMADOR.:  VITOR VIGANO             - CPM / PATO BRANCO *
      *    ANALISTA CPM:  ELIAS BOSCATO            - CPM / PATO BRANCO *
      *    ANALISTA....:  MARIA SALVE TORQUATO     - PROCWORK / GP.50  *
      *    DATA........:  07/07/2006                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  GRAVACAO  DE  ARQUIVO COM AS FLEXIBILIZACOES *
      *      POR AGENCIA / CONTA A VENCER NO PROXIMO MES, PARA EMISSAO *
      *      DE RELATORIO MENSAL.                                      *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                         INCLUDE/BOOK     *
      *                   FLEXINDV                      I#GFCTT9       *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                   TABLE                       INCLUDE/BOOK     *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *                   DB2PRD.FLEXZ_INDVD_CTA        GFCTB058       *
      *                   DB2PRD.PDIDO_FLEXZ_CTA        GFCTB0B5       *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100.      *
      *    I#GFCTT9 - LAYOUT ARQUIVO DE FLEXIBILIZACOES DEFERIDAS.     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - MODULO PARA ACESSO DB2.                          *
      *    POOL7100 - MODULO PARA TRATAMENTO DE ERROS.                 *
      *----------------------------------------------------------------
      *    ULTIMA ALTERACAO EM 16/01/2007 - VALERIA                    *
      *    VERIFICA SE O ANO EH BISSEXTO PARA MOVER 28 OU 29 PARA O DIA*
      *    DA DATA FINAL DO CURSOR                                     *
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

           SELECT FLEXINDV ASSIGN      TO UT-S-FLEXINDV
           FILE STATUS                 IS WRK-FS-FLEXINDV.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO COM FLEXIBILIZACOES DEFERIDAS               *
      *            ORG. SEQUENCIAL     -  LRECL = 300                  *
      *----------------------------------------------------------------*

       FD  FLEXINDV
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTT9.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(033)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(022)         VALUE
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-TABELA            PIC  9(009)   COMP-3 VALUE ZEROS.
       77  ACU-GRAVADOS-FLEXINDV       PIC  9(009)   COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(030)         VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC  Z.ZZZ.ZZZ.999  VALUE ZEROS.
       77  WRK-FIM-CSR01-GFCTB058      PIC  X(001)         VALUE SPACES.
       77  WRK-DATA-INI-TAB            PIC  X(010)         VALUE SPACES.
       77  WRK-DATA-FIM-TAB            PIC  X(010)         VALUE SPACES.

       77  WRK-QUOCIENTE               PIC  9(003)         VALUE ZEROS.
       77  WRK-RESTO                   PIC  9(003)         VALUE ZEROS.

       01  WRK-DATA-INI.
           05 WRK-DIA-INI              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-MES-INI              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-ANO-INI              PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-FIM.
           05 WRK-DIA-FIM              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-MES-FIM              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-ANO-FIM              PIC  9(004)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'AREA PARA TESTE DE FILE STATUS'.
      *----------------------------------------------------------------*

       01  WRK-FS-FLEXINDV             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(029)         VALUE
           'AREA PARA FORMATACAO DE ERROS'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-POOL7100.
           05 FILLER                   PIC  X(008)         VALUE
               '** ERRO '.
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE
               ' - FILE STATUS = '.
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE
               ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(018)         VALUE
           'AREA PARA POOL7100'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(013)         VALUE
           'AREA PARA DB2'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB058
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B5
           END-EXEC.

           EXEC SQL
               DECLARE CSR01-GFCTB058 CURSOR FOR
               SELECT
                   CDEPDC_CTA_CLI,
                   CNRO_CTA_CLI,
                   CSERVC_TARIF,
                   DINIC_FLEXZ_CTA,
                   DFIM_FLEXZ_CTA,
                   QFRANQ_FLEXZ_CTA,
                   CPER_FRANQ_CTA,
                   VNEGOC_FLEXZ_CTA,
                   PDESC_FLEXZ_CTA,
                   CTPO_OPER_MOTVO,
                   CMOTVO_JUSTF
             FROM  DB2PRD.FLEXZ_INDVD_CTA
             WHERE
                   CDEPDC_ENCRR_VGCIA = :GFCTB058.CDEPDC-ENCRR-VGCIA AND
                   DFIM_FLEXZ_CTA    >= :WRK-DATA-INI-TAB            AND
                   DFIM_FLEXZ_CTA    <= :WRK-DATA-FIM-TAB
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    INICIA ROTINAS DO PROGRAMA.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-TRATAR-CURSOR.

           PERFORM 3000-PROCESSAR      UNTIL
           WRK-FIM-CSR01-GFCTB058      EQUAL 'S'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE INICIALIZA O PROGRAMA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           PERFORM 1100-ACESSAR-GFCTB0A1.

           OPEN OUTPUT FLEXINDV.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FS-FLEXINDV.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE ACESSO A TABELA GFCTB0A1.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-ACESSAR-GFCTB0A1           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC
                                       OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC

           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W' )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0010'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-INI
                                          WRK-DATA-FIM.
           MOVE 01                     TO WRK-DIA-INI.
           MOVE 31                     TO WRK-DIA-FIM.

           IF (WRK-MES-INI             NOT EQUAL 12)
               ADD 1                   TO WRK-MES-INI
           ELSE
               MOVE 01                 TO WRK-MES-INI
               ADD 1                   TO WRK-ANO-INI
           END-IF.

           IF (WRK-MES-FIM             NOT EQUAL 12)
               ADD 1                   TO WRK-MES-FIM
           ELSE
               MOVE 01                 TO WRK-MES-FIM
               ADD 1                   TO WRK-ANO-FIM
           END-IF.

           IF (WRK-MES-FIM             EQUAL 02)
               DIVIDE WRK-ANO-FIM      BY 4
               GIVING WRK-QUOCIENTE REMAINDER WRK-RESTO
               IF WRK-RESTO            EQUAL ZEROS
                   MOVE 29             TO WRK-DIA-FIM
               ELSE
                   MOVE 28             TO WRK-DIA-FIM
               END-IF
           ELSE
               IF (WRK-MES-FIM         EQUAL 04 OR 06 OR 09 OR 11)
                   MOVE 30             TO WRK-DIA-FIM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTA FILE STATUS DO ARQUIVO FLEXINDV.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-TESTAR-FS-FLEXINDV         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-FLEXINDV         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXINDV'         TO WRK-NOME-ARQ
               MOVE WRK-FS-FLEXINDV    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE CONTROLE DO CURSOR CSR01-GFCTB058.                *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-TRATAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-ABRIR-CURSOR.
           PERFORM 2200-LER-CURSOR.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA ABERTURA DO CURSOR CSR01-GFCTB058.              *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO CDEPDC-ENCRR-VGCIA
                                       OF GFCTB058.
           MOVE WRK-DATA-INI           TO WRK-DATA-INI-TAB.
           MOVE WRK-DATA-FIM           TO WRK-DATA-FIM-TAB.

           EXEC SQL
               OPEN CSR01-GFCTB058
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_INDVD_CTA ' TO ERR-DBD-TAB
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0020'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA LEITURA DO CURSOR CSR01-GFCTB058.               *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-GFCTB058 INTO
                   :GFCTB058.CDEPDC-CTA-CLI,
                   :GFCTB058.CNRO-CTA-CLI,
                   :GFCTB058.CSERVC-TARIF,
                   :GFCTB058.DINIC-FLEXZ-CTA,
                   :GFCTB058.DFIM-FLEXZ-CTA,
                   :GFCTB058.QFRANQ-FLEXZ-CTA,
                   :GFCTB058.CPER-FRANQ-CTA,
                   :GFCTB058.VNEGOC-FLEXZ-CTA,
                   :GFCTB058.PDESC-FLEXZ-CTA,
                   :GFCTB058.CTPO-OPER-MOTVO,
                   :GFCTB058.CMOTVO-JUSTF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_INDVD_CTA ' TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0030'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF (SQLCODE                 EQUAL +100)
               MOVE 'S'                TO WRK-FIM-CSR01-GFCTB058
           ELSE
               ADD 1                   TO ACU-LIDOS-TABELA
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS PRINCIPAIS DO PROGRAMA.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-PESQUISAR-PEDIDO.
           PERFORM 3200-GRAVAR-FLEXINDV.
           PERFORM 2200-LER-CURSOR.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PESQUISA O PEDIDO ATRAVES DO ACESSO A TABELA GFCTB0B5       *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-PESQUISAR-PEDIDO           SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CBCO-CTA-CLI
                                       OF GFCTB0B5.
           MOVE CDEPDC-CTA-CLI         OF GFCTB058
                                       TO CJUNC-DEPDC
                                       OF GFCTB0B5.
           MOVE CNRO-CTA-CLI           OF GFCTB058
                                       TO CCTA-CLI
                                       OF GFCTB0B5.
           MOVE CSERVC-TARIF           OF GFCTB058
                                       TO CSERVC-TARIF
                                       OF GFCTB0B5.
           MOVE DINIC-FLEXZ-CTA        OF GFCTB058
                                       TO DINIC-FLEXZ-CTA
                                       OF GFCTB0B5.

           EXEC SQL
             SELECT
                   CDEPDC_ORIGE_FLEXZ
             INTO
                   :GFCTB0B5.CDEPDC-ORIGE-FLEXZ
             FROM   DB2PRD.PDIDO_FLEXZ_CTA
             WHERE
                   CBCO_CTA_CLI       = :GFCTB0B5.CBCO-CTA-CLI       AND
                   CJUNC_DEPDC        = :GFCTB0B5.CJUNC-DEPDC        AND
                   CCTA_CLI           = :GFCTB0B5.CCTA-CLI           AND
                   CSERVC_TARIF       = :GFCTB0B5.CSERVC-TARIF       AND
                   DINIC_FLEXZ_CTA    = :GFCTB0B5.DINIC-FLEXZ-CTA
           END-EXEC.

ATEN       IF (SQLCODE             NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_CTA ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0040'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF (SQLCODE                 EQUAL +100)
               MOVE ZEROS              TO GFCTT9-DEPEND-SOL
           ELSE
               MOVE CDEPDC-ORIGE-FLEXZ OF GFCTB0B5
                                       TO GFCTT9-DEPEND-SOL
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVA O ARQUIVO DE SAIDA FLEXINDV.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-GRAVAR-FLEXINDV            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           MOVE DFIM-FLEXZ-CTA         OF GFCTB058
                                       TO GFCTT9-DFIM-FLEXZ.
           MOVE DINIC-FLEXZ-CTA        OF GFCTB058
                                       TO GFCTT9-DINIC-FLEXZ.
           MOVE 01                     TO GFCTT9-TIPO-FLEX.
           MOVE CDEPDC-CTA-CLI         OF GFCTB058
                                       TO GFCTT9-AGENCIA.
           MOVE CNRO-CTA-CLI           OF GFCTB058
                                       TO GFCTT9-CONTA.
           MOVE CSERVC-TARIF           OF GFCTB058
                                       TO GFCTT9-CSERVC-TARIF.
           MOVE QFRANQ-FLEXZ-CTA       OF GFCTB058
                                       TO GFCTT9-QFRANQ-FLEXZ.
           MOVE VNEGOC-FLEXZ-CTA       OF GFCTB058
                                       TO GFCTT9-VNEGOC-FLEXZ.
           MOVE PDESC-FLEXZ-CTA        OF GFCTB058
                                       TO GFCTT9-PDESC-FLEXZ.
           MOVE CTPO-OPER-MOTVO        OF GFCTB058
                                       TO GFCTT9-CTPO-OPER-MOTVO.
           MOVE CMOTVO-JUSTF           OF GFCTB058
                                       TO GFCTT9-CMOTVO-JUSTF.
           MOVE ZEROS                  TO GFCTT9-AG-VIP
                                          GFCTT9-CTA-VIP
                                          GFCTT9-SEGMTO
                                          GFCTT9-CAGPTO-CTA
                                          GFCTT9-CCGC-AGP04
                                          GFCTT9-CFLIAL-AGP04
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
                                          GFCTT9-UF-AGP19
                                          GFCTT9-DESCR-MUN-AGP18
                                          GFCTT9-DESCR-UF-AGP19.

           WRITE GFCTT9-REGISTRO.

           PERFORM 1200-TESTAR-FS-FLEXINDV.

           ADD 1                       TO ACU-GRAVADOS-FLEXINDV.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZA O PROGRAMA.                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE FLEXINDV.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FS-FLEXINDV.
           PERFORM 4200-FECHAR-CURSOR.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITE O DISPLAY DE PROCESSAMENTO CONCLUIDO COM SUCESSO.     *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           IF (ACU-GRAVADOS-FLEXINDV   GREATER ZEROS)
               DISPLAY '*---------------------------------------------*'
               DISPLAY '*             PROGRAMA GFCT1750               *'
               DISPLAY '*             -----------------               *'
               DISPLAY '*                                             *'
               DISPLAY '*     PROCESSAMENTO EFETUADO COM SUCESSO      *'
               DISPLAY '*                                             *'
               DISPLAY '*---------------------------------------------*'
               DISPLAY '*---------------------------------------------*'

               MOVE ACU-LIDOS-TABELA   TO WRK-MASCARA
               DISPLAY '************************************************
      -        '***************'
               DISPLAY '*  TOTAL DE REGISTROS LIDOS    EM FLEXIBILIZ =
      -        '' WRK-MASCARA
                            ' *'
               MOVE ACU-GRAVADOS-FLEXINDV
                                       TO WRK-MASCARA
               DISPLAY '*  TOTAL DE REGISTROS GRAVADOS EM FLEXVINDV  =
      -        '' WRK-MASCARA
                            ' *'
               DISPLAY '************************************************
      -        '***************'
           ELSE
               DISPLAY '*---------------------------------------------*'
               DISPLAY '*             PROGRAMA GFCT1750               *'
               DISPLAY '*             -----------------               *'
               DISPLAY '*                                             *'
               DISPLAY '*          ARQUIVO DE FLEXINDV VAZIO          *'
               DISPLAY '*                                             *'
               DISPLAY '*---------------------------------------------*'
           END-IF.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA FECHAMENTO DO CURSOR CSR01-GFCTB058.            *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR01-GFCTB058
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_INDVD_CTA ' TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0050'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSA ROTINA DE ERRO DO PROGRAMA.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1750'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
