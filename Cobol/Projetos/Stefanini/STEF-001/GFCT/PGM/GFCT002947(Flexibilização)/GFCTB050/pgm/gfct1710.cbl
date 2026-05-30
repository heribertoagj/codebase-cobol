      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT1710.
       AUTHOR.     MARIELI MUSIAL.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1710                                    *
      *    PROGRAMADORA:   MARIELI MUSIAL          - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ADRIANO LOPES SANT'ANA  - CPM PATO BRANCO   *
      *    ANALISTA....:   MARIA VALERIA TORQUATO  - PROCWORK / GP.50  *
      *    DATA........:   12/06/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   LE ARQUIVO DE PEDIDOS DEFERIDOS DE FLEXIBI- *
      *      LIZACAO POR AGRUPAMENTO E INCLUI NA TABELA QUENTE E TABE- *
      *      LAS FILHAS.                                               *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    DEFERIDO                  I#GFCTT3          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
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
      *                    DB2PRD.CTRL_ROTNA_GFCT        GFCTB000      *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100.     *
      *    I#GFCTT3 - LAY-OUT DO ARQUIVO DE FLEXIBILIZACOES DEFERIDAS. *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - CONEXAO COM DB2.                                 *
      *    POOL7100 - TRATAMENTO DE ERROS PADRAO BRADESCO.             *
      *================================================================*
080826*----------------------------------------------------------------*
080826*    ULTIMA ALTERACAO - VINICIUS CRISTIAN MADUREIRA - SONDPROCW  *
080826*    INCLUSAO DE CAMPOS NA TABELA E GFCTB049                     *
080826*    E ALTERACAO DA INC GFCTT3                                   *
080826*================================================================*
      *----------------------------------------------------------------*
      *    PROJETO - PWT-09-2711 - ALTERACAO JUNHO/2010                *
      *    INCLUSAO DA TABELA GFCTB0M8 -  TFLEXZ_GRP_RZ                *
      *    E ATUALIZAR ARQUIVO I#GFCTT6                               *
      *================================================================
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
      *    INPUT : ARQUIVO DE FLEXIBILIZACOES DEFERIDAS.               *
      *            ORG. SEQUENCIAL     -   LRECL = 400                 *
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
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-GRAVA-GFCTB049          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-GFCTB054          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-GFCTB051          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-GFCTB055          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-GFCTB050          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-GFCTB056          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-GFCTB053          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-GFCTB057          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-GFCTB052          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-COMMIT                  PIC  9(009) COMP-3  VALUE ZEROS.
LFM    77  ACU-GRAVA-GFCTB0M8          PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE TESTES DE FILE STATUS *'.
      *----------------------------------------------------------------*

       77  WRK-FS-DEFERIDO             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MASCARA                 PIC  ZZZZ.ZZZ.999   VALUE ZEROS.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA CHAVES *'.
      *---------------------------------------------------------------*

       01  WRK-CH-RESTART.
           05 WRK-REST-CSERVC-TARIF    PIC  9(005)         VALUE ZEROS.
           05 WRK-REST-CAGPTO-CTA      PIC  9(003)         VALUE ZEROS.
           05 WRK-REST-HIDTFD-FLEXZ-AG PIC  X(026)         VALUE SPACES.

       01  WRK-CH-MOVEVNTO.
           05 WRK-MOV-CSERVC-TARIF     PIC  9(005)         VALUE ZEROS.
           05 WRK-MOV-CAGPTO-CTA       PIC  9(003)         VALUE ZEROS.
           05 WRK-MOV-HIDTFD-FLEXZ-AG  PIC  X(026)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS DE NULO *'.
      *---------------------------------------------------------------*

       01  WRK-GFCTB000-NULL.
           03 WRK-CUSUAR-SENHA-NULL    PIC S9(004) COMP    VALUE +0.
           03 WRK-HATULZ-NULL          PIC S9(004) COMP    VALUE +0.
           03 WRK-WCHAVE-RESTT-NULL    PIC S9(004) COMP    VALUE +0.

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
             INCLUDE GFCTB000
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

           PERFORM 5000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONEXAO COM DB2 E ATERTURA DO ARQUIVO                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT  DEFERIDO.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FS-DEFERIDO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO DEFERIDO                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FS-DEFERIDO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DEFERIDO         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-DEFERIDO    TO WRK-FILE-STATUS
               MOVE 'DEFERIDO'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA SE ARQUIVO ESTA VAZIO E SE EXISTE RESTART          *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-DEFERIDO.

           IF WRK-FS-DEFERIDO         EQUAL '10'
              DISPLAY '******************** GFCT1710 ******************'
              DISPLAY '*                                              *'
              DISPLAY '*              PROGRAMA GFCT1710               *'
              DISPLAY '*              -----------------               *'
              DISPLAY '*                                              *'
              DISPLAY '*          ARQUIVO DE DEFERIDO VAZIO           *'
              DISPLAY '*                                              *'
              DISPLAY '******************** GFCT1710 ******************'
              PERFORM 5000-FINALIZAR
           ELSE
               PERFORM 2200-VERIFICAR-RESTART
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

           IF  WRK-FS-DEFERIDO            EQUAL '10'
               MOVE HIGH-VALUES           TO WRK-CH-MOVEVNTO
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA               TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FS-DEFERIDO.

           MOVE GFCTT3-CSERVC-TARIF       TO WRK-MOV-CSERVC-TARIF.
           MOVE GFCTT3-CAGPTO-CTA         TO WRK-MOV-CAGPTO-CTA.
           MOVE GFCTT3-HIDTFD-FLEXZ-AGPTO TO WRK-MOV-HIDTFD-FLEXZ-AG.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA RESTART                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-VERIFICAR-RESTART          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1710'             TO CPROG            OF GFCTB000.

           EXEC SQL
             SELECT
                   WCHAVE_RESTT
             INTO
                   :GFCTB000.WCHAVE-RESTT
                     :WRK-WCHAVE-RESTT-NULL
             FROM   DB2PRD.CTRL_ROTNA_GFCT
             WHERE
                   CPROG              = :GFCTB000.CPROG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CTRL_ROTNA_GFCT'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 2210-INCLUIR-CTRL-RESTART
               MOVE LOW-VALUES         TO WCHAVE-RESTT-TEXT OF GFCTB000
           ELSE
               PERFORM 2220-VERIFICAR-CHAVE
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INCLUIR CTRL RESTART                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-INCLUIR-CTRL-RESTART       SECTION.
      *----------------------------------------------------------------*

           MOVE 1000                   TO QREG-COMIT        OF GFCTB000.
           MOVE 'N'                    TO CSIT-RESTT        OF GFCTB000.
           MOVE SPACES                 TO CUSUAR-SENHA      OF GFCTB000
                                          WCHAVE-RESTT-TEXT OF GFCTB000.
           MOVE LENGTH OF WRK-CH-RESTART
                                       TO WCHAVE-RESTT-LEN OF GFCTB000.
           MOVE '0001-01-01-01.01.01.000001'
                                       TO HATULZ            OF GFCTB000.
           MOVE 'XXXXXXX'              TO CJOB              OF GFCTB000.
           MOVE ZEROS                  TO WRK-CUSUAR-SENHA-NULL
                                          WRK-HATULZ-NULL
                                          WRK-WCHAVE-RESTT-NULL.

           EXEC SQL
             INSERT INTO DB2PRD.CTRL_ROTNA_GFCT
                  (CPROG,
                   CJOB,
                   QREG_COMIT,
                   CSIT_RESTT,
                   CUSUAR_SENHA,
                   HATULZ,
                   WCHAVE_RESTT)
               VALUES
                  (:GFCTB000.CPROG,
                   :GFCTB000.CJOB,
                   :GFCTB000.QREG-COMIT,
                   :GFCTB000.CSIT-RESTT,
                   :GFCTB000.CUSUAR-SENHA
                     :WRK-CUSUAR-SENHA-NULL,
                   :GFCTB000.HATULZ
                     :WRK-HATULZ-NULL,
                   :GFCTB000.WCHAVE-RESTT
                     :WRK-WCHAVE-RESTT-NULL)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -803) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CTRL_ROTNA_GFCT'  TO ERR-DBD-TAB
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA A CHAVE DE RESTART                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2220-VERIFICAR-CHAVE            SECTION.
      *----------------------------------------------------------------*

           IF  WRK-WCHAVE-RESTT-NULL   LESS ZEROS
               MOVE LOW-VALUES         TO WCHAVE-RESTT-TEXT OF GFCTB000
           END-IF.

           MOVE WCHAVE-RESTT-TEXT      OF GFCTB000
                                       TO WRK-CH-RESTART.

           PERFORM 2100-LER-DEFERIDO   UNTIL
                   WRK-CH-MOVEVNTO     GREATER WRK-CH-RESTART OR
                   WRK-CH-MOVEVNTO     EQUAL HIGH-VALUES.

           IF  WRK-CH-MOVEVNTO         EQUAL HIGH-VALUES
               DISPLAY '********** GFCT1710 **********'
               DISPLAY '*                            *'
               DISPLAY '*   REGISTRO NAO ENCOTRADO   *'
               DISPLAY '*        PARA RESTART        *'
               DISPLAY '*                            *'
               DISPLAY '********** GFCT1710 **********'
               PERFORM 5000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-INCLUIR-GFCTB049.

           EVALUATE GFCTT3-CAGPTO-CTA
               WHEN 15
                   PERFORM 3200-INCLUIR-GFCTB054
               WHEN 14
                   PERFORM 3300-INCLUIR-GFCTB051
               WHEN 17
                   PERFORM 3400-INCLUIR-GFCTB055
               WHEN 4
                   PERFORM 3500-INCLUIR-GFCTB050
               WHEN 9
                   PERFORM 3600-INCLUIR-GFCTB056
               WHEN 18
                   PERFORM 3700-INCLUIR-GFCTB053
               WHEN 19
                   PERFORM 3800-INCLUIR-GFCTB057
               WHEN 16
                   PERFORM 3900-INCLUIR-GFCTB052
LFM            WHEN 20
LFM                PERFORM 3400-INCLUIR-GFCTB055
LFM            WHEN 26
LFM                PERFORM 3910-INCLUIR-GFCTB0M8
           END-EVALUATE.

           ADD 1                       TO ACU-COMMIT
                                          ACU-GRAVA-GFCTB049.

           IF  ACU-COMMIT              GREATER 1000
               PERFORM 4000-EFETUAR-COMMIT
               PERFORM 4100-MOVER-CAMPOS
               PERFORM 4200-ATUALIZAR-GFCTB000
               MOVE ZEROS              TO ACU-COMMIT
           END-IF.

           PERFORM 2100-LER-DEFERIDO.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INCLUIR NA TABELA MAE QUENTE                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-INCLUIR-GFCTB049           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT3-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB049.
           MOVE GFCTT3-CAGPTO-CTA     TO CAGPTO-CTA         OF GFCTB049.
           MOVE GFCTT3-HIDTFD-FLEXZ-AGPTO
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB049.
           MOVE GFCTT3-DINIC-FLEXZ-AGPTO
                                      TO DINIC-FLEXZ-AGPTO  OF GFCTB049.
           MOVE GFCTT3-DFIM-FLEXZ-AGPTO
                                      TO DFIM-FLEXZ-AGPTO   OF GFCTB049.
           MOVE GFCTT3-QFRANQ-FLEXZ-AGPTO
                                      TO QFRANQ-FLEXZ-AGPTO OF GFCTB049.
           MOVE GFCTT3-CPER-FRANQ-AGPTO
                                      TO CPER-FRANQ-AGPTO   OF GFCTB049.
           MOVE GFCTT3-VNEGOC-FLEXZ-AGPTO
                                      TO VNEGOC-FLEXZ-AGPTO OF GFCTB049.
           MOVE GFCTT3-PDESC-FLEXZ-AGPTO
                                      TO PDESC-FLEXZ-AGPTO  OF GFCTB049.
           MOVE GFCTT3-DDIA-AGNDA     TO DDIA-AGNDA         OF GFCTB049.
           MOVE GFCTT3-QDIA-FLEXZ-AGPTO
                                      TO QDIA-FLEXZ-AGPTO   OF GFCTB049.
           MOVE GFCTT3-CINIC-PER-FRANQ
                                      TO CINIC-PER-FRANQ    OF GFCTB049.
           MOVE GFCTT3-CINDCD-AGPTO-TOT
                                      TO CINDCD-AGPTO-TOT   OF GFCTB049.
           MOVE GFCTT3-CTPO-OPER-MOTVO
                                      TO CTPO-OPER-MOTVO    OF GFCTB049.
           MOVE GFCTT3-CMOTVO-JUSTF   TO CMOTVO-JUSTF       OF GFCTB049.
           MOVE GFCTT3-CDEPDC-ORIGE-FLEXZ
                                      TO CEMPR-INC          OF GFCTB049.
           MOVE ZEROS                 TO CDEPDC-ENCRR-VGCIA OF GFCTB049
                                         CFUNC-ENCRR-VGCIA  OF GFCTB049.
           MOVE '0001-01-01-01.01.01.000001'
                                      TO HVGCIA-REGRA       OF GFCTB049.

080826     MOVE GFCTT3-CPER-COBR-TARIF TO CPER-COBR-TARIF   OF GFCTB049.
080826     MOVE GFCTT3-CINIC-PER-COBR  TO CINIC-PER-COBR    OF GFCTB049.

           EXEC SQL
             INSERT INTO DB2PRD.FLEXZ_AGPTO_CTA

                  (CSERVC_TARIF,
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
080826             CPER_COBR_TARIF,
080826             CINIC_PER_COBR)
               VALUES
                   (:GFCTB049.CSERVC-TARIF,
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
080826              :GFCTB049.CINIC-PER-COBR)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -803) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_AGPTO_CTA ' TO ERR-DBD-TAB
               MOVE 'INSERT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INCLUIR PAB 15 NA TABELA FILHA QUENTE                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-INCLUIR-GFCTB054           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT3-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB054.
           MOVE GFCTT3-CAGPTO-CTA     TO CAGPTO-CTA         OF GFCTB054.
           MOVE GFCTT3-HIDTFD-FLEXZ-AGPTO
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB054.
           MOVE GFCTT3-CEMPR-INC15    TO CEMPR-INC          OF GFCTB054.
           MOVE GFCTT3-CDEPDC15       TO CDEPDC             OF GFCTB054.
           MOVE GFCTT3-CPOSTO-SERVC15 TO CPOSTO-SERVC       OF GFCTB054.

           EXEC SQL
             INSERT INTO DB2PRD.FLEXZ_GRP_PAB
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   HIDTFD_FLEXZ_AGPTO,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC)
               VALUES
                  (:GFCTB054.CSERVC-TARIF,
                   :GFCTB054.CAGPTO-CTA,
                   :GFCTB054.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB054.CEMPR-INC,
                   :GFCTB054.CDEPDC,
                   :GFCTB054.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -803) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_PAB'    TO ERR-DBD-TAB
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0040'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                        TO ACU-GRAVA-GFCTB054.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INCLUIR AGENCIA 05 NA TABELA FILHA QUENTE                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-INCLUIR-GFCTB051           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT3-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB051.
           MOVE GFCTT3-CAGPTO-CTA     TO CAGPTO-CTA         OF GFCTB051.
           MOVE GFCTT3-HIDTFD-FLEXZ-AGPTO
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB051.
           MOVE GFCTT3-CEMPR-INC05    TO CEMPR-INC          OF GFCTB051.
           MOVE GFCTT3-CDEPDC05       TO CDEPDC             OF GFCTB051.

           EXEC SQL
             INSERT INTO DB2PRD.FLEXZ_GRP_DEPDC
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   HIDTFD_FLEXZ_AGPTO,
                   CEMPR_INC,
                   CDEPDC)
               VALUES
                  (:GFCTB051.CSERVC-TARIF,
                   :GFCTB051.CAGPTO-CTA,
                   :GFCTB051.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB051.CEMPR-INC,
                   :GFCTB051.CDEPDC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -803) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_DEPDC'  TO ERR-DBD-TAB
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0050'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                        TO ACU-GRAVA-GFCTB051.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INCLUIR POSTAL 17 NA TABELA FILHA QUENTE                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-INCLUIR-GFCTB055           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT3-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB055.
           MOVE GFCTT3-CAGPTO-CTA     TO CAGPTO-CTA         OF GFCTB055.
           MOVE GFCTT3-HIDTFD-FLEXZ-AGPTO
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB055.
           MOVE GFCTT3-CEMPR-INC17    TO CEMPR-INC          OF GFCTB055.
           MOVE GFCTT3-CDEPDC17       TO CDEPDC             OF GFCTB055.
           MOVE GFCTT3-CPOSTO-SERVC17 TO CPOSTO-SERVC       OF GFCTB055.

           EXEC SQL
             INSERT INTO DB2PRD.FLEXZ_GRP_PSTAL
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   HIDTFD_FLEXZ_AGPTO,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC)
               VALUES
                  (:GFCTB055.CSERVC-TARIF,
                   :GFCTB055.CAGPTO-CTA,
                   :GFCTB055.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB055.CEMPR-INC,
                   :GFCTB055.CDEPDC,
                   :GFCTB055.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -803) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_PSTAL'  TO ERR-DBD-TAB
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0060'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                        TO ACU-GRAVA-GFCTB055.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INCLUIR CLIENTE 04 NA TABELA FILHA QUENTE                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-INCLUIR-GFCTB050           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT3-CSERVC-TARIF    TO CSERVC-TARIF     OF GFCTB050.
           MOVE GFCTT3-CAGPTO-CTA      TO CAGPTO-CTA       OF GFCTB050.
           MOVE GFCTT3-HIDTFD-FLEXZ-AGPTO
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB050.
ST2506*    MOVE GFCTT3-CCGC-CPF04      TO CCGC-CPF         OF GFCTB050.
ST2506*    MOVE GFCTT3-CFLIAL-CGC04    TO CFLIAL-CGC       OF GFCTB050.
ST2506*    MOVE GFCTT3-CCTRL-CPF-CGC04 TO CCTRL-CPF-CGC    OF GFCTB050.
ST2506     MOVE GFCTT3-CCGC-CPF04      TO CCGC-CPF-ST      OF GFCTB050.
ST2506     MOVE GFCTT3-CFLIAL-CGC04    TO CFLIAL-CGC-ST    OF GFCTB050.
ST2506     MOVE GFCTT3-CCTRL-CPF-CGC04 TO CCTRL-CPF-CGC-ST OF GFCTB050.

           EXEC SQL
             INSERT INTO DB2PRD.FLEXZ_GRP_CLI
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   HIDTFD_FLEXZ_AGPTO,
                   CCGC_CPF,
                   CFLIAL_CGC,
                   CCTRL_CPF_CGC,
ST2506             CCGC_CPF_ST,
ST2506             CFLIAL_CGC_ST,
ST2506             CCTRL_CPF_CGC_ST)
               VALUES
                  (:GFCTB050.CSERVC-TARIF,
                   :GFCTB050.CAGPTO-CTA,
                   :GFCTB050.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB050.CCGC-CPF,
                   :GFCTB050.CFLIAL-CGC,
                  :GFCTB050.CCTRL-CPF-CGC,
ST2506             :GFCTB050.CCGC-CPF-ST,
ST2506             :GFCTB050.CFLIAL-CGC-ST,
ST2506             :GFCTB050.CCTRL-CPF-CGC-ST)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -803) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_CLI'    TO ERR-DBD-TAB
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0070'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                        TO ACU-GRAVA-GFCTB050.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INCLUIR SEGMENTO 09 NA TABELA FILHA QUENTE                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3600-INCLUIR-GFCTB056           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT3-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB056.
           MOVE GFCTT3-CAGPTO-CTA     TO CAGPTO-CTA         OF GFCTB056.
           MOVE GFCTT3-HIDTFD-FLEXZ-AGPTO
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB056.
           MOVE GFCTT3-CSGMTO-GSTAO-TARIF09
                                      TO CSGMTO-GSTAO-TARIF OF GFCTB056.

           EXEC SQL
             INSERT INTO DB2PRD.FLEXZ_GRP_SGMTO
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   HIDTFD_FLEXZ_AGPTO,
                   CSGMTO_GSTAO_TARIF)
               VALUES
                  (:GFCTB056.CSERVC-TARIF,
                   :GFCTB056.CAGPTO-CTA,
                   :GFCTB056.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB056.CSGMTO-GSTAO-TARIF)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -803) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_SGMTO'  TO ERR-DBD-TAB
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0080'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                        TO ACU-GRAVA-GFCTB056.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INCLUIR MUNICIPIO 18 NA TABELA FILHA QUENTE                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3700-INCLUIR-GFCTB053           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT3-CSERVC-TARIF  TO CSERVC-TARIF       OF GFCTB053.
           MOVE GFCTT3-CAGPTO-CTA    TO CAGPTO-CTA         OF GFCTB053.
           MOVE GFCTT3-HIDTFD-FLEXZ-AGPTO
                                     TO HIDTFD-FLEXZ-AGPTO OF GFCTB053.
           MOVE GFCTT3-CMUN-IBGE18   TO CMUN-IBGE          OF GFCTB053.

           EXEC SQL
             INSERT INTO DB2PRD.FLEXZ_GRP_MUN
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   HIDTFD_FLEXZ_AGPTO,
                   CMUN_IBGE)
               VALUES
                  (:GFCTB053.CSERVC-TARIF,
                   :GFCTB053.CAGPTO-CTA,
                   :GFCTB053.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB053.CMUN-IBGE)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -803) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_MUN'    TO ERR-DBD-TAB
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0090'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                        TO ACU-GRAVA-GFCTB053.

      *----------------------------------------------------------------*
       3700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INCLUIR UF 19 NA TABELA FILHA QUENTE                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3800-INCLUIR-GFCTB057           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT3-CSERVC-TARIF TO CSERVC-TARIF       OF GFCTB057.
           MOVE GFCTT3-CAGPTO-CTA   TO CAGPTO-CTA         OF GFCTB057.
           MOVE GFCTT3-HIDTFD-FLEXZ-AGPTO
                                    TO HIDTFD-FLEXZ-AGPTO OF GFCTB057.
           MOVE GFCTT3-CSGL-UF19    TO CSGL-UF            OF GFCTB057.

           EXEC SQL
             INSERT INTO DB2PRD.FLEXZ_GRP_UF
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   HIDTFD_FLEXZ_AGPTO,
                   CSGL_UF)
               VALUES
                  (:GFCTB057.CSERVC-TARIF,
                   :GFCTB057.CAGPTO-CTA,
                   :GFCTB057.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB057.CSGL-UF)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -803) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_UF'     TO ERR-DBD-TAB
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0100'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                        TO ACU-GRAVA-GFCTB057.

      *----------------------------------------------------------------*
       3800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INCLUIR EMPRESA 16 NA TABELA FILHA QUENTE                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3900-INCLUIR-GFCTB052           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT3-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB052.
           MOVE GFCTT3-CAGPTO-CTA     TO CAGPTO-CTA         OF GFCTB052.
           MOVE GFCTT3-HIDTFD-FLEXZ-AGPTO
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB052.
           MOVE GFCTT3-CJUNC-DEPDC16  TO CJUNC-DEPDC        OF GFCTB052.
           MOVE GFCTT3-CCTA-CLI16     TO CCTA-CLI           OF GFCTB052.
           MOVE GFCTT3-CAG-EMPR16     TO CAG-EMPR           OF GFCTB052.
           MOVE GFCTT3-CRZ-CTA-EMPR16 TO CRZ-CTA-EMPR       OF GFCTB052.
           MOVE GFCTT3-CNRO-CTA-DEB16 TO CNRO-CTA-DEB       OF GFCTB052.
           MOVE GFCTT3-CLCTO-EMPR16   TO CLCTO-EMPR         OF GFCTB052.
           MOVE GFCTT3-PPGTO-RTEIO-EMPR16
                                      TO PPGTO-RTEIO-EMPR   OF GFCTB052.
           MOVE GFCTT3-RSEGDA-LIN-EXTRT16
                                      TO RSEGDA-LIN-EXTRT   OF GFCTB052.

           EXEC SQL
             INSERT INTO DB2PRD.FLEXZ_GRP_EMPR
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   HIDTFD_FLEXZ_AGPTO,
                   CJUNC_DEPDC,
                   CCTA_CLI,
                   CAG_EMPR,
                   CRZ_CTA_EMPR,
                   CNRO_CTA_DEB,
                   CLCTO_EMPR,
                   PPGTO_RTEIO_EMPR,
                   RSEGDA_LIN_EXTRT)
               VALUES
                  (:GFCTB052.CSERVC-TARIF,
                   :GFCTB052.CAGPTO-CTA,
                   :GFCTB052.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB052.CJUNC-DEPDC,
                   :GFCTB052.CCTA-CLI,
                   :GFCTB052.CAG-EMPR,
                   :GFCTB052.CRZ-CTA-EMPR,
                   :GFCTB052.CNRO-CTA-DEB,
                   :GFCTB052.CLCTO-EMPR,
                   :GFCTB052.PPGTO-RTEIO-EMPR,
                   :GFCTB052.RSEGDA-LIN-EXTRT)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -803) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_EMPR'   TO ERR-DBD-TAB
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0110'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                        TO ACU-GRAVA-GFCTB052.

      *----------------------------------------------------------------*
       3900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

LFM   ******************************************************************
LFM   *    INCLUIR RAZAO   26 NA TABELA FILHA QUENTE                   *
LFM   ******************************************************************
LFM   *----------------------------------------------------------------*
LFM    3910-INCLUIR-GFCTB0M8           SECTION.
LFM   *----------------------------------------------------------------*
LFM
LFM        MOVE GFCTT3-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB0M8.
LFM        MOVE GFCTT3-CAGPTO-CTA     TO CAGPTO-CTA         OF GFCTB0M8.
LFM        MOVE GFCTT3-HIDTFD-FLEXZ-AGPTO
LFM                                   TO HIDTFD-FLEXZ-AGPTO OF GFCTB0M8.
LFM        MOVE GFCTT3-GRP-CTBIL26    TO CGRP-CTBIL-ATUAL OF GFCTB0M8.
LFM        MOVE GFCTT3-SGRP-CTBIL26   TO CSGRP-CTBIL      OF GFCTB0M8.
LFM        MOVE GFCTT3-EMPR-CTBIL26   TO CEMPR            OF GFCTB0M8.
LFM
LFM        EXEC SQL
LFM           INSERT INTO DB2PRD.TFLEXZ_GRP_RZ
LFM                (CSERVC_TARIF,
LFM                 CAGPTO_CTA,
LFM                 HIDTFD_FLEXZ_AGPTO,
LFM                 CEMPR,
LFM                 CGRP_CTBIL_ATUAL,
LFM                 CSGRP_CTBIL)
LFM             VALUES
LFM                (:GFCTB0M8.CSERVC-TARIF,
LFM                 :GFCTB0M8.CAGPTO-CTA,
LFM                 :GFCTB0M8.HIDTFD-FLEXZ-AGPTO,
LFM                 :GFCTB0M8.CEMPR,
LFM                 :GFCTB0M8.CGRP-CTBIL-ATUAL,
LFM                 :GFCTB0M8.CSGRP-CTBIL)
LFM        END-EXEC.
LFM
LFM        IF (SQLCODE                 NOT EQUAL ZEROS AND -803) OR
LFM          (SQLWARN0                EQUAL 'W'      )
LFM           MOVE 'DB2'              TO ERR-TIPO-ACESSO
LFM           MOVE 'FLEXZ_GRP_EMPR'   TO ERR-DBD-TAB
LFM           MOVE 'INSERT'           TO ERR-FUN-COMANDO
LFM           MOVE  SQLCODE           TO ERR-SQL-CODE
LFM           MOVE '0110'             TO ERR-LOCAL
LFM           MOVE  SPACES            TO ERR-SEGM
LFM           PERFORM 9999-PROCESSAR-ROTINA-ERRO
LFM        END-IF.
LFM
LFM        ADD 1                        TO ACU-GRAVA-GFCTB0M8.
LFM
LFM   *----------------------------------------------------------------*
LFM    3910-99-FIM.                    EXIT.
LFM   *----------------------------------------------------------------*

      ******************************************************************
      *    EFETUAR COMMIT                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-EFETUAR-COMMIT             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               COMMIT
           END-EXEC.

           IF  SQLCODE                 NOT EQUAL ZEROS
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'COMMIT'           TO ERR-DBD-TAB
                                           ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0120'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MOVE CAMPOS DA CHAVE                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-MOVER-CAMPOS               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MOV-CSERVC-TARIF    TO WRK-REST-CSERVC-TARIF.
           MOVE WRK-MOV-CAGPTO-CTA      TO WRK-REST-CAGPTO-CTA.
           MOVE WRK-MOV-HIDTFD-FLEXZ-AG TO WRK-REST-HIDTFD-FLEXZ-AG.
           MOVE 'S'                     TO CSIT-RESTT      OF GFCTB000.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ATUALIZAR GFCTB000                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-ATUALIZAR-GFCTB000         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CH-RESTART         TO WCHAVE-RESTT-TEXT OF GFCTB000.
           MOVE 'GFCT1710'             TO CPROG             OF GFCTB000.
           MOVE LENGTH OF WRK-CH-RESTART
                                       TO WCHAVE-RESTT-LEN  OF GFCTB000.
           MOVE ZEROS                  TO WRK-WCHAVE-RESTT-NULL.

           EXEC SQL
             UPDATE   DB2PRD.CTRL_ROTNA_GFCT
               SET
                   CSIT_RESTT         = :GFCTB000.CSIT-RESTT,
                   WCHAVE_RESTT       = :GFCTB000.WCHAVE-RESTT
                                          :WRK-WCHAVE-RESTT-NULL
             WHERE
                   CPROG              = :GFCTB000.CPROG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CTRL_ROTNA_GFCT'  TO ERR-DBD-TAB
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0130'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS REALIZADOS NO FINAL DO PROCESSAMENTO          *
      ******************************************************************
      *----------------------------------------------------------------*
       5000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-COMMIT              GREATER ZEROS
               PERFORM 4000-EFETUAR-COMMIT
               PERFORM 5100-DELETAR-RESTART
           END-IF.

           CLOSE DEFERIDO.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FS-DEFERIDO.

           IF  ACU-GRAVA-GFCTB049      NOT EQUAL ZEROS
               PERFORM 5200-EMITIR-TOTAIS
           END-IF.

           STOP RUN.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    DELETAR REGISTRO DE RESTART                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       5100-DELETAR-RESTART            SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1710'           TO CPROG             OF GFCTB000.

           EXEC SQL
             DELETE FROM    DB2PRD.CTRL_ROTNA_GFCT
             WHERE
                   CPROG              = :GFCTB000.CPROG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CTRL_ROTNA_GFCT'  TO ERR-DBD-TAB
               MOVE 'DELETE'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0140'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       5100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITIR TOTAIS                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       5200-EMITIR-TOTAIS              SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-GRAVA-GFCTB049     TO WRK-MASCARA.

           DISPLAY '******************** GFCT1710 ******************'.
           DISPLAY '*                                              *'.
           DISPLAY '*              PROGRAMA GFCT1710               *'.
           DISPLAY '*              -----------------               *'.
           DISPLAY '*                                              *'.
           DISPLAY '*      PROCESSAMENTO EFETUADO COM SUCESSO      *'.
           DISPLAY '*                                              *'.
           DISPLAY '*   GRAVADOS NA TABELA GFCTB049: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-GFCTB057     TO WRK-MASCARA.

           DISPLAY '*   GRAVADOS NA TABELA GFCTB057: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-GFCTB053     TO WRK-MASCARA.

           DISPLAY '*   GRAVADOS NA TABELA GFCTB053: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-GFCTB055     TO WRK-MASCARA.

           DISPLAY '*   GRAVADOS NA TABELA GFCTB055: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-GFCTB054     TO WRK-MASCARA.

           DISPLAY '*   GRAVADOS NA TABELA GFCTB054: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-GFCTB056     TO WRK-MASCARA.

           DISPLAY '*   GRAVADOS NA TABELA GFCTB056: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-GFCTB051     TO WRK-MASCARA.

           DISPLAY '*   GRAVADOS NA TABELA GFCTB051: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-GFCTB050     TO WRK-MASCARA.

           DISPLAY '*   GRAVADOS NA TABELA GFCTB050: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-GFCTB052     TO WRK-MASCARA.

           DISPLAY '*   GRAVADOS NA TABELA GFCTB052: ' WRK-MASCARA '  *'
LFM        MOVE ACU-GRAVA-GFCTB0M8         TO WRK-MASCARA
LFM
LFM        DISPLAY '*   GRAVADOS NA TABELA GFCTB0M8: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.
           DISPLAY '******************** GFCT1710 ******************'.

      *----------------------------------------------------------------*
       5200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXECUTA PROCEDIMENTO DE CANCELAMENTO DO PGM EM CASO DE ERRO *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1710'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
