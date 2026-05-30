      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2210.
       AUTHOR.     CARLA FLACH ZANETTI.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2210                                    *
      *    PROGRAMADORA:   CARLA FLACH ZANETTI     - CPM PATO BRANCO   *
      *    ANALISTA CPM:   EDSON LUIZ DOS SANTOS   - CPM PATO BRANCO   *
      *    ANALISTA....:   VANI NUNES              - PROCWORK / GP.50  *
      *    DATA........:   06/07/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR EVENTOS PARCELADOS DE TARIFAS - NAO   *
      *      CORRENTISTAS                                              *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    MOVPARCN                  I#GFCTSY          *
      *                    TARIFNAO                  I#GFCTSU          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.TMOVTO_PCELD_NAO       GFCTB0I4      *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - CONEXAO COM DB2                                  *
      *    POOL7100 - TRATAMENTO DE ERROS PADRAO BRADESCO              *
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

           SELECT MOVPARCN ASSIGN      TO UT-S-MOVPARCN
           FILE STATUS                 IS WRK-FS-MOVPARCN.

           SELECT TARIFNAO ASSIGN      TO UT-S-TARIFNAO
           FILE STATUS                 IS WRK-FS-TARIFNAO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OUTPUT: MOVIMENTO PARCELADO DE TARIFAS NAO CORRENTISTA      *
      *            ORG. SEQUENCIAL     -    LRECL = 128                *
      *----------------------------------------------------------------*

       FD  MOVPARCN
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTSY'.

      *----------------------------------------------------------------*
      *    OUTPUT: EVENTOS DE TARIFAS PARCELADAS NAO CORRENTISTA       *
      *            ORG. SEQUENCIAL     -    LRECL = 100                *
      *----------------------------------------------------------------*

       FD  TARIFNAO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTSU'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA ACUMULADORES *'.
      *----------------------------------------------------------------*

       01  ACU-LIDOS-MOVPARCN          PIC  9(011) COMP-3  VALUE ZEROS.
       01  ACU-GRAVA-TARIFNAO          PIC  9(011) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       01  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       01  WRK-MASCARA                 PIC  Z.ZZZ.ZZZ.Z99  VALUE ZEROS.
       01  WRK-VLR-POTENCIAL           PIC  9(011)V99      VALUE ZEROS.

       01  WRK-DATA                    PIC  X(010)         VALUE SPACES.
       01  WRK-DATA-R                  REDEFINES           WRK-DATA.
           05 WRK-DIA                  PIC  9(002).
           05 FILLER                   PIC  X(001).
           05 WRK-MES                  PIC  9(002).
           05 FILLER                   PIC  X(001).
           05 WRK-ANO                  PIC  9(004).

       01  WRK-AAAAMM.
           05 WRK-AAAA                 PIC  9(004).
           05 WRK-MM                   PIC  9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TESTES DE FILE STATUS *'.
      *----------------------------------------------------------------*

       01  WRK-FS-MOVPARCN             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-TARIFNAO             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

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
             INCLUDE GFCTB0I4
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      * CHAMA AS ROTINAS A SEREM PROCESSADAS                           *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR
                   UNTIL WRK-FS-MOVPARCN   EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CONEXAO COM DB2, ABERTURA DE ARQUIVOS E TESTE DE FILE STATUS   *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT  MOVPARCN
                OUTPUT TARIFNAO.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTE DE FILE STATUS DOS ARQUIVOS SEQUENCIAIS                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-MOVPARCN.

           PERFORM 1120-TESTAR-FS-TARIFNAO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTE DE FILE STATUS DO ARQUIVO MOVPARCN                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-MOVPARCN         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-MOVPARCN         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-MOVPARCN    TO WRK-FILE-STATUS
               MOVE 'MOVPARCN'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTE DE FILE STATUS DO ARQUIVO TARIFNAO                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-TARIFNAO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-TARIFNAO         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-TARIFNAO    TO WRK-FILE-STATUS
               MOVE 'TARIFNAO'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICA SE ARQUIVO DE ENTRADA MOVPARCN VAZIO                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-MOVPARCN.

           IF  WRK-FS-MOVPARCN         EQUAL '10'
               DISPLAY '************** GFCT2210 **************'
               DISPLAY '*                                    *'
               DISPLAY '*       ARQUIVO MOVPARCN VAZIO       *'
               DISPLAY '*                                    *'
               DISPLAY '*      PROCESSAMENTO  ENCERRADO      *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT2210 **************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LEITURA DO ARQUIVO MOVPARCN                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-MOVPARCN              SECTION.
      *----------------------------------------------------------------*

           READ MOVPARCN.

           IF  WRK-FS-MOVPARCN         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-MOVPARCN.

           ADD 1                       TO ACU-LIDOS-MOVPARCN.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO PRINCIPAL DO PROGRAMA                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTSU-REG-PARCELADOS.
           INITIALIZE GFCTSU-REG-PARCELADOS.

           PERFORM 3100-ACESSAR-GFCTB0I4.

           MOVE  1                         TO GFCTSU-IND-TARIF-GRPSERV.
           MOVE GFCTSY-CSERVC-TARIF        TO GFCTSU-CD-TARIF-GPSRV.

           IF  DENVIO-MOVTO-TARIF          OF GFCTB0I4
                                           NOT EQUAL '01.01.0001'
               MOVE DENVIO-MOVTO-TARIF     OF GFCTB0I4
                                           TO WRK-DATA
               MOVE WRK-MES                TO WRK-MM
               MOVE WRK-ANO                TO WRK-AAAA
               MOVE WRK-AAAAMM             TO GFCTSU-ANO-MES-REF
           ELSE
               MOVE GFCTSY-DOCOR-EVNTO     TO WRK-DATA
               MOVE WRK-MES                TO WRK-MM
               MOVE WRK-ANO                TO WRK-AAAA
               MOVE WRK-AAAAMM             TO GFCTSU-ANO-MES-REF
           END-IF.

           MOVE CPCELA-TARIF               OF GFCTB0I4
                                           TO GFCTSU-NUMERO-PARC.
           MOVE CTOT-PCELA-TARIF           OF GFCTB0I4
                                           TO GFCTSU-QTD-TOTAL-PARC.
           MOVE 02                         TO GFCTSU-CTPO-CLIE.

           MOVE GFCTSY-VTARIF-CADTR-SIST   TO GFCTSU-VALOR-POTENCIAL.
           MOVE GFCTSY-VBRUTO-TARIF-MOVTO  TO GFCTSU-VALOR-BRUTO.
           MOVE GFCTSY-VTARIF-LIQ-MOVTO    TO GFCTSU-VALOR-LIQUIDO.
           MOVE GFCTSY-VTARIF-DEB-MOVTO    TO GFCTSU-VALOR-DEBITADO.

           PERFORM 3200-GRAVAR-TARIFNAO.

           PERFORM 2100-LER-MOVPARCN.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSO A TABELA DE MOVIMENTO DE PARCELAS NAO CORRENTISTA       *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-ACESSAR-GFCTB0I4           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTSY-DENVIO-MOVTO-TARIF
                                      TO DENVIO-MOVTO-TARIF OF GFCTB0I4.
           MOVE GFCTSY-CROTNA-ORIGE-MOVTO
                                      TO CROTNA-ORIGE-MOVTO OF GFCTB0I4.
           MOVE GFCTSY-CNRO-ARQ-MOVTO TO CNRO-ARQ-MOVTO     OF GFCTB0I4.
           MOVE GFCTSY-CSEQ-MOVTO     TO CSEQ-MOVTO         OF GFCTB0I4.

           EXEC SQL
             SELECT
                   CPCELA_TARIF,
                   CTOT_PCELA_TARIF
             INTO
                   :GFCTB0I4.CPCELA-TARIF,
                   :GFCTB0I4.CTOT-PCELA-TARIF
             FROM   DB2PRD.TMOVTO_PCELD_NAO
             WHERE
                   DENVIO_MOVTO_TARIF = :GFCTB0I4.DENVIO-MOVTO-TARIF AND
                   CROTNA_ORIGE_MOVTO = :GFCTB0I4.CROTNA-ORIGE-MOVTO AND
                   CNRO_ARQ_MOVTO     = :GFCTB0I4.CNRO-ARQ-MOVTO     AND
                   CSEQ_MOVTO         = :GFCTB0I4.CSEQ-MOVTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TMOVTO_PCELD_NAO' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0010'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * GRAVA REGISTRO NO ARQUIVO DE SAIDA TARIFNAO                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-GRAVAR-TARIFNAO            SECTION.
      *----------------------------------------------------------------*

           WRITE GFCTSU-REG-PARCELADOS.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-TARIFNAO.

           ADD 1                       TO ACU-GRAVA-TARIFNAO.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EMISSAO DE DISPLAY E FECHAMENTO DE ARQUIVOS                    *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS-MOVPARCN      GREATER ZEROS
               PERFORM 4100-EMITIR-DISPLAY
           END-IF.

           CLOSE MOVPARCN
                 TARIFNAO.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EMISSAO DE DISPLAY FINAL COM TOTALIZADORES                     *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*                PROGRAMA GFCT2210               *'.
           DISPLAY '*                -----------------               *'.
           DISPLAY '*                                                *'.
           DISPLAY '*       PROCESSAMENTO EFETUADO COM SUCESSO       *'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-MOVPARCN     TO WRK-MASCARA.
           DISPLAY '* REGISTROS LIDOS    MOVPARCN = ' WRK-MASCARA
                                                            '    *'.
           MOVE ACU-GRAVA-TARIFNAO     TO WRK-MASCARA.
           DISPLAY '* REGISTROS GRAVADOS TARIFNAO = ' WRK-MASCARA
                                                            '    *'.
           DISPLAY '*                                                *'.
           DISPLAY '*------------------------------------------------*'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EXECUTA TRATAMENTO DE ERROS                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2210'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
