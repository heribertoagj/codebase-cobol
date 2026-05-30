      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2220.
       AUTHOR.     ANA LUIZA PEZZINI.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2220                                    *
      *    PROGRAMADORA:   ANA LUIZA PEZZINI       - CPM PATO BRANCO   *
      *    ANALISTA CPM:   FELIPE SOMMER           - CPM PATO BRANCO   *
      *    ANALISTA....:   VANI NUNES              - PROCWORK / GP.50  *
      *    DATA........:   03/07/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR EVENTOS PARCELADOS DE GRUPO DE SER-   *
      *      VICOS CORRENTISTAS.                                       *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    MPARCCRT                  I#GFCTST          *
      *                    GRUPOCRT                  I#GFCTSU          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.TMOVTO_PCELD_CRRTT     GFCTB0I3      *
      *                    DB2PRD.COMPS_GRP_SERVC        GFCTB028      *
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

           SELECT MPARCCRT ASSIGN      TO UT-S-MPARCCRT
           FILE STATUS                 IS WRK-FS-MPARCCRT.

           SELECT GRUPOCRT ASSIGN      TO UT-S-GRUPOCRT
           FILE STATUS                 IS WRK-FS-GRUPOCRT.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OUTPUT: MOVIMENTO PARCELADO DE GRUPO DE SERVICOS CORRENTISTA*
      *            ORG. SEQUENCIAL     -    LRECL = 132                *
      *----------------------------------------------------------------*

       FD  MPARCCRT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTST.

      *----------------------------------------------------------------*
      *    OUTPUT: GRUPO DE SERVICOS CORRENTISTAS                      *
      *            ORG. SEQUENCIAL     -    LRECL = 100                *
      *----------------------------------------------------------------*

       FD  GRUPOCRT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTSU.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(037)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(026)         VALUE
           '* AREA PARA ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-MPARCCRT          PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-GRUPOCRT          PIC  9(011) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(034)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-FIM-CSR                 PIC  X(001)         VALUE SPACES.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC  Z.ZZZ.ZZZ.Z99  VALUE ZEROS.

       01  WRK-DATA                    PIC  X(010)         VALUE SPACES.
       01  WRK-DATA-R                  REDEFINES           WRK-DATA.
           05 WRK-DIA                  PIC  9(002).
           05 FILLER                   PIC  X(001).
           05 WRK-MES                  PIC  9(002).
           05 FILLER                   PIC  X(001).
           05 WRK-ANO                  PIC  9(004).

       01  WRK-AAAAMM                  PIC  9(006)         VALUE ZEROS.
       01  WRK-AAAAMM-R                REDEFINES WRK-AAAAMM.
           05 WRK-AAAA                 PIC  9(004).
           05 WRK-MM                   PIC  9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(035)         VALUE
           '* AREA PARA TESTES DE FILE STATUS *'.
      *----------------------------------------------------------------*

       01  WRK-FS-MPARCCRT             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-GRUPOCRT             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(035)         VALUE
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
       01  FILLER                      PIC  X(032)         VALUE
           '* AREA PARA TRATAMENTO DE ERRO *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(017)         VALUE
           '* AREA PARA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0I3
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB028
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(010)         VALUE
           '* CURSOR *'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR-GFCTB028      CURSOR FOR
             SELECT  CGRP_SERVC
             FROM    DB2PRD.COMPS_GRP_SERVC
             WHERE
                   CSERVC_TARIF       = :GFCTB028.CSERVC-TARIF    AND
                   DINIC_DEB_SERVC   <= :GFCTB028.DINIC-DEB-SERVC AND
                   DFIM_DEB_SERVC    >= :GFCTB028.DFIM-DEB-SERVC
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(034)         VALUE
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
                   UNTIL WRK-FS-MPARCCRT   EQUAL '10'.

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

           OPEN INPUT  MPARCCRT
                OUTPUT GRUPOCRT.

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

           PERFORM 1110-TESTAR-FS-MPARCCRT.

           PERFORM 1120-TESTAR-FS-GRUPOCRT.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTE DE FILE STATUS DO ARQUIVO MPARCCRT                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-MPARCCRT         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-MPARCCRT         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-MPARCCRT    TO WRK-FILE-STATUS
               MOVE 'MPARCCRT'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTE DE FILE STATUS DO ARQUIVO GRUPOCRT                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-GRUPOCRT         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GRUPOCRT         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-GRUPOCRT    TO WRK-FILE-STATUS
               MOVE 'GRUPOCRT'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICA SE ARQUIVO DE ENTRADA MPARCCRT VAZIO                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-MPARCCRT.

           IF  WRK-FS-MPARCCRT         EQUAL '10'
               DISPLAY '************** GFCT2220 **************'
               DISPLAY '*                                    *'
               DISPLAY '*       ARQUIVO MPARCCRT VAZIO       *'
               DISPLAY '*      PROCESSAMENTO  ENCERRADO      *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT2220 **************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LEITURA DO ARQUIVO MPARCCRT                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-MPARCCRT              SECTION.
      *----------------------------------------------------------------*

           READ MPARCCRT.

           IF  WRK-FS-MPARCCRT         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-MPARCCRT.

           ADD 1                       TO ACU-LIDOS-MPARCCRT.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO PRINCIPAL DO PROGRAMA                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-ABRIR-CURSOR-B028.

           PERFORM 3200-LER-CURSOR-B028.

           PERFORM 3300-TRATAR-GRUPO-SERVICOS  UNTIL
                   WRK-FIM-CSR                 EQUAL 'S'.

           PERFORM 3400-FECHAR-CURSOR-B028.

           PERFORM 2100-LER-MPARCCRT.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ABRIR CURSOR NA TABELA GCFTB028                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-ABRIR-CURSOR-B028          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTST-CSERVC-TARIF    TO CSERVC-TARIF     OF GFCTB028.
           MOVE GFCTST-DOCOR-EVNTO     TO DINIC-DEB-SERVC  OF GFCTB028
                                          DFIM-DEB-SERVC   OF GFCTB028.
           MOVE SPACES                 TO WRK-FIM-CSR.

           EXEC SQL
             OPEN    CSR-GFCTB028
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'COMPS_GRP_SERVC ' TO ERR-DBD-TAB
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0010'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LER   CURSOR NA TABELA GCFTB028                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-LER-CURSOR-B028            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH   CSR-GFCTB028
             INTO   :GFCTB028.CGRP-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'COMPS_GRP_SERVC ' TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0020'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CSR
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FECHAR CURSOR NA TABELA GCFTB028                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-FECHAR-CURSOR-B028         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE   CSR-GFCTB028
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'COMPS_GRP_SERVC ' TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0030'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAMENTO DO GRUPO DE SERVICOS                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-TRATAR-GRUPO-SERVICOS      SECTION.
      *----------------------------------------------------------------*

           PERFORM 3310-ACESSAR-TAB-MOVTO-PARCELA.

           MOVE SPACES                     TO GFCTSU-REG-PARCELADOS.
           INITIALIZE GFCTSU-REG-PARCELADOS.

           MOVE 2                          TO GFCTSU-IND-TARIF-GRPSERV.
           MOVE CGRP-SERVC OF GFCTB028     TO GFCTSU-CD-TARIF-GPSRV.

           IF  GFCTST-DEFETV-DEB-MOVTO     NOT EQUAL '01.01.0001'
               MOVE GFCTST-DEFETV-DEB-MOVTO TO WRK-DATA
               MOVE WRK-MES                TO WRK-MM
               MOVE WRK-ANO                TO WRK-AAAA
               MOVE WRK-AAAAMM             TO GFCTSU-ANO-MES-REF
           ELSE
               MOVE GFCTST-DOCOR-EVNTO     TO WRK-DATA
               MOVE WRK-MES                TO WRK-MM
               MOVE WRK-ANO                TO WRK-AAAA
               MOVE WRK-AAAAMM             TO GFCTSU-ANO-MES-REF
           END-IF.

           MOVE CPCELA-TARIF               OF GFCTB0I3
                                           TO GFCTSU-NUMERO-PARC.
           MOVE CTOT-PCELA-TARIF           OF GFCTB0I3
                                           TO GFCTSU-QTD-TOTAL-PARC.
           MOVE 1                          TO GFCTSU-CTPO-CLIE.
           MOVE GFCTST-VTARIF-CADTR-SIST   TO GFCTSU-VALOR-POTENCIAL.
           MOVE GFCTST-VTARIF-BRUTO-MOVTO  TO GFCTSU-VALOR-BRUTO.
           MOVE GFCTST-VTARIF-LIQ-MOVTO    TO GFCTSU-VALOR-POTENCIAL.
           MOVE GFCTST-VTARIF-DEB-MOVTO    TO GFCTSU-VALOR-DEBITADO.

           PERFORM 3320-GRAVAR-GRUPOCRT.

           PERFORM 3200-LER-CURSOR-B028.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSO A TABELA DE MOVIMENTO DE PARCELAS                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3310-ACESSAR-TAB-MOVTO-PARCELA  SECTION.
      *----------------------------------------------------------------*

           INSPECT GFCTST-DENVIO-MOVTO-TARIF REPLACING ALL '/' BY '.'.

           MOVE GFCTST-DENVIO-MOVTO-TARIF     TO
                DENVIO-MOVTO-TARIF    OF  GFCTB0I3.
           MOVE GFCTST-CROTNA-ORIGE-MOVTO     TO
                CROTNA-ORIGE-MOVTO    OF  GFCTB0I3.
           MOVE GFCTST-CNRO-ARQ-MOVTO TO CNRO-ARQ-MOVTO     OF GFCTB0I3.
           MOVE GFCTST-CSEQ-MOVTO     TO CSEQ-MOVTO         OF GFCTB0I3.

           EXEC SQL
             SELECT
                   CPCELA_TARIF,
                   CTOT_PCELA_TARIF
             INTO
                   :GFCTB0I3.CPCELA-TARIF,
                   :GFCTB0I3.CTOT-PCELA-TARIF
             FROM   DB2PRD.TMOVTO_PCELD_CRRTT
             WHERE
                   DENVIO_MOVTO_TARIF = :GFCTB0I3.DENVIO-MOVTO-TARIF AND
                   CROTNA_ORIGE_MOVTO = :GFCTB0I3.CROTNA-ORIGE-MOVTO AND
                   CNRO_ARQ_MOVTO     = :GFCTB0I3.CNRO-ARQ-MOVTO     AND
                   CSEQ_MOVTO         = :GFCTB0I3.CSEQ-MOVTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TMOVTO_PCELD_CRRTT' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0040'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSA DADOS PARA OBTER ARQUIVO DE SAIDA E RELATORIO.     *
      ******************************************************************
      *----------------------------------------------------------------*
       3320-GRAVAR-GRUPOCRT            SECTION.
      *----------------------------------------------------------------*

           WRITE GFCTSU-REG-PARCELADOS.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-GRUPOCRT.

           ADD 1                       TO ACU-GRAVA-GRUPOCRT.

      *----------------------------------------------------------------*
       3320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EMISSAO DE DISPLAY E FECHAMENTO DE ARQUIVOS                    *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS-MPARCCRT  GREATER  ZEROS
               PERFORM 4100-EMITIR-DISPLAY
           END-IF.

           CLOSE MPARCCRT
                 GRUPOCRT.

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
           DISPLAY '*                PROGRAMA GFCT2220               *'.
           DISPLAY '*                -----------------               *'.
           DISPLAY '*                                                *'.
           DISPLAY '*       PROCESSAMENTO EFETUADO COM SUCESSO       *'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-MPARCCRT     TO WRK-MASCARA.
           DISPLAY '* REGISTROS LIDOS    MPARCCRT = ' WRK-MASCARA
                                                                '    *'.
           MOVE ACU-GRAVA-GRUPOCRT     TO WRK-MASCARA.
           DISPLAY '* REGISTROS GRAVADOS GRUPOCRT = ' WRK-MASCARA
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

           MOVE 'GFCT2220'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
